{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-cse -O0 #-}

#if 0
#define NOT_IMPL() (error ("Not implemented error at " ++ __FILE__ ++ ":" ++ show __LINE__))
#else
#define NOT_IMPL() ([])
#endif

module FCAbstract.CESK where

import GhcPlugins
import qualified Data.Map as Map
import Data.Map ((!), Map, member)
import Data.IORef
import System.IO.Unsafe
import Data.Char (ord, chr)
import Data.Either (partitionEithers)

instance OutputableBndr b => Show (Expr b) where
  show = showSDoc tracingDynFlags . ppr

unsafeLog :: Show a => String -> a -> a
unsafeLog m x = unsafePerformIO (putStrLn (m ++ show x)) `seq` x

newtype Addr = Addr Int
             deriving (Ord, Eq)
newtype KAddr = KAddr Int
             deriving (Ord, Eq)

instance Show Addr where
  show (Addr x) = "A" ++ show x

instance Show KAddr where
  show (KAddr x) = "K" ++ show x

instance Show Var where
  show = showSDoc tracingDynFlags . ppr

data Storable = StVal CoreExpr Env
              | StThunk CoreExpr Env
              deriving Show

type Env = Map Var Addr

data Store = Store {
  sValues :: Map Addr [Storable],
  sKonts :: Map KAddr [Continuation],
  sNextAddr :: Int,
  sCompactor :: Int -> Int
}

instance Show Store where
  show s = "Store {sValues = {" ++ show (Map.assocs (sValues s)) ++
           "}, sKonts = {" ++ show (Map.assocs (sKonts s)) ++
           "}, sNextAddr = " ++ show (sNextAddr s) ++ "}"

data MachineSize = Finite Int Int -- memory and time
                | Infinite

finiteness :: MachineSize
-- finiteness = Finite 50 70
finiteness = Infinite

mkStore :: Store
mkStore = Store Map.empty Map.empty 0 (case finiteness of
                                          Finite m t -> flip mod m
                                          Infinite -> id)

storeVal :: Store -> Storable -> (Addr, Store)
storeVal (Store vm km nextA c) v =
  (Addr nextA,
   Store (Map.alter change (Addr nextA) vm) km (c (nextA + 1)) c)
  where
    change :: Maybe [Storable] -> Maybe [Storable]
    change Nothing = Just [v]
    change (Just vs) = Just (v : vs)

storeKont :: Store -> Continuation -> (KAddr, Store)
storeKont (Store vm km nextA c) k =
  (KAddr nextA,
   Store vm (Map.alter  change (KAddr nextA) km) (c (nextA + 1)) c)
  where
    change :: Maybe [Continuation] -> Maybe [Continuation]
    change Nothing = Just [k]
    change (Just ks) = Just (k : ks)

replaceVal :: Store -> Addr -> Storable -> [Store]
replaceVal (Store vm km nextA c) a v = do
  (left, target, right) <- allPivots (vm ! a)
  return (Store (Map.insert a (left ++ (v : right)) vm) km nextA c)

allPivots :: [a] -> [([a], a, [a])]
allPivots = help []
  where
    help _ [] = []
    help left (x : xs) = (left, x, xs) : help (left ++ [x]) xs

lookupVal :: Store -> Addr -> [Storable]
lookupVal s = (sValues s !)

lookupKont :: Store -> KAddr -> [Continuation]
lookupKont s = (sKonts s !)

data Continuation = Kmt
                    | Kar CoreExpr Env KAddr
                    | Kst Addr KAddr
                      -- store result of thunk in place of variable
                    deriving Show

data CESK = CESK {
  control :: CoreExpr,
  env :: Env,
  store :: Store,
  kont :: KAddr
  } deriving Show

inject :: CoreExpr -> CESK
inject e = CESK e Map.empty s ka
  where (ka, s) = storeKont mkStore Kmt

step :: CESK -> ([CoreExpr], [CESK])
--step (CESK c e s Kmt) = Left c
step (CESK c e s ka) =
  partitionEithers (concat (map (stepk c e s) (lookupKont s ka)))
 where
    stepk :: CoreExpr -> Env -> Store -> Continuation
             -> [Either CoreExpr CESK]
    stepk (Var ident) e s k
      | Just addr <- Map.lookup ident e = do
        val <- lookupVal s addr
        case val of
          StVal v e' -> return (Right (CESK v e' s ka))
          StThunk v e' ->
            let (thunkKa, thunkS) = storeKont s (Kst addr ka) in
            return (Right (CESK v e' thunkS thunkKa))
    stepk (Lit _) e s k = NOT_IMPL()
    stepk (App fn arg) e s k =
      return (Right (CESK fn e thunkS thunkKa))
      where
        (thunkKa, thunkS) = storeKont s (Kar arg e ka)
    stepk c e s (Kst addr ka') =
        return (Right (CESK c e s' ka'))
      where
        (addr, s') = storeVal s (StVal c e)
    stepk (Lam formal body) e s (Kar v e' ka') =
        let (addr, s') = storeVal s (StThunk v e')
            e'' = Map.insert formal addr e in
        return (Right (CESK body e'' s' ka'))
    stepk (Let (NonRec var val) expr) e s k =
      -- punt and desugar
      stepk (App (Lam var expr) val) e s k
    stepk (Let (Rec binds) expr) e s k =
      return (Right (CESK expr e' s' ka))
      where
        (s', varAddrs) = foldl storeBind (s, []) binds
        storeBind :: (Store, [(Addr, Var)]) -> (Var, CoreExpr)
                     -> (Store, [(Addr, Var)])
        storeBind (s, as) (var, val) = (s', (a, var) : as)
          where (a, s') = storeVal s (StThunk val e')
        e' = foldl (\m (a, var) -> Map.insert var a m) e varAddrs
    stepk (Case _ _ _ _) e s k = NOT_IMPL()
    stepk (Cast _ _) e s k = NOT_IMPL()
    stepk (Type _) e s k = NOT_IMPL()
    stepk (Coercion _) e s k = NOT_IMPL()
    stepk c _ _ Kmt = return (Left c)
    stepk c e s k = []

eval :: CoreExpr -> [CoreExpr]
eval = eval' (case finiteness of
                 Finite m t -> Just t
                 Infinite -> Nothing) . inject
  where
    eval' :: Maybe Int -> CESK -> [CoreExpr]
    eval' (Just 0) _ = []
    eval' n m =
      case step m of
        (vs, []) -> vs
        (vs, ms) -> vs ++ (ms >>= eval' (fmap (subtract 1) n))
