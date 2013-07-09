{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-cse -O0 #-}

#define NOT_IMPL() (error ("Not implemented error at " ++ __FILE__ ++ ":" ++ show __LINE__))

module FCAbstract.CESK where

import GhcPlugins
import qualified Data.Map as Map
import Data.Map ((!), Map, member)
import Data.IORef
import System.IO.Unsafe
import Data.Char (ord, chr)

instance OutputableBndr b => Show (Expr b) where
  show = showSDoc . ppr

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

data Storable = StVal CoreExpr Env
              | StThunk CoreExpr Env
              deriving Show

type Env = Map Var Addr

data Store = Store (Map Addr Storable) (Map KAddr Continuation) Int
           deriving Show

mkStore :: Store
mkStore = Store Map.empty Map.empty 0

storeVal :: Store -> Storable -> (Addr, Store)
storeVal (Store vm km nextA) v =
  (Addr nextA, Store (Map.insert (Addr nextA) v vm) km (nextA + 1))

storeKont :: Store -> Continuation -> (KAddr, Store)
storeKont (Store vm km nextA) k =
  (KAddr nextA, Store vm (Map.insert (KAddr nextA) k km) (nextA + 1))

replaceVal :: Store -> Addr -> Storable -> Store
replaceVal (Store vm km nextA) a v =
  Store (Map.insert a v vm) km nextA

lookupVal :: Store -> Addr -> Storable
lookupVal (Store vm _ _) = (vm !)

lookupKont :: Store -> KAddr -> Continuation
lookupKont (Store _ km _) = (km !)

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

step :: CESK -> Either CoreExpr CESK
--step (CESK c e s Kmt) = Left c
step (CESK c e s ka) =
  stepk c e s (lookupKont s ka)
  where
    stepk :: CoreExpr -> Env -> Store -> Continuation
             -> Either CoreExpr CESK
    stepk (Var ident) e s k
      | Just addr <- Map.lookup ident e =
        case lookupVal s addr of
          StVal v e' -> Right (CESK v e' s ka)
          StThunk v e' ->
            let (thunkKa, thunkS) = storeKont s (Kst addr ka) in
            Right (CESK v e' thunkS thunkKa)
    stepk (Lit _) e s k = NOT_IMPL()
    stepk (App fn arg) e s k =
      Right (CESK fn e thunkS thunkKa)
      where
        (thunkKa, thunkS) = storeKont s (Kar arg e ka)
    stepk c e s (Kst addr ka') =
        Right (CESK c e s' ka')
      where
        (addr, s') = storeVal s (StVal c e)
    stepk (Lam formal body) e s (Kar v e' ka') =
        let (addr, s') = storeVal s (StThunk v e')
            e'' = Map.insert formal addr e in
        Right (CESK body e'' s' ka')
    stepk (Let (NonRec var val) expr) e s k =
      -- punt and desugar
      stepk (App (Lam var expr) val) e s k
    stepk (Let (Rec binds) expr) e s k =
      Right (CESK expr e' s' ka)
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
    stepk c _ _ Kmt = Left c

eval :: CoreExpr -> CoreExpr
eval = eval' . inject
  where
    eval' :: CESK -> CoreExpr
    eval' m = case step (unsafeLog "\n-> " m) of
      Left v -> unsafeLog "\n=> " v
      Right m' -> eval' m'
