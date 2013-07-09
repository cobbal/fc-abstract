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

instance OutputableBndr b => Show (Expr b) where
  show = showSDoc . ppr

unsafeLog :: Show a => String -> a -> a
unsafeLog m x = unsafePerformIO (putStrLn (m ++ show x)) `seq` x

counter :: IORef Int
counter = unsafePerformIO $ do
  putStrLn "MAKING A NEW IOREF"
  newIORef 0

genInt :: () -> Int
genInt _ = unsafePerformIO $ do
  modifyIORef counter (+1)
  r <- readIORef counter
  putStrLn ("gen -> " ++ show r)
  return r
{-# NOINLINE genInt #-}

-- Value only types in here please
-- (not quite sure if that means anything in Haskell)
-- (TODO: figure that out)
type Val = CoreExpr

type Addr = Int

data Stored = StVal Val Env
            | StThunk Val Env
            deriving Show

type Env = Map Var Addr

data Store = Store (Map Addr Stored) Addr
           deriving Show

data Continuation = Kmt
                    | Kar CoreExpr Env Continuation
                    | Kst Addr Continuation
                      -- store result of thunk in place of variable
                    deriving Show

data CESK = CESK {
  control :: CoreExpr,
  env :: Env,
  store :: Store,
  kont :: Continuation
  } deriving Show

inject :: CoreExpr -> CESK
inject e = CESK e Map.empty (Store Map.empty 0) Kmt

step :: CESK -> Either Val CESK
--step (CESK c e s Kmt) = Left c
step (CESK (Var ident) e s@(Store ss _) k)
  | Just addr <- Map.lookup ident e =
    case ss ! addr of
        StVal v e' -> Right (CESK v e' s k)
        StThunk v e' -> Right (CESK v e' s (Kst addr k))
    --Nothing -> error ("unbound variable: " ++ showSDoc (ppr ident))
step (CESK (Lit _) e s k) = NOT_IMPL()
step (CESK (App fn arg) e s k) =
  Right (CESK fn e s (Kar arg e k))
step (CESK c e (Store ss maxAddr) (Kst addr k)) =
  Right (CESK c e s' k)
  where s' = Store (Map.insert addr (StVal c e) ss) maxAddr
step (CESK (Lam formal body) e (Store ss addr) (Kar v e' k)) =
  Right (CESK body e'' s' k)
  where
    e'' = Map.insert formal addr e
    s' = Store (Map.insert addr (StThunk v e') ss) (addr + 1)
step (CESK (Let (NonRec var val) expr) e s k) =
  -- punt and desugar
  Right (CESK (App (Lam var expr) val) e s k)
step (CESK (Let (Rec binds) expr) e s@(Store ss addr) k) =
  Right (CESK expr e' s' k)
  where
    addrsVarsVals = zip3 [addr..] (map fst binds) (map snd binds)
    e' = foldl (\m (a, var, val) -> Map.insert var a m) e addrsVarsVals
    s' = foldl (\(Store m _) (a, var, val) ->
                 Store (Map.insert a (StThunk val e') m) (a + 1))
         s addrsVarsVals
step (CESK (Case _ _ _ _) e s k) = NOT_IMPL()
step (CESK (Cast _ _) e s k) = NOT_IMPL()
step (CESK (Type _) e s k) = NOT_IMPL()
step (CESK (Coercion _) e s k) = NOT_IMPL()
step (CESK c _ _ Kmt) = Left c

eval :: CoreExpr -> Val
eval = eval' . inject
  where
    eval' :: CESK -> Val
    eval' m = case step (unsafeLog "\n-> " m) of
      Left v -> unsafeLog "\n=> " v
      Right m' -> eval' m'
