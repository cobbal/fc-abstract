{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-cse -O0 #-}

#define NOT_IMPL() (error ("Not implemented error at " ++ __FILE__ ++ ":" ++ show __LINE__))

module FCAbstract.CESK where

import GhcPlugins
import Data.Map as Map
import Data.Map ((!), Map, member)
import Data.IORef
import System.IO.Unsafe

instance OutputableBndr b => Show (Expr b) where
  show = showSDoc . ppr

unsafeLog :: Show a => a -> a
unsafeLog x = unsafePerformIO (putStrLn (show x)) `seq` x

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

-- TODO: not require env when Val isn't a function
type Closure = (Val, Env)
type Env = Map Var Addr
type Store = Map Addr Closure

data Continuation = Kmt
                    | Kar CoreExpr Env Continuation
                    | Kfn Val Env Continuation
                    deriving Show

data CESK = CESK {
  control :: CoreExpr,
  env :: Env,
  store :: Store,
  kontinuation :: Continuation
  } deriving Show

inject :: CoreExpr -> CESK
inject e = CESK e Map.empty Map.empty Kmt

step :: CESK -> Either Val CESK
--step (CESK c e s Kmt) = Left c
step (CESK (Var ident) e s k) =
  case Map.lookup ident e of
    Just addr -> let (v, e') = (s ! addr) in
      Right (CESK v e' s k)
    Nothing -> error ("unbound variable: " ++ showSDoc (ppr ident))
step (CESK (Lit _) e s k) = NOT_IMPL()
step (CESK (App fn arg) e s k) =
  Right (CESK fn e s (Kar arg e k))
step (CESK l@(Lam formal body) e s (Kar c e' k)) =
  Right (CESK c e' s (Kfn l e k))
step (CESK c e s (Kfn (Lam formal body) e' k)) =
  Right (CESK body e'' s' k)
  where
    -- TODO: do more than hope birthday paradox doesn't happen
    addr = genInt ()
    e'' = Map.insert formal addr e'
    s' = Map.insert addr (c, e) s
step (CESK (Let (NonRec var val) expr) e s k) =
  Right (CESK val e s (Kfn (Lam var expr) e k))
step (CESK (Case _ _ _ _) e s k) = NOT_IMPL()
step (CESK (Cast _ _) e s k) = NOT_IMPL()
step (CESK t@(Type _) e s (Kar c e' k)) =
  Right (CESK c e' s (Kfn t e k))
step (CESK (Coercion _) e s k) = NOT_IMPL()
step (CESK c _ _ Kmt) = Left c

eval :: CoreExpr -> Val
eval = eval' . inject
  where
    eval' :: CESK -> Val
    eval' m = case step (unsafeLog m) of
      Left v -> unsafeLog v
      Right m' -> eval' m'
