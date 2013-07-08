module LazyCESKLambda where

import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.List (intercalate)

data Expr = Id String
          | Lam String Expr
          | App Expr Expr
          deriving Eq

data Stored = Val Expr Env
            | Thunk Expr Env

data Continuation = Kmt
                  | Kar Expr Env Continuation
                  | Kst Int Continuation

type Env = Map String Int
type Store = Map Int Stored

data LCESK = LCESK {
  control :: Expr,
  env :: Env,
  store :: Store,
  kont :: Continuation,
  counter :: Int
  }

pprMap :: (Show a, Show b) => Map a b -> String
pprMap m = "{" ++ intercalate ", " (map pairPrint (Map.assocs m)) ++ "}"
  where
    pairPrint :: (Show a, Show b) => (a, b) -> String
    pairPrint (k, v) = show k ++ "→" ++ show v

instance Show Expr where
  show e
    | e == true = "t"
    | e == false = "f"
    | e == omega = "ω"
    | e == uComb = "U"
    | e == iComb = "I"
  show (Id s) = s
  show (Lam x e) = concat ["(λ", x, ".", show e, ")"]
  show (App f a) = concat ["(", show f, " ", show a, ")"]

instance Show Stored where
  show (Val expr env) = concat ["ε(", show expr, ", ", pprMap env, ")"]
  show (Thunk expr env) = concat ["θ(", show expr, ", ", pprMap env, ")"]

instance Show Continuation where
  show Kmt = "mt"
  show (Kar expr env k) = concat ["ar(", show expr, ", ", pprMap env, ", ", show k, ")"]
  show (Kst a k) = concat ["st(", show a, ", ", show k, ")"]

instance Show LCESK where
  show (LCESK c e s k i) = "〈" ++ intercalate ", " [show c, pprMap e, pprMap s, show k] ++ "〉"

inject :: Expr -> LCESK
inject e = LCESK e Map.empty Map.empty Kmt 0

step :: LCESK -> Either Expr LCESK
step (LCESK (Id ident) e s k i) =
  case Map.lookup ident e of
    Just addr ->
      case (s ! addr) of
        Val v e' -> Right (LCESK v e' s k i)
        Thunk v e' -> Right (LCESK v e' s (Kst addr k) i)
    Nothing -> error ("unbound variable: " ++ ident)
step (LCESK (App fn arg) e s k i) =
  Right (LCESK fn e s (Kar arg e k) i)
step (LCESK c e s (Kst addr k) i) =
  Right (LCESK c e (Map.insert addr (Val c e) s) k i)
step (LCESK (Lam formal body) e s (Kar v e' k) addr) =
  Right (LCESK body e'' s' k (addr + 1))
  where
    e'' = Map.insert formal addr e
    s' = Map.insert addr (Thunk v e') s
step (LCESK c _ _ Kmt _) = Left c

true :: Expr
true = Lam "a" (Lam "b" (Id "a"))

false :: Expr
false = Lam "a" (Lam "b" (Id "b"))

uComb :: Expr
uComb = Lam "x" (App (Id "x") (Id "x"))

omega :: Expr
omega = App uComb uComb

example :: Expr
example = App (App true false) true

test :: Expr
test = App (App false omega) true

iComb :: Expr
iComb = Lam "x" (Id "x")

eval :: Expr -> IO Expr
eval e = eval' (inject e)
  where
    eval' :: LCESK -> IO Expr
    eval' m = do
      putStrLn (" -> " ++ show m)
      case step m of
        Left e -> putStrLn (" => " ++ show e) >> return e
        Right m' -> eval' m'
