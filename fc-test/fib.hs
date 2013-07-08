module Main where

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

dummy :: Int -> Int
dummy n = let f x = x in f n

data Foo = Bar

stupid :: Int -> Int
stupid = y (true true) 2
  where
    id x = x
    y f = f (y f)
    true a b = a
    false a b = b
    not m = m false true

main :: IO ()
main = putStrLn (show (fib (dummy 10)))
