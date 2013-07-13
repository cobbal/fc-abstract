module Main where

data Foo = Bar
         | Baz

y f = f (y f)
true a b = a
false a b = b
not m = m false true

-- type Boolean a = a -> a -> a
-- and :: Boolean a -> Boolean a -> Boolean a
and_ x y = x y false
or_ x y = x true y

stupid = y ((or_ false true) true) Bar Baz

main = return ()
