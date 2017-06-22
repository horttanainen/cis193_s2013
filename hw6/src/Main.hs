module Main where

fib :: Integer -> Integer
fib 0   = 0
fib 1   = 1
fib n   = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

fibs2 :: [Integer]
fibs2 = fibTmp 0 1 where
    fibTmp a b = a : fibTmp b (a + b)

data Stream a = Cons a (Stream a) 

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a rest) = a : streamToList rest

main :: IO ()
main = do
  putStrLn "hello world"
