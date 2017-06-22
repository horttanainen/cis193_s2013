module Main where

fib :: Integer -> Integer
fib 0   = 0
fib 1   = 1
fib n   = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

main :: IO ()
main = do
  putStrLn "hello world"
