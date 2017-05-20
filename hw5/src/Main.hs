module Main where

import ExprT

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

main :: IO ()
main = do
  putStrLn "hello world"
