module Main where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

evalStr :: String -> Maybe Integer
evalStr str = fmap eval $ parseExp Lit Add Mul str

class Expr a where
	lit :: Integer -> a
	mul :: a -> a -> a
	add :: a -> a -> a

instance Expr ExprT where
	lit = Lit
	add = Add
	mul = Mul




main :: IO ()
main = do
  putStrLn "hello world"
