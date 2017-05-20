{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import ExprT
import Parser
import StackVM

eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add e1 e2) = (eval e1) + (eval e2)
eval (ExprT.Mul e1 e2) = (eval e1) * (eval e2)

evalStr :: String -> Maybe Integer
evalStr str = fmap eval $ parseExp ExprT.Lit ExprT.Add ExprT.Mul str

class Expr a where
        lit :: Integer -> a
        mul :: a -> a -> a
        add :: a -> a -> a

instance Expr ExprT where
        lit = ExprT.Lit
        add = ExprT.Add
        mul = ExprT.Mul

instance Expr Integer where
        lit = id
        add = (+)
        mul = (*)

instance Expr Bool where
        lit x
                | x <= 0         = False
                | otherwise         = True
        add = (||)
        mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
        lit = MinMax
        add (MinMax x) (MinMax y) = MinMax (max x y)
        mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
        lit x = Mod7 (mod x 7)
        add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
        mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr Program where
        lit i         = [PushI i]
        add x y = x ++ y ++ [StackVM.Add]
        mul x y = x ++ y ++ [StackVM.Mul]

testProg = testExp :: Maybe Program

compile :: String -> Maybe Program
compile = parseExp lit add mul

main :: IO ()
main = do
  putStrLn "hello world"
