{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a rest) = Cons (f a) $ streamMap f rest

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nats :: Stream Integer
nats = streamFromSeed (+1) 0


-- ruler skipped. Could not figure it out

-- ex 6 skipped because I am not interested

data Matrix = Matrix Integer Integer Integer Integer
    deriving Show

instance Num Matrix where
    (*) (Matrix a b c d) (Matrix e f g h) = (Matrix (a * e + b * g) (a * f + b * h) (c * e + d * g) (c * f + d * h) )

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = let get (Matrix _ b _ _) = b
    in get ((Matrix 1 1 1 0)^n)

main :: IO ()
main = do
  putStrLn "hello world"
