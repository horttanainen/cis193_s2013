module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m
tag (Append m _ _)  = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append ((tag l1) <> (tag l2)) l1 l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ i _ | i < 0      = Nothing
indexJ i (Single _ a)
    | i == 0            = Just a
    | otherwise         = Nothing
indexJ i (Append b left right)
    | i >= wholeSize            = Nothing
    | i < leftSize              = indexJ i left
    | otherwise                 = indexJ (i - leftSize) right
    where
        wholeSize   = getSize $ size b
        leftSize    = getSize $ size $ tag left


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty           = Empty
dropJ 0 joinL           = joinL
dropJ n (Single _ _)    = Empty
dropJ n (Append b left right)
    | n >= wholeSize    = Empty
    | n == leftSize     = right
    | n < leftSize      = Append b (dropJ n left) right
    | n > leftSize      = dropJ (n - leftSize) right
    where
        wholeSize   = getSize $ size b
        leftSize    = getSize $ size $ tag left

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty           = Empty
takeJ 0 _               = Empty
takeJ n s@(Single _ _)  = s
takeJ n f@(Append b left right)
    | n >= wholeSize    = f
    | n == leftSize     = left
    | n < leftSize      = takeJ n left
    | n > leftSize      = Append b left (takeJ (n - leftSize) right)
    where
        wholeSize   = getSize $ size b
        leftSize    = getSize $ size $ tag left
-- For testing purposes only

(!!?) :: [a] -> Int -> Maybe a
[]  !!? _ = Nothing
_   !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty          = []
jlToList (Single _ a)   = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

jl  :: JoinList Size Char
jl  = Append (Size 4)
            (Append (Size 3)
                (Single (Size 1) 'y')
                (Append (Size 2)
                    (Single (Size 1) 'e')
                    (Single (Size 1) 'a')))
            (Single (Size 1) 'h')
