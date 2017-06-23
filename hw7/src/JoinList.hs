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
indexJ _ Empty                              = Nothing
indexJ i _ | i < 0                          = Nothing
indexJ i (Single _ a)
    | i == 0            = Just a
    | otherwise         = Nothing
indexJ i (Append b left right)
    | i > wholeSize             = Nothing
    | i <= leftSize             = indexJ i left
    | otherwise                 = indexJ (i - leftSize) right
    where
        wholeSize   = getSize $ size b
        leftSize    = getSize $ size $ tag left
