{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score c
  | elem c' "aeilnorstu" = Score 1
  | elem c' "dg"         = Score 2
  | elem c' "bcmp"       = Score 3
  | elem c' "fhvwy"      = Score 4
  | elem c' "k"          = Score 5
  | elem c' "jx"         = Score 8
  | elem c' "qz"         = Score 10
  | otherwise            = Score 0
    where c' = toLower c

scoreString :: String -> Score
scoreString s   = foldr (\el acc -> (score el) <> acc) mempty s
