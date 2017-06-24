{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module ScoreSizeJoin where

import Buffer
import Sized
import JoinList
import Scrabble
import Editor

type ScoreSizeJoin = JoinList (Score, Size) String

instance Buffer ScoreSizeJoin where
    toString Empty = mempty
    toString (Single _ s) = s
    toString (Append _ left Empty) = toString left
    toString (Append _ Empty right) = toString right
    toString (Append _ left right) = concat [ toString left,"\n", toString right]

    fromString = stringToJoinL . lines
        where
            stringToJoinL []    = Empty
            stringToJoinL [s]   = Single (scoreString s, Size 1) s
            stringToJoinL lines = stringToJoinL left +++ stringToJoinL right
                where (left, right) = splitAt (div (length lines) 2) lines

    line = indexJ 

    replaceLine i s b = before +++ fromString s +++ after
        where
            before  = takeJ i b
            after   = dropJ (i + 1) b

    numLines l = getSize $ snd $ tag $ l

    value l = getScore $ fst $ tag $ l

main2 :: IO ()
main2 = runEditor editor (((fromString . unlines)
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ])::ScoreSizeJoin)
