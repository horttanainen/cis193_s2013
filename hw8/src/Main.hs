module Main where

import Party

main :: IO ()
main = do
  readFile "src/company.txt" >>= print . maxFun . read
