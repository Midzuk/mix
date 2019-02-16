module Main where

import qualified Data.Set as S

import Lib
import Mix

main :: IO ()
main = print $ mix' f [1..30000]

f x y
  | x * 2 == y = True
  | x == y + 2 = True
  | x * 3 == y = True
  | x == y ^ 3 = True
  | x * 4 == y = True
  | x == y ^ 4 = True
  | x + 2 == y = True
  | otherwise = False