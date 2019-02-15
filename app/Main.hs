module Main where

import qualified Data.Set as S

import Lib
import Mix

main :: IO ()
main = print $ mix f (S.fromList [1..6])

f 1 2 = True
f 2 3 = True
f 3 1 = True
f 1 4 = True
f 4 1 = True
f 2 5 = True
f 4 6 = True
f _ _ = False