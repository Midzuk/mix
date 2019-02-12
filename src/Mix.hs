module Mix
  ( mix
  ) where

import Control.Comonad.Cofree
import Control.Monad.State
import Control.Comonad
import Data.Either
import Data.Either.Combinators
import Data.Either.Validation

mix :: (Comonad w, Semigroup (w a))
    => (a -> a -> Maybe (w a))
    -> [a]
    -> [a]

mix _ [] = []
mix _ [x] = [x]
mix f (x:xs) =
  -- case partitionEithers $ (\x1 -> x1 `maybeToRight` f x x1) <$> xs of　-- :: ([a], [w a])
  --  (_, _) -> undefined
  -- where
  --   g :: (a -> Maybe (w a)) -> a -> Validation [a] (w a)
  --   g h x = eitherToValidation . maybeToRight [x] $ h x

mix :: Monoid a
    => (a -> a -> Bool)
    -> [a]
    -> [a]

mix _ [] = []
mix _ [x] = [x]
mix f (x:xs) =
  let
    (xs1, xs2) = f x `partition` xs
  in
    if null xs1
    then x : mix xs
    mix $ ((x <>) <$> xs1) <> xs2

-- Cofree [] a