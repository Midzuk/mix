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
  case partitionEithers $ (\x1 -> x1 `maybeToRight` f x x1) <$> xs of
    (_, _) -> undefined
  -- where
  --   g :: (a -> Maybe (w a)) -> a -> Validation [a] (w a)
  --   g h x = eitherToValidation . maybeToRight [x] $ h x