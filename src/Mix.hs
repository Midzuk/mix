{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs         #-}
-- {-# LANGUAGE AllowAmbiguousTypes  #-}


module Mix
  ( mix
  ) where

import Control.Comonad.Cofree
import Control.Monad.State
import Control.Comonad
import Data.Either
-- import Data.Either.Combinators
-- import Data.Either.Validation
import Data.Validation
-- import qualified Data.Set as S

data Bowl = Bowl [Int] [Bowl] -- Cofree [] [Int]

{-
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
-}

{-
newtype Bowl a = Bowl (Either [a] a)

instance Semigroup a => Semigroup (Bowl a) where
  Bowl (Left xs1) <> Bowl (Left xs2) = Bowl (Left (xs1 <> xs2))
  Bowl (Left _) <> y = y
  x <> Bowl (Left _) = x
  Bowl (Right x1) <> Bowl (Right x2) = Bowl (Right (x1 <> x2))

instance Semigroup a => Monoid (Bowl a) where
  mempty = Bowl . Left $ []

lump :: Bowl a -> Bowl (Bowl a) -- ダマ
lump (Bowl (Left xs)) = Bowl . Left $ (Bowl . Right) <$> xs
lump (Bowl (Right x)) = Bowl . Right . Bowl . Right $ x

muddle :: Monoid a => Bowl a -> a -- ごちゃ混ぜ
muddle (Bowl (Left xs)) = mconcat xs
muddle (Bowl (Right x)) = x

mix :: Monoid a
    => (a -> a -> Bool)
    -> [a]
    -> [a]
-}

{-
instance Monoid a => Comonad Bowl | Bowl -> a where
  extract (Bowl (Left xs)) = mconcat xs
  extract (Bowl (Right x)) = x

  duplicate (Bowl (Left xs)) = Bowl . Left $ (Bowl . Right) <$> xs
  duplicate (Bowl (Right x)) = Bowl . Right . Bowl . Right $ x
-}
