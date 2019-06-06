{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE LambdaCase       #-}



module Mix where

import           Control.Comonad.Cofree
import           Control.Monad.Free
import           Control.Monad.State
import           Control.Comonad
import           Data.Either
-- import Data.Either.Combinators
-- import Data.Either.Validation
import           Data.Validation
import qualified Data.Set as S
import           Control.Monad.Zip
import           Data.Foldable
import           Data.List
import           Data.Monoid



-- type Flour = S.Set Int
-- type Bowl = Cofree [] Flour

-- バグあり
mix' :: (Int -> Int -> Bool)
     -> [Int]
     -> [[Int]]
mix' _ [] = []
mix' _ [x] = [[x]]
mix' f xs = f2 xs >>= toList
  where
    f1 :: Int
       -> [Cofree [] [Int]]
       -> [Cofree [] [Int]]
    f1 x ys =
      case partition (or . fmap (f x) . extract) ys of --　バグ要修正
        (ys1, ys2) -> -- ys1, ys2 :: [Cofree [] [Int]]
          let
            zs = f11 <$> ys1 :: [Cofree [] (Either [Int] [Int])]
          in
            ((x : (zs >>= f12)) :< (zs >>= f13)) : ys2
      where
        f11 :: Cofree [] [Int]
            -> Cofree [] (Either [Int] [Int])
        f11 y =
          mzipWith
            (\b x -> if b then Right x else Left x)
            (or <$> (duplicate $ (or . fmap (`f` x)) <$> y))
            y -- y :: Cofree [] [Int]
        
        -- True: 結合
        f12 :: Cofree [] (Either [Int] [Int])
            -> [Int]
        f12 x =
          fold $ (\case { Left _ -> []; Right y -> y }) <$> x
        
        -- False: 不結合
        f13 :: Cofree [] (Either [Int] [Int])
            -> [Cofree [] [Int]]
        f13 (Left x :< xs) = [x :< (xs >>= f13)]
        f13 (Right _ :< xs) = xs >>= f13
    
    f2 :: [Int]
       -> [Cofree [] [Int]]
    f2 [] = []
    f2 [x] = [[x] :< []]
    f2 (x:xs) = f1 x . f2 $ xs

mix :: (Int -> Int -> Bool)
    -> [Int]
    -> [[Int]]
mix f = undefined
  where
    g :: Int
      -> [Free (Cofree []) Int]
      -> [Free (Cofree []) Int]
    g x [] = [Pure x]
    g x (y:ys) =
      case y of
        Pure x'
          | x `f` x' && x' `f` x -> Free $ (Free $ Pure x :< [Pure x']) :< [Pure x']
          | x `f` x'             -> Free $ Pure x :< [Pure x']
          | x' `f` x             -> Free $ (Free $ Pure x :< [Pure x']) :< []
    
    to :: Int
       -> [Free (Cofree []) Int]
       -> [Free (Cofree []) Int]
    to x [] = []
    to x (y:ys) = undefined

    from :: Int
         -> [Free (Cofree []) Int]
         -> [Free (Cofree []) Int]
    from = undefined

    rotate :: Int
           -> Free (Cofree []) Int
           -> Maybe (Free (Cofree []) Int)
    rotate = undefined

{-
mix :: (Int -> Int -> Maybe Bool)
    -> [Int]
    -> [[[Int]]]
mix _ [] = []
mix _ [x] = [[[x]]]
mix f xs = f3 <$> (f2 xs >>= toList) -- foldr _ []  -- f3 f xs >>= foldr (:) []
  where
    f1 :: Int
       -> [Cofree [] (Cofree [] Int)]
       -> [Cofree [] (Cofree [] Int)]
    f1 x ys =
      case partition (or . _ . fmap (f x) . extract) ys of
        (ys1, ys2) -> -- ys1, ys2 :: [Cofree [] [Int]]
          let
            zs = f11 <$> ys1 :: [Cofree [] (Either (Cofree [] Int) (Cofree [] Int))]
          in
            ((x : (zs >>= f12)) :< (zs >>= f13)) : ys2　-- 要修正?
      where
        f11 :: Cofree [] (Cofree [] Int)
            -> Cofree [] (Either (Cofree [] Int) (Cofree [] Int))
        f11 y =
          mzipWith　(\b x -> if b then Right x else Left x) (or <$> (duplicate $ (or . fmap (`f` x)) <$> y)) y -- y :: Cofree [] [Int]
          where
            f112 = undefined -- ここから

        -- True: 結合
        f12 :: Cofree [] (Either (Cofree [] Int) (Cofree [] Int))
            -> (Cofree [] Int)
        f12 x =
          fold $ (\case { Left _ -> []; Right y -> y }) <$> x
        
        -- False: 不結合
        f13 :: Cofree [] (Either (Cofree [] Int) (Cofree [] Int))
            -> [Cofree [] (Cofree [] Int)]
        f13 (Left x :< xs) = [x :< (xs >>= f13)]
        f13 (Right _ :< xs) = xs >>= f13

    f2 :: [Int]
       -> [Cofree [] (Cofree [] Int)]
    f2 [] = []
    f2 [x] = [(x :< []) :< []]
    f2 (x:xs) = f1 x . f2 $ xs

    -- 分岐で分割
    f3 :: Cofree [] Int -> [[Int]]
    f3 (x :< []) = [[x]]
    f3 (x :< [y]) =
      case f3 y of
        xs : xss ->
          (x : xs) : xss
    f3 (x :< ys) = [x] : (((x:<) . (:[]) <$> ys) >>= f3)
-}

{-
    f1 :: Int
       -> Cofree [] Int
       -> 
    f1 x xss = undefined
      where
        f1 :: Int
           -> [Int]
           -> Maybe Bool
        -- f1 _ [] = Nothing
        f1 x xs = (getAny <$>) . fold $ fmap Any . (x `f`) <$> xs -- (x `f`) <$> xs :: [Maybe Bool] -- (getAny <$>) $ (foldr (<>) Nothing $ (Any <$>) . g x1 <$> xs2) <> (Any <$> f1 g xs1 xs2)

    f2 :: Int
       -> [Cofree [] [[Int]]]
       -> [Cofree [] [[Int]]]
    f2 x ys =
      case partition (f1 x . extract) ys of
        (ys1, ys2) -> -- ys1, ys2 :: [Cofree [] [Int]]
          let
            zs = f21 <$> ys1 :: [Cofree [] (Either [Int] [Int])]
          in
            ((x : (zs >>= f22)) :< (zs >>= f23)) : ys2
      where
        f21 :: Cofree [] [Int]
            -> Cofree [] (Either [Int] [Int])
        f21 y =
          mzipWith　(\b x -> if b then Right x else Left x) (or <$> (duplicate $ (or . fmap (`f` x)) <$> y)) y -- y :: Cofree [] [Int]
        
        -- True: 結合
        f22 :: Cofree [] (Either [Int] [Int])
            -> [Int]
        f22 x =
          fold $ (\case { Left _ -> []; Right y -> y }) <$> x
        
        -- False: 不結合
        f23 :: Cofree [] (Either [Int] [Int])
            -> [Cofree [] [Int]]
        f23 (Left x :< xs) = [x :< (xs >>= f23)]
        f23 (Right _ :< xs) = xs >>= f23
-}
{-
    f3 :: [Int]
       -> [Cofree [] [Int]]
    f3 [] = []
    f3 [x] = [[x] :< []]
    f3 (x:xs) = f2 x . f3 $ xs

-}


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

{-
mix :: (Int -> Int -> Bool)
    -> S.Set Int
    -> [S.Set Int]
mix f xs
  | S.null xs = []
  | S.size xs == 1 = [xs]
  | otherwise = f3 f xs >>= foldr (:) []
  where
    f1 :: (Int -> Int -> Bool)
       -> S.Set Int
       -> S.Set Int
       -> Bool
    f1 _ (S.null -> True) _ =
      False
    f1 g (S.deleteFindMin -> (x1, xs1)) xs2 =
      (foldr (||) False . S.map (g x1) $ xs2) || f1 g xs1 xs2
    
    f2 :: (Int -> Int -> Bool)
       -> S.Set Int
       -> [Cofree [] (S.Set Int)]
       -> [Cofree [] (S.Set Int)]
    f2 g xs ys =
      case partition (f1 g xs . extract) ys of
        (fmap duplicate -> ys1, ys2) -> -- ys1 :: [Cofree [] (Cofree [] (S.Set Int))], ys2 :: [Cofree [] (S.Set Int)]
          let
            zs = (f21 xs <$>) <$> ys1 :: [Cofree [] (Either (Cofree [] (S.Set Int)) (S.Set Int))]
          in
            (foldr (<>) xs (f22 <$> zs) :< (zs >>= f23)) : ys2 -- (f21 xs <$>) <$> ys1 -- f21 xs <$> ys1 :: [Cofree [] (Either (Cofree [] (S.Set Int)) (S.Set Int))]
      where
        f21 :: S.Set Int
            -> Cofree [] (S.Set Int)
            -> Either (Cofree [] (S.Set Int)) (S.Set Int)
        f21 xs ys
          | f1 g (extract ys) xs || (not . null . rights $ f21 xs <$> unwrap ys) =
            Right . extract $ ys
          | otherwise =
            Left ys
        
        f22 :: Cofree [] (Either (Cofree [] (S.Set Int)) (S.Set Int))
            -> S.Set Int
        f22 x = foldMap id $ f223 <$> x
          where
            f223 :: Either (Cofree [] (S.Set Int)) (S.Set Int)
                 -> S.Set Int
            f223 (Left _) = S.empty
            f223 (Right y) = y

        f23 :: Cofree [] (Either (Cofree [] (S.Set Int)) (S.Set Int))
            -> [Cofree [] (S.Set Int)]
        f23 (Left x :< _) = [x]
        f23 (Right _ :< xs) = xs >>= f23
    
    f3 :: (Int -> Int -> Bool)
       -> S.Set Int
       -> [Cofree [] (S.Set Int)]
    f3 _ (S.null -> True) = []
    f3 _ x@(S.size -> 1) = [x :< []]
    f3 f (S.splitAt 1 -> (x, xs)) = f2 f x . f3 f $ xs
-}

{-
mix :: (Int -> Int -> Bool)
    -> [Int]
    -> [[Int]]
mix _ [] = []
mix _ [x] = [[x]]
mix f xs = f3 f xs >>= foldr (:) []
  where
    f1 :: (Int -> Int -> Bool)
       -> [Int]
       -> [Int]
       -> Bool
    f1 _ [] _ =
      False
    f1 g (x1:xs1) xs2 =
      (foldr (||) False $ g x1 <$> xs2) || f1 g xs1 xs2

    f2 :: (Int -> Int -> Bool)
       -> [Int]
       -> [Cofree [] [Int]]
       -> [Cofree [] [Int]]
    f2 g xs ys =
      case partition (f1 g xs . extract) ys of
        (fmap duplicate -> ys1, ys2) -> -- ys1 :: [Cofree [] (Cofree [] (S.Set Int))], ys2 :: [Cofree [] (S.Set Int)]
          let
            zs = (f21 xs <$>) <$> ys1 :: [Cofree [] (Either (Cofree [] [Int]) [Int])]
          in
            (foldr (<>) xs (f22 <$> zs) :< (zs >>= f23)) : ys2 -- (f21 xs <$>) <$> ys1 -- f21 xs <$> ys1 :: [Cofree [] (Either (Cofree [] (S.Set Int)) (S.Set Int))]
      where
        f21 :: [Int]
            -> Cofree [] [Int]
            -> Either (Cofree [] [Int]) [Int]
        f21 xs ys
          | f1 g (extract ys) xs || (not . null . rights $ f21 xs <$> unwrap ys) =
            Right . extract $ ys
          | otherwise =
            Left ys

        f22 :: Cofree [] (Either (Cofree [] [Int]) [Int])
            -> [Int]
        f22 x = foldMap id $ f223 <$> x
          where
            f223 :: Either (Cofree [] [Int]) [Int]
                 -> [Int]
            f223 (Left _) = []
            f223 (Right y) = y

        f23 :: Cofree [] (Either (Cofree [] [Int]) [Int])
            -> [Cofree [] [Int]]
        f23 (Left x :< _) = [x]
        f23 (Right _ :< xs) = xs >>= f23

    f3 :: (Int -> Int -> Bool)
       -> [Int]
       -> [Cofree [] [Int]]
    f3 _ [] = []
    f3 _ [x] = [[x] :< []]
    f3 f (x:xs) = f2 f [x] . f3 f $ xs
-}

{-
mix' :: (Int -> Int -> Bool)
     -> [Int]
     -> [[Int]]
mix' _ [] = []
mix' _ [x] = [[x]]
mix' f xs = f3 xs >>= foldr (:) []
  where
    f1 :: [Int]
       -> [Int]
       -> Bool
    f1 [] _ = False
    f1 (x1:xs1) xs2 = (foldr (||) False $ f x1 <$> xs2) || f1 xs1 xs2
    
    f2 :: [Int]
       -> [Cofree [] [Int]]
       -> [Cofree [] [Int]]
    f2 xs ys =
      case partition (f1 xs . extract) ys of
        (ys1, ys2) -> -- ys1, ys2 :: [Cofree [] [Int]]
          let
            zs = f21 <$> ys1 :: [Cofree [] (Either [Int] [Int])]
          in
            ((xs <> (zs >>= f22)) :< (zs >>= f23)) : ys2
      where
        f21 :: Cofree [] [Int]
            -> Cofree [] (Either [Int] [Int])
        f21 y =
          mzipWith　(\b x -> if b then Right x else Left x) (or <$> (duplicate $ (`f1` xs) <$> y)) y
        
        -- True: 結合
        f22 :: Cofree [] (Either [Int] [Int])
            -> [Int]
        f22 x =
          fold $ (\case { Left _ -> []; Right y -> y }) <$> x
        
        -- False: 不結合
        f23 :: Cofree [] (Either [Int] [Int])
            -> [Cofree [] [Int]]
        f23 (Left x :< xs) = [x :< (xs >>= f23)]
        f23 (Right _ :< xs) = xs >>= f23
    
    f3 :: [Int]
       -> [Cofree [] [Int]]
    f3 [] = []
    f3 [x] = [[x] :< []]
    f3 (x:xs) = f2 [x] . f3 $ xs
-}