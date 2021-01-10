{-# LANGUAGE FlexibleInstances #-}

module Chapter20 where

import Data.Foldable
import Data.Monoid (Sum)
import Protolude
import Test.Hspec

-- Exercises: Library functions

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . Data.Foldable.foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . Data.Foldable.foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\y alreadyFound -> alreadyFound || y == x) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum =
  foldr (\x maybeMinimum -> (min x) <$> (Just $ fromMaybe x maybeMinimum)) Nothing

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum =
  foldr (\x maybeMaximum -> (max x) <$> (Just $ fromMaybe x maybeMaximum)) Nothing

null :: (Foldable t) => t a -> Bool
null = foldr (const (&& False)) True

length :: (Foldable t) => t a -> Int
length = foldr (const (+ 1)) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = Data.Foldable.foldMap (\a -> mappend mempty a)

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (mappend . f) mempty

mainLibraryFunctions :: IO ()
mainLibraryFunctions =
  hspec $ do
    specify "sum" $
      HaskellProgrammingChapter20.sum [1, 2, 3, 4, 5] `shouldBe` (15 :: Int)
    specify "product" $
      HaskellProgrammingChapter20.product [1, 2, 3, 4, 5] `shouldBe` (120 :: Int)
    specify "elem true" $
      HaskellProgrammingChapter20.elem 2 [1, 2, 3, 4, 5] `shouldBe` True
    specify "elem false" $
      HaskellProgrammingChapter20.elem 6 [1, 2, 3, 4, 5] `shouldBe` False
    specify "minimum Int" $
      HaskellProgrammingChapter20.minimum [3, 2, 3, 4, 1, 5] `shouldBe` (Just 1)
    specify "minimum List Maybe result" $
      HaskellProgrammingChapter20.minimum ([Nothing, Just 3, Just 2, Just 5] :: [Maybe Int]) `shouldBe` Just Nothing
    specify "minimum List Maybe no result" $
      HaskellProgrammingChapter20.minimum ([Nothing, Nothing, Nothing] :: [Maybe Int]) `shouldBe` Just Nothing
    specify "minimum empty List" $
      HaskellProgrammingChapter20.minimum ([] :: [Maybe Int]) `shouldBe` Nothing
    specify "maximum Int" $
      HaskellProgrammingChapter20.maximum [3, 2, 3, 4, 1, 5] `shouldBe` (Just 5)
    specify "maximum List Maybe result" $
      HaskellProgrammingChapter20.maximum ([Nothing, Just 3, Just 2, Just 5] :: [Maybe Int]) `shouldBe` Just (Just 5)
    specify "maximum List Maybe no result" $
      HaskellProgrammingChapter20.maximum ([Nothing, Nothing, Nothing] :: [Maybe Int]) `shouldBe` Just Nothing
    specify "maximum empty List" $
      HaskellProgrammingChapter20.maximum ([] :: [Maybe Int]) `shouldBe` Nothing
    specify "null empty list" $
      HaskellProgrammingChapter20.null ([] :: [Int]) `shouldBe` True
    specify "null non-empty list" $
      HaskellProgrammingChapter20.null ([1, 2, 34] :: [Int]) `shouldBe` False
    specify "length empty list" $
      HaskellProgrammingChapter20.length ([] :: [Int]) `shouldBe` 0
    specify "length non-empty list" $
      HaskellProgrammingChapter20.length ([1, 2, 34] :: [Int]) `shouldBe` 3
    specify "toList empty list" $
      HaskellProgrammingChapter20.toList ([] :: [Int]) `shouldBe` []
    specify "toList non-empty list" $
      HaskellProgrammingChapter20.toList ([1, 2, 34] :: [Int]) `shouldBe` [1, 2, 34]
    specify "fold empty list Sum" $
      HaskellProgrammingChapter20.fold ([] :: [Sum Int]) `shouldBe` 0
    specify "fold non-empty list Sum" $
      HaskellProgrammingChapter20.fold ([1, 2, 34] :: [Sum Int]) `shouldBe` 37
    specify "fold empty list Product" $
      HaskellProgrammingChapter20.fold ([] :: [Product Int]) `shouldBe` 1
    specify "fold non-empty list Product" $
      HaskellProgrammingChapter20.fold ([1, 2, 34] :: [Product Int]) `shouldBe` 68
    specify "foldMap empty list Product" $
      HaskellProgrammingChapter20.foldMap Product ([] :: [Int]) `shouldBe` 1
    specify "foldMap non-empty list Product" $
      HaskellProgrammingChapter20.foldMap Product ([1, 2, 34] :: [Int]) `shouldBe` 68

-- Chapter Exercises

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

data Two a b
  = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

data Three a b c
  = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

data Three' a b
  = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b c) = mappend (f b) (f c)

data Four' a b
  = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b c d) = mappend (mappend (f b) (f c)) (f d)

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF matchFunction =
  Data.Foldable.foldMap
    ( \a -> if matchFunction a then pure a else mempty
    )

mainChapterExercises :: IO ()
mainChapterExercises =
  hspec $ do
    specify "Constant a b foldMap Sum" $
      Data.Foldable.foldMap Sum (Constant (4 :: Int)) `shouldBe` (Sum 4)
    specify "Constant a b foldMap Product" $
      Data.Foldable.foldMap Product (Constant (6 :: Int)) `shouldBe` (Product 6)
    specify "Two a b foldMap Sum" $
      Data.Foldable.foldMap Sum (Two (4 :: Int) (4 :: Int)) `shouldBe` (Sum 4)
    specify "Two a b foldMap Product" $
      Data.Foldable.foldMap Product (Two (6 :: Int) (4 :: Int)) `shouldBe` (Product 4)
    specify "Three a b c foldMap Sum" $
      Data.Foldable.foldMap Sum (Three (4 :: Int) (4 :: Int) (4 :: Int)) `shouldBe` (Sum 4)
    specify "Three a b c foldMap Product" $
      Data.Foldable.foldMap Product (Three (6 :: Int) (4 :: Int) (4 :: Int)) `shouldBe` (Product 4)
    specify "Three' a b c foldMap Sum" $
      Data.Foldable.foldMap Sum (Three' (4 :: Int) (4 :: Int) (4 :: Int)) `shouldBe` (Sum 8)
    specify "Three' a b c foldMap Product" $
      Data.Foldable.foldMap Product (Three' (6 :: Int) (4 :: Int) (4 :: Int)) `shouldBe` (Product 16)
    specify "Four' a b c d foldMap Sum" $
      Data.Foldable.foldMap Sum (Four' (4 :: Int) (4 :: Int) (4 :: Int) (4 :: Int)) `shouldBe` (Sum 12)
    specify "Four' a b c d foldMap Product" $
      Data.Foldable.foldMap Product (Four' (6 :: Int) (4 :: Int) (4 :: Int) (4 :: Int)) `shouldBe` (Product 64)
    specify "filterF" $
      HaskellProgrammingChapter20.filterF even [1, 2, 3, 4, 5, 6] `shouldBe` [2, 4, 6]
