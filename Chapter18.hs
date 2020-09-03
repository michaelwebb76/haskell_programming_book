{-# LANGUAGE FlexibleInstances #-}

module HaskellProgrammingChapter18 where

import Control.Monad (join, liftM2)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- The answer is the exercise

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

-- Short Exercise: Either Monad

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where

  pure = Second

  (<*>) (First a) _ = First a
  (<*>) _ (First a) = First a
  (<*>) (Second f) (Second a) = Second (f a)

instance Monad (Sum a) where

  return = pure

  (>>=) (First a) _ = First a
  (>>=) (Second a) f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, First <$> arbitrary), (2, Second <$> arbitrary)]

instance (EqProp a, EqProp b) => EqProp (Sum a b) where
  (=-=) (First a) (First b) = a =-= b
  (=-=) (Second a) (Second b) = a =-= b
  (=-=) _ _ = property False

mainEitherMonad :: IO ()
mainEitherMonad =
  hspec $ do
    it "is functor" $
      quickBatch
        ( functor
            (First (1, 2, 3) :: Sum (Int, Int, Int) (Int, Int, Int))
        )
    it "is applicative" $
      quickBatch
        ( applicative
            (First (1, 2, 3) :: Sum (Int, Int, Int) (Int, Int, Int))
        )
    it "is monad" $
      quickBatch
        ( monad
            (First (1, 2, 3) :: Sum (Int, Int, Int) (Int, Int, Int))
        )

-- Chapter Exercises

-- Nope

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where

  pure _ = NopeDotJpg

  (<*>) _ _ = NopeDotJpg

instance Monad Nope where

  return _ = NopeDotJpg

  (>>=) _ _ = NopeDotJpg

instance EqProp (Nope a) where
  (=-=) _ _ = property True

instance Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

-- PhhhbbtttEither

data PhhhbbtttEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft (f a)

instance Applicative (PhhhbbtttEither b) where

  pure = PLeft

  (<*>) (PRight b) _ = PRight b
  (<*>) _ (PRight b) = PRight b
  (<*>) (PLeft f) (PLeft a) = PLeft (f a)

instance Monad (PhhhbbtttEither b) where

  return = pure

  (>>=) (PRight b) _ = PRight b
  (>>=) (PLeft a) faToFB = faToFB a

instance (EqProp a, EqProp b) => EqProp (PhhhbbtttEither b a) where
  (=-=) (PRight b1) (PRight b2) = b1 =-= b2
  (=-=) (PLeft a1) (PLeft a2) = a1 =-= a2
  (=-=) _ _ = property False

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = frequency [(1, PRight <$> arbitrary), (1, PLeft <$> arbitrary)]

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where

  pure = Identity

  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where

  return = pure

  (>>=) (Identity a) faToMB = faToMB a

instance EqProp a => EqProp (Identity a) where
  (=-=) (Identity a) (Identity b) = a =-= b

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- List

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = fold (append . f) Nil

instance Applicative List where

  pure a = Cons a Nil

  (<*>) fs as =
    flatMap
      (\f -> fold (\a list -> Cons (f a) list) Nil as)
      fs

instance Eq a => EqProp (List a) where
  (=-=) Nil Nil = property True
  (=-=) (Cons a as) (Cons b bs) = property $ (a == b) .&. (as =-= bs)
  (=-=) _ _ = property False

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency
      [ ( 2,
          do
            a <- arbitrary
            Cons a <$> arbitrary
        ),
        (1, return Nil)
      ]

instance Monad List where

  return = pure

  (>>=) Nil _ = Nil
  (>>=) list faToMB = flatMap faToMB list

mainWriteMonads :: IO ()
mainWriteMonads =
  hspec $ do
    specify "Nope is monad" $
      quickBatch
        ( monad
            (NopeDotJpg :: Nope (Int, Int, Int))
        )
    specify "PhhhbbtttEither is monad" $
      quickBatch
        ( monad
            (PLeft (1, 2, 3) :: PhhhbbtttEither (Int, Int, Int) (Int, Int, Int))
        )
    specify "Identity is monad" $
      quickBatch
        ( monad
            (Identity (1, 2, 3) :: Identity (Int, Int, Int))
        )
    specify "List is monad" $
      quickBatch
        ( monad
            (Nil :: List (Int, Int, Int))
        )

-- Monad functions

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

apply :: Monad m => m a -> m (a -> b) -> m b
apply = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a : as) faToMB = faToMB a >>= (\b -> (:) b <$> meh as faToMB)

flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id

mainWriteMonadFunctions :: IO ()
mainWriteMonadFunctions =
  hspec $ do
    specify "j List" $
      j [[1, 2], [], [3]] `shouldBe` ([1, 2, 3] :: [Int])
    specify "j Maybe Just" $
      j (Just (Just 1)) `shouldBe` (Just 1 :: Maybe Int)
    specify "j Maybe Nothing" $
      j (Just Nothing) `shouldBe` (Nothing :: Maybe Int)
    specify "j Nothing" $
      j Nothing `shouldBe` (Nothing :: Maybe Int)
