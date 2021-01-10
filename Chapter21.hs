{-# LANGUAGE FlexibleInstances #-}

module Chapter21 where

import Control.Applicative (liftA, liftA2, liftA3)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = (Identity . f) a

instance Foldable Identity where
  foldMap f (Identity a) = f a

  foldr f z (Identity a) = f a z

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) (Identity a) (Identity b) = property $ (a == b)

mainIdentity = do
  let trigger :: Identity (Int, Int, [Int])
      trigger = Identity (1, 2, [3, 4, 5])
  (quickBatch . traversable) trigger

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ constant = Constant $ getConstant constant

instance Foldable (Constant a) where
  foldr f z constant = z

instance Traversable (Constant a) where
  traverse f constant = pure $ Constant (getConstant constant)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) constantA constantB = property $ (getConstant constantA == getConstant constantB)

mainConstant = do
  let trigger :: Constant (Int, Int, [Int]) (Int, Int, [Int])
      trigger = Constant (1, 2, [3, 4, 5])
  (quickBatch . traversable) trigger

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = (Yep . f) a

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep a) = f a z

instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    oneof
      [ (Yep <$> arbitrary),
        pure Nada
      ]

instance Eq a => EqProp (Optional a) where
  (=-=) Nada Nada = property True
  (=-=) (Yep a) (Yep b) = property $ a == b
  (=-=) _ _ = property False

mainOptional = do
  let trigger :: Optional (Int, Int, [Int])
      trigger = Yep (1, 2, [3, 4, 5])
  (quickBatch . traversable) trigger

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a list) = mappend (f a) (foldMap f list)

  foldr f z Nil = z
  foldr f z (Cons y ys) = f y (foldr f z ys)

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons a list) = liftA2 Cons (f a) (traverse f list)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    oneof
      [ (Cons <$> arbitrary <*> arbitrary),
        pure Nil
      ]

instance Eq a => EqProp (List a) where
  (=-=) Nil Nil = property True
  (=-=) (Cons a listA) (Cons b listB) = property $ (a == b) && (listA == listB)
  (=-=) _ _ = property False

mainList = do
  let trigger :: List (Int, Int, [Int])
      trigger = Cons (1, 2, [3, 4, 5]) Nil
  (quickBatch . traversable) trigger

data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) (Three a1 b1 c1) (Three a2 b2 c2) = property $ (a1 == a2) && (b1 == b2) && (c1 == c2)

mainThree = do
  let trigger :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
      trigger = Three (1, 2, [3, 4, 5]) (1, 2, [3, 4, 5]) (1, 2, [3, 4, 5])
  (quickBatch . traversable) trigger

data Pair a b = Pair a b deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldr f z (Pair a b) = f b z

instance Traversable (Pair a) where
  traverse f (Pair a b) = (Pair a) <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) (Pair a1 b1) (Pair a2 b2) = property $ (a1 == a2) && (b1 == b2)

mainPair = do
  let trigger :: Pair (Int, Int, [Int]) (Int, Int, [Int])
      trigger = Pair (1, 2, [3, 4, 5]) (1, 2, [3, 4, 5])
  (quickBatch . traversable) trigger

data Big a b = Big a b b deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
  foldr f z (Big a b c) = f b (f c z)

instance Traversable (Big a) where
  traverse f (Big a b c) = liftA2 (Big a) (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = liftA3 Big arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) (Big a1 b1 c1) (Big a2 b2 c2) = property $ (a1 == a2) && (b1 == b2) && (c1 == c2)

mainBig = do
  let trigger :: Big (Int, Int, [Int]) (Int, Int, [Int])
      trigger = Big (1, 2, [3, 4, 5]) (1, 2, [3, 4, 5]) (1, 2, [3, 4, 5])
  (quickBatch . traversable) trigger

data Bigger a b = Bigger a b b b deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
  foldr f z (Bigger a b c d) = f b (f c (f d z))

instance Traversable (Bigger a) where
  traverse f (Bigger a b c d) = liftA3 (Bigger a) (f b) (f c) (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) (Bigger a1 b1 c1 d1) (Bigger a2 b2 c2 d2) = property $ (a1 == a2) && (b1 == b2) && (c1 == c2) && (d1 == d2)

mainBigger = do
  let trigger :: Bigger (Int, Int, [Int]) (Int, Int, [Int])
      trigger = Bigger (1, 2, [3, 4, 5]) (1, 2, [3, 4, 5]) (1, 2, [3, 4, 5]) (1, 2, [3, 4, 5])
  (quickBatch . traversable) trigger

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance (Functor n, Foldable n) => Foldable (S n) where
  foldr f z (S na a) = foldr f (f a z) na

instance (Functor n, Traversable n) => Traversable (S n) where
  traverse f (S na a) = liftA2 S nb b
    where
      nb = sequenceA (f <$> na)
      b = (f a)

mainS = do
  let trigger :: S [] (Int, Int, [Int])
      trigger = S [(1, 2, [1, 2, 3]), (1, 2, [1, 2, 3])] (1, 2, [1, 2, 3])
  (quickBatch . traversable) trigger

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node tree1 a tree2) = Node (fmap f tree1) (f a) (fmap f tree2)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = mappend mempty (f a)
  foldMap f (Node tree1 a tree2) = mappend (foldMap f tree1) (mappend (f a) (foldMap f tree2))

  foldr _ z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Node tree1 a tree2) = foldr f (f a (foldr f z tree2)) tree1

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = liftA Leaf (f a)
  traverse f (Node tree1 a tree2) = liftA3 Node (traverse f tree1) (f a) (traverse f tree2)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    oneof
      [ (Leaf <$> arbitrary),
        (liftA3 Node arbitrary arbitrary arbitrary),
        pure Empty
      ]

instance Eq a => EqProp (Tree a) where
  (=-=) Empty Empty = property $ True
  (=-=) (Leaf a) (Leaf b) = property $ a == b
  (=-=) (Node treeA1 a treeA2) (Node treeB1 b treeB2) = property $ treeA1 == treeB1 && a == b && treeA2 == treeB2
  (=-=) _ _ = property $ False

mainTree = do
  let trigger :: Tree (Int, Int, [Int])
      trigger = Empty
  (quickBatch . traversable) trigger
