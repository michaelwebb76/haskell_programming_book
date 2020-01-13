{-# LANGUAGE FlexibleInstances #-}

module HaskellProgrammingChapter17 where

import Data.List (elemIndex)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Lookups

added :: Maybe Integer
added = (+ 3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6] :: [(Integer, Integer)])

addedY :: Maybe Integer
addedY = lookup 3 $ (zip [1, 2, 3] [4, 5, 6] :: [(Integer, Integer)])

addedZ :: Maybe Integer
addedZ = lookup 2 $ (zip [1, 2, 3] [4, 5, 6] :: [(Integer, Integer)])

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> addedY <*> addedZ

maxx :: Maybe Int
maxx = elemIndex 3 ([1, 2, 3, 4, 5] :: [Int])

maxy :: Maybe Int
maxy = elemIndex 4 ([1, 2, 3, 4, 5] :: [Int])

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> maxx <*> maxy

summedxs :: [Integer]
summedxs = [1, 2, 3]

summedys :: [Integer]
summedys = [4, 5, 6]

summedX :: Maybe Integer
summedX = lookup 3 $ zip summedxs summedys

summedY :: Maybe Integer
summedY = lookup 2 $ zip summedxs summedys

summed :: Maybe Integer
summed = (+) <$> summedX <*> summedY

-- Exercise: Identity instance

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where

  pure = Identity

  (<*>) (Identity f) (Identity a) = Identity (f a)

-- Exercise: Constant instance

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where

  pure _ = Constant mempty

  (<*>) (Constant f) (Constant a) = Constant (f <> a)

-- Exercise: Fixer upper

one :: Maybe String
one = const <$> Just "Hello" <*> Just "World"

two :: Maybe (Int, Int, String, [Int])
two = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- List Applicative Exercise

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

mainListApplicativeExercise :: IO ()
mainListApplicativeExercise = hspec $ do
  it "is implemented correctly" $ do
    let functions = Cons (+ 1) (Cons (* 2) Nil)
    let values = Cons 1 (Cons 2 Nil)
    functions <*> values `shouldBe` (Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil))) :: List Int)
  it "is flatmappable" $ do
    let toMyList = foldr Cons Nil
    let xs = toMyList [1, 2, 3]
    let c = Cons
    flatMap (\x -> x `c` (9 `c` Nil)) xs `shouldBe` (Cons 1 (Cons 9 (Cons 2 (Cons 9 (Cons 3 (Cons 9 Nil))))) :: List Int)
  it "is applicative" $ quickBatch (applicative (Nil :: List (Int, Int, Int)))

-- ZipList Applicative Exercise

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a as) = Cons a (take' (n - 1) as)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take' 3000 l
      ys' =
        let (ZipList' l) = ys
         in take' 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where

  pure a = ZipList' (pure a)

  (<*>) (ZipList' listf) (ZipList' lista) = ZipList' (listf <*> lista)

mainZipListApplicativeExercise :: IO ()
mainZipListApplicativeExercise =
  hspec $
    it
      "is applicative"
      ( quickBatch
          ( applicative
              (ZipList' Nil :: ZipList' (Int, Int, Int))
          )
      )

-- Exercises: Variations on Either Garden

data Validation e a = VFailure e | VSuccess a deriving (Eq, Show)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency [(1, VFailure <$> arbitrary), (1, VSuccess <$> arbitrary)]

instance (EqProp e, EqProp a) => EqProp (Validation e a) where
  (=-=) (VFailure a) (VFailure b) = property $ a =-= b
  (=-=) (VSuccess a) (VSuccess b) = property $ a =-= b
  (=-=) _ _ = property False

instance Functor (Validation e) where
  fmap _ (VFailure e) = VFailure e
  fmap f (VSuccess a) = VSuccess (f a)

instance Monoid e => Applicative (Validation e) where

  pure = VSuccess

  (<*>) (VFailure a) (VFailure b) = VFailure (a <> b)
  (<*>) (VFailure a) _ = VFailure a
  (<*>) _ (VFailure a) = VFailure a
  (<*>) (VSuccess f) (VSuccess a) = VSuccess (f a)

mainValidationApplicativeExercise :: IO ()
mainValidationApplicativeExercise =
  hspec $
    it
      "is applicative"
      ( quickBatch
          ( applicative
              (VSuccess (1, 2, 3) :: Validation String (Int, Int, Int))
          )
      )

-- Chapter Exercises

data Pair a = Pair a a deriving (Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    firstA <- arbitrary
    Pair firstA <$> arbitrary

instance EqProp a => EqProp (Pair a) where
  (=-=) (Pair a b) (Pair c d) = (a =-= c) .&. (b =-= d)

instance Applicative Pair where

  pure a = Pair a a

  (<*>) (Pair f g) (Pair a b) = Pair (f a) (g b)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    firstA <- arbitrary
    Two firstA <$> arbitrary

instance (EqProp a, EqProp b) => EqProp (Two a b) where
  (=-=) (Two a b) (Two c d) = (a =-= c) .&. (b =-= d)

instance Monoid a => Applicative (Two a) where

  pure = Two mempty

  (<*>) (Two fa fb) (Two a b) = Two (fa <> a) (fb b)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    firstA <- arbitrary
    firstB <- arbitrary
    Three firstA firstB <$> arbitrary

instance (EqProp a, EqProp b, EqProp c) => EqProp (Three a b c) where
  (=-=) (Three a b c) (Three d e f) = (a =-= d) .&. (b =-= e) .&. (c =-= f)

instance (Monoid a, Monoid b) => Applicative (Three a b) where

  pure = Three mempty mempty

  (<*>) (Three fa fb fc) (Three a b c) = Three (fa <> a) (fb <> b) (fc c)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    firstA <- arbitrary
    firstB <- arbitrary
    Three' firstA firstB <$> arbitrary

instance (EqProp a, EqProp b) => EqProp (Three' a b) where
  (=-=) (Three' a b c) (Three' d e f) = (a =-= d) .&. (b =-= e) .&. (c =-= f)

instance Monoid a => Applicative (Three' a) where

  pure b = Three' mempty b b

  (<*>) (Three' fa fb fc) (Three' a b c) = Three' (fa <> a) (fb b) (fc c)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    firstA <- arbitrary
    firstB <- arbitrary
    firstC <- arbitrary
    Four firstA firstB firstC <$> arbitrary

instance (EqProp a, EqProp b, EqProp c, EqProp d) => EqProp (Four a b c d) where
  (=-=) (Four a b c d) (Four e f g h) = (a =-= e) .&. (b =-= f) .&. (c =-= g) .&. (d =-= h)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where

  pure = Four mempty mempty mempty

  (<*>) (Four fa fb fc fd) (Four a b c d) = Four (fa <> a) (fb <> b) (fc <> c) (fd d)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    firstA <- arbitrary
    firstB <- arbitrary
    firstC <- arbitrary
    Four' firstA firstB firstC <$> arbitrary

instance (EqProp a, EqProp b) => EqProp (Four' a b) where
  (=-=) (Four' a b c d) (Four' e f g h) = (a =-= e) .&. (b =-= f) .&. (c =-= g) .&. (d =-= h)

instance Monoid a => Applicative (Four' a) where

  pure = Four' mempty mempty mempty

  (<*>) (Four' fa fb fc fd) (Four' a b c d) = Four' (fa <> a) (fb <> b) (fc <> c) (fd d)

mainApplicativeInstances :: IO ()
mainApplicativeInstances =
  hspec $ do
    specify
      "Pair is applicative"
      ( quickBatch
          ( applicative
              (Pair (1, 2, 3) (1, 2, 3) :: Pair (Int, Int, Int))
          )
      )
    specify
      "Two is applicative"
      ( quickBatch
          ( applicative
              (Two ("a", "b", "c") ("a", "b", "c") :: Two (String, String, String) (String, String, String))
          )
      )
    specify
      "Three is applicative"
      ( quickBatch
          ( applicative
              (Three ("a", "b", "c") ("a", "b", "c") ("a", "b", "c") :: Three (String, String, String) (String, String, String) (String, String, String))
          )
      )
    specify
      "Three' is applicative"
      ( quickBatch
          ( applicative
              (Three' ("a", "b", "c") ("a", "b", "c") ("a", "b", "c") :: Three' (String, String, String) (String, String, String))
          )
      )
    specify
      "Four is applicative"
      ( quickBatch
          ( applicative
              (Four ("a", "b", "c") ("a", "b", "c") ("a", "b", "c") ("a", "b", "c") :: Four (String, String, String) (String, String, String) (String, String, String) (String, String, String))
          )
      )
    specify
      "Four' is applicative"
      ( quickBatch
          ( applicative
              (Four' ("a", "b", "c") ("a", "b", "c") ("a", "b", "c") ("a", "b", "c") :: Four' (String, String, String) (String, String, String))
          )
      )

-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos as bs cs = [(,,)] <*> as <*> bs <*> cs

mainCombinations :: IO ()
mainCombinations =
  hspec
    ( it
        "generates the right number of combinations"
        ( length (combos stops vowels stops) `shouldBe` (6 * 5 * 6)
        )
    )
