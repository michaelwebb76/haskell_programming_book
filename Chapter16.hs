{-# LANGUAGE FlexibleInstances #-}

module HaskellProgrammingChapter16 where

import Test.Hspec
import Test.QuickCheck

-- Exercises: Heavy Lifting

mainHeavyLifting :: IO ()
mainHeavyLifting = hspec $ do
  it "solves 1." $ ((+ 1) <$> read "[1]" :: [Int]) `shouldBe` ([2] :: [Int])
  it "solves 2." $ (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"]) `shouldBe` Just ["Hi,lol", "Hellolol"]
  it "solves 3." $ ((* 2) <$> (\x -> x - 2)) 1 `shouldBe` (-2 :: Int)
  it "solves 4." $ return '1' ++ show ((\x -> [x, 1 .. 3]) (0 :: Int)) `shouldBe` "1[0,1,2,3]"
  it "solves 5." $ e >>= (`shouldBe` 3693)

e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = read . ("123" ++) . show <$> ioi
   in (* 3) <$> changed

-- Exercises: Instances of Func

-- Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityIdentity = Identity String -> Bool

type IdentityCompose = (String -> String) -> (String -> Int) -> Identity String -> Bool

-- Pair

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    firstA <- arbitrary
    Pair firstA <$> arbitrary

type PairIdentity = Pair String -> Bool

type PairCompose = (String -> String) -> (String -> Int) -> Pair String -> Bool

-- Two

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    firstA <- arbitrary
    Two firstA <$> arbitrary

type TwoIdentity = Two String String -> Bool

type TwoCompose = (String -> String) -> (String -> Int) -> Two String String -> Bool

-- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    firstA <- arbitrary
    firstB <- arbitrary
    Three firstA firstB <$> arbitrary

type ThreeIdentity = Three String String String -> Bool

type ThreeCompose = (String -> String) -> (String -> Int) -> Three String String String -> Bool

-- Three'

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    firstA <- arbitrary
    firstB <- arbitrary
    Three' firstA firstB <$> arbitrary

type ThreeIdentity' = Three' String String -> Bool

type ThreeCompose' = (String -> String) -> (String -> Int) -> Three' String String -> Bool

-- Four

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    firstA <- arbitrary
    firstB <- arbitrary
    firstC <- arbitrary
    Four firstA firstB firstC <$> arbitrary

type FourIdentity = Four String String String String -> Bool

type FourCompose = (String -> String) -> (String -> Int) -> Four String String String String -> Bool

-- Four'

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    firstA <- arbitrary
    firstB <- arbitrary
    firstC <- arbitrary
    Four' firstA firstB firstC <$> arbitrary

type FourIdentity' = Four' String String -> Bool

type FourCompose' = (String -> String) -> (String -> Int) -> Four' String String -> Bool

-- Trivial cannot have Functor, it is not of kind * -> *

instance Show (a -> b) where
  show _ = "<function>"

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

mainInstancesOfFunc :: IO ()
mainInstancesOfFunc = hspec $ do
  it "Identity satisfies law of identity" $ quickCheck (functorIdentity :: IdentityIdentity)
  it "Identity satisfies law of composition" $ quickCheck (functorCompose :: IdentityCompose)
  it "Pair satisfies law of identity" $ quickCheck (functorIdentity :: PairIdentity)
  it "Pair satisfies law of composition" $ quickCheck (functorCompose :: PairCompose)
  it "Two satisfies law of identity" $ quickCheck (functorIdentity :: TwoIdentity)
  it "Two satisfies law of composition" $ quickCheck (functorCompose :: TwoCompose)
  it "Three satisfies law of identity" $ quickCheck (functorIdentity :: ThreeIdentity)
  it "Three satisfies law of composition" $ quickCheck (functorCompose :: ThreeCompose)
  it "Three' satisfies law of identity" $ quickCheck (functorIdentity :: ThreeIdentity')
  it "Three' satisfies law of composition" $ quickCheck (functorCompose :: ThreeCompose')
  it "Four satisfies law of identity" $ quickCheck (functorIdentity :: FourIdentity)
  it "Four satisfies law of composition" $ quickCheck (functorCompose :: FourCompose)
  it "Four' satisfies law of identity" $ quickCheck (functorIdentity :: FourIdentity')
  it "Four' satisfies law of composition" $ quickCheck (functorCompose :: FourCompose')

-- Exercise: Possibly

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = frequency [(1, return LolNope), (1, Yeppers <$> arbitrary)]

type PossiblyIdentity = Possibly String -> Bool

type PossiblyCompose = (String -> String) -> (String -> Int) -> Possibly String -> Bool

mainPossibly :: IO ()
mainPossibly = hspec $ do
  it "Possibly satisfies law of identity" $ quickCheck (functorIdentity :: PossiblyIdentity)
  it "Possibly satisfies law of composition" $ quickCheck (functorCompose :: PossiblyCompose)

-- Exercise: Sum

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second a) = Second (f a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, First <$> arbitrary), (1, Second <$> arbitrary)]

type SumIdentity = Sum String String -> Bool

type SumCompose = (String -> String) -> (String -> Int) -> Sum String String -> Bool

mainSum :: IO ()
mainSum = hspec $ do
  it "Sum satisfies law of identity" $ quickCheck (functorIdentity :: SumIdentity)
  it "Sum satisfies law of composition" $ quickCheck (functorCompose :: SumCompose)

-- Chapter exercises

-- Valid functor exists?
-- 1. No
-- 2. Yes
-- 3. Yes
-- 4. No
-- 5. No

-- Rearrange
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write functor

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

newtype K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap fn (Flip (K a)) = Flip (K (fn a))

newtype EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

newtype LiftItOut f a = LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ f <$> fa

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g a)

instance Functor (IgnoreOne f g a) where
  fmap _ (IgnoringSomething fa ga) = IgnoringSomething fa ga

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (f <$> as)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gla glb glc) = MoreGoats (f <$> gla) (f <$> glb) (f <$> glc)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print string something) = Print string (f something)
  fmap f stringToA = f <$> stringToA
