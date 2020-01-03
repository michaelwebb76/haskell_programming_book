module HaskellProgrammingChapter15 where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck

-- Exercise: Optional Monoid
data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  (Only a) <> (Only b) = Only (a <> b)
  (Only a) <> Nada = Only a
  Nada <> (Only a) = Only a
  Nada <> Nada = Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (1, (Only <$> arbitrary))]

-- Exercise: Madness
type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said "
    <> adv
    <> " as he jumped into his car "
    <> noun
    <> " and drove off with his "
    <> adj
    <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat
    [ e,
      "! he said ",
      adv,
      " as he jumped into his car ",
      noun,
      " and drove off with his ",
      adj,
      " wife."
    ]

-- Exercise: Maybe Another Monoid

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Semigroup a => Monoid (First' a) where
  mempty = First' Nada

instance Semigroup a => Semigroup (First' a) where
  (<>) (First' a) (First' b) = First' (a <> b)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary

firstMappend :: Semigroup a => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

mainMaybeAnotherMonoid :: IO ()
mainMaybeAnotherMonoid = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

-- Chapter Exercises

-- Semigroup exercises

-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

trivialAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
trivialAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

mainTrivial :: IO ()
mainTrivial = quickCheck (trivialAssoc :: TrivialAssoc)

-- Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

identityAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
identityAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

mainIdentity :: IO ()
mainIdentity = quickCheck (identityAssoc :: IdentityAssoc String)

-- Two

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    genA <- arbitrary
    genB <- arbitrary
    pure $ Two genA genB

twoAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
twoAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

mainTwo :: IO ()
mainTwo = quickCheck (twoAssoc :: TwoAssoc String String)

-- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three a b c) (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    genA <- arbitrary
    genB <- arbitrary
    genC <- arbitrary
    pure $ Three genA genB genC

threeAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
threeAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

mainThree :: IO ()
mainThree = quickCheck (threeAssoc :: ThreeAssoc String String String)

-- Four

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    genA <- arbitrary
    genB <- arbitrary
    genC <- arbitrary
    genD <- arbitrary
    pure $ Four genA genB genC genD

fourAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
fourAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

mainFour :: IO ()
mainFour = quickCheck (fourAssoc :: FourAssoc String String String String)

-- BoolConj

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj a) (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

boolConjAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
boolConjAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

mainBoolConj :: IO ()
mainBoolConj = quickCheck (boolConjAssoc :: BoolConjAssoc)

-- BoolDisj

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj a) (BoolDisj b) = BoolDisj (a || b)

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

boolDisjAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
boolDisjAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

mainBoolDisj :: IO ()
mainBoolDisj = quickCheck (boolDisjAssoc :: BoolDisjAssoc)

-- Or

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Snd x) _ = Snd x
  (<>) _ (Snd x) = Snd x
  (<>) _ (Fst x) = Fst x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, Fst <$> arbitrary), (1, Snd <$> arbitrary)]

orAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
orAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

mainOr :: IO ()
mainOr = hspec $ do
  describe "Or" $ do
    it "(Fst 1 <> Snd 2) == Snd 2" $ do
      ((Fst 1 <> Snd 2) :: Or Int Int) `shouldBe` (Snd 2 :: Or Int Int)
    it "(Fst 1 <> Fst 2) == Fst 2" $ do
      ((Fst 1 <> Fst 2) :: Or Int Int) `shouldBe` (Fst 2 :: Or Int Int)
    it "(Snd 1 <> Fst 2) == Snd 1" $ do
      ((Snd 1 <> Fst 2) :: Or Int Int) `shouldBe` (Snd 1 :: Or Int Int)
    it "(Snd 1 <> Snd 2) == Snd 1" $ do
      ((Snd 1 <> Snd 2) :: Or Int Int) `shouldBe` (Snd 1 :: Or Int Int)
    it "is associative" $ do
      property (orAssoc :: OrAssoc String String)
