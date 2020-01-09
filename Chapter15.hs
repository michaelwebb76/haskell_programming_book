{-# LANGUAGE TupleSections #-}

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

instance Monoid Trivial where
  mempty = Trivial

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

trivialAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
trivialAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

mainTrivial :: IO ()
mainTrivial = do
  quickCheck (trivialAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

-- Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

identityAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
identityAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

mainIdentity :: IO ()
mainIdentity = do
  quickCheck (identityAssoc :: IdentityAssoc String)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

-- Two

data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    genA <- arbitrary
    Two genA <$> arbitrary

twoAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
twoAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

mainTwo :: IO ()
mainTwo = do
  quickCheck (twoAssoc :: TwoAssoc String String)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)

-- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three a b c) (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    genA <- arbitrary
    genB <- arbitrary
    Three genA genB <$> arbitrary

threeAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
threeAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

mainThree :: IO ()
mainThree = do
  quickCheck (threeAssoc :: ThreeAssoc String String String)
  quickCheck (monoidLeftIdentity :: Three String String String -> Bool)
  quickCheck (monoidRightIdentity :: Three String String String -> Bool)

-- Four

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    genA <- arbitrary
    genB <- arbitrary
    genC <- arbitrary
    Four genA genB genC <$> arbitrary

fourAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
fourAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

mainFour :: IO ()
mainFour = do
  quickCheck (fourAssoc :: FourAssoc String String String String)
  quickCheck (monoidLeftIdentity :: Four String String String String -> Bool)
  quickCheck (monoidRightIdentity :: Four String String String String -> Bool)

-- BoolConj

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Monoid BoolConj where
  mempty = BoolConj True

instance Semigroup BoolConj where
  (<>) (BoolConj a) (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

boolConjAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
boolConjAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

mainBoolConj :: IO ()
mainBoolConj = do
  quickCheck (boolConjAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

-- BoolDisj

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Semigroup BoolDisj where
  (<>) (BoolDisj a) (BoolDisj b) = BoolDisj (a || b)

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

boolDisjAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
boolDisjAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

mainBoolDisj :: IO ()
mainBoolDisj = do
  quickCheck (boolDisjAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

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

-- Combine

newtype Combine a b = Combine {unCombine :: a -> b}

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty

instance Semigroup b => Semigroup (Combine a b) where
  (<>) combineA combineB = Combine $ \n -> ((unCombine combineA) n) <> ((unCombine combineB) n)

instance Show (Combine a b) where
  show _ = "Combine a b"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

combineAssoc :: String -> Combine String String -> Combine String String -> Combine String String -> Bool
combineAssoc x a b c =
  let fa = (a <> (b <> c))
      fb = ((a <> b) <> c)
   in ((unCombine fa) x) == ((unCombine fb) x)

mainCombine :: IO ()
mainCombine =
  let f = Combine $ \n -> Sum (n + 1)
      g = Combine $ \n -> Sum (n - 1)
   in hspec $ do
        describe "Combine" $ do
          it "unCombine (f <> g) $ 0 == Sum { getSum = 0 }" $ do
            (unCombine (f <> g) $ 0) `shouldBe` Sum {getSum = 0}
          it "unCombine (f <> g) $ 1 == Sum { getSum = 2 }" $ do
            (unCombine (f <> g) $ 1) `shouldBe` Sum {getSum = 2}
          it "unCombine (f <> f) $ 1 == Sum { getSum = 4 }" $ do
            (unCombine (f <> f) $ 1) `shouldBe` Sum {getSum = 4}
          it "unCombine (g <> f) $ 1 == Sum { getSum = 2 }" $ do
            (unCombine (g <> f) $ 1) `shouldBe` Sum {getSum = 2}
          it "is associative" $ do
            property combineAssoc
          it "implements monoid" $ do
            (unCombine (mappend f mempty) $ 1) `shouldBe` Sum {getSum = 2}

-- Comp

newtype Comp a = Comp {unComp :: a -> a}

instance Monoid a => Monoid (Comp a) where
  mempty = Comp mempty

instance Semigroup a => Semigroup (Comp a) where
  (<>) compA compB = Comp $ \n -> ((unComp compA) n) <> ((unComp compB) n)

instance Show (Comp a) where
  show _ = "Comp a"

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

compAssoc :: String -> Comp String -> Comp String -> Comp String -> Bool
compAssoc x a b c =
  let fa = (a <> (b <> c))
      fb = ((a <> b) <> c)
   in unComp fa x == unComp fb x

mainComp :: IO ()
mainComp =
  hspec $ do
    describe "Comp" $ do
      it "is associative" $ do
        property compAssoc

-- Validation

data Validation a b = VFailure a | VSuccess b deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) where
  (<>) (VFailure a) (VFailure b) = VFailure (a <> b)
  (<>) (VFailure a) _ = VFailure a
  (<>) _ (VFailure a) = VFailure a
  (<>) (VSuccess a) _ = VSuccess a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = frequency [(1, VFailure <$> arbitrary), (1, VSuccess <$> arbitrary)]

validationAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
validationAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

mainValidation :: IO ()
mainValidation = hspec $ do
  describe "Validation" $ do
    it "is associative" $ do
      property (validationAssoc :: ValidationAssoc String String)

-- AccumulateRight

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance (Semigroup b) => Semigroup (AccumulateRight a b) where
  (<>) (AccumulateRight (VFailure _)) (AccumulateRight (VFailure b)) = AccumulateRight (VFailure b)
  (<>) (AccumulateRight (VFailure a)) _ = AccumulateRight (VFailure a)
  (<>) _ (AccumulateRight (VFailure a)) = AccumulateRight (VFailure a)
  (<>) (AccumulateRight (VSuccess a)) (AccumulateRight (VSuccess b)) = AccumulateRight (VSuccess (a <> b))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = frequency [(1, AccumulateRight <$> (VFailure <$> arbitrary)), (1, AccumulateRight <$> (VSuccess <$> arbitrary))]

accumulateRightAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
accumulateRightAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type AccumulateRightAssoc a b = AccumulateRight a b -> AccumulateRight a b -> AccumulateRight a b -> Bool

mainAccumulateRight :: IO ()
mainAccumulateRight = hspec $ do
  describe "AccumulateRight" $ do
    it "is associative" $ do
      property (accumulateRightAssoc :: AccumulateRightAssoc String String)

-- AccumulateBoth

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (<>) (AccumulateBoth (VFailure a)) (AccumulateBoth (VFailure b)) = AccumulateBoth (VFailure $ a <> b)
  (<>) (AccumulateBoth (VSuccess a)) (AccumulateBoth (VSuccess b)) = AccumulateBoth (VSuccess $ a <> b)
  (<>) (AccumulateBoth (VFailure a)) _ = AccumulateBoth (VFailure a)
  (<>) _ (AccumulateBoth (VFailure a)) = AccumulateBoth (VFailure a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = frequency [(1, AccumulateBoth <$> (VFailure <$> arbitrary)), (1, AccumulateBoth <$> (VSuccess <$> arbitrary))]

accumulateBothAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
accumulateBothAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type AccumulateBothAssoc a b = AccumulateBoth a b -> AccumulateBoth a b -> AccumulateBoth a b -> Bool

mainAccumulateBoth :: IO ()
mainAccumulateBoth = hspec $ do
  describe "AccumulateBoth" $ do
    it "is associative" $ do
      property (accumulateBothAssoc :: AccumulateBothAssoc String String)

-- Mem

newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance (Semigroup a) => Semigroup (Mem s a) where
  (<>) memA memB = Mem $ \s1 ->
    let firstRunMem = runMem memA s1
        secondRunMem = runMem memB (snd firstRunMem)
     in (fst firstRunMem <> fst secondRunMem, snd secondRunMem)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ (mempty,)

mainMem :: IO ()
mainMem =
  let f' = Mem $ \s -> ("hi", s + 1)
   in hspec $ do
        describe "Mem" $ do
          it "works" $ do
            runMem (f' <> mempty) 0 `shouldBe` ("hi", 1)
            runMem (mempty <> f') 0 `shouldBe` ("hi", 1)
            (runMem mempty 0 :: (String, Int)) `shouldBe` ("", 0)
            runMem (f' <> mempty) 0 `shouldBe` runMem f' 0
            runMem (mempty <> f') 0 `shouldBe` runMem f' 0
