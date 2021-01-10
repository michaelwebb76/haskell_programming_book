{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter23 where

import Data.Bifunctor
import Data.Sequence.Internal
import System.Random
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Prelude

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use error sparingly
    x ->
      error $
        "intToDie got non 1-6 integer: "
          ++ show x

rollsToGetN :: Int -> StdGen -> (Int, [Die])
rollsToGetN n g = go 0 (0, []) g
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum countAndDie gen
      | sum >= n = countAndDie
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go
              (sum + die)
              ((fst countAndDie + 1), snd countAndDie ++ [intToDie die])
              nextGen

-- Write State for yourself

newtype Moi s a
  = Moi {runMoi :: s -> (a, s)}
  deriving (Show)

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ (first f . g)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ (\s -> (a, s))

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi stateFAToB) <*> (Moi stateA) = Moi $ uncurry (\fAToB -> first fAToB . stateA) . stateFAToB

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ uncurry runMoi . first g . f

putMoi :: s -> Moi s ()
putMoi newState = Moi $ \_ -> ((), newState)

getMoi :: Moi s s
getMoi = Moi $ \state -> (state, state)

mainMoi = hspec $ do
  describe "Functor" $ do
    it "works" $ do
      runMoi functor 0 `shouldBe` (1, 0)
  describe "Applicative" $ do
    it "works" $ do
      runMoi applicative 0 `shouldBe` (1, 0)
    it "works" $ do
      runMoi (putMoi 1 *> putMoi 2 *> getMoi) (1110 :: Int) `shouldBe` (2, 2)
  describe "Monad" $ do
    it "works" $ do
      runMoi monad 0 `shouldBe` (1, 100)
    it "works" $ do
      runMoi (putMoi 1 >> putMoi 2 >> getMoi) (1110 :: Int) `shouldBe` (2, 2)
  where
    functor = (+ 1) <$> (Moi $ \s -> (0, s))
    applicative = (Moi $ \s -> ((+ 1), s)) <*> (Moi $ \s -> (0, s))
    monad = (Moi $ \s -> (0, s + 1)) >>= (\a -> Moi $ \s -> ((a + 1), s * 100))

-- Chapter exercises

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ (\_ -> (s, ()))

exec :: State s a -> s -> s
exec (State sa) = fst . sa

eval :: State s a -> s -> a
eval (State sa) = snd . sa

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())

mainChapterExercises = hspec $ do
  describe "1" $ do
    it "works" $ do
      runState get "curryIsAmaze" `shouldBe` ("curryIsAmaze", "curryIsAmaze")
  describe "2" $ do
    it "works" $ do
      runState (put "blah") "woot" `shouldBe` ("blah", ())
  describe "3" $ do
    it "works" $ do
      exec (put "wilma") "daphne" `shouldBe` "wilma"
    it "works" $ do
      exec get "scooby papu" `shouldBe` "scooby papu"
  describe "4" $ do
    it "works" $ do
      eval get "bunnicula" `shouldBe` "bunnicula"
    it "works" $ do
      eval get "stake a bunny" `shouldBe` "stake a bunny"
  describe "5" $ do
    it "works" $ do
      let f = modify (+ 1)
      runState f (0 :: Int) `shouldBe` (1, ())
