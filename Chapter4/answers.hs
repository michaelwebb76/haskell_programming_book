-- Exercises: Mood swing
-- 1. Mood
-- 2. Blah or Woot
-- 3. Woot is a value, not a type.
-- 4. changeMood Blah = Woot
--    changeMood Woot = Blah
module Answers where
  data Mood = Blah | Woot deriving Show

  changeMood :: Mood -> Mood
  changeMood Blah = Woot
  changeMood Woot = Blah

-- Exercises: Find the mistakes
-- 1. true is not capitalized.
-- 2. Needs ==.
-- 3. Fine
-- 4. Needs quotes.
-- 5. Can't concatenate a string to an array of integers.

-- Chapter Exercises

  awesome = ["Papuchon", "curry", ":)"]
  alsoAwesome = ["Quake", "The Simons"]
  allAwesome = [awesome, alsoAwesome]

-- 1. length :: Foldable t => t a -> Int
-- 2.
-- a) 5
-- b) 3
-- c) 2
-- d) 4
-- 3. The second one will return an error because length [1,2,3] needs to be assessed first.
-- 4. 6 `div` length [1,2,3]
-- 5. Boolean, True
-- 6. Boolean, False
-- 7.
-- a) Works, True
-- b) Won't work, mixed types
-- c) Works, 5
-- d) Works, False
-- e) Doesn't work, mixing Bool and Integer

  isPalindrome :: (Eq a) => [a] -> Bool
  isPalindrome x = reverse(x) == x

  myAbs :: Integer -> Integer
  myAbs x = if x < 0 then x * (-1) else x

  f :: (a, b) -> (c, d) -> ((b, d), (a, c))
  f x y = ((snd x, snd y), (fst x, fst y))

  -- Correcting syntax

  lengthPlusOne :: String -> Int
  lengthPlusOne xs = w + 1
    where w = length xs

  identityFunction :: a -> a
  identityFunction x = x

  firstItemInList :: [a] -> a
  firstItemInList xs = head xs

  firstItemInTuple :: (a, b) -> a
  firstItemInTuple a = fst a

  -- Match the function names to their types
  -- 1. c)
  -- 2. b)
  -- 3. a)
  -- 4. d)
