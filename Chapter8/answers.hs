module Answers where
import Data.List (intersperse)
-- Intermission: Exercise
-- applyTimes 5 (+1) 5 = (+1) . applyTimes (4) (+1) $ 5
--                     = (+1) . (+1) . applyTimes (3) (+1) $ 5
--                     = (+1) . (+1) . (+1) applyTimes (2) (+1) $ 5
--                     = (+1) . (+1) . (+1) . (+1) applyTimes (1) (+1) $ 5
--                     = (+1) . (+1) . (+1) . (+1) . (+1) applyTimes (0) (+1) $ 5
--                     = (+1) . (+1) . (+1) . (+1) . (+1) $ 5
--                     = (+1) . (+1) . (+1) . (+1) $ 6
--                     = (+1) . (+1) . (+1) $ 7
--                     = (+1) . (+1) $ 8
--                     = (+1) $ 9
--                     = 10

-- Chapter Exercises
-- Review of types
-- 1. d
-- 2. b
-- 3. d
-- 4. b

-- Reviewing currying
-- 1. woops mrow woohoo
-- 2. 1 mrow haha
-- 3. woops mrow 2 mrow haha
-- 4. woops mrow blue mrow haha
-- 5. pink mrow haha mrow green mrow woops mrow blue
-- 6. are mrow Pugs mrow awesome

-- Recursion
-- 1. dividedBy 15 2 = go 15 2 0
--                   = go (15 - 2) 2 (0 + 1)
--                   = go 13 2 1
--                   = go (13 - 2) 2 (1 + 1)
--                   = go 11 2 2
--                   = go (11 - 2) 2 (2 + 1)
--                   = go 9 2 3
--                   = go (9 - 2) 2 (3 + 1)
--                   = go 7 2 4
--                   = go (7 - 2) 2 (4 + 1)
--                   = go 5 2 5
--                   = go (7 - 2) 2 (4 + 1)
--                   = go 5 2 5
--                   = go (5 - 2) 2 (5 + 1)
--                   = go 3 2 6
--                   = go (3 - 2) 2 (6 + 1)
--                   = go 1 2 7
--                   = (7, 1)
-- 2.
recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum x
  | x == 0 = 0
  | otherwise = x + recursiveSum(x - 1)
-- 3.
integralMultiplier :: (Integral a) => a -> a -> a
integralMultiplier x y
  | x == 0 = 0
  | otherwise = y + integralMultiplier (x - 1) y

-- Fixing dividedBy
data DividedResult = Result Integer | DividedByZero deriving Show
dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom = fixSign $ go (abs num) (abs denom) 0
  where go n d count
         | d == 0 = DividedByZero
         | n < d = Result count
         | otherwise = go (n - d) d (count + 1)
        fixSign (Result r) = Result (r * signum num * signum denom)
        fixSign DividedByZero = DividedByZero
-- McCarthy 91 function
mc91 :: Integer -> Integer
mc91 x
 | x > 100 = x - 10
 | otherwise = mc91(mc91(x + 11))
-- Numbers into words
digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = "more than one digit"

digits :: Int -> [Int]
digits n
  | n >= 10 = digits(div n 10) ++ [mod n 10]
  | otherwise = [n]

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))
