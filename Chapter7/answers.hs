module Answers where

-- Exercises: Grab Bag
-- 1. a) b) c)
-- 2. d
-- 3. a)
addOneIfOdd n = case odd n of
  True  -> f n
  False -> n
  where f = \n -> n + 1
-- 3. b)
addFive = \x -> \y -> (if x > y then y else x) + 5
-- 3. c)
mflip f x y = f y x

-- Exercises: Variety Pack
-- 1. a) k :: (x, y) -> x
-- 1. b) k2 :: [Char], no
-- 1. c) k3
-- 2. f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- Exercises: Case Practice
-- 1.
functionC x y = case (x > y) of
  True  -> x
  False -> y
-- 2.
ifEvenAdd2 n = case even n of
  True  -> n + 2
  False -> n
-- 3.
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

-- Exercises: Artful Dodgy
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

-- 2. 11
-- 3. 22
-- 4. 21
-- 5. 12
-- 6. 11
-- 7. 21
-- 8. 21
-- 9. 22
-- 10. 31
-- 11. 23

-- Exercises: Guard duty
-- 1. You'll get F for everything.
-- 2. No, they're processed sequentially.
-- 3. b
-- 4. An array of things
-- 5. pal :: [a] -> Bool
-- 6. c
-- 7. Ord, Num
-- 8. numbers :: (Ord a, Num a, Num b) => a -> b

-- Chapter Exercises
-- Multiple choice
-- 1 d
-- 2 b
-- 3 d
-- 4 b
-- 5 a

-- Let's write code
-- 1. a
tensDigit :: Integral a => a -> a
tensDigit = ((fst . (flip divMod 10)) . ((snd . (flip divMod 100))))

tensDigitShortNP :: Integral a => a -> a
tensDigitShortNP = ((flip div 10) . (flip mod 100))
-- 1. b Yes
-- 1. c
hundredsDigit :: Integral a => a -> a
hundredsDigit = ((fst . (flip divMod 100)) . ((snd . (flip divMod 1000))))

hundredsDigitShortNP :: Integral a => a -> a
hundredsDigitShortNP = ((flip div 100) . (flip mod 1000))
-- 2
foldBoolWithCase :: a -> a -> Bool -> a
foldBoolWithCase x y b = case b of
  True      -> x
  otherwise -> y

foldBoolWithGuard :: a -> a -> Bool -> a
foldBoolWithGuard x y b | b == True = x
                        | otherwise = y
-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
-- 4
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)
-- 5 & 6
roundTripPF :: (Show a, Read b) => a -> b
roundTripPF = (read . show)
