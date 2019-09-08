module Answers where
import           Data.Bool
import           Data.Char
-- Exercise: EnumFromTo
eftBool :: Bool -> Bool -> [Bool]
eftBool = eftUniversal

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftUniversal

eftInt :: Int -> Int -> [Int]
eftInt = eftUniversal

eftChar :: Char -> Char -> [Char]
eftChar = eftUniversal

eftUniversal :: Enum a => a -> a -> [a]
eftUniversal u1 u2 | enumU1 == enumU2 = [u1]
                   | enumU1 > enumU2  = []
                   | otherwise        = [u1] ++ eftUniversal (succ u1) u2
 where
  enumU1 = fromEnum u1
  enumU2 = fromEnum u2

-- Exercise: Thy Fearful Symmetry
-- 1.
myWords :: String -> [String]
myWords = flip splitString $ ' '
-- 2.
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = (flip splitString) '\n'

splitString :: String -> Char -> [String]
splitString "" _ = []
splitString s d
  | head s == d = splitString (tail s) d
  | otherwise   = (takeWhile (/= d) s) : splitString (dropWhile (/= d) s) d

shouldEqual :: [String]
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
-- Exercise: Comprehend Thy Lists
-- let mySqr = [x^2 | x <- [1..5]]

-- [x | x <- mySqr, rem x 2 == 0]
-- [4, 16]

-- [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- []

-- take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]
-- []
-- Exercise: Square Cube
mySqr = [ x ^ 2 | x <- [1 .. 5] ]
myCube = [ y ^ 3 | y <- [1 .. 5] ]
mySquareCube = [ (x, y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]
mySquareCubeLength = length mySquareCube

-- Exercise: Bottom Madness
-- Will it blow up?
-- 1. Bottom
-- 2. Value
-- 3. Bottom
-- 4. Value
-- 5. Bottom
-- 6. Value
-- 7. Bottom
-- 8. Value
-- 9. Value
-- 10. Bottom
-- Intermission: Is it in normal form?
-- 1. Neither
-- 2. Neither
-- 3. NF
-- 4. WHNF
-- 5. NF
-- 6. NF
-- 7. Neither

-- Exercises: More Bottoms
-- 1. Bottom
-- 2. Yes
-- 3. No
-- 4.
itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs
--    Returns an array of Booleans describing which elements in the string are vowels
-- 5 a. [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
--   b. [1, 10, 20]
--   c. [15, 15, 15]
-- 6.
makeThreeNegative :: [Int] -> [Int]
makeThreeNegative []       = []
makeThreeNegative (x : xs) = (bool (-3) x (x /= 3)) : makeThreeNegative xs

-- Exercises: Filtering
howManyMultiplesOfThree :: [Integer] -> Int
howManyMultiplesOfThree = (length . (filter (\x -> mod x 3 == 0)))

removeArticles :: String -> [String]
removeArticles ""        = []
removeArticles (' ' : s) = removeArticles s
removeArticles s
  | (word == "the" || word == "a" || word == "") = removeArticles restOfSentence
  | otherwise = word : removeArticles restOfSentence
 where
  word = takeWhile (/= ' ') s
  restOfSentence | dropWhile (/= ' ') s == "" = ""
                 | otherwise                  = tail (dropWhile (/= ' ') s)

-- Zipping exercises
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ []       _        = []
myZipWith _ _        []       = []
myZipWith f (a : as) (b : bs) = (f a b) : myZipWith f as bs

myZip :: [a] -> [b] -> [(a, b)]
myZip a b = myZipWith (\a -> \b -> (a, b)) a b

-- Chapter Exercises
-- Data.Char
onlyUpper :: String -> String
onlyUpper = filter (isUpper)

firstCharUpper :: String -> String
firstCharUpper ""       = ""
firstCharUpper (x : xs) = toUpper x : xs

allToUpper :: String -> String
allToUpper ""       = ""
allToUpper (x : xs) = toUpper x : allToUpper xs

firstCharOnlyUpper :: String -> Char
firstCharOnlyUpper = (head . firstCharUpper)

-- Writing your own standard functions
-- 1.
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) | x         = True
              | otherwise = myOr xs
-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) | f x       = True
                 | otherwise = myAny f xs
-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem a (b : bs) | a == b    = True
                  | bs == []  = False
                  | otherwise = myElem a bs
myElemAny :: Eq a => a -> [a] -> Bool
myElemAny a bs = any (== a) bs
-- 4.
myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = (myReverse xs) ++ [x]
-- 5.
squish :: [[a]] -> [a]
squish []       = []
squish (x : xs) = x ++ squish xs
-- 6.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []       = []
squishMap f (x : xs) = (f x) ++ squishMap f xs
-- 7.
squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (map id) xs
--8.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy f (x1 : x2 : xs) | f x1 x2 == GT = myMaximumBy f (x1 : xs)
                             | otherwise     = myMaximumBy f (x2 : xs)
myMaximumBy f (x : xs) = x
--9.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy f (x1 : x2 : xs) | f x1 x2 == LT = myMinimumBy f (x1 : xs)
                             | otherwise     = myMinimumBy f (x2 : xs)
myMinimumBy f (x : xs) = x

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
