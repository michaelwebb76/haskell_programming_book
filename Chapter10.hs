{-# LANGUAGE TupleSections #-}

module HaskellProgrammingChapter10 where
-- Exercises: Understanding Folds
-- 1. foldr (*) 1 [1..5]
--    = (1 * (2 * (3 * (4 * (5 * 1)))))
--    = (1 * (2 * (3 * (4 * 5))))
--    = (1 * (2 * (3 * 20)))
--    = (1 * (2 * 60))
--    = (1 * 120)
--    = 120
-- a) Won't compile
-- b) foldl (*) 1 [1..5] - Matches
--    = (((((1 * 1) * 2) * 3) * 4) * 5)
--    = ((((1 * 2) * 3) * 4) * 5)
--    = (((2 * 3) * 4) * 5)
--    = ((6 * 4) * 5)
--    = (24 * 5)
--    = 120
-- c) foldl (flip (*)) 1 [1..5] - Matches because operator is the same regardless of order
-- 2. foldl (flip (*)) 1 [1..3]
--    = (flip (*)) (foldl (flip (*)) 1 [1, 2]) 3
--    = (flip (*)) ((flip (*)) (foldl (flip (*)) 1 [1]) 2) 3
--    = (flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3
--    = (flip (*)) ((flip (*)) 1 2) 3
--    = (flip (*)) 2 3
--    = 6
-- 3. a
-- 4. a
-- 5. a) foldr (++) "" ["woot", "WOOT", "woot"]
--    b) foldr max 'a' "fear is the little death"
--    c) foldr and True [False, True]
--    d) No because the acc value is True
--    e) foldl (flip ((flip (++)) . show)) "" [1..5]
--    f) foldr const 'a' ['1'..'5']
--    g) foldr const '0' "tacos"
--    h) foldl (flip const) '0' "burritos"
--    i) foldl (flip const) 'z' ['1'..'5']

import           Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbNumber 90
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr
    (\a b -> case a of
        DbDate utcTime -> utcTime : b
        _              -> b
    )
    []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr
    (\a b -> case a of
        DbNumber int -> int : b
        _            -> b
    )
    []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = foldr
    max
    (UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0))
    (filterDbDate items)

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb items =
    fromIntegral (sumDb items) / (fromIntegral . length . filterDbNumber) items

-- Scans exercises
fibs :: [Integer]
fibs = 1 : takeWhile (< 100) (scanl (+) 1 fibs)

fibsN :: Int -> Integer
fibsN x = fibs !! x

factorial :: Integer -> [Integer]
factorial x = scanl (*) 1 [1 .. x]

-- 10.10 Chapter Exercises
makeUnrealWords :: String -> String -> [(Char, Char, Char)]
makeUnrealWords stops vowels =
    concatMap (\x -> concatMap (\y -> map (x, y, ) stops) vowels) stops

seekritFunc :: String -> Float
seekritFunc x = fromIntegral (sum (map length wordsInString))
    / fromIntegral numberOfWordsInString
  where
    wordsInString         = words x
    numberOfWordsInString = length wordsInString

-- Rewriting functions using folds

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 a = myOr . map (== a)

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a = myAny (== a)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f list = foldr
    (\x acc -> case f x acc of
        GT -> x
        _  -> acc
    )
    (head list)
    list

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f list = foldr
    (\x acc -> case f x acc of
        LT -> x
        _  -> acc
    )
    (head list)
    list
