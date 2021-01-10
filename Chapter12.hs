{-# LANGUAGE LambdaCase #-}

module Chapter12 where
-- Chapter Exercises

import           Data.Maybe                     ( fromMaybe )
import           Data.Char                      ( toLower )

notThe :: String -> Maybe String
notThe "the"  = Nothing
notThe string = Just string

replaceThe :: String -> String
replaceThe string =
    unwords (map (Data.Maybe.fromMaybe "a" . notThe) (words string))

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (x : xs) | f x       = 1 + count f xs
                 | otherwise = count f xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel "" = 0
countTheBeforeVowel string =
    if head stringWords == "the" && elem (head (head (tail stringWords))) vowels
        then 1 + countTheBeforeVowel (unwords (tail stringWords))
        else countTheBeforeVowel (unwords (tail stringWords))
    where stringWords = words string


vowels :: String
vowels = "aeiou"

consonants :: String
consonants = "bcdfghjklmnpqrstvwxyz"

countVowels :: String -> Integer
countVowels "" = 0
countVowels (x : xs) =
    if toLower x `elem` vowels then 1 + countVowels xs else countVowels xs

countConsonants :: String -> Integer
countConsonants ""       = 0
countConsonants (x : xs) = if toLower x `elem` consonants
    then 1 + countConsonants xs
    else countConsonants xs

newtype Word' =
    Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord string = if countVowels string > countConsonants string
    then Nothing
    else Just (Word' string)

data Nat = Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero       = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0 = Nothing
    | n == 0 = Just Zero
    | otherwise = case integerToNat (n - 1) of
        Nothing  -> Nothing
        Just nat -> Just (Succ nat)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ atob (Just a) = atob a
mayybee b _    Nothing  = b

fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []                 = Just []
flipMaybe (Just a : maybeAs) = case flipMaybe maybeAs of
    Nothing   -> Nothing
    Just list -> Just (a : list)
flipMaybe (Nothing : _) = Nothing

lefts' :: [Either a b] -> [a]
lefts' = foldr
    (\eitherAb -> flip
        (++)
        (case eitherAb of
            Left  a -> [a]
            Right _ -> []
        )
    )
    []

rights' :: [Either a b] -> [b]
rights' = foldr
    (\eitherAb -> flip
        (++)
        (case eitherAb of
            Left  _ -> []
            Right b -> [b]
        )
    )
    []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eitherAbs = (lefts' eitherAbs, rights' eitherAbs)

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe _    (Left  _) = Nothing
eitherMaybe btoc (Right b) = (Just . btoc) b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' atoc _    (Left  a) = atoc a
either' _    btoc (Right b) = btoc b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' btoc = either' (const Nothing) (Just . btoc)

myIterate :: (a -> a) -> a -> [a]
myIterate atoa a = resultA : myIterate atoa resultA where resultA = atoa a

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr bToMaybeAB b = case maybeAB of
    Just ab -> fst ab : myUnfoldr bToMaybeAB (snd ab)
    Nothing -> myUnfoldr bToMaybeAB b
    where maybeAB = bToMaybeAB b

betterIterate :: Num a => (a -> a) -> a -> [a]
betterIterate atoa = myUnfoldr (\b -> Just (b, atoa b))

data BinaryTree a = Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold aToMaybeABA a = case maybeABA of
    Just (a1, b, a2) -> Node (unfold aToMaybeABA a1) b (unfold aToMaybeABA a2)
    Nothing          -> Leaf
    where maybeABA = aToMaybeABA a

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold
    (\case
        0 -> Nothing
        x -> Just (x - 1, n - x, x - 1)
    )
    n
