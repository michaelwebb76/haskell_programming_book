{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter11 where

-- Exercises: Dog types
-- 1. Type constructor
-- 2. * -> *
-- 3. *
-- 4. Num a => Doggies a
-- 5. Doggies Int
-- 6. Doggies String
-- 7. Both
-- 8. doge -> DogueDeBordeux doge
-- 9. DogueDeBordeux String

-- Exercises: Vehicles

import Data.Char
  ( toLower,
    toUpper,
  )
import Data.List
  ( find,
    maximumBy,
    sort,
  )
import Data.Maybe (isJust)

data Price = Price Integer deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data PlaneSize = Seats Int deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline PlaneSize
  deriving (Eq, Show)

-- 1. Vehicle

isCar :: Vehicle -> Bool
isCar v = case v of
  Car _ _ -> True
  _ -> False

isPlane :: Vehicle -> Bool
isPlane v = case v of
  Plane _ _ -> True
  _ -> False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu v = case v of
  Car manufacturer _ -> manufacturer
  _ -> undefined

-- Exercises: Cardinality
-- 1. 0
-- 2. 3
-- 3. 65536
-- 4. They is big
-- 5. It is 2 ^ X.

-- Exercises: For Example
-- 1. Example. Can't get type of Example (it's a kind?)
-- 2. Can run :info and find that it has an instance of Show.
-- 3. It has become a constructor function.

-- Exercises: Logic Goats
class TooMany a where tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

instance TooMany (Int, Int) where
  tooMany (x, y) = (x + y) > 42

instance (Num a, Ord a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = (a + b) > 42

-- Exercises: Pity the Bool
-- 1. = Big Bool | Small Bool
--    = Big 2 | Small 2
--    = 4
-- 2. = Numba Int8 | BoolyBool Bool
--    = Number 256 | BoolyBool 2
--    = 256 + 2
--    = 258

-- Exercises: How Does Your Garden Grow?
-- 1. Cardinality(String) * Cardinality(FlowerType)
--    = data Garden = Gardenia Gardener
--                  | Daisy Gardener
--                  | Rose Gardener
--                  | Lilac Gardener

-- Exercises: Programmers

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer
  = Programmer
      { os :: OperatingSystem,
        lang :: ProgrammingLanguage
      }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  concatMap
    (\os' -> map (\lang' -> Programmer {os = os', lang = lang'}) allLanguages)
    allOperatingSystems

-- Binary Tree

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

-- convert binary trees to Lists
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = (a : (preorder left)) ++ preorder right

inorder :: Ord a => BinaryTree a -> [a]
inorder = sort . preorder

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = preorder left ++ preorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = foldTree f newRightAcc right
  where
    newLeftAcc = f a acc
    newRightAcc = foldTree f newLeftAcc left

-- 11.18 Chapter Exercises
-- 1. a
-- 2. c
-- 3. b
-- 4. d

-- As-patterns

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf subList@(a : as) list@(b : bs)
  | a == b = (isSubsequenceOf as bs) || (isSubsequenceOf subList bs)
  | a /= b = isSubsequenceOf subList bs

capitalizeWords :: String -> [(String, String)]
capitalizeWords "" = []
capitalizeWords string@(x : xs) =
  (firstWord, capitalizedFirstWord)
    : (capitalizeWords . concat . tail) wordList
  where
    wordList = words string
    firstWord = head wordList
    capitalizedFirstWord = ([toUpper x] ++ tail firstWord) :: String

-- Phone exercises

type ButtonChar = Char

type AlternateChar = Char

type CapitalizeLast = Bool

data ButtonConfig
  = ButtonConfig
      ButtonChar
      (Maybe AlternateChar)
      (Maybe AlternateChar)
      (Maybe AlternateChar)
      (Maybe AlternateChar)
      CapitalizeLast
  deriving (Eq, Show)

data DaPhone
  = DaPhone
      { one :: ButtonConfig,
        two :: ButtonConfig,
        three :: ButtonConfig,
        four :: ButtonConfig,
        five :: ButtonConfig,
        six :: ButtonConfig,
        seven :: ButtonConfig,
        eight :: ButtonConfig,
        nine :: ButtonConfig,
        star :: ButtonConfig,
        zero :: ButtonConfig,
        hash :: ButtonConfig
      }
  deriving (Eq, Show)

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"
  ]

type Digit = Char

type Presses = Int

daPhoneConfig :: DaPhone
daPhoneConfig =
  DaPhone
    { one = ButtonConfig '1' Nothing Nothing Nothing Nothing False,
      two = ButtonConfig '2' (Just 'a') (Just 'b') (Just 'c') Nothing False,
      three = ButtonConfig '3' (Just 'd') (Just 'e') (Just 'f') Nothing False,
      four = ButtonConfig '4' (Just 'g') (Just 'h') (Just 'i') Nothing False,
      five = ButtonConfig '5' (Just 'j') (Just 'k') (Just 'l') Nothing False,
      six = ButtonConfig '6' (Just 'm') (Just 'n') (Just 'o') Nothing False,
      seven = ButtonConfig '7' (Just 'p') (Just 'q') (Just 'r') (Just 's') False,
      eight = ButtonConfig '8' (Just 't') (Just 'u') (Just 'v') Nothing False,
      nine = ButtonConfig '9' (Just 'w') (Just 'x') (Just 'y') (Just 'z') False,
      star = ButtonConfig '*' Nothing Nothing Nothing Nothing True,
      zero = ButtonConfig '0' (Just ' ') Nothing Nothing Nothing False,
      hash = ButtonConfig '#' (Just '.') (Just ',') Nothing Nothing False
    }

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (x : xs)
  | f x = 1 + count f xs
  | otherwise = count f xs

buttonPresses :: ButtonConfig -> Char -> Maybe Presses
buttonPresses (ButtonConfig buttonChar maybeAltChar1 maybeAltChar2 maybeAltChar3 maybeAltChar4 _) char
  | buttonChar == (toLower char) =
    Just
      ( 1
          + count
            (\x -> isJust x)
            [maybeAltChar1, maybeAltChar2, maybeAltChar3, maybeAltChar4]
      )
  | maybeAltChar1 == Just (toLower char) =
    Just 1
  | maybeAltChar2 == Just (toLower char) =
    Just 2
  | maybeAltChar3 == Just (toLower char) =
    Just 3
  | maybeAltChar4 == Just (toLower char) =
    Just 4
  | otherwise =
    Nothing

buttonConfigWithChar :: DaPhone -> Char -> Maybe ButtonConfig
buttonConfigWithChar daPhone char =
  find
    ( \buttonConfig -> case buttonPresses buttonConfig char of
        Just _ -> True
        Nothing -> False
    )
    [ one daPhone,
      two daPhone,
      three daPhone,
      four daPhone,
      five daPhone,
      six daPhone,
      seven daPhone,
      eight daPhone,
      nine daPhone,
      star daPhone,
      zero daPhone,
      hash daPhone
    ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps daPhone char
  | char == toUpper char && (toLower char /= toUpper char) =
    ('*', 1) : (reverseTaps daPhone (toLower char))
  | otherwise =
    case buttonConfigWithChar daPhone char of
      Just buttonConfig@(ButtonConfig buttonChar _ _ _ _ _) ->
        case buttonPresses buttonConfig char of
          Just presses -> [(buttonChar, presses)]
          Nothing -> [(buttonChar, -1)]
      Nothing -> undefined

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead daPhone = concatMap (\x -> reverseTaps daPhone x)

mostPopularLetter :: String -> Char
mostPopularLetter string =
  fst
    (maximumBy (\x y -> compare (snd x) (snd y)) charCounts)
  where
    charCounts = map (\x -> (x, count (\y -> y == x) string)) string

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord stringList =
  fst
    (maximumBy (\x y -> compare (snd x) (snd y)) wordCounts)
  where
    allWords = concatMap words stringList
    wordCounts = map (\x -> (x, count (\y -> y == x) allWords)) allWords

-- Hutton's Razor

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit int) = int
eval (Add expr1 expr2) = (eval expr1) + (eval expr2)

printExpr :: Expr -> String
printExpr (Lit int) = show int
printExpr (Add expr1 expr2) = (printExpr expr1) ++ " + " ++ (printExpr expr2)
