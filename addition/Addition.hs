module Addition where

import           Data.List
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Data.Char                      ( toUpper )


main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > (1 :: Int) `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` (4 :: Int)
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

digitToWord :: Int -> String
digitToWord n | n == 0    = "zero"
              | n == 1    = "one"
              | n == 2    = "two"
              | n == 3    = "three"
              | n == 4    = "four"
              | n == 5    = "five"
              | n == 6    = "six"
              | n == 7    = "seven"
              | n == 8    = "eight"
              | n == 9    = "nine"
              | otherwise = "more than one digit"

digits :: Int -> [Int]
digits n | n >= 10   = digits (div n 10) ++ [mod n 10]
         | otherwise = [n]

wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord (digits n))

mainWordNumber :: IO ()
mainWordNumber = hspec $ do
    describe "digitToWord" $ do
        it "returns zero for 0" $ digitToWord 0 `shouldBe` "zero"
        it "returns one for 1" $ digitToWord 1 `shouldBe` "one"
    describe "digits" $ do
        it "returns [1] for 1" $ digits 1 `shouldBe` [1]
        it "returns [1, 0, 0] for 100" $ digits 100 `shouldBe` [1, 0, 0]
    describe "wordNumber" $ do
        it "one-zero-zero given 100" $ wordNumber 100 `shouldBe` "one-zero-zero"
        it "nine-zero-zero-one for 9001"
            $          wordNumber 9001
            `shouldBe` "nine-zero-zero-one"

half :: Fractional a => a -> a
half x = x / 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_      , False) = status
    go y (       Nothing, t    ) = (Just y, t)
    go y (       Just x , _    ) = (Just y, x >= y)

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

multiplyAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multiplyAssociative x y z = x * (y * z) == (x * y) * z

multiplyCommutative :: (Eq a, Num a) => a -> a -> Bool
multiplyCommutative x y = x * y == y * x

quoteRemRelationship :: NonZero Int -> NonZero Int -> Bool
quoteRemRelationship (NonZero x) (NonZero y) = (quot x y) * y + (rem x y) == x

divModRelationship :: NonZero Int -> NonZero Int -> Bool
divModRelationship (NonZero x) (NonZero y) = (div x y) * y + (mod x y) == x

newtype LargerThanOne = LargerThanOne Int deriving (Eq, Show)

largerThanOneGen :: Gen LargerThanOne
largerThanOneGen = LargerThanOne <$> choose (2, 10)

instance Arbitrary LargerThanOne where
    arbitrary = largerThanOneGen

powerAssociative :: InfiniteList (Int, Int, Int) -> Bool
powerAssociative (InfiniteList xs _) = all
    (\(x, y, z) -> x ^ (y ^ z) /= (x ^ y) ^ z)
    xs100
    where xs100 = take 100 xs

powerCommutative :: InfiniteList (LargerThanOne, LargerThanOne) -> Bool
powerCommutative (InfiniteList xs _) = all
    (\(LargerThanOne x, LargerThanOne y) -> (x ^ y) == (y ^ x))
    (take 100 xs)

doubleReverseEqualsId :: Eq a => [a] -> Bool
doubleReverseEqualsId firstList = (reverse . reverse) firstList == firstList

dollarSignActsLikeBrackets :: Eq b => (a -> b) -> a -> Bool
dollarSignActsLikeBrackets f a = (f $ a) == f a

dotComposesFunctions :: (Float -> Int) -> (String -> Float) -> String -> Bool
dotComposesFunctions f g a = (f . g) a == f (g a)

capitalizeWord :: String -> String
capitalizeWord ""       = []
capitalizeWord (x : xs) = [toUpper x] ++ xs

twice :: (b -> b) -> b -> b
twice f = f . f
fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

instance Show (a -> b) where
    show _ = "<function>"

data Fool = Fulse | Frue deriving (Eq, Show)

equalChanceFoolGen :: Gen Fool
equalChanceFoolGen = do
    a <- arbitrary :: Gen Int
    return (if (a `mod` 2) == 0 then Fulse else Frue)

twoThirdsFulseFoolGen :: Gen Fool
twoThirdsFulseFoolGen = do
    a <- arbitrary :: Gen Int
    return (if (a `mod` 3) == 0 then Frue else Fulse)



mainUsingQuickCheck :: IO ()
mainUsingQuickCheck = hspec $ do
    describe "half property check" $ do
        it "should halve the number" $ do
            property $ \x -> half (x * 2) == (x :: Double)
    describe "sort property check" $ do
        it "should sort the list" $ do
            property $ \xs -> listOrdered $ sort (xs :: [Int])
    describe "addition property check" $ do
        it "should be associative" $ do
            property
                $ \x y z -> plusAssociative (x :: Int) (y :: Int) (z :: Int)
        it "should be commutative" $ do
            property $ \x y -> plusCommutative (x :: Int) (y :: Int)
    describe "multiplication property check" $ do
        it "should be associative" $ do
            property $ \x y z ->
                multiplyAssociative (x :: Int) (y :: Int) (z :: Int)
        it "should be commutative" $ do
            property $ \x y -> multiplyCommutative (x :: Int) (y :: Int)
    describe "quote rem relationship check" $ do
        it "should be related in the correct way" $ do
            property $ \x y -> quoteRemRelationship x y
    describe "div mod relationship check" $ do
        it "should be related in the correct way" $ do
            property $ \x y -> divModRelationship x y
    -- describe "power property check" $ do
    --     it "should not be associative" $ do
    --         property $ \x -> powerAssociative x
    --     it "should not be commutative" $ do
    --         property (not . powerCommutative)
    describe "double reversing list" $ do
        it "should result in the same list" $ do
            property $ \x -> doubleReverseEqualsId (x :: [Int])
    describe "$" $ do
        it "should use $ as brackets" $ do
            property $ \f a ->
                dollarSignActsLikeBrackets (f :: Int -> Int) (a :: Int)
        it "should use $ as brackets" $ do
            property
                $ \f a -> dollarSignActsLikeBrackets (f :: String -> Int)
                                                     (a :: String)
        -- it "should use . to compose functions" $ do
        --     property $ dotComposesFunctions
    describe "idempotence" $ do
        it "should apply capitalizeWord multiple times with the same result"
            $ do
                  property $ \x ->
                      (capitalizeWord x == twice capitalizeWord x)
                          && (capitalizeWord x == fourTimes capitalizeWord x)
        it "should apply sort multiple times with the same result" $ do
            property
                $ \x ->
                      (sort (x :: [String]) == twice sort x)
                          && (sort x == fourTimes sort x)
