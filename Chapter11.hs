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

data Price = Price Integer deriving (Eq, Show)

data Manufacturer =
    Mini | Mazda | Tata
    deriving (Eq, Show)

data Airline =
    PapuAir | CatapultsR'Us | TakeYourChancesUnited
    deriving (Eq, Show)

data PlaneSize = Seats Int deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price | Plane Airline PlaneSize
    deriving (Eq, Show)

-- 1. Vehicle

isCar :: Vehicle -> Bool
isCar v = case v of
    Car _ _ -> True
    _       -> False

isPlane :: Vehicle -> Bool
isPlane v = case v of
    Plane _ _ -> True
    _         -> False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu v = case v of
    Car manufacturer _ -> manufacturer
    _                  -> undefined

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