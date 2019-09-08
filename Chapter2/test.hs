sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

----------------------------------------------------------------------------------------------------

triple :: Integer -> Integer
triple x = x * 3

----------------------------------------------------------------------------------------------------

-- Exercises: Comprehension Check
-- 1. Add let to the start of the statement.
-- 2.
exerciseTwo :: Integer -> Double
exerciseTwo x = fromIntegral (x ^ 2) * pi

----------------------------------------------------------------------------------------------------

-- Exercises: Parentheses and Association
-- 1. Yes.
-- 2. No.
-- 3. Yes.

----------------------------------------------------------------------------------------------------

-- Exercises: Heal the Sick

-- let area x = 3.14 * (x * x)
-- let double x = x * 2
-- x = 7
-- y = 10
-- f = x + y

----------------------------------------------------------------------------------------------------

-- Exercises: A Head Code

-- 1. 5
-- 2. 25
-- 3. 25
-- 4. 30
-- 5. 6

----------------------------------------------------------------------------------------------------

-- Exercises with let and where!
-- 1. x + 3 + y where x = 3; y = 1000
-- 2. x * 5 where y = 10; x = 10 * 5 + y
-- 3. z / x + y where x = 7; y = negate x; z = y * 10


----------------------------------------------------------------------------------------------------

-- Chapter exercises
-- Parenthesization
-- 2 + (2 * 3) - 1
-- 10 ^ (1 + 1)
-- ((2 ^ 2) * (4 ^ 5)) + 1

-- Equivalent expressions
-- 1, 2

-- More fun with functions
-- let z = 7
-- let y = z + 8
-- let x = y ^ 2
-- let waxOn = x * 5

-- 1.
-- 1135
-- 1135
-- -1110
-- 1110

-- 3.
waxOn = x * 5
 where
  x = y ^ 2
  y = z + 8
  z = 7

-- 6.
waxOff x = triple x
