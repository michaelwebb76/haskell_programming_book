-- Exercises: Type matching
-- a) c)
-- b) d)
-- c) b)
-- d) a)
-- e) e)

-- Exercises: Type arguments
-- 1. a)
-- 2. d)
-- 3. d)
-- 4. c)
-- 5. a)
-- 6. e)
-- 7. d)
-- 8. a)
-- 9. c)

-- Exercises: Apply yourself
-- 1. myConcat :: [Char] -> [Char]
-- 2. myMult :: (Fractional a) => a -> a
-- 3. myTake :: Int -> [Char]
-- 4. myCom :: Int -> Bool
-- 5. myAlph :: Char -> Bool

-- Chapter Exercises
-- Multiple choice
-- 1. c)
-- 2. a)
-- 3. b)
-- 4. c)

-- Determine the type
-- 1.
-- a) Num a => a
-- b) Num t => (t, [Char])
-- c) (Integer, [Char])
-- d) Bool
-- e) Int
-- f) Bool
-- 2. Num a => a
-- 3. Num a => a -> a
-- 4. Fractional a => a
-- 5. [Char]

-- Does it compile?
-- 1. wahoo will squawk.
-- 2. no lines will squawk.
-- 3. Lines c & d will squawk. a b 10 & a c 200
-- 4. Both lines with squawk. b isn't defined yet and c doesn't exist.

-- Type variable or specific type constructor?
-- 2. fully polymorphic, concrete, concrete
-- 3. fully polymorphic, constrained polymorphic, concrete
-- 4. fully polymorphic, fully polymorphic, concrete

-- Write a type signature
-- 1. [x] -> x
-- 2. (Ord x, Ord y) => x -> y -> Bool
-- 3. x -> y -> y

module Answers where
-- Given a type, write the function
-- 1. i x = x
  i :: a -> a
  i a = a

-- 2. c a b = a
  c :: a -> b -> a
  c a b = a

-- 3. c'' b a = b
  c'' :: b -> a -> b
  c'' b a = b
-- 4. c' a b = b
  c' :: a -> b -> b
  c' a b = b
-- 5. r a = body a, r a = a :: a
  r1 :: [a] -> [a]
  r1 a = tail a

  r2 :: [a] -> [a]
  r2 a = a ++ a
-- 6.
  co :: (b -> c) -> (a -> b) -> (a -> c)
  co t0 t1 a = t0 (t1 a)
-- 7.
  a :: (a -> c) -> a -> a
  a t0 x = x
-- 8.
  a' :: (a -> b) -> a -> b
  a' t0 x = t0 x
-- Fix it
  fstString :: [Char] -> [Char]
  fstString x = x ++ " in the rain"

  sndString :: [Char] -> [Char]
  sndString x = x ++ " over the rainbow"

  sing = if (x < y) then fstString x else sndString y
         where x = "Singin"
               y = "Somewhere"

  main :: IO ()
  main = do
    print $ (1 + 2)
    putStrLn "10"
    print $ negate (-1)
    print $ ((+) 0 blah)
    where blah = negate 1

  -- Type Kwan Do
  -- 1.
  f :: Int -> String
  f = undefined

  g :: String -> Char
  g = undefined

  h :: Int -> Char
  h a = g (f a)

  -- 2.
  data A
  data B
  data C

  q :: A -> B
  q = undefined

  w :: B -> C
  w = undefined

  e :: A -> C
  e a = w (q a)

  -- 3.
  data X
  data Y
  data Z

  xz :: X -> Z
  xz = undefined

  yz :: Y -> Z
  yz = undefined

  xform :: (X, Y) -> (Z, Z)
  xform(x, y) = (xz x, yz y)

  munge :: (x -> y) -> (y -> (w, z)) -> x -> w
  munge a b c = fst(b (a c))
