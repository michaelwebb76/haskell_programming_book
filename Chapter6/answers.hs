module Answers where
import Data.List
-- Exercises: Will They Work?
-- 1. Yes
-- 2. Yes
-- 3. No
-- 4. Yes

-- Exercises: Eq Instances

-- 1.
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (TisAn a) == (TisAn a') = (a == a')

-- 2.
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (Two a b) == (Two a' b') = ((a == a') && (b == b'))

-- 3.
data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (TisAString a) == (TisAString a') = a == a'
  (TisAnInt a) == (TisAnInt a') = a == a'
  _ == _ = False

-- 4.
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (Pair a b) == (Pair a' b') = (a == a') && (b == b')

-- 5.
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple a b) == (Tuple a' b') = (a == a') && (b == b')

-- 6.
data Which a =
   ThisOne a
 | ThatOne a

instance Eq a => Eq (Which a) where
  (ThisOne a) == (ThisOne a') = a == a'
  (ThatOne a) == (ThatOne a') = a == a'
  _ == _ = False

-- 7.
data EitherOr a =
   Hello a
 | Goodbye a

instance Eq a => Eq (EitherOr a) where
  (Hello a) == (Hello a') = a == a'
  (Goodbye a) == (Goodbye a') = a == a'
  _ == _ = False

-- Chapter Exercises
-- Multiple choice
-- 1. c
-- 2. b
-- 3. a
-- 4. c
-- 5. a

-- Does it typecheck?
-- 1. No, because Person does not have Show defined
-- 2. No, becase Mood does not derive Eq.
-- 3. a) Blah or Woot.
--    b) Type error because must be Blah or Woot. Comparison with another type fails.
--    c) Fails because doesn't implement Ord.
-- 4. Yes.

-- Given a datatype declaration, what can we do?
-- 1. No, because Papu doesn't take a string as an argument
-- 2. Yes.
-- 3. Yes.
-- 4. No, because Papu doesn't derive Ord.

--  Match the types
-- 1. No, because a may not be an Int type.
-- 2. No, because Num is too broad.
-- 3. Yes.
-- 4. Yes
-- 5. Yes
-- 6. Yes
-- 7. No, type is not necessarily Int.
-- 8. No, Num is not sufficiently constrained.
-- 9. Yes.
-- 10. Yes.
-- 11. No, mySort requires array of Char

-- Type-Kwon-Do Two: Electric Typealoo
-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk a b c = a b == c
-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith a b c = (a c) 
