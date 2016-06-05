-- Exercises: Scope
-- 1. Yes
-- 2. No, h is not defined.
-- 3. No, the funcions have a circular reference.
-- 4. r and d are in scope.

-- Exercises: Syntax Errors
-- 1. Won't compile, infix operator.
-- 2. Won't compile, single quotes.
-- 3. Will compile (array is acceptable to specify sets of arguments, apparently).

-- Chapter Exercises

-- Reading syntax
-- 1.
-- a) concat [[1, 2, 3], [4, 5, 6]]
-- b) (++) [1, 2, 3] [4, 5, 6]
-- c) (++) "hello" " world"
-- d) ["hello" ++ " world"]
-- e) "hello" !! 4
-- f) (!!) "hello" 4
-- g) take 4 "lovely"
-- h) take 3 "awesome"
-- 2.
-- a) d)
-- b) c)
-- c) e)
-- d) a)
-- e) b)

-- Building functions
module Reverse where
  addExclamationMark :: String -> String
  addExclamationMark x = x ++ "!"

  thirdChar :: String -> Char
  thirdChar x = x !! 2

  letterIndex :: Int -> Char
  letterIndex x = "Curry is awesome!" !! x

  fifthChar :: String -> Char
  fifthChar x = x !! 4

  withoutFirstNineCharsAsString :: String -> String
  withoutFirstNineCharsAsString x = drop 9 x

  rvrs :: String -> String
  rvrs x = do
    lastWord ++ " " ++ middleWord ++ " " ++ firstWord
    where lastWord = drop 9 x
          middleWord = take 2 (drop 6 x)
          firstWord = take 5 x

  main :: IO ()
  main = print $ rvrs "Curry is awesome"
