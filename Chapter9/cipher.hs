module Cipher where
import Data.Char

caesar :: String -> Int -> String
caesar "" _ = ""
caesar (x:xs) shift
 | elem x ['a'..'z'] = replacementLetter : caesar xs shift
 | elem x ['A'..'Z'] = (toUpper replacementLetter) : caesar xs shift
 | otherwise = x : caesar xs shift
 where replacementLetter = chr shiftedOrd
       shiftedOrd = (mod (alphabetIndex + shift) 26) + 97
       alphabetIndex = (ord . toLower) x - 97

unCaesar :: String -> Int -> String
unCaesar s shift = caesar s (shift * (-1))
