module Hangman where

import           Control.Monad                  ( forever )
import           Data.Char                      ( toLower )
import           Data.Maybe                     ( isJust
                                                , catMaybes
                                                )
import           Data.List                      ( intersperse )
import           System.Exit                    ( exitSuccess )
import           System.Random                  ( randomRIO )
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

mainTest :: IO ()
mainTest = hspec $ do
  describe "fillInCharacter" $ do
    it "puts a character in the right fields" $ do
      property $ \x ->
        let result = fillInCharacter (Puzzle "testable" [] "") x
        in  if elem x "testable"
              then result `shouldBe` Puzzle "testable" [Just x] [x]
              else result `shouldBe` Puzzle "testable" [] [x]
  describe "handleGuess" $ do
    it "responds correctly" $ do
      property $ \x ->
        let result = handleGuess (Puzzle "testable" [] "") x
        in  result
              >>= (\unwrappedResult -> if elem x "testable"
                    then unwrappedResult
                      `shouldBe` Puzzle "testable" [Just x] [x]
                    else unwrappedResult `shouldBe` Puzzle "testable" [] [x]
                  )

newtype WordList = WordList [String] deriving (Eq, Show)

type TargetWord = String

type DiscoveredLetters = [Maybe Char]

type GuessedLetters = String

data Puzzle = Puzzle TargetWord DiscoveredLetters GuessedLetters deriving Eq

data GuessResult = AlreadyGuessed | GuessInWord | GuessNotInWord

-- instance Show Puzzle where
--   show (Puzzle _ discovered guessed) =
--     intersperse ' ' (fmap renderPuzzleChar discovered)
--       ++ " Guessed so far: "
--       ++ guessed
instance Show Puzzle where
  show (Puzzle word discovered guessed) =
    "Puzzle "
      ++ word
      ++ " Discovered "
      ++ (catMaybes discovered)
      ++ " Guessed "
      ++ guessed

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn
      "Your guess must\
                        \ be a single character"

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = case guessResult puzzle guess of
  AlreadyGuessed -> do
    putStrLn "You already guessed that, fuckknuckle"
    return puzzle
  GuessInWord -> do
    putStrLn "Noice"
    return (fillInCharacter puzzle guess)
  GuessNotInWord -> do
    putStrLn "Narp"
    return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed - length wordToGuess) >= 7
    then do
      putStrLn "You lose, dumbass!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle targetWord filledInSoFar _) = if all isJust filledInSoFar
  then do
    putStrLn $ "You win! The word was " ++ targetWord
    exitSuccess
  else return ()

guessResult :: Puzzle -> Char -> GuessResult
guessResult puzzle@(Puzzle _ _ _) guess =
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_    , True) -> AlreadyGuessed
    (True , _   ) -> GuessInWord
    (False, _   ) -> GuessNotInWord

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just char) = char
renderPuzzleChar Nothing     = '_'

freshPuzzle :: TargetWord -> Puzzle
freshPuzzle tw = Puzzle tw (map (const Nothing) tw) ""

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle tw _ _) char = char `elem` tw

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar guessed) char = Puzzle
  word
  newFilledInSoFar
  (char : guessed)
 where
  newFilledInSoFar = if elem (Just char) filledInSoFar
    then filledInSoFar
    else if elem char word then filledInSoFar ++ [Just char] else filledInSoFar


alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = char `elem` guessed

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)

  return $ wl !! randomIndex

gameWords :: IO WordList
gameWords = do
  (WordList wordList) <- allWords
  return $ WordList (filter gameLength wordList)
 where
  gameLength w =
    length (w :: String)
      >= minWordLength
      && length (w :: String)
      <= maxWordLength

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9
