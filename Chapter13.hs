module HaskellProgrammingChapter13 where

import           Control.Monad                  ( forever )
import           Text.Read                      ( readMaybe )

type Name = String

type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                        | AgeTooLow
                        | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0
    = Right $ Person name age
    | name == ""
    = Left NameEmpty
    | age <= 0
    = Left AgeTooLow
    | otherwise
    = Left
        $  PersonInvalidUnknown
        $  "Name was: "
        ++ show name
        ++ " Age was: "
        ++ show age

gimmePerson :: IO ()
gimmePerson = forever $ do
    putStr "Gimme a name: "
    name <- getLine
    putStr "Gimme an age: "
    ageString <- getLine
    case readMaybe ageString of
        Just age -> print (show $ mkPerson name age)
        Nothing  -> putStrLn "Invalid age"
