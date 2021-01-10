{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Chapter24 where

import Control.Applicative
import Data.Bits
import Data.List
import Data.Maybe
import Data.Ratio ((%))
import Data.Word
import Numeric
import Test.Hspec
import Text.Parser.Combinators
import Text.RawString.QQ
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser ()
one = char '1' >> eof

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser ()
oneTwo = char '1' >> char '2' >> eof

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

p123 :: Parser String
p123 = (string "123" <|> string "12" <|> string "1") >> stop

testParse123 :: (Show a) => Parser a -> IO ()
testParse123 p = print $ parseString p mempty "123"

testParseString :: Parser String -> String -> IO ()
testParseString p = print . parseString p mempty

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

mainParsers = do
  pNL "stop:"
  testParse123 (stop :: Parser Char)
  pNL "one:"
  testParse123 one
  pNL "one':"
  testParse123 one'
  pNL "oneTwo:"
  testParse123 oneTwo
  pNL "oneTwo':"
  testParse123 oneTwo'
  pNL "p123 1:"
  testParseString p123 "1"
  pNL "p123 12:"
  testParseString p123 "12"
  pNL "p123 123:"
  testParseString p123 "123"
  pNL "p123 1234:"
  testParseString p123 "1234"

-- Parsing fractions
badFraction = "1/0"

alsoBad = "10"

shouldWork = "1/2"

shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

failingParseFractionMain :: IO ()
failingParseFractionMain = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be 0"
    _ -> return (numerator % denominator)

virtuousFractionMain :: IO ()
virtuousFractionMain = do
  let parseFraction' = parseString virtuousFraction mempty
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork

-- Exercise: Unit of success

parseAndReturnInteger :: Parser Integer
parseAndReturnInteger = integer <* eof

parseAndReturnIntegerMain :: IO ()
parseAndReturnIntegerMain = do
  let parseAndReturn = parseString parseAndReturnInteger mempty
  print $ parseAndReturn "123"
  print $ parseAndReturn "123abc"

-- Exercise: Try try

type FractionOrNumber = Either Rational Integer

tryTryParser :: Parser FractionOrNumber
tryTryParser =
  try (Right <$> parseAndReturnInteger) <|> (Left <$> virtuousFraction)

tryTryMain :: IO ()
tryTryMain = do
  let parseAndReturn = parseString tryTryParser mempty
  print $ parseAndReturn "1/2"
  print $ parseAndReturn "12"
  print $ parseAndReturn "12/9"
  print $ parseAndReturn "12/0"

-- Chapter exercises

-- 1.

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

parseSemVer :: Parser SemVer
parseSemVer =
  let numberOrStringParser =
        try (NOSI <$> decimal <* notFollowedBy letter)
          <|> (NOSS <$> some alphaNum)
      releaseParser = try (char '-' >> sepBy numberOrStringParser (char '.')) <|> pure []
      metadataParser = try (char '+' >> sepBy numberOrStringParser (char '.')) <|> pure []
   in SemVer
        <$> decimal
          <*> (char '.' >> decimal)
          <*> (char '.' >> decimal)
          <*> releaseParser
          <*> (metadataParser <* eof)

isSuccessAndContains actual expected =
  case actual of
    Success value ->
      value `shouldBe` expected
    Failure message ->
      expectationFailure $ show message

isFailure actual =
  case actual of
    Success value ->
      expectationFailure $ "it's a success, fuckknuckle!" ++ show value
    Failure _ ->
      pure ()

isSuccess actual =
  case actual of
    Success _ ->
      pure ()
    Failure failure ->
      expectationFailure $ "it's a failure, fuckknuckle!" ++ show failure

mainParseSemVer :: IO ()
mainParseSemVer =
  let ps = parseString
      psv = ps parseSemVer mempty
   in hspec $ do
        describe "parseSemVer" $ do
          it "succeeds with no release or metadata" $ do
            psv "2.1.1" `isSuccessAndContains` SemVer 2 1 1 [] []
          it "succeeds with release and no metadata" $ do
            psv "1.0.0-x.7.z.92" `isSuccessAndContains` SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] []
          it "suceeds with release and metadata" $ do
            psv "1.0.0-gamma+002" `isSuccessAndContains` SemVer 1 0 0 [NOSS "gamma"] [NOSI 2]
          it "suceeds with release and multiple metadata" $ do
            psv "1.0.0-beta+oof.sha.41af286" `isSuccessAndContains` SemVer 1 0 0 [NOSS "beta"] [NOSS "oof", NOSS "sha", NOSS "41af286"]

-- 2. & 3.

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

base10Integer :: Parser Integer
base10Integer = read <$> ((maybeToList <$> (optional (char '-'))) <> some parseDigit)

mainParsePositiveInteger :: IO ()
mainParsePositiveInteger =
  let ps = parseString
      psd = ps parseDigit mempty
      psb = ps base10Integer mempty
   in hspec $ do
        describe "parsePositiveInteger" $ do
          it "" $ do
            psd "123" `isSuccessAndContains` '1'
          it "" $ do
            isFailure $ psd "abc"
        describe "base10Integer" $ do
          it "" $ do
            psb "123abc" `isSuccessAndContains` 123
          it "" $ do
            psb "-123-abc" `isSuccessAndContains` (-123)
          it "" $ do
            psb "-1-2-3-abc" `isSuccessAndContains` (-1)
          it "" $ do
            isFailure $ psb "abc"

-- 4.

type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone =
  let numberingPlanAreaParser =
        read
          <$> ( optional (char '(')
                  *> optional (string "1-")
                  *> count 3 digit
                  <* optional (char ')')
                  <* optional (char ' ')
              )
      exchangeParser = read <$> (optional (char '-') >> count 3 digit)
      lineNumberParser = read <$> (optional (char '-') >> count 4 digit)
   in PhoneNumber <$> numberingPlanAreaParser <*> exchangeParser <*> lineNumberParser

mainParsePhone :: IO ()
mainParsePhone =
  hspec $ do
    describe "parsePhone" $ do
      it "" $ do
        parseString parsePhone mempty "123-456-7890" `isSuccessAndContains` (PhoneNumber 123 456 7890)
      it "" $ do
        parseString parsePhone mempty "1234567890" `isSuccessAndContains` (PhoneNumber 123 456 7890)
      it "" $ do
        parseString parsePhone mempty "(123) 456-7890" `isSuccessAndContains` (PhoneNumber 123 456 7890)
      it "" $ do
        parseString parsePhone mempty "1-123-456-7890" `isSuccessAndContains` (PhoneNumber 123 456 7890)

-- 5.

sampleDiaryEntries :: String
sampleDiaryEntries =
  [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

type Activity = String

data LogEntry = LogEntry DiaryTime Activity

data DiaryDate = DiaryDate Integer Integer Integer

data DiaryTime = DiaryTime Integer Integer

data DiaryEntry = DiaryEntry DiaryDate [LogEntry]

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

skipEOL :: Parser ()
skipEOL =
  skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
  skipMany
    ( do
        string "--"
        skipMany (noneOf "\n")
        skipEOL
    )

parseDayHeader :: Parser DiaryDate
parseDayHeader = do
  string "# " <?> "dayHeaderStart"
  year <- decimal <?> "year"
  char '-'
  month <- decimal <?> "month"
  char '-'
  day <- decimal <?> "day"
  skipMany (noneOf "\n")
  skipEOL <?> "skipEOL"
  return $ DiaryDate year month day

parseLogEntry :: Parser LogEntry
parseLogEntry = do
  hour <- decimal <?> "hour"
  char ':'
  minute <- decimal <?> "minute"
  char ' '
  activity <- some (noneOf "\n") <?> "activity"
  skipEOL
  return $ LogEntry (DiaryTime hour minute) activity

parseDiaryEntry :: Parser DiaryEntry
parseDiaryEntry = do
  skipWhitespace <?> "parseDiaryEntry-skipWhitespace1"
  skipComments <?> "parseDiaryEntry-skipComments"
  skipWhitespace <?> "parseDiaryEntry-skipWhitespace2"
  day <- parseDayHeader <?> "parseDiaryEntry-parseDayHeader"
  logEntries <- some (parseLogEntry <?> "parseDiaryEntry-parseLogEntry")
  return $ DiaryEntry day logEntries

parseDiaryEntries :: Parser [DiaryEntry]
parseDiaryEntries = some parseDiaryEntry

mainParseDiaryEntries :: IO ()
mainParseDiaryEntries = hspec $ do
  describe "parseDiaryEntries" $ do
    it "parses the sample data into 2 diary entries" $ do
      (length <$> parseString parseDiaryEntries mempty sampleDiaryEntries) `isSuccessAndContains` 2
    it "parses the sample data into 2 diary entries, the first with 11 entries" $ do
      ((map (\(DiaryEntry _ list) -> length list)) <$> parseString parseDiaryEntries mempty sampleDiaryEntries) `isSuccessAndContains` [11, 10]

-- 6., 7. & 8.

data IPAddress = IPAddress Word32 deriving (Eq, Ord)

instance Show IPAddress where
  show (IPAddress a) =
    intercalate "." [first, second, third, fourth]
    where
      sectionToDecimal i = show $ shiftR a (24 - (i * 8)) .&. 255
      first = sectionToDecimal 0
      second = sectionToDecimal 1
      third = sectionToDecimal 2
      fourth = sectionToDecimal 3

parseIPAddress :: Parser IPAddress
parseIPAddress = do
  first <- decimal
  char '.'
  second <- decimal
  char '.'
  third <- decimal
  char '.'
  fourth <- decimal
  return $ IPAddress (fromInteger (first * (2 ^ 24) + second * (2 ^ 16) + third * (2 ^ 8) + fourth))

mainParseIPAddress :: IO ()
mainParseIPAddress = hspec $ do
  describe "parseIPAddress" $ do
    it "" $ do
      parseString parseIPAddress mempty "172.16.254.1" `isSuccessAndContains` IPAddress 2886794753
    it "" $ do
      parseString parseIPAddress mempty "204.120.0.15" `isSuccessAndContains` IPAddress 3430416399
  describe "show" $ do
    it "" $ do
      (show <$> parseString parseIPAddress mempty "172.16.254.1") `isSuccessAndContains` "172.16.254.1"
    it "" $ do
      (show <$> parseString parseIPAddress mempty "204.120.0.15") `isSuccessAndContains` "204.120.0.15"

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

type FillIndex = Int

data IPAddress6Sections
  = IPAddress6Abbreviated [Word16] [Word16]
  | IPAddress6Unabbreviated Word16 Word16 Word16 Word16 Word16 Word16 Word16 Word16
  deriving (Eq, Ord, Show)

word64ToFourCharHexSections :: Word64 -> [String]
word64ToFourCharHexSections a =
  let hexStringInSection i =
        showHex (shiftR a (48 - (i * 16)) .&. 65535) ""
   in [ hexStringInSection 0,
        hexStringInSection 1,
        hexStringInSection 2,
        hexStringInSection 3
      ]

abbreviateIPAddress6 :: [String] -> String
abbreviateIPAddress6 hexSections =
  let groupedSections = group (map hexStringToNumeric hexSections) :: [[Word16]]
      longestConsecutiveZeroesLength =
        maximum
          ( mapMaybe
              ( \x ->
                  if head x == 0
                    then Just (length x)
                    else Nothing
              )
              $ groupedSections
          )
      longestConsecutiveZeroesIndex =
        elemIndex (replicate longestConsecutiveZeroesLength 0) groupedSections
      abbreviatedHexSections =
        case longestConsecutiveZeroesIndex of
          Just index ->
            take index groupedSections ++ [[]] ++ drop (index + 1) groupedSections
          Nothing ->
            groupedSections
      abbreviatedHexSectionStrings =
        map (\x -> map (\y -> showHex y "") x) abbreviatedHexSections
   in case abbreviatedHexSectionStrings of
        xs
          | head xs == [] -> ":" ++ (intercalate ":" $ map (intercalate ":") xs)
          | last xs == [] ->
            (intercalate ":" $ map (intercalate ":") xs) ++ ":"
          | otherwise ->
            intercalate ":" $ map (intercalate ":") abbreviatedHexSectionStrings

instance Show IPAddress6 where
  show (IPAddress6 firstWord secondWord) =
    abbreviateIPAddress6
      ( word64ToFourCharHexSections firstWord ++ word64ToFourCharHexSections secondWord
      )

parseIPAddress6Sections :: Parser IPAddress6Sections
parseIPAddress6Sections = do
  sections <- sepBy (many hexDigit) (char ':')
  if length sections > 8
    then fail "More than 8 sections in the address"
    else case sections of
      a : (b : (c : (d : (e : (f : (g : (h : []))))))) ->
        return $
          IPAddress6Unabbreviated
            (hexStringToNumeric a)
            (hexStringToNumeric b)
            (hexStringToNumeric c)
            (hexStringToNumeric d)
            (hexStringToNumeric e)
            (hexStringToNumeric f)
            (hexStringToNumeric g)
            (hexStringToNumeric h)
      _ ->
        case elemIndex "" sections of
          Nothing ->
            fail "Less than 8 sections, and nothing collapsed"
          Just index ->
            return $
              IPAddress6Abbreviated
                (map hexStringToNumeric $ take index sections)
                (map hexStringToNumeric $ drop (index + 1) sections)

hexStringToNumeric :: (Eq a, Num a) => String -> a
hexStringToNumeric =
  (fst . head . readHex)

sectionAtFilledIndex :: [Word16] -> [Word16] -> FillIndex -> Word16
sectionAtFilledIndex sectionsBeforeBreak sectionsAfterBreak targetIndex =
  if targetIndex < length sectionsBeforeBreak
    then sectionsBeforeBreak !! targetIndex
    else
      if targetIndex > (7 - length sectionsAfterBreak)
        then sectionsAfterBreak !! (targetIndex - (7 - length sectionsAfterBreak + 1))
        else 0

shiftTo64 :: Word16 -> Int -> Word64
shiftTo64 = shift . toEnum . fromEnum

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 =
  ( \ipAddress6Sections ->
      case ipAddress6Sections of
        IPAddress6Abbreviated sectionsBeforeBreak sectionsAfterBreak ->
          let guaranteedSection = sectionAtFilledIndex sectionsBeforeBreak sectionsAfterBreak
           in IPAddress6
                ( shiftTo64 (guaranteedSection 0) 48
                    + shiftTo64 (guaranteedSection 1) 32
                    + shiftTo64 (guaranteedSection 2) 16
                    + shiftTo64 (guaranteedSection 3) 0
                )
                ( shiftTo64 (guaranteedSection 4) 48
                    + shiftTo64 (guaranteedSection 5) 32
                    + shiftTo64 (guaranteedSection 6) 16
                    + shiftTo64 (guaranteedSection 7) 0
                )
        IPAddress6Unabbreviated a b c d e f g h ->
          IPAddress6
            ( shiftTo64 a 48
                + shiftTo64 b 32
                + shiftTo64 c 16
                + shiftTo64 d 0
            )
            ( shiftTo64 e 48
                + shiftTo64 f 32
                + shiftTo64 g 16
                + shiftTo64 h 0
            )
  )
    <$> parseIPAddress6Sections

mainParseIPAddress6 :: IO ()
mainParseIPAddress6 = hspec $ do
  describe "hexStringToNumeric" $ do
    it "" $ do
      hexStringToNumeric "abcd" `shouldBe` 43981
  describe "parseIPAddress6Sections" $ do
    it "" $ do
      parseString parseIPAddress6Sections mempty "0:0:0:0:0:ffff:ac10:fe01"
        `isSuccessAndContains` IPAddress6Unabbreviated 0 0 0 0 0 65535 44048 65025
    it "" $ do
      parseString parseIPAddress6Sections mempty "0:0:0:0:0:ffff:ac10:f"
        `isSuccessAndContains` IPAddress6Unabbreviated 0 0 0 0 0 65535 44048 15
    it "" $ do
      parseString parseIPAddress6Sections mempty "FE80::0202:B3FF:FE1E:8329"
        `isSuccessAndContains` IPAddress6Abbreviated [65152] [514, 46079, 65054, 33577]
  describe "parseIPAddress6" $ do
    it "" $ do
      parseString parseIPAddress6 mempty "0:0:0:0:0:ffff:ac10:fe01"
        `isSuccessAndContains` IPAddress6 0 281473568538113
    it "" $ do
      parseString parseIPAddress6 mempty "0:0:0:0:0:ffff:cc78:f"
        `isSuccessAndContains` IPAddress6 0 281474112159759
    it "" $ do
      parseString parseIPAddress6 mempty "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
        `isSuccessAndContains` IPAddress6 18338657682652659712 144876050090722089
    it "" $ do
      parseString parseIPAddress6 mempty "FE80::0202:B3FF:FE1E:8329"
        `isSuccessAndContains` IPAddress6 18338657682652659712 144876050090722089
    it "" $ do
      parseString parseIPAddress6 mempty "2001:DB8::8:800:200C:417A"
        `isSuccessAndContains` IPAddress6 2306139568115548160 2260596444381562
  describe "show" $ do
    it "" $ do
      (show <$> parseString parseIPAddress6 mempty "0:0:0:0:0:ffff:ac10:fe01")
        `isSuccessAndContains` "::ffff:ac10:fe01"
    it "" $ do
      (show <$> parseString parseIPAddress6 mempty "0:0:0:0:0:ffff:cc78:f")
        `isSuccessAndContains` "::ffff:cc78:f"
    it "" $ do
      (show <$> parseString parseIPAddress6 mempty "FE80:0000:0000:0000:0202:B3FF:0000:8329")
        `isSuccessAndContains` "fe80::202:b3ff:0:8329"
    it "" $ do
      (show <$> parseString parseIPAddress6 mempty "FE80::0202:B3FF:FE1E:8329")
        `isSuccessAndContains` "fe80::202:b3ff:fe1e:8329"
    it "" $ do
      (show <$> parseString parseIPAddress6 mempty "FE80:ABCD:DEFA:1111:0000:0000:0000:0000")
        `isSuccessAndContains` "fe80:abcd:defa:1111::"
    it "" $ do
      (show <$> parseString parseIPAddress6 mempty "2001:DB8::8:800:200C:417A")
        `isSuccessAndContains` "2001:db8::8:800:200c:417a"
