{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter22 where

import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Data.Maybe
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Test.Hspec
import Web.Scotty

boop = (* 2)

doop = (+ 10)

bip :: Integer -> Integer
bip = boop . doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- Short Exercise: Warming Up

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled =
  liftA2 (,) cap rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (,) rev cap

monadicTupled :: [Char] -> ([Char], [Char])
monadicTupled = do
  cap >>= (rev >>= return . (,))

mainWarmingUp = hspec $ do
  describe "composed" $ do
    it "composed julie == EILUJ" $ do
      composed "julie" `shouldBe` "EILUJ"
  describe "fmapped" $ do
    it "fmapped chris == SIRHC" $ do
      fmapped "chris" `shouldBe` "SIRHC"
  describe "tupled" $ do
    it "tupled Julie == (JULIE, eiluJ)" $ do
      tupled "Julie" `shouldBe` ("JULIE", "eiluJ")
  describe "tupled'" $ do
    it "tupled' Julie == (eiluJ, JULIE)" $ do
      tupled' "Julie" `shouldBe` ("eiluJ", "JULIE")
  describe "monadicTupled" $ do
    it "monadicTupled Julie == (JULIE, eiluJ)" $ do
      monadicTupled "Julie" `shouldBe` ("JULIE", "eiluJ")

newtype Reader r a
  = Reader {runReader :: r -> a}

ask :: Reader a a
ask = Reader id

-- Exercise: Reading Comprehension

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 = ((<*>) .) . (<$>)

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap = (Reader .) . (. runReader) . (<$>)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader <$> const

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rab) (Reader ra) = Reader $ \r -> rab r (ra r)

-- Exercise: Reader Monad

instance Monad (Reader r) where
  return =
    pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader ra) aRb = Reader $ \r -> runReader (aRb (ra r)) r

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person
  = Person
      { humanName :: HumanName,
        dogName :: DogName,
        address :: Address
      }
  deriving (Eq, Show)

data Dog
  = Dog
      { dogsName :: DogName,
        dogsAddress :: Address
      }
  deriving (Eq, Show)

getDogRM :: Reader Person Dog
getDogRM = nameReader >>= dogReader
  where
    nameReader = Reader $ dogName
    dogReader name = Reader $ (Dog name) . address

-- Chapter Exercises

x = [1, 2, 3]

y = [4, 5, 6]

z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num a => (a, a) -> a
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (> 3) (< 8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(> 3), (< 8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

mainChapterExercises :: IO ()
mainChapterExercises = do
  -- print $ sequenceA [Just 3, Just 2, Just 1]
  -- print $ sequenceA [x, y]
  -- print $ sequenceA [xs, ys]
  -- print $ summed <$> ((,) <$> xs <*> ys)
  -- print $ fmap summed ((,) <$> xs <*> zs)
  -- print $ bolt 7
  -- print $ fmap bolt z
  -- print $ sequenceA [(> 3), (< 8), even] 7
  print $ foldr (&&) True (sequA 7)
  print $ foldr (&&) True (sequA 4)
  print $ fromMaybe [] ((Just sequA) <*> s')
  print $ fromMaybe False ((Just bolt) <*> ys)

-- Shawty refactoring exercise

alphaNum :: String
alphaNum = ['A' .. 'Z'] ++ ['0' .. '9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- Right of arrow is IO Int, so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

shortyGen :: IO String
shortyGen =
  replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection -> BC.ByteString -> BC.ByteString -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

getURI :: R.Connection -> BC.ByteString -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat
    [ "<a href=\"",
      shorty,
      "\">Copy and paste your short URL</a>"
    ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat
    [ TL.pack (show resp),
      " shorty is: ",
      TL.pack (linkShorty shawty)
    ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat
    [ uri,
      " wasn't a url, did you forget http://?"
    ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

type ScottyDatabaseReaderT = ReaderT R.Connection ScottyM ()

app :: ScottyDatabaseReaderT
app = ReaderT $ \rConn -> do
  get "/" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Just _ -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
            uri' = encodeUtf8 (TL.toStrict uri)
        resp <- liftIO (saveURI rConn shorty uri')
        html (shortyCreated resp shawty)
      Nothing -> text (shortyAintUri uri)
  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI rConn short)
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html (shortyFound tbs)
          where
            tbs :: TL.Text
            tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 $ runReaderT app rConn
