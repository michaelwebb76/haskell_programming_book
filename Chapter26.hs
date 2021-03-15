{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Chapter26 where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Reader as R
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Functor.Identity
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Console.ANSI
import System.Environment (getArgs)
import System.Random
import Web.Scotty.Trans

-- import Prelude

-- MaybeT

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . (pure . pure)

  (<*>) (MaybeT fmamb) (MaybeT ma) = MaybeT $ (<*>) <$> fmamb <*> ma

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (>>=) (MaybeT ma) faToMaybeTmb =
    MaybeT $
      ma
        >>= ( \maybeA -> runMaybeT $ case maybeA of
                Just a -> faToMaybeTmb a
                Nothing -> MaybeT $ pure Nothing
            )

-- EitherT

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance (Applicative m) => Applicative (EitherT e m) where
  pure = EitherT . (pure . pure)

  (<*>) (EitherT f) (EitherT ema) = EitherT $ (<*>) <$> f <*> ema

instance (Monad m) => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (>>=) (EitherT ema) faToEitherTEmb =
    EitherT $
      ema
        >>= ( \eitherA -> runEitherT $ case eitherA of
                Left e -> EitherT $ pure (Left e)
                Right a -> faToEitherTEmb a
            )

swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT aToMc bToMc (EitherT amb) = do
  eitherAB <- amb
  case eitherAB of
    Left a -> aToMc a
    Right b -> bToMc b

-- ReaderT

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT e m) where
  pure = ReaderT . (pure . pure)

  (<*>) (ReaderT f) (ReaderT rma) = ReaderT $ (<*>) <$> f <*> rma

instance (Monad m) => Monad (ReaderT e m) where
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (>>=) (ReaderT rma) fAToReaderTRmb =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (fAToReaderTRmb a) r

-- StateT

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ fmap (Data.Bifunctor.first f) . sma

instance (Monad m) => Applicative (StateT s m) where
  pure = \a -> StateT (\s -> pure (a, s))
  (<*>) (StateT smAtoB) (StateT sma) =
    StateT
      ( \s -> do
          (a, as) <- sma s
          (f, fs) <- smAtoB as
          return (f a, fs)
      )

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: (Monad m) => StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (>>=) (StateT sma) f =
    StateT
      ( \s -> do
          (a, as) <- sma s
          let stateSmb = f a
          runStateT stateSmb as
      )

-- Exercise: Wrap it up

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = (MaybeT . ExceptT . ReaderT . (\f _ -> return $ f ())) (const (Right (Just (1 :: Int))))

-- Exercise: Lift more

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadTrans (StateT s) where
  lift x = StateT $ \s -> fmap (,s) x

-- Exercises: Some instances

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO

-- Chapter Exercises

-- Write the code

type Reader r = ReaderT r Identity

rDec :: Num a => Reader a a
rDec = ReaderT (Identity . flip (-) 1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT (Identity . show)

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc =
  ReaderT
    ( \a -> do
        putStrLn ("Hi: " ++ show a)
        return (a + 1)
    )

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum =
  StateT
    ( \a -> do
        putStrLn ("Hi: " ++ show a)
        return (show a, a + 1)
    )

-- Fix the code

isValid :: String -> Bool
isValid v = '!' `elem` v

instance (Applicative a, Monad a) => Alternative (MaybeT a) where
  empty = MaybeT $ pure Nothing

  (<|>) (MaybeT a) (MaybeT b) =
    MaybeT $
      a
        >>= ( \a1 ->
                if isNothing a1
                  then b
                  else a
            )

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- MaybeT (Just <$> getLine)
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)

-- Hit counter

data Config = Config {counts :: IORef (M.Map Text Integer), prefix :: Text}

type Scotty = ScottyT Text (R.ReaderT Config IO)

type Handler = ActionT Text (R.ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = do
  let newValue = maybe 1 (+ 1) (m M.!? k)
  (M.insert k newValue m, newValue)

app :: Scotty ()
app =
  get "/:key" $ do
    key' <- mappend <$> lift (R.asks prefix) <*> param "key"
    ioRefCounts <- lift $ R.asks counts
    counts <- liftIO (readIORef ioRefCounts)
    let (newMap, newInteger) = bumpBoomp key' counts
    liftIO (writeIORef ioRefCounts newMap)
    html $ mconcat ["<h1>Success! Count was: ", TL.pack $ show newInteger, "</h1>"]

hitCounter :: IO ()
hitCounter = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR = (\(R.ReaderT a) -> a config)
  scottyT 3000 runR app

-- Morra

data Hand
  = OneFinger
  | TwoFingers
  | ThreeFingers
  | FourFingers
  | FiveFingers
  deriving (Show)

type PlayerName = String

data Turn = Turn PlayerName Hand deriving (Show)

data Game = Game {player1Turn :: Turn, player2Turn :: Turn} deriving (Show)

data Score = Score {player1Score :: Integer, player2Score :: Integer} deriving (Show)

type MorraStateT = StateT Score IO Game

morra :: IO ()
morra = do
  putStr "Select 1 to play Computer, 2 to play another Human:"
  gameChoice <- getLine
  let score = Score 0 0
  putStrLn "-- Player 1 is Human"
  case gameChoice of
    "1" -> do
      putStrLn "-- Player 2 is Computer"
      runStateT humanVsComputerRound score
    "2" -> do
      putStrLn "-- Player 2 is Human"
      runStateT humanVsHumanRound score
    _ ->
      error "invalid game mode selected"
  putStrLn "All done!"

updateScoreFromGame :: Game -> MorraStateT
updateScoreFromGame game@(Game (Turn player1Name player1Hand) (Turn player2Name player2Hand)) = StateT $ \score ->
  let pHandInt = handToInt player1Hand
      cHandInt = handToInt player2Hand
      pScore = player1Score score
      cScore = player2Score score
      updatedScore
        | pHandInt > cHandInt = Score (pScore + 1) cScore
        | pHandInt < cHandInt = Score pScore (cScore + 1)
        | otherwise = score
   in do
        liftIO $ putStr (player1Name ++ " played ")
        liftIO $ print player1Hand
        liftIO $ putStr (player2Name ++ " played ")
        liftIO $ print player2Hand
        liftIO $ print score
        if player1Score score > player2Score score
          then putStrLn (player1Name ++ " is winning")
          else
            if player2Score score > player1Score score
              then putStrLn (player2Name ++ " is winning")
              else putStrLn "It's neck and neck!"
        return (game, updatedScore)

humanVsComputerRound :: MorraStateT
humanVsComputerRound = do
  liftIO $ putStr "Player 1: "
  playerHand <- liftIO $ intToHand . read <$> getLine
  computerHand <- liftIO randomComputerHand
  let game = Game (Turn "Player" playerHand) (Turn "Computer" computerHand)
  updateScoreFromGame game
  humanVsComputerRound

humanVsHumanRound :: MorraStateT
humanVsHumanRound = do
  liftIO clearScreen
  liftIO $ putStr "Player 1: "
  player1Hand <- liftIO $ intToHand . read <$> getLine
  liftIO $ putStr "Player 2: "
  player2Hand <- liftIO $ intToHand . read <$> getLine
  let game = Game (Turn "Player 1" player1Hand) (Turn "Player 2" player2Hand)
  updateScoreFromGame game
  liftIO $ putStr "Hit enter/return to continue"
  liftIO getLine
  humanVsHumanRound

intToHand :: Int -> Hand
intToHand = \case
  1 -> OneFinger
  2 -> TwoFingers
  3 -> ThreeFingers
  4 -> FourFingers
  5 -> FiveFingers
  _ -> error "invalid integer"

handToInt :: Hand -> Int
handToInt = \case
  OneFinger -> 1
  TwoFingers -> 2
  ThreeFingers -> 3
  FourFingers -> 4
  FiveFingers -> 5

randomComputerHand :: IO Hand
randomComputerHand =
  getStdRandom
    ( \stdGen ->
        let (int, stdgen) = randomR (1, 5) stdGen
         in (intToHand int, stdgen)
    )
