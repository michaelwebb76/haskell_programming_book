{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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
import System.Environment (getArgs)
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
  let currentValue = m M.!? k
      newValue = case currentValue of
        Nothing ->
          1
        Just x ->
          x + 1
  (M.insert k newValue m, newValue)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key" :: Handler Text
    p <- lift $ R.asks prefix
    let key' = mappend p unprefixed
    ioRefCounts <- lift $ R.asks counts
    counts <- lift (R.ReaderT $ \_ -> readIORef ioRefCounts)
    let (newMap, newInteger) = bumpBoomp key' counts
    lift (R.ReaderT $ \_ -> writeIORef ioRefCounts newMap)
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
  | TwoFinger
  | ThreeFinger
  | FourFinger
  | FiveFinger

data Call
  = CallOne
  | CallTwo
  | CallThree
  | CallFour
  | CallFive
  | CallSix
  | CallSeven
  | CallEight
  | CallNine
  | CallTen

data Turn = Turn Call Hand

data Game = Game {playerTurn :: Turn, computerTurn :: Turn}

data Score = Score {playerScore :: Integer, computerScore :: Integer}
