{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Web.Scotty.Internal.Types ( ActionT (..) )

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding ( get )
import Data.Monoid ( mconcat )

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance MonadTrans ( EitherT e ) where
  lift = EitherT . liftM Right

instance Functor m => Functor (EitherT e m) where
  fmap f = EitherT . (fmap . fmap) f . runEitherT

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT fab) <*> (EitherT mma) = EitherT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f = EitherT $ do
    e <- ma
    case e of
      (Left err) -> return (Left err)
      Right a -> runEitherT ( f a )

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    ( ActionT
      . ( ExceptT . liftM Right )
      . ReaderT . const
      . \m -> StateT ( \s -> do
                          a <- m
                          return (a, s) )
      ) ( putStrLn "hello" )
    html $ mconcat ["<h1>Scotty, ", beam, " me up?</h1>"]
