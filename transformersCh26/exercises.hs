import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f = MaybeT . (fmap . fmap) f . runMaybeT

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  (MaybeT mmab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> mmab <*> mma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f = MaybeT $ ma >>= \maybe -> case maybe of
                                                  Nothing -> return Nothing
                                                  (Just a) -> ( runMaybeT . f ) a

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO io = MaybeT . ( fmap Just ) . liftIO $ io

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance MonadTrans ( EitherT e ) where
  lift = EitherT . liftM Right

instance Functor m => Functor (EitherT e m) where
  fmap f = EitherT . (fmap . fmap) f . runEitherT

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . Right
  (EitherT fab) <*> (EitherT mma) = EitherT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f = EitherT $ do
    e <- ma
    case e of
      (Left err) -> return (Left err)
      Right a -> runEitherT ( f a )

swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT e = EitherT $ swapEither <$> runEitherT e

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT fac fbc (EitherT meab) =
  meab >>= ( \eab -> case eab of
               (Left a)  -> fac a
               (Right b) -> fbc b )

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap fab (ReaderT frma) = ReaderT ( \r -> fab <$> frma r )

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . const . pure
  (ReaderT frmab) <*> (ReaderT frma) = ReaderT ( \r -> frmab r <*> frma r )

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT frma) >>= farmb = ReaderT ( \r -> frma r >>= ( \a -> ( (runReaderT . farmb) a ) r ) )

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = ReaderT . const . liftIO

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap fab (StateT fsmas) = StateT $
    \s -> fmap
            (\tup -> (fab (fst tup), snd tup))
            ( fsmas s )

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a,s)
  (StateT fsmabs) <*> (StateT fsmas) =
    StateT $ \s -> 
      fsmas s >>= 
      ( \(a,s0) ->
          fmap
            ( \(f, s1) -> (f a, s1) )
            ( fsmabs s0 ) )

instance Monad m => Monad (StateT s m) where
  return = pure
  (StateT fsmas) >>= fasmbs = 
    StateT $ ( \s -> fsmas s >>= ( \(a,s0) -> (runStateT . fasmbs) a s0 ) )

instance MonadTrans (StateT s) where
  lift m = StateT $
    \s -> liftM ( \a -> (a, s) ) m

instance MonadIO m => MonadIO (StateT s m) where
  liftIO io = StateT $
    \s -> liftM ( \a -> (a, s) ) ( liftIO io )

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT
         $ ExceptT
         $ ReaderT
         $ return . const ( Right ( Just 1 ) )
