newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

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
