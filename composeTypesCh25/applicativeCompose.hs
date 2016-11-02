{-# LANGUAGE InstanceSigs #-}

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose a) = Compose $ (fmap . fmap) f a

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose g) = Compose $ ((\innerF -> (innerF <*>)) <$> f) <*> g

instance (Foldable f, Foldable g) =>
         Foldable (Compose f g) where
  foldMap f (Compose t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) =>
         Traversable (Compose f g) where
  traverse f (Compose ts) = Compose <$> traverse (traverse f) ts
