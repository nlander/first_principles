module Bifunctor where

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first = flip bimap id

  second :: (c -> d) -> p a c -> p a d
  second = bimap id

data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where
  first f (Deux a b) = Deux (f a) b
  second f (Deux a b) = Deux a $ f b

data Const a b = Const a deriving (Eq, Show)

instance Bifunctor Const where
  bimap f _ (Const a) = Const $ f a

data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a $ f b

data SemiDrei a b c = SemiDrei a deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data OneOf a b = 
    This a
  | That b deriving (Eq, Show)

instance Bifunctor OneOf where
  bimap f _ (This a) = This $ f a
  bimap _ g (That b) = That $ g b
