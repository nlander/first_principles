data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z

  foldl f z (Identity x) = f z x

  foldMap f (Identity x) = f x

data Optional a = Yep a | Nada

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

--Chapter Exercises
--1
data Constant a b =
  Constant a

instance Foldable (Constant a) where
  foldr _ acc _ = acc

--2
data Two a b =
  Two a b

instance Foldable (Two a) where
  foldr f acc (Two a b) = f b acc

--3
data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f acc (Three a b c) = f c acc

--4
data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldr f acc (Three' a b1 b2) = f b1 $ f b2 acc

--5
data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldr f acc (Four' a b1 b2 b3) = f b1 $ f b2 $ f b3 acc

--Thinking Cap!!!!!!!
filterF :: (Applicative f, Foldable f, Monoid (f a)) =>
          (a -> Bool) -> f a -> f a
filterF f = foldr dropIf mempty where
              dropIf val acc = case f val of
                                 True -> mappend (pure val) acc
                                 False -> acc

filterF' :: (Applicative f, Foldable f, Monoid (f a)) =>
           (a -> Bool) -> f a -> f a
filterF' p = foldMap (\x -> if p x then pure x else mempty)

filterF'' :: (Applicative g, Foldable f, Monoid (g a)) =>
            (a -> Bool) -> f a -> g a
filterF'' p = foldMap (\x -> if p x then pure x else mempty)
