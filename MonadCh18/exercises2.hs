--1
j :: Monad m => m (m a) -> m a
j nested = nested >>= fmap id

--2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

--3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = a1 mb $ ma >>= (return . f)

--4
a1 :: Monad m => m a -> m (a -> b) -> m b
a1 ma mf = mf >>= \f -> ma >>= return . f

--5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = f x >>= \b -> fmap (b:) (meh xs f)

--6
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
