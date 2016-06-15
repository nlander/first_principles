f :: Int -> Bool
f 1 = True
f _ = False

data Identity a =
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'
