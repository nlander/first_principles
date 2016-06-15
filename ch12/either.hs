lefts' :: [Either a b] -> [a]
lefts' = foldr consLeft [] where
         consLeft (Left x)  acc = x : acc
         consLeft (Right _) acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr consRight [] where
            consRight (Right x) acc = x : acc
            consRight (Left _)  acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right y) = g y

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\x -> Just $ f x)
