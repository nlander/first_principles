isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee defaultValue _ Nothing  = defaultValue
mayybee _            f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe fallBack Nothing  = fallBack
fromMaybe _        (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just (head xs)

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (m:ms) = case m of
  Nothing  -> catMaybes ms
  (Just a) -> a : catMaybes ms

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = case length xs == length (catMaybes xs) of
                    True  -> Just $ catMaybes xs
                    False -> Nothing
