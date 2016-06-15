myTail          :: [a] -> [a]
myTail []       = []
myTail (_ : xs) = xs

safeTail        :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

safeHead [] = Nothing
safeHead (x : _) = Just x
