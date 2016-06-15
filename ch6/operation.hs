add ::  Int -> Int -> Int
add x y = x + y

addWeird :: Int -> Int -> Int
addWeird x y =
  if x > 1
  then x + y
  else x

check' ::  Int -> Int -> Bool
check' a a' = a == a'
