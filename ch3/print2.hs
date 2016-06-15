--print2.hs
module Print2 where

main :: IO()
main = do
  putStr "Count to four for me:"
  putStr "one, two"
  putStr ", three, and"
  putStr " four!"
