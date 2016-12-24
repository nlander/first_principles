module Main where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.IO.Class
import Data.Function
import Data.Profunctor

english :: Int -> String
english 0 = "zero"
english 1 = "one"
english 2 = "two"
english 3 = "three"
english 4 = "four"
english 5 = "five"
english 6 = "six"
english 7 = "seven"
english 8 = "eight"
english 9 = "nine"
english 10 = "ten"
english 11 = "eleven"
english 12 = "twelve"
english 13 = "thirteen"
english 14 = "fouteen"
english 15 = "fifteen"
english 16 = "sixteen"
english 17 = "seventeen"
english 18 = "eighteen"
english 19 = "nineteen"
english 20 = "twenty"
english 30 = "thirty"
english 40 = "forty"
english 50 = "fifty"
english 60 = "sixty"
english 70 = "seventy"
english 80 = "eighty"
english 90 = "ninety"
english x | x > 20 && x < 100 = toninety
  where
    digit = english ( mod x 10 )
    tens = english ( x - mod x 10 )
    toninety = mconcat [ tens, "-", digit ]
english x | x > 99 && x < 1000 = toninehundred
  where
    tens = english ( mod x 100 )
    hundreds = english ( x - mod x 100 & flip div 100 )
    toninehundred = case tens of
      "zero" -> mconcat
        [ hundreds
        , " "
        , "hundred" ]
      _ -> mconcat
        [ hundreds
        , " "
        , "hundred"
        , " "
        , tens ]

groovSnrrgOut :: Int -> String
groovSnrrgOut x
  | x `mod` 15 == 0 = "GroovSnrrg"
  | x `mod` 5  == 0 = "Snrrg"
  | x `mod` 3  == 0 = "Groovy"
  | otherwise       = english x

withStuff :: (s -> a) -> (s -> s) -> Kleisli ((,) a) s s
withStuff out step = Kleisli $ \s -> (out s, step s)

countBy :: Int -> Kleisli ((,) String) Int Int
countBy x = withStuff english (+x)

groovSnrrgState :: Kleisli ((,) String) Int Int
groovSnrrgState = withStuff groovSnrrgOut (+1)

groovSnrrg :: StateT Int IO String
groovSnrrg = fmap head . replicateM 100 $
  state ( groovSnrrgState & runKleisli ) >>= liftIO . print'

counting :: StateT Int IO String
counting = fmap head . replicateM counts $
  state ( countBy stepSize
        & rmap (+1)
        & runKleisli ) >>= liftIO . print'

print' = liftA2 (*>) putStrLn return

countStart = 0

stepSize = 1

counts = 30

main :: IO ()
main = do
  result <- execStateT counting countStart
  putStrLn ( english result )
