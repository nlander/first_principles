{-# LANGUAGE InstanceSigs #-}

module RandomExample where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 1
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= n = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
  where go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
        go sum count rolls gen
          | sum >= n = (count, reverse rolls)
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
                freshRolls = intToDie die : rolls
            in go (sum + die) (count + 1) freshRolls  nextGen

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ (\tup -> (f (fst tup), snd tup)) . g

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \x -> (a, x)

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> (fst (f s)
                                      ((fst . g)
                                        ((snd . f) s)),
                                      ((snd . g)
                                        ((snd . f) s)))

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = Moi $ \s -> (runMoi (g (fst (f s)))) s
