import Control.Applicative
import Control.Monad
import Data.Char

hurr :: Num n => n -> n
hurr = (*2)

durr :: Num n => n -> n
durr = (+10)

m :: Num n => n -> n
m = hurr . durr

m' :: Integer -> Integer
m' = fmap hurr durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

hurrDurr :: Integer -> Integer
hurrDurr = do
    a <- hurr
    b <- durr
    return (a + b)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    a <- cap
    b <- rev
    return $ (,) b a

tupled'' :: [Char] -> ([Char], [Char])
tupled'' chars = ((return . cap) chars >>= (,)) . rev $ chars

