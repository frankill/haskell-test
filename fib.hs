module Frank
(
fib1,
fib2,
fib3,
fib4,
fib5,
Seq(..)
) where

import Data.List(unfoldr)

-- base
fib1 :: Integer -> Integer
fib1 0 =1
fib1 1 = 1
fib1 x = fib1 (x-1) + fib1 (x-2)

-- list
f1 :: [Integer]
f1 = 1:1:zipWith (+) f1 (tail f1)

fib2 :: Int -> Integer
fib2 n = f1 !! n

-- lazy function
f2 :: Integer -> Integer -> [Integer]
f2 a b = a:f2 b (a+b)

fib3 :: Int -> Integer
fib3 n = (f2 1 1) !! n

-- Tail recursion
data Seq = Seq {id :: Int,fir :: Integer,las :: Integer}  deriving (Show)

next :: Seq -> Int -> Seq
next (Seq currentN x y) n
    | bool = next (Seq (currentN + 1) y (x + y) ) n
    | otherwise = Seq currentN x y
    where bool = currentN < n

fib4 :: Int -> Integer
fib4 n = las $ next (Seq 0 0 1) n

-- unfoldr function
f3 :: [Integer]
f3 = unfoldr (\(f,l) -> Just (f, (l, f+l ) ) ) (0,1)

fib5 :: Int -> Integer
fib5 n = last . take (n+2) $ f3

-- other
fib6 :: Int -> Integer
fib6 n = last . take (n+2) $ [ fst t | t <- iterate (\(a,b) -> (b,a+b)) (0,1)]

fib7 :: Int -> Integer
fib7 n = last . take (n+2) . map fst $ iterate f (0,1) where f (a,b) = (b,a+b)

fib8 :: Int -> Integer
fib8 n = last . take (n+2) . map head $ iterate (\(x:y:xs) -> (x+y):x:xs) [0,1]
