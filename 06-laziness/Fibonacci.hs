{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = [0, 1] ++ [ fibs2 !! (i - 2) + fibs2 !! (i - 1) | i <- [2 ..] ]

-- Exercise 3
data Stream a = Scons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Scons a s) = a : streamToList s

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat i = Scons i (streamRepeat i)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Scons i s) = Scons (f i) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f i = Scons i (streamFromSeed f (f i))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = streamMap (f 0) $ streamFromSeed (+ 1) 1
  where
    f :: Integer -> Integer -> Integer
    f acc n = if odd n then acc else f (acc + 1) (div n 2)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Scons i s) s' = Scons i (interleaveStreams s' s)

-- credit to Eric D.Burgess - http://oeis.org/A001511
ruler2 :: Stream Integer
ruler2 = f 0 where f n = interleaveStreams (streamRepeat n) (f (n + 1))

-- Exercise 6
x :: Stream Integer
x = Scons 0 (Scons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger a = Scons a (streamRepeat 0)
    negate = streamMap negate
    (+) (Scons a0 a') (Scons b0 b') = Scons (a0 + b0) (a' + b')
    (*) (Scons a0 a') b@(Scons b0 b') =
        Scons (a0 * b0) (streamMap (* a0) b' + a' * b)

instance Fractional (Stream Integer) where
    (/) (Scons a0 a') (Scons b0 b') = q
        where q = Scons (a0 `div` b0) (streamMap (`div` b0) (a' - q * b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer
    deriving Show

instance Num Matrix where
    (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) = Matrix
        (a11 * b11 + a12 * b21)
        (a11 * b12 + a12 * b22)
        (a21 * b11 + a22 * b21)
        (a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer
fib4 0 = 1
fib4 n = case f ^ n of
    Matrix _ a _ _ -> a
    where f = Matrix 1 1 1 0
    
