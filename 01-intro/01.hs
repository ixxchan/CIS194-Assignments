{-# OPTIONS_GHC -Wall #-}


-- Part 1 : Validating Credit Card Numbers
-- Exercise 1

-- toDigits converts positive Integers to a list of digits. 
-- (For 0 or negative inputs, toDigits returns the empty list.)
toDigits :: Integer -> [Integer]
toDigits n 
    | n > 0     = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise = []

-- toDigitsRev does the same as toDigits, but with the digits reversed.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
    | n > 0     = n `mod` 10 : toDigitsRev (n `div` 10) 
    | otherwise = []


-- Exercise 2

-- doubleEveryOther doubles every other number beginning from the right, 
-- that is, the second-to-last, fourth-to-last, ... numbers are doubled.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse

-- doubleEveryOther' does the same as doubleEveryOther, but from the left.
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' []     = []
doubleEveryOther' [x]    = [x]
doubleEveryOther' (x:y:xs) = [x, 2*y] ++ doubleEveryOther' xs


-- Exercise 3

-- calculates the sum of all digits.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)


-- Exercise 4

-- indicates whether an Integer could be a valid credit card number.
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0


-- Part 2 : The Towers of Hanoi
-- Exercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src dst tmp 
    | n > 0  = hanoi (n-1) src tmp dst ++ (src, dst) : hanoi (n-1) tmp dst src
    | otherwise = []

-- Exercise 6

-- Hanoi problem with four pegs
-- There should be length(hanoi 15 "a" "b" "c") = 32767,
-- and length(hanoi4 15 "a" "b" "c" "d") = 129
-- My solution needs memorization to speed up :)
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n src dst tmp1 tmp2 
    | n <= 0    = []
    | n == 1    = [(src, dst)]
    | otherwise = shortest [ candidate i | i <- [1..(n-1)] ]
                where candidate i = hanoi4 i src tmp1 dst tmp2 ++ 
                                    hanoi (n-i) src dst tmp2 ++ 
                                    hanoi4 i tmp1 dst src tmp2

-- returns the shortest element in a nested list
shortest :: [[a]] -> [a]
shortest []   = error "empty list"
shortest [x]  = x
shortest (x:xs) = if length x < length y then x else y
                    where y = shortest xs