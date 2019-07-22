{-# OPTIONS_GHC -Wall #-}
module Golf where

-- Exercise 1 Hopscotch
-- The nth list in the output should contain every 
-- nth element from the input list.
-- apply takeEveryNth to xs with 1..length xs 
skips :: [a] -> [[a]]
skips xs = [ takeEveryNth i xs | i <- [1 .. length xs] ]

-- Returns every nth element from the input list.
takeEveryNth :: Int -> [a] -> [a]
takeEveryNth n = map snd . filter (\x -> fst x `mod` n == 0) . zip [1 ..]


-- Exercise 2 Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : xs) | y > x && y > z = y : localMaxima (y : z : xs)
                             | otherwise      = localMaxima (y : z : xs)
localMaxima _ = []


-- Exercise 3 Histogram
-- takes as input a list of Integer s between 0 and 9 (inclusive),
-- and outputs a vertical histogram showing how many of each number
-- were in the input list
histogram :: [Integer] -> String
histogram xs =
    concat [ nthLine n counts | n <- [m, m - 1 .. 1] ]
        ++ "==========\n0123456789\n"
  where
    counts = count xs
    m      = maximum counts

-- return the nth line of the histogram
nthLine :: Integer -> [Integer] -> String
nthLine n counts = map (\m -> if m < n then ' ' else '*') counts ++ "\n"

-- counts the occurences of the digits
count :: [Integer] -> [Integer]
count = foldl
    (\res n ->
        take (fromInteger n) res
            ++ ((res !! fromInteger n) + 1)
            :  drop (fromInteger n + 1) res
    )
    (replicate 10 0)
