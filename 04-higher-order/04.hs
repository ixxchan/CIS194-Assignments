{-# OPTIONS_GHC -Wall #-}

module HW4 where
import           Data.List

-- Exercise 1 Wholemeal programming
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate
    (\n -> if even n then n `div` 2 else 3 * n + 1)


-- Exercise 2 Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- generates a balanced binary tree from a list of values using foldr
foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

height :: Tree a -> Integer
height Leaf           = -1
height (Node n _ _ _) = n

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node h t1 y t2) | h1 < h2   = Node h t1' y t2
                              | h1 > h2   = Node h t1 y (insertTree x t2)
                              | otherwise = Node (1 + h') t1' y t2
  where
    h1  = height t1
    h2  = height t2
    t1' = insertTree x t1
    h'  = height t1'


-- Exercise 3 More folds!
xor :: [Bool] -> Bool
xor = foldr (\b acc -> if b then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x l -> f x : l) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse


-- Exercise 4 Finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 2 * x + 1) . sieveOut

generateSieve :: Integer -> [Integer]
generateSieve n =
    [ x | j <- [1 .. n], i <- [1 .. j], let x = i + j + 2 * i * j, x <= n ]

sieveOut :: Integer -> [Integer]
sieveOut n = [1 .. n] \\ generateSieve n
