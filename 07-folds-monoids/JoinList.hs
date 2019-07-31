{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module JoinList where

import           Sized
import           Scrabble
import Buffer
import Editor

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l1 +++ l2 = Append (tag l1 <> tag l2) l1 l2

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single x _  ) = x
tag (Append x _ _) = x

-- Exercise 2
-- (indexJ i jl) == (jlToList jl !!? i)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ i (Append sz l r) | i < 0 || i >= sizeRoot = Nothing
                         | i < sizeL              = indexJ i l
                         | otherwise              = indexJ (i - sizeL) r
 where
  sizeRoot = getSize . size $ sz
  sizeL    = getSize . size . tag $ l

-- jlToList (dropJ n jl) == drop n (jlToList jl).
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl | i <= 0  = jl
dropJ _ Empty        = Empty
dropJ _ (Single _ _) = Empty
dropJ i (Append sz l r) | i >= sizeRoot = Empty
                        | i <= sizeL    = (dropJ i l) +++ r
                        | otherwise     = dropJ (i - sizeL) r
 where
  sizeRoot = getSize . size $ sz
  sizeL    = getSize . size . tag $ l

-- jlToList (takeJ n jl) == take n (jlToList jl)
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i _ | i <= 0      = Empty
takeJ _ Empty           = Empty
takeJ 1 jl@(Single _ _) = jl
takeJ _ (   Single _ _) = Empty
takeJ i jl@(Append sz l r) | i >= sizeRoot = jl
                           | i <= sizeL    = takeJ i l
                           | otherwise     = l +++ takeJ (sizeRoot - i) r
 where
  sizeRoot = getSize . size $ sz
  sizeL    = getSize . size . tag $ l

-- For testing

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a    ) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x : _ ) !!? 0  = Just x
(_ : xs) !!? i  = xs !!? (i - 1)

testJL :: JoinList Size String
testJL = Append
  (Size 3)
  (Append (Size 2) (Single (Size 1) "hi") (Single (Size 1) "bye"))
  (Single (Size 1) "tschau")

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  
  fromString = foldr (\str jl -> toSingle str +++ jl) Empty . lines
    where toSingle :: String -> JoinList (Score, Size) String
          toSingle str = Single (scoreString str, Size 1) str

  line = indexJ
  
  replaceLine i str jl = takeJ i jl +++ fromString str +++ dropJ (i+1) jl
  
  numLines = getSize . snd . tag 

  value = getScore . fst . tag
    where getScore (Score x) = x

test :: JoinList (Score, Size) String
test = fromString . unlines $
  [ "This buffer is for notes you don't want to save, and for"
  , "evaluation of steam valve coefficients."
  , "To load a different file, type the character L followed"
  , "by the name of the file."
  ]

main = runEditor editor $ test