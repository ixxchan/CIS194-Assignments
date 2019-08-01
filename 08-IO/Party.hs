{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import           Employee
import           Data.Tree
import           Data.List
-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons x@Emp { empFun = fun } (GL xs s) = GL (x : xs) (fun + s)

instance Semigroup GuestList where
    (GL xs s1) <> (GL ys s2) = GL (xs ++ ys) (s1 + s2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1@(GL _ s1) l2@(GL _ s2) | s1 < s2   = l2
                                  | otherwise = l1

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) $ ts)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss@Emp { empFun = fun } l =
    (GL (boss : glEmployees l2) (fun + glFun l2), l1)
  where
    l1 = mconcat $ map fst l
    l2 = mconcat $ map snd l
    glEmployees (GL xs _) = xs
    glFun (GL _ s) = s

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = maximum . foldTree nextLevel

-- Exercise 5

putGuestList :: GuestList -> IO ()
putGuestList (GL xs s) =
    putStr "Total fun: " >> print s >> putStr (unlines guestNames)
    where guestNames = sort $ map empName xs

main :: IO ()
main = readFile "./company.txt" >>= putGuestList . maxFun . read
