{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = calcBattle bf <$> atkDices <*> defDices
  where
    sortDes = sortBy $ flip compare
    atkDices = sortDes <$> replicateM (min 3 (attackers bf)) die
    defDices = sortDes <$> replicateM (min 2 (defenders bf)) die

calcBattle :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
calcBattle (Battlefield a d) atk def = Battlefield (a-fs) (d-ts)
  where
    res = zipWith (>) atk def
    ts = countTrue res
    fs = length res - ts

countTrue :: [Bool] -> Int 
countTrue = foldr (\b s -> if b then s + 1 else s) 0

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield a d)
    | a < 2 || d == 0 = uniform [bf]
    | otherwise = battle bf >>= invade

attackWin :: Battlefield -> Bool
attackWin (Battlefield a d)
    | a < 2 = False
    | otherwise = True

calcProb :: [Bool] -> Double 
calcProb xs = (fromIntegral $ countTrue xs) / (fromIntegral $ length xs)

successProb :: Battlefield -> Rand StdGen Double
successProb bf = calcProb . map attackWin <$> replicateM 1000 (invade bf)
