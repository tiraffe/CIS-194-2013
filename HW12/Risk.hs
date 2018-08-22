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
  deriving (Show)


dieAny :: Int -> Rand StdGen [DieValue]
dieAny 0 = return []
dieAny n = 
  die >>= \x -> 
  dieAny (n - 1) >>= \xs ->
  return $ reverse $ sort (x:xs)


battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield attackerNum defendersNum) = 
  dieAny (min (attackerNum-1) 3) >>= \attackDies ->
  dieAny (min defendersNum 2) >>= \defendDies ->
  return $ fight attackerNum defendersNum attackDies defendDies
  where 
    fight a d [] _  = Battlefield a d
    fight a d _ []  = Battlefield a d
    fight a d (x:xs) (y:ys) 
      | x > y       = fight a (d-1) xs ys
      | otherwise   = fight (a-1) d xs ys 


invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield attackerNum defendersNum) 
  | attackerNum < 2 || defendersNum == 0 = return bf
  | otherwise                            = battle bf >>= (\x -> invade x)


successProb :: Battlefield -> Rand StdGen Double
successProb bf@(Battlefield attackerNum defendersNum) =
  playGames 1000 >>= \xs ->
  return $ sum [1 | x <- xs, defenders x == 0] / 1000 
  where 
    playGames 0 = return []
    playGames n = invade bf >>= \x -> 
                  playGames (n-1) >>= \xs ->
                  return $ x:xs
