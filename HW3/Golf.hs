module HW3.Golf where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map


-- Exercise 1 Hopscotch

skips :: [a] -> [[a]]
skips x = zipWith (\a n -> [ x!!(i-1) | i<-[1..length a], i `mod` n == 0]) (replicate (length x) x) [1..] 


-- Exercise 2 Local maxima 

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:more)
    | y > x && y > z = y : (localMaxima $ y:z:more)
    | otherwise      = localMaxima $ y:z:more
localMaxima _ = []


-- Exercise 3 Histogram

frequency :: [Integer] -> Map Integer Int
frequency = Map.fromList . map (\s -> (head s, length s)) . group . sort

getHistogram :: Int -> Map Integer Int -> String
getHistogram high freq 
    | high > 0  = [if Map.member x freq && (freq Map.! x) >= high then '*' else ' ' | x <- [0..9] ] ++ "\n" ++ getHistogram (high - 1) freq 
    | otherwise = "==========\n0123456789\n"

histogram :: [Integer] -> String
histogram x = getHistogram (high freq) freq
    where freq = frequency x
          high = maximum . map snd . Map.toList