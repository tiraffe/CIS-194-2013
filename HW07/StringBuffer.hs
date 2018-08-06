{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module StringBuffer where

import Data.Monoid

import Buffer
import JoinList
import Sized
import Scrabble

instance Buffer String where
  toString     = id
  fromString   = id
  line n b     = safeIndex n (lines b)
  replaceLine n l b = unlines . uncurry replaceLine' . splitAt n . lines $ b
      where replaceLine' pre [] = pre
            replaceLine' pre (_:ls) = pre ++ l:ls
  numLines     = length . lines
  value        = length . words

safeIndex :: Int -> [a] -> Maybe a
safeIndex n _ | n < 0 = Nothing
safeIndex _ []        = Nothing
safeIndex 0 (x:_)     = Just x
safeIndex n (_:xs)    = safeIndex (n-1) xs


instance Buffer (JoinList (Score, Size) String) where
  toString Empty          = []
  toString (Single _ x)   = x
  toString (Append _ x y) = (toString x) ++ (toString y)

  fromString              = fromLines
  line n b                = indexJ n b 
  replaceLine n l b       = (takeJ n b) +++ (fromString l) +++ (dropJ (n + 1) b) 
  numLines                = getSize . size . tag
  value                   = getScore . fst . tag


fromLines :: String -> JoinList (Score, Size) String
fromLines = foldl (+++) Empty . map fromString . lines
  where fromString s = Single (scoreString s, Size 1) s
