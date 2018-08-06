module JoinList where

import Sized
import Scrabble

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)


tag :: Monoid m => JoinList m a -> m
tag (Single x _)   = x
tag (Append x _ _) = x
tag Empty          = mempty


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append ((tag x) <> (tag y)) x y


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0    = Nothing
indexJ _ Empty        = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ left right)
    | sizeLeft <= i   = indexJ (i - sizeLeft) right 
    | otherwise       = indexJ i left
    where sizeLeft    = getSize . size $ tag left 


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty         = Empty
dropJ n x | n <= 0    = x
dropJ _ (Single _ _)  = Empty
dropJ n (Append _ left right)
    | sizeLeft <= n   = dropJ (n - sizeLeft) right 
    | otherwise       = (dropJ n left) +++ right
    where sizeLeft    = getSize . size $ tag left 


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty         = Empty
takeJ n _ | n <= 0    = Empty
takeJ _ (Single m a)  = Single m a
takeJ n (Append _ left right)
    | sizeLeft <= n   = left +++ (takeJ (n - sizeLeft) right) 
    | otherwise       = takeJ n left
    where sizeLeft    = getSize . size $ tag left 


scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


---- TEST ----
-- show $ (indexJ 1 (Empty::JoinList Size a)) == Nothing
-- show $ (indexJ 1 (Single (Size 1) 'A')) == Nothing
-- show $ (indexJ (-1) (Single (Size 1) 'A')) == Nothing
-- show $ (indexJ 0 (Single (Size 1) 'A')) == Just 'A'
