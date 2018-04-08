module HW4.Task where

-- Exercise 1: Wholemeal programming

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) *fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3*n + 1)


fun1' :: [Integer] -> Integer
fun1' = product . map (+ (-2)) . filter even 

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3*n + 1)

-- test: and [fun2' x == fun2 x | x <- [1..1000]]


-- Exercise 2: Folding with trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node h left root right) 
    | getHeight left > getHeight right = Node (getHeight left + 1) left root (insertTree x right)
    | otherwise                        = Node (getHeight (insertTree x left) + 1) (insertTree x left) root right
    where getHeight (Node h _ _ _) = h
          getHeight Leaf           = -1

foldTree :: [a] -> Tree a 
foldTree = foldr insertTree Leaf


-- Exercise 3: More folds!

xor :: [Bool] -> Bool
xor = foldl (\acc x -> acc /= x) False 

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x):acc) []


-- Exercise 4: Finding primes

sieveSundaram :: Integer -> [Integer] 
sieveSundaram n = map (\x -> 2*x + 1) $ filter (\x -> notElem x skiped) [1..n]
    where skiped = [i + j + 2*i*j | i <- [1..n], j <- [1..n], i < j, i + j + 2*i*j <= n]
    