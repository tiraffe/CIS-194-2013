module Fibonacci where 


-- Exercise 1

fib1 :: Integer -> Integer
fib1 x 
    | x <= 1    = x
    | otherwise = (fib1 $ x-1) + (fib1 $ x-2)
    

fibs1 :: [Integer]
fibs1 = map fib1 [0..]


-- Exercise 2

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(x, y) -> (y, x + y)) (0, 1)


-- Exercise 3

data Stream a = Stream [a]

streamToList :: Stream a -> [a]
streamToList (Stream xs) = xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList


-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat = Stream . repeat

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a) = Stream $ map f a

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream $ a : (streamToList $ streamFromSeed f (f a))


-- Exercise 5

nats :: Stream Integer
nats = Stream [0..]

ruler :: Stream Integer
ruler = Stream $ map findRuler [1..]
    where findRuler x 
            | x `mod` 2 == 1 = 0
            | otherwise      = 1 + findRuler (x `div` 2)
