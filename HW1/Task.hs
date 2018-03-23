module HW1.Task where

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0    = []
    | otherwise = (toDigits $ x `div` 10) ++ [(x `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0    = []
    | otherwise = (x `mod` 10):(toDigitsRev $ x `div` 10)


-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
    | odd (length xs) = (x * 2) : doubleEveryOther xs
    | otherwise       = x : doubleEveryOther xs


-- Exercise 3 

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits) 


-- Exercise 4

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther $ toDigits x) `mod` 10 == 0


-- Exercise 5 

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c 
    | x == 0    = []
    | otherwise = (hanoi (x-1) a c b) ++ [(a, b)] ++ (hanoi (x-1) c b a) 

