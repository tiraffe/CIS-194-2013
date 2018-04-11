{-# LANGUAGE FlexibleInstances #-}
module Calc where 
import ExprT
import Parser
import StackVM


-- Exercise 1

eval :: ExprT -> Integer
eval (ExprT.Add a b) = (eval a) + (eval b)
eval (ExprT.Mul a b) = (eval a) * (eval b)
eval (ExprT.Lit a)   = a


-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr str = case (parseExp ExprT.Lit ExprT.Add ExprT.Mul str) of 
                    (Just x) -> Just $ eval x
                    (Nothing) -> Nothing


-- Exercise 3

class Expr a where 
    lit :: Integer -> a 
    mul :: a -> a -> a 
    add :: a -> a -> a 

instance Expr ExprT where
    lit = ExprT.Lit
    mul = ExprT.Mul
    add = ExprT.Add


-- Exercise 4

newtype MinMax = MinMax Integer deriving (Eq, Show) 
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit = toInteger
    mul = (*)
    add = (+)

instance Expr Bool where
    lit x = if x > 0 then True else False
    mul = (&&)
    add = (||)

instance Expr MinMax where
    lit = MinMax
    mul (MinMax a) (MinMax b) = MinMax $ min a b
    add (MinMax a) (MinMax b) = MinMax $ max a b

instance Expr Mod7 where
    lit = Mod7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a*b) `mod` 7
    add (Mod7 a) (Mod7 b) = Mod7 $ (a+b) `mod` 7

testExp :: Expr a => Maybe a 
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer 
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


-- Exercise 5

testVM = testExp :: Maybe Program

instance Expr Program where
    lit x = [PushI x]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program 
compile str = (parseExp lit add mul str)


