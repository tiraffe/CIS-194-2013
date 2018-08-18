{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a,c) -> (b,c)
first f (a, b) = (f a, b)

instance Functor Parser where  
  fmap f (Parser g) = Parser h
    where h xs = (first f) <$> (g xs)

instance Applicative Parser where
  pure x = Parser (\_ -> Just (x, ""))

  (Parser f) <*> (Parser g) = Parser h
    where 
      h xs 
        | null xs   = Nothing
        | otherwise = case f xs of
          Nothing -> Nothing
          Just (y, ys) -> case g $ dropWhile isSpace ys of
            Nothing -> Nothing
            Just (z, zs) -> Just (y z, zs)
          

abParser :: Parser (Char, Char)
abParser = Parser f
  where 
    f (a:b:xs)
      | (a == 'a') && (b == 'b') = Just ((a, b), xs)
      | otherwise                = Nothing


toEmpty :: a -> ()
toEmpty _ = ()

abParser_ :: Parser ()
abParser_ = toEmpty <$> abParser


intPair :: Parser [Integer]
intPair = makePair <$> posInt <*> posInt
  where makePair x y = [x, y]


instance Alternative Parser where
  empty = Parser (\x -> Nothing)

  (Parser f) <|> (Parser g) = Parser h
    where 
      h xs = case f xs of 
        Nothing -> g xs
        _       -> f xs 


intOrUppercase :: Parser ()
intOrUppercase = intParser <|> uppercaseParser
  where 
    intParser       = toEmpty <$> (posInt)
    uppercaseParser = toEmpty <$> (satisfy isUpper)