{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage s
    | items !! 0 == "I" = LogMessage Info (read (items !! 1)::Int) (unwords $ drop 2 items) 
    | items !! 0 == "W" = LogMessage Warning (read (items !! 1)::Int) (unwords $ drop 2 items)
    | items !! 0 == "E" = LogMessage (Error (read (items !! 1)::Int)) (read (items !! 2)::Int) (unwords $ drop 3 items)
    | otherwise         = Unknown s
    where items = words s

parse :: String -> [LogMessage]
parse = map parseMessage . lines


-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree


-- Exercise 3

build :: [LogMessage] -> MessageTree


-- Exercise 4

inOrder :: MessageTree -> [LogMessage]


-- Exercise 5 

whatWentWrong :: [LogMessage] -> [String]