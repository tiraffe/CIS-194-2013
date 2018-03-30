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
insert log Leaf         = Node Leaf log Leaf
insert (Unknown _) tree = tree
insert x (Node left root right) 
    | (getTimeStamp x) < (getTimeStamp root) = Node (insert x left) root right
    | (getTimeStamp x) > (getTimeStamp root) = Node left root (insert x right)
    where getTimeStamp (LogMessage _ timestamp _)  = timestamp


-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = (inOrder left) ++ [root] ++ (inOrder right)


-- Exercise 5 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter isWrong
    where isWrong (LogMessage (Error severity) _ _) = severity >= 50
          isWrong _                                 = False
          getMessage (LogMessage _ _ msg)           = msg
