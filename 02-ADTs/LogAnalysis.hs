{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1 
-- parses an individual message.
parseMessage :: String -> LogMessage
parseMessage s = case words s of
    ("I":t:xs) -> LogMessage Info (read t) (unwords xs)
    ("W":t:xs) -> LogMessage Warning (read t) (unwords xs)
    ("E":lv:t:xs) -> LogMessage (Error (read lv)) (read t) (unwords xs)
    _ -> Unknown s

-- parses an entire log file 
parse :: String -> [LogMessage]
parse = map parseMessage . lines


-- Exercise 2
-- inserts a new LogMessage into an existing MessageTree
-- suppose timestamps are distinct
insert :: LogMessage -> MessageTree -> MessageTree
insert m@(LogMessage _ _ _) Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node left m'@(LogMessage _ t' _) right)
    | t < t'    = Node (insert m left) m' right
    | otherwise = Node left m' (insert m right)
insert _ tree = tree

-- Exercise 3
-- builds a complete MessageTree from a list of messages
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


-- Exercise 4
-- in-order traversal of the MessageTree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ m : inOrder right


-- Exercise 5
-- takes an unsorted list of LogMessage s, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map takeErrorMessage . inOrder . build . filter isSevere
    where isSevere (LogMessage (Error s) _ _) 
            | s >= 50 = True
            | otherwise = False
          isSevere _ = False
          takeErrorMessage (LogMessage (Error _) _ msg) = msg
          takeErrorMessage _ = ""
