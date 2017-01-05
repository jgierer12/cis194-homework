{-# OPTIONS_GHC -Wall #-}

-- CIS 194 Homework 2 - Exercises 1-4

module LogAnalysis where
import Log

-- Parse an individual line from the log file
-- e.g. parseMessage "E 2 562 help help"
--        == LogMessage (Error 2) 562 "help help"
--      parseMessage "I 29 la la la"
--        == LogMessage Info 29 "la la la"
--      parseMessage "This is not in the right format"
--        == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage msg =
  let ws = words msg
  in case ws of
    ("E":l:t:c) -> LogMessage (Error (read l)) (read t) (unwords c)
    ("I":t:c) -> LogMessage Info (read t) (unwords c)
    ("W":t:c) -> LogMessage Warning (read t) (unwords c)
    _ -> Unknown msg

-- Parse a whole log file
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Insert a `LogMessage` into an existing `MessageTree`
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ t1 _) (Node ltree msg2@(LogMessage _ t2 _) rtree)
   | t1 < t2 = Node (insert msg1 ltree) msg2 rtree
   | otherwise = Node ltree msg2 (insert msg1 rtree)

-- Creates a sorted `MessageTree` from a list of `LogMessage`s
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (msg:msgs) = insert msg (build msgs)

-- Lists all `LogMessage`s in the `MessageTree` from left to right
-- (in-order traversal)
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ltree msg rtree) = inOrder ltree ++ [msg] ++ inOrder rtree
