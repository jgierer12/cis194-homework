{-# OPTIONS_GHC -Wall #-}

-- CIS 194 Homework 2 - Exercise 1

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
