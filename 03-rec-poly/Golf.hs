{-# OPTIONS_GHC -Wall #-}

-- CIS 194 Homework 3 - Exercise 1

module Golf where

-- Skip more and more letters
-- e.g. skips "Hello!" == ["Hello!", "el!", "l!", "l", "o", "!"]
skips :: [a] -> [[a]]
skips l = [every i l | i <- [1..length l]]

-- Skip every nth list item
-- e.g. every 2 "Hello" == "el!"
every :: Int -> [a] -> [a]
every n l = [l !! i | i <- [n-1,n-1+n..length l-1]]
