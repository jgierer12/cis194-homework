-- CIS 194 Homework 1 - Exercises 1-4

import Data.Char

-- Converts a positive Integer to a list of its digits
-- e.g. `toDigits 143 = [1, 4, 3]`
toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

-- Converts a positive Integer to a list of its digits and reverts the list
-- e.g. `toDigitsRev 143 = [3, 4, 1]`
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse $ toDigits x

-- Doubles every second item in a list of Integers
-- e.g. `doubleEveryOther [1, 3, 4, 6] = [1, 6, 4, 12]`
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : (y*2) : (doubleEveryOther zs)

-- Sums the digits of a list of Integers
-- e.g. `sumDigits [16, 3, 5] = 1 + 6 + 3 + 5 = 15`
sumDigits :: [Integer] -> Integer
sumDigits xs = sum [sum $ toDigits x | x <- xs]

-- Validates a credit card number
-- e.g. `validate 4012888888881881 = True`
--      `validate 4012888888881882 = False`
validate :: Integer -> Bool
validate x = (sumDigits $ doubleEveryOther $ toDigitsRev x) `mod` 10 == 0
