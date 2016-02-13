{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0    = []
  | n > 0     = lastDigit n : toRevDigits (dropLastDigit n)
  | otherwise = error "Invalid input!"

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1, 2])

-- another solution
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' list = f list False
f :: [Integer] -> Bool -> [Integer]
f [] _ = []
f (x:xs) flag = (\val bool -> if bool then 2 * val else val) x flag : f xs (not flag)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map (sum.toRevDigits) xs)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn num
  | result `mod` 10 == 0 = True
  | otherwise   = False
  where result = sumDigits $ doubleEveryOther $ toRevDigits num

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi num tower1 tower2 tower3
  | num == 0 = []
  | num > 0  = hanoi (pred num) tower1 tower3 tower2 ++ [(tower1, tower3)] ++ hanoi (pred num) tower3 tower2 tower1
  | otherwise = error "num should be non-negative!"
