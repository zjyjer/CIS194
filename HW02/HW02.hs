{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = length . filter (== True) $ zipWith (==) a b

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors a = fmap (\x->length $ filter (== x) a) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches a b = sum $ zipWith min (countColors a) (countColors b)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove a b = Move b exact $ total - exact
              where exact = exactMatches a b
                    total = matches a b

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m c = (exact == x) && (partial == y)
                   where Move original exact partial = m
                         Move _ x y = getMove original c


-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = allCodes (n - 1) >>= helper

helper :: Code -> [Code]
helper m = fmap ($ m) f
           where f = fmap (:) colors

-- Exercise 7 -----------------------------------------



solve :: Code -> [Move]
solve a = solver (allCodes $ length a) (replicate (length a) Red) a

solver :: [Code] -> Code -> Code -> [Move]
solver remain guess goal
  | guess == goal = [Move guess (length guess) 0]
  | otherwise = result : solver nextset (head nextset) goal
                where result = getMove goal guess
                      nextset = filterCodes result remain

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
