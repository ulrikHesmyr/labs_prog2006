module Lib
    ( countScore
    ) where

-- | Count the total score of the four dice game data.
-- This function is already implemented for you.
-- 
countScore :: String -> Int
countScore txt = sum $ map processLine (lines txt)


-- | Process a single line of the input data.
--

-- | processLine "4 21 10 5 4 21 13 11"
-- >>> processLine "4 21 10 5 4 21 13 11"
-- 5


-- | processLine "4 21 10 5 4 21 13 11 10"
-- >>> processLine "4 21 10 5 4 21 13 11 10"
-- 7

-- | processLine "4 21 10 5 4 21 13 11 10 10"
-- >>> processLine "4 21 10 5 4 21 13 11 10 10"
-- 11


-- | processLine "10 21 10 5 4 21 13 11 10"
-- >>> processLine "10 21 10 5 4 21 13 11 10"
-- 8

-- | processLine "10 21 10 5 8 20 13 11"
-- >>> processLine "10 21 10 5 8 20 13 11"
-- 0


-- | processLine "10 10 10 5 4 21 13 11 10 10 10"
-- >>> processLine "10 10 10 5 4 21 13 11 10 10 10"
-- 56

-- | processLine "8 14 16 5 8 14 16 14"
-- >>> processLine "8 14 16 5 8 14 16 14"
-- 9

-- | processLine "8 14 16 5 8 18 16 12"
-- >>> processLine "8 14 16 5 8 18 16 12"
-- 3

processLine :: String -> Int
processLine line = getScoring (map read (words line))


-- Function that will map the retrievePoints function for each number in the list where the first param is the index of the number, the second param is the number in itself, and the third param is the entire number list
getScoring :: [Int] -> Int
getScoring numbers = sum [retrievePoints index x numbers | (index, x) <- zip [0..] (drop 3 numbers)]

-- zip [1, 2, 3] [4, 5, 6]
-- [(1, 4), (2, 5), (3, 6)]

-- retrievePoints 0 5 [4, 21, 10, 5, 4, 21, 13, 11]
-- > 0 

-- retrievePoints 1 4 [4, 21, 10, 5, 4, 21, 13, 11]
-- > 1

-- retrievePoints 2 21 [4, 21, 10, 5, 4, 21, 13, 11]
-- > 4

retrievePoints :: Int -> Int -> [Int] -> Int 
retrievePoints index x allNumbers
    | x `elem` winningNumbers = retrieveValue x winningNumbers * retrieveMultiplier x (take (index+1) scoringNumbers)
    | otherwise = 0
    where 
        winningNumbers = take 3 allNumbers
        scoringNumbers = drop 3 allNumbers

-- [10, 14, 16][10, 10, 12, 14, 16]
-- [10]
-- [10, 10]
-- [10, 10, 12]
-- [10, 10, 12, 14]
-- [10, 10, 12, 14, 16]


-- Our original scoring system
retrieveValue :: Int -> [Int] -> Int 
retrieveValue number winNumbers
    | number >= 4 && number <= 9 = 1 * mult
    | number >= 10 && number <= 19 = 2 * mult
    | number >= 20 && number <= 24 = 4 * mult
    | otherwise = 0
    where 
        mult = retrieveMultiplier number winNumbers


-- | retrieveMultiplier 5 [5, 5, 5, 8, 1, 2, 3, 4, 9, 8, 2, 3, 4]
-- >>> retrieveMultiplier 5 [5, 5, 5, 8, 1, 2, 3, 4, 9, 8, 2, 3, 4]
-- 4


-- | retrieveMultiplier 5 [5, 8, 1, 2, 3, 4, 9, 8, 2, 3, 4]
-- >>> retrieveMultiplier 5 [5, 8, 1, 2, 3, 4, 9, 8, 2, 3, 4]
-- 1

-- Creating a recursive function that will return a number x. The number will be multiplied by 2 if there are duplicates of x in the list

-- | retrieveMultiplier 21 [13,14,15,16]
-- >>> retrieveMultiplier 21 [13,14,15,16]
-- 1

retrieveMultiplier :: Int -> [Int] -> Int
retrieveMultiplier x list
    | x `elem` list = 2^ (length (filter (==x) list) - 1)
    | otherwise = 1
