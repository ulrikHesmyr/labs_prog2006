module Lib
    ( decodeMessage, decodeMessageImproved
    ) where


-- | Decode an intergalactic message from a string.
-- The message is a sequence of integers separated by spaces.

-- | decodeMessage "5 5 5 5 8 1 2 3 4 9 8 2 3 4"
-- >>> decodeMessage "5 5 5 5 8 1 2 3 4 9 8 2 3 4"
-- Just 4

-- | decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- Nothing

-- | decodeMessage "5 5 5 2 9 3 4 8 1 3"
-- >>> decodeMessage "5 5 5 2 9 3 4 8 1 3"
-- Just 3

decodeMessage :: String -> Maybe Int
decodeMessage msg = getMessage1 (map read (words msg))

getMessage1 :: [Int] -> Maybe Int 
getMessage1 imsg 
    | min /= 0 && max /= 0 && even sum = Just cosmicMsg
    | otherwise = Nothing
    where
        min = case getUniqueNumber (head imsg, 0) False imsg of
            Just (x, _) -> x
            Nothing -> 0
        max =  case getUniqueNumber (head imsg, 0) True imsg of 
            Just (x, _) -> x 
            Nothing -> 0
        sum = min + max 
        magicN = sum `div` 2
        cosmicMsg = count magicN imsg


-- | Decode an intergalactic message from a string.
-- The message is a sequence of integers separated by spaces.
-- This is an improved version of the previous function, with a more
-- informative error messages.

-- | decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- Left "Communication interference is detected: Minimum number is not unique!"

-- | decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- Left "Communication interference is detected: Maximum number is not unique!"

-- | decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 8"
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 8"
-- Right 3

-- | decodeMessageImproved "5 5 5 1 2 3 4 8 2 3"
-- >>> decodeMessageImproved "5 5 5 1 2 3 4 8 2 3"
-- Left "Communication interference is detected: The sum of min and max is not divisible by 2!"

decodeMessageImproved :: String -> Either String Int
decodeMessageImproved msg = getMessage2 (map read (words msg))

getMessage2 :: [Int] -> Either String Int
getMessage2 imsg
    | min == 0 = Left $ comInDet ++ "Minimum number is not unique!"
    | max == 0 = Left $ comInDet ++ "Maximum number is not unique!"
    | odd sum = Left $ comInDet ++ "The sum of min and max is not divisible by 2!"
    | otherwise = Right cosmicMsg
    where
        comInDet = "Communication interference is detected: "
        min = case getUniqueNumber (head imsg, 0) False imsg of
            Just (x, _) -> x
            Nothing -> 0
        max =  case getUniqueNumber (head imsg, 0) True imsg of 
            Just (x, _) -> x 
            Nothing -> 0
        sum = min + max 
        magicN = sum `div` 2
        cosmicMsg = count magicN imsg




-- | getUniqueNumber (5, 0) True [5, 5, 5, 8, 1, 2, 3, 4, 9, 8, 2, 3, 4]
-- >>> getUniqueNumber (5, 0) True [5, 5, 5, 8, 1, 2, 3, 4, 9, 8, 2, 3, 4]
-- Just (9,1)

-- | getUniqueNumber (5, 0) False [5, 5, 5, 8, 2, 3, 4, 9, 8, 3, 4]
-- >>> getUniqueNumber (5, 0) False [5, 5, 5, 8, 2, 3, 4, 9, 8, 3, 4]
-- Just (2,1)

-- | getUniqueNumber (5, 0) True [5, 6, 6, 7, 8, 1, 7, 8, 5]
-- >>> getUniqueNumber (5, 0) True [5, 6, 6, 7, 8, 1, 7, 8, 5]
-- Nothing

-- Initial values: first element of the list, initial count which must be 0 for the
--  count to be correct due to the recursive function calls, boolean which is 
--  true if we want to find the largest number and false if we want to find the 
--  smallest and then the intergalactic message as a list of integers.
getUniqueNumber :: (Int, Int) -> Bool -> [Int] -> Maybe (Int, Int)
getUniqueNumber (number, amount) biggest [] = if amount == 1 then Just (number, amount) else Nothing
getUniqueNumber (number, amount) biggest (current:remaining)
    | current == number = getUniqueNumber (number, amount+1) biggest remaining
    | biggest && current > number = updateUniqueNumber current 1
    | not biggest && current < number = updateUniqueNumber current 1
    | otherwise = getUniqueNumber (number, amount) biggest remaining
  where
    updateUniqueNumber num count = getUniqueNumber (num, count) biggest remaining

-- | count 5 [1,2,3,4,5]
-- >>> count 5 [1,2,3,4,5]
-- 1


-- | count 5 [1,2,3,4]
-- >>> count 5 [1,2,3,4]
-- 0


-- | count 5 [1,2,3,4,5]
-- >>> count 5 [1,2,5,3,4,5]
-- 2

count :: Int -> [Int] -> Int 
count number l = length (filter (number==) l)