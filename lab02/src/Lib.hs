module Lib
    ( mpadding, mulTable, mreverse, oldestStudent
    ) where

import System.IO

-- Task 1

-- |mreverse "Hello" is "olleH"
-- >>> mreverse "Hello"
-- "olleH"

-- | mreverse ['e', 'b', 'a', 'b'] is "babe"
-- >>> mreverse ['e', 'b', 'a', 'b']
-- "babe"

-- | mreverse [1, 2, 3, 4] is [4, 3, 2, 1]
-- >>> mreverse [1,2,3,4]
-- [4,3,2,1]

-- | mpadding 1 is "   1"
-- >>> mpadding 1
-- "   1"

-- Rekursiv funksjon som flytter det forreste elementet helt bakerst 
mreverse :: [a] -> [a]
mreverse [] = []
mreverse (h:t) = mreverse t ++ [h]



-- Task 2

mpadding :: Int -> String
mpadding t
    | t < 10 = "   " ++ show t
    | t < 100 =  "  " ++ show t 
    | otherwise =  " " ++ show t


mulTable :: Int -> IO()
mulTable n 
    | n < 0 = error "Too smol"
    | n > 32 = error "Too large"
    | otherwise = mapM_ rowPrinter [[x*y | x <- [1 .. n]] | y <- [1 .. n]]
        where rowPrinter row = putStrLn $ concatMap mpadding row


-- Task 3

oldestStudent :: IO ()
oldestStudent = do 
    fd <- openFile "studentData.txt" ReadMode 
    contents <- hGetContents fd 
    -- "Alice Cooper 25\n Morgan Sulele 24\n"
    let students = lines contents 
    -- students = ["Alice Cooper 25", "Morgan Sulele 24", ...]
    let ages = map getAge students
    -- ages = [25, 24, ...]
    let amountOldest = snd (mreduce (0,0) ages)
    -- amountOldest = 3
    print $ "Amount of oldest student: " ++ show amountOldest
    hClose fd


-- | getAge "Alice Cooper 25" is 25
-- >>> getAge "Alice Cooper 25"
-- 25

getAge :: String -> Int
getAge w = read (last (words w))

-- | mreduce (0, 0) [23, 24, 25, 34, 10] is (34, 1)
-- >>> mreduce (0, 0) [23, 24, 25, 34, 10]
-- (34,1)

mreduce :: (Int, Int) -> [Int] -> (Int, Int)
mreduce (m, a) [] = (m,a)
mreduce (m, a) (h:t)
    | h > m = mreduce (h, 1) t 
    | h == m = mreduce (m, a+1) t 
    | otherwise = mreduce (m, a) t


-- Task 4

-- Complexity has function of O(N) because constants are removed, originally it would be O(N*4)
-- We improved our code by reducing traversals of the data by 1. 
    -- This is the old code:

    -- let maxAge = maximum ages
    -- let amountOldest = length (filter (==maxAge) ages)
    

    -- Then we made a function called "mreduce" to merge "maximum" and "filter" - which does two traversals - into one single 
    -- function that does one single traversal, now the code looks like this:

    -- let amountOldest = snd (mreduce (0,0) ages)
