module Lib( task1, task2, task3 ) where

task1 :: IO ()
task1 = putStrLn "Hello world!"

task2 :: IO ()
task2 = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn $ "Hello " ++ name
-- dollartegn grouper alt på høyre side og send som argument til venstre side|


task3 :: IO ()
task3 = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn "What is your age?"
    age <- getLine
    age <- return (read age :: Int)
    putStrLn $ "Hello " ++ name ++ ", in 10 years you will be " ++ show(addAge (Age age) (Age 10)) ++ "."
    
-- | mhead [3, 5, 7, 9] is 3
-- >>> mhead [3, 5, 7, 9] 
-- NOW 3

-- | mhead "Hello" is "H"
-- >>> mhead "Hello"
-- 'H'

-- | mhead ['a', 'b', 'c']
-- >>> mhead ['a', 'b', 'c']
-- 'a'
mhead :: [a] -> a
mhead (x:_) = x
mhead [] = error "Empty list has no first element"
    


newtype Age = Age Int deriving (Show)

-- | addAge (Age 1) (Age 2) is (Age 3)
-- >>> addAge (Age 1) (Age 2)
-- Age 3
addAge :: Age -> Age -> Age
addAge (Age x) (Age y) = Age (x + y)

-- | addNumber 3.5 3.0 is 7.5
-- >>> addNumber 3.5 3.0
-- 6.5
addNumber :: Num a => a -> a -> a
addNumber x y = x + y
