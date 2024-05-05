module Main (main) where

import Lib
import Data.List (map)

main :: IO ()
main = do 
    transactions <- readFile "../test.txt"
    
    -- Getting all transactions as a list of Integers
    let transactions' = [read (removeDot x) :: Integer | x <- lines transactions]
    
    print $ getFee (0, 30, transactions', [])