module Main (main) where

import Lib(mulTable, mreverse, oldestStudent)

main :: IO ()
main = do 
    print $ show(mreverse [1, 2, 3])
    mulTable 6
    oldestStudent
