module Main (main) where

import Lib(mfact) 

main :: IO ()
main = do
    print $ mfact 4
