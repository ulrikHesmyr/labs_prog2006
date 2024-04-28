module Main (main) where

import Lib(decodeMessage, decodeMessageImproved)

main :: IO ()
main = do
    print "Decode an intergalactic message: "
    msg <- getLine
    case decodeMessageImproved msg of 
        Left str -> print str 
        Right n -> print $ "The message is " ++ show n ++ "!"
