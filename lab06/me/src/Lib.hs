module Lib
    ( 
        removeDot
    ) where

-- Using jar rounding and integers for currencies to avoid floating point errors.
getFee :: (Integer, Integer, [Integer], [Integer]) -> (Integer, Integer, [Integer], [Integer]) -- return jar, percentage of transactions, old-fees and new-fees
getFee (jar, percentage, [], fees) = (jar, percentage, [], fees)
getFee (jar, percentage, fees, transaction:t) 
    | remainder + jar >= 5 = getFee(remainder+jar-10 , percentage , t , fees ++ [fee+1])
    | otherwise = getFee(remainder+jar, percentage, t, fees ++ [fee])
    where
        n = transaction*(percentage `div` 10) -- DOES NOT REALLY WORK
        fee = floor n -- THIS IS UNECESSARY
        remainder = n*10 `mod` 10





-- | Removes the dot from the string, so that we can convert it to Integer.
removeDot :: String -> String
removeDot = filter (/= '.')

-- | Reintroduces the dot for the last two decimal places into an Integer.
addDot :: Integer -> String
addDot x = show (x `div` 100) ++ "." ++ show (x `mod` 100)

