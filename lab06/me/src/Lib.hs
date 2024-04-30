module Lib
    ( 
        removeDot,
        getFee,
        addDot
    ) where

-- transaction: 353 (actually 3.53)
-- percentage: 30

-- || getFee (0, 30, [353, 353, 353, 353, 353, 353, 353, 353, 353, 353], [])
-- >>> getFee (0, 30, [353, 353, 353, 353, 434, 562, 243, 435, 563, 129], [])
-- WAS (4,30,[],[106,106,106,106,130,168,73,131,169,38])
-- NOW (4,30,[],[106,106,106,106,130,168,73,131,169,38])


-- Using jar rounding and integers for currencies to avoid floating point errors.
getFee :: (Integer, Integer, [Integer], [Integer]) -> (Integer, Integer, [Integer], [Integer]) -- return jar, percentage of transactions, old-fees and new-fees
getFee (jar, percentage, [], fees) = (jar, percentage, [], fees)
getFee (jar, percentage, transaction:t, fees) 
    | remainder + jar >= 5 = getFee(remainder+jar-10 , percentage , t , fees ++ [fee+1])
    | otherwise = getFee(remainder+jar, percentage, t, fees ++ [fee])
    where
        n = transaction*(percentage `div` 10)
        fee = n `div` 10
        remainder = n `mod` 10





-- | Removes the dot from the string, so that we can convert it to Integer.
removeDot :: String -> String
removeDot = filter (/= '.')

-- | Reintroduces the dot for the last two decimal places into an Integer.
addDot :: Integer -> String
addDot x = show (x `div` 100) ++ "." ++ show (x `mod` 100)

