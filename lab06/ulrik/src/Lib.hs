module Lib
    ( feeCalculate
    , earningsCalculate
    , convert2String
    , round2s
    , correctSum
    , Money
    ) where

import Text.Printf (printf)

type Money = Integer

-- | Calculates 30% fee on each of the transactions.
-- The fee is rounded to 2 decimal places.
feeCalculate :: [Money] -> [Money]
feeCalculate txs = let (_, _, _, fourth) = getFee (0, 30, txs, []) in fourth

getFee :: (Integer, Integer, [Money], [Money]) -> (Integer, Integer, [Money], [Money]) -- return jar, percentage of transactions, old-fees and new-fees
getFee (jar, percentage, [], fees) = (jar, percentage, [], fees)
getFee (jar, percentage, transaction:t, fees) 
    | remainder + jar >= 5 = getFee(remainder+jar-10 , percentage , t , fees ++ [fee+1])
    | otherwise = getFee(remainder+jar, percentage, t, fees ++ [fee])
    where
        n = transaction*(percentage `div` 10)
        fee = n `div` 10
        remainder = n `mod` 10

-- | Calculates 70% of the earnings from the transactions.
earningsCalculate :: [Money] -> [Money]
earningsCalculate txs = let (_, _, _, fourth) = getFee (0, 70, txs, []) in fourth


-- | Converts Floats to Strings and joins them with a newline.
-- The numbers are rounded to 2 decimal places.
convert2String :: [Money] -> String
convert2String = unlines . map round2s

-- | Rounds a number to 2 decimal places. It always prints two decimal places,
-- if it is a round number, like 1, it should finish 1.00
round2s :: Money -> String
round2s = addDot



-- ===================================

-- | Correct implementation of the sum function that converts text of
-- numbers with two decimal places into Integer values that are subsequently
-- added without any floating point error accumulation.
correctSum :: [String] -> String
correctSum txs = addDot (sum t)
    where t = map ((read . removeDot) :: String -> Integer) txs


-- | Removes the dot from the string, so that we can convert it to Integer.
removeDot :: String -> String
removeDot = filter (/= '.')

-- | Reintroduces the dot for the last two decimal places into an Integer.
addDot :: Integer -> String
addDot x = show (x `div` 100) ++ "." ++ show (x `mod` 100)

