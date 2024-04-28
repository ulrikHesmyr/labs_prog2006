module Lib
    ( mfact ) where

--task1



-- Function argument pattern matching
-- | mhead1 [1, 2, 3]
-- >>> mhead1 [1,2,3]
-- 1

-- | mhead1 [] is "Empty list has no first element"
-- >>> mhead1 []
-- Empty list has no first element
mhead1::[a] -> a
mhead1 (e:_) = e
mhead1 [] = error "Empty list has no first element"



-- Function guards
-- | mhead2 ['b','c','d'] is 'b'
-- >>> mhead2 ['b','c','d']
-- 'b'

-- | mhead2 [] is "Empty list has no first element"
-- >>> mhead2 []
-- Empty list has no first element
mhead2::[a] -> a 
mhead2 e
    | null e = error "Empty list has no first element"
    | otherwise = e !! 0



-- Function with if ... else expression
-- | mhead3 [8, 9, 10] is 8
-- >>> mhead3 [8, 9, 10]
-- 8

-- | mhead3 [] is "Empty list has no first element"
-- >>> mhead3 []
-- Empty list has no first element
mhead3::[a] -> a 
mhead3 e = if not (null e) then e !! 0 else error "Empty list has no first element"



-- Function with use of let ... in
-- | mhead4 [10, 11, 12] is 10
-- >>> mhead4 [10, 11, 12]
-- 10

-- | mhead4 [] is "Empty list has no first element"
-- >>> mhead4 []
-- Empty list has no first element
mhead4::[a] -> a 
mhead4 e =
    if null e 
        then error "Empty list has no first element"
    else 
        let l = e !! 0
        in l




-- Function that uses where expression

-- | mhead5 [3, 4, 5] is 3
-- >>> mhead5 [3, 4, 5]
-- NOW 3

-- | mhead5 [] is 
-- >>> mhead []
mhead5::[a] -> a 
mhead5 e
    | not (null e) = a e
    | otherwise = error "Empty list has no first element"
    where 
        a (b:_) = b


-- Function using case ... of expression
-- | mhead6 [4, 5, 6]
-- >>> mhead6 [4, 5, 6]
-- 4

-- | mhead6 [] is "Empty list has no first element"
-- >>> mhead6 []
-- Empty list has no first element
mhead6::[a] -> a
mhead6 e = case e of
    [] -> error "Empty list has no first element"
    (e:_) -> e


--task2

-- | mfact 6 is 720
-- >>> mfact 6
-- 720

-- | mfact 5 is 120
-- >>> mfact 5
-- 120
mfact :: Integer -> Integer
mfact n
    | n < 0 = error "Factorial is not possible"
    | n == 0 = 1
    | otherwise = product [1..n]





--task3
-- Parameter: index for n-th number
-- Return: value of the index

-- | fib 8 is 21
-- >>> fib 8
-- 21

fib :: Integer -> Integer 
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)




--task4
--Infinite list that is recursively defined
fibs::[Integer]
fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t

-- | fib2 8 is 21
-- >>> fib2 8
-- 21
fib2 :: Int -> Integer
fib2 n = fibs !! n


-- (b:_) is pattern matching 
-- t@ is for naming the tail of the list, to recursevly create the tail of the next recursive call

