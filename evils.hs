--An evil number is a non-negative integer that has an even number of 1s when represented in binary. 
--An odious number has an odd number of 1s in its binary representation.
--Implement a function isEvil :: Int -> Bool that returns True if the argument has an even number of 1s 
--in its binary representation and False otherwise. 
--Use this function to implement the function evils :: [Int] that returns the list of non-negative evil numbers.





toBinary :: Int -> [ Int ] -- takes an int and returns a list of ints
toBinary 0 = [ 0 ]            -- if the number entered is 0 the binary rep will just be 0  
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]
--this is how the toBinary function works for the last case :
-- for example if you enter toBinary 33:
--1. toBinary (33 `quot` 2) ++ [33 `rem` 2] which would be  toBinary(16) ++ 1 
--2. toBinary (8) ++ [0]
--3. toBinary(4) ++ [0] 
--4. toBinary(2) ++ [0] 
--5. toBinary(1) ++ [0]
--6. toBinary(0) ++ [1] 
--0100001

cnt :: Eq a => a -> [a] -> Int
cnt x [] = 0
cnt x (y:ys)
    | x == y = 1 + (cnt x ys)
    | otherwise = cnt x ys
-- the above function counts the presence of x in the list y. we need this because potentially we will 
-- be counting the 1's in the list if we convert a binary number to a list of numbers

isEvil :: Int -> Bool
isEvil n = even(cnt 1 (toBinary n)) 
-- isEvil returns True if the argument has an even number of 1s in its binary representation and False -- otherwise

evils:: [Int] 
evils = filter (isEvil) [0..]
-- evils returns the list of non-negative evil numbers by filtering. then we use take to get a few 
-- elements from this infinite list.



