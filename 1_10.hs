-- 99 haskell problems

-- Problem 1
-- Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' = (!! 0) . foldl (\ acc x -> x : []) []

myLast'' :: [a] -> a
myLast'' = foldl1 (\_ x -> x)


-- Problem 2
-- Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast [x] = error "List len should be at least 2"
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

-- Problem 3
-- Find the K'th element of a list. 
-- The first element in the list is number 1.
elementAt :: [a] -> Integer -> a
elementAt [] _ = error "Empty List"
elementAt (x:xs) k
    | k < 1 = error "k must be greater than or equal to 1!"
    | k == 1 = x
    | k > 1 = elementAt xs (k-1)

elementAt' :: [a] -> Int -> a
elementAt' xs k = xs !! (k-1)

-- Problem 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldl (\acc _ -> acc + 1) 0

-- Problem 5
-- Reverse a list.
-- TODO rewrite this, computationally expensive
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (\acc x -> x:acc) []

myReverse'' :: [a] -> [a]
myReverse'' = foldl (flip (:)) []

-- Problem 6
-- Find out whether a list is a palindrome. 
-- A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Ord a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)


-- Problem 7
--  Flatten a nested list structure.
--  Transform a list, possibly holding 
--  lists as elements into a `flat' list 
--  by replacing each list with its elements (recursively).

-- Problem 8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be 
-- replaced with a single copy of the element. 
-- The order of the elements should not be changed.

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists.

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 
-- to implement the so-called run-length encoding data compression method. 
-- Consecutive duplicates of elements are encoded as lists (N E) 
-- where N is the number of duplicates of the element E.

