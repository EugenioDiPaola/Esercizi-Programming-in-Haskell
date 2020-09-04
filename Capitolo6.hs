
-- CH.6 RECURSIVE FUNCTIONS - Programming in Haskell - Graham Hutton - II Edizione (2016)

-- Interpreter: GHCi

-- 1. How does the recursive version of the factorial function behave if applied to a
-- negative argument, such as (-1)? Modify the definition to prohibit negative
-- arguments by adding a guard to the recursive case.

factorial :: Int -> Int
factorial x | x == 0 = 1
            | x < 0 = 0
            | x > 0 = x * factorial (x - 1)
	      

-- 2. Define a recursive function sumdown :: Int -> Int that returns the sum
-- of the non-negative integers from a given value down to zero. For example,
-- sumdown 3 should return the result 3 + 2 + 1 + 0 = 6.

sumdown :: Int -> Int
sumdown x | x < 0 = 0
          | x == 0 = 0
          | x > 0 = x + sumdown (x - 1)
	  
-- 3. Define the exponentiation operator ^ for non-negative integers using the
-- same pattern of recursion as the multiplication operator *, and show
-- how 2 ^ 3 is evaluated using your definition.

-- * in forma ricorsiva è:

(^) :: Int -> Int -> Int
| m < 0
(^) m 0 = 1
(^) m n = m * m ^ (n - 1)


{- 2 ^ 3
 - 2 * 2 ^ (3 - 1)
 - 2 * 2 ^ 2
 - 2 * (2 * 2^(2-1))
 - 2 * (2 * 2^1)
 - 2 * (2 * (2 * 2^(1 - 1)))
 - 2 * (2 * (2 * 2^0))
 - 2 * (2 * (2 * 1))
 - 2 * (2 * 2)
 - 2 * 4
 - 8
 -}

-- Non mi è chiarissimo perché si ferma a 0 e non diminuisce l'esponenete anche nei numeri negativi 


-- 4. Define a recursive function e u c l i d : : I n t - > I n t - > I n t that implements
-- Euclid’s algorithm for calculating the greatest common divisor of two nonnegative integers: if the two numbers are equal, this number is the result;
-- otherwise, the smaller number is subtracted from the larger, and the same
-- process is then repeated. For example:
-- > euclid 6 27
-- 3


-- 5. Using the recursive definitions given in this chapter, show how l e n g t h [ 1 , 2 , 3 ],
-- drop 3 [1, 2, 3, 4, 5], and init [1, 2, 3] are evaluated.

-- non c'ho voglia


-- 6. Without looking at the definitions from the standard prelude, define the
-- following library functions using recursion:
-- a. Decide if all logical values in a list are True:
-- and :: [Bool] -> Bool
-- b. Concatenate a list of lists:
-- concat :: [[a]] -> [a]
-- c. Produce a list with n identical elements:
-- replicate :: Int -> a -> [a]
-- d. Select the nth element of a list:
-- (!!) :: [a] -> Int -> a
-- e. Decide if a value is an element of a list:
-- elem :: Eq a => a -> [a] -> Bool
-- Note: most of these functions are in fact defined in the prelude using
-- other library functions, rather than using explicit recursion.

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x 

(!!) :: [a] -> Int -> a
(!!) (x:xs) 0 = x
(!!) (x:xs) p | p >= length xs = head (reverse xs)
              | p < length xs = (!!) xs (p - 1)
	      
-- ((!!) originale conta da 0)
-- oppure:

(!!) :: [a] -> Int -> a
(!!) [] n = 0
(!!) l 0 = (take 1 l) !! 0
(!!) l n = (!!) (drop 1 l)  (n - 1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem y (x:xs) = (y == x) || elem y xs


-- 7. Define a recursive function merge :: Ord a ⇒ [a] -> [a] -> [a] that
-- merges two sorted lists to give a single sorted list. For example:
-- > merge [2, 5, 6] [1, 3, 4]
-- [1, 2, 3, 4, 5, 6]
-- Note: your definition should not use other functions on sorted lists such
-- as insert or isort, but should be defined using explicit recursion.

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort less : x : sort more
              where 
                    smaller = [y | y <- xs | y <= x]
	            larger = [y | y <- xs | y > x]

merge :: [a] -> [a] -> [a]
merge _ [] = _
merge [] _ = _
merge (x:xs) (y:ys) | x <= y =  [x,y] ++ merge xs ys

-- Notare che è diverso dal classico algoritmo di merge sort


-- 8. Using merge, define a recursive function msort :: Ord a ⇒ [a] -> [a]
-- that implements merge sort, in which the empty list and singleton lists
-- are already sorted, and any other list is sorted by merging together the
-- two lists that result from sorting the two halves of the list separately.
-- Hint: first define a function halve :: [a ] → [([a ], [a ])] that splits a list
-- into two halves whose length differs by at most one.


-- 9. Using the five step process, define the library functions that calculate
-- the sum of a list of numbers, take a given number of elements from the
-- start of a list, and select the last element of a non-empty list.
