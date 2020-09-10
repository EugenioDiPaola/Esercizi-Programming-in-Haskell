-- CAPITOLO 1 - INTRODUCTION - Programming with Haskell - Graham Hutton - II Edizione (2016)

-- 1. Give another possible calculation for the result of double (double 2).

-- double (double 2)
-- = double (2 + 2)
-- = (2 + 2) + (2 + 2)
-- = 4 + (2 + 2)
-- = 4 + 4
-- = 8


-- 2. Show that sum [x] = x for any number x.

-- sum [x]
-- = x + sum []
-- = x + 0
-- = x


-- 3. Define a function product that produces the product of a list of numbers,
-- and show using your definition that product [2, 3, 4 ] = 24.

product :: [Int] -> Int
product [] = 1
product x:xs = x * product xs

-- product [2, 3, 4]
-- = 2 ∗ (product [3, 4])
-- = 2 ∗ (3 ∗ product [4])
-- = 2 ∗ (3 ∗ (4 ∗ product []))
-- = 2 ∗ (3 ∗ (4 ∗ 1))
-- = 24


-- 4. How should the definition of the function qsort be modified so that it produces a reverse sorted version of a list?

qsortReverse :: Ord a => [a] -> [a]
qsortReverse [] = []
qsortReverse (x:xs) = qsortReverse smaller ++ [x] ++ qsortReverse larger
               where
                  smaller = [a | a <- xs, a * x ]
                  larger = [b | b <- xs, b > x ]

-- 5. What would be the effect of replacing < = by < in the original definition of
-- qsort? Hint: consider the example qsort [2, 2, 3, 1, 1].

-- Rimuove i duplicati, cioè qsortReverse [2, 2, 3, 1, 1] con <= invece che < restituisce [1, 2, 3] invece che [1, 1, 2, 2, 3].



-- CAPITOLO 2 - FIRST STEPS - Programming with Haskell - Graham Hutton - II Edizione (2016)

-- 1. Parenthesise the following arithmetic expressions:
-- 2 ^ 3 ∗ 4
-- 2 ∗ 3 + 4 ∗ 5
-- 2 + 3 ∗ 4 ^ 5

-- (2 ^ 3) * 4
-- (2 * 3) + (4 * 5)
-- 2 + (3 * (4 * 5))


-- 2. Work through the examples from this chapter using Hugs.


-- 3. The script below contains three syntactic errors. Correct these errors
-- and then check that your script works properly using Hugs.
-- N = a ’div’ length xs
-- where
-- a = 10
-- xs = [1, 2, 3, 4, 5]


-- 4. Show how the library function last that selects the last element of a nonempty list could be defined in terms of the library functions introduced
-- in this chapter. Can you think of another possible definition?


-- 5. Show how the library function init that removes the last element from
-- a non-empty list could similarly be defined in two different ways.


-- CAPITOLO 3 - INTRODUCTION - Programming with Haskell - Graham Hutton - II Edizione (2016)

-- 1. What are the types of the following values?
-- ['a', 'b', 'c']
-- (’a’, ’b’, ’c’)
-- [(False, ’O’),(True, ’1)]
-- ([False, True],[’0’, ’1’] )
-- [tail, init, reverse]

-- ['a', 'b', 'c'] è di tipo Char

-- ('a', 'b', 'c') è di tipo (Char, Char, Char)

-- [(False, 'O'), (True, '1')] è di tipo [(Bool, Char)]

-- ([False, True], ['0', '1']) è di tipo ([Bool], [Char])

-- [tail, init, reverse] è di tipo [[a] -> [a]]


-- 2. Write down definitions that have the following types; it does not matter what
-- the definitions actually do as long as they are type correct.
-- bools :: [Bool]
-- nums :: [[Int]]
-- add :: Int -> Int -> Int -> Int
-- copy :: a -> (a, a)
-- apply :: (a -> b) -> a -> b

bools = [True, False, False]

nums = [[1,2,3], [4,5], [7,8,9,10]]

add = \x y z -> x + y + z

copy = \x -> (x, x)

apply = \f x -> f x


-- 3. What are the types of the following functions?
-- second xs = head (tail xs)
-- swap (x, y) = (y, x)
-- pair x y = (x, y)
-- double x = x * 2
-- palindrome xs = reverse xs == xs
-- twice f x = f (f x)
-- Hint: take care to include the necessary class constraints in the types if the
-- functions are defined using overloaded operators.

second :: [a] -> a

swap :: (a, b) -> (b, a)

palindrome :: Eq a => [a] -> Bool

double :: Num a => a -> a

pair :: a -> b -> (a, b)

twice :: (a -> a) -> a -> a


-- 4. Check your answers to the preceding three questions using GHCi.

-- Basta usare :type seguito dall'espressione.


-- 5. Why is it not feasible in general for function types to be instances of the Eq
-- class? When is it feasible? Hint: two functions of the same type are equal if
-- they always return equal results for equal arguments.

-- Perché spesso le funzioni hanno un numero di argomenti infinito o molto grande.


