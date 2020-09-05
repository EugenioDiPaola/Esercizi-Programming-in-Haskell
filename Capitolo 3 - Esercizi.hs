-- CAPITOLO 3 - TYPES AND CLASSES

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


