-- CAPITOLO 3 - TYPES AND CLASSES

-- 1. What are the types of the following values?
-- ['a', 'b', 'c']
-- (’a’, ’b’, ’c’)
-- [(False, ’O’),(True, ’1)]
-- ([False, True],[’0’, ’1’] )
-- [tail, init, reverse]

:type ['a', 'b', 'c']
--


[tail, init, reverse] :: [[a] -> [a]]

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
-- secondxs=head(tailxs)
-- swap(x,y)=(y,x)
-- pairxy=(x,y)
-- doublex=x*2
-- palindrome xs = reverse xs == xs
-- twice f x = f (f x)
-- Hint: take care to include the necessary class constraints in the types if the
-- functions are defined using overloaded operators.

-- 4. Check your answers to the preceding three questions using GHCi.

-- 5. Why is it not feasible in general for function types to be instances of the E q
-- class? When is it feasible? Hint: two functions of the same type are equal if
-- they always return equal results for equal arguments.