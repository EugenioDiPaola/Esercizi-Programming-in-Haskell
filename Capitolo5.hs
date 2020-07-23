1. Using a list comprehension, give an expression that calculates the sum
1^2 + 2^2 + . . . 100^2 of the first one hundred integer squares.

sum [x * x | x <- [1..100]]


2. In a similar way to the function length, show how the library function
replicate :: Int -> a -> [a] that produces a list of identical elements can
be defined using a list comprehension. For example:
> replicate 3 True
[True, True, True]

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]


3. A triple (x, y, z) of positive integers can be termed pythagorean if x^2 +
y^2 = z^2. Using a list comprehension, define a function pyths :: Int ->
[(Int, Int, Int)] that returns the list of all pythagorean triples whose
components are at most a given limit. For example:
> pyths 10
[(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


4. A positive integer is perfect if it equals the sum of its factors, excluding
the number itself. Using a list comprehension and the function factors,
define a function perfects :: Int -> [Int] that returns the list of all perfect
numbers up to a given limit. For example:
> perfects 500
[6, 28, 496]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], (mod) n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum (factors x) - x)  == x]


5. Show how the single comprehension [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]
with two generators can be re-expressed using two comprehensions with
single generators. Hint: make use of the library function concat and nest
one comprehension within the other.



6. Redefine the function positions using the function find.

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                 where n = length xs − 1

positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = 


7. The scalar product of two lists of integers xs and ys of length n is given
by the sum of the products of corresponding integers:
\sum_(i = 0)^(n - 1)(xs_i * xs_i)
In a similar manner to the function chisqr, show how a list comprehension 
can be used to define a function scalarproduct :: [Int] -> [Int] -> Int
that returns the scalar product of two lists. For example:
> scalarproduct [1, 2, 3] [4, 5, 6]
32

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

8. Modify the Caesar cipher program to also handle upper-case letters.

import Data.Char

convertCharToInt :: Char -> Int
convertCharToInt c | isLower c = [ i | (i, t) <- zip [0..25] ['a'..'z'], t == c] !! 0
                   | isUpper c = [ i | (i, t) <- zip [26..51] ['A'..'Z'], t == c] !! 0

convertCharToInt :: Char -> Int
convertCharToInt c | isLower c = length['a'..c] - 1
                   | isUpper c = 26 + length['A'..c] - 1

convertIntToChar :: Int -> Char
convertIntToChar i | i >= 0 && i <= 25 = [ c | (c, t) <- zip ['a'..'z'] [0..25], t == i] !! 0
                   | i >= 26 && i <= 51 = [ c | (c, t) <- zip ['A'..'Z'] [26..51], t == i] !! 0

convertIntToChar :: Int -> Char
convertIntToChar i =  (['a'..'z'] ++ ['A'..'Z']) !! i

caesarCypherChar :: Char -> Int -> Char
caesarCypherChar c d | isLower c = convertIntToChar (mod (convertCharToInt c + mod d 26) 26)
                     | isUpper c && (convertCharToInt c + mod d 26) <= 51 = convertIntToChar (convertCharToInt c + mod d 26)
                     | isUpper c && (convertCharToInt c + mod d 26) > 51 = convertIntToChar (26 + mod (convertCharToInt c + mod d 26) 52)

caesarCypherString :: String -> Int -> String
caesarCypherString s d = [caesarCypherChar x d | x <- s]

caesarDecypherChar :: Char -> Int -> Char
caesarDecypherChar c d | isLower c && (convertCharToInt c - mod d 26) >= 0 = convertIntToChar (convertCharToInt c - mod d 26)
                       | isLower c && (convertCharToInt c - mod d 26) < 0 = convertIntToChar (26 - (abs (convertCharToInt c - mod d 26)))
                       | isUpper c && (convertCharToInt c - mod d 26) >= 26 = convertIntToChar (convertCharToInt c - mod d 26)
                       | isUpper c && (convertCharToInt c - mod d 26) < 26 = convertIntToChar (52 - (26 - abs (convertCharToInt c - mod d 26)))

caesarDecypherString :: String -> Int -> String
caesarDecypherString s d = [caesarDecypherChar x d | x <- s]
