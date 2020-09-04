-- 1. Show how the list comprehension [f x | x <- xs, p x ] can be re-expressed
-- using the higher-order functions map and filter.

-- [f x | x <- xs , p x] può essere scritta come un funzione g siffatta:

g f p = map f . filter p


-- 2. Without looking at the definitions from the standard prelude, define the following higher-order library functions on lists.
-- a. Decide if all elements of a list satisfy a predicate:
-- all :: (a -> Bool) - > [Bool] - > Bool
-- b. Decide if any element of a list satisfies a predicate:
-- any :: (a -> Bool) - > [Bool] - > Bool
-- c. Select elements from a list while they satisfy a predicate:
-- takeWhile :: (a -> Bool) - > [a] -> [a]
-- d. Remove elements from a list while they satisfy a predicate:
-- dropWhile :: (a -> Bool) - > [a] -> [a]
-- Note: in the prelude the first two of these functions are generic functions
-- rather than being specific to the type of lists.

-- a.
-- La funzione all dello standard prelude prende per argomento un Foldable. Una versione 
-- che funziona solo su liste la definirei così:

all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p (x:xs) = (p x) && (all p xs)

-- Usando foldr:

all :: (a -> Bool) -> [a] -> Bool
all p xs = foldr ((&&).p) True xs

-- b.
-- La funzione all dello standard prelude prende per argomento un Foldable. Una versione 
-- che funziona solo su liste la definirei così:

any :: (a -> Bool) -> [a] -> Bool
any p [] = False
any p (x:xs) = (p x) || (any p xs)

-- Usando foldr:

any :: (a -> Bool) -> [a] -> Bool
any p xs = foldr ((||).p) False xs

-- c.
takeWhile :: 


-- Le funzioni foldr e foldl sono così definiti nello standard prelude:

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] = v
foldl f v (x:xs) = foldl f (f v x) xs


-- 3. Redefine the functions map f and filter p using foldr.

-- Nello standard prelude map è definita così:

map :: (a -> b) -> [a] -> [b]
map f [ ] = [ ]
map f (x : xs) = f x : map f xs

-- Usando foldr:

map :: (a -> b) -> [a] -> [b]
map f xs = foldr (f.(:).f) (:[]) xs

-- 4. Using f o l d l, define a function d e c 2 i n t : : [ I n t ] - > I n t that converts a
-- decimal number into an integer. For example:
-- > d e c 2 i n t [ 2 , 3 , 4 , 5 ]
-- 2345

-- 5. Without looking at the definitions from the standard prelude, define the
-- higher-order library function c u r r y that converts a function on pairs into
-- a curried function, and, conversely, the function u n c u r r y that converts a curried function with two arguments into a function on pairs.
-- Hint: first write down the types of the two functions.


-- 6. A higher-order function u n f o l d that encapsulates a simple pattern of recursion for producing a list can be defined as follows:
-- unfoldphtx | p x = [ ]
-- | otherwise = h x : u n f o l d p h t ( t x )
-- That is, the function u n f o l d p h t produces the empty list if the predicate p
-- is true of the argument value, and otherwise produces a non-empty list by
-- applying the function h to this value to give the head, and the function t
-- to generate another argument that is recursively processed in the same way
-- to produce the tail of the list. For example, the function i n t 2 b i n can be
-- rewritten more compactly using u n f o l d as follows:
-- i n t 2 b i n = u n f o l d ( = = 0 ) ( ‘ m o d ‘ 2 ) ( ‘ d i v ‘ 2 )
-- Redefine the functions c h o p 8, m a p f and i t e r a t e f using u n f o l d.


-- 7. Modify the binary string transmitter example to detect simple transmission
-- errors using the concept of parity bits. That is, each eight-bit binary number
-- produced during encoding is extended with a parity bit, set to one if the
-- number contains an odd number of ones, and to zero otherwise. In turn, each
-- resulting nine-bit binary number consumed during decoding is checked to
-- ensure that its parity bit is correct, with the parity bit being discarded if this
-- is the case, and a parity error being reported otherwise.
-- Hint: the library function error :: String -> a displays the given string
-- as an error message and terminates the program; the polymorphic result type
-- ensures that e r r o r can be used in any context.


-- 8. Test your new string transmitter program from the previous exercise using a
-- faulty communication channel that forgets the first bit, which can be modelled using the t a i l function on lists of bits.


-- 9. Define a function altMap :: (a->b) -> (a->b) -> [a] -> [b] that
-- alternately applies its two argument functions to successive elements in a list,
-- in turn about order. For example:
-- > altMap (+10) (+100) [0,1,2,3,4]
-- [10,101,12,103,14]


-- 10. Using a l t M a p, define a function l u h n : : [ I n t ] - > B o o l that implements
-- the Luhn algorithm from the exercises in chapter 4 for bank card numbers of
-- any length. Test your new function using your own bank card.































-- 4. Using foldl, define a function dec2int :: [Int] -> Int that converts a
-- decimal number into an integer. For example:
-- > dec2int [2, 3, 4, 5]

dec2int :: [Int] -> Int
dec2int xs = foldl (+.*10) 0 (zip xs (reverse [10^i | i <- [0..length xs]]))

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] = v
foldl f v (x:xs) = f v (foldl f v xs)


5. Explain why the following definition is invalid:
sumsqreven = compose [sum, map (^2), filter even]

-- 6. Without looking at the standard prelude, define the higher-order library
-- function curry that converts a function on pairs into a curried function,
-- and conversely, the function uncurry that converts a curried function
-- with two arguments into a function on pairs.
-- Hint: first write down the types of the two functions.

curry :: ((a,b) -> c) -> (a -> (b -> c))
curry f = \x y -> f(x,y)
curry f = \x -> \y -> f(x,y)
(curry f) x y = f(x,y)
((curry f) x) y = f(x,y)

uncurry :: (a -> (b -> c)) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y
(uncurry f) (x,y) = f x y 

-- 7. A higher-order function unfold that encapsulates a simple pattern of
-- recursion for producing a list can be defined as follows:
-- unfold p h t x | p x = [ ]
--	       | otherwise = h x : unfold p h t (t x)
-- That is, the function unfold p h t produces the empty list if the predicate p is true of the argument, 
-- and otherwise produces a non-empty list by applying the function h to give the head, and the function t to
-- generate another argument that is recursively processed in the same way
-- to produce the tail of the list. For example, the function int2bin can be
-- rewritten more compactly using unfold as follows:
-- int2bin = unfold (== 0) (‘mod‘2) (‘div‘2)
-- Redefine the functions chop8 , map f and iterate f using unfold.

map f [ ] = [ ]
map f (x : xs) = f x : map f xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

isEmpty :: [a] -> Bool
isEmpty x |list =  [] = True
	  |otherwise =  False


map :: (a -> b) -> [a] -> [b]
map f = unfold (isEmpty) (f.head) (id)

iterate :: (a -> a) -> a -> [a]
iterate f x = unfold (isEmpty) (id) (f) x





8. Modify the string transmitter program to detect simple transmission
errors using parity bits. That is, each eight-bit binary number produced
during encoding is extended with a parity bit, set to one if the number
contains an odd number of ones, and to zero otherwise. In turn, each
resulting nine-bit binary number consumed during decoding is checked
to ensure that its parity bit is correct, with the parity bit being discarded
if this is the case, and a parity error reported otherwise.
Hint: the library function error :: String -> a terminates evaluation and
displays the given string as an error message.


9. Test your new string transmitter program from the previous exercise
using a faulty communication channel that forgets the first bit, which
can be modelled using the tail function on lists of bits.
