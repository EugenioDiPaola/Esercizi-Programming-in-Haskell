-- CHAPTER 7 - HIGHER-ORDER FUNCTIONS - Programming with Haskell - Graham Hutton - Edizione II (2016) 
-- Per comodità mi riporto qua le definizioni di foldr e foldl come definite nello standard prelude:

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] = v
foldl f v (x:xs) = foldl f (f v x) xs

-- 1. Show how the list comprehension [f x | x <- xs, p x ] can be re-expressed
-- using the higher-order functions map and filter.

-- [f x | x <- xs , p x] può essere scritta come un funzione g siffatta:

g f p = map f (filter p xs)


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
takeWhile :: (a -> Bool) - > [a] -> [a]
takeWhile p [] = []
takeWhile | p x = x : takeWhile p xs
          | not (p x) = []
	  
-- d.
dropWhile :: (a -> Bool) - > [a] -> [a]
dropWhile p [] = []
dropWhile | p (x:xs) = dropWhile p xs
          | not (x:xs) = xs


-- 3. Redefine the functions map f and filter p using foldr.

-- Nello standard prelude map è definita così:

map :: (a -> b) -> [a] -> [b]
map f [ ] = [ ]
map f (x : xs) = f x : map f xs

-- La struttura è 
-- foldr (funzione binaria su due elementi della lista) (elemento neutro) (lista da foldare da destra verso sinistra). 
-- Usando foldr dunque:

map :: (a -> b) -> [a] -> [b]
map f (x:xs) = foldr (\x xs -> (f x) : xs) [] (x:xs)

filter :: (a -> Bool) -> [a] -> [a]
filter p (x:xs) = foldr (\x xs -> if (p x) then x:xs else xs) [] (x:xs)


-- 4. Using foldl, define a function dec2int :: [Int] -> Int that converts a
-- decimal number into an integer. For example:
-- > dec2int [2,3,4,5]
-- 2345

-- La funzione foldl parte ad associare gli elementi della lista a coppie tramite una funzione binaria 
-- associando prima l'elemento neutro con il primo elemento della lista verso destra, 
-- foldr fa la stessa cosa però partendo dall'ultimo elemento e verso destra.

dec2int :: [a] -> a
dec2int (x:xs) = foldl (\x1 x2 -> x1 * 10 + x2) 0 (x:xs)


-- 5. Explain why the following definition is invalid:
-- sumsqreven = compose [sum, map (^2), filter even]

-- Perché i domini e le immagini di quelle tre funzioni son otali per cui non è possibile comporre in quell'ordine
-- dalla definizione nello standard prelude di compose si ha che
-- [sum, map (^2), filter even] è uguale equivalente a foldr (.) id [sum, map (^2), filter even], che restituisce
-- filter even . map (^2) . sum, l'immagine di sum è Int mentre il dominio di map (^2) è [Int], quindi non posso comporre
-- filter even e map in quell'ordine. Sarebbe ammesso ad esempio compose [filter even, map (^2), sum].


-- 6. Without looking at the definitions from the standard prelude, define the
-- higher-order library function curry that converts a function on pairs into
-- a curried function, and, conversely, the function uncurry that converts a 
-- curried function with two arguments into a function on pairs.
-- Hint: first write down the types of the two functions.

curry :: ((a, b) -> c) -> (a -> (b -> c))
curry f = \x y -> f(x, y)

-- oppure:
curry :: ((a, b) -> c) -> (a -> (b -> c))
curry f = \x -> \y -> f(x, y)

curry :: ((a, b) -> c) -> (a -> (b -> c))
(curry f) x y = f(x, y)

curry :: ((a, b) -> c) -> (a -> (b -> c))
((curry f) x) y = f(x, y)

uncurry :: (a -> (b -> c)) -> ((a, b) -> c)

-- oppure:
uncurry :: (a -> (b -> c)) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y

uncurry :: (a -> (b -> c)) -> ((a, b) -> c)
(uncurry f) (x, y) = f x y 


-- 7. A higher-order function unfold that encapsulates a simple pattern of recursion 
-- for producing a list can be defined as follows:
unfold p h t x | p x = [ ]
               | otherwise = h x : unfold p h t (t x)
-- That is, the function unfold p h t produces the empty list if the predicate p
-- is true of the argument value, and otherwise produces a non-empty list by
-- applying the function h to this value to give the head, and the function t
-- to generate another argument that is recursively processed in the same way
-- to produce the tail of the list. For example, the function int2bin can be
-- rewritten more compactly using unfold as follows:
-- int2bin = unfold (== 0) (‘mod‘ 2) (‘div‘ 2)
-- Redefine the functions chop8, map f and iterate f using unfold.

-- Ricordiamo che:
-- chop8 :: [Bit] -> [[Bit]]
-- chop8 [] = []
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)
-- dove type Bit = Int;
-- iterate :: (a -> a) -> a -> [a]
-- iterate f x = x : iterate f (f x)
-- Allora:

chop8 xs = unfold (\l -> (length l == 0)) (take 8) (drop 8) xs

map f xs = unfold (\l -> (length l == 0)) (f . head) (tail) xs

iterate f xs = unfold (\l -> (length l == 0)) (head) (map f) xs


-- 7. Modify the binary string transmitter example to detect simple transmission
-- errors using the concept of parity bits. That is, each eight-bit binary number
-- produced during encoding is extended with a parity bit, set to one if the
-- number contains an odd number of ones, and to zero otherwise. In turn, each
-- resulting nine-bit binary number consumed during decoding is checked to
-- ensure that its parity bit is correct, with the parity bit being discarded if this
-- is the case, and a parity error being reported otherwise.
-- Hint: the library function error :: String -> a displays the given string
-- as an error message and terminates the program; the polymorphic result type
-- ensures that error can be used in any context.


-- 8. Test your new string transmitter program from the previous exercise using a
-- faulty communication channel that forgets the first bit, which can be modelled 
-- using the tail function on lists of bits.


-- 9. Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that
-- alternately applies its two argument functions to successive elements in a list,
-- in turn about order. For example:
-- > altMap (+10) (+100) [0, 1, 2, 3, 4]
-- [10, 101, 12, 103, 14]


-- 10. Using altMap, define a function luhn :: [Int] -> Bool that implements
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

curry :: ((a, b) -> c) -> (a -> (b -> c))
curry f = \x y -> f(x, y)
curry f = \x -> \y -> f(x, y)
(curry f) x y = f(x, y)
((curry f) x) y = f(x, y)

uncurry :: (a -> (b -> c)) -> ((a,b) -> c)
uncurry f = \(x, y) -> f x y
(uncurry f) (x, y) = f x y 

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
