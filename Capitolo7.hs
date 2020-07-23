1. Show how the list comprehension [f x | x <- xs, p x ] can be re-expressed
using the higher-order functions map and filter.

[f x | x <- xs , p x] = map (f)(filter p xs)

2. Without looking at the definitions from the standard prelude, define the
higher-order functions all, any, takeWhile and dropWhile.

any :: (a -> Bool) -> [a] -> Bool
any p [] = False
any p (x:xs) = (p x) || (any p xs)

any :: (a -> Bool) -> [a] -> Bool
any p xs = foldr ((||).p) False xs

all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p (x:xs) = (p x) && (all p xs)

all :: (a -> Bool) -> [a] -> Bool
all p xs = foldr ((&&).p) True xs

takeWhile :: 

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] = v
foldl f v (x:xs) = foldl f (f v x) xs

3. Redefine the functions map f and filter p using foldr.

map :: (a -> b) -> [a] -> [b]
map f [ ] = [ ]
map f (x : xs) = f x : map f xs

map :: (a -> b) -> [a] -> [b]
map f [ ] = [ ]
map f (x : xs) = f x : map f xs

map :: (a -> b) -> [a] -> [b]
map f xs = foldr (f.(:).f) (:[]) xs

4. Using foldl, define a function dec2int :: [Int] -> Int that converts a
decimal number into an integer. For example:
> dec2int [2, 3, 4, 5]

dec2int :: [Int] -> Int
dec2int xs = foldl (+.*10) 0 (zip xs (reverse [10^i | i <- [0..length xs]]))

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] = v
foldl f v (x:xs) = f v (foldl f v xs)


5. Explain why the following definition is invalid:
sumsqreven = compose [sum, map (^2), filter even]

6. Without looking at the standard prelude, define the higher-order library
function curry that converts a function on pairs into a curried function,
and conversely, the function uncurry that converts a curried function
with two arguments into a function on pairs.
Hint: first write down the types of the two functions.

curry :: ((a,b) -> c) -> (a -> (b -> c))
curry f = \x y -> f(x,y)
curry f = \x -> \y -> f(x,y)
(curry f) x y = f(x,y)
((curry f) x) y = f(x,y)


uncurry :: (a -> (b -> c)) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y
(uncurry f) (x,y) = f x y 

7. A higher-order function unfold that encapsulates a simple pattern of
recursion for producing a list can be defined as follows:
unfold p h t x | p x = [ ]
	       | otherwise = h x : unfold p h t (t x)
That is, the function unfold p h t produces the empty list if the predicate p is true of the argument, 
and otherwise produces a non-empty list by applying the function h to give the head, and the function t to
generate another argument that is recursively processed in the same way
to produce the tail of the list. For example, the function int2bin can be
rewritten more compactly using unfold as follows:
int2bin = unfold (== 0) (‘mod‘2) (‘div‘2)
Redefine the functions chop8 , map f and iterate f using unfold.

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
