-- CAPITOLO 4 - DEFINING FUNCTIONS - Programming with Haskell - Graham Hutton - Edizione II (2016) 

-- 1. Using library functions, define a function halve :: [a] -> ([a], [a]) that
-- splits an even-lengthed list into two halves. For example:
-- > halve [1, 2, 3, 4, 5, 6]
-- ([1, 2, 3], [4, 5, 6])

primiN :: [a] -> Int -> [a]
primiN _ 0 = []
primiN (x:xs) n = x : (primiN xs (n-1))

ultimiN :: [a] -> Int -> [a]
ultimiN _ 0 = []
ultimiN xs n = reverse (take n (reverse xs))

halve1 :: [a] -> ([a], [a])
halve1 xs |even (length xs) = (primiN xs n, ultimiN xs n)
	  |odd (length xs) = (primiN xs (n + 1), ultimiN xs (n + 1))
          	where n = div (length xs) 2
	   
-- oppure:

halve2 :: [a] -> ([a], [a])
halve2 xs |even (length xs) = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs)
          |odd (length xs) = (take (d + 1) xs, drop d xs)
          	where d = div (length xs) 2


-- 2. Define a function t h i r d : : [ a ] - > a that returns the third element in a list
-- that contains at least this many elements using:
-- a. h e a d and t a i l;
-- b. list indexing ! !;
-- c. pattern matching.

thirdA :: [a] -> a
thirdA xs = if (length xs < 3) then error "lista troppo corta"
thirdA xs = head (tail (tail xs))

thirdB :: [a] -> a
thirdB xs = if (length xs < 3) then error "lista troppo corta"
thirdB xs = xs !! 2

thirdC :: [a] -> a
thirdC xs = if (length xs < 3) then error "lista troppo corta"
thirdC (_:_:x3:_) = x3

	
-- 3. Consider a function safetail :: [a] -> [a] that behaves as the library
-- function tail, except that safetail maps the empty list to itself, whereas
-- tail produces an error in this case. Define safetail using:
-- (a) a conditional expression;
-- (b) guarded equations;
-- (c) pattern matching.
-- Hint: make use of the library function null.

safetailA :: [a] -> [a]
safetailA xs = if (null xs) then [] else tail xs

safetailB :: [a] -> [a]
safetailB xs |null xs = []
             |not (null xs) = tail xs

safetailC :: [a] -> [a]
safetailC [] = []
safetailC (_:xs) = xs


-- 4. In a similar way to ∧, show how the logical disjunction operator ∨ can
-- be defined in four different ways using pattern matching.

(||\) :: Bool -> Bool -> Bool
True ||\ True = True 
True ||\ False = True
False ||\ True = True
False ||\ False = False

(|\|) :: Bool -> Bool -> Bool
False |\| False = False
_ |\| _ = True

(|||) :: Bool -> Bool -> Bool
True ||| _ = True
False ||| b = b

(||^) :: Bool -> Bool -> Bool
(||^) c d |c == d = d
          |otherwise = True
	  

-- 5. Redefine the following version of the conjunction operator using conditional expressions 
-- rather than pattern matching:
-- True && True = True
-- _ && _ = False

(&&/) :: Bool -> Bool -> Bool
(&&/) a b = if (a == True && b == True) then True 
	    else False
            

-- 6. Do the same for the following version, and note the difference in the
-- number of conditional expressions required:
-- True ∧ b = b
-- False ∧ = False

f :: Bool -> Bool -> Bool
f a b = if (a == True && b == True) then True
	if (a == True && b == False) then False
	else False


-- 7. Show how the curried function definition mult x y z = x * y * z can be
-- understood in terms of lambda expressions.

mult :: Num a => a -> a -> a -> a
mult = \x -> \y ->\ z -> x * y * z

-- >:type (mult 3) 
-- :: Num a => a -> a -> a

-- >:type (mult 3 4)
-- :: Num a => a -> a

map (mult 3 4) [1..10]
[12, 24, 36, 48, 60, 72, 84, 96, 108, 120]


-- 8. The Luhn algorithm is used to check bank card numbers for simple errors
-- such as mistyping a digit, and proceeds as follows:
-- consider each digit as a separate number;
-- moving left, double every other number from the second last;
-- subtract 9 from each number that is now greater than 9;
-- add all the resulting numbers together;
-- if the total is divisible by 10, the card number is valid.
-- Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts 9 if the result is greater than 9. For example:
-- > luhnDouble 3
-- 6 >
-- luhnDouble 6
-- 3
-- Using luhnDouble and the integer remainder function mod, define a function
-- luhn :: Int -> Int -> Int -> Int -> Bool that decides if a four-digit
-- bank card number is valid. For example:
-- > luhn 1 7 8 4
-- True
-- > luhn 4 7 8 3
-- False
-- In the exercises for chapter 7 we will consider a more general version of this
-- function that accepts card numbers of any length.

luhnDouble = \x -> if x * 2 > 9 then x * 2 - 9 else 2 * x

luhn = \x1 x2 x3 x4 -> (if (mod (luhnDouble x3 + luhnDouble x1 + x2 + x4) 10 == 0) then True else False)
   
