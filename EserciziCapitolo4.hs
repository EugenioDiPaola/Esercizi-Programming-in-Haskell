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

	
-- 2. Consider a function safetail :: [a] -> [a] that behaves as the library
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

-- 3. In a similar way to ∧, show how the logical disjunction operator ∨ can
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
	  

-- 4. Redefine the following version of the conjunction operator using conditional expressions 
-- rather than pattern matching:
-- True && True = True
-- _ && _ = False

(&&/) :: Bool -> Bool -> Bool
(&&/) a b = if (a == True && b == True) then True 
	    else False
            

-- 5. Do the same for the following version, and note the difference in the
-- number of conditional expressions required:
-- True ∧ b = b
-- False ∧ = False

f :: Bool -> Bool -> Bool
f a b = if (a == True && b == True) then True
	if (a == True && b == False) then False
	else False

-- 6. Show how the curried function definition mult x y z = x * y * z can be
-- understood in terms of lambda expressions.

mult :: Num a => a -> a -> a -> a
mult = \x -> \y ->\ z -> x * y * z

-- >:type (mult 3) 
-- :: Num a => a -> a -> a

-- >:type (mult 3 4)
-- :: Num a => a -> a

map (mult 3 4) [1..10]
[12, 24, 36, 48, 60, 72, 84, 96, 108, 120]
