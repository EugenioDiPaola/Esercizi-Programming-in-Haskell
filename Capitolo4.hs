-- 1. Using library functions, define a function halve :: [a] -> ([a], [a]) that
-- splits an even-lengthed list into two halves. For example:
-- > halve [1, 2, 3, 4, 5, 6]
-- ([1, 2, 3], [4, 5, 6])

firstN :: [a] -> Int -> [a]
firstN _ 0 = []
firstN (x:xs) n = x : (firstN xs (n-1))

lastN :: [a] -> Int -> [a]
lastN _ 0 = []
lastN xs n = reverse (take n (reverse xs))

halve1 :: [a] -> ([a], [a])
halve1 xs |even (length xs) = (firstN xs n, lastN xs n)
	  |odd (length xs) = (firstN xs (n+1), lastN xs (n+1))
          	where n = div (length xs) 2
	   
-- oppure:

halve2 :: [a] -> ([a], [a])
halve2 xs |even (length xs) = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs)
	  |odd (length xs) = (take ((div (length xs) 2) + 1) xs, drop (div (length xs) 2) xs)
          	where n = div (length xs) 2

	
-- 2. Consider a function safetail :: [a] -> [a] that behaves as the library
-- function tail, except that safetail maps the empty list to itself, whereas
-- tail produces an error in this case. Define safetail using:
-- (a) a conditional expression;
-- (b) guarded equations;
-- (c) pattern matching.
-- Hint: make use of the library function null.

safetail1 :: [a] -> [a]
safetail1 [] = []
safetail1 (x:xs) = xs

safetail2 :: [a] -> [a]
safetail2 xs |null xs = []
             |not (null xs) = tail xs

safetail3 :: [a] -> [a]
safetail3 xs = if (null xs) then [] else tail xs


-- 3. In a similar way to ∧, show how the logical disjunction operator ∨ can
-- be defined in four different ways using pattern matching.

(||\) :: Bool -> Bool -> Bool
True ||\ True = True 
True ||\ False = True
False ||\ True = True
False ||\ False = False

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

che è ovviamente equivalente a
(|||) :: Bool -> Bool -> Bool
(|||) False False = False
(|||) _ _ = True

-- > ||| False True
-- True
-- > False ||| True
-- True

(|\|) :: Bool -> Bool -> Bool
False

(||?) :: Bool -> Bool -> Bool
(||?) a b |a == b = b
          |otherwise = True


-- 4. Redefine the following version of the conjunction operator using conditional expressions 
-- rather than pattern matching:
-- True && True = True
-- _ && _ = False

(||/) :: Bool -> Bool -> Bool
(||/) a b = if (a == True || b == True) then True else 
            if (a == True || b == False) then True else
            if (a == False || b == True) then True else
	    False

-- 5. Do the same for the following version, and note the difference in the
-- number of conditional expressions required:
-- True ∧ b = b
-- False ∧ = False


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
