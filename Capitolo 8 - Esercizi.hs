-- 1. In a similar manner to the function add, define a recursive multiplication function
-- mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:
-- Hint: make use of add in your definition.

:{
data Nat = Zero | Succ Nat deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n)

-- testo se funzionano add e mult

nat1 = Succ Zero
nat2 = Succ nat1
nat3 = add nat1 nat2
nat6 = mult nat2 nat3
nat10 = Succ(Succ(Succ(Succ(nat6))))

int1 = nat2int(nat1)
int2 = nat2int(nat3)
int6 = nat2int(nat6)
int10 = nat2int(nat10)

checkint = [int1, int2, int6, int10]
checknat = [nat1, nat2, nat3, nat6, nat10]
:}


-- 2. Although not included in appendix B, the standard prelude defines
-- data Ordering = LT | EQ | GT
-- together with a function
-- compare :: Ord a => a -> a -> Ordering
-- that decides if one value in an ordered type is less than (LT), equal to (EQ),
-- or greater than (GT) another value. Using this function, redefine the function
-- occurs :: Ord a => a -> Tree a -> Bool for search trees. Why is this
-- new definition more efficient than the original version?

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r)|compare x y == EQ = True
                     |compare x y == LT = occurs x l
                     |compare x y == GT = occurs x r
                     
-- non mi sembra più efficiente, può sempre capitare che debba fare due compare
                     
                     
-- 3. Consider the following type of binary trees:
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
-- Let us say that such a tree is balanced if the number of leaves in the left and
-- right subtree of every node differs by at most one, with leaves themselves being trivially balanced. 
-- Define a function balanced :: Tree a -> Bool that
-- decides if a binary tree is balanced or not.
-- Hint: first define a function that returns the number of leaves in a tree.

-- questo albero differisce da data Tree = Leaf a | Node (Tree a) a (Tree a) nel fatto che 
-- nessun nodo contine un valore a tranne le foglie al termine dell'albero.

:{
data Tree a = Leaf a | Node (Tree a) (Tree a)

nDiFoglie :: Tree a -> Int
nDiFoglie :: Leaf _ = 1
nDiFoglie :: Node l r = nDiFoglie l + nDiFoglie r

balanced :: Tree a -> Bool
balanced Leaf _ = True
balanced Node l r |nL == nR = True
                  |abs (nL - nR) <= 1 = True && blanced && l balanced r
                  |otherwise = False
                  where nL = nDiFoglie l, nR = nDiFoglie r
                  
alberoTest1 = Node (Node (Node (Leaf 3)) Node (Node (Leaf 4 Leaf 5) Node (Leaf 6)))
alberoTest2 = Node (Node (Node (Leaf 3)) Node (Node (Leaf 4 Leaf 5)))

balanced alberoTest
:}



-- 4. Define a function balance :: [a] -> Tree a that converts a non-empty
-- list into a balanced tree. Hint: first define a function that splits a list into two
-- halves whose length differs by at most one.



-- 5. Given the type declaration
-- data Expr = Val Int | Add Expr Expr
-- define a higher-order function
-- folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
-- such that folde f g replaces each V a l constructor in an expression by the
-- function f, and each A d d constructor by the function g.

:{
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val v) =  f v
folde f g (Add x1 x2) = g (folde f g x1) (folde f g x2)
:}


-- 6. Using folde, define a function eval :: Expr -> Int that evaluates an expression to an integer value, 
-- and a function size :: Expr -> Int that calculates the number of values in an expression.

:{
eval :: Expr -> Int
eval e = folde id (+) e

-- non capisco perchè questa verisone non mi funziona:

--size :: Expr -> Int
--size e = sum (folde (\x-> [1]) (:) e)

-- altrimenti più semplicemente: 

size :: Expr -> Int
size e = folde (\x -> 1) (+) e

eval (Add (Add (Add (Val 5) (Val 6)) (Add (Val 1) (Val 8))) (Val 1)) == 21
size (Add (Add (Add (Val 5) (Val 6)) (Add (Val 1) (Val 8))) (Val 1)) == 5
:}

-- 7. Complete the following instance declarations:
-- instance Eq a = > Eq (Maybe a) where
-- ...
-- instance Eq a => Eq [a] where
-- ...

-- 8. Extend the tautology checker to support the use of logical disjunction (_)
-- and equivalence (ô) in propositions.
-- 9. Extend the abstract machine to support the use of multiplication.
