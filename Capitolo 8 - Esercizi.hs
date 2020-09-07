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
checkint
checknat


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
-- such that folde f g replaces each Val constructor in an expression by the
-- function f, and each Add constructor by the function g.

:{
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val v) =  f v
folde f g (Add x1 x2) = g (folde f g x1) (folde f f x2)
:}


-- 6. Using folde, define a function eval :: Expr -> Int that evaluates an expression to an integer value, 
-- and a function size :: Expr -> Int that calculates the number of values in an expression.

:{
eval :: Expr -> Int
eval e = folde id (+) e

-- non capisco perchè questa versione non mi funziona:

--size :: Expr -> Int
--size ex = sum (folde (\x-> [1]) (:) ex)

-- altrimenti, più semplicemente: 

size :: Expr -> Int
size ex = folde (\x -> 1) (+) ex

eval (Add (Add (Add (Val 5) (Val 6)) (Add (Val 1) (Val 8))) (Val 1)) == 21
size (Add (Add (Add (Val 5) (Val 6)) (Add (Val 1) (Val 8))) (Val 1)) == 5
:}


-- 7. Complete the following instance declarations:
-- instance Eq a => Eq (Maybe a) where
-- ...
-- instance Eq a => Eq [a] where
-- ...

data Maybe a = Nothing | Just a

instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    Just x1 == Just x2 = (x1 == x2)
    Nothing == Just x2 = False
    Just x2 == Nothing = False

-- non c'è bisogno di definire come si comporta Maybe con \= perchè \= è già definito in termini di == nella classe Eq.

instance Eq a => Eq [a] where
    [] == [] = True
    x:xs == y:ys = x == y && xs == ys
    _ == _ = False


-- 8. Extend the tautology checker to support the use of logical disjunction (v)
-- and equivalence (<=>) in propositions.

:{
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equivalent Prop Prop
          deriving (Show)

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- p5 :: Prop
-- p5 = Equiv (p1 p2)

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

type Subst = Assoc Char Bool

-- funzione che valuta una Prop per una certa Subst 
eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Equivalent p q) = eval s p == eval s q

-- funzione che ritorna una lista di tutte le Var in una Prop
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Equivalent p q) = vars p ++ vars q

-- funzione che genera una lista di liste di tutte le possibili liste di valori logici di lunghezza data
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n - 1)

-- funzione che rimuove i duplicati in una lista definita nel Capitolo 7
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- funzione che crea una tabella di Subst per una certa Prop
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)
           
-- funzione che controlla se una Prop è una tautologia
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
:}

isTaut p1 == True
isTaut p2 == True
isTaut p3 == True
isTaut p4 == True



-- 9. Extend the abstract machine to support the use of multiplication.

:{
data Expr = Val Int | Add Expr Expr
value :: Expr - > Int
value (Val n) = n
value (Add x y) = value x + value y

type Cont = [Op]
data Op = EVAL Expr | ADD Int

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = execc (n + m)

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)




value :: Expr -> Int
value e = eval e []
:}
