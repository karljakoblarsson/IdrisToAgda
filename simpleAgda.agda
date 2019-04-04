module simpleAgda where

-- main : IO ()
-- main = putStrLn "Testing simple Idris"

--
-- Some simple equivalent Idris and Agda code.
--

-- data N = Z | Suc N
data N : Set where
  Z : N
  suc : N -> N
  
-- one : N
-- one = Suc Z
one : N
one = suc Z

-- addOne : N -> N
-- addOne Z = Suc Z
-- addOne (Suc n) = Suc (Suc n)

addOne : N -> N
addOne Z = suc Z
addOne (suc a) = suc (suc a)

-- add : N -> N -> N
-- add Z s = s
-- add (Suc a) b = add a (Suc b)
add : N -> N -> N
add Z s = s
add (suc a) b = add a (suc b)

-- data Vec : Type -> N -> Type where
--   Nil : Vec a Z
--   (::) : a -> Vec a n -> Vec a (Suc n)
data Vec (A : Set) : N -> Set where
  Nil : Vec A Z 
  _::_ : {n : N} -> A -> Vec A n -> Vec A (suc n)
  
-- empt : Vec N Z
-- empt = Nil
empt : Vec N Z
empt = Nil

open import Agda.Builtin.Nat
-- test : Vec Nat (Suc Main.one)
-- test = 1 :: 2 :: Nil
test : Vec Nat (suc (suc Z))
test = 1 :: (2 :: Nil)

-- test2 : Vec Nat (Suc (Suc Main.one))
-- test2 = 3 :: 4 :: 5 :: Nil
test2 : Vec Nat (suc (suc (suc Z)))
test2 = 3 :: (4 :: (5 :: Nil))

-- concat : Vec g a -> Vec g b -> Vec g (add a b)
-- concat Nil rest = rest
-- concat (a :: rest) b = concat rest (a :: b)
concat : {a b : N} {g : Set} -> (Vec g a) -> (Vec g b) -> (Vec g (add a b))
concat Nil rest = rest
concat (a :: rest) b = concat rest (a :: b)

-- t3 : Vec (addOne $ addOne $ addOne $ addOne Main.one) Nat
-- t3 = concat test test2
t3 : Vec Nat (addOne (addOne (addOne (addOne one))))
t3 = concat test test2
