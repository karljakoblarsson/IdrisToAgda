-- module Main

-- main : IO ()
-- main = putStrLn "Testing simple Idris"



data N = Z | Suc N
  
one : N
one = Suc Z

addOne : N -> N
addOne Z = Suc Z
addOne (Suc n) = Suc (Suc n)

add : N -> N -> N
add Z s = s
add (Suc a) b = add a (Suc b)

infixr 10 ::
data Vec : N -> Type -> Type where
  Nil : Vec Z a
  (::) : a -> Vec n a -> Vec (Suc n) a

-- (::) : A -> Vec n A -> Vec (Suc n) A

empt : Vec Z N
empt = Nil

test : Vec (Suc Main.one) Nat
test = 1 :: 2 :: Nil

test2 : Vec (Suc (Suc Main.one)) Nat
test2 = 3 :: 4 :: 5 :: Nil

concat : Vec a g -> Vec b g -> Vec (add a b) g
concat Nil rest = rest
concat (a :: rest) b = concat rest (a :: b)

t3 : Vec (addOne $ addOne $ addOne $ addOne Main.one) Nat
t3 = concat test test2
