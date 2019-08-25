-- module Main

data N = Zero | Suc N
  
addOne : N -> N
addOne Zero = Suc Zero
addOne (Suc n) = Suc (Suc n)

One : N
One = Suc Zero
Two : N
Two = (Suc (Suc Zero))
Three : N
Three = addOne Two
Four : N
Four = addOne Three
Five : N
Five = addOne Four


add : N -> N -> N
add Zero s = s
add (Suc a) b = add a (Suc b)

data Vec : (a : Type) -> N -> Type where
  Nil : Vec a Zero
  Cons : {n : N} -> a -> Vec a n -> Vec a (Suc n)

empt : Vec N Zero
empt = Nil

test : Vec N Two
test = Cons One (Cons Two Nil)

test2 : Vec N Three
test2 = Cons Three (Cons Four (Cons Five Nil))

concat : {g : Type} -> {a : N} -> {b : N} -> Vec g a -> Vec g b -> Vec g (add a b)
concat Nil rest = rest
concat (Cons a rest) b = concat rest (Cons a b)

t3 : Vec N (addOne (addOne (addOne (addOne One))))
t3 = concat test test2
