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
  Cons : a -> Vec a n -> Vec a (Suc n)

empt : Vec N Zero
empt = Nil

test : Vec N Two
test = Cons One (Cons Two Nil)

test2 : Vec N Three
test2 = Cons Three (Cons Four (Cons Five Nil))

cc :  Vec g a -> Vec g b -> Vec g (add a b)
cc Nil rest = rest
cc (Cons a rest) b = cc rest (Cons a b)

t3 : Vec N (addOne (addOne (addOne (addOne One))))
t3 = cc test test2
