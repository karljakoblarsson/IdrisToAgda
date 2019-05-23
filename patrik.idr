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

test : (n : N) -> Type
test Zero  = N
test (Suc n) = N -> N

foo : (n : N) -> test n
foo Zero  = Three
foo (Suc n) = add n
