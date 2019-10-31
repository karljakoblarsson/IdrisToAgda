-- module Main

data N = Zero | Suc N

add : N -> N -> N
add Zero s = s
add (Suc a) b = add a (Suc b)

data Vec : (a : Type) -> N -> Type where
  Nil : Vec a Zero
  Cons : {n : N} -> a -> Vec a n -> Vec a (Suc n)


ccExp : {g : Type} -> {a : N} -> {b : N} -> Vec g a -> Vec g b -> Vec g (add a b)
ccImp : Vec g a -> Vec g b -> Vec g (add a b)
