
data Li : (a : Set) -> Set where
  Ni : {a : Set} -> Li a
  Co : {a : Set} -> a -> Li a -> Li a

{-# BUILTIN NATURAL name #-}

t : Li (Set 2) -> Set 1
t li = Set 1
