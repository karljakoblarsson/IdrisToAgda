open import Agda.Primitive
open import Data.List
-- f1 : {l : Level} -> (a : Set l) -> (x : a) -> (y : x) -> x
-- f1 : (a : Set2) -> (x : a) -> (y : x) -> x
-- f1 a x y = ?

f0 : (a : Set) -> (x : a) -> a
f0 a x = x

f1 : (a : Set1) -> (x : a) -> a
f1 a x = x

myprod : (As : List Set) -> Set
myprod = {!!}

mysum : (As : List Set) -> Set
mysum = {!!}

hej : (tyOp : List Set -> Set) -> Set
hej = {!!}

test1 = hej myprod
test2 = hej mysum

Type = Set

data Dummy : Set where Dum : Dummy

-- Start of example

hej2 : List Type -> Type     -- Is universe-monomorphic in Agda, but polymorphic in Idris
hej2 (t ∷ ts) = t
hej2 _ = ?

test21 : Type
test21 = hej2 [ Type ]  -- test21 == Type in Idris, but Agda complains

test22 : Type
test22 = hej2 [ Dummy ]   -- test22 == Dummy
