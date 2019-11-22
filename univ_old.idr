
data Bool = True | False


data Li : (a : Type) -> Type where
  Ni : Li a
  Co : a -> Li a -> Li a


myprod : Li Type -> Type
myprod li = Int
  
t : Li Type -> Type
t li = Type
  
hej : (Li Type -> Type) -> Type
hej f = f (Co (Li Type) Ni)

-- data Cart : (a : Type) -> (b : Type) -> Type where
--   Mul : a -> b -> Cart a b
 
-- v : Cart Type Type
-- v = Mul Int Int

-- -- Prod : Li Type -> Cart a a
-- -- Prod (Co b (Co a nil)) = Mul a b
-- -- Prod : Li Type -> Type
-- -- Prod (Co a rest) = Mul a rest

-- -- data Top : Type where
-- --   qwer : Top
  
-- Prod : Li Type -> Type
-- Prod Ni = Type
-- Prod (Co q qs) = Mul q (Prod qs)

-- -- myid : (a : Type) -> a -> a
-- -- myid _ x = x
  
-- id2 : a -> a
-- id2 x = x

-- idid : {a : Type} -> a -> a
-- idid = id2

-- q : Type -> Type
-- q a = id2 (id2 a)

-- test : Type -> Type
-- test x = x
  
-- a : Type
-- a = test Int

-- b : Type
-- b = test Type


 
 
