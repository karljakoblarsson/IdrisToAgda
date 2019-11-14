-- f1 : (a:Type) -> (x:a) -> (y:x) -> x
-- f1 a x y = y

hej : (tyOp : List Type -> Type) -> Type
hej tyOp = tyOp [Int, Bool]

hej2 : List Type -> Type
hej2 (t :: ts) = t

test21 : Type
test21 = hej2 [Type]  -- test21 == Type

test22 : Type
test22 = hej2 [Int]   -- test22 == Int
