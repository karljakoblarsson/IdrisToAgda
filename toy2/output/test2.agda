data N : Set where
  Z : N
  suc : N -> N
one : N
one = Suc Z
addOne : N -> N
addOne Z = Suc Z
addOne Suc n = Suc Suc n
add : N -> N -> N
add Z s = s
add Suc a b = add a Suc b
infixr 10 ::
data Vec : Set where
  Z : N
  suc : N -> N
empt : Vec Z N
empt = Nil
test : Vec Suc Main.one Nat
test = :: fromInteger 1 :: fromInteger 2 Nil
test2 : Vec Suc Suc Main.one Nat
test2 = :: fromInteger 3 :: fromInteger 4 :: fromInteger 5 Nil
concat : Vec a g -> Vec b g -> Vec add a b g
concat Nil rest = rest
concat :: a rest b = concat rest :: a b
t3 : Vec addOne addOne addOne addOne Main.one Nat
t3 = concat test test2