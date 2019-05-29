data N : Set where
  Z : N
  Suc : N -> N
one : N
one = (Suc Z)
addOne : N -> N
addOne Z = (Suc Z)
addOne (Suc n) = (Suc (Suc n))
two : N
two = (Suc (Suc Z))
three : N
three = addOne two
four : N
four = addOne three
five : N
five = addOne four

add : N -> N -> N
add Z s = s
add (Suc a) b = (add a (Suc b))
data Vec (a : Set) : N -> Set where
  Nil : (Vec a Z)
  cons : {n : N} -> a -> (Vec a n) -> (Vec a (Suc n))
empt : (Vec N Z)
empt = Nil
test : (Vec N (Suc one))
test = (cons one (cons two Nil))
test2 : (Vec N (Suc (Suc one)))
test2 = (cons three (cons four (cons five Nil)))
concat : {a b : N} {g : Set} -> (Vec g a) -> (Vec g b) -> (Vec g (add a b) )
concat Nil rest = rest
concat (cons a rest) b = (concat rest (cons a b))
t3 : (Vec N (addOne (addOne (addOne (addOne one)))))
t3 = (concat test test2)
