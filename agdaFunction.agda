module agdaFunction where

addOne : N -> N
addOne Z = suc Z
addOne (suc a) = suc (suc a)

