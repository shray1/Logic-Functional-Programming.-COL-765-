{-

search x ls:  at each step x is compared to the head of the remaining list

-}

search1 x ls =
    case ls of
      [] -> False
      (y:ys) -> if x == y
                then True
                else (search1 x ys)

{- 
 B.S. you would have to rely to indices over lists
 !! -- value at an index
-}                     

-- higher order function
-- correctness arguments:
-- 1. of course matcing the mathematical specification
-- 2. Is termination argument!
-- 'a -> 'a -> ('a-> 'a)-> ('a-> 'a) -> 'a
sumHO l u f s =
    if l > u
    then  0 
    else (f l) + (sumHO (s l) u f s)

-- summation of a sequence [a,b]
sumSeq a b =
    let
        s x = x + 1
        f x = x
    in
      sumHO a b f s

sumOfsqSeq a b =
    let
        s x = x + 1
        f x = x*x 
    in
      sumHO a b f s

genAccum l u f s o i =
    if l > u
    then   i 
    else o (f l) (genAccum (s l) u f s o i)

fact1 n =
    let
        f x = x
        s x = x+1
    in
      genAccum 1 n f s (*) 1 

sumSeq1 a b =
    let
        f x = x
        s x = x+1
    in
      genAccum a b f s (+) 0
