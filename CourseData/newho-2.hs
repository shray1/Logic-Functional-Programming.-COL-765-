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

-- f: int -> int -> int
-- f 5: int -> int 
-- curry notation

sumSeq1 a b =
    let
        f x = x
        s x = x+1
    in
      genAccum a b f s (+) 0

-- HoF on Lists

-- Maps, Filters, Folds (reduce)

-- Map: takes as input f and the list and produces a new list
-- f [a, b, c, .. ] --> [f a, f b, f c, . .. ]

mymap f  ls =
    case ls of
      [] -> []
      (x:xs) -> (f x): (mymap f xs)

mapsquare ls =
    let
        f x = x*x
    in
      mymap f ls

{- filter: takes a list as input and
a predicate (a constraint), and excludes 
all those elements from the list that 
do not satisfy the predicate
p: x -> bool
-}

myfilter p ls =
    case ls of
      [] -> []
      (x:xs) -> if p x
                then x: (myfilter p xs)
                else myfilter p xs

positiveOnly ls =
    let
        p x = x > 0
    in
      myfilter p ls


{- fold function (reduce on a certain computation)
   takes as input: 
               a binary function f 
               an identity value for the function f 
               the list itself
               
foldl f e [a1, a2,....an] --> o
foldr f e [a1, a2,....an] 
-}


-- f(an, f (an-1, ... f(a1, e) ....))
myfoldl f e ls =
    case ls of
      [] -> e
      (x:xs) ->  myfoldl f (f x e) xs

-- foldr: f (a1, f(a2, .... f(an, e)...))

--- a1: a2:a3: ...... :an:[]
--- x:xs ---> x: 'a /\ xs: ['a]
--- xs:x --> type error

myfoldr f e ls =
    case ls of
      [] -> e
      (x:xs) ->  f x (myfoldr f e xs)
{-
f = (+) 
e = 0 
ls = [1, 2, 3]
myfoldr .... [1,2,3]
   -- f 1 (myfoldr [2, 3])
      -- (f 1 (f 2 myfoldr [3]))
         -- (f 1 (f 2 (f 3 (myfoldr ... []))))
             -- (f 1 (f 2 (f 3 0)))

-}

{- 
If foldl f e ls = foldr f e ls? 
-- The  answer is no because f need not be 
   associative and commutative. 
-}

-- example of map
-- is a list [a, a+d, a + 2d, ... a+ (n-1)d]

arithProg a d n =
    if n <= 0
    then []
    else a: mymap ((+) d)  (arithProg a d (n-1))

{- 
n = 3
a: AP (a+d) d 2 
a: ((a+d): AP (a+2d) d 1)
a: ((a+d): (a+2d): AP (a+3d) d 0)
a: (a+d): (a+2d)
-- what if I want to apply map?
mymap f ls 

n=3, MM, AP
a: MM (+ d) AP (a d 2)
a: MM (+d) (a: MM (+ d) AP (a d 1) )
a: MM (+d) (a: MM (+ d) (a: MM (+ d) AP (a d 0)))
a: MM (+d) (a: MM (+ d) (a: MM (+ d) []))
              [a, a+d]   [a]
    [a, a+d, a+2d]

an expansion phase -- [] -- []
-}


         
    
