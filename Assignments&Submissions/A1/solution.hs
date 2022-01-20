import System.Random

-------------------------------
-- problem 1: lists and sorting
-------------------------------

-- tail-recursive reversal of a list
reverse_aux []     o = o
reverse_aux (x:xs) o = reverse_aux xs (x:o)
myreverse l = reverse_aux l []

{-
Correctness argument:
---------------------
Invariant: For input list l, at each invocation of (reverse_aux l' o), we have (reverse(l') ++ o == reverse(l))
At entry, (o = []) and (l == l'), so (reverse(l) ++ []) trivially equals (reverse(l))
Assume the invariant is true for (l' == x:xs) i.e. (reverse(x:xs) ++ o == reverse(l))
Then we need to prove that it holds for new values of l' and o as well i.e. (reverse(xs) ++ (x:o) == reverse(l))
we have (reverse(x:xs) == reverse(xs) ++ [x]) and ([x] ++ o == (x:o))
Thus, we can rewrite (reverse(x:xs) ++ o == reverse(l)) as (reverse(xs) ++ (x:o) == reverse(l)).
At exit, we have l' == [], therefore, o == reverse(l), which completes the proof.

Timing analysis:
----------------
O(n) as each invocation decreases the size of the list by one.
-}

-- tail-recursive merge function
reverse_concat [] l2 = l2
reverse_concat (x:xs) l2 = reverse_concat xs (x:l2)

merge_aux []       r        o = reverse_concat o r
merge_aux l        []       o = reverse_concat o l
merge_aux l@(x:xs) r@(y:ys) o =
    if x < y
    then merge_aux xs r (x:o)
    else merge_aux l ys (y:o)
merge l r = merge_aux l r []

{-
NB: We assume the correctness of reverse_concat.

Correctness argument:
---------------------
Invariant: For sorted input lists l and r, at each invocation of (merge_aux l' r' o), we have (reverse(o) ++ sort(l'++r') == sort(l++r)), (sorted(l')) and (sorted(r''))
At entry, (o = []), (l == l') and (r = r'), so (reverse([]) ++ sort(l++r)) trivially equals (sort(l++r)).  Also, as l and r are sorted, sorted(l) and sorted(r) hold as well.
Assume the invariant is true for (l' == x:xs) and (r' == y:ys) i.e. (reverse(o) ++ sort((x:xs)++(y:ys)) == sort(l++r)), (sorted(x:xs)) and (sorted(y:ys)) hold.
Then we need to prove that it holds for new values of l', r' and o as well.
Case analysis:
1. (x < y)
  We have (reverse(x:o) == reverse(o) ++ [x])
  and, given sorted(x:xs) and sorted(y:ys), it can be easily seen that (sort((x:xs)++(y:ys)) == [x]:sort(xs++(y:yes))), i.e. x is the smallest element of ((x:xs)++(y:ys))
  Thus, we can rewrite (reverse(o) ++ sort((x:xs)++(y:ys)) == sort(l++r)) as:
      (reverse(o) ++ ([x]++sort(xs++(y:ys))) == sort(l++r)),
  or, (reverse(o) ++ [x] ++ sort(xs++(y:ys)) == sort(l++r)),
  or, (reverse(x:o) ++ sort(xs++(y:ys)) == sort(l++r)), which was to be proven
  Also, sorted(x:xs) => sorted(xs)
2. (y <= x) (the else part)
  Same argument as above with x -> y
At exits, we have either (l' == []) or (r' == []).  Considering the first case:
  We have (reverse(o) ++ sort([]++r') == sort(l++r)) and sorted(r')
  or, (reverse(o) ++ sort(r') == sort(l++r)) and sorted(r')
  or, (reverse(o) ++ r' == sort(l++r))
  As (reverse_concat o r' == reverse(o) ++ r'), this completes the proof.

Timing analysis:
----------------
O(m+n), where m = length(l) and n = length(r), as each invocation decreases the size of either of the list by one.
-}

-- tail-recursive fibonacci function
myfibo_aux a b 0 = a
myfibo_aux a b 1 = b
myfibo_aux a b n = myfibo_aux b (a+b) (n-1)
myfibo n = myfibo_aux 0 1 n

{-
Correctness argument:
---------------------
Invariant: For input n, at each invocation of (myfibo_aux a b n'), we have (a == fib(n-n')) and (b == fib(n-n'+1))
At entry, (n' == n), (a = 0) and (b == 1), so (0 == fib(0) and (1 == fib(1) holds true.
Assume the invariant is true for a, b and n'.
Then we need to prove that it holds for new values of a, b and n' as well, i.e. (b == fib(n-(n'-1))) and ((a+b) == fib(n-(n'-1)+1))
First part holds trivially.
For second part, we have (fib(k+1) = fib(k) + fib(k-1)), for k >= 1
or, (fib((n-n'+1)+1) = fib(n-n'+1) + fib((n-n'+1)-1)), by substituting k -> (n-n'+1) (note that (n-n'+1) >= 1)
or, (fib(n-n'+2) = fib(n-n'+1) + fib(n-n')), by substituting k -> (n-n'+1) (note that (n-n'+1) >= 1)
or, (fib(n-n'+2) = b + a),
At exit, we have (n' == 0) or (n' == 1).  For the first case this gives:
  (a == fib(n)) which completes the proof

Timing analysis:
----------------
O(n) as each invocation decreases n by 1.
-}

-- tail-recursive insertion sort

insert a []     = [a]
insert a (x:xs) = if a < x then a:x:xs
                           else x:(insert a xs)
iSort_aux []     o = o
iSort_aux (x:xs) o = iSort_aux xs (insert x o)
iSort l = iSort_aux l []

{-
NB: We assume the correctness of insert.

Correctness argument:
---------------------
Invariant: For input list l, at each invocation of (iSort_aux l' o), we have (o ++ sort(l') == sort(l)) and sorted(o)
At entry, (o = []) and (l == l'), so ([] ++ sort(l)) trivially equals (sort(l)).  Also, sorted([]) holds as well.
Assume the invariant is true for (l' == x:xs) i.e. (o ++ sort(x:xs) == sort(l)) and sorted(o)
Then we need to prove that it holds for new values of l' and o as well i.e. ((insert x o) ++ sort(xs) ++ sort(l)) and sorted((insert x o))
insert guarantees that for sorted list q and number a, (sorted(insert a q)) holds true.  This proves the second part.
For the first part, we have (sort(o) ++ sort(x:xs) == sort(x:o) ++ sort(xs)) i.e it is safe to transfer an element from second list to first as long the result is sorted again.
We can rewrite (o ++ sort(x:xs) == sort(l)) as:
      (sort(o) ++ sort(x:xs) == sort(l)), as sorted(o) holds
  or, (sort(x:o) ++ sort(xs) == sort(l)), from above
  or, ((insert x o) ++ sort(xs) == sort(l)), from insert's guarantee
At exit, we have l' == [], therefore, o == sort(l), which completes the proof.

Timing analysis:
----------------
T(n) = Sum(i=1 to n){O(i) + c} = O(n^2), where O(i) comes from insert
-}

-- recursive quick sort
qSort []     = []
qSort (x:xs) = (qSort lesser)++[x]++(qSort greater)
  where
    lesser = filter (< x) xs
    greater = filter (>= x) xs

{-
Correctness argument:
---------------------
qSort(l) == sort(l)
Strong induction on length of the list l.
Basis: length(l) == 0, (qSort [] == [] == sort([]))
IH: qSort(xs) == sort(xs) for all lists xs of length <= k
IS:
  We have, from properties of filter:
         (filter (< x) xs) == elements of xs < x,                             ..(1)
         (filter (>= x) xs) == elements of xs >= x,                           ..(2)
     and sort((filter (< x) xs) ++ [x] ++ (filter (>= x) xs)) == sort(x:xs)   ..(3)
  Further, we have, (length((filter (< x) xs)) <= length(xs))                 ..(4)
                and (length((filter (>= x) xs)) <= length(xs))                ..(5)
  It can easily be seen that
      (sort(p) ++ [e] ++ sort(q) == sort(p++[e]++q)),                         ..(6)
    for lists p and q s.t. all elements of p < e and all elements of q >= e.
  For list x:xs of length k+1, we have from (6):
      (sort(lesser) ++ [x] ++ sort(greater) == sort(lesser++[x]++greater)), by substituting p and q with lesser and greater respectively using (1) and (2)
  or, (sort(lesser) ++ [x] ++ sort(greater) == sort(x:xs)), from (3)
  or, (qSort(lesser) ++ [x] ++ qSort(greater) == sort(x:xs)), from IH and (4), (5)
  This completes the proof.

Timing analysis:
----------------
Assuming a balanced split i.e. |lesser| == |greater|, we have T(n) = 2*T(n/2) + O(n), where O(n) is the cost for filter and ++ operations.
Solving it yields T(n) = O(n*log(n))
-}

-- recursive binary search
bSearch_aux e l i j
    | i > j     = -1
    | l!!m == e = m
    | l!!m < e  = bSearch_aux e l (m+1) j
    | otherwise = bSearch_aux e l i (m-1)
    where
        m = (i+j)`quot`2
bSearch e l = bSearch_aux e l 0 ((length l) - 1)

{-
Correctness argument:
---------------------
For sorted list l, indices i and j and element e,
         if (i <= j) and e in l[i:j] then l[k] == e and i <= k <= j.
  where, k = bSearch_aux(e, l, i, j),
     and l[i:j] refers to slice of list l from index i upto j
Strong induction on n = length of l.
Basis: n == 0 => (i > j) thus the property trivially holds
IH: The property holds for lists of size <= k
IS:
For i, j st. (j-i+1) == k+1
If (i > j) or e is not in l[i:j] then property trivially holds so assuming both (i <= j) and e in l[i:j].
With m = (i+j)/2, i.e. the (lower) middle element of list.
Case analysis: 
1. l[m] == e:
  As ((k <- bSearch_aux(e, l, i, j)) == m), (l[k] == e) holds and so does (i <= k <= j).
2. l[m] < e: 
  For sorted list l, e cannot be in l[i:m], thus e can only be in l[m+1:j].
  From IH, l[k] == e, where (k == bSearch_aux(e, l, (m+1, j))) and (m+1 <= k <= j)
  Here, (bSearch_aux(e, l, i, j) == bSearch_aux(e, l, (m+1, j))), thus the property holds.
3. l[m] >= e:
  Same argument as case 2 but with "e cannot be in l[m+1:j]"
This completes the proof.

Timing analysis:
----------------
T(n) = O(n*logn), where O(n) comes from the cost of accessing the middle element of the list.
-}

-------------------------------
-- problem 2: primality testing
-------------------------------

expmod_aux b e m o
    | e == 0    = o
    | e_is_even = expmod_aux ((b*b)`rem`m) (e`div`2) m o
    | otherwise = expmod_aux b (e-1) m ((b*o)`rem`m)
    where
        e_is_even = (e`rem`2) == 0
expmod b e m = expmod_aux b e m 1

fermat_test n seed =
    let r = fst $ randomR (1, n-1) (mkStdGen (fromInteger seed)) in
    r == (expmod r n n)

prime :: Integer -> Integer -> Bool
prime 0 q = False
prime n 0 = True
prime n q =
  if not (fermat_test n seed)
  then False
  else prime n (q-1)
    where
      seed = toInteger (n*q)
-- prime 2017 10

------------------------------------
-- problem 3: higher order functions
------------------------------------

-- newton-raphson method
deriv f = let dx = 0.00001 in \x -> (f(x+dx) - f(x-dx))/(2.0*dx)

newton_raphson_transform f guess = guess - (f guess)/((deriv f) guess)

close_enough ng g e = abs (ng-g) < e

newton_raphson_root f g epsilon =
    let new_guess = newton_raphson_transform f g in
    if close_enough new_guess g epsilon
    then new_guess
    else newton_raphson_root f new_guess epsilon

{-
Explanation (or correctness argument):
--------------------------------------
For small enough dx, ((f(x+dx) - f(x-dx))/(2*dx)) is a numerical approximation for f'(x).
(deriv f) uses this fact to compute numerical approximation for derivative of f.

Newton-Raphson method states under certain assumptions for function f and initial guess g, (g' = g - (f(g)/f'(g))) is a better approximation of the root of f than g.
(newton_raphson_transform f guess) codes the above transformation using deriv for derivative.

The iteration stops if the new guess does not change beyond the old one by an additive factor of epsilon.

With a good enough initial guess, reasonable value of epsilon, and a function which satisfies the Newton-Raphson method's requirement, newton_raphson_root would return a root of f.

Timing analysis:
----------------
Each iteration step is O(1), the actual covergence depends on f, g, and epsilon.
For given f, g, and epsilon, if the algorithm were to terminate in n steps then the time complexity would be O(n).
-}

-- double summation method
double_sum f a b c d = sum (map (\i -> sum (map (\j -> f i j) [c..d])) [a..b])
{-
Correctness argument:
---------------------
Assuming the usual semantics for sum and map.
Innermost map computes sum of f(i,j) for j in [c,d] while the enclosing map calls sum on the former for i in [a,b].
The elements returned by innermost map would be: [f(i,c), f(i, c+1), ..., f(i, d)].
Applying sum and factoring in enclosing map, this becomes: [sum([f(a,c), f(a,c+1), ..., f(a,d)]), sum([f(a+1,c), f(a+1,c+1), ..., f(a+1,d)]), ..., sum([f(b,c), f(b,c+1), ..., f(b,d)])]
Applying the enclosing sum we have sum([sum([f(a,c), f(a,c+1), ..., f(a,d)]), sum([f(a+1,c), f(a+1,c+1), ..., f(a+1,d)]), ..., sum([f(b,c), f(b,c+1), ..., f(b,d)])]),
which matches the expression Sum{i=[a,b]}{j=[c,d]}{f(i,j)}.

Timing analysis:
----------------
Assuming O(1) time for f(i,j), the time complexity would be O((b-a)*(d-c)*O(1)) = O((b-a)*(d-c))
-}

main = do
  putStrLn "Assignment 1"
  putStr "Problem 1.1: "
  let l1 = [10,20,30,40]::[Int]
  putStrLn ("Reverse of list " ++ (show l1) ++ " = " ++ (show $ myreverse l1))
  putStr "Problem 1.2: "
  let l2 = [11,22,33,44]::[Int]
  putStrLn ("Sorted merge of lists " ++ (show l1) ++ " and " ++ (show l2) ++ " = " ++ (show $ merge l1 l2))
  putStr "Problem 1.3: "
  putStrLn ("16th fibonacci number = " ++ (show $ myfibo 16))
  putStr "Problem 1.4: "
  let l3 =  [1123,31,14154,3,410]
  putStrLn ("Insertion sort of list, " ++ (show $ l3) ++ ", = " ++ (show $ iSort l3))
  putStr "Problem 1.5: "
  putStrLn ("Quick sort of list, " ++ (show $ l3) ++ ", = " ++ (show $ qSort l3))
  putStr "Problem 1.6: "
  let num = 33::Int
  putStrLn ("Location index of number " ++ (show $ num) ++ " in list " ++ (show $ l2) ++ ": " ++ (show $ bSearch num l2))
  putStr "Problem 2: "
  putStrLn ("Is 2017 prime? " ++ (show $ prime 2017 10))
  putStr "Problem 3.1: "
  putStrLn ("Roots of (x^2 - x - 2 = 0) are x = " ++ (show $ newton_raphson_root (\x -> x*x - x - 2) (-10) 0.001) ++ " and x = " ++ (show $ newton_raphson_root (\x -> x*x - x - 2) 10 0.001))
  putStr "Problem 3.2: "
  putStrLn ("sum{i=1..10}{j=1..10}{i*j} = " ++ (show $ double_sum (\x y -> x*y) 1 10 1 10))
