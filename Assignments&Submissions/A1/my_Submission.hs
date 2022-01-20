import System.Random

{-
BEGIN_SOLUTION_Q1
-}


tReverseList l = tReverseListHelper l []

tReverseListHelper [] lnew = lnew 
tReverseListHelper (x:xs) lnew = tReverseListHelper xs (x:lnew)
{-
Proof Of Correctness: 
-> We take an auxillary variable 'lnew' to store the reversed list (output)
    while computing. 

Correctness Invariant : 
base case: if the input list is empty, then the output is empty as well. 
           Since, lnew is an empty list at the starting. 

IH :   Let 'l' be a list of length 'n' elements such that
            l = [a1, a2, a3.... a(n)]
        Then output of tReverseList is 
            lnew = [a(n), ... a3, a2 , a1]

        So we have, 
            tReverseListHelper l lnew = tReverseListHelper [] [a(n),...a3,a2,a1]

IS :  Let 'l' be a list of length n', where n'=n+1
           l = [a1, a2, a3....a(n), a(n+1)]
      Now, 
          tReverseListHelper (x:xs) lnew = tReverseListHelper xs (x:lnew)
    

Time Complexity : 
At, each step we are taking the head of list 'l' and then 
appending it to our auxillary list 'lnew'. Hence 
T(n) = T(n-1) + 1
     = T(n-2) + 1 + 1
     = ... 
     = O(n)

-}


merge l1 l2 = mergeHelper l1 l2 []
mergeHelper [] [] lnew  = lnew 
mergeHelper l1 [] lnew  = lnew ++ l1 
mergeHelper [] l2 lnew  = lnew ++ l2
mergeHelper (x:xs) (y:ys) lnew  = 
        if x < y
        then mergeHelper xs (y:ys) (lnew++[x]) 
        else mergeHelper (x:xs) ys (lnew++[y])

{-
Proof of Correctness : 
Assumptions-> Input lists l1,l2 are sorted in ascending order.

Loop invariant => At start of the each recursive call 'n' of mergeHelper,
                  we have 'lnew' containing the smallest 'n-1' elements of 
                  l1 and l2 in sorted order. Also the head of l1 and l2 depict
                  the smallest elements in list l1 and l2. 

Initialisation -> At the first call, the list 'lnew' is empty and head of l1 and l2
                  depict the smallest element in l1 and l2. 

Maintainence -> Now, only the smaller element of l1[0] and l2[0]
                is appened to the list 'lnew'. Then the function is recursively calls again.

Termination -> Case 1, l2 becomes empty : In this case, l1 is appened to lnew, since all elements 
                                            in l1 are greater than the elements in lnew. And l1 is a sorted list.

               Case 2, l1 becomes empty : In this case, l2 is appened to lnew, since all elements 
                                            in l1 are greater than the elements in lnew. And l2 is a sorted list.

            Hence, our new list, 'lnew' contains the merged elements in sorted fashion.

Time Complexity : 
T(n,m) = O(n + m) 
    since, we will iterate over both the list of size n and m.
    Hence, O(n+m). 
-}

tFib n = 
    if n == 0
    then 0
    else tFibHelper n 1 0
tFibHelper n a b = 
    if n==0
    then b
    else tFibHelper(n-1) (a+b) a

{-
    Fibonacci 
    F(n) =  0                   ,if n == 0
            1                   ,if n == 1
            F(n-1) + F(n-2)     ,otherwise.

    Now, we keep two auxillary variables a, b when building 
    tail recursive solution :
        a holds the value of F(x+1),
        b holds the value of F(x)
    
        F(x) = F(x-1)+F(x-2)
Base Case : 
    n == 0 , we have tFibHelper returning b, which is 
             initialised to 0, when called from tFib function.

IH :  
    For n>0, n belongs to natural number. 
    tFib n = tFibHelper n 1 0 = F(n)

IS : 
    Let n'=n+1. 
    So we have, 
    tFib n' = tFibHelper n' 1 0 
            = tFibHelper 1  F(n+1) F(n) 
            -- [From IH] and [n'=n+1], so we have 1 left on the first argument.
            = tFibHelper 0  (F(n)+F(n+1)) F(n)+F(n-1)
            = F(n)+F(n-1)
            = F((n+1)- 1) + F((n+1)-2) 
            = F(n+1)
        since F(x) = F(x-1)+F(x-2)
    Hence proved. 


Time Complexity : 
T(n) = 1          ,if n==0
       T(n-1)+1   ,if n>0


Now, computing the relation, 
T(n) = T(n-1) + 1
     = T(n-2) + 1 + 1 
     = ...
     = T(0) + n
     = O(n) 

-}



insert x ls = insertHelper x ls []   
insertHelper x [] l = l++[x]
insertHelper x (y:ys) lnew = 
    if x < y
    then lnew ++ [x] ++ y:ys
    else insertHelper x ys (lnew ++ [y])


insertionSort l  = insertionSortHelper l []
insertionSortHelper [] l = l
insertionSortHelper (x:xs) l = insertionSortHelper xs (insert x l)
{-

Proof Of Correctness for tail recursive insert function.
-----------------------------------------------------------------------
    insert x ls = insertHelper x ls []   
    insertHelper x [] l = l++[x]
    insertHelper x (y:ys) lnew  = 
                             if x < y
                             then lnew + [x] + [y:ys]
                             else insertHelper x ys (lnew ++ [y])
-----------------------------------------------------------------------

Auxially variable 'lnew' to hold the output at each step of insertHelper. 

Base Case: 
    If input list 'ls' is empty, 
    we insert the element 'x' directly into the new list 'lnew', which is empty
    and return. 

IH :  Let 'ls' be a sorted list = [a0, a1, .... a(k-1),a(k)]. 
      And let x be a number, using insertHelper, we assume that 
      insertHelper will return a sorted list of lenght k. 


IS :  over n, n = k+1
      Let 'ls' be a sorted list = [a0, a1, .... a(n-1), a(n)]
      Let x be a number, 
        Case a:   x < ai , wehre ai belongs to ls. 
                 (x is smallest element)         
                 In this case, x should be present in the starting of new list.
                 At the first check, we can see that, 
                 x < a0, therefore our output list becomes -> [x, a0, a1....a(n)]
                        
        Case b:  x > ai , where ai belongs to ls.
                (x is greatest element)
                In this case, x should be present at the end of new list 'lnew'.
                Till the a(n) element of list 'ls' is 
                 x > a(n), therefore our output list becomes -> [a0, a1....a(n),x]
                since, the input list 'ls' exhausts and becomes empty, at which we 
                append x to our auxillary list lnew and return the list.

        Case c: x lies somewhere in btw. 
                    Let a(i) be the next greater element to x, where i>0 and i<n. 
                    Now, we can see, that we take out the first element of list l,
                    till the point we reach the index i. 
                    So at this point our lnew = [a0,a1,...a(i-1)]
                    Now,
                     x < a(i)
                    Hence, we first add x to our new list and then append the remaining list l
                    to our auxillary list lnew. 

Time Complexity of insert function: 
T(n) = 1 + T(n-1)
    = O(n)


Correctness Proof of Insertion Sort:
Let P(n) denote that insertion sort works correctly over list of size n. 
Base Case: n=0, Insertion sort should return an empty list for an empty input list.
                Hence this is correct.

Inductive Hypothesis : P(k) holds true and sorts the list correctly for k<n.
Induction Step over n : For n=k+1, we have an a sorted list of length k. (Using IH)
                        Now for the last element, we insert the element into its right place
                        in the already sorted list of size k. 
                        Now, since our insert functions works correctly (Proof given above)
                        hence our induction holds true over n. 

Time Complexity : 
We know,  Time complexity of insert function = O(n) where n is the length of list
Also, in insertionSort, we see that 
T(x) = T(x-1) + O(1)
     = T(x-2) + O(2) + O(1)
     = T(x-3) + O(3) + O(2) + O(1)
     = ... 
     = O(n) + O(n-1) + .. O(1)
     = O(n^2)
-}


quickSort [] = []
quickSort (x:xs) =  
    let l1 =partitionSmaller x xs;
        l2 =partitionBigger x xs;
    in 
    quickSort(l1) ++ [x] ++ quickSort(l2)


partitionBigger x [] = []  
partitionBigger x (y:ys) =
    if x < y
    then y:(partitionBigger x ys)
    else partitionBigger x ys


partitionSmaller x [] = []
partitionSmaller x (y:ys) = 
    if x < y
    then partitionSmaller x ys
    else y:(partitionSmaller x ys)



{-
Correctness for partition algorithm :
-- It is evident, from the code that on iterating the input list, 
    we keep on adding elements to a new list, for the smaller and 
    larger elements. Thus, the new list formed only contains elements 
    larger/smaller than the pivot element. 
Time Complexity for partition algorithm : 
T(n) = O(n) , as we do a full iteration over our input list. 



Correctness proof quickSort: 
Q(n) : Quicksort over list of size n. 
Base Case : 
Q(1) is a list of size 1, and such a list is already sorted.

Indutive Hypothesis:
Q(k) sorts the list correctly. Where k < n.

Inductive Step: 
For n, we have 
Q(n) = Q([list of smaller elements])++ [head element of list] + Q([list of bigger elements])
From our hypothesis we have our sorted lists of smaller elements and bigger elements 
than the the first element(pivot) of the list. 
And since, the pivot is located in the correct position, Q(n) will return a sorted list. 
(Pivot is located at correct position since, it is ahead of all elements less than it,
and it is behind all elements greater than it.)
Hence proved. 

Time Complexity : 
T(n) = T(k) + T(n-k-1) + 2n
where k is the number of elements smaller than the pivot. 

Now, for our worst case scenario, we can have 0 elements smaller than the pivot element. 
So we have for worst case, 
T(n) = T(n-1) + 2n
     = O(n^2)

For best case, we can have (n-1)/2 smaller elements than the pivot. 
So we have for best case, 

T(n) = T((n-1)/2) + T((n-1)/2) + 2n 
     = O(n*log(n))

-}

binarySearch x l = bsHelper x l 0 (length l)

bsHelper x l si ei 
    | si > ei      = -1
    | midValue < x = bsHelper x l (mid+1) ei
    | midValue > x = bsHelper x l si  (mid-1)
    | otherwise  = mid
    where mid = (si+ei) `quot` 2
          midValue = at mid l 

at x (y:ys) = 
    if x == 0 
    then y
    else at (x-1) (ys)

{-
Assumption : Input list is sorted.
Correctness Proof for bsHelper : 
Let P(n) denote that bsHelper works correctly for its inputs si and ei, 
        where n = ei - si
        searching for element x.
Base Case : si == ei == mid 
        Now in this case , if element at 'mid' is equal to x, then we can 
        see it works correctly. And if the element is not equal to x, then we 
        see that, in the next function call, si becomes greater than ei, and therefore
        it returns -1, thus denoting that the function works correctly.

Induction Hypotheses : P(k) works correctly for k<n. 

Induction Step over n.
    Now as we can see from the code, we have 3 cases,
    Case 1: list[mid] < x. Element at mid point is smaller than x, then 
            we call bsHelper again but with si'=mid+1 and ei'=ei.
            Now we can see that from our assumption, all elements 
            after index 'mid' will be greater than the current element. 
            Hence P(n - mid) works correctly from IH. 
    Case 2 : list[mid] > x.  Element at mid point is greater than x, then 
            we call bsHelper again but with si'=si and ei'=mid-1.
            Now we can see that from our assumption, all elements 
            before index 'mid' will be smaller than the current element.
            Hence P(n-mid) works correctly from IH.
    Case 3 : list[mid] == x. Element at mid point is equal to x. This will then 
            return the current index and hence the function evaluates correctly. 


Time Complexity : 
T(n) = [TimeComplexity of at function] + T(n/2)
     = n + T(n/2)   
     (since 'at' function iterates over the whole input list till the index is reached)
     = O(n*log(n))

Kindly note, if the element access in the list, is O(1), as in the case of arrays, 
We will have 
T(n) = 1 + T(n/2)
    = O(log n)


-}

{-
END_SOLUTION_Q1
-}


{-
BEGIN_SOLUTION_Q2
Solution to Q2. 
Fermats Theorem. 
-}

prime n q = primeHelper n False (getRandomNumberList 1 (n-1) q)

primeHelper n result [] = result
primeHelper n result (a:l) = primeHelper n (doesFermatHolds a n) l

{-
Reference for Random Numbers : 
https://www.markhneedham.com/blog/2012/05/08/haskell-generating-random-numbers/
-}
getRandomNumberList low high size = getRandNumListHelper low high size []
getRandNumListHelper low high size lrandom= 
    let randomValue = fst (randomR (low,high)  (mkStdGen size))
    in 
        if size == 0 
        then lrandom
        else getRandNumListHelper low high (size-1) (randomValue:lrandom) 

doesFermatHolds:: Integer -> Integer -> Bool
doesFermatHolds a n = 
    if (val `rem` n) == a
    then True
    else False
    where val = fastPowerWithMod a n

fastPowerWithMod :: Integer -> Integer -> Integer 
fastPowerWithMod a n = fastPowerModHelper a n 1 n

fastPowerModHelper :: Integer -> Integer -> Integer -> Integer ->Integer
fastPowerModHelper a n x modN = 
    if n==0
    then (x`mod`modN) 
    else 
        if n `rem` 2 == 1
        then fastPowerModHelper ((a*a)`mod`modN) (n`quot`2) ((a*x)`mod`modN) modN
        else fastPowerModHelper ((a*a)`mod`modN) (n`quot`2) x modN

{-
END_SOLUTION_Q2
-}




{-
BEGIN_SOLUTION_Q3 
Part a ) Newtons Method For Computing Root.
Part b ) Higher Order Submisison
-}

{-
Part a.
-}

{-
Correctness proof :: 
The method is an implementation of Newton's method to estimate root, 
we can assume that the Newton's method provides a way to get the estimated root over 
several iterations. 
IH = > Newton methods converges the guess to an approximate root
IS :: Let, the guess be far away from the root of a function. 
      Now we can say that over serveral iterations, the newton methods 
      converges the guess to an approximate root. (Using IH)
      Hence IS is correct too. 

Time Complexity : 
  Let n, denote the number of iterations

  T(newtonMethod) = O(1) for a single iterations,
  T(n) = n for 'n' number of iterations.
-}
newtonRoot f guess error
    | rootDiff < error = rootValue1
    | otherwise = newtonRoot f rootValue1 error
    where rootDiff = abs (rootValue1 - guess)
          rootValue1 = guess - ((f guess)/(funcDerivative f guess 0.001))

funcDerivative func x delta
    = ((func (x+delta)) - (func (x-delta))) / (2*delta)

{-
newtonRoot method might not stop...since the accuracy may 
never be reached.

safeNewtonRoot f guess error count
    | count > 1000 = guess
    | rootDiff < error = rootValue1
    | otherwise = safeNewtonRoot f rootValue1 error (count+1)
    where rootDiff = abs (rootValue1 - guess)
          rootValue1 = guess - ((f guess)/(funcDerivative f guess 0.001))
-}


{-
Part b. 
Submission(a to b) { Submission (c to d)  { f(i,j) }  }

Proof: 
Lets prove the inner summission first, then we will use it to prove the outer summission. 

Inner summission ::
sumCtoD a c d sj f = 
    if c > d 
    then 0
    else (f a c) + sumCtoD a (c+sj) d sj f

Now, 
Base Case : For c > d , it returns 0, which is correct since it doesnt go 
            into the summission and there should be no term. 
IH : sumCtoD gives correct result over i', where
     d = c + i'*sj   where sj>0, i>0.  [sj is the step for c in each term]
     sumCtoD over c -> d, (where c,d) gives correct output when c is stepped by an increment of sj.
     sumCtoD = summission(f(a,x)) , x=c+(i*sj) till x reaches d, where i increments from 0 to i'.

IS : Now, for  
     d' = c + (i'+1)sj  , i > 0, sj>0.
     sumCtoD over c->d'
              = summission(f(a,x)) where x=c+(i*sj) where i increments from 0 to i'+1
              = (f(a,c+i'd)) + summission(f(a,x)) where x=c+(i*sj) where i increments from 0 to i'
              = (f(a,c+i'd)) + sumCtoD over c->d    [using IH]
              = sumCtoD over c->d' 

    Hence proved. 
                                                                  
                                                                  
For outer summission, we can prove using the same way and hence the 
implementation is correct. 


Time Complexity = 
  Let n,m be the number of terms in outer and inner summission respectively.
  Now, 
  T(n,m) = n * T(m)
         = n * (m * f(x,y))
         = n*m*T(f)
         = nm * T(f)
    where T(f) represents time complextity of the function passed as parameter.
-}  
ans3b a b c d f = sumAtoB a b c d 1 1 f

sumAtoB a b c d si sj f = 
    if a > b 
    then 0
    else (sumCtoD a c d sj f) + sumAtoB (a+si) b c d si sj f

sumCtoD a c d sj f = 
    if c > d 
    then 0
    else (f a c) + sumCtoD a (c+sj) d sj f




