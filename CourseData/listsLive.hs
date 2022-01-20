lenL [] = 0
lenL (x:xs) = 1 + (lenL xs)

empty ls
      | ls == [] = True
      | otherwise = False

empty1 [] = True
empty1 (x:xs) = False            

singleton ls =
    if empty ls
    then False
    else empty (tail ls) -- function composition

myMax a b = if a>=b
            then a
            else b

maxL ls = if singleton ls
          then head ls
          else myMax (head ls) (maxL (tail ls))

-- expecting that you will write the inductive
-- correctness proofs 

appendL l1 l2 =
    case l1 of
      [] -> l2
      (x:xs) ->  x:(appendL xs l2)
{-
[1,2] ++ [3,4]

1: appendL [2] [3,4]
1: 2 : appendL [] [3,4]
1: 2 : [3,4]  
[1,2,3,4] 
-}

{- 
[1,2,3]
[2,3] ++ [1]
[3] ++ [2,1]
[] ++ [3,2,1]
-- stop
-}

reverseL ls =
    case ls of
      [] -> []
      (x:xs) ->  (reverseL xs)++[x]

-- write the inductive proof of correctness
-- Develop an iterative revIter ls and
-- write its proof of correctness!
-- Do the time complexity analysis

-- sorting of lists

-- insert an element in a sorted list
-- assumption is that the list is a sorted
-- list 
insert a ls =
    case ls of
      [] -> [a]
      (x:xs) -> if a< x
                then a:x:xs
                else x: (insert a xs)

{- insertion sort 
insert the head of the list 
at the right position in the output list
recursively!
iSort :: aLst -> aListSorted
Time complexity 
proof of correctness yourselves 
-}

iSort ls =
    case ls of
      [] -> []
      (x:xs) ->  insert x (iSort xs)
-- insert x ( insert x' ( iSort xs')
-- insert x ( insert x' ( insert x'' (iSort xs''_) )

{-
Iterative variation of iSort. 
Proof of correctness and time complexity 
analysis is also required 
-}

-- mergeSort
-- takes two "sorted" lists and merges them!

merge [] l2 = l2
merge l1 [] = l1
merge (x:xs) (y:ys) =
    if x <= y
    then x:(merge xs (y:ys))
    else y:(merge (x:xs) ys)

splitL ls =
    case ls of
      [] -> ([], [])
      [x] -> ([x], [])
      x:y:xs -> let
                (p1, p2) = splitL xs
           in 
             (x:p1, y:p2)

mSort ls =
    case ls of
      [] -> []
      [x] -> [x]
      _ -> let (p1, p2) = splitL ls
           in merge (mSort p1) (mSort p2)
-- write its proof of correctness
-- write a tail recursive version
-- perform time complexity analysis
-- of the recursive variant! 
