module MyBigInteger  (add , multiply, karatsuba , fromString, toString, Invalid_Input_Exception )where 
import Control.Exception

data Invalid_Input_Exception = Invalid_Input_Exception String  deriving (Eq,Show)
instance Exception Invalid_Input_Exception

{-
Time Complexity : O(n)
-}
fromString :: String -> [Int]
fromString [] = []
fromString (x:xs) = (getDigitFromChar x):fromString xs

{-
Time Complexity : O(n)
-}
toString :: [Int] -> String
toString [] = []
toString (x:xs) = (getCharFromDigit x):toString xs

{-
Time Complexity : O(max(m+n)) 
m : length of argument 1
n : length of argument 2
-}
add :: String -> String -> Int -> String 
add s1 s2 b  
    | s1 == "" = add "0" s2 b 
    | s2 == "" = add s1 "0" b
    | bIncorrectInputS1 = throw (Invalid_Input_Exception "Invalid first argument to 'add' function. Kindly check the inputs.")
    | bIncorrectInputS2 = throw (Invalid_Input_Exception "Invalid second argument to 'add' function. Kindly check the inputs.")
    | otherwise = toString (addI (fromString s1) (fromString s2) b)
        where bIncorrectInputS1 = (isCorrectInput s1 b)==False
              bIncorrectInputS2 = (isCorrectInput s2 b)==False

{-
Time Complexity : O (n^lg2(3))
where n = max(x,y)
x : length of argument 1
y : length of argument 2
-}
multiply :: String -> String -> Int -> String 
multiply s1 s2 b 
    | s1 == "" = multiply "1" s2 b 
    | s2 == "" = multiply s1 "1" b
    | bIncorrectInputS1 = throw (Invalid_Input_Exception "Invalid first argument to 'multiply' function. Kindly check the inputs.")
    | bIncorrectInputS2 = throw (Invalid_Input_Exception "Invalid second argument to 'multiply' function. Kindly check the inputs.")
    | otherwise = toString (kMul (fromString s1) (fromString s2) b)
        where bIncorrectInputS1 = (isCorrectInput s1 b)==False
              bIncorrectInputS2 = (isCorrectInput s2 b)==False

karatsuba :: [Int] -> [Int] -> Int -> [Int] 
karatsuba l1 l2 b 
    | bIncorrectL1 = throw (Invalid_Input_Exception "Invalid first argument to 'karatsuba' function. Kindly check the inputs.")
    | bIncorrectL2 = throw (Invalid_Input_Exception "Invalid second argument to 'karatsuba' function. Kindly check the inputs.")
    | otherwise = kMul l1 l2 b 
        where bIncorrectL1 = (isValidNumberList l1 b) == False
              bIncorrectL2 = (isValidNumberList l2 b) == False

------------------------------------------------------
-- Helper Functions
getDigitFromChar '0' = 0
getDigitFromChar '1' = 1
getDigitFromChar '2' = 2
getDigitFromChar '3' = 3
getDigitFromChar '4' = 4
getDigitFromChar '5' = 5
getDigitFromChar '6' = 6
getDigitFromChar '7' = 7
getDigitFromChar '8' = 8
getDigitFromChar '9' = 9
getDigitFromChar x = 0

getCharFromDigit 0 = '0' 
getCharFromDigit 1 = '1'
getCharFromDigit 2 = '2'
getCharFromDigit 3 = '3'
getCharFromDigit 4 = '4'
getCharFromDigit 5 = '5'
getCharFromDigit 6 = '6'
getCharFromDigit 7 = '7'
getCharFromDigit 8 = '8'
getCharFromDigit 9 = '9'
getCharFromDigit x = '0'

isValidDigit '0' b = (getDigitFromChar '0') < b
isValidDigit '1' b = (getDigitFromChar '1') < b
isValidDigit '2' b = (getDigitFromChar '2') < b
isValidDigit '3' b = (getDigitFromChar '3') < b
isValidDigit '4' b = (getDigitFromChar '4') < b
isValidDigit '5' b = (getDigitFromChar '5') < b
isValidDigit '6' b = (getDigitFromChar '6') < b
isValidDigit '7' b = (getDigitFromChar '7') < b
isValidDigit '8' b = (getDigitFromChar '8') < b
isValidDigit '9' b = (getDigitFromChar '9') < b
isValidDigit x  b = False

isCorrectInput [] b = True
isCorrectInput (x:xs) b = if (isValidDigit x b) 
                          then isCorrectInput xs b
                          else False

addI [] [] b = []
addI [] l2 b = l2
addI l1 [] b = l1
addI l1 l2 b   = let revResult = addHelper (reverse l1) (reverse l2) 0 b []
                in reverse revResult

addHelper [] [] c b result = if c > 0 then result++[c] else result
addHelper (x:xs) [] c b result = let s = (x+c) `mod` b 
                                     cc = if (x+c)>= b then 1 else 0
                                 in addHelper xs [] cc b (result++[s])
addHelper [] (y:ys) c b result = let s = (y+c) `mod` b 
                                     cc = if (y+c)>= b then 1 else 0
                                 in addHelper [] ys cc b (result++[s])
addHelper (x:xs) (y:ys) c b result = let s = (x + y + c) `mod` b
                                         cc = if (x+y+c) >= b then 1 else 0
                                     in addHelper xs ys cc b (result++[s])

isValidNumberList [] b = True
isValidNumberList (x:xs) b = if (x<b) then (isValidNumberList xs b) else False

kMul l1 l2 base = 
               if ((length l1) ==(length l2)) && ((length l1) == 1)
               then removeFrontZeros (getSimpleProduct (head l1) (head l2) base)
               else 
                   removeFrontZeros (kMulHelper lxe lye base)
                   where len1 = length l1
                         len2 = length l2
                         lx = appendZeors l1 (max len1 len2)
                         ly = appendZeors l2 (max len1 len2)
                         lxe = makeEvenSize lx
                         lye = makeEvenSize ly


kMulHelper l1 l2 base  = addI t1 (addI t2 t3 base) base
                         where  (a,b) = breakListHalf l1
                                (c,d) = breakListHalf l2
                                x = kMul a c base
                                y = kMul b d base
                                z = kMul (addI a b base) (addI c d base) base
                                w = sub z (addI x y base) base
                                n = length a
                                t1 = appendZerosBack x (2*n)
                                t2 = y
                                t3 = appendZerosBack w n

appendZeors l x = if length l < x then appendZeors (0:l) x else l

makeEvenSize l = if (length l) `mod` 2 == 0 then  l else 0:l     


getSimpleProduct a b base = if res >= base
                            then (res`div`base):(res`mod`base):[]
                            else [res]
                            where res = a*b             

appendZerosBack l x = if x == 0 then l else appendZerosBackHelper l x []

appendZerosBackHelper l x lRes = if x == 0 then l ++ lRes else appendZerosBackHelper l (x-1) (0:lRes)

sub l1 l2 base = 
                if lenS > len1 then removeFrontZeros(addI (tail tempSum) [1] base)
                else 
                    --if adding one, increase the length. Means the result is zero.
                    if length (addI tempSum [1] base) > len1
                    then [0]
                    else error "Subtracting leads to a negative result."
                where lenl1 = length l1
                      lenl2 = length l2
                      lx = appendZeors l1 (max lenl1 lenl2)
                      ly = appendZeors l2 (max lenl1 lenl2)
                      l2x = reducedRadix ly base
                      tempSum = addI lx l2x base 
                      lenS = length tempSum
                      len1 = length lx

reducedRadix l base =   [base-1-x | x<-l]

breakListHalf l = (take x l,drop x l) where x = (length l)`div`2

removeFrontZeros [] = [0]
removeFrontZeros (x:xs) = if x == 0 then removeFrontZeros xs else (x:xs)

