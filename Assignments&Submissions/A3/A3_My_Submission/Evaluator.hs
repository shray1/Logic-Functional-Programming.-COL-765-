module Evaluator (eval) where

import Ast
import Lexer
import Parser
import Typing
import Control.Exception

data Eval_Exception = Eval_Exception String  deriving (Eq,Show)
instance Exception Eval_Exception


{-
    eval function will return EResult. 
-}
data EResult = ResultI Integer | ResultB Bool | ResultExpr Expr 
                deriving (Eq,Show)


{-
In the eval function, "l" denotes the evaluation environment.. (acting as the symbol table) 

eval :: Expr -> list -> EResult 
list containts tuples of type (String,EResult) 
-}

eval (BinExpr op a b) l 
    | op == Plus = ResultI (aint + bint)
    | op == Minus = ResultI (aint - bint)
    | op == Times = ResultI (aint * bint)
    | op == Equals = ResultB (abEqualVal) 
    | op == LessThan = ResultB (aint < bint)
    | op == GreaterThan = ResultB (aint > bint)
    | op == And = ResultB (abool && bbool)
    | op == Or = ResultB (abool || bbool)
    | op == Xor =  ResultB (xorFunction abool bbool)
    | otherwise  = throw (Eval_Exception "Invalid Binary Operator.")
        where evalA = eval a l 
              evalB = eval b l
              (ResultI aint) = evalA
              (ResultI bint) = evalB
              (ResultB abool) = evalA 
              (ResultB bbool) = evalB
              abEqualVal = equalValues evalA evalB 
eval (UnExpr op a) l 
        | op == Negate = ResultI (aint * (-1))
        | op == Not  = ResultB (notFunction abool)
        | otherwise  = throw (Eval_Exception "Invalid Unary Operator.")
            where evalA = eval a l 
                  (ResultI aint) = evalA
                  (ResultB abool) = evalA 
eval (Ite a b c) l 
    | condition  = eval b l 
    | otherwise  = eval c l 
        where (ResultB condition) = (eval a l)
eval (Let aVar bodyExpr) l
    = bodyValue
    where (Decl id idExpr) = aVar 
          idValue = eval idExpr l 
          --idString = getString id
          l1 = (id,idValue):l 
          bodyValue = eval bodyExpr l1 
eval (BoolConst a) l  = ResultB a 
eval (IntConst  a) l  = ResultI a
eval (VarExpr id) l   = fetchIdValueFromList l id 
eval (AppExpr (Fn  argExpr argTypeExpr bodyExpr) paramExpr) l 
    = appValue
    where paramValue = eval paramExpr l
          (VarExpr id) = argExpr
          l1 = (id,paramValue):l 
          appValue = eval bodyExpr l1   
eval (AppExpr (Fun funcNameExpr argExpr argTypeExpr retTypeExpr bodyExpr) paramExpr) l 
    = appValue
    where paramValue = eval paramExpr l
          (VarExpr id) = argExpr
          (VarExpr funcId) = funcNameExpr
          l1 = (id,paramValue):l
          l2 = (funcId,ResultExpr (Fun funcNameExpr argExpr argTypeExpr retTypeExpr bodyExpr)):l1
          appValue = eval bodyExpr l2
eval (AppExpr (VarExpr id) y) l  =  
    case idValResult of 
    (ResultExpr idVal) -> eval (AppExpr idVal y) l
    otherwise -> throw (Eval_Exception ("First Argument in an application"
                        ++ " expression can only be a typed or untyped funtion."
                        ++ "'" ++ id ++ "' not a valid function.")) 
    where idValResult = fetchIdValueFromList l id
eval (AppExpr x y) l =  throw (Eval_Exception ("First Argument in an application"
                        ++ " expression can only be a typed or untyped function.")) 
eval x l  = ResultExpr x 


{-Helper Functions-}
equalValues (ResultB a) (ResultB b) = a == b 
equalValues (ResultI a) (ResultI b) = a == b 
equalValues  x  y  = throw (Eval_Exception "Invalid arguments to Equals Operator.")

xorFunction True True = False
xorFunction True False = True
xorFunction False True = True
xorFunction False False = False


notFunction True = False
notFunction False = True

fetchIdValueFromList [] idName = throw (Eval_Exception ("No Evaluation found for '" ++ idName ++ "'."))
fetchIdValueFromList (x:xs) idName = if (xid == idName) then xValue else fetchIdValueFromList xs idName 
                                    where (xid,xValue) = x 

getString :: Id -> [Char]
getString id = id







