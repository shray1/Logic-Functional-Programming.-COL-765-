module Typing (getType) where

import Ast
import Lexer
import Parser
import Control.Exception

data Type_Exception = Type_Exception String  deriving (Eq,Show)
instance Exception Type_Exception

{-
Type data structure defined in Ast module. 

data ExprType = IntType | BoolType | Arrow ExprType ExprType 
                deriving (Eq,Show)

-}


{-
Plus,Minus,Times  :: Int operands -> Int output
And,Or,Xor,Implies :: Bool operands -> Bool Output
Greater,Lesser :: Int operands -> Bool Output
-}
getType (BinExpr op a b)  l 
        | (bSameType == False) = throw (Type_Exception "Error")
        | op == Equals  = BoolType 
        | bTFOp         = BoolType
        | bIntBoolOp    = BoolType
        | bIntOp        = IntType
        -- otherwise        = IntType --Should never reach here.
            where aType = getType a l
                  bType = getType b l 
                  bSameType = (aType == bType) 
                  bTFOp    = (op == And) || (op == Or)  || (op == Xor) || (op == Implies)  
                  bIntBoolOp = (op == LessThan) || (op == GreaterThan) 
                  bIntOp   = (op == Plus) || (op == Minus) || (op == Times) 
{-
Not :: Bool operand -> Bool output
Negate :: Int operand -> Int output
-}
getType (UnExpr op a) l 
        | op == Negate = if aType == IntType then IntType else throw (Type_Exception "Expected operand for Negate operator is Integer type. Please check the argument to Negate Operator.")
        | op == Not    = if aType == BoolType then BoolType else throw (Type_Exception "Expected operand for Not operator is Boolean type. Please check the argument to Not Operator.")
        -- otherwise = BoolType --Should never reach here. 
            where  aType = getType a l

{-
condition expr => bool type 
b , c => b,c should be of the same type. 
-}
getType (Ite a b c) l 
    | aNotBool = throw (Type_Exception "Expected type of conditional expression in ITE expression is Boolean. Please check the ITE expression.")
    | cType /= bType = throw (Type_Exception "Expected same types of <expr1> and <expr2> in ITE <condition> <expr1> <expr2>. Please check the ITE expression.")
    | otherwise = bType
        where aType = getType a l
              bType = getType b l
              cType = getType c l 
              aNotBool = aType /= BoolType 
getType (BoolConst a) l  = BoolType
getType (IntConst  a) l  = IntType
getType (VarExpr id) l  = fetchIdTypeFromList l id 
getType (Let (Decl id idexp) b) l  =  bType 
                                        where idType = getType idexp l  
                                              l1 =  (id,idType):l
                                              bType = getType b l1  
{-
Application Expr (A B) 
Here, A cannot be a primitive type. It should be able to take an argument. 
The argument type of A should match with B type.
-}
getType (AppExpr a b) l
        | isAPrimitiveType =  throw (Type_Exception "First Paramter in Application Expression cannot be a primitive type. (i.e. First argument cannot be of Int/Bool type)")
        | (bType /= argType) = throw (Type_Exception "Expected argument type to 'A' in Application Expression (A B) does not match with type of 'B'.")
        | otherwise = returnType 
            where aType = getType a l
                  bType = getType b l
                  isAPrimitiveType = (aType==IntType) || (aType==BoolType)
                  (Arrow argType returnType) = aType
{-
Fn 
The body expr type on the basis of argument type, will become our return type.
-} 
getType (Fn a argtype b)  l 
        =   Arrow argtype btype 
            where   (VarExpr arg) = a 
                    -- Parser makes sure, that "a" is of only VarExpr type.
                    l1 = (arg,argtype):l 
                    btype = getType b l1 
{-
Fun 
The body expr type should match the return type of the function.
We can evaluate the body expr type by replacing the arguments with its type. 
-}
getType (Fun name arg argType retType bodyExpr)  l 
        =  if bodyType==retType then funType else throw (Type_Exception ("Return type of function '" ++ funcId ++ "' does not match with the type of body expression of this function."))
            where funType = Arrow argType retType
                  (VarExpr funcId) = name
                  (VarExpr argId)  = arg 
                  l1 = (funcId,funType):l 
                  l2 = (argId, argType):l1 
                  bodyType = getType bodyExpr l2 


fetchIdTypeFromList [] id = throw (Type_Exception ("No type found for symbol '" ++ id ++ "' in the type environment table."))
fetchIdTypeFromList (x:xs) id 
    | id == a = b 
    | otherwise = fetchIdTypeFromList xs id 
       where (a,b) = x 

