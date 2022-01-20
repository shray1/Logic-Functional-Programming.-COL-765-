module Ast where

type Id = String

data VarDecl = Decl Id Expr
        deriving (Eq,Show)

data UnOp = Negate | Not
        deriving (Eq,Show)

data BinOp = Plus | Minus | Times | Equals | LessThan | GreaterThan | And | Or | Xor | Implies
        deriving (Eq,Show)

data Expr =
             BinExpr BinOp Expr Expr
           | UnExpr UnOp Expr
           | Ite Expr Expr Expr
           | Let VarDecl Expr
           | BoolConst Bool
           | IntConst Integer
           | VarExpr Id
        deriving (Eq,Show)
