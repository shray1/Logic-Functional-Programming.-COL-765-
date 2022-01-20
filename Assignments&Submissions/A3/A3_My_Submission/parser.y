{
module Parser where
import Ast
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token
       ";"     { EOF $$ }
       "("     { LPAREN $$ }
       ")"     { RPAREN $$ }
       -- int ops
       "+"     { PLUS $$ }
       "-"     { MINUS $$ }
       "*"     { TIMES $$ }
       "~"     { NEGATE $$ }
       "="     { EQUALS $$ }
       "<"     { LESSTHAN $$ }
       ">"     { GREATERTHAN $$ }
       -- bool ops
       "!"     { NOT $$ }
       "&&"    { AND $$ }
       "||"    { OR $$ }
       "^"     { XOR $$ }
       "=>"    { IMPLIES $$ }
       -- ite
       "if"    { IF $$ }
       "then"  { THEN $$ }
       "else"  { ELSE $$ }
       "fi"    { FI $$ }
       -- let
       "let"   { LET $$ }
       ":="    { ASSIGN $$ }
       "in"    { IN $$ }
       "end"   { END $$ }


       "fn"      { FN $$ }
       "fun"     { FUN $$ } 
       "::"       { DBLCOLON $$ }
       "->"      { ARROW $$ }
       "int"     { INT $$}
       "bool"    { BOOL $$}

       -- atoms
       "false" { FALSE $$ }
       "true"  { TRUE $$ }
       int     { CONST $$ }
       var     { ID $$ }


%nonassoc ">" "<" "=" "->" "=>"
%left "+" "-"
%left "*"
%left "^" "||" "&&"
%right "!" "~"

%%

Start
    : Expr ";"				     { $1 }

Decl
    : var ":=" Expr                          { Decl $1 $3 }

Expr
    : "(" Expr ")"                           { $2 }

    -- int ops
    | "~" Expr                               { UnExpr Negate $2 }
    | Expr "+" Expr                          { BinExpr Plus $1 $3 }
    | Expr "-" Expr                          { BinExpr Minus $1 $3 }
    | Expr "*" Expr                          { BinExpr Times $1 $3 }
    | Expr "=" Expr                          { BinExpr Equals $1 $3 }
    | Expr "<" Expr                          { BinExpr LessThan $1 $3 }
    | Expr ">" Expr                          { BinExpr GreaterThan $1 $3 }

    -- bool ops
    | "!" Expr                               { UnExpr Not $2 }
    | Expr "&&" Expr                         { BinExpr And $1 $3 }
    | Expr "||" Expr                         { BinExpr Or $1 $3 }
    | Expr "^" Expr                          { BinExpr Xor $1 $3 }
    -- | Expr "=>" Expr                         { BinExpr Implies $1 $3 }

    -- specials
    | "let" Decl "in" Expr "end"             { Let $2 $4 }
    | "if" Expr "then" Expr "else" Expr "fi" { Ite $2 $4 $6 }


    | "(" Expr Expr ")"                              { AppExpr $2 $3 }
    | "fn" "(" VarExp "::" ExprType ")" "=>" Expr "end"   { Fn $3 $5 $8 }
    | "fun" VarExp "(" VarExp "::" ExprType ")" 
        "::" ExprType "=>" Expr "end"                     {Fun $2 $4 $6 $9 $11}

    -- atom
    | "false"                                { BoolConst False }
    | "true"                                 { BoolConst True }
    | int                                    { IntConst $1 }
    | var                                    { VarExpr $1 }


ExprType 
    : "int"         { IntType  }
    | "bool"        { BoolType }
    | ExprType "->" ExprType    {Arrow $1 $3} 

VarExp 
    : var           {VarExpr $1}

{

parseError [] = error $ "Syntax Error in the last token"
parseError (x:xs) = error $ "Syntax Error: " ++ (show x)

}
