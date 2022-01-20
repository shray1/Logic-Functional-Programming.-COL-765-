{
module Lexer  where
--module Tokens_posn (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
import Control.Exception

}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
  $white+       ;
  "--".*        ;
  let           { tok (\p s -> LetP p) }
  in            { tok (\p s -> InP p) }
  if            { tok (\p s -> IfP p) }
  fi            { tok (\p s -> FiP p) }
  end           { tok (\p s -> EndP p) }
  else          { tok (\p s -> ElseP p) }
  then          { tok (\p s -> ThenP p) }
  $digit+       { tok (\p s -> IntP p (read s)) }
  [\+]          {tok (\p s -> AddP p )}
  [\-]          {tok (\p s -> SubP p )}
  [\*]          {tok (\p s -> MulP p )}
  [\=]          {tok (\p s -> EqualP p )}
  [\(]          {tok (\p s -> LParenP p )}
  [\)]          {tok (\p s -> RParenP p )}
  [\$]          {tok (\p s -> EofP p )}
  [\;]          {tok (\p s -> SemicolonP p )}
  [\>]          {tok (\p s -> GreaterTP p )}
  [\<]          {tok (\p s -> LessTP p )}
  AND            {tok (\p s -> AndP p )}
  OR             {tok (\p s -> OrP p )}
  XOR            {tok (\p s -> XorP p )}
  EQUALS         {tok (\p s -> EqualsP p )}
  IMPLIES        {tok (\p s -> ImpliesP p )}
  NOT            {tok (\p s -> NotP p )}
  TRUE            {tok (\p s -> ConstP p s )}
  FALSE           {tok (\p s -> ConstP p s )}
  $alpha [$alpha $digit \_ \']*   { tok (\p s -> VarP p s) }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token

-- Some action helpers:
tok f p s = f p s

-- The token type:
data Token =
  LetP AlexPosn |
  InP  AlexPosn |
  IfP  AlexPosn |
  FiP  AlexPosn |
  EndP  AlexPosn |
  ElseP  AlexPosn |
  ThenP  AlexPosn |
  AddP AlexPosn |
  SubP AlexPosn |
  MulP AlexPosn |
  EqualP AlexPosn |
  LParenP AlexPosn |
  RParenP AlexPosn |
  VarP AlexPosn String |
  IntP AlexPosn Int | 
  EofP AlexPosn |
  GreaterTP AlexPosn | 
  LessTP AlexPosn |
  AndP AlexPosn | 
  OrP AlexPosn | 
  XorP AlexPosn | 
  EqualsP AlexPosn | 
  ImpliesP AlexPosn | 
  NotP AlexPosn |
  ConstP AlexPosn String |
  SemicolonP AlexPosn |
  Expr  | LetExpr | IfExpr | SimpleExpr |
  AndExpr | OrExpr  | XorExpr | EqualsExpr | 
  GreaterThanExpr | LessThanExpr | 
  AddExpr | SubtractExpr | MulExpr |
  ParenthesisExpr | NegateExpr | NotExpr |
  EmptyP 
  deriving (Eq,Show)

getLineNumber (AlexPn offset lineNum colNum) = lineNum
getColNumber (AlexPn offset lineNum colNum) = colNum

data Invalid_Input_Exception = Invalid_Input_Exception String  deriving (Eq,Show)
instance Exception Invalid_Input_Exception

myScanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
              AlexEOF -> []
--              AlexError ((AlexPn _ line column),_,_,sstr) -> error $ "Error : Unkown token:" ++ 
--                    (show line) ++ ":" ++ (show column) ++ ":" ++ sstr 
              AlexError ((AlexPn _ line column),_,_,sstr) -> throw (Invalid_Input_Exception ("Unkown token:" ++ 
                    (show line) ++ ":" ++ (show column) ++ ":" ++ (getNextToken sstr))) 
              AlexSkip  inp' len     -> go inp'
              AlexToken inp' len act -> act pos (take len str) : go inp'


getNextToken "" = ""
getNextToken (x:xs) = if x==' ' then "" else x:(getNextToken xs) 

token_posn (LetP p) = p
token_posn (InP p) = p
token_posn (IfP p) = p
token_posn (FiP p) = p
token_posn (EndP p) = p
token_posn (ElseP p) = p
token_posn (ThenP p) = p
token_posn (AddP p) = p
token_posn (SubP p) = p
token_posn (MulP p) = p
token_posn (EqualP p) = p
token_posn (LParenP p) = p
token_posn (RParenP p) = p
token_posn (VarP p _) = p
token_posn (IntP p _) = p
token_posn (EofP p ) = p
token_posn (GreaterTP p ) = p
token_posn (LessTP p ) = p
token_posn (AndP p ) = p
token_posn (OrP p ) = p
token_posn (XorP p ) = p
token_posn (EqualsP p ) = p
token_posn (ImpliesP p ) = p
token_posn (NotP p ) = p
token_posn (ConstP p _) = p
token_posn (SemicolonP p ) = p



data TokenPrint = 
      LET String | IN String | 
      IF String | FI String | 
      END String | ELSE String |
      THEN String | ADD String |
      SUB String | MUL String |
      EQUAL String| LPAREN String |
      RPAREN String | ID String |
      NUM String  | EOF String |
      GREATERTHAN String | LESSTHAN String |
      AND String | OR String |
      XOR String | EQUALS String | 
      IMPLIES String | NOT String | 
      SEMICOLON String |
      CONST String 
      deriving (Eq,Show) 

getSingleTokenOutput (LetP p) = LET "let"
getSingleTokenOutput (InP p) = IN "in"
getSingleTokenOutput (IfP p) = IF "if"
getSingleTokenOutput (FiP p) = FI "fi"
getSingleTokenOutput (EndP p) = END "end"
getSingleTokenOutput (ElseP p) = ELSE "else"
getSingleTokenOutput (ThenP p) = THEN "then"
getSingleTokenOutput (AddP p) = ADD "+"
getSingleTokenOutput (SubP p) = SUB "-"
getSingleTokenOutput (MulP p) = MUL "*"
getSingleTokenOutput (EqualP p) = EQUAL "="
getSingleTokenOutput (LParenP p) = LPAREN "("
getSingleTokenOutput (RParenP p) = RPAREN "("
getSingleTokenOutput (VarP p s) = ID s
getSingleTokenOutput (IntP p n) = NUM (show n)
getSingleTokenOutput (EofP p) = EOF ("$")
getSingleTokenOutput (GreaterTP p) = GREATERTHAN (">")
getSingleTokenOutput (LessTP p) = LESSTHAN "<"
getSingleTokenOutput (AndP p) = AND "AND"
getSingleTokenOutput (OrP p) = OR "OR"
getSingleTokenOutput (XorP p) = XOR "XOR"
getSingleTokenOutput (EqualsP p) = EQUALS "EQUALS"
getSingleTokenOutput (ImpliesP p) = IMPLIES "IMPLIES"
getSingleTokenOutput (NotP p) = NOT "NOT"
getSingleTokenOutput (ConstP p s) = CONST s
getSingleTokenOutput (SemicolonP p) = SEMICOLON ";"

printLexerOutput [] = []
printLexerOutput (x:xs) = (getSingleTokenOutput x):printLexerOutput xs 


getStr (LetP p) = show (getSingleTokenOutput (LetP p))
getStr (InP p) = show (getSingleTokenOutput (InP p))
getStr (IfP p) = show (getSingleTokenOutput (IfP p))
getStr (FiP p) = show (getSingleTokenOutput (FiP p))
getStr (EndP p) = show (getSingleTokenOutput (EndP p))
getStr (ElseP p) = show (getSingleTokenOutput (ElseP p))
getStr (ThenP p) = show (getSingleTokenOutput (ThenP p))
getStr (AddP p) = show (getSingleTokenOutput (AddP p))
getStr (SubP p) = show (getSingleTokenOutput (SubP p))
getStr (MulP p) = show (getSingleTokenOutput (MulP p))
getStr (EqualP p) = show (getSingleTokenOutput (EqualP p))
getStr (LParenP p) = show (getSingleTokenOutput (LParenP p))
getStr (RParenP p) = show (getSingleTokenOutput (RParenP p))
getStr (VarP p s) = show (getSingleTokenOutput (VarP p s))
getStr (IntP p i) = show (getSingleTokenOutput (IntP p i))
getStr (EofP p) = show (getSingleTokenOutput (EofP p))
getStr (GreaterTP p) = show (getSingleTokenOutput (GreaterTP p))
getStr (LessTP p) = show (getSingleTokenOutput (LessTP p))
getStr (AndP p) = show (getSingleTokenOutput (AndP p))
getStr (OrP p) = show (getSingleTokenOutput (OrP p))
getStr (XorP p) = show (getSingleTokenOutput (XorP p))
getStr (EqualsP p) = show (getSingleTokenOutput (EqualsP p))
getStr (ImpliesP p) = show (getSingleTokenOutput (ImpliesP p))
getStr (NotP p) = show (getSingleTokenOutput (NotP p))
getStr (SemicolonP p) = show (getSingleTokenOutput (SemicolonP p))
getStr (ConstP p s) = show (getSingleTokenOutput (ConstP p s))
getStr (Expr) = "Expr"
getStr (LetExpr) = "LetExpr"
getStr (IfExpr) = "IfExpr"
getStr (SimpleExpr) = "SimpleExpr"
getStr (AndExpr) = "AndExpr"
getStr (OrExpr) = "OrExpr"
getStr (XorExpr) = "XorExpr"
getStr (EqualsExpr) = "EqualsExpr"
getStr (GreaterThanExpr) = "GreaterThanExpr"
getStr (LessThanExpr) = "LessThanExpr"
getStr (AddExpr) = "AddExpr"
getStr (SubtractExpr) = "SubtractExpr"
getStr (MulExpr) = "MulExpr"
getStr (ParenthesisExpr) = "ParenthesisExpr"
getStr (NegateExpr) = "NegateExpr"
getStr (NotExpr) = "NotExpr"




isLetToken (LetP p) = True
isLetToken (InP p) = False
isLetToken (IfP p) = False
isLetToken (FiP p) = False
isLetToken (EndP p) = False
isLetToken (ElseP p) = False
isLetToken (ThenP p) = False
isLetToken (AddP p) = False
isLetToken (SubP p) = False
isLetToken (MulP p) = False
isLetToken (EqualP p) = False
isLetToken (LParenP p) = False
isLetToken (RParenP p) = False
isLetToken (VarP p _) = False
isLetToken (IntP p _) = False
isLetToken (EofP p) = False
isLetToken (GreaterTP p) = False
isLetToken (LessTP p) = False
isLetToken (AndP p) = False
isLetToken (OrP p) = False
isLetToken (XorP p) = False
isLetToken (EqualsP p) = False
isLetToken (ImpliesP p) = False
isLetToken (NotP p) = False
isLetToken (SemicolonP p) = False
isLetToken (ConstP p s) = False


isInToken (LetP p) = False
isInToken (InP p) = True
isInToken (IfP p) = False
isInToken (FiP p) = False
isInToken (EndP p) = False
isInToken (ElseP p) = False
isInToken (ThenP p) = False
isInToken (AddP p) = False
isInToken (SubP p) = False
isInToken (MulP p) = False
isInToken (EqualP p) = False
isInToken (LParenP p) = False
isInToken (RParenP p) = False
isInToken (VarP p _) = False
isInToken (IntP p _) = False
isInToken (EofP p) = False
isInToken (GreaterTP p) = False
isInToken (LessTP p) = False
isInToken (AndP p) = False
isInToken (OrP p) = False
isInToken (XorP p) = False
isInToken (EqualsP p) = False
isInToken (ImpliesP p) = False
isInToken (NotP p) = False
isInToken (SemicolonP p) = False
isInToken (ConstP p s) = False

isIfToken (LetP p) = False
isIfToken (InP p) = False
isIfToken (IfP p) = True
isIfToken (FiP p) = False
isIfToken (EndP p) = False
isIfToken (ElseP p) = False
isIfToken (ThenP p) = False
isIfToken (AddP p) = False
isIfToken (SubP p) = False
isIfToken (MulP p) = False
isIfToken (EqualP p) = False
isIfToken (LParenP p) = False
isIfToken (RParenP p) = False
isIfToken (VarP p _) = False
isIfToken (IntP p _) = False
isIfToken (EofP p) = False
isIfToken (GreaterTP p) = False
isIfToken (LessTP p) = False
isIfToken (AndP p) = False
isIfToken (OrP p) = False
isIfToken (XorP p) = False
isIfToken (EqualsP p) = False
isIfToken (ImpliesP p) = False
isIfToken (NotP p) = False
isIfToken (SemicolonP p) = False
isIfToken (ConstP p s) = False

isFiToken (LetP p) = False
isFiToken (InP p) = False
isFiToken (IfP p) = False
isFiToken (FiP p) = True
isFiToken (EndP p) = False
isFiToken (ElseP p) = False
isFiToken (ThenP p) = False
isFiToken (AddP p) = False
isFiToken (SubP p) = False
isFiToken (MulP p) = False
isFiToken (EqualP p) = False
isFiToken (LParenP p) = False
isFiToken (RParenP p) = False
isFiToken (VarP p _) = False
isFiToken (IntP p _) = False
isFiToken (EofP p) = False
isFiToken (GreaterTP p) = False
isFiToken (LessTP p) = False
isFiToken (AndP p) = False
isFiToken (OrP p) = False
isFiToken (XorP p) = False
isFiToken (EqualsP p) = False
isFiToken (ImpliesP p) = False
isFiToken (NotP p) = False
isFiToken (SemicolonP p) = False
isFiToken (ConstP p s) = False

isEndToken (LetP p) = False
isEndToken (InP p) = False
isEndToken (IfP p) = False
isEndToken (FiP p) = False
isEndToken (EndP p) = True
isEndToken (ElseP p) = False
isEndToken (ThenP p) = False
isEndToken (AddP p) = False
isEndToken (SubP p) = False
isEndToken (MulP p) = False
isEndToken (EqualP p) = False
isEndToken (LParenP p) = False
isEndToken (RParenP p) = False
isEndToken (VarP p _) = False
isEndToken (IntP p _) = False
isEndToken (EofP p) = False
isEndToken (GreaterTP p) = False
isEndToken (LessTP p) = False
isEndToken (AndP p) = False
isEndToken (OrP p) = False
isEndToken (XorP p) = False
isEndToken (EqualsP p) = False
isEndToken (ImpliesP p) = False
isEndToken (NotP p) = False
isEndToken (SemicolonP p) = False
isEndToken (ConstP p s) = False

isElseToken (LetP p) = False
isElseToken (InP p) = False
isElseToken (IfP p) = False
isElseToken (FiP p) = False
isElseToken (EndP p) = False
isElseToken (ElseP p) = True
isElseToken (ThenP p) = False
isElseToken (AddP p) = False
isElseToken (SubP p) = False
isElseToken (MulP p) = False
isElseToken (EqualP p) = False
isElseToken (LParenP p) = False
isElseToken (RParenP p) = False
isElseToken (VarP p _) = False
isElseToken (IntP p _) = False
isElseToken (EofP p) = False
isElseToken (GreaterTP p) = False
isElseToken (LessTP p) = False
isElseToken (AndP p) = False
isElseToken (OrP p) = False
isElseToken (XorP p) = False
isElseToken (EqualsP p) = False
isElseToken (ImpliesP p) = False
isElseToken (NotP p) = False
isElseToken (SemicolonP p) = False
isElseToken (ConstP p s) = False

isThenToken (LetP p) = False
isThenToken (InP p) = False
isThenToken (IfP p) = False
isThenToken (FiP p) = False
isThenToken (EndP p) = False
isThenToken (ElseP p) = False
isThenToken (ThenP p) = True
isThenToken (AddP p) = False
isThenToken (SubP p) = False
isThenToken (MulP p) = False
isThenToken (EqualP p) = False
isThenToken (LParenP p) = False
isThenToken (RParenP p) = False
isThenToken (VarP p _) = False
isThenToken (IntP p _) = False
isThenToken (EofP p) = False
isThenToken (GreaterTP p) = False
isThenToken (LessTP p) = False
isThenToken (AndP p) = False
isThenToken (OrP p) = False
isThenToken (XorP p) = False
isThenToken (EqualsP p) = False
isThenToken (ImpliesP p) = False
isThenToken (NotP p) = False
isThenToken (SemicolonP p) = False
isThenToken (ConstP p s) = False


isAddToken (LetP p) = False
isAddToken (InP p) = False
isAddToken (IfP p) = False
isAddToken (FiP p) = False
isAddToken (EndP p) = False
isAddToken (ElseP p) = False
isAddToken (ThenP p) = False
isAddToken (AddP p) = True
isAddToken (SubP p) = False
isAddToken (MulP p) = False
isAddToken (EqualP p) = False
isAddToken (LParenP p) = False
isAddToken (RParenP p) = False
isAddToken (VarP p _) = False
isAddToken (IntP p _) = False
isAddToken (EofP p) = False
isAddToken (GreaterTP p) = False
isAddToken (LessTP p) = False
isAddToken (AndP p) = False
isAddToken (OrP p) = False
isAddToken (XorP p) = False
isAddToken (EqualsP p) = False
isAddToken (ImpliesP p) = False
isAddToken (NotP p) = False
isAddToken (SemicolonP p) = False
isAddToken (ConstP p s) = False


isSubToken (LetP p) = False
isSubToken (InP p) = False
isSubToken (IfP p) = False
isSubToken (FiP p) = False
isSubToken (EndP p) = False
isSubToken (ElseP p) = False
isSubToken (ThenP p) = False
isSubToken (AddP p) = False
isSubToken (SubP p) = True
isSubToken (MulP p) = False
isSubToken (EqualP p) = False
isSubToken (LParenP p) = False
isSubToken (RParenP p) = False
isSubToken (VarP p _) = False
isSubToken (IntP p _) = False
isSubToken (EofP p) = False
isSubToken (GreaterTP p) = False
isSubToken (LessTP p) = False
isSubToken (AndP p) = False
isSubToken (OrP p) = False
isSubToken (XorP p) = False
isSubToken (EqualsP p) = False
isSubToken (ImpliesP p) = False
isSubToken (NotP p) = False
isSubToken (SemicolonP p) = False
isSubToken (ConstP p s) = False

isMulToken (LetP p) = False
isMulToken (InP p) = False
isMulToken (IfP p) = False
isMulToken (FiP p) = False
isMulToken (EndP p) = False
isMulToken (ElseP p) = False
isMulToken (ThenP p) = False
isMulToken (AddP p) = False
isMulToken (SubP p) = False
isMulToken (MulP p) = True
isMulToken (EqualP p) = False
isMulToken (LParenP p) = False
isMulToken (RParenP p) = False
isMulToken (VarP p _) = False
isMulToken (IntP p _) = False
isMulToken (EofP p) = False
isMulToken (GreaterTP p) = False
isMulToken (LessTP p) = False
isMulToken (AndP p) = False
isMulToken (OrP p) = False
isMulToken (XorP p) = False
isMulToken (EqualsP p) = False
isMulToken (ImpliesP p) = False
isMulToken (NotP p) = False
isMulToken (SemicolonP p) = False
isMulToken (ConstP p s) = False

isEqualToken (LetP p) = False
isEqualToken (InP p) = False
isEqualToken (IfP p) = False
isEqualToken (FiP p) = False
isEqualToken (EndP p) = False
isEqualToken (ElseP p) = False
isEqualToken (ThenP p) = False
isEqualToken (AddP p) = False
isEqualToken (SubP p) = False
isEqualToken (MulP p) = False
isEqualToken (EqualP p) = True
isEqualToken (LParenP p) = False
isEqualToken (RParenP p) = False
isEqualToken (VarP p _) = False
isEqualToken (IntP p _) = False
isEqualToken (EofP p) = False
isEqualToken (GreaterTP p) = False
isEqualToken (LessTP p) = False
isEqualToken (AndP p) = False
isEqualToken (OrP p) = False
isEqualToken (XorP p) = False
isEqualToken (EqualsP p) = False
isEqualToken (ImpliesP p) = False
isEqualToken (NotP p) = False
isEqualToken (SemicolonP p) = False
isEqualToken (ConstP p s) = False

isLParenToken (LetP p) = False
isLParenToken (InP p) = False
isLParenToken (IfP p) = False
isLParenToken (FiP p) = False
isLParenToken (EndP p) = False
isLParenToken (ElseP p) = False
isLParenToken (ThenP p) = False
isLParenToken (AddP p) = False
isLParenToken (SubP p) = False
isLParenToken (MulP p) = False
isLParenToken (EqualP p) = False
isLParenToken (LParenP p) = True
isLParenToken (RParenP p) = False
isLParenToken (VarP p _) = False
isLParenToken (IntP p _) = False
isLParenToken (EofP p) = False
isLParenToken (GreaterTP p) = False
isLParenToken (LessTP p) = False
isLParenToken (AndP p) = False
isLParenToken (OrP p) = False
isLParenToken (XorP p) = False
isLParenToken (EqualsP p) = False
isLParenToken (ImpliesP p) = False
isLParenToken (NotP p) = False
isLParenToken (SemicolonP p) = False
isLParenToken (ConstP p s) = False

isRParenToken (LetP p) = False
isRParenToken (InP p) = False
isRParenToken (IfP p) = False
isRParenToken (FiP p) = False
isRParenToken (EndP p) = False
isRParenToken (ElseP p) = False
isRParenToken (ThenP p) = False
isRParenToken (AddP p) = False
isRParenToken (SubP p) = False
isRParenToken (MulP p) = False
isRParenToken (EqualP p) = False
isRParenToken (LParenP p) = False
isRParenToken (RParenP p) = True
isRParenToken (VarP p _) = False
isRParenToken (IntP p _) = False
isRParenToken (EofP p) = False
isRParenToken (GreaterTP p) = False
isRParenToken (LessTP p) = False
isRParenToken (AndP p) = False
isRParenToken (OrP p) = False
isRParenToken (XorP p) = False
isRParenToken (EqualsP p) = False
isRParenToken (ImpliesP p) = False
isRParenToken (NotP p) = False
isRParenToken (SemicolonP p) = False
isRParenToken (ConstP p s) = False

isVarToken (LetP p) = False
isVarToken (InP p) = False
isVarToken (IfP p) = False
isVarToken (FiP p) = False
isVarToken (EndP p) = False
isVarToken (ElseP p) = False
isVarToken (ThenP p) = False
isVarToken (AddP p) = False
isVarToken (SubP p) = False
isVarToken (MulP p) = False
isVarToken (EqualP p) = False
isVarToken (LParenP p) = False
isVarToken (RParenP p) = False
isVarToken (VarP p _) = True
isVarToken (IntP p _) = False
isVarToken (EofP p) = False
isVarToken (GreaterTP p) = False
isVarToken (LessTP p) = False
isVarToken (AndP p) = False
isVarToken (OrP p) = False
isVarToken (XorP p) = False
isVarToken (EqualsP p) = False
isVarToken (ImpliesP p) = False
isVarToken (NotP p) = False
isVarToken (SemicolonP p) = False
isVarToken (ConstP p s) = False

isIntToken (LetP p) = False
isIntToken (InP p) = False
isIntToken (IfP p) = False
isIntToken (FiP p) = False
isIntToken (EndP p) = False
isIntToken (ElseP p) = False
isIntToken (ThenP p) = False
isIntToken (AddP p) = False
isIntToken (SubP p) = False
isIntToken (MulP p) = False
isIntToken (EqualP p) = False
isIntToken (LParenP p) = False
isIntToken (RParenP p) = False
isIntToken (VarP p _) = False
isIntToken (IntP p _) = True
isIntToken (EofP p) = False
isIntToken (GreaterTP p) = False
isIntToken (LessTP p) = False
isIntToken (AndP p) = False
isIntToken (OrP p) = False
isIntToken (XorP p) = False
isIntToken (EqualsP p) = False
isIntToken (ImpliesP p) = False
isIntToken (NotP p) = False
isIntToken (SemicolonP p) = False
isIntToken (ConstP p s) = False

isEofToken (LetP p) = False
isEofToken (InP p) = False
isEofToken (IfP p) = False
isEofToken (FiP p) = False
isEofToken (EndP p) = False
isEofToken (ElseP p) = False
isEofToken (ThenP p) = False
isEofToken (AddP p) = False
isEofToken (SubP p) = False
isEofToken (MulP p) = False
isEofToken (EqualP p) = False
isEofToken (LParenP p) = False
isEofToken (RParenP p) = False
isEofToken (VarP p _) = False
isEofToken (IntP p _) = False
isEofToken (EofP p) = True
isEofToken (GreaterTP p) = False
isEofToken (LessTP p) = False
isEofToken (AndP p) = False
isEofToken (OrP p) = False
isEofToken (XorP p) = False
isEofToken (EqualsP p) = False
isEofToken (ImpliesP p) = False
isEofToken (NotP p) = False
isEofToken (SemicolonP p) = False
isEofToken (ConstP p s) = False

isGTToken (LetP p) = False
isGTToken (InP p) = False
isGTToken (IfP p) = False
isGTToken (FiP p) = False
isGTToken (EndP p) = False
isGTToken (ElseP p) = False
isGTToken (ThenP p) = False
isGTToken (AddP p) = False
isGTToken (SubP p) = False
isGTToken (MulP p) = False
isGTToken (EqualP p) = False
isGTToken (LParenP p) = False
isGTToken (RParenP p) = False
isGTToken (VarP p _) = False
isGTToken (IntP p _) = False
isGTToken (EofP p) = False
isGTToken (GreaterTP p) = True
isGTToken (LessTP p) = False
isGTToken (AndP p) = False
isGTToken (OrP p) = False
isGTToken (XorP p) = False
isGTToken (EqualsP p) = False
isGTToken (ImpliesP p) = False
isGTToken (NotP p) = False
isGTToken (SemicolonP p) = False
isGTToken (ConstP p s) = False

isLTToken (LetP p) = False
isLTToken (InP p) = False
isLTToken (IfP p) = False
isLTToken (FiP p) = False
isLTToken (EndP p) = False
isLTToken (ElseP p) = False
isLTToken (ThenP p) = False
isLTToken (AddP p) = False
isLTToken (SubP p) = False
isLTToken (MulP p) = False
isLTToken (EqualP p) = False
isLTToken (LParenP p) = False
isLTToken (RParenP p) = False
isLTToken (VarP p _) = False
isLTToken (IntP p _) = False
isLTToken (EofP p) = False
isLTToken (GreaterTP p) = False
isLTToken (LessTP p) = True
isLTToken (AndP p) = False
isLTToken (OrP p) = False
isLTToken (XorP p) = False
isLTToken (EqualsP p) = False
isLTToken (ImpliesP p) = False
isLTToken (NotP p) = False
isLTToken (SemicolonP p) = False
isLTToken (ConstP p s) = False

isAndToken (LetP p) = False
isAndToken (InP p) = False
isAndToken (IfP p) = False
isAndToken (FiP p) = False
isAndToken (EndP p) = False
isAndToken (ElseP p) = False
isAndToken (ThenP p) = False
isAndToken (AddP p) = False
isAndToken (SubP p) = False
isAndToken (MulP p) = False
isAndToken (EqualP p) = False
isAndToken (LParenP p) = False
isAndToken (RParenP p) = False
isAndToken (VarP p _) = False
isAndToken (IntP p _) = False
isAndToken (EofP p) = False
isAndToken (GreaterTP p) = False
isAndToken (LessTP p) = False
isAndToken (AndP p) = True
isAndToken (OrP p) = False
isAndToken (XorP p) = False
isAndToken (EqualsP p) = False
isAndToken (ImpliesP p) = False
isAndToken (NotP p) = False
isAndToken (SemicolonP p) = False
isAndToken (ConstP p s) = False


isOrToken (LetP p) = False
isOrToken (InP p) = False
isOrToken (IfP p) = False
isOrToken (FiP p) = False
isOrToken (EndP p) = False
isOrToken (ElseP p) = False
isOrToken (ThenP p) = False
isOrToken (AddP p) = False
isOrToken (SubP p) = False
isOrToken (MulP p) = False
isOrToken (EqualP p) = False
isOrToken (LParenP p) = False
isOrToken (RParenP p) = False
isOrToken (VarP p _) = False
isOrToken (IntP p _) = False
isOrToken (EofP p) = False
isOrToken (GreaterTP p) = False
isOrToken (LessTP p) = False
isOrToken (AndP p) = False
isOrToken (OrP p) = True
isOrToken (XorP p) = False
isOrToken (EqualsP p) = False
isOrToken (ImpliesP p) = False
isOrToken (NotP p) = False
isOrToken (SemicolonP p) = False
isOrToken (ConstP p s) = False

isXorToken (LetP p) = False
isXorToken (InP p) = False
isXorToken (IfP p) = False
isXorToken (FiP p) = False
isXorToken (EndP p) = False
isXorToken (ElseP p) = False
isXorToken (ThenP p) = False
isXorToken (AddP p) = False
isXorToken (SubP p) = False
isXorToken (MulP p) = False
isXorToken (EqualP p) = False
isXorToken (LParenP p) = False
isXorToken (RParenP p) = False
isXorToken (VarP p _) = False
isXorToken (IntP p _) = False
isXorToken (EofP p) = False
isXorToken (GreaterTP p) = False
isXorToken (LessTP p) = False
isXorToken (AndP p) = False
isXorToken (OrP p) = False
isXorToken (XorP p) = True
isXorToken (EqualsP p) = False
isXorToken (ImpliesP p) = False
isXorToken (NotP p) = False
isXorToken (SemicolonP p) = False
isXorToken (ConstP p s) = False

isEqualsToken (LetP p) = False
isEqualsToken (InP p) = False
isEqualsToken (IfP p) = False
isEqualsToken (FiP p) = False
isEqualsToken (EndP p) = False
isEqualsToken (ElseP p) = False
isEqualsToken (ThenP p) = False
isEqualsToken (AddP p) = False
isEqualsToken (SubP p) = False
isEqualsToken (MulP p) = False
isEqualsToken (EqualP p) = False
isEqualsToken (LParenP p) = False
isEqualsToken (RParenP p) = False
isEqualsToken (VarP p _) = False
isEqualsToken (IntP p _) = False
isEqualsToken (EofP p) = False
isEqualsToken (GreaterTP p) = False
isEqualsToken (LessTP p) = False
isEqualsToken (AndP p) = False
isEqualsToken (OrP p) = False
isEqualsToken (XorP p) = False
isEqualsToken (EqualsP p) = True
isEqualsToken (ImpliesP p) = False
isEqualsToken (NotP p) = False
isEqualsToken (SemicolonP p) = False
isEqualsToken (ConstP p s) = False

isImpliesToken (LetP p) = False
isImpliesToken (InP p) = False
isImpliesToken (IfP p) = False
isImpliesToken (FiP p) = False
isImpliesToken (EndP p) = False
isImpliesToken (ElseP p) = False
isImpliesToken (ThenP p) = False
isImpliesToken (AddP p) = False
isImpliesToken (SubP p) = False
isImpliesToken (MulP p) = False
isImpliesToken (EqualP p) = False
isImpliesToken (LParenP p) = False
isImpliesToken (RParenP p) = False
isImpliesToken (VarP p _) = False
isImpliesToken (IntP p _) = False
isImpliesToken (EofP p) = False
isImpliesToken (GreaterTP p) = False
isImpliesToken (LessTP p) = False
isImpliesToken (AndP p) = False
isImpliesToken (OrP p) = False
isImpliesToken (XorP p) = False
isImpliesToken (EqualsP p) = False
isImpliesToken (ImpliesP p) = True
isImpliesToken (NotP p) = False
isImpliesToken (SemicolonP p) = False
isImpliesToken (ConstP p s) = False

isNotToken (LetP p) = False
isNotToken (InP p) = False
isNotToken (IfP p) = False
isNotToken (FiP p) = False
isNotToken (EndP p) = False
isNotToken (ElseP p) = False
isNotToken (ThenP p) = False
isNotToken (AddP p) = False
isNotToken (SubP p) = False
isNotToken (MulP p) = False
isNotToken (EqualP p) = False
isNotToken (LParenP p) = False
isNotToken (RParenP p) = False
isNotToken (VarP p _) = False
isNotToken (IntP p _) = False
isNotToken (EofP p) = False
isNotToken (GreaterTP p) = False
isNotToken (LessTP p) = False
isNotToken (AndP p) = False
isNotToken (OrP p) = False
isNotToken (XorP p) = False
isNotToken (EqualsP p) = False
isNotToken (ImpliesP p) = False
isNotToken (NotP p) = True
isNotToken (SemicolonP p) = False
isNotToken (ConstP p s) = False

isSemicolonToken (LetP p) = False
isSemicolonToken (InP p) = False
isSemicolonToken (IfP p) = False
isSemicolonToken (FiP p) = False
isSemicolonToken (EndP p) = False
isSemicolonToken (ElseP p) = False
isSemicolonToken (ThenP p) = False
isSemicolonToken (AddP p) = False
isSemicolonToken (SubP p) = False
isSemicolonToken (MulP p) = False
isSemicolonToken (EqualP p) = False
isSemicolonToken (LParenP p) = False
isSemicolonToken (RParenP p) = False
isSemicolonToken (VarP p _) = False
isSemicolonToken (IntP p _) = False
isSemicolonToken (EofP p) = False
isSemicolonToken (GreaterTP p) = False
isSemicolonToken (LessTP p) = False
isSemicolonToken (AndP p) = False
isSemicolonToken (OrP p) = False
isSemicolonToken (XorP p) = False
isSemicolonToken (EqualsP p) = False
isSemicolonToken (ImpliesP p) = False
isSemicolonToken (NotP p) = False
isSemicolonToken (SemicolonP p) = True
isSemicolonToken (ConstP p s) = False

isBoolTFToken (LetP p) = False
isBoolTFToken (InP p) = False
isBoolTFToken (IfP p) = False
isBoolTFToken (FiP p) = False
isBoolTFToken (EndP p) = False
isBoolTFToken (ElseP p) = False
isBoolTFToken (ThenP p) = False
isBoolTFToken (AddP p) = False
isBoolTFToken (SubP p) = False
isBoolTFToken (MulP p) = False
isBoolTFToken (EqualP p) = False
isBoolTFToken (LParenP p) = False
isBoolTFToken (RParenP p) = False
isBoolTFToken (VarP p _) = False
isBoolTFToken (IntP p _) = False
isBoolTFToken (EofP p) = False
isBoolTFToken (GreaterTP p) = False
isBoolTFToken (LessTP p) = False
isBoolTFToken (AndP p) = False
isBoolTFToken (OrP p) = False
isBoolTFToken (XorP p) = False
isBoolTFToken (EqualsP p) = False
isBoolTFToken (ImpliesP p) = False
isBoolTFToken (NotP p) = False
isBoolTFToken (SemicolonP p) = False
isBoolTFToken (ConstP p s) = True

}