import Control.Exception
import System.IO
import System.Environment   
import Data.List  
import Lexer 


data Parser_Exception = Parser_Exception String  deriving (Eq,Show)
instance Exception Parser_Exception

main = do
  args <- getArgs
  contents <- readFile (head args) 
  putStr (runParser contents)

runParser s = "Input :\n" ++ s ++ "\n" ++ lexerStr ++ "\n\n" ++ parserStr ++ "\n\n"
        where tokenList = myScanTokens s 
             -- lexerStr = "Lexer Output \n" ++ 
             --               lexerOutputStr (lexerOutput tokenList)
              lexerStr = "Lexer Output : \n" ++ show (printLexerOutput tokenList)
              tree = parseS tokenList
              parserStr = "Parser Output : \n" ++  preOrder tree 

lexerOutputStr [] = ""
lexerOutputStr (x:[]) = x   
lexerOutputStr (x:xs) = x ++  ", " ++ lexerOutputStr xs

lexerOutput [] = []
lexerOutput (x:xs) = (getStr x):lexerOutput xs 

data AST a = EmptyTree | Node a [AST a] deriving (Eq,Show)

preOrder EmptyTree = ""
preOrder (Node a []) = (getStr a) 
preOrder (Node a l) = (getStr a) ++ "(" ++ childrenStr ++ ")"
                      where childrenStr = preOrderList l

preOrderList [] = ""
preOrderList (EmptyTree:l) = preOrderList l
preOrderList (x:[]) = preOrder x 
preOrderList (x:xs) = (preOrder x) ++ "," ++ preOrderList xs
                      

generateErrorText [] s = "Syntax Error : No Token Found. " ++ s
generateErrorText l  s = "Syntax Error :" ++ (show (getLineNumber (token_posn token))) 
                              ++ ":" ++ (show (getColNumber (token_posn token))) ++ ":"
                              ++ s 
                          where token = head l
--generateErrorText (x:xs) s = 


-- S : Expr EOF
parseS l = if (length leftTokens == 1) 
           then if (isEofToken (head leftTokens))
                then tree 
                else throw (Parser_Exception (generateErrorText leftTokens "Expecting EOF token. S -> E EOF."))
           else throw (Parser_Exception (generateErrorText leftTokens "Expecting EOF token. S -> E EOF."))
           where (tree , leftTokens) = parseExpr l

--  Expr : LetExpr
--       | IfExpr 
--       | SimpleExpr
parseExpr [] = throw (Parser_Exception (generateErrorText [] (
                " Expr -> LetExpr | IfExpr | SimpleExpr.")))
parseExpr  (hToken : l)
    | isLetToken hToken = parseLetExpr (hToken:l) 
    | isIfToken hToken = parseIfExpr (hToken:l)
    | otherwise =  parseSimpleExpr (hToken:l)

-- LetExpr : "let" Var "=" Expr "in" Expr "end"
parseLetExpr (hToken:l) 
    = (resultTree , l6)
      where letTree = Node hToken []
            (varTree,l1) = parseVariableToken l 
            (equalTree,l2) = parseEqualToken l1
            (expr1Tree, l3) = parseExpr l2 
            (inTree, l4) = parseInToken l3 
            (expr2Tree, l5) = parseExpr l4
            (endTree, l6) = parseEndToken l5
            resultTree = (Node LetExpr [letTree,varTree,equalTree,expr1Tree,inTree,expr2Tree,endTree])

-- IfExpr : "if"  Expr "then" Expr "else" Expr
parseIfExpr (hToken:l)
      =  (resultTree , l6)
         where ifTree = Node hToken []
               (expr1Tree,l1) = parseExpr l
               (thenTree ,l2) = parseThenToken l1 
               (expr2Tree,l3) = parseExpr l2
               (elseTree, l4) = parseElseToken l3 
               (expr3Tree,l5) = parseExpr l4
               (fiTree, l6) = parseFiToken l5
               resultTree = Node IfExpr [ifTree,expr1Tree,thenTree,
                                expr2Tree, elseTree, expr3Tree, fiTree]

-- SimpleExpr : CExpr "implies" SimpleExpr 
--        |  CExpr 
parseSimpleExpr l 
        | lenL1 == 0 = ((Node SimpleExpr [ctree]),l1)
        | isImpliesToken hl1 = 
                          let (bztree,l2) = parseSimpleExpr (tail l1)
                          in ((Node SimpleExpr [ctree,(Node (head l1) []),bztree])
                                , l2)
        | otherwise = ((Node SimpleExpr [ctree]),l1) 
                where (ctree,l1) = parseCExpr l 
                      lenL1 = length l1
                      hl1 = head l1 


--CExpr : DExpr AOXEExpr 
    
parseCExpr l  
      | aoxetree == EmptyTree = (dtree,l1)
      | otherwise  = ((Node a (dtree:treeList)),l2)
               where (dtree,l1) = parseDExpr l
                     (aoxetree,l2) = parseAOXEExpr l1
                     (Node a treeList) = aoxetree

-- AOXEExpr : "AND" DExpr AOXEExpr
--       | "OR" DExpr AOXEExpr
--       | "XOR" DExpr AOXEExpr
--       | "EQUALS" DExpr AOXEExpr
--       | empty 

parseAOXEExpr l 
    | length l == 0 = (EmptyTree , l)
    | isAndToken h = ((Node AndExpr [resultTree]),l2)
    | isOrToken h = ((Node OrExpr [resultTree]),l2)
    | isXorToken h = ((Node XorExpr [resultTree]),l2)
    | isEqualsToken h = ((Node EqualsExpr [resultTree]),l2)
    | otherwise = (EmptyTree, l)
    where h = head l 
          (t1,l1) = parseDExpr (tail l) 
          (t2,l2) = parseAOXEExpr l2
          resultTree = getTreeAOXEExpr t1 t2

getTreeAOXEExpr dtree aoxetree 
  | aoxetree == EmptyTree = dtree
  | otherwise = Node a (dtree:l)
     where (Node a l) = aoxetree

--DExpr : FExpr LGExpr
parseDExpr l  
    | lgtree == EmptyTree = (ftree,l1)
    | otherwise = ((Node a (ftree:treeList)),l2)
        where (ftree,l1) = parseFExpr l
              (lgtree,l2) = parseLGExpr l1
              (Node a treeList) = lgtree

-- LGExpr : ">" FExpr LGExpr
--        | "<" FExpr LGExpr
--        | empty
parseLGExpr l 
    | length l == 0 = (EmptyTree , l)
    | isGTToken h = ((Node GreaterThanExpr [resultTree]),l2)
    | isLTToken h = ((Node LessThanExpr [resultTree]),l2)
    | otherwise = (EmptyTree, l)
    where h = head l 
          (ftree,l1) = parseFExpr (tail l)
          (lgtree,l2) = parseLGExpr l1
          resultTree = getTreeLGExpr ftree lgtree

getTreeLGExpr ftree lgtree 
  | lgtree == EmptyTree = ftree
  | otherwise = Node a (ftree:l)
     where (Node a l) = lgtree


-- FExpr : HExpr PMExpr
parseFExpr l  
    | pmtree == EmptyTree = (htree,l1)
    | otherwise = ((Node a (htree:treeList)),l2)
        where (htree,l1) = parseHExpr l
              (pmtree,l2) = parsePMExpr l1
              (Node a treeList) = pmtree


-- PMExpr : "+" HExpr PMExpr
--        | "-" HExpr PMExpr
--        | empty
parsePMExpr l 
    | length l == 0 = (EmptyTree , l)
    | isAddToken h = ((Node AddExpr [resultTree]),l2)
    | isSubToken h = ((Node SubtractExpr [resultTree]),l2)
    | otherwise = (EmptyTree, l)
    where h = head l 
          (htree,l1) = parseHExpr (tail l)
          (pmtree,l2) = parsePMExpr (l1)
          resultTree = getTreePMExpr htree pmtree

getTreePMExpr htree pmtree 
  | pmtree == EmptyTree = htree
  | otherwise = Node a (htree:l)
     where (Node a l) = pmtree


-- HExpr : IExpr MulExpr 
parseHExpr l  
    | multree == EmptyTree = (itree,l1)
    | otherwise = ((Node a (itree:treeList)),l2)
        where (itree,l1) = parseIExpr l
              (multree,l2) = parseMulExpr l1
              (Node a treeList) = multree


-- MulExpr : "*" IExpr MulExpr
--         | empty
parseMulExpr l 
    | length l == 0 = (EmptyTree , l)
    | isMulToken h = ((Node MulExpr [resultTree]),l2)
    | otherwise = (EmptyTree, l)
    where h = head l 
          (itree,l1) = parseIExpr (tail l)
          (multree,l2) = parseMulExpr (l1)
          resultTree = getTreeMulExpr itree multree

getTreeMulExpr itree multree 
  | multree == EmptyTree = itree
  | otherwise = Node a (itree:l)
     where (Node a l) = multree

-- IExpr : "(" Expr ")"
--    | "NOT" Expr 
--    | Variable
--    | Number 
--    | Boolean 
parseIExpr l 
    | length l == 0 = throw (Parser_Exception (generateErrorText l " IExpr -> (Expr) | NOT Expr | ID | Num | CONST "))
    | isLParenToken h = parseParenthesisExpr l 
    | isSubToken  h = parseNegateExpr l 
    | isNotToken  h = parseNotExpr l
    | isVarToken h =  ((Node h []),(tail l))
    | isIntToken h = ((Node h []),(tail l))
    | isBoolTFToken h = ((Node h []),(tail l)) 
    | isBoolTFToken h = ((Node h []),(tail l))
        where h = head l 

parseParenthesisExpr l 
    | length l1 == 0 = throw (Parser_Exception (generateErrorText l1 "No token found after LPAREN. IExpr -> ( Expr ) "))
    | length l2 == 0 = throw (Parser_Exception (generateErrorText l2 "Expecting RPAREN token. IExpr -> ( Expr ) "))
    | isRParenToken tokent = ((Node ParenthesisExpr [tLParen,texpr,tRParen]),(tail l2))
    | otherwise = throw (Parser_Exception (generateErrorText l2 "Expecting RPAREN token. IExpr -> ( Expr ) "))
        where l1 = tail l 
              (texpr, l2) = parseExpr l1 
              tokent = head l2 
              tLParen = Node (head l) []
              tRParen = Node tokent []

parseNegateExpr l = ((Node NegateExpr [tNegate, texpr]),l2)
                    where l1 = tail l 
                          (texpr, l2) = parseIExpr l1 
                          tNegate = Node (head l) []

parseNotExpr l = ((Node NotExpr [tNot, texpr]),l2)
                    where l1 = tail l 
                          (texpr, l2) = parseIExpr l1 
                          tNot = Node (head l) []
 
parseVariableToken [] = throw (Parser_Exception (generateErrorText [] "Expecting ID token. Variable -> ID "))
parseVariableToken (hToken:l) = 
            if isVarToken hToken 
            then ((Node hToken []),l)
            else throw (Parser_Exception (generateErrorText (hToken:l) "Expecting ID token. Variable -> ID "))

parseEqualToken [] = throw (Parser_Exception (generateErrorText [] "Expecting EQUAL token. LetExpr -> LET ID EQUAL Expr IN Expr END"))
parseEqualToken (hToken:l) = 
            if isEqualToken hToken
            then ((Node hToken []),l)
            else throw (Parser_Exception (generateErrorText (hToken:l) "Expecting EQUAL token. LetExpr -> LET ID EQUAL Expr IN Expr END"))

parseInToken [] = throw (Parser_Exception (generateErrorText [] "Expecting IN token. LetExpr -> LET ID EQUAL Expr IN Expr END"))
parseInToken (hToken:l) = 
            if isInToken hToken
            then ((Node hToken []),l)
            else throw (Parser_Exception (generateErrorText (hToken:l) "Expecting IN token. LetExpr -> LET ID EQUAL Expr IN Expr END"))

parseEndToken [] = throw (Parser_Exception (generateErrorText [] "Expecting END token. LetExpr -> LET ID EQUAL Expr IN Expr END"))
parseEndToken (hToken:l) = 
            if isEndToken hToken 
            then ((Node hToken []),l)
            else throw (Parser_Exception (generateErrorText (hToken:l) "Expecting END token. LetExpr -> LET ID EQUAL Expr IN Expr END"))


parseThenToken [] = throw (Parser_Exception (generateErrorText [] "Expecting THEN token. IfExpr -> IF  Expr THEN Expr ELSE Expr FI"))
parseThenToken (hToken:l) = 
                if isThenToken hToken 
                then ((Node hToken []),l)
                else throw (Parser_Exception (generateErrorText (hToken:l) "Expecting THEN token. IfExpr -> IF  Expr THEN Expr ELSE Expr FI"))

parseElseToken [] = throw (Parser_Exception (generateErrorText [] "Expecting ELSE token. IfExpr -> IF  Expr THEN Expr ELSE Expr FI"))
parseElseToken (hToken:l) = 
                if isElseToken hToken 
                then ((Node hToken []),l)
                else throw (Parser_Exception (generateErrorText (hToken:l) "Expecting ELSE token. IfExpr -> IF  Expr THEN Expr ELSE Expr FI"))

parseFiToken [] = throw (Parser_Exception (generateErrorText [] "Expecting FI token. IfExpr -> IF  Expr THEN Expr ELSE Expr FI"))
parseFiToken (hToken:l) = 
                if isFiToken hToken 
                then ((Node hToken []),l)
                else throw (Parser_Exception (generateErrorText (hToken:l) "Expecting FI token. IfExpr -> IF  Expr THEN Expr ELSE Expr FI"))


