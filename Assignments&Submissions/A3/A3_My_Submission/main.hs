import Ast
import Evaluator
import Lexer
import Parser
import Typing
import Control.Exception
import System.IO
import System.Environment   
import Data.List  

main = do
  args <- getArgs
  contents <- readFile (head args) 
  putStr (runParser contents)

runParser s = "Input :\n" ++ s ++ "\n" ++ lexerStr ++ "\n\n" ++ parserStr ++ "\n\n" ++ typeStr ++ "\n\n" ++ evalStr
        where tokenList = alexScanTokens s 
              lexerStr = "Lexer Output : \n" ++ show (tokenList)
              tree = parser tokenList
              parserStr = "Parser Output : \n" ++  (preOrder tree)
              typeExpr = getType tree []  
              evalOutput = eval tree [] 
              typeStr = "Type : " ++ (show typeExpr)
              evalStr = "Evaluation : " ++ (show evalOutput) ++ "\n"



preOrder (BinExpr op b c) = " BinExpr( " ++ (show op) ++ "(" ++ (preOrder b ) ++ "),(" ++ (preOrder c ) ++ ") ) "                                  
preOrder (UnExpr  op a) = " UnExpr( " ++ (show op) ++  "(" ++ (preOrder a ) ++ ") ) "
preOrder (Ite c a b) = " Ite( " ++  "(" ++ (preOrder c) ++ "), (" ++  (preOrder a) ++ "), (" ++ (preOrder b) ++ ") ) "
preOrder (Let decl a) = " Let " ++ (show id) ++ "= (" ++ (preOrder idexpr) ++ ") in (" ++ (preOrder a) ++ ") "
                        where (Decl id idexpr) = decl 
preOrder (BoolConst b) = " BoolConst '" ++ show b ++ "' "
preOrder (IntConst a) = " IntConst '" ++ show a ++ "' "
preOrder (VarExpr v) = " VarExpr '" ++ show v ++ "' "
preOrder (AppExpr a b) = " AppExpr (" ++ (preOrder a) ++ (preOrder b) ++ ")"
preOrder (Fn a atype b) = "Fn (" ++ (preOrder a) ++ " :: " ++ (show atype) ++ ") => " ++ (preOrder b) ++ " end "
preOrder (Fun a b atype btype c) = "Fun " ++ (preOrder a) ++ " (" ++ (preOrder b) ++ " :: " ++ (show atype) ++ 
                                    ") :: " ++ (show btype) ++ " => " ++ (preOrder c) ++ " end "
