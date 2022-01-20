a2 : lexer.x parser.y Evaluator.hs Ast.hs Typing.hs main.hs
	alex lexer.x 
	happy parser.y
	ghc main.hs -o a3
	@rm Lexer.o
	@rm Lexer.hi 
	@rm parser.hi
	@rm parser.o 
	@rm Lexer.hs
	@rm Evaluator.hi 
	@rm Evaluator.o
	@rm Typing.hi 
	@rm Typing.o 
	@rm main.o 
	@rm main.hi
	@rm Ast.hi
	@rm Ast.o
	@rm parser.hs
	@echo " "
	@echo " "
	@echo "Usage ::  ./a3 <text file> "
	@echo " "


clean : 
	@rm a3
