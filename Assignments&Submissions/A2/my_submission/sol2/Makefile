a2 : lexer.x parser.hs
	alex lexer.x 
	ghc parser.hs -o a2 
	@rm Lexer.o
	@rm Lexer.hi 
	@rm parser.hi
	@rm parser.o 
	@rm Lexer.hs 
	@echo " "
	@echo " "
	@echo "Usage ::  ./a2 <text file> "
	@echo " "


clean : 
	@rm a2
