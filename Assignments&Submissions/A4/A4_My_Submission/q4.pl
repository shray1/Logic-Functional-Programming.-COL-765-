/* Tested on gnu-prolog (gprolog). */


%:- use_module(library(lists)).

/* Q4 Solution 

	puzzle(...) :: Takes 9 variables as arguments. 
	puzzleTuples(...) :: Takes tuples as arguments. 

*/ 


puzzle(A,B,C,D,E,F,G,H,I) :- puzzleTuples((A,B,C),(D,E,F),(G,H,I)).

puzzleTuples((A,B,C),
 	(D,E,F),
 	(G,H,I))    :- 
					Q = [1,2,3,4,5,6,7,8,9],  
					permutation(Q, [A,B,C,D,E,F,G,H,I]),
					Row1 is A+B+C,
					Row2 is D+E+F, 
					Row3 is G+H+I,
					Col1 is A+D+G,
					Col2 is B+E+H,
					Col3 is C+F+I,
					Daig1 is A+E+I,
					Daig2 is G+E+C,
					Row1 =:= Row2,
					Row2 =:= Row3,
					Row3 =:= Col1, 
					Col1 =:= Col2, 
					Col2 =:= Col3, 
					Col3 =:= Daig1,
					Daig1 =:= Daig2,
					printMapping(A,B,C,D,E,F,G,H,I).
					

printMapping(A,B,C,D,E,F,G,H,I) :- 
	format('The mapping of the puzzle ~n[A,B,C]~n[D,E,F]~n[G,H,I]~n is ~n[~w,~w,~w]~n[~w,~w,~w]~n[~w,~w,~w]~n',[A,B,C,D,E,F,G,H,I]).
