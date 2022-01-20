/* Tested on gnu-prolog (gprolog). */


/* 
	Q5 Solution. 

	Sample Usage : 

	-load file. 
	-run "nQueens(<integer>)." 
	-example "?- nQueens(4)."  It will output a possible list of positions of N-Queens. 


	Note : Some explanation & reference given at the end of file.

*/


nQueens(N) :- 
	N >= 0,
	n_queens(N,Qs),
	format('~nThe below list is one such possible permuation of N-Queens in [(Row,Col)] format. ~n[ ',[N]),
	print_n_queens(Qs,1).
	
print_n_queens([],_) :- format('].~n',[]).
print_n_queens([Queen|QList],ColNum) :-
	format('(~w,~w) ',[Queen,ColNum]),
	ColNum2 is ColNum+1,
	print_n_queens(QList,ColNum2).

n_queens(N, Qs) :-
        getNumList(NumList,N),
        permutation(NumList,Qs),
        myLength(Qs, N),
        notExceeding(Qs, N),
        safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :-
        safe_queens(Qs, Q, 1),
        safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
        Q0 =\= Q,
        abs(Q0 - Q) =\= D0,
        D1 is D0+1,
        safe_queens(Qs, Q0, D1).

myLength([], 0).
myLength([_|TailList], N) :- 
		myLength(TailList,M),
		N is M+1.

notExceeding([],_) .
notExceeding([A|TailList],N) :-
		A =< N, 
		notExceeding(TailList,N). 


getNumList([],0).
getNumList([A|TailList],N) :- 
		N >= 0, 
		A is N,
		M is N-1, 
		getNumList(TailList,M).


/* 
	A basic solution to N-queens would be to permute on all possible
	values of queen and check for each constraint. 
	But this solution would be too slow, and hence,
	comes the need to provide constraints in such a manner
	that our permutations are not wasted. 

	Keeping this in mind, the solution, already seperates the different 
	queens and permutes them in such that way that no queen can be put 
	in the same row. Further, keeping queens in a list of size N helps in
	ensuring that, one queen is only present in one column only. 


	There is a wonderful explanation given in the following video, and I have taken
	motivation/reference from this video. 
	Reference Material  : "https://youtu.be/l_tbL9RjFdo"

*/ 



