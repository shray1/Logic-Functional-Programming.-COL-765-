/* Tested on gnu-prolog (gprolog). */

:- dynamic(parent/2).
parent(arti,babli).
parent(arti,bunty).
parent(babli,chitra).
parent(babli,chintan).
parent(bunty,divya).
parent(bunty,divesh).
male(bunty).
male(chintan).
male(divesh).



/* 
	Q2 Solution

	updateQ2  :: This will bring father(X,Y) and mother(X,Y) predicates in our knowledge base
				 and remove all parent(X,Y) predicates from our knowledge base (database). 

	addFather(X,Y) :: This will add a father, if X is male, and Y is a child of X. 
	addMother(X,Y) :: This will add a mother, if X is female and Y is a child of X. 

Sample Run Steps : 
	-Load the database.
	-Do listing. 
	-run predicate 'updateQ2'.
	-Do listing and check for updated database.

*/ 


updateQ2 :- 
	parentList(X), 
	updateList(X), 
	retractall(parent(_,_)).

addFather(X,Y) :- 
	male(X),
	parent(X,Y),
	assertz(father(X,Y)).

addMother(X,Y) :- 
	female(X),
	parent(X,Y),
	assertz(mother(X,Y)).


% Helper predicates. 
parentList(Z) :- findall([X,Y|[]],parent(X,Y),Z).
female(X) :- \+ male(X).
updateList([]).
updateList([FirstPair|TailList]) :- 
	 [A,B|[]] = FirstPair, 
	 assertItem(A,B),
	 updateList(TailList).

assertItem(X,Y) :- 
	 male(X),
	 assertz(father(X,Y)).
assertItem(X,Y) :- 
	 female(X),
	 assertz(mother(X,Y)).

