/* Tested on gnu-prolog (gprolog). */

parent(arti,babli).
parent(arti,bunty).
parent(babli,chitra).
parent(babli,chintan).
parent(bunty,divya).
parent(bunty,divesh).
male(bunty).
male(chintan).
male(divesh).



/* Q1 Solution.

	isChildless(X)   :: It checks if the passed input person is a parent of any child or not. 
						It is successful of if the passed person is not a parent of any child. 

	getAllChildless(X) :: It will calculate all the childless persons.					

 */ 

isChildless(X) :- \+ parent(X,_).

getAllChildless(A) :-  
		setof(X,Y^parent(X,Y),Z1) , 
		setof(Y,X^parent(X,Y),Z2) , 
		concatList(Z1,Z2,Z3) , 
		sort(Z3,Z) , 
		fetchChildlessPersons(Z,A) . 



/* Helper Predicates. */

fetchChildlessPersons([],[]).
fetchChildlessPersons([Person|TailList],X) :- 
		 \+ isChildless(Person), 
		 fetchChildlessPersons(TailList,Y), 
		 X = Y.
fetchChildlessPersons([Person|TailList],[Person|Result]) :-
		 isChildless(Person), 
		 fetchChildlessPersons(TailList,Result).

concatList([],L,L).
concatList([H|T],L,[H|Z]):- concatList(T,L,Z).



