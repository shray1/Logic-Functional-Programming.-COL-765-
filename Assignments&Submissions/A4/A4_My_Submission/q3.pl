/* Tested on gnu-prolog (gprolog). */


/* Q3 Solution. 

	Pseudo-logic.
	towerOfHanoi(n, source, target, aux){
		if(n==0) return;
		towerOfHanoi(n-1, source, aux, target)
		shiftDisk(n,source,target)
		towerOfHanoi(n-1, aux, target, source)
	}

	towerOfHanoi(X) :: It will print the moves, 
						which we should make to shift X number 
						of disks from LEFT stack to RIGHT stack
						using auxillary CENTER stack. 

Sample Usage :: 

	- load file. 
	- run 'towerOfHanoi(4).'
*/ 


towerOfHanoi(N) :- 
	format('Tower Of Hanoi : To move ~w disks from left stack to right stack using center stack.',[N]),
	towerOfHanoiHelper(N,left,right,center).

towerOfHanoiHelper(0,_,_,_).
towerOfHanoiHelper(Num, A, B, C) :- 
		Num >= 1,
		Num2 is Num-1,
		towerOfHanoiHelper(Num2,A,C,B), 
		format('shifting element ~w from stack ~w to ~w.~n',[Num,A,B]),
		towerOfHanoiHelper(Num2,C,B,A).

