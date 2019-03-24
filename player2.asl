// Agent player2 in project cuatroenraya.mas2j

/* Initial beliefs and rules */
// TEST POSITIONS
testPut(0,3).
testPut(1,1).
testPut(1,4).
testPut(2,2).
testPut(2,4).
testPut(3,3).
testPut(3,5).
testPut(4,1).
testPut(4,5).
testPut(4,6).
testPut(5,2).
testPut(5,3).
testPut(6,0).
testPut(6,1).
testPut(7,0).
testPut(7,2).
testPut(7,3).
testPut(7,4).
testPut(7,6).


movement(1).


// Gets the player number and adds to the beliefs player(playerNumber).
playerNumber :- 
	.my_name(N) &
	.term2string(N,S)&
	.length(S,M) &
	.substring(S,X,(M-1)) &
	.term2string(Y,X) &
	.asserta(player(Y)).


// Forms a list of all possible winning movements
//checkWinningPositions(E,WL):-
//	threeInARow(term((X1,Y1),()))	

winnerVerticalPair(X1,Y1,X1,Y4) :-
	vertical(X1,Y1,X1,Y2) &
	Y1 < Y2 &
	vertical(X1,Y2,X1,Y3).
	
winnerVerticalPair(X1,Y1,X1,Y4) :-
	vertical(X1,Y1,X1,Y2) &
	Y2 < Y1 &
	vertical(X1,Y1,X1,Y3).
	
winnerVerticalPair(X1,Y1,X1,Y4) :-
	vertical(X1,Y1,X1,Y2) & 
	Y1 < Y2 &
	verticalTwoInThree(X1,Y2,X1,Y4).
	
winnerVerticalPair(X1,Y2,X1,Y4) :-
	vertical(X1,Y1,X1,Y2) &
	Y2 < Y1 &
	verticalTwoInThree(X1,Y1,X1,Y4).                             
	  
winnerVerticalPair(_,_,_,_).


threeInARow(TL):-
	pairs(PL) &
	verticalFour(PL, VL) &
	horizontalFour([], HL) &
	diagonalFour([], DL) &
	.concat(VL, HL, TmpL) &
	.concat(TmpL, DL, PL).


// Forms a list of all pairs of chips in the board
pairs(PL) :-
	verticalPair([], VL) &
	horizontalPair([], HL) &
	diagonalPair([], DL) &
	.concat(VL, HL, TmpL) &
	.concat(TmpL, DL, PL).	


// Forms a list of all pairs of chips of the form X[]X in the board
twoInThreePairs(PL) :-
	verticalTwoInThreePair([], VL) &
	horizontalTwoInThreePair([], HL) &
	diagonalTwoInThreePair([], DL) &
	.concat(VL, HL, TmpL) &
	.concat(TmpL, DL, PL).


// Forms a list of all pairs of chips of the form X[][]X in the board
twoInFourPairs(PL) :-
	verticalTwoInFourPair([], VL) &
	horizontalTwoInFourPair([], HL) &
	diagonalTwoInFourPair([], DL) &
	.concat(VL, HL, TmpL) &
	.concat(TmpL, DL, PL).	

	
// Rules for vertical pairs	
verticalPair([],LV) :-
	vertical(X1,Y1,X1,Y2) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y2))],[],TmpL) &
	verticalPair(TmpL,LV).

verticalPair(TmpL,LV) :-
	vertical(X1,Y1,X1,Y2) &
	not .member(pairPos(pos(X1,Y1), pos(X1,Y2)), TmpL) &
	not .member(pairPos(pos(X1,Y2), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y2))],TmpL,TmpL2) &
	verticalPair(TmpL2,LV).

verticalPair(TmpL,LV) :- LV = TmpL.


// Rules for horizontal pairs
horizontalPair([],HL) :-
	horizontal(X1,Y1,X2,Y1) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y1))],[],TmpL) &
	horizontalPair(TmpL,HL).

horizontalPair(TmpL,HL) :-
	horizontal(X1,Y1,X2,Y1) &
	not .member(pairPos(pos(X1,Y1), pos(X2,Y1)), TmpL) &
	not .member(pairPos(pos(X2,Y1), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y1))], TmpL, TmpL2) &
	horizontalPair(TmpL2,HL).

horizontalPair(TmpL,HL) :- HL = TmpL.


// Rules for diagonal pairs
diagonalPair([],DL) :-
	diagonal(X1,Y1,X2,Y2) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y2))],[],TmpL) &
	diagonalPair(TmpL,DL).

diagonalPair(TmpL,DL) :-
	diagonal(X1,Y1,X2,Y2) &
	not .member(pairPos(pos(X1,Y1), pos(X2,Y2)), TmpL) &
	not .member(pairPos(pos(X2,Y2), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y2))],TmpL,TmpL2) &
	diagonalPair(TmpL2,DL).

diagonalPair(TmpL,DL) :- DL = TmpL.


// Rules for two chips in vertical of the form X[]X with no chip in between
verticalTwoInThreePair([],VTL) :-
	verticalTwoInThree(X1,Y1,X1,Y3) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y3))],[],TmpL) &
	verticalTwoInThreePair(TmpL,VTL).
	
verticalTwoInThreePair(TmpL,VTL) :-
	verticalTwoInThree(X1,Y1,X1,Y3) &
	not .member(pairPos(pos(X1,Y1), pos(X1,Y3)), TmpL) &
	not .member(pairPos(pos(X1,Y3), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y3))], TmpL, TmpL2) &
	verticalTwoInThreePair(TmpL2,VTL).
	
verticalTwoInThreePair(TmpL,VTL) :- VTL = TmpL.


// Rules for two chips in horizontal of the form X[]X with no chip in between 
horizontalTwoInThreePair([],HTL) :-
	horizontalTwoInThree(X1,Y1,X3,Y1) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y1))],[],TmpL) &
	horizontalTwoInThreePair(TmpL,HTL).
	
horizontalTwoInThreePair(TmpL,HTL) :-
	horizontalTwoInThree(X1,Y1,X3,Y1) &
	not .member(pairPos(pos(X1,Y1), pos(X3,Y1)), TmpL) &
	not .member(pairPos(pos(X3,Y1), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y1))], TmpL, TmpL2) &
	horizontalTwoInThreePair(TmpL2,HTL).
	
horizontalTwoInThreePair(TmpL,HTL) :- HTL = TmpL.


// Rules for two chips in diagonal of the form X[]X with no chip in between 
diagonalTwoInThreePair([],DTL) :-
	diagonalTwoInThree(X1,Y1,X1,Y3) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y3))],[],TmpL) &
	diagonalTwoInThreePair(TmpL,DTL).
	 
diagonalTwoInThreePair(TmpL,DTL) :-
	diagonalTwoInThree(X1,Y1,X1,Y3) &
	not .member(pairPos(pos(X1,Y1), pos(X3,Y3)), TmpL) &
	not .member(pairPos(pos(X3,Y3), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y3))], TmpL, TmpL2) &
	diagonalTwoInThreePair(TmpL2,DTL).
	
diagonalTwoInThreePair(TmpL,DTL) :- DTL = TmpL.


// Rules for two chips in vertical of the form X[][]X with no chips in between
verticalTwoInFourPair([],VTL) :-
	verticalTwoInFour(X1,Y1,X1,Y4) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y4))],[],TmpL) &
	verticalTwoInFourPair(TmpL,VTL).
	
verticalTwoInFourPair(TmpL,VTL) :-
	verticalTwoInFour(X1,Y1,X1,Y4) &
	not .member(pairPos(pos(X1,Y1), pos(X1,Y4)), TmpL) &
	not .member(pairPos(pos(X1,Y4), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y4))], TmpL, TmpL2) &
	verticalTwoInFourPair(TmpL2,VTL).
	
verticalTwoInFourPair(TmpL,VTL) :- VTL = TmpL.


// Rules for two chips in horizontal of the form X[][]X with no chips in between 
horizontalTwoInFourPair([],HTL) :-
	horizontalTwoInFour(X1,Y1,X4,Y1) &
	.concat([pairPos(pos(X1,Y1), pos(X4,Y1))],[],TmpL) &
	horizontalTwoInFourPair(TmpL,HTL).
	
horizontalTwoInFourPair(TmpL,HTL) :-
	horizontalTwoInFour(X1,Y1,X4,Y1) &
	not .member(pairPos(pos(X1,Y1), pos(X4,Y1)), TmpL) &
	not .member(pairPos(pos(X4,Y1), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X4,Y1))], TmpL, TmpL2) &
	horizontalTwoInFourPair(TmpL2,HTL).
	
horizontalTwoInFourPair(TmpL,HTL) :- HTL = TmpL.


// Rules for two chips in diagonal of the form X[][]X with no chips in between 
diagonalTwoInFourPair([],DTL) :-
	diagonalTwoInFour(X1,Y1,X4,Y4) &
	.concat([pairPos(pos(X1,Y1), pos(X4,Y4))],[],TmpL) &
	diagonalTwoInFourPair(TmpL,DTL).
	 
diagonalTwoInFourPair(TmpL,DTL) :-
	diagonalTwoInFour(X1,Y1,X4,Y4) &
	not .member(pairPos(pos(X1,Y1), pos(X4,Y4)), TmpL) &
	not .member(pairPos(pos(X4,Y4), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X4,Y4))], TmpL, TmpL2) &
	diagonalTwoInFourPair(TmpL2,DTL).
	
diagonalTwoInFourPair(TmpL,DTL) :- DTL = TmpL.


// Rule to get a vertical pair XX
vertical(X1,Y1,X1,Y2) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X1,Y2,M) &
	(Y2 = Y1 + 1).


// Rule to get a horizontal pair XX
horizontal(X1,Y1,X2,Y1) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y1,M) &
	(X2 = X1 + 1).


// Rule to get a diagonal pair XX
diagonal(X1,Y1,X2,Y2) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y2,M) &
	( ((X2 = X1 + 1) & (Y2 = Y1 + 1)) |
	  ((X2 = X1 + 1) & (Y2 = Y1 - 1)) |
	  ((X2 = X1 - 1) & (Y2 = Y1 + 1)) |
	  ((X2 = X1 - 1) & (Y2 = Y1 - 1)) ).


// Rule to get a vertical pair X[]X
verticalTwoInThree(X1,Y1,X1,Y3) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X1,Y2,0) &
	tablero(X1,Y3,M) &
	( ((Y3 = Y1 + 2) & (Y2 = Y1 + 1)) | 
	  ((Y3 = Y1 - 2) & (Y2 = Y1 - 1)) ).


// Rule to get a horizontal pair X[]X
horizontalTwoInThree(X1,Y1,X3,Y1) :- 
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y1,0) &
	tablero(X3,Y1,M) &
	( ((X3 = X1 + 2) & (X2 = X1 + 1)) | 
	  ((X3 = X1 - 2) & (X2 = X1 - 1)) ).


// Rule to get a diagonal pair X[]X
diagonalTwoInThree(X1,Y1,X3,Y3) :- 
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y2,0) &
	tablero(X3,Y3,M) &
	( ( (X3 = X1 + 2) & (X2 = X1 + 1) &
	    (Y3 = Y1 + 2) & (Y2 = Y1 + 1) ) |
	  ( (X3 = X1 + 2) & (X2 = X1 + 1) &
	    (Y3 = Y1 - 2) & (Y2 = Y1 - 1) ) |
	  ( (X3 = X1 - 2) & (X2 = X1 - 1) &
	  	(Y3 = Y1 + 2) & (Y2 = Y1 + 1) ) |
	  ( (X3 = X1 - 2) & (X2 = X1 - 1) &
	  	(Y3 = Y1 - 2) & (Y2 = Y1 - 1) ) ).


// Rule to get a vertical pair X[][]X
verticalTwoInFour(X1,Y1,X1,Y4) :- 
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X1,Y2,0) &
	tablero(X1,Y3,0) &
	tablero(X1,Y4,M) &
	( ((Y4 = Y1 + 3) & (Y3 = Y1 + 2) & (Y2 = Y1 + 1)) | 
	  ((Y4 = Y1 - 3) & (Y3 = Y1 - 2) & (Y2 = Y1 - 1)) ).


// Rule to get a horizontal pair X[][]X
horizontalTwoInFour(X1,Y1,X4,Y1) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y1,0) &
	tablero(X3,Y1,0) &
	tablero(X4,Y1,M) &
	( ((X4 = X1 + 3) & (X3 = X1 + 2) & (X2 = X1 + 1)) | 
	  ((X4 = X1 - 3) & (X3 = X1 - 2) & (X2 = X1 - 1)) ).


// Rule to get a diagonal pair X[][]X
diagonalTwoInFour(X1,Y1,X4,Y4):-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y2,0) &
	tablero(X3,Y3,0) &
	tablero(X4,Y4,M) &
	( ( (X4 = X1 + 3) & (X3 = X1 + 2) & (X2 = X1 + 1) &
	    (Y4 = Y1 + 3) & (Y3 = Y1 + 2) & (Y2 = Y1 + 1) ) |
	  ( (X4 = X1 + 3) & (X3 = X1 + 2) & (X2 = X1 + 1) &
	    (Y4 = Y1 - 3) & (Y3 = Y1 - 2) & (Y2 = Y1 - 1) ) |
	  ( (X4 = X1 - 3) & (X3 = X1 - 2) & (X2 = X1 - 1) &
	    (Y4 = Y1 + 3) & (Y3 = Y1 + 2) & (Y2 = Y1 + 1) ) |
	  ( (X4 = X1 - 3) & (X3 = X1 - 2) & (X2 = X1 - 1) &
	    (Y4 = Y1 - 3) & (Y3 = Y1 - 2) & (Y2 = Y1 - 1) ) ).

orderPositionsVertical(X1,Y1,X1,Y2):-
	Y1 < Y2.
orderPositionsVertical(X1,Y1,X1,Y2):-
	Y2 < Y1.

orderPositionsHorizontal(X1,Y1,X2,Y1):-
	X1 < X2.
orderPositionsHorizonal(X1,Y1,X2,Y1):-
	X2 < X1.



verticalListSort([pairPos(pos(X1,Y1), pos(X1,Y2))|Tail1],
						[pairPos(pos(X1,Y1), pos(X1,Y2))]|Tail2) :-
	Y1 < Y2 &
	verticalListSort(Tail1,Tail2).

verticalListSort([pairPos(pos(X1,Y1), pos(X1,Y2))|Tail1],
						[pairPos(pos(X1,Y2), pos(X1,Y1))]|Tail2) :-
	verticalListSort(Tail1,Tail2).

verticalListSort([],[]).


horizontalListSort([pairPos(pos(X1,Y1), pos(X2,Y1))|Tail1],
						[pairPos(pos(X1,Y1), pos(X2,Y1))]|Tail2) :-
	X1 < X2 &
	horizontalListSort(Tail1,Tail2).

horizontalListSort([pairPos(pos(X1,Y1), pos(X2,Y1))|Tail1],
						[pairPos(pos(X2,Y1), pos(X1,Y1))]|Tail2) :-
	horizontalListSort(Tail1,Tail2).
	
horizontalListSort([],[]).




/* Initial goals */



!play.



/* Plans */

// PLAY TO WIN
+!play: playerNumber &
	estrategia(jugarAGanar) <-
		.print("A ganar");
		!playToTest.

// PLAY TO LOSE
+!play:
	estrategia(jugarAPerder) <-
		.print("A perder");
		!playToLose.
		
//TEST AREA		
// Plan to play a few rounds and generate a board's state to test
+!playToTest:
	testPut(X,Y) &
	turno(player2) <- 
		put(X,Y);
		-testPut(X,Y);
		-+movement(N+1);
		!playToTest.
		
+!playToTest:
	not testPut(X,Y) <-
		?twoInThreePairs(L);
		.print("Pair list of two in three: ", L);
		?twoInFourPairs(L2);
		.print("Pair list of two in four: ", L2);
		?test(X1,Y1,X2,Y2);
		.print("Test: (",X1,",",Y1,")(",X2,",",Y2,")");
		!playToWin.

+!playToTest <- !playToTest.


//WINNING PLAN
+!playToWin <-
	!move(X,Y,E).


//LOSING PLAN



//MOVEMENT PLAN
+!move <-
	!checkWinningPositions(E,WL);
	!checkLosingPosition(E,LL);
	!decide(WL,LL,X,Y);
	.print("He decidido mover a: (",X,",",Y,")"). 


//!checkWinningPositions(E,WL) <-
	
	
//ERRORS
+!play <- .print("Error in !play").
+!playToWin <- .print("Error in !playToWin").
+!playToLose <- .print("Error in !playToLose").
+!playToTest <- .print("Error or finish !playToTest").
