// Agent player2 in project cuatroenraya.mas2j

/* Initial beliefs and rules */
// TEST POSITIONS
testPut(0,3).
testPut(1,1).
testPut(1,4).
testPut(2,2).
testPut(2,5).
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
testPut(5,1).


movement(1).


// Gets the player number and adds to the beliefs player(playerNumber).
playerNumber :- 
	.my_name(N) &
	.term2string(N,S) &
	.length(S,M) &
	.substring(S,X,(M-1)) &
	.term2string(Y,X) &
	.asserta(player(Y)).


checkEmpty(X,Y):-
	tablero(X,Y,0).

	
winnerTotal(L,P):-
	winnerVertical(L1,P) &
	winnerHorizontal(L2,P) & 
	winnerDiagonal(L3,P) &
	.concat(L1,L2,LT1) &
	.concat(LT1,L3,L).


winnerVertical(L,P):-
	winnerVerticalPairTop([],L1,P) &
	winnerVerticalPairBottom([],L2,P) &
	.union(L1,L2,L).


winnerVerticalPairTop([],L,P) :-
	vertical(X1,Y1,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y0) &
	(Y0 = Y1 - 1) &
	.concat([pos(X1,Y0)],[],TmpL) &
	winnerVerticalPairTop(TmpL,L,P).

winnerVerticalPairTop(TmpL,L,P) :-
	vertical(X1,Y1,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y0) &
	(Y0 = Y1 - 1) &
	not .member(pos(X1,Y0),TmpL) &
	.concat([pos(X1,Y0)],TmpL,TmpL2) &
	winnerVerticalPairTop(TmpL2,L,P).

winnerVerticalPairTop([],[],P).
winnerVerticalPairTop(TmpL,L,P) :- L = TmpL.


winnerVerticalPairBottom([],L,P) :-
	vertical(X1,Y1,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y4) &
	(Y4 = Y3 + 1) &
	.concat([pos(X1,Y4)],[],TmpL) &
	winnerVerticalPairBottom(TmpL,L,P).

winnerVerticalPairBottom(TmpL,L,P) :-
	vertical(X1,Y1,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y4) &
	(Y4 = Y3 + 1) &
	not .member(pos(X1,Y4),TmpL) &
	.concat([pos(X1,Y4)],TmpL,TmpL2) &
	winnerVerticalPairBottom(TmpL2,L,P).

winnerVerticalPairBottom([],[],P).
winnerVerticalPairBottom(TmpL,L,P) :- L = TmpL.


winnerHorizontal(L,P):-
	winnerHorizontalPairLeft([],L1,P) &
	winnerHorizontalPairRight([],L2,P) &
	.union(L1,L2,L).


winnerHorizontalPairLeft([],L,P) :-
	horizontal(X1,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X0,Y1) &
	(X0 = X1 - 1) &
	.concat([pos(X0,Y1)],[],TmpL) &
	winnerHorizontalPairLeft(TmpL,L,P).

winnerHorizontalPairLeft(TmpL,L,P) :-
	horizontal(X1,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X0,Y1) &
	(X0 = X1 - 1) &
	not .member(pos(X0,Y1),TmpL) &
	.concat([pos(X0,Y1)],TmpL,TmpL2) &
	winnerHorizontalPairLeft(TmpL2,L,P).

winnerHorizontalPairLeft([],[],P).
winnerHorizontalPairLeft(TmpL,L,P) :- L = TmpL.


winnerHorizontalPairRight([],L,P) :-
	horizontal(X1,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X4,Y1) &
	(X4 = X3 + 1) &
	.concat([pos(X4,Y1)],[],TmpL) &
	winnerHorizontalPairRight(TmpL,L,P).

winnerHorizontalPairRight(TmpL,L,P) :-
	horizontal(X1,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X4,Y1) &
	(X4 = X3 + 1) &
	not .member(pos(X4,Y1),TmpL) &
	.concat([pos(X4,Y1)],TmpL,TmpL2) &
	winnerHorizontalPairRight(TmpL2,L,P).

winnerHorizontalPairRight([],[],P).
winnerHorizontalPairRight(TmpL,L,P) :- L = TmpL.


winnerDiagonal(L,P) :-
	winnerDiagonalPairTopLeft(L1,P) &            
	winnerDiagonalPairBottomRight(L2,P) &
	winnerDiagonalPairTopRight(L3,P) &            
	winnerDiagonalPairBottomLeft(L4,P) &
	.concat(L1,L2,LT1) &
	.concat(L3,L4,LT2) &
	.concat(LT1,LT2,L).



















/**
winnerVerticalPairBottom([pos(X1,Y4)],P) :-
	vertical(X1,Y1,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y4) &
	(Y4 = Y3 + 1).

winnerVertical([],_).


winnerHorizontal(L,P):-
	winnerHorizontalPairLeft(L1,P) &            
	winnerHorizontalPairRight(L2,P) &
	.concat(L1,L2,L).

winnerHorizontalPairLeft([pos(X0,Y1)],P) :-
	horizontal(X1,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X0,Y1) &
	(X0 = X1 - 1).

winnerHorizontalPairRight([pos(X4,Y1)],P) :-
	horizontal(X1,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X4,Y1) &
	(X4 = X3 + 1).

winnerHorizontal([],_).
winnerHorizontalPairTop([],_).
winnerHorizontalPairBottom([],_).


winnerDiagonal(L,P) :-
	winnerDiagonalPairTopLeft(L1,P) &            
	winnerDiagonalPairBottomRight(L2,P) &
	winnerDiagonalPairTopRight(L3,P) &            
	winnerDiagonalPairBottomLeft(L4,P) &
	.concat(L1,L2,LT1) &
	.concat(L3,L4,LT2) &
	.concat(LT1,LT2,L).


winnerDiagonalPairTopLeft([pos(X0,Y0)],P) :-
	diagonal(X1,Y1,X2,Y2,P) &                           
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2 + 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X0,Y0) &
	(Y0 = Y1 - 1) &
	(X0 = X1 - 1).

winnerDiagonalPairBottomRight([pos(X4,Y4)],P) :-
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2 + 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X4,Y4) &
	(Y4 = Y3 + 1) &
	(X4 = X3 + 1).

winnerDiagonalPairTopRight([pos(X0,Y0)],P) :-
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2 - 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X0,Y0) &
	(Y0 = Y1 - 1) &
	(X0 = X1 + 1).

winnerDiagonalPairBottomLeft([pos(X4,Y4)],P) :-
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2 - 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X4,Y4) &
	(Y4 = Y3 + 1) &
	(X4 = X3 - 1).

winnerDiagonal([],_).
winnerDiagonalPairTopLeft([],_).
winnerDiagonalPairBottomRight([],_).
winnerDiagonalPairTopRight([],_).
winnerDiagonalPairBottomLeft([],_).
*/


//verticalWinningPair(WPL,E) :- 
//	winnerVerticalPair(X,Y) &

//horizontalWinningPair() :-
//diagonalWinningPair() :-

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
	.concat([pairPos(pos(X1,Y1), pos(X4,Y4))], TmpL, TmpL2) &
	diagonalTwoInFourPair(TmpL2,DTL).
	
diagonalTwoInFourPair(TmpL,DTL) :- DTL = TmpL.


// Rule to get a vertical pair XX
vertical(X1,Y1,X1,Y2,P) :-
	tablero(X1,Y1,P) &
	tablero(X1,Y2,P) &
	(Y2 = Y1 + 1).


// Rule to get a horizontal pair XX
horizontal(X1,Y1,X2,Y1,P) :-
	tablero(X1,Y1,P) &
	tablero(X2,Y1,P) &
	(X2 = X1 + 1).


// Rule to get a diagonal pair XX
diagonal(X1,Y1,X2,Y2,P) :-
	tablero(X1,Y1,P) &
	tablero(X2,Y2,P) &
	( ((X2 = X1 + 1) & (Y2 = Y1 + 1)) |
	  ((X2 = X1 - 1) & (Y2 = Y1 + 1)) ).


// Rule to get a vertical pair X[]X
verticalTwoInThree(X1,Y1,X1,Y3,P) :-
	tablero(X1,Y1,P) &
	tablero(X1,Y2,0) &
	tablero(X1,Y3,P) &
	(Y3 = Y1 + 2) &
	(Y2 = Y1 + 1).


// Rule to get a horizontal pair X[]X
horizontalTwoInThree(X1,Y1,X3,Y1,P) :- 
	tablero(X1,Y1,P) &
	tablero(X2,Y1,0) &
	tablero(X3,Y1,P) &
	(X3 = X1 + 2) &
	(X2 = X1 + 1).


// Rule to get a diagonal pair X[]X
diagonalTwoInThree(X1,Y1,X3,Y3,P) :- 
	tablero(X1,Y1,P) &
	tablero(X2,Y2,0) &
	tablero(X3,Y3,P) &
	( ((X3 = X1 + 2) & (X2 = X1 + 1)  &
	   (Y3 = Y1 + 2) & (Y2 = Y1 + 1)) |
	  ((X3 = X1 - 2) & (X2 = X1 - 1)  &
	   (Y3 = Y1 + 2) & (Y2 = Y1 + 1)) ).


// Rule to get a vertical pair X[][]X
verticalTwoInFour(X1,Y1,X1,Y4,P) :- 
	tablero(X1,Y1,P) &
	tablero(X1,Y2,0) &
	tablero(X1,Y3,0) &
	tablero(X1,Y4,P) &
	(Y4 = Y1 + 3) &
	(Y3 = Y1 + 2) &
	(Y2 = Y1 + 1).


// Rule to get a horizontal pair X[][]X
horizontalTwoInFour(X1,Y1,X4,Y1,P) :-
	tablero(X1,Y1,P) &
	tablero(X2,Y1,0) &
	tablero(X3,Y1,0) &
	tablero(X4,Y1,P) &
	(X4 = X1 + 3) &
	(X3 = X1 + 2) &
	(X2 = X1 + 1).


// Rule to get a diagonal pair X[][]X
diagonalTwoInFour(X1,Y1,X4,Y4,P):-
	tablero(X1,Y1,P) &
	tablero(X2,Y2,0) &
	tablero(X3,Y3,0) &
	tablero(X4,Y4,P) &
	( ( (X4 = X1 + 3) & (X3 = X1 + 2) & (X2 = X1 + 1) &
	    (Y4 = Y1 + 3) & (Y3 = Y1 + 2) & (Y2 = Y1 + 1) ) |
	  ( (X4 = X1 - 3) & (X3 = X1 - 2) & (X2 = X1 - 1) &
	    (Y4 = Y1 + 3) & (Y3 = Y1 + 2) & (Y2 = Y1 + 1) ) ).


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
		!playToWin.

+!playToTest <- !playToTest.


//WINNING PLAN
+!playToWin <-
	!checkBoard(L,E).


//LOSING PLAN



//MOVEMENT PLAN
+!checkBoard(L,E):
	player(M) <-
		?winnerVertical(L,P);
		.print(L);
		?winnerHorizontal
		(L2,P);
		.print(L2).
		
	//!checkWinningPosition(E,WL);
	//!checkLosingPosition(E,LL);
	//!decide(WL,LL,X,Y)

//!checkWinningPosition(E,WL):
//	player(M) <-

	
	
//ERRORS
+!play <- .print("Error in !play").
+!playToWin <- .print("Error in !playToWin").
+!playToLose <- .print("Error in !playToLose").
+!playToTest <- .print("Error or finish !playToTest").
+!checkBoard(_,_) <- .print("Error in !checkBoard").