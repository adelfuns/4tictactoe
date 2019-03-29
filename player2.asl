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


// Gets the players number and adds to the beliefs player(playerNumber)
// and opponent(playerNumber2).
playerNumbers :- 
	.my_name(N) &
	.term2string(N,S) &
	.length(S,M) &
	.substring(S,X,(M-1)) &
	.term2string(Y,X) &
	.asserta(player(Y)) &
	enemyNumber(Y,Z) &
	.asserta(opponent(Z)).

// Gets the opponent player number
enemyNumber(X,2):-
	X = 1.

enemyNumber(X,1):-
	X = 2.


// Checks if the board in X-Y position is free
checkEmpty(X,Y):-
	tablero(X,Y,0).


// Gets all of the winning positions	
listWinPositions(L):-
	player(P) &
	listVerticalWinPositions(L1,P) &
	listHorizontalWinPositions(L2,P) & 
	listDiagonalWinPositions(L3,P) &
	.union(L1,L2,LT1) &
	.union(LT1,L3,L).

// Gets all of the losing positions
listLosePositions(L):-
	opponent(P) &
	listVerticalWinPositions(L1,P) &
	listHorizontalWinPositions(L2,P) & 
	listDiagonalWinPositions(L3,P) &
	.union(L1,L2,LT1) &
	.union(LT1,L3,L).


// Gets vertical winning positions
listVerticalWinPositions(L,P):-
	listVerticalWinPositionsTop([],L1,P) &
	listVerticalWinPositionsBottom([],L2,P) &
	.union(L1,L2,L).


listVerticalWinPositionsTop([],L,P) :-
	vertical(X1,Y1,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y0) &
	(Y0 = Y1 - 1) &
	.concat([pos(X1,Y0)],[],TmpL) &
	listVerticalWinPositionsTop(TmpL,L,P).

listVerticalWinPositionsTop(TmpL,L,P) :-
	vertical(X1,Y1,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y0) &
	(Y0 = Y1 - 1) &
	not .member(pos(X1,Y0),TmpL) &
	.concat([pos(X1,Y0)],TmpL,TmpL2) &
	listVerticalWinPositionsTop(TmpL2,L,P).

listVerticalWinPositionsTop([],[],P).
listVerticalWinPositionsTop(TmpL,L,P) :- L = TmpL.


listVerticalWinPositionsBottom([],L,P) :-
	vertical(X1,Y1,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y4) &
	(Y4 = Y3 + 1) &
	.concat([pos(X1,Y4)],[],TmpL) &
	listVerticalWinPositionsBottom(TmpL,L,P).

listVerticalWinPositionsBottom(TmpL,L,P) :-
	vertical(X1,Y1,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y4) &
	(Y4 = Y3 + 1) &
	not .member(pos(X1,Y4),TmpL) &
	.concat([pos(X1,Y4)],TmpL,TmpL2) &
	listVerticalWinPositionsBottom(TmpL2,L,P).

listVerticalWinPositionsBottom([],[],P).
listVerticalWinPositionsBottom(TmpL,L,P) :- L = TmpL.


// Gets horizontal winning positions
listHorizontalWinPositions(L,P):-
	listHorizontalWinPositionsLeft([],L1,P) &
	listHorizontalWinPositionsRight([],L2,P) &
	.union(L1,L2,L).


listHorizontalWinPositionsLeft([],L,P) :-
	horizontal(X1,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X0,Y1) &
	(X0 = X1 - 1) &
	.concat([pos(X0,Y1)],[],TmpL) &
	listHorizontalWinPositionsLeft(TmpL,L,P).

listHorizontalWinPositionsLeft(TmpL,L,P) :-
	horizontal(X1,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X0,Y1) &
	(X0 = X1 - 1) &
	not .member(pos(X0,Y1),TmpL) &
	.concat([pos(X0,Y1)],TmpL,TmpL2) &
	listHorizontalWinPositionsLeft(TmpL2,L,P).

listHorizontalWinPositionsLeft([],[],P).
listHorizontalWinPositionsLeft(TmpL,L,P) :- L = TmpL.


listHorizontalWinPositionsRight([],L,P) :-
	horizontal(X1,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X4,Y1) &
	(X4 = X3 + 1) &
	.concat([pos(X4,Y1)],[],TmpL) &
	listHorizontalWinPositionsRight(TmpL,L,P).

listHorizontalWinPositionsRight(TmpL,L,P) :-
	horizontal(X1,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X4,Y1) &
	(X4 = X3 + 1) &
	not .member(pos(X4,Y1),TmpL) &
	.concat([pos(X4,Y1)],TmpL,TmpL2) &
	listHorizontalWinPositionsRight(TmpL2,L,P).

listHorizontalWinPositionsRight([],[],P).
listHorizontalWinPositionsRight(TmpL,L,P) :- L = TmpL.


// Gets diagonal winning positions
listDiagonalWinPositions(L,P) :-
	listDiagonalWinPositionsTopLeft([],L1,P) &            
	listDiagonalWinPositionsBottomRight([],L2,P) &
	listDiagonalWinPositionsTopRight([],L3,P) &            
	listDiagonalWinPositionsBottomLeft([],L4,P) &
	.union(L1,L2,LT1) &
	.union(L3,L4,LT2) &
	.union(LT1,LT2,L).


listDiagonalWinPositionsTopLeft([],L,P) :-
	diagonal(X1,Y1,X2,Y2,P) &                           
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2 + 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X0,Y0) &
	(Y0 = Y1 - 1) &
	(X0 = X1 - 1) &
	.concat([pos(X0,Y0)],[],TmpL) &
	listDiagonalWinPositionsTopLeft(TmpL,L,P).

listDiagonalWinPositionsTopLeft(TmpL,L,P) :-
	diagonal(X1,Y1,X2,Y2,P) &                           
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2 + 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X0,Y0) &
	(Y0 = Y1 - 1) &
	(X0 = X1 - 1) &
	not .member(pos(X0,Y0),TmpL) &
	.concat([pos(X0,Y0)],[],TmpL) &
	listDiagonalWinPositionsTopLeft(TmpL,L,P).

listDiagonalWinPositionsTopLeft([],[],P).
listDiagonalWinPositionsTopLeft(TmpL,L,P) :- L = TmpL.


listDiagonalWinPositionsTopRight([],L,P) :-
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2 - 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X0,Y0) &
	(Y0 = Y1 - 1) &
	(X0 = X1 + 1) &
	.concat([pos(X0,Y0)],[],TmpL) &
	listDiagonalWinPositionsTopRight(TmpL,L,P).

listDiagonalWinPositionsTopRight(TmpL,L,P) :-
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2 - 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X0,Y0) &
	(Y0 = Y1 - 1) &
	(X0 = X1 + 1) &
	not .member(pos(X0,Y0),TmpL) &
	.concat([pos(X0,Y0)],[],TmpL) &
	listDiagonalWinPositionsTopRight(TmpL,L,P).

listDiagonalWinPositionsTopRight([],[],P).
listDiagonalWinPositionsTopRight(TmpL,L,P) :- L = TmpL.


listDiagonalWinPositionsBottomLeft([],L,P) :-
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2 - 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X4,Y4) &
	(Y4 = Y3 + 1) &
	(X4 = X3 - 1) &
	.concat([pos(X4,Y4)],[],TmpL) &
	listDiagonalWinPositionsBottomLeft(TmpL,L,P).

listDiagonalWinPositionsBottomLeft(TmpL,L,P) :-
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2 - 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X4,Y4) &
	(Y4 = Y3 + 1) &
	(X4 = X3 - 1) &
	not .member(pos(X4,Y4),TmpL) &
	.concat([pos(X4,Y4)],[],TmpL) &
	listDiagonalWinPositionsBottomLeft(TmpL,L,P).

listDiagonalWinPositionsBottomLeft([],[],P).
listDiagonalWinPositionsBottomLeft(TmpL,L,P) :- L = TmpL.


listDiagonalWinPositionsBottomRight([],L,P) :-
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2 + 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X4,Y4) &
	(Y4 = Y3 + 1) &
	(X4 = X3 + 1) &
	.concat([pos(X4,Y4)],[],TmpL) &
	listDiagonalWinPositionsBottomRight(TmpL,L,P).

listDiagonalWinPositionsBottomRight(TmpL,L,P) :-
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2 + 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X4,Y4) &
	(Y4 = Y3 + 1) &
	(X4 = X3 + 1) &
	not .member(pos(X4,Y4),TmpL) &
	.concat([pos(X4,Y4)],[],TmpL) &
	listDiagonalWinPositionsBottomRight(TmpL,L,P).

listDiagonalWinPositionsBottomRight([],[],P).
listDiagonalWinPositionsBottomRight(TmpL,L,P) :- L = TmpL.


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
+!play: playerNumbers &
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
		?listWinPositions(L1);
		?listLosePositions(L2);
		.print("Winning positions: ",L1);
		.print("Losing positions: ",L2).
		
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