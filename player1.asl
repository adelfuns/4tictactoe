// Agent player1 in project cuatroenraya.mas2j

/* Initial beliefs and rules */
//TEST POSITIONS
testPut(0,0).
testPut(0,1).
testPut(0,2).
testPut(0,4).
testPut(0,7).
testPut(1,6).
testPut(1,7).
testPut(2,6).
testPut(3,0).
testPut(3,1).
testPut(3,2).
testPut(3,4).
testPut(3,7).
testPut(4,4).
testPut(4,7).
testPut(6,3).
testPut(6,4).
testPut(6,5).
testPut(6,7).

movement(0).


// Gets the player number and adds to the beliefs player(playerNumber).
playerNumber(X) :- 
	.my_name(N) &
	.term2string(N,S)&
	.length(S,M) &
	.substring(S,X,(M-1)) &
	.term2string(Y,X) &
	.asserta(player(Y)).
	

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
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X1,Y2,M) &
	( (Y2 = Y1 + 1) |
	  (Y2 = Y1 - 1) ) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y2))],[],TmpL) &
	verticalPair(TmpL,LV).

verticalPair(TmpL,LV) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X1,Y2,M) &
	( (Y2 = Y1 + 1) |
	  (Y2 = Y1 - 1) ) &
	not .member(pairPos(pos(X1,Y1), pos(X1,Y2)), TmpL) &
	not .member(pairPos(pos(X1,Y2), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y2))],TmpL,TmpL2) &
	verticalPair(TmpL2,LV).

verticalPair(TmpL,LV) :- LV = TmpL.


// Rules for horizontal pairs
horizontalPair([],HL) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y1,M) &
	( (X2 = X1 + 1) |
	  (X2 = X1 - 1)	 ) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y1))],[],TmpL) &
	horizontalPair(TmpL,HL).

horizontalPair(TmpL,HL) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y1,M) &
	( (X2 = X1 + 1) |
	  (X2 = X1 - 1) ) &
	not .member(pairPos(pos(X1,Y1), pos(X2,Y1)), TmpL) &
	not .member(pairPos(pos(X2,Y1), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y1))], TmpL, TmpL2) &
	horizontalPair(TmpL2,HL).

horizontalPair(TmpL,HL) :- HL = TmpL.


// Rules for diagonal pairs
diagonalPair([],DL) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y2,M) &
	( ((X2 = X1 + 1) & (Y2 = Y1 + 1)) |
	  ((X2 = X1 + 1) & (Y2 = Y1 - 1)) |
	  ((X2 = X1 - 1) & (Y2 = Y1 + 1)) |
	  ((X2 = X1 - 1) & (Y2 = Y1 - 1)) ) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y2))],[],TmpL) &
	diagonalPair(TmpL,DL).

diagonalPair(TmpL,DL) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y2,M) &
	( ((X2 = X1 + 1) & (Y2 = Y1 + 1)) |
	  ((X2 = X1 + 1) & (Y2 = Y1 - 1)) |
	  ((X2 = X1 - 1) & (Y2 = Y1 + 1)) |
	  ((X2 = X1 - 1) & (Y2 = Y1 - 1)) ) &
	not .member(pairPos(pos(X1,Y1), pos(X2,Y2)), TmpL) &
	not .member(pairPos(pos(X2,Y2), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y2))],TmpL,TmpL2) &
	diagonalPair(TmpL2,DL).

diagonalPair(TmpL,DL) :- DL = TmpL.


// Rules for two chips in vertical of the form X[]X with no chip in between
verticalTwoInThreePair([],VTL) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X1,Y2,0) &
	tablero(X1,Y3,M) &
	( ((Y3 = Y1 + 2) & (Y2 = Y1 + 1)) | 
	  ((Y3 = Y1 - 2) & (Y2 = Y1 - 1)) ) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y3))],[],TmpL) &
	verticalTwoInThreePair(TmpL,VTL).
	
verticalTwoInThreePair(TmpL,VTL) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X1,Y2,0) &
	tablero(X1,Y3,M) &
	( ((Y3 = Y1 + 2) & (Y2 = Y1 + 1)) | 
	  ((Y3 = Y1 - 2) & (Y2 = Y1 - 1)) ) &
	not .member(pairPos(pos(X1,Y1), pos(X1,Y3)), TmpL) &
	not .member(pairPos(pos(X1,Y3), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y3))], TmpL, TmpL2) &
	verticalTwoInThreePair(TmpL2,VTL).
	
verticalTwoInThreePair(TmpL,VTL) :- VTL = TmpL.


// Rules for two chips in horizontal of the form X[]X with no chip in between 
horizontalTwoInThreePair([],HTL) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y1,0) &
	tablero(X3,Y1,M) &
	( ((X3 = X1 + 2) & (X2 = X1 + 1)) | 
	  ((X3 = X1 - 2) & (X2 = X1 - 1)) ) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y1))],[],TmpL) &
	horizontalTwoInThreePair(TmpL,HTL).
	
horizontalTwoInThreePair(TmpL,HTL) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y1,0) &
	tablero(X3,Y1,M) &
	( ((X3 = X1 + 2) & (X2 = X1 + 1)) | 
	  ((X3 = X1 - 2) & (X2 = X1 - 1)) ) &
	not .member(pairPos(pos(X1,Y1), pos(X3,Y1)), TmpL) &
	not .member(pairPos(pos(X3,Y1), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y1))], TmpL, TmpL2) &
	horizontalTwoInThreePair(TmpL2,HTL).
	
horizontalTwoInThreePair(TmpL,HTL) :- HTL = TmpL.


// Rules for two chips in diagonal of the form X[]X with no chip in between 
diagonalTwoInThreePair([],DTL) :-
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
	  	(Y3 = Y1 - 2) & (Y2 = Y1 - 1) ) ) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y3))],[],TmpL) &
	diagonalTwoInThreePair(TmpL,DTL).
	 
diagonalTwoInThreePair(TmpL,DTL) :-
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
	  	(Y3 = Y1 - 2) & (Y2 = Y1 - 1) ) ) &
	not .member(pairPos(pos(X1,Y1), pos(X3,Y3)), TmpL) &
	not .member(pairPos(pos(X3,Y3), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y3))], TmpL, TmpL2) &
	diagonalTwoInThreePair(TmpL2,DTL).
	
diagonalTwoInThreePair(TmpL,DTL) :- DTL = TmpL.


// Rules for two chips in vertical of the form X[][]X with no chips in between
verticalTwoInFourPair([],VTL) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X1,Y2,0) &
	tablero(X1,Y3,0) &
	tablero(X1,Y4,M) &
	( ((Y4 = Y1 + 3) & (Y3 = Y1 + 2) & (Y2 = Y1 + 1)) | 
	  ((Y4 = Y1 - 3) & (Y3 = Y1 - 2) & (Y2 = Y1 - 1)) ) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y4))],[],TmpL) &
	verticalTwoInFourPair(TmpL,VTL).
	
verticalTwoInFourPair(TmpL,VTL) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X1,Y2,0) &
	tablero(X1,Y3,0) &
	tablero(X1,Y4,M) &
	( ((Y4 = Y1 + 3) & (Y3 = Y1 + 2) & (Y2 = Y1 + 1)) | 
	  ((Y4 = Y1 - 3) & (Y3 = Y1 - 2) & (Y2 = Y1 - 1)) ) &
	not .member(pairPos(pos(X1,Y1), pos(X1,Y4)), TmpL) &
	not .member(pairPos(pos(X1,Y4), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y4))], TmpL, TmpL2) &
	verticalTwoInFourPair(TmpL2,VTL).
	
verticalTwoInFourPair(TmpL,VTL) :- VTL = TmpL.


// Rules for two chips in horizontal of the form X[][]X with no chips in between 
horizontalTwoInFourPair([],HTL) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y1,0) &
	tablero(X3,Y1,0) &
	tablero(X4,Y1,M) &
	( ((X4 = X1 + 3) & (X3 = X1 + 2) & (X2 = X1 + 1)) | 
	  ((X4 = X1 - 3) & (X3 = X1 - 2) & (X2 = X1 - 1)) ) &
	.concat([pairPos(pos(X1,Y1), pos(X4,Y1))],[],TmpL) &
	horizontalTwoInFourPair(TmpL,HTL).
	
horizontalTwoInFourPair(TmpL,HTL) :-
	player(M) &
	tablero(X1,Y1,M) &
	tablero(X2,Y1,0) &
	tablero(X3,Y1,0) &
	tablero(X4,Y1,M) &
	( ((X4 = X1 + 3) & (X3 = X1 + 2) & (X2 = X1 + 1)) | 
	  ((X4 = X1 - 3) & (X3 = X1 - 2) & (X2 = X1 - 1)) ) &
	not .member(pairPos(pos(X1,Y1), pos(X4,Y1)), TmpL) &
	not .member(pairPos(pos(X4,Y1), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X4,Y1))], TmpL, TmpL2) &
	horizontalTwoInFourPair(TmpL2,HTL).
	
horizontalTwoInFourPair(TmpL,HTL) :- HTL = TmpL.


// Rules for two chips in diagonal of the form X[][]X with no chips in between 
diagonalTwoInFourPair([],DTL) :-
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
	    (Y4 = Y1 - 3) & (Y3 = Y1 - 2) & (Y2 = Y1 - 1) ) ) &
	.concat([pairPos(pos(X1,Y1), pos(X4,Y4))],[],TmpL) &
	diagonalTwoInFourPair(TmpL,DTL).
	 
diagonalTwoInFourPair(TmpL,DTL) :-
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
	    (Y4 = Y1 - 3) & (Y3 = Y1 - 2) & (Y2 = Y1 - 1) ) ) &
	not .member(pairPos(pos(X1,Y1), pos(X4,Y4)), TmpL) &
	not .member(pairPos(pos(X4,Y4), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X4,Y4))], TmpL, TmpL2) &
	diagonalTwoInFourPair(TmpL2,DTL).
	
diagonalTwoInFourPair(TmpL,DTL) :- DTL = TmpL.


/* Initial goals */


!play.



/* Plans */

// PLAY TO WIN
+!play:
	playerNumber(X) &
	estrategia(jugarAGanar) <-
		.print("A ganar");
		!playToTest;
		!playToWin.

// PLAY TO LOSE
+!play:
	estrategia(jugarAPerder) <-
		.print("A perder");
		!playToLose.	
		
////////////////////////////////////////////////////////////////
//////////////////////TESTING AREA/////////////////////////////
////////////////////////////////////////////////////////////////

// Plan to play a few rounds and generate a board's state to test
+!playToTest:
	testPut(X,Y) &
	turno(player1) <- 
		put(X,Y);
		-testPut(X,Y);
		-+movement(N+1);
		!playToTest.

+!playToTest:
	not testPut(X,Y) <-
		.print("Acabe.").
		//?twoInThreePairs(L);
		//print("Pair list of two in three: ", L);
		//?twoInFourPairs(L2);
		//.print("Pair list of two in four: ", L2).

+!playToTest <- !playToTest.		
////////////////////////////////////////////////////////////////
//////////////////////WINNING PLAN//////////////////////////////
////////////////////////////////////////////////////////////////
//+!playToWin:      
//	turno(N) &
//	myself(N) &
//	movement(M) &
//	M = 0<-
//		?estrategia(E);
//		!decideMovement(X,Y,E);
//		put(X,Y,E).

//+!playToWin:
//	turno(N) &
//	myself(N) &
//	movement(M) &
//	M > 0<-
//		?estrategia(E);
//		!checkHisMovement(X1,Y1);
//		!generateBoardsAround(X1,Y1,LB);
//		!playBoards(LB,E);
//		!decideMovement(X2,Y2,E);
//		put(X2,Y2).


////////////////////////////////////////////////////////////////
//////////////////////LOSING PLAN//////////////////////////////
////////////////////////////////////////////////////////////////




////////////////////////////////////////////////////////////////
///////////////////////////ERRORS//////////////////////////////
////////////////////////////////////////////////////////////////
+!play <- .print("Error in !play").
+!playToWin <- .print("Error in !playToWin").
+!playToLose <- .print("Error in !playToLose").
+!playToTest <- .print("Error or finish !playToTest").
