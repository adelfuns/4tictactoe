// Agent player1 in project cuatroenraya.mas2j

/* Initial beliefs and rules */
movement(0).


// Forms a list of all pairs of chips in the board
pairs(PL) :-
	verticalPair([], VL) &
	horizontalPair([], HL) &
	diagonalPair([], DL) &
	.concat(VL, HL, TmpL) &
	.concat(TmpL, DL, PL).	

	
// Rules for vertical pairs	
verticalPair([],LV) :-
	tablero(X1,Y1,1) &
	tablero(X1,Y2,1) &
	( 	(Y2 = Y1 + 1)  |
		(Y2 = Y1 - 1)	) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y2))],[],TmpL) &
	verticalPair(TmpL,LV).

verticalPair(TmpL,LV) :-
	tablero(X1,Y1,1) &
	tablero(X1,Y2,1) &
	( 	(Y2 = Y1 + 1)  |
		(Y2 = Y1 - 1)	) &
	not .member(pairPos(pos(X1,Y1), pos(X1,Y2)), TmpL) &
	not .member(pairPos(pos(X1,Y2), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y2))],TmpL,TmpL2) &
	verticalPair(TmpL2,LV).

verticalPair(TmpL,LV) :- LV = TmpL.


// Rules for horizontal pairs
horizontalPair([],HL) :-
	tablero(X1,Y1,1) &
	tablero(X2,Y1,1) &
	( 	(X2 = X1 + 1) |
		(X2 = X1 - 1)	) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y1))],[],TmpL) &
	horizontalPair(TmpL,HL).

horizontalPair(TmpL,HL) :-
	tablero(X1,Y1,1) &
	tablero(X2,Y1,1) &
	( (X2 = X1 + 1) |
	  (X2 = X1 - 1) ) &
	not .member(pairPos(pos(X1,Y1), pos(X2,Y1)), TmpL) &
	not .member(pairPos(pos(X2,Y1), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y1))], TmpL, TmpL2) &
	horizontalPair(TmpL2,HL).

horizontalPair(TmpL,HL) :- HL = TmpL.


// Rules for diagonal pairs
diagonalPair([],DL) :-
	tablero(X1,Y1,1) &
	tablero(X2,Y2,1) &
	( ((X2 = X1 + 1) & (Y2 = Y1 + 1)) |
	  ((X2 = X1 + 1) & (Y2 = Y1 - 1)) |
	  ((X2 = X1 - 1) & (Y2 = Y1 + 1)) |
	  ((X2 = X1 - 1) & (Y2 = Y1 - 1)) ) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y2))],[],TmpL) &
	diagonalPair(TmpL,DL).

diagonalPair(TmpL,DL) :-
	tablero(X1,Y1,1) &
	tablero(X2,Y2,1) &
	( ((X2 = X1 + 1) & (Y2 = Y1 + 1)) |
	  ((X2 = X1 + 1) & (Y2 = Y1 - 1)) |
	  ((X2 = X1 - 1) & (Y2 = Y1 + 1)) |
	  ((X2 = X1 - 1) & (Y2 = Y1 - 1)) ) &
	not .member(pairPos(pos(X1,Y1), pos(X2,Y2)), TmpL) &
	not .member(pairPos(pos(X2,Y2), pos(X1,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y2))],TmpL,TmpL2) &
	diagonalPair(TmpL2,DL).

diagonalPair(TmpL,DL) :- DL = TmpL.


// To know your own name
myself(X) :- 
	.my_name(X).


/* Initial goals */



!play.



/* Plans */

// PLAY TO WIN
+!play:
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
//////////////////////TESTING AREA//////////////////////////////
////////////////////////////////////////////////////////////////

// Plan to play a few rounds and generate a board's state to test
+!playToTest:
	turno(player1) &
	movement(N) &
	N = 0 <- 
		put(1,1);
		-+movement(N+2);
		!playToTest.
+!playToTest:
	turno(player1) &
	movement(N) &
	N = 2 <- 
		put(1,2);
		-+movement(N+2);
		!playToTest.
+!playToTest:
	turno(player1) &
	movement(N) &
	N = 4 <- 
		put(1,4);
		-+movement(N+2);
		!playToTest.
+!playToTest:
	turno(player1) &
	movement(N) &
	N = 6 <- 
		put(1,5);
		-+movement(N+2);
		!playToTest.
+!playToTest:
	turno(player1) &
	movement(N) &
	N = 8 <- 
		put(0,6);
		-+movement(N+2);
		!playToTest.
+!playToTest:
	turno(player1) &
	movement(N) &
	N = 10<- 
		put(0,5);
		-+movement(N+2);
		!playToTest.
+!playToTest:
	turno(player1) &
	movement(N) &
	N = 12<- 
		put(1,6);
		-+movement(N+2);
		!playToTest.	
+!playToTest:
	movement(N) &
	N = 14 <-
		?pairs(L);
		.print("Pair list: ",L).

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

