// Agent player2 in project cuatroenraya.mas2j


                                                           
/* Initial beliefs and rules */
movimiento(1).


/* Initial goals */



//!start.

// Agent player1 in project cuatroenraya.mas2j



/* Initial beliefs and rules */
movement(0).

pares(LPares) :-
	paresVertical(LV) &
//	paresHorizontal(LH) &
//	paresDiagonal(LD) &
//	.concat(LV,LH,LT) &
//	.concat(LT,LD,LPares).
	.concat(LV,[],LPares).

paresVertical(LV):-
	tablero(X1,Y1,P) &
	tablero(X1,Y2,P) &
	( 	(Y2 = Y1 + 1)  |
		(Y2 = Y1 - 1)	) &
	not .member(parPos(pos(X1,Y1), pos(X1,Y2)),
		LV) &
	not .member(parPos(pos(X1,Y2), pos(X1,Y1)),
		LV) &
	paresVertical([parPos(pos(X1,Y1), pos(X1,Y2)) | LV]).
	
myself(X) :- 
	.my_name(X).
/* Initial goals */



!play.



/* Plans */

+!play:
	estrategia(jugarAGanar) <-
		.print("A ganar");
		!playToWin.

+!play:
	estrategia(jugarAPerder) <-
		.print("A perder");
		!playToLose.	
		
////////////////////////////////////////////////////////////////
//////////////////////winning plan//////////////////////////
////////////////////////////////////////////////////////////////

+!playToWin: turno(player2) & movimiento(N) & N = 1<- 
	put(2,1);
	-+movimiento(N+2);
	!playToWin.
+!playToWin: turno(player2) & movimiento(N) & N = 3<- 
	put(2,2);
	-+movimiento(N+2);
	!playToWin.
+!playToWin: turno(player2) & movimiento(N) & N = 5<- 
	put(2,4);
	-+movimiento(N+2);
	!playToWin.
+!playToWin: turno(player2) & movimiento(N) & N = 7<- 
	put(2,7);
	-+movimiento(N+2);
	!playToWin.
+!playToWin: turno(player2) & movimiento(N) & N = 9<- 
	put(3,4);
	-+movimiento(N+2);
	!playToWin.
+!playToWin: turno(player2) & movimiento(N) & N = 11<- 
	put(5,5);
	-+movimiento(N+2);
	!playToWin.
+!playToWin: turno(player2) & movimiento(N) & N = 13<- 
	put(5,6);
	-+movimiento(N+2);
	!playToWin.
+!playToWin <- !playToWin.
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
///////////////////////////errors//////////////////////////////
////////////////////////////////////////////////////////////////
+!play <- .print("Error").
+!playToWin <- .print("Error").
+!playToLose <- .print("Error").



/* Plans */



//+!start : true <- .print("hello world.").


