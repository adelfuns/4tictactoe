// Agent player1 in project cuatroenraya.mas2j



/* Initial beliefs and rules */
movimiento(0).

pares(LPares) :-
	paresVertical(LV) &
	recuperarConocimiento(LV) &
	paresHorizontal(LH) &
	recuperarConocimiento(LH) &
	paresDiagonal(LD) &
	recuperarConocimiento(LD) &
	.concat(LV,LH,LT) &
	.concat(LT,LD,LPares).	

paresVertical([parPos(pos(X1,Y1), pos(X1,Y2)) | Tail]):-
	tablero(X1,Y1,1) &
	tablero(X1,Y2,1) &
	( 	(Y2 = Y1 + 1)  |
		(Y2 = Y1 - 1)	) &
	.abolish(tablero(X1,Y1,1)) &
	.abolish(tablero(X1,Y2,1)) &
	paresVertical(Tail).

paresVertical([]).


paresHorizontal([parPos(pos(X1,Y1), pos(X1,Y2)) | Tail]):-
	tablero(X1,Y1,1) &
	tablero(X2,Y1,1) &
	( 	(X2 = X1 + 1)  |
		(X2 = X1 - 1)	) &
	.abolish(tablero(X1,Y1,1)) &
	.abolish(tablero(X2,Y1,1)) &
	paresVertical(Tail).

paresHorizontal([]).


paresDiagonal([parPos(pos(X1,Y1), pos(X1,Y2)) | Tail]):-
	tablero(X1,Y1,1) &
	tablero(X2,Y2,1) &
	(	( 	(X2 = X1 + 1)  &
			(Y2 = Y1 - 1)	)	|
			
		( 	(X2 = X1 - 1)	&
			(Y2 = Y1 - 1)	)	|
			
		( 	(X2 = X1 - 1)  &
			(Y2 = Y1 + 1)	)	|
			
		( 	(X2 = X1 + 1)  &
			(Y2 = Y1 + 1)	)	) &
	.abolish(tablero(X1,Y1,1)) &
	.abolish(tablero(X2,Y2,1)) &
	paresVertical(Tail).

paresDiagonal([]).



recuperarConocimiento([parPos(pos(X1,Y1), pos(X2,Y2))| Tail]):-
	.asserta(tablero(X1,Y1,1)) &
	.asserta(tablero(X2,Y2,1)).	
recuperarConocimiento([]).

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
+!playToWin: turno(player1) & movimiento(N) & N = 0<- 
	put(1,1);
	-+movimiento(N+2);
	!playToWin.
+!playToWin: turno(player1) & movimiento(N) & N = 2<- 
	put(1,2);
	-+movimiento(N+2);
	!playToWin.
+!playToWin: turno(player1) & movimiento(N) & N = 4<- 
	put(1,4);
	-+movimiento(N+2);
	!playToWin.
+!playToWin: turno(player1) & movimiento(N) & N = 6<- 
	put(1,5);
	-+movimiento(N+2);
	!playToWin.
+!playToWin: turno(player1) & movimiento(N) & N = 8<- 
	put(0,6);
	-+movimiento(N+2);
	!playToWin.
+!playToWin: turno(player1) & movimiento(N) & N = 10<- 
	put(0,5);
	-+movimiento(N+2);
	!playToWin.
+!playToWin: turno(player1) & movimiento(N) & N = 12<- 
	put(1,6);
	-+movimiento(N+2);
	!playToWin.	
+!playToWin: movimiento(N) & N = 14<-
		?pares(L);
		.print(L).
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


