// Agent player2 in project cuatroenraya.mas2j

/* Initial beliefs and rules */



// Gets player's number and adds it to the beliefs (alongside the opponent's number) 
playerNumber :- 
	.my_name(N) &
	.term2string(N,S) &
	.length(S,M) &
	.substring(S,X,(M-1)) &
	.term2string(P,X) &
	.asserta(player(P)) &
	rivalNumber(P,R) &
	.asserta(rival(R)).

// Gets opponent's number
rivalNumber(X,2):-
	X = 1 &
	.asserta(movement(0)).

rivalNumber(X,1):-
	X = 2 &
	.asserta(movement(1)).



// if player moves second, chooses the closest cell between rival's movement and the center
// in case the rival puts their first chip on a corner
closerToCenter(X,Y):-
	jugadaActual(pos(XR,YR)) &
	XR = 0 & YR = 0 &
	X = XR+2 & Y = YR+2.

closerToCenter(X,Y):-
	jugadaActual(pos(XR,YR)) &
	XR = 0 & YR = 7 &
	X = XR+2 & Y = YR-2.

closerToCenter(X,Y):-
	jugadaActual(pos(XR,YR)) &
	XR = 7 & YR = 0 &
	X = XR-2 & Y = YR+2.

closerToCenter(X,Y):-
	jugadaActual(pos(XR,YR)) &
	XR = 7 & YR = 7 &
	X = XR-2 & Y = YR-2.

// other cases
closerToCenter(X,Y):-
	jugadaActual(pos(XR,YR)) &
	XR <= 3 & YR <= 3 &
	X = XR+1 & Y = YR+1.

closerToCenter(X,Y):-
	jugadaActual(pos(XR,YR)) &
	XR <= 3 & YR >= 3 &
	X = XR+1 & Y = YR-1.

closerToCenter(X,Y):-
	jugadaActual(pos(XR,YR)) &
	XR >= 3 & YR <= 3 &
	X = XR-1 & Y = YR+1.

closerToCenter(X,Y):-
	jugadaActual(pos(XR,YR)) &
	XR >= 3 & YR >= 3 &
	X = XR-1 & Y = YR-1.



// Rules to decide next move in winning game
// NEXT MOVE WIN
nextMove(X, Y):-
	player(P) &
	winnerPlayer([pos(X,Y)|_]).

// NEXT MOVE LOSE
nextMove(X, Y):-
	player(P) &
	rival(R) &
	winnerRival([pos(X,Y)|_]).

	
// NEXT MOVE CHECK
nextMove(X, Y):-
	rival(R) &
	winnerRival([pos(X,Y)]).


// NEXT MOVE NEXT MOVE WIN
nextMove(X,Y):-
	winningTrioPlayer([pos(X,Y)|_]).


nextMove(X,Y):-
	winningTrioTPlayer([pos(X,Y)|_]).



// NEXT MOVE RIVAL NEXT MOVE WIN
nextMove(X,Y):-
	rival(R) &
	player(P) &
	blockForcePlayer(BFLP) &
	.length(BFLP, 0) &
	winningTrioRival([pos(X,Y)|_]).


nextMove(X,Y):-
	rival(R) &
	player(P) &
	blockForcePlayer(BFLP) &
	.length(BFLP, 0) &
	winningTrioTRival([pos(X,Y)|_]).


// NEXT MOVE FORCE BLOCK

// FORCE BLOCK NEXT MOVE WIN
nextMove(X,Y):-
	player(P) &
	blockForcePlayer(BFLP) &
	not .length(BFLP, 0) &
	.sort(BFLP, L) &
	checkRepeat(L, X, Y).



// FORCE BLOCK RIVAL NEXT MOVE WIN
nextMove(X,Y):-
	player(P) &
	blockForcePlayer([pos(X,Y)|_]) &


nextMove(X,Y):-
	player(P) &
	blockForcePlayer(BFLP) &
	not .length(BFLP, 0) &
	bestMoveBlock(BFLP, X, Y, P).


// NEXT MOVE NADA M�?S
nextMove(X,Y):-
	player(P) &
	tablero(X,Y,0)[source(percept)] &
	utilPos(X, Y, P).

	
// rules to determinate when a pair can become a winning trio
winningTrio([],[],_).

winningTrio([pairPos(pos(X1,Y1),pos(X2,Y2))|L], [pos(X,Y)|TL],P):-
	triple(X1,Y1,X2,Y2,X,Y,P) &
	winningTrio(L,TL,P).

winningTrio([_|L], TL,P):-
	winningTrio(L,TL,P).

winningTrio([], L, _).

// checks what type of pair form (X1,Y1) and (X2,Y2) and if it forms a winning trio
triple(X1,Y1,X2,Y2,X,Y,P):-
	tripleVertical(X1,Y1,X2,Y2,X,Y,P) |
	tripleHorizontal(X1,Y1,X2,Y2,X,Y,P) |
	tripleDiagonal(X1,Y1,X2,Y2,X,Y,P).

tripleVertical(X,Y1, X,Y2, X,YD,P):-
	vertical(X,Y1,X,Y2,P) &
	tablero(X,YD,0)[source(percept)] &
	tablero(X,YD1,0)[source(percept)] &
	tablero(X,YD0,0)[source(percept)] &
	((YD = Y2+1 &
	YD1 = Y2+2 &
	YD0 = Y2-2) |
	(YD = Y2-2 &
	YD1 = Y2-3 &
	YD0 = Y2+1)).

tripleHorizontal(X1,Y, X2,Y, XD,Y,P):-
	vertical(X1,Y,X2,Y,P) &
	tablero(XD,Y,0)[source(percept)] &
	tablero(XD1,Y,0)[source(percept)] &
	tablero(XD0,Y,0)[source(percept)] &
	((XD = X2+1  &
	XD1 = X2+2 &
	XD0 = X2-2) |
	(XD = X2-2  &
	XD1 = X2-3 &
	XD0 = X2+1)).

tripleDiagonal(X1,Y1,X2,Y2,XD,YD,P):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(XD,YD,0)[source(percept)] &
	tablero(XD1,YD,0)[source(percept)] &
	tablero(XD0,YD,0)[source(percept)] &
	(((X2 = X1+1 & Y2 = Y1+1) & 
	((XD = X2+1 & YD= Y2+1 & 
	XD1 = X2+2 & YD1 = Y2+2 & 
	XD0 = X2-2 & YD0 = Y2-2) |  
	(XD = X2-2 & YD= Y2-2 & 
	XD1 = X2-3 & YD1 = Y2-3 & 
	XD0 = X2+1 & YD0 = Y2+1))) |
	((X2 = X1-1 & Y2 = Y1-1) & 
	((XD = X2-1 & YD= Y2+1 & 
	XD1 = X2-2 & YD1 = Y2+2 & 
	XD0 = X2+2 & YD0 = Y2-2) |
	(XD = X2+2 & YD= Y2-2 & 
	XD1 = X2+3 & YD1 = Y2-3 & 
	XD0 = X2-1 & YD0 = Y2+1)))).


// rules to determinate when a two in three can become a winning trio
winningTrioTinT([],[],_).

winningTrioTinT([pairPos(pos(X1,Y1),pos(X2,Y2))|L], [pos(X,Y)|TL], P):-
	tripleTinT(X1,Y1,X2,Y2,X,Y,P).

winningTrioTinT([_|L], TL, P):-
	winningTrioTinT(L, TL ,P).

winningTrioTinT([], L, _).

// checks what type of two in three form (X1,Y1) and (X2,Y2) and if it forms a winning trio
tripleTinT(X1,Y1,X2,Y2,X,Y,P):-
	tripleVerticalTinT(X1,Y1,X2,Y2,X,Y,P) |
	tripleHorizontalTinT(X1,Y1,X2,Y2,X,Y,P) |
	tripleDiagonalTinT(X1,Y1,X2,Y2,X,Y,P).

tripleVerticalTinT(X,Y1, X,Y2, X,YD, P):-
	verticalTwoInThree(X,Y1,X,Y2,P) &
	tablero(X,YD,0)[source(percept)] &
	tablero(X,Y0,0)[source(percept)] &
	tablero(X,Y4,0)[source(percept)] &
	YD = Y1+1 &
	Y0 = Y1-1 &
	Y4 = Y1+3.

tripleHorizontalTinT(X1,Y, X2,Y, XD,Y,P):-
	horizontalTwoInThree(X1,Y,X2,Y,P) &
	tablero(XD,Y,0)[source(percept)] &
	tablero(X0,Y,0)[source(percept)] &
	tablero(X4,Y,0)[source(percept)] &
	XD = X1+1 &
	X0 = X1-1 &
	X4 = X1+3.

tripleDiagonalTinT(X1,Y1,X2,Y2,XD,YD,P):-
	diagonalTwoInThree(X1,Y1,X2,Y2,P) &
	tablero(XD,YD,0)[source(percept)] &
	tablero(X0,Y0,0)[source(percept)] &
	tablero(X4,Y4,0)[source(percept)] &
	((XD = X1+1 & YD = Y1+1 &
	X0 = X1-1 & Y0 = Y1-1 &
	X4 = X1+3 & Y4 = Y1+3) |
	(XD = X1-1 & YD = Y1+1 &
	X0 = X1+1 & Y0 = Y1-1 &
	X4 = X1-3 & Y4 = Y1+3)).



// lists all not winning trios (forces to block)
allNotWinningTrio(P,R, L):-
	pairs(PL, P) &
	notWinningTrio(PL, PBL, P, R) &
	twoInThreePairs(TL, P) &
	notWinningTrioTinT(TL, TBL, P, R) &
	twoInFourPairs(FL, P) &
	notWinningTrioTinF(FL, FBL, P, R) &
	.concat(PBL, TBL, TempL) &
	.concat(TempL, FBL, L).
	

// rules to determinate when a pair can become a not winning trio (forces to block)
notWinningTrio([],[],_,_).

notWinningTrio([pairPos(pos(X1,Y1),pos(X2,Y2))|PL], [pos(X,Y),pos(X0,Y0)|L], P, R):-
	notWtriple(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R) &
	notWinningTrio(PL, L, P, R).

notWinningTrio([_|PL], L, P, R):-
	notWinningTrio(PL, L, P, R).

notWinningTrio([], L, _, _).

// checks what type of pair form (X1,Y1) and (X2,Y2) and if it forms a not winning trio
notWtriple(X1,Y1,X2,Y2, X,Y, X0,Y0, P, R):-
	notWClosePair(X1,Y1,X2,Y2, X,Y, X0,Y0, P, R) |
	notWOpenPair(X1,Y1,X2,Y2, X,Y, X0,Y0, P, R).

notWClosePair(X1,Y, X2,Y, X,Y, X4,Y, P, R) :-
	horizontal(X1,Y, X2,Y, P) &
	tablero(XE,Y1,R)[source(percept)] &
	tablero(X, Y, 0)[source(percept)] &
	tablero(X4, Y, 0)[source(percept)] &
	//RIGHT TO LEFT
	((X = X2+1 &
	XE = X2-2 &
	X4 = X2+2) |
	//LEFT TO RIGHT
	(X = X2-2 &
	XE = X2+1 &
	X4 = X2-3)).

notWClosePair(X,Y1, X,Y2, X,Y, X,Y4, P, R) :-
	vertical(X,Y1, X,Y2, P) &
	tablero(X,YE,R)[source(percept)] &
	tablero(X, Y, 0)[source(percept)] &
	tablero(X, Y4, 0)[source(percept)] &
	//TOP TO BOTTOM
	((Y = Y2+1 &
	YE = Y2-2 &
	Y4 = Y2+2) |
	//BOTTOM TO TOP
	(Y = Y2-2 &
	YE = Y2+1 &
	Y4 = Y2-3)).

notWClosePair(X1,Y1, X2,Y2, X,Y, X4,Y4, P, R) :-
	diagonal(X1,Y1, X2,Y2, P) &
	tablero(XE,YE,R)[source(percept)] &
	tablero(X, Y, 0)[source(percept)] &
	tablero(X4, Y4, 0)[source(percept)] &
	// TOP LEFT
	((X2 = X1+1 & Y2 = Y1+1 &
	XE = X1+2 & YE = Y1+2 &
	X = X1-1 & Y = Y1-1 &
	X4 = X1-2 & Y4 = Y1-2) | 
	// BOTTOM RIGHT
	(X2 = X1+1 & Y2 = Y1+1 &
	XE = X1-1 & YE = Y1-1 &
	X = X1+2 & Y = Y1+2 &
	X4 = X1+3 & Y4 = Y1+3) |
	// TOP RIGHT
	(X2 = X1-1 & Y2 = Y1+1 &
	XE = X1-2 & YE = Y1+2 &
	X = X1+1 & Y = Y1-1 &
	X4 = X1+2 & Y4 = Y1-2) |
	// BOTTOM LEFT
	(X2 = X1-1 & Y2 = Y1+1 &
	XE = X1+1 & YE = Y1-1 &
	X = X1-2 & Y = Y1+2 &
	X4 = X1-3 & Y4 = Y1+3)).
	

notWOpenPair(X1,Y, X2,Y, X,Y, X4,Y, P, R) :-
	horizontal(X1,Y, X2,Y, P) &
	tablero(XE,Y,R)[source(percept)] &
	tablero(X, Y, 0)[source(percept)] &
	tablero(X4, Y, 0)[source(percept)] &
	//RIGHT TO LEFT
	((X = X2-2 &
	XE = X2-3 &
	X4 = X2+1) |
	//LEFT TO RIGHT
	(X = X2+1 &
	XE = X2+2 &
	X4 = X2-2)).

notWOpenPair(X,Y1, X,Y2, X,Y, X,Y4, P, R) :- 
	vertical(X,Y1, X,Y2, P) &
	tablero(X,YE,R)[source(percept)] &
	tablero(X, Y, 0)[source(percept)] &
	tablero(X, Y4, 0)[source(percept)] &
	//TOP TO BOTTOM
	((Y = Y2-2 &
	YE = Y2-3 &
	Y4 = Y2+1) |
	//BOTTOM TO TOP
	(Y = Y2+1 &
	YE = Y2+2 &
	Y4 = Y2-2)).

notWOpenPair(X1,Y1, X2,Y2, X,Y, X4,Y4, P, R) :-
	diagonal(X1,Y1, X2,Y2, P) &
	tablero(XE,YE,R)[source(percept)] &
	tablero(X, Y, 0)[source(percept)] &
	tablero(X4, Y4, 0)[source(percept)] &
	// TOP LEFT
	((X2 = X1+1 & Y2 = Y1+1 &
	XE = X1-2 & YE = Y1-2 &
	X = X1-1 & Y = Y1-1 &
	X4 = X1+2 & Y4 = Y1+2) | 
	// BOTTOM RIGHT
	(X2 = X1+1 & Y2 = Y1+1 &
	XE = X1+3 & YE = Y1+3 &
	X = X1+2 & Y = Y1+2 &
	X4 = X1-1 & Y4 = Y1-1) |
	// TOP RIGHT
	(X2 = X1-1 & Y2 = Y1+1 &
	XE = X1+2 & YE = Y1-2 &
	X = X1+1 & Y = Y1-1 &
	X4 = X1-3 & Y4 = Y1+3) |
	// BOTTOM LEFT
	(X2 = X1-1 & Y2 = Y1+1 &
	XE = X1-3 & YE = Y1+3 &
	X = X1-2 & Y = Y1+2 &
	X4 = X1+1 & Y4 = Y1-1)).


// rules to determinate when a two in three can become a not winning trio (forces to block)
notWinningTrioTinT([],[],_, _).

notWinningTrioTinT([pairPos(pos(X1,Y1),pos(X2,Y2))|PL], [pos(X,Y),pos(X0,Y0)|L], P, R):-
	notWtripleTinT(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R) &
	notWinningTrioTinT(PL, L, P, R).

notWinningTrioTinT([_|PL], L, P, R):-
	notWinningTrioTinT(PL, L, P, R).

notWinningTrioTinT([], L, _, _).

// checks what type of two in three form (X1,Y1) and (X2,Y2) and if it forms a not winning trio
notWtripleTinT(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R):-
	notWVerticalTinT(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R) |
	notWHorizontalTinT(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R) |
	notWDiagonalTinT(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R).

notWVerticalTinT(X,Y1, X,Y2, X,Y, X,Y0, P, R):-
	verticalTwoInThree(X,Y1, X,Y2, P) &
	tablero(X, Y0, 0)[source(percept)] &
	tablero(X, YR, R)[source(percept)] &
	//TOP
	((YR = Y1-1 &
	Y0 = Y1+3 &
	Y = Y1+1) |
	//BOTTOM
	(YR = Y1+3 &
	Y0 = Y1-1 &
	Y = Y1+1)).

notWHorizontalTinT(X1,Y, X2,Y, X,Y, X0,Y, P, R):-
	horizontalTwoInThree(X1,Y, X2,Y, P) &
	tablero(X0, Y, 0)[source(percept)] &
	tablero(XR, Y, R)[source(percept)] &
	//LEFT
	((XR = X1-1 &
	X0 = X1+3 &
	X = X1+1) |
	//RIGHT
	(XR = X1+3 &
	X0 = X1-1 &
	X = X1+1)).

notWDiagonalTinT(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R):-
	diagonalTwoInThree(X1,Y1, X2,Y2, P) &
	tablero(X0, Y0, 0)[source(percept)] &
	tablero(XR, YR, R)[source(percept)] &
	//TOP LEFT
	((XR = X1-1 & YR = Y1-1 &
	X0 = X1+3 & Y0 = Y0+3 &
	X = X1+1  & Y = Y1+1) |
	//TOP RIGHT
	(XR = X1+1 & YR = Y1-1 &
	X0 = X1-3 & Y0 = Y0+3 &
	X = X1-1  & Y = Y1+1) |
	//BOTTOM LEFT
	(XR = X1-3 & YR = Y1+3 &
	X0 = X1+1 & Y0 = Y0-1 &
	X = X1-1  & Y = Y1+1) |
	//BOTTOM RIGHT
	(XR = X1+3 & YR = Y1+3 &
	X0 = X1-1 & X0 = X0-1 &
	X = X1+1  & Y = Y1+1)).


// rules to determinate when a two in four can become a not winning trio (forces to block)
notWinningTrioTinF([],[],_, _).

notWinningTrioTinF([pairPos(pos(X1,Y1),pos(X2,Y2))|PL], [pos(X1,Y1),pos(X2,Y2)|L], P, R):-
	notWtripleTinF(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R) &
	notWinningTrioTinF(PL, L, P, R).

notWinningTrioTinF([_|PL], L, P, R):-
	notWinningTrioTinF(PL, L, P, R).

notWinningTrioTinF([], L, _, _).

// checks what type of two in four form (X1,Y1) and (X2,Y2) and if it forms a not winning trio
notWtriple(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R):-
	notWVerticalTinF(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R) |
	notWHorizontalTinF(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R) |
	notWDiagonalTinF(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R).

notWVerticalTinF(X,Y1, X,Y2, X,Y, X,Y0, P, R):-
	verticalTwoInFour(X,Y1, X,Y2, P) &
	tablero(X, YR, R)[source(percept)] &
	Y = Y1+1 &
	Y0 = Y1+2 &
	//TOP
	((YR = Y1-1) |
	//BOTTOM
	(YR = Y1+4)).

notWHorizontalTinT(X1,Y, X2,Y, X,Y, X0,Y, P, R):-
	horizontalTwoInFour(X1,Y, X2,Y, P) &
	tablero(XR, Y, R)[source(percept)] &
	X = X1+1 &
	X0 = X1+2 &
	//LEFT
	((XR = X1-1) |
	//RIGHT
	(XR = X1+4)).

notWDiagonalTinT(X1,Y1, X2,Y2, X,Y, X0,Y0, P, R):-
	diagonalTwoInFour(X1,Y1, X2,Y2, P) &
	tablero(XR, YR, R)[source(percept)] &
	//TOP LEFT
	((XR = X1-1 & YR = Y1-1 &
	X = X1+1  & Y = Y1+1 &
	X0 = X1+2 & Y0 = Y1+2) |
	//TOP RIGHT
	(XR = X1+1 & YR = Y1-1 &
	X = X1-1  & Y = Y1+1 &
	X0 = X1-2 & Y0 = Y1+2) |
	//BOTTOM LEFT
	(XR = X1-4 & YR = Y1+4 &
	X = X1-2  & Y = Y1+2 &
	X0 = X1-3 & Y0 = Y1+3) |
	//BOTTOM RIGHT
	(XR = X1+4 & YR = Y1+4 &
	X = X1+2  & Y = Y1+2 &
	X0 = X1+3 & Y0 = Y1+3)).


// rules to determinate best move
checkRepeat([pos(X,Y),pos(X,Y)], X, Y).

checkRepeat([pos(X,Y),pos(X2,Y2)], 8, 8):-
	not X = X2 & not Y = Y2.

checkRepeat([pos(X,Y)|TL], X, Y):-
	.member(pos(X,Y), TL).

checkRepeat([pos(X,Y)|TL], XD, YD):-
	not .member(pos(X,Y), TL) &
	checkRepeat(TL, XD,YD).


bestMoveBlock(L, X, Y, P):-
	.member(pos(X,Y), L) &
	utilpos(X,Y,P).


bestMoveBlock(L, X, Y, P). //FALTA HACER


bestMove(L, X, Y). //FALTA HACER




// checks if a position can create an util pair/two in three (winning move in the next two turns)
utilPos(X,Y,P):-
	utilPair(X,Y,P) |
	utilTinT(X,Y,P).


utilPair(X,Y,P):-
	utilPairTop(X,Y,P) |
	utilPairBottom(X,Y,P).


utilPairTop(X,Y,P):-
	utilPairVT(X,Y,P) |
	utilPairHT(X,Y,P) |
	utilPairDT(X,Y,P).

utilPairVT(X,Y,P):-
	tablero(X,Y-1,P)[source(percept)] &
	tablero(X,Y+1,0)[source(percept)] &
	tablero(X,Y-2,0)[source(percept)] &
	(tablero(X,Y+2,0)[source(percept)] |
	tablero(X,Y-3,0)[source(percept)]).

utilPairHT(X,Y,P):-
	tablero(X-1,Y,P)[source(percept)] &
	tablero(X+1,Y,0)[source(percept)] &
	tablero(X-2,Y,0)[source(percept)] &
	(tablero(X+2,Y,0)[source(percept)] |
	tablero(X-3,Y,0)[source(percept)]).

utilPairDT(X,Y,P):-
	utilPairDTLeft(X,Y,P) |
	utilPairDTRight(X,Y,P).

utilPairDTLeft(X,Y,P):-
	tablero(X-1,Y-1,P)[source(percept)] &
	tablero(X+1,Y+1,0)[source(percept)] &
	tablero(X-2,Y-2,0)[source(percept)] &
	(tablero(X+2,Y+2,0)[source(percept)] |
	tablero(X-3,Y-3,0)[source(percept)]).

utilPairDTRight(X,Y,P):-
	tablero(X+1,Y-1,P)[source(percept)] &
	tablero(X-1,Y+1,0)[source(percept)] &
	tablero(X+2,Y-2,0)[source(percept)] &
	(tablero(X-2,Y+2,0)[source(percept)] |
	tablero(X+3,Y-3,0)[source(percept)]).


utilPairBottom(X,Y,P):-
	utilPairVB(X,Y,P) |
	utilPairHB(X,Y,P) |
	utilPairDB(X,Y,P).

utilPairVB(X,Y,P):-
	tablero(X,Y+1,P)[source(percept)] &
	tablero(X,Y-1,0)[source(percept)] &
	tablero(X,Y+2,0)[source(percept)] &
	(tablero(X,Y+3,0)[source(percept)] |
	tablero(X,Y-2,0)[source(percept)]).

utilPairHB(X,Y,P):-
	tablero(X+1,Y,P)[source(percept)] &
	tablero(X-1,Y,0)[source(percept)] &
	tablero(X+2,Y,0)[source(percept)] &
	(tablero(X-2,Y,0)[source(percept)] |
	tablero(X+3,Y,0)[source(percept)]).

utilPairDB(X,Y,P):-
	utilPairDBLeft(X,Y,P) |
	utilPairDBRight(X,Y,P).

utilPairDBLeft(X,Y,P):-
	tablero(X+1,Y+1,P)[source(percept)] &
	tablero(X-1,Y-1,0)[source(percept)] &
	tablero(X+2,Y+2,0)[source(percept)] &
	(tablero(X-2,Y-2,0)[source(percept)] |
	tablero(X+3,Y+3,0)[source(percept)]).

utilPairDBRight(X,Y,P):-
	tablero(X-1,Y+1,P)[source(percept)] &
	tablero(X+1,Y-1,0)[source(percept)] &
	tablero(X-2,Y+2,0)[source(percept)] &
	(tablero(X+2,Y-2,0)[source(percept)] |
	tablero(X-3,Y+3,0)[source(percept)]).


utilTinT(X,Y,P):-
	utilTinTTop(X,Y,P) |
	utilTinTBottom(X,Y,P).


utilTinTTop(X,Y,P):-
	utilTinTVT(X,Y,P) |
	utilTinTHT(X,Y,P) |
	utilTinTDT(X,Y,P).

utilTinTVT(X,Y,P):-
	tablero(X,Y+2,P)[source(percept)] &
	tablero(X,Y+1,0)[source(percept)] &
	tablero(X,Y+3,0)[source(percept)] &
	tablero(X,Y-1,0)[source(percept)].

utilTinTHT(X,Y,P):-
	tablero(X+2,Y,P)[source(percept)] &
	tablero(X+1,Y,0)[source(percept)] &
	tablero(X+3,Y,0)[source(percept)] &
	tablero(X-1,Y,0)[source(percept)].

utilTinTDT(X,Y,P):-
	utilTinTDTLeft(X,Y,P) |
	utilTinTDTRight(X,Y,P).

utilTinTDTLeft(X,Y,P):-
	tablero(X+2,Y+2,P)[source(percept)] &
	tablero(X+1,Y+1,0)[source(percept)] &
	tablero(X+3,Y+3,0)[source(percept)] &
	tablero(X-1,Y-1,0)[source(percept)].

utilTinTDTRight(X,Y,P):-
	tablero(X-2,Y+2,P)[source(percept)] &
	tablero(X-1,Y+1,0)[source(percept)] &
	tablero(X-3,Y+3,0)[source(percept)] &
	tablero(X+1,Y-1,0)[source(percept)].


utilTinTBottom(X,Y,P):-
	utilPairVB(X,Y,P) |
	utilPairHB(X,Y,P) |
	utilPairDB(X,Y,P).

utilTinTVB(X,Y,P):-
	tablero(X,Y-2,P)[source(percept)] &
	tablero(X,Y-1,0)[source(percept)] &
	tablero(X,Y-3,0)[source(percept)] &
	tablero(X,Y+1,0)[source(percept)].

utilTinTHB(X,Y,P):-
	tablero(X-2,Y,P)[source(percept)] &
	tablero(X-1,Y,0)[source(percept)] &
	tablero(X-3,Y,0)[source(percept)] &
	tablero(X+1,Y,0)[source(percept)].

utilTinTDB(X,Y,P):-
	utilTinTDBLeft(X,Y,P) |
	utilTinTDBRight(X,Y,P).

utilTinTDBLeft(X,Y,P):-
	tablero(X-2,Y-2,P)[source(percept)] &
	tablero(X-1,Y-1,0)[source(percept)] &
	tablero(X-3,Y-3,0)[source(percept)] &
	tablero(X+1,Y+1,0)[source(percept)].

utilTinTDBRight(X,Y,P):-
	tablero(X+2,Y-2,P)[source(percept)] &
	tablero(X+1,Y-1,0)[source(percept)] &
	tablero(X+3,Y-3,0)[source(percept)] &
	tablero(X-1,Y+1,0)[source(percept)].



// generates all lists of pairs for a player
allLists(L,P):-
	pairs(PL, P) &
	twoInThreePairs(TL, P) &
	twoInFourPairs(FL, P) &
	winnerTotal(WL, P) &
	.concat(PL, TL, TempL) &
	.concat(FL, TempL, L).

// Rules to decide next move in losing game
nextMove(X, Y):-
	estrategia(jugarAPerder)[source(percept)].
	
nextMove(X, Y):-
	estrategia(jugarAPerder)[source(percept)].















// auto explanatory
checkEmpty(X,Y):-
	tablero(X,Y,0)[source(percept)].

// Forms a list of all winning positions in the board              
winnerTotal(L,P):-
	winnerVertical(L1,P) &
	winnerHorizontal(L2,P) & 
	winnerDiagonal(L3,P) &
	winnerVerticalThreeInFour(L4,P) &
	winnerHorizontalThreeInFour(L5,P) &
	winnerDiagonalThreeInFour(L6,P) &
	.union(L1,L2,LT1) &
	.union(L3,L4,LT2) &
	.union(L5,L6, LT3) &
	.union(LT1, LT2, LTemp) &
	.union(LTemp, LT3, L).


// Forms a list of all winning vertical positions in the board	
winnerVertical(L,P):-
	winnerVerticalPairTop(0,0,L1,P) &            
	winnerVerticalPairBottom(0,0,L2,P) &
	.concat(L1,L2,L).

// Top
winnerVerticalPairTop(8,Y1,[],P).

winnerVerticalPairTop(X1,Y1,[pos(X1,Y0)|WVL],P) :-
	Y1 < 7 &
	X1 < 8 &
	vertical(X1,Y1,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y0) & 
	(Y0 = Y1-1) &
	winnerVerticalPairTop(X1,Y1+1,WVL,P).

winnerVerticalPairTop(X1,Y1,WVL,P) :-
	Y1 < 7 &
	X1 < 8 &
	winnerVerticalPairTop(X1,Y1+1,WVL,P).

winnerVerticalPairTop(X1,7,WVL,P) :-
	winnerVerticalPairTop(X1+1,0,WVL,P).

// Bottom
winnerVerticalPairBottom(8,Y1,[],P).

winnerVerticalPairBottom(X1,Y1,[pos(X1,Y4)|WVL],P) :-
	Y1 < 7 &
	X1 < 8 &
	vertical(X1,Y1,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y4) &
	(Y4 = Y3+1) &
	winnerVerticalPairBottom(X1,Y1+1,WVL,P).

winnerVerticalPairBottom(X1,Y1,WVL,P) :-
	Y1 < 7 &
	X1 < 8 &
	winnerVerticalPairBottom(X1,Y1+1,WVL,P).

winnerVerticalPairBottom(X1,7,WVL,P) :-
	winnerVerticalPairBottom(X1+1,0,WVL,P).


// Forms a list of all winning horizontal positions in the board
winnerHorizontal(L,P):-
	winnerHorizontalPairLeft(0,0,L1,P) &            
	winnerHorizontalPairRight(0,0,L2,P) &
	.concat(L1,L2,L).

// Left
winnerHorizontalPairLeft(X1,8,[],P).

winnerHorizontalPairLeft(X1,Y1,[pos(X0,Y1)|WHL],P) :-
	X1 < 7 &
	Y1 < 8 &
	horizontal(X1,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X0,Y1) & 
	(X0 = X1-1) &
	winnerHorizontalPairLeft(X1+1,Y1,WHL,P).

winnerHorizontalPairLeft(X1,Y1,WHL,P) :-
	X1 < 7 &
	Y1 < 8 &
	winnerHorizontalPairLeft(X1+1,Y1,WHL,P).

winnerHorizontalPairLeft(7,Y1,WHL,P) :-
	winnerHorizontalPairLeft(0,Y1+1,WHL,P).

// Right
winnerHorizontalPairRight(X1,8,[],P).

winnerHorizontalPairRight(X1,Y1,[pos(X4,Y1)|WHL],P) :-
	X1 < 7 &
	Y1 < 8 &
	horizontal(X1,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X4,Y1) & 
	(X4 = X3+1) &
	winnerHorizontalPairRight(X1+1,Y1,WHL,P).

winnerHorizontalPairRight(X1,Y1,WHL,P) :-
	X1 < 7 &
	Y1 < 8 &
	winnerHorizontalPairRight(X1+1,Y1,WHL,P).

winnerHorizontalPairRight(7,Y1,WHL,P) :-
	winnerHorizontalPairRight(0,Y1+1,WHL,P).


// Forms a list of all winning diagonal positions in the board
winnerDiagonal(L,P) :-
	winnerDiagonalPairTopLeft(0,0,L1,P) &            
	winnerDiagonalPairBottomRight(0,0,L2,P) &
	winnerDiagonalPairTopRight(0,0,L3,P) &            
	winnerDiagonalPairBottomLeft(0,0,L4,P) &
	.concat(L1,L2,LT1) &
	.concat(L3,L4,LT2) &
	.concat(LT1,LT2,L).

// Top Left
winnerDiagonalPairTopLeft(8,Y1,[],P).

winnerDiagonalPairTopLeft(X1,Y1,[pos(X0,Y0)|DHL],P) :-
	Y1 < 7 &
	X1 < 8 &
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2+1) &
	(Y3 = Y2+1) &
	checkEmpty(X0,Y0) & 
	(Y0 = Y1-1) &
	(X0 = X1-1) &
	winnerDiagonalPairTopLeft(X1,Y1+1,DHL,P).

winnerDiagonalPairTopLeft(X1,Y1,DHL,P) :-
	Y1 < 7 &
	X1 < 8 &
	winnerDiagonalPairTopLeft(X1+1,Y1,DHL,P).

winnerDiagonalPairTopLeft(X1,7,DHL,P) :-
	winnerDiagonalPairTopLeft(X1+1,0,DHL,P).

// Bottom Right
winnerDiagonalPairBottomRight(8,Y1,[],P).

winnerDiagonalPairBottomRight(X1,Y1,[pos(X4,Y4)|DHL],P) :-
	Y1 < 7 &
	X1 < 8 &
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2-1) &
	(Y3 = Y2+1) &
	checkEmpty(X0,Y0) & 
	(Y4 = Y3+1) &
	(X4 = X3-1) &
	winnerDiagonalPairBottomRight(X1,Y1+1,DHL,P).

winnerDiagonalPairBottomRight(X1,Y1,DHL,P) :-
	Y1 < 7 &
	X1 < 8 &
	winnerDiagonalPairBottomRight(X1+1,Y1,DHL,P).

winnerDiagonalPairBottomRight(X1,7,DHL,P) :-
	winnerDiagonalPairBottomRight(X1+1,0,DHL,P).

// Top Right
winnerDiagonalPairTopRight(8,Y1,[],P).

winnerDiagonalPairTopRight(X1,Y1,[pos(X0,Y0)|DHL],P) :-
	Y1 < 7 &
	X1 < 8 &
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2-1) &
	(Y3 = Y2+1) &
	checkEmpty(X0,Y0) & 
	(Y0 = Y1-1) &
	(X0 = X1+1) &
	winnerDiagonalPairTopRight(X1,Y1+1,DHL,P).

winnerDiagonalPairTopRight(X1,Y1,DHL,P) :-
	Y1 < 7 &
	X1 < 8 &
	winnerDiagonalPairTopRight(X1+1,Y1,DHL,P).

winnerDiagonalPairTopRight(X1,7,DHL,P) :-
	winnerDiagonalPairTopRight(X1+1,0,DHL,P).

// Bottom Left
winnerDiagonalPairBottomLeft(8,Y1,[],P).

winnerDiagonalPairBottomLeft(X1,Y1,[pos(X4,Y4)|DHL],P) :-
	Y1 < 7 &
	X1 < 8 &
	diagonal(X1,Y1,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X3 = X2-1) &
	(Y3 = Y2+1) &
	checkEmpty(X0,Y0) & 
	(Y4 = Y3-1) &
	(X4 = X3+1) &
	winnerDiagonalPairBottomLeft(X1,Y1+1,DHL,P).

winnerDiagonalPairBottomLeft(X1,Y1,DHL,P) :-
	Y1 < 7 &
	X1 < 8 &
	winnerDiagonalPairBottomLeft(X1+1,Y1,DHL,P).

winnerDiagonalPairBottomLeft(X1,7,DHL,P) :-
	winnerDiagonalPairBottomLeft(X1+1,0,DHL,P).


// Forms a list of all winning vertical positions in the board, having the empty space between chips
winnerVerticalThreeInFour(L,P):-
	winnerVerticalThreeInFourTop(0,0,L1,P) &            
	winnerVerticalThreeInFourBottom(0,0,L2,P) &
	.concat(L1,L2,L).

//Top
winnerVerticalThreeInFourTop(8,Y1,[],P).

winnerVerticalThreeInFourTop(X1,Y1,[pos(X1,Y2)|WTVL],P) :-
	Y1 < 7 &
	X1 < 8 &
	vertical(X1,Y3,X1,Y4,P) &
	verticalTwoInThree(X1,Y1,X1,Y3,P) &
	(Y2 = Y1+1) &
	winnerVerticalThreeInFourTop(X1,Y1+1,WTVL,P).

winnerVerticalThreeInFourTop(X1,Y1,WTVL,P) :-
	Y1 < 7 &
	X1 < 8 &
	winnerVerticalThreeInFourTop(X1,Y1+1,WTVL,P).

winnerVerticalThreeInFourTop(X1,7,WTVL,P) :-
	winnerVerticalThreeInFourTop(X1+1,0,WTVL,P).

//Bottom
winnerVerticalThreeInFourBottom(8,Y1,[],P).

winnerVerticalThreeInFourBottom(X1,Y1,[pos(X1,Y3)|WTVL],P) :-
	Y1 < 7 &
	X1 < 8 &
	vertical(X1,Y1,X1,Y2,P) &
	verticalTwoInThree(X1,Y2,X1,Y4,P) &
	(Y3 = Y2+1) &
	winnerVerticalThreeInFourBottom(X1,Y1+1,WTVL,P).

winnerVerticalThreeInFourBottom(X1,Y1,WTVL,P) :-
	Y1 < 7 &
	X1 < 8 &
	winnerVerticalThreeInFourBottom(X1,Y1+1,WTVL,P).

winnerVerticalThreeInFourBottom(X1,7,WTVL,P) :-
	winnerVerticalThreeInFourBottom(X1+1,0,WTVL,P).


// Forms a list of all winning horizontal positions in the board, having the empty space between chips
winnerHorizontalThreeInFour(L,P):-
	winnerHorizontalThreeInFourLeft(0,0,L1,P) &            
	winnerHorizontalThreeInFourRight(0,0,L2,P) &
	.concat(L1,L2,L).

//Left
winnerHorizontalThreeInFourLeft(X1,8,[],P).

winnerHorizontalThreeInFourLeft(X1,Y1,[pos(X2,Y1)|WTHL],P) :-
	X1 < 7 &
	Y1 < 8 &
	horizontal(X3,Y1,X4,Y1,P) &
	horizontalTwoInThree(X1,Y1,X3,Y1,P) &
	(X2 = X1+1) &
	winnerHorizontalThreeInFourLeft(X1+1,Y1,WTHL,P).

winnerHorizontalThreeInFourLeft(X1,Y1,WTHL,P) :-
	X1 < 7 &
	Y1 < 8 &
	winnerHorizontalThreeInFourLeft(X1+1,Y1,WTHL,P).

winnerHorizontalThreeInFourLeft(7,Y1,WTHL,P) :-
	winnerHorizontalThreeInFourLeft(0,Y1+1,WTHL,P).

//Right
winnerHorizontalThreeInFourRight(X1,8,[],P).

winnerHorizontalThreeInFourRight(X1,Y1,[pos(X3,Y1)|WTHL],P) :-
	X1 < 7 &
	Y1 < 8 &
	horizontal(X1,Y1,X2,Y1,P) &
	horizontalTwoInThree(X2,Y1,X4,Y1,P) &
	(X3 = X2+1) &
	winnerHorizontalThreeInFourRight(X1+1,Y1,WTHL,P).

winnerHorizontalThreeInFourRight(X1,Y1,WTHL,P) :-
	X1 < 7 &
	Y1 < 8 &
	winnerHorizontalThreeInFourRight(X1+1,Y1,WTHL,P).

winnerHorizontalThreeInFourRight(7,Y1,WTHL,P) :-
	winnerHorizontalThreeInFourRight(0,Y1+1,WTHL,P).


// Forms a list of all winning diagonal positions in the board, having the empty space between chips
winnerDiagonalThreeInFour(L,P):-
	winnerDiagonalThreeInFourTopLeft(0,0,L1,P) &     
	winnerDiagonalThreeInFourBottomRight(0,0,L2,P) &         
	winnerDiagonalThreeInFourTopRight(0,0,L3,P) &
	winnerDiagonalThreeInFourBottomLeft(0,0,L4,P) &
	.concat(L1,L2,LT1) &
	.concat(L3,L4,LT2) &
	.concat(LT1,LT2,L).

// Top Left
winnerDiagonalThreeInFourTopLeft(8,Y1,[],P).

winnerDiagonalThreeInFourTopLeft(X1,Y1,[pos(X2,Y2)|WTDL],P) :-
	Y1 < 7 &
	X1 < 8 &
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	diagonal(X3,Y3,X4,Y4,P) &
	(X4 = X3+1) & 
	(Y4 = Y3+1) &
	(X2 = X1+1) &
	(Y2 = Y1+1) &
	winnerDiagonalThreeInFourTopLeft(X1,Y1+1,WTDL,P).

winnerDiagonalThreeInFourTopLeft(X1,Y1,WTDL,P) :-
	Y1 < 7 &
	X1 < 8 &
	winnerDiagonalThreeInFourTopLeft(X1,Y1+1,WTDL,P).

winnerDiagonalThreeInFourTopLeft(X1,7,WTDL,P) :-
	winnerDiagonalThreeInFourTopLeft(X1+1,0,WTDL,P).

// Bottom Right
winnerDiagonalThreeInFourBottomRight(8,Y1,[],P).

winnerDiagonalThreeInFourBottomRight(X1,Y1,[pos(X3,Y3)|WTDL],P) :-
	Y1 < 7 &
	X1 < 8 &
	diagonal(X1,Y1,X2,Y2,P) &
	diagonalTwoInThree(X2,Y2,X4,Y4,P) &
	(X4 = X1+3) & 
	(Y4 = Y1+3) &
	(X3 = X2+1) &
	(Y3 = Y2+1) &
	winnerDiagonalThreeInFourBottomRight(X1,Y1+1,WTDL,P).

winnerDiagonalThreeInFourBottomRight(X1,Y1,WTDL,P) :-
	Y1 < 7 &
	X1 < 8 &
	winnerDiagonalThreeInFourBottomRight(X1,Y1+1,WTDL,P).

winnerDiagonalThreeInFourBottomRight(X1,7,WTDL,P) :-
	winnerDiagonalThreeInFourBottomRight(X1+1,0,WTDL,P).

// Top Right
winnerDiagonalThreeInFourTopRight(8,Y1,[],P).

winnerDiagonalThreeInFourTopRight(X1,Y1,[pos(X2,Y2)|WTDL],P) :-
	Y1 < 7 &
	X1 < 8 &
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	diagonal(X3,Y3,X4,Y4,P) &
	(X4 = X3-1) & 
	(Y4 = Y3+1) &
	(X2 = X1-1) &
	(Y2 = Y1+1) &
	winnerDiagonalThreeInFourTopRight(X1,Y1+1,WTDL,P).

winnerDiagonalThreeInFourTopRight(X1,Y1,WTDL,P) :-
	Y1 < 7 &
	X1 < 8 &
	winnerDiagonalThreeInFourTopRight(X1,Y1+1,WTDL,P).

winnerDiagonalThreeInFourTopRight(X1,7,WTDL,P) :-
	winnerDiagonalThreeInFourTopRight(X1+1,0,WTDL,P).

// Bottom Left
winnerDiagonalThreeInFourBottomLeft(8,Y1,[],P).

winnerDiagonalThreeInFourBottomLeft(X1,Y1,[pos(X3,Y3)|WTDL],P) :-
	Y1 < 7 &
	X1 < 8 &
	diagonal(X1,Y1,X2,Y2,P) &
	diagonalTwoInThree(X2,Y2,X4,Y4,P) &
	(X4 = X1-3) & 
	(Y4 = Y1+3) &
	(X3 = X2-1) &
	(Y3 = Y2+1) &
	winnerDiagonalThreeInFourBottomLeft(X1,Y1+1,WTDL,P).

winnerDiagonalThreeInFourBottomLeft(X1,Y1,WTDL,P) :-
	Y1 < 7 &
	X1 < 8 &
	winnerDiagonalThreeInFourBottomLeft(X1+1,Y1,WTDL,P).

winnerDiagonalThreeInFourBottomLeft(X1,7,WTDL,P) :-
	winnerDiagonalThreeInFourBottomLeft(X1+1,0,WTDL,P).




// Forms a list of all pairs of chips in the board
pairs(PL,P) :-
	verticalPair(0, 0, VL, P) &
	horizontalPair(0, 0, HL, P) &
	diagonalPair(0, 0, DL, P) &
	.concat(VL, HL, TmpL) &
	.concat(TmpL, DL, PL).


// Forms a list of all pairs of chips of the form X[]X in the board
twoInThreePairs(PTL, P) :-
	verticalTwoInThreePair(0, 0, VTL, P) &
	horizontalTwoInThreePair(0, 0, HTL, P) &
	diagonalTwoInThreePair(0, 0, DTL, P) &
	.concat(VTL, HTL, TmpL) &
	.concat(TmpL, DTL, PTL).


// Forms a list of all pairs of chips of the form X[][]X in the board
twoInFourPairs(PFL, P) :-
	verticalTwoInFourPair(0, 0, VFL, P) &
	horizontalTwoInFourPair(0, 0, HFL, P) &
	diagonalTwoInFourPair(0, 0, DFL, P) &
	.concat(VFL, HFL, TmpL) &
	.concat(TmpL, DFL, PFL).	

	
// Rules for vertical pairs	
verticalPair(8, Y1, [],P).
	
verticalPair(X1, Y1, [pairPos(pos(X1,Y1), pos(X1,Y2))|LV],P) :-
    Y1 < 7 & 
	X1 < 8 &
	vertical(X1,Y1,X1,Y2,P) &
	verticalPair(X1, Y1+1, LV,P).
	
verticalPair(X1, Y1, LV,P) :-
    Y1 < 7 & 
	X1 < 8 &
	not vertical(X1,Y1,X1,Y2, P) &
	verticalPair(X1, Y1+1, LV, P).

verticalPair(X1, 7, LV, P) :-
	verticalPair(X1+1, 0, LV, P).

// Rules for horizontal pairs
horizontalPair(X1, 8, [], P).

horizontalPair(X1, Y1, [pairPos(pos(X1,Y1), pos(X2,Y1))|HV], P) :-
    X1 < 7 & 
	Y1 < 8 &
	horizontal(X1,Y1,X2,Y1, P) &
	horizontalPair(X1+1, Y1, HV, P).
	
horizontalPair(X1, Y1, HV, P) :-
    X1 < 7 & 
	Y1 < 8 &
	not horizontal(X1,Y1,X2,Y1, P) &
	horizontalPair(X1+1, Y1, HV, P).

horizontalPair(7, Y1, HV, P) :-
	horizontalPair(0, Y1+1, HV, P).

// Rules for diagonal pairs
diagonalPair(8, Y1, [], P).
	
diagonalPair(X1, Y1, [pairPos(pos(X1,Y1), pos(X2,Y2))|DV], P) :-
    Y1 < 7 & 
	X1 < 8 &
	diagonal(X1,Y1,X2,Y2, P) &
	diagonalPair(X1, Y1+1, DV, P).
	
diagonalPair(X1, Y1, DV, P) :-
    Y1 < 7 & 
	X1 < 8 &
	not diagonal(X1,Y1,X2,Y2, P) &
	diagonalPair(X1, Y1+1, DV, P).

diagonalPair(X1, 7, DV, P) :-
	diagonalPair(X1+1, 0, DV, P).



// Rules for two chips in vertical of the form X[]X with no chip in between
verticalTwoInThreePair(8, Y1, [], P).

verticalTwoInThreePair(X1, Y1, [pairPos(pos(X1,Y1), pos(X1,Y3))|VTL], P) :-
    Y1 < 7 & 
	X1 < 8 &
	verticalTwoInThree(X1,Y1,X1,Y3, P) &
	verticalTwoInThreePair(X1, Y1+1, VTL, P).
	
verticalTwoInThreePair(X1, Y1, VTL, P) :-
    Y1 < 7 & 
	X1 < 8 &
	not verticalTwoInThree(X1,Y1,X1,Y3, P) &
	verticalTwoInThreePair(X1, Y1+1, VTL, P).
	
verticalTwoInThreePair(X1, 7, VTL, P) :-
	verticalTwoInThreePair(X1+1, 0, VTL, P).

// Rules for two chips in horizontal of the form X[]X with no chip in between 
horizontalTwoInThreePair(X1, 8, [], P).

horizontalTwoInThreePair(X1, Y1, [pairPos(pos(X1,Y1), pos(X3,Y1))|HTL], P) :-
    X1 < 7 & 
	Y1 < 8 &
	horizontalTwoInThree(X1,Y1,X3,Y1, P) &
	horizontalTwoInThreePair(X1+1, Y1, HTL, P).
	
horizontalTwoInThreePair(X1, Y1, HTL, P) :-
    X1 < 7 & 
	Y1 < 8 &
	not horizontalTwoInThree(X1,Y1,X3,Y1, P) &
	horizontalTwoInThreePair(X1+1, Y1, HTL, P).
	
horizontalTwoInThreePair(7, Y1, HTL, P) :-
	horizontalTwoInThreePair(0, Y1+1, HTL, P).

// Rules for two chips in diagonal of the form X[]X with no chip in between 
diagonalTwoInThreePair(8, Y1, [], P).

diagonalTwoInThreePair(X1, Y1, [pairPos(pos(X1,Y1), pos(X3,Y3))|DTL], P) :-
    Y1 < 7 & 
	X1 < 8 &
	diagonalTwoInThree(X1,Y1,X3,Y3, P) &
	diagonalTwoInThreePair(X1, Y1+1, DTL, P).
	
diagonalTwoInThreePair(X1, Y1, DTL, P) :-
    Y1 < 7 & 
	X1 < 8 &
	not diagonalTwoInThree(X1,Y1,X3,Y3, P) &
	diagonalTwoInThreePair(X1, Y1+1, DTL, P).
	
diagonalTwoInThreePair(X1, 7, DTL, P) :-
	diagonalTwoInThreePair(X1+1, 0, DTL, P).


// Rules for two chips in vertical of the form X[][]X with no chips in between
verticalTwoInFourPair(8, Y1, [], P).

verticalTwoInFourPair(X1, Y1, [pairPos(pos(X1,Y1), pos(X1,Y4))|VFL], P) :-
    Y1 < 7 & 
	X1 < 8 &
	verticalTwoInFour(X1,Y1,X1,Y4, P) &
	verticalTwoInFourPair(X1, Y1+1, VFL, P).
	
verticalTwoInFourPair(X1, Y1, VFL, P) :-
    Y1 < 7 & 
	X1 < 8 &
	not verticalTwoInFour(X1,Y1,X1,Y4, P) &
	verticalTwoInFourPair(X1, Y1+1, VFL, P).
	
verticalTwoInFourPair(X1, 7, VFL, P) :-
	verticalTwoInFourPair(X1+1, 0, VFL, P).

// Rules for two chips in horizontal of the form X[][]X with no chips in between 
horizontalTwoInFourPair(X1, 8, [], P).

horizontalTwoInFourPair(X1, Y1, [pairPos(pos(X1,Y1), pos(X4,Y1))|HFL], P) :-
    X1 < 7 & 
	Y1 < 8 &
	horizontalTwoInFour(X1,Y1,X4,Y1, P) &
	horizontalTwoInFourPair(X1+1, Y1, HFL, P).
	
horizontalTwoInFourPair(X1, Y1, HFL, P) :-
    X1 < 7 & 
	Y1 < 8 &
	not horizontalTwoInFour(X1,Y1,X4,Y1, P) &
	horizontalTwoInFourPair(X1+1, Y1, HFL, P).
	
horizontalTwoInFourPair(7, Y1, HFL, P) :-
	horizontalTwoInFourPair(0, Y1+1, HFL, P).

// Rules for two chips in diagonal of the form X[][]X with no chips in between 
diagonalTwoInFourPair(8, Y1, [], P).

diagonalTwoInFourPair(X1, Y1, [pairPos(pos(X1,Y1), pos(X4,Y4))|VFL], P) :-
    Y1 < 7 & 
	X1 < 8 &
	diagonalTwoInFour(X1,Y1,X4,Y4, P) &
	diagonalTwoInFourPair(X1, Y1+1, VFL, P).
	
diagonalTwoInFourPair(X1, Y1, VFL, P) :-
    Y1 < 7 & 
	X1 < 8 &
	not diagonalTwoInFour(X1,Y1,X4,Y4, P) &
	diagonalTwoInFourPair(X1, Y1+1, VFL, P).
	
diagonalTwoInFourPair(X1, 7, VFL, P) :-
	diagonalTwoInFourPair(X1+1, 0, VFL, P).

// rules to get pairs
// Rule to get a vertical pair XX
vertical(X1,Y1,X1,Y2,P) :-
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X1,Y2,P)[source(percept)] &
	(Y2 = Y1+1).

// Rule to get a horizontal pair XX
horizontal(X1,Y1,X2,Y1,P) :-
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X2,Y1,P)[source(percept)] &
	(X2 = X1+1).

// Rule to get a diagonal pair XX
diagonal(X1,Y1,X2,Y2,P) :-
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X2,Y2,P)[source(percept)] &
	(((X2 = X1+1) & (Y2 = Y1+1)) |
	((X2 = X1-1) & (Y2 = Y1+1))).

// rules to get two in three
// Rule to get a vertical pair X[]X
verticalTwoInThree(X1,Y1,X1,Y3,P) :-
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X1,Y2,0)[source(percept)] &
	tablero(X1,Y3,P)[source(percept)] &
	(Y3 = Y1+2) &
	(Y2 = Y1+1).

// Rule to get a horizontal pair X[]X
horizontalTwoInThree(X1,Y1,X3,Y1,P) :- 
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X2,Y1,0)[source(percept)] &
	tablero(X3,Y1,P)[source(percept)] &
	(X3 = X1+2) &
	(X2 = X1+1).

// Rule to get a diagonal pair X[]X
diagonalTwoInThree(X1,Y1,X3,Y3,P) :- 
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X2,Y2,0)[source(percept)] &
	tablero(X3,Y3,P)[source(percept)] &
	(((X3 = X1+2) & (X2 = X1+1)  &
	(Y3 = Y1+2) & (Y2 = Y1+1)) |
	((X3 = X1-2) & (X2 = X1-1)  &
	(Y3 = Y1+2) & (Y2 = Y1+1))).

// rules to get two in four
// Rule to get a vertical pair X[][]X
verticalTwoInFour(X1,Y1,X1,Y4,P) :- 
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X1,Y2,0)[source(percept)] &
	tablero(X1,Y3,0)[source(percept)] &
	tablero(X1,Y4,P)[source(percept)] &
	(Y4 = Y1+3) &
	(Y3 = Y1+2) &
	(Y2 = Y1+1).

// Rule to get a horizontal pair X[][]X
horizontalTwoInFour(X1,Y1,X4,Y1,P) :-
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X2,Y1,0)[source(percept)] &
	tablero(X3,Y1,0)[source(percept)] &
	tablero(X4,Y1,P)[source(percept)] &
	(X4 = X1+3) &
	(X3 = X1+2) &
	(X2 = X1+1).

// Rule to get a diagonal pair X[][]X
diagonalTwoInFour(X1,Y1,X4,Y4,P):-
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X2,Y2,0)[source(percept)] &
	tablero(X3,Y3,0)[source(percept)] &
	tablero(X4,Y4,P)[source(percept)] &
	(((X4 = X1+3) & (X3 = X1+2) & (X2 = X1+1) &
	(Y4 = Y1+3) & (Y3 = Y1+2) & (Y2 = Y1+1)) |
	((X4 = X1-3) & (X3 = X1-2) & (X2 = X1-1) &
	(Y4 = Y1+3) & (Y3 = Y1+2) & (Y2 = Y1+1))).





/* Initial goals */



!play.



/* Plans */

// PLAY TO WIN
+!play: playerNumber &
	estrategia(jugarAGanar)[source(percept)] <-
		.print("A ganar");
		!playToWin.

// PLAY TO LOSE
+!play:
	estrategia(jugarAPerder)[source(percept)] <-
		.print("A perder");
		!playToLose.
		
//TEST AREA		


//WINNING PLAN



+!playToWin: 
	estrategia(jugarAGanar)[source(percept)] &
	movement(N) &
	N = 0 &
	.my_name(Player) &
	turno(Player)[source(percept)] <-
	put(3,3);
	-+movement(N+2);
	+ventaja(0);
	!playToWin.

+!playToWin:
	estrategia(jugarAGanar)[source(percept)] &
	movement(N) &
	N = 1 &
	.my_name(Player) &
	turno(Player)[source(percept)] &
	rival(R) &
	tablero(X,Y,R)[source(percept)] <-
	+jugadaActual(pos(X,Y));
	?closerToCenter(X1,Y1);
	put(X1,Y1);
	-+movement(N+2);
	-jugadaActual(pos(x,y));
	!playToWin.


+!playToWin:
	estrategia(jugarAGanar)[source(percept)] &
	movement(N) &
	.my_name(Player) &
	turno(Player)[source(percept)] &
	player(P) &
	rival(R) &
	winnerTotal(WPL, P) &
	winnerTotal(WRL, R) &
	pairs(PLP, P) &
	twoInThreePairs(TLP, P) &
	pairs(PLR, R) &
	twoInThreePairs(TLR, R) &
	allNotWinningTrio(P, R, BFLP) &
	winningTrio(PLP, WTLP, P) &
	winningTrioTinT(TLP, WTTLP, P) &
	winningTrio(PLR, WTLR, R) &
	winningTrioTinT(TLR, WTTLR, R)<-
	 
	.asserta(winnerPlayer(WPL));
	.asserta(winnerRival(WRL));
	.asserta(pairsPlayer(PLP));
	.asserta(twoInThreePlayer(TLP));
	.asserta(pairsRival(PLR));
	.asserta(twoInThreeRival(TLR));
	.asserta(blockForcePlayer(BFLP));
	.asserta(winningTrioPlayer(WTLP));
	.asserta(winningTrioTPlayer(WTTLP));
	.asserta(winningTrioRival(WTLR));
	.asserta(winningTrioTRival(WTTLR));
	?nextMove(X1,Y1);
	put(X1,Y1);
	-+movement(N+2);
	.abolish(winnerPlayer(WPL));
	.abolish(winnerRival(WRL));
	.abolish(pairsPlayer(PLP));
	.abolish(twoInThreePlayer(TLP));
	.abolish(pairsRival(PLR));
	.abolish(twoInThreeRival(TLR));
	.abolish(blockForcePlayer(BFLP));
	.abolish(winningTrioPlayer(WTLP));
	.abolish(winningTrioTPlayer(WTTLP));
	.abolish(winningTrioRival(WTLR));
	.abolish(winningTrioTRival(WTTLR));
	!playToWin.




+!playToWin <- !playToWin.

//LOSING PLAN



	
	
//ERRORS
+!play <- .print("Error in !play").
+!playToLose <- .print("Error in !playToLose").