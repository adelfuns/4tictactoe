// Agent player1 in project cuatroenraya.mas2j

/* Initial beliefs and rules */
movement(0).


// Gets the players number and adds to the beliefs player(number)
// and opponent(number2).
playerNumbers :- 
	.my_name(N) &
	.term2string(N,S) &
	.length(S,M) &
	.substring(S,X,(M-1)) &
	.term2string(Y,X) &
	.asserta(player(Y)) &
	enemyNumber(Y,Z) &
	.asserta(opponent(Z)).

// Gets the opponent's number
enemyNumber(X,2):-
	X = 1.

enemyNumber(X,1):-
	X = 2.


// Checks if the board in X-Y position is free
checkEmpty(X,Y):-
	tablero(X,Y,0)[source(percept)].


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
	listVerticalWinPositionsTwoInThreeTop([],L3,P) &
	listVerticalWinPositionsTwoInThreeBottom([],L4,P) &
	.union(L1,L2,L5) &
	.union(L3,L4,L6) &
	.union(L5,L6,L).


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


listVerticalWinPositionsTwoInThreeTop([],L,P) :-
	verticalTwoInThree(X1,Y0,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y1) &
	(Y1 = Y0 + 1) &
	.concat([pos(X1,Y1)],[],TmpL) &
	listVerticalWinPositionsTwoInThreeTop(TmpL,L,P).

listVerticalWinPositionsTwoInThreeTop(TmpL,L,P) :-
	verticalTwoInThree(X1,Y0,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y1) &
	(Y1 = Y0 + 1) &
	not .member(pos(X1,Y1),TmpL) &
	.concat([pos(X1,Y1)],TmpL,TmpL2) &
	listVerticalWinPositionsTwoInThreeTop(TmpL2,L,P).

listVerticalWinPositionsTwoInThreeTop([],[],P).
listVerticalWinPositionsTwoInThreeTop(TmpL,L,P) :- L = TmpL.


listVerticalWinPositionsTwoInThreeBottom([],L,P) :-
	verticalTwoInThree(X1,Y1,X1,Y3,P) &
	vertical(X0,Y0,X1,Y1,P) &
	checkEmpty(X1,Y2) &
	(Y2 = Y1 + 1) &
	.concat([pos(X1,Y2)],[],TmpL) &
	listVerticalWinPositionsTwoInThreeBottom(TmpL,L,P).

listVerticalWinPositionsTwoInThreeBottom(TmpL,L,P) :-
	verticalTwoInThree(X1,Y1,X1,Y3,P) &
	vertical(X0,Y0,X1,Y1,P) &
	checkEmpty(X1,Y2) &
	(Y2 = Y1 + 1) &
	not .member(pos(X1,Y2),TmpL) &
	.concat([pos(X1,Y2)],TmpL,TmpL2) &
	listVerticalWinPositionsTwoInThreeBottom(TmpL2,L,P).

listVerticalWinPositionsTwoInThreeBottom([],[],P).
listVerticalWinPositionsTwoInThreeBottom(TmpL,L,P) :- L = TmpL.


// Gets horizontal winning positions
listHorizontalWinPositions(L,P):-
	listHorizontalWinPositionsLeft([],L1,P) &
	listHorizontalWinPositionsRight([],L2,P) &
	listHorizontalWinPositionsTwoInThreeLeft([],L3,P) &
	listHorizontalWinPositionsTwoInThreeRight([],L4,P) &
	.union(L1,L2,L5) &
	.union(L3,L4,L6) &
	.union(L5,L6,L).


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


listHorizontalWinPositionsTwoInThreeLeft([],L,P) :-
	horizontalTwoInThree(X0,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X1,Y1) &
	(X1 = X0 + 1) &
	.concat([pos(X1,Y1)],[],TmpL) &
	listHorizontalWinPositionsTwoInThreeLeft(TmpL,L,P).

listHorizontalWinPositionsTwoInThreeLeft(TmpL,L,P) :-
	horizontalTwoInThree(X0,Y1,X2,Y1,P) &
	horizontal(X2,Y1,X3,Y1,P) &
	checkEmpty(X1,Y1) &
	(X1 = X0 + 1) &
	not .member(pos(X1,Y1),TmpL) &
	.concat([pos(X1,Y1)],TmpL,TmpL2) &
	listHorizontalWinPositionsTwoInThreeLeft(TmpL2,L,P).

listHorizontalWinPositionsTwoInThreeLeft([],[],P).
listHorizontalWinPositionsTwoInThreeLeft(TmpL,L,P) :- L = TmpL.


listHorizontalWinPositionsTwoInThreeRight([],L,P) :-
	horizontalTwoInThree(X1,Y1,X3,Y1,P) &
	horizontal(X0,Y1,X1,Y1,P) &
	checkEmpty(X2,Y1) &
	(X2 = X1 + 1) &
	.concat([pos(X2,Y1)],[],TmpL) &
	listHorizontalWinPositionsTwoInThreeRight(TmpL,L,P).

listHorizontalWinPositionsTwoInThreeRight(TmpL,L,P) :-
	horizontalTwoInThree(X1,Y1,X3,Y1,P) &
	horizontal(X0,Y1,X1,Y1,P) &
	checkEmpty(X2,Y1) &
	(X2 = X1 + 1) &
	not .member(pos(X2,Y1),TmpL) &
	.concat([pos(X2,Y1)],TmpL,TmpL2) &
	listHorizontalWinPositionsTwoInThreeRight(TmpL2,L,P).

listHorizontalWinPositionsTwoInThreeRight([],[],P).
listHorizontalWinPositionsTwoInThreeRight(TmpL,L,P) :- L = TmpL.


// Gets diagonal winning positions
listDiagonalWinPositions(L,P) :-
	listDiagonalWinPositionsTopLeft([],L1,P) &            
	listDiagonalWinPositionsBottomRight([],L2,P) &
	listDiagonalWinPositionsTopRight([],L3,P) &            
	listDiagonalWinPositionsBottomLeft([],L4,P) &
	listDiagonalWinPositionsTwoInThreeTopLeft([],L5,P) &
	listDiagonalWinPositionsTwoInThreeBottomLeft([],L6,P) &
	listDiagonalWinPositionsTwoInThreeTopRight([],L7,P) &
	listDiagonalWinPositionsTwoInThreeBottomRight([],L8,P) &
	.union(L1,L2,L9) &
	.union(L3,L4,L10) &
	.union(L5,L6,L11) &
	.union(L7,L8,L12) &
	.union(L9,L10,L13) &
	.union(L11,L12,L14) &
	.union(L13,L14,L).


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


listDiagonalWinPositionsTwoInThreeTopLeft([],L,P) :-
	diagonalTwoInThree(X0,Y0,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X2 = X0 + 2) &
	(Y2 = Y0 + 2) &
	checkEmpty(X1,Y1) &
	(X1 = X0 + 1) &
	(Y1 = Y0 + 1) &
	.concat([pos(X1,Y1)],[],TmpL) &
	listDiagonalWinPositionsTwoInThreeTopLeft(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeTopLeft(TmpL,L,P) :-
	diagonalTwoInThree(X0,Y0,X2,Y2,P) &
	diagonal(X2,Y2,X3,Y3,P) &
	(X2 = X0 + 2) &
	(Y2 = Y0 + 2) &
	checkEmpty(X1,Y1) &
	(X1 = X0 + 1) &
	(Y1 = Y0 + 1) &
	not .member(pos(X1,Y1),TmpL) &
	.concat([pos(X1,Y1)],[],TmpL) &
	listDiagonalWinPositionsTwoInThreeTopLeft(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeTopLeft([],[],P).
listDiagonalWinPositionsTwoInThreeTopLeft(TmpL,L,P) :- L = TmpL.


listDiagonalWinPositionsTwoInThreeBottomRight([],L,P) :-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	diagonal(X0,Y0,X1,Y1,P) &
	(X3 = X1 + 2) &
	(Y3 = Y1 + 2) &
	checkEmpty(X2,Y2) &
	(X2 = X1 + 1) &
	(Y2 = Y1 + 1) &
	.concat([pos(X2,Y2)],[],TmpL) &
	listDiagonalWinPositionsTwoInThreeBottomRight(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeBottomRight(TmpL,L,P) :-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	diagonal(X0,Y0,X1,Y1,P) &
	(X3 = X1 + 2) &
	(Y3 = Y1 + 2) &
	checkEmpty(X2,Y2) &
	(X2 = X1 + 1) &
	(Y2 = Y1 + 1) &
	not .member(pos(X2,Y2),TmpL) &
	.concat([pos(X2,Y2)],[],TmpL) &
	listDiagonalWinPositionsTwoInThreeBottomRight(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeBottomRight([],[],P).
listDiagonalWinPositionsTwoInThreeBottomRight(TmpL,L,P) :- L = TmpL.


listDiagonalWinPositionsTwoInThreeTopRight([],L,P) :-
	diagonalTwoInThree(X3,Y0,X1,Y2,P) &
	diagonal(X1,Y2,X0,Y3,P) &
	(X1 = X3 - 2) &
	(Y2 = Y0 + 2) &
	checkEmpty(X2,Y1) &
	(X2 = X1 + 1) &
	(Y1 = Y2 - 1) &
	.concat([pos(X2,Y1)],[],TmpL) &
	listDiagonalWinPositionsTwoInThreeTopRight(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeTopRight(TmpL,L,P) :-
	diagonalTwoInThree(X3,Y0,X1,Y2,P) &
	diagonal(X1,Y2,X0,Y3,P) &
	(X1 = X3 - 2) &
	(Y2 = Y0 + 2) &
	checkEmpty(X2,Y1) &
	(X2 = X1 + 1) &
	(Y1 = Y2 - 1) &
	not .member(pos(X2,Y1),TmpL) &
	.concat([pos(X2,Y1)],[],TmpL) &
	listDiagonalWinPositionsTwoInThreeTopRight(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeTopRight([],[],P).
listDiagonalWinPositionsTwoInThreeTopRight(TmpL,L,P) :- L = TmpL.


listDiagonalWinPositionsTwoInThreeBottomLeft([],L,P) :-
	diagonalTwoInThree(X2,Y1,X0,Y3,P) &
	diagonal(X3,Y0,X2,Y1,P) &
	(X0 = X2 - 2) &
	(Y3 = Y1 + 2) &
	checkEmpty(X1,Y2) &
	(X1 = X2 - 1) &
	(Y2 = Y1 + 1) &
	.concat([pos(X1,Y2)],[],TmpL) &
	listDiagonalWinPositionsTwoInThreeBottomLeft(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeBottomLeft(TmpL,L,P) :-
	diagonalTwoInThree(X2,Y1,X0,Y3,P) &
	diagonal(X3,Y0,X2,Y1,P) &
	(X0 = X2 - 2) &
	(Y3 = Y1 + 2) &
	checkEmpty(X1,Y2) &
	(X1 = X2 - 1) &
	(Y2 = Y1 + 1) &
	not .member(pos(X1,Y2),TmpL) &
	.concat([pos(X1,Y2)],[],TmpL) &
	listDiagonalWinPositionsTwoInThreeBottomLeft(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeBottomLeft([],[],P).
listDiagonalWinPositionsTwoInThreeBottomLeft(TmpL,L,P) :- L = TmpL.



// Check all of the player's chip forms
allMyForms(LP,TTL,TFL):-
	player(P) &
	.print(P) &
	pairs(PL,P) &
	.print(PL) &
	twoInThreePairs(TTL,P) &
	.print(TTL) &
	twoInFourPairs(TFL,P) &
	.print(TFL).


// Check all of the opponent's chip forms
allHisForms(LP,TTL,TFL):-
	opponent(P) &
	pairs(PL,P) &
	twoInThreePairs(TTL,P) &
	twoInFourPairs(TFL,P).


// Forms a list of all pairs of chips in the board
pairs(PL,P) :-
	verticalPair([], VL, P) &
	horizontalPair([], HL, P) &
	diagonalPair([], DL, P) &
	.union(VL, HL, TmpL) &
	.union(TmpL, DL, PL).


// Forms a list of all pairs of chips of the form X[]X in the board
twoInThreePairs(PL,P) :-
	verticalTwoInThreePair([], VL, P) &
	horizontalTwoInThreePair([], HL, P) &
	diagonalTwoInThreePair([], DL, P) &
	.union(VL, HL, TmpL) &
	.union(TmpL, DL, PL).


// Forms a list of all pairs of chips of the form X[][]X in the board
twoInFourPairs(PL,P) :-
	verticalTwoInFourPair([], VL, P) &
	horizontalTwoInFourPair([], HL, P) &
	diagonalTwoInFourPair([], DL, P) &
	.union(VL, HL, TmpL) &
	.union(TmpL, DL, PL).	

	
// Rules for vertical pairs	
verticalPair([],LV,P) :-
	vertical(X1,Y1,X1,Y2,P) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y2))],[],TmpL) &
	verticalPair(TmpL,LV,P).

verticalPair(TmpL,LV,P) :-
	vertical(X1,Y1,X1,Y2,P) &
	not .member(pairPos(pos(X1,Y1), pos(X1,Y2)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y2))],TmpL,TmpL2) &
	verticalPair(TmpL2,LV,P).

verticalPair(TmpL,LV,_) :- LV = TmpL.


// Rules for horizontal pairs
horizontalPair([],HL,P) :-
	horizontal(X1,Y1,X2,Y1,P) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y1))],[],TmpL) &
	horizontalPair(TmpL,HL,P).

horizontalPair(TmpL,HL,P) :-
	horizontal(X1,Y1,X2,Y1,P) &
	not .member(pairPos(pos(X1,Y1), pos(X2,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y1))], TmpL, TmpL2) &
	horizontalPair(TmpL2,HL,P).

horizontalPair(TmpL,HL,_) :- HL = TmpL.


// Rules for diagonal pairs
diagonalPair([],DL,P) :-
	diagonal(X1,Y1,X2,Y2,P) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y2))],[],TmpL) &
	diagonalPair(TmpL,DL,P).

diagonalPair(TmpL,DL,P) :-
	diagonal(X1,Y1,X2,Y2,P) &
	not .member(pairPos(pos(X1,Y1), pos(X2,Y2)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X2,Y2))],TmpL,TmpL2) &
	diagonalPair(TmpL2,DL,P).

diagonalPair(TmpL,DL,_) :- DL = TmpL.


// Rules for two chips in vertical of the form X[]X with no chip in between
verticalTwoInThreePair([],VTL,P) :-
	verticalTwoInThree(X1,Y1,X1,Y3,P) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y3))],[],TmpL) &
	verticalTwoInThreePair(TmpL,VTL,P).
	
verticalTwoInThreePair(TmpL,VTL,P) :-
	verticalTwoInThree(X1,Y1,X1,Y3,P) &
	not .member(pairPos(pos(X1,Y1), pos(X1,Y3)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y3))], TmpL, TmpL2) &
	verticalTwoInThreePair(TmpL2,VTL,P).
	
verticalTwoInThreePair(TmpL,VTL,P) :- VTL = TmpL.


// Rules for two chips in horizontal of the form X[]X with no chip in between 
horizontalTwoInThreePair([],HTL,P) :-
	horizontalTwoInThree(X1,Y1,X3,Y1,P) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y1))],[],TmpL) &
	horizontalTwoInThreePair(TmpL,HTL,P).
	
horizontalTwoInThreePair(TmpL,HTL,P) :-
	horizontalTwoInThree(X1,Y1,X3,Y1,P) &
	not .member(pairPos(pos(X1,Y1), pos(X3,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y1))], TmpL, TmpL2) &
	horizontalTwoInThreePair(TmpL2,HTL,P).
	
horizontalTwoInThreePair(TmpL,HTL,P) :- HTL = TmpL.


// Rules for two chips in diagonal of the form X[]X with no chip in between 
diagonalTwoInThreePair([],DTL,P) :-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y3))],[],TmpL) &
	diagonalTwoInThreePair(TmpL,DTL,P).
	 
diagonalTwoInThreePair(TmpL,DTL,P) :-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	not .member(pairPos(pos(X1,Y1), pos(X3,Y3)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y3))], TmpL, TmpL2) &
	diagonalTwoInThreePair(TmpL2,DTL,P).
	
diagonalTwoInThreePair(TmpL,DTL,P) :- DTL = TmpL.


// Rules for two chips in vertical of the form X[][]X with no chips in between
verticalTwoInFourPair([],VTL,P) :-
	verticalTwoInFour(X1,Y1,X1,Y4,P) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y4))],[],TmpL) &
	verticalTwoInFourPair(TmpL,VTL,P).
	
verticalTwoInFourPair(TmpL,VTL,P) :-
	verticalTwoInFour(X1,Y1,X1,Y4,P) &
	not .member(pairPos(pos(X1,Y1), pos(X1,Y4)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X1,Y4))], TmpL, TmpL2) &
	verticalTwoInFourPair(TmpL2,VTL,P).
	
verticalTwoInFourPair(TmpL,VTL,P) :- VTL = TmpL.


// Rules for two chips in horizontal of the form X[][]X with no chips in between 
horizontalTwoInFourPair([],HTL,P) :-
	horizontalTwoInFour(X1,Y1,X4,Y1,P) &
	.concat([pairPos(pos(X1,Y1), pos(X4,Y1))],[],TmpL) &
	horizontalTwoInFourPair(TmpL,HTL,P).
	
horizontalTwoInFourPair(TmpL,HTL,P) :-
	horizontalTwoInFour(X1,Y1,P,X4,Y1,P) &
	not .member(pairPos(pos(X1,Y1), pos(X4,Y1)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X4,Y1))], TmpL, TmpL2) &
	horizontalTwoInFourPair(TmpL2,HTL,P).
	
horizontalTwoInFourPair(TmpL,HTL,P) :- HTL = TmpL.


// Rules for two chips in diagonal of the form X[][]X with no chips in between 
diagonalTwoInFourPair([],DTL,P) :-
	diagonalTwoInFour(X1,Y1,X4,Y4,P) &
	.concat([pairPos(pos(X1,Y1), pos(X4,Y4))],[],TmpL) &
	diagonalTwoInFourPair(TmpL,DTL,P).
	 
diagonalTwoInFourPair(TmpL,DTL,P) :-
	diagonalTwoInFour(X1,Y1,X4,Y4,P) &
	not .member(pairPos(pos(X1,Y1), pos(X4,Y4)), TmpL) &
	.concat([pairPos(pos(X1,Y1), pos(X4,Y4))], TmpL, TmpL2) &
	diagonalTwoInFourPair(TmpL2,DTL,P).
	
diagonalTwoInFourPair(TmpL,DTL,P) :- DTL = TmpL.


// Rule to get a vertical pair XX
vertical(X1,Y1,X1,Y2,P) :-
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X1,Y2,P)[source(percept)] &
	(Y2 = Y1 + 1).


// Rule to get a horizontal pair XX
horizontal(X1,Y1,X2,Y1,P) :-
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X2,Y1,P)[source(percept)] &
	(X2 = X1 + 1).


// Rule to get a diagonal pair XX
diagonal(X1,Y1,X2,Y2,P) :-
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X2,Y2,P)[source(percept)] &
	( ((X2 = X1 + 1) & (Y2 = Y1 + 1)) |
	  ((X2 = X1 - 1) & (Y2 = Y1 + 1)) ).


// Rule to get a vertical pair X[]X
verticalTwoInThree(X1,Y1,X1,Y3,P) :-
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X1,Y2,0)[source(percept)] &
	tablero(X1,Y3,P)[source(percept)] &
	(Y3 = Y1 + 2) &
	(Y2 = Y1 + 1).


// Rule to get a horizontal pair X[]X
horizontalTwoInThree(X1,Y1,X3,Y1,P) :- 
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X2,Y1,0)[source(percept)] &
	tablero(X3,Y1,P)[source(percept)] &
	(X3 = X1 + 2) &
	(X2 = X1 + 1).


// Rule to get a diagonal pair X[]X
diagonalTwoInThree(X1,Y1,X3,Y3,P) :- 
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X2,Y2,0)[source(percept)] &
	tablero(X3,Y3,P)[source(percept)] &
	( ((X3 = X1 + 2) & (X2 = X1 + 1)  &
	   (Y3 = Y1 + 2) & (Y2 = Y1 + 1)) |
	  ((X3 = X1 - 2) & (X2 = X1 - 1)  &
	   (Y3 = Y1 + 2) & (Y2 = Y1 + 1)) ).


// Rule to get a vertical pair X[][]X
verticalTwoInFour(X1,Y1,X1,Y4,P) :- 
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X1,Y2,0)[source(percept)] &
	tablero(X1,Y3,0)[source(percept)] &
	tablero(X1,Y4,P)[source(percept)] &
	(Y4 = Y1 + 3) &
	(Y3 = Y1 + 2) &
	(Y2 = Y1 + 1).


// Rule to get a horizontal pair X[][]X
horizontalTwoInFour(X1,Y1,X4,Y1,P) :-
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X2,Y1,0)[source(percept)] &
	tablero(X3,Y1,0)[source(percept)] &
	tablero(X4,Y1,P)[source(percept)] &
	(X4 = X1 + 3) &
	(X3 = X1 + 2) &
	(X2 = X1 + 1).


// Rule to get a diagonal pair X[][]X
diagonalTwoInFour(X1,Y1,X4,Y4,P):-
	tablero(X1,Y1,P)[source(percept)] &
	tablero(X2,Y2,0)[source(percept)] &
	tablero(X3,Y3,0)[source(percept)] &
	tablero(X4,Y4,P)[source(percept)] &
	( ( (X4 = X1 + 3) & (X3 = X1 + 2) & (X2 = X1 + 1) &
	    (Y4 = Y1 + 3) & (Y3 = Y1 + 2) & (Y2 = Y1 + 1) ) |
	  ( (X4 = X1 - 3) & (X3 = X1 - 2) & (X2 = X1 - 1) &
	    (Y4 = Y1 + 3) & (Y3 = Y1 + 2) & (Y2 = Y1 + 1) ) ).


// Top left corner
closestCenterDiagonal(X,Y):-
	actualMovement(X1,Y1) &
	(X1 = 0) &
	(Y1 = 0) &
	(X = X1 + 2) &
	(Y = Y1 + 2).

// Top right corner
closestCenterDiagonal(X,Y):-
	actualMovement(X1,Y1) &
	(X1 = 7) &
	(Y1 = 0) &
	(X = X1 - 2) &
	(Y = Y1 + 2).

// Bottom left corner
closestCenterDiagonal(X,Y):-
	actualMovement(X1,Y1) &
	(X1 = 0) &
	(Y1 = 7) &
	(X = X1 + 2) &
	(Y = Y1 - 2).

// Bottom right corner
closestCenterDiagonal(X,Y):-
	actualMovement(X1,Y1) &
	(X1 = 7) &
	(Y1 = 7) &
	(X = X1 - 2) &
	(Y = Y1 - 2).

// Top left quadrant
closestCenterDiagonal(X,Y):-
	actualMovement(X1,Y1) &
	(X1 <= 3) &
	(Y1 <= 3) &
	(X = X1 + 1) &
	(Y = Y1 + 1). 

// Top right quadrant
closestCenterDiagonal(X,Y):-
	actualMovement(X1,Y1) &
	(X1 >= 4) &
	(Y1 <= 3) &
	(X = X1 - 1) &
	(Y = Y1 + 1). 

// Bottom left quadrant
closestCenterDiagonal(X,Y):-
	actualMovement(X1,Y1) &
	(X1 <= 3) &
	(Y1 >= 4) &
	(X = X1 + 1) &
	(Y = Y1 - 1). 

// Bottom right quadrant
closestCenterDiagonal(X,Y):-
	actualMovement(X1,Y1) &
	(X1 >= 4) &
	(Y1 >= 4) &
	(X = X1 - 1) &
	(Y = Y1 - 1). 

// If you have 1 or more winning positions 
decideMovement(X,Y):-
	estrategia(jugarAGanar)[source(percept)] &
	listWinPositions([pos(X,Y)|_]).

//If you have only one losing position
decideMovement(X,Y):-
	estrategia(jugarAGanar)[source(percept)] &
	listLosePositions([pos(X,Y)]).

//If you have one or more losing position
//decideMovement(X,Y):-
//	estrategia(jugarAGanar)[source(percept)] &
//	listLosePositions(L) &
//	.length(L,N) &
//	N > 1 &
//	.

// If you win with a triple []xXXx[]
decideMovement(X,Y):-
	estrategia(jugarAGanar)[source(percept)] &
	pairs(PL) &
	not .empty(PL) &
	winnningTriple(LP,X,Y).

// If you win with a triple of the form []XxX[]
decideMovement(X,Y):-
	estrategia(jugarAGanar)[source(percept)] &
	twoInThreePairs(PL) &
	not .empty(PL) &
	winnningTripleTwoInThree(PL,X,Y).

// Make the opponent cover you
decideMovement(X,Y):-
	estrategia(jugarAGanar)[source(percept)] &
	twoInFourPairs(LP) &
	not .empty(LP) &
	notWinnningTriple(LP,X,Y).


decideMovement(X,Y):-
	estrategia(jugarAGanar)[source(percept)] &
	pairs(LP) &
	not .empty(LP) &
	winnningTwoInThreeTriple(LP,X,Y).








//FALTA TESTEAR
checkRepeatedElements([pos(X,Y),pos(X,Y)],X,Y).
checkRepeatedElements([pos(X,Y),pos(X2,Y2)],-1,-1):-
	not (X = X2) &
	not (Y = Y2).

checkRepeatedElements([pos(X,Y)|Tail],X,Y):-
	.member(pos(X,Y),Tail).

checkRepeatedElements([pos(X,Y)|Tail],X2,Y2):-
	not .member(pos(X,Y),Tail) &
	checkRepeatedElements(Tail,X2,Y2).


//FALTA TESTEAR
listForcePositions(L):-
	listMyForceBlockPositionTrioVertical(L1) &
	listMyForceBlockPositionTrioHorizontal(L2) &
	listMyForceBlockPositionTrioDiagonal(L3) &
	listMyForceBlockPositionTwoInThree(L4) &
	listMyForceBlockPositionTwoInFour(L5) &
	.concat(L1,L2,LT) &
	.concat(L3,L4,LT2) &
	.concat(L5,LT,LT3) &
	.concat(LT2,LT3,L).


//FALTA TESTEAR
listMyForceBlockPositionTrioVertical(L):-
	player(P) &
	opponent(O) &
	listForceBlockPositionVerticalTopBottom([],L1,P,O) &
	listForceBlockPositionVerticalBottomTop([],L2,P,O) &
	.concat(L1,L2,L).


//FALTA TESTEAR
listMyForceBlockPositionTrioHorizontal(L):-
	player(P) &
	opponent(O) &
	listForceBlockPositionHorizontalLeftRight([],L1,P,O) &
	listForceBlockPositionHorizontalRightLeft([],L2,P,O) &
	.concat(L1,L2,L).


//FALTA TESTEAR
listMyForceBlockPositionTrioDiagonal(L):-
	player(P) &
	opponent(O) &
	listForceBlockPositionDiagonalTopLeftBottomRight([],L1,P,O) &
	listForceBlockPositionDiagonalBottomRightTopLeft([],L2,P,O) &
	listForceBlockPositionDiagonalTopRightBottomLeft([],L3,P,O) &
	listForceBlockPositionDiagonalBottomLeftTopRight([],L4,P,O) &
	.concat(L1,L2,LT) &
	.concat(L3,L4,LT2) &
	.concat(LT,LT2,L).


//FALTA TESTEAR
listMyForceBlockPositionTwoInThree(L):-
	player(P) &
	opponent(O) &
	listForceBlockPositionVerticalTwoInThree([],L1,P,O) &
	listForceBlockPositionHorizontalTwoInThree([],L2,P,O) &
	listForceBlockPositionDiagonalTwoInThreeLeft([],L3,P,O) &
	listForceBlockPositionDiagonalTwoInThreeRight([],L4,P,O) &
	.concat(L1,L2,LT) &
	.concat(L3,L4,LT2) &
	.concat(LT,LT2,L).


//FALTA TESTEAR
listMyForceBlockPositionTwoInFour(L):-
	player(P) &
	opponent(O) &
	listForceBlockPositionVerticalTwoInFour([],L1,P,O) &
	listForceBlockPositionHorizontalTwoInFour([],L2,P,O) &
	listForceBlockPositionDiagonalTwoInFourLeft([],L3,P,O) &
	listForceBlockPositionDiagonalTwoInFourRight([],L4,P,O) &
	.concat(L1,L2,LT) &
	.concat(L3,L4,LT2) &
	.concat(LT,LT2,L).








winnningTriple(L):-
	listTripleVertical([],L1,P) &
	listTripleHorizontal([],L2,P) &
	listTripleDiagonal([],L3,P) &
	.concat(L1,L2,LT) &
	.concat(LT,L3,L).


winnningTripleTwoInThree(L):-
	listTripleVertical([],L1,P) &
	listTripleHorizontal([],L2,P) &
	listTripleDiagonal([],L3,P) &
	.union(L1,L2,LT) &
	.union(LT,L3,L).

// List all possible winning triples in vertical top
listTripleVerticalTop([],L,P):-
	vertical(X,Y1,X,Y2,P) &
	tablero(X,YD,0)[source(percept)] &
	tablero(X,YD1,0)[source(percept)] &
	tablero(X,YD0,0)[source(percept)] &
	(YD = Y2-2) &
	(YD1 = Y2-3) &
	(YD0 = Y2+1) &
	.concat([pos(X,YD)],[],TL) &
	listTripleVerticalTop(TL,L,P).

listTripleVerticalTop(TL,L,P):-
	vertical(X,Y1,X,Y2,P) &
	tablero(X,YD,0)[source(percept)] &
	tablero(X,YD1,0)[source(percept)] &
	tablero(X,YD0,0)[source(percept)] &
	(YD = Y2-2) &
	(YD1 = Y2-3) &
	(YD0 = Y2+1) &
	not .member(pos(X,YD),TL) &
	.concat([pos(X,YD)],TL,TL2) &
	listTripleVerticalTop(TL2,L,P).

listTripleVerticalTop([],[],_).
listTripleVerticalTop(TL,L,_):- L = TL.

// List all possible losing triples in vertical bottom
listTripleVerticalBottom([],L,P):-
	vertical(X,Y1,X,Y2,P) &
	tablero(X,YD,0)[source(percept)] &
	tablero(X,YD1,0)[source(percept)] &
	tablero(X,YD0,0)[source(percept)] &
	(YD=Y2+1)  &
	(YD1=Y2+2) &
	(YD0=Y2-2) &
	.concat([pos(X,YD)],[],TL) & 
	listTripleVerticalBottom(TL,L,P).

listTripleVerticalBottom(TL,L,P):-
	vertical(X,Y1,X,Y2,P) &
	tablero(X,YD,0)[source(percept)] &
	tablero(X,YD1,0)[source(percept)] &
	tablero(X,YD0,0)[source(percept)] &
	(YD=Y2+1)  &
	(YD1=Y2+2) &
	(YD0=Y2-2) &
	not .member(pos(X,YD),TL) &
	.concat([pos(X,YD)],TL,TL2) &
	listTripleVerticalBottom(TL2,L,P).

listTripleVerticalBottom([],[],_).
listTripleVerticalBottom(TL,L,_):- L = TL.


// List all possible triples in horizontal left
listTripleHorizontalLeft([],L,P):-
	horizontal(X1,Y,X2,Y,P) &
	tablero(XD,Y,0)[source(percept)] &
	tablero(XD1,Y,0)[source(percept)] &
	tablero(XD0,Y,0)[source(percept)] &
	(XD = X2-2) &
    (XD1 = X2-3) &
    (XD0 = X2+1) &
	.concat([pos(XD,Y)],[],TL) &
	listTripleHorizontalLeft(TL,L,P).

listTripleHorizontalLeft(TL,L,P):-
	horizontal(X1,Y,X2,Y,P) &
	tablero(XD,Y,0)[source(percept)] &
	tablero(XD1,Y,0)[source(percept)] &
	tablero(XD0,Y,0)[source(percept)] &
	(XD = X2-2) &
    (XD1 = X2-3) &
    (XD0 = X2+1) &
	not .member(pos(XD,Y),TL) &
	.concat([pos(XD,Y)],TL,TL2) &
	listTripleHorizontalLeft(TL2,L,P).

listTripleHorizontalLeft([],[],_).
listTripleHorizontalLeft(TL,L,_):- L = TL.


// List all possible triples in horizontal right
listTripleHorizontalRight([],L,P):-
	horizontal(X1,Y,X2,Y,P) &
	tablero(XD,Y,0)[source(percept)] &
	tablero(XD1,Y,0)[source(percept)] &
	tablero(XD0,Y,0)[source(percept)] &
	(XD = X2+1) &
	(XD1 = X2+2) &
	(XD0 = X2-2) &
	.concat([pos(XD,Y)],[],TL) &
	listTripleHorizontalRight(TL,L,P).

listTripleHorizontalRight(TL,L,P):-
	horizontal(X1,Y,X2,Y,P) &
	tablero(XD,Y,0)[source(percept)] &
	tablero(XD1,Y,0)[source(percept)] &
	tablero(XD0,Y,0)[source(percept)] &
	(XD = X2+1) &
	(XD1 = X2+2) &
	(XD0 = X2-2) &
	not .member(pos(XD,Y),TL) &
	.concat([pos(XD,Y)],TL,TL2) &
	listTripleHorizontalRight(TL2,L,P).

listTripleHorizontalRight([],[],_).
listTripleHorizontalRight(TL,L,_):- L = TL.


// List all possible losing triples in diagonal top left
listTripleDiagonalTopLeft([],L,P):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(XD,YD,0)[source(percept)] &
	tablero(XD1,YD1,0)[source(percept)] &
	tablero(XD0,YD0,0)[source(percept)] &
	(XD = X2-2) &
	(YD= Y2-2) & 
    (XD1 = X2-3) &
    (YD1 = Y2-3) & 
    (XD0 = X2+1) &
    (YD0 = Y2+1) &
	.concat([pos(XD,YD)],[],TL) & 
	listTripleDiagonalTopLeft(TL,L,P).

listTripleDiagonalTopLeft(TL,L,P):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(XD,YD,0)[source(percept)] &
	tablero(XD1,YD1,0)[source(percept)] &
	tablero(XD0,YD0,0)[source(percept)] &
	(XD = X2-2) &
	(YD = Y2-2) & 
    (XD1 = X2-3) &
    (YD1 = Y2-3) & 
    (XD0 = X2+1) &
    (YD0 = Y2+1) &
	not .member(pos(XD,YD),TL) &
	.concat([pos(XD,YD)],TL,TL2) &
	listTripleDiagonalTopLeft(TL2,L,P).

listTripleDiagonalTopLeft([],[],_).
listTripleDiagonalTopLeft(TL,L,_):- L = TL.


// List all possible losing triples in diagonal bottom right
listTripleDiagonalBottomRight([],L,P):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(XD,YD,0)[source(percept)] &
	tablero(XD1,YD1,0)[source(percept)] &
	tablero(XD0,YD0,0)[source(percept)] &
	(X2 = X1+1) &
	(Y2 = Y1+1) & 
	(XD = X2+1) &
  	(YD = Y2+1) & 
    (XD1 = X2+2) &
    (YD1 = Y2+2) & 
    (XD0 = X2-2) &
    (YD0 = Y2-2) &
	.concat([pos(XD,YD)],[],TL) & 
	listTripleDiagonalBottomRight(TL,L,P).

listTripleDiagonalBottomRight(TL,L,P):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(XD,YD,0)[source(percept)] &
	tablero(XD1,YD1,0)[source(percept)] &
	tablero(XD0,YD0,0)[source(percept)] &
	(X2 = X1+1) &
	(Y2 = Y1+1) & 
	(XD = X2+1) &
  	(YD = Y2+1) & 
    (XD1 = X2+2) &
    (YD1 = Y2+2) & 
    (XD0 = X2-2) &
    (YD0 = Y2-2) &
	not .member(pos(XD,YD),TL) &
	.concat([pos(XD,YD)],TL,TL2) &
	listTripleDiagonalBottomRight(TL2,L,P).

listTripleDiagonalBottomRight([],[],_).
listTripleDiagonalBottomRight(TL,L,_):- L = TL.


// List all possible losing triples in diagonal top right
listTripleDiagonalTopRight([],L,P):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(XD,YD,0)[source(percept)] &
	tablero(XD1,YD1,0)[source(percept)] &
	tablero(XD0,YD0,0)[source(percept)] &
	(XD = X2+2) &
	(YD = Y2-2) &
	(XD1 = X2+3) &
	(YD1 = Y2-3) &
	(XD0 = X2-1) &
	(YD0 = Y2+1) &
	.concat([pos(XD,YD)],[],TL) & 
	listTripleDiagonalTopRight(TL,L,P).

listTripleDiagonalTopRight(TL,L,P):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(XD,YD,0)[source(percept)] &
	tablero(XD1,YD1,0)[source(percept)] &
	tablero(XD0,YD0,0)[source(percept)] &
	(XD = X2+2) &
	(YD = Y2-2) &
	(XD1 = X2+3) &
	(YD1 = Y2-3) &
	(XD0 = X2-1) &
	(YD0 = Y2+1) &
	not .member(pos(XD,YD),TL) &
	.concat([pos(XD,YD)],TL,TL2) &
	listTripleDiagonalTopRight(TL2,L,P).

listTripleDiagonalTopRight([],[],_).
listTripleDiagonalTopRight(TL,L,_):- L = TL.


// List all possible losing triples in diagonal bottom left
listTripleDiagonalBottomLeft([],L,P):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(XD,YD,0)[source(percept)] &
	tablero(XD1,YD1,0)[source(percept)] &
	tablero(XD0,YD0,0)[source(percept)] &
	(X2 = X1-1) &
	(Y2 = Y1+1) &
	(XD = X2-1) &
	(YD = Y2+1) &
	(XD1 = X2-2) &
	(YD1 = Y2+2) &
	(XD0 = X2+2) &
	(YD0 = Y2-2) &
	.concat([pos(XD,YD)],[],TL) & 
	listTripleDiagonalBottomLeft(TL,L,P).

listTripleDiagonalBottomLeft(TL,L,P):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(XD,YD,0)[source(percept)] &
	tablero(XD1,YD1,0)[source(percept)] &
	tablero(XD0,YD0,0)[source(percept)] &
	(X2 = X1-1) &
	(Y2 = Y1+1) &
	(XD = X2-1) &
	(YD = Y2+1) &
	(XD1 = X2-2) &
	(YD1 = Y2+2) &
	(XD0 = X2+2) &
	(YD0 = Y2-2) &
	not .member(pos(XD,YD),TL) &
	.concat([pos(XD,YD)],TL,TL2) &
	listTripleDiagonalBottomLeft(TL2,L,P).

listTripleDiagonalBottomLeft([],[],_).
listTripleDiagonalBottomLeft(TL,L,_):- L = TL.


listTripleTwoInThreeVertical([],L,P):-
	verticalTwoInThree(X,Y1,X,Y3,P) &
	tablero(X,YD0,0)[source(percept)] &
	tablero(X,YD4,0)[source(percept)] &
	(YD0 = Y3-3) &
	(YD4 = Y3+1) &
	(YD = Y3-1) &
	.concat([pos(X,YD)],[],TL) &
	listTripleTwoInThreeVertical(TL,L,P).

listTripleTwoInThreeVertical(TL,L,P):-
	verticalTwoInThree(X,Y1,X,Y3,P) &
	tablero(X,YD0,0)[source(percept)] &
	tablero(X,YD4,0)[source(percept)] &
	(YD0 = Y3-3) &
	(YD4 = Y3+1) &
	(YD = Y3-1) &
	not .member(pos(X,YD),TL) &
	.concat([pos(X,YD)],TL,TL2) &
	listTripleTwoInThreeVertical(TL2,L,P).

listTripleTwoInThreeVertical([],[],_).
listTripleTwoInThreeVertical(TL,L,_):- L = TL.


listTripleTwoInThreeHorizontal([],L,P):-
	horizontalTwoInThree(X1,Y,X3,Y,P) &
	tablero(XD0,Y,0)[source(percept)] &
	tablero(XD4,Y,0)[source(percept)] &
	(XD0 = X3-3) &
	(XD4 = X3+1) &
	(XD = X3-1) &
	.concat([pos(XD,Y)],[],TL) &
	listTripleTwoInThreeHorizontal(TL,L,P).

listTripleTwoInThreeHorizontal(TL,L,P):-
	horizontalTwoInThree(X1,Y,X3,Y,P) &
	tablero(XD0,Y,0)[source(percept)] &
	tablero(XD4,Y,0)[source(percept)] &
	(XD0 = X3-3) &
	(XD4 = X3+1) &
	(XD = X3-1) &
	not .member(pos(XD,Y),TL) &
	.concat([pos(XD,Y)],TL,TL2) &
	listTripleTwoInThreeHorizontal(TL2,L,P).

listTripleTwoInThreeHorizontal([],[],_).
listTripleTwoInThreeHorizontal(TL,L,_):- L = TL.


listTripleTwoInThreeDiagonalLeft([],L,P):-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	tablero(XD0,YD0,0)[source(percept)] &
	tablero(XD4,YD4,0)[source(percept)] &
	(XD0 = X3-3) &
	(XD4 = X3+1) &
	(YD0 = Y3-3) &
	(YD4 = Y3+1) &
	(XD = X3-1) &
	(YD = Y3-1) &
	.concat([pos(XD,YD)],[],TL) &
	listTripleTwoInThreeDiagonalLeft(TL,L,P).

listTripleTwoInThreeDiagonalLeft(TL,L,P):-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	tablero(XD0,YD0,0)[source(percept)] &
	tablero(XD4,YD4,0)[source(percept)] &
	(XD0 = X3-3) &
	(XD4 = X3+1) &
	(YD0 = Y3-3) &
	(YD4 = Y3+1) &
	(XD = X3-1) &
	(YD = Y3-1) &
	not .member(pos(XD,YD),TL) &
	.concat([pos(XD,YD)],TL,TL2) &
	listTripleTwoInThreeDiagonalLeft(TL2,L,P).

listTripleTwoInThreeDiagonalLeft([],[],_).
listTripleTwoInThreeDiagonalLeft(TL,L,_):- L = TL.


listTripleTwoInThreeDiagonalRight([],L,P):-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	tablero(XD0,YD0,0)[source(percept)] &
	tablero(XD4,YD4,0)[source(percept)] &
	(XD0 = X3+3) &
	(XD4 = X3-1) &
	(YD0 = Y3-3) &
	(YD4 = Y3+1) &
	(XD = X3+1) &
	(YD = Y3-1) &
	.concat([pos(XD,YD)],[],TL) &
	listTripleTwoInThreeDiagonalRight(TL,L,P).

listTripleTwoInThreeDiagonalRight(TL,L,P):-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	tablero(XD0,YD0,0)[source(percept)] &
	tablero(XD4,YD4,0)[source(percept)] &
	(XD0 = X3+3) &
	(XD4 = X3-1) &
	(YD0 = Y3-3) &
	(YD4 = Y3+1) &
	(XD = X3+1) &
	(YD = Y3-1) &
	not .member(pos(XD,YD),TL) &
	.concat([pos(XD,YD)],TL,TL2) &
	listTripleTwoInThreeDiagonalRight(TL2,L,P).

listTripleTwoInThreeDiagonalRight([],[],_).
listTripleTwoInThreeDiagonalRight(TL,L,_):- L = TL.


listForceBlockPositionVerticalTopBottom([],L,P,R):-
	vertical(X,Y1,X,Y2,P) &
	tablero(X,Y3,0)[source(percept)] &
	tablero(X,Y4,0)[source(percept)] &
	tablero(X,YR,R)[source(percept)] &
	(Y3 = Y2 + 1) &
	(Y4 = Y2 + 2) &
	(YR = Y2 - 2) &
	.concat([pos(X,Y3),pos(X,Y4)],[],TL) &
	listForceBlockPositionVerticalTopBottom(TL,L,P,R).

listForceBlockPositionVerticalTopBottom(TL,L,P,R):-
	vertical(X,Y1,X,Y2,P) &
	tablero(X,Y3,0)[source(percept)] &
	tablero(X,Y4,0)[source(percept)] &
	tablero(X,YR,R)[source(percept)] &
	(Y3 = Y2 + 1) &
	(Y4 = Y2 + 2) &
	(YR = Y2 - 2) &
	not .member(pos(X,Y3),TL) &
	not .member(pos(X,Y4),TL) &
	.concat([pos(X,Y3),pos(X,Y4)],TL,TL2) &
	listForceBlockPositionVerticalTopBottom(TL2,L,P,R).

listForceBlockPositionVerticalTopBottom([],[],_,_).
listForceBlockPositionVerticalTopBottom(TL,L,_,_):- L = TL.


listForceBlockPositionVerticalBottomTop([],L,P,R):-
	vertical(X,Y1,X,Y2,P) &
	tablero(X,Y3,0)[source(percept)] &
	tablero(X,Y4,0)[source(percept)] &
	tablero(X,YR,R)[source(percept)] &
	(Y3 = Y2 - 2) &
	(Y4 = Y2 - 3) &
	(YR = Y2 + 1) &
	.concat([pos(X,Y3),pos(X,Y4)],[],TL) &
	listForceBlockPositionVerticalBottomTop(TL,L,P,R).

listForceBlockPositionVerticalBottomTop(TL,L,P,R):-
	vertical(X,Y1,X,Y2,P) &
	tablero(X,Y3,0)[source(percept)] &
	tablero(X,Y4,0)[source(percept)] &
	tablero(X,YR,R)[source(percept)] &
	(Y3 = Y2 - 2) &
	(Y4 = Y2 - 3) &
	(YR = Y2 + 1) &
	not .member(pos(X,Y3),TL) &
	not .member(pos(X,Y4),TL) &
	.concat([pos(X,Y3),pos(X,Y4)],TL,TL2) &
	listForceBlockPositionVerticalBottomTop(TL2,L,P,R).

listForceBlockPositionVerticalBottomTop([],[],_,_).
listForceBlockPositionVerticalBottomTop(TL,L,_,_):- L = TL.


listForceBlockPositionHorizontalLeftRight([],L,P,R):-
	horizontal(X1,Y,X2,Y,P) &
	tablero(X3,Y,0)[source(percept)] &
	tablero(X4,Y,0)[source(percept)] &
	tablero(XR,Y,R)[source(percept)] &
	(X3 = X2 + 1) &
	(X4 = X2 + 2) &
	(XR = X2 - 2) &
	.concat([pos(X3,Y),pos(X4,Y)],[],TL) &
	listForceBlockPositionHorizontalLeftRight(TL,L,P,R).

listForceBlockPositionHorizontalLeftRight(TL,L,P,R):-
	horizontal(X1,Y,X2,Y,P) &
	tablero(X3,Y,0)[source(percept)] &
	tablero(X4,Y,0)[source(percept)] &
	tablero(XR,Y,R)[source(percept)] &
	(X3 = X2 + 1) &
	(X4 = X2 + 2) &
	(XR = X2 - 2) &
	not .member(pos(X3,Y),TL) &
	not .member(pos(X4,Y),TL) &
	.concat([pos(X3,Y),pos(X4,Y)],TL,TL2) &
	listForceBlockPositionHorizontalLeftRight(TL2,L,P,R).

listForceBlockPositionHorizontalLeftRight([],[],_,_).
listForceBlockPositionHorizontalLeftRight(TL,L,_,_):- L = TL.


listForceBlockPositionHorizontalRightLeft([],L,P,R):-
	horizontal(X1,Y,X2,Y,P) &
	tablero(X3,Y,0)[source(percept)] &
	tablero(X4,Y,0)[source(percept)] &
	tablero(XR,Y,R)[source(percept)] &
	(X3 = X2 - 2) &
	(X4 = X2 - 3) &
	(XR = X2 + 1) &
	.concat([pos(X3,Y),pos(X4,Y)],[],TL) &
	listForceBlockPositionHorizontalRightLeft(TL,L,P,R).

listForceBlockPositionHorizontalRightLeft(TL,L,P,R):-
	horizontal(X1,Y,X2,Y,P) &
	tablero(X3,Y,0)[source(percept)] &
	tablero(X4,Y,0)[source(percept)] &
	tablero(XR,Y,R)[source(percept)] &
	(X3 = X2 - 2) &
	(X4 = X2 - 3) &
	(XR = X2 + 1) &
	not .member(pos(X3,Y),TL) &
	not .member(pos(X4,Y),TL) &
	.concat([pos(X3,Y),pos(X4,Y)],TL,TL2) &
	listForceBlockPositionHorizontalRightLeft(TL2,L,P,R).

listForceBlockPositionHorizontalRightLeft([],[],_,_).
listForceBlockPositionHorizontalRightLeft(TL,L,_,_):- L = TL.


listForceBlockPositionDiagonalTopLeftBottomRight([],L,P,R):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(X3,Y3,0)[source(percept)] &
	tablero(X4,Y4,0)[source(percept)] &
	tablero(XR,YR,R)[source(percept)] &
	(X3 = X2 + 1) &
	(X4 = X2 + 2) &
	(XR = X2 - 2) &
	(Y3 = Y2 + 1) &
	(Y4 = Y2 + 2) &
	(YR = Y2 - 2) &
	.concat([pos(X3,Y3),pos(X4,Y4)],[],TL) &
	listForceBlockPositionDiagonalTopLeftBottomRight(TL,L,P,R).

listForceBlockPositionDiagonalTopLeftBottomRight(TL,L,P,R):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(X3,Y3,0)[source(percept)] &
	tablero(X4,Y4,0)[source(percept)] &
	tablero(XR,YR,R)[source(percept)] &
	(X3 = X2 + 1) &
	(X4 = X2 + 2) &
	(XR = X2 - 2) &
	(Y3 = Y2 + 1) &
	(Y4 = Y2 + 2) &
	(YR = Y2 - 2) &
	not .member(pos(X3,Y3),TL) &
	not .member(pos(X4,Y4),TL) &
	.concat([pos(X3,Y3),pos(X4,Y4)],TL,TL2) &
	listForceBlockPositionDiagonalTopLeftBottomRight(TL2,L,P,R).

listForceBlockPositionDiagonalTopLeftBottomRight([],[],_,_).
listForceBlockPositionDiagonalTopLeftBottomRight(TL,L,_,_):- L = TL.


listForceBlockPositionDiagonalBottomRightTopLeft([],L,P,R):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(X3,Y3,0)[source(percept)] &
	tablero(X4,Y4,0)[source(percept)] &
	tablero(XR,YR,R)[source(percept)] &
	(X3 = X2 - 2) &
	(X4 = X2 - 3) &
	(XR = X2 + 1) &
	(Y3 = Y2 - 2) &
	(Y4 = Y2 - 3) &
	(YR = Y2 + 1) &
	.concat([pos(X3,Y3),pos(X4,Y4)],[],TL) &
	listForceBlockPositionDiagonalBottomRightTopLeft(TL,L,P,R).

listForceBlockPositionDiagonalBottomRightTopLeft(TL,L,P,R):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(X3,Y3,0)[source(percept)] &
	tablero(X4,Y4,0)[source(percept)] &
	tablero(XR,YR,R)[source(percept)] &
	(X3 = X2 - 2) &
	(X4 = X2 - 3) &
	(XR = X2 + 1) &
	(Y3 = Y2 - 2) &
	(Y4 = Y2 - 3) &
	(YR = Y2 + 1) &
	not .member(pos(X3,Y3),TL) &
	not .member(pos(X4,Y4),TL) &
	.concat([pos(X3,Y3),pos(X4,Y4)],TL,TL2) &
	listForceBlockPositionDiagonalBottomRightTopLeft(TL2,L,P,R).

listForceBlockPositionDiagonalBottomRightTopLeft([],[],_,_).
listForceBlockPositionDiagonalBottomRightTopLeft(TL,L,_,_):- L = TL.


listForceBlockPositionDiagonalTopRightBottomLeft([],L,P,R):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(X3,Y3,0)[source(percept)] &
	tablero(X4,Y4,0)[source(percept)] &
	tablero(XR,YR,R)[source(percept)] &
	(X3 = X2 - 1) &
	(X4 = X2 - 2) &
	(XR = X2 + 2) &
	(Y3 = Y2 + 1) &
	(Y4 = Y2 + 2) &
	(YR = Y2 - 2) &
	.concat([pos(X3,Y3),pos(X4,Y4)],[],TL) &
	listForceBlockPositionDiagonalTopRightBottomLeft(TL,L,P,R).

listForceBlockPositionDiagonalTopRightBottomLeft(TL,L,P,R):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(X3,Y3,0)[source(percept)] &
	tablero(X4,Y4,0)[source(percept)] &
	tablero(XR,YR,R)[source(percept)] &
	(X3 = X2 - 1) &
	(X4 = X2 - 2) &
	(XR = X2 + 2) &
	(Y3 = Y2 + 1) &
	(Y4 = Y2 + 2) &
	(YR = Y2 - 2) &
	not .member(pos(X3,Y3),TL) &
	çnot .member(pos(X4,Y4),TL) &
	.concat([pos(X3,Y3),pos(X4,Y4)],TL,TL2) &
	listForceBlockPositionDiagonalTopRightBottomLeft(TL2,L,P,R).

listForceBlockPositionDiagonalTopRightBottomLeft([],[],_,_).
listForceBlockPositionDiagonalTopRightBottomLeft(TL,L,_,_):- L = TL.


listForceBlockPositionDiagonalBottomLeftTopRight([],L,P,R):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(X3,Y3,0)[source(percept)] &
	tablero(X4,Y4,0)[source(percept)] &
	tablero(XR,YR,R)[source(percept)] &
	(X3 = X2 + 2) &
	(X4 = X2 + 3) &
	(XR = X2 - 1) &
	(Y3 = Y2 - 2) &
	(Y4 = Y2 - 3) &
	(YR = Y2 + 1) &
	.concat([pos(X3,Y3),pos(X4,Y4)],[],TL) &
	listForceBlockPositionDiagonalBottomLeftTopRight(TL,L,P,R).

listForceBlockPositionDiagonalBottomLeftTopRight(TL,L,P,R):-
	diagonal(X1,Y1,X2,Y2,P) &
	tablero(X3,Y3,0)[source(percept)] &
	tablero(X4,Y4,0)[source(percept)] &
	tablero(XR,YR,R)[source(percept)] &
	(X3 = X2 + 2) &
	(X4 = X2 + 3) &
	(XR = X2 - 1) &
	(Y3 = Y2 - 2) &
	(Y4 = Y2 - 3) &
	(YR = Y2 + 1) &
	not .member(pos(X3,Y3),TL) &
	not .member(pos(X4,Y4),TL) &
	.concat([pos(X3,Y3),pos(X4,Y4)],TL,TL2) &
	listForceBlockPositionDiagonalBottomLeftTopRight(TL2,L,P,R).

listForceBlockPositionDiagonalBottomLeftTopRight([],[],_,_).
listForceBlockPositionDiagonalBottomLeftTopRight(TL,L,_,_):- L = TL.


listForceBlockPositionVerticalTwoInThree([],L,P,R):-
	verticalTwoInThree(X,Y1,X,Y3,P) &
	tablero(X,Y0,0)[source(percept)] &
	tablero(X,YR,R)[source(percept)] &
	(((Y0 = Y3 + 1) &
	  (YR = Y3 - 3)) |
	 ((Y0 = Y3 - 3) &
	  (YR = Y3 + 1))) &
	(YD = Y3 - 1) &
	.concat([pos(X,YD),pos(X,Y0)],[],LT) &
	listForceBlockPositionVerticalTwoInThree(LT,L,P,R).

listForceBlockPositionVerticalTwoInThree(LT,L,P,R):-
	verticalTwoInThree(X,Y1,X,Y3,P) &
	tablero(X,Y0,0)[source(percept)] &
	tablero(X,YR,R)[source(percept)] &
	(((Y0 = Y3 + 1) &
	  (YR = Y3 - 3)) |
	 ((Y0 = Y3 - 3) &
	  (YR = Y3 + 1))) &
	(YD = Y3 - 1) &
	not .member(pos(X,YD),LT) &
	not .member(pos(X,Y0),LT) &
	.concat([pos(X,YD),pos(X,Y0)],LT,LT2) &
	listForceBlockPositionVerticalTwoInThree(LT2,L,P,R).

listForceBlockPositionVerticalTwoInThree([],[],_,_).
listForceBlockPositionVerticalTwoInThree(TL,L,_,_):- L = TL.


listForceBlockPositionHorizontalTwoInThree([],L,P,R):-
	horizontalTwoInThree(X1,Y,X3,Y,P) &
	tablero(X0,Y,0)[source(percept)] &
	tablero(XR,Y,R)[source(percept)] &
	(((X0 = X3 + 1) &
	  (XR = X3 - 3)) |
	 ((X0 = X3 - 3) &
	  (XR = X3 + 1))) &
	(XD = X3 - 1) &
	.concat([pos(XD,Y),pos(X0,Y)],[],LT) &
	listForceBlockPositionHorizontalTwoInThree(LT,L,P,R).

listForceBlockPositionHorizontalTwoInThree(LT,L,P,R):-
	horizontalTwoInThree(X1,Y,X3,Y,P) &
	tablero(X0,Y,0)[source(percept)] &
	tablero(XR,Y,R)[source(percept)] &
	(((X0 = X3 + 1) &
	  (XR = X3 - 3)) |
	 ((X0 = X3 - 3) &
	  (XR = X3 + 1))) &
	(XD = X3 - 1) &
	not .member(pos(XD,Y),LT) &
	not .member(pos(X0,Y),LT) &
	.concat([pos(XD,Y),pos(X0,Y)],LT,LT2) &
	listForceBlockPositionHorizontalTwoInThree(LT2,L,P,R).

listForceBlockPositionHorizontalTwoInThree([],[],_,_).
listForceBlockPositionHorizontalTwoInThree(TL,L,_,_):- L = TL.


listForceBlockPositionDiagonalTwoInThreeLeft([],L,P,R):-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	tablero(X0,Y0,0)[source(percept)] &
	tablero(XR,YR,R)[source(percept)] &
	(((X0 = X3 + 1) &
	  (XR = X3 - 3) &
	  (Y0 = Y3 + 1) &
	  (YR = Y3 - 3)) |
	 ((X0 = X3 - 3) &
	  (XR = X3 + 1) &
	  (Y0 = Y3 - 3) &
	  (YR = Y3 + 1))) &
	(XD = X3 - 1) &
	(YD = Y3 - 1) &
	.concat([pos(XD,YD),pos(X0,Y0)],[],LT) &
	listForceBlockPositionDiagonalTwoInThreeLeft(LT,L,P,R).

listForceBlockPositionDiagonalTwoInThreeLeft(LT,L,P,R):-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	tablero(X0,Y0,0)[source(percept)] &
	tablero(XR,YR,R)[source(percept)] &
	(((X0 = X3 + 1) &
	  (XR = X3 - 3) &
	  (Y0 = Y3 + 1) &
	  (YR = Y3 - 3)) |
	 ((X0 = X3 - 3) &
	  (XR = X3 + 1) &
	  (Y0 = Y3 - 3) &
	  (YR = Y3 + 1))) &
	(XD = X3 - 1) &
	(YD = Y3 - 1) &
	not .member(pos(XD,YD),LT) &
	not .member(pos(X0,Y0),LT) &
	.concat([pos(XD,YD),pos(X0,Y0)],LT,LT2) &
	listForceBlockPositionDiagonalTwoInThreeLeft(LT2,L,P,R).

listForceBlockPositionDiagonalTwoInThreeLeft([],[],_,_).
listForceBlockPositionDiagonalTwoInThreeLeft(TL,L,_,_):- L = TL.


listForceBlockPositionDiagonalTwoInThreeRight([],L,P,R):-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	tablero(X0,Y0,0)[source(percept)] &
	tablero(XR,YR,R)[source(percept)] &
	(((X0 = X3 - 1) &
	  (XR = X3 + 3) &
	  (Y0 = Y3 + 1) &
	  (YR = Y3 - 3)) |
	 ((X0 = X3 + 3) &
	  (XR = X3 - 1) &
	  (Y0 = Y3 - 3) &
	  (YR = Y3 + 1))) &
	(XD = X3 + 1) &
	(YD = Y3 - 1) &
	.concat([pos(XD,YD),pos(X0,Y0)],[],LT) &
	listForceBlockPositionDiagonalTwoInThreeRight(LT,L,P,R).

listForceBlockPositionDiagonalTwoInThreeRight(LT,L,P,R):-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	tablero(X0,Y0,0)[source(percept)] &
	tablero(XR,YR,R)[source(percept)] &
	(((X0 = X3 - 1) &
	  (XR = X3 + 3) &
	  (Y0 = Y3 + 1) &
	  (YR = Y3 - 3)) |
	 ((X0 = X3 + 3) &
	  (XR = X3 - 1) &
	  (Y0 = Y3 - 3) &
	  (YR = Y3 + 1))) &
	(XD = X3 + 1) &
	(YD = Y3 - 1) &
	not .member(pos(XD,YD),LT) &
	not .member(pos(X0,Y0),LT) &
	.concat([pos(XD,YD),pos(X0,Y0)],LT,LT2) &
	listForceBlockPositionDiagonalTwoInThreeRight(LT2,L,P,R).

listForceBlockPositionDiagonalTwoInThreeRight([],[],_,_).
listForceBlockPositionDiagonalTwoInThreeRight(TL,L,_,_):- L = TL.



listForceBlockPositionVerticalTwoInFour([],L,P,R):-
	verticalTwoInFour(X,Y0,X,Y3,P) &
	tablero(X,YR,R)[source(percept)] &
	(((YR = Y0 - 1) &
	  (YD = Y0 + 1) &
	  (YD1 = Y0 + 2) |
	 ((YR = Y3 + 1) &
	  (YD = Y3 - 1) &
	  (YD1 = Y3 - 2))) &
	.concat([pos(X,YD),pos(X,YD1)],[],LT) &
	listForceBlockPositionVerticalTwoInFour(LT,L,P,R).

listForceBlockPositionVerticalTwoInFour(LT,L,P,R):-
	verticalTwoInFour(X,Y0,X,Y3,P) &
	tablero(X,YR,R)[source(percept)] &
	(((YR = Y0 - 1) &
	  (YD = Y0 + 1) &
	  (YD1 = Y0 + 2) |
	 ((YR = Y3 + 1) &
	  (YD = Y3 - 1) &
	  (YD1 = Y3 - 2))) &
	not .member(pos(X,YD),LT) &
	not .member(pos(X,YD1),LT) &
	.concat([pos(X,YD),pos(X,YD1)],LT,LT2) &
	listForceBlockPositionVerticalTwoInFour(LT2,L,P,R).

listForceBlockPositionVerticalTwoInFour([],[],_,_).
listForceBlockPositionVerticalTwoInFour(LT,L,_,_):- L = LT.


listForceBlockPositionHorizontalTwoInFour([],L,P,R):-
	horizontalTwoInFour(X0,Y,X3,Y,P) &
	tablero(XR,Y,R)[source(percept)] &
	(((XR = X0 - 1) &
	  (XD = X0 + 1) &
	  (XD1 = X0 + 2)) |
	 ((XR = X3 + 1) &
	  (XD = X3 - 1) &
	  (XD1 = X3 - 2))) &
	.concat([pos(XD,Y),pos(XD1,Y)],[],LT) &
	listForceBlockPositionHorizontalTwoInFour(LT,L,P,R).

listForceBlockPositionHorizontalTwoInFour(LT,L,P,R):-
	horizontalTwoInFour(X0,Y,X3,Y,P) &
	tablero(XR,Y,R)[source(percept)] &
	(((XR = X0 - 1) &
	  (XD = X0 + 1) &
	  (XD1 = X0 + 2)) |
	 ((XR = X3 + 1) &
	  (XD = X3 - 1) &
	  (XD1 = X3 - 2))) &
	not .member(pos(XD,Y),LT) &
	.concat([pos(XD,Y),pos(XD1,Y)],LT,LT2) &
	listForceBlockPositionHorizontalTwoInFour(LT2,L,P,R).

listForceBlockPositionHorizontalTwoInFour([],[],_,_).
listForceBlockPositionHorizontalTwoInFour(LT,L,_,_):- L = LT.


listForceBlockPositionDiagonalTwoInFourLeft([],L,P,R):-
	diagonalTwoInFour(X0,Y0,X3,Y3,P) &
	tablero(XR,YR,R)[source(percept)] &
	(((XR = X0 - 1) &
	  (XD = X0 + 1) &
	  (XD1 = X0 + 2) &
	  (YR = Y0 - 1) &
	  (YD = Y0 + 1) &
	  (YD1 = Y0 + 2)) |
	 ((XR = X3 + 1) &
	  (XD = X3 - 1) &
	  (XD1 = X3 - 2) &
	  (YR = Y3 + 1) &
	  (YD = Y3 - 1) &
	  (YD1 = Y3 - 2))) &
	.concat([pos(XD,YD),pos(XD1,YD1)],[],LT) &
	listForceBlockPositionDiagonalTwoInFourLeft(LT,L,P,R).

listForceBlockPositionDiagonalTwoInFourLeft(LT,L,P,R):-
	diagonalTwoInFour(X0,Y0,X3,Y3,P) &
	tablero(XR,YR,R)[source(percept)] &
	(((XR = X0 - 1) &
	  (XD = X0 + 1) &
	  (XD1 = X0 + 2) &
	  (YR = Y0 - 1) &
	  (YD = Y0 + 1) &
	  (YD1 = Y0 + 2)) |
	 ((XR = X3 + 1) &
	  (XD = X3 - 1) &
	  (XD1 = X3 - 2) &
	  (YR = Y3 + 1) &
	  (YD = Y3 - 1) &
	  (YD1 = Y3 - 2))) &
	not .member(pos(XD,YD),LT) &
	not .member(pos(XD1,YD1),LT) &
	.concat([pos(XD,YD),pos(XD1,YD1)],LT,LT2) &
	listForceBlockPositionDiagonalTwoInFourLeft(LT2,L,P,R).

listForceBlockPositionDiagonalTwoInFourLeft([],[],_,_).
listForceBlockPositionDiagonalTwoInFourLeft(LT,L,_,_):- L = LT.


listForceBlockPositionDiagonalTwoInFourRight([],L,P,R):-
	diagonalTwoInFour(X0,Y0,X3,Y3,P) &
	tablero(XR,YR,R)[source(percept)] &
	(((XR = X0 + 1) &
	  (XD = X0 - 1) &
	  (XD1 = X0 - 2) &
	  (YR = Y0 - 1) &
	  (YD = Y0 + 1) &
	  (YD1 = Y0 + 2)) |
	 ((XR = X3 - 1) &
	  (XD = X3 + 1) &
	  (XD1 = X3 + 2) &
	  (YR = Y3 + 1) &
	  (YD = Y3 - 1) &
	  (YD1 = Y3 - 2))) &
	.concat([pos(XD,YD),pos(XD1,YD1)],[],LT) &
	listForceBlockPositionDiagonalTwoInFourRight(LT,L,P,R).

listForceBlockPositionDiagonalTwoInFourRight(LT,L,P,R):-
	diagonalTwoInFour(X0,Y0,X3,Y3,P) &
	tablero(XR,YR,R)[source(percept)] &
	(((XR = X0 + 1) &
	  (XD = X0 - 1) &
	  (XD1 = X0 - 2) &
	  (YR = Y0 - 1) &
	  (YD = Y0 + 1) &
	  (YD1 = Y0 + 2)) |
	 ((XR = X3 - 1) &
	  (XD = X3 + 1) &
	  (XD1 = X3 + 2) &
	  (YR = Y3 + 1) &
	  (YD = Y3 - 1) &
	  (YD1 = Y3 - 2))) &
	not .member(pos(XD,YD),LT) &
	not .member(pos(XD1,YD1),LT) &
	.concat([pos(XD,YD),pos(XD1,YD1)],LT,LT2) &
	listForceBlockPositionDiagonalTwoInFourRight(LT2,L,P,R).

listForceBlockPositionDiagonalTwoInFourRight([],[],_,_).
listForceBlockPositionDiagonalTwoInFourRight(LT,L,_,_):- L = LT.

/* Initial goals */


!play.

/* Plans */

// PLAY TO WIN
+!play: playerNumbers &
	estrategia(jugarAGanar)[source(percept)] <-
		.print("A ganar");
		!tests.
		//!playToTest.

// PLAY TO LOSE
+!play:
	estrategia(jugarAPerder)[source(percept)] <-
		.print("A perder");
		!playToLose.

// WINNING PLAN
+!playToWin:
	movement(N) &
	(N = 0) &
	turno(.my_name(X))[source(percept)] <-
	put(3,3);
	-+movement(N+2);
	+advantage(0);
	!playToWin.

+!playToWin:
	movement(N) &
	(N = 1) &
	turno(.my_name(X))[source(percept)] &
	opponent(R) &
	tablero(X0,Y0,R)[source(percept)] <-
		advantage(1);
		+movementRecord([pos(X0,Y0)]);
		+actualMovement(pos(X0,Y0));
		?closestCenterDiagonal(X1,Y1);
		put(X1,Y1);
		-+movement(N+1);
		-actualMovement(pos(X0,Y0));
		!playToWin.

+!playToWin:
	movement(N) &
	turno(.my_name(X))[source(percept)] &
	opponent(R) &
	tablero(X0,Y0,R)[source(percept)] &
	movementRecord(L) &
	not .member(pos(X0,X0),L) <-
		-+movementRecord([pos(X0,Y0)|_]);
		+actualMovement(pos(X0,Y0));
		?listWinPositions(LWin);
		?listLosePositions(LLose);
		?pa
		?decideMovement(X1,Y1);
		put(X1,Y1);
		-+movement(N+1);
		-actualMovement(pos(X0,Y0));
		!playToWin.

+!playToWin <- !playToWin. 


// LOSING PLAN





// MOVEMENT PLAN


/////////////////////////////////////////
////////////////TEST PLANS///////////////
/////////////////////////////////////////

+!tests <-
	.println("");
	.println("");
	.println("");
	.println("Running all tests...");
	.println("");

	//!testPackage1(S1,T1);
	//!resetBoardTests;
	//.println("");

	//!testPackage2(S2,T2);
	//!resetBoardTests;
	//.println("");

	//!testPackage3(S3,T3);
	//!resetBoardTests;
	//.println("");

	//!testPackage4(S4,T4);
	//!resetBoardTests;
	//.println("");

	//!testPackage5(S5,T5);
	//!resetBoardTests;
	//.println("");

	//!testPackage6(S6,T6);
	//!resetBoardTests;
	//.println("");

	//!testPackage7(S7,T7);
	//!resetBoardTests;
	//.println("");


	//.println("First package done: ",S1,"/",T1);
	//.println("Second package done: ",S2,"/",T2);
	//.println("Third package done: ",S3,"/",T3);
	//.println("Fourth package done: ",S4,"/",T4);
	//.println("Fifth package done: ",S5,"/",T5);
	//.println("Sixth package done: ",S6,"/",T6);
	//.println("Seventh package done: ",S7,"/",T7);


	.println("All tests done...");
	!resetBoardTests;
	.println("DONE.");
	.println("");
	.println("").


// Tests package 1
+!testPackage1(Sucesfull,Total) <-
	.println("Running testPackage1...");
	Total = 26;
	!generateTestBoard;
	!testPlayerNumbers(N1); //DONE
	.println("");
	!testEnemyNumber(N2); //DONE
	.println("");
	!testCheckEmpty(N3); //DONE
	.println("");
	!testVertical(N4); //DONE
	.println("");
	!testHorizontal(N5); //DONE
	.println("");
	!testDiagonal(N6); //DONE
	.println("");
	!testVerticalTwoInThree(N7); //DONE
	.println("");
	!testHorizontalTwoInThree(N8); //DONE
	.println("");
	!testDiagonalTwoInThree(N9); //DONE
	.println("");
	!testVerticalTwoInFour(N10); //DONE
	.println("");
	!testHorizontalTwoInFour(N11); //DONE
	.println("");
	!testDiagonalTwoInFour(N12); //DONE
	.println("");
	!testClosestCenterDiagonal(N13); //DONE
	.println("");
	!testVerticalPair(N14); //DONE
	.println("");
	!testHorizontalPair(N15); //DONE
	.println("");
	!testDiagonalPair(N16); //DONE
	.println("");
	!testVerticalTwoInThreePair(N17); //DONE
	.println("");
	!testHorizontalTwoInThreePair(N18); //DONE
	.println("");
	!testDiagonalTwoInThreePair(N19); //DONE
	.println("");
	!testVerticalTwoInFourPair(N20); //DONE
	.println("");
	!testHorizontalTwoInFourPair(N21); //DONE
	.println("");
	!testDiagonalTwoInFourPair(N22); //DONE
	.println("");
	!testListVerticalWinPositionsTop(N23); //DONE
	.println("");
	!testListVerticalWinPositionsBottom(N24); //DONE
	.println("");
	!testListVerticalWinPositionsTwoInThreeTop(N25); //DONE
	.println("");
	!testListVerticalWinPositionsTwoInThreeBottom(N26); //DONE
	.println("");
	Sucesfull = N1+N2+N3+N4+N5+N6+N7+N8+N9+N10+N11+N12+N13+N14+N15+N16+N17+N18+
	N19+N20+N21+N22+N23+N24+N25+N26.


// Tests package 2
+!testPackage2(Sucesfull,Total) <-
	.println("Running testPackage2...");
	Total = 20;//22 
	!generateTestBoard2;
	!testListHorizontalWinPositionsLeft(N1); //DONE
	.println("");
	!testListHorizontalWinPositionsRight(N2); //DONE
	.println("");
	!testListHorizontalWinPositionsTwoInThreeLeft(N3); //DONE
	.println("");
	!testListHorizontalWinPositionsTwoInThreeRight(N4); //DONE
	.println("");
	!testListDiagonalWinPositionsTopLeft(N5); //DONE
	.println("");
	!testListDiagonalWinPositionsBottomRight(N6); //DONE
	.println("");
	!testListDiagonalWinPositionsTopRight(N7); //DONE
	.println("")
	!testListDiagonalWinPositionsBottomLeft(N8); //DONE
	.println("");
	!testListDiagonalWinPositionsTwoInThreeTopLeft(N9); //DONE
	.println("");
	!testListDiagonalWinPositionsTwoInThreeBottomLeft(N10); //DONE
	.println("");
	!testListDiagonalWinPositionsTwoInThreeTopRight(N11); //DONE
	.println("");
	!testListDiagonalWinPositionsTwoInThreeBottomRight(N12); //DONE
	.println("");
	!testListVerticalWinPositions(N13); //DONE
	.println("");
	!testListHorizontalWinPositions(N14); //DONE
	.println("");
	!testListDiagonalWinPositions(N15); //DONE
	.println("");
	!testListWinPositions(N16); //DONE
	.println("");
	!testListLosePositions(N17); //DONE
	.println("");
	!testPairs(N18); //DONE
	.println("");
	!testTwoInThreePairs(N19); //DONE
	.println("");
	!testTwoInFourPairs(N20); //DONE
	.println("");
	//!testAllMyForms(N21); //NOT WORKING <- NEEDS CHECKING 
	//.println("");
	//!testAllHisForms(N22); //NOT WORKING <- NEEDS CHECKING
	Sucesfull = N1+N2+N3+N4+N5+N6+N7+N8+N9+N10+N11+N12+N13+N14+N15+
	N16+N17+N18+N19+N20.//+N21+N22.


// Tests package 3
+!testPackage3(Sucesfull,Total) <-
	.println("Running testPackage3...");
	Total = 8;
	!generateTestBoard3;
	.println("");
	!testListTripleVerticalTop(N1); //DONE
	.println("");
	!testListTripleVerticalBottom(N2); //DONE
	.println("");
	!testListTripleHorizontalLeft(N3); //DONE
	.println("");
	!testListTripleHorizontalRight(N4); //DONE
	.println("");
	!testListTripleDiagonalTopLeft(N5); //DONE
	.println("");
	!testListTripleDiagonalBottomRight(N6); //DONE
	.println("");
	!testListTripleDiagonalTopRight(N7); //DONE
	.println("");
	!testListTripleDiagonalBottomLeft(N8); //DONE
	.println("");
	Sucesfull = N1+N2+N3+N4+N5+N6+N7+N8.


// Tests package 4
+!testPackage4(Sucesfull,Total) <-
	.println("Running testPackage4...");
	Total = 4;
	!generateTestBoard4;
	.println("");
	!testListTripleTwoInThreeVertical(N1); //DONE
	.println("");
	!testListTripleTwoInThreeHorizontal(N2); //DONE
	.println("");
	!testListTripleTwoInThreeDiagonalLeft(N3); //DONE
	.println("");
	!testListTripleTwoInThreeDiagonalRight(N4); //DONE
	.println("");
	Sucesfull = N1+N2+N3+N4.


// Tests package 5
+!testPackage5(Sucesfull,Total) <-
	.println("Running testPackage5...");
	Total = 8;
	!generateTestBoard5;
	.println("");
	!testListForceBlockPositionVerticalTopBottom(N1); //REHACER TESTS
	.println("");
	!testListForceBlockPositionVerticalBottomTop(N2);//REHACER TESTS
	.println("");
	!testListForceBlockPositionHorizontalLeftRight(N3);//REHACER TESTS
	.println("");
	!testListForceBlockPositionHorizontalRightLeft(N4);//REHACER TESTS
	.println("");
	!testListForceBlockPositionDiagonalTopLeftBottomRight(N5);//REHACER TESTS
	.println("");
	!testListForceBlockPositionDiagonalBottomRightTopLeft(N6);//REHACER TESTS
	.println("");
	!testListForceBlockPositionDiagonalTopRightBottomLeft(N7);//REHACER TESTS
	.println("");
	!testListForceBlockPositionDiagonalBottomLeftTopRight(N8);//REHACER TESTS
	.println("");
	Sucesfull = N1+N2+N3+N4+N5+N6+N7+N8.


// Tests package 6
+!testPackage6(Sucesfull,Total) <-
	!generateTestBoard6;
	.println("Running testPackage6...");
	Total = 4;
	!testListForceBlockPositionVerticalTwoInThree(N1); //REHACER TESTS
	.println("");
	!testListForceBlockPositionHorizontalTwoInThree(N2); //REHACER TESTS
	.println("");
	!testListForceBlockPositionDiagonalTwoInThreeLeft(N3); //REHACER TESTS
	.println("");
	!testListForceBlockPositionDiagonalTwoInThreeRight(N4); //REHACER TESTS
	.println("");
	Sucesfull = N1+N2+N3+N4.


// Tests package 7
+!testPackage7(Sucesfull,Total) <-
	!generateTestBoard7;
	.println("Running testPackage7...");
	Total = 4;
	!testListForceBlockPositionVerticalTwoInFour(N1); //REHACER TESTS
	.println("");
	!testListForceBlockPositionHorizontalTwoInFour(N2); //REHACER TESTS
	.println("");
	!testListForceBlockPositionDiagonalTwoInFourLeft(N3); //REHACER TESTS
	.println("");
	!testListForceBlockPositionDiagonalTwoInFourRight(N4); //REHACER TESTS
	.println("");
	Sucesfull = N1+N2+N3+N4.


// Resets the board
+!resetBoardTests:
	tablero(X,Y,1)[source(percept)] <-
	.println("Board reseting...");
	.wait(2000);
	put(X,Y);
	.wait(2000);
	.println("Board reset completed.").



+!generateTestBoard <-
	//Adds the test board beliefs
	+testPut(0,0);
	+testPut(0,1);
	+testPut(0,2);
	+testPut(0,4);
	+testPut(0,7);
	+testPut(1,6);
	+testPut(1,7);
	+testPut(1,5);
	+testPut(3,0);
	+testPut(3,1);
	+testPut(3,2);
	+testPut(3,4);
	+testPut(3,7);
	+testPut(4,4);
	+testPut(4,7);
	+testPut(6,3);
	+testPut(6,4);
	+testPut(6,6);
	+testPut(6,7);
	.send(player2,achieve,test);
	!playToTest.
	

+!generateTestBoard2 <-
	//Adds the test board beliefs
	+testPut(0,1);
	+testPut(2,1);
	+testPut(3,1);
	+testPut(4,1);
	+testPut(6,1);
	+testPut(0,3);
	+testPut(1,3);
	+testPut(1,4);
	+testPut(0,4);
	+testPut(0,5);
	+testPut(1,5);
	.send(player2,achieve,test2);
	!playToTest.


+!generateTestBoard3 <-
	//Adds the test board beliefs
	+testPut(6,1);
	+testPut(6,2);
	+testPut(4,3);
	+testPut(5,4);
	+testPut(4,4);
	+testPut(6,5);
	+testPut(6,6);
	+testPut(5,6);
	.send(player2,achieve,test3);
	!playToTest.


+!generateTestBoard4 <-
	//Adds the test board beliefs
	+testPut(2,2);
	+testPut(2,4);
	+testPut(4,2);
	+testPut(4,4);
	.send(player2,achieve,test4);
	!playToTest.

+!generateTestBoard5 <-
	//Adds the test board beliefs
	+testPut(1,1);
	+testPut(2,1);
	+testPut(1,2);
	+testPut(2,2);
	+testPut(2,3);
	+testPut(1,6);
	+testPut(2,6);
	+testPut(1,5);
	+testPut(2,5);
	+testPut(3,5);
	+testPut(6,6);
	+testPut(5,6);
	+testPut(6,5);
	+testPut(5,5);
	+testPut(5,4);
	+testPut(5,1);
	+testPut(6,1);
	+testPut(5,2);
	+testPut(6,2);
	+testPut(4,2);
	.send(player2,achieve,test5);
	!playToTest.


+!generateTestBoard6 <-
	// Adds the test board beliefs
	+testPut(1,3);
	+testPut(0,2);
	+testPut(0,4);
	+testPut(0,6);
	+testPut(1,1);
	+testPut(2,1);
	+testPut(3,3);
	+testPut(3,4);
	+testPut(4,0);
	+testPut(5,1);
	+testPut(5,2);
	+testPut(5,5);
	+testPut(5,6);
	+testPut(6,2);
	+testPut(6,3);
	+testPut(6,7);
	+testPut(7,0);
	+testPut(7,2);
	+testPut(7,4);
	+testPut(7,6);
	.send(player2,achieve,test6);
	!playToTest.
	

+!generateTestBoard7 <-
	// Adds the test board beliefs
	+testPut(0,1);
	+testPut(0,4);
	+testPut(1,0);
	+testPut(2,6);
	+testPut(3,2);
	+testPut(3,3);
	+testPut(3,4);
	+testPut(4,0);
	+testPut(5,3);
	+testPut(6,1);
	+testPut(6,5);
	+testPut(6,6);
	+testPut(6,7);
	+testPut(7,6);
	+testPut(7,3);
	+testPut(3,7);
	.send(player2,achieve,test7);
	!playToTest.


// Plan to play a few rounds and generate a board's state to test
+!playToTest:
	testPut(X,Y) &
	turno(player1)[source(percept)] <- 
		put(X,Y);
		-testPut(X,Y);
		-+movement(N+1);
		!playToTest.
		
+!playToTest:
	not testPut(X,Y) <-
		.print("End of !playToTest of player1").

+!playToTest <- !playToTest.


+!testPlayerNumbers(N): 
	.println("Running test [testPlayerNumbers]") &
	playerNumbers &
	player(X) &
	opponent(Y) &
	X = 1 &
	Y = 2 <-
		N = 1;
		-player(1);
		-opponent(2);
		.print("Test sucesfull").

+!testPlayerNumbers(N) <-
	.print("Test failed");
	N = 0.


+!testEnemyNumber(N):
	.println("Running test [testEnemyNumber]") &
	enemyNumber(1,N0) &
	enemyNumber(2,N1) &
	N0 = 2 &
	N1 = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testEnemyNumber(N) <-
	.print("Test failed");
	N = 0.


+!testCheckEmpty(N):
	.println("Running test [testCheckEmpty]") &
	checkEmpty(0,5) & 
	not checkEmpty(0,0) &
	not checkEmpty(1,1) <-
		N = 1;
		.print("Test sucesfull").

+!testCheckEmpty(N) <-
	.print("Test failed");
	N = 0.


+!testVertical(N):
	.println("Running test [testVertical]") &
	vertical(0,5,0,6,0) &
	vertical(0,0,0,1,1) &
	vertical(5,2,5,3,2) &
	not vertical(0,0,0,1,0) &
	not vertical(4,5,4,6,1) &
	not vertical(0,5,0,6,2) <-
		N = 1;
		.print("Test sucesfull").

+!testVertical(N) <-
	.print("Test failed");
	N = 0.


+!testHorizontal(N):
	.println("Running test [testHorizontal]") &
	horizontal(1,0,2,0,0) &
	horizontal(0,7,1,7,1) &
	horizontal(2,5,3,5,2) &
	not horizontal(2,5,3,5,0) &
	not horizontal(1,0,2,0,1) &
	not horizontal(0,7,1,7,2) <-
		N = 1;
		.print("Test sucesfull").

+!testHorizontal(N) <-
	.print("Test failed");
	N = 0.


+!testDiagonal(N):
	.println("Running test [testDiagonal]") &
	diagonal(1,0,2,1,0) &
	diagonal(0,4,1,5,1) &
	diagonal(1,1,2,2,2) &
	not diagonal(1,1,2,2,0) &
	not diagonal(1,0,2,1,1) &
	not diagonal(0,4,1,5,2) <-
		N = 1;
		.print("Test sucesfull").

+!testDiagonal(N) <-
	.print("Test failed");
	N = 0.


+!testVerticalTwoInThree(N):
	.println("Running test [testVerticalTwoInThree]") &
	verticalTwoInThree(6,4,6,6,1) &
	verticalTwoInThree(7,0,7,2,2) &
	not verticalTwoInThree(6,6,6,4,1) &
	not verticalTwoInThree(0,2,0,4,1) &
	not verticalTwoInThree(0,0,0,2,1) <-
		N = 1;
		.print("Test sucesfull").

+!testVerticalTwoInThree(N) <-
	.print("Test failed");
	N = 0.


+!testHorizontalTwoInThree(N):
	.println("Running test [testHorizontalTwoInThree]") &
	horizontalTwoInThree(1,7,3,7,1) &
	horizontalTwoInThree(3,3,5,3,2) &
	not horizontalTwoInThree(3,7,1,7,1) &
	not horizontalTwoInThree(5,3,7,3,2) &
	not horizontalTwoInThree(4,1,6,1,2) <-
		N = 1;
		.print("Test sucesfull").

+!testHorizontalTwoInThree(N) <-
	.print("Test failed");
	N = 0.

	
+!testDiagonalTwoInThree(N):
	.println("Running test [testDiagonalTwoInThree]") &
	diagonalTwoInThree(1,5,3,7,1) &
	diagonalTwoInThree(5,1,3,3,2) &
	not diagonalTwoInThree(3.3,5,1,2) &
	not diagonalTwoInThree(3,7,1,5,1) &
	not diagonalTwoInThree(1,1,3,3,2) &
	not diagonalTwoInThree(5,2,7,4,2) <-
		N = 1;
		.print("Test sucesfull").

+!testDiagonalTwoInThree(N) <-
	.print("Test failed");
	N = 0.


+!testVerticalTwoInFour(N):
	.println("Running test [testVerticalTwoInFour]") &
	verticalTwoInFour(0,4,0,7,1) &
	verticalTwoInFour(2,2,2,5,2) &
	not verticalTwoInFour(0,7,0,4,1) &
	not verticalTwoInFour(7,0,7,3,2) &
	not verticalTwoInFour(4,4,4,7,1) <-
	N = 1;
	.print("Test sucesfull").

+!testVerticalTwoInFour(N) <-
	.print("Test failed");
	N = 0.

	
+!testHorizontalTwoInFour(N):
	.println("Running test [testHorizontalTwoInFour]") &
	horizontalTwoInFour(0,0,3,0,1) &
	horizontalTwoInFour(0,3,3,3,2) &
	not horizontalTwoInFour(3,0,0,0,1) &
	not horizontalTwoInFour(0,2,3,2,1) & 
	not horizontalTwoInFour(3,4,6,4,1) <-
		N = 1;
		.print("Test sucesfull").

+!testHorizontalTwoInFour(N) <-
	.print("Test failed");
	N = 0.

	
+!testDiagonalTwoInFour(N):
	.println("Running test [testDiagonalTwoInFour]") &
	diagonalTwoInFour(0,1,3,4,1) &
	diagonalTwoInFour(6,4,3,7,1) &
	not diagonalTwoInFour(3,4,0,1,1) &
	not diagonalTwoInFour(4,4,1,7,1) &
	not diagonalTwoInFour(3,4,0,7,1) &
	not diagonalTwoInFour(6,0,3,3,2) <-
		N = 1;
		.print("Test sucesfull").

+!testDiagonalTwoInFour(N) <-
	.print("Test failed");
	N = 0.


+!testClosestCenterDiagonal(N):
	.println("Running test [testClosestCenterDiagonal]") &
	.asserta(actualMovement(0,0)) &
	closestCenterDiagonal(2,2) &
	.abolish(actualMovement(0,0)) &
	.asserta(actualMovement(7,0)) &
	closestCenterDiagonal(5,2) &
	.abolish(actualMovement(7,0)) &
	.asserta(actualMovement(0,7)) &
	closestCenterDiagonal(2,5) &
	.abolish(actualMovement(0,7)) &
	.asserta(actualMovement(7,7)) &
	closestCenterDiagonal(5,5) &
	.abolish(actualMovement(7,7)) &
	.asserta(actualMovement(1,0)) &
	closestCenterDiagonal(2,1) &
	.abolish(actualMovement(1,0)) &
	.asserta(actualMovement(6,0)) &
	closestCenterDiagonal(5,1) &
	.abolish(actualMovement(6,0)) &
	.asserta(actualMovement(1,7)) &
	closestCenterDiagonal(2,6) &
	.abolish(actualMovement(1,7)) &
	.asserta(actualMovement(7,6)) &
	closestCenterDiagonal(6,5) &
	.abolish(actualMovement(7,6)) <-
		N = 1;
		.print("Test sucesfull").

+!testClosestCenterDiagonal(N) <-
	.print("Test failed");
	N = 0.


+!testVerticalPair(N):
	.println("Running test [testVerticalPair]") &
	verticalPair([],L,1) &
	.member(pairPos(pos(0,0), pos(0,1)),L) &
	.member(pairPos(pos(0,1), pos(0,2)),L) &
	.member(pairPos(pos(1,5), pos(1,6)),L) &
	.member(pairPos(pos(1,6), pos(1,7)),L) &
	.member(pairPos(pos(3,0), pos(3,1)),L) &
	.member(pairPos(pos(3,1), pos(3,2)),L) &
	.member(pairPos(pos(6,3), pos(6,4)),L) &
	.member(pairPos(pos(6,6), pos(6,7)),L) &
	not .member(pairPos(pos(6,4), pos(6,3)),L) &
	.length(L,M) &
	M = 8 <-
		N = 1;
		.print("Test sucesfull").

+!testVerticalPair(N) <-
	.print("Test failed");
	N = 0.

	
+!testHorizontalPair(N):
	.println("Running test [testHorizontalPair]") &
	horizontalPair([],L,1) &
	.member(pairPos(pos(0,7), pos(1,7)),L) &
	.member(pairPos(pos(3,4), pos(4,4)),L) &
	.member(pairPos(pos(3,7), pos(4,7)),L) &
	not .member(pairPos(pos(4,4), pos(3,4)),L) &
	.length(L,M) &
	M = 3 <-
		N = 1;
		.print("Test sucesfull").

+!testHorizontalPair(N) <-
	.print("Test failed");
	N = 0.

	
+!testDiagonalPair(N):
	.println("Running test [testDiagonalPair]") &
	diagonalPair([],L,1) &
	.member(pairPos(pos(0,4), pos(1,5)),L) &
	.member(pairPos(pos(1,6), pos(0,7)),L) &
	not .member(pairPos(pos(1,5), pos(0,4)),L) &
	not .member(pairPos(pos(0,7), pos(1,6)),L) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		.print("Test sucesfull").

+!testDiagonalPair(N) <-
	.print("Test failed");
	N = 0.


+!testVerticalTwoInThreePair(N):
	.println("Running test [testVerticalTwoInThreePair]") &
	verticalTwoInThreePair([],L,1) &
	.member(pairPos(pos(6,4), pos(6,6)),L) &
	not .member(pairPos(pos(6,6), pos(6,4)),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testVerticalTwoInThreePair(N) <-
	.print("Test failed");
	N = 0.

	
+!testHorizontalTwoInThreePair(N):
	.println("Running test [testHorizontalTwoInThreePair]") &
	horizontalTwoInThreePair([],L,1) &
	.member(pairPos(pos(1,7), pos(3,7)),L) &
	.member(pairPos(pos(4,4), pos(6,4)),L) &
	.member(pairPos(pos(4,7), pos(6,7)),L) &
	not .member(pairPos(pos(3,7), pos(1,7)),L) &
	.length(L,M) &
	M = 3 <-
		N = 1;
		.print("Test sucesfull").

+!testHorizontalTwoInThreePair(N) <-
	.print("Test failed");
	N = 0.

	
+!testDiagonalTwoInThreePair(N):
	.println("Running test [testDiagonalTwoInThreePair]") &
	diagonalTwoInThreePair([],L,1) &
	.member(pairPos(pos(1,5), pos(3,7)),L) &
	.member(pairPos(pos(4,4), pos(6,6)),L) &
	not .member(pairPos(pos(3,4), pos(1,6)),L) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		.print("Test sucesfull").

+!testDiagonalTwoInThreePair(N) <-
	.print("Test failed");
	N = 0.


+!testVerticalTwoInFourPair(N):
	.println("Running test [testVerticalTwoInFourPair]") &
	verticalTwoInFourPair([],L,1) &
	.member(pairPos(pos(0,4), pos(0,7)),L) &
	not .member(pairPos(pos(6,3), pos(6,6)),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testVerticalTwoInFourPair(N) <-
	.print("Test failed");
	N = 0.

	
+!testHorizontalTwoInFourPair(N):
	.println("Running test [testHorizontalTwoInFourPair]") &
	horizontalTwoInFourPair([],L,1) &
	.member(pairPos(pos(0,0), pos(3,0)),L) &
	not .member(pairPos(pos(0,2), pos(3,2)),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testHorizontalTwoInFourPair(N) <-
	.print("Test failed");
	N = 0.

	
+!testDiagonalTwoInFourPair(N):
	.println("Running test [testDiagonalTwoInFourPair]") &
	diagonalTwoInFourPair([],L,1) &
	.member(pairPos(pos(6,4), pos(3,7)),L) &
	not .member(pairPos(pos(3,1), pos(0,4)),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testDiagonalTwoInFourPair(N) <-
	.print("Test failed");
	N = 0.


+!testListVerticalWinPositionsTop(N):
	.println("Running test [testListVerticalWinPositionsTop]") &
	listVerticalWinPositionsTop([],L,2) &
	.member(pos(5,0),L) &
	.member(pos(7,1),L) &
	not .member(pos(7,5),L) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		.print("Test sucesfull").

+!testListVerticalWinPositionsTop(N) <-
	.print("Test failed");
	N = 0.

	
+!testListVerticalWinPositionsBottom(N):
	.println("Running test [testListVerticalWinPositionsBottom]") &
	listVerticalWinPositionsBottom([],L,2) &
	.member(pos(7,5),L) &
	.member(pos(5,4),L) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		.print("Test sucesfull").

+!testListVerticalWinPositionsBottom(N) <-
	.print("Test failed");
	N = 0.
	
+!testListVerticalWinPositionsTwoInThreeTop(N):
	.println("Running test [testListVerticalWinPositionsTwoInThreeTop]") &
	listVerticalWinPositionsTwoInThreeTop([],L,2) &
	.member(pos(7,1),L) &
	not .member(pos(5,0),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListVerticalWinPositionsTwoInThreeTop(N) <-
	.print("Test failed");
	N = 0.

	
+!testListVerticalWinPositionsTwoInThreeBottom(N):
	.println("Running test [testListVerticalWinPositionsTwoInThreeBottom]") &
	listVerticalWinPositionsTwoInThreeBottom([],L,1) &
	.member(pos(6,5),L) &
	not .member(pos(7,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListVerticalWinPositionsTwoInThreeBottom(N) <-
	.print("Test failed");
	N = 0.


+!testListHorizontalWinPositionsLeft(N):
	.println("Running test [testListHorizontalWinPositionsLeft]") &
	listHorizontalWinPositionsLeft([],L,1) &
	.member(pos(1,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListHorizontalWinPositionsLeft(N) <-
	.print("Test failed");
	N = 0.

	
+!testListHorizontalWinPositionsRight(N):
	.println("Running test [testListHorizontalWinPositionsRight]") &
	listHorizontalWinPositionsRight([],L,1) &
	.member(pos(5,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListHorizontalWinPositionsRight(N) <-
	.print("Test failed");
	N = 0.

	
+!testListHorizontalWinPositionsTwoInThreeLeft(N):
	.println("Running test [testListHorizontalWinPositionsTwoInThreeLeft]") &
	listHorizontalWinPositionsTwoInThreeLeft([],L,1) &
	.member(pos(1,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListHorizontalWinPositionsTwoInThreeLeft(N) <-
	.print("Test failed");
	N = 0.

	
+!testListHorizontalWinPositionsTwoInThreeRight(N):
	.println("Running test [testListHorizontalWinPositionsTwoInThreeRight]") &
	listHorizontalWinPositionsTwoInThreeRight([],L,1) &
	.member(pos(5,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListHorizontalWinPositionsTwoInThreeRight(N) <-
	.print("Test failed");
	N = 0.


+!testListDiagonalWinPositionsTopLeft(N):
	.println("Running test [testListDiagonalWinPositionsTopLeft]") &
	listDiagonalWinPositionsTopLeft([],L,2) &
	.member(pos(1,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListDiagonalWinPositionsTopLeft(N) <-
	.print("Test failed");
	N = 0.

	
+!testListDiagonalWinPositionsBottomRight(N):
	.println("Running test [testListDiagonalWinPositionsBottomRight]") &
	listDiagonalWinPositionsBottomRight([],L,2) &
	.member(pos(5,5),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListDiagonalWinPositionsBottomRight(N) <-
	.print("Test failed");
	N = 0.

	
+!testListDiagonalWinPositionsTopRight(N):
	.println("Running test [testListDiagonalWinPositionsTopRight]") &
	listDiagonalWinPositionsTopRight([],L,2) &
	.member(pos(6,2),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListDiagonalWinPositionsTopRight(N) <-
	.print("Test failed");
	N = 0.

	
+!testListDiagonalWinPositionsBottomLeft(N):
	.println("Running test [testListDiagonalWinPositionsBottomLeft]") &
	listDiagonalWinPositionsBottomLeft([],L,2) &
	.member(pos(2,6),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListDiagonalWinPositionsBottomLeft(N) <-
	.print("Test failed");
	N = 0.

	
+!testListDiagonalWinPositionsTwoInThreeTopLeft(N):
	.println("Running test [testListDiagonalWinPositionsTwoInThreeTopLeft]") &
	listDiagonalWinPositionsTwoInThreeTopLeft([],L,2) &
	.member(pos(1,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListDiagonalWinPositionsTwoInThreeTopLeft(N) <-
	.print("Test failed");
	N = 0.


+!testListDiagonalWinPositionsTwoInThreeBottomRight(N):
	.println("Running test [testListDiagonalWinPositionsTwoInThreeBottomRight]") &
	listDiagonalWinPositionsTwoInThreeBottomRight([],L,2) &
	.member(pos(5,5),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListDiagonalWinPositionsTwoInThreeBottomRight(N) <-
	.print("Test failed");
	N = 0.

	
+!testListDiagonalWinPositionsTwoInThreeTopRight(N):
	.println("Running test [testListDiagonalWinPositionsTwoInThreeTopRight]") &
	listDiagonalWinPositionsTwoInThreeTopRight([],L,2) &
	.member(pos(6,2),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListDiagonalWinPositionsTwoInThreeTopRight(N) <-
	.print("Test failed");
	N = 0.
	

+!testListDiagonalWinPositionsTwoInThreeBottomLeft(N):
	.println("Running test [testListDiagonalWinPositionsTwoInThreeBottomLeft]") &
	listDiagonalWinPositionsTwoInThreeBottomLeft([],L,2) &
	.member(pos(2,6),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListDiagonalWinPositionsTwoInThreeBottomLeft(N) <-
	.print("Test failed");
	N = 0.

	
+!testListVerticalWinPositions(N):
	.println("Running test [testListVerticalWinPositions]") &
	listVerticalWinPositions(L,1) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListVerticalWinPositions(N) <-
	.print("Test failed");
	N = 0.

	
+!testListHorizontalWinPositions(N):
	.println("Running test [testListHorizontalWinPositions]") &
	listHorizontalWinPositions(L,1) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		.print("Test sucesfull").

+!testListHorizontalWinPositions(N) <-
	.print("Test failed");
	N = 0.

	
+!testListDiagonalWinPositions(N):
	.println("Running test [testListDiagonalWinPositions]") &
	listDiagonalWinPositions(L,2) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListDiagonalWinPositions(N) <-
	.print("Test failed");
	N = 0.


+!testListWinPositions(N):
	.println("Running test [testListWinPositions]") &
	playerNumbers &
	listWinPositions(L) &
	.length(L,M) &
	M = 6 <-
		.abolish(player(1));
		.abolish(opponent(2));
		N = 1;
		.print("Test sucesfull").

+!testListWinPositions(N) <-
	.print("Test failed");
	N = 0.

	
+!testListLosePositions(N):
	.println("Running test [testListLosePositions]") &
	playerNumbers &
	listLosePositions(L) &
	.length(L,M) &
	M = 5 <-
		.abolish(player(1));
		.abolish(opponent(2));
		N = 1;
		.print("Test sucesfull").

+!testListLosePositions(N) <-
	.print("Test failed");
	N = 0.


+!testPairs(N):
	.println("Running test [testPairs]") &
	pairs(L,1) &
	.length(L,M) &
	M = 13 <-
		N = 1;
		.print("Test sucesfull").

+!testPairs(N) <-
	.print("Test failed");
	N = 0.

	
+!testTwoInThreePairs(N):
	.println("Running test [testTwoInThreePairs]") &
	twoInThreePairs(L,1) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testTwoInThreePairs(N) <-
	.print("Test failed");
	N = 0.

	
+!testTwoInFourPairs(N):
	.println("Running test [testTwoInFourPairs]") &
	twoInFourPairs(L,1) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testTwoInFourPairs(N) <-
	.print("Test failed");
	N = 0.


+!testAllMyForms(N):
	.println("Running test [testAllMyForms]") &
	playerNumbers &
	allMyForms(LP,TTL,TFL) &
	.length(LP,M1) &
	M1 = 13<-
		.abolish(player(1));
		.abolish(opponent(2));
		N = 1;
		.print("Test sucesfull").

+!testAllMyForms(N) <-
	.print("Test failed");
	N = 0.

	
+!testAllHisForms(N):
	.println("Running test [testAllHisForms]") &
	playerNumbers &
	allHisForms(LP,TTL,TFL) & 
	.length(LP,M1) &
	.length(TTL,M2) &
	.length(TFL,M3) &
	M1 = 6 &
	M2 = 6 &
	M3 = 1 <-
		.abolish(player(1));
		.abolish(opponent(2));
		N = 1;
		.print("Test sucesfull").

+!testAllHisForms(N) <-
	.print("Test failed");
	N = 0.
	

+!testListTripleVerticalTop(N):
	.println("Running test [testlistTripleVerticalTop]") &
	listTripleVerticalTop([],L,1) &
	.member(pos(4,2),L) &
	.member(pos(6,4),L) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		.print("Test sucesfull").

+!testListTripleVerticalTop(N) <-
	.print("Test failed");
	N = 0.


+!testListTripleVerticalBottom(N):
	.println("Running test [testListTripleVerticalBottom]") &
	listTripleVerticalBottom([],L,1) &
	.member(pos(4,5),L) &
	.member(pos(6,3),L) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		.print("Test sucesfull").

+!testListTripleVerticalBottom(N) <-
	.print("Test failed");
	N = 0.


+!testListTripleHorizontalLeft(N):
	.println("Running test [testListTripleHorizontalLeft]") &
	listTripleHorizontalLeft([],L,1) &
	.member(pos(4,6),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListTripleHorizontalLeft(N) <-
	.print("Test failed");
	N = 0.


+!testListTripleHorizontalRight(N):
	.println("Running test [testListTripleHorizontalRight]") &
	listTripleHorizontalRight([],L,1) &
	.member(pos(6,4),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListTripleHorizontalRight(N) <-
	.print("Test failed");
	N = 0.


+!testListTripleDiagonalTopLeft(N):
	.println("Running test [testListTripleDiagonalTopLeft]") &
	listTripleDiagonalTopLeft([],L,2) &
	.member(pos(1,3),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListTripleDiagonalTopLeft(N) <-
	.print("Test failed");
	N = 0.


+!testListTripleDiagonalBottomRight(N):
	.println("Running test [testListTripleDiagonalBottomRight]") &
	listTripleDiagonalBottomRight([],L,2) &
	.member(pos(4,6),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListTripleDiagonalBottomRight(N) <-
	.print("Test failed");
	N = 0.


+!testListTripleDiagonalTopRight(N):
	.println("Running test [testListTripleDiagonalTopRight]") &
	listTripleDiagonalTopRight([],L,2) &
	.member(pos(4,1),L) &
	.member(pos(3,3),L) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		.print("Test sucesfull").

+!testListTripleDiagonalTopRight(N) <-
	.print("Test failed");
	N = 0.


+!testListTripleDiagonalBottomLeft(N):
	.println("Running test [testListTripleDiagonalBottomLeft]") &
	listTripleDiagonalBottomLeft([],L,2) &
	.member(pos(1,4),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListTripleDiagonalBottomLeft(N) <-
	.print("Test failed");
	N = 0.


+!testListTripleTwoInThreeVertical(N):
	.println("Running test [testListTripleTwoInThreeVertical]") &
	listTripleTwoInThreeVertical([],L,1) &
	.member(pos(2,3),L) &
	.member(pos(4,3),L) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		.print("Test sucesfull").

+!testListTripleTwoInThreeVertical(N) <-
	.print("Test failed");
	N = 0.


+!testListTripleTwoInThreeHorizontal(N):
	.println("Running test [testListTripleTwoInThreeHorizontal]") &
	listTripleTwoInThreeHorizontal([],L,1) &
	.member(pos(3,2),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListTripleTwoInThreeHorizontal(N) <-
	.print("Test failed");
	N = 0.	


+!testListTripleTwoInThreeDiagonalLeft(N):
	.println("Running test [testListTripleTwoInThreeDiagonalLeft]") &
	listTripleTwoInThreeDiagonalLeft([],L,1) &
	.member(pos(3,3),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListTripleTwoInThreeDiagonalLeft(N) <-
	.print("Test failed");
	N = 0.	


+!testListTripleTwoInThreeDiagonalRight(N):
	.println("Running test [testListTripleTwoInThreeDiagonalRight]") &
	listTripleTwoInThreeDiagonalRight([],L,1) &
	.member(pos(3,3),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		.print("Test sucesfull").

+!testListTripleTwoInThreeDiagonalRight(N) <-
	.print("Test failed");
	N = 0.	



//HAY QUE REHACERLAS
+!testListForceBlockPositionVerticalTopBottom(N):
	.println("Running test [testListForceBlockPositionVerticalTopBottom]") &
	listForceBlockPositionVerticalTopBottom([],L,1,2) &
	.member(pos(1,3),L) &
	.member(pos(6,3),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionVerticalTopBottom(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionVerticalBottomTop(N):
	.println("Running test [testListForceBlockPositionVerticalBottomTop]") &
	listForceBlockPositionVerticalBottomTop([],L,1,2) &
	.member(pos(1,4),L) &
	.member(pos(6,4),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionVerticalBottomTop(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionHorizontalLeftRight(N):
	.println("Running test [testListForceBlockPositionHorizontalLeftRight]") &
	listForceBlockPositionHorizontalLeftRight([],L,1,2) &
	.member(pos(3,6),L) &
	.member(pos(3,1),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionHorizontalLeftRight(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionHorizontalRightLeft(N):
	.println("Running test [testListForceBlockPositionHorizontalRightLeft]") &
	listForceBlockPositionHorizontalRightLeft([],L,1,2) &
	.member(pos(4,6),L) &
	.member(pos(4,1),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionHorizontalRightLeft(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionDiagonalTopLeftBottomRight(N):
	.println("Running test [testListForceBlockPositionDiagonalTopLeftBottomRight]") &
	listForceBlockPositionDiagonalTopLeftBottomRight([],L,1,2) &
	.member(pos(3,3),L) &
	.member(pos(3,4),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionDiagonalTopLeftBottomRight(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionDiagonalBottomRightTopLeft(N):
	.println("Running test [testListForceBlockPositionDiagonalBottomRightTopLeft]") &
	listForceBlockPositionDiagonalBottomRightTopLeft([],L,1,2) &
	.member(pos(4,3),L) &
	.member(pos(4,4),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionDiagonalBottomRightTopLeft(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionDiagonalTopRightBottomLeft(N):
	.println("Running test [testListForceBlockPositionDiagonalTopRightBottomLeft]") &
	listForceBlockPositionDiagonalTopRightBottomLeft([],L,1,2) &
	.member(pos(4,3),L) &
	.member(pos(3,3),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionDiagonalTopRightBottomLeft(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionDiagonalBottomLeftTopRight(N):
	.println("Running test [testListForceBlockPositionDiagonalBottomLeftTopRight]") &
	listForceBlockPositionDiagonalBottomLeftTopRight([],L,1,2) &
	.member(pos(3,4),L) &
	.member(pos(4,4),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionDiagonalBottomLeftTopRight(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionVerticalTwoInThree(N):
	.println("Running test [ListForceBlockPositionVerticalTwoInThree]") &
	listForceBlockPositionVerticalTwoInThree([],L,1,2) &
	.member(pos(0,3),L) &
	.member(pos(0,5),L) &
	.member(pos(7,3),L) &
	.member(pos(7,5),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 8 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionVerticalTwoInThree(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionHorizontalTwoInThree(N):
	.println("Running test [ListForceBlockPositionHorizontalTwoInThree]") &
	listForceBlockPositionHorizontalTwoInThree([],L,2,1) &
	.print(L) &
	.member(pos(2,2),L) &
	.member(pos(2,0),L) &
	.member(pos(2,6),L) &
	.member(pos(4,7),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 8 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionHorizontalTwoInThree(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionDiagonalTwoInThreeLeft(N):
	.println("Running test [ListForceBlockPositionDiagonalTwoInThreeLeft]") &
	listForceBlockPositionDiagonalTwoInThreeLeft([],L,1,2) &
	listForceBlockPositionDiagonalTwoInThreeLeft([],L2,2,1) &
	.member(pos(4,4),L) &
	.member(pos(4,3),L2) &
	.member(pos(,),L) &
	.member(pos(,),L2) &
	.length(L,M) &
	.length(L2,M2) &
	M = 2 &
	M2 = 2 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionDiagonalTwoInThreeLeft(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionDiagonalTwoInThreeRight(N):
	.println("Running test [ListForceBlockPositionDiagonalTwoInThreeRight]") &
	listForceBlockPositionDiagonalTwoInThreeRight([],L,2,1) &
	listForceBlockPositionDiagonalTwoInThreeRight([],L2,1,2) &
	.print("L1: ",L) &
	.print("L2: ",L2) &
	.member(pos(4,4),L) &
	.member(pos(4,5),L) &
	.member(pos(),L) &
	.member(pos(),L) &
	.member(pos(4,2),L2) &
	.member(pos(4,3),L2) &
	.member(pos(,),L2) &
	.member(pos(,),L2) &
	.length(L,M) &
	.length(L2,M2) &
	M = 4 &
	M2 = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionDiagonalTwoInThreeRight(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionVerticalTwoInFour(N):
	.println("Running test [testListForceBlockPositionVerticalTwoInFour]") &
	listForceBlockPositionVerticalTwoInFour([],L,1,2) &
	.member(pos(0,2),L) & 
	.member(pos(7,5),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionVerticalTwoInFour(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionHorizontalTwoInFour(N):
	.println("Running test [testListForceBlockPositionHorizontalTwoInFour]") &
	listForceBlockPositionHorizontalTwoInFour([],L,1,2) &
	.member(pos(2,0),L) & 
	.member(pos(3,0),L) &
	.member(pos(5,7),L) &
	.member(pos(4,7),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 8 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionHorizontalTwoInFour(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionDiagonalTwoInFourLeft(N):
	.println("Running test [testListForceBlockPositionDiagonalTwoInFourLeft]") &
	listForceBlockPositionDiagonalTwoInFourLeft([],L,1,2) &
	.member(pos(4,3),L) & 
	.member(pos(2,3),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionDiagonalTwoInFourLeft(N) <-
	.print("Test failed");
	N = 0.


+!testListForceBlockPositionDiagonalTwoInFourRight(N):
	.println("Running test [testListForceBlockPositionDiagonalTwoInFourRight]") &
	listForceBlockPositionDiagonalTwoInFourRight([],L,1,2) &
	.member(pos(5,2),L) & 
	.member(pos(3,5),L) &
	.member(pos(,),L) &
	.member(pos(,),L) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		.print("Test sucesfull").

+!testListForceBlockPositionDiagonalTwoInFourRight(N) <-
	.print("Test failed");
	N = 0.



/*+!test(N):
	.println("Running test []") &
	<-
		N = 1;
		.print("Test sucesfull").

+!test(N) <-
	.print("Test failed");
	N = 0.	
*/

//ERRORS
+!play <- .print("Error in !play").
+!playToWin <- .print("Error in !playToWin").
+!playToLose <- .print("Error in !playToLose").
+!playToTest <- .print("Error or finish !playToTest").
+!checkBoard(_,_) <- .print("Error in !checkBoard").
+!tests <- .print("Error in !tests").
+!testPackage1 <- .print("Error in testPackage1").
+!testPackage2 <- .print("Error in testPackage2").
+!testPackage3 <- .print("Error in testPackage3").
+!resetBoardTests <- .print("Error in resetBoardTests").