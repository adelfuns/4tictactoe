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
	estrategia(jugarAGanar) &
	listWinPositions([pos(X,Y)|_]).

//If you have only one losing position
decideMovement(X,Y):-
	estrategia(jugarAGanar) &
	listLosePositions([pos(X,Y)]).

//If you have one or more losing position
//decideMovement(X,Y):-
//	estrategia(jugarAGanar) &
//	listLosePositions(L) &
//	.length(L,N) &
//	N > 1 &
//	.

// If you win with a triple []xXXx[]
decideMovement(X,Y):-
	estrategia(jugarAGanar) &
	pairs(PL) &
	not .empty(PL) &
	winnningTriple(LP,X,Y).

// If you win with a triple of the form []XxX[]
decideMovement(X,Y):-
	estrategia(jugarAGanar) &
	twoInThreePairs(PL) &
	not .empty(PL) &
	winnningTripleTwoInThree(PL,X,Y).

// Make the opponent cover you
decideMovement(X,Y):-
	estrategia(jugarAGanar) &
	twoInFourPairs(LP) &
	not .empty(LP) &
	winnningTriple(LP,X,Y).


decideMovement(X,Y):-
	estrategia(jugarAGanar) &
	pairs(LP) &
	not .empty(LP) &
	winnningTriple(LP,X,Y).


/* Initial goals */


!play.


/* Plans */

// PLAY TO WIN
+!play: playerNumbers &
	estrategia(jugarAGanar) <-
		.print("A ganar");
		!tests.
		//!playToTest.

// PLAY TO LOSE
+!play:
	estrategia(jugarAPerder) <-
		.print("A perder");
		!playToLose.

// WINNING PLAN
+!playToWin:
	movement(N) &
	(N = 0) &
	turno(.my_name(X)) <-
	put(3,3);
	-+movement(N+2);
	+advantage(0);
	!playToWin.

+!playToWin:
	movement(N) &
	(N = 1) &
	turno(.my_name(X)) &
	opponent(R) &
	tablero(X0,Y0,R) <-
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
	turno(.my_name(X)) &
	opponent(R) &
	tablero(X0,Y0,R) &
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
	!generateTestBoard;
	Total = 49;
	.println("");
	.println("");
	.println("");
	.println("Running all tests...");
	!testPlayerNumbers(N0,K0);
	.println("");
	!testEnemyNumber(N1,K1);
	.println("");
	!testCheckEmpty(N2,K2);
	.println("");
	!testVertical(N3,K3);
	.println("");
	!testHorizontal(N4,K4);
	.println("");
	!testDiagonal(N5,K5);
	.println("");
	!testVerticalTwoInThree(N6,K6);
	.println("");
	!testHorizontalTwoInThree(N7,K7);
	.println("");
	!testDiagonalTwoInThree(N8,K8);
	.println("");
	!testVerticalTwoInFour(N9,K9);
	.println("");
	!testHorizontalTwoInFour(N10,K10);
	.println("");
	!testDiagonalTwoInFour(N11,K11);
	.println("");
	!testClosestCenterDiagonal(N12,K12);
	.println("");
	!testVerticalPair(N13,K13);
	.println("");
	!testHorizontalPair(N14,K14);
	.println("");
	!testDiagonalPair(N15,K15);
	.println("");
	!testVerticalTwoInThreePair(N16,K16);
	.println("");
	!testHorizontalTwoInThreePair(N17,K17);
	.println("");
	!testDiagonalTwoInThreePair(N18,K18);
	.println("");
	!testVerticalTwoInFourPair(N19,K19);
	.println("");
	!testHorizontalTwoInFourPair(N20,K20);
	.println("");
	!testDiagonalTwoInFourPair(N21,K21);
	.println("");
	!testListVerticalWinPositionsTop(N22,K22);
	.println("");
	!testListVerticalWinPositionsBottom(N23,K23);
	.println("");
	!testListVerticalWinPositionsTwoInThreeTop(N24,K24);
	.println("");
	!testListVerticalWinPositionsTwoInThreeBottom(N25,K25);
	.println("");
	.println("Board redo...");
	put(0,0);
	.wait(500);
	.println("Redoing...");
	.wait(500);
	!generateTestBoard2;
	.print("Now the tests continue...");
	.wait(500);
	!testListHorizontalWinPositionsLeft(N26,K26);
	.println("");
	!testListHorizontalWinPositionsRight(N27,K27);
	.println("");
	!testListHorizontalWinPositionsTwoInThreeLeft(N28,K28);
	.println("");
	!testListHorizontalWinPositionsTwoInThreeRight(N29,K29);
	.println("");
	!testListDiagonalWinPositionsTopLeft(N30,K30);
	.println("");
	!testListDiagonalWinPositionsBottomRight(N31,K31);
	.println("");
	!testListDiagonalWinPositionsTopRight(N32,K32);
	.println("");
	!testListDiagonalWinPositionsBottomLeft(N33,K33);
	.println("");
	!testListDiagonalWinPositionsTwoInThreeTopLeft(N34,K34);
	.println("");
	!testListDiagonalWinPositionsTwoInThreeBottomLeft(N35,K35);
	.println("");
	!testListDiagonalWinPositionsTwoInThreeTopRight(N36,K36);
	.println("");
	!testListDiagonalWinPositionsTwoInThreeBottomRight(N37,K37);
	.println("");
	!testListVerticalWinPositions(N38,K38);
	.println("");
	!testListHorizontalWinPositions(N39,K39);
	.println("");
	!testListDiagonalWinPositions(N40,K40);
	.println("");
	!testListWinPositions(N41,K41);
	.println("");
	!testListLosePositions(N42,K42);
	.println("");
	!testPairs(N43,K43);
	.println("");
	!testTwoInThreePairs(N44,K44);
	.println("");
	!testTwoInFourPairs(N45,K45);
	.println("");
	!testAllMyForms(N46,K46);
	.println("");
	!testAllHisForms(N47,K47);
	//!testDecideMovement(N48,K48);
	N = N0+N1+N2+N3+N4+N5+N6+N7+N8+N9+N10+N11+N12+N13+N14+N15+N16+N17+N18+N19+
	N20+N21+N22+N23+N24+N25+N26+N27+N28+N29+N30+N31+N32+N33+N34+N35+N36+N37+N38
	+N39+N40+N41+N42+N43+N44+N45+N46+N47;//+N48;
	K = K0+K1+K2+K3+K4+K5+K6+K7+K8+K9+K10+K11+K12+K13+K14+K15+K16+K17+K18+K19+
	K20+K21+K22+K23+K24+K25+K26+K27+K28+K29+K30+K31+K32+K33+K34+K35+K36+K37+K38
	+K39+K40+K41+K42+K43+K44+K45+K46+K47;//+K48;
	.println("All tests done...");
	.println("Tests: ",N,"/",K);
	.println("Board reset");
	put(0,0);
	.wait(500);
	.println("Reseted.").



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
		.print("End of !playToTest of player1").

+!playToTest <- !playToTest.


+!testPlayerNumbers(N,K): 
	.println("Running test [testPlayerNumbers]") &
	playerNumbers &
	player(X) &
	opponent(Y) &
	X = 1 &
	Y = 2 <-
		N = 1;
		K = 1;
		-player(1);
		-opponent(2);
		.print("Test [testPlayerNumbers] sucesfull").

+!testPlayerNumbers(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testEnemyNumber(N,K):
	.println("Running test [testEnemyNumber]") &
	enemyNumber(1,N0) &
	enemyNumber(2,N1) &
	N0 = 2 &
	N1 = 1 <-
		N = 1;
		K = 1;
		.print("Test [testEnemyNumber] sucesfull").

+!testEnemyNumber(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testCheckEmpty(N,K):
	.println("Running test [testCheckEmpty]") &
	checkEmpty(0,5) & 
	not checkEmpty(0,0) &
	not checkEmpty(1,1) <-
		N = 1;
		K = 1;
		.print("Test [testCheckEmpty] sucesfull").

+!testCheckEmpty(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testVertical(N,K):
	.println("Running test [testVertical]") &
	vertical(0,5,0,6,0) &
	vertical(0,0,0,1,1) &
	vertical(5,2,5,3,2) &
	not vertical(0,0,0,1,0) &
	not vertical(4,5,4,6,1) &
	not vertical(0,5,0,6,2) <-
		N = 1;
		K = 1;
		.print("Test [testVertical] sucesfull").

+!testVertical(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testHorizontal(N,K):
	.println("Running test [testHorizontal]") &
	horizontal(1,0,2,0,0) &
	horizontal(0,7,1,7,1) &
	horizontal(2,5,3,5,2) &
	not horizontal(2,5,3,5,0) &
	not horizontal(1,0,2,0,1) &
	not horizontal(0,7,1,7,2) <-
		N = 1;
		K = 1;
		.print("Test [testHorizontal] sucesfull").

+!testHorizontal(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testDiagonal(N,K):
	.println("Running test [testDiagonal]") &
	diagonal(1,0,2,1,0) &
	diagonal(0,4,1,5,1) &
	diagonal(1,1,2,2,2) &
	not diagonal(1,1,2,2,0) &
	not diagonal(1,0,2,1,1) &
	not diagonal(0,4,1,5,2) <-
		N = 1;
		K = 1;
		.print("Test [testDiagonal] sucesfull").

+!testDiagonal(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testVerticalTwoInThree(N,K):
	.println("Running test [testVerticalTwoInThree]") &
	verticalTwoInThree(6,4,6,6,1) &
	verticalTwoInThree(7,0,7,2,2) &
	not verticalTwoInThree(6,6,6,4,1) &
	not verticalTwoInThree(0,2,0,4,1) &
	not verticalTwoInThree(0,0,0,2,1) <-
		N = 1;
		K = 1;
		.print("Test [testVerticalTwoInThree] sucesfull").

+!testVerticalTwoInThree(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testHorizontalTwoInThree(N,K):
	.println("Running test [testHorizontalTwoInThree]") &
	horizontalTwoInThree(1,7,3,7,1) &
	horizontalTwoInThree(3,3,5,3,2) &
	not horizontalTwoInThree(3,7,1,7,1) &
	not horizontalTwoInThree(5,3,7,3,2) &
	not horizontalTwoInThree(4,1,6,1,2) <-
		N = 1;
		K = 1;
		.print("Test [testHorizontalTwoInThree] sucesfull").

+!testHorizontalTwoInThree(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testDiagonalTwoInThree(N,K):
	.println("Running test [testDiagonalTwoInThree]") &
	diagonalTwoInThree(1,5,3,7,1) &
	diagonalTwoInThree(5,1,3,3,2) &
	not diagonalTwoInThree(3.3,5,1,2) &
	not diagonalTwoInThree(3,7,1,5,1) &
	not diagonalTwoInThree(1,1,3,3,2) &
	not diagonalTwoInThree(5,2,7,4,2) <-
		N = 1;
		K = 1;
		.print("Test [testDiagonalTwoInThree] sucesfull").

+!testDiagonalTwoInThree(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testVerticalTwoInFour(N,K):
	.println("Running test [testVerticalTwoInFour]") &
	verticalTwoInFour(0,4,0,7,1) &
	verticalTwoInFour(2,2,2,5,2) &
	not verticalTwoInFour(0,7,0,4,1) &
	not verticalTwoInFour(7,0,7,3,2) &
	not verticalTwoInFour(4,4,4,7,1) <-
	N = 1;
	K = 1;
	.print("Test [testVerticalTwoInFour] sucesfull").

+!testVerticalTwoInFour(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testHorizontalTwoInFour(N,K):
	.println("Running test [testHorizontalTwoInFour]") &
	horizontalTwoInFour(0,0,3,0,1) &
	horizontalTwoInFour(0,3,3,3,2) &
	not horizontalTwoInFour(3,0,0,0,1) &
	not horizontalTwoInFour(0,2,3,2,1) & 
	not horizontalTwoInFour(3,4,6,4,1) <-
		N = 1;
		K = 1;
		.print("Test [testHorizontalTwoInFour] sucesfull").

+!testHorizontalTwoInFour(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testDiagonalTwoInFour(N,K):
	.println("Running test [testDiagonalTwoInFour]") &
	diagonalTwoInFour(0,1,3,4,1) &
	diagonalTwoInFour(6,4,3,7,1) &
	not diagonalTwoInFour(3,4,0,1,1) &
	not diagonalTwoInFour(4,4,1,7,1) &
	not diagonalTwoInFour(3,4,0,7,1) &
	not diagonalTwoInFour(6,0,3,3,2) <-
		N = 1;
		K = 1;
		.print("Test [testDiagonalTwoInFour] sucesfull").

+!testDiagonalTwoInFour(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testClosestCenterDiagonal(N,K):
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
		K = 1;
		.print("Test [testClosestCenterDiagonal] sucesfull").

+!testClosestCenterDiagonal(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testVerticalPair(N,K):
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
		K = 1;
		.print("Test [testVerticalPair] sucesfull").

+!testVerticalPair(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testHorizontalPair(N,K):
	.println("Running test [testHorizontalPair]") &
	horizontalPair([],L,1) &
	.member(pairPos(pos(0,7), pos(1,7)),L) &
	.member(pairPos(pos(3,4), pos(4,4)),L) &
	.member(pairPos(pos(3,7), pos(4,7)),L) &
	not .member(pairPos(pos(4,4), pos(3,4)),L) &
	.length(L,M) &
	M = 3 <-
		N = 1;
		K = 1;
		.print("Test [testHorizontalPair] sucesfull").

+!testHorizontalPair(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testDiagonalPair(N,K):
	.println("Running test [testDiagonalPair]") &
	diagonalPair([],L,1) &
	.member(pairPos(pos(0,4), pos(1,5)),L) &
	.member(pairPos(pos(1,6), pos(0,7)),L) &
	not .member(pairPos(pos(1,5), pos(0,4)),L) &
	not .member(pairPos(pos(0,7), pos(1,6)),L) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		K = 1;
		.print("Test [testDiagonalPair] sucesfull").

+!testDiagonalPair(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testVerticalTwoInThreePair(N,K):
	.println("Running test [testVerticalTwoInThreePair]") &
	verticalTwoInThreePair([],L,1) &
	.member(pairPos(pos(6,4), pos(6,6)),L) &
	not .member(pairPos(pos(6,6), pos(6,4)),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testVerticalTwoInThreePair] sucesfull").

+!testVerticalTwoInThreePair(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testHorizontalTwoInThreePair(N,K):
	.println("Running test [testHorizontalTwoInThreePair]") &
	horizontalTwoInThreePair([],L,1) &
	.member(pairPos(pos(1,7), pos(3,7)),L) &
	.member(pairPos(pos(4,4), pos(6,4)),L) &
	.member(pairPos(pos(4,7), pos(6,7)),L) &
	not .member(pairPos(pos(3,7), pos(1,7)),L) &
	.length(L,M) &
	M = 3 <-
		N = 1;
		K = 1;
		.print("Test [testHorizontalTwoInThreePair] sucesfull").

+!testHorizontalTwoInThreePair(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testDiagonalTwoInThreePair(N,K):
	.println("Running test [testDiagonalTwoInThreePair]") &
	diagonalTwoInThreePair([],L,1) &
	.member(pairPos(pos(1,5), pos(3,7)),L) &
	.member(pairPos(pos(4,4), pos(6,6)),L) &
	not .member(pairPos(pos(3,4), pos(1,6)),L) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		K = 1;
		.print("Test [testDiagonalTwoInThreePair] sucesfull").

+!testDiagonalTwoInThreePair(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testVerticalTwoInFourPair(N,K):
	.println("Running test [testVerticalTwoInFourPair]") &
	verticalTwoInFourPair([],L,1) &
	.member(pairPos(pos(0,4), pos(0,7)),L) &
	not .member(pairPos(pos(6,3), pos(6,6)),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testVerticalTwoInFourPair] sucesfull").

+!testVerticalTwoInFourPair(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testHorizontalTwoInFourPair(N,K):
	.println("Running test [testHorizontalTwoInFourPair]") &
	horizontalTwoInFourPair([],L,1) &
	.member(pairPos(pos(0,0), pos(3,0)),L) &
	not .member(pairPos(pos(0,2), pos(3,2)),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testHorizontalTwoInFourPair] sucesfull").

+!testHorizontalTwoInFourPair(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testDiagonalTwoInFourPair(N,K):
	.println("Running test [testDiagonalTwoInFourPair]") &
	diagonalTwoInFourPair([],L,1) &
	.member(pairPos(pos(6,4), pos(3,7)),L) &
	not .member(pairPos(pos(3,1), pos(0,4)),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testDiagonalTwoInFourPair] sucesfull").

+!testDiagonalTwoInFourPair(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testListVerticalWinPositionsTop(N,K):
	.println("Running test [testListVerticalWinPositionsTop]") &
	listVerticalWinPositionsTop([],L,2) &
	.member(pos(5,0),L) &
	.member(pos(7,1),L) &
	not .member(pos(7,5),L) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		K = 1;
		.print("Test [testListVerticalWinPositionsTop] sucesfull").

+!testListVerticalWinPositionsTop(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListVerticalWinPositionsBottom(N,K):
	.println("Running test [testListVerticalWinPositionsBottom]") &
	listVerticalWinPositionsBottom([],L,2) &
	.member(pos(7,5),L) &
	.member(pos(5,4),L) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		K = 1;
		.print("Test [testListVerticalWinPositionsBottom] sucesfull").

+!testListVerticalWinPositionsBottom(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.
	
+!testListVerticalWinPositionsTwoInThreeTop(N,K):
	.println("Running test [testListVerticalWinPositionsTwoInThreeTop]") &
	listVerticalWinPositionsTwoInThreeTop([],L,2) &
	.member(pos(7,1),L) &
	not .member(pos(5,0),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListVerticalWinPositionsTwoInThreeTop] sucesfull").

+!testListVerticalWinPositionsTwoInThreeTop(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListVerticalWinPositionsTwoInThreeBottom(N,K):
	.println("Running test [testListVerticalWinPositionsTwoInThreeBottom]") &
	listVerticalWinPositionsTwoInThreeBottom([],L,1) &
	.member(pos(6,5),L) &
	not .member(pos(7,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListVerticalWinPositionsTwoInThreeBottom] sucesfull").

+!testListVerticalWinPositionsTwoInThreeBottom(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testListHorizontalWinPositionsLeft(N,K):
	.println("Running test [testListHorizontalWinPositionsLeft]") &
	listHorizontalWinPositionsLeft([],L,1) &
	.member(pos(1,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListHorizontalWinPositionsLeft] sucesfull").

+!testListHorizontalWinPositionsLeft(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListHorizontalWinPositionsRight(N,K):
	.println("Running test [testListHorizontalWinPositionsRight]") &
	listHorizontalWinPositionsRight([],L,1) &
	.member(pos(5,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListHorizontalWinPositionsRight] sucesfull").

+!testListHorizontalWinPositionsRight(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListHorizontalWinPositionsTwoInThreeLeft(N,K):
	.println("Running test [testListHorizontalWinPositionsTwoInThreeLeft]") &
	listHorizontalWinPositionsTwoInThreeLeft([],L,1) &
	.member(pos(1,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListHorizontalWinPositionsTwoInThreeLeft] sucesfull").

+!testListHorizontalWinPositionsTwoInThreeLeft(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListHorizontalWinPositionsTwoInThreeRight(N,K):
	.println("Running test [testListHorizontalWinPositionsTwoInThreeRight]") &
	listHorizontalWinPositionsTwoInThreeRight([],L,1) &
	.member(pos(5,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListHorizontalWinPositionsTwoInThreeRight] sucesfull").

+!testListHorizontalWinPositionsTwoInThreeRight(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testListDiagonalWinPositionsTopLeft(N,K):
	.println("Running test [testListDiagonalWinPositionsTopLeft]") &
	listDiagonalWinPositionsTopLeft([],L,2) &
	.member(pos(1,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListDiagonalWinPositionsTopLeft] sucesfull").

+!testListDiagonalWinPositionsTopLeft(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListDiagonalWinPositionsBottomRight(N,K):
	.println("Running test [testListDiagonalWinPositionsBottomRight]") &
	listDiagonalWinPositionsBottomRight([],L,2) &
	.member(pos(5,5),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListDiagonalWinPositionsBottomRight] sucesfull").

+!testListDiagonalWinPositionsBottomRight(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListDiagonalWinPositionsTopRight(N,K):
	.println("Running test [testListDiagonalWinPositionsTopRight]") &
	listDiagonalWinPositionsTopRight([],L,2) &
	.member(pos(6,2),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListDiagonalWinPositionsTopRight] sucesfull").

+!testListDiagonalWinPositionsTopRight(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListDiagonalWinPositionsBottomLeft(N,K):
	.println("Running test [testListDiagonalWinPositionsBottomLeft]") &
	listDiagonalWinPositionsBottomLeft([],L,2) &
	.member(pos(2,6),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListDiagonalWinPositionsBottomLeft] sucesfull").

+!testListDiagonalWinPositionsBottomLeft(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListDiagonalWinPositionsTwoInThreeTopLeft(N,K):
	.println("Running test [testListDiagonalWinPositionsTwoInThreeTopLeft]") &
	listDiagonalWinPositionsTwoInThreeTopLeft([],L,2) &
	.member(pos(1,1),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListDiagonalWinPositionsTwoInThreeTopLeft] sucesfull").

+!testListDiagonalWinPositionsTwoInThreeTopLeft(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testListDiagonalWinPositionsTwoInThreeBottomRight(N,K):
	.println("Running test [testListDiagonalWinPositionsTwoInThreeBottomRight]") &
	listDiagonalWinPositionsTwoInThreeBottomRight([],L,2) &
	.member(pos(5,5),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListDiagonalWinPositionsTwoInThreeBottomRight] sucesfull").

+!testListDiagonalWinPositionsTwoInThreeBottomRight(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListDiagonalWinPositionsTwoInThreeTopRight(N,K):
	.println("Running test [testListDiagonalWinPositionsTwoInThreeTopRight]") &
	listDiagonalWinPositionsTwoInThreeTopRight([],L,2) &
	.member(pos(6,2),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListDiagonalWinPositionsTwoInThreeTopRight] sucesfull").

+!testListDiagonalWinPositionsTwoInThreeTopRight(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.
	

+!testListDiagonalWinPositionsTwoInThreeBottomLeft(N,K):
	.println("Running test [testListDiagonalWinPositionsTwoInThreeBottomLeft]") &
	listDiagonalWinPositionsTwoInThreeBottomLeft([],L,2) &
	.member(pos(2,6),L) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testListDiagonalWinPositionsTwoInThreeBottomLeft] sucesfull").

+!testListDiagonalWinPositionsTwoInThreeBottomLeft(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListVerticalWinPositions(N,K):
	.println("Running test [testListVerticalWinPositions]") &
	listVerticalWinPositions(L,1) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		K = 1;
		.print("Test [testListVerticalWinPositions] sucesfull").

+!testListVerticalWinPositions(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListHorizontalWinPositions(N,K):
	.println("Running test [testListHorizontalWinPositions]") &
	listHorizontalWinPositions(L,1) &
	.length(L,M) &
	M = 2 <-
		N = 1;
		K = 1;
		.print("Test [testListHorizontalWinPositions] sucesfull").

+!testListHorizontalWinPositions(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListDiagonalWinPositions(N,K):
	.println("Running test [testListDiagonalWinPositions]") &
	listDiagonalWinPositions(L,2) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		K = 1;
		.print("Test [testListDiagonalWinPositions] sucesfull").

+!testListDiagonalWinPositions(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testListWinPositions(N,K):
	.println("Running test [testListWinPositions]") &
	playerNumbers &
	listWinPositions(L) &
	.length(L,M) &
	M = 6 <-
		.abolish(player(1));
		.abolish(opponent(2));
		N = 1;
		K = 1;
		.print("Test [testListWinPositions] sucesfull").

+!testListWinPositions(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testListLosePositions(N,K):
	.println("Running test [testListLosePositions]") &
	playerNumbers &
	listLosePositions(L) &
	.length(L,M) &
	M = 5 <-
		.abolish(player(1));
		.abolish(opponent(2));
		N = 1;
		K = 1;
		.print("Test [testListLosePositions] sucesfull").

+!testListLosePositions(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testPairs(N,K):
	.println("Running test [testPairs]") &
	pairs(L,1) &
	.length(L,M) &
	M = 13 <-
		N = 1;
		K = 1;
		.print("Test [testPairs] sucesfull").

+!testPairs(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testTwoInThreePairs(N,K):
	.println("Running test [testTwoInThreePairs]") &
	twoInThreePairs(L,1) &
	.length(L,M) &
	M = 4 <-
		N = 1;
		K = 1;
		.print("Test [testTwoInThreePairs] sucesfull").

+!testTwoInThreePairs(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testTwoInFourPairs(N,K):
	.println("Running test [testTwoInFourPairs]") &
	twoInFourPairs(L,1) &
	.length(L,M) &
	M = 1 <-
		N = 1;
		K = 1;
		.print("Test [testTwoInFourPairs] sucesfull").

+!testTwoInFourPairs(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.


+!testAllMyForms(N,K):
	.println("Running test [testAllMyForms]") &
	playerNumbers &
	allMyForms(LP,TTL,TFL) &
	.length(LP,M1) &
	M1 = 13<-
		.abolish(player(1));
		.abolish(opponent(2));
		N = 1;
		K = 1;
		.print("Test [testAllMyForms] sucesfull").

+!testAllMyForms(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.

	
+!testAllHisForms(N,K):
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
		K = 1;
		.print("Test [testAllHisForms] sucesfull").

+!testAllHisForms(N,K) <-
	.print("Test failed");
	K = 1;
	N = 0.
	


//ERRORS
+!play <- .print("Error in !play").
+!playToWin <- .print("Error in !playToWin").
+!playToLose <- .print("Error in !playToLose").
+!playToTest <- .print("Error or finish !playToTest").
+!checkBoard(_,_) <- .print("Error in !checkBoard").
+!tests <- .print("Error in !tests").