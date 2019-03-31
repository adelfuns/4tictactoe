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
testPut(1,5).
testPut(3,0).
testPut(3,1).
testPut(3,2).
testPut(3,4).
testPut(3,7).
testPut(4,4).
testPut(4,7).
testPut(6,3).
testPut(6,4).
testPut(6,6).
testPut(6,7).

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
	verticalTwoInThree(X1,Y1,X1,Y3,P) &
	vertical(X0,Y0,X1,Y1,P) &
	checkEmpty(X1,Y2) &
	(Y2 = Y1 + 1) &
	.concat([pos(X1,Y2)],[],TmpL) &
	listVerticalWinPositionsTwoInThreeTop(TmpL,L,P).

listVerticalWinPositionsTwoInThreeTop(TmpL,L,P) :-
	verticalTwoInThree(X1,Y1,X1,Y3,P) &
	vertical(X0,Y0,X1,Y1,P) &
	checkEmpty(X1,Y2) &
	(Y2 = Y1 + 1) &
	not .member(pos(X1,Y2),TmpL) &
	.concat([pos(X1,Y0)],TmpL,TmpL2) &
	listVerticalWinPositionsTwoInThreeTop(TmpL2,L,P).

listVerticalWinPositionsTwoInThreeTop([],[],P).
listVerticalWinPositionsTwoInThreeTop(TmpL,L,P) :- L = TmpL.


listVerticalWinPositionsTwoInThreeBottom([],L,P) :-
	verticalTwoInThree(X1,Y0,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y1) &
	(Y1 = Y0 + 1) &
	.concat([pos(X1,Y1)],[],TmpL) &
	listVerticalWinPositionsTwoInThreeBottom(TmpL,L,P).

listVerticalWinPositionsTwoInThreeBottom(TmpL,L,P) :-
	verticalTwoInThree(X1,Y0,X1,Y2,P) &
	vertical(X1,Y2,X1,Y3,P) &
	checkEmpty(X1,Y1) &
	(Y1 = Y0 + 1) &
	not .member(pos(X1,Y1),TmpL) &
	.concat([pos(X1,Y1)],TmpL,TmpL2) &
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
	.print("All values: (",X0,",",Y0,"),(",X1,",",Y1,"),(",X2,",",Y2,"),(",X3,",",Y3,")") &
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
	.print("All values: (",X0,",",Y0,"),(",X1,",",Y1,"),(",X2,",",Y2,"),(",X3,",",Y3,")") &
	listDiagonalWinPositionsTwoInThreeTopLeft(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeTopLeft([],[],P).
listDiagonalWinPositionsTwoInThreeTopLeft(TmpL,L,P) :- L = TmpL.


listDiagonalWinPositionsTwoInThreeBottomLeft([],L,P) :-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	diagonal(X0,Y0,X1,Y1,P) &
	(X3 = X1 + 2) &
	(Y3 = Y1 + 2) &
	checkEmpty(X2,Y2) &
	(X2 = X1 + 1) &
	(Y2 = Y1 + 1) &
	.concat([pos(X2,Y2)],[],TmpL) &
	.print("All values: (",X0,",",Y0,"),(",X1,",",Y1,"),(",X2,",",Y2,"),(",X3,",",Y3,")") &
	listDiagonalWinPositionsTwoInThreeBottomLeft(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeBottomLeft(TmpL,L,P) :-
	diagonalTwoInThree(X1,Y1,X3,Y3,P) &
	diagonal(X0,Y0,X1,Y1,P) &
	(X3 = X1 + 2) &
	(Y3 = Y1 + 2) &
	checkEmpty(X2,Y2) &
	(X2 = X1 + 1) &
	(Y2 = Y1 + 1) &
	not .member(pos(X2,Y2),TmpL) &
	.concat([pos(X2,Y2)],[],TmpL) &
	.print("All values: (",X0,",",Y0,"),(",X1,",",Y1,"),(",X2,",",Y2,"),(",X3,",",Y3,")") &
	listDiagonalWinPositionsTwoInThreeBottomLeft(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeBottomLeft([],[],P).
listDiagonalWinPositionsTwoInThreeBottomLeft(TmpL,L,P) :- L = TmpL.


listDiagonalWinPositionsTwoInThreeTopRight([],L,P) :-
	diagonalTwoInThree(X3,Y0,X1,Y2,P) &
	diagonal(X0,Y3,X1,Y2,P) &
	(X1 = X3 - 2) &
	(Y2 = Y0 + 2) &
	checkEmpty(X2,Y1) &
	(X2 = X0 + 2) &
	(Y1 = Y0 + 1) &
	.concat([pos(X2,Y1)],[],TmpL) &
	listDiagonalWinPositionsTwoInThreeTopRight(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeTopRight(TmpL,L,P) :-
	diagonalTwoInThree(X3,Y0,X1,Y2,P) &
	diagonal(X0,Y3,X1,Y2,P) &
	(X1 = X3 - 2) &
	(Y2 = Y0 + 2) &
	checkEmpty(X2,Y1) &
	(X2 = X0 + 2) &
	(Y1 = Y0 + 1) &
	not .member(pos(X2,Y1),TmpL) &
	.concat([pos(X2,Y1)],[],TmpL) &
	listDiagonalWinPositionsTwoInThreeTopRight(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeTopRight([],[],P).
listDiagonalWinPositionsTwoInThreeTopRight(TmpL,L,P) :- L = TmpL.


listDiagonalWinPositionsTwoInThreeBottomRight([],L,P) :-
	diagonalTwoInThree(X0,Y3,X2,Y1,P) &
	diagonal(X2,Y1,X3,Y0,P) &
	(X2 = X0 + 2) &
	(Y1 = Y3 - 2) &
	checkEmpty(X1,Y2) &
	(X1 = X3 - 2) &
	(Y2 = Y1 + 1) &
	.concat([pos(X1,Y2)],[],TmpL) &
	listDiagonalWinPositionsTwoInThreeBottomRight(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeBottomRight(TmpL,L,P) :-
	diagonalTwoInThree(X0,Y3,X2,Y1,P) &
	diagonal(X2,Y1,X3,Y0,P) &
	(X2 = X0 + 2) &
	(Y1 = Y3 - 2) &
	checkEmpty(X1,Y2) &
	(X1 = X3 - 2) &
	(Y2 = Y1 + 1) &
	not .member(pos(X1,Y2),TmpL) &
	.concat([pos(X1,Y2)],[],TmpL) &
	listDiagonalWinPositionsTwoInThreeBottomRight(TmpL,L,P).

listDiagonalWinPositionsTwoInThreeBottomRight([],[],P).
listDiagonalWinPositionsTwoInThreeBottomRight(TmpL,L,P) :- L = TmpL.



// Check all of the player's chip forms
allMyForms(LP,TTL,TFL):-
	player(P) &
	pairs(PL,P) &
	twoInThreePairs(TTL,P) &
	twoInFourPairs(TFL,P).


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
twoInThreePairs(PL) :-
	verticalTwoInThreePair([], VL, P) &
	horizontalTwoInThreePair([], HL, P) &
	diagonalTwoInThreePair([], DL, P) &
	.union(VL, HL, TmpL) &
	.union(TmpL, DL, PL).


// Forms a list of all pairs of chips of the form X[][]X in the board
twoInFourPairs(PL) :-
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
	diagonalTwoInThree(X1,Y1,X1,Y3,P) &
	.concat([pairPos(pos(X1,Y1), pos(X3,Y3))],[],TmpL) &
	diagonalTwoInThreePair(TmpL,DTL,P).
	 
diagonalTwoInThreePair(TmpL,DTL,P) :-
	diagonalTwoInThree(X1,Y1,X1,Y3,P) &
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

// Decide movement rules
decideMovement(X,Y):-
	estrategia(jugarAGanar) &
	listWinPositions([pos(X,Y)|_]).

decideMovement(X,Y):-
	estrategia(jugarAGanar) &
	listLosePositions([pos(X,Y)]).

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
		!playToTest.

// PLAY TO LOSE
+!play:
	estrategia(jugarAPerder) <-
		.print("A perder");
		!playToLose.
		
// TEST AREA		
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
		!playToWin.

+!playToTest <- !playToTest.


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
		-+movementRecord([pos(X0,Y0)|Lº]);
		+actualMovement(pos(X0,Y0));
		?listWinPositions(LWin);
		?listLosePositions(LLose);
		?pa
		?decideMovement(X1,Y1);
		put(X1,Y1);
		-+movement(N+1);
		-actualMovement(pos(X0,Y0));
		!playToWin.

!playToWin <- !playToWin. 


// LOSING PLAN





// MOVEMENT PLAN
+!checkBoard(L,E):
	player(M) <-
		?listDiagonalWinPositionsTwoInThreeTopLeft([],L1,M);
		.print("Diagonal 2in3 TopLeft positions: ", L1)
		?listDiagonalWinPositionsTwoInThreeBottomLeft([],L2,M);
		.print("Diagonal 2in3 BottomLeft positions: ", L2)
		?listDiagonalWinPositionsTwoInThreeTopRight([],L3,M);
		.print("Diagonal 2in3 TopRight positions: ", L3)
		?listDiagonalWinPositionsTwoInThreeBottomRight([],L4,M);
		.print("Diagonal 2in3 BottomRight positions: ", L4).
		
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
