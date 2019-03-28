// Agent player2 in project cuatroenraya.mas2j

/* Initial beliefs and rules */
// TEST POSITIONS
testPut(1,3).
testPut(1,2).
testPut(0,6).
testPut(2,1).
testPut(3,6).
testPut(3,4).
testPut(3,5).
testPut(4,1).
testPut(5,5).
testPut(4,5).
testPut(2,2).
testPut(4,6).
testPut(4,7).
testPut(6,1).
testPut(7,0).
testPut(7,1).
testPut(7,2).
testPut(7,7).
testPut(4,3).


movement(1).


// Gets the player number and adds to the beliefs player(playerNumber).
playerNumber :- 
	.my_name(N) &
	.term2string(N,S)&
	.length(S,M) &
	.substring(S,X,(M-1)) &
	.term2string(Y,X) &
	.asserta(player(Y)).


checkEmpty(X,Y):-
	tablero(X,Y,0).

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
	(Y0 = Y1 - 1) &
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
	(Y4 = Y3 + 1) &
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
	(X0 = X1 - 1) &
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
	(X4 = X3 + 1) &
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
	(X3 = X2 + 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X0,Y0) & 
	(Y0 = Y1 - 1) &
	(X0 = X1 - 1) &
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
	(X3 = X2 - 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X0,Y0) & 
	(Y4 = Y3 + 1) &
	(X4 = X3 - 1) &
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
	(X3 = X2 - 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X0,Y0) & 
	(Y0 = Y1 - 1) &
	(X0 = X1 + 1) &
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
	(X3 = X2 - 1) &
	(Y3 = Y2 + 1) &
	checkEmpty(X0,Y0) & 
	(Y4 = Y3 - 1) &
	(X4 = X3 + 1) &
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
	(Y2 = Y1 + 1) &
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
	(Y3 = Y2 + 1) &
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
	(X2 = X1 + 1) &
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
	(X3 = X2 + 1) &
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
	(X4 = X3 + 1) & 
	(Y4 = Y3 + 1) &
	(X2 = X1 + 1) &
	(Y2 = Y1 + 1) &
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
	(X4 = X1 + 3) & 
	(Y4 = Y1 + 3) &
	(X3 = X2 + 1) &
	(Y3 = Y2 + 1) &
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
	(X4 = X3 - 1) & 
	(Y4 = Y3 + 1) &
	(X2 = X1 - 1) &
	(Y2 = Y1 + 1) &
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
	(X4 = X1 - 3) & 
	(Y4 = Y1 + 3) &
	(X3 = X2 - 1) &
	(Y3 = Y2 + 1) &
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
	player(P) <-
		?winnerTotal(L,P);
		.print(L).
		
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