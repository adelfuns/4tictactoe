# TIC TAC TOE IN A MULTYAGENT SYSTEM WITH JASON

## Definition

This program will play a TIC TAC TOE with 4 in a row/column/diagonal as winning condition. The agents have two strategies: **play to win** and **play to lose**. The board is 8x8 rows and columns respectively.

NOTE: Is not a *connect 4* game, there is no gravity, you can put your chip were you want.

### Play to win

The agent will play with winning conditions in mind, he will try to connect 4 before the rival.

### Play to lose

The agent will play trying to make the opponent connect 4 before himself.

## Strategy

The strategy is one that reduces the complexity of having a board longer that 4x4. Whenever a player puts a chip in the board, the agent will all possible "little boards" (mini-board now forward) of 4x4 around that position.

Example:
```
     1     2     3     4     5     6     7     8  
1 |     |     |     |     |     |     |     |     |
2 |     |     |     |     |     |     |     |     |
3 |     |     |     |  x  |  x  |  x  |     |     |
4 |     |     |     |  o  |  o  |  o  |     |     |
5 |     |     |     |     |     |     |     |     |
6 |     |     |     |     |     |     |     |     |
7 |     |     |     |     |     |     |     |     |
8 |     |     |     |     |     |     |     |     |
```
*Base setting*

```
     1     2     3     4     5     6     7     8  
1 |     |     |     |     |     |     |     |     |
2 |     |     |     |     |     |     |     |     |
3 |     |     |     |  x  |  x  |  x  |     |     |
4 |     |     |     |  o  |  o  |  o  |     |     |
5 |     |     |     |  x  |     |     |     |     |
6 |     |     |     |     |     |     |     |     |
7 |     |     |     |     |     |     |     |     |
8 |     |     |     |     |     |     |     |     |
```
*Player one puts a x in row 5 column 4*

```
     1     2     3     4    
1 |  x  |  x  |  x  |     |
2 |  o  |  o  |  o  |     |
3 |  x  |     |     |     |
4 |     |     |     |     | 
```
*First 4x4 board evaluated*
```
     1     2     3     4    
1 |  o  |  o  |  o  |     |
2 |  x  |     |     |     |
3 |     |     |     |     | 
4 |     |     |     |     |
```
*Second 4x4 board evaluated*

It will evaluate all possible boards that can be created with a movement. In order to decide a movement, the agent will always evaluate first the rival's movement. If it is the first movement it will put his chip in one of the 
four centers of the board.

This strategy will evaluate every mini-board and play that as if it was a common tic tac toe. Once evaluated the best outcome in each case, it will make a decision. The decision and the outcome of each case will change based in the strategy that the agent is following.

## Implementation

The program runs in [JASON](http://jason.sourceforge.net/). To run it you will need to download, instructions to run a program are in the page.

## Disclaimer

The board is not our creation.