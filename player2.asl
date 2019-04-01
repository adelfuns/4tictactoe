// Agent player2 in project cuatroenraya.mas2j



/* Initial beliefs and rules */


/* Initial goals */



/* Plans */



//Enviar en REPL AGENT ESTO .send(player2,achieve,ponFicha(X,Y))
+!ponFicha(X,Y)[source(_)]:
	turno(player2) <-
		put(X,Y).
		
+!ponFicha(_,_)[_] <- .print("Error in !ponFicha").


// Plan to play a few rounds and generate a board's state to test
+!test[source(player1)] <-
	+testPut(0,3);
	+testPut(1,1);
	+testPut(1,4);
	+testPut(2,2);
	+testPut(2,5);
	+testPut(3,3);
	+testPut(3,5);
	+testPut(4,1);
	+testPut(4,5);
	+testPut(5,6);
	+testPut(5,2);
	+testPut(5,3);
	+testPut(6,0);
	+testPut(6,1);
	+testPut(7,0);
	+testPut(7,2);
	+testPut(7,3);
	+testPut(7,4);
	+testPut(5,1);
	!playToTest.


+!test2[source(player1)] <-
	+testPut(0,0);
	+testPut(2,2);
	+testPut(3,3);
	+testPut(4,4);
	+testPut(6,6);
	+testPut(7,1);
	+testPut(5,3);
	+testPut(3,5);
	+testPut(1,7);
	+testPut(6,5);
	+testPut(6,7);
	!playToTest.


+!playToTest:
	testPut(X,Y) &
	turno(player2) <- 
		put(X,Y);
		-testPut(X,Y);
		-+movement(N+1);
		!playToTest.
		
+!playToTest:
	not testPut(X,Y) <-
		.print("End of !playToTest of player2").
		
+!playToTest <- !playToTest.

// ERRORS
+!test <- .print("Error or finish !test").
+!playToTest <- .print("Error or finish !playToTest").