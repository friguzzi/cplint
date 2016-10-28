/*
Program describing the Monty Hall puzzle which gets its name from the TV game 
show hosted by Monty Hall. A player is given the opportunity to
select one of three closed doors, behind one of which there is a prize. 
Behind the other two doors are empty rooms.
Once the player has made a selection, Monty is obligated to open one of the 
remaining closed doors which does not contain the prize, showing that the room 
behind it is empty. He then asks the player if he would like to switch
his selection to the other unopened door, or stay with his original choice. 
Here is the problem: Does it matter if he switches?

From Chitta Baral, Michael Gelfond, and Nelson Rushton. (2009)
"Probabilistic reasoning with answer sets." 
Theory and Practice of Logic Programming 9.01, 57-144.
Simplified program code by Richard de Rozario.

Casting the game into a probability logic program reveals the key to the puzzle.
Namely, that Monty will reveal a goat no matter what the player picks.  
Therefore, the probability of revealing a goat is 100%.  In turn,
this means that getting a car when switching depends solely on having
picked a goat originally -- which of course had a probability of 2/3.
*/

:- use_module(library(pita)).
:- pita.
:- begin_lpad.

picked(car):1/3.
picked(goat):- \+ picked(car).

revealed(goat):- picked(_).

switch_gets(car):- picked(goat), revealed(goat).

:- end_lpad.

main:-
prob(switch_gets(car),P),
format('Probability that switching gets the car is ~2f',[P]).

/** <examples>

?- main.

*/
