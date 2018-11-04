/*
Computing the probability of success of an attack to the Bitcoin protocol
From Damiano Azzolini, Fabrizio Riguzzi, Evelina Lamma, Elena Bellodi, 
and Riccardo Zese.  
Modeling bitcoin protocols with probabilistic logic programming. 
PLP 2018 http://ceur-ws.org/Vol-2219/paper6.pdf
 
*/
:- use_module(library(mcintyre)).
:- mc.
:- begin_lpad.

% progress function
attacker_progress_poisson(X):poisson(X,Lambda):-
   Lambda is 10*0.3/0.7.
attacker_progress_pascal(X):pascal(X,10,0.3).

move(T,1):0.7; move(T,-1):0.3.

% catch up function
walk(InitialPosition):-
  	walk(InitialPosition,0).

walk(0,_).
walk(X,T0):-
    X > 0, 
    X < 100, %threshold for not winning
    move(T0,Move),
    T1 is T0+1,
    X1 is X+Move,
    walk(X1,T1).

success_poisson:-
    attacker_progress_poisson(A),
    V is 10 - A,
    (   V = 0 ->  
    	true;
    	walk(V)
    ).

success_pascal:-
    attacker_progress_pascal(A),
    V is 10 - A,
    (   V = 0 ->  
    	true;
    	walk(V)
    ).

:- end_lpad.

/** <examples>
?- mc_prob(success_poisson,Prob).
?- mc_prob(success_pascal,Prob).
*/
