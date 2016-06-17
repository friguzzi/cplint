/*
duello a tre, vedi libro di federico peiretti il grande gioco dei numeri
*/

/** <examples>
% see http://www.prismmodelchecker.org/casestudies/synchronous_leader.php
% What is the probability that eventually a leader is elected?
?- best_strategy([a,b,c],S).
% expected result 1

% What is the probability of electing a leader within 3 rounds?
?- mc_sample(bounded_eventually(elect,3),1000,P).
% expected result 0.97

% What is the expected number of rounds to elect a leader?
?- mc_expectation(eventually(elect,T),1000,T,E).
% expected result 1.2


?- graph_prob(G). 
% graph the probability of electing a leader within L rounds as
% a function of L

?- graph_exp_rounds_n(G).
% graph the expected number of rounds to elect a leader as a
% funtion of the number of processes when K=3

?- graph_exp_rounds_k(G).
% graph the expected number of rounds to elect a leader as a
% funtion of K when N=3

?- network_topology(G).
% draw a graph representing the network topology
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.
:- dynamic kr/1,num/1.
:- mc.

:- begin_lpad.

survives([A],A,_):-!.

survives(L,A,T):-
    survives_round(L,L,A,T,L1),
    survives(L1,A,s(T)).

survives_round(_,[A],A,_,[A]):-!.

survives_round([],L,_,_,L).

survives_round([H|Rest],L0,A,T,L):-
	write(survives_round([H|Rest],L0,A,T,L)),nl,
    best_strategy(H,Rest,L0,0,S,_P),
    write(best_strategy(H,Rest,L0,T,S,_P)),nl,
    write(a=A),nl,
    shoot(H,S,Rest,L0,T,Rest1,L1),
    write(shoot(H,S,Rest,L0,T,Rest1,L1)),nl,
    member(A,L1),
    survives_round(Rest1,L1,A,T,L).

survives_bounded(_,_,s(s(0))):-!.

survives_bounded([A],A,_):-!.

survives_bounded(L,A,T):-
    survives_round_bounded(L,L,A,T,L1),
    survives_bounded(L1,A,s(T)).

survives_round_bounded(_,[A],A,_,[A]):-!.

survives_round_bounded([],L,_,_,L).

survives_round_bounded([H|Rest],L0,A,T,L):-
	write(survives_round([H|Rest],L0,A,T,L)),nl,
    best_strategy(H,Rest,L0,T,S,_P),
    write(best_strategy(H,Rest,L0,T,S,_P)),nl,
    write(a=A),nl,
    shoot(H,S,Rest,L0,T,Rest1,L1),
    write(shoot(H,S,Rest,L0,T,Rest1,L1)),nl,
    member(A,L1),
    survives_round_bounded(Rest1,L1,A,T,L).
      

best_strategy(A,Rest,L,T,S,P):-
write(best_strategy(A,Rest,L,T,S,P)),nl,
    delete(L,A,L1),
    write(l1=L1),nl,
    %maplist(ev_action(A,Rest,T,L),[sky|L],LP),
    maplist(ev_action(A,Rest,T,L),[sky|L1],LP),
    write(LP),nl,
    max_member(P-S,LP).

ev_action(A,Rest,T,L,S,P-S):-
    mc_sample(survives_rest(A,Rest,T,L,S),10,P).

survives_rest(A,Rest0,T,L0,S):-
    shoot(A,S,Rest0,L0,T,Rest,L1),
    write((survives_rest(A,Rest0,T,L0,S),shoot(A,S,Rest0,L0,T,Rest,L1))),nl,
    survives_round_bounded(Rest,L1,A,T,L),
    survives_bounded(L,A,s(T)).

shoot(H,S,Rest0,L0,T,Rest,L):-
    (S=sky ->  
      L=L0,
      Rest=Rest0
    ;
      (hit(T,H) ->
        delete(L0,S,L),
        delete(Rest0,S,Rest)
      ;   
        L=L0,
        Rest=Rest0
      )
    ).



hit(_,a):1/3.

hit(_,b):2/3.

hit(_,c):1.

    

:- end_lpad.





