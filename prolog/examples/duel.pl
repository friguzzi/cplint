/*
duello a tre, vedi libro di federico peiretti il grande gioco dei numeri
*/

/** <examples>
?- survives([a,b,c],a,0).
?- mc_sample(survives([a,b,c],a,0),5,P,[]).
best_strategy(A,Rest,L,T,S,P):-
?- best_strategy(a,[b,c],[a,b,c],0,S,P).
% expected result 1

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
    table(best_strategy(H,Rest,L0,0,S,P)),
    write(best_strategy(H,Rest,L0,T,S,P)),nl,
    write(a=A),nl,
    shoot(H,S,Rest,L0,T,Rest1,L1),
    write(shoot(H,S,Rest,L0,T,Rest1,L1)),nl,
    member(A,L1),
    survives_round(Rest1,L1,A,T,L).

%survives_bounded(_,_,s(s(s(s(s(s(0))))))):-!.

survives_bounded([A],A,_):-!.

survives_bounded(L,A,T):-
    survives_round_bounded(L,L,A,T,L1),
    survives_bounded(L1,A,s(T)).

survives_round_bounded(_,[A],A,_,[A]):-!.

survives_round_bounded([],L,_,_,L).

survives_round_bounded([H|Rest],L0,A,T,L):-
	write(survives_round([H|Rest],L0,A,T,L)),nl,
    table(best_strategy(H,Rest,L0,T,S,_P)),
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
    append(L1,[sky],L2),
    maplist(ev_action(A,Rest,T,L),L2,LP),
    write(LP),nl,
    keysort(LP,[P-S|_]).
%    max_member(P-S,LP).

ev_action(A,Rest,T,L,S,P-S):-
    mc_sample(survives_rest(A,Rest,T,L,S),5,P,[]).

%ev_action(A,Rest,T,L,S,P-S):-
%    mc_sample(survives_rest1(A,Rest,T,L,S),5,P,[]).

survives_rest1(A,Rest0,T,L0,S):-
    shoot(A,S,Rest0,L0,T,Rest,L1),
    write((survives_rest(A,Rest0,T,L0,S),shoot(A,S,Rest0,L0,T,Rest,L1))),nl,
    survives_round_bounded(Rest,L1,A,T,L),
    survives_bounded(L,A,s(T)).

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


table(A):-
  tabled(A),!.

table(A):-
  call(A),
  assert(tabled(A)).

hit(_,a):1/3.

hit(_,b):2/3.

hit(_,c):1.



:- end_lpad.

:- retractall(tabled(_)).
