
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.


binomial(N,1.0,N):-!.

binomial(N,P,X):-
  Pr0 is (1-P)^N,
  random(U),
  binomial_cycle(0,X,N,P,Pr0,Pr0,U).

binomial_cycle(X,X,_N,_P,_Pr,CPr,U):-
  U=<CPr,!.

binomial_cycle(X0,X,N,P,Pr0,CPr0,U):-
  X1 is X0+1,
  Pr is Pr0*P*(N-X0)/(X1*(1-P)),
  CPr is CPr0+Pr,
  binomial_cycle(X1,X,N,P,Pr,CPr,U).

disc(binomial(_,_)).

:- mc.

:- begin_lpad.

a(X):user(X,binomial(20,0.5)).

:- end_lpad.


/** <examples>
?- mc_expectation(a(X),1000,X,E).
*/