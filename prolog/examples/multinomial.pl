:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

m(S,N,P):multinomial(S,N,P).

query(N,P):-
    m(S,N,P),
    S=[N1,N2|_],
    N1>N2.
    
:- end_lpad.

prob_query(N,P,Prob):-
  mc_sample(query(N,P),1000,Prob).