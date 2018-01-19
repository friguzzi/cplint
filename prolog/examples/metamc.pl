/*
Probabilities computation in the body of probabilistic clauses.
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- mc.

:- set_mc(k,20).

:- begin_lpad.

a:0.2:-
  mc_sample(b,20,P,[]),
  P>0.2.

b:0.5:-
  c.

c.

:- end_lpad.

/** <examples>

?- mc_prob(a,Prob,[]).
% expected result 0.2

?- mc_sample(a,10,Prob,[]).
% expected result 0.2


*/
