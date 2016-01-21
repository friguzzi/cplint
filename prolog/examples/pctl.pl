
:- use_module(library(pita)).

:- cplint.
% State Formulae 
models(S, prop(A)) :-
  holds(S,A).
models(S, neg(A)) :-
  \+ models(S,A).
models(S, and(SF1, SF2)):-
  models(S, SF1),
  models(S, SF2).
models(S, pr(PF, gt, B)) :-
  prob(pmodels(S, PF), P),
  P > B.
models(S, pr(PF, geq, B)) :-
  prob(pmodels(S, PF), P),
  P >= B.
% Path Formulae
pmodels(S, PF) :-
  pmodels(S, PF, _).

pmodels(S, until(_SF1, SF2), _H) :-
  models(S,SF2).
pmodels(S, until(SF1, SF2), H) :-
  models(S, SF1),
  trans(S, H, T),
  pmodels(T, until(SF1, SF2), next(H)).
pmodels(S, next(SF), H) :-
  trans(S, H, T),
  pmodels(T, SF).

% reach(S, I, T) starting at state S at instance I,
%   state T is reachable.
reach(S, I, T) :-
  trans(S, I, U),
  reach(U, next(I), T).

reach(S, _, S).

% trans(S,I,T) transition from S at instance I goes to T

trans(s0,_,s0):0.5.
trans(s0,_,s1):0.3.
trans(s0,_,s2):0.2.

trans(s1,_,s1):0.4.
trans(s1,_,s3):0.1.
trans(s1,_,s4):0.5.

trans(s4,_,s3).
:- end_cplint.
