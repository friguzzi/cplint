:- use_module(library(pita)).

:- pita.

:- begin_lpad.

holdsAt(F, T) :-
  initially(F),
  \+ clipped(0, F, T).
holdsAt(F, T2) :- 
  initiates(F, T1),
  T1 < T2 ,
  \+ clipped(T1 ,F, T2 ).
clipped(T1 ,F, T3) :-
  terminates(F, T2),
  T1 < T2 , T2 < T3.

initiates(locatedIn(A, B), T):0.66 :- 
  happens(arrive(A, B), T).
terminates(locatedIn(A, D), T):0.66 :-
  happens(arrive(A, B), T),
  B \= D.
initially(locatedIn(bob, garden)).
happens(arrive(bob, kitchen), 3). 
happens(arrive(bob, garage), 5).

:- end_lpad.

/** <examples>


?- prob(holdsAt(locatedIn(bob,garden),2),P).
% 1.0
?- prob(holdsAt(locatedIn(bob,garden),4),P). 
% 0.34
?- prob(holdsAt(locatedIn(bob,kitchen),4),P).
% 0.66
?- prob(holdsAt(locatedIn(bob,garage),6),P).
% 0.66.

*/

