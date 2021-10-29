:- use_module(library(pita)).

:- pita.



:- begin_lpad.

person(john).
person(mary).
person(bob).

map_query 0.7::burglary.
map_query 0.2::earthquake.

map_query 0.9::alarm :- burglary, earthquake.
map_query 0.8::alarm :- burglary, \+earthquake.
map_query 0.1::alarm :- \+burglary, earthquake.

map_query 0.8::calls(X) :- alarm, person(X).
map_query 0.1::calls(X) :- \+alarm, person(X).

evidence:- \+ calls(john),calls(mary).

:- end_lpad.

/** <examples> Your example queries go here, e.g.

?- map_bdd_dot_string(evidence,BDD,Var,VarA,P,Exp).


*/
