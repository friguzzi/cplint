
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.

:- begin_lpad.

win :- red, green.
win :- blue, yellow.

map_query 0.4::red.
map_query 0.9::green.
map_query 0.5::blue.
map_query 0.6::yellow.


:- end_lpad.

/** <examples>
?- map_bdd_dot_string(win,BDD,Var,VarA,P,Exp).

?- abd_prob(win,Prob,Exp).



*/
