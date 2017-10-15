
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.

:- begin_lpad.

red(b1):- pick(b1), pf(1, 1).
green(b1):- pick(b1), pf(1, 2), \+ pf(1, 1).
blue(b1):- pick(b1), \+ pf(1, 2), \+ pf(1, 1).
pick(b1):- pf(2, 1).
no_pick(b1):- \+ pf(2, 1).
map_query 0.6::pf(1, 1).
map_query 0.75::pf(1, 2).
map_query 0.6::pf(2, 1).

ev:- \+ blue(b1).
:- end_lpad.

/** <examples>
?- map_bdd_dot_string(ev,BDD,Var,VarA,P,Exp).

?- abd_prob(win,Prob,Exp).



*/
