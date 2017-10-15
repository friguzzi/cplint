
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.

:- set_pita(tabling,explicit).

:- begin_lpad.

win :- red, green.
win :- blue, yellow.

 0.4::red.
 0.9::green.
 0.5::blue.
 0.6::yellow.


:- end_lpad.

/** <examples>


?- vit_prob(win,P,Exp).
P=0.36
Exp=[
  rule(0, red, [red:0.4, '':0.6], []),
  rule(1, green, [green:0.9, '':0.09999999999999998], [])]

?-vit_bdd_dot_string(win,G,LV,P,MAP).

*/
