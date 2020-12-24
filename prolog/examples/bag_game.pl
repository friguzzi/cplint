
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

0.4::red.
0.9::green.
0.5::blue.
0.6::yellow.


:- end_lpad.

/** <examples>

?- prob(win,P).
P= 0.552

*/
