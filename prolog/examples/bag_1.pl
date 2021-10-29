
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.

:- begin_lpad.

0.6::red(b1); 0.3::green(b1); 0.1::blue(b1) :- pick(b1).
map_query 0.6::pick(b1); 0.4::no_pick(b1).

ev:- \+ blue(b1).

:- end_lpad.

/** <examples>
?- map(ev,P,Exp).
P = 0.5399999999999999,
Exp = [rule(1, pick(b1), [pick(b1):0.6, no_pick(b1):0.4], true)].

*/
