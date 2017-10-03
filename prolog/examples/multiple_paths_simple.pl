:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.

:- begin_lpad.

p:- a,b.

p:- \+ a,\+b.

a:0.3.

b:0.4.

:- end_lpad.


/** <examples>

?- bdd_dot_string(p,BDD,Var).
?- prob(p,P).
*/
