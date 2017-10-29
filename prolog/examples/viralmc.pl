/*
Viral Marketing
*/

/** <examples>
?- prob(has(2),do(has(3)),P).
?- prob(has(2),has(3),P).
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(graphviz).
:- endif.
graph(digraph([rankdir="LR"|G])):-
    findall(edge(A -> B,[]),
      clause(trusts(A,B),_),
      G).

:- mc.

:- begin_lpad.

:- mcaction has/1.

has(_):0.1.

has(P) :0.4 :- trusts(P, Q), has(Q).

trusts(2,1).
trusts(3,1).
trusts(3,2).
trusts(4,1).
trusts(4,3).
:-end_lpad.
