/*
Computing the probability of a path between two nodes in a probabilistic graph.
Each edge has a probability of being present.
From
L. De Raedt, A. Kimmig, and H. Toivonen. ProbLog: A probabilistic Prolog and
its application in link discovery. In International Joint Conference on
Artificial Intelligence, pages 2462-2467, 2007.
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.

:- begin_lpad.

% path(X,Y) is true if there is a path between nodes X and Y
% edge(a,b) indicates that there is an edge between a and b

path(X,X).
% there is surely a path between a node and itself

path(X,Y):-
  edge(X,Z),path(Z,Y).
% there is surely a path between X and Y if there is another node Z such that
% there is an edge between X and Z and there is a path between Z and Y

edge(a,b):0.2.
% there is an edge between a and b with probability 0.2
edge(b,e):0.5.
edge(a,c):0.3.
edge(c,d):0.4.
edge(d,e):0.4.
edge(a,e):0.1.

:- end_lpad.

graph(digraph([rankdir='LR'|G])):-
    findall(edge((A -> B),[label=P]),
      clause(edge(A,B,_,_),(get_var_n(_,_,_,_,[P|_],_),_)),
      G).


/** <examples>

?- prob(path(a,e),Prob). % what is the probability that a and e are connected?
% expected result 0.22888
?- prob(path(a,e),Prob),bar(Prob,C). % what is the probability that a and e are connected?
% expected result 0.22888
?- graph(G). % shows the probabilistic graph

?- bdd_dot_string(path(a,e),BDD,Var).
% What is the BDD for query path(a,e)?
% A solid edge indicates a 1-child, a dashed edge indicates a 0-child and
% a dotted
% edge indicates a negated 0-child.
% The table Var contains the associations between the rule groundings and the
% multivalued variables.

*/
