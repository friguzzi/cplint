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

path(X,Y):- edge(X,Z), a(X,Z), path(Z,Y).
% there is surely a path between X and Y if there is another 
% node Z such that
% there is an edge between X and Z, the abducible fact
% representing that edge is selected, and there is a path 
% between Z and Y 

abducible a(a,b).
% fact a(a,b) is abducible
edge(a,b):0.1.
% there is an edge between a and b with probability 0.1

abducible a(b,e).
edge(b,e):0.5.

abducible a(a,c).
edge(a,c):0.3.

abducible a(c,d).
edge(c,d):0.4.

abducible a(d,e).
edge(d,e):0.4.

abducible a(a,e).
edge(a,e):0.1.

:- a(X,Y), a(X,Z), Y \= Z.
% integrity constraints that prevent that two edges with the same 
% source node are selected

:- end_lpad.

% predicate to plot the induced graph
graph(digraph([rankdir='LR'|G])):-
    findall(edge((A -> B),[label=P]),
      clause(edge(A,B,_,_),(get_var_n(_,_,_,_,[P|_],_),_)),
      G).


/** <examples>

?- abd_prob(path(a,e),Prob,Delta). 
% Prob is the probability that exists a path between a and e
% Delta is the set of abducibles that maximizes the joint
% probability of the query and the integrity constraint, i.e,
% the probabilistic abductive explanation
% ?- abd_prob(path(a,e),Prob,Delta).
% Prob = 0.1,
% Delta = [[a(a, e)]].

% If we set the probability of edge(a,b) to 0.2,
% edge(a,b):0.2.
% we get
% ?- abd_prob(path(a,e),Prob,Delta).
% Prob = 0.1,
% Delta = [[a(a, b), a(b, e)], [a(a, e)]].

?- abd_bdd_dot_string(path(a,e),BDD,A,B).
% Prints the BDD for query path(a,e)
% A solid edge indicates a 1-child, a dashed edge indicates a 0-child 
% and a dotted edge indicates a negated 0-child.
% The two tables contain the associations between the rule groundings 
% and the multivalued variables (abducibles and probabilistic facts)
*/
