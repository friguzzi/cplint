
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.

:- mc.

:- begin_lpad.

a(X):binomial(X,20,0.5).

:- end_lpad.



/** <examples>
?- mc_expectation(a(X),1000,X,E).
*/