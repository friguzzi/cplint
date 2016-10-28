:- use_module(library(pita)).


:- pita.

:- begin_lpad.

a:0.5.

b:0.5.

p:0.3:-a,b.

p:0.7:- \+ a,b.

p:0.4:- \+ a,\+ b.

p:0.6 :- a,\+b.

evidence:-p,b.

a_b:-a,b.
a_nb:-a,\+b.
na_b:- \+a,b.
na_nb:- \+a,\+b.

:- end_lpad.

/** <examples>

?- prob(a,evidence,P).
?- prob(a_b,p,P).
?- prob(a_nb,p,P).
?- prob(na_b,p,P).
?- prob(na_nb,p,P).

*/

