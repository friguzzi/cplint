
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(graphviz).
:- endif.

:- pita.

:- begin_lpad.
:- action drug/0.

female:0.5.

recovery:0.6:- drug,\+ female.
recovery:0.7:- \+ drug,\+ female.

recovery:0.2:- drug,female.
recovery:0.3:- \+ drug,female.

drug:30/40:- \+ female.
drug:10/40:-female.


:-end_lpad.

/** <examples>
?-prob(recovery,drug,P).
?-prob(recovery,\+ drug,P).
?-prob(recovery,(drug,female),P).
?-prob(recovery,(\+drug,female),P).
?-prob(recovery,(drug,\+female),P).
?-prob(recovery,(\+ drug,\+female),P).
?-prob(recovery,do(drug),P).
?-prob(recovery,do(\+ drug),P).
?-prob(recovery,(do(drug),female),P).
?-prob(recovery,(do(\+drug),female),P).
?-prob(recovery,(do(drug),\+ female),P).
?-prob(recovery,(do(\+ drug),\+ female),P).
*/
