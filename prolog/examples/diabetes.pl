/** <examples>
 
?- mc_sample(dm,1000,Prob).
?- mc_mh_sample(dm,e,10000,Prob).

*/

:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.

:- mc.
:- begin_lpad.

predisposition(average):0.698;predisposition(moderate):0.227;predisposition(high):0.075.
dm:0.054:-
  predisposition(average).
dm:0.131:-
  predisposition(moderate).
dm:0.266:-
  predisposition(high).

gluc_if_dm(G):gaussian(G,7.5,3.8).
gluc_if_not_dm(G):gaussian(G,5.79,0.98).

noise_if_dm(N):gaussian(N,0.0,3.3).
noise_if_not_dm(N):gaussian(N,0.0,0.3).

hba1c(H):-
    dm,
    gluc_if_dm(G),
    noise_if_dm(N),
    {H=:=1.4+0.92*G+N}.

hba1c(H):-
    \+ dm,
    gluc_if_not_dm(G),
    noise_if_not_dm(N),
    {H=:=0.6+0.9*G+N}.

e:- hba1c(H),{H>7.2}.
:- end_lpad.
