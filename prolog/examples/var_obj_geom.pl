/*
Existence uncertainty/unknown objects.
This programs models a domain where the number of objects is uncertain.
In particular, the number of objects follows a geometric distribution 
with parameter 0.7.
We can ask what is the probability that the object number n exists.
This program uses directly the geometric distribution primitive.
From
Poole, David. "The independent choice logic and beyond." Probabilistic 
inductive logic programming. Springer Berlin Heidelberg, 2008. 222-243.
*/


:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- mc.

:- begin_lpad.
numObj_1(N):geometric(N,0.7).

numObj(N):-
  numObj_1(N0),
  N is N0-1.

obj(I):-
 numObj(N),
 between(1, N, I).

:- end_lpad.



/** <examples>

?- mc_prob(obj(2),P). % what is the probability that object 2 exists?
% expected result ~ 0.08992307692307693
?- mc_prob(obj(2),P),bar(P,C). % what is the probability that object 2 exists?
% expected result ~ 0.08992307692307693
?- mc_prob(obj(5),P). % what is the probability that object 5 exists?
% expected result ~ 0.002666
?- mc_prob(obj(5),P),bar(P,C). % what is the probability that object 5 exists?
% expected result ~ 0.002666
?- mc_prob(numObj(2),P). % what is the probability that there are 2 objects?
% expected result ~ 0.0656
?- mc_prob(numObj(5),P). % what is the probability that there are 5 objects?
% expected result ~ 0.0014
?- mc_sample(obj(5),1000,P,[successes(T),failures(F)]). % take 1000 samples of obj(5)
?- mc_sample(obj(5),1000,P),bar(P,C). % take 1000 samples of obj(5)
?- mc_sample_arg(numObj(N),100,N,O),argbar(O,C). % take 100 samples of L in
% findall(N,numObj(N),L)
?- mc_sample_arg(obj(I),100,I,O),argbar(O,C). % take 100 samples of L in
% findall(I,obj(I),L)
?- mc_sample_arg(obj(I),100,I,Values). % take 100 samples of L in 
% findall(I,obj(I),L)

*/

