/*
Random arithmetic function from http://forestdb.org/models/arithmetic.html
The model generatively defines a random arithmetic function.
The problem is to predict the value returned by the function given one or
two couples of input-output, i.e., to compute a conditional probability.
Translated from the Church functional probabilistic programming language.
Sampling is necessary as queries have an infinite number of explanations.
Both rejection sampling and Metropolis/Hastings can be applied.
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- mc.

:- begin_lpad.

eval(X,Y):-
  random_fn(X,0,F),
  Y is F.

op(L,+):0.5;op(L,-):0.5.



random_fn(X,L,F):-
  comb(L),
  random_fn(X,l(L),F1),
  random_fn(X,r(L),F2),
  op(L,Op),
  F=..[Op,F1,F2].

random_fn(X,L,F):-
  \+ comb(L),
  base_random_fn(X,L,F).

comb(_):0.3.

base_random_fn(X,L,X):-
  identity(L).

base_random_fn(_X,L,C):-
  \+ identity(L),
  random_const(L,C).

identity(_):0.5.

random_const(_,C):discrete(C,[0:0.1,1:0.1,2:0.1,3:0.1,4:0.1,
  5:0.1,6:0.1,7:0.1,8:0.1,9:0.1]).





:- end_lpad.

/** <examples>

?- mc_mh_sample(eval(2,4),eval(1,3),100,P,
  [mix(100),lag(3),successes(T),failures(F)]).
% perform Metropolis Hastings sampling of eval(2,Y) given that
% eval(1,3) is true (100 samples, 100 mixing samplesi, lag 3)
% expected result
% T = 17,
% F = 83,
% P = 0.17.

?- mc_mh_sample(eval(2,4),eval(1,3),100,P,
  [lag(3),successes(T),failures(F)]).
% perform Metropolis Hastings sampling of eval(2,Y) given that
% eval(1,3) is true
% expected result
% T = 17,
% F = 83,
% P = 0.17.

?- mc_mh_sample(eval(2,4),(eval(0,2),eval(1,3)),100,P,
  [lag(3),successes(T),failures(F)]).
% perform Metropolis Hastings sampling of eval(2,Y) given that
% eval(0,2) and eval(1,3) are true
% expected result
% T = 100,
% F = 0,
% P = 1.

?- mc_gibbs_sample(eval(2,4),eval(1,3),100,P,
  [mix(100),successes(T),failures(F)]).
% perform Metropolis Hastings sampling of eval(2,Y) given that
% eval(1,3) is true (100 samples, 100 mixing samplesi, lag 3)
% expected result
% T = 17,
% F = 83,
% P = 0.17.

?- mc_gibbs_sample(eval(2,4),eval(1,3),500,P,[]).
% expected result
% P = 0.17.

?- mc_gibbs_sample(eval(2,4),eval(1,3),100,P,
  [successes(T),failures(F),mix(100)]).
% perform Gibbs sampling of eval(2,Y) given that
% eval(1,3) is true
% expected result
% T = 17,
% F = 83,
% P = 0.17.

?- mc_gibbs_sample(eval(2,4),(eval(0,2),eval(1,3)),100,P,
  []).
% perform Gibbs sampling of eval(2,Y) given that
% eval(0,2) and eval(1,3) are true
% expected result
% P = 1.

?-  mc_rejection_sample(eval(2,4),eval(1,3),100,P,
  [successes(T),failures(F)]).
% perform rejection sampling of eval(2,4) given that eval(1,3) is true
% expected result
% T = 10,
% F = 90,
% P = 0.1.

?- mc_mh_sample_arg(eval(2,Y),(eval(0,2),eval(1,3)),100,Y,V,
  [mix(100),lag(3)]).
% sample arg Y of eval(2,Y) given that 
% eval(0,2) and eval(1,3) are true
% Sample using Metropolis Hastings
% exected result
% V = [[4]-100].
?- mc_mh_sample_arg(eval(2,Y),(eval(0,2),eval(1,3)),100,Y,V,
  [mix(100),lag(3)]),argbar(V,C).

?- mc_mh_sample_arg(eval(2,Y),eval(1,3),100,Y,V,
  [mix(100),lag(3)]).
% sample arg Y of eval(2,Y) given that 
% eval(1,3) is true
% Sample using Metropolis Hastings
% exected result
% V = [[3]-52, [6]-20, [5]-16, [4]-12]
?- mc_mh_sample_arg(eval(2,Y),eval(1,3),100,Y,V,
  [mix(100),lag(3)]),argbar(V,C).


?- mc_rejection_sample_arg(eval(2,Y),eval(1,3),100,Y,V).
% sample argument Y of eval(2,Y) given that 
% eval(1,3) is true
% Sample using rejection sampling
% exected result
% V = [[3]-79, [4]-8, [6]-8, [2]-5].
?- mc_rejection_sample_arg(eval(2,Y),eval(1,3),100,Y,V),argbar(V,C).

?- mc_expectation(eval(2,Y),100,Y,E).
% what is the expected value of Y in eval(2,Y)?
% expected result
% E = 3.48

?- mc_mh_expectation(eval(2,Y),eval(1,3),100,Y,E,
  [mix(100),lag(3)]).
% what is the expected value of Y in eval(2,Y) given that eval(1,3) is true?
% expected result
% E = 3.52
?- mc_rejection_expectation(eval(2,Y),eval(1,3),100,Y,E).
% what is the expected value of Y in eval(2,Y) given that eval(1,3) is true?
% expected result
% E = 3.06
*/
 
