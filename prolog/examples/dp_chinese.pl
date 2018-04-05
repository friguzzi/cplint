/*
Dirichlet process (DP), see https://en.wikipedia.org/wiki/Dirichlet_process
Samples are drawn from a base distribution. New samples have a nonzero
probability of being equal to already sampled values. The process depends
on a parameter alpha (concentration parameter): with alpha->0, a single 
value is sampled, with alpha->infinite the distribution is equal to the base
distribution.
In this example the base distribution is a Guassian with mean 0 and variance
1, as in https://en.wikipedia.org/wiki/Dirichlet_process#/media/File:Dirichlet_process_draws.svg
To model the process, this example uses the Chinese Restaurant Process: 
# Draw <math>X_{1}</math> from the base distribution <math>H</math>.
# For <math>n>1</math>: <poem>
:::: a)  With probability  <math>\frac{\alpha}{\alpha+n-1}</math>  draw <math>X_{n}</math> from <math>H</math>.

:::: b)  With probability  <math>\frac{n_{x}}{\alpha+n-1}</math>  set <math>X_{n}=x</math>, where <math>n_{x}</math> is the number of previous observations <math>X_{j}, j<n</math>, such that <math>X_{j}=x</math>. </poem>

The example queries show both the distribution of indexes and values of the DP.
Moreover, they show the distribution of unique indexes as in 
http://www.robots.ox.ac.uk/~fwood/anglican/examples/viewer/?worksheet=nonparametrics/dp-mixture-model
*/
/** <examples>
?- hist_val(200,100,G).
% show the distribution of values with concentration parameter 10. Should look
% like row 2 of https://en.wikipedia.org/wiki/Dirichlet_process#/media/File:Dirichlet_process_draws.svg

*/
:- use_module(library(mcintyre)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

% dp_n_values(N0,N,Alpha,L,Counts0,Counts)
% returns in L a list of N-N0 samples from the DP with concentration parameter
% Alpha and initial counts Counts0. Also returns the updated counts Counts
dp_n_values(N,N,_Alpha,[],Counts,Counts):-!.

% dp_value(NV,Alpha,V)
% returns in V the NVth sample from the DP with concentration parameter
% Alpha
dp_n_values(N0,N,Alpha,[[V]-1|Vs],Counts0,Counts):-
  N0<N,
  dp_value(N0,Alpha,Counts0,V,Counts1),
  N1 is N0+1,
  dp_n_values(N1,N,Alpha,Vs,Counts1,Counts).
  
dp_value(NV,Alpha,Counts,V,Counts1):-
  draw_sample(Counts,NV,Alpha,I),
  update_counts(0,I,Alpha,Counts,Counts1),
  dp_pick_value(I,V).


update_counts(_I0,_I,Alpha,[_C],[1,Alpha]):-!.


update_counts(I,I,_Alpha,[C|Rest],[C1|Rest]):-
  C1 is C+1.

update_counts(I0,I,Alpha,[C|Rest],[C|Rest1]):-
  I1 is I0+1,
  update_counts(I1,I,Alpha,Rest,Rest1).

draw_sample(Counts,NV,Alpha,I):-
  NS is NV+Alpha,
  maplist(div(NS),Counts,Probs),
  length(Counts,LC),
  numlist(1,LC,Values),
  maplist(pair,Values,Probs,Discrete),
  take_sample(NV,Discrete,I).

take_sample(_,D,V):discrete(V,D).

% dp_pick_value(I,V)
% returns in V the value of index I of the base distribution 
% (in this case N(0,1))
dp_pick_value(_,V):gaussian(V,0,1).

div(Den,V,P):- P is V/Den.

pair(A,B,A:B).

:- end_lpad.


hist_val(Samples,NBins,Chart):-
  mc_sample_arg_first(dp_n_values(0,Samples,10.0,V,[10.0],_),1,V,L),
  L=[Vs-_],
  histogram(Vs,Chart,[nbins(NBins)])

