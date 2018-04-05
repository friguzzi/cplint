/*
Dirichlet process (DP), see https://en.wikipedia.org/wiki/Dirichlet_process
Samples are drawn from a base distribution. New samples have a nonzero
probability of being equal to already sampled values. The process depends
on a parameter alpha (concentration parameter): with alpha->0, a single 
value is sampled, with alpha->infinite the distribution is equal to the base
distribution.
In this example the base distribution is a Guassian with mean 0 and variance
1, as in https://en.wikipedia.org/wiki/Dirichlet_process#/media/File:Dirichlet_process_draws.svg
To model the process, this example uses a stick breaking process: to sample
a value, a sample beta_1 is taken from Beta(1,alpha) and a coin with heads
probability beta_1 is flipped. If the coin lands heads, a sample from the base
distribution is taken and returned. Otherwise, a sample beta_2 is taken again
from Beta(1,alpha) and a coin is flipped. This procedure is repeated until
a heads is obtained, the index of i beta_i is the index of the value to be 
returned.
The example queries show both the distribution of indexes and values of the DP.
Moreover, they show the distribution of unique indexes as in 
http://www.robots.ox.ac.uk/~fwood/anglican/examples/viewer/?worksheet=nonparametrics/dp-mixture-model
*/
/** <examples>
?- hist(200,100,G).
% show the distribution of indexes with concentration parameter 10. 
?- hist_val(200,100,G).
% show the distribution of values with concentration parameter 10. Should look
% like row 2 of https://en.wikipedia.org/wiki/Dirichlet_process#/media/File:Dirichlet_process_draws.svg
?- hist_repeated_indexes(100,40,G).
% show the distribution of unique indexes in 100 samples with concentration parameter 10. 


*/
 :- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

% dp_n_values(N0,N,Alpha,L)
% returns in L a list of N-N0 samples from the DP with concentration parameter
% Alpha
dp_n_values(N,N,_Alpha,[]):-!.

dp_n_values(N0,N,Alpha,[[V]-1|Vs]):-
  N0<N,
  dp_value(N0,Alpha,V),
  N1 is N0+1,
  dp_n_values(N1,N,Alpha,Vs).
  
% dp_value(NV,Alpha,V)
% returns in V the NVth sample from the DP with concentration parameter
% Alpha
dp_value(NV,Alpha,V):-
  dp_stick_index(NV,Alpha,I),
  dp_pick_value(I,V).

% dp_pick_value(I,V)
% returns in V the value of index I of the base distribution 
% (in this case N(0,1))
dp_pick_value(_,V):gaussian(V,0,1).

% dp_stick_index(NV,Alpha,I)
% returns in I the index of the NVth sample from the DP
dp_stick_index(NV,Alpha,I):-
  dp_stick_index(1,NV,Alpha,I).

dp_stick_index(N,NV,Alpha,V):-
  stick_proportion(N,Alpha,P),
  choose_prop(N,NV,Alpha,P,V).
  
% choose_prop(N,NV,Alpha,P,V)
% returns in V the index of the end of the stick breaking process starting
% from index N for the NVth value to be sampled from the DP
choose_prop(N,NV,_Alpha,P,N):-
  pick_portion(N,NV,P).

choose_prop(N,NV,Alpha,P,V):-
  neg_pick_portion(N,NV,P),
  N1 is N+1,
  dp_stick_index(N1,NV,Alpha,V).
 
% sample of the beta_i parameters
stick_proportion(_,Alpha,P):beta(P,1,Alpha).

% flip of the coing for the portion of the stick of size P
pick_portion(N,NV,P):P;neg_pick_portion(N,NV,P):1-P.

:- end_lpad.

hist(Samples,NBins,Chart):-
  mc_sample_arg(dp_stick_index(1,10.0,V),Samples,V,L),
  histogram(L,Chart,[nbins(NBins)]).

hist_repeated_indexes(Samples,NBins,Chart):-
  repeat_sample(0,Samples,L),
  histogram(L,Chart,[nbins(NBins)]).

repeat_sample(S,S,[]):-!.

repeat_sample(S0,S,[[N]-1|LS]):-
  mc_sample_arg_first(dp_stick_index(1,1,10.0,V),10,V,L),
  length(L,N),
  S1 is S0+1,
  repeat_sample(S1,S,LS).

hist_val(Samples,NBins,Chart):-
  mc_sample_arg_first(dp_n_values(0,Samples,10.0,V),1,V,L),
  L=[Vs-_],
  histogram(Vs,Chart,[nbins(NBins)]).



