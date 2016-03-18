/*
Throwing a coin with uncertainty on its fairness, from
J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated 
disjunctions. In International Conference on Logic Programming, 
volume 3131 of LNCS, pages 195-209. Springer, 2004.
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

heads:0.6;tails:0.4. 
g(X): gaussian(X,0, 1).
h(X):gaussian(X,5, 2).

mix(X) :- heads, g(X).
mix(X) :- tails, h(X).


:- end_lpad.

hist(G):-
  mc_sample_arg(mix(X),1000,X,L0),
  maplist(val,L0,L),
  max_list(L,Max),
  min_list(L,Min),
  D is Max-Min,
  NBins=20,
  BinWidth is D/NBins,
  bin(NBins,L,Min,BinWidth,LB),
  Chart = c3{data:_{x:elem, rows:[elem-freq|LB], type:bar},
          axis:_{ rotated: true
             },
                   size:_{height: 100},
                  legend:_{show: false}}.

bin(0,_L,_Min,_BW,[]):-!.

bin(N,L,Lower,BW,[V-Freq|T]):-
  V is Lower+BW/2,
  Upper is Lower+BW,
  

val([E]-_,E).
/** <examples>

?- mc_prob(heads(coin),Prob).  % what is the probability that coin lands heads?
% expected result 0.51
?- mc_prob(tails(coin),Prob).  % what is the probability that coin lands tails?
% expected result 0.49
?- mc_prob_bar(heads(coin),Prob).  % what is the probability that coin lands heads?
% expected result 0.51
?- mc_prob_bar(tails(coin),Prob).  % what is the probability that coin lands tails?
% expected result 0.49
?- mc_sample(heads(coin),1000,T,F,Prob).  
% take 1000 sample of heads(coin) and return the number of successes (T),
% the number of failures (F) and the probability

?- mc_sample(tails(coin),1000,T,F,Prob).  
% take 1000 sample of tails(coin) and return the number of successes (T),
% the number of failures (F) and the probability

?- mc_sample(heads(coin),1000,Prob).  
% take 1000 sample of heads(coin) and return the probability

?- mc_sample(tails(coin),1000,Prob).  
% take 1000 sample of tails(coin) and return the probability

?- mc_sample_bar(heads(coin),1000,Chart).  
% take 1000 sample of heads(coin) and chart the number of successes and 
% faliures

?- mc_sample_bar(tails(coin),1000,Chart).  
% take 1000 sample of tails(coin) and chart the number of successes and 
% faliures

?- mc_rejection_sample(heads(coin),biased(coin),1000,S,F,P).
% take 1000 sample of heads(coin) given that biasdd(coin) is true
% Use rejection sampling
% F = 387,
% P = 0.613,
% S = 613
*/
 
