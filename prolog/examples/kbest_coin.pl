/*
Throwing a coin with uncertainty on its fairness, from
J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated
disjunctions. In International Conference on Logic Programming,
volume 3131 of LNCS, pages 195-209. Springer, 2004.
*/
:- use_module(library(kbest)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- kbest.

:- begin_lpad.

heads(Coin): 1/2; tails(Coin) : 1/2:-toss(Coin),\+biased(Coin).
% if we toss a Coin that is not biased then it lands heads with probability 1/2
% and tails with probability 1/2
heads(Coin): 0.6 ; tails(Coin) : 0.4:-toss(Coin),biased(Coin).
% if we toss a Coin that is biased then it lands heads with probability 0.6
% % and tails with probability 0.4
fair(Coin):0.9 ; biased(Coin):0.1.
% a Coin is fair with probability 0.9 and biased with probability 0.1
toss(coin).
% coin is certainly tossed

:- end_lpad.

/** <examples>

?- kbest(heads(coin),1,Prob,Exp).
 Prob=0.45
 Exp = [0.45000000000000007-[rule(0, heads(coin), [heads(coin):0.5, tails(coin):0.5], [toss(coin), \+biased(coin)]),
 	rule(2, fair(coin), [fair(coin):0.9, biased(coin):0.1], [])]]),


*/
