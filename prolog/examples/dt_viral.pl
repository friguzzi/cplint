:- use_module(library(pita)).

:- pita.

:- begin_lpad.

? :: marketed(theo).
? :: marketed(bernd).
? :: marketed(guy).
? :: marketed(ingo).
? :: marketed(angelika). 
? :: marketed(martijn).
? :: marketed(laura).
? :: marketed(kurt).

utility(marketed(theo),-2).
utility(marketed(bernd),-2).
utility(marketed(guy),-2).
utility(marketed(ingo),-2).
utility(marketed(angelika),-2).
utility(marketed(martijn),-2).
utility(marketed(laura),-2).
utility(marketed(kurt),-2).

utility(buys(theo),5).
utility(buys(bernd),5).
utility(buys(guy),5).
utility(buys(ingo),5).
utility(buys(angelika),5).
utility(buys(martijn),5).
utility(buys(laura),5).
utility(buys(kurt),5).

0.2 :: buy_from_marketing(_).
0.3 :: buy_from_trust(_,_).

trusts(X,Y) :- trusts_directed(X,Y).
trusts(X,Y) :- trusts_directed(Y,X).

trusts_directed(bernd,ingo).
trusts_directed(ingo,theo).
trusts_directed(theo,angelika).
trusts_directed(bernd,martijn).
trusts_directed(ingo,martijn).
trusts_directed(martijn,guy).
trusts_directed(guy,theo).
trusts_directed(guy,angelika).
trusts_directed(laura,ingo).
trusts_directed(laura,theo).
trusts_directed(laura,guy).
trusts_directed(laura,martijn).
trusts_directed(kurt,bernd).

buys(X):-
    marketed(X),
    buy_from_marketing(X).
buys(X):-
    trusts(X,Y),
    buy_from_trust(X,Y),
    buys(Y).

:- end_lpad.

/*
 * ?- dt_solve(Strategy,Value).
 * Expected result:
 * Strategy = [[marketed(theo)],[marketed(martijn)],[marketed(ingo)],[marketed(guy)]]
 * Value = 3.21
*/
