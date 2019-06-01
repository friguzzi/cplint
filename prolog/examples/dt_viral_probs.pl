:- use_module(library(pita)).

:- pita.

:- begin_lpad.

marketed(theo).
marketed(bernd).
marketed(guy).
marketed(ingo).
marketed(angelika).
marketed(martijn).
marketed(laura).
marketed(kurt).

% Probabilistic facts
0.2 :: buy_from_marketing(_). % :- format('market: ~w ~n',[X]).
0.3 :: buy_from_trust(_,_). % :- format('trust: ~w ~w ~n',[X,Y]).

trusts(X,Y) :- trusts_directed(X,Y).%, format('~w ~w ~n',[X,Y]).
trusts(X,Y) :- trusts_directed(Y,X).%, format('~w ~w ~n',[X,Y]).

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

compute_probs(PB,PI,PT,PA,PG,PM,PL,PK):-
    prob(buys(bernd),PB),
    prob(buys(ingo),PI),
    prob(buys(theo),PT),
    prob(buys(angelika),PA),
    prob(buys(guy),PG),
    prob(buys(martijn),PM),
    prob(buys(laura),PL),
    prob(buys(kurt),PK).