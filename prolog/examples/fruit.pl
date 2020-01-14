:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.

:- mc.
:- begin_lpad.
yield(apple,Y): gaussian(Y,12000.0, 1000.0).
yield(banana,Y): gaussian(Y,10000.0, 1500.0).
support(apple): 0.3.
support(banana):0.5.
basic_price(apple,B):- 
  yield(apple,Y),
  {B=:=250-0.007 * Y}.
basic_price(banana,B):- 
  yield(banana,Y),
  {B=:=200-0.006 * Y}.
price(Fruit,P):- 
  basic_price(Fruit,B),
  support(Fruit), 
  {P=:=B+50}.
price(Fruit,B):- 
  basic_price(Fruit,B),
  \+ support(Fruit).
buy(Fruit):-
  price(Fruit,P),
  max_price(Fruit,M),{P =< M}.
max_price(apple,M):gamma(M,10.0, 18.0).
max_price(banana,M): gamma(M,12.0, 10.0).
:- end_lpad.

