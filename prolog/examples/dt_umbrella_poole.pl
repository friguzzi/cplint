:- use_module(library(pita)).


:- pita. 
 
:- begin_lpad.

? :: umbrella(rainy).
? :: umbrella(sunny).
? :: umbrella(cloudy).

0.7::weather(sunshine);0.3::weather(rain).
0.7::forecast(sunny);0.2::forecast(cloudy);0.1::forecast(rainy):-weather(sunshine).
0.15::forecast(sunny);0.25::forecast(cloudy);0.6::forecast(rainy):-weather(rain).

s1:-weather(sunshine),forecast(X),umbrella(X).
s2:-weather(sunshine),forecast(X),\+umbrella(X).
s3:-weather(rain),forecast(X),umbrella(X).
s4:-weather(rain),forecast(X),\+umbrella(X).

utility(s1,20).
utility(s2,100).
utility(s3,70).
utility(s4,0).


:- end_lpad.

/*
 * ?- dt_solve(Strategy,Value).
 * Expected result:
 * Strategy = [umbrella(rainy)]
 * Value = 77.0
*/