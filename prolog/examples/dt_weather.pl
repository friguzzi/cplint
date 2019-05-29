% From [Van den Broeck et al., 2010].

:- use_module(library(pita)).

:- pita. 

:- begin_lpad.

0.3::weather(rain); 0.7::weather(norain).

0.7::forecast(sunny); 0.2::forecast(cloudy); 0.1::forecast(rainy) :- weather(norain).
0.15::forecast(sunny); 0.25::forecast(cloudy); 0.6::forecast(rainy) :- weather(rain).

umbrella(takeIt) :- forecast(X), decide_u(X).
umbrella(leaveIt) :- forecast(X), \+ decide_u(X).

? :: decide_u(sunny).
? :: decide_u(rainy).
? :: decide_u(cloudy).

line1 :- weather(norain), umbrella(takeIt).
line2 :- weather(norain), umbrella(leaveIt).
line3 :- weather(rain), umbrella(takeIt).
line4 :- weather(rain), umbrella(leaveIt).

utility(line1,20).
utility(line2,100).
utility(line3,70).
utility(line4,0).

:- end_lpad.

/*
 * ?- dt_solve(Strategy,Value).
 * Expected result:
 * Strategy = [decide_u(rainy)]
 * Value = 77.0
*/