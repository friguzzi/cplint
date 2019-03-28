:- use_module(library(pita)).

:- pita.

:- begin_lpad.

0.3::rain.
0.5::wind.

% decision facts
? :: umbrella.
? :: raincoat.

broken_umbrella :- umbrella,rain,wind.
dry :- rain, raincoat.
dry :- rain, umbrella, \+(broken_umbrella).
dry :- \+(rain).

% utility facts
broken_umbrella => -40.
raincoat => -20.
umbrella => -2.
dry => 60.

/*
 * ?- dt_solve(Strategy,Value).
 * Expected result:
 * Strategy = [umbrella]
 * Value = 43.0
*/

:- end_lpad.
