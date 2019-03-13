:- use_module(library(pita)).

:- pita.

:- begin_lpad.

0.3::rain.
0.5::wind.

? :: umbrella.
? :: raincoat.

broken_umbrella :- umbrella, rain, wind.
dry :- rain, raincoat.
dry :- rain, umbrella, \+(broken_umbrella).
dry :- \+(rain).

% utility(broken_umbrella, -40).
% utility(raincoat, -20).
% utility(umbrella, -2).
% utility(dry, 60).

broken_umbrella => -40.
raincoat => -20.
umbrella => -2.
dry => 60.

:- end_lpad.
