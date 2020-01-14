:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.

:- mc.
:- begin_lpad.
saved:- 
  time_comp_1(T),
  {T<0.75}.
saved:-
  time_comp_1(T1),
  time_comp_2(T2),
  {T1 < 1.25},
  {T1 + 0.25 * T2 < 1.375}.
time_comp_1(T) : exponential(T,1).
time_comp_2(T) : exponential(T,1).
:- end_lpad.
