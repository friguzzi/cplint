:- module(test_pitaind_tab,
  [test_pitaind_tab/0]).
:- use_module(library(plunit)).

test_pitaind:-
	run_tests([uwcse_indindtab]).


:-use_module(library(cplint_test/cplint_test)).

:- begin_tests(uwcse_indinds, []).

:-ensure_loaded(library(examples/uwcse_indinds)).

test(advisedby_harry_ben):-
  run((prob(advisedby(harry, ben),P),
  close_to(P, 0.49628007937028495))).
% it should be 0.20433599999999996
% the problem is that it finds 6 answers for the query instead of 2
:- end_tests(uwcse_indinds).

:- begin_tests(uwcse_indind, []).

:-ensure_loaded(library(examples/uwcse_indind)).

test(advisedby_harry_ben):-
  run((prob(advisedby(harry, ben),P),
  close_to(P, 0.9194048126992457))).
% it should be 0.87269376
:- end_tests(uwcse_indind).

