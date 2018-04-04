:- module(test_cplint_util,
  [test_util/0]).
:- use_module(library(plunit)).


:-use_module(library(cplint_test/cplint_test)).

test_util:-
	run_tests([
    beta
  ]).



:- begin_tests(beta, []).

:-ensure_loaded(library(cplint_util)).

test(beta_1_1):-
  run((beta([1,1],B),
  close_to(B,1.0))).


test(beta_2_2):-
  run((beta([2,2],B),
  close_to(B,0.16666666666666663))).

test(beta_1_2):-
  run((beta([1,2],B),
  close_to(B,0.49999999999999994))).


test(beta_05_05):-
  run((beta([0.5,0.5],B),
  close_to(B,3.1415926535897927))).

test(beta_03_03):-
  run((beta([0.3,0.3],B),
  close_to(B,6.009623683731014))).

:- end_tests(beta).

