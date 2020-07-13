:- set_test_options([load(always)]).
:- use_module(test_pita).
:- use_module(test_mc).
:- use_module(test_kbest).
:- use_module(test_viterbi).
:- use_module(test_sc).
:- use_module(test_lemur).
:- use_module(test_cplint_util).

:- format(user_error,
	  'cplint test suite.  To run all tests run ?- test.~n~n', []).

% :- set_prolog_flag(trace_gc, true).

test:-
  collect_failed([ test_pita,
                   test_mc,
                   test_kbest,
                   test_viterbi,
                   test_sc,
                   test_lemur,
                   test_util
                 ], Failed),
  (   Failed == []
  ->  format(user_error, 'All test suites succeeded~n', [])
  ;   format(user_error, 'These test suites failed: ~p~n', [Failed]),
      fail
  ).

collect_failed([], []).
collect_failed([H|T], Failed) :-
  (   call(H)
  ->  collect_failed(T, Failed)
  ;   Failed = [H|Failed1],
      collect_failed(T, Failed1)
  ).
