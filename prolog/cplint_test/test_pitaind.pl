:- module(test_pitaind,
  [test_pitaind/0]).
:- use_module(library(plunit)).

test_pitaind:-
	run_tests([uwcse_indind,
    bloodtype_indexc,
    mendel_indexc,uwcse_indinds]).


:-use_module(library(cplint_test/cplint_test)).

:- begin_tests(uwcse_indind, []).

:-ensure_loaded(library(examples/uwcse_indind)).

test(advisedby_harry_ben):-
  run((prob_ind(advisedby(harry, ben),P),
  close_to(P, 0.87269376))).

:- end_tests(uwcse_indind).

:- begin_tests(uwcse_indinds, []).

:-ensure_loaded(library(examples/uwcse_indinds)).

test(advisedby_harry_ben):-
  run((prob_ind(advisedby(harry, ben),P),
  close_to(P, 0.20433599999999996))).

:- end_tests(uwcse_indinds).

:- begin_tests(bloodtype_indexc, []).

:-ensure_loaded(library(examples/bloodtype_indexc)).

test(pc_p_f_a):-
  run((prob_ind(pchrom(p_f,a),Prob),
  close_to(Prob, 0.318))).

test(p_f_a):-
  run((prob_ind(pchrom(p_f,a),Prob),
  close_to(Prob, 0.3))).

test(p_a):-
  run((prob_ind(bloodtype(p,a),Prob),
  close_to(Prob, 0.3186942939999999))).

test(p_b):-
  run((prob_ind(bloodtype(p,b),Prob),
  close_to(Prob, 0.2239874943000002))).

test(p_aa):-
  run((prob_ind(bloodtype(p,ab),Prob),
  close_to(Prob, 0.19329257700000035))).

test(p_null):-
  run((prob_ind(bloodtype(p,null),Prob),
  close_to(Prob, 0.16751706690000012))).

:- end_tests(bloodtype_indexc).

:- begin_tests(mendel_indexc, []).

:-ensure_loaded(library(examples/mendel_indexc)).

test(s_1_p):-
  run((prob_ind(cg(s,1,p),P),close_to(P,0.5))).
test(s_1_w):-
  run((prob_ind(cg(s,1,w),P),close_to(P,0.5))).
test(s_2_p):-
  run((prob_ind(cg(s,2,p),P),close_to(P,0.5))).
test(s_2_w):-
  run((prob_ind(cg(s,2,w),P),close_to(P,0.5))).

:- end_tests(mendel_indexc).