:-use_module(library(cplint_test/cplint_test)).

:- begin_tests(coin, []).

:-ensure_loaded(library(examples/coin)).

test(h_c):-
  run((prob(heads(coin),P),close_to(P,0.51))).
test(h_c_b_c):-
  run((prob((heads(coin),biased(coin)),P),close_to(P,0.06))).
test(t_c):-
  run((prob(tails(coin),P),close_to(P,0.49))).
test(h_c_g_b_c):-
  run((prob(heads(coin),biased(coin),P),close_to(P,0.6))).
test(h_c_n_b_c):-
  run((prob((heads(coin),\+ biased(coin)),P),close_to(P,0.45))).
test(n_h_c):-
  run((prob(\+ heads(coin),P),close_to(P,0.49))).

:- end_tests(coin).


:- begin_tests(coinmsw, []).

:-ensure_loaded(library(examples/coinmsw)).

test(r_c_h):-
  run((prob(res(coin,heads),P),close_to(P,0.51))).
test(r_c_t):-
  run((prob(res(coin,tails),P),close_to(P,0.49))).

:- end_tests(coinmsw).

:- begin_tests(dice, []).

:-ensure_loaded(library(examples/dice)).

test(on_0_1):-
  run((prob(on(0,1),P),close_to(P,0.16666666666666666))).
test(on_1_1):-
  run((prob(on(1,1),P),close_to(P,0.13888888888888887))).
test(on_2_1):-
  run((prob(on(2,1),P),close_to(P,0.11574074074074071))).
test(on_2_1_0_1):-
  run((prob(on(2,1),on(0,1),P),close_to(P,0.13888888888888887))).
test(on_2_1_ev):-
  run((prob(on(2,1),evidence,P),close_to(P,0.16666666666666666))).

:- end_tests(dice).

:- begin_tests(epidemic, []).

:-ensure_loaded(library(examples/epidemic)).

test(ep):-
  run((prob(epidemic,P),close_to(P,0.588))).
test(pan):-
  run((prob(pandemic,P),close_to(P,0.357))).

:- end_tests(epidemic).

:- begin_tests(earthquake, []).

:-ensure_loaded(library(examples/earthquake)).

test(s_s):-
  run((prob(earthquake(stromboli,strong),P),close_to(P,0.43999999999999995))).
test(s_m):-
  run((prob(earthquake(stromboli,moderate),P),close_to(P,0.7999999999999998))).
test(e_s):-
  run((prob(earthquake(eyjafjallajkull,strong),P),close_to(P,0.2))).
test(e_m):-
  run((prob(earthquake(eyjafjallajkull,moderate),P),close_to(P,0.6))).
:- end_tests(earthquake).


:- begin_tests(sneezing, []).

:-ensure_loaded(library(examples/sneezing)).

test(s_s):-
  run((prob(strong_sneezing(bob),P),close_to(P,0.43999999999999995))).
test(m_s):-
  run((prob(moderate_sneezing(bob),P),close_to(P,0.7999999999999998))).

:- end_tests(sneezing).

:- begin_tests(trigger, []).

:-ensure_loaded(library(examples/trigger)).

test(death):-
  run((prob(death,P),close_to(P,0.305555555555556))).

:- end_tests(trigger).


:- begin_tests(light, []).

:-ensure_loaded(library(examples/light)).

test(light):-
  run((prob(light,P),close_to(P,0.4))).
test(replace):-
  run((prob(replace,P),close_to(P,0.6))).

:- end_tests(light).

:- begin_tests(threesideddice, []).

:-ensure_loaded(library(examples/threesideddice)).

test(on_0_1):-
  run((prob(on(0,1),P),close_to(P,0.333333333333333))).
test(on_1_1):-
 run((prob(on(1,1),P),close_to(P,0.222222222222222))).
test(on_2_1):-
  run((prob(on(2,1),P),close_to(P,0.148148147703704))).

test(on_2_1_0_1):-
 run((prob(on(2,1),on(0,1),P),close_to(P,0.222222222222222))).
test(on_2_1_1_1):-
 run((prob(on(2,1),on(1,1),P),close_to(P,0.333333333333333))).

:- end_tests(threesideddice).

:- begin_tests(mendel, []).

:-ensure_loaded(library(examples/mendel)).


test(s_1_p):-
  run((prob(cg(s,1,p),P),close_to(P,0.5))).
test(s_1_w):-
  run((prob(cg(s,1,w),P),close_to(P,0.5))).
test(s_2_p):-
  run((prob(cg(s,2,p),P),close_to(P,0.5))).
test(s_2_w):-
  run((prob(cg(s,2,w),P),close_to(P,0.5))).

:- end_tests(mendel).

:- begin_tests(coin2, []).

:-ensure_loaded(library(examples/coin2)).

test(h_coin1):-
  run((prob(heads(coin1),P),close_to(P,0.51))).
test(h_coin2):-
  run((prob(heads(coin2),P),close_to(P,0.51))).

test(t_coin1):-
  run((prob(tails(coin1),P),close_to(P,0.49))).
test(t_coin2):-
  run((prob(tails(coin2),P),close_to(P,0.49))).

:- end_tests(coin2).

:- begin_tests(simpson, []).

:-ensure_loaded(library(examples/simpson)).

test(r_d):-
  run((prob(recovery,drug,P),close_to(P,0.5))).
test(r_n_d):-
  run((prob(recovery,\+ drug,P),close_to(P,0.4))).
test(d_f):-
  run((prob(recovery,(drug,female),P),close_to(P,0.2))).
test(n_d_f):-
  run((prob(recovery,(\+drug,female),P),close_to(P,0.3))).
test(d_n_m):-
  run((prob(recovery,(drug,\+female),P),close_to(P,0.6))).
test(n_d_m):-
  run((prob(recovery,(\+ drug,\+female),P),close_to(P,0.7))).
test(d_d):-
  run((prob(recovery,do(drug),P),close_to(P,0.4))).
test(d_n_d):-
  run((prob(recovery,do(\+ drug),P),close_to(P,0.5))).
test(d_d_f):-
  run((prob(recovery,(do(drug),female),P),close_to(P,0.2))).
test(d_n_d_f):-
  run((prob(recovery,(do(\+drug),female),P),close_to(P,0.3))).
test(d_d_m):-
  run((prob(recovery,(do(drug),\+ female),P),close_to(P,0.6))).
test(d_n_d_m):-
  run((prob(recovery,(do(\+ drug),\+ female),P),close_to(P,0.7))).

:- end_tests(simpson).


:- begin_tests(viral, []).

:-ensure_loaded(library(examples/viral)).

test(h_2_3):-
  run((prob(has(2),has(3),P),close_to(P,0.4065135474609725))).
test(h_2_d_3):-
  run((prob(has(2),do(has(3)),P),close_to(P,0.136))).

:- end_tests(viral).

:- begin_tests(uwcse, []).

:-ensure_loaded(library(examples/uwcse)).

test(t_c1_p1):-
  run((prob(taught_by(c1,p1),P),close_to(P,0.0926040439925477))).

:- end_tests(uwcse).

:- begin_tests(path, []).

:-ensure_loaded(library(examples/path)).

test(p_a_e):-
  run((prob(path(a,e),P),close_to(P,0.22888))).

:- end_tests(path).

:- begin_tests(pathdb, []).

:-ensure_loaded(library(examples/pathdb)).

test(p_a_e):-
  run((prob(path(a,e),P),close_to(P,0.22888))).

:- end_tests(pathdb).

:- begin_tests(multiple_paths_simple, []).

:-ensure_loaded(library(examples/multiple_paths_simple)).

test(p):-
  run((prob(p,P),close_to(P,0.54))).

:- end_tests(multiple_paths_simple).

:- begin_tests(multiple_paths, []).

:-ensure_loaded(library(examples/multiple_paths)).

test(p):-
  run((prob(p,P),close_to(P,0.0636))).

:- end_tests(multiple_paths).
