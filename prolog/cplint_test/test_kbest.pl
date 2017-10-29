:- module(test_kbest,
  [test_kbest/0]).
:- use_module(library(plunit)).

test_kbest:-
  run_tests([
    kbest_win,
    kbest_hmm,
    kbest_coin,
    kbest_mendel
  ]).
:-use_module(library(cplint_test/cplint_test)).

:- begin_tests(kbest_win, []).

:-ensure_loaded(library(examples/kbest_win)).
test(win,[true(Exp=[0.36000000000000004-[rule(0, red, [red:0.4, '':0.6], []),
rule(1, green, [green:0.9, '':0.09999999999999998], [])]])]):-
  run((kbest(win,1,Exp),true)).

test(winP,[true(Exp=[0.36000000000000004-[rule(0, red, [red:0.4, '':0.6], []),
rule(1, green, [green:0.9, '':0.09999999999999998], [])]])]):-
  run((kbest(win,1,P,Exp),close_to(P,0.36))).

test(win2,[true(Exp=[0.36000000000000004-[rule(0, red, [red:0.4, '':0.6], []),
   rule(1, green, [green:0.9, '':0.09999999999999998], [])],
 0.30000000000000004-[rule(2, blue, [blue:0.5, '':0.5], []),
 rule(3, yellow, [yellow:0.6, '':0.4], [])]])]):-
  kbest(win,2,Exp).

test(win2P,[true(Exp=[0.36000000000000004-[rule(0, red, [red:0.4, '':0.6], []),
   rule(1, green, [green:0.9, '':0.09999999999999998], [])],
 0.30000000000000004-[rule(2, blue, [blue:0.5, '':0.5], []),
 rule(3, yellow, [yellow:0.6, '':0.4], [])]])]):-
  run((kbest(win,2,P,Exp),close_to(P,0.552))).

:- end_tests(kbest_win).

:- begin_tests(kbest_hmm, []).

:-ensure_loaded(library(examples/kbest_hmm)).
test(a_g_g):-
  run((kbest(hmm([a,g,g]),1,P,_Exp),close_to(P,0.000405)
  )).

test(a_a_a):-
  run((kbest(hmm([a,a,a]),1,P,_Exp),close_to(P,0.0008000000000000003)
  )).

:- end_tests(kbest_hmm).

:- begin_tests(kbest_coin, []).

:-ensure_loaded(library(examples/kbest_coin)).
test(h_c,[true( Exp = [0.45000000000000007-[rule(0, heads(coin), [heads(coin):0.5, tails(coin):0.5], [toss(coin), \+biased(coin)]),
 	rule(2, fair(coin), [fair(coin):0.9, biased(coin):0.1], [])]])]):-
  run((kbest(heads(coin),1,Prob,Exp),close_to(Prob,0.45))).

:- end_tests(kbest_coin).

:- begin_tests(kbest_mendel, []).

:-ensure_loaded(library(examples/kbest_mendel)).
test(s_p,[true(Exp = [0.5-[rule(0, cg(s, 1, p), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
 [mother(m, s), cg(m, 1, p), cg(m, 2, w)])]])]):-
  run((kbest(color(s,purple),1,Prob,Exp),close_to(Prob,0.5))).

test(s_w,[true(Exp = [0.25-[rule(0, cg(s, 1, w), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
   [mother(m, s), cg(m, 1, p), cg(m, 2, w)]),
 rule(1, cg(s, 2, w), [cg(s, 2, w):0.5, cg(s, 2, p):0.5],
   [father(f, s), cg(f, 1, w), cg(f, 2, p)])]])]):-
  run((kbest(color(s,white),1,Prob,Exp),close_to(Prob,0.25))).

test(s_p_P,[true(Exp = [0.5-[rule(1, cg(s, 2, p), [cg(s, 2, w):0.5, cg(s, 2, p):0.5],
[father(f, s), cg(f, 1, w), cg(f, 2, p)])],
0.5-[rule(0, cg(s, 1, p), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
[mother(m, s), cg(m, 1, p), cg(m, 2, w)])]])]):-
  run((kbest(color(s,purple),2,Prob,Exp),close_to(Prob,0.75))).

test(s_w_P,[true(Exp = [0.25-[rule(0, cg(s, 1, w), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
   [mother(m, s), cg(m, 1, p), cg(m, 2, w)]),
 rule(1, cg(s, 2, w), [cg(s, 2, w):0.5, cg(s, 2, p):0.5],
   [father(f, s), cg(f, 1, w), cg(f, 2, p)])]])]):-
  run((kbest(color(s,white),2,Prob,Exp),close_to(Prob,0.25))).

:- end_tests(kbest_mendel).
