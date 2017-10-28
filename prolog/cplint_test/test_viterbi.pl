:-use_module(library(cplint_test/cplint_test)).

:- begin_tests(vit_win, []).

:-ensure_loaded(library(examples/vit_win)).
test(win):-
  run((viterbi(win,P,Exp),close_to(P,0.36),
Exp=[
  rule(0, red, [red:0.4, '':0.6], []),
  rule(1, green, [green:0.9, '':0.09999999999999998], [])])).

:- end_tests(vit_win).

:- begin_tests(vit_hmm, []).

:-ensure_loaded(library(examples/vit_hmm)).

test(a_g_g):-
  run((viterbi(hmm1(S,[a,g,g]),P,_Exp),close_to(P,0.000405),
  S = [q2, q2, q1]
  )).

test(a_a_a):-
  run((viterbi(hmm1(S,[a,a,a]),P,_Exp),close_to(P,0.0008000000000000003),
  S = [q1, q1, q1]
  )).

:- end_tests(vit_hmm).

:- begin_tests(vit_coin, []).

:-ensure_loaded(library(examples/vit_coin)).
test(h_c):-
  run((viterbi(heads(coin),Prob,Exp),close_to(Prob,0.45),
Exp = [rule(0, heads(coin), [heads(coin):0.5, tails(coin):0.5], [toss(coin), \+biased(coin)]),
	rule(2, fair(coin), [fair(coin):0.9, biased(coin):0.1], [])])).

:- end_tests(vit_coin).

:- begin_tests(vit_mendel, []).

:-ensure_loaded(library(examples/vit_mendel)).
test(s_p):-
  run((viterbi(color(s,purple),Prob,Exp),close_to(Prob,0.5),
	Exp = [rule(0, cg(s, 1, p), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
	 [mother(m, s), cg(m, 1, p), cg(m, 2, w)])])).


test(s_w):-
  run((viterbi(color(s,white),Prob,Exp),close_to(Prob,0.25),
  Exp = [rule(0, cg(s, 1, w), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
     [mother(m, s), cg(m, 1, p), cg(m, 2, w)]),
	 rule(1, cg(s, 2, w), [cg(s, 2, w):0.5, cg(s, 2, p):0.5],
	   [father(f, s), cg(f, 1, w), cg(f, 2, p)])])).
:- end_tests(vit_mendel).
