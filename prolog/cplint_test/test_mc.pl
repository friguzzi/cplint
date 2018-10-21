:- module(test_mc,
  [test_mc/0,test_mc_rev/0]).
:- use_module(library(plunit)).

:-set_random(seed(100)).
:-use_module(library(cplint_test/cplint_test)).

test_list([
    coinmc,
    threesideddicemc,
    markov_chain,
    prefix,
    pre_plcg,
    pctl_slep,
    arithm,
    gaussian_mixture,
    kalman_filter,
    gauss_mean_est,
    slp_pdcg,
    indian_gpa,
    indian_gpadc,
    nballs,
    nballsdc,
    simpsonmc,
    viralmc,
    uwcsemc,
    lda,
    bitcoin_attack
  ]).

test_mc:-
  test_list(L),
	run_tests(L).

test_mc_rev:-
  test_list(L),
	reverse(L,Rev),
  run_tests(Rev).


:- begin_tests(coinmc, []).

:-ensure_loaded(library(examples/coinmc)).

test(heads):-
  run((
	mc_sample(heads(coin),1000,P),
  close_to(P,0.51))).

test(tails):-
	run((
	mc_sample(tails(coin),1000,P),
	close_to(P,0.49)
	)).

test(tails_o):-
	run((
	mc_sample(tails(coin),1000,P,[successes(S),failures(F),bar(B)]),
	close_to(P,0.49), close_to(S,490,50), close_to(F,510,50), is_dict(B,c3)
	)).

:- end_tests(coinmc).

:- begin_tests(threesideddicemc, []).

:-ensure_loaded(library(examples/threesideddicemc)).

test(on_0_1):-
	run((mc_sample(on(0,1),1000,P),close_to(P,0.333333333333333))).
test(on_1_1):-
  run((mc_sample(on(1,1),1000,P),close_to(P,0.222222222222222))).
test(on_2_1):-
	run((mc_sample(on(2,1),1000,P),close_to(P,0.148148147703704))).
test(on_2_1__on_1_1_mh):-
  run((mc_mh_sample(on(2,1),on(1,1),1000,P,[mix(1000)]),close_to(P,0.333333333333333))).
test(on_2_1__on_0_1_mh):-
  run((mc_mh_sample(on(2,1),on(0,1),1000,P,[mix(1000)]),close_to(P,0.222222222222222))).


test(on_0_1_g):-
	run((mc_gibbs_sample(on(0,1),1000,P),close_to(P,0.333333333333333))).
test(on_1_1_g):-
  run((mc_gibbs_sample(on(1,1),1000,P),close_to(P,0.222222222222222))).
test(on_2_1_g):-
	run((mc_gibbs_sample(on(2,1),1000,P),close_to(P,0.148148147703704))).

test(on_2_1_g_m):-
	run((mc_gibbs_sample(on(2,1),1000,P,[mix(100)]),close_to(P,0.148148147703704))).

test(on_2_1_g_m_b):-
	run((mc_gibbs_sample(on(2,1),1000,P,[mix(100),block(2)]),close_to(P,0.148148147703704))).

test(on_2_1__on_1_1_g):-
  run((mc_gibbs_sample(on(2,1),on(1,1),1000,P,[mix(1000)]),close_to(P,0.333333333333333))).
test(on_2_1__on_0_1_g):-
  run((mc_gibbs_sample(on(2,1),on(0,1),1000,P,[mix(1000)]),close_to(P,0.222222222222222))).

test(on_2_1__on_0_1_g_b):-
  run((mc_gibbs_sample(on(2,1),on(0,1),1000,P,[mix(1000),block(2)]),close_to(P,0.222222222222222))).

:- end_tests(threesideddicemc).

:- begin_tests(markov_chain, []).

:-ensure_loaded(library(examples/markov_chain)).

test(reach_s0_0_s0):-
  run((mc_sample(reach(s0,0,s0),1000,P),close_to(P,1))).
test(reach_s0_0_s1):-
  run((mc_sample(reach(s0,0,s1),1000,P),close_to(P,0.5984054054054054))).
test(reach_s0_0_s2):-
	run((mc_sample(reach(s0,0,s2),1000,P),close_to(P,0.4025135135135135))).
test(reach_s0_0_s3):-
	run((mc_sample(reach(s0,0,s3),1000,P),close_to(P,0.5998378378378378))).
test(reach_s0_0_s4):-
	run((mc_sample(reach(s0,0,s4),1000,P),close_to(P,0.49948717948717947))).
test(reach_s1_0_s0):-
	run((mc_sample(reach(s1,0,s0),1000,P),close_to(P,0))).
test(reach_s0_0_S_s0):-
	run((mc_sample_arg(reach(s0,0,S),50,S,Values),\+ member([s0]-_,Values))).
test(reach_s0_0_S_s0):-
	run((mc_mh_sample_arg(reach(s0,0,S),reach(s0,0,s1),50,S,Values),\+ member([s0]-_,Values))).
test(reach_s0_0_S_s0_g):-
	run((mc_gibbs_sample_arg(reach(s0,0,S),50,S,Values),\+ member([s0]-_,Values))).
test(reach_s0_0_S_s0):-
	run((mc_gibbs_sample_arg(reach(s0,0,S),reach(s0,0,s1),50,S,Values,[mix(100)]),\+ member([s0]-_,Values))).

test(reach_s0_0_S_s0_b):-
	run((mc_gibbs_sample_arg(reach(s0,0,S),reach(s0,0,s1),50,S,Values,[mix(100),block(2)]),\+ member([s0]-_,Values))).

test(reach_s0_0_s0_s3_s2,[nondet]):-
	run((mc_sample_arg_first(reach(s0,0,S),50,S,Values),member(s3-_,Values),member(s2-_,Values))).
test(reach_s0_0_S_s0_o):-
	run((mc_sample_arg(reach(s0,0,S),50,S,Values,[bar(B)]),\+ member([s0]-_,Values),is_dict(B,c3))).
test(reach_s0_0_s0_s3_s2_o,[nondet]):-
	run((mc_sample_arg_first(reach(s0,0,S),50,S,Values,[bar(B)]),member(s3-_,Values),member(s2-_,Values),is_dict(B,c3))).
:- end_tests(markov_chain).

:- begin_tests(prefix, []).

:-ensure_loaded(library(examples/prefix)).

test(pre_cfg_a):-
	run((mc_sample(pre_pcfg([a]),1000,P),close_to(P,0.5))).
test(pre_cfg_a_b):-
	run((mc_sample(pre_pcfg([a,b]),1000,P),close_to(P,0.09692857142857143))).
test(pre_cfg_b):-
	run((mc_sample(pre_pcfg([b]),1000,P),close_to(P,0.5))).
test(pre_cfg_a_b_a):-
	run((mc_sample(pre_pcfg([a,b,a]),1000,P),close_to(P,0.03))).
test(pre_cfg_b_a):-
	run((mc_sample(pre_pcfg([b,a]),1000,P),close_to(P,0.1014))).

:- end_tests(prefix).

:- begin_tests(pre_plcg, []).

:-ensure_loaded(library(examples/pre_plcg)).
test(pre_plc_a_b):-
	run((mc_sample(pre_plc([a,b]),1000,P),close_to(P,0.0326))).
:- end_tests(pre_plcg).

:- begin_tests(pctl_slep, []).

:-ensure_loaded(library(examples/pctl_slep)).
test(eventually_elect):-
	run((mc_sample(eventually(elect),100,P),close_to(P,1))).
test(bounded_eventually_elect):-
	run((mc_sample(bounded_eventually(elect,3),100,P),close_to(P,0.97))).
test(exp_eventually_elect):-
	run((mc_expectation(eventually(elect,T),100,T,P),relatively_close_to(P,1.2))).
:- end_tests(pctl_slep).

:- begin_tests(arithm, []).

:-ensure_loaded(library(examples/arithm)).

test(eval_1_3):-
	run((mc_mh_sample(eval(2,4),eval(1,3),500,P),close_to(P,0.1151,0.4))).
test(eval_1_3_o):-
	run((mc_mh_sample(eval(2,4),eval(1,3),500,P,[mix(10),lag(2),successes(S),failures(F)]),
  close_to(P,0.1151,0.4),close_to(S,51,100),close_to(F,449,100))).
test(eval_0_2_1_3):-
  run((mc_mh_sample(eval(2,4),(eval(0,2),eval(1,3)),200,P),close_to(P,1))).
%test((mc_rejection_sample(eval(2,4),eval(1,3),1000,P),close_to(P,0.1151)),arithm).
%test((mc_rejection_sample(eval(2,4),(eval(0,2),eval(1,3)),1000,P),close_to(P,1)),arithm).
test(exp_eval_2):-
  run((mc_expectation(eval(2,Y),100,Y,E),relatively_close_to(E,3.968,1))).
test(exp_eval_2_eval_1_3):-
  run((mc_mh_expectation(eval(2,Y),eval(1,3),300,Y,E),relatively_close_to(E,2.855,1))).
test(exp_eval_2_eval_1_3_o):-
  run((mc_mh_expectation(eval(2,Y),eval(1,3),300,Y,E,[mix(10),lag(2)]),relatively_close_to(E,2.855,1))).

test(eval_1_3_g):-
	run((mc_gibbs_sample(eval(2,4),eval(1,3),500,P,[]),close_to(P,0.604,0.4))).
test(eval_1_3_g_n):-
	run((mc_gibbs_sample(eval(2,4),eval(1,3),500,P,[block(2)]),close_to(P,0.2,0.4))).
test(eval_1_3_o_g):-
	run((mc_gibbs_sample(eval(2,4),eval(1,3),500,P,[mix(10),successes(S),failures(F)]),
  close_to(P,0.1151,0.4),close_to(S,51,150),close_to(F,449,150))).
test(eval_0_2_1_3_g):-
  run((mc_gibbs_sample(eval(2,4),(eval(0,2),eval(1,3)),200,P,[]),close_to(P,1))).

test(exp_eval_2_g):-
  run((mc_gibbs_expectation(eval(2,Y),300,Y,E),relatively_close_to(E,3.968,1))).
test(exp_eval_2_g_m):-
  run((mc_gibbs_expectation(eval(2,Y),500,Y,E,[mix(100)]),relatively_close_to(E,3.968,1))).
test(exp_eval_2_eval_1_3_g):-
  run((mc_gibbs_expectation(eval(2,Y),eval(1,3),300,Y,E,[]),relatively_close_to(E,2.855,1))).
test(exp_eval_2_eval_1_3_o_g):-
  run((mc_gibbs_expectation(eval(2,Y),eval(1,3),300,Y,E,[mix(10)]),relatively_close_to(E,2.855,1))).

test(exp_eval_2_eval_1_3_o_g_b):-
  run((mc_gibbs_expectation(eval(2,Y),eval(1,3),300,Y,E,[mix(10),block(3)]),relatively_close_to(E,2.855,1))).

:- end_tests(arithm).

:- begin_tests(gaussian_mixture, []).

:-ensure_loaded(library(examples/gaussian_mixture)).

test(mix_X):-
	run((mc_expectation(mix(X),1000,X,E),relatively_close_to(E,1.9911150139041882,0.2))).
test(mix_X_heads):-
	run((mc_mh_expectation(mix(X),heads,1000,X,E),close_to(E,0,1))).
test(mix_X_mix):-
	run((mc_mh_expectation(mix(X),(mix(Y),Y>2),1000,X,E),relatively_close_to(E,5.00202846171105,0.25))).
:- end_tests(gaussian_mixture).

:- begin_tests(kalman_filter, []).

:-ensure_loaded(library(examples/kalman_filter)).

test(exp_kf):-
	run((mc_expectation(kf(1,_O2,[T]),1000,T,E),close_to(E,0,0.1))).
test(lw_exp_kf):-
	run((mc_lw_expectation(kf(1,_O2,T),kf(1,[2.5],_T),3000,T,[E]),relatively_close_to(E,0.6324846033555553,0.2))).

:- end_tests(kalman_filter).


:- begin_tests(gauss_mean_est, []).

:-ensure_loaded(library(examples/gauss_mean_est)).
test(lw_exp_value_0_X):-
  run((mc_lw_expectation(val(0,X),(val(1,9),val(2,8)),2000,X,E),relatively_close_to(E,7.166960047178755,0.25))).
test(exp_value_0_X):-
	run((mc_expectation(val(0,X),2000,X,E),relatively_close_to(E,0.9698875384639362,0.25))).
test(part_exp_value_0_X):-
	run((mc_particle_expectation(val(0,X),[val(1,9),val(2,8)],2000,X,E),relatively_close_to(E,7.166960047178755,0.25))).
:- end_tests(gauss_mean_est).

:- begin_tests(slp_pdcg, []).

:-ensure_loaded(library(examples/slp_pdcg)).

test(is_word):-
	run((mc_sample(is_word,1000,P),close_to(P,0.067222))).
:- end_tests(slp_pdcg).

:- begin_tests(indian_gpa, []).

:-ensure_loaded(library(examples/indian_gpa)).
test(nation_a_st):-
	run((mc_lw_sample(nation(a),student_gpa(4.0),1000,P),close_to(P,1.0))).
test(nation_a):-
  run((mc_sample(nation(a),1000,P),close_to(P,0.25))).
:- end_tests(indian_gpa).

:- begin_tests(indian_gpadc, []).

:-ensure_loaded(library(examples/indian_gpadc)).
test(nation_a_st):-
  run((mc_lw_sample(nation(a),student_gpa(4.0),1000,P),close_to(P,1.0))).
test(nation_a):-
  run((mc_sample(nation(a),1000,P),close_to(P,0.25))).
:- end_tests(indian_gpadc).

:- begin_tests(nballs, []).

:-ensure_loaded(library(examples/nballs)).
test(drawn_1_1):-
  run((mc_sample(drawn(1,1),1000,P),close_to(P,0.285))).
test(drawn_1_1_wood):-
	run((mc_sample((drawn(1,1),material(1,wood)),1000,P),close_to(P,0.086))).
test(drawn_1_1_wood_black):-
	run((mc_sample((drawn(1,1),material(1,wood),color(1,black)),1000,P),close_to(P,0.044))).
:- end_tests(nballs).

:- begin_tests(nballsdc, []).

:-ensure_loaded(library(examples/nballsdc)).
test(drawn_1_1):-
  run((mc_sample(drawn(1,1),1000,P),close_to(P,0.285))).
test(drawn_1_1_wood):-
  run((mc_sample((drawn(1,1),material(1,wood)),1000,P),close_to(P,0.086))).
test(drawn_1_1_wood_black):-
  run((mc_sample((drawn(1,1),material(1,wood),color(1,black)),1000,P),close_to(P,0.044))).
:- end_tests(nballsdc).

:- begin_tests(simpsonmc, []).

:-ensure_loaded(library(examples/simpsonmc)).
test(rec_drug):-
	run((mc_rejection_sample(recovery,drug,500,P),close_to(P,0.5))).
test(rec_drug_o):-
	run((mc_rejection_sample(recovery,drug,500,P,[successes(S),failures(F)]),
  close_to(P,0.5),close_to(S,250,50),close_to(F,250,50))).
test(rec_n_drug):-
  run((mc_rejection_sample(recovery,\+ drug,500,P),close_to(P,0.4))).
test(rec_drug_f):-
  run((mc_rejection_sample(recovery,(drug,female),500,P),close_to(P,0.2))).
test(rec_n_drug_f):-
  run((mc_rejection_sample(recovery,(\+drug,female),500,P),close_to(P,0.3))).
test(rec_drug_m):-
  run((mc_rejection_sample(recovery,(drug,\+female),500,P),close_to(P,0.6))).
test(rec_n_drug_m):-
  run((mc_rejection_sample(recovery,(\+ drug,\+female),500,P),close_to(P,0.7))).
test(rec_d_drug):-
  run((mc_rejection_sample(recovery,do(drug),500,P),close_to(P,0.4))).
test(rec_d_n_drug):-
  run((mc_rejection_sample(recovery,do(\+ drug),500,P),close_to(P,0.5))).
test(rec_d_drug_f):-
  run((mc_rejection_sample(recovery,(do(drug),female),500,P),close_to(P,0.2))).
test(rec_d_n_drug_f):-
  run((mc_rejection_sample(recovery,(do(\+drug),female),500,P),close_to(P,0.3))).
test(rec_d_drug_m):-
  run((mc_rejection_sample(recovery,(do(drug),\+ female),500,P),close_to(P,0.6))).
test(rec_d_n_drug_m):-
  run((mc_rejection_sample(recovery,(do(\+ drug),\+ female),500,P),close_to(P,0.7))).

test(mh_rec_drug):-
  run((mc_mh_sample(recovery,drug,500,P,[lag(2)]),close_to(P,0.5,0.15))).
test(mh_rec_n_drug):-
  run((mc_mh_sample(recovery,\+ drug,500,P,[lag(2)]),close_to(P,0.4))).
test(mh_rec_drug_f):-
  run((mc_mh_sample(recovery,(drug,female),500,P,[lag(2)]),close_to(P,0.2))).
test(mh_rec_n_drug_f):-
  run((mc_mh_sample(recovery,(\+drug,female),500,P,[lag(2)]),close_to(P,0.3))).
test(mh_rec_drug_m):-
  run((mc_mh_sample(recovery,(drug,\+female),500,P,[lag(2)]),close_to(P,0.6))).
test(mh_rec_n_drug_m):-
  run((mc_mh_sample(recovery,(\+ drug,\+female),500,P,[lag(2)]),close_to(P,0.7))).
test(mh_rec_d_drug):-
  run((mc_mh_sample(recovery,do(drug),500,P,[lag(2)]),close_to(P,0.4))).
test(mh_rec_d_n_drug):-
  run((mc_mh_sample(recovery,do(\+ drug),500,P,[lag(2)]),close_to(P,0.5))).
test(mh_rec_d_drug_f):-
  run((mc_mh_sample(recovery,(do(drug),female),500,P,[lag(2)]),close_to(P,0.2))).
test(mh_rec_d_n_drug_f):-
  run((mc_mh_sample(recovery,(do(\+drug),female),500,P,[lag(2)]),close_to(P,0.3))).
test(mh_rec_d_drug_m):-
  run((mc_mh_sample(recovery,(do(drug),\+ female),500,P,[lag(2)]),close_to(P,0.6))).
test(mh_rec_d_n_drug_m):-
  run((mc_mh_sample(recovery,(do(\+ drug),\+ female),500,P,[lag(2)]),close_to(P,0.7))).
:- end_tests(simpsonmc).

:- begin_tests(viralmc, []).

:-ensure_loaded(library(examples/viralmc)).
test(has_2_has_3):-
  run((mc_rejection_sample(has(2),has(3),500,P),close_to(P,0.4065135474609725,0.1))).
test(has_2_d_has_3):-
  run((mc_rejection_sample(has(2),do(has(3)),500,P),close_to(P,0.136))).
test(mh_has_2_d_has_3):-
  run((mc_mh_sample(has(2),do(has(3)),500,P),close_to(P,0.136))).
:- end_tests(viralmc).

:- begin_tests(uwcsemc, []).

:-ensure_loaded(library(examples/uwcsemc)).
test(taught_by_c1_p1):-
  run((mc_sample(taught_by(c1,p1),1000,P),close_to(P,0.0926040439925477))).
:- end_tests(uwcsemc).

:- begin_tests(lda, []).

:-ensure_loaded(library(examples/lda)).

test(topic_1_1_1):-
  run((mc_sample(topic(1,1,1),400,P),close_to(P,0.5,0.2))).
test(topic_1_1_1_g):-
  run((mc_gibbs_sample(topic(1,1,1),400,P),close_to(P,0.5,0.2))).

test(topic_1_1_1_ww):-
  run((mc_mh_sample(topic(1,1,1),(word(1,1,1),word(1,2,1)),100,G,[]),close_to(G,0.5,0.5))).
test(topic_1_1_1_ww_g):-
  run((mc_gibbs_sample(topic(1,1,1),(word(1,1,1),word(1,2,1)),100,G,[]),close_to(G,0.5,0.5))).
test(topic_1_1_1_ww_g_b):-
  run((mc_gibbs_sample(topic(1,1,1),(word(1,1,1),word(1,2,1)),100,G,[block(2)]),close_to(G,0.5,0.5))).

:- end_tests(lda).


:- begin_tests(bitcoin_attack, []).

:-ensure_loaded(library(examples/bitcoin_attack)).

test(poisson):-
  run((mc_sample(success_poisson,1000,P),close_to(P,0.042))).

test(pascal):-
  run((mc_sample(success_pascal,1000,P),close_to(P,0.036))).

:- end_tests(bitcoin_attack).

:- begin_tests(bitcoin_two_phase, []).

:-ensure_loaded(library(examples/bitcoin_two_phase)).
test(a):-
  run((mc_sample(reach(a0,0,a2),1000,P),close_to(P,0.071))).

test(b):-
  run((mc_sample(reach(b0,0,b2),1000,P),close_to(P,0.115))).

:- end_tests(bitcoin_two_phase).

:- begin_tests(binomial, []).

:-ensure_loaded(library(examples/binomial)).

test(exp):-
  run((mc_expectation(a(X),1000,X,E),close_to(E,10))).


:- end_tests(binomial).

