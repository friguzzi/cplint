:- module(test_pita,
  [test_pita/0]).
:- use_module(library(plunit)).

test_pita:-
	run_tests([coin,
    coinmsw,
    coinmsw_memo,
    coin_mpe,
    dice,
    epidemic,
    earthquake,
    sneezing,
    trigger,
    light,
    threesideddice,
    bloodtype,
    mendel,
    coin2,
    simpson,
    viral,
    uwcse,
    path,
    pathdb,
    multiple_paths_simple,
    multiple_paths,
    abd1,
    abd2,
    abd3,
    abd1cons1,
    abd1cons2,
    bag_game_mpe,
    bag_pb_mpe,
    bag_simple,
    bag_mpe,
    eruption_mpe,
    bag_1,
    bag_2,
    hmm_mpe,
    meta,
    pcfg,
    pcfglrdb,
    var_objdb,
    card,
    dt_umbrella,
    dt_winning,
    dt_weather,
    dt_viral,
    tabling_probs,
    event_calculus
    ]).

:-use_module(library(cplint_test/cplint_test)).


:- begin_tests(event_calculus, []).
:- ensure_loaded(library(examples/event_calculus)).
test(garden2):-
  run((prob(holdsAt(locatedIn(bob,garden),2),P),close_to(P,1))).
test(garden4):-
  run((prob(holdsAt(locatedIn(bob,garden),4),P),close_to(P,0.34))).
test(kitchen4):-
  run((prob(holdsAt(locatedIn(bob,kitchen),4),P),close_to(P,0.66))).
test(garage6):-
  run((prob(holdsAt(locatedIn(bob,garage),6),P),close_to(P,0.66))).
:- end_tests(event_calculus).


:- begin_tests(dt_winning, []).
:- ensure_loaded(library(examples/dt_winning)).
test(best_st_wnning):-
  run((dt_solve(Strategy,ExpValue),close_to(ExpValue,17.25),perm(Strategy,[[play1]]))).
:- end_tests(dt_winning).

:- begin_tests(dt_umbrella, []).
:- ensure_loaded(library(examples/dt_umbrella)).
test(best_st_umbrella):-
  run((dt_solve(Strategy,ExpValue),close_to(ExpValue,43),perm(Strategy,[[umbrella]]))).
:- end_tests(dt_umbrella).

:- begin_tests(dt_weather, []).
:- ensure_loaded(library(examples/dt_weather)).
test(best_st_weather):-
  run((dt_solve(Strategy,ExpValue),close_to(ExpValue,77),perm(Strategy,[[decide_u(rainy)]]))).
:- end_tests(dt_weather).

:- begin_tests(dt_viral, []).
:- ensure_loaded(library(examples/dt_viral)).
test(best_st_viral):-
  ansi_format([bold,fg(cyan)], '~nThis test takes some time.~n',[]),
  % run((dt_solve(Strategy,ExpValue),close_to(ExpValue,2.217),perm(Strategy,[[marketed(theo)],[marketed(guy)]]))).
  run((dt_solve(Strategy,ExpValue),close_to(ExpValue,3.21),perm(Strategy,[[marketed(theo)],[marketed(martijn)],[marketed(ingo)],[marketed(guy)]]))).
:- end_tests(dt_viral).

:- begin_tests(tabling_probs, []).
:- ensure_loaded(library(examples/dt_viral_probs)).
test(correct_probs):-
  run((compute_probs(PB,PI,PT,PA,PG,PM,PL,PK),close_to(PB,0.420),close_to(PI,0.480),close_to(PT,0.486),close_to(PA,0.384),close_to(PG,0.480),close_to(PM,0.486),close_to(PL,0.490),close_to(PK,0.291))).
:- end_tests(tabling_probs).

:- begin_tests(coin, []).

:-ensure_loaded(library(examples/coin)).

test(h_c):-
  run((prob(heads(coin),P),close_to(P,0.51))).
test(h_c_b):-
  run(((prob(heads(coin),P),bar(P,C)),close_to(P,0.51),is_dict(C))).
test(h_c_b2):-
  run(((prob(heads(coin),P),bar1(P,C)),close_to(P,0.51),is_dict(C))).
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

:- begin_tests(coin_mpe, []).

:-ensure_loaded(library(examples/coin_mpe)).

test(h_c):-
  run((map(heads(coin),Prob,Exp),close_to(Prob,0.27),
    perm(Exp, [rule(2,fair(coin),[fair(coin):0.9,biased(coin):0.1],true),
  rule(0,heads(coin),[heads(coin):0.5,tails(coin):0.5],(toss(coin),\+biased(coin))),
  rule(1,heads(coin),[heads(coin):0.6,tails(coin):0.4],(toss(coin),biased(coin)))]
  ))).
:- end_tests(coin_mpe).

:- begin_tests(coinmsw, []).

:-ensure_loaded(library(examples/coinmsw)).

test(r_c_h):-
  run((prob(res(coin,heads),P),close_to(P,0.51))).
test(r_c_t):-
  run((prob(res(coin,tails),P),close_to(P,0.49))).

:- end_tests(coinmsw).

:- begin_tests(coinmsw_memo, []).

:-ensure_loaded(library(examples/coinmsw_memo)).

test(r_c_h):-
  run((prob(res(coin,heads),fairness(coin,fair),P),close_to(P,0.5))).
test(r_c_t):-
  run((prob(res(coin,tails),fairness(coin,fair),P),close_to(P,0.5))).

:- end_tests(coinmsw_memo).

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

:- begin_tests(bloodtype, []).

:-ensure_loaded(library(examples/bloodtype)).

test(pc_p_f_a):-
  run((prob(pchrom(p_f,a),Prob),
  close_to(Prob, 0.318))).

test(p_f_a):-
  run((prob(pchrom(p_f,a),Prob),
  close_to(Prob, 0.3))).

test(p_a):-
  run((prob(bloodtype(p,a),Prob),
  close_to(Prob, 0.3186942939999999))).

test(p_b):-
  run((prob(bloodtype(p,b),Prob),
  close_to(Prob, 0.2239874943000002))).

test(p_aa):-
  run((prob(bloodtype(p,ab),Prob),
  close_to(Prob, 0.19329257700000035))).

test(p_null):-
  run((prob(bloodtype(p,null),Prob),
  close_to(Prob, 0.16751706690000012))).

:- end_tests(bloodtype).

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

:- begin_tests(mendel_mpe, []).

:-ensure_loaded(library(examples/mendel_mpe)).

test(s_p):-
  run((map(color(s,purple),Prob,Exp),close_to(Prob,0.5),
	perm(Exp, [rule(0, cg(s, 1, p), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
	 [mother(m, s), cg(m, 1, p), cg(m, 2, w)])]))).


test(s_w):-
  run((map(color(s,white),Prob,Exp),close_to(Prob,0.25),
  perm(Exp, [rule(0, cg(s, 1, w), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
     [mother(m, s), cg(m, 1, p), cg(m, 2, w)]),
	 rule(1, cg(s, 2, w), [cg(s, 2, w):0.5, cg(s, 2, p):0.5],
	   [father(f, s), cg(f, 1, w), cg(f, 2, p)])]))).

:- end_tests(mendel_mpe).

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

:-ensure_loaded(library(examples/uwcse_inf)).

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

:- begin_tests(abd1, []).

:-ensure_loaded(library(examples/abd1)).

test(a):-
  run((abd_prob(a,P,Exp),close_to(P,0.72),perm_map(Exp,[[e,c]]))).
test(bdd_a):-
  run((abd_bdd_dot_string(a,_BDD,_Var,_VarA,P,Exp),close_to(P,0.72),perm_map(Exp,[[e,c]]))).

:- end_tests(abd1).

:- begin_tests(abd1cons1, []).

:-ensure_loaded(library(examples/abd1cons1)).

test(a):-
  run((abd_prob(a,P,[[e]]),close_to(P,0.6))).

:- end_tests(abd1cons1).

:- begin_tests(abd1cons2, []).

:-ensure_loaded(library(examples/abd1cons2)).

test(a):-
  run((abd_prob(a,P,Exp),close_to(P,0.648),perm_map(Exp,[[c,e]]))).

:- end_tests(abd1cons2).

:- begin_tests(abd2, []).

:-ensure_loaded(library(examples/abd2)).

test(a):-
  run((abd_prob(a,P,Exp),close_to(P,0.72),perm_map(Exp,[[f,c,d]]))).
test(bdd_a):-
  run((abd_bdd_dot_string(a,_BDD,_Var,_VarA,P,Exp),close_to(P,0.72), perm_map(Exp,[[f,c,d]]))).

:- end_tests(abd2).

:- begin_tests(abd3, []).

:-ensure_loaded(library(examples/abd3)).

test(a):-
  run((abd_prob(a,P,Exp),close_to(P,0.72),perm_map(Exp,[[c,d,f]]))).
test(bdd_a):-
  run((abd_bdd_dot_string(a,_BDD,_Var,_VarA,P,Exp),close_to(P,0.72),
  perm_map(Exp,[[c,d,f]]))).

:- end_tests(abd3).

:- begin_tests(bag_game_mpe, []).

:-ensure_loaded(library(examples/bag_game_mpe)).

test(winb):-
  run((map_bdd_dot_string(win,_BDD,_Var,_VarA,P,Exp),close_to(P,0.162),
  perm(Exp,[
	  rule(0, '', [red:0.4, '':0.6], true),
		rule(2, blue, [blue:0.5, '':0.5], true),
		rule(3, yellow, [yellow:0.6, '':0.4], true),
	  rule(1, green, [green:0.9, '':0.09999999999999998], true)
	 ]))).

test(win):-
  run((map(win,P,Exp),close_to(P,0.162),
  perm(Exp,[
	  rule(0, '', [red:0.4, '':0.6], true),
		rule(2, blue, [blue:0.5, '':0.5], true),
		rule(3, yellow, [yellow:0.6, '':0.4], true),
	  rule(1, green, [green:0.9, '':0.09999999999999998], true)
	 ]))).
:- end_tests(bag_game_mpe).

:- begin_tests(bag_pb_mpe, []).

:-ensure_loaded(library(examples/bag_pb_mpe)).

test(ev):-
  run((map(ev,P,Exp),close_to(P,0.27),
	  perm(Exp,[
		  rule(2,pf(2,1),[pf(2,1):0.6,'':0.4],true),
			rule(1,pf(1,2),[pf(1,2):0.75,'':0.25],true),
			rule(0,pf(1,1),[pf(1,1):0.6,'':0.4],true)
		  ]))).

test(evb):-
  run((map_bdd_dot_string(ev,_BDD,_Var,_VarA,P,Exp),close_to(P,0.27),
	  perm(Exp,[
		  rule(2,pf(2,1),[pf(2,1):0.6,'':0.4],true),
			rule(1,pf(1,2),[pf(1,2):0.75,'':0.25],true),
			rule(0,pf(1,1),[pf(1,1):0.6,'':0.4],true)
		  ]))).

:- end_tests(bag_pb_mpe).

:- begin_tests(bag_simple, []).

:-ensure_loaded(library(examples/bag_simple)).

test(evb):-
  run((map_bdd_dot_string(ev,_BDD,_Var,_VarA,P,Exp),close_to(P,0.6),
	  perm(Exp,[
		  rule(0, red(b1), [red(b1):0.6, green(b1):0.3, blue(b1):0.1], pick(b1))
		  ]))).
test(ev):-
  run((map(ev,P,Exp),close_to(P,0.6),
	  perm(Exp,[
		  rule(0, red(b1), [red(b1):0.6, green(b1):0.3, blue(b1):0.1], pick(b1))
		  ]))).
:- end_tests(bag_simple).

:- begin_tests(bag_mpe, []).

:-ensure_loaded(library(examples/bag_mpe)).

test(evb):-
  run((map_bdd_dot_string(ev,_BDD,_Var,_VarA,P,Exp),close_to(P,0.36),
	  perm(Exp,[
		  rule(1, pick(b1), [pick(b1):0.6, no_pick(b1):0.4], true),
			rule(0, red(b1), [red(b1):0.6, green(b1):0.3, blue(b1):0.1], pick(b1))
		  ]))).
test(ev):-
  run((map(ev,P,Exp),close_to(P,0.36),
	  perm(Exp,[
		  rule(1, pick(b1), [pick(b1):0.6, no_pick(b1):0.4], true),
			rule(0, red(b1), [red(b1):0.6, green(b1):0.3, blue(b1):0.1], pick(b1))
		  ]))).
:- end_tests(bag_mpe).

:- begin_tests(eruption_mpe, []).

:-ensure_loaded(library(examples/eruption_mpe)).

test(eruption):-
  run((map(eruption,P,Exp),close_to(P,0.08316),
	  perm(Exp,[rule(1,sudden_energy_release,[sudden_energy_release:0.7,'':0.30000000000000004],true),
      rule(2,fault_rupture(southwest_northeast),[fault_rupture(southwest_northeast):0.6,'':0.4],true),
      rule(3,fault_rupture(east_west),[fault_rupture(east_west):0.55,'':0.44999999999999996],true),
      rule(0,eruption,[eruption:0.6,earthquake:0.3,'':0.10000000000000003],(sudden_energy_release,fault_rupture(southwest_northeast))),
      rule(0,eruption,[eruption:0.6,earthquake:0.3,'':0.10000000000000003],(sudden_energy_release,fault_rupture(east_west)))
		  ]))).
:- end_tests(eruption_mpe).

:- begin_tests(bag_1, []).

:-ensure_loaded(library(examples/bag_1)).

test(evb):-
  run((map_bdd_dot_string(ev,_BDD,_Var,_VarA,P,Exp),close_to(P,0.54),
	  perm(Exp,[
		  rule(1, pick(b1), [pick(b1):0.6, no_pick(b1):0.4], true)
		  ]))).
test(ev):-
  run((map(ev,P,Exp),close_to(P,0.54),
	  perm(Exp,[
		  rule(1, pick(b1), [pick(b1):0.6, no_pick(b1):0.4], true)
		  ]))).
:- end_tests(bag_1).

:- begin_tests(bag_2, []).

:-ensure_loaded(library(examples/bag_2)).
test(evb):-
  run((map_bdd_dot_string(ev,_BDD,_Var,_VarA,P,Exp),close_to(P,0.6),
	  perm(Exp,[
		  rule(0, red(b1), [red(b1):0.6, green(b1):0.3, blue(b1):0.1], pick(b1))
		  ]))).
test(ev):-
  run((map(ev,P,Exp),close_to(P,0.6),
	  perm(Exp,[
		  rule(0, red(b1), [red(b1):0.6, green(b1):0.3, blue(b1):0.1], pick(b1))
		  ]))).
:- end_tests(bag_2).


:- begin_tests(hmm_mpe, []).

:-ensure_loaded(library(examples/hmm_mpe)).

test(hmm_a_g):-

  run((map(hmm([a,g]),P,Exp),close_to(P,0.00054),
    perm(Exp,
      [rule(0,next_state(q1,q2,[]),[next_state(q1,q1,[]):0.5,next_state(q1,q2,[]):0.45,next_state(q1,end,[]):0.05],true),
      rule(2,letter(q1,a,[]),[letter(q1,a,[]):0.4,letter(q1,c,[]):0.3,letter(q1,g,[]):0.2,letter(q1,t,[]):0.1],true),
      rule(0,next_state(q1,q1,[q1]),[next_state(q1,q1,[q1]):0.5,next_state(q1,q2,[q1]):0.45,next_state(q1,end,[q1]):0.05],true),
      rule(2,letter(q1,a,[q1]),[letter(q1,a,[q1]):0.4,letter(q1,c,[q1]):0.3,letter(q1,g,[q1]):0.2,letter(q1,t,[q1]):0.1],true),
      rule(1,next_state(q2,end,[q1]),[next_state(q2,q1,[q1]):0.45,next_state(q2,q2,[q1]):0.5,next_state(q2,end,[q1]):0.05],true),
      rule(3,letter(q2,g,[q1]),[letter(q2,a,[q1]):0.1,letter(q2,c,[q1]):0.2,letter(q2,g,[q1]):0.3,letter(q2,t,[q1]):0.4],true)]
  ))).

:- end_tests(hmm_mpe).

:- begin_tests(meta, []).

:-ensure_loaded(library(examples/meta)).

test(meta):-
  run((prob(a,Prob),close_to(Prob,0.2)
  )).

:- end_tests(meta).


:- begin_tests(var_objdb, []).

:-ensure_loaded(library(examples/var_objdb)).

test(obj):-
  run((prob(obj(2),Prob),close_to(Prob, 0.08190000000000008)
  )).

test(nobj):-
  run((prob(numObj(0,2),Prob),close_to(Prob,0.06300000000000006)
  )).

:- end_tests(var_objdb).


:- begin_tests(pcfg, []).

:-ensure_loaded(library(examples/pcfg)).

test(pcfg):-
  run((prob(pcfg([a,b,a,a]),Prob),close_to(Prob,0.0024,0.0001)
  )).

:- end_tests(pcfg).


:- begin_tests(pcfglrdb, []).

:-ensure_loaded(library(examples/pcfglrdb)).

test(pcfglrdb):-
  run((prob(pcfg([a]),Prob),close_to(Prob,0.3)
  )).

:- end_tests(pcfglrdb).

:- begin_tests(card, []).

:-ensure_loaded(library(examples/card)).

test(card):-
  run((prob(pair,Prob),close_to(Prob,0.07692307692307693))).

:- end_tests(card).

:- begin_tests(card_body, []).

:-ensure_loaded(library(examples/card_body)).

test(card_body):-
  run((prob(pair,Prob),close_to(Prob,0.07692307692307693))).

:- end_tests(card_body).

:- begin_tests(card_disc, []).

:-ensure_loaded(library(examples/card_disc)).

test(card_disc):-
  run((prob(pair,Prob),close_to(Prob,0.07692307692307693))).

:- end_tests(card_disc).

:- begin_tests(card_disc_body, []).

:-ensure_loaded(library(examples/card_disc_body)).

test(card_disc_body):-
  run((prob(pair,Prob),close_to(Prob,0.07692307692307693))).

:- end_tests(card_disc_body).
