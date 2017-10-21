
setting(check,true).

main:-
	format("~nTesting mcintyre~n",[]),
	setof(File,A^test(A,File),F),
	statistics(runtime,[_,_]),
	test_files(F),
	statistics(runtime,[_,T]),
	T1 is T /1000,
	format("Test successful, time ~f secs.~n",[T1]),

main:-
	format("Test unsuccessful.",[]).

main_rev:-
	format("~nTesting mcintyre~n",[]),
	setof(File,A^test(A,File),F0),
	reverse(F0,F),
	statistics(runtime,[_,_]),
	test_files(F),
	statistics(runtime,[_,T]),
	T1 is T /1000,
	format("Test successful, time ~f secs.~n",[T1]),!.

main_rev:-
	format("Test unsuccessful.",[]).

test_files([]).

test_files([H|T]):-
	format("~n~a.pl~n",[H]),
	load_files([H]),
%	library_directory(LD),
%	atom_concat(LD,'/cplint/examples/',ExDir),
%	atom_concat(ExDir,H,NH),
%	p(NH),!,
	findall(A,test(A,H),L),
	test_all(L,H),
	unload_file(H),
	test_files(T).

test_all([],_F).

test_all([H|T],F):-
	copy_term(H,NH),
	numbervars(NH),
%	NH=(_Query,close_to('P',_Prob)),
	format("~a ~p.~n",[F,NH]),
	(H=(G,R)),
	time(call(G)),!,
	format("\t~p.~n",[G]),
	(setting(check,true)->
	  call(R),!
        ;
          true),
	test_all(T,F).

epsilon(0.09).

close_to(V,T):-
	epsilon(E),
	TLow is T-E,
	THigh is T+E,
	TLow<V,
	V<THigh.

close_to(V,T,E):-
	TLow is T-E,
	THigh is T+E,
	TLow<V,
	V<THigh.

relative_epsilon(0.1).

relatively_close_to(V,T):-
	relative_epsilon(E),
	TLow is T*(1-E),
	THigh is T*(1+E),
	TLow<V,
	V<THigh.

relatively_close_to(V,T,E):-
	TLow is T*(1-E),
	THigh is T*(1+E),
	TLow<V,
	V<THigh.

test((mc_sample(heads(coin),1000,P),close_to(P,0.51)),coinmc).
test((mc_sample(tails(coin),1000,P),close_to(P,0.49)),coinmc).

test((mc_sample(on(0,1),1000,P),close_to(P,0.333333333333333)),threesideddicemc).
test((mc_sample(on(1,1),1000,P),close_to(P,0.222222222222222)),threesideddicemc).
test((mc_sample(on(2,1),1000,P),close_to(P,0.148148147703704)),threesideddicemc).

test((mc_sample(reach(s0,0,s0),1000,P),close_to(P,1)),markov_chain).
test((mc_sample(reach(s0,0,s1),1000,P),close_to(P,0.5984054054054054)),markov_chain).
test((mc_sample(reach(s0,0,s2),1000,P),close_to(P,0.4025135135135135)),markov_chain).
test((mc_sample(reach(s0,0,s3),1000,P),close_to(P,0.5998378378378378)),markov_chain).
test((mc_sample(reach(s0,0,s4),1000,P),close_to(P,0.49948717948717947)),markov_chain).
test((mc_sample(reach(s1,0,s0),1000,P),close_to(P,0)),markov_chain).
test((mc_sample_arg(reach(s0,0,S),50,S,Values),\+ member([s0]-_,Values)),markov_chain).
test((mc_sample_arg_first(reach(s0,0,S),50,S,Values),member(s3-_,Values),member(s2-_,Values)),markov_chain).

test((mc_sample(pre_pcfg([a]),1000,P),close_to(P,0.5)),prefix).
test((mc_sample(pre_pcfg([a]),1000,P),close_to(P,0.5)),prefix).
test((mc_sample(pre_pcfg([a,b]),1000,P),close_to(P,0.09692857142857143)),prefix).
test((mc_sample(pre_pcfg([b]),1000,P),close_to(P,0.5)),prefix).
test((mc_sample(pre_pcfg([a,b,a]),1000,P),close_to(P,0.03)),prefix).
test((mc_sample(pre_pcfg([b,a]),1000,P),close_to(P,0.1014)),prefix).

test((mc_sample(pre_plc([a,b]),1000,P),close_to(P,0.0326)),pre_plcg).

test((mc_sample(eventually(elect),100,P),close_to(P,1)),pctl_slep).
test((mc_sample(bounded_eventually(elect,3),100,P),close_to(P,0.97)),pctl_slep).
test((mc_expectation(eventually(elect,T),100,T,P),relatively_close_to(P,1.2)),pctl_slep).

test((mc_mh_sample(eval(2,4),eval(1,3),100,1,P),close_to(P,0.1151,0.3)),arithm).
test((mc_mh_sample(eval(2,4),(eval(0,2),eval(1,3)),100,1,P),close_to(P,1)),arithm).
%test((mc_rejection_sample(eval(2,4),eval(1,3),1000,P),close_to(P,0.1151)),arithm).
%test((mc_rejection_sample(eval(2,4),(eval(0,2),eval(1,3)),1000,P),close_to(P,1)),arithm).
test((mc_expectation(eval(2,Y),100,Y,E),relatively_close_to(E,3.968,1)),arithm).
test((mc_mh_expectation(eval(2,Y),eval(1,3),100,1,Y,E),relatively_close_to(E,2.855,1)),arithm).

test((mc_expectation(mix(X),1000,X,E),relatively_close_to(E,2.017964749114414,0.1)),gaussian_mixture).
test((mc_mh_expectation(mix(X),heads,1000,1,X,E),close_to(E,0,1)),gaussian_mixture).
test((mc_mh_expectation(mix(X),(mix(Y),Y>2),1000,1,X,E),relatively_close_to(E,5.00202846171105,0.25)),gaussian_mixture).

test((mc_expectation(kf(1,_O2,[T]),1000,T,E),close_to(E,0,0.1)),kalman_filter).
test((mc_lw_expectation(kf(1,_O2,T),kf(1,[2.5],_T),3000,T,[E]),relatively_close_to(E,0.6324846033555553)),kalman_filter).



test((mc_lw_expectation(value(0,X),(value(1,9),value(2,8)),1000,X,E),relatively_close_to(E,7.166960047178755,0.2)),gauss_mean_est).
test((mc_expectation(value(0,X),1000,X,E),relatively_close_to(E,0.9698875384639362,0.25)),gauss_mean_est).
test((mc_particle_expectation(value(0,X),[value(1,9),value(2,8)],1000,X,E),relatively_close_to(E,7.166960047178755,0.2)),gauss_mean_est).

test((mc_sample(is_word,1000,P),close_to(P,0.067222)),slp_pdcg).

test((mc_lw_sample(nation(a),student_gpa(4.0),1000,P),close_to(P,1.0)),indian_gpa).
test((mc_sample(nation(a),1000,P),close_to(P,0.25)),indian_gpa).

test((mc_lw_sample(nation(a),student_gpa(4.0),1000,P),close_to(P,1.0)),indian_gpadc).
test((mc_sample(nation(a),1000,P),close_to(P,0.25)),indian_gpadc).

test((mc_sample(drawn(1,1),1000,P),close_to(P,0.285)),nballs).
test((mc_sample((drawn(1,1),material(1,wood)),1000,P),close_to(P,0.086)),nballs).
test((mc_sample((drawn(1,1),material(1,wood),color(1,black)),1000,P),close_to(P,0.044)),nballs).

test((mc_sample(drawn(1,1),1000,P),close_to(P,0.285)),nballsdc).
test((mc_sample((drawn(1,1),material(1,wood)),1000,P),close_to(P,0.086)),nballsdc).
test((mc_sample((drawn(1,1),material(1,wood),color(1,black)),1000,P),close_to(P,0.044)),nballsdc).

test((mc_rejection_sample(recovery,drug,500,P),close_to(P,0.5)),simpsonmc).
test((mc_rejection_sample(recovery,\+ drug,500,P),close_to(P,0.4)),simpsonmc).
test((mc_rejection_sample(recovery,(drug,female),500,P),close_to(P,0.2)),simpsonmc).
test((mc_rejection_sample(recovery,(\+drug,female),500,P),close_to(P,0.3)),simpsonmc).
test((mc_rejection_sample(recovery,(drug,\+female),500,P),close_to(P,0.6)),simpsonmc).
test((mc_rejection_sample(recovery,(\+ drug,\+female),500,P),close_to(P,0.7)),simpsonmc).
test((mc_rejection_sample(recovery,do(drug),500,P),close_to(P,0.4)),simpsonmc).
test((mc_rejection_sample(recovery,do(\+ drug),500,P),close_to(P,0.5)),simpsonmc).
test((mc_rejection_sample(recovery,(do(drug),female),500,P),close_to(P,0.2)),simpsonmc).
test((mc_rejection_sample(recovery,(do(\+drug),female),500,P),close_to(P,0.3)),simpsonmc).
test((mc_rejection_sample(recovery,(do(drug),\+ female),500,P),close_to(P,0.6)),simpsonmc).
test((mc_rejection_sample(recovery,(do(\+ drug),\+ female),500,P),close_to(P,0.7)),simpsonmc).

test((mc_mh_sample(recovery,drug,500,2,P),close_to(P,0.5)),simpsonmc).
test((mc_mh_sample(recovery,\+ drug,500,2,P),close_to(P,0.4)),simpsonmc).
test((mc_mh_sample(recovery,(drug,female),500,2,P),close_to(P,0.2)),simpsonmc).
test((mc_mh_sample(recovery,(\+drug,female),500,2,P),close_to(P,0.3)),simpsonmc).
test((mc_mh_sample(recovery,(drug,\+female),500,2,P),close_to(P,0.6)),simpsonmc).
test((mc_mh_sample(recovery,(\+ drug,\+female),500,2,P),close_to(P,0.7)),simpsonmc).
test((mc_mh_sample(recovery,do(drug),500,2,P),close_to(P,0.4)),simpsonmc).
test((mc_mh_sample(recovery,do(\+ drug),500,2,P),close_to(P,0.5)),simpsonmc).
test((mc_mh_sample(recovery,(do(drug),female),500,2,P),close_to(P,0.2)),simpsonmc).
test((mc_mh_sample(recovery,(do(\+drug),female),500,2,P),close_to(P,0.3)),simpsonmc).
test((mc_mh_sample(recovery,(do(drug),\+ female),500,2,P),close_to(P,0.6)),simpsonmc).
test((mc_mh_sample(recovery,(do(\+ drug),\+ female),500,2,P),close_to(P,0.7)),simpsonmc).

test((mc_rejection_sample(has(2),has(3),500,P),close_to(P,0.4065135474609725,0.1)),viralmc).
test((mc_mh_sample(has(2),has(3),1000,2,P),close_to(P,0.4065135474609725,0.1)),viralmc).
test((mc_rejection_sample(has(2),do(has(3)),500,P),close_to(P,0.136)),viralmc).
test((mc_mh_sample(has(2),do(has(3)),500,1,P),close_to(P,0.136)),viralmc).

test((mc_sample(taught_by(c1,p1),1000,P),close_to(P,0.0926040439925477)),uwcsemc).
