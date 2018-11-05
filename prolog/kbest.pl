
:- module(kbest,[ kbest/3,kbest/4,
  op(600,xfy,'::')
    ]).
/** <module> kbest

This module performs reasoning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It reads probabilistic program and computes the probability of queries
using kbest inference.


@author Stefano Bragaglia and Fabrizio Riguzzi
@license Artistic License 2.0 https://opensource.org/licenses/Artistic-2.0
@copyright Stefano Bragaglia and Fabrizio Riguzzi
*/

:-use_module(library(pita)).

:- thread_local kbest_input_mod/1.

:-meta_predicate kbest(:,-,-).
:-meta_predicate kbest(:,-,-,-).



default_setting_kbest(epsilon_parsing, 1e-5).
default_setting_kbest(k, 64).
default_setting_kbest(prob_bound, 0.001).
default_setting_kbest(prob_step, 0.001).



% :- source.
% :- yap_flag(single_var_warnings, on).






/**
 * kbest(:Query:conjunction,+K:int,-Probability:float,-Exps:list) is nondet
 *
 * The predicate computes the K most probable explanations of the conjunction of literals Query.
 * It returns the explanations in Exps together with their Probability
 */
kbest(M:Goals, K, P, Exps) :-
  compute_exp(Goals,M,K,BestK),
  convert_exps(BestK,M,Exps),
  compute_prob(BestK,M,P).

/**
 * kbest(:Query:conjunction,+K:int,-Exps:list) is nondet
 *
 * The predicate computes the K most probable explanations of the conjunction of literals Query.
 * It returns the explanations in Exps
 */
kbest(M:Goals, K, Exps) :-
  compute_exp(Goals,M,K,BestK),
  convert_exps(BestK,M,Exps).

compute_prob(Exps,M,P):-
  init(Env),
  retractall(M:v(_,_,_)),
  maplist(exp2bdd(M,Env),Exps,LB),
  or_list(LB,Env,BDD),
  ret_prob(Env,BDD,P),
  end(Env).

exp2bdd(M,Env,_P-(Exp,_,_),BDD):-
  one(Env,One),
  foldl(choice2bdd(Env,M),Exp,One,BDD).

choice2bdd(Env,M,(N,R,S),BDD0,BDD):-
  M:rule_by_num(R, _S, _N, Head, _Body),
  get_probs(Head,Probs),
  get_var_n(M,Env,R,S,Probs,V),
  equality(Env,V,N,B),
  and(Env,BDD0,B,BDD).

compute_exp(Goals,M,K,BestK):-
  list2and(GL,Goals),
	M:local_kbest_setting(prob_step, ProbStep),
	ProbStepLog is log(ProbStep),
	% NB: log(1.0) == 0.0 !!!
	main([0.0-0.0-([], [], GL)], M, K, ProbStepLog, BestK).

convert_exps([],_M,[]).

convert_exps([LogP-(E, _, _)|T],M,[P-Exp|TE]):-
  P is exp(LogP),
  convert_exp(E,M,Exp),
  convert_exps(T,M,TE).

convert_exp([],_M,[]).

convert_exp([(N,R,S)|T],M,[rule(R,Head,HeadList,Body)|TDelta]):-
	M:rule(Head, _, N, R, S, _NH, HeadList, Body),!,
  convert_exp(T,M,TDelta).




/* main(Goals, K, ProbStep, Best)
 * ------------------------------
 * This tail recursive predicate returns the Best K complete solutions to the
 * given Goals. The probability bound is dinamically computed at each iteration.
 *
 * INPUT
 *  - Goals: list of goals to achive.
 *  - K: desired number of solutions.
 *  - ProbStep: value used to update the probability bound.
 *
 * OUTPUT
 *  - Best: list of best solutions (at most k).
 */
main(Goals, M, K, ProbStep, Best) :-
	K > 0,
	main(Goals, M, ProbStep, K, 0.0, [], Best).

main([], _M, _ProbStep, _Left, _Worst, Best, Best):-!.

main(Goals, M, ProbStep, Left0, Worst0, Best0, Best1) :-
	findall(Prob1-Bound-(Gnd1, Var1, Goals1),
			(member(Prob0-Bound0-(Gnd0, Var0, Goals0), Goals),
      Bound is Bound0+ ProbStep,
      explore(Bound, M, Prob0-(Gnd0, Var0, Goals0), Prob1-(Gnd1, Var1, Goals1))),
			Found),
	separate_main(Found, [], Complete, [], _UpperList, [], Incomplete),
	keepbest(Complete, Left0, Left2, Worst0, Worst2, Best0, Best2),
	main(Incomplete, M, ProbStep, Left2, Worst2, Best2, Best1).


/* separate(List, Low, Up, Next)
 * -----------------------------
 * This tail recursive predicate parses the input list and builds the list for
 * the lower bound, the upper bound and the pending goals.
 * The upper bound list contains both the items of the lower bound list and the
 * incomplete ones.
 *
 * INPUT
 *  - List: input list.
 *
 * OUTPUT
 *  - Low: list for lower bound.
 *  - Up: list for upper bound.
 *  - Next: list of pending goals.
 */
separate(List, Low, Up, Next) :-
%% Polarization: initial low, up and next lists are empty.
	separate(List, [], Low, [], Up, [], Next).

separate([], Low, Low, Up, Up, Next, Next) :- !.
%% Closing condition: stop if no more results (current lists are now final lists).

separate([Prob0-(Gnd0, [], [])|Tail], Low0, [Gnd0|Low1], Up0, [Prob0-(Gnd0, [], [])|Up1], Next0, Next1) :- !,
	separate(Tail, Low0, Low1, Up0, Up1, Next0, Next1).

separate([Prob0-(Gnd0, Var0, Goals)|Tail], Low0, Low1, Up0, [Prob0-(Gnd0, Var0, Goals)|Up1], Next0, [Prob0-(Gnd0, Var0, Goals)|Next1]) :-
	separate(Tail, Low0, Low1, Up0, Up1, Next0, Next1).

separate_main([], Low, Low, Up, Up, Next, Next) :- !.
%% Closing condition: stop if no more results (current lists are now final lists).

separate_main([Prob0-_Bound0-(Gnd0, [], [])|Tail], Low0, [Prob0-(Gnd0, [], [])|Low1], Up0, [Prob0-(Gnd0, [], [])|Up1], Next0, Next1) :- !,
	separate_main(Tail, Low0, Low1, Up0, Up1, Next0, Next1).

separate_main([Prob0-Bound0-(Gnd0, Var0, Goals)|Tail], Low0, Low1, Up0, [Prob0-Bound0-(Gnd0, Var0, Goals)|Up1], Next0, [Prob0-Bound0-(Gnd0, Var0, Goals)|Next1]) :-
	separate_main(Tail, Low0, Low1, Up0, Up1, Next0, Next1).



/* explore(ProbBound, Prob0-(Gnd0, Var0, Goals0), Prob1-(Gnd1, Var1, Goals1))
 * --------------------------------------------------------------------------
 * This tail recursive predicate reads current explanation and returns the
 * explanation after the current iteration without dropping below the given
 * probability bound.
 *
 * INPUT
 *  - ProbBound: the desired probability bound;
 *  - Prob0-(Gnd0, Var0, Goals0): current explanation
 *      - Gnd0: list of current ground choices,
 *      - Var0: list of current non-ground choices,
 *      - Prob0: probability of Gnd0,
 *      - Goals0: list of current goals.
 *
 * OUTPUT
 *  - Prob1-(Gnd1, Var1, Prob1, Goals1): explanation after current iteration
 *      - Gnd1: list of final ground choices,
 *      - Var1: list of final non-ground choices,
 *      - Prob1: probability of Gnd1,
 *      - Goals1: list of final goals.
 */
explore(_ProbBound, _M, Prob-(Gnd, Var, []), Prob-(Gnd, Var, [])) :- !.
%% Closing condition: stop if no more goals (input values are output values).

explore(ProbBound, _M, Prob-(Gnd, Var, Goals), Prob-(Gnd, Var, Goals)) :-
	%% Closing condition: stop if bound has been reached (input values are output values).
	Prob =< ProbBound, !.

% Negation, builtin
explore(ProbBound, M, Prob0-(Gnd0, Var0, [\+ Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :-
	builtin(Head), !,
	call((\+ Head)),
	explore(ProbBound, M, Prob0-(Gnd0, Var0, Tail), Prob1-(Gnd1, Var1, Goals1)).
	%% Recursive call: consider next goal (building next values)

% Negation
explore(ProbBound, M, Prob0-(Gnd0, Var0, [\+ Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :-
  !,
	list2and(HeadList, Head),
	findall(Prob-(Gnd, Var, CurrentGoals),
   explore(ProbBound, M, 0.0-([], [], HeadList),
    Prob-(Gnd, Var, CurrentGoals)),
   List),
	separate(List, [], LowerBound, [], _UpperBound, [], PendingGoals),
	(PendingGoals \= [] ->
		Var2 = Var0,
		Gnd2 = Gnd0,
		Goals1 = [\+ Head|Goals],
		explore(ProbBound, M, Prob0-(Gnd2, Var2, Tail), Prob1-(Gnd1, Var1, Goals))
  ;
		%% Recursive call: consider next goal (building next values)
		choose_clausesc(Gnd0, M, Var0, LowerBound, Var),
		get_prob(Var, M, 1, Prob),
		append(Gnd0, Var, Gnd2),
		Prob2 is Prob0 + log(Prob),
		explore(ProbBound, M, Prob2-(Gnd2, [], Tail), Prob1-(Gnd1, Var1, Goals1))
  ).
		%% Recursive call: consider next goal (building next values)

% Main, builtin
explore(ProbBound, M, Prob0-(Gnd0, Var0, [Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :-
	builtin(Head), !,
	call(Head),
	explore(ProbBound, M, Prob0-(Gnd0, Var0, Tail), Prob1-(Gnd1, Var1, Goals1)).
	% Recursive call: consider next goal (building next values)

% Main, def_rule
explore(ProbBound, M, Prob0-(Gnd0, Var0, [Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :-
	M:def_rule(Head, Goals0),
	append(Goals0, Tail, Goals2),
	explore(ProbBound, M, Prob0-(Gnd0, Var0, Goals2), Prob1-(Gnd1, Var1, Goals1)).
	% Recursive call: consider next goal (building next values)

% Main, find_rulec
explore(ProbBound, M, Prob0-(Gnd0, Var0, [Head|Tail]), Prob1-(Gnd1, Var1, Goals1)) :-
	find_rulec(Head, M, (R, S, N), Goals, Var0, _Prob),
	explore_pres(ProbBound, M, R, S, N, Goals, Prob0-(Gnd0, Var0, Tail), Prob1-(Gnd1, Var1, Goals1)).

explore_pres(ProbBound, M, R, S, N, Goals, Prob0-(Gnd0, Var0, Goals0), Prob1-(Gnd1, Var1, Goals)) :-
	(member_eq((N, R, S), Var0);
	member_eq((N, R, S), Gnd0)), !,
	append(Goals, Goals0, Goals2),
	explore(ProbBound, M, Prob0-(Gnd0, Var0, Goals2), Prob1-(Gnd1, Var1, Goals)).
	% Recursive call: consider next goal (building next values)

explore_pres(ProbBound, M, R, S, N, Goals, Prob0-(Gnd0, Var0, Goals0), Prob1-(Gnd1, Var1, Goals1)) :-
	append(Var0, [(N, R, S)], Var),
	append(Goals, Goals0, Goals2),
	get_prob(Var, M, 1, Prob),
	append(Gnd0, Var, Gnd2),
	Prob2 is Prob0 + log(Prob),
	explore(ProbBound, M, Prob2-(Gnd2, [], Goals2), Prob1-(Gnd1, Var1, Goals1)).
	% Recursive call: consider next goal (building next values)



/* keepbest(List, K, BestK)
 * ------------------------
 * This tail recursive predicate parses the given list of quads and returns the
 * list of its best k quads. If the given list of quads contains less than k
 * items, the predicate returns them all.
 *
 * INPUT
 *  - List: list of quads to parse.
 *  - K: desired number of quads.
 *
 * OUTPUT
 *  - BestK: final list of (at most) best k quads.
 */
keepbest(List, K, BestK) :-
	K > 0,
	keepbest(List, K, _Left, 0.0, _Worst, [], BestK).

/*keepbest([], _Left, _Worst, List, List).

keepbest([Prob-(_Gnd, _Var, _Goals)|Tail], 0, Worst, List0, List1) :-
	Prob =< Worst, !,
	keepbest(Tail, 0, Worst, List0, List1).

keepbest([Prob-(Gnd, Var, Goals)|Tail], 0, Worst, List0, List1) :-
	Prob > Worst, !,
	discard(Prob-(Gnd, Var, Goals), List0, List2, Worst2),
	keepbest(Tail, 0, Worst2, List2, List1).

keepbest([Prob-(Gnd, Var, Goals)|Tail], Left, Worst, List0, List1) :-
	insert(Prob-(Gnd, Var, Goals), List0, Worst, List2, Worst2),
	Left2 is Left - 1,
	keepbest(Tail, Left2, Worst2, List2, List1).*/



keepbest([], Left, Left, Worst, Worst, List, List).

keepbest([Prob-(_Gnd, _Var, _Goals)|Tail], 0, Left1, Worst0, Worst1, List0, List1) :-
	Prob =< Worst0, !,
	keepbest(Tail, 0, Left1, Worst0, Worst1, List0, List1).

keepbest([Prob-(Gnd, Var, Goals)|Tail], 0, Left1, Worst0, Worst1, List0, List1) :-
	Prob > Worst0, !,
	discard(Prob-(Gnd, Var, Goals), List0, List2, Worst2),
	keepbest(Tail, 0, Left1, Worst2, Worst1, List2, List1).

keepbest([Prob-(Gnd, Var, Goals)|Tail], Left0, Left1, Worst0, Worst1, List0, List1) :-
	insert(Prob-(Gnd, Var, Goals), List0, Worst0, List2, Worst2),
	Left2 is Left0 - 1,
	keepbest(Tail, Left2, Left1, Worst2, Worst1, List2, List1).



/* insert(Prob-(Gnd, Var, Goals), Sorted0, Worst0, Sorted1, Worst1)
 * ----------------------------------------------------------------
 * This tail recursive predicate inserts the given quad into the given sorted
 * list and returns the final sorted list. The input list must be sorted.
 * It also updates the prob value of the worst quad.
 *
 * INPUT
 *  - Prob-(Gnd, Var, Goals): quad to insert.
 *  - Sorted0: sorted list to insert the quad into.
 *  - Worst0: current worst prob value.
 *
 * OUTPUT
 *  - Sorted1: the final sorted list.
 *  - Worst1: the final worst prob value.
 */
insert(Prob-(Gnd, Var, Goals), [], _Worst, [Prob-(Gnd, Var, Goals)], Prob):-!.

insert(Prob-(Gnd, Var, Goals), [Prob_i-(Gnd_i, Var_i, Goals_i)|Tail], Worst, [Prob-(Gnd, Var, Goals), Prob_i-(Gnd_i, Var_i, Goals_i)|Tail], Worst) :-
	Prob >= Prob_i, !.

insert(Prob-(Gnd, Var, Goals), [Prob_i-(Gnd_i, Var_i, Goals_i)|Tail], Worst0, [Prob_i-(Gnd_i, Var_i, Goals_i)|Next], Worst1) :-
	Prob < Prob_i, !,
	insert(Prob-(Gnd, Var, Goals), Tail, Worst0, Next, Worst1).



/* discard(Prob-(Gnd, Var, Goals), Sorted0, Sorted1, Worst)
 * --------------------------------------------------------
 * This tail recursive predicate inserts the given quad into the given sorted
 * list, removes the last quad from it and returns the final sorted list.
 * The given sorted list contains at least one quad and must be sorted.
 * Previous worst prob value is not needed because it necessarely changes and
 * the new value is not known in advance.
 * It also updates the prob value of the worst quad.
 *
 * INPUT
 *  - Prob-(Gnd, Var, Goals): quad to insert.
 *  - Sorted0: sorted list to insert the quad into.
 *
 * OUTPUT
 *  - Sorted1: the final sorted list.
 *  - Worst: the final worst prob value.
 */
discard(Prob-(Gnd, Var, Goals), [_Prob_i-(_Gnd_i, _Var_i, _Goals_i)], [Prob-(Gnd, Var, Goals)], Prob) :- !.

discard(Prob-(Gnd, Var, Goals), [Prob_i-(Gnd_i, Var_i, Goals_i), Prob_l-(Gnd_l, Var_l, Goals_l)|Tail], [Prob-(Gnd, Var, Goals)|Next], Worst) :-
	Prob >= Prob_i, !,
	discard(Prob_i-(Gnd_i, Var_i, Goals_i), [Prob_l-(Gnd_l, Var_l, Goals_l)|Tail], Next, Worst).

discard(Prob-(Gnd, Var, Goals), [Prob_i-(Gnd_i, Var_i, Goals_i), Prob_l-(Gnd_l, Var_l, Goals_l)|Tail], [Prob_i-(Gnd_i, Var_i, Goals_i)|Next], Worst) :-
	Prob < Prob_i, !,
	discard(Prob-(Gnd, Var, Goals), [Prob_l-(Gnd_l, Var_l, Goals_l)|Tail], Next, Worst).

find_rulec(H, M, (R, S, N), Body, C, P) :-
	M:rule(H, P, N, R, S, _NH, _Head, Body),
	not_already_present_with_a_different_head(N, R, S, C).


not_already_present_with_a_different_head(_HeadId, _RuleId, _Subst, []).

not_already_present_with_a_different_head(HeadId, RuleId, Subst, [(HeadId1, RuleId, Subst1)|Tail]) :-
	not_different(HeadId, HeadId1, Subst, Subst1), !,
	not_already_present_with_a_different_head(HeadId, RuleId, Subst, Tail).

not_already_present_with_a_different_head(HeadId, RuleId, Subst, [(_HeadId1, RuleId1, _Subst1)|Tail]) :-
	RuleId \== RuleId1,
	not_already_present_with_a_different_head(HeadId, RuleId, Subst, Tail).



not_different(_HeadId, _HeadId1, Subst, Subst1) :-
	Subst \= Subst1, !.

not_different(HeadId, HeadId1, Subst, Subst1) :-
	HeadId \= HeadId1, !,
	dif(Subst, Subst1).

not_different(HeadId, HeadId, Subst, Subst).

get_groundc([], _M, [], [], P, P) :- !.

get_groundc([H|T], M, [H|T1], TV, P0, P1) :-
	ground(H), !,
	H=(N, R, S),
	M:rule_by_num(R, S, _N, Head, _Body),
	(nth0(N, Head, (_A:P));
  nth0(N, Head, (_A::P))),!,
	P2 is P0*P,
	get_groundc(T, M, T1, TV, P2, P1).

get_groundc([H|T], M, T1, [H|TV], P0, P1) :-
	get_groundc(T, M, T1, TV, P0, P1).

get_prob([], _M, P, P) :- !.

get_prob([H|T], M, P0, P1) :-
	H=(N, R, S),
	M:rule_by_num(R, S, _N, Head, _Body),
	(nth0(N, Head, (_A:P));
  nth0(N, Head, (_A::P))),!,
	P2 is P0*P,
	get_prob(T, M, P2, P1).



choose_clausesc(_G, _M, C, [], C).

choose_clausesc(CG0, M, CIn, [D|T], COut) :-
	member((N, R, S), D),
	choose_clauses_present(M, N, R, S, CG0, CIn, COut, T).

choose_clausesc(G0, M, CIn, [D|T], COut) :-
	member((N, R, S), D),
	new_head(M,N, R, S, N1),
	\+ already_present(N1, R, S, CIn),
	\+ already_present(N1, R, S, G0),
	impose_dif_cons(R, S, CIn),
	choose_clausesc(G0, M, [(N1, R, S)|CIn], T, COut).



choose_clauses_present(M, N, R, S, CG0, CIn, COut, T) :-
	already_present_with_a_different_head_ground(N, R, S, CG0), !,
	choose_clausesc(CG0, M, CIn, T, COut).

choose_clauses_present(M, N, R, S, CG0, CIn, COut, T) :-
	already_present_with_a_different_head(N, R, S, CIn),
	choose_a_head(N, R, S, CIn, C1),
	choose_clausesc(CG0, M, C1, T, COut).



/* new_head(N, R, S, N1)
 * ---------------------
 * This predicate selects an head for rule R different from N with substitution
 * S and returns it in N1.
 */
new_head(M, N, R, S, N1) :-
	M:rule_by_num(R, S, Numbers, _Head, _Body),
	nth0(N, Numbers, _Elem, Rest),
	member(N1, Rest).




/* already_present(N, R, S, [(N, R, SH)|_T])
 * -----------------------------------------
 * This predicate checks if a rule R with head N and selection S (or one of its
 * generalizations is in C) is already present in C.
 */
already_present(N, R, S, [(N, R, SH)|_T]) :-
	S=SH.

already_present(N, R, S, [_H|T]) :-
	already_present(N, R, S, T).



already_present_with_a_different_head(N, R, S, [(NH, R, SH)|_T]) :-
	\+ \+ S=SH, NH \= N.

already_present_with_a_different_head(N, R, S, [_H|T]) :-
	already_present_with_a_different_head(N, R, S, T).

already_present_with_a_different_head_ground(N, R, S, [(NH, R, SH)|_T]) :-
	S=SH, NH \= N.

already_present_with_a_different_head_ground(N, R, S, [_H|T]) :-
	already_present_with_a_different_head_ground(N, R, S, T).



impose_dif_cons(_R, _S, []) :- !.

impose_dif_cons(R, S, [(_NH, R, SH)|T]) :- !,
	dif(S, SH),
	impose_dif_cons(R, S, T).

impose_dif_cons(R, S, [_H|T]) :-
	impose_dif_cons(R, S, T).



/* choose_a_head(N, R, S, [(NH, R, SH)|T], [(NH, R, SH)|T])
 * --------------------------------------------------------
 * This predicate chooses and returns an head.
 * It instantiates a more general rule if it is contained in C with a different
 * head.
 */
choose_a_head(N, R, S, [(NH, R, SH)|T], [(NH, R, SH)|T]) :-
	S=SH,
	dif(N, NH).

/* choose_a_head(N, R, S, [(NH, R, SH)|T], [(NH, R, S), (NH, R, SH)|T])
 * --------------------------------------------------------------------
 * This predicate chooses and returns an head.
 * It instantiates a more general rule if it is contained in C with a different
 * head.
 * It ensures the same ground clause is not generated again.
 */
choose_a_head(N, R, S, [(NH, R, SH)|T], [(NH, R, S), (NH, R, SH)|T]) :-
	\+ \+ S=SH, S\==SH,
	dif(N, NH),
	dif(S, SH).

choose_a_head(N, R, S, [H|T], [H|T1]) :-
	choose_a_head(N, R, S, T, T1).


builtin(average(_L,_Av)).
builtin(prob(_,_)).
builtin(G):-
  predicate_property(G,built_in).
builtin(G):-
  predicate_property(G,imported_from(lists)).

average(L,Av):-
        sum_list(L,Sum),
        length(L,N),
        Av is Sum/N.


listN(N, N, []) :- !.

listN(NIn, N, [NIn|T]) :-
	N1 is NIn+1,
	listN(N1, N, T).

/* assert_rules()
 * --------------
 * This tail recursive predicate parses the given list of (Head:Prob) couples
 * and stores them incrementally as rules along with the other parameters.
 *
 * INPUT
 *  - Head: current head part.
 *  - Prob: probability of the current head part.
 *  - Index: index of the current head part.
 *  - Subst: substitution for the current head part.
 *  - Choices: list of current head parts indexes.
 *  - HeadList: complete head or list of its parts.
 *  - BodyList: complete body or list of its parts.
 */
assert_rules([],_M, _Index, _HeadList, _BodyList, _Choices, _Id, _Subst) :- !. % Closing condition.

assert_rules(['':_Prob], _M,_Index, _HeadList, _BodyList, _Choices, _Id, _Subst) :- !.

assert_rules([Head:Prob|Tail],M, Index, HeadList, BodyList, Choices, Id, Subst) :-
	assertz(M:rule(Head, Prob, Index, Id, Subst, Choices, HeadList, BodyList)),
	Next is Index + 1,
	assert_rules(Tail,M, Next, HeadList, BodyList,Choices,Id,Subst).


list2and([],true):-!.

list2and([X],X):-
    X\=(_,_),!.

list2and([H|T],(H,Ta)):-!,
    list2and(T,Ta).

member_eq(Item, [Head|_Tail]) :-
	Item==Head, !.

member_eq(Item, [_Head|Tail]) :-
	member_eq(Item, Tail).

process_head(HeadList, GroundHeadList) :-
  ground_prob(HeadList), !,
  process_head_ground(HeadList, 0, GroundHeadList).

process_head(HeadList0, HeadList):-
  get_probs(HeadList0,PL),
  foldl(minus,PL,1,PNull),
  append(HeadList0,['':PNull],HeadList).

minus(A,B,B-A).

prob_ann(_:P,P):-!.
prob_ann(P::_,P).


gen_head(H,P,V,V1,H1:P):-copy_term((H,V),(H1,V1)).
gen_head_disc(H,V,V1:P,H1:P):-copy_term((H,V),(H1,V1)).


/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([H], Prob, [Head:ProbHead1|Null]) :-
  (H=Head:ProbHead;H=ProbHead::Head),!,
  ProbHead1 is ProbHead,
  ProbLast is 1 - Prob - ProbHead1,
  prolog_load_context(module, M),kbest_input_mod(M),
  M:local_kbest_setting(epsilon_parsing, Eps),
  EpsNeg is - Eps,
  ProbLast > EpsNeg,
  (ProbLast > Eps ->
    Null = ['':ProbLast]
  ;
    Null = []
  ).

process_head_ground([H|Tail], Prob, [Head:ProbHead1|Next]) :-
  (H=Head:ProbHead;H=ProbHead::Head),
  ProbHead1 is ProbHead,
  ProbNext is Prob + ProbHead1,
  process_head_ground(Tail, ProbNext, Next).


ground_prob([]).

ground_prob([_Head:ProbHead|Tail]) :-!,
  ground(ProbHead), % Succeeds if there are no free variables in the term ProbHead.
  ground_prob(Tail).

ground_prob([ProbHead::_Head|Tail]) :-
  ground(ProbHead), % Succeeds if there are no free variables in the term ProbHead.
  ground_prob(Tail).


get_probs(Head, PL):-
  maplist(prob_ann,Head,PL).

/*get_probs([], []).

get_probs([_H:P|T], [P1|T1]) :-
  P1 is P,
  get_probs(T, T1).
*/


list2or([],true):-!.

list2or([X],X):-
    X\=;(_,_),!.

list2or([H|T],(H ; Ta)):-!,
    list2or(T,Ta).


/**
 * set_pita(:Parameter:atom,+Value:term) is det
 *
 * The predicate sets the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 *
 */
set_vit(M:Parameter,Value):-
  retract(M:local_kbest_setting(Parameter,_)),
  assert(M:local_kbest_setting(Parameter,Value)).

/**
 * setting_pita(:Parameter:atom,?Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_vit(M:P,V):-
  M:local_kbest_setting(P,V).

extract_vars_list(L,[],V):-
  rb_new(T),
  extract_vars_tree(L,T,T1),
  rb_keys(T1,V).

extract_vars(Term,V):-
  rb_new(T),
  extract_vars_term(Term,T,T1),
  rb_keys(T1,V).

extract_vars_term(Variable, Var0, Var1) :-
  var(Variable), !,
  (rb_lookup(Variable, Var0,_) ->
    Var1 = Var0
  ;
    rb_insert(Var0,Variable,1,Var1)
  ).

extract_vars_term(Term, Var0, Var1) :-
  Term=..[_F|Args],
  extract_vars_tree(Args, Var0, Var1).



extract_vars_tree([], Var, Var).

extract_vars_tree([Term|Tail], Var0, Var1) :-
  extract_vars_term(Term, Var0, Var),
  extract_vars_tree(Tail, Var, Var1).

assert_all([],_M,[]).

assert_all([H|T],M,[HRef|TRef]):-
  assertz(M:H,HRef),
  assert_all(T,M,TRef).


get_next_rule_number(PName,R):-
  retract(PName:rule_n(R)),
  R1 is R+1,
  assert(PName:rule_n(R1)).

user:term_expansion(end_of_file, end_of_file) :-
  prolog_load_context(module, M),
  kbest_input_mod(M),!,
  retractall(kbest_input_mod(M)),
  style_check(+discontiguous).

user:term_expansion((:- kbest), []) :-!,
  prolog_load_context(module, M),
  retractall(M:local_kbest_setting(_,_)),
  findall(local_kbest_setting(P,V),default_setting_kbest(P,V),L),
  assert_all(L,M,_),
  assert(kbest_input_mod(M)),
  retractall(M:rule_n(_)),
  assert(M:rule_n(0)),
  M:(dynamic rule_by_num/5, rule/8, rule/4, query_rule/4),
  retractall(M:rule_by_num(_,_,_,_,_)),
  retractall(M:rule(_,_,_,_,_,_,_,_)),
  style_check(-discontiguous).

user:term_expansion((:- begin_plp), []) :-
  prolog_load_context(module, M),
  kbest_input_mod(M),!,
  assert(M:kbest_on).

user:term_expansion((:- end_plp), []) :-
  prolog_load_context(module, M),
  kbest_input_mod(M),!,
  retractall(M:kbest_on).

user:term_expansion((:- begin_lpad), []) :-
  prolog_load_context(module, M),
  kbest_input_mod(M),!,
  assert(M:kbest_on).

user:term_expansion((:- end_lpad), []) :-
  prolog_load_context(module, M),
  kbest_input_mod(M),!,
  retractall(M:kbest_on).

user:term_expansion((Head :- Body), []):-
  prolog_load_context(module, M),kbest_input_mod(M),M:kbest_on,
% disjunctive clause with more than one head atom
  Head = (_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  list2and(BodyList, Body),
	length(HeadList, LH),
	listN(0, LH, NH),
  get_next_rule_number(M,R),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  assert_rules(HeadList, M, 0, HeadList, BodyList, NH, R, VC),
	assertz(M:rule_by_num(R, VC, NH, HeadList, BodyList)).


user:term_expansion((Head :- Body), []):-
  prolog_load_context(module, M),kbest_input_mod(M),M:kbest_on,
	(Head=(_:_); Head=(_::_)),  !,
	list2or(HeadListOr, Head),
	process_head(HeadListOr, HeadList),
	list2and(BodyList, Body),
	length(HeadList, LH),
	listN(0, LH, NH),
  get_next_rule_number(M,R),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
	assert_rules(HeadList, M,0, HeadList, BodyList, NH, R, VC),
	assertz(M:rule_by_num(R, VC, NH, HeadList, BodyList)).

user:term_expansion((Head :- Body), []):-
  prolog_load_context(module, M),kbest_input_mod(M),M:kbest_on,!,
	list2and(BodyList, Body),
	assert(M:def_rule(Head, BodyList)).

user:term_expansion(Head , []):-
  prolog_load_context(module, M),kbest_input_mod(M),M:kbest_on,
	Head=(_;_), !,
	list2or(HeadListOr, Head),
	process_head(HeadListOr, HeadList),
	length(HeadList, LH),
	listN(0, LH, NH),
  get_next_rule_number(M,R),
  extract_vars_list(HeadList,[],VC),
  assert_rules(HeadList, M, 0, HeadList, [], NH, R, VC),
	assertz(M:rule_by_num(R, VC, NH, HeadList, [])).

user:term_expansion(Head , []):-
  prolog_load_context(module, M),kbest_input_mod(M),M:kbest_on,
	(Head=(_:_); Head=(_::_)), !,
	list2or(HeadListOr, Head),
	process_head(HeadListOr, HeadList),
	length(HeadList, LH),
	listN(0, LH, NH),
  get_next_rule_number(M,R),
  extract_vars_list(HeadList,[],VC),
  assert_rules(HeadList, M, 0, HeadList, [], NH, R, VC),
	assertz(M:rule_by_num(R, VC, NH, HeadList, [])).

user:term_expansion(Head, []):-
  prolog_load_context(module, M),kbest_input_mod(M),M:kbest_on,!,
	assert(M:def_rule(Head, [])).

:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(kbest:kbest(_,_,_), []).
sandbox:safe_meta(kbest:kbest(_,_,_,_), []).
