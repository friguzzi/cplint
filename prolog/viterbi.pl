
:- module(viterbi,[ viterbi/3,
  op(600,xfy,'::')
    ]).
/** <module> kbest

This module performs reasoning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It reads probabilistic program and computes the most likely explanation
of the query


@author Stefano Bragaglia and Fabrizio Riguzzi
@license Artistic License 2.0 https://opensource.org/licenses/Artistic-2.0
@copyright Stefano Bragaglia and Fabrizio Riguzzi
*/
:-use_module(library(rbtrees)).

:- thread_local vit_input_mod/1.

:-meta_predicate viterbi(:,-,-).



default_setting_viterbi(epsilon_parsing, 1e-5).
/**
 * viterbi(:Query:conjunction,+K:int,-Exp:list) is nondet
 *
 * The predicate computes the most probable explanation of the conjunction of literals Query.
 * It returns the explanation in Exp
 */
viterbi(M:Goals,Prob,Exp):-
  retractall(M:best_prob(_)),
  retractall(M:best_exp(_)),
  retractall(M:best_goal(_)),
  assert(M:best_prob(0.0)),
  assert(M:best_exp([])),
  list2and(GL,Goals),
  assert(M:best_goal(GL)),
	findall(_,find_exp(GL,M),_),
  M:best_prob(Prob),
  M:best_exp(Exp0),
  M:best_goal(GL),
  convert_exp(Exp0,M,Exp),
  retractall(M:best_prob(_)),
  retractall(M:best_exp(_)).

convert_exp([],_M,[]).

convert_exp([(R,S,N,_)|T],M,[rule(R,Head,HeadList,Body)|TDelta]):-
	M:rule(Head, _, N, R, S, _NH, HeadList, Body),!,
  convert_exp(T,M,TDelta).

find_exp(GL,M):-
  solve(GL,M,[],Exp,1,P),
  retract(M:best_prob(_)),
  retract(M:best_exp(_)),
  retract(M:best_goal(_)),
  assert(M:best_prob(P)),
  assert(M:best_exp(Exp)),
  assert(M:best_goal(GL)).

/* EXTERNAL FILE
 * -------------
 * The following libraries are required by the program to work fine.
 */

% :- source.
% :- yap_flag(single_var_warnings, on).


solve([],_M,C,C,P,P):-!.

solve([\+ H|T],M,CIn,COut,P0,P):-
	builtin(H),!,
	call(\+ H),
	solve(T,M,CIn,COut,P0,P).

solve([\+ H |T],M,CIn,COut,P0,P):-
  !,
	list2and(HL,H),
  (setof(D,solve_nob(HL,M,[],D),L)->
    choose_clauses(L,CIn,M,C1,P0,P1),
    solve(T,M,C1,COut,P1,P)
  ;
    solve(T,M,CIn,COut,P0,P)
  ).

solve([H|T],M,CIn,COut,P0,P):-
	builtin(H),!,
	call(H),
	solve(T,M,CIn,COut,P0,P).

solve([H|T],M,CIn,COut,P0,P):-
	M:def_rule(H,B),
	append(B,T,NG),
	solve(NG,M,CIn,COut,P0,P).

solve([H|T],M,CIn,COut,P0,P):-
	find_rule(H,M,(R,S,N,PR),B,CIn),
	append(B,T,NG),
	solve(NG,M,CIn,C1,P0,P1),
  update_exp(C1,COut,(R,S,N,PR),P1,P),
  check_bound(P,M).


update_exp(C,C,Ch,P,P):-
  member(Ch,C),!.

update_exp(C0,[(R,S,N,PR)|C0],(R,S,N,PR),P0,P):-
  P is P0*PR.

check_bound(P,M):-
  M:best_prob(BP),
  P>BP.


solve_nob([],_M,C,C):-!.

solve_nob([\+ H|T],M,CIn,COut):-
	builtin(H),!,
	call(\+ H),
	solve_nob(T,M,CIn,COut).

solve_nob([\+ H |T],M,CIn,COut):-!,
	list2and(HL,H),
	(setof(D,solve_nob(HL,M,[],D),L)->
		choose_clauses_nob(L,CIn,M,C1),
		solve_nob(T,M,C1,COut)
	;
		solve_nob(T,M,CIn,COut)
	).

solve_nob([H|T],M,CIn,COut):-
	builtin(H),!,
	call(H),
	solve_nob(T,M,CIn,COut).

solve_nob([H|T],M,CIn,COut):-
	M:def_rule(H,B),
	append(B,T,NG),
	solve_nob(NG,M,CIn,COut).

solve_nob([H|T],M,CIn,COut):-
	find_rule(H,M,(R,S,N,P),B,CIn),
	append(B,T,NG),
	solve_nob(NG,M,CIn,C1),
  update_exp(C1,COut,(R,S,N,P),1,_P2).


find_rule(H, M,(R, S, N,P), Body, C) :-
	M:rule(H, P, N, R, S, _NH, _Head, Body),
	not_already_present_with_a_different_head(N, R, S, C).

not_already_present_with_a_different_head(_HeadId, _RuleId, _Subst, []).

not_already_present_with_a_different_head(HeadId, RuleId, Subst, [(HeadId1, RuleId, Subst1,_P)|Tail]) :-
	not_different(HeadId, HeadId1, Subst, Subst1), !,
	not_already_present_with_a_different_head(HeadId, RuleId, Subst, Tail).

not_already_present_with_a_different_head(HeadId, RuleId, Subst, [(_HeadId1, RuleId1, _Subst1,_P)|Tail]) :-
	RuleId \== RuleId1,
	not_already_present_with_a_different_head(HeadId, RuleId, Subst, Tail).



not_different(_HeadId, _HeadId1, Subst, Subst1) :-
	Subst \= Subst1, !.

not_different(HeadId, HeadId1, Subst, Subst1) :-
	HeadId \= HeadId1, !,
	dif(Subst, Subst1).

not_different(HeadId, HeadId, Subst, Subst).


choose_clauses([],C,_M,C,P,P).

choose_clauses([D|T],CIn,M,COut,P0,P):-
	member((N,R,S,_P),D),
	already_present_with_a_different_head(N,R,S,CIn),!,
	choose_clauses(T,CIn,M,COut,P0,P).


choose_clauses([D|T],CIn,M,COut,P0,P):-
	member((R,S,N,_P),D),
	new_head(M,N,R,S,N1,PR),
	\+ already_present(N1,R,S,CIn),
  P1 is P0*PR,
  check_bound(P1,M),
	choose_clauses(T,[(R,S,N1,PR)|CIn],M,COut,P1,P).

choose_clauses_nob([],C,_M,C).

choose_clauses_nob([D|T],CIn,M,COut):-
	member((R,S,N,_P),D),
	already_present_with_a_different_head(N,R,S,CIn),!,
	choose_clauses_nob(T,CIn,M,COut).


choose_clauses_nob([D|T],CIn,M,COut):-
	member((R,S,N,_P),D),
	new_head(M,N,R,S,N1,PR),
	\+ already_present(N1,R,S,CIn),
	choose_clauses_nob(T,[(R,S,N1,PR)|CIn],M,COut).

/* select a head different from N for rule R with
substitution S, return it in N1 */
new_head(M,N,R,S,N1,P):-
	M:rule_by_num(R,S,Numbers,_Head,_Body),
	nth0(N, Numbers, _Elem, Rest),
	member(N1,Rest),
	M:rule(_H, P, N1, R, _S, _NH, _HL, _B).



already_present_with_a_different_head(N,R,S,[(NH,R,SH,_P)|_T]):-
	 S=SH,NH \= N.

already_present_with_a_different_head(N,R,S,[_H|T]):-
	already_present_with_a_different_head(N,R,S,T).


/* checks that a rule R with head N and selection S is already
present in C (or a generalization of it is in C) */
already_present(N,R,S,[(N,R,S,_P)|_T]):-!.

already_present(N,R,S,[_H|T]):-
	already_present(N,R,S,T).

/* SOLVING PREDICATES
 * ------------------
 * The predicates in this section solve any given problem with several class of
 * algorithms.
 *
 * Note: the original predicates (no more need and eligible to be deleted) have
 *       been moved to the end of the file.
 */


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
  prolog_load_context(module, M),vit_input_mod(M),
  M:local_viterbi_setting(epsilon_parsing, Eps),
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

/**
 * or_list(++ListOfBDDs:list,++Environment,--BDD:int) is det
 *
 * Returns in BDD a pointer to a BDD belonging to environment Environment
 * representing the disjunction of all the BDDs in ListOfBDDs
 */
or_list([H],_Env,H):-!.

or_list([H|T],Env,B):-
  or_list1(T,Env,H,B).


or_list1([],_Env,B,B).

or_list1([H|T],Env,B0,B1):-
  or(Env,B0,H,B2),
  or_list1(T,Env,B2,B1).

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
  retract(M:local_viterbi_setting(Parameter,_)),
  assert(M:local_viterbi_setting(Parameter,Value)).

/**
 * setting_pita(:Parameter:atom,?Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_vit(M:P,V):-
  M:local_viterbi_setting(P,V).

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
  vit_input_mod(M),!,
  retractall(vit_input_mod(M)),
  style_check(+discontiguous).

user:term_expansion((:- viterbi), []) :-!,
  prolog_load_context(module, M),
  retractall(M:local_viterbi_setting(_,_)),
  findall(local_viterbi_setting(P,V),default_setting_viterbi(P,V),L),
  assert_all(L,M,_),
  assert(vit_input_mod(M)),
  retractall(M:rule_n(_)),
  assert(M:rule_n(0)),
  M:(dynamic rule_by_num/5),
  M:(dynamic rule/8),
  retractall(M:rule_by_num(_,_,_,_,_)),
  retractall(M:rule(_,_,_,_,_,_,_,_)),
  style_check(-discontiguous).

user:term_expansion((:- begin_plp), []) :-
  prolog_load_context(module, M),
  vit_input_mod(M),!,
  assert(M:vit_on).

user:term_expansion((:- end_plp), []) :-
  prolog_load_context(module, M),
  vit_input_mod(M),!,
  retractall(M:vit_on).

user:term_expansion((:- begin_lpad), []) :-
  prolog_load_context(module, M),
  vit_input_mod(M),!,
  assert(M:vit_on).

user:term_expansion((:- end_lpad), []) :-
  prolog_load_context(module, M),
  vit_input_mod(M),!,
  retractall(M:vit_on).

user:term_expansion((Head :- Body), []):-
  prolog_load_context(module, M),vit_input_mod(M),M:vit_on,
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
  prolog_load_context(module, M),vit_input_mod(M),M:vit_on,
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
  prolog_load_context(module, M),vit_input_mod(M),M:vit_on,!,
	list2and(BodyList, Body),
	assert(M:def_rule(Head, BodyList)).

user:term_expansion(Head , []):-
  prolog_load_context(module, M),vit_input_mod(M),M:vit_on,
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
  prolog_load_context(module, M),vit_input_mod(M),M:vit_on,
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
  prolog_load_context(module, M),vit_input_mod(M),M:vit_on,!,
	assert(M:def_rule(Head, [])).

:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(viterbi:viterbi(_,_,_), []).
