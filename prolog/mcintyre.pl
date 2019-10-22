:- module(mcintyre,[
  mc_prob/2,
  mc_prob/3,
  mc_rejection_sample/5,
  mc_rejection_sample/4,
  mc_sample/3,
  mc_sample/4,
  mc_sample_arg/4,
  mc_sample_arg/5,
  mc_mh_sample/4,
  mc_mh_sample/5,
  mc_lw_sample/4,
  mc_gibbs_sample/5,
  mc_gibbs_sample/4,
  mc_gibbs_sample/3,
  mc_rejection_sample_arg/6,
  mc_rejection_sample_arg/5,
  mc_mh_sample_arg/5,
  mc_mh_sample_arg/6,
  mc_gibbs_sample_arg/4,
  mc_gibbs_sample_arg/5,
  mc_gibbs_sample_arg/6,
  mc_sample_arg_first/4,
  mc_sample_arg_first/5,
  mc_sample_arg_one/4,
  mc_sample_arg_one/5,
  mc_sample_arg_raw/4,
  mc_expectation/4,
  mc_mh_expectation/5,
  mc_mh_expectation/6,
  mc_gibbs_expectation/4,
  mc_gibbs_expectation/5,
  mc_gibbs_expectation/6,
  mc_rejection_expectation/5,
  set_mc/2,setting_mc/2,
  mc_load/1,mc_load_file/1,
  take_a_sample/5,
  sample_head/5,
  mc_lw_sample_arg/5,
  mc_lw_sample_arg_log/5,
  mc_lw_expectation/5,
  mc_particle_sample_arg/5,
  mc_particle_expectation/5,
  gaussian/5,
  gaussian/4,
  add_prob/3,
  op(600,xfy,'::'),
  op(600,xfy,'~'),
  op(500,xfx,'~='),
  op(1200,xfy,':='),
  op(1150,fx,mcaction),
  ~= /2,
  swap/2,
  msw/2,
  set_sw/2
  ]).

/** <module> mcintyre

This module performs reasoning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It reads probabilistic program and computes the probability of queries
using sampling.

See https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html for
details.

@author Fabrizio Riguzzi
@license Artistic License 2.0 https://opensource.org/licenses/Artistic-2.0
@copyright Fabrizio Riguzzi
*/

:- reexport(library(cplint_util)).
:- reexport(library(clpr)).

:-meta_predicate s(:,-).
:-meta_predicate mc_prob(:,-).
:-meta_predicate mc_prob(:,-,+).
:-meta_predicate mc_sample(:,+,-,-,-).
:-meta_predicate mc_rejection_sample(:,:,+,-,-,-).
:-meta_predicate mc_rejection_sample(:,:,+,-,+).
:-meta_predicate mc_rejection_sample(:,:,+,-).
:-meta_predicate mc_mh_sample(:,:,+,-).
:-meta_predicate mc_mh_sample(:,:,+,-,+).
:-meta_predicate mc_mh_sample(:,:,+,+,+,-,-,-).
:-meta_predicate mc_gibbs_sample(:,:,+,-,+).
:-meta_predicate mc_gibbs_sample(:,+,-,+).
:-meta_predicate mc_gibbs_sample(:,+,-).
:-meta_predicate mc_sample(:,+,-).
:-meta_predicate mc_sample(:,+,-,+).
:-meta_predicate mc_sample_arg(:,+,?,-).
:-meta_predicate mc_sample_arg(:,+,?,-,+).
:-meta_predicate mc_rejection_sample_arg(:,:,+,?,-,+).
:-meta_predicate mc_rejection_sample_arg(:,:,+,?,-).
:-meta_predicate mc_mh_sample_arg0(:,:,+,+,+,?,-).
:-meta_predicate mc_mh_sample_arg(:,:,+,?,-,+).
:-meta_predicate mc_mh_sample_arg(:,:,+,?,-).
:-meta_predicate mc_gibbs_sample_arg0(:,:,+,+,+,?,-).
:-meta_predicate mc_gibbs_sample_arg0(:,+,+,+,?,-).
:-meta_predicate mc_gibbs_sample_arg(:,:,+,?,-,+).
:-meta_predicate mc_gibbs_sample_arg(:,+,?,-,+).
:-meta_predicate mc_gibbs_sample_arg(:,+,?,-).
:-meta_predicate mc_sample_arg_first(:,+,?,-).
:-meta_predicate mc_sample_arg_first(:,+,?,-,+).
:-meta_predicate mc_sample_arg_one(:,+,?,-,+).
:-meta_predicate mc_sample_arg_one(:,+,?,-).
:-meta_predicate mc_sample_arg_raw(:,+,?,-).
:-meta_predicate mc_expectation(:,+,?,-).
:-meta_predicate mc_mh_expectation(:,:,+,?,-).
:-meta_predicate mc_mh_expectation(:,:,+,?,-,+).
:-meta_predicate mc_gibbs_expectation(:,+,?,-).
:-meta_predicate mc_gibbs_expectation(:,+,?,-,+).
:-meta_predicate mc_gibbs_expectation(:,:,+,?,-,+).
:-meta_predicate mc_rejection_expectation(:,:,+,?,-).
:-meta_predicate montecarlo_cycle(-,-,:,-,-,-,-,-,-).
:-meta_predicate montecarlo(-,-,-,:,-,-).
:-meta_predicate initial_sample_cycle(:).
:-meta_predicate gibbs_sample_cycle(:).
:-meta_predicate initial_sample(:).
:-meta_predicate lw_sample_bool(+,:,:,-).
:-meta_predicate initial_sample_neg(:).

:-meta_predicate mc_lw_sample(:,:,+,-).
:-meta_predicate mc_lw_sample_arg(:,:,+,?,-).
:-meta_predicate mc_lw_sample_arg_log(:,:,+,?,-).
:-meta_predicate mc_lw_expectation(:,:,+,?,-).
:-meta_predicate mc_particle_expectation(:,:,+,?,-).
:-meta_predicate mc_particle_sample_arg(:,:,+,?,-).
:-meta_predicate particle_sample_arg(:,:,+,?,-).
:-meta_predicate particle_sample_first(+,+,:,:,?).
:-meta_predicate particle_sample_arg_gl(:,:,+,+,+,-).
:-meta_predicate particle_sample_first_gl(+,+,:,:,-,-).
:-meta_predicate particle_sample(+,+,:,-).
:-meta_predicate lw_sample_cycle(:).
:-meta_predicate lw_sample_weight_cycle(:,-).
:-meta_predicate ~=(:,-).
:-meta_predicate msw(:,-).
:-meta_predicate set_sw(:,+).

:-meta_predicate mh_sample_arg(+,+,+,:,:,?,+,-,+,-).
:-meta_predicate mh_montecarlo(+,+,+,+,+,+,-,:,:,+,-).
:-meta_predicate gibbs_montecarlo(+,+,+,:,-).
:-meta_predicate gibbs_montecarlo(+,+,+,:,:,-).

:-meta_predicate mc_gibbs_sample(:,:,+,+,+,-,-,-).
:-meta_predicate mc_gibbs_sample(:,+,+,+,-,-,-).
:-meta_predicate gibbs_sample_arg(+,:,:,+,?,+,-).
:-meta_predicate gibbs_sample_arg(+,:,+,?,+,-).


:-meta_predicate rejection_montecarlo(+,+,+,:,:,-,-).
:-meta_predicate set_mc(:,+).
:-meta_predicate setting_mc(:,-).

:-use_module(library(lists)).
:-use_module(library(rbtrees)).
:-use_module(library(apply)).
:-use_module(library(assoc)).
:-use_module(library(clpr)).
:-use_module(library(clpr)).
:-use_module(library(clpfd)).
:-use_module(library(matrix)).
:-use_module(library(option)).
:-use_module(library(predicate_options)).

:-predicate_options(mc_prob/3,3,[bar(-)]).
:-predicate_options(mc_sample/4,4,[successes(-),failures(-),bar(-)]).
:-predicate_options(mc_mh_sample/5,5,[successes(-),failures(-),mix(+),lag(+)]).
:-predicate_options(mc_gibbs_sample/5,5,[successes(-),failures(-),mix(+),block(+)]).
:-predicate_options(mc_gibbs_sample/4,4,[successes(-),failures(-),mix(+),block(+)]).
:-predicate_options(mc_rejection_sample/5,5,[successes(-),failures(-)]).
:-predicate_options(mc_sample_arg/5,5,[bar(-)]).
:-predicate_options(mc_rejection_sample_arg/6,6,[bar(-)]).
:-predicate_options(mc_mh_sample_arg/6,6,[mix(+),lag(+),bar(-)]).
:-predicate_options(mc_gibbs_sample_arg/6,6,[mix(+),bar(-),block(+)]).
:-predicate_options(mc_gibbs_sample_arg/5,5,[mix(+),bar(-),block(+)]).
:-predicate_options(mc_sample_arg_first/5,5,[bar(-)]).
:-predicate_options(mc_sample_arg_one/5,5,[bar(-)]).
:-predicate_options(mc_mh_expectation/6,6,[mix(+),lag(+)]).
:-predicate_options(mc_gibbs_expectation/6,6,[mix(+),block(+)]).
:-predicate_options(mc_gibbs_expectation/5,5,[mix(+),block(+)]).
:-predicate_options(histogram/3,3,[max(+),min(+),nbins(+)]).
:-predicate_options(density/3,3,[max(+),min(+),nbins(+)]).
:-predicate_options(density2d/3,3,[xmax(+),xmin(+),ymax(+),ymin(+),nbins(+)]).

:- style_check(-discontiguous).



:- thread_local v/3,rule_n/1,mc_input_mod/1,local_mc_setting/2.

/*:- multifile one/2,zero/2,and/4,or/4,bdd_not/3,init/3,init_bdd/2,init_test/1,
  end/1,end_bdd/1,end_test/0,ret_prob/3,em/9,randomize/1,
  get_var_n/5,add_var/5,equality/4.*/
%  remove/3.


/* k
 * -
 * This parameter shows the amount of items of the same type to consider at once.
 *
 * Default value:	500
 * Applies to:		bestfirst, bestk, montecarlo
 */
default_setting_mc(k, 500).
/* min_error
 * ---------
 * This parameter shows the threshold for the probability interval.
 *
 * Default value:	0.02
 * Applies to:		bestfirst, montecarlo
 */
default_setting_mc(min_error, 0.02).

default_setting_mc(max_samples,5e4).


default_setting_mc(epsilon_parsing, 1e-5).
/* on, off */

default_setting_mc(compiling,off).

:-set_prolog_flag(unknown,warning).

default_setting_mc(depth_bound,false).  %if true, it limits the derivation of the example to the value of 'depth'
default_setting_mc(depth,2).
default_setting_mc(single_var,false). %false:1 variable for every grounding of a rule; true: 1 variable for rule (even if a rule has more groundings),simpler.
default_setting_mc(prism_memoization,false). %false: original prism semantics, true: semantics with memoization
/**
 * mc_load(++File:atom) is det
 *
 * Loads File.lpad if it exists, otherwise loads File.cpl if it exists.
 */
mc_load(File):-
  atomic_concat(File,'.lpad',FileLPAD),
  (exists_file(FileLPAD)->
    mc_load_file(FileLPAD)
  ;
    atomic_concat(File,'.cpl',FileCPL),
    (exists_file(FileCPL)->
      mc_load_file(FileCPL)
    )
  ).

/**
 * mc_load_file(++FileWithExtension:atom) is det
 *
 * Loads FileWithExtension.
 */
mc_load_file(File):-
  begin_lpad_pred,
  user:consult(File),
  end_lpad_pred.

/**
 * s(:Query:atom,-Probability:float) is nondet
 *
 * The predicate computes the probability of the ground query Query.
 * If Query is not ground, it returns in backtracking all instantiations of
 * Query together with their probabilities
 */
s(Mo:Goal,P):-
  Mo:local_mc_setting(min_error, MinError),
  Mo:local_mc_setting(k, K),
% Resetting the clocks...
% Performing resolution...
  copy_term(Goal,Goal1),
  numbervars(Goal1),
  save_samples(Mo,Goal1),
  montecarlo_cycle(0, 0, Mo:Goal, K, MinError, _Samples, _Lower, P, _Upper),
  !,
  erase_samples(Mo),
  restore_samples_delete_copy(Mo,Goal1).

save_samples_tab(M,I,S):-
  M:sampled(R,Sub,V),
  assert(M:mem(I,S,R,Sub,V)),
  retract(M:sampled(R,Sub,V)),
  fail.

save_samples_tab(M,I,S):-
  M:sampled_g(Sub,V),
  assert(M:mem(I,S,rw,Sub,V)),
  retract(M:sampled_g(Sub,V)),
  fail.

save_samples_tab(M,I,S):-
  M:sampled_g(Sub),
  assert(M:mem(I,S,r,Sub,1)),
  retract(M:sampled_g(Sub)),
  fail.

save_samples_tab(_M,_I,_S).

save_samples(M,I,S):-
  M:sampled(R,Sub,V),
  assert(M:mem(I,S,R,Sub,V)),
  retract(M:sampled(R,Sub,V)),
  fail.

save_samples(M,_I,_S):-
  retractall(M:sampled_g(_,_)),
  retractall(M:sampled_g(_)).

save_samples(M,G):-
  M:sampled(R,Sub,V),
  assert(M:mem(G,R,Sub,V)),
  erase(M:sampled(R,Sub,V)),
  fail.

save_samples(_M,_G).

restore_samples(M,I,S):-
  M:mem(I,S,R,Sub,V),
  assert_samp(R,M,Sub,V),
  fail.

restore_samples(_M,_I,_S).

assert_samp(r,M,Sub,_V):-!,
  assertz(M:sampled_g(Sub)).

assert_samp(rw,M,Sub,V):-!,
  assertz(M:sampled_g(Sub,V)).

assert_samp(R,M,Sub,V):-
  assertz(M:sampled(R,Sub,V)).


restore_samples(M,G):-
  M:mem(G,R,Sub,V),
  assertz(M:sampled(R,Sub,V)),
  fail.

restore_samples(_M,_G).


restore_samples_delete_copy(M,G):-
  retract(M:mem(G,R,Sub,V)),
  assertz(M:sampled(R,Sub,V)),
  fail.

restore_samples_delete_copy(_M,_G).

save_samples_copy(M,G):-
  M:sampled(R,Sub,V),
  assert(M:mem(G,R,Sub,V)),
  fail.

save_samples_copy(_M,_G).

delete_samples_copy(M,G):-
  retract(M:mem(G,_R,_Sub,_V)),
  fail.

delete_samples_copy(_M,_G).

count_samples(M,N):-
  findall(a,M:sampled(_Key,_Sub,_Val),L),
  length(L,N).


resample(_M,0):-!.

resample(M,N):-
  findall(sampled(Key,Sub,Val),M:sampled(Key,Sub,Val),L),
  sample_one(L,S),
  retractall(M:S),
  N1 is N-1,
  resample(M,N1).


erase_samples(M):-
  retractall(M:sampled(_,_,_)),
  retractall(M:sampled_g(_,_)),
  retractall(M:sampled_g(_)).

print_samples(M):-
  M:sampled(Key,Sub,Val),
  write(Key-(Sub,Val)),nl,
  fail.

print_samples(_M):-
  write(end),nl.

montecarlo_cycle(N0, S0, M:Goals, K, MinError, Samples, Lower, Prob, Upper):-!,
  montecarlo(K,N0, S0, M:Goals, N, S),
  P is S / N,
  D is N - S,
  Semi is 1.95996 * sqrt(P * (1 - P) / N),
  Int is 2 * Semi,
  M:local_mc_setting(max_samples,MaxSamples),
  /*   N * P > 5;   N * S / N > 5;   S > 5
  *   N (1 - P) > 5;   N (1 - S / N) > 5;   N (N - S) / N > 5;   N - S > 5
  */
  %format("Batch: samples ~d positive ~d interval ~f~n",[N,S,Int]),
% flush_output,
  (((S > 5, D > 5, (Int < MinError; Int =:= 0));
    ((Int < MinError; Int =:= 0),N>MaxSamples)) ->
    Samples is N,
    Lower is P - Semi,
    Prob is P,
    Upper is P + Semi %,
%   writeln(Semi)
   ;
     montecarlo_cycle(N, S, M:Goals, K, MinError, Samples, Lower, Prob, Upper)
   ).

montecarlo(0,N,S , _Goals,N,S):-!.

montecarlo(K1,Count, Success, M:Goals,N1,S1):-
  erase_samples(M),
  copy_term(Goals,Goals1),
  (M:Goals1->
    Valid=1
  ;
    Valid=0
  ),
  N is Count + 1,
  S is Success + Valid,
  %format("Sample ~d Valid ~d~n",[N,Valid]),
  %flush_output,
  K2 is K1-1,
  montecarlo(K2,N, S, M:Goals, N1,S1).


rejection_montecarlo(0,N,S , _Goals,_Ev,N,S):-!.

rejection_montecarlo(K1,Count, Success, M:Goals,M:Ev,N1,S1):-
  erase_samples(M),
  copy_term(Ev,Ev1),
  (M:Ev1->
    copy_term(Goals,Goals1),
    (M:Goals1->
      Succ=1
    ;
      Succ=0
    ),
    N is Count + 1,
    S is Success + Succ,
  %format("Sample ~d Valid ~d~n",[N,Valid]),
  %flush_output,
    K2 is K1-1
  ;
    N = Count,
    S = Success,
    K2 = K1
  ),
  rejection_montecarlo(K2,N, S, M:Goals,M:Ev, N1,S1).

mh_montecarlo(_L,K,_NC0,N,S,Succ0,Succ0, _Goals,_Ev,N,S):-
  K=<0,!.

mh_montecarlo(L,K0,NC0,N0, S0,Succ0, SuccNew,M:Goal, M:Evidence, N, S):-
  resample(M,L),
  copy_term(Evidence,Ev1),
  (M:Ev1->
    copy_term(Goal,Goal1),
    (M:Goal1->
      Succ1=1
    ;
      Succ1=0
    ),
    count_samples(M,NC1),
    (accept(NC0,NC1)->
      Succ = Succ1,
      delete_samples_copy(M,Goal),
      save_samples_copy(M,Goal)
    ;
      Succ = Succ0,
      erase_samples(M),
      restore_samples(M,Goal)
    ),
    N1 is N0 + 1,
    S1 is S0 + Succ,
  %format("Sample ~d Valid ~d~n",[N,Valid]),
  %flush_output,
    K1 is K0-1
  ;
    NC1 = NC0,
    Succ = Succ0,
    N1 is N0 + 1,
    K1 is K0-1,
    S1 is S0 + Succ,
    erase_samples(M),
    restore_samples(M,Goal)
  ),
  mh_montecarlo(L,K1,NC1,N1, S1,Succ, SuccNew,M:Goal,M:Evidence, N,S).

accept(NC1,NC2):-
  P is min(1,NC1/NC2),
  random(P0),
  P>P0.

/**
 * mc_prob(:Query:atom,-Probability:float,+Options:list) is det
 *
 * The predicate computes the probability of the query Query
 * If Query is not ground, it considers it as an existential query
 * and returns the probability that there is a satisfying assignment to
 * the query.
 *
 * Options is a list of options, the following are recognised by mc_prob/3:
 * * bar(-BarChart:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with a bar for the
 *   probability of success and a bar for the probability of failure.
 */
mc_prob(M:Goal,P,Options):-
  s(M:Goal,P),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    bar(P,Chart)
  ).
/**
 * mc_prob(:Query:atom,-Probability:float) is det
 *
 * Equivalent to mc_prob/2 with an empty option list.
 */
mc_prob(M:Goal,P):-
  mc_prob(M:Goal,P,[]).

/**
 * mc_sample(:Query:atom,+Samples:int,-Probability:float,+Options:list) is det
 *
 * The predicate samples Query a number of Samples times and returns
 * the resulting Probability (Successes/Samples)
 * If Query is not ground, it considers it as an existential query
 *
 * Options is a list of options, the following are recognised by mc_sample/4:
 * * successes(-Successes:int)
 *   Number of successes
 * * failures(-Failures:int)
 *   Number of failures
 * * bar(-BarChart:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with a bar for the
 *   number of successes and a bar for the number of failures.
 */
mc_sample(M:Goal,S,P,Options):-
  option(successes(T),Options,_T),
  option(failures(F),Options,_F),
  mc_sample(M:Goal,S,T,F,P),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    bar(T,F,Chart)
  ).

/**
 * mc_sample(:Query:atom,+Samples:int,-Probability:float) is det
 * 
 * Equivalent to mc_sample/4 with an empty option list.
 */
mc_sample(M:Goal,S,P):-
  mc_sample(M:Goal,S,P,[]).

/**
 * mc_sample(:Query:atom,+Samples:int,-Successes:int,-Failures:int,-Probability:float) is det
 *
 * The predicate samples Query  a number of Samples times and returns
 * the number of Successes, of Failures and the
 * Probability (Successes/Samples)
 * If Query is not ground, it considers it as an existential query
 */
mc_sample(M:Goal,S,T,F,P):-
  copy_term(Goal,Goal1),
  numbervars(Goal1),
  save_samples(M,Goal1),
  montecarlo(S,0, 0, M:Goal, N, T),
  P is T / N,
  F is N - T,
  erase_samples(M),
  restore_samples_delete_copy(M,Goal1).

/**
 * mc_rejection_sample(:Query:atom,:Evidence:atom,+Samples:int,-Probability:float,+Options:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true and returns
 * the Probability of Query.
 * It performs rejection sampling: if in a sample Evidence is false, the
 * sample is discarded.
 * If Query/Evidence are not ground, it considers them an existential queries.
 *
 * Options is a list of options, the following are recognised by mc_rejection_sample/5:
 * * successes(-Successes:int)
 *   Number of succeses
 * * failures(-Failures:int)
 *   Number of failueres
 */
mc_rejection_sample(M:Goal,M:Evidence,S,P,Options):-
  option(successes(T),Options,_T),
  option(failures(F),Options,_F),
  mc_rejection_sample(M:Goal,M:Evidence,S,T,F,P).

/**
 * mc_rejection_sample(:Query:atom,:Evidence:atom,+Samples:int,-Probability:float) is det
 *
 * Equivalent to mc_rejection_sample/5 with an empty option list.
 */
mc_rejection_sample(M:Goal,M:Evidence,S,P):-
  mc_rejection_sample(M:Goal,M:Evidence,S,P,[]).
 /**
 * mc_rejection_sample(:Query:atom,:Evidence:atom,+Samples:int,-Successes:int,-Failures:int,-Probability:float) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true and returns
 * the number of Successes, of Failures and the
 * Probability (Successes/Samples).
 * It performs rejection sampling: if in a sample Evidence is false, the
 * sample is discarded.
 * If Query/Evidence are not ground, it considers them an existential queries.
 */
mc_rejection_sample(M:Goal,M:Evidence0,S,T,F,P):-
  test_prism(M),
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  rejection_montecarlo(S,0, 0, M:Goal,M:Evidence, N, T),
  P is T / N,
  F is N - T,
  erase_samples(M),
  maplist(erase,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).

deal_with_ev(Ev,M,EvGoal,UC,CA):-
  list2and(EvL,Ev),
  partition(ac,EvL,ActL,EvNoActL),
  deal_with_actions(ActL,M,UC,CA),
  list2and(EvNoActL,EvGoal).

deal_with_actions(ActL,M,UC,CA):-
  empty_assoc(AP0),
  foldl(get_pred_const,ActL,AP0,AP),
  assoc_to_list(AP,LP),
  maplist(update_clauses(M),LP,UCL,CAL),
  partition(nac,ActL,_NActL,PActL),
  maplist(assert_actions(M),PActL,ActRefs),
  append([ActRefs|UCL],UC),
  append(CAL,CA).

assert_actions(M,do(A),Ref):-
  M:assertz(A,Ref).

update_clauses(M,P/0- _,[],LCA):-!,
  findall(Ref,M:clause(P,_B,Ref),UC),
  findall((P:-B),M:clause(P,B),LCA),
  maplist(erase,UC).

update_clauses(M,P/A-Constants,UC,CA):-
  functor(G,P,A),
  G=..[_|Args],
  findall((G,B,Ref),M:clause(G,B,Ref),LC),
  maplist(get_const(Args),Constants,ConstraintsL),
  list2and(ConstraintsL,Constraints),
  maplist(add_cons(G,Constraints,M),LC,UC,CA).

add_cons(G,C,M,(H,B,Ref),Ref1,(H:-B)):-
  copy_term((G,C),(G1,C1)),
  G1=H,
  erase(Ref),
  M:assertz((H:-(C1,B)),Ref1).


get_const(Args,Constants,Constraint):-
  maplist(constr,Args,Constants,ConstraintL),
  list2and(ConstraintL,Constraint).

constr(V,C,dif(V,C)).

get_pred_const(do(Do0),AP0,AP):-
  (Do0= (\+ Do)->
    true
  ;
    Do=Do0
  ),
  functor(Do,F,A),
  Do=..[_|Args],
  (get_assoc(F/A,AP0,V)->
    put_assoc(F/A,AP0,[Args|V],AP)
  ;
    put_assoc(F/A,AP0,[Args],AP)
  ).


ac(do(_)).
nac(do(\+ _)).

/**
 * mc_gibbs_sample(:Query:atom,+Samples:int,-Probability:float,+Options:list) is det
 *
 * The predicate samples Query  a number of Mix+Samples (Mix is set with the options, default value 0) 
 * times.
 * The first Mix (that is set with the options, default value 0) samples are discarded (mixing time).
 * It performs Gibbs sampling: each sample is obtained from the previous one by resampling
 * a variable given the values of the variables in its Markov blanket.
 * If Query/Evidence are not ground, it considers them as existential queries.
 *
 * Options is a list of options, the following are recognised by mc_gibbs_sample/4:
 * * block(+Block:int)
 *   Perform blocked Gibbs: Block variables are sampled together, default value 1
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * successes(-Successes:int)
 *   Number of succeses
 * * failures(-Failures:int)
 *   Number of failueres
 */
mc_gibbs_sample(M:Goal,S,P,Options):-
  option(mix(Mix),Options,0),
  option(block(Block),Options,1),
  option(successes(T),Options,_T),
  option(failures(F),Options,_F),
  mc_gibbs_sample(M:Goal,S,Mix,Block,T,F,P).

/**
 * mc_gibbs_sample(:Query:atom,+Samples:int,-Probability:float) is det
 *
 * Equivalent to mc_gibbs_sample/4 with an empty option list.
 */
mc_gibbs_sample(M:Goal,S,P):-
  mc_gibbs_sample(M:Goal,S,P,[]).

mc_gibbs_sample(M:Goal,S,Mix,Block,T,F,P):-
  copy_term(Goal,Goal1),
  (M:Goal1->
    Succ=1
  ;
    Succ=0
  ),
  (Mix=0->
    T1=Succ,
    S1 is S-1
  ;
    T1=0,
    S1=S,
    Mix1 is Mix-1,
    gibbs_montecarlo(Mix1,0,Block,M:Goal,_TMix)
  ),
  gibbs_montecarlo(S1,T1,Block,M:Goal,T),
  P is T / S,
  F is S - T,
  erase_samples(M).

gibbs_montecarlo(K,T,_Block,_Goals,T):-
  K=<0,!.

gibbs_montecarlo(K0, T0,Block,M:Goal,T):-
  remove_samples(M,Block,LS),
  copy_term(Goal,Goal1),
  (M:Goal1->
    Succ=1
  ;
    Succ=0
  ),
  T1 is T0 + Succ,
  K1 is K0-1,
  check_sampled(M,LS),
  gibbs_montecarlo(K1,T1,Block,M:Goal,T).

remove_samples(M,Block,Samp):-
  remove_samp(M,Block,Samp).

remove_samp(_M,0,[]):-!.

remove_samp(M,Block0,[(R,S)|Samp]):-
  retract(M:sampled(R,S,_)),!,
  Block is Block0-1,
  remove_samp(M,Block,Samp).

remove_samp(_M,_Block,[]).


% check_sampled(M,R,S):-
%   M:sampled(R,S,_),!.

check_sampled(M,S):-
  maplist(check_sam(M),S).

check_sam(M,(R,S)):-
  M:samp(R,S,_).
/**
 * mc_gibbs_sample(:Query:atom,:Evidence:atom,+Samples:int,-Probability:float,+Options:list) is det
 *
 * The predicate samples Query  a number of Mix+Samples (Mix is set with the options, default value 0) times given that
 * Evidence
 * is true and returns
 * the number of Successes, of Failures and the
 * Probability (Successes/Samples).
 * The first Mix (that is set with the options, default value 0) samples are discarded (mixing time).
 * It performs Gibbs sampling: each sample is obtained from the previous one by resampling
 * a variable given the values of the variables in its Markov blanket.
 * If Query/Evidence are not ground, it considers them as existential queries.
 *
 * Options is a list of options, the following are recognised by mc_gibbs_sample/5:
 * * block(+Block:int)
 *   Perform blocked Gibbs: Block variables are sampled together, default value 1
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * successes(-Successes:int)
 *   Number of succeses
 * * failures(-Failures:int)
 *   Number of failueres
 */
mc_gibbs_sample(M:Goal,M:Evidence,S,P,Options):-
  test_prism(M),
  option(mix(Mix),Options,0),
  option(block(Block),Options,1),
  option(successes(T),Options,_T),
  option(failures(F),Options,_F),
  mc_gibbs_sample(M:Goal,M:Evidence,S,Mix,Block,T,F,P).


mc_gibbs_sample(M:Goal,M:Evidence0,S,Mix,Block,T,F,P):-
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  gibbs_sample_cycle(M:Evidence),
  copy_term(Goal,Goal1),
  (M:Goal1->
    Succ=1
  ;
    Succ=0
  ),
  (Mix=0->
    T1=Succ,
    S1 is S-1
  ;
    T1=0,
    S1=S,
    Mix1 is Mix-1,
    gibbs_montecarlo(Mix1,0,Block,M:Goal,M:Evidence,_TMix)
  ),
  gibbs_montecarlo(S1,T1,Block,M:Goal,M:Evidence,T),
  P is T / S,
  F is S - T,
  erase_samples(M),
  maplist(erase,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).

gibbs_montecarlo(K,T,_Block,_G,_Ev,T):-
  K=<0,!.

gibbs_montecarlo(K0, T0,Block, M:Goal, M:Evidence,  T):-
  remove_samples(M,Block,LS),
  save_samples_copy(M,Evidence),
  gibbs_sample_cycle(M:Evidence),
  delete_samples_copy(M,Evidence),
  copy_term(Goal,Goal1),
  (M:Goal1->
    Succ=1
  ;
    Succ=0
  ),
  T1 is T0 + Succ,
  K1 is K0-1,
  check_sampled(M,LS),
  gibbs_montecarlo(K1, T1,Block,M:Goal,M:Evidence, T).


/**
 * mc_gibbs_sample_arg(:Query:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query  a number of Samples times.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * The first Mix (that is set with the options, default value 0) samples are discarded (mixing time).
 * It performs Gibbs sampling: each sample is obtained from the previous one by resampling
 * a variable given the values of the variables in its Markov blanket.
 *
 * Options is a list of options, the following are recognised by mc_gibbs_sample_arg/5:
 * * block(+Block:int)
 *   Perform blocked Gibbs: Block variables are sampled together, default value 1
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with
 *   a bar for each possible value of L,
 *   the list of value of Arg for which Query succeeds in
 *   a world sampled at random.
 */
mc_gibbs_sample_arg(M:Goal,S,Arg,ValList,Options):-
  test_prism(M),
  option(mix(Mix),Options,0),
  option(block(Block),Options,1),
  option(bar(Chart),Options,no),
  mc_gibbs_sample_arg0(M:Goal,S,Mix,Block,Arg,ValList),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).
/**
 * mc_gibbs_sample_arg(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * Equivalent to mc_gibbs_sample_arg/5 with an empty option list.
 */
mc_gibbs_sample_arg(M:Goal,S,Arg,ValList):-
  mc_gibbs_sample_arg(M:Goal,S,Arg,ValList,[]).
/**
 * mc_gibbs_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * The first Mix (that is set with the options, default value 0) samples are discarded (mixing time).
 * It performs Gibbs sampling: each sample is obtained from the previous one by resampling
 * a variable given the values of the variables in its Markov blanket.
 *
 * Options is a list of options, the following are recognised by mc_gibbs_sample_arg/6:
 * * block(+Block:int)
 *   Perform blocked Gibbs: Block variables are sampled together, default value 1
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with
 *   a bar for each possible value of L,
 *   the list of value of Arg for which Query succeeds in
 *   a world sampled at random.
 */
mc_gibbs_sample_arg(M:Goal,M:Evidence,S,Arg,ValList,Options):-
  test_prism(M),
  option(mix(Mix),Options,0),
  option(block(Block),Options,1),
  option(bar(Chart),Options,no),
  mc_gibbs_sample_arg0(M:Goal,M:Evidence,S,Mix,Block,Arg,ValList),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).




gibbs_sample_arg(K,_Goals,_Ev,_Block,_Arg,V,V):-
  K=<0,!.

gibbs_sample_arg(K0,M:Goal, M:Evidence,Block, Arg,V0,V):-
  remove_samples(M,Block,LS),
  save_samples_copy(M,Evidence),
  gibbs_sample_cycle(M:Evidence),
  delete_samples_copy(M,Evidence),
  findall(Arg,M:Goal,La),
  numbervars(La),
  (get_assoc(La, V0, N)->
    N1 is N+1,
    put_assoc(La,V0,N1,V1)
  ;
    put_assoc(La,V0,1,V1)
  ),
  K1 is K0-1,
  check_sampled(M,LS),
  gibbs_sample_arg(K1,M:Goal,M:Evidence,Block,Arg,V1,V).


/**
 * mc_gibbs_sample_arg0(:Query:atom,:Evidence:atom,+Samples:int,+Mix:int,+Block:int,+Lag:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * It performs blocked Gibbs sampling: each sample is obtained from the 
 * previous one by resampling
 * Block variables given the values of the variables in its Markov blanket.
 */
mc_gibbs_sample_arg0(M:Goal,M:Evidence0,S,Mix,Block,Arg,ValList):-
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  gibbs_sample_cycle(M:Evidence),
  empty_assoc(Values0),
  findall(Arg,M:Goal,La),
  numbervars(La),
  put_assoc(La,Values0,1,Values1),
  (Mix=0->
    Values2=Values1,
    S1 is S-1
  ;
    Mix1 is Mix-1,
    gibbs_sample_arg(Mix1,M:Goal,M:Evidence,Block,Arg, Values1,_Values),
    S1=S,
    Values2=Values0
  ),
  gibbs_sample_arg(S1,M:Goal,M:Evidence,Block,Arg, Values2,Values),
  erase_samples(M),
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList),
  maplist(erase,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).

/**
 * mc_gibbs_sample_arg0(:Query:atom,+Samples:int,+Mix:int,+Block:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query  a number of Samples times.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * It performs blocked Gibbs sampling: each sample is obtained from the previous one 
 * by resampling
 * Block variables given the values of the variables in its Markov blanket.
 */
mc_gibbs_sample_arg0(M:Goal,S,Mix,Block,Arg,ValList):-
  empty_assoc(Values0),
  findall(Arg,M:Goal,La),
  numbervars(La),
  put_assoc(La,Values0,1,Values1),
  (Mix=0->
    Values2=Values1,
    S1 is S-1
  ;
    Mix1 is Mix-1,
    gibbs_sample_arg(Mix1,M:Goal,Block,Arg, Values1,_Values),
    S1=S,
    Values2=Values0
  ),
  gibbs_sample_arg(S1,M:Goal,Block,Arg, Values2,Values),
  erase_samples(M),
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList).

gibbs_sample_arg(K,_Goals,_Block,_Arg,V,V):-
  K=<0,!.

gibbs_sample_arg(K0,M:Goal,Block, Arg,V0,V):-
  remove_samples(M,Block,LS),
  findall(Arg,M:Goal,La),
  numbervars(La),
  (get_assoc(La, V0, N)->
    N1 is N+1,
    put_assoc(La,V0,N1,V1)
  ;
    put_assoc(La,V0,1,V1)
  ),
  check_sampled(M,LS),
  K1 is K0-1,
  gibbs_sample_arg(K1,M:Goal,Block,Arg,V1,V).

/**
 * mc_mh_sample(:Query:atom,:Evidence:atom,+Samples:int,-Probability:float,+Options:list) is det
 *
 * The predicate samples Query  a number of Mix+Samples (Mix is set with the options, default value 0) times given that
 * Evidence
 * is true and returns
 * the number of Successes, of Failures and the
 * Probability (Successes/Samples).
 * The first Mix (that is set with the options, default value 0) samples are discarded (mixing time).
 * It performs Metropolis/Hastings sampling: between each sample, Lag (that is set with the options, default value 1) sampled
 * choices are forgotten and each sample is accepted with a certain probability.
 * If Query/Evidence are not ground, it considers them as existential queries.
 *
 * Options is a list of options, the following are recognised by mc_mh_sample/5:
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * lag(+Lag:int)
 *   lag between each sample, Lag sampled choices are forgotten, default value 1
 * * successes(-Successes:int)
 *   Number of succeses
 * * failures(-Failures:int)
 *   Number of failueres
 */
mc_mh_sample(M:Goal,M:Evidence,S,P,Options):-
  test_prism(M),
  option(lag(L),Options,1),
  option(mix(Mix),Options,0),
  option(successes(T),Options,_T),
  option(failures(F),Options,_F),
  mc_mh_sample(M:Goal,M:Evidence,S,Mix,L,T,F,P).

/**
 * mc_mh_sample(:Query:atom,:Evidence:atom,+Samples:int,-Probability:float) is det
 *
 * Equivalent to mc_mh_sample/5 with an empty option list.
 */
mc_mh_sample(M:Goal,M:Evidence,S,P):-
  mc_mh_sample(M:Goal,M:Evidence,S,P,[]).
  
/**
 * mc_mh_sample(:Query:atom,:Evidence:atom,+Samples:int,+Mix:int,+Lag:int,-Successes:int,-Failures:int,-Probability:float) is det
 *
 * The predicate samples Query  a number of Mix+Samples times given that
 * Evidence
 * is true and returns
 * the number of Successes, of Failures and the
 * Probability (Successes/Samples).
 * The first Mix samples are discarded (mixing time).
 * It performs Metropolis/Hastings sampling: between each sample, Lag sampled
 * choices are forgotten and each sample is accepted with a certain probability.
 * If Query/Evidence are not ground, it considers them as existential queries.
 */
mc_mh_sample(M:Goal,M:Evidence0,S,Mix,L,T,F,P):-
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  initial_sample_cycle(M:Evidence),!,
  copy_term(Goal,Goal1),
  (M:Goal1->
    Succ=1
  ;
    Succ=0
  ),
  count_samples(M,NC),
  Mix1 is Mix-1,
  save_samples_copy(M,Goal),
  mh_montecarlo(L,Mix1,NC,0, Succ,Succ,Succ1,M:Goal, M:Evidence, _NMix, _TMix),
  count_samples(M,NC1),
  mh_montecarlo(L,S,NC1,0, 0,Succ1,_Succ1, M:Goal, M:Evidence, _N, T),
  P is T / S,
  F is S - T,
  erase_samples(M),
  delete_samples_copy(M,Goal),
  maplist(erase,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).


gibbs_sample_cycle(M:G):-
  copy_term(G,G1),
  (M:G1->
    true
  ;
    erase_samples(M),
    restore_samples(M,G),
    gibbs_sample_cycle(M:G)
  ).


initial_sample_cycle(M:G):-
  copy_term(G,G1),
  (initial_sample(M:G1)->
    true
  ;
    erase_samples(M),
    initial_sample_cycle(M:G)
  ).

initial_sample(_M:true):-!.

initial_sample(M:(A~= B)):-!,
  add_arg(A,B,A1),
  initial_sample(M:A1).

initial_sample(M:msw(A,B)):-!,
  msw(M:A,B).

initial_sample(_M:(sample_head(R,VC,M,HL,NH))):-!,
  sample_head(R,VC,M,HL,NH).

initial_sample(_M:take_a_sample(R,VC,M,Distr,S)):-!,
  take_a_sample(R,VC,M,Distr,S).

initial_sample(M:(G1,G2)):-!,
  initial_sample(M:G1),
  initial_sample(M:G2).

initial_sample(M:(G1;G2)):-!,
  initial_sample(M:G1);
  initial_sample(M:G2).

initial_sample(M:(\+ G)):-!,
  \+ initial_sample(M:G).
%  initial_sample_neg(M:G).

initial_sample(M:findall(A,G,L)):-!,
  findall(A,initial_sample(M:G),L).

initial_sample(M:G):-
  builtin(G),!,
  M:call(G).

initial_sample(M:G):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  initial_sample(M:B).

initial_sample_neg(_M:true):-!,
  fail.

initial_sample_neg(_M:(sample_head(R,VC,M,HL,N))):-!,
  sample_head(R,VC,M,HL,NH),
  NH\=N.

initial_sample_neg(_M:take_a_sample(R,VC,M,Distr,S)):-!,
  take_a_sample(R,VC,M,Distr,S).


initial_sample_neg(M:(G1,G2)):-!,
  (initial_sample_neg(M:G1),!;
  initial_sample(M:G1),
  initial_sample_neg(M:G2)).

initial_sample_neg(M:(G1;G2)):-!,
  initial_sample_neg(M:G1),
  initial_sample_neg(M:G2).

initial_sample_neg(M:(\+ G)):-!,
  initial_sample(M:G).

initial_sample_neg(M:G):-
  builtin(G),!,
  \+ M:call(G).

initial_sample_neg(M:G):-
  findall(B,M:clause(G,B),L),
  initial_sample_neg_all(L,M).

initial_sample_neg_all([],_M).

initial_sample_neg_all([H|T],M):-
  initial_sample_neg(M:H),
  initial_sample_neg_all(T,M).

check(R,VC,M,N):-
  M:sampled(R,VC,N),!.

check(R,VC,M,N):-
  \+ M:sampled(R,VC,_N),
  assertz(M:sampled(R,VC,N)).

check_neg(R,VC,M,_LN,N):-
  M:sampled(R,VC,N1),!,
  N\=N1.

check_neg(R,VC,M,LN,N):-
  \+ M:sampled(R,VC,_N),
  member(N1,LN),
  N1\= N,
  assertz(M:sampled(R,VC,N1)).

listN(0,[]):-!.

listN(N,[N1|T]):-
  N1 is N-1,
  listN(N1,T).

/**
 * mc_sample_arg(:Query:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query a number of Samples times.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 *
 * Options is a list of options, the following are recognised by mc_sample_arg/5:
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with
 *   a bar for each possible value of L,
 *   the list of value of Arg for which Query succeeds in
 *   a world sampled at random.
 */
mc_sample_arg(M:Goal,S,Arg,ValList,Options):-
  empty_assoc(Values0),
  sample_arg(S,M:Goal,Arg, Values0,Values),
  erase_samples(M),
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).
/**
 * mc_sample_arg(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * Equivalent to mc_sample_arg/5 with an empty option list.
 */
mc_sample_arg(M:Goal,S,Arg,ValList):-
  mc_sample_arg(M:Goal,S,Arg,ValList,[]).

/**
 * mc_rejection_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query a number of Samples times given that
 * Evidence is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * Rejection sampling is performed.
 *
 * Options is a list of options, the following are recognised by mc_rejection_sample_arg/6:
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with
 *   a bar for each possible value of L,
 *   the list of value of Arg for which Query succeeds in
 *   a world sampled at random.
 */
mc_rejection_sample_arg(M:Goal,M:Evidence0,S,Arg,ValList,Options):-
  test_prism(M),
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  empty_assoc(Values0),
  rejection_sample_arg(S,M:Goal,M:Evidence,Arg, Values0,Values),
  erase_samples(M),
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList),
  maplist(erase,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).

/**
 * mc_rejection_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * Equivalent to mc_rejection_sample_arg/6 with an empty option list.
 */
mc_rejection_sample_arg(M:Goal,M:Evidence0,S,Arg,ValList):-
  mc_rejection_sample_arg(M:Goal,M:Evidence0,S,Arg,ValList,[]).

/**
 * mc_mh_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * The first Mix (that is set with the options, default value 0) samples are discarded (mixing time).
 * It performs Metropolis/Hastings sampling: between each sample, Lag (that is set with the options, default value 1) sampled
 * choices are forgotten and each sample is accepted with a certain probability.
 *
 * Options is a list of options, the following are recognised by mc_mh_sample_arg/6:
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * lag(+Lag:int)
 *   lag between each sample, Lag sampled choices are forgotten, default value 1
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with
 *   a bar for each possible value of L,
 *   the list of value of Arg for which Query succeeds in
 *   a world sampled at random.
 */
mc_mh_sample_arg(M:Goal,M:Evidence,S,Arg,ValList,Options):-
  test_prism(M),
  option(mix(Mix),Options,0),
  option(lag(L),Options,1),
  option(bar(Chart),Options,no),
  mc_mh_sample_arg0(M:Goal,M:Evidence,S,Mix,L,Arg,ValList),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).

/**
 * mc_mh_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * Equivalent to mc_mh_sample_arg/6 with an empty option list.
 */
mc_mh_sample_arg(M:Goal,M:Evidence,S,Arg,ValList):-
  mc_mh_sample_arg(M:Goal,M:Evidence,S,Arg,ValList,[]).
  
/**
 * mc_mh_sample_arg0(:Query:atom,:Evidence:atom,+Samples:int,+Mix:int,+Lag:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * It performs Metropolis/Hastings sampling: between each sample, Lag sampled
 * choices are forgotten and each sample is accepted with a certain probability.
 */
mc_mh_sample_arg0(M:Goal,M:Evidence0,S,Mix,L,Arg,ValList):-
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  initial_sample_cycle(M:Evidence),!,
  empty_assoc(Values0),
  findall(Arg,M:Goal,La),
  numbervars(La),
  put_assoc(La,Values0,1,Values1),
  count_samples(M,NC),
  Mix1 is Mix-1,
  save_samples_copy(M,Goal),
  mh_sample_arg(L,Mix1,NC,M:Goal,M:Evidence,Arg, La,La1,Values1,_Values),
  count_samples(M,NC1),
  mh_sample_arg(L,S,NC1,M:Goal,M:Evidence,Arg, La1,_La,Values0,Values),
  erase_samples(M),
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList),
  delete_samples_copy(M,Goal),
  maplist(erase,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).



mh_sample_arg(_L,K,_NC0,_Goals,_Ev,_Arg,AP,AP,V,V):-
  K=<0,!.

mh_sample_arg(L,K0,NC0,M:Goal, M:Evidence, Arg,AP0,AP,V0,V):-
  resample(M,L),
  copy_term(Evidence,Ev1),
  (M:Ev1->
    findall(Arg,M:Goal,La),
    numbervars(La),
    count_samples(M,NC1),
    (accept(NC0,NC1)->
     (get_assoc(La, V0, N)->
        N1 is N+1,
        put_assoc(La,V0,N1,V1)
      ;
        put_assoc(La,V0,1,V1)
      ),
      delete_samples_copy(M,Goal),
      save_samples_copy(M,Goal),
      K1 is K0-1,
      AP1 = La
    ;
      (get_assoc(AP0, V0, N)->
        N1 is N+1,
        put_assoc(AP0,V0,N1,V1)
      ;
        put_assoc(AP0,V0,1,V1)
      ),
      K1 is K0-1,
      AP1=AP0,
      erase_samples(M),
      restore_samples(M,Goal)
    )
  ;
    (get_assoc(AP0, V0, N)->
      N1 is N+1,
      put_assoc(AP0,V0,N1,V1)
    ;
      put_assoc(AP0,V0,1,V1)
    ),
    K1 is K0-1,
    NC1 = NC0,
    AP1=AP0,
    erase_samples(M),
    restore_samples(M,Goal)
  ),
  mh_sample_arg(L,K1,NC1,M:Goal,M:Evidence,Arg,AP1,AP,V1,V).


rejection_sample_arg(0,_Goals,_Ev,_Arg,V,V):-!.

rejection_sample_arg(K1, M:Goals,M:Ev,Arg,V0,V):-
  erase_samples(M),
  copy_term(Ev,Ev1),
  (M:Ev1->
    copy_term((Goals,Arg),(Goals1,Arg1)),
    findall(Arg1,M:Goals1,L),
    numbervars(L),
    (get_assoc(L, V0, N)->
      N1 is N+1,
      put_assoc(L,V0,N1,V1)
    ;
      put_assoc(L,V0,1,V1)
    ),
    K2 is K1-1
  ;
    V1=V0,
    K2=K1
  ),
  rejection_sample_arg(K2,M:Goals,M:Ev,Arg,V1,V).

sample_arg(0,_Goals,_Arg,V,V):-!.

sample_arg(K1, M:Goals,Arg,V0,V):-
  erase_samples(M),
  copy_term((Goals,Arg),(Goals1,Arg1)),
  findall(Arg1,M:Goals1,L),
  numbervars(L),
  (get_assoc(L, V0, N)->
    N1 is N+1,
    put_assoc(L,V0,N1,V1)
  ;
    put_assoc(L,V0,1,V1)
  ),
  K2 is K1-1,
  sample_arg(K2,M:Goals,Arg,V1,V).

/**
 * mc_particle_sample(:Query:atom,:Evidence:list,+Samples:int,-Prob:float) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true. Evidence is a list of goals.
 * The predicate returns in Prob the probability that the query is true.
 * It performs particle filtering with likelihood weighting:
 * each sample is weighted by the
 * likelihood of an element of the Evidence list and constitutes a particle.
 * After weighting, particles are resampled and the next element of Evidence
 * is considered.
 */
mc_particle_sample(M:Goal,M:Evidence,S,P):-
  M:asserta(('$goal'(1):-Goal,!),Ref1),
  M:asserta('$goal'(0),Ref0),
  mc_particle_sample_arg(M:'$goal'(A),M:Evidence,S,A,ValList),
  foldl(agg_val,ValList,0,Sum),
  foldl(value_cont_single,ValList,0,SumTrue),
  P is SumTrue/Sum,
  erase(Ref1),
  erase(Ref0).

/**
 * mc_particle_sample_arg(:Query:atom,+Evidence:list,+Samples:int,?Arg:term,-Values:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * It performs particle filtering with likelihood weighting:
 * each sample is weighted by the
 * likelihood of an element of the Evidence list and constitutes a particle.
 * After weighting, particles are resampled and the next element of Evidence
 * is considered.
 * Arg should be a variable in Query. Evidence is a list of goals.
 * Query can be either a single goal or a list of goals.
 * When Query is a single goal, the predicate returns in Values
 * a list of couples V-W where V is a value of Arg for which Query succeeds in
 * a particle in the last set of particles and W is the weight of the particle.
 * For each element of Evidence, the particles are obtained by sampling Query
 * in each current particle and weighting the particle by the likelihood
 * of the evidence element.
 * When Query is a list of goals, Arg is a list of variables, one for
 * each query of Query and Arg and Query must have the same length of Evidence.
 * Values is then list of the same length of Evidence and each of its
 * elements is a list of couples V-W where
 * V is a value of the corresponding element of Arg for which the corresponding
 * element of Query succeeds in a particle and W is the weight of the particle.
 * For each element of Evidence, the particles are obtained by sampling the
 * corresponding element of Query in each current particle and weighting
 * the particle by the likelihood of the evidence element.
 */
mc_particle_sample_arg(M:Goal,M:Evidence,S,Arg,[V0|ValList]):-
  test_prism(M),
  Goal=[G1|GR],!,
  Evidence=[Ev1|EvR],
  Arg=[A1|AR],
  particle_sample_first_gl(0,S,M:G1,M:Ev1,A1,V0),
  particle_sample_arg_gl(M:GR,M:EvR,AR,1,S,ValList),
  retractall(M:mem(_,_,_,_)),
  retractall(M:mem(_,_,_,_,_)),
  retractall(M:value_particle(_,_,_)),
  retractall(M:weight_particle(_,_,_)).

mc_particle_sample_arg(M:Goal,M:Evidence,S,Arg,ValList):-
  Evidence=[Ev1|EvR],
  particle_sample_first(0,S,M:Goal,M:Ev1,Arg),
  particle_sample_arg(M:EvR,M:Goal,1,S,ValList0),
  foldl(agg_val,ValList0,0,Sum),
  Norm is S/Sum,
  retractall(M:mem(_,_,_,_)),
  retractall(M:mem(_,_,_,_,_)),
  retractall(M:value_particle(_,_,_)),
  retractall(M:weight_particle(_,_,_)),
  maplist(norm(Norm),ValList0,ValList).

/**
 * mc_particle_expectation(:Query:atom,:Evidence:atom,+N:int,?Arg:var,-Exp:float) is det
 *
 * The predicate computes the expected value of Arg in Query given Evidence by
 * particle filtering.
 * It uses N particle and sums up the weighted value of Arg for
 * each particle. The overall sum is divided by the sum of weights to give Exp.
 * Arg should be a variable in Query.
 */
mc_particle_expectation(M:Goal,M:Evidence,S,Arg,E):-
  mc_particle_sample_arg(M:Goal,M:Evidence,S,Arg,ValList),
  average(ValList,E).

particle_sample_arg_gl(M:[],M:[],[],I,_S,[]):- !,
  retractall(M:mem(I,_,_,_,_)).

particle_sample_arg_gl(M:[HG|TG],M:[HE|TE],[HA|TA],I,S,[HV|TV]):-
  I1 is I+1,
  resample_gl(M,I,I1,S),
  retractall(M:value_particle(I,_,_)),
  retractall(M:weight_particle(I,_,_)),
  particle_sample_gl(0,S,M:HG,M:HE,HA,I1,HV),
  particle_sample_arg_gl(M:TG,M:TE,TA,I1,S,TV).

resample_gl(M,I,I1,S):-
  get_values(M,I,V0),
  foldl(agg_val,V0,0,Sum),
  Norm is 1.0/Sum,
  maplist(norm(Norm),V0,V1),
  numlist(1,S,L),
  maplist(weight_to_prob,L,V1,V2),
  W is 1.0/S,
  take_samples_gl(M,0,S,I,I1,W,V2),
  retractall(M:mem(I,_,_,_,_)).

weight_to_prob(I,_V-W,I:W).

take_samples_gl(_M,S,S,_I,_I1,_W,_V):-!.

take_samples_gl(M,S0,S,I,I1,W,V):-
  S1 is S0+1,
  discrete(V,_M,SInd),
  restore_samples(M,I,SInd),
  save_samples_tab(M,I1,S1),
  take_samples_gl(M,S1,S,I,I1,W,V).

particle_sample_gl(K,K,M:_G,_Ev,_A,I,L):-!,
  get_values(M,I,L0),
  foldl(agg_val,L0,0,Sum),
  Norm is K/Sum,
  maplist(norm(Norm),L0,L).


particle_sample_gl(K0,S,M:Goal,M:Evidence,Arg,I,L):-
  K1 is K0+1,
  restore_samples(M,I,K1),
  copy_term((Goal,Arg),(Goal1,Arg1)),
  lw_sample_cycle(M:Goal1),
  copy_term(Evidence,Ev1),
  lw_sample_weight_cycle(M:Ev1,W),
  save_samples_tab(M,I,K1),
  assert(M:weight_particle(I,K1,W)),
  assert(M:value_particle(I,K1,Arg1)),
  particle_sample_gl(K1,S,M:Goal,M:Evidence,Arg,I,L).

particle_sample_first_gl(K,K,M:_Goals,_Ev,_Arg,L):-!,
  get_values(M,1,L0),
  foldl(agg_val,L0,0,Sum),
  Norm is K/Sum,
  maplist(norm(Norm),L0,L).


particle_sample_first_gl(K0,S,M:Goal, M:Evidence, Arg,V):-
  K1 is K0+1,
  copy_term((Goal,Arg),(Goal1,Arg1)),
  lw_sample_cycle(M:Goal1),
  copy_term(Evidence,Ev1),
  lw_sample_weight_cycle(M:Ev1,W),
  save_samples_tab(M,1,K1),
  assert(M:weight_particle(1,K1,W)),
  assert(M:value_particle(1,K1,Arg1)),
  particle_sample_first_gl(K1,S,M:Goal,M:Evidence,Arg,V).


particle_sample_arg(M:[],_Goal,I,_S,L):-!,
  get_values(M,I,L).

particle_sample_arg(M:[HE|TE],M:Goal,I,S,L):-
  I1 is I+1,
  resample(M,I,I1,S),
  retractall(M:value_particle(I,_,_)),
  retractall(M:weight_particle(I,_,_)),
  particle_sample(0,S, M:HE, I1),
  retractall(M:mem(I,_,_,_,_)),
  particle_sample_arg(M:TE,M:Goal,I1,S,L).

resample(M,I,I1,S):-
  get_values(M,I,V0),
  foldl(agg_val,V0,0,Sum),
  Norm is 1.0/Sum,
  maplist(norm(Norm),V0,V1),
  numlist(1,S,L),
  maplist(weight_to_prob,L,V1,V2),
  W is 1.0/S,
  take_samples(M,0,S,I,I1,W,V2).


take_samples(_M,S,S,_I,_I1,_W,_V):-!.

take_samples(M,S0,S,I,I1,W,V):-
  S1 is S0+1,
  discrete(V,_M,SInd),
  restore_samples(M,I,SInd),
  save_samples_tab(M,I1,S1),
  M:value_particle(I,SInd,Arg),!,
  assert(M:value_particle(I1,S1,Arg)),
  take_samples(M,S1,S,I,I1,W,V).


particle_sample(K,K,_Ev,_I):-!.

particle_sample(K0,S,M:Evidence,I):-
  K1 is K0+1,
  restore_samples(M,I,K1),
  copy_term(Evidence,Ev1),
  lw_sample_weight_cycle(M:Ev1,W),
  save_samples_tab(M,I,K1),
  assert(M:weight_particle(I,K1,W)),
  particle_sample(K1,S,M:Evidence,I).

particle_sample_first(K,K,_Goals,_Ev,_Arg):-!.

particle_sample_first(K0,S,M:Goal, M:Evidence, Arg):-
  K1 is K0+1,
  copy_term((Goal,Arg),(Goal1,Arg1)),
  lw_sample_cycle(M:Goal1),
  copy_term(Evidence,Ev1),
  lw_sample_weight_cycle(M:Ev1,W),
  save_samples_tab(M,1,K1),
  assert(M:weight_particle(1,K1,W)),
  assert(M:value_particle(1,K1,Arg1)),
  particle_sample_first(K1,S,M:Goal,M:Evidence,Arg).

get_values(M,I,V):-
  findall(A-W,(M:value_particle(I,S,A),M:weight_particle(I,S,W)),V).

/**
 * mc_lw_sample(:Query:atom,:Evidence:atom,+Samples:int,-Prob:float) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * The predicate returns in Prob the probability that the query is true.
 * It performs likelihood weighting: each sample is weighted by the
 * likelihood of evidence in the sample.
 */
mc_lw_sample(M:Goal,M:Evidence0,S,P):-
  test_prism(M),
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  erase_samples(M),
  lw_sample_bool(S,M:Goal,M:Evidence,ValList),
  foldl(agg_val,ValList,0,Sum),
  foldl(value_cont_single,ValList,0,SumTrue),
  P is SumTrue/Sum,
  maplist(erase,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).


value_cont_single(H-W,S,S+H*W).


/**
 * mc_lw_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples V-W where
 * V is a value of Arg for which Query succeeds in
 * a world sampled at random and W is the weight of the sample.
 * It performs likelihood weighting: each sample is weighted by the
 * likelihood of evidence in the sample.
 */
mc_lw_sample_arg(M:Goal,M:Evidence0,S,Arg,ValList):-
  test_prism(M),
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  lw_sample_arg(S,M:Goal,M:Evidence,Arg,ValList0),
  foldl(agg_val,ValList0,0,Sum),
  Norm is S/Sum,
  maplist(norm(Norm),ValList0,ValList),
  maplist(erase,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).

/**
 * mc_lw_sample_arg_log(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples V-W where
 * V is a value of Arg for which Query succeeds in
 * a world sampled at random and W is the natural logarithm of the weight of \
 * the sample.
 * It performs likelihood weighting: each sample is weighted by the
 * likelihood of evidence in the sample.
 * It differs from mc_lw_sample_arg/5 because the natural logarithm of the
 * weight is returned, useful when the evidence is very unlikely.
 */
mc_lw_sample_arg_log(M:Goal,M:Evidence0,S,Arg,ValList):-
  test_prism(M),
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  lw_sample_arg_log(S,M:Goal,M:Evidence,Arg,ValList),
  maplist(erase,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).

/**
 * mc_lw_expectation(:Query:atom,:Evidence:atom,+N:int,?Arg:var,-Exp:float) is det
 *
 * The predicate computes the expected value of Arg in Query given Evidence by
 * likelihood weighting.
 * It takes N samples of Query and sums up the weighted value of Arg for
 * each sample. The overall sum is divided by the sum of weights to give Exp.
 * Arg should be a variable in Query.
 */
mc_lw_expectation(M:Goal,M:Evidence,S,Arg,E):-
  mc_lw_sample_arg(M:Goal,M:Evidence,S,Arg,ValList),
  average(ValList,E).



norm(NF,V-W,V-W1):-
  W1 is W*NF.

lw_sample_bool(0,_Goals,_Ev,[]):-!.

lw_sample_bool(K0,M:Goal, M:Evidence, [S-W|V]):-
  copy_term(Goal,Goal1),
  (lw_sample(M:Goal1)->
    S=1
  ;
    S=0
  ),
  copy_term(Evidence,Ev1),
  (lw_sample_weight(M:Ev1,1,W0)->
    W=W0
  ;
    W=0
  ),
  K1 is K0-1,
  erase_samples(M),
  lw_sample_bool(K1,M:Goal,M:Evidence,V).

lw_sample_arg(0,_Goals,_Ev,_Arg,[]):-!.

lw_sample_arg(K0,M:Goal, M:Evidence, Arg,[Arg1-W|V]):-
  copy_term((Goal,Arg),(Goal1,Arg1)),
  lw_sample_cycle(M:Goal1),
  copy_term(Evidence,Ev1),
  lw_sample_weight_cycle(M:Ev1,W),
  K1 is K0-1,
  erase_samples(M),
  lw_sample_arg(K1,M:Goal,M:Evidence,Arg,V).

lw_sample_cycle(M:G):-
  (lw_sample(M:G)->
    true
  ;
    erase_samples(M),
    lw_sample_cycle(M:G)
  ).

lw_sample_arg_log(0,_Goals,_Ev,_Arg,[]):-!.

lw_sample_arg_log(K0,M:Goal, M:Evidence, Arg,[Arg1-W|V]):-
  copy_term((Goal,Arg),(Goal1,Arg1)),
  lw_sample_cycle(M:Goal1),
  copy_term(Evidence,Ev1),
  lw_sample_logweight_cycle(M:Ev1,W),
  K1 is K0-1,
  erase_samples(M),
  lw_sample_arg_log(K1,M:Goal,M:Evidence,Arg,V).


lw_sample(_M:true):-!.

lw_sample(M:A~=B):-!,
  add_arg(A,B,A1),
  lw_sample(M:A1).

lw_sample(M:msw(A,B)):-!,
  msw(M:A,B).

lw_sample(M:G):-
  G=..[call,P|A],!,
  G1=..[P|A],
  lw_sample(M:G1).

lw_sample(_M:(sample_head(R,VC,M,HL,N))):-!,
  sample_head(R,VC,M,HL,N).

lw_sample(_M:take_a_sample(R,VC,M,Distr,S)):-!,
  take_a_sample(R,VC,M,Distr,S).

lw_sample(M:(G1,G2)):-!,
  lw_sample(M:G1),
  lw_sample(M:G2).

lw_sample(M:(G1;G2)):-!,
  lw_sample(M:G1);
  lw_sample(M:G2).

lw_sample(M:(\+ G)):-!,
  \+ lw_sample(M:G).

lw_sample(M:G):-
  builtin(G),!,
  M:call(G).

lw_sample(M:G):-
  \+ \+ M:sampled_g(G),!,
  M:sampled_g(G).

lw_sample(M:G):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  lw_sample(M:B),
  assert(M:sampled(r,G,1)).


lw_sample_weight_cycle(M:G,W):-
  (lw_sample_weight(M:G,1,W)->
    true
  ;
    erase_samples(M),
    lw_sample_weight_cycle(M:G,W)
  ).

lw_sample_weight(_M:true,W,W):-!.

lw_sample_weight(M:A~= B,W0,W):-!,
  add_arg(A,B,A1),
  lw_sample_weight(M:A1,W0,W).

lw_sample_weight(M:G,W0,W):-
  G=..[call,P|A],!,
  G1=..[P|A],
  lw_sample_weight(M:G1,W0,W).

lw_sample_weight(M:msw(A,B),W0,W):-!,
  (var(B)->
    msw(M:A,B),
    W=W0
  ;
    msw_weight(M:A,B,W1),
    W is W0*W1
  ).

lw_sample_weight(_M:(sample_head(R,VC,M,HL,N)),W0,W):-!,
  % sample_head(R,VC,M,HL,N0),
  % N=N0,
  check(R,VC,M,N),
%  W=W0.
  nth0(N,HL,_:P),
  W is W0*P.


lw_sample_weight(_M:take_a_sample(R,VC,M,Distr,S),W0,W):-!,
  (var(S)->
    take_a_sample(R,VC,M,Distr,S),
    W=W0
  ;
    (is_discrete(M,Distr)->
      check(R,VC,M,S)
    ;
      true
    ),
    call(Distr,M,S,D),
    W is W0*D
   ).

lw_sample_weight(M:(G1,G2),W0,W):-!,
  lw_sample_weight(M:G1,W0,W1),
  lw_sample_weight(M:G2,W1,W).

lw_sample_weight(M:(G1;G2),W0,W):-!,
  lw_sample_weight(M:G1,W0,W);
  lw_sample_weight(M:G2,W0,W).

lw_sample_weight(M:(\+ G),W0,W):-!,
  lw_sample_weight(M:G,1,W1),
  W is W0*(1-W1).

lw_sample_weight(M:G,W,W):-
  builtin(G),!,
  M:call(G).

lw_sample_weight(M:G,W0,W):-
  \+ \+ M:sampled_g(G,_W1),!,
  M:sampled_g(G,W1),
  W is W0*W1.

lw_sample_weight(M:G,W0,W):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  lw_sample_weight(M:B,1,W1),
  assert(M:sampled(rw,G,W1)),
  W is W0*W1.


lw_sample_logweight_cycle(M:G,W):-
  (lw_sample_logweight(M:G,0,W)->
    true
  ;
    erase_samples(M),
    lw_sample_logweight_cycle(M:G,W)
  ).


lw_sample_logweight(_M:true,W,W):-!.

lw_sample_logweight(M:A~= B,W0,W):-!,
  add_arg(A,B,A1),
  lw_sample_logweight(M:A1,W0,W).

lw_sample_logweight(M:msw(A,B),W0,W):-!,
  (var(B)->
    msw(M:A,B),
    W=W0
  ;
    msw_weight(M:A,B,W1),
    W is W0+log(W1)
  ).

lw_sample_logweight(_M:(sample_head(R,VC,M,HL,N)),W0,W):-!,
  sample_head(R,VC,M,HL,N0),
  N=N0,
  check(R,VC,M,N),
 % W=W0.
  nth0(N,HL,_:P),
  W is W0+log(P).

lw_sample_logweight(_M:take_a_sample(R,VC,M,Distr,S),W0,W):-!,
  (var(S)->
    take_a_sample(R,VC,M,Distr,S),
    W=W0
  ;
    (is_discrete(M,Distr)->
      check(R,VC,M,S)
    ;
      true
    ),
    call(Distr,M,S,D),
    W is W0+log(D)
   ).

lw_sample_logweight(M:(G1,G2),W0,W):-!,
  lw_sample_logweight(M:G1,W0,W1),
  lw_sample_logweight(M:G2,W1,W).

lw_sample_logweight(M:(G1;G2),W0,W):-!,
  lw_sample_logweight(M:G1,W0,W);
  lw_sample_logweight(M:G2,W0,W).

lw_sample_logweight(M:(\+ G),W0,W):-!,
  lw_sample_logweight(M:G,0,W1),
  W is W0-W1.


lw_sample_logweight(M:G,W,W):-
  builtin(G),!,
  M:call(G).

lw_sample_logweight(M:G,W0,W):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  lw_sample_logweight(M:B,W0,W).

is_var(S):-
  var(S),!.

is_var(S):-
  maplist(var,S).

/**
 * mc_sample_arg_first(:Query:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query a number of Samples times.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples V-N where
 * V is the value of Arg returned as the first answer by Query in
 * a world sampled at random and N is the number of samples
 * returning that value.
 * V is failure if the query fails.
 *
 * Options is a list of options, the following are recognised by mc_sample_arg_first/5:
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with
 *   with a bar for each value of Arg returned as a first answer by Query in
 *   a world sampled at random.
 *   The size of the bar is the number of samples that returned that value.
 */
mc_sample_arg_first(M:Goal,S,Arg,ValList,Options):-
  empty_assoc(Values0),
  sample_arg_first(S,M:Goal,Arg, Values0,Values),
  erase_samples(M),
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).

/**
 * mc_sample_arg_first(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * Equivalent to mc_sample_arg_first/5 with an empty option list.
 */
mc_sample_arg_first(M:Goal,S,Arg,ValList):-
  mc_sample_arg_first(M:Goal,S,Arg,ValList,[]).

sample_arg_first(0,_Goals,_Arg,V,V):-!.

sample_arg_first(K1, M:Goals,Arg,V0,V):-
  erase_samples(M),
  copy_term((Goals,Arg),(Goals1,Arg1)),
  (M:Goals1->
    numbervars(Arg1),
    Val=Arg1
  ;
    Val=failure
  ),
  (get_assoc(Val, V0, N)->
    N1 is N+1,
    put_assoc(Val,V0,N1,V1)
  ;
    put_assoc(Val,V0,1,V1)
  ),
  K2 is K1-1,
  sample_arg_first(K2,M:Goals,Arg,V1,V).

/**
 * mc_sample_arg_one(:Query:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query a number of Samples times.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples V-N where
 * V is a value of Arg sampled with uniform probability from those returned
 * by Query in a world sampled at random and N is the number of samples
 * returning that value.
 * V is failure if the query fails.
 *
 * Options is a list of options, the following are recognised by mc_sample_arg_one/5:
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart 
 *   with a bar for each value of Arg returned by sampling with uniform
 *   probability one answer from those returned by Query in a world sampled
 *   at random.
 *   The size of the bar is the number of samples.
 */
mc_sample_arg_one(M:Goal,S,Arg,ValList,Options):-
  empty_assoc(Values0),
  sample_arg_one(S,M:Goal,Arg, Values0,Values),
  erase_samples(M),
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).

/**
 * mc_sample_arg_one(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * Equivalent to mc_sample_arg_one/5 with an empty option list.
 */
mc_sample_arg_one(M:Goal,S,Arg,ValList):-
  mc_sample_arg_one(M:Goal,S,Arg,ValList,[]).

sample_arg_one(0,_Goals,_Arg,V,V):-!.

sample_arg_one(K1, M:Goals,Arg,V0,V):-
  erase_samples(M),
  copy_term((Goals,Arg),(Goals1,Arg1)),
  findall(Arg1,M:Goals1,L),
  numbervars(L),
  sample_one(L,Val),
  (get_assoc(Val, V0, N)->
    N1 is N+1,
    put_assoc(Val,V0,N1,V1)
  ;
    put_assoc(Val,V0,1,V1)
  ),
  K2 is K1-1,
  sample_arg_one(K2,M:Goals,Arg,V1,V).

sample_one([],failure):-!.

sample_one(List,El):-
  length(List,L),
  random(0,L,Pos),
  nth0(Pos,List,El).

sample_one_back([],_):-!,
  fail.

sample_one_back(List,El):-
  length(List,L),
  random(0,L,Pos),
  nth0(Pos,List,El0,Rest),
  sample_backtracking(Rest,El0,El).

sample_backtracking([],El,El):-!.

sample_backtracking(_,El,El).

sample_backtracking(L,_El,El):-
  sample_one_back(L,El).
/**
 * mc_sample_arg_raw(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query a number of Samples times.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of values
 * of Arg returned as the first answer by Query in
 * a world sampled at random.
 * The value is failure if the query fails.
 */
mc_sample_arg_raw(M:Goal,S,Arg,Values):-
  sample_arg_raw(S,M:Goal,Arg,Values),
  erase_samples(M).

sample_arg_raw(0,_Goals,_Arg,[]):-!.

sample_arg_raw(K1, M:Goals,Arg,[Val|V]):-
  erase_samples(M),
  copy_term((Goals,Arg),(Goals1,Arg1)),
  (M:Goals1->
    numbervars(Arg1),
    Val=Arg1
  ;
    Val=failure
  ),
  K2 is K1-1,
  sample_arg_raw(K2,M:Goals,Arg,V).


/**
 * mc_expectation(:Query:atom,+N:int,?Arg:var,-Exp:float) is det
 *
 * The predicate computes the expected value of Arg in Query by
 * sampling.
 * It takes N samples of Query and sums up the value of Arg for
 * each sample. The overall sum is divided by N to give Exp.
 * Arg should be a variable in Query.
 */
mc_expectation(M:Goal,S,Arg,E):-
  sample_val(S,M:Goal,Arg, 0,Sum),
  erase_samples(M),
  E is Sum/S.

/**
 * mc_gibbs_expectation(:Query:atom,+N:int,?Arg:var,-Exp:float,+Options:list) is det
 *
 * The predicate computes the expected value of Arg in Query by
 * sampling.
 * It takes N samples of Query and sums up the value of Arg for
 * each sample. The overall sum is divided by N to give Exp.
 * Arg should be a variable in Query.
 * Options is a list of options, the following are recognised by mc_mh_sample_arg/6:
 * * block(+Block:int)
 *   Perform blocked Gibbs: Block variables are sampled together, default value 1
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 */
mc_gibbs_expectation(M:Goal,S,Arg,E,Options):-
  mc_gibbs_sample_arg(M:Goal,S,Arg,ValList,Options),
  average(ValList,[E]),
  erase_samples(M).

/**
 * mc_gibbs_expectation(:Query:atom,+N:int,?Arg:var,-Exp:float) is det
 *
 * Equivalent to mc_gibbs_expectation/5 with an empty option list.
 */
mc_gibbs_expectation(M:Goal,S,Arg,E):-
  mc_gibbs_expectation(M:Goal,S,Arg,E,[]).


/**
 * mc_rejection_expectation(:Query:atom,:Evidence:atom,+N:int,?Arg:var,-Exp:float) is det
 *
 * The predicate computes the expected value of Arg in Query by
 * sampling.
 * It takes N samples of Query and sums up the value of Arg for
 * each sample. The overall sum is divided by N to give Exp.
 * Arg should be a variable in Query.
 */
mc_rejection_expectation(M:Goal,M:Evidence,S,Arg,E):-
  mc_rejection_sample_arg(M:Goal,M:Evidence,S,Arg,ValList,[]),
  average(ValList,[E]),
  erase_samples(M).

/**
 * mc_gibbs_expectation(:Query:atom,:Evidence:atom,+N:int,?Arg:var,-Exp:float,+Options:list) is det
 *
 * The predicate computes the expected value of Arg in Query by
 * Gibbs sampling.
 * It takes N samples of Query and sums up the value of Arg for
 * each sample. The overall sum is divided by N to give Exp.
 * Arg should be a variable in Query.
 *
 * Options is a list of options, the following are recognised by mc_mh_expectation/6:
 * * block(+Block:int)
 *   Perform blocked Gibbs: Block variables are sampled together, default value 1
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 */
mc_gibbs_expectation(M:Goal,M:Evidence,S,Arg,E,Options):-
  mc_gibbs_sample_arg(M:Goal,M:Evidence,S,Arg,ValList,Options),
  average(ValList,[E]),
  erase_samples(M).

/**
 * mc_mh_expectation(:Query:atom,:Evidence:atom,+N:int,?Arg:var,-Exp:float,+Options:list) is det
 *
 * The predicate computes the expected value of Arg in Query by
 * Metropolis Hastings sampling.
 * It takes N samples of Query and sums up the value of Arg for
 * each sample. The overall sum is divided by N to give Exp.
 * Arg should be a variable in Query.
 *
 * Options is a list of options, the following are recognised by mc_mh_expectation/6:
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * lag(+Lag:int)
 *   lag between each sample, Lag sampled choices are forgotten, default value 1
 */
mc_mh_expectation(M:Goal,M:Evidence,S,Arg,E,Options):-
  mc_mh_sample_arg(M:Goal,M:Evidence,S,Arg,ValList,Options),
  average(ValList,[E]),
  erase_samples(M).

/**
 * mc_mh_expectation(:Query:atom,:Evidence:atom,+N:int,?Arg:var,-Exp:float) is det
 *
 * Equivalent to mc_mh_expectation/6 with an empty option list.
 */
mc_mh_expectation(M:Goal,M:Evidence,S,Arg,E):-
  mc_mh_expectation(M:Goal,M:Evidence,S,Arg,E,[]).

value_cont([]-_,0):-!.

value_cont([H|_T]-N,S,S+N*H).

sample_val(0,_Goals,_Arg,Sum,Sum):-!.

sample_val(K1, M:Goals,Arg,Sum0,Sum):-
  erase_samples(M),
  copy_term((Goals,Arg),(Goals1,Arg1)),
  (M:Goals1->
    Sum1 is Sum0+Arg1
  ;
    Sum1=Sum
  ),
  K2 is K1-1,
  sample_val(K2,M:Goals,Arg,Sum1,Sum).


get_next_rule_number(PName,R):-
  retract(PName:rule_n(R)),
  R1 is R+1,
  assert(PName:rule_n(R1)).


assert_all([],_M,[]).

assert_all([H|T],M,[HRef|TRef]):-
  assertz(M:H,HRef),
  assert_all(T,M,TRef).


retract_all([]):-!.

retract_all([H|T]):-
  erase(H),
  retract_all(T).



add_mod_arg(A,_Module,A1):-
  A=..[P|Args],
  A1=..[P|Args].
/**
 * sample_head(+R:int,+Variables:list,+M:module,+HeadList:list,-HeadNumber:int) is det
 *
 * samples a head from rule R instantiated as indicated by Variables (list of
 * constants, one per variable. HeadList contains the head as a list.
 * HeadNumber is the number of the sample head.
 * Internal predicates used by the transformed input program
 */
sample_head(R,VC,M,_HeadList,N):-
  M:sampled(R,VC,NH),!,
  N=NH.

sample_head(R,VC,M,HeadList,N):-
  sample(HeadList,NH),
  assertz(M:sampled(R,VC,NH)),
  N=NH.

sample(HeadList, HeadId) :-
  random(Prob),
  sample(HeadList, 0, 0, Prob, HeadId), !.

sample([_HeadTerm:HeadProb|Tail], Index, Prev, Prob, HeadId) :-
	Succ is Index + 1,
	Next is Prev + HeadProb,
	(Prob =< Next ->
		HeadId = Index;
		sample(Tail, Succ, Next, Prob, HeadId)).

/**
 * uniform_dens(+Lower:float,+Upper:float,-S:float) is det
 *
 * Returns in S a sample from a distribution uniform in (Lower,Upper)
 */
uniform_dens(L,U,_M,S):-
  number(L),
  random(V),
  S is L+V*(U-L).

uniform_dens(L,U,_M,_S,D):-
  D is 1/(U-L).

/**
 * uniform(+Values:list,-S:float) is det
 *
 * Returns in S a value from Values chosen uniformly
 */
uniform(Values,M,S):-
  length(Values,Len),Prob is 1.0/Len,
  maplist(add_prob(Prob),Values,Dist),
  discrete(Dist,M,S).


uniform(Values,_M,_S,D):-
  length(Values,L),
  D is 1.0/L.

/**
 * take_a_sample(+R:int,+VC:list,+M:module,+Distr:term,-S:term) is det
 *
 * Returns in S a sample for a random variable with distribution Distr
 * associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
take_a_sample(R,VC,M,_Distr,S):-
  M:sampled(R,VC,S0),!,
  S=S0.

take_a_sample(R,VC,M,Distr,S):-
  call(Distr,M,S0),
  assertz(M:sampled(R,VC,S0)),
  S=S0.

/**
 * gaussian(+Mean:float,+Variance:float,-S:float) is det
 *
 * samples a value from a Gaussian with mean Mean and variance
 * Variance and returns it in S
 */
gaussian(Mean,Variance,_M,S):-
  number(Mean),!,
  random(U1),
  random(U2),
  R is sqrt(-2*log(U1)),
  Theta is 2*pi*U2,
  S0 is R*cos(Theta),
  StdDev is sqrt(Variance),
  S is StdDev*S0+Mean.

gaussian([Mean1,Mean2],Covariance,_M,[X1,X2]):-!,
  random(U1),
  random(U2),
  R is sqrt(-2*log(U1)),
  Theta is 2*pi*U2,
  S0 is R*cos(Theta),
  S1 is R*sin(Theta),
  cholesky_decomposition(Covariance,A),
  matrix_multiply(A,[[S0],[S1]],Az),
  matrix_sum([[Mean1],[Mean2]],Az,[[X1],[X2]]).

gaussian(Mean,Covariance,_M,X):-
  length(Mean,N),
  n_gaussian_var(0,N,Z),
  cholesky_decomposition(Covariance,A),
  transpose([Z],ZT),
  matrix_multiply(A,ZT,Az),
  transpose([Mean],MT),
  matrix_sum(MT,Az,XT),
  transpose(XT,[X]).

n_gaussian_var(N,N,[]):-!.

n_gaussian_var(N1,N,[Z]):-
  N1 is N-1,!,
  random(U1),
  random(U2),
  R is sqrt(-2*log(U1)),
  Theta is 2*pi*U2,
  Z is R*cos(Theta).

n_gaussian_var(N1,N,[Z1,Z2|T]):-
  N2 is N1+2,
  random(U1),
  random(U2),
  R is sqrt(-2*log(U1)),
  Theta is 2*pi*U2,
  Z1 is R*cos(Theta),
  Z2 is R*sin(Theta),
  n_gaussian_var(N2,N,T).


/**
 * gaussian(+Mean:float,+Variance:float,+S:float,-Density:float) is det
 *
 * Computes the probability density of value S according to a Gaussian with
 * mean Mean and variance Variance and returns it in Density.
 */
gaussian(Mean,Variance,_M,S,D):-
  number(Mean),!,
  StdDev is sqrt(Variance),
  D is 1/(StdDev*sqrt(2*pi))*exp(-(S-Mean)*(S-Mean)/(2*Variance)).

gaussian(Mean,Covariance,_M,S,D):-
  determinant(Covariance,Det),
  matrix_diff([Mean],[S],S_M),
  matrix_inversion(Covariance,Cov_1),
  transpose(S_M,S_MT),
  matrix_multiply(S_M,Cov_1,Aux),
  matrix_multiply(Aux,S_MT,[[V]]),
  length(Mean,K),
  D is 1/sqrt((2*pi)^K*Det)*exp(-V/2).


/**
 * gamma(+Shape:float,+Scale:float,-S:float) is det
 *
 * samples a value from a Gamma density function with shape Shape and
 * scale Scale returns it in S
 */
gamma(A,Scale,_M,S):-
  (A>=1->
    gamma_gt1(A,S1)
  ;
    random(U),
    A1 is A+1,
    gamma_gt1(A1,S0),
    S1 is S0*U^(1/A)
  ),
  S is Scale*S1.

gamma_gt1(A,S):-
  D is A-1.0/3.0,
  C is 1.0/sqrt(9.0*D),
  cycle_gamma(D,C,S).

cycle_gamma(D,C,S):-
  getv(C,X,V),
  random(U),
  S0 is D*V,
  (U<1-0.0331*X^4->
    S=S0
  ;
    LogU is log(U),
    LogV is log(V),
    (LogU<0.5*X^2+D*(1-V+LogV)->
      S=S0
    ;
      cycle_gamma(D,C,S)
    )
  ).

getv(C,X,V):-
  gaussian(0.0,1.0,_M,X0),
  V0 is (1+C*X0)^3,
  (V0=<0->
    getv(C,X,V)
  ;
    V=V0,
    X=X0
  ).

gamma(K,Scale,_M,S,D):-
  D is exp(-lgamma(K))/(Scale^K)*S^(K-1)*exp(-S/Scale).

/**
 * beta(+Alpha:float,+Beta:float,-S:float) is det
 *
 * samples a value from a beta probability distribution with parameters
 * Alpha and Beta and returns it in S.
 * Uses the algorithm of
 * van der Waerden, B. L., "Mathematical Statistics", Springer
 * see also
 * https://en.wikipedia.org/wiki/Beta_distribution#Generating_beta-distributed_random_variates
 */
beta(Alpha,Beta,M,S):-
  gamma(Alpha,1,M,X),
  gamma(Beta,1,M,Y),
  S is X/(X+Y).

beta(Alpha,Beta,_M,X,D):-
  B is exp(lgamma(Alpha)+lgamma(Beta)-lgamma(Alpha+Beta)),
  D is X^(Alpha-1)*(1-X)^(Beta-1)/B.


/**
 * poisson(+Lambda:float,-S:int) is det
 *
 * samples a value from a Poisson probability distribution with parameter
 * Lambda and returns it in S.
 * Uses the inversion by sequential search
 * Devroye, Luc (1986). "Discrete Univariate Distributions"
 * Non-Uniform Random Variate Generation. New York: Springer-Verlag. p. 505.
 */
poisson(Lambda,_M,X):-
  P is exp(-Lambda),
  random(U),
  poisson_cycle(0,X,Lambda,P,P,U).

poisson_cycle(X,X,_L,_P,S,U):-
  U=<S,!.

poisson_cycle(X0,X,L,P0,S0,U):-
  X1 is X0+1,
  P is P0*L/X1,
  S is S0+P,
  poisson_cycle(X1,X,L,P,S,U).

poisson(Lambda,_M,X,P):-
  fact(X,1,FX),
  P is (Lambda^X)*exp(-Lambda)/FX.

fact(N,F,F):- N =< 0, !.

fact(N,F0,F):-
  F1 is F0*N,
  N1 is N-1,
  fact(N1,F1,F).

/**
 * binomial(+N:int,+P:float,-S:int) is det
 *
 * samples a value from a binomial probability distribution with parameters
 * N and P and returns it in S.
 */
binomial(N,1.0,_M,N):-!.

binomial(N,P,_M,X):-
  Pr0 is (1-P)^N,
  random(U),
  binomial_cycle(0,X,N,P,Pr0,Pr0,U).

binomial_cycle(X,X,_N,_P,_Pr,CPr,U):-
  U=<CPr,!.

binomial_cycle(X0,X,N,P,Pr0,CPr0,U):-
  X1 is X0+1,
  Pr is Pr0*P*(N-X0)/(X1*(1-P)),
  CPr is CPr0+Pr,
  binomial_cycle(X1,X,N,P,Pr,CPr,U).

binomial(N,P,_M,X,Pr):-
  fact(N,1,FN),
  fact(X,1,FX),
  N_X is N-X,
  fact(N_X,1,FN_X),
  Pr is P^X*(1-P)^N_X*FN/(FX*FN_X).

/**
 * dirichlet(+Par:list,-S:float) is det
 *
 * samples a value from a Dirichlet probability density with parameters
 * Par and returns it in S
 */
dirichlet(Par,_M,S):-
  maplist(get_gamma,Par,Gammas),
  sum_list(Gammas,Sum),
  maplist(divide(Sum),Gammas,S).

divide(S0,A,S):-
  S is A/S0.

get_gamma(A,G):-
  gamma(A,1.0,_M,G).

dirichlet(Par,_M,S,D):-
  beta(Par,B),
  foldl(prod,S,Par,1,D0),
  D is D0*B.

prod(X,A,P0,P0*X^(A-1)).


/**
 * geometric(+P:float,-I:int) is det
 *
 * samples a value from a geometric probability distribution with parameters
 * P and returns it in I (I belongs to [1,infinity]
 */
geometric(P,_M,I):-
  geometric_val(1,P,I).

geometric_val(N0,P,N):-
  random(R),
  (R=<P->
    N=N0
  ;
    N1 is N0+1,
    geometric_val(N1,P,N)
  ).

geometric(P,_M,I,D):-
  D is (1-P)^(I-1)*P.
/**
 * finite(+Distribution:list,-S:float) is det
 *
 * samples a value from a discrete distribution Distribution (a list
 * of couples Val:Prob) and returns it in S
 */
finite(D,M,S):-
  discrete(D,M,S).

finite(D,M,S,Dens):-
  discrete(D,M,S,Dens).

/**
 * discrete(+Distribution:list,-S:float) is det
 *
 * samples a value from a discrete distribution Distribution (a list
 * of couples Val:Prob) and returns it in S
 */
discrete(D,_M,S):-
  random(U),
  discrete_int(D,0,U,S).


discrete_int([S:_],_,_,S):-!.

discrete_int([S0:W|T],W0,U,S):-
  W1 is W0+W,
  (U=<W1->
    S=S0
  ;
    discrete_int(T,W1,U,S)
  ).

discrete(D,_M,S,Dens):-
  member(S:Dens,D).
% discrete(_D,_S,1.0).
/** 
 * exponential(+Lambda:float, -V:int) is det
 *
 * Samples a value from exponential distribution with parameter Lambda 
 * 
**/
exponential(Lambda,_M,V):-
  V0 is 1 - exp(-Lambda),
  random(RandomVal),    
  exponential_(1,RandomVal,Lambda,V0,V).

exponential_(I,RandomVal,_,CurrentProb,I):-
  RandomVal =< CurrentProb, !.
exponential_(I,RandomVal,Lambda,_,V):-
  I1 is I+1,
  CurrentProb is 1 - exp(-Lambda*I1),
  exponential_(I1,RandomVal,Lambda,CurrentProb,V).

exponential(Lambda,_M,X,V):-
  V is Lambda*exp(-Lambda*X).

/**
 * pascal(+R:int,+P:float,-Value:int) is det
 *
 * samples a value from a pascal probability distribution with parameters
 * R and P and returns it in Value. 
 * R is the number of failures
 * P is the success probability
 */

% R number of failures
% P probability of success
pascal(R,P,_M,Value):-
  pascal_int(0,R,P,V0),
  random(RandomVal),
  pascal_prob_(0,R,P,V0,RandomVal,Value).

pascal_prob_(I,_,_,CurrentProb,RandomVal,I):-
  RandomVal =< CurrentProb, !.
pascal_prob_(I,R,P,CurrentProb,RandomVal,V):-
  I1 is I+1,
  pascal_int(I1,R,P,V0),
  CurrentProb1 is V0 + CurrentProb,
  pascal_prob_(I1,R,P,CurrentProb1,RandomVal,V).

/*
* K number of successes
* R number of failures
* P probability of success
*/
pascal_int(K,R,P,Value):-
  KR1 is K+R-1,
  binomial_coeff(KR1,K,Bin),
  Value is Bin*(P**K)*(1-P)**R.

binomial_coeff(N,K,Val):-
  fact(N,1,NF),
  fact(K,1,KF),
  NK is N-K,
  fact(NK,1,NKF),
  Val is NF/(KF*NKF).

/**
 * user(+Distr:atom,+M:module,-Value:int) is det
 *
 * samples a value from a user defined distribution
 */
user(Distr,M,S):-
  call(M:Distr,S).

user(Distr,M,S,D):-
  call(M:Distr,S,D).

generate_rules_fact([],HeadList,VC,M,R,_N,[Rule]):-
  Rule=(samp(R,VC,N):-(sample_head(R,VC,M,HeadList,N))).

generate_rules_fact([],_HL,_VC,_M,_R,_N,[]).

generate_rules_fact([Head:_P1,'':_P2],HeadList,VC,M,R,N,[Clause]):-!,
  Clause=(Head:-(sample_head(R,VC,M,HeadList,N))).

generate_rules_fact([Head:_P|T],HeadList,VC,M,R,N,[Clause|Clauses]):-
  Clause=(Head:-(sample_head(R,VC,M,HeadList,N))),
  N1 is N+1,
  generate_rules_fact(T,HeadList,VC,M,R,N1,Clauses).


generate_clause_distr(Head,Body,VC,M,R,Var,Distr,Clause):-
  Clause=[(Head:-(Body,take_a_sample(R,VC,M,Distr,Var))),
  (samp(R,VC,Var):-take_a_sample(R,VC,M,Distr,Var))].


generate_clause_samp(Head,Body,HeadList,VC,M,R,N,[Clause,Rule]):-
  generate_clause(Head,Body,HeadList,VC,M,R,N,Clause),
  Rule=(samp(R,VC,Val):-sample_head(R,VC,M,HeadList,Val)).

generate_clause(Head,Body,HeadList,VC,M,R,N,Clause):-
  Clause=[(Head:-(Body,sample_head(R,VC,M,HeadList,N)))].


generate_rules([],_Body,HeadList,VC,M,R,_N,[Rule]):-
  Rule=(samp(R,VC,N):-sample_head(R,VC,M,HeadList,N)).

generate_rules([Head:_P1,'':_P2],Body,HeadList,VC,M,R,N,[Clause]):-!,
  generate_clause(Head,Body,HeadList,VC,M,R,N,Clause).

generate_rules([Head:_P|T],Body,HeadList,VC,M,R,N,[Clause|Clauses]):-
  generate_clause(Head,Body,HeadList,VC,M,R,N,Clause),
  N1 is N+1,
  generate_rules(T,Body,HeadList,VC,M,R,N1,Clauses).






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

/**
 * add_prob(?Prob:float,:Goal:atom,?AnnGoal:atom) is det
 *
 * From Prob and Goal builds the annotated atom AnnGoal=Goal:Prob.
 */
add_prob(P,A,A:P).

/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([H], Prob, [Head:ProbHead1|Null]) :-
  (H=Head:ProbHead;H=ProbHead::Head),!,
  ProbHead1 is ProbHead,
  ProbLast is 1 - Prob - ProbHead1,
  prolog_load_context(module, M),mc_input_mod(M),
  M:local_mc_setting(epsilon_parsing, Eps),
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



/**
 * set_mc(:Parameter:atom,+Value:term) is det
 *
 * The predicate sets the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
set_mc(M:Parameter,Value):-
  retract(M:local_mc_setting(Parameter,_)),
  assert(M:local_mc_setting(Parameter,Value)).

/**
 * setting_mc(:Parameter:atom,?Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_mc(M:P,V):-
  M:local_mc_setting(P,V).

extract_vars_list(L,[],V):-
  rb_new(T),
  extract_vars_term(L,T,T1),
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

delete_equal([],_,[]).

delete_equal([H|T],E,T):-
  H == E,!.

delete_equal([H|T],E,[H|T1]):-
  delete_equal(T,E,T1).

add_arg(A,Arg,A1):-
  A=..L,
  append(L,[Arg],L1),
  A1=..L1.

/**
 * set_sw(:Var:term,+List:lit) is det
 *
 * Sets the domain of the random variable Var to List.
 * This is a predicate for programs in the PRISM syntax
 */
set_sw(M:A,B):-
  M:local_mc_setting(prism_memoization,false),!,
  assert(M:sw(A,B)).

set_sw(M:A,B):-
  M:local_mc_setting(prism_memoization,true),
  get_next_rule_number(M,R),
  assert(M:sw(R,A,B)).

/**
 * msw(:Var:term,?Value:term) is det
 *
 * Gets or tests the Value of the random variable Var.
 * This is a predicate for programs in the PRISM syntax
 */
msw(M:A,B):-
  M:values(A,V),
  M:sw(A,D),!,
  sample_msw(V,D,B).

msw(M:A,B):-
  M:values(A,V),
  M:sw(R,A,D),
  sample_msw(V,M,R,A,D,B).

sample_msw(real,norm(Mean,Variance),V):-!,
  gaussian(Mean,Variance,_M,S),
  S=V.

sample_msw(Values,Dist,V):-
  maplist(combine,Values,Dist,VD),
  sample(VD,N),
  nth0(N,Values,V).

sample_msw(real,M,R,A,norm(Mean,Variance),V):-!,
  take_a_sample(R,A,M,gaussian(Mean,Variance),V).

sample_msw(Values,M,R,A,Dist,V):-
  maplist(combine,Values,Dist,VD),
  take_a_sample(R,A,M,discrete(VD),V).

combine(V,P,V:P).

msw_weight(M:A,B,W):-
  M:values(A,V),
  M:sw(A,D),!,
  msw_weight(V,D,B,W).

msw_weight(M:A,B,W):-
  M:values(A,V),
  M:sw(_R,A,D),
  msw_weight(V,D,B,W).

msw_weight(real,norm(Mean,Variance),V,W):-!,
  gaussian(Mean,Variance,V,W).

msw_weight(Values,Dist,V,W):-
  maplist(combine,Values,Dist,VD),
  member(V:W,VD).


test_prism(M):-
  (M:local_mc_setting(prism_memoization,false),M:values(_,_)->
    throw(error("This predicate doesn't support PRISM programs without memoization."))
  ;
    true
  ).

act(M,A/B):-
  M:(dynamic A/B).

tab(A/B,A/B1):-
  B1 is B + 2.

system:term_expansion(end_of_file, end_of_file) :-
  prolog_load_context(module, M),
  mc_input_mod(M),!,
  retractall(mc_input_mod(M)),
  style_check(+discontiguous).

system:term_expansion((:- mcaction Conj), []) :-!,
  prolog_load_context(module, M),
  mc_input_mod(M),!,
  list2and(L,Conj),
  maplist(act(M),L).

system:term_expansion((:- mc), []) :-!,
  prolog_load_context(module, M),
  retractall(local_mc_setting(_,_)),
  findall(local_mc_setting(P,V),default_setting_mc(P,V),L),
  assert_all(L,M,_),
  assert(mc_input_mod(M)),
  retractall(M:rule_n(_)),
  assert(M:rule_n(0)),
  dynamic((M:samp/3,M:mem/4,M:mc_on/0,M:sw/2,M:sw/3,M:sampled/3, M:sampled_g/2, M:sampled_g/1, M:disc/1,M:values/2)),
  retractall(M:samp(_,_,_)),
  style_check(-discontiguous).

system:term_expansion((:- table(Conj)), [:- table(Conj1)]) :-!,
  prolog_load_context(module, M),
  mc_input_mod(M),!,
  list2and(L,Conj),
  maplist(tab,L,L1),
  list2and(L1,Conj1).

system:term_expansion((:- begin_lpad), []) :-
  prolog_load_context(module, M),
  mc_input_mod(M),!,
  assert(M:mc_on).

system:term_expansion((:- end_lpad), []) :-
  prolog_load_context(module, M),
  mc_input_mod(M),!,
  retractall(M:mc_on).

system:term_expansion((Head:=Body),(H1:-Body)) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with guassia distr
  (Head \= ((system:term_expansion(_,_)) :- _ )),
  Head=(H~val(Var)), !,
  add_arg(H,Var,H1).


system:term_expansion((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% fact with distr
  (Head \= ((system:term_expansion(_,_)) :- _ )),
  Head=(H~Distr0), !,
  add_arg(H,Var,H1),
  switch_finite(Distr0,Distr),
  extract_vars_list([Head,Body],[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_distr(H1,Body,[],M,R,Var,Distr,Clause)
  ;
    generate_clause_distr(H1,Body,VC,M,R,Var,Distr,Clause)
  ).

system:term_expansion((Head:=Body),(Head:- Body)) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,!.

system:term_expansion((Head :- Body), Clauses):-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive clause with more than one head atom senza depth_bound
  Head = (_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list((Head :- Body),[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_rules(HeadList,Body,HeadList,[],M,R,0,Clauses)
  ;
    generate_rules(HeadList,Body,HeadList,VC,M,R,0,Clauses)
  ).

system:term_expansion((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% rule with distr
  (Head \= ((system:term_expansion(_,_)) :- _ )),
  Head=(H:Distr0),
  nonvar(Distr0),
  \+ number(Distr0),
  Distr0=..[D,Var|Pars],
  is_dist(M,D),!,
  Distr=..[D|Pars],
  extract_vars_list([Head,Body],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_distr(H,Body,[],M,R,Var,Distr,Clause)
  ;
    generate_clause_distr(H,Body,VC,M,R,Var,Distr,Clause)
  ).

system:term_expansion((Head :- Body), []) :-
% disjunctive clause with a single head atom con prob. 0 senza depth_bound --> la regola e' non  caricata nella teoria e non e' conteggiata in NR
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
  ((Head:-Body) \= ((system:term_expansion(_,_) ):- _ )),
  (Head = (_:P);Head=(P::_)),
  ground(P),
  P=:=0.0, !.


system:term_expansion((Head :- Body), Clauses) :-
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
  ((Head:-Body) \= ((system:term_expansion(_,_) ):- _ )),
  (Head = (H:_);Head = (_::H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list((Head :- Body),[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_samp(H,Body,HeadList,[],M,R,0,Clauses)
  ;
    generate_clause_samp(H,Body,HeadList,VC,M,R,0,Clauses)
  ).


system:term_expansion(Head,Clauses) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with more than one head atom senza db
  Head=(_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_rules_fact(HeadList,HeadList,[],M,R,0,Clauses)
  ;
    generate_rules_fact(HeadList,HeadList,VC,M,R,0,Clauses)
  ).

system:term_expansion(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% fact with distr
  (Head \= ((system:term_expansion(_,_)) :- _ )),
  Head=(H~Distr0),
  nonvar(Distr0),
  !,
  switch_finite(Distr0,Distr),
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_distr(H1,true,[],M,R,Var,Distr,Clause)
  ;
    generate_clause_distr(H1,true,VC,M,R,Var,Distr,Clause)
  ).

system:term_expansion(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with dirichlet distr
  (Head \= ((system:term_expansion(_,_)) :- _ )),
  Head=(H:Distr0),
  nonvar(Distr0),
  \+ number(Distr0),
  Distr0=..[D,Var|Pars],
  is_dist(M,D),!,
  Distr=..[D|Pars],
  extract_vars_list([Head],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_distr(H,true,[],M,R,Var,Distr,Clause)
  ;
    generate_clause_distr(H,true,VC,M,R,Var,Distr,Clause)
  ).


system:term_expansion(Head,[]) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with a single head atom con prob. 0
  (Head \= ((system:term_expansion(_,_)) :- _ )),
  (Head = (_:P); Head = (P::_)),
  ground(P),
  P=:=0.0, !.


system:term_expansion(Head,H) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((system:term_expansion(_,_)) :- _ )),
  (Head = (H:P);Head =(P::H)),
  ground(P),
  P=:=1.0, !.


system:term_expansion(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with a single head atom e prob. generiche, senza db
  (Head \= ((system:term_expansion(_,_)) :- _ )),
  (Head=(H:_);Head=(_::H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_samp(H,true,HeadList,[],M,R,0,Clause)
  ;
    generate_clause_samp(H,true,HeadList,VC,M,R,0,Clause)
  ).



/**
 * begin_lpad_pred is det
 *
 * Initializes LPAD loading.
 */
begin_lpad_pred:-
  assert(mc_input_mod(user)),
  assert(user:mc_on).

/**
 * end_lpad_pred is det
 *
 * Terminates the cplint inference module.
 */
end_lpad_pred:-
  retractall(mc_input_mod(_)),
  retractall(user:mc_on).

list2or([],true):-!.

list2or([X],X):-
    X\=;(_,_),!.

list2or([H|T],(H ; Ta)):-!,
    list2or(T,Ta).


list2and([],true):-!.

list2and([X],X):-
    X\=(_,_),!.

list2and([H|T],(H,Ta)):-!,
    list2and(T,Ta).

/**
 * builtin(+Goal:atom) is det
 *
 * Succeeds if Goal is an atom whose predicate is defined in Prolog
 * (either builtin or defined in a standard library).
 */
builtin(G):-
  builtin_int(G),!.

builtin_int(average(_L,_Av)).
builtin_int(mc_prob(_,_,_)).
builtin_int(mc_prob(_,_)).
builtin_int(mc_sample(_,_,_,_)).
builtin_int(db(_)).
builtin_int(G):-
  predicate_property(G,built_in).
builtin_int(G):-
  predicate_property(G,imported_from(lists)).
builtin_int(G):-
  predicate_property(G,imported_from(apply)).
builtin_int(G):-
  predicate_property(G,imported_from(nf_r)).
builtin_int(G):-
  predicate_property(G,imported_from(matrix)).
builtin_int(G):-
  predicate_property(G,imported_from(clpfd)).


is_dist(_M,D):-
  member(D,[
    finite,
    discrete,
    uniform,
    uniform_dens,
    gaussian,
    dirichlet,
    gamma,
    beta,
    poisson,
    binomial,
    geometric,
    exponential,
    pascal,
    user
    ]),!.


switch_finite(D0,D):-
  D0=..[F,Arg0],
  (F=finite;F=discrete),!,
  maplist(swap,Arg0,Arg),
  D=..[F,Arg].

switch_finite(D,D).

is_discrete(_M,D):-
  functor(D,F,A),
  member(F/A,[
    finite/1,
    discrete/1,
    uniform/1
    ]),!.

is_discrete(M,D):-
  functor(D,F,_A),
  M:disc(F).
/**
 * swap(?Term1:term,?Term2:term) is det
 *
 * If Term1 is of the form A:B, then Term2 is of the form B:A.
 */
swap(A:B,B:A).

/**
 * ~=(:Term:term, +B:term) is det
 *
 * equality predicate for distributional clauses
 */
(M:A) ~= B :-
  A=..L,
  append(L,[B],L1),
  A1=..L1,
  M:A1.

:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(mcintyre:s(_,_), []).
sandbox:safe_meta(mcintyre:mc_prob(_,_,_), []).
sandbox:safe_meta(mcintyre:mc_prob(_,_), []).
sandbox:safe_meta(mcintyre:mc_sample(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_rejection_sample(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_rejection_sample(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_sample(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_sample(_,_,_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_gibbs_sample(_,_,_), []).
sandbox:safe_meta(mcintyre:mc_gibbs_sample(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_gibbs_sample(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample(_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_first(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_first(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_one(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_one(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_raw(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_rejection_sample_arg(_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_rejection_sample_arg(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_sample_arg(_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_sample_arg(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_gibbs_sample_arg(_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_gibbs_sample_arg(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_gibbs_sample_arg(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_expectation(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_expectation(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_expectation(_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_rejection_expectation(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_gibbs_expectation(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_gibbs_expectation(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_gibbs_expectation(_,_,_,_,_,_), []).

sandbox:safe_meta(mcintyre:mc_lw_sample(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_lw_sample_arg(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_lw_sample_arg_log(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_lw_expectation(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_particle_expectation(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_particle_sample_arg(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:msw(_,_), []).

sandbox:safe_meta(mcintyre:set_mc(_,_), []).
sandbox:safe_meta(mcintyre:setting_mc(_,_), []).
sandbox:safe_meta(mcintyre:set_sw(_,_) ,[]).

:- license(artisticv2).
