/** <module> mcintyre

This module performs reasoning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It reads probabilistic program and computes the probability of queries
using sampling.

See https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or 
http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html for
details.

@author Fabrizio Riguzzi
@license Artistic License 2.0
@copyright Fabrizio Riguzzi
*/


:- module(mcintyre,[mc_prob/2, mc_prob_bar/2, 
  mc_sample/5,  
  mc_rejection_sample/6,  
  mc_rejection_sample/4,  
  mc_sample/3,mc_sample_bar/3,
  mc_sample_arg/4,mc_sample_arg_bar/4,
  mc_mh_sample/7,
  mc_mh_sample/5,
  mc_lw_sample/4,
  mc_rejection_sample_arg/5,mc_rejection_sample_arg_bar/5,
  mc_mh_sample_arg/6,mc_mh_sample_arg_bar/6,
  mc_sample_arg_first/4,mc_sample_arg_first_bar/4,
  mc_sample_arg_one/4,mc_sample_arg_one_bar/4,
  mc_sample_arg_raw/4,
  mc_expectation/4,
  mc_mh_expectation/6,
  set_mc/2,setting_mc/2,
  mc_load/1,mc_load_file/1,
  sample_gauss/5,
  sample_uniform/5,
  sample_dirichlet/4,
  sample_gamma/5,
  sample_poisson/4,
  sample_beta/5,
  sample_discrete/4,
  sample_head/4,
  mc_lw_sample_arg/5,
  mc_lw_sample_arg_log/5,
  mc_lw_expectation/5,
  gauss_density/4,
  gauss/3,
  histogram/3,
  densities/4,
  add_prob/3,
  op(600,xfy,'::'),
  op(600,xfy,'~'),
  op(500,xfx,'~='),
  op(1200,xfy,':='),
  ~= /2,
  swap/2,
  msw/2,
  set_sw/2
  ]).
:-meta_predicate s(:,-).
:-meta_predicate mc_prob(:,-).
:-meta_predicate mc_prob_bar(:,-).
:-meta_predicate mc_sample(:,+,-,-,-).
:-meta_predicate mc_rejection_sample(:,:,+,-,-,-).
:-meta_predicate mc_rejection_sample(:,:,+,-).
:-meta_predicate mc_mh_sample(:,:,+,+,-,-,-).
:-meta_predicate mc_mh_sample(:,:,+,+,-).
:-meta_predicate mc_sample(:,+,-).
:-meta_predicate mc_sample_bar(:,+,-).
:-meta_predicate mc_sample_arg(:,+,+,-).
:-meta_predicate mc_sample_arg_bar(:,+,+,-).
:-meta_predicate mc_rejection_sample_arg(:,:,+,+,-).
:-meta_predicate mc_rejection_sample_arg_bar(:,:,+,+,-).
:-meta_predicate mc_mh_sample_arg(:,:,+,+,+,-).
:-meta_predicate mc_mh_sample_arg_bar(:,:,+,+,+,-).
:-meta_predicate mc_sample_arg_first(:,+,+,-).
:-meta_predicate mc_sample_arg_first_bar(:,+,+,-).
:-meta_predicate mc_sample_arg_one(:,+,+,-).
:-meta_predicate mc_sample_arg_one_bar(:,+,+,-).
:-meta_predicate mc_sample_arg_raw(:,+,+,-).
:-meta_predicate mc_expectation(:,+,+,-).
:-meta_predicate mc_mh_expectation(:,:,+,+,+,-).
:-meta_predicate montecarlo_cycle(-,-,:,-,-,-,-,-,-).
:-meta_predicate montecarlo(-,-,-,:,-,-).
:-meta_predicate initial_sample_cycle(:).
:-meta_predicate initial_sample(:).
:-meta_predicate lw_sample_bool(+,:,:,-).
:-meta_predicate initial_sample_neg(:).

:-meta_predicate mc_lw_sample(:,:,+,-).
:-meta_predicate mc_lw_sample_arg(:,:,+,+,-).
:-meta_predicate mc_lw_sample_arg_log(:,:,+,+,-).
:-meta_predicate mc_lw_expectation(:,:,+,+,-).
:-meta_predicate lw_sample_cycle(:).
:-meta_predicate lw_sample_weight_cycle(:,-).
:-meta_predicate ~=(:,-).
:-meta_predicate msw(:,-).

:-use_module(library(lists)).
:-use_module(library(rbtrees)).
:-use_module(library(apply)).
:-use_module(library(assoc)).
:-use_module(library(clpr)).

:- style_check(-discontiguous).

:- thread_local v/3,rule_n/1,mc_module/1,mc_input_mod/1,local_mc_setting/2,mem/3.

/*:- multifile one/2,zero/2,and/4,or/4,bdd_not/3,init/3,init_bdd/2,init_test/1,
  end/1,end_bdd/1,end_test/0,ret_prob/3,em/9,randomize/1,
  get_var_n/5,add_var/5,equality/4.*/
%  remove/3.


/* k
 * -
 * This parameter shows the amount of items of the same type to consider at once.
 *
 * Default value:	64
 * Applies to:		bestfirst, bestk, montecarlo
 */
default_setting_mc(k, 1000).
/* min_error
 * ---------
 * This parameter shows the threshold for the probability interval.
 *
 * Default value:	0.01
 * Applies to:		bestfirst, montecarlo
 */
default_setting_mc(min_error, 0.01).

default_setting_mc(max_samples,10e4).


default_setting_mc(epsilon_parsing, 1e-5).
/* on, off */

default_setting_mc(compiling,off).

:-set_prolog_flag(unknown,warning).

default_setting_mc(depth_bound,false).  %if true, it limits the derivation of the example to the value of 'depth'
default_setting_mc(depth,2).
default_setting_mc(single_var,false). %false:1 variable for every grounding of a rule; true: 1 variable for rule (even if a rule has more groundings),simpler.

/** 
 * mc_load(++File:atom) is det
 *
 * Loads File.lpad if it exists, otherwise loads File.cpl if it exists.
 */
mc_load(File):-
  atomic_concat(File,'.lpad',FileLPAD),
  (exists_file(FileLPAD)->
    load_file(FileLPAD)
  ;
    atomic_concat(File,'.cpl',FileCPL),
    (exists_file(FileCPL)->
      load_file(FileCPL)
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
s(M:Goal,P):-
  mc_input_mod(Mo),
  Mo:local_mc_setting(min_error, MinError),
  Mo:local_mc_setting(k, K),
% Resetting the clocks...
% Performing resolution...
  copy_term(Goal,Goal1),
  numbervars(Goal1),
  save_samples(Goal1),
  montecarlo_cycle(0, 0, M:Goal, K, MinError, _Samples, _Lower, P, _Upper),
  !,
  erase_samples,
  restore_samples(Goal1).

save_samples(G):-
  mc_input_mod(M),
  recorded(R,Val,Ref),
  assert(M:mem(G,R,Val)),
  erase(Ref),
  fail.

save_samples(_G).

restore_samples(G):-
  mc_input_mod(M),
  retract(M:mem(G,R,Val)),
  recorda(R,Val),
  fail.

restore_samples(_G).

save_samples_copy(G):-
  mc_input_mod(M),
  recorded(R,Val,_Ref),
  assert(M:mem(G,R,Val)),
  fail.

save_samples_copy(_G).

delete_samples_copy(G):-
  mc_input_mod(M),
  retract(M:mem(G,_R,_Val)),
  fail.

delete_samples_copy(_G).

count_samples(N):-
  findall(Ref,recorded(_Key,_Val,Ref),L),
  length(L,N).


resample(0):-!.

resample(N):-
  findall((Key,Val,Ref),recorded(Key,Val,Ref),L),
  sample_one(L,(Key,Val,Ref)),
  erase(Ref),
  N1 is N-1,
  resample(N1).


erase_samples:-
  recorded(_Key,_Val,Ref),
  erase(Ref),
  fail.

erase_samples.

print_samples:-
  recorded(Key,Val,_Ref),
  write(Key-Val),nl,
  fail.

print_samples:-
  write(end),nl.

montecarlo_cycle(N0, S0, M:Goals, K, MinError, Samples, Lower, Prob, Upper):-!,
  montecarlo(K,N0, S0, M:Goals, N, S),
  P is S / N,
  D is N - S,
  Semi is 1.95996 * sqrt(P * (1 - P) / N),
  Int is 2 * Semi,
  mc_input_mod(Mo),
  Mo:local_mc_setting(max_samples,MaxSamples),
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
  erase_samples,
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
  erase_samples,
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

mh_montecarlo(_L,0,_NC0,N,S,_Succ0, _Goals,_Ev,N,S):-!.

mh_montecarlo(L,K0,NC0,N0, S0,Succ0, M:Goal, M:Evidence, N, S):-
  save_samples_copy(Goal),
  resample(L),
  copy_term(Evidence,Ev1),
  (M:Ev1->
    copy_term(Goal,Goal1),
    (M:Goal1->
      Succ1=1
    ;
      Succ1=0
    ),
    count_samples(NC1),
    (accept(NC0,NC1)->
      Succ = Succ1,
      delete_samples_copy(Goal)
    ;
      Succ = Succ0,
      erase_samples,
      restore_samples(Goal)
    ),
    N1 is N0 + 1,
    S1 is S0 + Succ,
  %format("Sample ~d Valid ~d~n",[N,Valid]),
  %flush_output,
    K1 is K0-1
  ;
    N1 = N0,
    S1 = S0,
    K1 = K0,
    NC1 = NC0,
    Succ = Succ0,
    erase_samples,
    restore_samples(Goal)
  ),
  mh_montecarlo(L,K1,NC1,N1, S1,Succ, M:Goal,M:Evidence, N,S).

accept(NC1,NC2):-
  P is min(1,NC1/NC2),
  random(P0),
  P>P0.

/** 
 * mc_prob(:Query:atom,-Probability:float) is det
 *
 * The predicate computes the probability of the query Query
 * If Query is not ground, it considers it as an existential query
 * and returns the probability that there is a satisfying assignment to
 * the query.
 */
mc_prob(M:Goal,P):-
  s(M:Goal,P).

/** 
 * mc_prob_bar(:Query:atom,-Probability:dict) is det
 *
 * The predicate computes the probability of the ground query Query
 * and returns it as a dict for rendering with c3 as a bar chart with 
 * a bar for the probability of Query true and a bar for the probability of 
 * Query false.
 * If Query is not ground, it considers it as an existential query
 * and returns the probability that there is a satisfying assignment to
 * the query.
 */
mc_prob_bar(M:Goal,Chart):-
  s(M:Goal,P),
  PF is 1.0-P,
  Chart = c3{data:_{x:elem, rows:[elem-prob,'T'-P,'F' -PF], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,max:1.0,padding:_{bottom:0.0,top:0.0},
             tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}},
	           size:_{height: 100},
	          legend:_{show: false}}.

/** 
 * mc_sample(:Query:atom,+Samples:int,-Probability:float) is det
 *
 * The predicate samples Query a number of Samples times and returns
 * the resulting Probability (Successes/Samples)
 * If Query is not ground, it considers it as an existential query
 */
mc_sample(M:Goal,S,P):-
  mc_sample(M:Goal,S,_T,_F,P).

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
  save_samples(Goal1),
  montecarlo(S,0, 0, M:Goal, N, T),
  P is T / N,
  F is N - T,
  erase_samples,
  restore_samples(Goal1).

/** 
 * mc_rejection_sample(:Query:atom,:Evidence:atom,+Samples:int,-Probability:float) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true and returns
 * the Probability of Query.
 * It performs rejection sampling: if in a sample Evidence is false, the 
 * sample is discarded.
 * If Query/Evidence are not ground, it considers them an existential queries.
 */
mc_rejection_sample(M:Goal,M:Evidence,S,P):-
  mc_rejection_sample(M:Goal,M:Evidence,S,_T,_F,P).
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
mc_rejection_sample(M:Goal,M:Evidence,S,T,F,P):-
  rejection_montecarlo(S,0, 0, M:Goal,M:Evidence, N, T),
  P is T / N,
  F is N - T,
  erase_samples.

/** 
 * mc_mh_sample(:Query:atom,:Evidence:atom,+Samples:int,+Lag:int,-Probability:float) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true and returns the
 * Probability of the query.
 * It performs Metropolis/Hastings sampling: between each sample, Lag sampled
 * choices are forgotten and each sample is accepted with a certain probability. 
 * If Query/Evidence are not ground, it considers them as existential queries.
 */
mc_mh_sample(M:Goal,M:Evidence,S,L,P):-
  mc_mh_sample(M:Goal,M:Evidence,S,L,_T,_F,P).

 /** 
 * mc_mh_sample(:Query:atom,:Evidence:atom,+Samples:int,+Lag:int,-Successes:int,-Failures:int,-Probability:float) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true and returns
 * the number of Successes, of Failures and the 
 * Probability (Successes/Samples).
 * It performs Metropolis/Hastings sampling: between each sample, Lag sampled
 * choices are forgotten and each sample is accepted with a certain probability. 
 * If Query/Evidence are not ground, it considers them as existential queries.
 */
mc_mh_sample(M:Goal,M:Evidence,S,L,T,F,P):-
  initial_sample_cycle(M:Evidence),!,
  copy_term(Goal,Goal1),
  (M:Goal1->
    Succ=1
  ;
    Succ=0
  ),
  count_samples(NC),
  S1 is S-1,
  mh_montecarlo(L,S1,NC,0, Succ,Succ, M:Goal, M:Evidence, _N, T),
  P is T / S,
  F is S - T,
  erase_samples.

initial_sample_cycle(M:G):-
  copy_term(G,G1),
  (initial_sample(M:G1)->
    true
  ;
    erase_samples,
    initial_sample_cycle(M:G)
  ).

initial_sample(_M:true):-!.

initial_sample(M:(A~= B)):-!,
  add_arg(A,B,A1),
  initial_sample(M:A1).

initial_sample(M:msw(A,B)):-!,
  msw(M:A,B).

initial_sample(_M:(sample_head(R,VC,HL,NH))):-!,
  sample_head(R,VC,HL,NH).

initial_sample(_M:sample_gauss(R,VC,Mean,Variance,S)):-!,
  sample_gauss(R,VC,Mean,Variance,S).

initial_sample(_M:sample_dirichlet(R,VC,Par,S)):-!,
  sample_dirichlet(R,VC,Par,S).

initial_sample(_M:sample_discrete(R,VC,D,S)):-!,
  sample_discrete(R,VC,D,S).

initial_sample(_M:sample_poisson(R,VC,Lambda,S)):-!,
  sample_poisson(R,VC,Lambda,S).

initial_sample(_M:sample_beta(R,VC,Alpha,Beta,S)):-!,
  sample_beta(R,VC,Alpha,Beta,S).

initial_sample(_M:sample_gamma(R,VC,Shape,Scale,S)):-!,
  sample_gamma(R,VC,Shape,Scale,S).

initial_sample(_M:sample_uniform(R,VC,L,U,S)):-!,
  sample_uniform(R,VC,L,U,S).

initial_sample(M:(G1,G2)):-!,
  initial_sample(M:G1),
  initial_sample(M:G2).

initial_sample(M:(G1;G2)):-!,
  initial_sample(M:G1);
  initial_sample(M:G2).

initial_sample(M:(\+ G)):-!,
  initial_sample_neg(M:G).

initial_sample(_M:G):-
  builtin(G),!,
  call(G).

initial_sample(M:G):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  initial_sample(M:B).

initial_sample_neg(_M:true):-!,
  fail.

initial_sample_neg(_M:(sample_head(R,VC,HL,N))):-!,
  sample_head(R,VC,HL,NH),
  NH\=N.

initial_sample_neg(_M:sample_gauss(R,VC,Mean,Variance,S)):-!,
  sample_gauss(R,VC,Mean,Variance,S).


initial_sample_neg(M:(G1,G2)):-!,
  (initial_sample_neg(M:G1);
  initial_sample_neg(M:G2)).

initial_sample_neg(M:(G1;G2)):-!,
  initial_sample_neg(M:G1),
  initial_sample_neg(M:G2).

initial_sample_neg(M:(\+ G)):-!,
  initial_sample(M:G).

initial_sample_neg(_M:G):-
  builtin(G),!,
  \+ call(G).

initial_sample_neg(M:G):-
  findall(B,M:clause(G,B),L),
  initial_sample_neg_all(L,M).

initial_sample_neg_all([],_M).

initial_sample_neg_all([H|T],M):-
  initial_sample_neg(M:H),
  initial_sample_neg_all(T,M).

check(R,VC,N):-
  recorded(R,sampled(VC,N)),!.

check(R,VC,N):-
  \+ recorded(R,sampled(VC,_N)),
  recorda(R,sampled(VC,N)).
 
check_neg(R,VC,_LN,N):-
  recorded(R,sampled(VC,N1)),!,
  N\=N1.

check_neg(R,VC,LN,N):-
  \+ recorded(R,sampled(VC,_N)),
  member(N1,LN),
  N1\= N,
  (recorded(R,sampled(VC,_N1),Ref)->
    erase(Ref)
  ;
    true
  ),
  recorda(R,sampled(VC,N1)).

listN(0,[]):-!.

listN(N,[N1|T]):-
  N1 is N-1,
  listN(N1,T).

/** 
 * mc_sample_bar(:Query:atom,+Samples:int,-Chart:dict) is det
 *
 * The predicate samples Query a number of Samples times and returns
 * a dict for rendering with c3 as a bar chart with 
 * a bar for the number of successes and a bar for the number
 * of failures.
 * If Query is not ground, it considers it as an existential query
 */
mc_sample_bar(M:Goal,S,Chart):-
  mc_sample(M:Goal,S,T,F,_P),
  Chart = c3{data:_{x:elem, rows:[elem-prob,'T'-T,'F' -F], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,padding:_{bottom:0.0}}},
	           size:_{height: 100},
	          legend:_{show: false}}.

/** 
 * mc_sample_arg(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query a number of Samples times. 
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where 
 * L is the list of values of Arg for which Query succeeds in 
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 */
mc_sample_arg(M:Goal,S,Arg,ValList):-
  empty_assoc(Values0),
  sample_arg(S,M:Goal,Arg, Values0,Values),
  erase_samples,
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList).

/** 
 * mc_sample_arg_bar(:Query:atom,+Samples:int,?Arg:var,-Chart:dict) is det
 *
 * The predicate samples Query Samples times. Arg should be a variable
 * in Query.
 * The predicate returns in Chart a dict for rendering with c3 as a bar chart
 * with a bar for each possible value of L,
 * the list of values of Arg for which Query succeeds in 
 * a world sampled at random. 
 * The size of the bar is the number of samples
 * returning that list of values.
 */
mc_sample_arg_bar(M:Goal,S,Arg,Chart):-
  mc_sample_arg(M:Goal,S,Arg,ValList0),
  maplist(to_atom,ValList0,ValList),
  Chart = c3{data:_{x:elem, rows:[elem-prob|ValList], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,padding:_{bottom:0.0}}},
	         %  size:_{height: 100},
	          legend:_{show: false}}.

/** 
 * mc_rejection_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query a number of Samples times given that
 * Evidence is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where 
 * L is the list of values of Arg for which Query succeeds in 
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * Rejection sampling is performed.
 */
mc_rejection_sample_arg(M:Goal,M:Ev,S,Arg,ValList):-
  empty_assoc(Values0),
  rejection_sample_arg(S,M:Goal,M:Ev,Arg, Values0,Values),
  erase_samples,
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList).

/** 
 * mc_rejection_sample_arg_bar(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Chart:dict) is det
 *
 * The predicate call mc_rejection_sample_arg/5 and build a c3 graph
 * of the results.
 * It returns in Chart a dict for rendering with c3 as a bar chart
 * with a bar for each possible value of L,
 * the list of values of Arg for which Query succeeds
 * given that Evidence is true
 * The size of the bar is the number of samples
 * returning that list of values.
 */
mc_rejection_sample_arg_bar(M:Goal,M:Ev,S,Arg,Chart):-
  mc_rejection_sample_arg(M:Goal,M:Ev,S,Arg,ValList0),
  maplist(to_atom,ValList0,ValList),
  Chart = c3{data:_{x:elem, rows:[elem-prob|ValList], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,padding:_{bottom:0.0}}},
	         %  size:_{height: 100},
	          legend:_{show: false}}.

/** 
 * mc_mh_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,+Lag:int,?Arg:var,-Values:list) is det
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
mc_mh_sample_arg(M:Goal,M:Evidence,S,L,Arg,ValList):-
  initial_sample_cycle(M:Evidence),!,
  empty_assoc(Values0),
  findall(Arg,M:Goal,La),
  numbervars(La),
  put_assoc(La,Values0,1,Values1),
  count_samples(NC),
  S1 is S-1,
  mh_sample_arg(L,S1,NC,M:Goal,M:Evidence,Arg, Values1,Values),
  erase_samples,
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList).

/** 
 * mc_mh_sample_arg_bar(:Query:atom,:Evidence:atom,+Samples:int,+Lag:int,?Arg:var,-Chart:dict) is det
 *
 * The predicate call mc_mh_sample_arg/6 and build a c3 graph
 * of the results.
 * The predicate returns in Chart a dict for rendering with c3 as a bar chart
 * with a bar for each possible value of L,
 * the list of values of Arg for which Query succeeds in 
 * a world sampled at random. 
 * The size of the bar is the number of samples
 * returning that list of values.
 */
mc_mh_sample_arg_bar(M:Goal,M:Ev,S,L,Arg,Chart):-
  mc_mh_sample_arg(M:Goal,M:Ev,S,L,Arg,ValList0),
  maplist(to_atom,ValList0,ValList),
  Chart = c3{data:_{x:elem, rows:[elem-prob|ValList], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,padding:_{bottom:0.0}}},
	         %  size:_{height: 100},
	          legend:_{show: false}}.




to_atom(A0-N,A-N):-
  term_to_atom(A0,A).

mh_sample_arg(_L,0,_NC0,_Goals,_Ev,_Arg,V,V):-!.

mh_sample_arg(L,K0,NC0,M:Goal, M:Evidence, Arg,V0,V):-
  save_samples_copy(Goal),
  resample(L),
  copy_term(Evidence,Ev1),
  (M:Ev1->
    findall(Arg,M:Goal,La),
    numbervars(La),
    count_samples(NC1),
    (accept(NC0,NC1)->
     (get_assoc(La, V0, N)->
        N1 is N+1,
        put_assoc(La,V0,N1,V1)
      ;
        put_assoc(La,V0,1,V1)
      ),
      delete_samples_copy(Goal),
      K1 is K0-1
    ;
      V1=V0,
      K1=K0,
      erase_samples,
      restore_samples(Goal)
    )
  ;
    K1 = K0,
    NC1 = NC0,
    V1 = V0,
    erase_samples,
    restore_samples(Goal)
  ),
  mh_sample_arg(L,K1,NC1,M:Goal,M:Evidence,Arg,V1,V).


rejection_sample_arg(0,_Goals,_Ev,_Arg,V,V):-!.

rejection_sample_arg(K1, M:Goals,M:Ev,Arg,V0,V):-
  erase_samples,
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
  erase_samples,
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
 * mc_lw_sample(:Query:atom,:Evidence:atom,+Samples:int,-Prob:floar) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * The predicate returns in Prob the probability that the query is true.
 * It performs likelihood weighting: each sample is weighted by the
 * likelihood of evidence in the sample.
 */
mc_lw_sample(M:Goal,M:Evidence,S,P):-
  erase_samples,
  lw_sample_bool(S,M:Goal,M:Evidence,ValList),
  foldl(agg_val,ValList,0,Sum),
  foldl(value_cont_single,ValList,0,SumTrue),
  P is SumTrue/Sum.


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
mc_lw_sample_arg(M:Goal,M:Evidence,S,Arg,ValList):-
  lw_sample_arg(S,M:Goal,M:Evidence,Arg,ValList0),
  foldl(agg_val,ValList0,0,Sum),
  Norm is S/Sum,
  maplist(norm(Norm),ValList0,ValList).

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
mc_lw_sample_arg_log(M:Goal,M:Evidence,S,Arg,ValList):-
  lw_sample_arg_log(S,M:Goal,M:Evidence,Arg,ValList).

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
  foldl(single_value_cont,ValList,0,Sum),
  erase_samples,
  foldl(agg_val,ValList,0,SW),
  E is Sum/SW.


single_value_cont(H-N,S,S+N*H).


agg_val(_ -N,S,S+N).

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
  erase_samples,
  lw_sample_bool(K1,M:Goal,M:Evidence,V).

lw_sample_arg(0,_Goals,_Ev,_Arg,[]):-!.

lw_sample_arg(K0,M:Goal, M:Evidence, Arg,[Arg1-W|V]):-
  copy_term((Goal,Arg),(Goal1,Arg1)),
  lw_sample_cycle(M:Goal1),
  copy_term(Evidence,Ev1),
  lw_sample_weight_cycle(M:Ev1,W),
  K1 is K0-1,
  erase_samples,
  lw_sample_arg(K1,M:Goal,M:Evidence,Arg,V).

lw_sample_cycle(M:G):-
  (lw_sample(M:G)->
    true
  ;
    erase_samples,
    lw_sample_cycle(M:G)
  ).

lw_sample_arg_log(0,_Goals,_Ev,_Arg,[]):-!.

lw_sample_arg_log(K0,M:Goal, M:Evidence, Arg,[Arg1-W|V]):-
  copy_term((Goal,Arg),(Goal1,Arg1)),
  lw_sample_cycle(M:Goal1),
  copy_term(Evidence,Ev1),
  lw_sample_logweight(M:Ev1,0,W),
  K1 is K0-1,
  erase_samples,
  lw_sample_arg_log(K1,M:Goal,M:Evidence,Arg,V).


lw_sample(_M:true):-!.

lw_sample(M:A~=B):-!,
  add_arg(A,B,A1),
  lw_sample(M:A1).

lw_sample(M:msw(A,B)):-!,
  msw(M:A,B).

lw_sample(_M:(sample_head(R,VC,_HL,N))):-!,
  check(R,VC,N).

lw_sample(_M:sample_gauss(R,VC,Mean,Variance,S)):-!,
  sample_gauss(R,VC,Mean,Variance,S).

lw_sample(_M:sample_poisson(R,VC,Lambda,S)):-!,
  sample_poisson(R,VC,Lambda,S).

lw_sample(_M:sample_beta(R,VC,Alpha,Beta,S)):-!,
  sample_gamma(R,VC,Alpha,Beta,S).

lw_sample(_M:sample_gamma(R,VC,Shape,Scale,S)):-!,
  sample_gamma(R,VC,Shape,Scale,S).

lw_sample(_M:sample_dirichlet(R,VC,Par,S)):-!,
  sample_dirichlet(R,VC,Par,S).

lw_sample(_M:sample_uniform(R,VC,L,U,S)):-!,
  sample_uniform(R,VC,L,U,S).

%lw_sample(_M:sample_discrete(R,VC,D,S)):-!,
%  sample_discrete(R,VC,D,S).

lw_sample(_M:sample_discrete(R,VC,D,S)):-!,
  sample_head(R,VC,D,SN),
  nth0(SN,D,S:_P).

lw_sample(M:(G1,G2)):-!,
  lw_sample(M:G1),
  lw_sample(M:G2).

lw_sample(M:(G1;G2)):-!,
  lw_sample(M:G1);
  lw_sample(M:G2).

lw_sample(M:(\+ G)):-!,
  \+ lw_sample(M:G).

lw_sample(_M:G):-
  builtin(G),!,
  call(G).

lw_sample(M:G):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  lw_sample(M:B).


lw_sample_weight_cycle(M:G,W):-
  (lw_sample_weight(M:G,1,W)->
    true
  ;
    erase_samples,
    lw_sample_weight_cycle(M:G,W)
  ).

lw_sample_weight(_M:true,W,W):-!.

lw_sample_weight(M:A~= B,W0,W):-!,
  add_arg(A,B,A1),
  lw_sample_weight(M:A1,W0,W).

lw_sample_weight(M:msw(A,B),W0,W):-!,
  (var(B)->
    msw(M:A,B),
    W=W0
  ;
    msw_weight(M:A,B,W1),
    W is W0*W1
  ).

lw_sample_weight(_M:(sample_head(R,VC,HL,N)),W0,W):-!,
  check(R,VC,N),
  nth0(N,HL,_:P),
  W is W0*P.

lw_sample_weight(_M:sample_discrete(R,VC,D,S),W0,W):-!,
  sample_head(R,VC,D,SN),
  nth0(SN,D,S:P),
  W is W0*P.


lw_sample_weight(_M:sample_uniform(R,VC,L,U,S),W0,W):-!,
  (var(S)->
    sample_uniform(R,VC,L,U,S),
    W=W0
  ;
    uniform_density(L,U,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_gauss(R,VC,Mean,Variance,S),W0,W):-!,
  (var(S)->
    sample_gauss(R,VC,Mean,Variance,S),
    W=W0
  ;
    gauss_density(Mean,Variance,S,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_poisson(R,VC,Lambda,S),W0,W):-!,
  (var(S)->
    sample_poisson(R,VC,Lambda,S),
    W=W0
  ;
    poisson_prob(Lambda,S,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_gamma(R,VC,Shape,Scale,S),W0,W):-!,
  (var(S)->
    sample_gamma(R,VC,Shape,Scale,S),
    W=W0
  ;
    gamma_density(Shape,Scale,S,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_beta(R,VC,Alpha,Beta,S),W0,W):-!,
  (var(S)->
    sample_beta(R,VC,Alpha,Beta,S),
    W=W0
  ;
    beta_density(Alpha,Beta,S,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_dirichlet(R,VC,Par,S),W0,W):-!,
  (var(S)->
    sample_dirichlet(R,VC,Par,S),
    W=W0
  ;
    dirichlet_density(Par,S,D),
    W is W0*D
   ).

lw_sample_weight(M:(G1,G2),W0,W):-!,
  lw_sample_weight(M:G1,W0,W1),
  lw_sample_weight(M:G2,W1,W).

lw_sample_weight(M:(G1;G2),W0,W):-!,
  lw_sample_weight(M:G1,W0,W);
  lw_sample_weight(M:G2,W0,W).

lw_sample_weight(M:(\+ G),W0,W):-!,
  lw_sample(M:G,1,W1),
  W is W0*(1-W1).

lw_sample_weight(_M:G,W,W):-
  builtin(G),!,
  call(G).

lw_sample_weight(M:G,W0,W):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  lw_sample_weight(M:B,W0,W).


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

lw_sample_logweight(_M:(sample_head(R,VC,HL,N)),W0,W):-!,
  check(R,VC,N),
  nth0(N,HL,_:P),
  W is W0+log(P).

lw_sample_logweight(_M:sample_discrete(R,VC,D,S),W0,W):-!,
  sample_head(R,VC,D,SN),
  nth0(SN,D,S:P),
  W is W0+log(P).

lw_sample_logweight(_M:sample_uniform(R,VC,L,U,S),W0,W):-!,
  (var(S)->
    sample_uniform(R,VC,L,U,S),
    W=W0
  ;
    uniform_density(L,U,D),
    W is W0+log(D)
   ).

lw_sample_logweight(_M:sample_gauss(R,VC,Mean,Variance,S),W0,W):-!,
  (var(S)->
    sample_gauss(R,VC,Mean,Variance,S),
    W=W0
  ;
    gauss_density(Mean,Variance,S,D),
    W is W0+log(D)
   ).

lw_sample_logweight(_M:sample_gamma(R,VC,Shape,Scale,S),W0,W):-!,
  (var(S)->
    sample_gamma(R,VC,Shape,Scale,S),
    W=W0
  ;
    gamma_density(Shape,Scale,S,D),
    W is W0+log(D)
   ).

lw_sample_logweight(_M:sample_dirichlet(R,VC,Par,S),W0,W):-!,
  (var(S)->
    sample_dirichlet(R,VC,Par,S),
    W=W0
  ;
    dirichlet_density(Par,S,D),
    W is W0+log(D)
   ).

lw_sample_logweight(M:(G1,G2),W0,W):-!,
  lw_sample_logweight(M:G1,W0,W1),
  lw_sample_logweight(M:G2,W1,W).

lw_sample_logweight(M:(G1;G2),W0,W):-!,
  lw_sample_logweight(M:G1,W0,W);
  lw_sample_logweight(M:G2,W0,W).

lw_sample_logweight(M:(\+ G),W0,W):-!,
  lw_sample(M:G,0,W1),
  W is W0-log(W1).


lw_sample_logweight(_M:G,W,W):-
  builtin(G),!,
  call(G).

lw_sample_logweight(M:G,W0,W):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  lw_sample_logweight(M:B,W0,W).



/** 
 * mc_sample_arg_first(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query a number of Samples times. 
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples V-N where 
 * V is the value of Arg returned as the first answer by Query in 
 * a world sampled at random and N is the number of samples
 * returning that value.
 * V is failure if the query fails.
 */
mc_sample_arg_first(M:Goal,S,Arg,ValList):-
  empty_assoc(Values0),
  sample_arg_first(S,M:Goal,Arg, Values0,Values),
  erase_samples,
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList).

/** 
 * mc_sample_arg_first_bar(:Query:atom,+Samples:int,?Arg:var,-Chart:dict) is det
 *
 * The predicate samples Query Samples times. Arg should be a variable
 * in Query.
 * The predicate returns in Chart a dict for rendering with c3 as a bar chart
 * with a bar for each value of Arg returned as a first answer by Query in 
 * a world sampled at random.
 * The size of the bar is the number of samples that returned that value.
 * The value is failure if the query fails.
 */
mc_sample_arg_first_bar(M:Goal,S,Arg,Chart):-
  mc_sample_arg_first(M:Goal,S,Arg,ValList0),
  maplist(to_atom,ValList0,ValList),
  Chart = c3{data:_{x:elem, rows:[elem-prob|ValList], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,padding:_{bottom:0.0}}},
	         %  size:_{height: 100},
	          legend:_{show: false}}.


sample_arg_first(0,_Goals,_Arg,V,V):-!.

sample_arg_first(K1, M:Goals,Arg,V0,V):-
  erase_samples,
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
 * mc_sample_arg_one(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query a number of Samples times. 
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples V-N where 
 * V is a value of Arg sampled with uniform probability from those returned 
 * by Query in a world sampled at random and N is the number of samples
 * returning that value.
 * V is failure if the query fails.
 */
mc_sample_arg_one(M:Goal,S,Arg,ValList):-
  empty_assoc(Values0),
  sample_arg_one(S,M:Goal,Arg, Values0,Values),
  erase_samples,
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList).

/** 
 * mc_sample_arg_one_bar(:Query:atom,+Samples:int,?Arg:var,-Chart:dict) is det
 *
 * The predicate samples Query Samples times. Arg should be a variable
 * in Query.
 * The predicate returns in Chart a dict for rendering with c3 as a bar chart
 * with a bar for each value of Arg returned by sampling with uniform 
 * probabability one answer from those returned by Query in a world sampled 
 * at random. 
 * The size of the bar is the number of samples.
 * The value is failure if the query fails.
 */
mc_sample_arg_one_bar(M:Goal,S,Arg,Chart):-
  mc_sample_arg_one(M:Goal,S,Arg,ValList0),
  maplist(to_atom,ValList0,ValList),
  Chart = c3{data:_{x:elem, rows:[elem-prob|ValList], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,padding:_{bottom:0.0}}},
	         %  size:_{height: 100},
	          legend:_{show: false}}.


sample_arg_one(0,_Goals,_Arg,V,V):-!.

sample_arg_one(K1, M:Goals,Arg,V0,V):-
  erase_samples,
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
 * The predicate returns in Values a list of couples V-N where 
 * V is the value of Arg returned as the first answer by Query in 
 * a world sampled at random and N is the number of samples
 * returning that value.
 * V is failure if the query fails.
 */
mc_sample_arg_raw(M:Goal,S,Arg,Values):-
  sample_arg_raw(S,M:Goal,Arg,Values),
  erase_samples.

sample_arg_raw(0,_Goals,_Arg,[]):-!.

sample_arg_raw(K1, M:Goals,Arg,[Val|V]):-
  erase_samples,
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
  erase_samples,
  E is Sum/S.

/** 
 * mc_mh_expectation(:Query:atom,:Evidence:atom,+N:int,+Lag:int,?Arg:var,-Exp:float) is det
 *
 * The predicate computes the expected value of Arg in Query by
 * sampling.
 * It takes N samples of Query and sums up the value of Arg for
 * each sample. The overall sum is divided by N to give Exp.
 * Arg should be a variable in Query.
 */
mc_mh_expectation(M:Goal,M:Evidence,S,L,Arg,E):-
  mc_mh_sample_arg(M:Goal,M:Evidence,S,L,Arg,ValList),
  foldl(value_cont,ValList,0,Sum),
  erase_samples,
  E is Sum/S.

value_cont([]-_,0):-!.

value_cont([H|_T]-N,S,S+N*H).

sample_val(0,_Goals,_Arg,Sum,Sum):-!.

sample_val(K1, M:Goals,Arg,Sum0,Sum):-
  erase_samples,
  copy_term((Goals,Arg),(Goals1,Arg1)),
  (M:Goals1->
    Sum1 is Sum0+Arg1
  ;
    Sum1=Sum
  ),
  K2 is K1-1,
  sample_val(K2,M:Goals,Arg,Sum1,Sum).


load(FileIn,C1,R):-
  open(FileIn,read,SI),
  read_clauses_dir(SI,C),
  close(SI),
  process_clauses(C,[],C1,[],R).

get_node(Goal,Env,B):-
  mc_input_mod(M),
  M:local_mc_setting(depth_bound,true),!,
  M:local_mc_setting(depth,DB),
  retractall(v(_,_,_)),
  add_bdd_arg_db(Goal,Env,BDD,DB,Goal1),%DB=depth bound
  (bagof(BDD,Goal1,L)*->
    or_list(L,Env,B)
  ;
    zero(Env,B)
  ).

get_node(Goal,Env,B):- %with DB=false
  retractall(v(_,_,_)),
  add_bdd_arg(Goal,Env,BDD,Goal1),
  (bagof(BDD,Goal1,L)*->
    or_list(L,Env,B)
  ;  
    zero(Env,B)
  ).


get_next_rule_number(R):-
  mc_input_mod(PName),
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

/** 
 * get_var_n(++Environment:int,++Rule:int,++Substitution:term,++Probabilities:list,-Variable:int) is det
 *
 * Returns the index Variable of the random variable associated to rule with 
 * index Rule, grouding substitution Substitution and head distribution 
 * Probabilities in environment Environment.
 */
get_var_n(Env,R,S,Probs0,V):-
  (ground(Probs0)->
    maplist(is,Probs,Probs0),
    (v(R,S,V)->
      true
    ;
      length(Probs,L),
      add_var(Env,L,Probs,R,V),    
      assert(v(R,S,V))
    )
  ;
    trhow(error('Non ground probailities not instantiated by the body'))
  ).

add_bdd_arg(M:A,Env,BDD,M:A1):-
  A=..[P|Args],
  append(Args,[Env,BDD],Args1),
  A1=..[P|Args1].


add_bdd_arg_db(M:A,Env,BDD,DB,M:A1):-
  A=..[P|Args],
  append(Args,[Env,DB,BDD],Args1),
  A1=..[P|Args1].


add_bdd_arg(A,Env,BDD,_Module,A1):-
  A=..[P|Args],
  append(Args,[Env,BDD],Args1),
  A1=..[P|Args1].


add_bdd_arg_db(A,Env,BDD,DB,_Module,A1):-
  A=..[P|Args],
  append(Args,[Env,DB,BDD],Args1),
  A1=..[P|Args1].

add_mod_arg(A,_Module,A1):-
  A=..[P|Args],
  A1=..[P|Args].
/** 
 * sample_head(+R:int,+Variables:list,+HeadList:list,-HeadNumber:int) is det
 *
 * samples a head from rule R instantiated as indicated by Variables (list of
 * constants, one per variable. HeadList contains the head as a list.
 * HeadNumber is the number of the sample head.
 * Internal predicates used by the transformed input program
 */
sample_head(R,VC,_HeadList,N):-
  recorded(R,sampled(VC,NH)),!,
  N=NH.

sample_head(R,VC,HeadList,N):-
  sample(HeadList,NH),
  recorda(R,sampled(VC,NH)),
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
 * sample_uniform(+R:int,+VC:list,+Lower:float,+Upper:float,-S:float) is det
 *
 * Returns in S a sample from a distribution uniform in (Lower,Upper)
 * associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_uniform(R,VC,_L,_U,S):-
  recorded(R,sampled(VC,S)),!.

sample_uniform(R,VC,L,U,S):-
  random(V),
  S is L+V*(U-L),
  recorda(R,sampled(VC,S)).

uniform_density(L,U,D):-
  D is 1/(U-L).

/** 
 * sample_gauss(+R:int,+VC:list,+Mean:float,+Variance:float,-S:float) is det
 *
 * Returns in S a sample for a Gaussian variable with mean Mean and variance
 * Variance associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_gauss(R,VC,_Mean,_Variance,S):-
  recorded(R,sampled(VC,S)),!.

sample_gauss(R,VC,Mean,Variance,S):-
  gauss(Mean,Variance,S),
  recorda(R,sampled(VC,S)).
/** 
 * gauss(+Mean:float,+Variance:float,-S:float) is det
 *
 * samples a value from a Gaussian with mean Mean and variance
 * Variance and returns it in S
 */
gauss(Mean,Variance,S):-
  random(U1),
  random(U2),
  R is sqrt(-2*log(U1)),
  Theta is 2*pi*U2,
  S0 is R*cos(Theta),
  StdDev is sqrt(Variance),
  S is StdDev*S0+Mean.

/**
 * gauss_density(+Mean:float,+Variance:float,+S:float,-Density:float) is det
 *
 * Computes the probability density of value S according to a Gaussian with
 * mean Mean and variance Variance and returns it in Density.
 */
gauss_density(Mean,Variance,S,D):-
  StdDev is sqrt(Variance),
  D is 1/(StdDev*sqrt(2*pi))*exp(-(S-Mean)*(S-Mean)/(2*Variance)).

/**
 * sample_gamma(+R:int,+VC:list,+Shape:float,+Scale:float,-S:float) is det
 *
 * Returns in S a sample for a Gamma distributed variable with shape Shape and
 * scale Scale associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_gamma(R,VC,_Shape,_Scale,S):-
  recorded(R,sampled(VC,S)),!.

sample_gamma(R,VC,Shape,Scale,S):-
  gamma(Shape,Scale,S),
  recorda(R,sampled(VC,S)).

/**
 * gamma(+Shape:float,+Scale:float,-S:float) is det
 *
 * samples a value from a Gamma density function with shape Shape and
 * scale Scale returns it in S
 */
gamma(A,Scale,S):-
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
  gauss(0.0,1.0,X0),
  V0 is (1+C*X0)^3,
  (V0=<0->
    getv(C,X,V)
  ;
    V=V0,
    X=X0
  ).

gamma_density(K,Scale,S,D):-
  D is exp(-lgamma(K))/(Scale^K)*S^(K-1)*exp(-S/Scale).
    
/**
 * sample_beta(+R:int,+VC:list,+Alpha:float,+Beta:float,-S:float) is det
 *
 * Returns in S a sample for a beta distributed variable with parameters
 * Alpha and Beta associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_beta(R,VC,_A,_B,S):-
  recorded(R,sampled(VC,S)),!.

sample_beta(R,VC,A,B,S):-
  beta(A,B,S),
  recorda(R,sampled(VC,S)).

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
beta(Alpha,Beta,S):-
  gamma(Alpha,1,X),
  gamma(Beta,1,Y),
  S is X/(X+Y).

beta_density(Alpha,Beta,X,D):-
  B is exp(lgamma(Alpha)+lgamma(Beta)-lgamma(Alpha+Beta)),
  D is X^(Alpha-1)*(1-X)^(Beta-1)/B.


/**
 * sample_poisson(+R:int,+VC:list,+Lambda:float,-S:int) is det
 *
 * Returns in S a sample for a Poisson distributed variable with parameter
 * lambda Lambda associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_poisson(R,VC,_L,S):-
  recorded(R,sampled(VC,S)),!.

sample_poisson(R,VC,L,S):-
  poisson(L,S),
  recorda(R,sampled(VC,S)).

/**
 * poisson(+Lambda:float,-S:int) is det
 *
 * samples a value from a Poisson probability distribution with parameter
 * Lambda and returns it in S.
 * Uses the inversion by sequential search
 * Devroye, Luc (1986). "Discrete Univariate Distributions"
 * Non-Uniform Random Variate Generation. New York: Springer-Verlag. p. 505.
 */
poisson(Lambda,X):-
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

poisson_prob(Lambda,X,P):-
  fact(X,1,FX),
  P is (Lambda^X)*exp(-Lambda)/FX.

fact(0,F,F):-!.

fact(N,F0,F):-
  F1 is F0*N,
  N1 is N-1,
  fact(N1,F1,F).
/**
 * sample_dirichlet(+R:int,+VC:list,+Par:list,-S:float) is det
 *
 * Returns in S a sample for a Dirichlet distributed variable with parameters
 * Par associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_dirichlet(R,VC,_Par,S):-
  recorded(R,sampled(VC,S)),!.

sample_dirichlet(R,VC,Par,S):-
  dirichlet(Par,S),
  recorda(R,sampled(VC,S)).

/**
 * dirichlet(+Par:list,-S:float) is det
 *
 * samples a value from a Dirichlet probability density with parameters
 * Par and returns it in S
 */
dirichlet(Par,S):-
  maplist(get_gamma,Par,Gammas),
  sumlist(Gammas,Sum),
  maplist(divide(Sum),Gammas,S).

divide(S0,A,S):-
  S is A/S0.

get_gamma(A,G):-
  gamma(A,1.0,G).

dirichlet_density(Par,S,D):-
  beta(Par,B),
  foldl(prod,S,Par,1,D0),
  D is D0*B.

prod(X,A,P0,P0*X^(A-1)).

/**
 * sample_discrete(+R:int,+VC:list,+Distribution:list,-S:float) is det
 *
 * Returns in S a sample from a discrete distribution Distribution (a list
 * of couples Val:Prob) associated to rule R with substitution VC. 
 * If the variable has already been sampled, it retrieves the sampled 
 * value, otherwise it takes a new sample and records it for rule R with 
 * substitution VC.
 */
sample_discrete(R,VC,_D,S):-
  recorded(R,sampled(VC,S)),!.

sample_discrete(R,VC,D,S0):-
  discrete(D,S),
  S=S0,
  recorda(R,sampled(VC,S)).

/**
 * discrete(+Distribution:list,-S:float) is det
 *
 * samples a value from a discrete distribution Distribution (a list
 * of couples Val:Prob) and returns it in S
 */
discrete(D,S0):-
  append(D0,[LastS:_P],D),
  foldl(pick_val,D0,(1,_),(_,S)),
  (var(S)->  
    S0=LastS
  ;
    S0=S
  ).

pick_val(_,(P0,V0),(P0,V0)):-
  nonvar(V0).

pick_val(S:P,(P0,V0),(P1,V1)):-
  var(V0),
  PF is P/P0,
  random(U),
  (U=<PF->
    P1=PF,
    V1=S
  ;
    P1 is P0*(1-PF),
    V1=V0
  ).

generate_rules_fact([],_HL,_VC,_R,_N,[]).

generate_rules_fact([Head:_P1,'':_P2],HeadList,VC,R,N,[Clause]):-!,
  Clause=(Head:-(sample_head(R,VC,HeadList,N))).

generate_rules_fact([Head:_P|T],HeadList,VC,R,N,[Clause|Clauses]):-
  Clause=(Head:-(sample_head(R,VC,HeadList,N))),
  N1 is N+1,
  generate_rules_fact(T,HeadList,VC,R,N1,Clauses).


generate_rules_fact_db([],_Env,_VC,_R,_Probs,_N,[],_Module).

generate_rules_fact_db([Head:_P1,'':_P2],Env,VC,R,Probs,N,[Clause],Module):-!,
  add_bdd_arg_db(Head,Env,BDD,_DB,Module,Head1),
  Clause=(Head1:-(get_var_n(Env,R,VC,Probs,V),equality(Env,V,N,BDD))).

generate_rules_fact_db([Head:_P|T],Env,VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg_db(Head,Env,BDD,_DB,Module,Head1),
  Clause=(Head1:-(get_var_n(Env,R,VC,Probs,V),equality(Env,V,N,BDD))),
  N1 is N+1,
  generate_rules_fact_db(T,Env,VC,R,Probs,N1,Clauses,Module).


generate_clause(Head,Body,HeadList,VC,R,N,Clause):-
  Clause=(Head:-(Body,sample_head(R,VC,HeadList,N))).

generate_clause_gauss(Head,Body,VC,R,Var,Mean,Variance,Clause):-
  Clause=(Head:-(Body,sample_gauss(R,VC,Mean,Variance,Var))).

generate_clause_uniform(Head,Body,VC,R,Var,L,U,Clause):-
  Clause=(Head:-(Body,sample_uniform(R,VC,L,U,Var))).



generate_clause_db(Head,Env,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module):-
  add_bdd_arg_db(Head,Env,BDD,DBH,Module,Head1),
  Clause=(Head1:-(DBH>=1,DB is DBH-1,Body,get_var_n(Env,R,VC,Probs,V),equality(Env,V,N,B),and(Env,BDDAnd,B,BDD))).


generate_rules([],_Body,_HeadList,_VC,_R,_N,[]).

generate_rules([Head:_P1,'':_P2],Body,HeadList,VC,R,N,[Clause]):-!,
  generate_clause(Head,Body,HeadList,VC,R,N,Clause).

generate_rules([Head:_P|T],Body,HeadList,VC,R,N,[Clause|Clauses]):-
  generate_clause(Head,Body,HeadList,VC,R,N,Clause),
  N1 is N+1,
  generate_rules(T,Body,HeadList,VC,R,N1,Clauses).


generate_rules_db([],_Env,_Body,_VC,_R,_Probs,_DB,_BDDAnd,_N,[],_Module):-!.

generate_rules_db([Head:_P1,'':_P2],Env,Body,VC,R,Probs,DB,BDDAnd,N,[Clause],Module):-!,
  generate_clause_db(Head,Env,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module).

generate_rules_db([Head:_P|T],Env,Body,VC,R,Probs,DB,BDDAnd,N,[Clause|Clauses],Module):-
  generate_clause_db(Head,Env,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module),!,%agg.cut
  N1 is N+1,
  generate_rules_db(T,Env,Body,VC,R,Probs,DB,BDDAnd,N1,Clauses,Module).



process_body([],BDD,BDD,Vars,Vars,[],_Env,_Module).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Env,Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).
  
process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Env,Module):-
  db(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[
(((neg(H1);\+ H1),one(Env,BDDN));(bagof(BDDH,H2,L)->or_list(L,Env,BDDL),bdd_not(Env,BDDL,BDDN);one(Env,BDDN))),
  and(Env,BDD,BDDN,BDD2)
  |Rest],Env,Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg(H,Env,BDDH,Module,H2),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[
  neg(H1)|Rest],Env,Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([\+ H|T],BDD,BDD1,Vars,[BDDH,BDDN,L,BDDL,BDD2|Vars1],
[(bagof(BDDH,H1,L)->or_list(L,Env,BDDL),bdd_not(Env,BDDL,BDDN);one(Env,BDDN)),
  and(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
  add_bdd_arg(H,Env,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Env,Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Env,Module):-
  db(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,
[((H1,one(Env,BDDH));H2),and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg(H,Env,BDDH,Module,H2),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,
[H1|Rest],Env,Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H1|Rest],Env,Module):-
  add_mod_arg(H,Module,H1),
  db(H1),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,[BDDH,BDD2|Vars1],
[H1,and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-
  add_bdd_arg(H,Env,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module).

process_body_db([],BDD,BDD,_DB,Vars,Vars,[],_Env,_Module):-!.

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Env,Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).
  
process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Env,Module):-
  db(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[
(((neg(H1);\+ H1),one(Env,BDDN));(bagof(BDDH,H2,L)->or_list(L,Env,BDDL),bdd_not(Env,BDDL,BDDN);one(Env,BDDN))),
  and(Env,BDD,BDDN,BDD2)
  |Rest],Env,Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H2),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[
  neg(H1)|Rest],Env,Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,[BDDH,BDDN,L,BDDL,BDD2|Vars1],
[(bagof(BDDH,H1,L)->or_list(L,Env,BDDL),bdd_not(Env,BDDL,BDDN);one(Env,BDDN)),
  and(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([],BDD,BDD,_DB,Vars,Vars,[],_Env,_Module):-!.

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Env,Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).
  
process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Env,Module):-
  db(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[
(((neg(H1);\+ H1),one(Env,BDDN));(bagof(BDDH,H2,L)->or_list(L,Env,BDDL),bdd_not(Env,BDDL,BDDN);one(Env,BDDN))),
  and(Env,BDD,BDDN,BDD2)
  |Rest],Env,Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H2),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[
  neg(H1)|Rest],Env,Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,[BDDH,BDDN,L,BDDL,BDD2|Vars1],
[(bagof(BDDH,H1,L)->or_list(L,Env,BDDL),bdd_not(Env,BDDL,BDDN);one(Env,BDDN)),
  and(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  db(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,
[((H1,one(Env,BDDH));H2),and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H2),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,
[H1|Rest],Env,Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,[BDDH,BDD2|Vars1],
[H1,and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-!, %agg. cut
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  db(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,
[((H1,one(Env,BDDH));H2),and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H2),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,
[H1|Rest],Env,Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,[BDDH,BDD2|Vars1],
[H1,and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-!, %agg. cut
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).


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

add_prob(P,A,A:P).
/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([H], Prob, [Head:ProbHead1|Null]) :-
  (H=Head:ProbHead;H=ProbHead::Head),!,
  ProbHead1 is ProbHead,
  ProbLast is 1 - Prob - ProbHead1,
  mc_input_mod(M),
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


/** 
 * set_mc(++Parameter:atom,+Value:term) is det
 *
 * The predicate sets the value of a parameter
 * For a list of parameters see 
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or 
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
set_mc(Parameter,Value):-
  mc_input_mod(M),
  retract(M:local_mc_setting(Parameter,_)),
  assert(M:local_mc_setting(Parameter,Value)).

/** 
 * setting_mc(?Parameter:atom,?Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see 
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or 
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_mc(P,V):-
  mc_input_mod(M),
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

set_sw(A,B):-
  mc_module(M),
  assert(M:sw(A,B)).

msw(M:A,B):-
  M:values(A,V),
  M:sw(A,D),
  sample_msw(V,D,B).

sample_msw(real,norm(Mean,Variance),V):-!,
  gauss(Mean,Variance,S),
  S=V.

sample_msw(Values,Dist,V):-
  maplist(combine,Values,Dist,VD),
  sample(VD,N),
  nth0(N,Values,V).

combine(V,P,V:P).

msw_weight(M:A,B,W):-
  M:values(A,V),
  M:sw(A,D),
  msw_weight(V,D,B,W).

msw_weight(real,norm(Mean,Variance),V,W):-!,
  gauss_density(Mean,Variance,V,W).

msw_weight(Values,Dist,V,W):-
  maplist(combine,Values,Dist,VD),
  member(V:W,VD).

user:term_expansion((:- mc), []) :-!,
  prolog_load_context(module, M),
  retractall(local_mc_setting(_,_)),
  findall(local_mc_setting(P,V),default_setting_mc(P,V),L),
  assert_all(L,M,_),
  retractall(mc_input_mod(_)),
  assert(mc_input_mod(M)),
  retractall(M:rule_n(_)),
  assert(M:rule_n(0)),
  style_check(-discontiguous).

user:term_expansion((:- begin_lpad), []) :-
  mc_input_mod(M),!,
  assert(mc_module(M)).

user:term_expansion((:- end_lpad), []) :-
  mc_input_mod(_M0),!,
  retractall(mc_module(_M)).

user:term_expansion((Head:=Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% fact with uniform distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~uniform(L,U)), !, 
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_uniform(H1,Body,[],R,Var,L,U,Clause)
  ;
    generate_clause_uniform(H1,Body,VC,R,Var,L,U,Clause)
  ).

user:term_expansion((Head:=Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~gamma(Shape,Scale)), !, 
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-Body,sample_gamma(R,[],Shape,Scale,Var))
  ;
    Clause=(H1:-Body,sample_gamma(R,VC,Shape,Scale,Var))
  ).

user:term_expansion((Head:=Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~beta(Alpha,Beta)), !, 
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-Body,sample_beta(R,[],Alpha,Beta,Var))
  ;
    Clause=(H1:-Body,sample_beta(R,VC,Alpha,Beta,Var))
  ).


user:term_expansion((Head:=Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~poisson(Lambda)), !, 
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-Body,sample_poisson(R,[],Lambda,Var))
  ;
    Clause=(H1:-Body,sample_poisson(R,VC,Lambda,Var))
  ).

user:term_expansion((Head:=Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~uniform(D0)),!, 
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-Body,length(D0,Len),Prob is 1.0/Len,
      maplist(add_prob(Prob),D0,D),sample_discrete(R,[],D,Var))
  ;
    Clause=(H1:-Body,length(D0,Len),Prob is 1.0/Len,
      maplist(add_prob(Prob),D0,D),sample_discrete(R,VC,D,Var))
  ).

user:term_expansion((Head:=Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~finite(D0)),!, 
  add_arg(H,Var,H1),
  extract_vars_list([Head],[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-Body,maplist(swap,D0,D),sample_discrete(R,[],D,Var))
  ;
    Clause=(H1:-Body,maplist(swap,D0,D),sample_discrete(R,VC,D,Var))
  ).

user:term_expansion((Head:=Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~dirichlet(Par)), !, 
  add_arg(H,Var,H1),
  extract_vars_list([H],[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-Body,sample_dirichlet(R,[],Par,Var))
  ;
    Clause=(H1:-Body,sample_dirichlet(R,VC,Par,Var))
  ).

user:term_expansion((Head:=Body),(H1:-Body)) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~val(Var)), !, 
  add_arg(H,Var,H1).

user:term_expansion((Head:=Body),(Head:- Body)) :- 
  prolog_load_context(module, M),mc_module(M),!.

user:term_expansion((Head:-Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:P),
  nonvar(P),
  Head=(H:gaussian(Mean,Variance)), !, 
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gauss(H1,Body,[],R,Var,Mean,Variance,Clause)
  ;
    generate_clause_gauss(H1,Body,VC,R,Var,Mean,Variance,Clause)
  ).


user:term_expansion((Head:-Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% fact with uniform distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:uniform(Var,L,U)), !, 
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_uniform(H,Body,[],R,Var,L,U,Clause)
  ;
    generate_clause_uniform(H,Body,VC,R,Var,L,U,Clause)
  ).

user:term_expansion((Head:-Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:gamma(Var,Shape,Scale)), !, 
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-Body,sample_gamma(R,[],Shape,Scale,Var))
  ;
    Clause=(H:-Body,sample_gamma(R,VC,Shape,Scale,Var))
  ).

user:term_expansion((Head:-Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:beta(Var,Alpha,Beta)), !, 
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-Body,sample_beta(R,[],Alpha,Beta,Var))
  ;
    Clause=(H:-Body,sample_beta(R,VC,Alpha,Beta,Var))
  ).


user:term_expansion((Head:-Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:poisson(Var,Lambda)), !, 
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-Body,sample_poisson(R,[],Lambda,Var))
  ;
    Clause=(H:-Body,sample_poisson(R,VC,Lambda,Var))
  ).

user:term_expansion((Head:-Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:uniform(Var,D0)),!, 
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-Body,length(D0,Len),Prob is 1.0/Len,
      maplist(add_prob(Prob),D0,D),sample_discrete(R,[],D,Var))
  ;
    Clause=(H:-Body,length(D0,Len),Prob is 1.0/Len,
      maplist(add_prob(Prob),D0,D),sample_discrete(R,VC,D,Var))
  ).

user:term_expansion((Head:-Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head=(H:discrete(Var,D));Head=(H:finite(Var,D))),!, 
  extract_vars_list([Head],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-Body,sample_discrete(R,[],D,Var))
  ;
    Clause=(H:-Body,sample_discrete(R,VC,D,Var))
  ).

user:term_expansion((Head:-Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:dirichlet(Var,Par)), !, 
  extract_vars_list([H],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-Body,sample_dirichlet(R,[],Par,Var))
  ;
    Clause=(H:-Body,sample_dirichlet(R,VC,Par,Var))
  ).

user:term_expansion((Head:-Body),Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:gaussian(Var,Mean,Variance)), !, 
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gauss(H,Body,[],R,Var,Mean,Variance,Clause)
  ;
    generate_clause_gauss(H,Body,VC,R,Var,Mean,Variance,Clause)
  ).


user:term_expansion((Head :- Body), Clauses):-
  prolog_load_context(module, M),mc_module(M),
  M:local_mc_setting(depth_bound,true),
% disjunctive clause with more than one head atom e depth_bound
  Head = (_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,Env,Module),
  append([one(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (M:local_mc_setting(single_var,true)->
    generate_rules_db(HeadList,Env,Body1,[],R,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_rules_db(HeadList,Env,Body1,VC,R,Probs,DB,BDDAnd,0,Clauses,Module)
   ).
  
user:term_expansion((Head :- Body), Clauses):-
  prolog_load_context(module, M),mc_module(M),
% disjunctive clause with more than one head atom senza depth_bound
  Head = (_;_), !,
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list((Head :- Body),[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_rules(HeadList,Body,HeadList,[],R,0,Clauses)
  ;
    generate_rules(HeadList,Body,HeadList,VC,R,0,Clauses)
  ).

user:term_expansion((Head :- Body), []) :- 
% disjunctive clause with a single head atom con prob. 0 senza depth_bound --> la regola e' non  caricata nella teoria e non e' conteggiata in NR
  prolog_load_context(module, M),mc_module(M),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  (Head = (_:P);Head=(P::_)),
  ground(P),
  P=:=0.0, !. 

user:term_expansion((Head :- Body), Clauses) :- 
% disjunctive clause with a single head atom e depth_bound
  prolog_load_context(module, M),mc_module(M),
  M:local_mc_setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(H,Env,BDDAnd,DB,Module,Head1),
  Clauses=(Head1 :- Body1).

user:term_expansion((Head :- Body), Clauses) :- 
% disjunctive clause with a single head atom senza depth_bound con prob =1
  prolog_load_context(module, M),mc_module(M),
   ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,Env,BDDAnd,Module,Head1),
  Clauses=(Head1 :- Body1).

user:term_expansion((Head :- Body), Clauses) :- 
% disjunctive clause with a single head atom e DB, con prob. diversa da 1
  prolog_load_context(module, M),mc_module(M),
  M:local_mc_setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  (Head = (H:_);Head=(_::H)), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),%***test single_var
  (M:local_mc_setting(single_var,true)->
    generate_clause_db(H,Env,Body2,[],R,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_clause_db(H,Env,Body2,VC,R,Probs,DB,BDDAnd,0,Clauses,Module)
  ).

user:term_expansion((Head :- Body), Clauses) :- 
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  prolog_load_context(module, M),mc_module(M),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  (Head = (H:_);Head = (_::H)), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list((Head :- Body),[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_clause(H,Body,HeadList,[],R,0,Clauses)
  ;
    generate_clause(H,Body,HeadList,VC,R,0,Clauses)
  ).
  
user:term_expansion((Head :- Body),Clauses) :- 
% definite clause with depth_bound
  prolog_load_context(module, M),mc_module(M),  
  M:local_mc_setting(depth_bound,true),
   ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(Head,Env,BDDAnd,DB,Module,Head1),
  Clauses=(Head1 :- Body1).
  
user:term_expansion(Head,Clauses) :- 
  prolog_load_context(module, M),mc_module(M),
  M:local_mc_setting(depth_bound,true),
% disjunctive FACT with more than one head atom e db
  Head=(_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (M:local_mc_setting(single_var,true)->
    generate_rules_fact_db(HeadList,_Env,[],R,Probs,0,Clauses,_Module)
  ;
    generate_rules_fact_db(HeadList,_Env,VC,R,Probs,0,Clauses,_Module)
  ).

user:term_expansion(Head,Clauses) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with more than one head atom senza db
  Head=(_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_rules_fact(HeadList,HeadList,[],R,0,Clauses)
  ;
    generate_rules_fact(HeadList,HeadList,VC,R,0,Clauses)
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% fact with uniform distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~uniform(L,U)), !, 
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_uniform(H1,true,[],R,Var,L,U,Clause)
  ;
    generate_clause_uniform(H1,true,VC,R,Var,L,U,Clause)
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~gamma(Shape,Scale)), !, 
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-sample_gamma(R,[],Shape,Scale,Var))
  ;
    Clause=(H1:-sample_gamma(R,VC,Shape,Scale,Var))
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~beta(Alpha,Beta)), !, 
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-sample_beta(R,[],Alpha,Beta,Var))
  ;
    Clause=(H1:-sample_beta(R,VC,Alpha,Beta,Var))
  ).


user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~poisson(Lambda)), !, 
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-sample_poisson(R,[],Lambda,Var))
  ;
    Clause=(H1:-sample_poisson(R,VC,Lambda,Var))
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~uniform(D0)),!, 
  add_arg(H,Var,H1),
  length(D0,Len),
  Prob is 1.0/Len,
  maplist(add_prob(Prob),D0,D),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-length(D0,Len),Prob is 1.0/Len,
       maplist(add_prob(Prob),D0,D),sample_discrete(R,[],D,Var))
  ;
    Clause=(H1:-length(D0,Len),Prob is 1.0/Len,
       maplist(add_prob(Prob),D0,D),sample_discrete(R,VC,D,Var))
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H~finite(D0)),!, 
  add_arg(H,Var,H1),
  extract_vars_list([Head],[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-maplist(swap,D0,D),sample_discrete(R,[],D,Var))
  ;
    Clause=(H1:-maplist(swap,D0,D),sample_discrete(R,VC,D,Var))
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:P),
  nonvar(P),
  Head=(H:dirichlet(Var,Par)), !, 
  extract_vars_list([H],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-sample_dirichlet(R,[],Par,Var))
  ;
    Clause=(H:-sample_dirichlet(R,VC,Par,Var))
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:P),
  nonvar(P),
  Head=(H:gaussian(Var,Mean,Variance)), !, 
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gauss(H,true,[],R,Var,Mean,Variance,Clause)
  ;
    generate_clause_gauss(H,true,VC,R,Var,Mean,Variance,Clause)
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% fact with uniform distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:P),
  nonvar(P),
  Head=(H:uniform(Var,L,U)), !, 
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_uniform(H,true,[],R,Var,L,U,Clause)
  ;
    generate_clause_uniform(H,true,VC,R,Var,L,U,Clause)
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:P),
  nonvar(P),
  Head=(H:gamma(Var,Shape,Scale)), !, 
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-sample_gamma(R,[],Shape,Scale,Var))
  ;
    Clause=(H:-sample_gamma(R,VC,Shape,Scale,Var))
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:P),
  nonvar(P),
  Head=(H:beta(Var,Alpha,Beta)), !, 
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-sample_beta(R,[],Alpha,Beta,Var))
  ;
    Clause=(H:-sample_beta(R,VC,Alpha,Beta,Var))
  ).


user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:P),
  nonvar(P),
  Head=(H:poisson(Var,Lambda)), !, 
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-sample_poisson(R,[],Lambda,Var))
  ;
    Clause=(H:-sample_poisson(R,VC,Lambda,Var))
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:P),
  nonvar(P),
  Head=(H:uniform(Var,D0)),!, 
  length(D0,Len),
  Prob is 1.0/Len,
  maplist(add_prob(Prob),D0,D),
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-sample_discrete(R,[],D,Var))
  ;
    Clause=(H:-sample_discrete(R,VC,D,Var))
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:P),
  nonvar(P),
  (Head=(H:discrete(Var,D));Head=(H:finite(Var,D))),!, 
  extract_vars_list([Head],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-sample_discrete(R,[],D,Var))
  ;
    Clause=(H:-sample_discrete(R,VC,D,Var))
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:P),
  nonvar(P),
  Head=(H:dirichlet(Var,Par)), !, 
  extract_vars_list([H],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-sample_dirichlet(R,[],Par,Var))
  ;
    Clause=(H:-sample_dirichlet(R,VC,Par,Var))
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:P),
  nonvar(P),
  Head=(H:gaussian(Var,Mean,Variance)), !, 
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gauss(H,true,[],R,Var,Mean,Variance,Clause)
  ;
    generate_clause_gauss(H,true,VC,R,Var,Mean,Variance,Clause)
  ).

user:term_expansion(Head,[]) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with a single head atom con prob. 0
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head = (_:P); Head = (P::_)),
  ground(P),
  P=:=0.0, !.
  
user:term_expansion(Head,H) :- 
  prolog_load_context(module, M),mc_module(M),
  M:local_mc_setting(depth_bound,true),
% disjunctive fact with a single head atom con prob.1 e db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head = (H:P);Head = (P::H)),
  ground(P),
  P=:=1.0, !.

user:term_expansion(Head,H) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head = (H:P);Head =(P::H)),
  ground(P),
  P=:=1.0, !.

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
  M:local_mc_setting(depth_bound,true),
% disjunctive fact with a single head atom e prob. generiche, con db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head=(H:_);Head=(_::H)), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(Head,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_clause(H,true,HeadList,[],R,0,Clause)
  ;
    generate_clause(H,true,HeadList,VC,R,0,Clause)
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with a single head atom e prob. generiche, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head=(H:_);Head=(_::H)), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  (M:local_mc_setting(single_var,true)->
    generate_clause(H,true,HeadList,[],R,0,Clause)
  ;
    generate_clause(H,true,HeadList,VC,R,0,Clause)
  ).

user:term_expansion((:- set_pita(P,V)), []) :-!,
  prolog_load_context(module, M),mc_module(M),
  set_pita(P,V).


/** 
 * begin_lpad_pred is det
 *
 * Initializes LPAD loading.
 */
begin_lpad_pred:-
  M=user,
  mc_input_mod(M),
  assert(mc_module(M)).

/** 
 * end_lpad_pred is det
 *
 * Terminates the cplint inference module.
 */
end_lpad_pred:-
  retractall(mc_module(_M)).

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
builtin_int(mc_prob(_,_)).
builtin_int(mc_sample(_,_,_)).
builtin_int(G):-
  predicate_property(G,built_in).
builtin_int(G):-
  predicate_property(G,imported_from(lists)).
builtin_int(G):-
  predicate_property(G,imported_from(apply)).
builtin_int(G):-
  predicate_property(G,imported_from(nf_r)).




average(L,Av):-
        sum_list(L,Sum),
        length(L,N),
        Av is Sum/N.
/** 
 * histogram(+List:list,+NBins:int,-Chart:dict) is det
 *
 * Draws a histogram of the samples in List dividing the domain in
 * NBins bins. List must be a list of couples of the form [V]-W
 * where V is a sampled value and W is its weight.
 */
histogram(L0,NBins,Chart):-
  maplist(to_pair,L0,L1),
  maplist(key,L1,L2),
  max_list(L2,Max),
  min_list(L2,Min),
  keysort(L1,L),
  D is Max-Min,
  BinWidth is D/NBins,
  bin(NBins,L,Min,BinWidth,LB),
  maplist(key,LB,X),
  maplist(y,LB,Y),
  Chart = c3{data:_{x:x, 
    columns:[[x|X],[freq|Y]], type:bar},
    axis:_{ x:_{ tick:_{fit:false}}},
     bar:_{
     width:_{ ratio: 1.0 }},
     legend:_{show: false}}.

/** 
 * densities(+PriorList:list,+PostList:list,+NBins:int,-Chart:dict) is det
 *
 * Draws a line chart of the density of two sets of samples, usually
 * prior and post observations. The samples from the prior are in PriorList
 * as couples [V]-W, while the samples from the posterior are in PostList
 * as couples V-W where V is a value and W its weigth.
 * The lines are drawn dividing the domain in
 * NBins bins. 
 */
densities(Pri0,Post0,NBins,Chart):-
  maplist(to_pair,Pri0,Pri1),
  maplist(key,Pri1,Pri),
  maplist(key,Post0,Post),
  append(Pri,Post,All),
  max_list(All,Max),
  min_list(All,Min),
  D is Max-Min,
  BinWidth is D/NBins,
  keysort(Pri1,Pr),
  keysort(Post0,Po),
  bin(NBins,Pr,Min,BinWidth,LPr),
  bin(NBins,Po,Min,BinWidth,LPo),
  maplist(key,LPr,X),
  maplist(y,LPr,YPr),
  maplist(y,LPo,YPo),
  Chart = c3{data:_{x: x,
  columns: [[x|X],
    [pre|YPr],[post|YPo]]},
   axis:_{ x:_{ tick:_{fit:false}}}
  }.


to_pair([E]-W,E-W).

key(K-_,K).

y(_ - Y,Y).

bin(0,_L,_Min,_BW,[]):-!.

bin(N,L,Lower,BW,[V-Freq|T]):-
  V is Lower+BW/2,
  Upper is Lower+BW,
  count_bin(L,Upper,0,Freq,L1),
  N1 is N-1,
  bin(N1,L1,Upper,BW,T).

count_bin([],_U,F,F,[]).

count_bin([H-W|T0],U,F0,F,T):-
  (H>=U->
    F=F0,
    T=[H-W|T0]
  ;
    F1 is F0+W,
    count_bin(T0,U,F1,F,T)
  ).

swap(A:B,B:A).

(M:A) ~= B :-
  A=..L,
  append(L,[B],L1),
  A1=..L1,
  M:A1.

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(mcintyre:set_mc(_,_)).
sandbox:safe_primitive(mcintyre:setting_mc(_,_)).
sandbox:safe_primitive(mcintyre:histogram(_,_,_)).
sandbox:safe_primitive(mcintyre:densities(_,_,_,_)).
sandbox:safe_primitive(mcintyre:set_sw(_,_)).

:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(mcintyre:s(_,_), []).
sandbox:safe_meta(mcintyre:mc_prob(_,_), []).
sandbox:safe_meta(mcintyre:mc_prob_bar(_,_), []).
sandbox:safe_meta(mcintyre:mc_sample(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_rejection_sample(_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_rejection_sample(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_sample(_,_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_sample(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample(_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_bar(_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_bar(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_first(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_first_bar(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_one(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_one_bar(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_raw(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_rejection_sample_arg(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_rejection_sample_arg_bar(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_sample_arg(_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_sample_arg_bar(_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_expectation(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_expectation(_,_,_,_,_,_), []).

sandbox:safe_meta(mcintyre:mc_lw_sample(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_lw_sample_arg(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_lw_sample_arg_log(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_lw_expectation(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:msw(_,_), []).

