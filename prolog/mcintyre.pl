/** <module> pita

This module performs reasoning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It reads probabilistic program andcomputes the probability of queries.

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
  mc_sample/3,mc_sample_bar/3,
  mc_sample_arg/4,mc_sample_arg_bar/4,
  mc_mh_sample/7,
  mc_rejection_sample_arg/5,mc_rejection_sample_arg_bar/5,
  mc_mh_sample_arg/6,mc_mh_sample_arg_bar/6,
  mc_sample_arg_first/4,mc_sample_arg_first_bar/4,
  mc_sample_arg_one/4,mc_sample_arg_one_bar/4,
  mc_expectation/4,
  mc_mh_expectation/6,
  set_mc/2,setting_mc/2,
  mc_load/1,mc_load_file/1,
  sample_gauss/5,
  sample_head/4,
  mc_lw_sample_arg/5
  ]).
:-meta_predicate s(:,-).
:-meta_predicate mc_prob(:,-).
:-meta_predicate mc_prob_bar(:,-).
:-meta_predicate mc_sample(:,+,-,-,-).
:-meta_predicate mc_rejection_sample(:,:,+,-,-,-).
:-meta_predicate mc_mh_sample(:,:,+,+,-,-,-).
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
:-meta_predicate mc_expectation(:,+,+,-).
:-meta_predicate mc_mh_expectation(:,:,+,+,+,-).
:-meta_predicate montecarlo_cycle(-,-,:,-,-,-,-,-,-).
:-meta_predicate montecarlo(-,-,-,:,-,-).
:-meta_predicate initial_sample_cycle(:).
:-meta_predicate initial_sample(:).
:-meta_predicate initial_sample_neg(:).

:-meta_predicate mc_lw_sample_arg(:,:,+,+,-).
:-meta_predicate lw_sample_cycle(:).
:-meta_predicate lw_sample_weight_cycle(:,-).
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
  montecarlo(S,0, 0, M:Goal, N, T),
  P is T / N,
  F is N - T,
  erase_samples.

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

initial_sample(_M:(sample_head(R,VC,HL,NH),NH=N)):-!,
  sample_head(R,VC,HL,NH),
  NH=N.

initial_sample(_M:sample_gauss(R,VC,Mean,Variance,S)):-!,
  sample_gauss(R,VC,Mean,Variance,S).

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

initial_sample_neg(_M:(sample_head(R,VC,HL,NH),NH=N)):-!,
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
 * mc_lw_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,+Lag:int,?Arg:var,-Values:list) is det
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
mc_lw_sample_arg(M:Goal,M:Evidence,S,Arg,ValList):-
  lw_sample_arg(S,M:Goal,M:Evidence,Arg,ValList0),
  foldl(agg_val,ValList0,0,Sum),
  Norm is S/Sum,
  maplist(norm(Norm),ValList0,ValList).

agg_val(_ -N,S,S+N).

norm(NF,V-W,V-W1):-
  W1 is W*NF.

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

lw_sample(_M:true):-!.

lw_sample(_M:(sample_head(R,VC,_HL,NH),NH=N)):-!,
  check(R,VC,N).

lw_sample(_M:sample_gauss(R,VC,Mean,Variance,S)):-!,
  sample_gauss(R,VC,Mean,Variance,S).

lw_sample(M:(G1,G2)):-!,
  lw_sample(M:G1),
  lw_sample(M:G2).

lw_sample(M:(G1;G2)):-!,
  lw_sample(M:G1);
  lw_sample(M:G2).

lw_sample(M:(\+ G)):-!,
  lw_sample_neg(M:G).

lw_sample(_M:G):-
  builtin(G),!,
  call(G).

lw_sample(M:G):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  lw_sample(M:B).

lw_sample_neg(_M:true):-!,
  fail.

lw_sample_neg(_M:(sample_head(R,VC,HL,NH),NH=N)):-!,
  sample_head(R,VC,HL,NH),
  NH\=N.

lw_sample_neg(_M:sample_gauss(R,VC,Mean,Variance,S)):-!,
  sample_gauss(R,VC,Mean,Variance,S).


lw_sample_neg(M:(G1,G2)):-!,
  (lw_sample_neg(M:G1);
  lw_sample_neg(M:G2)).

lw_sample_neg(M:(G1;G2)):-!,
  lw_sample_neg(M:G1),
  lw_sample_neg(M:G2).

lw_sample_neg(M:(\+ G)):-!,
  lw_sample(M:G).

lw_sample_neg(_M:G):-
  builtin(G),!,
  \+ call(G).

lw_sample_neg(M:G):-
  findall(B,M:clause(G,B),L),
  lw_sample_neg_all(L,M).

lw_sample_neg_all([],_M).

lw_sample_neg_all([H|T],M):-
  lw_sample_neg(M:H),
  lw_sample_neg_all(T,M).


lw_sample_weight_cycle(M:G,W):-
  (lw_sample_weight(M:G,1,W)->
    true
  ;
    erase_samples,
    lw_sample_weight_cycle(M:G,W)
  ).

lw_sample_weight(_M:true,W,W):-!.

lw_sample_weight(_M:(sample_head(R,VC,_HL,NH),NH=N),W,W):-!,
  check(R,VC,N).

lw_sample_weight(_M:sample_gauss(R,VC,Mean,Variance,S),W0,W):-!,
  (var(S)->
    sample_gauss(R,VC,Mean,Variance,S),
    W=W0
  ;
    gauss_density(Mean,Variance,S,D),
    W is W0*D
   ).

lw_sample_weight(M:(G1,G2),W0,W):-!,
  lw_sample_weight(M:G1,W0,W1),
  lw_sample_weight(M:G2,W1,W).

lw_sample_weight(M:(G1;G2),W0,W):-!,
  lw_sample_weight(M:G1,W0,W);
  lw_sample_weight(M:G2,W0,W).

lw_sample_weight(_M:G,W,W):-
  builtin(G),!,
  call(G).

lw_sample_weight(M:G,W0,W):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  lw_sample_weight(M:B,W0,W).



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
sample_head(R,VC,_HeadList,NH):-
  recorded(R,sampled(VC,NH)),!.

sample_head(R,VC,HeadList,NH):-
  sample(HeadList,NH),
  recorda(R,sampled(VC,NH)).

sample(HeadList, HeadId) :-
  random(Prob), 
  sample(HeadList, 0, 0, Prob, HeadId), !.

sample([_HeadTerm:HeadProb|Tail], Index, Prev, Prob, HeadId) :-
	Succ is Index + 1,
	Next is Prev + HeadProb,
	(Prob =< Next ->
		HeadId = Index;
		sample(Tail, Succ, Next, Prob, HeadId)).


sample_uniform(R,VC,_L,_U,S):-
  recorded(R,sampled(VC,S)),!.

sample_uniform(R,VC,L,U,S):-
  random(V),
  S is L+V*(U-L),
  recorda(R,sampled(VC,S)).

uniform_density(L,U,D):-
  D is 1/(U-L).

sample_gauss(R,VC,_Mean,_Variance,S):-
  recorded(R,sampled(VC,S)),!.

sample_gauss(R,VC,Mean,Variance,S):-
  gauss(Mean,Variance,S),
  recorda(R,sampled(VC,S)).

gauss(Mean,Variance,S):-
  random(U1),
  random(U2),
  R is sqrt(-2*log(U1)),
  Theta is 2*pi*U2,
  S0 is R*cos(Theta),
  StdDev is sqrt(Variance),
  S is StdDev*S0+Mean.

gauss_density(Mean,Variance,S,D):-
  StdDev is sqrt(Variance),
  D is 1/(StdDev*sqrt(2*pi))*exp(-(S-Mean)*(S-Mean)/(2*Variance)).

generate_rules_fact([],_HL,_VC,_R,_N,[]).

generate_rules_fact([Head:_P1,'':_P2],HeadList,VC,R,N,[Clause]):-!,
  Clause=(Head:-(sample_head(R,VC,HeadList,NH),NH=N)).

generate_rules_fact([Head:_P|T],HeadList,VC,R,N,[Clause|Clauses]):-
  Clause=(Head:-(sample_head(R,VC,HeadList,NH),NH=N)),
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
  Clause=(Head:-(Body,sample_head(R,VC,HeadList,NH),NH=N)).

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

prob_ann(_:P,P).

/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([Head:ProbHead], Prob, [Head:ProbHead1|Null]) :-!,
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

process_head_ground([Head:ProbHead|Tail], Prob, [Head:ProbHead1|Next]) :- 
  ProbHead1 is ProbHead,
  ProbNext is Prob + ProbHead1, 
  process_head_ground(Tail, ProbNext, Next).


ground_prob([]).

ground_prob([_Head:ProbHead|Tail]) :- 
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

user:term_expansion((:- mc), []) :-!,
  prolog_load_context(module, M),
  findall(local_mc_setting(P,V),default_setting_mc(P,V),L),
  assert_all(L,M,_),
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
  Head = (_H:P),
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
  Head = (H:_), !, 
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
  Head = (H:_), !, 
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
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:uniform(Var,L,U)), !, 
  extract_vars_list(Head,[],VC),
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
  Head=(H:gaussian(Var,Mean,Variance)), !, 
  extract_vars_list(Head,[],VC),
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
  Head = (_H:P),
  ground(P),
  P=:=0.0, !.
  
user:term_expansion(Head,H) :- 
  prolog_load_context(module, M),mc_module(M),
  M:local_mc_setting(depth_bound,true),
% disjunctive fact with a single head atom con prob.1 e db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),
  ground(P),
  P=:=1.0, !.

user:term_expansion(Head,H) :- 
  prolog_load_context(module, M),mc_module(M),
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),
  ground(P),
  P=:=1.0, !.

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),mc_module(M),
  M:local_mc_setting(depth_bound,true),
% disjunctive fact with a single head atom e prob. generiche, con db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:_), !, 
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
  Head=(H:_), !, 
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

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(mcintyre:set_mc(_,_)).
sandbox:safe_primitive(mcintyre:setting_mc(_,_)).

:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(mcintyre:s(_,_), []).
sandbox:safe_meta(mcintyre:mc_prob(_,_), []).
sandbox:safe_meta(mcintyre:mc_prob_bar(_,_), []).
sandbox:safe_meta(mcintyre:mc_sample(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_rejection_sample(_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_sample(_,_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample(_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_bar(_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_bar(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_first(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_first_bar(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_one(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_one_bar(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_rejection_sample_arg(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_rejection_sample_arg_bar(_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_sample_arg(_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_sample_arg_bar(_,_,_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_expectation(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_mh_expectation(_,_,_,_,_,_), []).

sandbox:safe_meta(mcintyre:mc_lw_sample_arg(_,_,_,_,_), []).

