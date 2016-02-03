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
  mc_sample/3,mc_sample_bar/3,
  mc_sample_arg/4,mc_sample_arg_bar/4,
  mc_sample_arg_first/4,mc_sample_arg_first_bar/4,
  mc_sample_arg_one/4,mc_sample_arg_one_bar/4,
  set_mc/2,setting_mc/2,
  mc_load/1,mc_load_file/1,
  sample_head/4
  ]).
:-meta_predicate s(:,-).
:-meta_predicate mc_prob(:,-).
:-meta_predicate mc_prob_bar(:,-).
:-meta_predicate mc_sample(:,+,-,-,-).
:-meta_predicate mc_sample(:,+,-).
:-meta_predicate mc_sample_bar(:,+,-).
:-meta_predicate mc_sample_arg(:,+,+,-).
:-meta_predicate mc_sample_arg_bar(:,+,+,-).
:-meta_predicate mc_sample_arg_first(:,+,+,-).
:-meta_predicate mc_sample_arg_first_bar(:,+,+,-).
:-meta_predicate mc_sample_arg_one(:,+,+,-).
:-meta_predicate mc_sample_arg_one_bar(:,+,+,-).
:-meta_predicate montecarlo_cycle(-,-,:,-,-,-,-,-,-).
:-meta_predicate montecarlo(-,-,-,:,-,-).
:-use_module(library(lists)).
:-use_module(library(rbtrees)).
:-use_module(library(apply)).
:-use_module(library(assoc)).

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


erase_samples:-
  recorded(_Key,_Val,Ref),
  erase(Ref),
  fail.

erase_samples.

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


to_atom(A0-N,A-N):-
  term_to_atom(A0,A).

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
 * mc_sample_arg_first(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query a number of Samples times. 
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples V-N where 
 * V is the value of Arg returned as the first answer by Query in 
 * a world sampled at random and N is the number of samples.
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
 * Query in a world sampled at random and N is the number of samples.
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

minus(A,B,A-B).

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

builitin_int(average(_L,_Av)).
builitin_int(mc_prob(_,_)).
builitin_int(mc_sample(_,_,_)).
builitin_int(G):-
  predicate_property(G,built_in).
builitin_int(G):-
  predicate_property(G,imported_from(lists)).
builitin_int(G):-
  predicate_property(G,imported_from(apply)).



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
sandbox:safe_meta(mcintyre:mc_sample(_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_bar(_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_bar(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_first(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_first_bar(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_one(_,_,_,_), []).
sandbox:safe_meta(mcintyre:mc_sample_arg_one_bar(_,_,_,_), []).

