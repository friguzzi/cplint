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


:- module(pita,[s/2, prob/2, prob_bar/2, set_pita/2,setting_pita/2,
  init/3,init_bdd/2,init_test/2,end/1,end_bdd/1,end_test/1,
  one/2,zero/2,and/4,or/4,bdd_not/3,
  ret_prob/3,get_var_n/5,equality/4,or_list/3, 
  em/9,randomize/1,
  load/1,load_file/1]).
:-meta_predicate s(:,-).
:-meta_predicate prob(:,-).
:-meta_predicate prob_bar(:,-).
:-use_module(library(lists)).
:-use_module(library(rbtrees)).
:-use_module(library(apply)).
:-use_foreign_library(foreign(bddem),install).

:- style_check(-discontiguous).

:- thread_local v/3,rule_n/1,pita_module/1,pita_input_mod/1,local_pita_setting/2.

/*:- multifile one/2,zero/2,and/4,or/4,bdd_not/3,init/3,init_bdd/2,init_test/1,
  end/1,end_bdd/1,end_test/0,ret_prob/3,em/9,randomize/1,
  get_var_n/5,add_var/5,equality/4.*/
%  remove/3.


/** 
 * init(++NumberOfRules:int,++NumberOfHeads:list,--Context:int) is det
 *
 * Initializes a data structure for performing parameter learning.
 * NumberOfRules is the number of rules of the model, 
 * NumberOfHeads is a list of integers, one for each rule, indicating the number
 * of head atoms in each rule.
 * It returns an integer in Context that is a pointer to a
 * context data structure for performing the EM algorithm.
 */

/** 
 * end(++Context:int) is det
 *
 * Terminates the context data structure for performing parameter learning.
 * Context is a pointer to a context data structure for performing 
 * the EM algorithm  
 * Context must have been returned by a call to init/3. 
 * It frees the memory occupied by Context.
 */

/** 
 * init_bdd(++Context:int,--Environment:int) is det
 *
 * Initializes an enviroment data structure for storing a BDD.
 * Context is an integer that is a pointer to a context data structure 
 * created using init/3. 
 * Returns an integer Environment that is a pointer to a data structure for 
 * storing a single BDD to be used for the EM algorithm.
 */

/** 
 * end_bdd(++Environment:int) is det
 *
 * Terminates the evnironment data structure for storing a BDD.
 * Environment is a pointer to a data structure returned by init_bdd/2. 
 * It frees the memory occupied by the BDD.
 */


/** 
 * init_test(++NumberOfRules:int,--Environment:int) is det
 *
 * Initializes a data structure for storing a single BDD.
 * NumberOfRules is the number of rules of the model, 
 * Returns an integer Environment that is a pointer to a data structure for 
 * storing a single BDD to be used for inference only (no learning).
 */

/** 
 * end_test(++Environment:int) is det
 *
 * Terminates the environment data structure for storing a single BDD.
 * Environment is a pointer to a data structure returned by a call 
 * to init_test/2. 
 */

/** 
 * one(++Environment:int,--One:int) is det
 *
 * Returns in One a pointer to a BDD belonging to environment Environment 
 * representing the one Boolean function 
 */

/** 
 * zero(++Environment:int,--Zero:int) is det
 *
 * Returns in Zero a pointer to a BDD belonging to environment Environment 
 * representing the zero Boolean function 
 */

/** 
 * and(++Environment:int,++A:int,++B:int,--AandB:int) is det
 *
 * Returns in AandB a pointer to a BDD belonging to environment Environment 
 * representing the conjunction of BDDs A and B
 */

/** 
 * or(++Environment:int,++A:int,++B:int,--AorB:int) is det
 *
 * Returns in AorB a pointer to a BDD belonging to environment Environment 
 * representing the disjunction of BDDs A and B
 */

/** 
 * ret_prob(++Environment:int,++BDD:int,-Probability:float) is det
 *
 * Returns the Probability of BDD belonging to environment Environment 
 */

/** 
 * bdd_not(++Environment:int,++A:int,--NotA:int) is det
 *
 * Returns in NotA a pointer to a BDD belonging to environment Environment 
 * representing the negation of BDD A 
 */

/** 
 * equality(++Environment:int,++Variable:int,++Value:int,--BDD:int) is det
 *
 * Returns in BDD the BDD belonging to environment Environment 
 * that represents the equation Variable=Value.
 */

/** 
 * em(++Context:int,++ListOfBDDs:list,++EA:float,++ER:float,++NumberOfBDDs:int,++Iterations:int,-LL:float,-Parameters:list,-ExampleProbabilities:list) is det
 *
 * Performs EM learning.
 * Takes as input the Context, a list of BDDs each representing one example,
 * the minimum absolute difference EA and relative difference ER between the 
 * log likelihood of examples in two different iterations, the number of BDDs
 * NumberOfBDDs (=length(ListOfBDDs)), and the maximum number of iterations
 * Iterations.
 * Returns the final log likelihood of examples LL, the list of new Parameters
 * and a list with the final probabilities of each example.
 * Parameters is a list whose elements are of the form [N,P] where N is the rule
 * number and P is a list of probabilities, one for each head atom of rule N, 
 * in reverse order.
 */

/** 
 * randomize(++Context:int) is det
 *
 * Randomizes the parameters of random variables associated to Context.
 */

/** 
 * add_var(++Environment:int,++NumberOfHeads:int,++ProbabilityDistribution:list, ++Rule:int,-Variable:int) is det.
 *
 * Returns in Variable the index of a new random variable in Environment with 
 * NumberOHeads values and probability distribution ProbabilityDistribution
 */

default_setting_pita(epsilon_parsing, 1e-5).
/* on, off */

default_setting_pita(bagof,false).
/* values: false, intermediate, all, extra */

default_setting_pita(compiling,off).

:-set_prolog_flag(unknown,warning).

default_setting_pita(depth_bound,false).  %if true, it limits the derivation of the example to the value of 'depth'
default_setting_pita(depth,5).
default_setting_pita(single_var,false). %false:1 variable for every grounding of a rule; true: 1 variable for rule (even if a rule has more groundings),simpler.

/** 
 * load(++File:atom) is det
 *
 * Loads File.lpad if it exists, otherwise loads File.cpl if it exists.
 */
load(File):-
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
 * load_file(++FileWithExtension:atom) is det
 *
 * Loads FileWithExtension.
 */
load_file(File):-
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
  M:rule_n(NR),
  init_test(NR,Env),
  findall((Goal,P),get_p(M:Goal,Env,P),L),
  end_test(Env),
  member((Goal,P),L).

/** 
 * prob(:Query:atom,-Probability:float) is nondet
 *
 * The predicate computes the probability of the ground query Query
 * If Query is not ground, it returns in backtracking all instantiations of
 * Query together with their probabilities
 */
prob(M:Goal,P):-
  s(M:Goal,P).

/** 
 * prob_bar(:Query:atom,-Probability:dict) is nondet
 *
 * The predicate computes the probability of the ground query Query
 * and returns it as a dict for rendering with c3 as a bar chart with 
 * a bar for the probability of Query true and a bar for the probability of 
 * Query false.
 * If Query is not ground, it returns in backtracking all instantiations of
 * Query together with their probabilities
 */
prob_bar(M:Goal,Chart):-
  s(M:Goal,P),
  PF is 1.0-P,
  Chart = c3{data:_{x:elem, rows:[elem-prob,'T'-P,'F' -PF], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,max:1.0,padding:_{bottom:0.0,top:0.0},
             tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}},
	           size:_{height: 100},
	          legend:_{show: false}}.


get_p(M:Goal,Env,P):-
  get_node(M:Goal,Env,BDD),
  ret_prob(Env,BDD,P).

load(FileIn,C1,R):-
  open(FileIn,read,SI),
  read_clauses_dir(SI,C),
  close(SI),
  process_clauses(C,[],C1,[],R).

get_node(Goal,Env,B):-
  pita_input_mod(M),
  M:local_pita_setting(depth_bound,true),!,
  M:local_pita_setting(depth,DB),
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
  pita_module(PName),
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


generate_rules_fact([],_Env,_VC,_R,_Probs,_N,[],_Module).

generate_rules_fact([Head:_P1,'':_P2],Env,VC,R,Probs,N,[Clause],Module):-!,
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(get_var_n(Env,R,VC,Probs,V),equality(Env,V,N,BDD))).

generate_rules_fact([Head:_P|T],Env,VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(get_var_n(Env,R,VC,Probs,V),equality(Env,V,N,BDD))),
  N1 is N+1,
  generate_rules_fact(T,Env,VC,R,Probs,N1,Clauses,Module).


generate_rules_fact_db([],_Env,_VC,_R,_Probs,_N,[],_Module).

generate_rules_fact_db([Head:_P1,'':_P2],Env,VC,R,Probs,N,[Clause],Module):-!,
  add_bdd_arg_db(Head,Env,BDD,_DB,Module,Head1),
  Clause=(Head1:-(get_var_n(Env,R,VC,Probs,V),equality(Env,V,N,BDD))).

generate_rules_fact_db([Head:_P|T],Env,VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg_db(Head,Env,BDD,_DB,Module,Head1),
  Clause=(Head1:-(get_var_n(Env,R,VC,Probs,V),equality(Env,V,N,BDD))),
  N1 is N+1,
  generate_rules_fact_db(T,Env,VC,R,Probs,N1,Clauses,Module).


generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module):-
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(Body,get_var_n(Env,R,VC,Probs,V),equality(Env,V,N,B),and(Env,BDDAnd,B,BDD))).


generate_clause_db(Head,Env,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module):-
  add_bdd_arg_db(Head,Env,BDD,DBH,Module,Head1),
  Clause=(Head1:-(DBH>=1,DB is DBH-1,Body,get_var_n(Env,R,VC,Probs,V),equality(Env,V,N,B),and(Env,BDDAnd,B,BDD))).


generate_rules([],_Env,_Body,_VC,_R,_Probs,_BDDAnd,_N,[],_Module).

generate_rules([Head:_P1,'':_P2],Env,Body,VC,R,Probs,BDDAnd,N,[Clause],Module):-!,
  generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module).

generate_rules([Head:_P|T],Env,Body,VC,R,Probs,BDDAnd,N,[Clause|Clauses],Module):-
  generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module),
  N1 is N+1,
  generate_rules(T,Env,Body,VC,R,Probs,BDDAnd,N1,Clauses,Module).


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
  
process_body([\+ db(H)|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Env,Module):-
  !,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([\+ H|T],BDD,BDD1,Vars,[BDDH,BDDN,L,BDDL,BDD2|Vars1],
[(bagof(BDDH,H1,L)->or_list(L,Env,BDDL),bdd_not(Env,BDDL,BDDN);one(Env,BDDN)),
  and(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
  add_bdd_arg(H,Env,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Env,Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([db(H)|T],BDD,BDD1,Vars,Vars1,[H|Rest],Env,Module):-
  !,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,[BDDH,BDD2|Vars1],
[H1,and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-
  add_bdd_arg(H,Env,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module).

process_body_db([],BDD,BDD,_DB,Vars,Vars,[],_Env,_Module):-!.

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Env,Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).
  
process_body_db([\+ db(H)|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Env,Module):-
  !,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,[BDDH,BDDN,L,BDDL,BDD2|Vars1],
[(bagof(BDDH,H1,L)->or_list(L,Env,BDDL),bdd_not(Env,BDDL,BDDN);one(Env,BDDN)),
  and(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([],BDD,BDD,_DB,Vars,Vars,[],_Env,_Module):-!.

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,[BDDH,BDDN,L,BDDL,BDD2|Vars1],
[(bagof(BDDH,H1,L)->or_list(L,Env,BDDL),bdd_not(Env,BDDL,BDDN);one(Env,BDDN)),
  and(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([db(H)|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  !,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,[BDDH,BDD2|Vars1],
[H1,and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-!, %agg. cut
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

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
  pita_input_mod(M),
  M:local_pita_setting(epsilon_parsing, Eps), 
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
 * set_pita(++Parameter:atom,+Value:term) is det
 *
 * The predicate sets the value of a parameter
 * For a list of parameters see 
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or 
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
set_pita(Parameter,Value):-
  pita_input_mod(M),
  retract(M:local_pita_setting(Parameter,_)),
  assert(M:local_pita_setting(Parameter,Value)).

/** 
 * setting_pita(?Parameter:atom,?Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see 
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or 
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_pita(P,V):-
  pita_input_mod(M),
  M:local_pita_setting(P,V).

extract_vars_list(L,[],V):-
  rb_new(T),
  extract_vars_tree(L,T,T1),
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

user:term_expansion((:- pita), []) :-!,
  prolog_load_context(module, M),
  findall(local_pita_setting(P,V),default_setting_pita(P,V),L),
  assert_all(L,M,_),
  assert(pita_input_mod(M)),
  retractall(M:rule_n(_)),
  assert(M:rule_n(0)),
  style_check(-discontiguous).

user:term_expansion((:- begin_lpad), []) :-
  pita_input_mod(M),!,
  assert(pita_module(M)).

user:term_expansion((:- end_lpad), []) :-
  pita_input_mod(_M0),!,
  retractall(pita_module(_M)).

user:term_expansion((Head :- Body), Clauses):-
  prolog_load_context(module, M),pita_module(M),
  M:local_pita_setting(depth_bound,true),
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
  (M:local_pita_setting(single_var,true)->
    generate_rules_db(HeadList,Env,Body1,[],R,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_rules_db(HeadList,Env,Body1,VC,R,Probs,DB,BDDAnd,0,Clauses,Module)
   ).
  
user:term_expansion((Head :- Body), Clauses):-
  prolog_load_context(module, M),pita_module(M),
% disjunctive clause with more than one head atom senza depth_bound
  Head = (_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Env,Module),
  append([one(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (M:local_pita_setting(single_var,true)->
    generate_rules(HeadList,Env,Body1,[],R,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_rules(HeadList,Env,Body1,VC,R,Probs,BDDAnd,0,Clauses,Module)
  ).

user:term_expansion((Head :- Body), []) :- 
% disjunctive clause with a single head atom con prob. 0 senza depth_bound --> la regola non e' caricata nella teoria e non e' conteggiata in NR
  prolog_load_context(module, M),pita_module(M),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  Head = (_H:P),
  ground(P),
  P=:=0.0, !. 

user:term_expansion((Head :- Body), Clauses) :- 
% disjunctive clause with a single head atom e depth_bound
  prolog_load_context(module, M),pita_module(M),
  M:local_pita_setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and([DBH>=1,DB is DBH -1|BodyList3],Body1),
  add_bdd_arg_db(H,Env,BDDAnd,DBH,Module,Head1),
  Clauses=(Head1 :- Body1).

user:term_expansion((Head :- Body), Clauses) :- 
% disjunctive clause with a single head atom senza depth_bound con prob =1
  prolog_load_context(module, M),pita_module(M),
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
  prolog_load_context(module, M),pita_module(M),
  M:local_pita_setting(depth_bound,true),
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
  (M:local_pita_setting(single_var,true)->
    generate_clause_db(H,Env,Body2,[],R,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_clause_db(H,Env,Body2,VC,R,Probs,DB,BDDAnd,0,Clauses,Module)
  ).

user:term_expansion((Head :- Body), Clauses) :- 
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  prolog_load_context(module, M),pita_module(M),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  Head = (H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),%***test single_vars
  (M:local_pita_setting(single_var,true)->
    generate_clause(H,Env,Body2,[],R,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_clause(H,Env,Body2,VC,R,Probs,BDDAnd,0,Clauses,Module)
  ).
  
/*user:term_expansion((Head :- Body),Clauses) :- 
% definite clause for db facts
  prolog_load_context(module, M),pita_module(M),  
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),
  Head=db(Head1),!,
  Clauses=(Head1 :- Body).
*/
user:term_expansion((Head :- Body),Clauses) :- 
% definite clause with depth_bound
  prolog_load_context(module, M),pita_module(M),  
  M:local_pita_setting(depth_bound,true),
   ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and([DBH>=1,DB is DBH-1|BodyList3],Body1),
  add_bdd_arg_db(Head,Env,BDDAnd,DBH,Module,Head1),
  Clauses=(Head1 :- Body1).
 
user:term_expansion((Head :- Body),Clauses) :- 
% definite clause senza DB
  prolog_load_context(module, M),pita_module(M),  
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Head,Env,BDDAnd,Module,Head1),
  Clauses=(Head1 :- Body2).

user:term_expansion(Head,Clauses) :- 
  prolog_load_context(module, M),pita_module(M),
  M:local_pita_setting(depth_bound,true),
% disjunctive FACT with more than one head atom e db
  Head=(_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (M:local_pita_setting(single_var,true)->
    generate_rules_fact_db(HeadList,_Env,[],R,Probs,0,Clauses,_Module)
  ;
    generate_rules_fact_db(HeadList,_Env,VC,R,Probs,0,Clauses,_Module)
  ).

user:term_expansion(Head,Clauses) :- 
  prolog_load_context(module, M),pita_module(M),
% disjunctive fact with more than one head atom senza db
  Head=(_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs), %**** test single_var
  (M:local_pita_setting(single_var,true)->
    generate_rules_fact(HeadList,_Env,[],R,Probs,0,Clauses,_Module)
  ;
    generate_rules_fact(HeadList,_Env,VC,R,Probs,0,Clauses,_Module)
  ).

user:term_expansion(Head,[]) :- 
  prolog_load_context(module, M),pita_module(M),
% disjunctive fact with a single head atom con prob. 0
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (_H:P),
  ground(P),
  P=:=0.0, !.
  
user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),pita_module(M),
  M:local_pita_setting(depth_bound,true),
% disjunctive fact with a single head atom con prob.1 e db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),
  ground(P),
  P=:=1.0, !,
  list2and([one(Env,BDD)],Body1),
  add_bdd_arg_db(H,Env,BDD,_DB,_Module,Head1),
  Clause=(Head1 :- Body1).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),pita_module(M),
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),
  ground(P),
  P=:=1.0, !,
  list2and([one(Env,BDD)],Body1),
  add_bdd_arg(H,Env,BDD,_Module,Head1),
  Clause=(Head1 :- Body1).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),pita_module(M),
  M:local_pita_setting(depth_bound,true),
% disjunctive fact with a single head atom e prob. generiche, con db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  add_bdd_arg_db(H,Env,BDD,_DB,_Module,Head1),
  (M:local_pita_setting(single_var,true)->
    Clause=(Head1:-(get_var_n(Env,R,[],Probs,V),equality(Env,V,0,BDD)))
  ;
    Clause=(Head1:-(get_var_n(Env,R,VC,Probs,V),equality(Env,V,0,BDD)))
  ).

user:term_expansion(Head,Clause) :- 
  prolog_load_context(module, M),pita_module(M),
% disjunctive fact with a single head atom e prob. generiche, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  add_bdd_arg(H,Env,BDD,_Module,Head1),%***test single_var
  (M:local_pita_setting(single_var,true)->
    Clause=(Head1:-(get_var_n(Env,R,[],Probs,V),equality(Env,V,0,BDD)))
  ;
    Clause=(Head1:-(get_var_n(Env,R,VC,Probs,V),equality(Env,V,0,BDD)))
  ).

user:term_expansion((:- set_pita(P,V)), []) :-!,
  prolog_load_context(module, M),pita_module(M),
  set_pita(P,V).


user:term_expansion(Head, (Head1:-one(Env,One))) :- 
  prolog_load_context(module, M),pita_module(M),
  M:local_pita_setting(depth_bound,true),
% definite fact with db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg_db(Head,Env,One,_DB,_Module,Head1).

user:term_expansion(Head, (Head1:-one(Env,One))) :- 
  prolog_load_context(module, M),pita_module(M),
% definite fact without db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),
  add_bdd_arg(Head,Env,One,_Module,Head1).


/** 
 * begin_lpad_pred is det
 *
 * Initializes LPAD loading.
 */
begin_lpad_pred:-
  M=user,
  pita_input_mod(M),
  assert(pita_module(M)).

/** 
 * end_lpad_pred is det
 *
 * Terminates the cplint inference module.
 */
end_lpad_pred:-
  retractall(pita_module(_M)).

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

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(pita:set_pita(_,_)).
sandbox:safe_primitive(pita:setting_pita(_,_)).
/*sandbox:safe_primitive(pita:init(_,_,_)).
sandbox:safe_primitive(pita:init_bdd(_,_)).
sandbox:safe_primitive(pita:init_test(_,_)).
sandbox:safe_primitive(pita:ret_prob(_,_,_)).
sandbox:safe_primitive(pita:end(_)).
sandbox:safe_primitive(pita:end_bdd(_)).
sandbox:safe_primitive(pita:end_test(_)).
sandbox:safe_primitive(pita:one(_,_)).
sandbox:safe_primitive(pita:zero(_,_)).
sandbox:safe_primitive(pita:and(_,_,_,_)).
sandbox:safe_primitive(pita:or(_,_,_,_)).
sandbox:safe_primitive(pita:bdd_not(_,_,_)).
sandbox:safe_primitive(pita:get_var_n(_,_,_,_,_)).
sandbox:safe_primitive(pita:add_var(_,_,_,_,_)).
sandbox:safe_primitive(pita:equality(_,_,_,_)).
*/
:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(pita:s(_,_), []).
sandbox:safe_meta(pita:prob(_,_), []).
sandbox:safe_meta(pita:prob_bar(_,_), []).

