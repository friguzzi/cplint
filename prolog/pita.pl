/** <module> pita

This module performs reasoning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It reads probabilistic program andcomputes the probability of queries.

See https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html for
details.

@author Fabrizio Riguzzi
@license Artistic License 2.0 https://opensource.org/licenses/Artistic-2.0
@copyright Fabrizio Riguzzi
*/


:- module(pita,[s/2, prob/2, 
  abd_prob/3,
  vit_prob/3,
  prob/3,
  bdd_dot_file/3,
  bdd_dot_string/3,
  abd_bdd_dot_string/4,
  abd_bdd_dot_string/6,
  map_bdd_dot_string/6,
  vit_bdd_dot_string/5,
  set_pita/2,setting_pita/2,
  init/3,init_bdd/2,init_test/2,end/1,end_bdd/1,end_test/1,
  one/2,zero/2,and/4,or/4,bdd_not/3,
  onec/2,zeroc/2,andc/4,bdd_notc/3,
  orc/3,
  ret_prob/3,get_var_n/6,get_abd_var_n/6,equality/4,
  or_list/3,
  ret_probc/3,equalityc/4,
  or_listc/3,
  em/8,randomize/1,rand_seed/1,
  load/1,load_file/1,
  op(600,xfy,'::'),
  op(1150,fx,action),
  op(1200,fy,map_query),
  op(1200,fy,abducible),
  msw/4,
  msw/5
    ]).
:- reexport(library(tabling)).
:- reexport(library(cplint_util)).

:-meta_predicate s(:,-).
:-meta_predicate prob(:,-).
:-meta_predicate abd_prob(:,-,-).
:-meta_predicate vit_prob(:,-,-).
:-meta_predicate prob(:,:,-).
:-meta_predicate bdd_dot_file(:,+,-).
:-meta_predicate bdd_dot_string(:,-,-).
:-meta_predicate abd_bdd_dot_string(:,-,-,-).
:-meta_predicate abd_bdd_dot_string(:,-,-,-,-,-).
:-meta_predicate map_bdd_dot_string(:,-,-,-,-,-).
:-meta_predicate vit_bdd_dot_string(:,-,-,-,-).
:-meta_predicate msw(:,-,-,-).
:-meta_predicate msw(:,-,-,-,-).
:-meta_predicate get_p(:,+,-).
:-meta_predicate get_cond_p(:,:,+,-).
:-meta_predicate get_node(:,+,-).
:-meta_predicate get_cond_node(:,:,+,-,-).
:-meta_predicate set_pita(:,+).
:-meta_predicate setting_pita(:,-).
:-meta_predicate set_sw(:,+).

:-use_module(library(lists)).
:-use_module(library(rbtrees)).
:-use_module(library(apply)).
:-use_module(library(assoc)).
:-use_foreign_library(foreign(bddem),install).

:- style_check(-discontiguous).

:- thread_local rule_n/1,goal_n/1,pita_input_mod/1,local_pita_setting/2.

/*:- multifile one/2,zero/2,and/4,or/4,bdd_not/3,init/3,init_bdd/2,init_test/1,
  end/1,end_bdd/1,end_test/0,ret_prob/3,em/9,randomize/1,
  get_var_n/6,add_var/5,equality/4.*/
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
 * em(++Context:int,++ListOfBDDs:list,++EA:float,++ER:float,++Iterations:int,-LL:float,-Parameters:list,-ExampleProbabilities:list) is det
 *
 * Performs EM learning.
 * Takes as input the Context, a list of BDDs each representing one example,
 * the minimum absolute difference EA and relative difference ER between the
 * log likelihood of examples in two different iterations and the maximum number of iterations
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

/**
 * create_dot_string(++Env:int,++BDD:int,-Dot:string) is det
 *
 * The predicate returns the BDD in dot format.
 */

/**
 * create_dot(++Env:int,++BDD:int,++File:string) is det
 *
 * The predicate writes the BDD in dot format to
 * to file FileName
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

default_setting_pita(tabling,auto).
/* values:
  auto
  explicit
*/
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

orc((Env,A),(_,B),(Env,C)):-
  or(Env,A,B,C).

onec(Env,(Env,One)):-
  one(Env,One).
zeroc(Env,(Env,Zero)):-
  zero(Env,Zero).

andc(Env,(_,A),(_,B),(Env,C)):-
  (zero(Env,B)->
    fail
  ;
    and(Env,A,B,C)
  ).

andcnf(Env,(_,A),(_,B),(Env,C)):-
  and(Env,A,B,C).

bdd_notc(Env,(_,A),(Env,NA)):-
  bdd_not(Env,A,NA).

equalityc(Env,V,N,(Env,B)):-
  equality(Env,V,N,B).

ret_probc(Env,(_,BDD),P):-
  ret_prob(Env,BDD,P).


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
  term_variables(Goal,VG),
  get_next_goal_number(M,GN),
  atomic_concat('$goal',GN,NewGoal),
  Goal1=..[NewGoal|VG],
  list2and(GoalL,Goal),
  ( M:local_pita_setting(depth_bound,true) *->
      ( process_body_db(GoalL,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,M),
        add_bdd_arg_db(Goal1,Env,BDDAnd,DB,M,Head1)
      )
    ;
      ( process_body(GoalL,BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
        add_bdd_arg(Goal1,Env,BDDAnd,M,Head1)
      )
  ),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  M:(asserta((Head1 :- Body2),Ref)),
  M:rule_n(NR),
  init_test(NR,Env),
  findall((Goal,P),get_p(M:Goal1,Env,P),L),
  end_test(Env),
  erase(Ref),
  member((Goal,P),L).

abd_prob(M:Goal,P,Delta):-
  term_variables(Goal,VG),
  get_next_goal_number(M,GN),
  atomic_concat('$goal',GN,NewGoal),
  Goal1=..[NewGoal|VG],
  list2and(GoalL,Goal),
  process_body(GoalL,BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Goal1,Env,BDDAnd,M,Head1),
  M:(asserta((Head1 :- Body2),Ref)),
  M:rule_n(NR),
  init_test(NR,Env),
  findall((Goal,P,Exp),get_abd_p(M:Goal1,Env,P,Exp),L),
  end_test(Env),
  erase(Ref),
  member((Goal,P,Exp),L),
  from_assign_to_exp(Exp,M,Delta).

vit_prob(M:Goal,P,Delta):-
  term_variables(Goal,VG),
  get_next_goal_number(M,GN),
  atomic_concat('$goal',GN,NewGoal),
  Goal1=..[NewGoal|VG],
  list2and(GoalL,Goal),
  process_body(GoalL,BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Goal1,Env,BDDAnd,M,Head1),
  M:(asserta((Head1 :- Body2),Ref)),
  M:rule_n(NR),
  init_test(NR,Env),
  findall((Goal,P,Exp),get_vit_p(M:Goal1,Env,P,Exp),L),
  end_test(Env),
  erase(Ref),
  member((Goal,P,Exp0),L),
  reverse(Exp0,Exp),
  from_assign_to_vit_exp(Exp,M,Delta).

vit_bdd_dot_string(M:Goal,dot(Dot),LV,P,MAP):-
  M:rule_n(NR),
  init_test(NR,Env),
  get_node(M:Goal,Env,(_,BDD)),!,
  findall([V,R,S],M:v(R,S,V),LV),
  ret_vit_prob(Env,BDD,P,Exp0),
  reverse(Exp0,Exp),
  from_assign_to_vit_exp(Exp,M,MAP),
  create_dot_string(Env,BDD,Dot),
  end_test(Env).

from_assign_to_vit_exp([],_M,[]).

from_assign_to_vit_exp([Var-Val|TA],M,[rule(R,Head,HeadList,Body)|TDelta]):-
  M:v(R,S,Var),
  M:rule_by_num(R,HeadList,Body,S),
  nth0(Val,HeadList,Head:_),
  from_assign_to_vit_exp(TA,M,TDelta).

%  Delta=Exp.
%  from_assign_to_exp(Exp,M,Delta).

from_assign_to_exp([],_M,[]).

from_assign_to_exp([Var-Val|TA],M,[Abd|TDelta]):-
  M:av(R,S,Var),
  M:abd(R,S,H),
  (Val=1->
    Abd=H
  ;
    Abd= \+(H)
  ),
  from_assign_to_exp(TA,M,TDelta).


/**
 * bdd_dot_file(:Query:atom,+FileName:string,-LV:list) is det
 *
 * The predicate builds the BDD for Query and writes its dot representation
 * to file FileName and a list in LV with the association of variables to rules.
 * LV is a list of list, each sublist has three elements:
 * the mutlivalued variable number,
 * the rule number and the grounding substituion.
 */
bdd_dot_file(M:Goal,File,LV):-
  M:rule_n(NR),
  init_test(NR,Env),
  get_node(M:Goal,Env,(_,BDD)),!,
  findall([V,R,S],M:v(R,S,V),LV),
  create_dot(Env,BDD,File),
  end_test(Env).

/**
 * bdd_dot_string(:Query:atom,-DotString:string,-LV:list) is det
 *
 * The predicate builds the BDD for Query and returns its dot representation
 * in DotString and a list in LV with the association of variables to rules.
 * LV is a list of list, each sublist has three elements:
 * the mutlivalued variable number,
 * the rule number and the grounding substituion.
 */
bdd_dot_string(M:Goal,dot(Dot),LV):-
  M:rule_n(NR),
  init_test(NR,Env),
  get_node(M:Goal,Env,(_,BDD)),!,
  findall([V,R,S],M:v(R,S,V),LV),
  create_dot_string(Env,BDD,Dot),
  end_test(Env).

/**
 * bdd_dot_string(:Query:atom,-DotString:string,-LV:list) is det
 *
 * The predicate builds the BDD for Query and returns its dot representation
 * in DotString and a list in LV with the association of variables to rules.
 * LV is a list of list, each sublist has three elements:
 * the mutlivalued variable number,
 * the rule number and the grounding substituion.
 */
abd_bdd_dot_string(M:Goal,dot(Dot),LV,LAV):-
  M:rule_n(NR),
  init_test(NR,Env),
  get_node(M:Goal,Env,(_,BDD)),!,
  findall([V,R,S],M:v(R,S,V),LV),
  findall([V,R,S],M:av(R,S,V),LAV),
  create_dot_string(Env,BDD,Dot),
  end_test(Env).

abd_bdd_dot_string(M:Goal,dot(Dot),LV,LAV,P,Delta):-
  M:rule_n(NR),
  init_test(NR,Env),
  get_node(M:Goal,Env,(_,BDD)),!,
  findall([V,R,S],M:v(R,S,V),LV),
  findall([V,R,S],M:av(R,S,V),LAV),
  ret_abd_prob(Env,BDD,P,Exp),
  from_assign_to_exp(Exp,M,Delta),
  create_dot_string(Env,BDD,Dot),
  end_test(Env).

map_bdd_dot_string(M:Goal,dot(Dot),LV,LAV,P,MAP):-
  M:rule_n(NR),
  init_test(NR,Env),
  get_node(M:Goal,Env,(_,BDD0)),!,
  findall([V,R,S],M:v(R,S,V),LV),
  one(Env,One),
  make_query_vars(LV,M,Env,One,Cons,LAV),
  and(Env,BDD0,Cons,BDD),
  ret_map_prob(Env,BDD,P,Exp0),
  reverse(Exp0,Exp),
  from_assign_to_map(Exp,M,MAP),
  create_dot_string(Env,BDD,Dot),
  end_test(Env).

make_query_vars([],_M,_Env,C,C,[]).

make_query_vars([[V,R,S]|T],M,Env,Cons0,Cons,[[V,R,S]|TV]):-
  M:query_rule(R,_,_,_),!,
  make_query_var(Env,V,B),
  and(Env,Cons0,B,Cons1),
  make_query_vars(T,M,Env,Cons1,Cons,TV).

make_query_vars([_H|T],M,Env,Cons0,Cons,LV):-
  make_query_vars(T,M,Env,Cons0,Cons,LV).

from_assign_to_map([],_M,[]).

from_assign_to_map([Var-Val|TA],M,[rule(R,Head,HeadList,Body)|TDelta]):-
  M:v(R,S,Var),
  M:query_rule(R,HeadList,Body,S),
  nth1(Val,HeadList,Head:_),
  from_assign_to_map(TA,M,TDelta).

/**
 * prob(:Query:atom,-Probability:float) is nondet
 *
 * The predicate computes the probability of Query
 * If Query is not ground, it returns in backtracking all ground
 * instantiations of
 * Query together with their probabilities
 */
prob(M:Goal,P):-
  s(M:Goal,P).


/**
 * prob(:Query:atom,:Evidence:atom,-Probability:float) is nondet
 *
 * The predicate computes the probability of Query given
 * Evidence
 * If Query/Evidence are not ground, it returns in backtracking all
 * ground instantiations of
 * Query/Evidence together with their probabilities
 */
prob(M:Goal,M:Evidence,P):-
  get_next_goal_number(M,GN),
  atomic_concat('$ev',GN,NewEv),
  deal_with_ev(Evidence,M,NewEv,EvNoAct,UpdatedClausesRefs,ClausesToReAdd),
  term_variables(Goal,VG),
  atomic_concat('$goal',GN,NewGoal),
  Goal1=..[NewGoal|VG],
  list2and(GoalL,Goal),
  process_body(GoalL,BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Goal1,Env,BDDAnd,M,Head1),
  M:(asserta((Head1 :- Body2),Ref)),
  M:rule_n(NR),
  init_test(NR,Env),
  (EvNoAct=true->
    findall((Goal,P),get_p(M:Goal1,Env,P),L)
  ;
    findall((Goal,P),get_cond_p(M:Goal1,M:EvNoAct,Env,P),L)
  ),
  end_test(Env),
  retractall(M:NewEv),
  maplist(erase,UpdatedClausesRefs),
  erase(Ref),
  maplist(M:assertz,ClausesToReAdd),
  member((Goal,P),L).

deal_with_ev(Ev,M,NewEv,EvGoal,UC,CA):-
  list2and(EvL,Ev),
  partition(ac,EvL,ActL,EvNoActL),
  deal_with_actions(ActL,M,UC0,CA),
  (EvNoActL=[]->
    EvGoal=true,
    UC=UC0
  ;
    process_body(EvNoActL,BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
    append([onec(Env,BDD)],BodyList2,BodyList3),
    list2and(BodyList3,Body2),
    add_bdd_arg(NewEv,Env,BDDAnd,M,Head1),
    M:(asserta((Head1 :- Body2),Ref)),
    UC=[Ref|UC0],
    EvGoal=NewEv
  ).

deal_with_actions(ActL,M,UC,CA):-
  empty_assoc(AP0),
  foldl(get_pred_const,ActL,AP0,AP),
  assoc_to_list(AP,LP),
  maplist(update_clauses(M),LP,UCL,CAL),
  partition(nac,ActL,_NActL,PActL),
  maplist(assert_actions(M),PActL,ActRefs),
  append([ActRefs|UCL],UC),
  append(CAL,CA).

zero_clauses_actions(M,do(\+ A),Ref):-
  A=..[P|Args],
  append(Args,[Env,BDD],Args1),
  A1=..[P|Args1],
  M:assertz((A1:-zeroc(Env,BDD)),Ref).

assert_actions(M,do(A),Ref):-
  A=..[P|Args],
  append(Args,[Env,BDD],Args1),
  atomic_concat(P,' tabled',P1),
  A1=..[P1|Args1],
  M:assertz((A1:-onec(Env,BDD)),Ref).

update_clauses(M,P/0- _,[RefZ],[(H:-zeroc(Env,BDD))|LCA]):-!,
  functor(G1,P,2),
  findall(Ref,M:clause(G1,_B,Ref),UC),
  findall((G1:-B),M:clause(G1,B),LCA),
  H=..[P,Env,BDD],
  maplist(erase,UC),
  M:assertz((H:-zeroc(Env,BDD)),RefZ).

update_clauses(M,P/A-Constants,UC,CA):-
  functor(G,P,A),
  A1 is A+2,
  functor(G1,P,A1),
  G=..[_|Args],
  findall((G1,B,Ref),M:clause(G1,B,Ref),LC),
  maplist(get_const(Args),Constants,ConstraintsL),
  list2and(ConstraintsL,Constraints),
  maplist(add_cons(G1,Constraints,M),LC,UC,CA).

add_cons(_G,_C,M,(H,zeroc(Env,Zero),Ref),Ref1,(H:-zeroc(Env,Zero))):-!,
  erase(Ref),
  M:assertz((H:-zeroc(Env,Zero)),Ref1).

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
  atomic_concat(F,' tabled',F1),
  (get_assoc(F1/A,AP0,V)->
    put_assoc(F1/A,AP0,[Args|V],AP)
  ;
    put_assoc(F1/A,AP0,[Args],AP)
  ).


ac(do(_)).
nac(do(\+ _)).


get_p(M:Goal,Env,P):-
  get_node(M:Goal,Env,BDD),
  ret_probc(Env,BDD,P).

get_abd_p(M:Goal,Env,P,Exp):-
  get_node(M:Goal,Env,(_,BDD)),
  ret_abd_prob(Env,BDD,P,Exp).

get_vit_p(M:Goal,Env,P,Exp):-
  get_node(M:Goal,Env,(_,BDD)),
  ret_vit_prob(Env,BDD,P,Exp).

get_cond_p(M:Goal,M:Evidence,Env,P):-
  get_cond_node(M:Goal,M:Evidence,Env,BDDGE,BDDE),
  ret_probc(Env,BDDE,PE),
  ret_probc(Env,BDDGE,PGE),
  P is PGE/PE.


get_node(M:Goal,Env,B):-
  M:local_pita_setting(depth_bound,true),!,
  M:local_pita_setting(depth,DB),
  retractall(M:v(_,_,_)),
  retractall(M:av(_,_,_)),
  abolish_all_tables,
  add_bdd_arg_db(Goal,Env,BDD,DB,M,Goal1),%DB=depth bound
  (bagof(BDD,M:Goal1,L)*->
    or_listc(L,Env,B)
  ;
    zeroc(Env,B)
  ).

get_node(M:Goal,Env,B):- %with DB=false
  retractall(M:v(_,_,_)),
  retractall(M:av(_,_,_)),
  abolish_all_tables,
  add_bdd_arg(Goal,Env,BDD,M,Goal1),
  (bagof(BDD,M:Goal1,L)*->
    or_listc(L,Env,B)
  ;
    zeroc(Env,B)
  ).

get_cond_node(M:Goal,M:Ev,Env,BGE,BE):-
  M:local_pita_setting(depth_bound,true),!,
  M:local_pita_setting(depth,DB),
  retractall(M:v(_,_,_)),
  abolish_all_tables,
  add_bdd_arg_db(Goal,Env,BDD,DB,M,Goal1),%DB=depth bound
  (bagof(BDD,M:Goal1,L)*->
    or_listc(L,Env,BG)
  ;
    zeroc(Env,BG)
  ),
  add_bdd_arg_db(Ev,Env,BDDE,DB,M,Ev1),%DB=depth bound
  (bagof(BDDE,M:Ev1,LE)*->
    or_listc(LE,Env,BE)
  ;
    zeroc(Env,BE)
  ),
  andcnf(Env,BG,BE,BGE).



get_cond_node(M:Goal,M:Ev,Env,BGE,BE):- %with DB=false
  retractall(M:v(_,_,_)),
  abolish_all_tables,
  add_bdd_arg(Goal,Env,BDD,M,Goal1),
  (bagof(BDD,M:Goal1,L)*->
    or_listc(L,Env,BG)
  ;
    zeroc(Env,BG)
  ),
  add_bdd_arg(Ev,Env,BDDE,M,Ev1),
  (bagof(BDDE,M:Ev1,LE)*->
    or_listc(LE,Env,BE)
  ;
    zeroc(Env,BE)
  ),
  andcnf(Env,BG,BE,BGE).


get_next_goal_number(PName,R):-
  retract(PName:goal_n(R)),
  R1 is R+1,
  assert(PName:goal_n(R1)).


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

/**
 * get_var_n(++M:atomic,++Environment:int,++Rule:int,++Substitution:term,++Probabilities:list,-Variable:int) is det
 *
 * Returns the index Variable of the random variable associated to rule with
 * index Rule, grouding substitution Substitution and head distribution
 * Probabilities in environment Environment.
 */
get_var_n(M,Env,R,S,Probs0,V):-
  M:query_rule(R,_H,_B,_S),!,
  (ground(Probs0)->
    maplist(is,Probs,Probs0),
    (M:v(R,S,V)->
      true
    ;
      length(Probs,L),
      add_query_var(Env,L,Probs,R,V),
      assert(M:v(R,S,V))
    )
  ;
    throw(error('Non ground probailities not instantiated by the body'))
  ).

get_var_n(M,Env,R,S,Probs0,V):-
  (ground(Probs0)->
    maplist(is,Probs,Probs0),
    (M:v(R,S,V)->
      true
    ;
      length(Probs,L),
      add_var(Env,L,Probs,R,V),
      assert(M:v(R,S,V))
    )
  ;
    throw(error('Non ground probailities not instantiated by the body'))
  ).

/**
 * get_abd_var_n(++M:atomic,++Environment:int,++Rule:int,++Substitution:term,++Probabilities:list,-Variable:int) is det
 *
 * Returns the index Variable of the random variable associated to rule with
 * index Rule, grouding substitution Substitution and head distribution
 * Probabilities in environment Environment.
 */
get_abd_var_n(M,Env,R,S,Probs0,V):-
  (ground(Probs0)->
    maplist(is,Probs,Probs0),
    (M:av(R,S,V)->
      true
    ;
      length(Probs,L),
      add_abd_var(Env,L,Probs,R,V),
      assert(M:av(R,S,V))
    )
  ;
    throw(error('Non ground probailities not instantiated by the body'))
  ).

/**
 * msw(:Var:term,?Value:term,++Environment:int,--BDD:int) is det
 *
 * Returns a BDD representing Var=Value.
 * This is a predicate for programs in the PRISM syntax
 */
msw(M:A,B,Env,BDD):-
  M:values(A,Values),
  M:sw(R,A,Probs0),
  (ground(Probs0)->
    maplist(is,Probs,Probs0),
    length(Probs,L),
    add_var(Env,L,Probs,R,V),
    nth0(N,Values,B),
    equalityc(Env,V,N,BDD)
  ;
    throw(error('Non ground probailities not instantiated by the body'))
  ).

/**
 * msw(:Var:term,?Value:term,++Environment:int,--BDD:int,?DB:int) is det
 *
 * Returns a BDD representing Var=Value when there is a depth bound on
 * derivations.
 * This is a predicate for programs in the PRISM syntax
 */
msw(M:A,B,Env,BDD,_DB):-
  M:values(A,Values),
  M:sw(R,A,Probs0),
  (ground(Probs0)->
    maplist(is,Probs,Probs0),
    length(Probs,L),
    add_var(Env,L,Probs,R,V),
    nth0(N,Values,B),
    equalityc(Env,V,N,BDD)
  ;
    throw(error('Non ground probailities not instantiated by the body'))
  ).

combine(V,P,V:P).

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
  Clause=(Head1:-(get_var_n(Module,Env,R,VC,Probs,V),equalityc(Env,V,N,BDD))).

generate_rules_fact([Head:_P|T],Env,VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(get_var_n(Module,Env,R,VC,Probs,V),equalityc(Env,V,N,BDD))),
  N1 is N+1,
  generate_rules_fact(T,Env,VC,R,Probs,N1,Clauses,Module).


generate_rules_fact_vars([],_Env,_R,_Probs,_N,[],_Module).

generate_rules_fact_vars([Head:_P1,'':_P2],Env,R,Probs,N,[Clause],Module):-!,
  extract_vars_list([Head],[],VC),
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(get_var_n(Module,Env,R,VC,Probs,V),equalityc(Env,V,N,BDD))).

generate_rules_fact_vars([Head:_P|T],Env,R,Probs,N,[Clause|Clauses],Module):-
  extract_vars_list([Head],[],VC),
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(get_var_n(Module,Env,R,VC,Probs,V),equalityc(Env,V,N,BDD))),
  N1 is N+1,
  generate_rules_fact_vars(T,Env,R,Probs,N1,Clauses,Module).


generate_rules_fact_db([],_Env,_VC,_R,_Probs,_N,[],_Module).

generate_rules_fact_db([Head:_P1,'':_P2],Env,VC,R,Probs,N,[Clause],Module):-!,
  add_bdd_arg_db(Head,Env,BDD,_DB,Module,Head1),
  Clause=(Head1:-(get_var_n(Module,Env,R,VC,Probs,V),equalityc(Env,V,N,BDD))).

generate_rules_fact_db([Head:_P|T],Env,VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg_db(Head,Env,BDD,_DB,Module,Head1),
  Clause=(Head1:-(get_var_n(Module,Env,R,VC,Probs,V),equalityc(Env,V,N,BDD))),
  N1 is N+1,
  generate_rules_fact_db(T,Env,VC,R,Probs,N1,Clauses,Module).


generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module):-
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(Body,get_var_n(Module,Env,R,VC,Probs,V),equalityc(Env,V,N,B),andc(Env,BDDAnd,B,BDD))).


generate_clause_db(Head,Env,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module):-
  add_bdd_arg_db(Head,Env,BDD,DBH,Module,Head1),
  Clause=(Head1:-(DBH>=1,DB is DBH-1,Body,get_var_n(Module,Env,R,VC,Probs,V),equalityc(Env,V,N,B),andc(Env,BDDAnd,B,BDD))).


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

process_body([\+ H|T],BDD,BDD1,Vars,[BDDH,BDDN,BDD2|Vars1],
[(H1,bdd_notc(Env,BDDH,BDDN)),
  andc(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
  add_bdd_arg(H,Env,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Env,Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([db(H)|T],BDD,BDD1,Vars,Vars1,[H|Rest],Env,Module):-
  !,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,[BDDH,BDD2|Vars1],
[H1,andc(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-
  add_bdd_arg(H,Env,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module).

process_body_db([],BDD,BDD,_DB,Vars,Vars,[],_Env,_Module):-!.

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Env,Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([\+ db(H)|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Env,Module):-
  !,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,[BDDH,BDDN,BDD2|Vars1],
[(H1,bdd_notc(Env,BDDH,BDDN)),
  andc(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([db(H)|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  !,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,[BDDH,BDD2|Vars1],
[H1,andc(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-!, %agg. cut
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


gen_head(H,P,V,V1,H1:P):-copy_term((H,V),(H1,V1)).
gen_head_disc(H,V,V1:P,H1:P):-copy_term((H,V),(H1,V1)).


/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([H], Prob, [Head:ProbHead1|Null]) :-
  (H=Head:ProbHead;H=ProbHead::Head),!,
  ProbHead1 is ProbHead,
  ProbLast is 1 - Prob - ProbHead1,
  prolog_load_context(module, M),pita_input_mod(M),
  M:local_pita_setting(epsilon_parsing, Eps),
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
 * or_listc(++ListOfBDDs:list,++Environment,--BDD:int) is det
 *
 * Returns in BDD a couple (Env,B) with B a pointer to a
 * BDD belonging to environment Environment
 * representing the disjunction of all the BDDs in
 * ListOfBDDs (a list of couples (Env,BDD))
 */
or_listc([H],_Env,H):-!.

or_listc([H|T],Env,B):-
  or_listc1(T,Env,H,B).


or_listc1([],_Env,B,B).

or_listc1([H|T],Env,B0,B1):-
  orc(B0,H,B2),
  or_listc1(T,Env,B2,B1).

/**
 * set_pita(:Parameter:atom,+Value:term) is det
 *
 * The predicate sets the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 *
 */
set_pita(M:Parameter,Value):-
  retract(M:local_pita_setting(Parameter,_)),
  assert(M:local_pita_setting(Parameter,Value)).

/**
 * setting_pita(:Parameter:atom,?Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_pita(M:P,V):-
  M:local_pita_setting(P,V).

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


delete_equal([],_,[]).

delete_equal([H|T],E,T):-
  H == E,!.

delete_equal([H|T],E,[H|T1]):-
  delete_equal(T,E,T1).

set_sw(M:A,B):-
  get_next_rule_number(M,R),
  assert(M:sw(R,A,B)).

act(M,A/B):-
  (M:local_pita_setting(depth_bound,true)->
    B1 is B + 3
  ;
    B1 is B + 2
  ),
  atomic_concat(A,' tabled',A1),
  M:(dynamic A1/B1).

tab(M,A/B,P):-
  length(Args0,B),
  (M:local_pita_setting(depth_bound,true)->
    ExtraArgs=[-,_,lattice(orc/3)]
  ;
    ExtraArgs=[-,lattice(orc/3)]
  ),
  append(Args0,ExtraArgs,Args),
  P=..[A|Args],
  PT=..[A|Args0],
  assert(M:tabled(PT)).

zero_clause(M,A/B,(H:-maplist(nonvar,Args0),zeroc(Env,BDD))):-
  length(Args0,B),
  (M:local_pita_setting(depth_bound,true)->
    ExtraArgs=[Env,_,BDD]
  ;
    ExtraArgs=[Env,BDD]
  ),
  append(Args0,ExtraArgs,Args),
  H=..[A|Args].

to_table(M,Heads,[],Heads):-
  M:local_pita_setting(tabling,explicit),!.

to_table(M,Heads,ProcTabDir,Heads1):-
  maplist(tab_dir(M),Heads,TabDirList,Heads1L),
  append(TabDirList,TabDir),
  maplist(user:term_expansion,TabDir,ProcTabDirL),
  append(ProcTabDirL,ProcTabDir),
  append(Heads1L,Heads1).

tab_dir(_M,'':_,[],[]):-!.

tab_dir(M,H:P,[],[H1:P]):-
  M:tabled(H),!,
  H=..[F|Args],
  atomic_concat(F,' tabled',F1),
  H1=..[F1|Args].


tab_dir(M,H:P,[(:- table F/A)],[H1:P]):-
  functor(H,F,A),
  H=..[F|Args],
  functor(HT,F,A),
  atomic_concat(F,' tabled',F1),
  M:assert(tabled(HT)),
  H1=..[F1|Args].

user:term_expansion(end_of_file, C) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  retractall(pita_input_mod(M)),
  findall(LZ,M:zero_clauses(LZ),L0),
  append(L0,L),
  retractall(M:zero_clauses(_)),
  retractall(M:tabled(_)),
  append(L,[(:- style_check(+discontiguous)),end_of_file],C).

user:term_expansion((:- action Conj), []) :-!,
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  list2and(L,Conj),
  maplist(act(M),L).

user:term_expansion((:- pita), []) :-!,
  prolog_load_context(module, M),
  retractall(M:local_pita_setting(_,_)),
  findall(local_pita_setting(P,V),default_setting_pita(P,V),L),
  assert_all(L,M,_),
  assert(pita_input_mod(M)),
  retractall(M:rule_n(_)),
  retractall(M:goal_n(_)),
  assert(M:rule_n(0)),
  assert(M:goal_n(0)),
  M:(dynamic v/3, av/3, query_rule/4, rule_by_num/4,
    zero_clauses/1, pita_on/0, tabled/1),
  retractall(M:query_rule(_,_,_,_)),
  style_check(-discontiguous).

user:term_expansion((:- table(Conj)), [:- table(Conj1)]) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  list2and(L,Conj),
  maplist(tab(M),L,L1),
  maplist(zero_clause(M),L,LZ),
  assert(M:zero_clauses(LZ)),
  list2and(L1,Conj1).

user:term_expansion((:- begin_plp), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  assert(M:pita_on).

user:term_expansion((:- end_plp), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  retractall(M:pita_on).

user:term_expansion((:- begin_lpad), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  assert(M:pita_on).

user:term_expansion((:- end_lpad), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  retractall(M:pita_on).

user:term_expansion(values(A,B), values(A,B)) :-
  prolog_load_context(module, M),
  pita_input_mod(M),M:pita_on,!.

user:term_expansion(map_query(Clause),[query_rule(R,HeadList,Body,VC)|Clauses]):-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,!,
  M:rule_n(R),
  user:term_expansion(Clause, Clauses0),
  (Clause=(Head:-Body)->
    true
  ;
    Head=Clause,
    Body=true
  ),
  (is_list(Clauses0)->
    Clauses=Clauses0
  ;
    Clauses=[Clauses0]
  ),
  extract_vars(Clause,VC),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList).

user:term_expansion(abducible(Head),[Clause,abd(R,S,H)]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,!,
  ((Head=(H:P);Head=(P::H))->
    P1 is P,
    P0 is 1.0-P,
    Probs=[P1,P0]
  ;
    H=Head,
    Probs=[1.0,1.0]
  ),
  extract_vars_list([H],[],VC),
  get_next_rule_number(M,R),
  add_bdd_arg(H,Env,BDD,M,Head1), %***test single_var
  (M:local_pita_setting(single_var,true)->
    S=[]
  ;
    S=VC
  ),
  Clause=(Head1:-(get_abd_var_n(M,Env,R,S,Probs,V),equalityc(Env,V,0,BDD))).



user:term_expansion((Head :- Body),
  [rule_by_num(R,HeadList,BodyList,VC1)|Clauses]):-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
% disjunctive clause with more than one head atom e depth_bound
  Head = (_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,Env,M),
  append([onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  to_table(M,HeadList,TabDir,HeadList1),
  generate_rules_db(HeadList1,Env,Body1,VC1,R,Probs,DB,BDDAnd,0,Clauses0,M),
  append(TabDir,Clauses0,Clauses).


user:term_expansion((Head :- Body),
  [rule_by_num(R,HeadList,BodyList,VC1)|Clauses]):-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive clause with more than one head atom senza depth_bound
  Head = (_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Env,M),
  append([onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  to_table(M,HeadList,TabDir,HeadList1),
  generate_rules(HeadList1,Env,Body1,VC1,R,Probs,BDDAnd,0,Clauses0,M),
  append(TabDir,Clauses0,Clauses).

user:term_expansion((Head :- Body), []) :-
% disjunctive clause with a single head atom con prob. 0 senza depth_bound --> la regola non e' caricata nella teoria e non e' conteggiata in NR
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  (Head = (_:P);Head=(P::_)),
  ground(P),
  P=:=0.0, !.

user:term_expansion((Head :- Body), Clauses) :-
% disjunctive clause with a single head atom e depth_bound
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[_H:_],!,
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and([DBH>=1,DB is DBH -1|BodyList3],Body1),
  to_table(M,HeadList,TabDir,[H1:_]),
  add_bdd_arg_db(H1,Env,BDDAnd,DBH,M,Head1),
  append(TabDir,[(Head1 :- Body1)],Clauses).

user:term_expansion((Head :- Body), Clauses) :-
% disjunctive clause with a single head atom senza depth_bound con prob =1
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
   ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[_H:_],!,
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  to_table(M,HeadList,TabDir,[H1:_]),
  add_bdd_arg(H1,Env,BDDAnd,M,Head1),
  append(TabDir,[(Head1 :- Body1)],Clauses).

user:term_expansion((Head :- Body), Clauses) :-
% disjunctive clause with a single head atom e DB, con prob. diversa da 1
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  (Head = (_H:_);Head=(_::_H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),%***test single_var
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  to_table(M,HeadList,TabDir,[H1:_]),
  generate_clause_db(H1,Env,Body2,VC1,R,Probs,DB,BDDAnd,0,Clauses0,M),
  append(TabDir,[Clauses0],Clauses).

user:term_expansion((Head :- Body), [rule_by_num(R,HeadList,BodyList,VC1),Clauses]) :-
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  (Head = (_H:_);Head = (_::_H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),%***test single_vars
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  to_table(M,HeadList,TabDir,[H1:_]),
  generate_clause(H1,Env,Body2,VC1,R,Probs,BDDAnd,0,Clauses0,M),
  append(TabDir,[Clauses0],Clauses).

/*user:term_expansion((Head :- Body),Clauses) :-
% definite clause for db facts
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),
  Head=db(Head1),!,
  Clauses=(Head1 :- Body).
*/
user:term_expansion((Head :- Body),Clauses) :-
% definite clause with depth_bound
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
   ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and([DBH>=1,DB is DBH-1|BodyList3],Body1),
  to_table(M,[Head:_],TabDir,[Head1:_]),
  add_bdd_arg_db(Head1,Env,BDDAnd,DBH,M,Head2),
  append(TabDir,[(Head2 :- Body1)],Clauses).

user:term_expansion((Head :- Body),Clauses) :-
% definite clause senza DB
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  to_table(M,[Head:_],TabDir,[Head1:_]),
  add_bdd_arg(Head1,Env,BDDAnd,M,Head2),
  append(TabDir,[(Head2 :- Body2)],Clauses).

user:term_expansion(Head,
  [rule_by_num(R,HeadList,[],VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
% disjunctive FACT with more than one head atom e db
  Head=(_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  to_table(M,HeadList,TabDir,HeadList1),
  generate_rules_fact_db(HeadList1,_Env,VC1,R,Probs,0,Clauses0,M),
  append(TabDir,Clauses0,Clauses).

user:term_expansion(Head,[rule_by_num(R,HeadList,[],VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive fact with more than one head atom senza db
  Head=(_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs), %**** test single_var
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  to_table(M,HeadList,TabDir,HeadList1),
  generate_rules_fact(HeadList1,_Env,VC1,R,Probs,0,Clauses0,M),
  append(TabDir,Clauses0,Clauses).

user:term_expansion(Head,[rule_by_num(R,HeadList,[],VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive fact with uniform distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (_:P),
  nonvar(P),
  Head=(H:uniform(Var,D0)),!,
  length(D0,Len),
  Prob is 1.0/Len,
  maplist(gen_head(H,Prob,Var),D0,HeadList),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs), %**** test single_var
  extract_vars_list(HeadList,[],VC),
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  to_table(M,HeadList,TabDir,HeadList1),
  (M:local_pita_setting(single_var,true)->
    generate_rules_fact(HeadList1,_Env,[],R,Probs,0,Clauses0,M)
  ;
    generate_rules_fact_vars(HeadList1,_Env,R,Probs,0,Clauses0,M)
  ),
  append(TabDir,Clauses0,Clauses).


user:term_expansion(Head,[rule_by_num(R,HeadList,[],VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (_:P),
  nonvar(P),
  (Head=(H:discrete(Var,D));Head=(H:finite(Var,D))),!,
  maplist(gen_head_disc(H,Var),D,HeadList),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs), %**** test single_var
  extract_vars_list(HeadList,[],VC),
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  to_table(M,HeadList,TabDir,HeadList1),
  (M:local_pita_setting(single_var,true)->
    generate_rules_fact(HeadList1,_Env,[],R,Probs,0,Clauses0,M)
  ;
    generate_rules_fact_vars(HeadList1,_Env,R,Probs,0,Clauses0,M)
  ),
  append(TabDir,Clauses0,Clauses).

user:term_expansion(Head,[]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive fact with a single head atom con prob. 0
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head = (_:P); Head = (P::_)),
  ground(P),
  P=:=0.0, !.

user:term_expansion(Head,Clauses) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
% disjunctive fact with a single head atom con prob.1 e db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head = (_H:P); Head = (P::_H)),
  ground(P),
  P=:=1.0, !,
  list2and([onec(Env,BDD)],Body1),
  to_table(M,[Head:_],TabDir,[H1:_]),
  add_bdd_arg_db(H1,Env,BDD,_DB,M,Head1),
  append(TabDir,[(Head1 :- Body1)],Clauses).

user:term_expansion(Head,Clauses) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head = (_H:P);Head =(P::_H)),
  ground(P),
  P=:=1.0, !,
  list2and([onec(Env,BDD)],Body1),
  to_table(M,[Head:_],TabDir,[H1:_]),
  add_bdd_arg(H1,Env,BDD,M,Head1),
  append(TabDir,[(Head1 :- Body1)],Clauses).

user:term_expansion(Head,[rule_by_num(R,HeadList,[],VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
% disjunctive fact with a single head atom e prob. generiche, con db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head=(_H:_);Head=(_::_H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  to_table(M,HeadList,TabDir,[H1:_]),
  add_bdd_arg_db(H1,Env,BDD,_DB,M,Head1),
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  Clauses0=[(Head1:-(get_var_n(M,Env,R,VC1,Probs,V),equalityc(Env,V,0,BDD)))],
  append(TabDir,Clauses0,Clauses).

user:term_expansion(Head,[rule_by_num(R,HeadList,[],VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive fact with a single head atom e prob. generiche, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head=(_H:_);Head=(_::_H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  to_table(M,HeadList,TabDir,[H1:_]),
  add_bdd_arg(H1,Env,BDD,M,Head1),%***test single_var
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  Clauses0=[(Head1:-(get_var_n(M,Env,R,VC1,Probs,V),equalityc(Env,V,0,BDD)))],
  append(TabDir,Clauses0,Clauses).

user:term_expansion((:- set_pita(P,V)), []) :-!,
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  set_pita(P,V).

user:term_expansion((:- set_sw(A,B)), []) :-!,
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  set_sw(M:A,B).


user:term_expansion(Head, Clauses) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
% definite fact with db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  to_table(M,[Head:_],TabDir,[Head1:_]),
  add_bdd_arg_db(Head1,Env,One,_DB,M,Head2),
  append(TabDir,[(Head2:-onec(Env,One))],Clauses).

user:term_expansion(Head, Clauses) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% definite fact without db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),
  to_table(M,[Head:_],TabDir,[Head1:_]),
  add_bdd_arg(Head1,Env,One,M,Head2),
  append(TabDir,[(Head2:-onec(Env,One))],Clauses).

/**
 * begin_lpad_pred is det
 *
 * Initializes LPAD loading.
 */
begin_lpad_pred:-
  assert(pita_input_mod(user)),
  assert(user:pita_on).

/**
 * end_lpad_pred is det
 *
 * Terminates the cplint inference module.
 */
end_lpad_pred:-
  retractall(pita_input_mod(_)),
  retractall(user:pita_on).

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
sandbox:safe_primitive(pita:get_abd_var_n(_,_,_,_,_,_)).
sandbox:safe_primitive(pita:get_var_n(_,_,_,_,_,_)).
sandbox:safe_primitive(pita:add_var(_,_,_,_,_)).
sandbox:safe_primitive(pita:equality(_,_,_,_)).
*/
:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(pita:s(_,_), []).
sandbox:safe_meta(pita:prob(_,_), []).
sandbox:safe_meta(pita:abd_prob(_,_,_), []).
sandbox:safe_meta(pita:vit_prob(_,_,_), []).
sandbox:safe_meta(pita:prob(_,_,_), []).
sandbox:safe_meta(pita:bdd_dot_file(_,_,_), []).
sandbox:safe_meta(pita:bdd_dot_string(_,_,_), []).
sandbox:safe_meta(pita:abd_bdd_dot_string(_,_,_,_), []).
sandbox:safe_meta(pita:abd_bdd_dot_string(_,_,_,_,_,_), []).
sandbox:safe_meta(pita:map_bdd_dot_string(_,_,_,_,_,_), []).
sandbox:safe_meta(pita:vit_bdd_dot_string(_,_,_,_,_), []).
sandbox:safe_meta(pita:msw(_,_,_,_), []).
sandbox:safe_meta(pita:msw(_,_,_,_,_), []).
sandbox:safe_meta(pita:set_pita(_,_),[]).
sandbox:safe_meta(pita:setting_pita(_,_),[]).




:- license(artisticv2).