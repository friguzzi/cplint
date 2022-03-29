:- module(pita,[
  prob/2,
  prob/3,
  prob_meta/2,
  prob_meta/3,
  abd_prob/3,
  bdd_dot_file/3,
  bdd_dot_string/3,
  abd_bdd_dot_string/4,
  abd_bdd_dot_string/6,
  map_bdd_dot_string/6,
  map/3,
  set_pita/2,setting_pita/2,
  get_var_n/6,get_abd_var_n/6,
  get_dec_var_n/5,
  load/1,load_file/1,
  % dt_solve_complete/2, % complete solution without pruning
  dt_solve/2,
  % op(600,fx,'?'),
  op(600,xfy,'::'),
  op(500,fx,'?::'),
  op(600,xfx,'=>'),
  op(1150,fx,action),
  op(1200,fy,map_query),
  op(1200,fy,abducible),
  msw/4,
  msw/5
    ]).
/** <module> pita

This module performs reasoning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It reads probabilistic program and computes the probability of queries.

See http://friguzzi.github.io/cplint/_build/html/index.html for
details.

@author Fabrizio Riguzzi
@license Artistic License 2.0 https://opensource.org/licenses/Artistic-2.0
@copyright Fabrizio Riguzzi
*/
:- reexport(library(cplint_util)).
:- reexport(library(bddem)).

% :- prolog_debug(chk_secure).

:-meta_predicate abd_prob(:,-,-).
:-meta_predicate prob(:,-).
:-meta_predicate prob(:,:,-).
:-meta_predicate prob(:,:,-,+).
:-meta_predicate prob_meta(:,-).
:-meta_predicate prob_meta(:,:,+).
:-meta_predicate bdd_dot_file(:,+,-).
:-meta_predicate bdd_dot_string(:,-,-).
:-meta_predicate abd_bdd_dot_string(:,-,-,-).
:-meta_predicate abd_bdd_dot_string(:,-,-,-,-,-).
:-meta_predicate map(:,-,-).
:-meta_predicate map_bdd_dot_string(:,-,-,-,-,-).
:-meta_predicate msw(:,-,-,-).
:-meta_predicate msw(:,-,-,-,-).
:-meta_predicate get_p(:,+,-).
:-meta_predicate get_cond_p(:,:,+,-).
:-meta_predicate get_node(:,+,-).
:-meta_predicate get_cond_node(:,:,+,-,-).
:-meta_predicate set_pita(:,+).
:-meta_predicate setting_pita(:,-).
:-meta_predicate set_sw(:,+).
% :-meta_predicate dt_solve_complete(:,-).
:-meta_predicate dt_solve(:,-).

% :- dynamic utility/2.

:-use_module(library(lists)).
:-use_module(library(apply)).
:-use_module(library(assoc)).

:- style_check(-discontiguous).

:- thread_local rule_n/1,goal_n/1,pita_input_mod/1,local_pita_setting/2.




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
default_setting_pita(prism_memoization,false). %false: original prism semantics, true: semantics with memoization
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
 * dt_solve_complete(-Strategy:list,-Cost:float) is det
 *
 * The predicate computes the best solution for the decision theory
 * problem. It returns the best strategy in Strategy and it cost
 * in Cost. Complete solution without pruning.
 */

% this is a complete version, rename it dt_solve_complete
% when the other version works
dt_solve(M:Strategy,Cost):-
  abolish_all_tables,
  findall([H,U],M:'$util'(H,U),LUtils),
  init(Env),
  % statistics(walltime,[Start|_]),
  generate_solution(Env,M,LUtils,[],St,Cost),
  % statistics(walltime,[Stop|_]),
  end(Env),
  % Runtime is Stop - Start,
  % format('Runtime: ~w~n',[Runtime]),
  maplist(pair(M),St,Strategy).

pair(M,A,B):- M:rule_by_num(A,B,_,_).
split([A,B],A,B).

get_bdd(_,_,[],L,L):- !.
get_bdd(M,Env,[G|T],L,LO):-
  get_node(M:G,Env,Out),
  % writeln(Out),
  Out=(_,BDD),
  append(L,[BDD],LT),
  get_bdd(M,Env,T,LT,LO).

% compute the solution for dt problem
% generate_solution/6
% generate_solution(Env,M,GoalCostList,CurrentAdd,Solution,Cost)
% output Solution, Cost
generate_solution(Env,_,[],Add,Solution,Cost):- !,
  % create_dot(Env,Add,"final.dot"),
  ret_strategy(Env,Add,Solution,Cost).

generate_solution(Env,M,[[G,Cost]|TC],CurrentAdd,Solution,OptCost):-
  get_node(M:G,Env,Out),
  Out=(_,BDD),
  probability_dd(Env,BDD,AddConv),
  add_prod(Env,AddConv,Cost,AddScaled),
  (CurrentAdd = [] ->
    AddOut = AddScaled ;
    % writeln(CurrentAdd),
    add_sum(Env,CurrentAdd,AddScaled,AddOut)
    % writeln("sum"),
  ),
  generate_solution(Env,M,TC,AddOut,Solution,OptCost).

/**
 * dt_evaluate_strategy(+Strategy:List,-Cost:float) is det
 *
 * Computes the cost of the selected strategy
 */
% TODO
% dt_evaluate_strategy(LS,Cost).

/**
 * prob_meta(:Query:atom,-Probability:float) is nondet
 *
 * To be used in place of prob/2 for meta calls (doesn't abolish tables)
 */
prob_meta(M:Goal,P):-
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
  init(Env),
  findall((Goal,P),get_p(M:Goal1,Env,P),L),
  end(Env),
  erase(Ref),
  member((Goal,P),L).

/**
 * abd_prob(:Query:atom,-Probability:float,-Delta:list) is nondet
 *
 * The predicate computes the most probable abductive explanation of the ground query Query.
 * It returns the explanation in Delta together with its Probability
 */
abd_prob(M:Goal,P,Delta):-
  abolish_all_tables,
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
  init(Env),
  findall((Goal,P,Exp),get_abd_p(M:Goal1,M:'$constraints',Env,P,Exp),L),
  end(Env),
  erase(Ref),
  member((Goal,P,Exp),L),
  maplist(from_assign_to_exp(M),Exp,DeltaAll),
  simplify_delta(DeltaAll,Delta).

subset_([], []).
subset_([E|Tail], [E|NTail]):-
  subset_(Tail, NTail).
subset_([_|Tail], NTail):-
  subset_(Tail, NTail).

mycompare(<,L1,L2) :- length(L1,A1), length(L2,A2), A1 < A2.
mycompare(>, _, _).

sub([A],[A]).
sub([H|T],[V|R]):-
  findall(X,(subset_(H,X), member(X,T)),LX),
  (   LX = [] ->
    V = H,
    sub(T,R) ;
    sub(T,[V|R]) 
  ).

simplify_delta([],[]):- !.
simplify_delta(Din,Delta):-
  predsort(mycompare,Din,LS),
  reverse(LS,LNR),
  sub(LNR,Delta), !.



from_assign_to_exp(_M,[],[]):- !.
from_assign_to_exp(M,[Var-Val|TA],[Abd|TDelta]):-
  M:av(R,S,Var),
  M:abd(R,S,H),
  (Val=1->
    Abd=H
  ;
    Abd= \+(H)
    % Abd= []
  ),
  from_assign_to_exp(M,TA,TDelta).


/**
 * bdd_dot_file(:Query:atom,+FileName:string,-LV:list) is det
 *
 * The predicate builds the BDD for Query and writes its dot representation
 * to file FileName and a list in LV with the association of variables to rules.
 * LV is a list of list, each sublist has three elements:
 * the multivalued variable number,
 * the rule number and the grounding substitution.
 */
bdd_dot_file(M:Goal,File,LV):-
  abolish_all_tables,
  init(Env),
  get_node(M:Goal,Env,Out),
  Out=(_,BDD),!,
  findall([V,R,S],M:v(R,S,V),LV),
  create_dot(Env,BDD,File),
  end(Env).

/**
 * bdd_dot_string(:Query:atom,-DotString:string,-LV:list) is det
 *
 * The predicate builds the BDD for Query and returns its dot representation
 * in DotString and a list in LV with the association of variables to rules.
 * LV is a list of list, each sublist has three elements:
 * the multivalued variable number,
 * the rule number and the grounding substitution.
 */
bdd_dot_string(M:Goal,dot(Dot),LV):-
  abolish_all_tables,
  init(Env),
  get_node(M:Goal,Env,Out),
  Out=(_,BDD),!,
  findall([V,R,S],M:v(R,S,V),LV),
  create_dot_string(Env,BDD,Dot),
  end(Env).

/**
 * abd_bdd_dot_string(:Query:atom,-DotString:string,-LV:list,-LAV:list) is det
 *
 * The predicate builds the BDD for the abductive explanations for Query
 * and returns its dot representation
 * in DotString and lists LV and LAV, the association of variables to rules
 * and to abductive variables to rules respectively.
 * LV and LAV are lists of list, each sublist has three elements:
 * the multivalued variable number,
 * the rule number and the grounding substitution.
 */
abd_bdd_dot_string(M:Goal,dot(Dot),LV,LAV):-
  abd_bdd_dot_string(M:Goal,dot(Dot),LV,LAV,_P,_Delta).

/**
 * abd_bdd_dot_string(:Query:atom,-DotString:string,-LV:list,-LAV:list,-Probability:float,-Delta:list) is det
 *
 * The predicate builds the BDD for the abductive explanations for Query
 * It returns the explanation in Delta together with its Probability.
 * The predicate builds the BDD for Query and returns its dot representation
 * in DotString and lists LV and LAV, the association of variables to rules
 * and to abductive variables to rules respectively.
 * LV and LAV are lists of list, each sublist has three elements:
 * the multivalued variable number,
 * the rule number and the grounding substitution.
 */
abd_bdd_dot_string(M:Goal,dot(Dot),LV,LAV,P,Delta):-
  abolish_all_tables,
  init(Env),
  get_cond_node(M:Goal,M:'$constraints',Env,Out,_),
  Out=(_,BDD), !,
  ret_abd_prob(Env,BDD,P,Exp),
  create_dot_string(Env,BDD,Dot),
  end(Env),
  maplist(from_assign_to_exp(M),Exp,Delta),
  findall([V,R,S],M:v(R,S,V),LV),
  findall([V,R,S],M:av(R,S,V),LAV).

/**
 * map(:Query:atom,-Probability:float,-Delta:list) is nondet
 *
 * The predicate computes the explanation of the ground query Query
 * with Maximum A Posteriori (MAP) probability.
 * It returns the explanation in Delta together with its Probability
 */
map(M:Goal,P,MAP):-
  map_int(Goal,M,_LV,_LAV,P,MAP,Env,_BDD),
  end(Env).

/**
 * map_bdd_dot_string(:Query:atom,-DotString:string,-LV:list,-LAV:list,-Probability:float,-Delta:list) is nondet
 *
 * The predicate computes the explanation of the ground query Query
 * with Maximum A Posteriori (MAP) probability.
 * It returns the explanation in Delta together with its Probability
 * The predicate builds the BDD for Query and returns its dot representation
 * in DotString and lists LV and LAV, the association of variables to rules
 * and of query variables to rules respectively.
 * LV and LAV are lists of list, each sublist has three elements:
 * the multivalued variable number,
 * the rule number and the grounding substitution.
 */
map_bdd_dot_string(M:Goal,dot(Dot),LV,LAV,P,MAP):-
  map_int(Goal,M,LV,LAV,P,MAP,Env,BDD),
  create_dot_string(Env,BDD,Dot),
  end(Env).


map_int(Goal,M,LV,LAV,P,MAP,Env,BDD):-
  abolish_all_tables,
  init(Env),
  get_node(M:Goal,Env,Out),
  (Out=(_,BDD0)),!,
  findall([V,R,S],M:v(R,S,V),LV),
  one(Env,One),
  make_query_vars(LV,M,Env,One,Cons,LAV),
  and(Env,BDD0,Cons,BDD),
  ret_map_prob(Env,BDD,P,Exp0),
  reverse(Exp0,Exp),
  from_assign_to_map(Exp,M,MAP).


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
  abolish_all_tables,
  prob_meta(M:Goal,P).


/**
 * prob(:Query:atom,:Evidence:atom,-Probability:float) is nondet
 *
 * Equivalent to prob/4 with an empty option list.
 */
prob(M:Goal,M:Evidence,P):-
  abolish_all_tables,
  prob_meta(M:Goal,M:Evidence,P).

/**
 * prob_meta(:Query:atom,:Evidence:atom,-Probability:float) is nondet
 *
 * To be used in place of prob/3 for meta calls (doesn't abolish tables)
 */
prob_meta(M:Goal,M:Evidence,P):-
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
  init(Env),
  (EvNoAct=true->
    findall((Goal,P),get_p(M:Goal1,Env,P),L)
  ;
    findall((Goal,P),get_cond_p(M:Goal1,M:EvNoAct,Env,P),L)
  ),
  end(Env),
  retractall(M:NewEv),
  maplist(erase,UpdatedClausesRefs),
  erase(Ref),
  maplist(M:assertz,ClausesToReAdd),
  member((Goal,P),L).


/**
 * prob(:Query:atom,:Evidence:atom,-Probability:float,+Options:list) is nondet
 *
 * The predicate computes the probability of Query given
 * Evidence
 * If Query/Evidence are not ground, it returns in backtracking all
 * ground instantiations of
 * Query/Evidence together with their probabilities
 * Options is a list of options, the following are recognised by mc_prob/3:
 * * bar(-BarChart:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with a bar for the
 *   probability of success and a bar for the probability of failure.
 */
prob(M:Goal,M:Evidence,P,Options):-
  prob(M:Goal,M:Evidence,P),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    bar(P,Chart)
  ).


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
  A1=..[P|Args1],
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
  G1=..[_|Args1],
  append(Args,[_,_],Args1),
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
  (get_assoc(F/A,AP0,V)->
    put_assoc(F/A,AP0,[Args|V],AP)
  ;
    put_assoc(F/A,AP0,[Args],AP)
  ).


ac(do(_)).
nac(do(\+ _)).


get_p(M:Goal,Env,P):-
  get_node(M:Goal,Env,BDD),
  ret_probc(Env,BDD,P).


get_abd_p(M:Goal,M:Evidence,Env,P,Exp):-
  % get_node_no_rec(M:Evidence,Env,OutIC),
  get_cond_node(M:Goal,M:Evidence,Env,Out,_),
  % get_node(M:Evidence,Env,OutIC),
  Out=(_,BDD),
  % OutIC = (_,BDDIC),
  ret_abd_prob(Env,BDD,P,Exp).

get_cond_p(M:Goal,M:Evidence,Env,P):-
  get_cond_node(M:Goal,M:Evidence,Env,BDDGE,BDDE),
  ret_probc(Env,BDDE,PE),
  ret_probc(Env,BDDGE,PGE),
  ( PE =:= 0 -> 
      writeln("Undefined: probability of evidence 0.") ;
      P is PGE/PE
  ).


get_node(M:Goal,Env,BDD):-
  M:local_pita_setting(depth_bound,true),!,
  M:local_pita_setting(depth,DB),
  retractall(M:v(_,_,_)),
  retractall(M:av(_,_,_)),
  retractall(M:dec(_,_,_)),
  add_bdd_arg_db(Goal,Env,BDD,DB,M,Goal1),%DB=depth bound
  (M:Goal1*->
    true
  ;
    zeroc(Env,BDD)
  ).

get_node_no_rec(M:Goal,Env,BDD):- %with DB=false
  retractall(M:v(_,_,_)),
  retractall(M:v(_,_,_)),
  retractall(M:av(_,_,_)),
  add_bdd_arg(Goal,Env,BDD,M,Goal1),
  (M:Goal1*->
    true
  ;
    zeroc(Env,BDD)
    % format("-------------------------Failed goal: ~w ~n",[M:Goal])
  ).

get_node(M:Goal,Env,BDD):- %with DB=false
  retractall(M:v(_,_,_)),
  retractall(M:av(_,_,_)),
  retractall(M:dec(_,_,_)),
  add_bdd_arg(Goal,Env,BDD,M,Goal1),
  (M:Goal1*->
    true
  ;
    zeroc(Env,BDD)
    % format("-------------------------Failed goal: ~w ~n",[M:Goal])
  ).

get_cond_node(M:Goal,M:Ev,Env,BGE,BDDE):-
  M:local_pita_setting(depth_bound,true),!,
  M:local_pita_setting(depth,DB),
  retractall(M:v(_,_,_)),
  retractall(M:av(_,_,_)),
  retractall(M:dec(_,_,_)),
  add_bdd_arg_db(Goal,Env,BDD,DB,M,Goal1),%DB=depth bound
  (M:Goal1*->
    true
  ;
    zeroc(Env,BDD)
  ),
  add_bdd_arg_db(Ev,Env,BDDE,DB,M,Ev1),%DB=depth bound
  (M:Ev1*->
    true
  ;
    zeroc(Env,BDDE)
  ),
  andcnf(Env,BDD,BDDE,BGE).



get_cond_node(M:Goal,M:Ev,Env,BGE,BDDE):- %with DB=false
  retractall(M:v(_,_,_)),
  retractall(M:av(_,_,_)),
  retractall(M:dec(_,_,_)),
  add_bdd_arg(Goal,Env,BDD,M,Goal1),
  (M:Goal1*->
    true
  ;
    zeroc(Env,BDD)
  ),
  add_bdd_arg(Ev,Env,BDDE,M,Ev1),
  (M:Ev1*->
    true
  ;
    zeroc(Env,BDDE)
  ),
  andcnf(Env,BDD,BDDE,BGE).


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
 * index Rule, grounding substitution Substitution and head distribution
 * Probabilities in environment Environment.
 */
get_var_n(M,Env,R,S,Probs0,V):-
  M:query_rule(R,_H,_B,_S),!,
  (ground(Probs0)->
    maplist(is,Probs,Probs0),
    (M:v(R,S,V)->
      true
    ;
      add_query_var(Env,Probs,R,V),
      assert(M:v(R,S,V))
    )
  ;
    throw(error('Non ground probabilities not instantiated by the body'))
  ).

get_var_n(M,Env,R,S,Probs0,V):-
  (ground(Probs0)->
    maplist(is,Probs,Probs0),
    (M:v(R,S,V)->
      true
    ;
      % format("P: ~w ~w ~n",[Probs,R]),
      add_var(Env,Probs,R,V),
      assert(M:v(R,S,V))
    )
  ;
    throw(error('Non ground probabilities not instantiated by the body'))
  ).

/**
 * get_dec_var_n(++M:atomic,++Environment:int,++Rule:int,++Substitution:term,-Variable:int) is det
 *
 * Returns the index Variable of the random variable associated to rule with
 * index Rule in environment Environment.
 */
 get_dec_var_n(M,Env,R,S,V):-
  % format('get_dec_var: R: ~w - S: ~w - V: ~w - M: ~w ~n', [R,S,V,M]),
  ( M:dec(R,S,V) ->
  % findall([A,B,C],M:dec(A,B,C),LD),
    % writeln(LD),
    true ;
    add_decision_var(Env,R,V),
    % writeln("New dec var"),
    asserta(M:dec(R,S,V))
  ).
  % (M:v(R,S,V)->
  %     true
  %   ;
  %     % add_var(Env,1,R,V),
  %     assert(M:v(R,S,V))
  % ).

/**
 * get_abd_var_n(++M:atomic,++Environment:int,++Rule:int,++Substitution:term,++Probabilities:list,-Variable:int) is det
 *
 * Returns the index Variable of the random variable associated to rule with
 * index Rule, grounding substitution Substitution and head distribution
 * Probabilities in environment Environment.
 */
get_abd_var_n(M,Env,R,S,Probs0,V):-
  (ground(Probs0)->
    maplist(is,Probs,Probs0),
    (M:av(R,S,V)->
      true
    ;
      add_abd_var(Env,Probs,R,V),
      assert(M:av(R,S,V))
    )
  ;
    throw(error('Non ground probabilities not instantiated by the body'))
  ).

/**
 * msw(:Var:term,?Value:term,++Environment:int,--BDD:int) is det
 *
 * Returns a BDD representing Var=Value.
 * This is a predicate for programs in the PRISM syntax
 */
msw(M:A,B,Env,BDD):-
  msw_int(M,A,B,Env,BDD).

/**
 * msw(:Var:term,?Value:term,++Environment:int,--BDD:int,?DB:int) is det
 *
 * Returns a BDD representing Var=Value when there is a depth bound on
 * derivations.
 * This is a predicate for programs in the PRISM syntax
 */
msw(M:A,B,Env,BDD,_DB):-
  msw_int(M,A,B,Env,BDD).

msw_int(M,A,B,Env,BDD):-
  M:values(A,Values),
  M:sw(R,A,Probs0),
  (ground(Probs0)->
    maplist(is,Probs,Probs0),
    ((M:local_pita_setting(prism_memoization,true),M:v(R,A,V))->
      true
    ;
      add_var(Env,Probs,R,V)
    ),
    (M:local_pita_setting(prism_memoization,true)->
      assert(M:v(R,A,V))
    ;
      true
    ),
    nth0(N,Values,B),
    equalityc(Env,V,N,BDD)
  ;
    throw(error('Non ground probabilities not instantiated by the body'))
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
  term_variables([Head],VC),
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(get_var_n(Module,Env,R,VC,Probs,V),equalityc(Env,V,N,BDD))).

generate_rules_fact_vars([Head:_P|T],Env,R,Probs,N,[Clause|Clauses],Module):-
  term_variables([Head],VC),
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
[H1,bdd_notc(Env,BDDH,BDDN),
  andc(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
  add_bdd_arg(H,Env,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H1|Rest],Env,Module):-
  transform(H,H1),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

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
[H1,bdd_notc(Env,BDDH,BDDN),
  andc(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H1|Rest],Env,Module):-
  transform(H,H1),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

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


process_head(HeadList, GroundHeadList1) :-
  ground_prob(HeadList), !,
  process_head_ground(HeadList, 0.0, GroundHeadList),
  ( GroundHeadList = [V:P] -> 
      P1 is 1.0 - P, 
      GroundHeadList1 = [V:P,'':P1] ; 
      GroundHeadList1 = GroundHeadList
  ).

process_head(HeadList0, HeadList):-
  get_probs(HeadList0,PL),
  foldl(minus,PL,1.0,PNull),
  append(HeadList0,['':PNull],HeadList).

minus(A,B,B-A).

prob_ann(_:P0,P):-!, to_float(P0,P).
prob_ann(P0::_,P):- to_float(P0, P).

to_float(P0, P) :-
  ground(P0), !,
  P is float(P0).
to_float(P, P).

gen_head(H,P,VH,V,V1,H1:P):-copy_term((H,VH,V),(H1,VH,V1)).
gen_head_disc(H,VH,V,V1:P,H1:P1):-copy_term((H,VH,V),(H1,VH,V1)),P1 is float(P).


/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([H], Prob, [Head:ProbHead1|Null]) :-
  (H=Head:ProbHead;H=ProbHead::Head),!,
  ProbHead1 is float(ProbHead),
  ProbLast is 1.0 - Prob - ProbHead1,
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
  ProbHead1 is float(ProbHead),
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
  M:(dynamic A/B1).

tab(M,A/B,P):-
  length(Args0,B),
  (M:local_pita_setting(depth_bound,true)->
    ExtraArgs=[_,_,lattice(orc/3)]
  ;
    ExtraArgs=[_,lattice(orc/3)]
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
  maplist(system:term_expansion,TabDir,ProcTabDirL),
  append(ProcTabDirL,ProcTabDir),
  append(Heads1L,Heads1).

tab_dir(_M,'':_,[],[]):-!.

% tab dir for decision variables
% merge with the previous one?
% the predicates are equal except
% (?)::H and H:P
tab_dir(M,D::H,[],[H]):-
  (D == ? ; D == (?)),
  M:tabled(H),!.
% tab dir for decision variables
% merge with the previous one?
% the predicates are equal except
% (?)::H and '$util'(A,B)
tab_dir(M,H,[],[H]):-
  H=..[F|_],
  F = utility,
  M:tabled(H),!.

tab_dir(M,H:P,[],[H:P]):-
  M:tabled(H),!.

tab_dir(M,P::H,[],[H:P]):-
  P \== ?,
  M:tabled(H),!.



% tab dir for decision variables
% merge with the previous one?
% the predicates are equal
% except variable n 2 and 4.
tab_dir(M,P::H,[(:- table HT)],[H1]):-
  (P== ?;P == (?)),!,
  functor(H,F,A0),
  functor(PT,F,A0),
  PT=..[F|Args0],
  (M:local_pita_setting(depth_bound,true)->
    ExtraArgs=[_,_,lattice(orc/3)]
  ;
    ExtraArgs=[_,lattice(orc/3)]
  ),
  append(Args0,ExtraArgs,Args),
  HT=..[F|Args],
  H=..[_|ArgsH],
  H1=..[F|ArgsH],
  assert(M:tabled(PT)),
  zero_clause(M,F/A0,LZ),
  assert(M:zero_clauses(LZ)).

% tab dir for utility variables
% merge with the previous one?
% the predicates are equal
% except variable n 2 and 4.
tab_dir(M,H,[(:- table HT)],[H1]):-
  H=..[F|_],
  F = utility,!,
  functor(H,F,A0),
  functor(PT,F,A0),
  PT=..[F|Args0],
  (M:local_pita_setting(depth_bound,true)->
    ExtraArgs=[_,_,lattice(orc/3)]
  ;
    ExtraArgs=[_,lattice(orc/3)]
  ),
  append(Args0,ExtraArgs,Args),
  HT=..[F|Args],
  H=..[_|ArgsH],
  H1=..[F|ArgsH],
  assert(M:tabled(PT)),
  zero_clause(M,F/A0,LZ),
  assert(M:zero_clauses(LZ)).


tab_dir(M,Head,[(:- table HT)],[H1:P]):-
  (Head=H:P;Head=P::H),!,
  functor(H,F,A0),
  functor(PT,F,A0),
  PT=..[F|Args0],
  (M:local_pita_setting(depth_bound,true)->
    ExtraArgs=[_,_,lattice(orc/3)]
  ;
    ExtraArgs=[_,lattice(orc/3)]
  ),
  append(Args0,ExtraArgs,Args),
  HT=..[F|Args],
  H=..[_|ArgsH],
  H1=..[F|ArgsH],
  assert(M:tabled(PT)),
  zero_clause(M,F/A0,LZ),
  assert(M:zero_clauses(LZ)).


pita_expansion(begin_of_file,_):-
  !,
  fail.

pita_expansion((:- action Conj), []) :-!,
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  list2and(L,Conj),
  maplist(act(M),L).

pita_expansion((:- begin_plp), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  assert(M:pita_on).

pita_expansion((:- end_plp), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  retractall(M:pita_on).

pita_expansion((:- begin_lpad), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  assert(M:pita_on).

pita_expansion((:- end_lpad), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  retractall(M:pita_on).

pita_expansion(values(A,B), values(A,B)) :-
  prolog_load_context(module, M),
  pita_input_mod(M),M:pita_on,!.

pita_expansion((:- Constraint), Clauses) :-
  % constraint for abduction
  prolog_load_context(module, M),
  pita_input_mod(M),
  M:pita_on,
  Constraint\= (table _),
  Constraint\=(multifile _),
  Constraint\=set_sw(_,_),!,
  list2and(BodyList, Constraint),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  to_table(M,['$cons':_],TabDir,[Head1:_]),
  add_bdd_arg(Head1,Env,BDDAnd,M,Head2),
  append(TabDir,[(Head2 :- Body2)],Clauses).

pita_expansion((Prob:- Constraint), Clauses) :-
  % probabilistic constraint for abduction
  prolog_load_context(module, M),
  pita_input_mod(M),
  M:pita_on,
  float(Prob),
  Constraint\= (table _),
  Constraint\=(multifile _),
  Constraint\=set_sw(_,_),!,
  list2or(HeadListOr, '$cons':Prob),
  process_head(HeadListOr, HeadList),
  list2and(BodyList, Constraint),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
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



pita_expansion(map_query(Clause),[query_rule(R,HeadList,Body,VC)|Clauses]):-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,!,
  M:rule_n(R),
  pita_expansion(Clause, Clauses0),
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
  term_variables(Clause,VC),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList).

pita_expansion(abducible(Head),[Clause,abd(R,S,H)]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,!,
  ((Head=(H:P);Head=(P::H))->
    P1 is P,
    P0 is 1.0-P,
    Probs=[P1,P0]
  ;
    H=Head,
    Probs=[1.0,1.0]
  ),
  term_variables([H],VC),
  get_next_rule_number(M,R),
  add_bdd_arg(H,Env,BDD,M,Head1), %***test single_var
  (M:local_pita_setting(single_var,true)->
    S=[]
  ;
    S=VC
  ),
  Clause=(Head1:-(get_abd_var_n(M,Env,R,S,Probs,V),equalityc(Env,V,0,BDD))).

% decision facts with body and ground variables
% ?::a:- b.
pita_expansion(Head:-Body,[Clause,rule_by_num(R,H,Body1,[]),TabDir]) :-
  prolog_load_context(module, M),
  pita_input_mod(M),
  M:pita_on,
  ((Head:- Body) \= ((pita_expansion(_,_)) :- _ )),
  (Head \= ((pita_expansion(_,_)) :- _ )),
  (Head = ((?) :: H) ; Head = decision(H)), ground(H), !,
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Env,M),
  append([onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append([Head],BodyList,List),
  term_variables(List,VC),
  get_next_rule_number(M,R),
  to_table(M,[Head],TabDir,HeadList1),
  HeadList1 = [H1],
  add_bdd_arg(H1,Env,BO,M,Head1),
  Clause = (Head1:-(Body1,get_dec_var_n(M,Env,R,VC,V), equalityc(Env,V,0,B), andc(Env,BDDAnd,B,BO))).

% decision facts without body and ground variables
% ?::a.
pita_expansion(Head,[Clause,rule_by_num(R,[H],[],VC),TabDir]) :-
  prolog_load_context(module, M),
  pita_input_mod(M),
  M:pita_on,
  (Head \= ((pita_expansion(_,_)) :- _ )),
  (Head = ((?) :: H) ; Head = decision(H)), ground(H), !,
  term_variables([Head],VC), % VC is [] so maybe avoid the computation
  get_next_rule_number(M,R),
  to_table(M,[Head],TabDir,HeadList1),
  HeadList1 = [H1],
  add_bdd_arg(H1,Env,BDD,M,Head1),
  Clause = (Head1:-(get_dec_var_n(M,Env,R,VC,V),equalityc(Env,V,0,BDD))).

% utility attributes with body
% utility(a,N):- b.
pita_expansion(Head:-Body,[Clause,TabDir,'$util'(H,U)]) :-
  prolog_load_context(module, M),
  pita_input_mod(M),
  M:pita_on,
  (Head \= ((pita_expansion(_,_)) :- _ )),
  (Head = (H => U) ; Head = utility(H,U)),
  ( ground(H) -> true ; throw(error("Expected ground decision fact in utility/2"))),
  ( number(U) -> true ; throw(error("Expected a number for utility in utility/2"))), !,
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Env,M),
  append([onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append([Head],BodyList,List),
  term_variables(List,VC),
  get_next_rule_number(M,R),
  to_table(M,[Head],TabDir,HeadList1),  % <---------------------- if HEAD = H => U does NOT WORKS
  HeadList1 = [H1],
  add_bdd_arg(H1,Env,BO,M,Head2),
  Clause = (Head2:-(Body1,get_var_n(M,Env,R,VC,V),equalityc(Env,V,0,B),andc(Env,BDDAnd,B,BO))).

% utility attributes without body
% utility(a,N).
pita_expansion(Head,'$util'(H,U)) :-
  prolog_load_context(module, M),
  pita_input_mod(M),
  M:pita_on,
  (Head \= ((pita_expansion(_,_)) :- _ )),
  (Head = (H => U) ; Head = utility(H,U)),
  ( ground(H) -> true ; throw(error("Expected ground decision fact in utility/2"))),
  ( number(U) -> true ; throw(error("Expected a number for utility in utility/2"))), !.

pita_expansion(Head:-Body,[rule_by_num(R,HeadList,BodyList,VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive clause with uniform distr
  (Head \= ((pita_expansion(_,_)) :- _ )),
  Head = (_:P),
  nonvar(P),
  Head=(H:uniform(Var,D0)),!,
  (var(D0)->
     throw(error('Non ground list of values in uniform(Var,Values)'))
  ;
    true
  ),
  length(D0,Len),
  Prob is 1.0/Len,
  term_variables([H],VH),
  delete_equal(VH,Var,VH1),
  maplist(gen_head(H,Prob,VH1,Var),D0,HeadList),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs), %**** test single_var
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Env,M),
  append([onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  to_table(M,HeadList,TabDir,HeadList1),
  generate_rules(HeadList1,Env,Body1,VC1,R,Probs,BDDAnd,0,Clauses0,M),
  append(TabDir,Clauses0,Clauses).


pita_expansion(Head:-Body,[rule_by_num(R,HeadList,BodyList,VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive clause with discrete distr
  (Head \= ((pita_expansion(_,_)) :- _ )),
  Head = (_:P),
  nonvar(P),
  (Head=(H:discrete(Var,D));Head=(H:finite(Var,D))),!,
  (var(D)->
     throw(error('Non ground list of values in discrete(Var,Values) or finite(Var,Values)'))
  ;
    true
  ),
  term_variables([H],VH),
  delete_equal(VH,Var,VH1),
  maplist(gen_head_disc(H,VH1,Var),D,HeadList),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs), %**** test single_var
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Env,M),
  append([onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  to_table(M,HeadList,TabDir,HeadList1),
  generate_rules(HeadList1,Env,Body1,VC1,R,Probs,BDDAnd,0,Clauses0,M),
  append(TabDir,Clauses0,Clauses).

pita_expansion((Head :- Body),
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
  term_variables(List,VC),
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


pita_expansion((Head :- Body),
  [rule_by_num(R,HeadList,BodyList,VC1)|Clauses]):-
	  %trace,
    ((Head:- Body) \= ((pita_expansion(_,_)) :- _ )),
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
  term_variables(List,VC),
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

pita_expansion((Head :- Body), []) :-
% disjunctive clause with a single head atom con prob. 0 senza depth_bound --> la regola non e' caricata nella teoria e non e' conteggiata in NR
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  ((Head:-Body) \= ((pita_expansion(_,_) ):- _ )),
  (Head = (_:P);Head=(P::_)),
  ground(P),
  P=:=0.0, !.

pita_expansion((Head :- Body), Clauses) :-
% disjunctive clause with a single head atom e depth_bound
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
  ((Head:-Body) \= ((pita_expansion(_,_) ):- _ )),
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

pita_expansion((Head :- Body), Clauses) :-
% disjunctive clause with a single head atom senza depth_bound con prob =1
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  ((Head:-Body) \= ((pita_expansion(_,_) ):- _ )),
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

pita_expansion((Head :- Body), Clauses) :-
% disjunctive clause with a single head atom e DB, con prob. diversa da 1
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
  ((Head:-Body) \= ((pita_expansion(_,_) ):- _ )),
  (Head = (_H:_);Head=(_::_H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
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

pita_expansion((Head :- Body), [rule_by_num(R,HeadList,BodyList,VC1)|Clauses]) :-
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  ((Head:-Body) \= ((pita_expansion(_,_) ):- _ )),
  (Head = (_H:_);Head = (_::_H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
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

/*pita_expansion((Head :- Body),Clauses) :-
% definite clause for db facts
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  ((Head:-Body) \= ((pita_expansion(_,_)) :- _ )),
  Head=db(Head1),!,
  Clauses=(Head1 :- Body).
*/
pita_expansion((Head :- Body),Clauses) :-
% definite clause with depth_bound
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
  ((Head:-Body) \= ((pita_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and([DBH>=1,DB is DBH-1|BodyList3],Body1),
  to_table(M,[Head:_],TabDir,[Head1:_]),
  add_bdd_arg_db(Head1,Env,BDDAnd,DBH,M,Head2),
  append(TabDir,[(Head2 :- Body1)],Clauses).

pita_expansion((Head :- Body),Clauses) :-
%  writeln((Head:-Body)),
 % trace,
% definite clause senza DB
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  ((Head:-Body) \= ((pita_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  to_table(M,[Head:_],TabDir,[Head1:_]),
  add_bdd_arg(Head1,Env,BDDAnd,M,Head2),
  append(TabDir,[(Head2 :- Body2)],Clauses).

pita_expansion(Head,
  [rule_by_num(R,HeadList,[],VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
% disjunctive FACT with more than one head atom e db
  Head=(_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  term_variables(HeadList,VC),
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

pita_expansion(Head,[rule_by_num(R,HeadList,[],VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive fact with more than one head atom senza db
  Head=(_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  term_variables(HeadList,VC),
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

pita_expansion(Head,[rule_by_num(R,HeadList,[],VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive fact with uniform distr
  (Head \= ((pita_expansion(_,_)) :- _ )),
  Head = (_:P),
  nonvar(P),
  Head=(H:uniform(Var,D0)),!,
  (var(D0)->
     throw(error('Non ground list of values in uniform(Var,Values)'))
  ;
    true
  ),
  length(D0,Len),
  Prob is 1.0/Len,
  term_variables([H],VH),
  delete_equal(VH,Var,VH1),
  maplist(gen_head(H,Prob,VH1,Var),D0,HeadList),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs), %**** test single_var
  term_variables(HeadList,VC),
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  to_table(M,HeadList,TabDir,HeadList1),
  generate_rules_fact(HeadList1,_Env,VC1,R,Probs,0,Clauses0,M),
  append(TabDir,Clauses0,Clauses).


pita_expansion(Head,[rule_by_num(R,HeadList,[],VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  % disjunctive fact with discrete distr
  (Head \= ((pita_expansion(_,_)) :- _ )),
  Head = (_:P),
  nonvar(P),
  (Head=(H:discrete(Var,D));Head=(H:finite(Var,D))),!,
  (var(D)->
     throw(error('Non ground list of values in discrete(Var,Values) or finite(Var,Values)'))
  ;
    true
  ),
  term_variables([H],VH),
  delete_equal(VH,Var,VH1),
  maplist(gen_head_disc(H,VH1,Var),D,HeadList),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs), %**** test single_var
  term_variables(HeadList,VC),
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  to_table(M,HeadList,TabDir,HeadList1),
  generate_rules_fact(HeadList1,_Env,VC1,R,Probs,0,Clauses0,M),
  append(TabDir,Clauses0,Clauses).

pita_expansion(Head,[]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive fact with a single head atom con prob. 0
  (Head \= ((pita_expansion(_,_)) :- _ )),
  (Head = (_:P); Head = (P::_)),
  ground(P),
  P=:=0.0, !.

pita_expansion(Head,Clauses) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
% disjunctive fact with a single head atom con prob.1 e db
  (Head \= ((pita_expansion(_,_)) :- _ )),
  (Head = (_H:P); Head = (P::_H)),
  ground(P),
  P=:=1.0, !,
  list2and([onec(Env,BDD)],Body1),
  to_table(M,[Head:_],TabDir,[H1:_]),
  add_bdd_arg_db(H1,Env,BDD,_DB,M,Head1),
  append(TabDir,[(Head1 :- Body1)],Clauses).

pita_expansion(Head,Clauses) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((pita_expansion(_,_)) :- _ )),
  (Head = (_H:P);Head =(P::_H)),
  ground(P),
  P=:=1.0, !,
  list2and([onec(Env,BDD)],Body1),
  to_table(M,[Head:_],TabDir,[H1:_]),
  add_bdd_arg(H1,Env,BDD,M,Head1),
  append(TabDir,[(Head1 :- Body1)],Clauses).

pita_expansion(Head,[rule_by_num(R,HeadList,[],VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
% disjunctive fact with a single head atom e prob. generiche, con db
  (Head \= ((pita_expansion(_,_)) :- _ )),
  (Head=(_H:_);Head=(_::_H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  term_variables(HeadList,VC),
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

pita_expansion(Head,[rule_by_num(R,HeadList,[],VC1)|Clauses]) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% disjunctive fact with a single head atom e prob. generiche, senza db
  (Head \= ((pita_expansion(_,_)) :- _ )),
  (Head=(_H:_);Head=(_::_H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  term_variables(HeadList,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  to_table(M,HeadList,TabDir,[H1:_]),
  % write('headlist: '), writeln(HeadList),
  % write('h1: '), writeln(H1),
  add_bdd_arg(H1,Env,BDD,M,Head1),%***test single_var
  % write('head1: '), writeln(Head1),
  % write('vc: '), writeln(VC),
  (M:local_pita_setting(single_var,true)->
    VC1 = []
  ;
    VC1 = VC
  ),
  Clauses0=[(Head1:-(get_var_n(M,Env,R,VC1,Probs,V),equalityc(Env,V,0,BDD)))],
  append(TabDir,Clauses0,Clauses).

pita_expansion((:- set_sw(A,B)), []) :-!,
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  set_sw(M:A,B).


pita_expansion(Head, Clauses) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
  M:local_pita_setting(depth_bound,true),
% definite fact with db
  (Head \= ((pita_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  to_table(M,[Head:_],TabDir,[Head1:_]),
  add_bdd_arg_db(Head1,Env,One,_DB,M,Head2),
  append(TabDir,[(Head2:-onec(Env,One))],Clauses).

pita_expansion(Head, Clauses) :-
  prolog_load_context(module, M),pita_input_mod(M),M:pita_on,
% definite fact without db
  (Head \= ((pita_expansion(_,_) ):- _ )),
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

transform(H,H1):-
  H=..[prob|Args],
  H1=..[prob_meta|Args].

builtin(average(_L,_Av)) :- !.
builtin(G) :-
  swi_builtin(G).


:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(pita:s(_,_), []).
sandbox:safe_meta(pita:prob(_,_), []).
sandbox:safe_meta(pita:prob(_,_,_), []).
sandbox:safe_meta(pita:prob(_,_,_,_), []).
sandbox:safe_meta(pita:prob_meta(_,_), []).
sandbox:safe_meta(pita:prob_meta(_,_,_), []).
sandbox:safe_meta(pita:abd_prob(_,_,_), []).
sandbox:safe_meta(pita:bdd_dot_file(_,_,_), []).
sandbox:safe_meta(pita:bdd_dot_string(_,_,_), []).
sandbox:safe_meta(pita:abd_bdd_dot_string(_,_,_,_), []).
sandbox:safe_meta(pita:abd_bdd_dot_string(_,_,_,_,_,_), []).
sandbox:safe_meta(pita:map(_,_,_), []).
sandbox:safe_meta(pita:map_bdd_dot_string(_,_,_,_,_,_), []).
sandbox:safe_meta(pita:msw(_,_,_,_), []).
sandbox:safe_meta(pita:msw(_,_,_,_,_), []).
sandbox:safe_meta(pita:set_pita(_,_),[]).
sandbox:safe_meta(pita:setting_pita(_,_),[]).
% sandbox:safe_meta(pita:dt_solve_complete(_,_),[]).
sandbox:safe_meta(pita:dt_solve(_,_),[]).




:- license(artisticv2).

:- thread_local pita_file/1.

user:term_expansion(:-pita, Clauses) :-!,
  prolog_load_context(source, Source),
  asserta(pita_file(Source)),
  prolog_load_context(module, M),
  retractall(M:local_pita_setting(_,_)),
  findall(local_pita_setting(P,V),default_setting_pita(P,V),L),
  assert_all(L,M,_),
  assert(pita_input_mod(M)),
  retractall(M:rule_n(_)),
  retractall(M:goal_n(_)),
  assert(M:rule_n(0)),
  assert(M:goal_n(0)),
  M:(dynamic v/3, av/3, query_rule/4, rule_by_num/4, dec/3,
    zero_clauses/1, pita_on/0, tabled/1, '$cons'/2),
  retractall(M:query_rule(_,_,_,_)),
  style_check(-discontiguous),
  process_body([\+ '$cons'],BDD,BDDAnd,[],_Vars,BodyList2,Env,M),
  append([onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  to_table(M,['$constraints':_],TabDir,[Head1:_]),
  to_table(M,['$cons':_],TabDirCons,_),
  add_bdd_arg(Head1,Env,BDDAnd,M,Head2),
  append([TabDir,TabDirCons,[(Head2 :- Body2)]],Clauses).

user:term_expansion(end_of_file, C) :-
  pita_file(Source),
  prolog_load_context(source, Source),
  retractall(pita_file(Source)),
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  retractall(pita_input_mod(M)),
  findall(LZ,M:zero_clauses(LZ),L),
  retractall(M:zero_clauses(_)),
  retractall(M:tabled(_)),
  append(L,[(:- style_check(+discontiguous)),end_of_file],C).

user:term_expansion(In, Out) :-
   \+ current_prolog_flag(xref, true),
   pita_file(Source),
   prolog_load_context(source, Source),
   pita_expansion(In, Out).


