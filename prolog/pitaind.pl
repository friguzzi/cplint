

:- module(pitaind,[s/2, prob_ind/2, prob_bar/2, prob_ind/3, prob_bar/3,
  set_pitaind/2,setting_pitaind/2,
  onec/1,zeroc/1,andc/3,notc/2,andcnf/3,
  orc_ind/3,orc_exc/3,
  get_var_n/5,or_list_pitaind/3,
  or_list_ind/2,or_list_exc/2,
  equalityc/3,
  op(600,xfy,'::'),
  op(1150,fx,action)
    ]).

/** <module> pitaind

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

:-meta_predicate s(:,-).
:-meta_predicate prob_ind(:,-).
:-meta_predicate prob_bar(:,-).
:-meta_predicate prob_ind(:,:,-).
:-meta_predicate prob_bar(:,:,-).
:-meta_predicate msw_ind(:,-,-).
:-meta_predicate msw_ind(:,-,-,-).
:-meta_predicate get_p(:,-).
:-meta_predicate get_cond_p(:,:,-).
:-meta_predicate get_node(:,-).
:-meta_predicate get_cond_node(:,:,-,-).
:-meta_predicate set_pitaind(:,+).
:-meta_predicate setting_pitaind(:,-).
:-meta_predicate set_sw(:,+).

:-use_module(library(lists)).
:-use_module(library(rbtrees)).
:-use_module(library(apply)).
:-use_module(library(assoc)).

:- style_check(-discontiguous).

:- thread_local rule_n/1,goal_n/1,pitaind_input_mod/1,local_pitaind_setting/2.

%  remove/3.












default_setting_pitaind(epsilon_parsing, 1e-5).
/* on, off */

default_setting_pitaind(bagof,false).
/* values: false, intermediate, all, extra */

default_setting_pitaind(compiling,off).

:-set_prolog_flag(unknown,warning).

default_setting_pitaind(depth_bound,false).  %if true, it limits the derivation of the example to the value of 'depth'
default_setting_pitaind(depth,5).
default_setting_pitaind(single_var,false). %false:1 variable for every grounding of a rule; true: 1 variable for rule (even if a rule has more groundings),simpler.
default_setting_pitaind(or,ind).
/* values: ind, exc
how or is computed: by assuming independence or exclusion
*/




/**
 * orc_ind(++A:float,++B:float,--AorB:float) is det
 *
 * Returns A + B - A*B in AorB (or in case of independence)
 */
orc_ind(A,B,C):-
        C is 1-(1-A)*(1-B).

/**
 * orc_exc(++A:float,++B:float,--AorB:float) is det
 *
 * Returns A + B in AorB (or in case of exclusion)
 */
orc_exc(A,B,C):-
        C is A+B.

/**
 * onec(--One:float) is det
 *
 * Returns 1.0
 */
onec(1.0).

/**
 * zeroc(--Zero:float) is det
 *
 * Returns 0.0
 */
zeroc(0.0).


/**
 * andc(++A:float,++B:float,--AandB:float) is det
 *
 * Returns A*B in AandB (and in case of idependence). Fails if either A or B is 0.0
 */
andc(A,B,C):-
  ((A=0.0;B=0.0)->
    %C=and(A,B)
    fail
  ;
    (A=1.0->
      C=B
    ;
      (B=1.0->
        C=A
      ;
        C is A*B
      )
    )
  ).

/**
 * andcnf(++A:float,++B:float,--AandB:float) is det
 *
 * Returns A*B in AandB (and in case of idependence).
 */
andcnf(A,B,C):-
  (A=1.0->
    C=B
  ;
    (B=1.0->
      C=A
    ;
      C is A*B
    )
  ).


/**
 * notc(++A:float,--NotA:float) is det
 *
 * Returns 1-A in NotA (negation)
 */
notc(A,B):-
  (A=0.0->
    B=1.0
  ;
    (A=1.0->
      B=0.0
    ;
      B is 1.0-A
    )
  ).

/**
 * equalityc(++Variable:int,++Value:int,--P:float) is det
 *
 * Returns in P the probability that Variable takes Value.
 */
equalityc(Probs,N,P):-
  nth0(N,Probs,P).




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
  process_body(GoalL,BDD,BDDAnd,[],_Vars,BodyList2,M),
  append([onec(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Goal1,BDDAnd,M,Head1),
  M:(asserta((Head1 :- Body2),Ref)),
  findall((Goal,P),get_p(M:Goal1,P),L),
  erase(Ref),
  member((Goal,P),L).





/**
 * prob_ind(:Query:atom,-Probability:float) is nondet
 *
 * The predicate computes the probability of Query
 * If Query is not ground, it returns in backtracking all ground
 * instantiations of
 * Query together with their probabilities
 */
prob_ind(M:Goal,P):-
  s(M:Goal,P).

/**
 * prob_bar(:Query:atom,-Probability:dict) is nondet
 *
 * The predicate computes the probability of Query
 * and returns it as a dict for rendering with c3 as a bar chart with
 * a bar for the probability of Query true and a bar for the probability of
 * Query false.
 * If Query is not ground, it returns in backtracking all ground
 * instantiations of
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

/**
 * prob_ind(:Query:atom,:Evidence:atom,-Probability:float) is nondet
 *
 * The predicate computes the probability of Query given
 * Evidence
 * If Query/Evidence are not ground, it returns in backtracking all
 * ground instantiations of
 * Query/Evidence together with their probabilities
 */
prob_ind(M:Goal,M:Evidence,P):-
  get_next_goal_number(M,GN),
  atomic_concat('$ev',GN,NewEv),
  deal_with_ev(Evidence,M,NewEv,EvNoAct,UpdatedClausesRefs,ClausesToReAdd),
  term_variables(Goal,VG),
  atomic_concat('$goal',GN,NewGoal),
  Goal1=..[NewGoal|VG],
  list2and(GoalL,Goal),
  process_body(GoalL,BDD,BDDAnd,[],_Vars,BodyList2,M),
  append([onec(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Goal1,BDDAnd,M,Head1),
  M:(asserta((Head1 :- Body2),Ref)),
  (EvNoAct=true->
    findall((Goal,P),get_p(M:Goal1,P),L)
  ;
    findall((Goal,P),get_cond_p(M:Goal1,M:EvNoAct,P),L)
  ),
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
    process_body(EvNoActL,BDD,BDDAnd,[],_Vars,BodyList2,M),
    append([onec(BDD)],BodyList2,BodyList3),
    list2and(BodyList3,Body2),
    add_bdd_arg(NewEv,BDDAnd,M,Head1),
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
  append(Args,[BDD],Args1),
  A1=..[P|Args1],
  M:assertz((A1:-zeroc(BDD)),Ref).

assert_actions(M,do(A),Ref):-
  A=..[P|Args],
  append(Args,[BDD],Args1),
  atomic_concat(P,' tabled',P1),
  A1=..[P1|Args1],
  M:assertz((A1:-onec(BDD)),Ref).

update_clauses(M,P/0- _,[RefZ],[(H:-zeroc(BDD))|LCA]):-!,
  functor(G1,P,2),
  findall(Ref,M:clause(G1,_B,Ref),UC),
  findall((G1:-B),M:clause(G1,B),LCA),
  H=..[P,BDD],
  maplist(erase,UC),
  M:assertz((H:-zeroc(BDD)),RefZ).

update_clauses(M,P/A-Constants,UC,CA):-
  functor(G,P,A),
  A1 is A+2,
  functor(G1,P,A1),
  G=..[_|Args],
  findall((G1,B,Ref),M:clause(G1,B,Ref),LC),
  maplist(get_const(Args),Constants,ConstraintsL),
  list2and(ConstraintsL,Constraints),
  maplist(add_cons(G1,Constraints,M),LC,UC,CA).

add_cons(_G,_C,M,(H,zeroc(Zero),Ref),Ref1,(H:-zeroc(Zero))):-!,
  erase(Ref),
  M:assertz((H:-zeroc(Zero)),Ref1).

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

/**
 * prob_bar(:Query:atom,:Evidence:atom,-Probability:dict) is nondet
 *
 * The predicate computes the probability of the Query given Evidence
 * and returns it as a dict for rendering with c3 as a bar chart with
 * a bar for the probability of Query true and a bar for the probability of
 * Query false given Evidence.
 * If Query /Evidence are not ground, it returns in backtracking all
 * ground instantiations of
 * Query/Evidence together with their probabilities
 */
prob_bar(M:Goal,M:Evidence,Chart):-
  prob_ind(M:Goal,M:Evidence,P),
  PF is 1.0-P,
  Chart = c3{data:_{x:elem, rows:[elem-prob,'T'-P,'F' -PF], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,max:1.0,padding:_{bottom:0.0,top:0.0},
             tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}},
	           size:_{height: 100},
	          legend:_{show: false}}.


get_p(M:Goal,P):-
  get_node(M:Goal,P).

get_cond_p(M:Goal,M:Evidence,P):-
  get_cond_node(M:Goal,M:Evidence,PGE,PE),
  P is PGE/PE.


get_node(M:Goal,B):-
  M:local_pitaind_setting(depth_bound,true),!,
  M:local_pitaind_setting(depth,DB),
  retractall(M:v(_,_,_)),
  abolish_all_tables,
  add_bdd_arg_db(Goal,BDD,DB,M,Goal1),%DB=depth bound
  (bagof(BDD,M:Goal1,L)*->
    or_list_pitaind(L,M,B)
  ;
    zeroc(B)
  ).

get_node(M:Goal,B):- %with DB=false
  retractall(M:v(_,_,_)),
  abolish_all_tables,
  add_bdd_arg(Goal,BDD,M,Goal1),
  (bagof(BDD,M:Goal1,L)*->
    or_list_pitaind(L,M,B)
  ;
    zeroc(B)
  ).

get_cond_node(M:Goal,M:Ev,BGE,BE):-
  M:local_pitaind_setting(depth_bound,true),!,
  M:local_pitaind_setting(depth,DB),
  retractall(M:v(_,_,_)),
  abolish_all_tables,
  add_bdd_arg_db(Goal,BDD,DB,M,Goal1),%DB=depth bound
  (bagof(BDD,M:Goal1,L)*->
    or_list_pitaind(L,M,BG)
  ;
    zeroc(BG)
  ),
  add_bdd_arg_db(Ev,BDDE,DB,M,Ev1),%DB=depth bound
  (bagof(BDDE,M:Ev1,LE)*->
    or_list_pitaind(LE,M,BE)
  ;
    zeroc(BE)
  ),
  andcnf(BG,BE,BGE).



get_cond_node(M:Goal,M:Ev,BGE,BE):- %with DB=false
  retractall(M:v(_,_,_)),
  abolish_all_tables,
  add_bdd_arg(Goal,BDD,M,Goal1),
  (bagof(BDD,M:Goal1,L)*->
    or_list_pitaind(L,M,BG)
  ;
    zeroc(BG)
  ),
  add_bdd_arg(Ev,BDDE,M,Ev1),
  (bagof(BDDE,M:Ev1,LE)*->
    or_list_pitaind(LE,M,BE)
  ;
    zeroc(BE)
  ),
  andcnf(BG,BE,BGE).


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
 * get_var_n(++M:atomic,++Rule:int,++Substitution:term,++Probabilities:list,-Variable:int) is det
 *
 * Returns the index Variable of the random variable associated to rule with
 * index Rule, grouding substitution Substitution and head distribution
 * Probabilities in environment Environment.
 */
get_var_n(_M,_R,_S,Probs0,Probs):-
  (ground(Probs0)->
    maplist(is,Probs,Probs0)
  ;
    throw(error('Non ground probailities not instantiated by the body'))
  ).



combine(V,P,V:P).

add_bdd_arg(M:A,BDD,M:A1):-
  A=..[P|Args],
  append(Args,[BDD],Args1),
  A1=..[P|Args1].


add_bdd_arg_db(M:A,BDD,DB,M:A1):-
  A=..[P|Args],
  append(Args,[DB,BDD],Args1),
  A1=..[P|Args1].


add_bdd_arg(A,BDD,_Module,A1):-
  A=..[P|Args],
  append(Args,[BDD],Args1),
  A1=..[P|Args1].


add_bdd_arg_db(A,BDD,DB,_Module,A1):-
  A=..[P|Args],
  append(Args,[DB,BDD],Args1),
  A1=..[P|Args1].

add_mod_arg(A,_Module,A1):-
  A=..[P|Args],
  A1=..[P|Args].


generate_rules_fact([],_VC,_R,_Probs,_N,[],_Module).

generate_rules_fact([Head:_P1,'':_P2],VC,R,Probs,N,[Clause],Module):-!,
  add_bdd_arg(Head,BDD,Module,Head1),
  Clause=(Head1:-(get_var_n(Module,R,VC,Probs,V),equalityc(V,N,BDD))).

generate_rules_fact([Head:_P|T],VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg(Head,BDD,Module,Head1),
  Clause=(Head1:-(get_var_n(Module,R,VC,Probs,V),equalityc(V,N,BDD))),
  N1 is N+1,
  generate_rules_fact(T,VC,R,Probs,N1,Clauses,Module).


generate_rules_fact_vars([],__R,_Probs,_N,[],_Module).

generate_rules_fact_vars([Head:_P1,'':_P2],R,Probs,N,[Clause],Module):-!,
  extract_vars_list([Head],[],VC),
  add_bdd_arg(Head,BDD,Module,Head1),
  Clause=(Head1:-(get_var_n(Module,R,VC,Probs,V),equalityc(V,N,BDD))).

generate_rules_fact_vars([Head:_P|T],R,Probs,N,[Clause|Clauses],Module):-
  extract_vars_list([Head],[],VC),
  add_bdd_arg(Head,BDD,Module,Head1),
  Clause=(Head1:-(get_var_n(Module,R,VC,Probs,V),equalityc(V,N,BDD))),
  N1 is N+1,
  generate_rules_fact_vars(T,R,Probs,N1,Clauses,Module).


generate_rules_fact_db([],__VC,_R,_Probs,_N,[],_Module).

generate_rules_fact_db([Head:_P1,'':_P2],VC,R,Probs,N,[Clause],Module):-!,
  add_bdd_arg_db(Head,BDD,_DB,Module,Head1),
  Clause=(Head1:-(get_var_n(Module,R,VC,Probs,V),equalityc(V,N,BDD))).

generate_rules_fact_db([Head:_P|T],VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg_db(Head,BDD,_DB,Module,Head1),
  Clause=(Head1:-(get_var_n(Module,R,VC,Probs,V),equalityc(V,N,BDD))),
  N1 is N+1,
  generate_rules_fact_db(T,VC,R,Probs,N1,Clauses,Module).


generate_clause(Head,Body,VC,R,Probs,BDDAnd,N,Clause,Module):-
  add_bdd_arg(Head,BDD,Module,Head1),
  Clause=(Head1:-(Body,get_var_n(Module,R,VC,Probs,V),equalityc(V,N,B),andc(BDDAnd,B,BDD))).


generate_clause_db(Head,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module):-
  add_bdd_arg_db(Head,BDD,DBH,Module,Head1),
  Clause=(Head1:-(DBH>=1,DB is DBH-1,Body,get_var_n(Module,R,VC,Probs,V),equalityc(V,N,B),andc(BDDAnd,B,BDD))).


generate_rules([],__Body,_VC,_R,_Probs,_BDDAnd,_N,[],_Module).

generate_rules([Head:_P1,'':_P2],Body,VC,R,Probs,BDDAnd,N,[Clause],Module):-!,
  generate_clause(Head,Body,VC,R,Probs,BDDAnd,N,Clause,Module).

generate_rules([Head:_P|T],Body,VC,R,Probs,BDDAnd,N,[Clause|Clauses],Module):-
  generate_clause(Head,Body,VC,R,Probs,BDDAnd,N,Clause,Module),
  N1 is N+1,
  generate_rules(T,Body,VC,R,Probs,BDDAnd,N1,Clauses,Module).


generate_rules_db([],__Body,_VC,_R,_Probs,_DB,_BDDAnd,_N,[],_Module):-!.

generate_rules_db([Head:_P1,'':_P2],Body,VC,R,Probs,DB,BDDAnd,N,[Clause],Module):-!,
  generate_clause_db(Head,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module).

generate_rules_db([Head:_P|T],Body,VC,R,Probs,DB,BDDAnd,N,[Clause|Clauses],Module):-
  generate_clause_db(Head,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module),!,%agg.cut
  N1 is N+1,
  generate_rules_db(T,Body,VC,R,Probs,DB,BDDAnd,N1,Clauses,Module).


process_body([],BDD,BDD,Vars,Vars,[],__Module).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body([\+ db(H)|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Module):-
  !,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body([\+ H|T],BDD,BDD1,Vars,[BDDH,BDDN,L,BDDL,BDD2|Vars1],
[(bagof(BDDH,H1,L)*->or_list_ind(L,BDDL),notc(BDDL,BDDN);onec(BDDN)),
  andc(BDD,BDDN,BDD2)|Rest],Module):-
  Module:local_pitaind_setting(or,ind),!,
  add_bdd_arg(H,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Module).

process_body([\+ H|T],BDD,BDD1,Vars,[BDDH,BDDN,L,BDDL,BDD2|Vars1],
  [(bagof(BDDH,H1,L)*->or_list_exc(L,BDDL),notc(BDDL,BDDN);onec(BDDN)),
    andc(BDD,BDDN,BDD2)|Rest],Module):-!,
    add_bdd_arg(H,BDDH,Module,H1),
    process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Module).
  
process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body([db(H)|T],BDD,BDD1,Vars,Vars1,[H|Rest],Module):-
  !,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body([H|T],BDD,BDD1,Vars,[BDDH,BDD2|Vars1],
[bagof(BDDH,H1,L),or_list_ind(L,BDDL),andc(BDD,BDDL,BDD2)|Rest],Module):-
  Module:local_pitaind_setting(or,ind),!,
  add_bdd_arg(H,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Module).

process_body([H|T],BDD,BDD1,Vars,[BDDH,BDD2|Vars1],
  [bagof(BDDH,H1,L),or_list_exc(L,BDDL),andc(BDD,BDDL,BDD2)|Rest],Module):-
    add_bdd_arg(H,BDDH,Module,H1),
    process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Module).
  
process_body_db([],BDD,BDD,_DB,Vars,Vars,[],__Module):-!.

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([\+ db(H)|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Module):-
  !,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,[BDDH,BDDN,L,BDDL,BDD2|Vars1],
[(bagof(BDDH,H1,L)*->or_list_ind(L,BDDL),notc(BDDL,BDDN);onec(BDDN)),
  andc(BDD,BDDN,BDD2)|Rest],Module):-
  Module:local_pitaind_setting(or,ind),!,
  add_bdd_arg_db(H,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,[BDDH,BDDN,L,BDDL,BDD2|Vars1],
  [(bagof(BDDH,H1,L)*->or_list_exc(L,BDDL),notc(BDDL,BDDN);onec(BDDN)),
    andc(BDD,BDDN,BDD2)|Rest],Module):-!,
    add_bdd_arg_db(H,BDDH,DB,Module,H1),
    process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).
  
process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([db(H)|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Module):-
  !,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,[BDDH,BDD2|Vars1],
[bagof(BDDH,H1,L),or_list_ind(L,BDDL),andc(BDD,BDDL,BDD2)|Rest],Module):-
  Module:local_pitaind_setting(or,ind),!,
  add_bdd_arg_db(H,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,[BDDH,BDD2|Vars1],
  [bagof(BDDH,H1,L),or_list_exc(L,BDDL),andc(BDD,BDDL,BDD2)|Rest],Module):-!, %agg. cut
  add_bdd_arg_db(H,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).
  
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
  prolog_load_context(module, M),pitaind_input_mod(M),
  M:local_pitaind_setting(epsilon_parsing, Eps),
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
 * or_list_pitaind(++ListOfProbs:list,++Module:module,--P:float) is det
 *
 * Returns in P the probability of the disjunction of the random variables
 * whose probabilities are in ListOfProbs. Module is used to check the 
 * setting for disjunction, either independent or exclusive.
 */
or_list_pitaind(L,M,O):-
  M:local_pitaind_setting(or,ind),!,
  or_list_ind(L,O).

or_list_pitaind(L,_M,O):-
  or_list_exc(L,O).

/**
 * or_list_ind(++ListOfProbs:list,--P:float) is det
 *
 * Returns in P the probability of the disjunction of the random variables
 * whose probabilities are in ListOfProbs assuming independence.
 */
or_list_ind([H],H):-!.

or_list_ind([H|T],B):-
  or_list1_ind(T,H,B).


or_list1_ind([],B,B).

or_list1_ind([H|T],B0,B1):-
  orc_ind(B0,H,B2),
  or_list1_ind(T,B2,B1).

/**
 * or_list_exc(++ListOfProbs:list,--P:float) is det
 *
 * Returns in P the probability of the disjunction of the random variables
 * whose probabilities are in ListOfProbs assuming exclusiveness.
 */
or_list_exc([H],H):-!.

or_list_exc([H|T],B):-
  or_list1_exc(T,H,B).


or_list1_exc([],B,B).

or_list1_exc([H|T],B0,B1):-
  orc_exc(B0,H,B2),
  or_list1_exc(T,B2,B1).

/**
 * set_pitaind(:Parameter:atom,+Value:term) is det
 *
 * The predicate sets the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 *
 */
set_pitaind(M:Parameter,Value):-
  retract(M:local_pitaind_setting(Parameter,_)),
  assert(M:local_pitaind_setting(Parameter,Value)).

/**
 * setting_pitaind(:Parameter:atom,?Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_pitaind(M:P,V):-
  M:local_pitaind_setting(P,V).

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


delete_equal([],_,[]).

delete_equal([H|T],E,T):-
  H == E,!.

delete_equal([H|T],E,[H|T1]):-
  delete_equal(T,E,T1).

set_sw(M:A,B):-
  get_next_rule_number(M,R),
  assert(M:sw(R,A,B)).

act(M,A/B):-
  B1 is B + 2,
  M:(dynamic A/B1).

tab(A/B,A/B1):-
  B1 is B + 2.

user:term_expansion(end_of_file, end_of_file) :-
  prolog_load_context(module, M),
  pitaind_input_mod(M),!,
  retractall(pitaind_input_mod(M)),
  style_check(+discontiguous).

user:term_expansion((:- action Conj), []) :-!,
  prolog_load_context(module, M),
  list2and(L,Conj),
  maplist(act(M),L).

user:term_expansion((:- pitaind), []) :-!,
  prolog_load_context(module, M),
  retractall(M:local_pitaind_setting(_,_)),
  findall(local_pitaind_setting(P,V),default_setting_pitaind(P,V),L),
  assert_all(L,M,_),
  assert(pitaind_input_mod(M)),
  retractall(M:rule_n(_)),
  retractall(M:goal_n(_)),
  assert(M:rule_n(0)),
  assert(M:goal_n(0)),
  M:(dynamic v/3),
  style_check(-discontiguous).

user:term_expansion((:- table(Conj)), [:- table(Conj1)]) :-!,
  prolog_load_context(module, M),
  pitaind_input_mod(M),!,
  list2and(L,Conj),
  maplist(tab,L,L1),
  list2and(L1,Conj1).

user:term_expansion((:- begin_plp), []) :-
  prolog_load_context(module, M),
  pitaind_input_mod(M),!,
  assert(M:pitaind_on).

user:term_expansion((:- end_plp), []) :-
  prolog_load_context(module, M),
  pitaind_input_mod(M),!,
  retractall(M:pitaind_on).

user:term_expansion((:- begin_lpad), []) :-
  prolog_load_context(module, M),
  pitaind_input_mod(M),!,
  assert(M:pitaind_on).

user:term_expansion((:- end_lpad), []) :-
  prolog_load_context(module, M),
  pitaind_input_mod(M),!,
  retractall(M:pitaind_on).

user:term_expansion(values(A,B), values(A,B)) :-
  prolog_load_context(module, M),
  pitaind_input_mod(M),M:pitaind_on,!.

user:term_expansion((Head :- Body), Clauses):-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  M:local_pitaind_setting(depth_bound,true),
% disjunctive clause with more than one head atom e depth_bound
  Head = (_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,M),
  append([onec(BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  (M:local_pitaind_setting(single_var,true)->
    generate_rules_db(HeadList,Body1,[],R,Probs,DB,BDDAnd,0,Clauses,M)
  ;
    generate_rules_db(HeadList,Body1,VC,R,Probs,DB,BDDAnd,0,Clauses,M)
   ).

user:term_expansion((Head :- Body), Clauses):-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
% disjunctive clause with more than one head atom senza depth_bound
  Head = (_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,M),
  append([onec(BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  (M:local_pitaind_setting(single_var,true)->
    generate_rules(HeadList,Body1,[],R,Probs,BDDAnd,0,Clauses,M)
  ;
    generate_rules(HeadList,Body1,VC,R,Probs,BDDAnd,0,Clauses,M)
  ).

user:term_expansion((Head :- Body), []) :-
% disjunctive clause with a single head atom con prob. 0 senza depth_bound --> la regola non e' caricata nella teoria e non e' conteggiata in NR
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  (Head = (_:P);Head=(P::_)),
  ground(P),
  P=:=0.0, !.

user:term_expansion((Head :- Body), Clauses) :-
% disjunctive clause with a single head atom e depth_bound
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  M:local_pitaind_setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,M),
  append([onec(BDD)],BodyList2,BodyList3),
  list2and([DBH>=1,DB is DBH -1|BodyList3],Body1),
  add_bdd_arg_db(H,BDDAnd,DBH,M,Head1),
  Clauses=(Head1 :- Body1).

user:term_expansion((Head :- Body), Clauses) :-
% disjunctive clause with a single head atom senza depth_bound con prob =1
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
   ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,M),
  append([onec(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,BDDAnd,M,Head1),
  Clauses=(Head1 :- Body1).

user:term_expansion((Head :- Body), Clauses) :-
% disjunctive clause with a single head atom e DB, con prob. diversa da 1
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  M:local_pitaind_setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  (Head = (H:_);Head=(_::H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,M),
  append([onec(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),%***test single_var
  (M:local_pitaind_setting(single_var,true)->
    generate_clause_db(H,Body2,[],R,Probs,DB,BDDAnd,0,Clauses,M)
  ;
    generate_clause_db(H,Body2,VC,R,Probs,DB,BDDAnd,0,Clauses,M)
  ).

user:term_expansion((Head :- Body), Clauses) :-
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  (Head = (H:_);Head = (_::H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,M),
  append([onec(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),%***test single_vars
  (M:local_pitaind_setting(single_var,true)->
    generate_clause(H,Body2,[],R,Probs,BDDAnd,0,Clauses,M)
  ;
    generate_clause(H,Body2,VC,R,Probs,BDDAnd,0,Clauses,M)
  ).

/*user:term_expansion((Head :- Body),Clauses) :-
% definite clause for db facts
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),
  Head=db(Head1),!,
  Clauses=(Head1 :- Body).
*/
user:term_expansion((Head :- Body),Clauses) :-
% definite clause with depth_bound
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  M:local_pitaind_setting(depth_bound,true),
   ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,M),
  append([onec(BDD)],BodyList2,BodyList3),
  list2and([DBH>=1,DB is DBH-1|BodyList3],Body1),
  add_bdd_arg_db(Head,BDDAnd,DBH,M,Head1),
  Clauses=(Head1 :- Body1).

user:term_expansion((Head :- Body),Clauses) :-
% definite clause senza DB
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,M),
  append([onec(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Head,BDDAnd,M,Head1),
  Clauses=(Head1 :- Body2).

user:term_expansion(Head,Clauses) :-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  M:local_pitaind_setting(depth_bound,true),
% disjunctive FACT with more than one head atom e db
  Head=(_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
extract_vars_list(HeadList,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  (M:local_pitaind_setting(single_var,true)->
    generate_rules_fact_db(HeadList,[],R,Probs,0,Clauses,M)
  ;
    generate_rules_fact_db(HeadList,VC,R,Probs,0,Clauses,M)
  ).

user:term_expansion(Head,Clauses) :-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
% disjunctive fact with more than one head atom senza db
  Head=(_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs), %**** test single_var
  (M:local_pitaind_setting(single_var,true)->
    generate_rules_fact(HeadList,[],R,Probs,0,Clauses,M)
  ;
    generate_rules_fact(HeadList,VC,R,Probs,0,Clauses,M)
  ).

user:term_expansion(Head,Clauses) :-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
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
  (M:local_pitaind_setting(single_var,true)->
    generate_rules_fact(HeadList,[],R,Probs,0,Clauses,M)
  ;
    generate_rules_fact_vars(HeadList,R,Probs,0,Clauses,M)
  ).


user:term_expansion(Head,Clauses) :-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
% disjunctive fact with guassia distr
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (_:P),
  nonvar(P),
  (Head=(H:discrete(Var,D));Head=(H:finite(Var,D))),!,
  maplist(gen_head_disc(H,Var),D,HeadList),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs), %**** test single_var
  (M:local_pitaind_setting(single_var,true)->
    generate_rules_fact(HeadList,[],R,Probs,0,Clauses,M)
  ;
    generate_rules_fact_vars(HeadList,R,Probs,0,Clauses,M)
  ).

user:term_expansion(Head,[]) :-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
% disjunctive fact with a single head atom con prob. 0
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head = (_:P); Head = (P::_)),
  ground(P),
  P=:=0.0, !.

user:term_expansion(Head,Clause) :-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  M:local_pitaind_setting(depth_bound,true),
% disjunctive fact with a single head atom con prob.1 e db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head = (H:P); Head = (P::H)),
  ground(P),
  P=:=1.0, !,
  list2and([onec(BDD)],Body1),
  add_bdd_arg_db(H,BDD,_DB,M,Head1),
  Clause=(Head1 :- Body1).

user:term_expansion(Head,Clause) :-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head = (H:P);Head =(P::H)),
  ground(P),
  P=:=1.0, !,
  list2and([onec(BDD)],Body1),
  add_bdd_arg(H,BDD,M,Head1),
  Clause=(Head1 :- Body1).

user:term_expansion(Head,Clause) :-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  M:local_pitaind_setting(depth_bound,true),
% disjunctive fact with a single head atom e prob. generiche, con db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head=(H:_);Head=(_::H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  add_bdd_arg_db(H,BDD,_DB,M,Head1),
  (M:local_pitaind_setting(single_var,true)->
    Clause=(Head1:-(get_var_n(M,R,[],Probs,V),equality(V,0,BDD)))
  ;
    Clause=(Head1:-(get_var_n(M,R,VC,Probs,V),equality(V,0,BDD)))
  ).

user:term_expansion(Head,Clause) :-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
% disjunctive fact with a single head atom e prob. generiche, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  (Head=(H:_);Head=(_::H)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  add_bdd_arg(H,BDD,M,Head1),%***test single_var
  (M:local_pitaind_setting(single_var,true)->
    Clause=(Head1:-(get_var_n(M,R,[],Probs,V),equality(V,0,BDD)))
  ;
    Clause=(Head1:-(get_var_n(M,R,VC,Probs,V),equality(V,0,BDD)))
  ).

user:term_expansion((:- set_pitaind(P,V)), []) :-!,
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  set_pitaind(P,V).

user:term_expansion((:- set_sw(A,B)), []) :-!,
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  set_sw(M:A,B).


user:term_expansion(Head, (Head1:-onec(One))) :-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
  M:local_pitaind_setting(depth_bound,true),
% definite fact with db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg_db(Head,One,_DB,M,Head1).

user:term_expansion(Head, (Head1:-onec(One))) :-
  prolog_load_context(module, M),pitaind_input_mod(M),M:pitaind_on,
% definite fact without db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),
  add_bdd_arg(Head,One,M,Head1).

/**
 * begin_lpad_pred is det
 *
 * Initializes LPAD loading.
 */
begin_lpad_pred:-
  assert(pitaind_input_mod(user)),
  assert(user:pitaind_on).

/**
 * end_lpad_pred is det
 *
 * Terminates the cplint inference module.
 */
end_lpad_pred:-
  retractall(pitaind_input_mod(_)),
  retractall(user:pitaind_on).

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


:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(pitaind:s(_,_), []).
sandbox:safe_meta(pitaind:prob_ind(_,_), []).
sandbox:safe_meta(pitaind:prob_bar(_,_), []).
sandbox:safe_meta(pitaind:prob_ind(_,_,_), []).
sandbox:safe_meta(pitaind:prob_bar(_,_,_), []).
sandbox:safe_meta(pitaind:bdd_dot_file(_,_,_), []).
sandbox:safe_meta(pitaind:bdd_dot_string(_,_,_), []).
sandbox:safe_meta(pitaind:set_pitaind(_,_),[]).
sandbox:safe_meta(pitaind:setting_pitaind(_,_),[]).
