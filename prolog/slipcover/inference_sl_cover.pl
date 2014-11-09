/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzi and Elena Bellodi

*/
:-use_module(library(lists)).
:-use_module(library(rbtrees)).
:-use_foreign_library(foreign(bddem),install).

:-dynamic p/2,rule_n/1,setting/2.


rule_n(0).

setting(epsilon_parsing, 1e-5).
setting(tabling, off).
/* on, off */

setting(bagof,false).
/* values: false, intermediate, all, extra */

setting(compiling,off).

:-set_prolog_flag(unknown,warning).

setting(depth_bound,true).  %if true, it limits the derivation of the example to the value of 'depth'
setting(depth,2).
setting(single_var,true). %false:1 variable for every grounding of a rule; true: 1 variable for rule (even if a rule has more groundings),simpler.

%:- yap_flag(single_var_warnings, on).


load(FileIn,C1,R):-
  open(FileIn,read,SI),
  read_clauses_dir(SI,C),
  close(SI),
  process_clauses(C,[],C1,[],R).


add_inter_cl(CL):-
  %findall(A,(input(A);output(A)),L),
  findall(A,(input(A)),L),
  gen_cl(L,CL).


gen_cl([],[]).

gen_cl([H/A|T],[C|T1]):-
  functor(F,H,A),
  add_mod_arg(F,Module,F1),
  add_bdd_arg(F,BDD,Module,F2),
  C=(F2:-(F1,one(BDD))),
  gen_cl(T,T1).


assert_all([],[]).

assert_all([H|T],[HRef|TRef]):-
  assertz(slipcover:H,HRef),
  assert_all(T,TRef).


retract_all([]):-!.

retract_all([H|T]):-
  erase(H),
  retract_all(T).


read_clauses_dir(S,[Cl|Out]):-
  read_term(S,Cl,[]),
  (Cl=end_of_file->
    Out=[]
  ;
    read_clauses_dir(S,Out)
  ).


process_clauses([],C,C,R,R):-!.

process_clauses([end_of_file],C,C,R,R):-!.

process_clauses([H|T],C0,C1,R0,R1):-
  (term_expansion_int(H,H1)->
    true
  ;
    H1=(H,[])
  ),
  (H1=([_|_],R)->
    H1=(List,R),
    append(C0,List,C2),
    append(R0,R,R2)
  ;
    (H1=([],_R)->
      C2=C0,
      R2=R0
    ;
      H1=(H2,R),
      append(C0,[H2],C2),
      append(R0,R,R2)
    )
  ),
  process_clauses(T,C2,C1,R2,R1).


get_next_rule_number(R):-
  retract(rule_n(R)),
  R1 is R+1,
  assert(rule_n(R1)).


get_node(\+ Goal,BDD):-
  setting(depth_bound,true),!,
  setting(depth,DB),
  retractall(v(_,_,_)),
  add_bdd_arg_db(Goal,BDD,DB,Goal1),
  (bagof(BDD,Goal1,L)->
    or_list(L,B)
  ;
    zero(B)
  ),
  bdd_not(B,BDD).

get_node(\+ Goal,BDD):-!,
  retractall(v(_,_,_)),
  add_bdd_arg(Goal,BDD,Goal1),
  (bagof(BDD,Goal1,L)->
    or_list(L,B)
  ;
    zero(B)
  ),
  bdd_not(B,BDD).

get_node(Goal,B):-
  setting(depth_bound,true),!,
  setting(depth,DB),
  retractall(v(_,_,_)),
  add_bdd_arg_db(Goal,BDD,DB,Goal1),%DB=depth bound
  (bagof(BDD,Goal1,L)->
    or_list(L,B)
  ;
    zero(B)
  ).

get_node(Goal,B):- %with DB=false
  retractall(v(_,_,_)),
  add_bdd_arg(Goal,BDD,Goal1),
  (bagof(BDD,Goal1,L)->
    or_list(L,B)
  ;  
    zero(B)
  ).


s(Goal,P,CPUTime1,0,WallTime1,0):-
  statistics(cputime,[_,_]),
  statistics(walltime,[_,_]),
    init,
    retractall(v(_,_,_)),
    abolish_all_tables,
    add_bdd_arg(Goal,BDD,Goal1),
    (bagof(BDD,Goal1,L)->
      or_list(L,B),
      ret_prob(B,P)
    ;
      P=0.0
    ),
    end,
  statistics(cputime,[_,CT1]),
  CPUTime1 is CT1/1000,
  statistics(walltime,[_,WT1]),
  WallTime1 is WT1/1000.  


get_var_n(R,S,Probs,V):-
  (v(R,S,V)->
    true
  ;
    length(Probs,L),
    add_var(L,Probs,R,V),    
    assert(v(R,S,V))
  ).


generate_rules_fact([],_VC,_R,_Probs,_N,[],_Module).

generate_rules_fact([Head:_P1,'':_P2],VC,R,Probs,N,[Clause],Module):-!,
  add_bdd_arg(Head,BDD,Module,Head1),
  Clause=(Head1:-(get_var_n(R,VC,Probs,V),equality(V,N,BDD))).

generate_rules_fact([Head:_P|T],VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg(Head,BDD,Module,Head1),
  Clause=(Head1:-(get_var_n(R,VC,Probs,V),equality(V,N,BDD))),
  N1 is N+1,
  generate_rules_fact(T,VC,R,Probs,N1,Clauses,Module).


generate_rules_fact_db([],_VC,_R,_Probs,_N,[],_Module).

generate_rules_fact_db([Head:_P1,'':_P2],VC,R,Probs,N,[Clause],Module):-!,
  add_bdd_arg_db(Head,BDD,_DB,Module,Head1),
  Clause=(Head1:-(get_var_n(R,VC,Probs,V),equality(V,N,BDD))).

generate_rules_fact_db([Head:_P|T],VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg_db(Head,BDD,_DB,Module,Head1),
  Clause=(Head1:-(get_var_n(R,VC,Probs,V),equality(V,N,BDD))),
  N1 is N+1,
  generate_rules_fact_db(T,VC,R,Probs,N1,Clauses,Module).


generate_clause(Head,Body,VC,R,Probs,BDDAnd,N,Clause,Module):-
  add_bdd_arg(Head,BDD,Module,Head1),
  Clause=(Head1:-(Body,get_var_n(R,VC,Probs,V),equality(V,N,B),and(BDDAnd,B,BDD))).


generate_clause_db(Head,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module):-
  add_bdd_arg_db(Head,BDD,DBH,Module,Head1),
  Clause=(Head1:-(DBH>=1,DB is DBH-1,Body,get_var_n(R,VC,Probs,V),equality(V,N,B),and(BDDAnd,B,BDD))).


generate_rules([],_Body,_VC,_R,_Probs,_BDDAnd,_N,[],_Module).

generate_rules([Head:_P1,'':_P2],Body,VC,R,Probs,BDDAnd,N,[Clause],Module):-!,
  generate_clause(Head,Body,VC,R,Probs,BDDAnd,N,Clause,Module).

generate_rules([Head:_P|T],Body,VC,R,Probs,BDDAnd,N,[Clause|Clauses],Module):-
  generate_clause(Head,Body,VC,R,Probs,BDDAnd,N,Clause,Module),
  N1 is N+1,
  generate_rules(T,Body,VC,R,Probs,BDDAnd,N1,Clauses,Module).


generate_rules_db([],_Body,_VC,_R,_Probs,_DB,_BDDAnd,_N,[],_Module):-!.

generate_rules_db([Head:_P1,'':_P2],Body,VC,R,Probs,DB,BDDAnd,N,[Clause],Module):-!,
  generate_clause_db(Head,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module).

generate_rules_db([Head:_P|T],Body,VC,R,Probs,DB,BDDAnd,N,[Clause|Clauses],Module):-
  generate_clause_db(Head,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module),!,%agg.cut
  N1 is N+1,
  generate_rules_db(T,Body,VC,R,Probs,DB,BDDAnd,N1,Clauses,Module).


process_body_database([],[],_Module).

process_body_database([H|T],[H1|T1],Module):-
  add_mod_arg(H,H1,Module),
  process_body_database(T,T1,Module).
  

process_body_db([],BDD,BDD,_DB,Vars,Vars,[],_Module):-!.

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).
  
process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Module):-
  db(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[
(((neg(H1);\+ H1),one(BDDN));(bagof(BDDH,H2,L)->or_list(L,BDDL),bdd_not(BDDL,BDDN);one(BDDN))),
  and(BDD,BDDN,BDD2)
  |Rest],Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg_db(H,BDDH,DB,Module,H2),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[
  neg(H1)|Rest],Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,[BDDH,BDDN,L,BDDL,BDD2|Vars1],
[(bagof(BDDH,H1,L)->or_list(L,BDDL),bdd_not(BDDL,BDDN);one(BDDN)),
  and(BDD,BDDN,BDD2)|Rest],Module):-!,
  add_bdd_arg_db(H,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Module):-
  db(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,
[((H1,one(BDDH));H2),and(BDD,BDDH,BDD2)|Rest],Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg_db(H,BDDH,DB,Module,H2),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,
[H1|Rest],Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,[BDDH,BDD2|Vars1],
[H1,and(BDD,BDDH,BDD2)|Rest],Module):-!, %agg. cut
  add_bdd_arg_db(H,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).


process_body_def_db([],BDD,BDD,_DB,Vars,Vars,[],_Module).

process_body_def_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Module):-
  builtin(H),!,
  process_body_def_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).
  
process_body_def_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Module):-
  db(H),!,
  process_body_def_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_def_db([\+H|T],BDD,BDD1,DB,Vars,Vars1,
[(((neg(H1);\+ H1),one(BDDN));(bagof(BDDH,H2,L)->or_list(L,BDDL),bdd_not(BDDL,BDDN);one(BDDN))),
  and(BDD,BDDN,BDD2)|Rest],
Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg_db(H,BDDH,DB,Module,H2),
  process_body_def_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_def_db([\+H|T],BDD,BDD1,DB,Vars,Vars1,
[neg(H1)|Rest],
Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body_def_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_def_db([\+H|T],BDD,BDD1,DB,Vars,[BDD,BDDH,L,BDDL,BDDN|Vars1],
  [(bagof(BDDH,H1,L)->or_list(L,BDDL),bdd_not(BDDL,BDDN);one(BDDN)),
  and(BDD,BDDN,BDD2)|Rest],Module):-!,
  add_bdd_arg_db(H,BDDH,DB,Module,H1),
  process_body_def_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_def_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Module):-
  builtin(H),!,
  process_body_def_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_def_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Module):-
  db(H),!,
  process_body_def_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_def_db([H|T],BDD,BDD1,DB,Vars,Vars1,[((H1,one(BDDH));H2),and(BDD,BDDH,BDD2)|Rest],Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg_db(H,BDDH,DB,Module,H2),
  process_body_def_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_def_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H1|Rest],Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body_def_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_def_db([H|T],BDD,BDD1,DB,Vars,[BDD,BDDH|Vars1],[H1,and(BDD,BDDH,BDD2)|Rest],Module):-!,
  add_bdd_arg_db(H,BDDH,DB,Module,H1),
  process_body_def_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).



  
process_body_def([],BDD,BDD,Vars,Vars,[],_Module).

process_body_def([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Module):-
  builtin(H),!,
  process_body_def(T,BDD,BDD1,Vars,Vars1,Rest,Module).
  
process_body_def([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Module):-
  db(H),!,
  process_body_def(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body_def([\+H|T],BDD,BDD1,Vars,Vars1,
[(((neg(H1);\+ H1),one(BDDN));(bagof(BDDH,H2,L)->or_list(L,BDDL),bdd_not(BDDL,BDDN);one(BDDN))),
  and(BDD,BDDN,BDD2)|Rest],
  Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg(H,BDDH,Module,H2),
  process_body_def(T,BDD2,BDD1,Vars,Vars1,Rest,Module).

process_body_def([\+H|T],BDD,BDD1,Vars,Vars1,
[neg(H1)|Rest],
Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body_def(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body_def([\+H|T],BDD,BDD1,Vars,[BDD,BDDH,L,BDDL,BDDN|Vars1],
  [(bagof(BDDH,H1,L)->or_list(L,BDDL),bdd_not(BDDL,BDDN);one(BDDN)),
  and(BDD,BDDN,BDD2)|Rest],Module):-!,
  add_bdd_arg(H,BDDH,Module,H1),
  process_body_def(T,BDD2,BDD1,Vars,Vars1,Rest,Module).

process_body_def([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Module):-
  builtin(H),!,
  process_body_def(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body_def([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Module):-
  db(H),!,
  process_body_def(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body_def([H|T],BDD,BDD1,Vars,Vars1,[((H1,one(BDDH));H2),and(BDD,BDDH,BDD2)|Rest],Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg(H,BDDH,Module,H2),
  process_body_def(T,BDD2,BDD1,Vars,Vars1,Rest,Module).

process_body_def([H|T],BDD,BDD1,Vars,Vars1,[H1|Rest],Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body_def(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body_def([H|T],BDD,BDD1,Vars,[BDD,BDDH|Vars1],[H1,and(BDD,BDDH,BDD2)|Rest],Module):-!,
  add_bdd_arg(H,BDDH,Module,H1),
  process_body_def(T,BDD2,BDD1,Vars,Vars1,Rest,Module).


process_body_cw([],BDD,BDD,Vars,Vars,[],_Module).

process_body_cw([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Module):-
  builtin(H),!,
  process_body_cw(T,BDD,BDD1,Vars,Vars1,Rest,Module).
  
process_body_cw([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Module):-
  db(H),!,
  process_body_cw(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body_cw([\+ H|T],BDD,BDD1,Vars,Vars1,[
  neg(H1)|Rest],Module):-
  add_mod_arg(H,Module,H1),
  process_body_cw(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body_cw([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Module):-
  builtin(H),!,
  process_body_cw(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body_cw([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Module):-
  db(H),!,
  process_body_cw(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body_cw([H|T],BDD,BDD1,Vars,Vars1,
[H1|Rest],Module):-
  add_mod_arg(H,Module,H1),
  process_body_cw(T,BDD,BDD1,Vars,Vars1,Rest,Module).




process_body([],BDD,BDD,Vars,Vars,[],_Module).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).
  
process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Module):-
  db(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[
(((neg(H1);\+ H1),one(BDDN));(bagof(BDDH,H2,L)->or_list(L,BDDL),bdd_not(BDDL,BDDN);one(BDDN))),
  and(BDD,BDDN,BDD2)
  |Rest],Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg(H,BDDH,Module,H2),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Module).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[
  neg(H1)|Rest],Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body([\+ H|T],BDD,BDD1,Vars,[BDDH,BDDN,L,BDDL,BDD2|Vars1],
[(bagof(BDDH,H1,L)->or_list(L,BDDL),bdd_not(BDDL,BDDN);one(BDDN)),
  and(BDD,BDDN,BDD2)|Rest],Module):-!,
  add_bdd_arg(H,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Module):-
  db(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,
[((H1,one(BDDH));H2),and(BDD,BDDH,BDD2)|Rest],Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg(H,BDDH,Module,H2),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,
[H1|Rest],Module):-
  given_cw(H),!,
  add_mod_arg(H,Module,H1),
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H1|Rest],Module):-
  add_mod_arg(H,Module,H1),
  db(H1),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body([H|T],BDD,BDD1,Vars,[BDDH,BDD2|Vars1],
[H1,and(BDD,BDDH,BDD2)|Rest],Module):-
  add_bdd_arg(H,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Module).


given(H):-
  functor(H,P,Ar),
  (input(P/Ar)).


given_cw(H):-
  functor(H,P,Ar),
  (input_cw(P/Ar)).


and_list([],B,B).

and_list([H|T],B0,B1):-
  and(B0,H,B2),
  and_list(T,B2,B1).


or_list([H],H):-!.

or_list([H|T],B):-
  or_list1(T,H,B).


or_list1([],B,B).

or_list1([H|T],B0,B1):-
  or(B0,H,B2),
  or_list1(T,B2,B1).


/* set(Par,Value) can be used to set the value of a parameter */
set(Parameter,Value):-
  retract(setting(Parameter,_)),
  assert(setting(Parameter,Value)).

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

/*
extract_vars(Variable, Var0, Var1) :- 
  var(Variable), !, 
  (member_eq(Variable, Var0) ->
    Var1 = Var0
  ;
    Var1=[Variable| Var0]
  ).

extract_vars(Term, Var0, Var1) :- 
  Term=..[_F|Args], 
  extract_vars_list(Args, Var0, Var1).



extract_vars_list([], Var, Var).

extract_vars_list([Term|Tail], Var0, Var1) :- 
  extract_vars(Term, Var0, Var), 
  extract_vars_list(Tail, Var, Var1).
*/

difference([],_,[]).

difference([H|T],L2,L3):-
  member_eq(H,L2),!,
  difference(T,L2,L3).

difference([H|T],L2,[H|L3]):-
  difference(T,L2,L3).


member_eq(E,[H|_T]):-
  E==H,!.

member_eq(E,[_H|T]):-
  member_eq(E,T).


add_bdd_arg(A,BDD,A1):-
  A=..[P|Args],
  append(Args,[BDD],Args1),
  A1=..[P|Args1],
  (setting(tabling,on)->
    table_pred(A)
  ;
    true
  ).


add_bdd_arg_db(A,BDD,DB,A1):-
  A=..[P|Args],
  append(Args,[DB,BDD],Args1),
  A1=..[P|Args1],
  (setting(tabling,on)->
    table_pred(A)
  ;
    true
  ).


add_bdd_arg(A,BDD,Module,A1):-
  A=..[P|Args],
  append(Args,[BDD],Args1),
  A1=..[P,Module|Args1],
  (setting(tabling,on)->
    table_pred(A)
  ;
    true
  ).


add_bdd_arg_db(A,BDD,DB,Module,A1):-
  A=..[P|Args],
  append(Args,[DB,BDD],Args1),
  A1=..[P,Module|Args1],
  (setting(tabling,on)->
    table_pred(A)
  ;
    true
  ).  


add_mod_arg(A,Module,A1):-
  A=..[P|Args],
  A1=..[P,Module|Args].


table_pred(A):-  
  functor(A,P,Arity),
  Arity1 is Arity +1,
  (is_tabled((P/Arity1))->
    true
  ;
    call(table(P/Arity1))
  ).


process_head(HeadList, GroundHeadList) :- 
  ground_prob(HeadList), !,
  process_head_ground(HeadList, 0, GroundHeadList).
   
process_head(HeadList, HeadList).



/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([Head:ProbHead], Prob, [Head:ProbHead1|Null]) :-!,
  ProbHead1 is ProbHead,
  ProbLast is 1 - Prob - ProbHead1,
  setting(epsilon_parsing, Eps), 
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


get_probs([], []).

get_probs([_H:P|T], [P1|T1]) :- 
  P1 is P, 
  get_probs(T, T1).


generate_clauses_cw([],[],_N,C,C):-!.

generate_clauses_cw([H|T],[H1|T1],N,C0,C):-
  gen_clause_cw(H,N,N1,H1,CL),!,  %agg.cut
  append(C0,CL,C1),
  generate_clauses_cw(T,T1,N1,C1,C).

gen_clause_cw((H :- Body),N,N,(H :- Body),[(H :- Body)]):-!.
  
gen_clause_cw(rule(_R,HeadList,BodyList,Lit),N,N1,
  rule(N,HeadList,BodyList,Lit),Clauses):-!,
% disjunctive clause with more than one head atom senza depth_bound
  process_body_cw(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Module),
  append([one(BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  (setting(single_var,true)->
    generate_rules(HeadList,Body1,[],N,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_rules(HeadList,Body1,VC,N,Probs,BDDAnd,0,Clauses,Module)
  ),
  N1 is N+1.

gen_clause_cw(def_rule(H,BodyList,Lit),N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
% disjunctive clause with a single head atom senza depth_bound con prob =1
  process_body_cw(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,BDDAnd,Module,Head1),
  Clauses=[(Head1 :- Body1)].


generate_clauses([],[],_N,C,C):-!.

generate_clauses([H|T],[H1|T1],N,C0,C):-
  gen_clause(H,N,N1,H1,CL),!,  %agg.cut
  append(C0,CL,C1),
  generate_clauses(T,T1,N1,C1,C).


gen_clause((H :- Body),N,N,(H :- Body),[(H :- Body)]):-!.
  
gen_clause(rule(_R,HeadList,BodyList,Lit),N,N1,
  rule(N,HeadList,BodyList,Lit),Clauses):-
  setting(depth_bound,true),!,
% disjunctive clause with more than one head atom e depth_bound
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,Module),
  append([one(BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  (setting(single_var,true)->
    generate_rules_db(HeadList,Body1,[],N,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_rules_db(HeadList,Body1,VC,N,Probs,DB,BDDAnd,0,Clauses,Module)
   ),
  N1 is N+1.
  
gen_clause(rule(_R,HeadList,BodyList,Lit),N,N1,
  rule(N,HeadList,BodyList,Lit),Clauses):-!,
% disjunctive clause with more than one head atom senza depth_bound
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Module),
  append([one(BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  (setting(single_var,true)->
    generate_rules(HeadList,Body1,[],N,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_rules(HeadList,Body1,VC,N,Probs,BDDAnd,0,Clauses,Module)
  ),
  N1 is N+1.

gen_clause(def_rule(H,BodyList,Lit),N,N,def_rule(H,BodyList,Lit),Clauses) :- 
% disjunctive clause with a single head atom e depth_bound
  setting(depth_bound,true),!,
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(H,BDDAnd,DB,Module,Head1),
  Clauses=[(Head1 :- Body1)].

gen_clause(def_rule(H,BodyList,Lit),N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
% disjunctive clause with a single head atom senza depth_bound con prob =1
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,BDDAnd,Module,Head1),
  Clauses=[(Head1 :- Body1)].


term_expansion_int((Head :- Body), ((H :- Body),[])):-
  Head=db(H),!.
  
term_expansion_int((Head :- Body), (Clauses,[rule(R,HeadList,BodyList,true)])):-
  setting(compiling,on),
  setting(depth_bound,true),
% disjunctive clause with more than one head atom e depth_bound
  Head = (_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,Module),
  append([one(BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (setting(single_var,true)->
    generate_rules_db(HeadList,Body1,[],R,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_rules_db(HeadList,Body1,VC,R,Probs,DB,BDDAnd,0,Clauses,Module)
   ).
  
term_expansion_int((Head :- Body), (Clauses,[rule(R,HeadList,BodyList,true)])):-
  setting(compiling,on),
% disjunctive clause with more than one head atom senza depth_bound
  Head = (_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Module),
  append([one(BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (setting(single_var,true)->
    generate_rules(HeadList,Body1,[],R,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_rules(HeadList,Body1,VC,R,Probs,BDDAnd,0,Clauses,Module)
  ).

term_expansion_int((Head :- Body), ([],[])) :- 
% disjunctive clause with a single head atom con prob. 0 senza depth_bound --> la regola non è caricata nella teoria e non è conteggiata in NR
  setting(compiling,on),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  Head = (_H:P),P=:=0.0, !. 

term_expansion_int((Head :- Body), (Clauses,[def_rule(H,BodyList,true)])) :- 
% disjunctive clause with a single head atom e depth_bound
  setting(compiling,on),
  setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(H,BDDAnd,DB,Module,Head1),
  Clauses=(Head1 :- Body1).

term_expansion_int((Head :- Body), (Clauses,[def_rule(H,BodyList,true)])) :- 
% disjunctive clause with a single head atom senza depth_bound con prob =1
  setting(compiling,on),
   ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,BDDAnd,Module,Head1),
  Clauses=(Head1 :- Body1).

term_expansion_int((Head :- Body), (Clauses,[rule(R,HeadList,BodyList,true)])) :- 
% disjunctive clause with a single head atom e DB, con prob. diversa da 1
  setting(compiling,on),
  setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  Head = (H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),%***test single_var
  (setting(single_var,true)->
    generate_clause_db(H,Body2,[],R,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_clause_db(H,Body2,VC,R,Probs,DB,BDDAnd,0,Clauses,Module)
  ).

term_expansion_int((Head :- Body), (Clauses,[rule(R,HeadList,BodyList,true)])) :- 
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  setting(compiling,on),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  Head = (H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),%***test single_vars
  (setting(single_var,true)->
    generate_clause(H,Body2,[],R,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_clause(H,Body2,VC,R,Probs,BDDAnd,0,Clauses,Module)
  ).
  
term_expansion_int((Head :- Body),(Clauses,[])) :- 
% definite clause for db facts
  setting(compiling,on),  
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),
  Head=db(Head1),!,
  Clauses=(Head1 :- Body).

term_expansion_int((Head :- Body),(Clauses,[def_rule(Head,BodyList,true)])) :- 
% definite clause with depth_bound
  setting(compiling,on),  
  setting(depth_bound,true),
   ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(Head,BDDAnd,DB,Module,Head1),
  Clauses=(Head1 :- Body1).
  
term_expansion_int((Head :- Body),(Clauses,[def_rule(Head,BodyList,true)])) :- 
% definite clause senza DB
  setting(compiling,on),  
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Head,BDDAnd,Module,Head1),
  Clauses=(Head1 :- Body2).

term_expansion_int(Head,(Clauses,[rule(R,HeadList,[],true)])) :- 
  setting(compiling,on),
  setting(depth_bound,true),
% disjunctive FACT with more than one head atom e db
  Head=(_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (setting(single_var,true)->
    generate_rules_fact_db(HeadList,[],R,Probs,0,Clauses,_Module)
  ;
    generate_rules_fact_db(HeadList,VC,R,Probs,0,Clauses,_Module)
  ).

term_expansion_int(Head,(Clauses,[rule(R,HeadList,[],true)])) :- 
  setting(compiling,on),
% disjunctive fact with more than one head atom senza db
  Head=(_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs), %**** test single_var
  (setting(single_var,true)->
    generate_rules_fact(HeadList,[],R,Probs,0,Clauses,_Module)
  ;
    generate_rules_fact(HeadList,VC,R,Probs,0,Clauses,_Module)
  ).

term_expansion_int(Head,([],[])) :- 
  setting(compiling,on),
% disjunctive fact with a single head atom con prob. 0
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (_H:P),P=:=0.0, !.
  
term_expansion_int(Head,(Clause,[def_rule(H,[],true)])) :- 
  setting(compiling,on),
  setting(depth_bound,true),
% disjunctive fact with a single head atom con prob.1 e db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),P=:=1.0, !,
  list2and([one(BDD)],Body1),
  add_bdd_arg_db(H,BDD,_DB,_Module,Head1),
  Clause=(Head1 :- Body1).

term_expansion_int(Head,(Clause,[def_rule(H,[],true)])) :- 
  setting(compiling,on),
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),P=:=1.0, !,
  list2and([one(BDD)],Body1),
  add_bdd_arg(H,BDD,_Module,Head1),
  Clause=(Head1 :- Body1).

term_expansion_int(Head,(Clause,[rule(R,HeadList,[],true)])) :- 
  setting(compiling,on),
  setting(depth_bound,true),
% disjunctive fact with a single head atom e prob. generiche, con db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  add_bdd_arg_db(H,BDD,_DB,_Module,Head1),
  (setting(single_var,true)->
    Clause=(Head1:-(get_var_n(R,[],Probs,V),equality(V,0,BDD)))
  ;
    Clause=(Head1:-(get_var_n(R,VC,Probs,V),equality(V,0,BDD)))
  ).

term_expansion_int(Head,(Clause,[rule(R,HeadList,[],true)])) :- 
  setting(compiling,on),
% disjunctive fact with a single head atom e prob. generiche, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  add_bdd_arg(H,BDD,_Module,Head1),%***test single_var
  (setting(single_var,true)->
    Clause=(Head1:-(get_var_n(R,[],Probs,V),equality(V,0,BDD)))
  ;
    Clause=(Head1:-(get_var_n(R,VC,Probs,V),equality(V,0,BDD)))
  ).

term_expansion_int(Head, ((Head1:-one(One)),[def_rule(Head,[],true)])) :- 
  setting(compiling,on),
  setting(depth_bound,true),
% definite fact with db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg_db(Head,One,_DB,_Module,Head1).

term_expansion_int(Head, ((Head1:-one(One)),[def_rule(Head,[],true)])) :- 
  setting(compiling,on),
% definite fact without db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg(Head,One,_Module,Head1).

/*-----------*/
user:term_expansion((Head :- Body), (H :- Body)):-
  Head=db(H),!.
  
user:term_expansion((Head :- Body), Clauses):-
  setting(compiling,on),
  setting(depth_bound,true),
% disjunctive clause with more than one head atom e depth_bound
  Head = (_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,Module),
  append([one(BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (setting(single_var,true)->
    generate_rules_db(HeadList,Body1,[],R,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_rules_db(HeadList,Body1,VC,R,Probs,DB,BDDAnd,0,Clauses,Module)
   ).
  
user:term_expansion((Head :- Body), Clauses):-
  setting(compiling,on),
% disjunctive clause with more than one head atom senza depth_bound
  Head = (_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Module),
  append([one(BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (setting(single_var,true)->
    generate_rules(HeadList,Body1,[],R,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_rules(HeadList,Body1,VC,R,Probs,BDDAnd,0,Clauses,Module)
  ).

user:term_expansion((Head :- Body), []) :- 
% disjunctive clause with a single head atom con prob. 0 senza depth_bound --> la regola non è caricata nella teoria e non è conteggiata in NR
  setting(compiling,on),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  Head = (_H:P),P=:=0.0, !. 

user:term_expansion((Head :- Body), Clauses) :- 
% disjunctive clause with a single head atom e depth_bound
  setting(compiling,on),
  setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(H,BDDAnd,DB,Module,Head1),
  Clauses=(Head1 :- Body1).

user:term_expansion((Head :- Body), Clauses) :- 
% disjunctive clause with a single head atom senza depth_bound con prob =1
  setting(compiling,on),
   ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,BDDAnd,Module,Head1),
  Clauses=(Head1 :- Body1).

user:term_expansion((Head :- Body), Clauses) :- 
% disjunctive clause with a single head atom e DB, con prob. diversa da 1
  setting(compiling,on),
  setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  Head = (H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),%***test single_var
  (setting(single_var,true)->
    generate_clause_db(H,Body2,[],R,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_clause_db(H,Body2,VC,R,Probs,DB,BDDAnd,0,Clauses,Module)
  ).

user:term_expansion((Head :- Body), Clauses) :- 
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  setting(compiling,on),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  Head = (H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),%***test single_vars
  (setting(single_var,true)->
    generate_clause(H,Body2,[],R,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_clause(H,Body2,VC,R,Probs,BDDAnd,0,Clauses,Module)
  ).
  
user:term_expansion((Head :- Body),Clauses) :- 
% definite clause for db facts
  setting(compiling,on),  
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),
  Head=db(Head1),!,
  Clauses=(Head1 :- Body).

user:term_expansion((Head :- Body),Clauses) :- 
% definite clause with depth_bound
  setting(compiling,on),  
  setting(depth_bound,true),
   ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(Head,BDDAnd,DB,Module,Head1),
  Clauses=(Head1 :- Body1).
  
user:term_expansion((Head :- Body),Clauses) :- 
% definite clause senza DB
  setting(compiling,on),  
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Module),
  append([one(BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Head,BDDAnd,Module,Head1),
  Clauses=(Head1 :- Body2).

user:term_expansion(Head,Clauses) :- 
  setting(compiling,on),
  setting(depth_bound,true),
% disjunctive FACT with more than one head atom e db
  Head=(_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (setting(single_var,true)->
    generate_rules_fact_db(HeadList,[],R,Probs,0,Clauses,_Module)
  ;
    generate_rules_fact_db(HeadList,VC,R,Probs,0,Clauses,_Module)
  ).

user:term_expansion(Head,Clauses) :- 
  setting(compiling,on),
% disjunctive fact with more than one head atom senza db
  Head=(_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs), %**** test single_var
  (setting(single_var,true)->
    generate_rules_fact(HeadList,[],R,Probs,0,Clauses,_Module)
  ;
    generate_rules_fact(HeadList,VC,R,Probs,0,Clauses,_Module)
  ).

user:term_expansion(Head,[]) :- 
  setting(compiling,on),
% disjunctive fact with a single head atom con prob. 0
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (_H:P),P=:=0.0, !.
  
user:term_expansion(Head,Clause) :- 
  setting(compiling,on),
  setting(depth_bound,true),
% disjunctive fact with a single head atom con prob.1 e db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),P=:=1.0, !,
  list2and([one(BDD)],Body1),
  add_bdd_arg_db(H,BDD,_DB,_Module,Head1),
  Clause=(Head1 :- Body1).

user:term_expansion(Head,Clause) :- 
  setting(compiling,on),
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),P=:=1.0, !,
  list2and([one(BDD)],Body1),
  add_bdd_arg(H,BDD,_Module,Head1),
  Clause=(Head1 :- Body1).

user:term_expansion(Head,Clause) :- 
  setting(compiling,on),
  setting(depth_bound,true),
% disjunctive fact with a single head atom e prob. generiche, con db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  add_bdd_arg_db(H,BDD,_DB,_Module,Head1),
  (setting(single_var,true)->
    Clause=(Head1:-(get_var_n(R,[],Probs,V),equality(V,0,BDD)))
  ;
    Clause=(Head1:-(get_var_n(R,VC,Probs,V),equality(V,0,BDD)))
  ).

user:term_expansion(Head,Clause) :- 
  setting(compiling,on),
% disjunctive fact with a single head atom e prob. generiche, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  add_bdd_arg(H,BDD,_Module,Head1),%***test single_var
  (setting(single_var,true)->
    Clause=(Head1:-(get_var_n(R,[],Probs,V),equality(V,0,BDD)))
  ;
    Clause=(Head1:-(get_var_n(R,VC,Probs,V),equality(V,0,BDD)))
  ).

user:term_expansion(Head, (Head1:-one(One))) :- 
  setting(compiling,on),
  setting(depth_bound,true),
% definite fact with db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg_db(Head,One,_DB,_Module,Head1).

user:term_expansion(Head, (Head1:-one(One))) :- 
  setting(compiling,on),
% definite fact without db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg(Head,One,_Module,Head1).


builtin(_A is _B).
builtin(_A > _B).
builtin(_A < _B).
builtin(_A >= _B).
builtin(_A =< _B).
builtin(_A =:= _B).
builtin(_A =\= _B).
builtin(true).
builtin(false).
builtin(_A = _B).
builtin(_A==_B).
builtin(_A\=_B).
builtin(_A\==_B).
builtin('!').
builtin(length(_L,_N)).
builtin(member(_El,_L)).
builtin(average(_L,_Av)).
builtin(max_list(_L,_Max)).
builtin(min_list(_L,_Max)).
builtin(nth0(_,_,_)).
builtin(nth(_,_,_)).
builtin(name(_,_)).
builtin(float(_)).
builtin(integer(_)).
builtin(var(_)).
builtin(_ @> _).
builtin(memberchk(_,_)).


average(L,Av):-
        sum_list(L,Sum),
        length(L,N),
        Av is Sum/N.

