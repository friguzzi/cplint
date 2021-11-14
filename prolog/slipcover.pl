
:-module(slipcover,[set_sc/2,setting_sc/2,
  induce/2,induce_par/2,test/7,list2or/2,list2and/2,
  sample/4,learn_params/5,
  op(500,fx,#),op(500,fx,'-#'),
  test_prob/6,rules2terms/2,
  get_sc_var_n/6,
  process_clauses/6,
  generate_clauses/6,
  generate_clauses_bg/2,
  generate_body/3,
  make_dynamic/1,
  extract_fancy_vars/2,
  linked_clause/3,
  banned_clause/3,
  take_var_args/3,
  remove_duplicates/2,
  extract_type_vars/3,
  delete_one/3,
  get_next_rule_number/2,
  tab/3,
  zero_clause/3,
  member_eq/2,
  retract_all/1,assert_all/3,
  write2/2,write3/2,format2/3,format3/3,
  write_rules2/3,write_rules3/3,
  nl2/1,nl3/1]).

/** <module> slipcover

This module performs learning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It performs both parameter and structure learning.

See http://friguzzi.github.io/cplint/_build/html/index.html for
details.

@author Fabrizio Riguzzi, Elena Bellodi
@license Artistic License 2.0
@copyright Fabrizio Riguzzi, Elena Bellodi
*/

/*

SLIPCOVER

Copyright (c) 2016, Fabrizio Riguzzi and Elena Bellodi

*/
:-reexport(library(bddem)).
:-use_module(library(auc)).
:-use_module(library(lists)).
:-use_module(library(random)).
:-use_module(library(system)).
:-use_module(library(terms)).
:-use_module(library(apply)).
:-use_module(cplint_util).
:-set_prolog_flag(unknown,warning).


:- dynamic db/1.

:- dynamic sc_input_mod/1.

:- thread_local  sc_input_mod/1.

:- meta_predicate induce(:,-).
:- meta_predicate induce_rules(:,-).
:- meta_predicate induce_par(:,-).
:- meta_predicate induce_parameters(:,-).
:- meta_predicate test(:,+,-,-,-,-,-).
:- meta_predicate test_prob(:,+,-,-,-,-).
:- meta_predicate set_sc(:,+).
:- meta_predicate setting_sc(:,-).

default_setting_sc(epsilon_em,0.0001).
default_setting_sc(epsilon_em_fraction,0.00001).
default_setting_sc(eps,0.0001).
default_setting_sc(eps_f,0.00001).

/* if the difference in log likelihood in two successive em iteration is smaller
than epsilon_em, then EM stops */
default_setting_sc(epsilon_sem,2).

/* number of random restarts of em */
default_setting_sc(random_restarts_REFnumber,1).
default_setting_sc(random_restarts_number,1).
default_setting_sc(iterREF,-1).
default_setting_sc(iter,-1).
default_setting_sc(examples,atoms).
default_setting_sc(group,1).
default_setting_sc(d,1).
default_setting_sc(verbosity,1).
default_setting_sc(logzero,log(0.000001)).
default_setting_sc(megaex_bottom,1).
default_setting_sc(initial_clauses_per_megaex,1).
default_setting_sc(max_iter,10).
default_setting_sc(max_iter_structure,10000).
default_setting_sc(max_var,4).
default_setting_sc(max_rules,10).
default_setting_sc(maxdepth_var,2).
default_setting_sc(beamsize,100).
default_setting_sc(max_body_length,100).
default_setting_sc(neg_literals,false).
default_setting_sc(background_clauses,50).

default_setting_sc(specialization,bottom).
/* allowed values: mode,bottom */
default_setting_sc(specialize_head,false).

default_setting_sc(seed,seed(3032)).
default_setting_sc(c_seed,21344).
default_setting_sc(score,ll).
/* allowed values: ll aucpr */
default_setting_sc(neg_ex,cw).


default_setting_sc(epsilon_parsing, 1e-5).
default_setting_sc(tabling,auto).
/* values:
  auto
  explicit
*/

default_setting_sc(bagof,false).
/* values: false, intermediate, all, extra */

default_setting_sc(compiling,off).


default_setting_sc(depth_bound,true).  %if true, it limits the derivation of the example to the value of 'depth'
default_setting_sc(depth,2).
default_setting_sc(single_var,true). %false:1 variable for every grounding of a rule; true: 1 variable for rule (even if a rule has more groundings),simpler.

default_setting_sc(prob_approx,false). %if true, it limits the number of different solutions found when computing the probability
default_setting_sc(approx_value,100).

default_setting_sc(alpha,0.0).
% Sets the type of parameter initialization for EM on Environment:
% if alpha is 0.0, it uses a truncated Dirichlet process
% if alpha is a float > 0.0, it uses a symmetric Dirichlet distribution
% with that value as parameter

/**
 * induce(:TrainFolds:list_of_atoms,-P:probabilistic_program) is det
 *
 * The predicate performs structure learning using the folds indicated in
 * TrainFolds for training.
 * It returns in P the learned probabilistic program.
 */
induce(TrainFolds,P):-
  induce_rules(TrainFolds,P0),
  rules2terms(P0,P).

/**
 * test(:P:probabilistic_program,+TestFolds:list_of_atoms,-LL:float,-AUCROC:float,-ROC:dict,-AUCPR:float,-PR:dict) is det
 *
 * The predicate takes as input in P a probabilistic program,
 * tests P on the folds indicated in TestFolds and returns the
 * log likelihood of the test examples in LL, the area under the Receiver
 * Operating Characteristic curve in AUCROC, a dict containing the points
 * of the ROC curve in ROC, the area under the Precision Recall curve in AUCPR
 * and a dict containing the points of the PR curve in PR
 */
test(P,TestFolds,LL,AUCROC,ROC,AUCPR,PR):-
  test_prob(P,TestFolds,_NPos,_NNeg,LL,LG),
  compute_areas_diagrams(LG,AUCROC,ROC,AUCPR,PR).

/**
 * test_prob(:P:probabilistic_program,+TestFolds:list_of_atoms,-NPos:int,-NNeg:int,-LL:float,-Results:list) is det
 *
 * The predicate takes as input in P a probabilistic program,
 * tests P on the folds indicated in TestFolds and returns
 * the number of positive examples in NPos, the number of negative examples
 * in NNeg, the log likelihood in LL
 * and in Results a list containing the probabilistic result for each query contained in TestFolds.
 */
test_prob(M:P,TestFolds,NPos,NNeg,CLL,Results) :-
  write2(M,'Testing\n'),
  findall(Exs,(member(F,TestFolds),M:fold(F,Exs)),L),
  append(L,TE),
  reset_next_rule_number(M),
  set_sc(M:compiling,on),
  process_clauses(P,M,[],_,[],PRules),
  generate_clauses(PRules,M,RuleFacts,0,[],Th),
  assert_all(Th,M,ThRef),
  assert_all(RuleFacts,M,RFRef),
  (M:bg(RBG0)->
    process_clauses(RBG0,M,[],_,[],RBG),
    generate_clauses(RBG,M,_RBGRF,0,[],ThBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ClBG,M,ClBGRef),
    assert_all(ThBG,ThBGRef)
  ;
    true
  ),
  set_sc(M:compiling,off),
  test_no_area([TE],M,NPos,NNeg,CLL,Results),
  (M:bg(RBG0)->
    retract_all(ThBGRef),
    retract_all(ClBGRef)
  ;
    true
  ),
  retract_all(ThRef),
  retract_all(RFRef).

induce_rules(M:Folds,R):-
  set_sc(M:compiling,on),
  M:local_setting(seed,Seed),
  set_random(Seed),
  M:local_setting(c_seed,CSeed),
  rand_seed(CSeed),
  findall(Exs,(member(F,Folds),M:fold(F,Exs)),L),
  append(L,DB),
  assert(M:database(DB)),
  (M:bg(RBG0)->
    process_clauses(RBG0,M,[],_,[],RBG),
    generate_clauses(RBG,M,_RBG1,0,[],ThBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ThBG,M,ThBGRef),
    assert_all(ClBG,M,ClBGRef)
  ;
    true
  ),
  length(DB,NMegaEx),
  M:local_setting(megaex_bottom, NumMB),
  (NMegaEx >= NumMB ->
      true
    ;
      format2(M,"~nWARN: Number of required bottom clauses is greater than the number of training examples!~n. The number of required bottom clauses will be equal to the number of training examples", []),
      set_sc(M:megaex_bottom, NMegaEx)
  ),

  statistics(walltime,[_,_]),
  (M:local_setting(specialization,bottom)->
    M:local_setting(megaex_bottom,MB),
    deduct(MB,M,DB,[],InitialTheory),
    length(InitialTheory,_LI),
    remove_duplicates(InitialTheory,R1)
  ;
    get_head_atoms(O,M),
    generate_top_cl(O,M,R1)
  ),
  learn_struct(DB,M,R1,R2,Score2),
  learn_params(DB,M,R2,R,Score),
  format2(M,"~nRefinement score  ~f - score after EMBLEM ~f~n",[Score2,Score]),
  statistics(walltime,[_,WT]),
  WTS is WT/1000.0,
  write2(M,'\n\n'),
  format2(M,'/* SLIPCOVER Final score ~f~n',[Score]),
  format2(M,'Wall time ~f */~n',[WTS]),
  write_rules2(M,R,user_output),
  set_sc(M:compiling,off),
  (M:bg(RBG0)->
    retract_all(ThBGRef),
    retract_all(ClBGRef)
  ;
    true
  ),
  clean_up_db_structure(M).

clean_up_db_structure(M):-
  retractall(M:ref(_)),
  retractall(M:ref_clause(_)),
  clean_up_db(M).

/**
 * make_dynamic(+Module:atom) is det
 *
 * Makes the predicates required for learning dynamic.
 */
make_dynamic(M):-
  M:(dynamic int/1),
  findall(O,M:output(O),LO),
  findall(I,M:input(I),LI),
  findall(I,M:input_cw(I),LIC),
  findall(D,M:determination(D,_DD),LDH),
  findall(DD,M:determination(_D,DD),LDD),
  findall(DH,(M:modeh(_,_,_,LD),member(DH,LD)),LDDH),
  append([LO,LI,LIC,LDH,LDD,LDDH],L0),
  remove_duplicates(L0,L),
  maplist(to_dyn(M),L).

to_dyn(M,P/A):-
  A1 is A+1,
  M:(dynamic P/A1),
  A2 is A1+2,
  M:(dynamic P/A2),
  A3 is A2+1,
  M:(dynamic P/A3).



gen_fixed([],_M,[]).

gen_fixed([(H,B,BL)|T],M,[rule(R,H,B,BL,tunable)|T1]):-
  get_next_rule_number(M,R),
  gen_fixed(T,M,T1).


learn_struct(DB,Mod,R1,R,Score):-   %+R1:initial theory of the form [rule(NR,[h],[b]],...], -R:final theory of the same form, -CLL
  format2(Mod,"Clause search~n~n",[]),
  Mod:local_setting(max_iter,M),
  Mod:local_setting(depth_bound,DepthB),
  set_sc(Mod:depth_bound,false),
  findall((H,B,BL),Mod:fixed_rule(H,B,BL),LF),
  length(LF,LLF),
  gen_fixed(LF,Mod,LFR),
  format2(Mod,"Scoring fixed clauses: ~d clauses~n~n",[LLF]),
  score_clause_refinements(LFR,Mod,1,LLF,DB,[],NB1,[],CL0,[],CLBG0),
  append(NB1,R1,Beam),
  cycle_beam(Beam,Mod,DB,CL0,CL,CLBG0,BG,M),
  learn_params(DB,Mod,[],REmpty,S),
  set_sc(Mod:depth_bound,DepthB),
  format2(Mod,"Theory search~n~n",[]),
  Mod:local_setting(max_iter_structure,MS),
  cycle_structure(CL,Mod,REmpty,S,-1e20,DB,R2,Score,MS),
  format2(Mod,"Best target theory~n~n",[]),
  write_rules2(Mod,R2,user_output),
  Mod:local_setting(background_clauses,NBG1),
  length(BG,NBG),
  format2(Mod,"Background search: ~d of ~d clauses~n~n",[NBG1,NBG]),
  pick_first(NBG1,BG,BG1),
  remove_score(BG,BG2),
  write_rules2(Mod,BG2,user_output),
  write2(Mod,'\n'),
  append(R2,BG1,R).

pick_first(0,_,[]):-!.

pick_first(_,[],[]):-!.

pick_first(N,[(H,_S)|T],[H|T1]):-
  N1 is N-1,
  pick_first(N1,T,T1).

remove_score([],[]).

remove_score([(H,_S)|T],[H|T1]):-
  remove_score(T,T1).

cycle_structure([],_Mod,R,S,_SP,_DB,R,S,_I):-!.  %empty beam

cycle_structure(_CL,_Mod,R,S,_SP,_DB,R,S,0):-!.  %0 iterations

cycle_structure([(RH,_CLL)|RT],Mod,R0,S0,SP0,DB,R,S,M):-
  already_scored(Mod,[RH|R0],R3,Score),!,
  format2(Mod,"Theory iteration ~d~n~n",[M]),
  write3(Mod,'Already scored, updated refinement\n'),
  write_rules3(Mod,R3,user_output),
  write3(Mod,'Score '),write3(Mod,Score),write3(Mod,'\n\n\n'),
  (Score>S0->
    R4=R3,
    S4=Score,
    SP1=S0
  ;
    R4=R0,
    S4=S0,
    SP1=SP0
  ),
  M1 is M-1,
  cycle_structure(RT,Mod,R4,S4,SP1,DB,R,S,M1).

cycle_structure([(RH,_Score)|RT],Mod,R0,S0,SP0,DB,R,S,M):-
  format2(Mod,"Theory iteration ~d~n~n",[M]),
  reset_next_rule_number(Mod),
  generate_clauses([RH|R0],Mod,R2,0,[],Th1),
  format3(Mod,"Initial theory~n~n",[]),
  write_rules3(Mod,[RH|R0],user_output),
  assert_all(Th1,Mod,Th1Ref),
  assert_all(R2,Mod,R2Ref),!,
  init_em(ExData),
  retractall(Mod:v(_,_,_)),
  length(DB,NEx),
  abolish_all_tables,
  (Mod:local_setting(examples,atoms)->
    Mod:local_setting(group,G),
    derive_bdd_nodes_groupatoms(DB,Mod,ExData,NEx,G,[],Nodes,0,CLL0,LE,[]),!   % 1 BDD per example if G=1
  ;
    derive_bdd_nodes(DB,Mod,ExData,NEx,[],Nodes,0,CLL0),! % 1 BDD per model
  ),
  Mod:local_setting(random_restarts_number,N),
  format3(Mod,"~nInitial CLL ~f~n~n",[CLL0]),
  random_restarts(N,Mod,ExData,Nodes,CLL0,Score,initial,Par,LE),   %output:CLL,Par
  format3(Mod,"Score after EMBLEM = ~f~n",[Score]),
  retract_all(Th1Ref),
  retract_all(R2Ref),!,
  end_ex(ExData),
  update_theory(R2,Par,R3),
  write3(Mod,'Updated Theory\n'),
  write_rules3(Mod,R3,user_output),   %definite rules without probabilities in the head are not written
  (Score>S0->
    R4=R3,
    S4=Score,
    SP1=S0,
    write3(Mod,'New best score\n')
  ;
    R4=R0,
    S4=S0,
    SP1=SP0
  ),
  store_refinement(Mod,[RH|R0],R3,Score),
  M1 is M-1,
  cycle_structure(RT,Mod,R4,S4,SP1,DB,R,S,M1).

/**
 * induce_par(:TrainFolds:list_of_atoms,-P:probabilistic_program) is det
 *
 * The predicate learns the parameters of the program stored in the in/1 fact
 * of the input file using the folds indicated in TrainFolds for training.
 * It returns in P the input program with the updated parameters.
 */
induce_par(Folds,ROut):-
  induce_parameters(Folds,R),
  rules2terms(R,ROut).

induce_parameters(M:Folds,R):-
  set_sc(M:compiling,on),
  M:local_setting(seed,Seed),
  set_random(Seed),
  M:local_setting(c_seed,CSeed),
  rand_seed(CSeed),
  findall(Exs,(member(F,Folds),M:fold(F,Exs)),L),
  append(L,DB),
  assert(M:database(DB)),
  statistics(walltime,[_,_]),
  (M:bg(RBG0)->
    process_clauses(RBG0,M,[],_,[],RBG),
    generate_clauses(RBG,M,_RBG1,0,[],ThBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ClBG,M,ClBGRef),
    assert_all(ThBG,ThBGRef)
  ;
    true
  ),
  M:in(R00),
  process_clauses(R00,M,[],_,[],R0),
  statistics(walltime,[_,_]),
  learn_params(DB,M,R0,R,Score),
  statistics(walltime,[_,CT]),
  CTS is CT/1000.0,
  format2(M,'/* EMBLEM Final score ~f~n',[Score]),
  format2(M,'Wall time ~f */~n',[CTS]),
  write_rules2(M,R,user_output),
  set_sc(M:compiling,off),
  (M:bg(RBG0)->
    retract_all(ThBGRef),
    retract_all(ClBGRef)
  ;
    true
  ),
  clean_up_db(M).

clean_up_db(M):-
  retract(M:rule_sc_n(_)),
  assert(M:rule_sc_n(0)),
  retract(M:rule_ng_sc_n(_)),
  assert(M:rule_ng_sc_n(0)),
  retractall(M:v(_,_,_)),
  retractall(M:database(_)).

get_rule_info(M,RI-Info):-
  M:rule(R,HL,_BL,_Lit,Tun),
  R\= ng(_,_),
  (R=g(RI)->
    true
  ;
    RI=R
  ),
  length(HL,N),
  ((Tun=tunable;Tun=initial)->
    Info=N
  ;
    Info=[N]
  ).

get_rule_info_rand(M,RI-Info):-
  M:rule(R,HL,_BL,_Lit,Tun),
  R\= ng(_,_),
  (R=g(RI)->
    true
  ;
    RI=R
  ),
  length(HL,N),
  (Tun=tunable->
    Info=N
  ;
    get_probs(HL,Info0),
    (Tun=initial->
      Info=Info0
    ; % fixed parameters
      Info=[Info0]
    )
  ).


/**
 * learn_params(+DB:list_of_atoms,+M:atom,+R0:probabilistic_program,-P:probabilistic_program,-Score:float) is det
 *
 * The predicate learns the parameters of the program R0 and returns
 * the updated program in R and the score in Score.
 * DB contains the list of interpretations ids and M the module where
 * the data is stored.
 */

learn_params(DB,M,R0,R,Score):-  %Parameter Learning
  reset_next_rule_number(M),
  reset_next_nonground_rule_number(M),
  generate_clauses(R0,M,R1,0,[],Th0),
  format2(M,"Initial theory~n",[]),
  write_rules2(M,R1,user_output),
  assert_all(Th0,M,Th0Ref),
  assert_all(R1,M,R1Ref),!,
  init_em(ExData),
  retractall(M:v(_,_,_)),
  length(DB,NEx),
  abolish_all_tables,
  (M:local_setting(examples,atoms)->
    M:local_setting(group,G),
    derive_bdd_nodes_groupatoms(DB,M,ExData,NEx,G,[],Nodes,0,CLL0,LE,[]),!
  ;
    derive_bdd_nodes(DB,M,ExData,NEx,[],Nodes,0,CLL0),!
  ),
  format3(M,"Initial score ~f~n",[CLL0]),
  M:local_setting(random_restarts_number,N),
  random_restarts(N,M,ExData,Nodes,-1e20,Score,initial,Par,LE),  %computes new parameters Par
  end_ex(ExData),
  update_theory_par(M,R1,Par,R),
  retract_all(Th0Ref),
  retract_all(R1Ref),!,
  retractall(M:rule(_,_,_,_,_)).


update_theory_par(M,OldR,Par,Rules):-
   findall(def_rule(H,B,L),member(def_rule(H,B,L),OldR),DefRules0),
   findall((H:-B),member((H:-B),OldR),DefRules1),
   new_rules(Par,M,DisjRules0),
   sort(DisjRules0,DisjRules),
   append([DefRules0,DefRules1,DisjRules],Rules).

new_rules([],_M,[]).

new_rules([[_N,[1.0|_]]|T],M,R):-!,
  new_rules(T,M,R).


new_rules([[N,P]|T],M,[rule(N,H1,B,L,Tun)|R]):-
  reverse(P,P1),
  (M:rule(N,H,B,L,Tun);M:rule(g(N),H,B,L,Tun)),
  update_head_par(H,P1,H1),
  new_rules(T,M,R).


update_theory(R,initial,R):-!.

update_theory([],_Par,[]).

update_theory([def_rule(H,B,L)|T0],Par,[def_rule(H,B,L)|T]):-!,
  update_theory(T0,Par,T).

update_theory([(H:-B)|T0],Par,[(H:-B)|T]):-!,
  update_theory(T0,Par,T).

update_theory([rule(N,H,B,L,Tun)|T0],Par,[rule(N,H1,B,L,Tun)|T]):-
  member([N,P],Par),!,
  reverse(P,P1),
  update_head_par(H,P1,H1),
  update_theory(T0,Par,T).

update_head_par([],[],[]).

update_head_par([H:_P|T0],[HP|TP],[H:HP|T]):-
  update_head_par(T0,TP,T).

cycle_beam([],_Mod,_DB,CL,CL,CLBG,CLBG,_M):-!.

cycle_beam(_Beam,_Mod,_DB,CL,CL,CLBG,CLBG,0):-!.

cycle_beam(Beam,Mod,DB,CL0,CL,CLBG0,CLBG,M):-
  format2(Mod,"Clause iteration ~d~n~n",[M]),
  cycle_clauses(Beam,Mod,DB,[],NB,CL0,CL1,CLBG0,CLBG1),
  M1 is M-1,%decreases the number of max_iter M
  cycle_beam(NB,Mod,DB,CL1,CL,CLBG1,CLBG,M1).

cycle_clauses([],_M,_DB,NB,NB,CL,CL,CLBG,CLBG):-!.

cycle_clauses([(RH,_ScoreH)|T],M,DB,NB0,NB,CL0,CL,CLBG0,CLBG):-
  findall(RS,specialize_rule(RH,M,RS,_L),LR),!,   %-LR:list of lists, each one correponding to a different revised theory; specialize_rule defined in revise.pl
  length(LR,NR),
  write3(M,'Number of revisions '),write3(M,NR),write3(M,'\n'),
  score_clause_refinements(LR,M,1,NR,DB,NB0,NB1,CL0,CL1,CLBG0,CLBG1),
  cycle_clauses(T,M,DB,NB1,NB,CL1,CL,CLBG1,CLBG).

score_clause_refinements([],_M,_N,_NR,_DB,NB,NB,CL,CL,CLBG,CLBG).

score_clause_refinements([R1|T],M,Nrev,NRef,DB,NB0,NB,CL0,CL,CLBG0,CLBG):-  %scans the list of revised theories
  already_scored_clause(M,R1,R3,Score),!,
  format3(M,'Score ref.  ~d of ~d~n',[Nrev,NRef]),
  write3(M,'Already scored, updated refinement\n'),
  write_rules3(M,[R3],user_output),
  write3(M,'Score '),write3(M,Score),write3(M,'\n\n\n'),
  M:local_setting(beamsize,BS),
  insert_in_order(NB0,(R3,Score),BS,NB1),
  Nrev1 is Nrev+1,
  score_clause_refinements(T,M,Nrev1,NRef,DB,NB1,NB,CL0,CL,CLBG0,CLBG).

score_clause_refinements([R1|T],M,Nrev,NRef,DB,NB0,NB,CL0,CL,CLBG0,CLBG):-
  format3(M,'Score ref.  ~d of ~d~n',[Nrev,NRef]),
  write_rules3(M,[R1],user_output),
  generate_clauses_cw([R1],M,[R2],0,[],Th1),
  assert_all(Th1,M,Th1Ref),
  assert_all([R2],M,[R2Ref]),!,
  init_em(ExData),
  retractall(M:v(_,_,_)),
  length(DB,NEx),
  get_output_preds(R1,O),
  abolish_all_tables,
  (M:local_setting(examples,atoms)->
    M:local_setting(group,G),
    derive_bdd_nodes_groupatoms_output_atoms(DB,M,ExData,O,NEx,G,[],Nodes,0,CLL0,LE,[]),!
  ;
    derive_bdd_nodes(DB,M,ExData,NEx,[],Nodes,0,CLL0),!
  ),
  format3(M,"Initial CLL ~f~n",[CLL0]),
  M:local_setting(random_restarts_REFnumber,N),
  random_restarts_ref(N,M,ExData,Nodes,CLL0,Score,initial,Par,LE),
  end_ex(ExData),
  update_theory([R2],Par,[R3]),
  write3(M,'Updated refinement\n'),
  write_rules3(M,[R3],user_output),
  write3(M,'Score (CLL) '),write3(M,Score),write3(M,'\n\n\n'),
  retract_all(Th1Ref),
  retract_all([R2Ref]),!,
  M:local_setting(beamsize,BS),
  insert_in_order(NB0,(R3,Score),BS,NB1),
  (target(R3,M)->
    insert_in_order(CL0,(R3,Score),+1e20,CL1),
    length(CL1,LCL1),
    format2(M,"N. of target clauses ~d~n~n",[LCL1]),
    CLBG1=CLBG0
  ;
    (range_restricted(R3)->
      insert_in_order(CLBG0,(R3,Score),+1e20,CLBG1),
      length(CLBG1,LCL1),
      format2(M,"N. of background clauses ~d~n~n",[LCL1]),
      CL1=CL0
    ;
      format2(M,"Not range restricted~n~n",[]),
      CL1=CL0,
      CLBG1=CLBG0
    )
  ),
  store_clause_refinement(M,R1,R3,Score),
  Nrev1 is Nrev+1,
  score_clause_refinements(T,M,Nrev1,NRef,DB,NB1,NB,CL1,CL,CLBG1,CLBG).

range_restricted(rule(_N,HL,BL,_Lit,_Tun)):-
  term_variables(HL,VH),
  term_variables(BL,VB),
  sublisteq(VH,VB).

sublisteq([],_).

sublisteq([H|T],L):-
  member_eq(H,L),
  sublisteq(T,L).

target(R,M):-
  get_output_preds(R,O),
  member(T,O),
  M:output(T),!.

get_output_preds(rule(_N,HL,_BL,_Lit,_Tun),O):-
  scan_head(HL,[],O).

scan_head(['':_],O,O):-!.
scan_head([],O,O):-!.
scan_head([H:_P|T],O0,O):-
  functor(H,F,N),
  (member(F/N,O0)->
    O1=O0
  ;
    O1=[F/N|O0]
  ),
  scan_head(T,O1,O).



store_clause_refinement(M,Ref,RefP,Score):-
  elab_clause_ref(Ref,Ref1),
  assert(M:ref_clause(r(Ref1,RefP,Score))).

store_refinement(M,Ref,RefP,Score):-
  elab_ref(Ref,Ref1),
  assert(M:ref(r(Ref1,RefP,Score))).

already_scored_clause(M,R,R1,Score):-
  elab_ref([R],[rule(H,B)]),
  M:ref_clause(r(rule(H,B1),R1,Score)),
  permutation(B,B1).

already_scored(M,R,R1,Score):-
  elab_ref(R,RR),
  M:ref(r(RR,R1,Score)).


elab_clause_ref(rule(_NR,H,B,_Lits,_Tun),rule(H1,B1)):-
  copy_term((H,B),(H1,B1)).

elab_ref([],[]).

elab_ref([rule(_NR,H,B,_Lits,_Tun)|T],[rule(H1,B1)|T1]):-!,
  copy_term((H,B),(H1,B1)),
  numbervars((H1,B1),0,_N),
  elab_ref(T,T1).

elab_ref([def_rule(H,B,_Lits)|T],[rule(H1,B1)|T1]):-
  copy_term((H,B),(H1,B1)),
  numbervars((H1,B1),0,_N),
  elab_ref(T,T1).

% insertion in the beam
insert_in_order([],C,BeamSize,[C]):-
  BeamSize>0,!.

insert_in_order(Beam,_New,0,Beam):-!.

insert_in_order([(Th1,Heuristic1)|RestBeamIn],(Th,Heuristic),BeamSize,BeamOut):-
  Heuristic>Heuristic1,!,
  % larger heuristic, insert here
  NewBeam=[(Th,Heuristic),(Th1,Heuristic1)|RestBeamIn],
  length(NewBeam,L),
  (L>BeamSize->
    nth1(L,NewBeam,_Last,BeamOut)
  ;
    BeamOut=NewBeam
  ).

insert_in_order([(Th1,Heuristic1)|RestBeamIn],(Th,Heuristic),BeamSize,
[(Th1,Heuristic1)|RestBeamOut]):-
  BeamSize1 is BeamSize -1,
  insert_in_order(RestBeamIn,(Th,Heuristic),BeamSize1,
  RestBeamOut).



remove_int_atom_list([],[]).

remove_int_atom_list([\+ A|T],[\+ A1|T1]):-!,
  A=..[F,_|Arg],
  A1=..[F|Arg],
  remove_int_atom_list(T,T1).

remove_int_atom_list([A|T],[A1|T1]):-
  A=..[F,_|Arg],
  A1=..[F|Arg],
  remove_int_atom_list(T,T1).



remove_int_atom(\+ A,\+ A1):-!,
  A=..[F,_|T],
  A1=..[F|T].

remove_int_atom(A,A1):-
  A=..[F,_|T],
  A1=..[F|T].


get_heads([],[]).

get_heads([_-H|T],[H|TN]):-

  get_heads(T,TN).

derive_bdd_nodes([],_M,_ExData,_E,Nodes,Nodes,CLL,CLL).

derive_bdd_nodes([H|T],M,ExData,E,Nodes0,Nodes,CLL0,CLL):-
  get_output_atoms(O,M),
  generate_goal(O,M,H,[],GL),
  (M:'$prob'(H,P)->
    CardEx is P*E

  ;
    CardEx is 1.0
  ),
  init_ex(ExData,Env),
  one(Env,One),
  get_node_list(GL,M,Env,One,BDD,CardEx),
  ret_prob(Env,BDD,HP),
  (HP=:=0.0->
    setting_sc(logzero,LZ),
    CLL1 is CLL0+LZ*CardEx
  ;
    CLL1 is CLL0+log(HP)*CardEx
  ),
  end_ex(ExData),
  append(Nodes0,[[BDD,CardEx]],Nodes1),
  derive_bdd_nodes(T,M,ExData,E,Nodes1,Nodes,CLL1,CLL).


get_node_list([],_M,_Env,BDD,BDD,_CE).


get_node_list([H|T],M,Env,BDD0,BDD,CE):-
  get_node(H,M,Env,BDD1),
  and(Env,BDD0,BDD1,BDD2),
  get_node_list(T,M,Env,BDD2,BDD,CE).


derive_bdd_nodes_groupatoms_output_atoms([],_M,_ExData,_O,_E,_G,Nodes,Nodes,CLL,CLL,LE,LE).

derive_bdd_nodes_groupatoms_output_atoms([H|T],M,ExData,O,E,G,Nodes0,Nodes,CLL0,CLL,LE0,LE):-
  generate_goal(O,M,H,[],GL),
  length(GL,NA),
  (M:'$prob'(H,P)->
    CardEx is P*E/NA
  ;
    CardEx is 1.0
  ),
  get_node_list_groupatoms(GL,M,ExData,BDDs,CardEx,G,CLL0,CLL1,LE0,LE1),
  append(Nodes0,BDDs,Nodes1),
  derive_bdd_nodes_groupatoms_output_atoms(T,M,ExData,O,E,G,Nodes1,Nodes,CLL1,CLL,LE1,LE).


derive_bdd_nodes_groupatoms([],_M,_ExData,_E,_G,Nodes,Nodes,CLL,CLL,LE,LE).

derive_bdd_nodes_groupatoms([H|T],M,ExData,E,G,Nodes0,Nodes,CLL0,CLL,LE0,LE):-
  get_output_atoms(O,M),
  generate_goal(O,M,H,[],GL),
  length(GL,NA),
  (M:'$prob'(H,P)->
    CardEx is P*E/NA
  ;
    CardEx is 1.0
  ),
  get_node_list_groupatoms(GL,M,ExData,BDDs,CardEx,G,CLL0,CLL1,LE0,LE1),
  append(Nodes0,BDDs,Nodes1),
  derive_bdd_nodes_groupatoms(T,M,ExData,E,G,Nodes1,Nodes,CLL1,CLL,LE1,LE).

get_node_list_groupatoms([],_M,_ExData,[],_CE,_Gmax,CLL,CLL,LE,LE).

get_node_list_groupatoms([H|T],M,ExData,[[BDD,CE1]|BDDT],CE,Gmax,CLL0,CLL,LE0,LE):-
  init_ex(ExData,Env),
  one(Env,One),
  get_bdd_group([H|T],M,Env,T1,Gmax,G,One,BDD,CE,LE0,LE1),  %output:BDD,CLL
  CE1 is CE*(Gmax-G),
  ret_prob(Env,BDD,HP),
  end_ex(ExData),
  (HP =:=0.0->
    M:local_setting(logzero,LZ),
    CLL2 is CLL0+LZ*CE1
  ;
    CLL2 is CLL0+log(HP)*CE1
  ),
  get_node_list_groupatoms(T1,M,ExData,BDDT,CE,Gmax,CLL2,CLL,LE1,LE).


get_bdd_group([],_M,_Env,[],G,G,BDD,BDD,_CE,LE,LE):-!.

get_bdd_group(T,_M,_Env,T,0,0,BDD,BDD,_CE,LE,LE):- !.

get_bdd_group([H|T],M,Env,T1,Gmax,G1,BDD0,BDD,CE,[H|LE0],LE):-
  get_node(H,M,Env,BDD1),		%creates the BDD for atom H
  and(Env,BDD0,BDD1,BDD2),
  G is Gmax-1,
  get_bdd_group(T,M,Env,T1,G,G1,BDD2,BDD,CE,LE0,LE).

get_arg(_-A,A).

/* EM start */
random_restarts(0,_M,_ExData,_Nodes,Score,Score,Par,Par,_LE):-!.

random_restarts(N,M,ExData,Nodes,Score0,Score,Par0,Par,LE):-
  M:local_setting(random_restarts_number,NMax),
  Num is NMax-N+1,
  format3(M,"Restart number ~d~n~n",[Num]),
  findall(R-Info,get_rule_info_rand(M,R-Info),L),
  keysort(L,LS),
  maplist(get_arg,LS,LS1),
  M:local_setting(epsilon_em,EA),
  M:local_setting(epsilon_em_fraction,ER),
  M:local_setting(iter,Iter),
  M:local_setting(alpha,Alpha),
  initial_values(ExData,Alpha),
  em(ExData,LS1,Nodes,EA,ER,Iter,CLL,Par1,ExP),
  score(M,LE,ExP,CLL,ScoreR),
  format3(M,"Random_restart: Score ~f~n",[ScoreR]),
  N1 is N-1,
  (ScoreR>Score0->
    random_restarts(N1,M,ExData,Nodes,ScoreR,Score,Par1,Par,LE)
  ;
    random_restarts(N1,M,ExData,Nodes,Score0,Score,Par0,Par,LE)
  ).

random_restarts_ref(0,_M,_ExData,_Nodes,Score,Score,Par,Par,_LE):-!.

random_restarts_ref(N,M,ExData,Nodes,Score0,Score,Par0,Par,LE):-
  M:local_setting(random_restarts_REFnumber,NMax),
  Num is NMax-N+1,
  format3(M,"Restart number ~d~n~n",[Num]),
  findall(R-Info,get_rule_info_rand(M,R-Info),L),
  keysort(L,LS),
  maplist(get_arg,LS,LS1),
  M:local_setting(epsilon_em,EA),
  M:local_setting(epsilon_em_fraction,ER),
  M:local_setting(iterREF,Iter),
  M:local_setting(alpha,Alpha),
  initial_values(ExData,Alpha),
  em(ExData,LS1,Nodes,EA,ER,Iter,CLLR,Par1,ExP),
  score(M,LE,ExP,CLLR,ScoreR),
  format3(M,"Random_restart: Score ~f~n",[ScoreR]),
  N1 is N-1,
  (ScoreR>Score0->
    random_restarts_ref(N1,M,ExData,Nodes,ScoreR,Score,Par1,Par,LE)
  ;
    random_restarts_ref(N1,M,ExData,Nodes,Score0,Score,Par0,Par,LE)
  ).


score(M,_LE,_ExP,CLL,CLL):-
  M:local_setting(score,ll),!.

score(_M,LE,ExP,_CLL,Score):-
  compute_prob(LE,ExP,LPU,0,Pos,0,Neg),
  keysort(LPU,LPO),
  reverse(LPO,LP),
  compute_aucpr(LP,Pos,Neg,Score).


compute_prob([],[],[],Pos,Pos,Neg,Neg).

compute_prob([\+ HE|TE],[HP|TP],[P- (\+ HE)|T],Pos0,Pos,Neg0,Neg):-!,
  P is 1.0-HP,
  Neg1 is Neg0+1,
  compute_prob(TE,TP,T,Pos0,Pos,Neg1,Neg).

compute_prob([ HE|TE],[HP|TP],[HP-  HE|T],Pos0,Pos,Neg0,Neg):-
  Pos1 is Pos0+1,
  compute_prob(TE,TP,T,Pos1,Pos,Neg0,Neg).


compute_aucpr(L,Pos,Neg,A):-
  L=[P_0-E|TL],
  (E= (\+ _ )->
    FP=1,
    TP=0,
    FN=Pos,
    TN is Neg -1
  ;
    FP=0,
    TP=1,
    FN is Pos -1,
    TN=Neg
  ),
  compute_curve_points(TL,P_0,TP,FP,FN,TN,Points),
  Points=[R0-P0|_TPoints],
  (R0=:=0,P0=:=0->
    Flag=true
  ;
    Flag=false
  ),
  area(Points,Flag,Pos,0,0,0,A).

compute_curve_points([],_P0,TP,FP,_FN,_TN,[1.0-Prec]):-!,
  Prec is TP/(TP+FP).

compute_curve_points([P- (\+ _)|T],P0,TP,FP,FN,TN,Pr):-!,
  (P<P0->
    Prec is TP/(TP+FP),
    Rec is TP/(TP+FN),
    Pr=[Rec-Prec|Pr1],
    P1=P
  ;
    Pr=Pr1,
    P1=P0
  ),
  FP1 is FP+1,
  TN1 is TN-1,
  compute_curve_points(T,P1,TP,FP1,FN,TN1,Pr1).

compute_curve_points([P- _|T],P0,TP,FP,FN,TN,Pr):-!,
  (P<P0->
    Prec is TP/(TP+FP),
    Rec is TP/(TP+FN),
    Pr=[Rec-Prec|Pr1],
    P1=P
  ;
    Pr=Pr1,
    P1=P0
  ),
  TP1 is TP+1,
  FN1 is FN-1,
  compute_curve_points(T,P1,TP1,FP,FN1,TN,Pr1).

area([],_Flag,_Pos,_TPA,_FPA,A,A).

area([R0-P0|T],Flag,Pos,TPA,FPA,A0,A):-
 TPB is R0*Pos,
  (TPB=:=0->
    A1=A0,
    FPB=0
  ;
    R_1 is TPA/Pos,
    (TPA=:=0->
      (Flag=false->
        P_1=P0
      ;
        P_1=0.0
      )
    ;
      P_1 is TPA/(TPA+FPA)
    ),
    FPB is TPB*(1.0-P0)/P0,
    N is TPB-TPA+0.5,
    interpolate(1,N,Pos,R_1,P_1,TPA,FPA,TPB,FPB,A0,A1)
  ),
  area(T,Flag,Pos,TPB,FPB,A1,A).

interpolate(I,N,_Pos,_R0,_P0,_TPA,_FPA,_TPB,_FPB,A,A):-I>N,!.

interpolate(I,N,Pos,R0,P0,TPA,FPA,TPB,FPB,A0,A):-
  R is (TPA+I)/Pos,
  P is (TPA+I)/(TPA+I+FPA+(FPB-FPA)/(TPB-TPA)*I),
  A1 is A0+(R-R0)*(P+P0)/2,
  I1 is I+1,
  interpolate(I1,N,Pos,R,P,TPA,FPA,TPB,FPB,A1,A).



update_head([],[],_N,[]):-!.

update_head([H:_P|T],[PU|TP],N,[H:P|T1]):-
  P is PU/N,
  update_head(T,TP,N,T1).


/* EM end */


/* utilities */
/**
 * rules2terms(:R:list_of_rules,-T:tern) is det
 *
 * The predicate translates a list of rules from the internal
 * representation format (rule/4 and def_rule/3) to the
 * LPAD syntax.
 */
rules2terms(R,T):-
  maplist(rule2term,R,T).

rule2term(rule(_N,HL,BL,_Lit,_Tun),(H:-B)):-
  list2or(HL,H),
  list2and(BL,B).

rule2term(def_rule(H,BL,_Lit),((H:1.0):-B)):-
  list2and(BL,B).


write_rules([],_S).

write_rules([rule(_N,HL,BL,Lit,_Tun)|T],S):-!,
  copy_term((HL,BL,Lit),(HL1,BL1,Lit1)),
  numbervars((HL1,BL1,Lit1),0,_M),
  write_disj_clause(S,(HL1:-BL1)),
  write_rules(T,S).

write_rules([def_rule(H,BL,Lit)|T],S):-
  copy_term((H,BL,Lit),(H1,BL1,Lit1)),
  numbervars((H1,BL1,Lit1),0,_M),
  write_disj_clause(S,([H1:1.0]:-BL1)),
  write_rules(T,S).


new_par([],[],[]).

new_par([HP|TP],[Head:_|TO],[Head:HP|TN]):-
  new_par(TP,TO,TN).



write_disj_clause(S,(H:-[])):-!,
  write_head(S,H),
  format(S,".~n~n",[]).

write_disj_clause(S,(H:-B)):-
  write_head(S,H),
  format(S,' :-',[]),
  nl(S),
  write_body(S,B).


write_head(S,[A:1.0|_Rest]):-!,
  format(S,"~q:1.0",[A]).

write_head(S,[A:P,'':_P]):-!,
  format(S,"~q:~g",[A,P]).

write_head(S,[A:P]):-!,
  format(S,"~q:~g",[A,P]).

write_head(S,[A:P|Rest]):-
  format(S,"~q:~g ; ",[A,P]),
  write_head(S,Rest).

write_body(S,[]):-!,
  format(S,"  true.~n~n",[]).

write_body(S,[A]):-!,
  format(S,"  ~q.~n~n",[A]).

write_body(S,[A|T]):-
  format(S,"  ~q,~n",[A]),
  write_body(S,T).

/**
 * list2or(+List:list,-Or:term) is det.
 * list2or(-List:list,+Or:term) is det.
 *
 * The predicate succeeds when Or is a disjunction (using the ; operator)
 * of the terms in List
 */
list2or([],true):-!.

list2or([X],X):-
    X\=;(_,_),!.

list2or([H|T],(H ; Ta)):-!,
    list2or(T,Ta).


/**
 * list2and(+List:list,-And:term) is det.
 * list2and(-List:list,+And:term) is det.
 *
 * The predicate succeeds when And is a conjunction (using the , operator)
 * of the terms in List
 */
list2and([],true):-!.

list2and([X],X):-
    X\=(_,_),!.

list2and([H|T],(H,Ta)):-!,
    list2and(T,Ta).


deduct(0,_Mod,_DB,Th,Th):-!.

deduct(NM,Mod,DB,InTheory0,InTheory):-
  get_head_atoms(O,Mod),
  sample(1,DB,Sampled,DB1),
  (Sampled=[M]->
    generate_head(O,M,Mod,[],HL),
    NM1 is NM-1,
    ( HL \== [] ->
       (generate_body(HL,Mod,InTheory1),
	append(InTheory0,InTheory1,InTheory2),
	deduct(NM1,Mod,DB1,InTheory2,InTheory)
       )
      ;
       deduct(NM1,Mod,DB1,InTheory0,InTheory)
    )
  ;
    InTheory=InTheory0
  ).


get_head_atoms(O,M):-
  findall(A,M:modeh(_,A),O0),
  findall((A,B,D),M:modeh(_,A,B,D),O1),
  append(O0,O1,O).

generate_top_cl([],_M,[]):-!.

generate_top_cl([A|T],M,[(rule(R,[A1:0.5,'':0.5],[],true,tunable),-1e20)|TR]):-
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  A1=..[F|Arg],
  get_next_rule_number(M,R),
  generate_top_cl(T,M,TR).


generate_head([],_M,_Mod,HL,HL):-!.

generate_head([(A,G,D)|T],M,Mod,H0,H1):-!,
  generate_head_goal(G,M,Goals),
  findall((A,Goals,D),(member(Goal,Goals),call(Mod:Goal),ground(Goals)),L),
  Mod:local_setting(initial_clauses_per_megaex,IC),   %IC: represents how many samples are extracted from the list L of example
  sample(IC,L,L1),
  append(H0,L1,H2),
  generate_head(T,M,Mod,H2,H1).

generate_head([A|T],M,Mod,H0,H1):-
  functor(A,F,N),
  functor(F1,F,N),
  F1=..[F|Arg],
  Pred1=..[F,M|Arg],
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  findall((A,Pred1),call(Mod:Pred1),L),
  Mod:local_setting(initial_clauses_per_megaex,IC),
  sample(IC,L,L1),
  append(H0,L1,H2),
  generate_head(T,M,Mod,H2,H1).

generate_head_goal([],_M,[]).

generate_head_goal([H|T],M,[H1|T1]):-
  H=..[F|Arg],
  H1=..[F,M|Arg],
  generate_head_goal(T,M,T1).

keep_const([],[]).

keep_const([- _|T],[_|T1]):-!,
  keep_const(T,T1).

keep_const([+ _|T],[_|T1]):-!,
  keep_const(T,T1).

keep_const([-# _|T],[_|T1]):-!,
  keep_const(T,T1).

keep_const([H|T],[H1|T1]):-
  H=..[F|Args],
  keep_const(Args,Args1),
  H1=..[F|Args1],
  keep_const(T,T1).


/**
 * sample(+N,List:list,-Sampled:list,-Rest:list) is det
 *
 * Samples N elements from List and returns them in Sampled.
 * The rest of List is returned in Rest
 * If List contains less than N elements, Sampled is List and Rest
 * is [].
*/
sample(0,List,[],List):-!.

sample(N,List,List,[]):-
  length(List,L),
  L=<N,!.

sample(N,List,[El|List1],Li):-
  length(List,L),
  random(0,L,Pos),
  nth0(Pos,List,El,Rest),
  N1 is N-1,
  sample(N1,Rest,List1,Li).

sample(0,_List,[]):-!.

sample(N,List,List):-
  length(List,L),
  L=<N,!.

sample(N,List,[El|List1]):-
  length(List,L),
  random(0,L,Pos),
  nth0(Pos,List,El,Rest),
  N1 is N-1,
  sample(N1,Rest,List1).

get_args([],[],[],A,A,AT,AT,_).

get_args([HM|TM],[H|TH],[(H,HM)|TP],A0,A,AT0,AT,M):-
  HM=..[F|ArgsTypes],
  H=..[F,M|Args],
  append(A0,Args,A1),
  append(AT0,ArgsTypes,AT1),
  get_args(TM,TH,TP,A1,A,AT1,AT,M).

/* Generation of the bottom clauses */

gen_head([],P,['':P]).

gen_head([H|T],P,[H:P|TH]):-
  gen_head(T,P,TH).

get_modeb([],_Mod,B,B).

get_modeb([F/AA|T],Mod,B0,B):-
  findall((R,B),(Mod:modeb(R,B),functor(B,F,AA)),BL),
  (Mod:local_setting(neg_literals,true)->
    findall((R,(\+ B)),(Mod:modeb(R,B),functor(B,F,AA),all_plus(B)),BNL)
  ;
    BNL=[]
  ),
  append([B0,BL,BNL],B1),
  get_modeb(T,Mod,B1,B).

all_plus(B):-
  B=..[_|Args],
  all_plus_args(Args).

all_plus_args([]).

all_plus_args([+ _ |T]):-!,
  all_plus_args(T).

all_plus_args([H|T]):-
  H \= - _,
  H \= # _,
  H \= -# _,
  H=..[_|Args],
  all_plus_args(Args),
  all_plus_args(T).
/**
 * generate_body(+ModeDecs:list,+Module:atom,-BottomClauses:list) is det
 *
 * Generates the body of bottom clauses and returns the bottom clauses in BottomClauses.
 *
 */
generate_body([],_Mod,[]):-!.

generate_body([(A,H,Det)|T],Mod,[(rule(R,HP,[],BodyList,tunable),-1e20)|CL0]):-!,
  get_modeb(Det,Mod,[],BL),
  get_args(A,H,Pairs,[],Args,[],ArgsTypes,M),
  Mod:local_setting(d,D),
  cycle_modeb(ArgsTypes,Args,[],[],Mod,BL,a,[],BLout0,D,M),
  variabilize((Pairs:-BLout0),CLV),  %+(Head):-Bodylist;  -CLV:(Head):-Bodylist with variables _num in place of constants
  CLV=(Head1:-BodyList1),
  remove_int_atom_list(Head1,Head),
  remove_int_atom_list(BodyList1,BodyList2),
  remove_duplicates(BodyList2,BodyList),
  get_next_rule_number(Mod,R),
  length(Head,LH),
  Prob is 1.0/(LH+1),
  gen_head(Head,Prob,HP),
  copy_term((HP,BodyList),(HeadV,BodyListV)),
  numbervars((HeadV,BodyListV),0,_V),
  format2(Mod,"Bottom clause: example ~q~nClause~n",[H]),
  write_disj_clause2(Mod,user_output,(HeadV:-BodyListV)),
  generate_body(T,Mod,CL0).

generate_body([(A,H)|T],Mod,[(rule(R,[Head:0.5,'':0.5],[],BodyList,tunable),-1e20)|CL0]):-
  functor(A,F,AA),
  findall(FB/AB,Mod:determination(F/AA,FB/AB),Det),
  get_modeb(Det,Mod,[],BL),
  A=..[F|ArgsTypes],
  H=..[F,M|Args],
  Mod:local_setting(d,D),
  cycle_modeb(ArgsTypes,Args,[],[],Mod,BL,a,[],BLout0,D,M),
  variabilize(([(H,A)]:-BLout0),CLV),  %+(Head):-Bodylist;  -CLV:(Head):-Bodylist with variables _num in place of constants
  CLV=([Head1]:-BodyList1),
  remove_int_atom(Head1,Head),
  remove_int_atom_list(BodyList1,BodyList2),
  remove_duplicates(BodyList2,BodyList),
  get_next_rule_number(Mod,R),
  copy_term((Head,BodyList),(HeadV,BodyListV)),
  numbervars((HeadV,BodyListV),0,_V),
  format2(Mod,"Bottom clause: example ~q~nClause~n~q:0.5 :-~n",[H,HeadV]),
  write_body2(Mod,user_output,BodyListV),
  generate_body(T,Mod,CL0).


variabilize((H:-B),(H1:-B1)):-
  variabilize_list(H,H1,[],AS,M),
  variabilize_list(B,B1,AS,_AS,M).


variabilize_list([],[],A,A,_M).

variabilize_list([(\+ H,Mode)|T],[\+ H1|T1],A0,A,M):-
  builtin(H),!,
  H=..[F|Args],
  Mode=..[F|ArgTypes],
  variabilize_args(Args,ArgTypes, Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).

variabilize_list([(\+ H,Mode)|T],[\+ H1|T1],A0,A,M):-!,
  H=..[F,_M|Args],
  Mode=..[F|ArgTypes],
  variabilize_args(Args,ArgTypes, Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).

variabilize_list([(H,Mode)|T],[H1|T1],A0,A,M):-
  builtin(H),!,
  H=..[F|Args],
  Mode=..[F|ArgTypes],
  variabilize_args(Args,ArgTypes, Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).

variabilize_list([(H,Mode)|T],[H1|T1],A0,A,M):-
  H=..[F,_M|Args],
  Mode=..[F|ArgTypes],
  variabilize_args(Args,ArgTypes, Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).


variabilize_args([],[],[],A,A).

variabilize_args([C|T],[C|TT],[C|TV],A0,A):-!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[# _Ty|TT],[C|TV],A0,A):-!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[-# _Ty|TT],[C|TV],A0,A):-!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[Ty|TT],[V|TV],A0,A):-
  (Ty = +Ty1;Ty = -Ty1),
  member(C/Ty1/V,A0),!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[Ty|TT],[V|TV],A0,A):-
  (Ty = +Ty1;Ty = -Ty1),!,
  variabilize_args(T,TT,TV,[C/Ty1/V|A0],A).

variabilize_args([C|T],[Ty|TT],[V|TV],A0,A):-
  compound(C),
  C=..[F|Args],
  Ty=..[F|ArgsT],
  variabilize_args(Args,ArgsT,ArgsV,A0,A1),
  V=..[F|ArgsV],
  variabilize_args(T,TT,TV,A1,A).


cycle_modeb(ArgsTypes,Args,ArgsTypes,Args,_Mod,_BL,L,L,L,_,_M):-!.

cycle_modeb(_ArgsTypes,_Args,_ArgsTypes1,_Args1,_Mod,_BL,_L,L,L,0,_M):-!.

cycle_modeb(ArgsTypes,Args,_ArgsTypes0,_Args0,Mod,BL,_L0,L1,L,D,M):-
  find_atoms(BL,Mod,ArgsTypes,Args,ArgsTypes1,Args1,L1,L2,M),
  D1 is D-1,
  cycle_modeb(ArgsTypes1,Args1,ArgsTypes,Args,Mod,BL,L1,L2,L,D1,M).


find_atoms([],_Mod,ArgsTypes,Args,ArgsTypes,Args,L,L,_M).

find_atoms([(R,\+ H)|T],Mod,ArgsTypes0,Args0,ArgsTypes,Args,L0,L1,M):-!,
  H=..[F|ArgsT],
  findall((A,H),instantiate_query_neg(ArgsT,ArgsTypes0,Args0,F,M,A),L),
  call_atoms(L,Mod,[],At),
  remove_duplicates(At,At1),
  ((R = '*' ) ->
    R1= +1e20
  ;
    R1=R
  ),
  sample(R1,At1,At2),
  append(L0,At2,L2),
  find_atoms(T,Mod,ArgsTypes0,Args0,ArgsTypes,Args,L2,L1,M).

find_atoms([(R,H)|T],Mod,ArgsTypes0,Args0,ArgsTypes,Args,L0,L1,M):-
  H=..[F|ArgsT],
  findall((A,H),instantiate_query(ArgsT,ArgsTypes0,Args0,F,M,A),L),
  call_atoms(L,Mod,[],At),
  remove_duplicates(At,At1),
  ((R = '*' ) ->
    R1= +1e20
  ;
    R1=R
  ),
  sample(R1,At1,At2),
  extract_output_args(At2,ArgsT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  append(L0,At2,L2),
  find_atoms(T,Mod,ArgsTypes1,Args1,ArgsTypes,Args,L2,L1,M).


call_atoms([],_Mod,A,A).

call_atoms([(H,M)|T],Mod,A0,A):-
  findall((H,M),Mod:H,L),
  append(A0,L,A1),
  call_atoms(T,Mod,A1,A).


extract_output_args([],_ArgsT,ArgsTypes,Args,ArgsTypes,Args).

extract_output_args([(H,_At)|T],ArgsT,ArgsTypes0,Args0,ArgsTypes,Args):-
  builtin(H),!,
  H=..[_F|ArgsH],
  add_const(ArgsH,ArgsT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  extract_output_args(T,ArgsT,ArgsTypes1,Args1,ArgsTypes,Args).

extract_output_args([(H,_At)|T],ArgsT,ArgsTypes0,Args0,ArgsTypes,Args):-
  H=..[_F,_M|ArgsH],
  add_const(ArgsH,ArgsT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  extract_output_args(T,ArgsT,ArgsTypes1,Args1,ArgsTypes,Args).


add_const([],[],ArgsTypes,Args,ArgsTypes,Args).

add_const([_A|T],[+_T|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  add_const(T,TT,ArgsTypes0,Args0,ArgsTypes,Args).

add_const([A|T],[-Type|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  (already_present(ArgsTypes0,Args0,A,Type)->
    ArgsTypes1=ArgsTypes0,
    Args1=Args0
  ;
    ArgsTypes1=[+Type|ArgsTypes0],
    Args1=[A|Args0]
  ),
  add_const(T,TT,ArgsTypes1,Args1,ArgsTypes,Args).

add_const([A|T],[-# Type|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  (already_present(ArgsTypes0,Args0,A,Type)->
    ArgsTypes1=ArgsTypes0,
    Args1=Args0
  ;
    ArgsTypes1=[+Type|ArgsTypes0],
    Args1=[A|Args0]
  ),
  add_const(T,TT,ArgsTypes1,Args1,ArgsTypes,Args).

add_const([_A|T],[# _|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  add_const(T,TT,ArgsTypes0,Args0,ArgsTypes,Args).

add_const([A|T],[A|TT],ArgsTypes0,Args0,ArgsTypes,Args):-
  atomic(A),!,
  add_const(T,TT,ArgsTypes0,Args0,ArgsTypes,Args).

add_const([A|T],[AT|TT],ArgsTypes0,Args0,ArgsTypes,Args):-
  A=..[F|Ar],
  AT=..[F|ArT],
  add_const(Ar,ArT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  add_const(T,TT,ArgsTypes1,Args1,ArgsTypes,Args).


already_present([+T|_TT],[C|_TC],C,T):-!.

already_present([_|TT],[_|TC],C,T):-
  already_present(TT,TC,C,T).


instantiate_query_neg(ArgsT,ArgsTypes,Args,F,M,A):-
  instantiate_input(ArgsT,ArgsTypes,Args,ArgsB),
  A1=..[F|ArgsB],
  (builtin(A1)->
    A= (\+ A1)
  ;
    A0=..[F,M|ArgsB],
    A = (\+ A0)
  ).

instantiate_query(ArgsT,ArgsTypes,Args,F,M,A):-
  instantiate_input(ArgsT,ArgsTypes,Args,ArgsB),
  A1=..[F|ArgsB],
  (builtin(A1)->
    A=A1
  ;
    A=..[F,M|ArgsB]
  ).


instantiate_input([],_AT,_A,[]).

instantiate_input([-_Type|T],AT,A,[_V|TA]):-!,
  instantiate_input(T,AT,A,TA).

instantiate_input([+Type|T],AT,A,[H|TA]):-!,
  find_val(AT,A,+Type,H),
  instantiate_input(T,AT,A,TA).

instantiate_input([# Type|T],AT,A,[H|TA]):-!,
  find_val(AT,A,+Type,H),
  instantiate_input(T,AT,A,TA).

instantiate_input([-# _Type|T],AT,A,[_V|TA]):-!,
  instantiate_input(T,AT,A,TA).

instantiate_input([C|T],AT,A,[C1|TA]):-
  C=..[F|Args],
  instantiate_input(Args,AT,A,Args1),
  C1=..[F|Args1],
  instantiate_input(T,AT,A,TA).


find_val([T|_TT],[A|_TA],T,A).

find_val([HT|_TT],[HA|_TA],T,A):-
  nonvar(HA),
  HT=..[F|ArgsT],
  HA=..[F|Args],
  find_val(ArgsT,Args,T,A).

find_val([_T|TT],[_A|TA],T,A):-
  find_val(TT,TA,T,A).


get_output_atoms(O,M):-
  findall((A/Ar),M:output((A/Ar)),O).

generate_goal(LP,M,H,[],LG):-
  M:local_setting(neg_ex,given),!,
  find_ex_pred(LP,M,[H],[],LG,0,_Pos,0,_Neg).

generate_goal(LP,M,H,[],LG):-
  M:local_setting(neg_ex,cw),
  (M:modeh(_,_)->
    true
  ;
    throw(missing_mode_declarations)
  ),
  find_ex_pred_cw(LP,M,[H],[],LG,0,_Pos,0,_Neg).


/**
 * remove_duplicates(+List1:list,-List2:list) is det
 *
 * Removes duplicates from List1. Equality is checked with ==.
 */
remove_duplicates(L0,L):-
  remove_duplicates(L0,[],L1),
  reverse(L1,L).

remove_duplicates([],L,L).

remove_duplicates([H|T],L0,L):-
  member_eq(H,L0),!,
  remove_duplicates(T,L0,L).

remove_duplicates([H|T],L0,L):-
  remove_duplicates(T,[H|L0],L).


/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzi, Nicola di Mauro and Elena Bellodi

*/


specialize_rule(Rule,M,_SpecRule,_Lit):-
  M:local_setting(max_body_length,ML),
  Rule = rule(_ID,_LH,BL,_Lits,_Tun),
  length(BL,L),
  L=ML,!,
  fail.

%used by cycle_clauses in slipcover.pl
specialize_rule(Rule,M,SpecRule,Lit):-
  M:local_setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits,Tun),
  delete_one(Lits,RLits,Lit),
  \+ M:lookahead_cons(Lit,_),
  \+ M:lookahead_cons_var(Lit,_),
  \+ member_eq(Lit,BL),
  append(BL,[Lit],BL1),
  remove_prob(LH,LH1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  dv(LH2,BL1,M,DList),	%-DList: list of couples (variable,depth)
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,M,LH2),
  M:local_setting(maxdepth_var,MD),
  exceed_depth(DList,MD),
  \+ banned_clause(M,LH2,BL1),
  SpecRule=rule(ID,LH,BL1,RLits,Tun).

specialize_rule(Rule,M,SpecRule,Lit):-
  M:local_setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits,Tun),
  delete_one(Lits,RLits,Lit),
  \+ member_eq(Lit,BL),
  append(BL,[Lit],BL0),
  \+M:lookahead_cons_var(Lit,_),
  (M:lookahead(Lit,LLit1);M:lookahead_cons(Lit,LLit1)),
  copy_term(LLit1,LLit2),
  specialize_rule_la_bot(LLit2,RLits,RLits1,BL0,BL1),
  remove_prob(LH,LH1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  dv(LH2,BL1,M,DList),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,M,LH2),
  M:local_setting(maxdepth_var,MD),
  exceed_depth(DList,MD),
  \+ banned_clause(M,LH2,BL1),
  SpecRule=rule(ID,LH,BL1,RLits1,Tun).

specialize_rule(Rule,M,SpecRule,Lit):-
  M:local_setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits,Tun),
  delete_one(Lits,RLits,Lit),
  \+ member_eq(Lit,BL),
  append(BL,[Lit],BL0),
  M:lookahead_cons_var(Lit,LLit2),
  specialize_rule_la_bot(LLit2,RLits,_RLits1,BL0,BL1),
  remove_prob(LH,LH1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  dv(LH2,BL1,M,DList),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,M,LH2),
  M:local_setting(maxdepth_var,MD),
  exceed_depth(DList,MD),
  \+ banned_clause(M,LH2,BL1),
  SpecRule=rule(ID,LH,BL1,[],Tun).

specialize_rule(Rule,M,SpecRule,Lit):-
  M:local_setting(specialization,mode),%!,
  findall(BL , M:modeb(_,BL), BLS),
  specialize_rule(BLS,Rule,M,SpecRule,Lit).

%specializes the clause head
specialize_rule(rule(ID,LH,BL,Lits,Tun),M,rule(ID,LH2,BL,Lits,Tun),Lit):-
  M:local_setting(specialize_head,true),
	length(LH,L),
	L>2,
	delete_one(LH,LH1,Lit),  %deletes Lit
	Lit\='',
	update_head1(LH1,L-1,LH2).  %updates parameters

update_head1([],_N,[]):-!.

update_head1([H:_P|T],N,[H:P|T1]):-
	       P is 1/N,
	       update_head1(T,N,T1).

/**
 * banned_clause(+Module:atom,-Head:term,-Body:term) is nondet
 *
 * The predicate checks whether Head:-Body is a banned clause, as specified
 * by the user in the input file. Module is the module of the input file.
 */
banned_clause(M,H,B):-
  numbervars((H,B),0,_N),
  M:banned(H2,B2),
  mysublist(H2,H),
  mysublist(B2,B).


mysublist([],_).

mysublist([H|T],L):-
  member(H,L),
  mysublist(T,L).


specialize_rule([Lit|_RLit],Rule,M,SpecRul,SLit):-
  Rule = rule(ID,LH,BL,true,Tun),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,M,ALL,SLit),
  append(BL,[SLit],BL1),
  (M:lookahead(SLit,LLit1);M:lookahead_cons(SLit,LLit1)),
  specialize_rule_la(LLit1,M,LH1,BL1,BL2),
  append(LH1,BL2,ALL2),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  SpecRul = rule(ID,LH,BL2,true,Tun).

specialize_rule([Lit|_RLit],Rule,M,SpecRul,SLit):-
  Rule = rule(ID,LH,BL,true,Tun),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,M,ALL,SLit),
  \+ M:lookahead_cons(SLit,_),
  append(BL,[SLit],BL1),
  append(LH1,BL1,ALL1),
  extract_fancy_vars(ALL1,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  SpecRul = rule(ID,LH,BL1,true,Tun).

specialize_rule([_|RLit],Rule,M,SpecRul,Lit):-
  specialize_rule(RLit,Rule,M,SpecRul,Lit).


specialize_rule_la([],_M,_LH1,BL1,BL1).

specialize_rule_la([Lit1|T],M,LH1,BL1,BL3):-
  copy_term(Lit1,Lit2),
  M:modeb(_,Lit2),
  append(LH1,BL1,ALL1),
  specialize_rule1(Lit2,M,ALL1,SLit1),
  append(BL1,[SLit1],BL2),
  specialize_rule_la(T,M,LH1,BL2,BL3).


specialize_rule_la_bot([],Bot,Bot,BL,BL).

specialize_rule_la_bot([Lit|T],Bot0,Bot,BL1,BL3):-
  delete_one(Bot0,Bot1,Lit),
  \+ member_eq(Lit,BL1),
  append(BL1,[Lit],BL2),
  specialize_rule_la_bot(T,Bot1,Bot,BL2,BL3).


remove_prob(['':_P],[]):-!.

remove_prob([X:_|R],[X|R1]):-
  remove_prob(R,R1).


specialize_rule1(Lit,M,Lits,SpecLit):-
  Lit =.. [Pred|Args],
  extract_type_vars(Lits,M,TypeVars0),
  remove_duplicates(TypeVars0,TypeVars),
  take_var_args(Args,TypeVars,Args1),
  SpecLit =.. [Pred|Args1],
  \+ member_eq(SpecLit,Lits).


convert_to_input_vars([],[]):-!.

convert_to_input_vars([+T|RT],[+T|RT1]):-
  !,
  convert_to_input_vars(RT,RT1).

convert_to_input_vars([-T|RT],[+T|RT1]):-
  convert_to_input_vars(RT,RT1).



remove_eq(X,[Y|R],R):-
  X == Y,
  !.

remove_eq(X,[_|R],R1):-
  remove_eq(X,R,R1).


/**
 * linked_clause(+Literals:list,+Module:atom,+PrevLits:list) is det
 *
 * The predicate checks whether Literals form a linked list of literals
 * given that PrevLits are the previous literals.
 * In a linked list of literals input variables of a literal are output variables in
 * a previous literal.
 */
linked_clause([],_M,_).

linked_clause([L|R],M,PrevLits):-
  term_variables(PrevLits,PrevVars),
  input_variables(L,M,InputVars),
  linked(InputVars,PrevVars),!,
  linked_clause(R,M,[L|PrevLits]).


linked([],_).

linked([X|R],L) :-
  member_eq(X,L),
  !,
  linked(R,L).


input_variables(\+ LitM,M,InputVars):-
  !,
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  M:modeb(_,Lit1),
  Lit1 =.. [P|Args1],
  convert_to_input_vars(Args1,Args2),
  Lit2 =.. [P|Args2],
  input_vars(LitM,Lit2,InputVars).

input_variables(LitM,M,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  M:modeb(_,Lit1),
  input_vars(LitM,Lit1,InputVars).

input_variables(LitM,M,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  M:modeh(_,Lit1),
  input_vars(LitM,Lit1,InputVars).

input_vars(Lit,Lit1,InputVars):-
  Lit =.. [_|Vars],
  Lit1 =.. [_|Types],
  input_vars1(Vars,Types,InputVars).


input_vars1([],_,[]).

input_vars1([V|RV],[+_T|RT],[V|RV1]):-
  !,
  input_vars1(RV,RT,RV1).

input_vars1([_V|RV],[_|RT],RV1):-
  input_vars1(RV,RT,RV1).

/**
 * extract_type_vars(+Literals:list,+Module:atom,+Types:term) is det
 *
 * The predicate extracts the type of variables from the list of literals
 * Literals. Types is a list of elements of the form Variable=Type
 */
extract_type_vars([],_M,[]).

extract_type_vars([Lit|RestLit],M,TypeVars):-
  Lit =.. [Pred|Args],
  length(Args,L),
  length(Args1,L),
  Lit1 =.. [Pred|Args1],
  take_mode(M,Lit1),
  type_vars(Args,Args1,Types),
  extract_type_vars(RestLit,M,TypeVars0),
  !,
  append(Types,TypeVars0,TypeVars).


take_mode(M,Lit):-
  M:modeh(_,Lit),!.

take_mode(M,Lit):-
  M:modeb(_,Lit),!.

take_mode(M,Lit):-
  M:mode(_,Lit),!.


type_vars([],[],[]).

type_vars([V|RV],[+T|RT],[V=T|RTV]):-
  !,
  type_vars(RV,RT,RTV).

type_vars([V|RV],[-T|RT],[V=T|RTV]):-atom(T),!,
  type_vars(RV,RT,RTV).

type_vars([_V|RV],[_T|RT],RTV):-
  type_vars(RV,RT,RTV).

/**
 * take_var_args(+ArgSpec:list,+TypeVars:list,-Vars:list) is det
 *
 * The predicate returns in Vars the list of vars corresponding to
 * variables arguments in ArgSpec (those with argument specification
 * +type or -type). TypeVars is a list of terns of the form
 * Variable=Types as returnd by extract_type_vars/3.
 */
take_var_args([],_,[]).

take_var_args([+T|RT],TypeVars,[V|RV]):-
  !,
  member(V=T,TypeVars),
  take_var_args(RT,TypeVars,RV).

take_var_args([-T|RT],TypeVars,[_V|RV]):-
  atom(T),
  take_var_args(RT,TypeVars,RV).

take_var_args([-T|RT],TypeVars,[V|RV]):-
  member(V=T,TypeVars),
  take_var_args(RT,TypeVars,RV).

take_var_args([T|RT],TypeVars,[T|RV]):-
  T\= + _,(T\= - _; T= - A,number(A)),
  take_var_args(RT,TypeVars,RV).



add_probs([],['':P],P):-!.

add_probs([H|T],[H:P|T1],P):-
  add_probs(T,T1,P).

/**
 * extract_fancy_vars(+Term:term,-Vars:list) is nondet
 *
 * Given Term, it returns the list of all of its variables
 * in the form 'VN'=Var where VN is an atom with N an increasing integer
 * starting from 1 and Var a variable in Term.
 */
extract_fancy_vars(List,Vars):-
  term_variables(List,Vars0),
  fancy_vars(Vars0,1,Vars).


fancy_vars([],_,[]).

fancy_vars([X|R],N,[NN2=X|R1]):-
  name(N,NN),
  append([86],NN,NN1),
  name(NN2,NN1),
  N1 is N + 1,
  fancy_vars(R,N1,R1).


/**
 * delete_one(+List:list,-Rest:list,+Element:term) is nondet
 *
 * As the library predicate delete(+List1, @Elem, -List2) but
 * Element is unified with the deleted element (so it can be
 * instantiated by the call).
 */
delete_one([X|R],R,X).

delete_one([X|R],[X|R1],D):-
  delete_one(R,R1,D).




%Computation of the depth of the variables in the clause head/body
dv(H,B,M,DV1):-			%DV1: returns a list of couples (Variable, Max depth)
	term_variables(H,V),
	head_depth(V,DV0),
	findall((MD-DV),var_depth(B,M,DV0,DV,0,MD),LDs),
        get_max(LDs,-1,-,DV1).


input_variables_b(\+ LitM,M,InputVars):-!,
	  LitM=..[P|Args],
	  length(Args,LA),
	  length(Args1,LA),
	  Lit1=..[P|Args1],
	  M:modeb(_,Lit1),
	  all_plus(Lit1),
	  input_vars(LitM,Lit1,InputVars).

input_variables_b(LitM,M,InputVars):-
	  LitM=..[P|Args],
	  length(Args,LA),
	  length(Args1,LA),
	  Lit1=..[P|Args1],
	  M:modeb(_,Lit1),
	  input_vars(LitM,Lit1,InputVars).



%associates depth 0 to each variable in the clause head
head_depth([],[]).
head_depth([V|R],[[V,0]|R1]):-
  head_depth(R,R1).

%associates a depth to each variable in the clause body
var_depth([],_M,PrevDs1,PrevDs1,MD,MD):-!.

var_depth([L|R],M,PrevDs,PrevDs1,_MD,MD):-		%L = a body literal, MD = maximum depth set by the user
  input_variables_b(L,M,InputVars),
  term_variables(L, BodyAtomVars),
  output_vars(BodyAtomVars,InputVars,OutputVars),
  depth_InputVars(InputVars,PrevDs,0,MaxD),		%MaxD: maximum depth of the input variables in the body literal
  D is MaxD+1,
  compute_depth(OutputVars,D,PrevDs,PrevDs0),		%Computes the depth for the output variables in the body literal
  var_depth(R,M,PrevDs0,PrevDs1,D,MD).

get_max([],_,Ds,Ds).

get_max([(MD-DsH)|T],MD0,_Ds0,Ds):-
  MD>MD0,!,
  get_max(T,MD,DsH,Ds).

get_max([_H|T],MD,Ds0,Ds):-
	get_max(T,MD,Ds0,Ds).

delete_eq([],_E,[]).

delete_eq([H|T],E,T1):-
  H==E,!,
  delete_eq(T,E,T1).

delete_eq([H|T],E,[H|T1]):-
  delete_eq(T,E,T1).

output_vars(OutVars,[],OutVars):-!.
output_vars(BodyAtomVars,[I|InputVars],OutVars):-
  delete_eq(BodyAtomVars, I, Residue),
  output_vars(Residue,InputVars, OutVars).

% returns D as the maximum depth of the variables in the list (first argument)
depth_InputVars([],_,D,D).
depth_InputVars([I|Input],PrevDs,D0,D):-
	 member_l(PrevDs,I,MD),
	 (MD>D0->
		D1=MD
	;
		D1=D0
         ),
	 depth_InputVars(Input,PrevDs,D1,D).

member_l([[L,D]|_P],I,D):-
     I==L,!.
member_l([_|P],I,D):-
     member_l(P,I,D).

compute_depth([],_,PD,PD):-!.
compute_depth([O|Output],D,PD,RestO):-
	member_l(PD,O,_),!,
	compute_depth(Output,D,PD,RestO).

compute_depth([O|Output],D,PD,[[O,D]|RestO]):-
	compute_depth(Output,D,PD,RestO).



%checks if a variable depth exceeds the setting_sc
exceed_depth([],_):-!.
exceed_depth([H|T],MD):-
	nth1(2,H,Dep),
	Dep<MD, %setting_sc(maxdepth_var,MD),
	exceed_depth(T,MD).

/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzi and Elena Bellodi

*/

/**
 * assert_all(+Terms:list,+Module:atom,-Refs:list) is det
 *
 * The predicate asserts all terms in Terms in module Module using assertz(M:Term,Ref) and
 * returns the list of references in Refs
 */
assert_all([],_M,[]).

assert_all([H|T],M,[HRef|TRef]):-
  assertz(M:H,HRef),
  assert_all(T,M,TRef).

assert_all([],[]).

assert_all([H|T],[HRef|TRef]):-
  assertz(slipcover:H,HRef),
  assert_all(T,TRef).

/**
 * retract_all(+Refs:list) is det
 *
 * The predicate erases all references in Refs (using erase/1).
 */
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

/**
 * process_clauses(+InputClauses:list,+Module:atom,+Rules:list,-RulesOut:list,+Clauses:list,-ClausesOut:list) is det
 *
 * InputClauses is a list of probabilistic clauses in input syntax.
 * The predicate translates them into the internal format.
 * RulesOut/Rules is a difference list of term of the form rule(R,HeadList,BodyList,Lit,Tun).
 * ClausesOut/Clauses is a difference list of clauses to be asserted.
 */
process_clauses([],_M,C,C,R,R):-!.

process_clauses([end_of_file],_M,C,C,R,R):-!.

process_clauses([H|T],M,C0,C1,R0,R1):-
  (term_expansion_int(H,M,H1)->
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
  process_clauses(T,M,C2,C1,R2,R1).



/**
 * get_next_rule_number(+Module:atom,-R:integer) is det
 *
 * The predicate returns the next rule number. Module is used to access local
 * data.
 */
get_next_rule_number(M,R):-
  retract(M:rule_sc_n(R)),
  R1 is R+1,
  assert(M:rule_sc_n(R1)).

reset_next_rule_number(M):-
  retract(M:rule_sc_n(_)),
  assert(M:rule_sc_n(0)).

/**
 * get_next_nonground_rule_number(+Module:atom,-R:integer) is det
 *
 * The predicate returns the next nonground rule number used when a different
 * parameter for each rule grounding must be learned
 * Module is used to access local
 * data.
 */
get_next_nonground_rule_number(M,R):-
  retract(M:rule_ng_sc_n(R)),
  R1 is R+1,
  assert(M:rule_ng_sc_n(R1)).

reset_next_nonground_rule_number(M):-
  retract(M:rule_ng_sc_n(_)),
  assert(M:rule_ng_sc_n(0)).

get_node(\+ Goal,M,Env,BDD):-
  M:local_setting(depth_bound,true),!,
  M:local_setting(depth,DB),
  retractall(M:v(_,_,_)),
  abolish_all_tables,
  add_bdd_arg_db(Goal,Env,B,DB,Goal1),
  (M:Goal1->
    bdd_notc(Env,B,(_,BDD))
  ;
    onec(Env,(_,BDD))
  ).

get_node(\+ Goal,M,Env,BDD):-!,
  retractall(M:v(_,_,_)),
  abolish_all_tables,
  add_bdd_arg(Goal,Env,B,Goal1),
  (M:Goal1->
    bdd_notc(Env,B,(_,BDD))
  ;
    onec(Env,(_,BDD))
  ).

get_node(Goal,M,Env,BDD):-
  M:local_setting(depth_bound,true),!,
  M:local_setting(depth,DB),
  retractall(M:v(_,_,_)),
  abolish_all_tables,
  add_bdd_arg_db(Goal,Env,B,DB,Goal1),%DB=depth bound
  (M:Goal1->
    (_,BDD)=B
  ;
    zeroc(Env,(_,BDD))
  ).

get_node(Goal,M,Env,BDD):- %with DB=false
  retractall(M:v(_,_,_)),
  add_bdd_arg(Goal,Env,B,Goal1),
  abolish_all_tables,
%  trace,
  (M:Goal1->
    (_,BDD)=B
  ;
    zeroc(Env,(_,BDD))
  ).

add_int_arg(_I,A,A):-
  builtin(A),!.

add_int_arg(I,A,A1):-
  A=..[P|Args],
  A1=..[P,I|Args].

add_bdd_arg(A,Env,BDD,A1):-
  A=..[P|Args],
  append(Args,[Env,BDD],Args1),
  A1=..[P|Args1].


add_bdd_arg_db(A,Env,BDD,DB,A1):-
  A=..[P|Args],
  append(Args,[DB,Env,BDD],Args1),
  A1=..[P|Args1].


add_bdd_arg(A,Env,BDD,Module,A1):-
  A=..[P|Args],
  append(Args,[Env,BDD],Args1),
  A1=..[P,Module|Args1].


add_bdd_arg_db(A,Env,BDD,DB,Module,A1):-
  A=..[P|Args],
  append(Args,[DB,Env,BDD],Args1),
  A1=..[P,Module|Args1].

add_mod_arg(A,Module,A1):-
  A=..[P|Args],
  A1=..[P,Module|Args].


generate_rules_fact([],_Env,_VC,_R,_Probs,_N,[],_Module,_M).

generate_rules_fact([Head:_P1,'':_P2],Env,VC,R,Probs,N,[Clause],Module,M):-!,
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(slipcover:get_sc_var_n(M,Env,R,VC,Probs,V),slipcover:equalityc(Env,V,N,BDD))).

generate_rules_fact([Head:_P|T],Env,VC,R,Probs,N,[Clause|Clauses],Module,M):-
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(slipcover:get_sc_var_n(M,Env,R,VC,Probs,V),slipcover:equalityc(Env,V,N,BDD))),
  N1 is N+1,
  generate_rules_fact(T,Env,VC,R,Probs,N1,Clauses,Module,M).


generate_rules_fact_db([],_Env,_VC,_R,_Probs,_N,[],_Module,_M).

generate_rules_fact_db([Head:_P1,'':_P2],Env,VC,R,Probs,N,[Clause],Module,M):-!,
  add_bdd_arg_db(Head,Env,BDD,_DB,Module,Head1),
  Clause=(Head1:-(slipcover:get_sc_var_n(M,Env,R,VC,Probs,V),slipcover:equalityc(Env,V,N,BDD))).

generate_rules_fact_db([Head:_P|T],Env,VC,R,Probs,N,[Clause|Clauses],Module,M):-
  add_bdd_arg_db(Head,Env,BDD,_DB,Module,Head1),
  Clause=(Head1:-(slipcover:get_sc_var_n(M,Env,R,VC,Probs,V),slipcover:equalityc(Env,V,N,BDD))),
  N1 is N+1,
  generate_rules_fact_db(T,Env,VC,R,Probs,N1,Clauses,Module,M).


generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module,M):-
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(Body,slipcover:get_sc_var_n(M,Env,R,VC,Probs,V),slipcover:equalityc(Env,V,N,B),slipcover:andc(Env,BDDAnd,B,BDD))).


generate_clause_db(Head,Env,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module,M):-
  add_bdd_arg_db(Head,Env,BDD,DBH,Module,Head1),
  Clause=(Head1:-(DBH>=1,DB is DBH-1,Body,slipcover:get_sc_var_n(M,Env,R,VC,Probs,V),slipcover:equalityc(Env,V,N,B),slipcover:andc(Env,BDDAnd,B,BDD))).


generate_rules([],_Env,_Body,_VC,_R,_Probs,_BDDAnd,_N,[],_Module,_M).

generate_rules([Head:_P1,'':_P2],Env,Body,VC,R,Probs,BDDAnd,N,[Clause],Module,M):-!,
  generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module,M).

generate_rules([Head:_P|T],Env,Body,VC,R,Probs,BDDAnd,N,[Clause|Clauses],Module,M):-
  generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module,M),
  N1 is N+1,
  generate_rules(T,Env,Body,VC,R,Probs,BDDAnd,N1,Clauses,Module,M).


generate_rules_db([],_Env,_Body,_VC,_R,_Probs,_DB,_BDDAnd,_N,[],_Module,_M):-!.

generate_rules_db([Head:_P1,'':_P2],Env,Body,VC,R,Probs,DB,BDDAnd,N,[Clause],Module,M):-!,
  generate_clause_db(Head,Env,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module,M).

generate_rules_db([Head:_P|T],Env,Body,VC,R,Probs,DB,BDDAnd,N,[Clause|Clauses],Module,M):-
  generate_clause_db(Head,Env,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module,M),!,%agg.cut
  N1 is N+1,
  generate_rules_db(T,Env,Body,VC,R,Probs,DB,BDDAnd,N1,Clauses,Module,M).

process_body_bg([],[],_Module).

process_body_bg([\+ H|T],[\+ H|Rest],Module):-
  builtin(H),!,
  process_body_bg(T,Rest,Module).

process_body_bg([\+ H|T],[\+ H1|Rest],Module):-!,
  add_mod_arg(H,Module,H1),
  process_body_bg(T,Rest,Module).

process_body_bg([H|T],[H|Rest],Module):-
  builtin(H),!,
  process_body_bg(T,Rest,Module).

process_body_bg([H|T],[H1|Rest],Module):-!,
  add_mod_arg(H,Module,H1),
  process_body_bg(T,Rest,Module).



process_body([],BDD,BDD,Vars,Vars,[],_Env,_Module,_M).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Env,Module,M):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module,M).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Env,Module,M):-
  db(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module,M).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[
(((neg(H1);\+ H1),slipcover:onec(Env,BDDN));
  (H2,slipcover:bdd_notc(Env,BDDH,BDDN))),
  slipcover:andc(Env,BDD,BDDN,BDD2)
  |Rest],Env,Module,M):-
  given(M,H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg(H,Env,BDDH,Module,H2),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module,M).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[
  \+(H1)|Rest],Env,Module,M):-
  given_cw(M,H),!,
  add_mod_arg(H,Module,H1),
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module,M).

process_body([\+ H|T],BDD,BDD1,Vars,[BDDH,BDDN,BDD2|Vars1],
[H1,slipcover:bdd_notc(Env,BDDH,BDDN),
  slipcover:andc(Env,BDD,BDDN,BDD2)|Rest],Env,Module,M):-!,
  add_bdd_arg(H,Env,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module,M).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Env,Module,M):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module,M).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Env,Module,M):-
  db(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module,M).

process_body([H|T],BDD,BDD1,Vars,Vars1,
[((H1,slipcover:onec(Env,BDDH));H2),slipcover:andc(Env,BDD,BDDH,BDD2)|Rest],Env,Module,M):-
  given(M,H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg(H,Env,BDDH,Module,H2),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module,M).

process_body([H|T],BDD,BDD1,Vars,Vars1,
[H1|Rest],Env,Module,M):-
  given_cw(M,H),!,
  add_mod_arg(H,Module,H1),
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module,M).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H1|Rest],Env,Module,M):-
  add_mod_arg(H,Module,H1),
  db(H1),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module,M).

process_body([H|T],BDD,BDD1,Vars,[BDDH,BDD2|Vars1],
[H1,slipcover:andc(Env,BDD,BDDH,BDD2)|Rest],Env,Module,M):-
  add_bdd_arg(H,Env,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module,M).

process_body_db([],BDD,BDD,_DB,Vars,Vars,[],_Env,_Module,_M):-!.

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Env,Module,M):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module,M).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Env,Module,M):-
  db(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module,M).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[
  (((neg(H1);\+ H1),slipcover:onec(Env,BDDN));
    (H2,slipcover:bdd_notc(Env,BDDH,BDDN))),
  slipcover:andc(Env,BDD,BDDN,BDD2)
  |Rest],Env,Module,M):-
  given(M,H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H2),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module,M).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[
  neg(H1)|Rest],Env,Module,M):-
  given_cw(M,H),!,
  add_mod_arg(H,Module,H1),
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module,M).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,[BDDH,BDDN,BDD2|Vars1],
[H1,slipcover:bdd_notc(Env,BDDH,BDDN),
  slipcover:andc(Env,BDD,BDDN,BDD2)|Rest],Env,Module,M):-!,
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module,M).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module,M):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module,M).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module,M):-
  db(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module,M).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,
[((H1,slipcover:onec(Env,BDDH));H2),slipcover:andc(Env,BDD,BDDH,BDD2)|Rest],Env,Module,M):-
  given(M,H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H2),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module,M).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,
[H1|Rest],Env,Module,M):-
  given_cw(M,H),!,
  add_mod_arg(H,Module,H1),
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module,M).

process_body_db([H|T],BDD,BDD1,DB,Vars,[BDDH,BDD2|Vars1],
[H1,slipcover:andc(Env,BDD,BDDH,BDD2)|Rest],Env,Module,M):-!, %agg. cut
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module,M).



process_body_cw([],BDD,BDD,Vars,Vars,[],_Module).

process_body_cw([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Module):-
  builtin(H),!,
  process_body_cw(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body_cw([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Module):-
  db(H),!,
  process_body_cw(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body_cw([\+ H|T],BDD,BDD1,Vars,Vars1,[
  \+(H1)|Rest],Module):-
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


given(M,H):-
  functor(H,P,Ar),
  (M:input(P/Ar)).


given_cw(M,H):-
  functor(H,P,Ar),
  (M:input_cw(P/Ar)).

/**
 * set_sc(:Parameter:atom,+Value:term) is det
 *
 * The predicate sets the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
set_sc(M:Parameter,Value):-
  retract(M:local_setting(Parameter,_)),
  assert(M:local_setting(Parameter,Value)).

/**
 * setting_sc(:Parameter:atom,-Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_sc(M:P,V):-
  M:local_setting(P,V).



difference([],_,[]).

difference([H|T],L2,L3):-
  member_eq(H,L2),!,
  difference(T,L2,L3).

difference([H|T],L2,[H|L3]):-
  difference(T,L2,L3).

/**
 * member_eq(+List:list,+Element:term) is det
 *
 * Checks the presence of Element in List. Equality is checked with ==.
 */
member_eq(E,[H|_T]):-
  E==H,!.

member_eq(E,[_H|T]):-
  member_eq(E,T).




process_head(HeadList,M, GroundHeadList) :-
  ground_prob(HeadList), !,
  process_head_ground(HeadList,M, 0, GroundHeadList).

process_head(HeadList,_M, HeadList).


process_head_fixed([Head:p(ProbHead)],M, Prob, [Head:ProbHead1|Null]) :-!,
  ProbHead1 is ProbHead,
  ProbLast is 1 - Prob - ProbHead1,
  M:local_setting(epsilon_parsing, Eps),
  EpsNeg is - Eps,
  ProbLast > EpsNeg,
  (ProbLast > Eps ->
    Null = ['':ProbLast]
  ;
    Null = []
  ).

process_head_fixed([Head:p(ProbHead)|Tail], M, Prob, [Head:ProbHead1|Next]) :-
  ProbHead1 is ProbHead,
  ProbNext is Prob + ProbHead1,
  process_head_fixed(Tail, M, ProbNext, Next).


/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([Head:ProbHead],M, Prob, [Head:ProbHead1|Null]) :-!,
  ProbHead1 is ProbHead,
  ProbLast is 1 - Prob - ProbHead1,
  M:local_setting(epsilon_parsing, Eps),
  EpsNeg is - Eps,
  ProbLast > EpsNeg,
  (ProbLast > Eps ->
    Null = ['':ProbLast]
  ;
    Null = []
  ).

process_head_ground([Head:ProbHead|Tail], M, Prob, [Head:ProbHead1|Next]) :-
  ProbHead1 is ProbHead,
  ProbNext is Prob + ProbHead1,
  process_head_ground(Tail, M, ProbNext, Next).


ground_prob([]).

ground_prob([_Head:ProbHead|Tail]) :-
  ground(ProbHead), % Succeeds if there are no free variables in the term ProbHead.
  ground_prob(Tail).


get_probs([], []).

get_probs([_H:P|T], [P1|T1]) :-
  P1 is P,
  get_probs(T, T1).

get_probs_t([H:A|T],_M, LP,L3):-
  A=..[t,Prob|_],
  var(Prob),!,
  length([H:A|T],N),
  P is 1/(N+1),
  maplist(set_prob(P),[H:A|T],LP,L2),
  append(L2,['':P],L3).

get_probs_t(L,M,LP,L3):-
  process_head_init(L,M,0,L3),
  get_probs(L3,LP).

process_head_init([Head:Ann],M, Prob, [Head:ProbHead1|Null]) :-!,
  Ann=..[t,ProbHead|_],
  ProbHead1 is ProbHead,
  ProbLast is 1 - Prob - ProbHead1,
  M:local_setting(epsilon_parsing, Eps),
  EpsNeg is - Eps,
  ProbLast > EpsNeg,
  (ProbLast > Eps ->
    Null = ['':ProbLast]
  ;
    Null = []
  ).

process_head_init([Head:Ann|Tail], M, Prob, [Head:ProbHead1|Next]) :-
  Ann=..[t,ProbHead|_],
  ProbHead1 is ProbHead,
  ProbNext is Prob + ProbHead1,
  process_head_init(Tail, M, ProbNext, Next).

set_prob(P,A:_,P,A:P).

set_prob_t(A:Ann,P,A:P):-
  Ann=..[t,P|_].

get_at(A:_,A).

generate_clauses_cw([],_M,[],_N,C,C):-!.

generate_clauses_cw([H|T],M,[H1|T1],N,C0,C):-
  gen_clause_cw(H,M,N,N1,H1,CL),!,  %agg.cut
  append(C0,CL,C1),
  generate_clauses_cw(T,M,T1,N1,C1,C).

gen_clause_cw((H :- Body),_M,N,N,(H :- Body),[(H :- Body)]):-
  !.

gen_clause_cw(rule(_R,HeadList,BodyList,Lit,Tun),M,N,N1,
  rule(N,HeadList,BodyList,Lit,Tun),Clauses):-!,
% disjunctive clause with more than one head atom senza depth_bound
  process_body_cw(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Module),
  append([slipcover:onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules(HeadList,Env,Body1,[],N,Probs,BDDAnd,0,Clauses,Module,M)
  ;
    generate_rules(HeadList,Env,Body1,VC,N,Probs,BDDAnd,0,Clauses,Module,M)
  ),
  N1 is N+1.

gen_clause_cw(def_rule(H,BodyList,Lit),_M,N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
% disjunctive clause with a single head atom senza depth_bound con prob =1
  process_body_cw(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Module),
  append([slipcover:onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,Env,BDDAnd,Module,Head1),
  Clauses=[(Head1 :- Body1)].

/**
 * generate_clauses(+Rules0:list,+Module:atom,+StartingIndex:integer,-Rules:list,+Clauses:list,-ClausesOut:list) is det
 *
 * The predicate generate the internal representation of rules to produce clauses to be
 * asserted in the database.
 * Rules0 is a list of term of the form rule(R,HeadList,BodyList,Lit,Tun).
 * Rules is a list of terms of the form
 * rule(N,HeadList,BodyList,Lit,Tun) where N is
 * an increasing index starting from StartingIndex.
 * ClausesOut/Clauses is a difference list of clauses to be asserted.
 */
generate_clauses([],_M,[],_N,C,C):-!.

generate_clauses([H|T],M,[H1|T1],N,C0,C):-
  gen_clause(H,M,N,N1,H1,CL),!,  %agg.cut
  append(C0,CL,C1),
  generate_clauses(T,M,T1,N1,C1,C).


gen_clause((H :- Body),_M,N,N,(H :- Body),[(H :- Body)]):-
  !.

gen_clause(rule(R,HeadList,BodyList,Lit,Tun),M,N,N,
  rule(RI,HeadList,BodyList,Lit,Tun),Clauses):-
  M:local_setting(depth_bound,true),!,
% disjunctive clause with more than one head atom e depth_bound
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules_db(HeadList,Env,Body1,[],RI,Probs,DB,BDDAnd,0,Clauses,Module,M)
  ;
    generate_rules_db(HeadList,Env,Body1,VC,RI,Probs,DB,BDDAnd,0,Clauses,Module,M)
  ),
  (R=ng(_,Vals)->
    get_next_nonground_rule_number(M,RG),
    RI=ng(RG,Vals)
  ;
    get_next_rule_number(M,RI)
  ).

gen_clause(rule(R,HeadList,BodyList,Lit,Tun),M,N,N,
  rule(RI,HeadList,BodyList,Lit,Tun),Clauses):-!,
% disjunctive clause with more than one head atom senza depth_bound
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules(HeadList,Env,Body1,[],RI,Probs,BDDAnd,0,Clauses,Module,M)
  ;
    generate_rules(HeadList,Env,Body1,VC,RI,Probs,BDDAnd,0,Clauses,Module,M)
  ),
  (R=ng(_,Vals)->
    get_next_nonground_rule_number(M,RG),
    RI=ng(RG,Vals)
  ;
    get_next_rule_number(M,RI)
  ).


gen_clause(def_rule(H,BodyList,Lit),M,N,N,def_rule(H,BodyList,Lit),Clauses) :-
% disjunctive clause with a single head atom e depth_bound
  M:local_setting(depth_bound,true),!,
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(H,Env,BDDAnd,DBH,Module,Head1),
  Clauses=[(Head1 :- (DBH>=1,DB is DBH-1,Body1))].

gen_clause(def_rule(H,BodyList,Lit),M,N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
% disjunctive clause with a single head atom senza depth_bound con prob =1
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,Env,BDDAnd,Module,Head1),
  Clauses=[(Head1 :- Body1)].

/**
 * generate_clauses_bg(+Rules:list,-Clauses:list) is det
 *
 * The predicate generate clauses to be
 * asserted in the database for the rules from the background.
 * Rules is a list of term of the form def_rule(H,BodyList,_Lit).
 * Clauses is a list of clauses to be asserted.
 */
generate_clauses_bg([],[]):-!.

generate_clauses_bg([H|T],[CL|T1]):-
  gen_clause_bg(H,CL),  %agg.cut
  generate_clauses_bg(T,T1).

gen_clause_bg(def_rule(H,BodyList,_Lit),Clauses) :-
% disjunctive clause with a single head atom e depth_bound
  process_body_bg(BodyList,BodyList2,Module),
  list2and(BodyList2,Body1),
  add_mod_arg(H,Module,Head1),
  Clauses=(Head1 :- Body1).

/**
 * get_sc_var_n(++M:atomic,++Environment:int,++Rule:int,++Substitution:term,++Probabilities:list,-Variable:int) is det
 *
 * Returns the index Variable of the random variable associated to rule with
 * index Rule, grouding substitution Substitution and head distribution
 * Probabilities in environment Environment.
 * Differs from get_var_n/6 of pita because R can be ng(RN,Vals), indicating a rule for which
 * different instantiations get different parameters.
 */
get_sc_var_n(M,Env,R,S,Probs0,V):-
  (ground(Probs0)->
    maplist(is,Probs,Probs0),
    (M:v(R,S,V)->
      true
    ;
      (R=ng(RN,Vals)->
        M:rule(ng(RN,Vals),HeadList,BodyList,Lits,Tun),
        (M:rule(g(GR),HeadList,BodyList,Lits,Tun)->
          (M:v(GR,S,V)->
            true
          ;
            add_var(Env,Probs,GR,V),
            assert(M:v(GR,S,V))
          )
        ;
          get_next_rule_number(M,GR),
          assert(M:rule(g(GR),HeadList,BodyList,Lits,Tun)),
          add_var(Env,Probs,GR,V),
          assert(M:v(GR,S,V))
        )
      ;
        add_var(Env,Probs,R,V),
        assert(M:v(R,S,V))
      )
    )
  ;
    throw(error('Non ground probailities not instantiated by the body'))
  ).


builtin(average(_L,_Av)) :- !.
builtin(G) :-
  swi_builtin(G).


gen_cl_db_t(M,HeadList,BodyList,Clauses,rule(ng(R,Vals),HeadList1,BodyList,true,Tun)):-
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,Env,Module,M),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  append([slipcover:onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  get_next_nonground_rule_number(M,R),
  get_probs_t(HeadList,M,Probs,HeadList1),
  HeadList=[_:Ann|_],
  Ann=..[t,P|Vals],
  (var(P)->
    Tun=tunable
  ;
    Tun=initial
  ),
  (M:local_setting(single_var,true)->
    generate_rules_db(HeadList1,Env,Body1,[],ng(R,Vals),Probs,DB,BDDAnd,0,Clauses,Module,M)
  ;
    generate_rules_db(HeadList1,Env,Body1,VC,ng(R,Vals),Probs,DB,BDDAnd,0,Clauses,Module,M)
   ).

gen_cl_db_fact_t(M,HeadList,Clauses,rule(ng(R,Vals),HeadList1,[],true,Tun)):-
  term_variables(HeadList,VC),
  get_next_nonground_rule_number(M,R),
  get_probs_t(HeadList,M,Probs,HeadList1),
  HeadList=[_:Ann|_],
  Ann=..[t,P|Vals],
  (var(P)->
    Tun=tunable
  ;
    Tun=initial
  ),
  (M:local_setting(single_var,true)->
    generate_rules_fact_db(HeadList,_Env,[],ng(R,Vals),Probs,0,Clauses,_Module,M)
  ;
    generate_rules_fact_db(HeadList,_Env,VC,ng(R,Vals),Probs,0,Clauses,_Module,M)
  ).

gen_cl_fact_t(M,HeadList,Clauses,rule(ng(R,Vals),HeadList1,[],true,Tun)):-
  term_variables(HeadList,VC),
  get_next_nonground_rule_number(M,R),
  get_probs_t(HeadList,M,Probs,HeadList1),
  HeadList=[_:Ann|_],
  Ann=..[t,P|Vals],
  (var(P)->
    Tun=tunable
  ;
    Tun=initial
  ),
  (M:local_setting(single_var,true)->
    generate_rules_fact(HeadList,_Env,[],ng(R,Vals),Probs,0,Clauses,_Module,M)
  ;
    generate_rules_fact(HeadList,_Env,VC,ng(R,Vals),Probs,0,Clauses,_Module,M)
  ).

gen_cl_t(M,HeadList,BodyList,Clauses,rule(ng(R,Vals),HeadList1,BodyList,true,Tun)):-
  process_body(BodyList,BDD,BDDAnd, [],_Vars,BodyList1,Env,Module,M),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  append([slipcover:onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  get_next_nonground_rule_number(M,R),
  get_probs_t(HeadList,M,Probs,HeadList1),
  HeadList=[_:Ann|_],
  Ann=..[t,P|Vals],
  (var(P)->
    Tun=tunable
  ;
    Tun=initial
  ),
  (M:local_setting(single_var,true)->
    generate_rules(HeadList1,Env,Body1,[],ng(R,Vals),Probs,BDDAnd,0,Clauses,Module,M)
  ;
    generate_rules(HeadList1,Env,Body1,VC,ng(R,Vals),Probs,BDDAnd,0,Clauses,Module,M)
  ).

term_expansion_int((Head :- Body),_M, ((H :- Body),[])):-
  Head=db(H),!.

term_expansion_int((Head :- Body),M, (Clauses,[Rule])):-
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive clause with more than one head atom e depth_bound with individual pars
  Head = (_H:A;_),
  A=..[t,_P|_],
  !,
  list2or(HeadList, Head),
  list2and(BodyList, Body),
  gen_cl_db_t(M,HeadList,BodyList,Clauses,Rule).

term_expansion_int((Head :- Body),M, (Clauses,[Rule])):-
  M:local_setting(compiling,on),
% disjunctive clause with more than one head atom without depth_bound with individual pars
  Head = (_H:A;_),
  A=..[t,_P|_],
  !,
  list2or(HeadList0, Head),
  append(HeadList0,['':_],HeadList),
  list2and(BodyList, Body),
  gen_cl_t(M,HeadList,BodyList,Clauses,Rule).

term_expansion_int((Head :- Body),M, (Clauses,[rule(R,HeadList,BodyList,true,fixed)])):-
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive clause with more than one head atom e depth_bound, fixed par
  Head = (_H:p(_P);_),
  !,
  list2or(HeadListOr, Head),
  process_head_fixed(HeadListOr,M,0,HeadList),
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules_db(HeadList,Env,Body1,[],R,Probs,DB,BDDAnd,0,Clauses,Module,M)
  ;
    generate_rules_db(HeadList,Env,Body1,VC,R,Probs,DB,BDDAnd,0,Clauses,Module,M)
   ).

term_expansion_int((Head :- Body),M, (Clauses,[rule(R,HeadList,BodyList,true,fixed)])):-
  M:local_setting(compiling,on),
% disjunctive clause with more than one head atom senza depth_bound, fixed par
  Head = (_H:p(_P);_),
  !,
  list2or(HeadListOr, Head),
  process_head_fixed(HeadListOr,M,0,HeadList),
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules(HeadList,Env,Body1,[],R,Probs,BDDAnd,0,Clauses,Module,M)
  ;
    generate_rules(HeadList,Env,Body1,VC,R,Probs,BDDAnd,0,Clauses,Module,M)
  ).

term_expansion_int((Head :- Body),M, (Clauses,[rule(R,HeadList,BodyList,true,tunable)])):-
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive clause with more than one head atom e depth_bound
  Head = (_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M,HeadList),
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules_db(HeadList,Env,Body1,[],R,Probs,DB,BDDAnd,0,Clauses,Module,M)
  ;
    generate_rules_db(HeadList,Env,Body1,VC,R,Probs,DB,BDDAnd,0,Clauses,Module,M)
   ).

term_expansion_int((Head :- Body),M, (Clauses,[rule(R,HeadList,BodyList,true,tunable)])):-
  M:local_setting(compiling,on),
% disjunctive clause with more than one head atom senza depth_bound
  Head = (_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M,HeadList),
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules(HeadList,Env,Body1,[],R,Probs,BDDAnd,0,Clauses,Module,M)
  ;
    generate_rules(HeadList,Env,Body1,VC,R,Probs,BDDAnd,0,Clauses,Module,M)
  ).

term_expansion_int((Head :- Body),M, (Clauses,[Rule])) :-
% disjunctive clause with a single head atom e DB, con prob. diversa da 1 with individual pars
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
  ((Head:-Body) \= ((sc_expansion(_,_) ):- _ )),
  Head = (H:A),
  A=..[t,_P|_],
  !,
  HeadList=[H:A],
  list2and(BodyList, Body),
  gen_cl_db_t(M,HeadList,BodyList,Clauses,Rule).

term_expansion_int((Head :- Body),M, (Clauses,[Rule])):-
  M:local_setting(compiling,on),
% disjunctive clause with a single head atom without depth_bound with individual pars
  Head = (H:A),
  A=..[t,_P|_],
  !,
  HeadList=[H:A],
  list2and(BodyList, Body),
  gen_cl_t(M,HeadList,BodyList,Clauses,Rule).

term_expansion_int((Head :- Body),M, (Clauses,[rule(R,HeadList,BodyList,true,fixed)])) :-
% disjunctive clause with a single head atom e DB, con prob. diversa da 1, fixed par
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
  ((Head:-Body) \= ((sc_expansion(_,_) ):- _ )),
  Head = (H:p(_)),
  !,
  list2or(HeadListOr, Head),
  process_head_fixed(HeadListOr,M,0,HeadList),
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),%***test single_var
  (M:local_setting(single_var,true)->
    generate_clause_db(H,Env,Body2,[],R,Probs,DB,BDDAnd,0,Clauses,Module,M)
  ;
    generate_clause_db(H,Env,Body2,VC,R,Probs,DB,BDDAnd,0,Clauses,Module,M)
  ).

term_expansion_int((Head :- Body),M, (Clauses,[rule(R,HeadList,BodyList,true,fixed)])) :-
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1, fixed par
  M:local_setting(compiling,on),
  ((Head:-Body) \= ((sc_expansion(_,_) ):- _ )),
  Head = (H:p(_)),
  !,
  list2or(HeadListOr, Head),
  process_head_fixed(HeadListOr,M,0,HeadList),
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),%***test single_vars
  (M:local_setting(single_var,true)->
    generate_clause(H,Env,Body2,[],R,Probs,BDDAnd,0,Clauses,Module,M)
  ;
    generate_clause(H,Env,Body2,VC,R,Probs,BDDAnd,0,Clauses,Module,M)
  ).

term_expansion_int((Head :- Body),M, ([],[])) :-
% disjunctive clause with a single head atom con prob. 0 senza depth_bound --> la regola non è caricata nella teoria e non è conteggiata in NR
  M:local_setting(compiling,on),
  ((Head:-Body) \= ((sc_expansion(_,_) ):- _ )),
  Head = (_H:P),number(P),P=:=0.0, !.

term_expansion_int((Head :- Body),M, (Clauses,[def_rule(H,BodyList,true)])) :-
% disjunctive clause with a single head atom e depth_bound con prob =1
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
  ((Head:-Body) \= ((sc_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M,HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(H,Env,BDDAnd,DBH,Module,Head1),
  Clauses=(Head1 :- (DBH>=1,DB is DBH-1,Body1)).

term_expansion_int((Head :- Body), M,(Clauses,[def_rule(H,BodyList,true)])) :-
% disjunctive clause with a single head atom senza depth_bound con prob =1
  M:local_setting(compiling,on),
   ((Head:-Body) \= ((sc_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M,HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,Env,BDDAnd,Module,Head1),
  Clauses=(Head1 :- Body1).

term_expansion_int((Head :- Body),M, (Clauses,[rule(R,HeadList,BodyList,true,tunable)])) :-
% disjunctive clause with a single head atom e DB, con prob. diversa da 1
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
  ((Head:-Body) \= ((sc_expansion(_,_) ):- _ )),
  Head = (H:_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M,HeadList),
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),%***test single_var
  (M:local_setting(single_var,true)->
    generate_clause_db(H,Env,Body2,[],R,Probs,DB,BDDAnd,0,Clauses,Module,M)
  ;
    generate_clause_db(H,Env,Body2,VC,R,Probs,DB,BDDAnd,0,Clauses,Module,M)
  ).

term_expansion_int((Head :- Body),M, (Clauses,[rule(R,HeadList,BodyList,true,tunable)])) :-
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  M:local_setting(compiling,on),
  ((Head:-Body) \= ((sc_expansion(_,_) ):- _ )),
  Head = (H:_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M,HeadList),
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  term_variables(List,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),%***test single_vars
  (M:local_setting(single_var,true)->
    generate_clause(H,Env,Body2,[],R,Probs,BDDAnd,0,Clauses,Module,M)
  ;
    generate_clause(H,Env,Body2,VC,R,Probs,BDDAnd,0,Clauses,Module,M)
  ).

term_expansion_int((Head :- Body),M,(Clauses,[])) :-
% definite clause for db facts
  M:local_setting(compiling,on),
  ((Head:-Body) \= ((sc_expansion(_,_)) :- _ )),
  Head=db(Head1),!,
  Clauses=(Head1 :- Body).

term_expansion_int((Head :- Body),M,(Clauses,[def_rule(Head,BodyList,true)])) :-
% definite clause with depth_bound
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
   ((Head:-Body) \= ((sc_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body),
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(Head,Env,BDDAnd,DBH,Module,Head1),
  Clauses=(Head1 :- (DBH>=1,DB is DBH-1,Body1)).

term_expansion_int((Head :- Body),M,(Clauses,[def_rule(Head,BodyList,true)])) :-
% definite clause senza DB
  M:local_setting(compiling,on),
  ((Head:-Body) \= ((sc_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module,M),
  append([slipcover:onec(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Head,Env,BDDAnd,Module,Head1),
  Clauses=(Head1 :- Body2).

term_expansion_int(Head,M,(Clauses,[rule(R,HeadList,[],true,fixed)])) :-
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive FACT with more than one head atom e db, fixed par
  Head=(_:p(_);_), !,
  list2or(HeadListOr, Head),
  process_head_fixed(HeadListOr,M,0,HeadList),
  term_variables(HeadList,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules_fact_db(HeadList,_Env,[],R,Probs,0,Clauses,_Module,M)
  ;
    generate_rules_fact_db(HeadList,_Env,VC,R,Probs,0,Clauses,_Module,M)
  ).


term_expansion_int(Head,M,(Clauses,[rule(R,HeadList,[],true,fixed)])) :-
  M:local_setting(compiling,on),
% disjunctive fact with more than one head atom senza db, fixed par
  Head=(_:p(_);_), !,
  list2or(HeadListOr, Head),
  process_head_fixed(HeadListOr,M,0,HeadList),
  term_variables(HeadList,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs), %**** test single_var
  (M:local_setting(single_var,true)->
    generate_rules_fact(HeadList,_Env,[],R,Probs,0,Clauses,_Module,M)
  ;
    generate_rules_fact(HeadList,_Env,VC,R,Probs,0,Clauses,_Module,M)
  ).

term_expansion_int(Head,M,(Clauses,[Rule])) :-
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive FACT with more than one head atom e db with individual pars
  Head = (_H:A;_),
  A=..[t,_P|_],
  !,
  list2or(HeadList0, Head),
  append(HeadList0,['':_],HeadList),
  gen_cl_db_fact_t(M,HeadList,Clauses,Rule).

term_expansion_int(Head,M,(Clauses,[Rule])) :-
  M:local_setting(compiling,on),
% disjunctive FACT with more than one head atom wo db with individual pars
  Head = (_H:A;_),
  A=..[t,_P|_],
  !,
  list2or(HeadList0, Head),
  append(HeadList0,['':_],HeadList),
  gen_cl_fact_t(M,HeadList,Clauses,Rule).

term_expansion_int(Head,M,(Clauses,[rule(R,HeadList,[],true,tunable)])) :-
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive FACT with more than one head atom e db
  Head=(_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M,HeadList),
  term_variables(HeadList,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules_fact_db(HeadList,_Env,[],R,Probs,0,Clauses,_Module,M)
  ;
    generate_rules_fact_db(HeadList,_Env,VC,R,Probs,0,Clauses,_Module,M)
  ).


term_expansion_int(Head,M,(Clauses,[rule(R,HeadList,[],true,tunable)])) :-
  M:local_setting(compiling,on),
% disjunctive fact with more than one head atom senza db
  Head=(_;_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M,HeadList),
  term_variables(HeadList,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs), %**** test single_var
  (M:local_setting(single_var,true)->
    generate_rules_fact(HeadList,_Env,[],R,Probs,0,Clauses,_Module,M)
  ;
    generate_rules_fact(HeadList,_Env,VC,R,Probs,0,Clauses,_Module,M)
  ).

term_expansion_int(Head,M,([],[])) :-
  M:local_setting(compiling,on),
% disjunctive fact with a single head atom con prob. 0
  (Head \= ((sc_expansion(_,_)) :- _ )),
  Head = (_H:P),number(P),P=:=0.0, !.

term_expansion_int(Head,M,(Clause,[def_rule(H,[],true)])) :-
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive fact with a single head atom con prob.1 e db
  (Head \= ((sc_expansion(_,_)) :- _ )),
  Head = (H:P),number(P),P=:=1.0, !,
  list2and([slipcover:onec(Env,BDD)],Body1),
  add_bdd_arg_db(H,Env,BDD,_DB,_Module,Head1),
  Clause=(Head1 :- Body1).

term_expansion_int(Head,M,(Clause,[def_rule(H,[],true)])) :-
  M:local_setting(compiling,on),
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((sc_expansion(_,_)) :- _ )),
  Head = (H:P),number(P),P=:=1.0, !,
  list2and([slipcover:onec(Env,BDD)],Body1),
  add_bdd_arg(H,Env,BDD,_Module,Head1),
  Clause=(Head1 :- Body1).

term_expansion_int(Head,M,(Clause,[rule(R,HeadList,[],true,fixed)])) :-
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive fact with a single head atom e prob. generiche, con db, fixed par
  (Head \= ((sc_expansion(_,_)) :- _ )),
  Head=(H:p(_)), !,
  list2or(HeadListOr, Head),
  process_head_fixed(HeadListOr,M,0,HeadList),
  term_variables(HeadList,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  add_bdd_arg_db(H,Env,BDD,_DB,_Module,Head1),
  (M:local_setting(single_var,true)->
    Clause=(Head1:-(slipcover:get_sc_var_n(M,Env,R,[],Probs,V),slipcover:equalityc(Env,V,0,BDD)))
  ;
    Clause=(Head1:-(slipcover:get_sc_var_n(M,Env,R,VC,Probs,V),slipcover:equalityc(Env,V,0,BDD)))
  ).

term_expansion_int(Head,M,(Clause,[rule(R,HeadList,[],true,fixed)])) :-
  M:local_setting(compiling,on),
% disjunctive fact with a single head atom e prob. generiche, senza db, fixed par
  (Head \= ((sc_expansion(_,_)) :- _ )),
  Head=(H:p(_)), !,
  list2or(HeadListOr, Head),
  process_head_fixed(HeadListOr,M,0,HeadList),
  term_variables(HeadList,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  add_bdd_arg(H,Env,BDD,_Module,Head1),%***test single_var
  (M:local_setting(single_var,true)->
    Clause=(Head1:-(slipcover:get_sc_var_n(M,Env,R,[],Probs,V),slipcover:equalityc(Env,V,0,BDD)))
  ;
    Clause=(Head1:-(slipcover:get_sc_var_n(M,Env,R,VC,Probs,V),slipcover:equalityc(Env,V,0,BDD)))
  ).


term_expansion_int(Head,M,(Clauses,[Rule])) :-
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive FACT with a sigble head atom e db with individual pars
  Head = (H:A),
  A=..[t,_P|_],
  !,
  HeadList=[H:0.5,'':0.5],
  gen_cl_db_fact_t(M,HeadList,Clauses,Rule).

term_expansion_int(Head,M,(Clauses,[Rule])) :-
  M:local_setting(compiling,on),
% disjunctive FACT with a single head atom wo db with individual pars
  Head = (H:A),
  A=..[t,_P|_],
  !,
  HeadList=[H:0.5,'':0.5],
  gen_cl_fact_t(M,HeadList,Clauses,Rule).

term_expansion_int(Head,M,(Clause,[rule(R,HeadList,[],true,tunable)])) :-
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive fact with a single head atom e prob. generiche, con db
  (Head \= ((sc_expansion(_,_)) :- _ )),
  Head=(H:_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M,HeadList),
  term_variables(HeadList,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  add_bdd_arg_db(H,Env,BDD,_DB,_Module,Head1),
  (M:local_setting(single_var,true)->
    Clause=(Head1:-(slipcover:get_sc_var_n(M,Env,R,[],Probs,V),slipcover:equalityc(Env,V,0,BDD)))
  ;
    Clause=(Head1:-(slipcover:get_sc_var_n(M,Env,R,VC,Probs,V),slipcover:equalityc(Env,V,0,BDD)))
  ).

term_expansion_int(Head,M,(Clause,[rule(R,HeadList,[],true,tunable)])) :-
  M:local_setting(compiling,on),
% disjunctive fact with a single head atom e prob. generiche, senza db
  (Head \= ((sc_expansion(_,_)) :- _ )),
  Head=(H:_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M,HeadList),
  term_variables(HeadList,VC),
  get_next_rule_number(M,R),
  get_probs(HeadList,Probs),
  add_bdd_arg(H,Env,BDD,_Module,Head1),%***test single_var
  (M:local_setting(single_var,true)->
    Clause=(Head1:-(slipcover:get_sc_var_n(M,Env,R,[],Probs,V),slipcover:equalityc(Env,V,0,BDD)))
  ;
    Clause=(Head1:-(slipcover:get_sc_var_n(M,Env,R,VC,Probs,V),slipcover:equalityc(Env,V,0,BDD)))
  ).

term_expansion_int(Head,M, ((Head1:-slipcover:onec(Env,One)),[def_rule(Head,[],true)])) :-
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% definite fact with db
  (Head \= ((sc_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg_db(Head,Env,One,_DB,_Module,Head1).

term_expansion_int(Head,M, ((Head1:-slipcover:onec(Env,One)),[def_rule(Head,[],true)])) :-
  M:local_setting(compiling,on),
% definite fact without db
  (Head \= ((sc_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg(Head,Env,One,_Module,Head1).

/*-----------*/



:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(slipcover:induce_par(_,_) ,[]).
sandbox:safe_meta(slipcover:induce(_,_), []).
sandbox:safe_meta(slipcover:get_node(_,_), []).
sandbox:safe_meta(slipcover:test_prob(_,_,_,_,_,_), []).
sandbox:safe_meta(slipcover:test(_,_,_,_,_,_,_), []).
sandbox:safe_meta(slipcover:set_sc(_,_), []).
sandbox:safe_meta(slipcover:setting_sc(_,_), []).



test_no_area(TestSet,M,NPos,NNeg,CLL,Results):-
  test_folds(TestSet,M,[],Results,0,NPos,0,NNeg,0,CLL).


test_folds([],_M,LG,LG,Pos,Pos,Neg,Neg,CLL,CLL).

test_folds([HT|TT],M,LG0,LG,Pos0,Pos,Neg0,Neg,CLL0,CLL):-
  test_1fold(HT,M,LG1,Pos1,Neg1,CLL1),
  append(LG0,LG1,LG2),
  Pos2 is Pos0+Pos1,
  Neg2 is Neg0+Neg1,
  CLL2 is CLL0+CLL1,
  test_folds(TT,M,LG2,LG,Pos2,Pos,Neg2,Neg,CLL2,CLL).

test_1fold(F,M,LGOrd,Pos,Neg,CLL1):-
  find_ex(F,M,LG,Pos,Neg),
  compute_CLL_atoms(LG,M,0,0,CLL1,LG1),
  keysort(LG1,LGOrd).


find_ex(DB,M,LG,Pos,Neg):-
  findall(P/A,M:output(P/A),LP),
  M:local_setting(neg_ex,given),!,
  find_ex_pred(LP,M,DB,[],LG,0,Pos,0,Neg).

find_ex(DB,M,LG,Pos,Neg):-
  findall(P/A,M:output(P/A),LP),
  M:local_setting(neg_ex,cw),
  find_ex_pred_cw(LP,M,DB,[],LG,0,Pos,0,Neg).


find_ex_pred([],_M,_DB,LG,LG,Pos,Pos,Neg,Neg).

find_ex_pred([P/A|T],M,DB,LG0,LG,Pos0,Pos,Neg0,Neg):-
  functor(At,P,A),
  find_ex_db(DB,M,At,LG0,LG1,Pos0,Pos1,Neg0,Neg1),
  find_ex_pred(T,M,DB,LG1,LG,Pos1,Pos,Neg1,Neg).

find_ex_db([],_M,_At,LG,LG,Pos,Pos,Neg,Neg).

find_ex_db([H|T],M,At,LG0,LG,Pos0,Pos,Neg0,Neg):-
  At=..[P|L],
  At1=..[P,H|L],
  findall(At1,M:At1,LP),
  findall(\+ At1,M:neg(At1),LN),
  length(LP,NP),
  length(LN,NN),
  append([LG0,LP,LN],LG1),
  Pos1 is Pos0+NP,
  Neg1 is Neg0+NN,
  find_ex_db(T,M,At,LG1,LG,Pos1,Pos,Neg1,Neg).


find_ex_pred_cw([],_M,_DB,LG,LG,Pos,Pos,Neg,Neg).

find_ex_pred_cw([P/A|T],M,DB,LG0,LG,Pos0,Pos,Neg0,Neg):-
  functor(At,P,A),
  findall(Types,get_types(At,M,Types),LT),
  append(LT,LLT),
  remove_duplicates(LLT,Types1),
  find_ex_db_cw(DB,M,At,Types1,LG0,LG1,Pos0,Pos1,Neg0,Neg1),
  find_ex_pred_cw(T,M,DB,LG1,LG,Pos1,Pos,Neg1,Neg).

get_types(At,_M,[]):-
  At=..[_],!.

get_types(At,M,Types):-
  M:modeh(_,At),
  At=..[_|Args],
  get_args(Args,Types).

get_types(At,M,Types):-
  M:modeh(_,HT,_,_),
  member(At,HT),
  At=..[_|Args],
  get_args(Args,Types).


get_args([],[]).

get_args([+H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([-H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([#H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([-#H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([H|T],[H|T1]):-
  get_args(T,T1).




get_constants([],_M,_Mod,[]).

get_constants([Type|T],M,Mod,[(Type,Co)|C]):-
  find_pred_using_type(Type,Mod,LP),
  find_constants(LP,M,Mod,[],Co),
  get_constants(T,M,Mod,C).

find_pred_using_type(T,M,L):-
  (setof((P,Ar,A),pred_type(T,M,P,Ar,A),L)->
    true
  ;
    L=[]
  ).

pred_type(T,M,P,Ar,A):-
  M:modeh(_,S),
  S=..[P|Args],
  length(Args,Ar),
  scan_args(Args,T,1,A).

pred_type(T,M,P,Ar,A):-
  M:modeb(_,S),
  S=..[P|Args],
  length(Args,Ar),
  scan_args(Args,T,1,A).

scan_args([+T|_],T,A,A):-!.

scan_args([-T|_],T,A,A):-!.

scan_args([#T|_],T,A,A):-!.

scan_args([-#T|_],T,A,A):-!.

scan_args([_|Tail],T,A0,A):-
  A1 is A0+1,
  scan_args(Tail,T,A1,A).

find_constants([],_M,_Mod,C,C).

find_constants([(P,Ar,A)|T],M,Mod,C0,C):-
  gen_goal(1,Ar,A,Args,ArgsNoV,V),
  G=..[P,M|Args],
  (setof(V,ArgsNoV^call_goal(Mod,G),LC)->
    true
  ;
    LC=[]
  ),
  append(C0,LC,C1),
  remove_duplicates(C1,C2),
  find_constants(T,M,Mod,C2,C).

call_goal(M,G):-
  M:G.

gen_goal(Arg,Ar,_A,[],[],_):-
  Arg =:= Ar+1,!.

gen_goal(A,Ar,A,[V|Args],ArgsNoV,V):-!,
  Arg1 is A+1,
  gen_goal(Arg1,Ar,A,Args,ArgsNoV,V).

gen_goal(Arg,Ar,A,[ArgV|Args],[ArgV|ArgsNoV],V):-
  Arg1 is Arg+1,
  gen_goal(Arg1,Ar,A,Args,ArgsNoV,V).



find_ex_db_cw([],_M,_At,_Ty,LG,LG,Pos,Pos,Neg,Neg).

find_ex_db_cw([H|T],M,At,Types,LG0,LG,Pos0,Pos,Neg0,Neg):-
  get_constants(Types,H,M,C),
  At=..[P|L],
  get_types(At,M,TypesA),!,
  length(L,N),
  length(LN,N),
  At1=..[P,H|LN],
  findall(At1,M:At1,LP),
  (setof(\+ At1,neg_ex(LN,M,TypesA,At1,C),LNeg)->true;LNeg=[]),
  length(LP,NP),
  length(LNeg,NN),
  append([LG0,LP,LNeg],LG1),
  Pos1 is Pos0+NP,
  Neg1 is Neg0+NN,
  find_ex_db_cw(T,M,At,Types,LG1,LG,Pos1,Pos,Neg1,Neg).

neg_ex([],M,[],At1,_C):-
  \+ M:At1.

neg_ex([H|T],M,[HT|TT],At1,C):-
  member((HT,Co),C),
  member(H,Co),
  neg_ex(T,M,TT,At1,C).

compute_CLL_atoms([],_M,_N,CLL,CLL,[]):-!.

compute_CLL_atoms([\+ H|T],M,N,CLL0,CLL1,[PG- (\+ H)|T1]):-!,
  init(Env),
  abolish_all_tables,
  get_node(H,M,Env,BDD),!,
  ret_prob(Env,BDD,PG),
  end(Env),!,
  PG1 is 1-PG,
  (PG1=:=0.0->
    M:local_setting(logzero,LZ),
    CLL2 is CLL0+LZ
  ;
    CLL2 is CLL0+ log(PG1)
  ),
  N1 is N+1,
  compute_CLL_atoms(T,M,N1,CLL2,CLL1,T1).

compute_CLL_atoms([H|T],M,N,CLL0,CLL1,[PG-H|T1]):-
  init(Env),
  abolish_all_tables,
  get_node(H,M,Env,BDD),!,
  ret_prob(Env,BDD,PG),
  end(Env),!,
  (PG=:=0.0->
    M:local_setting(logzero,LZ),
    CLL2 is CLL0+LZ
  ;
    CLL2 is CLL0+ log(PG)
  ),
  N1 is N+1,
  compute_CLL_atoms(T,M,N1,CLL2,CLL1,T1).


/**
 * write2(+Module:atom,+Message:term) is det
 *
 * The predicate calls write(Message) if the verbosity is at least 2.
 * Module is used to get the verbosity setting
 */
write2(M,A):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write(A)
  ;
    true
  ).
/**
 * write3(+Module:atom,+Message:term) is det
 *
 * The predicate calls write(Message) if the verbosity is at least 3.
 * Module is used to get the verbosity setting.
 */
write3(M,A):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write(A)
  ;
    true
  ).
/**
 * nl2(+Module:atom) is det
 *
 * The predicate prints a newline if the verbosity is at least 2.
 * Module is used to get the verbosity setting.
 */
nl2(M):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    nl
  ;
    true
  ).
/**
 * nl3(+Module:atom) is det
 *
 * The predicate prints a newline if the verbosity is at least 3.
 * Module is used to get the verbosity setting.
 */
nl3(M):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    nl
  ;
    true
  ).
/**
 * format2(+Module:atom,+Format, :Arguments) is det
 *
 * The predicate calls format(Format,Arguments) if the verbosity is at least 2.
 * Module is used to get the verbosity setting.
 */
format2(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    format(A,B)
  ;
    true
  ).
/**
 * format3(+Module:atom,+Format, :Arguments) is det
 *
 * The predicate calls format(Format,Arguments) if the verbosity is at least 3.
 * Module is used to get the verbosity setting.
 */
format3(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    format(A,B)
  ;
    true
  ).
/**
 * write_rules2(+Module:atom,+Rules:list,+Stream:atom) is det
 *
 * The predicate write the rules in Rules on stream Stream if the verbosity is at least 2.
 * Module is used to get the verbosity setting.
 */
write_rules2(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_rules(A,B)
  ;
    true
  ).
/**
 * write_rules3(+Module:atom,+Rules:list,+Stream:atom) is det
 *
 * The predicate write the rules in Rules on stream Stream if the verbosity is at least 3.
 * Module is used to get the verbosity setting.
 */
write_rules3(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_rules(A,B)
  ;
    true
  ).


write_disj_clause2(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_disj_clause(A,B)
  ;
    true
  ).

write_disj_clause3(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_disj_clause(A,B)
  ;
    true
  ).

write_body2(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_body(A,B)
  ;
    true
  ).

write_body3(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_body(A,B)
  ;
    true
  ).

/**
 * tab(+Module:atom,+PredSpec:pred_spec,-TableSpec:term) is det
 *
 * Records the fact that predicate PredSpec must be tabled and returns
 * the necessary term for the tabling directive in TableSpec.
 * Module is used to store the information in the correct module
 */
tab(M,A/B,P):-
  length(Args0,B),
  (M:local_setting(depth_bound,true)->
    ExtraArgs=[_,_,_,lattice(slipcover:orc/3)]
  ;
    ExtraArgs=[_,_,lattice(slipcover:orc/3)]
  ),
  append(Args0,ExtraArgs,Args),
  P=..[A|Args],
  PT=..[A|Args0],
  (M:tabled(PT)->
    true
  ;
    assert(M:tabled(PT))
  ).
/**
 * zero_clause(+Module:atom,+PredSpec:pred_spec,-ZeroClause:term) is det
 *
 * Generates the zero clause for predicate PredSpec.
 * Module is the module of the input file.
 */
zero_clause(M,A/B,(H:-maplist(nonvar,Args0),slipcover:zeroc(Env,BDD))):-
  B1 is B+1,
  length(Args0,B1),
  (M:local_setting(depth_bound,true)->
    ExtraArgs=[_,Env,BDD]
  ;
    ExtraArgs=[Env,BDD]
  ),
  append(Args0,ExtraArgs,Args),
  H=..[A|Args].




sc_expansion((:- begin_bg), []) :-
  prolog_load_context(module, M),
  sc_input_mod(M),!,
  assert(M:bg_on).

sc_expansion(C, M:bgc(C)) :-
  prolog_load_context(module, M),
  C\= (:- end_bg),
  sc_input_mod(M),
  M:bg_on,!.

sc_expansion((:- end_bg), []) :-
  prolog_load_context(module, M),
  sc_input_mod(M),!,
  retractall(M:bg_on),
  findall(C,M:bgc(C),L),
  retractall(M:bgc(_)),
  (M:bg(BG0)->
    retract(M:bg(BG0)),
    append(BG0,L,BG),
    assert(M:bg(BG))
  ;
    assert(M:bg(L))
  ).

sc_expansion((:- begin_in), []) :-
  prolog_load_context(module, M),
  sc_input_mod(M),!,
  assert(M:in_on).

sc_expansion(C, M:inc(C)) :-
  prolog_load_context(module, M),
  C\= (:- end_in),
  sc_input_mod(M),
  M:in_on,!.

sc_expansion((:- end_in), []) :-
  prolog_load_context(module, M),
  sc_input_mod(M),!,
  retractall(M:in_on),
  findall(C,M:inc(C),L),
  retractall(M:inc(_)),
  (M:in(IN0)->
    retract(M:in(IN0)),
    append(IN0,L,IN),
    assert(M:in(IN))
  ;
    assert(M:in(L))
  ).

sc_expansion(begin(model(I)), []) :-
  prolog_load_context(module, M),
  sc_input_mod(M),!,
  retractall(M:model(_)),
  assert(M:model(I)),
  assert(M:int(I)).

sc_expansion(end(model(_I)), []) :-
  prolog_load_context(module, M),
  sc_input_mod(M),!,
  retractall(M:model(_)).

sc_expansion(output(P/A), [output(P/A)|TabDir]) :-
  prolog_load_context(module, M),
  sc_input_mod(M),
  M:local_setting(tabling,auto),!,
  tab(M,P/A,P1),
  zero_clause(M,P/A,Z),
  system:term_expansion((:- table P1),TabDir),
  assert(M:zero_clauses([Z])).

sc_expansion(input(P/A), [input(P/A)|TabDir]) :-
  prolog_load_context(module, M),
  sc_input_mod(M),
  M:local_setting(tabling,auto),!,
  tab(M,P/A,P1),
  zero_clause(M,P/A,Z),
  system:term_expansion((:- table P1),TabDir),
  assert(M:zero_clauses([Z])).

sc_expansion(At, A) :-
  prolog_load_context(module, M),
  sc_input_mod(M),
  M:model(Name),
  At \= (_ :- _),
  At \= end_of_file,
  (At=neg(Atom)->
    Atom=..[Pred|Args],
    Atom1=..[Pred,Name|Args],
    A=neg(Atom1)
  ;
    (At=prob(Pr)->
      A='$prob'(Name,Pr)
    ;
      At=..[Pred|Args],
      Atom1=..[Pred,Name|Args],
      A=Atom1
    )
  ).

:- thread_local sc_file/1.

user:term_expansion((:- sc), []) :-!,
  prolog_load_context(source, Source),
  asserta(sc_file(Source)),
  prolog_load_context(module, M),
  retractall(M:local_setting(_,_)),
  findall(local_setting(P,V),default_setting_sc(P,V),L),
  assert_all(L,M,_),
  assert(sc_input_mod(M)),
  retractall(M:rule_sc_n(_)),
  assert(M:rule_sc_n(0)),
  retractall(M:rule_ng_sc_n(_)),
  assert(M:rule_ng_sc_n(0)),
  M:dynamic((modeh/2,modeh/4,modeb/2,fixed_rule/3,banned/2,lookahead/2,
    lookahead_cons/2,lookahead_cons_var/2,'$prob'/2,output/1,input/1,input_cw/1,
    ref_clause/1,ref/1,model/1,neg/1,rule/5,determination/2,
    bg_on/0,bg/1,bgc/1,in_on/0,in/1,inc/1,int/1,v/3,
    query_rule/4,database/1,
    zero_clauses/1,tabled/1,
    fold/2)),
  retractall(M:tabled(_)),
  style_check(-discontiguous).

user:term_expansion(end_of_file, C) :-
  sc_file(Source),
  prolog_load_context(source, Source),
  retractall(sc_file(Source)),
  prolog_load_context(module, M),
  sc_input_mod(M),!,
  retractall(sc_input_mod(M)),
  make_dynamic(M),
  findall(LZ,M:zero_clauses(LZ),L0),
  append(L0,L),
  retractall(M:zero_clauses(_)),
%  retractall(M:tabled(_)),
  %retractall(sc_input_mod(M)),
  append(L,[(:- style_check(+discontiguous)),end_of_file],C).

user:term_expansion(In, Out) :-
  \+ current_prolog_flag(xref, true),
  sc_file(Source),
  prolog_load_context(source, Source),
  sc_expansion(In, Out).