/** <module> slipcover

This module performs learning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It performs both parameter and structure learning.

See https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or 
http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html for
details.

@author Fabrizio Riguzzi, Elena Bellodi
@license Artistic License 2.0
@copyright Fabrizio Riguzzi, Elena Bellodi
*/

/*

SLIPCOVER

Copyright (c) 2016, Fabrizio Riguzzi and Elena Bellodi

*/
:-module(slipcover,[set_sc/2,setting_sc/2,
  induce/2,induce_par/2,test/7,list2or/2,list2and/2,
  sample/4,
  op(500,fx,#),op(500,fx,'-#'),
  test_prob/6]).
%:- meta_predicate get_node(:,-).
:-use_module(library(auc)).
:-use_module(library(lists)).
:-use_module(library(random)).
:-use_module(library(system)).
:-use_module(library(terms)).
:-use_module(library(rbtrees)).
:-use_module(library(pita)).
:-use_module(library(apply)).
%:-use_foreign_library(foreign(bddem),install).
:-set_prolog_flag(unknown,warning).

%:-multifile setting_sc/2.
%:-use_module(library(sandbox)).




:- thread_local v/3, input_mod/1, local_setting/2, rule_sc_n/1.


%:- multifile init/3,init_bdd/2,init_test/2,ret_prob/3,end/1,end_bdd/1,end_test/1,one/2,zero/2,and/4,or/4,add_var/5,equality/4,remove/3.

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
default_setting_sc(background_clauses,50).

default_setting_sc(specialization,bottom).
%setting_sc(specialization,mode).
/* allowed values: mode,bottom */
default_setting_sc(specialize_head,false).

default_setting_sc(seed,rand(10,1231,3032)).  
default_setting_sc(score,ll).
/* allowed values: ll aucpr */
default_setting_sc(neg_ex,cw).


default_setting_sc(epsilon_parsing, 1e-5).
default_setting_sc(tabling, off).
/* on, off */

default_setting_sc(bagof,false).
/* values: false, intermediate, all, extra */

default_setting_sc(compiling,off).


default_setting_sc(depth_bound,true).  %if true, it limits the derivation of the example to the value of 'depth'
default_setting_sc(depth,2).
default_setting_sc(single_var,true). %false:1 variable for every grounding of a rule; true: 1 variable for rule (even if a rule has more groundings),simpler.




/** 
 * induce(+TrainFolds:list_of_atoms,-P:probabilistic_program) is det
 *
 * The predicate performs structure learning using the folds indicated in 
 * TrainFolds for training. 
 * It returns in P the learned probabilistic program.
 */
induce(TrainFolds,P):-
  induce_rules(TrainFolds,P0),
  rules2terms(P0,P).
%  generate_clauses(P0,P,0,[],_Th).

/** 
 * induce(+TrainFolds:list_of_atoms,+TestFolds:list_of_atoms,-P:probabilistic_program,-LL:float,-AUCROC:float,-ROC:dict,-AUCPR:float,-PR:dict) is det
 *
 * The predicate performs structure learning using the folds indicated in 
 * TrainFolds for training. 
 * It returns in P the learned probabilistic program.
 * Moreover, it tests P on the folds indicated in TestFolds and returns the
 * log likelihood of the test examples in LL, the area under the Receiver
 * Operating Characteristic curve in AUCROC, a dict containing the points
 * of the ROC curve in ROC, the area under the Precision Recall curve in AUCPR
 * and a dict containing the points of the PR curve in PR
 */
induce(TrainFolds,TestFolds,ROut,LL,AUCROC,ROC,AUCPR,PR):-
  induce_rules(TrainFolds,R),
  rules2terms(R,ROut),
  test(ROut,TestFolds,LL,AUCROC,ROC,AUCPR,PR).

/** 
 * test(+P:probabilistic_program,+TestFolds:list_of_atoms,-LL:float,-AUCROC:float,-ROC:dict,-AUCPR:float,-PR:dict) is det
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
 * test_prob(+P:probabilistic_program,+TestFolds:list_of_atoms,-NPos:int,-NNeg:int,-LL:float,-Results:list) is det
 *
 * The predicate takes as input in P a probabilistic program,
 * tests P on the folds indicated in TestFolds and returns 
 * the number of positive examples in NPos, the number of negative examples 
 * in NNeg, the log likelihood in LL
 * and in Results a list containing the probabilistic result for each query contained in TestFolds.
 */
test_prob(P,TestFolds,NPos,NNeg,CLL,Results) :-
  write2('Testing\n'),
  input_mod(M),
  make_dynamic(M),
  findall(Exs,(member(F,TestFolds),M:fold(F,Exs)),L),
  append(L,TE),
  set_sc(compiling,on),
  process_clauses(P,[],_,[],PRules),
  generate_clauses(PRules,RuleFacts,0,[],Th), 
  assert_all(Th,M,ThRef),
  assert_all(RuleFacts,M,RFRef),
  (M:bg(RBG0)->
    process_clauses(RBG0,[],_,[],RBG),
    generate_clauses(RBG,_RBGRF,0,[],ThBG),
    generate_clauses_bg(RBG,ClBG), 
    assert_all(ClBG,M,ClBGRef),
    assert_all(ThBG,ThBGRef)
%    assert_all(RBGRF,RBGRFRef)
  ;
    true
  ),
  set_sc(compiling,off),
  test_no_area([TE],NPos,NNeg,CLL,Results),
  % write(Results), 
  (M:bg(RBG0)->
    retract_all(ThBGRef),
%    retract_all(RBGRFRef),
    retract_all(ClBGRef)
  ;
    true
  ),
  retract_all(ThRef),
  retract_all(RFRef).
 
induce_rules(Folds,R):-
%tell(ciao),
  input_mod(M),
  make_dynamic(M),
  set_sc(compiling,on),
  M:local_setting(seed,Seed),
  setrand(Seed),
  %set_prolog_flag(unknown,warning),
  findall(Exs,(member(F,Folds),M:fold(F,Exs)),L),
  append(L,DB),
  assert(M:database(DB)),
  statistics(walltime,[_,_]),
%  findall(C,M:bg(C),RBG),
  (M:bg(RBG0)->
    process_clauses(RBG0,[],_,[],RBG),
    generate_clauses(RBG,_RBG1,0,[],ThBG), 
    generate_clauses_bg(RBG,ClBG), 
    assert_all(ThBG,M,ThBGRef),
    assert_all(ClBG,M,ClBGRef)
  ;
    true
  ),
  (M:local_setting(specialization,bottom)->
    M:local_setting(megaex_bottom,MB),
    deduct(MB,M,DB,[],InitialTheory),   
    length(InitialTheory,_LI),  
    remove_duplicates(InitialTheory,R1)
  ;
    get_head_atoms(O,M),
    generate_top_cl(O,R1)
  ),
  learn_struct(DB,M,R1,R2,Score2),
  learn_params(DB,M,R2,R,Score),  
  format2("~nRefinement score  ~f - score after EMBLEM ~f~n",[Score2,Score]),
  statistics(walltime,[_,WT]),
  WTS is WT/1000,
  write2('\n\n'),
  format2('/* SLIPCOVER Final score ~f~n',[Score]),
  format2('Wall time ~f */~n',[WTS]),
  write_rules2(R,user_output),
%  told,
  set_sc(compiling,off),
  (M:bg(RBG0)->
    retract_all(ThBGRef),
    retract_all(ClBGRef)
  ;
    true
  ).

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


/** 
 * sl(++FileStem:atom) is det
 *
 * The predicate performs structure learning for the problem stored in
 * the files FileStem.l (language bias), FileStem.kb (dataset), 
 * FileStem.bg (optional, background theory), FileStem.cpl (optional,
 * initial theory).
 * The result is stored in FileStem.rules
 */
sl(File):-
  setting_sc(seed,Seed),
  setrand(Seed),
  generate_file_names(File,FileKB,FileIn,FileBG,FileOut,FileL),
  reconsult(FileL),
  load_models(FileKB,DB),  
  assert(database(DB)),
  statistics(walltime,[_,_]),
  (exists_file(FileBG)->
    set_sc(compiling,on),
    load(FileBG,_ThBG,RBG),
    set_sc(compiling,off),
    generate_clauses(RBG,_RBG1,0,[],ThBG), 
    assert_all(ThBG,_ThBGRef)
  ;
    true
  ),
  (exists_file(FileIn)->
    set_sc(compiling,on),
    load(FileIn,_Th1,R1),
    set_sc(compiling,off)
  ;
    (setting_sc(specialization,bottom)->
      setting_sc(megaex_bottom,MB),
      deduct(MB,DB,[],InitialTheory),   
      length(InitialTheory,_LI),  
      remove_duplicates(InitialTheory,R1)
    ;
      get_head_atoms(O),
      generate_top_cl(O,R1)
    )
  ),
  learn_struct(DB,R1,R2,Score2),
  learn_params(DB,R2,R,Score),  
  statistics(walltime,[_,WT]),
  WTS is WT/1000,
  format2("~nRefinement score  ~f - score after EMBLEM ~f~n",[Score2,Score]),
  format2("Total execution time ~f~n~n",[WTS]),
  write_rules2(R,user_output),
  listing(setting_sc/2),
  open(FileOut,write,Stream),
  format(Stream,'/* SLIPCOVER Final score ~f~n',[Score]),
  format(Stream,'Execution time ~f~n',[WTS]),
  tell(Stream),
  listing(setting_sc/2),
  format(Stream,'*/~n~n',[]),
  told, 
  open(FileOut,append,Stream1),
  write_rules(R,Stream1),
  close(Stream1).

gen_fixed([],[]).

gen_fixed([(H,B,BL)|T],[rule(R,H,B,BL)|T1]):-
  get_next_rule_number(R), 
  gen_fixed(T,T1).


learn_struct(DB,Mod,R1,R,Score):-   %+R1:initial theory of the form [rule(NR,[h],[b]],...], -R:final theory of the same form, -CLL
  format2("Clause search~n~n",[]),
  Mod:local_setting(max_iter,M),
  Mod:local_setting(depth_bound,DepthB),
  set_sc(depth_bound,false),
  findall((H,B,BL),Mod:fixed_rule(H,B,BL),LF),
  length(LF,LLF),
  gen_fixed(LF,LFR),
  format2("Scoring fixed clauses: ~d clauses~n~n",[LLF]),
  score_clause_refinements(LFR,Mod,1,LLF,DB,[],NB1,[],CL0,[],CLBG0),
  append(NB1,R1,Beam),
  cycle_beam(Beam,Mod,DB,CL0,CL,CLBG0,BG,M),
  learn_params(DB,Mod,[],REmpty,S),
  set_sc(depth_bound,DepthB),
  format2("Theory search~n~n",[]),
  Mod:local_setting(max_iter_structure,MS),
  cycle_structure(CL,Mod,REmpty,S,-1e20,DB,R2,Score,MS),
  format2("Best target theory~n~n",[]),
  write_rules2(R2,user_output),
  Mod:local_setting(background_clauses,NBG1),
  length(BG,NBG),
  format2("Background search: ~d of ~d clauses~n~n",[NBG1,NBG]),
  pick_first(NBG1,BG,BG1),
  remove_score(BG,BG2),
  write_rules2(BG2,user_output),
  write2('\n'),
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
  already_scored([RH|R0],R3,Score),!,
  format2("Theory iteration ~d~n~n",[M]),
  write3('Already scored, updated refinement\n'),
  write_rules3(R3,user_output), 
  write3('Score '),write3(Score),write3('\n\n\n'),
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
  format2("Theory iteration ~d~n~n",[M]),
  generate_clauses([RH|R0],R2,0,[],Th1),
  format3("Initial theory~n~n",[]),
  write_rules3([RH|R0],user_output),
  assert_all(Th1,Mod,Th1Ref),  
  assert_all(R2,Mod,R2Ref),!,
  findall(R-HN,(Mod:rule(R,HL,_BL,_Lit),length(HL,HN)),L),  
  keysort(L,LS),
  get_heads(LS,LSH),  
  length(LSH,NR),
  init(NR,LSH,ExData),
  retractall(pita:v(_,_,_)),
  length(DB,NEx),  
  (Mod:local_setting(examples,atoms)->
    Mod:local_setting(group,G),
    derive_bdd_nodes_groupatoms(DB,Mod,ExData,NEx,G,[],Nodes,0,CLL0,LE,[]),!   % 1 BDD per example if G=1
  ;
    derive_bdd_nodes(DB,ExData,NEx,[],Nodes,0,CLL0),! % 1 BDD per model
  ),
  Mod:local_setting(random_restarts_number,N),
  format3("~nInitial CLL ~f~n~n",[CLL0]),
  random_restarts(N,ExData,Nodes,CLL0,Score,initial,Par,LE),   %output:CLL,Par
  format3("Score after EMBLEM = ~f~n",[Score]),
  retract_all(Th1Ref),
  retract_all(R2Ref),!,
  end(ExData),  
  update_theory(R2,Par,R3), 
  write3('Updated Theory\n'),
  write_rules3(R3,user_output),   %definite rules without probabilities in the head are not written
  (Score>S0->
    R4=R3,
    S4=Score,
    SP1=S0,
    write3('New best score\n')
  ;
    R4=R0,
    S4=S0,
    SP1=SP0
  ),
  store_refinement([RH|R0],R3,Score),
  M1 is M-1,
  cycle_structure(RT,Mod,R4,S4,SP1,DB,R,S,M1). 

/** 
 * induce_par(+TrainFolds:list_of_atoms,-P:probabilistic_program) is det
 *
 * The predicate learns the parameters of the program stored in the in/1 fact
 * of the input file using the folds indicated in TrainFolds for training. 
 * It returns in P the input program with the updated parameters.
 */
induce_par(Folds,ROut):-
  induce_parameters(Folds,R),
  rules2terms(R,ROut).

induce_parameters(Folds,R):-
  input_mod(M),
  make_dynamic(M),
  set_sc(compiling,on),
  M:local_setting(seed,Seed),
  setrand(Seed),
  findall(Exs,(member(F,Folds),M:fold(F,Exs)),L),
  append(L,DB),
  assert(M:database(DB)),
  statistics(walltime,[_,_]),
  (M:bg(RBG0)->
    process_clauses(RBG0,[],_,[],RBG),
    generate_clauses(RBG,_RBG1,0,[],ThBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ClBG,M,ClBGRef),
    assert_all(ThBG,ThBGRef)
  ;
    true
  ),
  M:in(R00),
  process_clauses(R00,[],_,[],R0),
  statistics(walltime,[_,_]),      
  learn_params(DB,M,R0,R,Score),
  statistics(walltime,[_,CT]),
  CTS is CT/1000,
  format2('/* EMBLEM Final score ~f~n',[Score]),
  format2('Wall time ~f */~n',[CTS]),
  write_rules2(R,user_output),
  set_sc(compiling,off),
  (M:bg(RBG0)->
    retract_all(ThBGRef),
    retract_all(ClBGRef)
  ;
    true
  ).

/** 
 * induce_par(+TrainFolds:list_of_atoms,+TestFolds:list_of_atoms,-P:probabilistic_program,-LL:float,-AUCROC:float,-ROC:dict,-AUCPR:float,-PR:dict) is det
 *
 * The predicate learns the parameters of the program stored in the in/1 fact
 * of the input file using the folds indicated in TrainFolds for training. 
 * It returns in P the input program with the updated parameters.
 * Moreover, it tests P on the folds indicated in TestFolds and returns the
 * log likelihood of the test examples in LL, the area under the Receiver
 * Operating Characteristic curve in AUCROC, a dict containing the points
 * of the ROC curve in ROC, the area under the Precision Recall curve in AUCPR
 * and a dict containing the points of the PR curve in PR
 */
induce_par(TrainFolds,TestFolds,ROut,CLL,AUCROC,ROC,AUCPR,PR):-
  induce_parameters(TrainFolds,R),
  rules2terms(R,ROut),
  write2('Testing\n'),
  input_mod(M),
  findall(Exs,(member(F,TestFolds),M:fold(F,Exs)),L),
  append(L,TE),
  set_sc(compiling,on),
  generate_clauses(R,RuleFacts,0,[],Th), 
  assert_all(Th,M,ThRef),
  assert_all(RuleFacts,M,RFRefs),
  set_sc(compiling,off),
  test([TE],CLL,AUCROC,ROC,AUCPR,PR),
  retract_all(ThRef),
  retract_all(RFRefs).
 

/** 
 * em(+FileStem:atom) is det
 *
 * The predicate performs parameter learning for the problem stored in
 * the files FileStem.l (language bias), FileStem.kb (dataset), 
 * FileStem.bg (optional, background theory), FileStem.cpl 
 * (theory).
 * The result is stored in FileStem.rules
 */
em(File):-
  generate_file_names(File,FileKB,FileIn,FileBG,FileOut,FileL),
  reconsult(FileL),
  load_models(FileKB,DB),
  (file_exists(FileBG)->
    set_sc(compiling,on),
    load(FileBG,_ThBG,RBG),
    set_sc(compiling,off),
    generate_clauses(RBG,_RBG1,0,[],ThBG), 
    assert_all(ThBG,_ThBGRef)
  ;
    true
  ),
  set_sc(compiling,on),
  load(FileIn,_TH,R0),
  set_sc(compiling,off),
  statistics(walltime,[_,_]),      
  learn_params(DB,R0,R,Score),
  statistics(walltime,[_,CT]),
  CTS is CT/1000,
  format2("EM: Final score ~f~n",[Score]),
  format2("Execution time ~f~n~n",[CTS]),
  write_rules2(R,user_output),
  listing(setting_sc/2),
  open(FileOut,write,Stream),
  format2(Stream,'/* EMBLEM Final score ~f~n',[Score]),
  format2(Stream,'Execution time ~f~n',[CTS]),
  tell(Stream),
  listing(setting_sc/2),
  format(Stream,'*/~n~n',[]),
  told,
  open(FileOut,append,Stream1),
  write_rules(R,Stream1),
  close(Stream1).

learn_params(DB,M,R0,R,Score):-  %Parameter Learning
  generate_clauses(R0,R1,0,[],Th0), 
  format2("Initial theory~n",[]),
  write_rules2(R1,user_output),
  assert_all(Th0,M,Th0Ref),
  assert_all(R1,M,R1Ref),!,
  findall(R-HN,(M:rule(R,HL,_BL,_Lit),length(HL,HN)),L),
  keysort(L,LS),
  get_heads(LS,LSH),
  length(LSH,NR),
  init(NR,LSH,ExData),
  retractall(pita:v(_,_,_)),
  length(DB,NEx),
  (M:local_setting(examples,atoms)->
    M:local_setting(group,G),  
    derive_bdd_nodes_groupatoms(DB,M,ExData,NEx,G,[],Nodes,0,CLL0,LE,[]),!   
  ; 
   derive_bdd_nodes(DB,ExData,NEx,[],Nodes,0,CLL0),!      
  ),
  format3("Initial score ~f~n",[CLL0]),
  M:local_setting(random_restarts_number,N),
  random_restarts(N,ExData,Nodes,-1e20,Score,initial,Par,LE),  %computes new parameters Par
  end(ExData),
  retract_all(Th0Ref),
  retract_all(R1Ref),!,
  update_theory_par(R1,Par,R).  %replaces in R1 the probabilities Par and outputs R


update_theory_par([],_Par,[]).

update_theory_par([def_rule(H,B,L)|T0],Par,[def_rule(H,B,L)|T]):-!,
  update_theory_par(T0,Par,T).

update_theory_par([(H:-B)|T0],Par,[(H:-B)|T]):-!,
  update_theory_par(T0,Par,T).

update_theory_par([rule(N,H,_B,_L)|T0],Par,T):-
  member([N,[1.0|_T]],Par),
  last(H,'':_P),!,  
  update_theory_par(T0,Par,T).

update_theory_par([rule(N,H,B,L)|T0],Par,[rule(N,H1,B,L)|T]):-
  member([N,P],Par),!, 
  reverse(P,P1),
  update_head_par(H,P1,H1),  
  update_theory_par(T0,Par,T).


update_theory(R,initial,R):-!.

update_theory([],_Par,[]).

update_theory([def_rule(H,B,L)|T0],Par,[def_rule(H,B,L)|T]):-!,
  update_theory(T0,Par,T).

update_theory([(H:-B)|T0],Par,[(H:-B)|T]):-!,
  update_theory(T0,Par,T).

update_theory([rule(N,H,B,L)|T0],Par,[rule(N,H1,B,L)|T]):-
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
  format2("Clause iteration ~d~n~n",[M]),
  cycle_clauses(Beam,Mod,DB,[],NB,CL0,CL1,CLBG0,CLBG1),
  M1 is M-1,%decreases the number of max_iter M
  cycle_beam(NB,Mod,DB,CL1,CL,CLBG1,CLBG,M1).

cycle_clauses([],_M,_DB,NB,NB,CL,CL,CLBG,CLBG):-!.

cycle_clauses([(RH,_ScoreH)|T],M,DB,NB0,NB,CL0,CL,CLBG0,CLBG):-
%  write3('\n\nRevising clause\n'),
%  write_rules3([RH],user_output),
%  RH=rule(_,H,B,Lits),
%  write3(H),write3(' '),write3(B),
%  write3('\n'),write3(Lits),write3('\n'),
  findall(RS,specialize_rule(RH,M,RS,_L),LR),!,   %-LR:list of lists, each one correponding to a different revised theory; specialize_rule defined in revise.pl
  length(LR,NR),
  write3('Number of revisions '),write3(NR),write3('\n'),
  score_clause_refinements(LR,M,1,NR,DB,NB0,NB1,CL0,CL1,CLBG0,CLBG1),
  cycle_clauses(T,M,DB,NB1,NB,CL1,CL,CLBG1,CLBG).

score_clause_refinements([],_M,_N,_NR,_DB,NB,NB,CL,CL,CLBG,CLBG).

score_clause_refinements([R1|T],M,Nrev,NRef,DB,NB0,NB,CL0,CL,CLBG0,CLBG):-  %scans the list of revised theories
  already_scored_clause(R1,R3,Score),!,
  format3('Score ref.  ~d of ~d~n',[Nrev,NRef]),
  write3('Already scored, updated refinement\n'),
  write_rules3([R3],user_output), 
  write3('Score '),write3(Score),write3('\n\n\n'),
  M:local_setting(beamsize,BS),
  insert_in_order(NB0,(R3,Score),BS,NB1),
  Nrev1 is Nrev+1,  
  score_clause_refinements(T,M,Nrev1,NRef,DB,NB1,NB,CL0,CL,CLBG0,CLBG).

score_clause_refinements([R1|T],M,Nrev,NRef,DB,NB0,NB,CL0,CL,CLBG0,CLBG):- 
  format3('Score ref.  ~d of ~d~n',[Nrev,NRef]),
  write_rules3([R1],user_output),   
  generate_clauses_cw([R1],[R2],0,[],Th1),
  assert_all(Th1,M,Th1Ref),
  assert_all([R2],M,[R2Ref]),!,
  findall(RN-HN,(M:rule(RN,HL,_BL,_Lit),length(HL,HN)),L),  
  keysort(L,LS),
  get_heads(LS,LSH),
  length(LSH,NR),
  init(NR,LSH,ExData),
  retractall(pita:v(_,_,_)),
  length(DB,NEx),
  get_output_preds(R1,O),
  (M:local_setting(examples,atoms)->
    M:local_setting(group,G),  
    derive_bdd_nodes_groupatoms_output_atoms(DB,M,ExData,O,NEx,G,[],Nodes,0,CLL0,LE,[]),!
  ; 
    derive_bdd_nodes(DB,ExData,NEx,[],Nodes,0,CLL0),!
  ),
  format3("Initial CLL ~f~n",[CLL0]),
  M:local_setting(random_restarts_REFnumber,N),
  random_restarts_ref(N,ExData,Nodes,CLL0,Score,initial,Par,LE),  
  end(ExData),
  update_theory([R2],Par,[R3]),
  write3('Updated refinement\n'),
  write_rules3([R3],user_output), 
  write3('Score (CLL) '),write3(Score),write3('\n\n\n'),
  retract_all(Th1Ref),
  retract_all([R2Ref]),!,
  M:local_setting(beamsize,BS),
  insert_in_order(NB0,(R3,Score),BS,NB1),
  (target(R3,M)->
    insert_in_order(CL0,(R3,Score),+1e20,CL1),
    length(CL1,LCL1),
    format2("N. of target clauses ~d~n~n",[LCL1]),
    CLBG1=CLBG0
  ;
    (range_restricted(R3)->
      insert_in_order(CLBG0,(R3,Score),+1e20,CLBG1),
      length(CLBG1,LCL1),
      format2("N. of background clauses ~d~n~n",[LCL1]),
      CL1=CL0
    ;
      format2("Not range restricted~n~n",[]),
      CL1=CL0,
      CLBG1=CLBG0
    )
  ),
  store_clause_refinement(R1,R3,Score),
  Nrev1 is Nrev+1,  
  score_clause_refinements(T,M,Nrev1,NRef,DB,NB1,NB,CL1,CL,CLBG1,CLBG).

range_restricted(rule(_N,HL,BL,_Lit)):-
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

get_output_preds(rule(_N,HL,_BL,_Lit),O):-
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



store_clause_refinement(Ref,RefP,Score):-
  elab_clause_ref(Ref,Ref1),
  input_mod(M),
  assert(M:ref_clause(r(Ref1,RefP,Score))).

store_refinement(Ref,RefP,Score):-
  elab_ref(Ref,Ref1),
  input_mod(M),
  assert(M:ref(r(Ref1,RefP,Score))).

already_scored_clause(R,R1,Score):-
  elab_ref([R],[rule(H,B)]),
  input_mod(M),
  M:ref_clause(r(rule(H,B1),R1,Score)),
  permutation(B,B1).

already_scored(R,R1,Score):-
  elab_ref(R,RR),
  input_mod(M),
  M:ref(r(RR,R1,Score)).


elab_clause_ref(rule(_NR,H,B,_Lits),rule(H1,B1)):-
  copy_term((H,B),(H1,B1)).

elab_ref([],[]).

elab_ref([rule(_NR,H,B,_Lits)|T],[rule(H1,B1)|T1]):-!,
  copy_term((H,B),(H1,B1)),
  numbervars((H1,B1),0,_N),
  elab_ref(T,T1).

elab_ref([def_rule(H,B,_Lits)|T],[rule(H1,B1)|T1]):-
  copy_term((H,B),(H1,B1)),
  numbervars((H1,B1),0,_N),
  elab_ref(T,T1).

%insertion in the beam
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

remove_int_atom_list([A|T],[A1|T1]):-
  A=..[F,_|Arg],
  A1=..[F|Arg],
  remove_int_atom_list(T,T1).



remove_int_atom(A,A1):-
  A=..[F,_|T],
  A1=..[F|T].


get_heads([],[]).

get_heads([_-H|T],[H|TN]):-
  get_heads(T,TN).

derive_bdd_nodes([],_ExData,_E,Nodes,Nodes,CLL,CLL).

derive_bdd_nodes([H|T],ExData,E,Nodes0,Nodes,CLL0,CLL):-
  get_output_atoms(O),
  generate_goal(O,H,[],GL),
  (prob(H,P)->
    CardEx is P*E
  
  ;
    CardEx is 1.0
  ),
  init_bdd(ExData,Env),
  one(Env,One),
  get_node_list(GL,Env,One,BDD,CardEx),
  ret_prob(Env,BDD,HP),
  (HP=:=0.0->
    setting_sc(logzero,LZ),
    CLL1 is CLL0+LZ*CardEx
  ;
    CLL1 is CLL0+log(HP)*CardEx
  ),
  end_bdd(ExData),
  append(Nodes0,[[BDD,CardEx]],Nodes1),
  derive_bdd_nodes(T,ExData,E,Nodes1,Nodes,CLL1,CLL).


get_node_list([],_Env,BDD,BDD,_CE).


get_node_list([H|T],Env,BDD0,BDD,CE):-
  get_node(H,Env,BDD1),
  and(Env,BDD0,BDD1,BDD2),
  get_node_list(T,Env,BDD2,BDD,CE).


derive_bdd_nodes_groupatoms_output_atoms([],_M,_ExData,_O,_E,_G,Nodes,Nodes,CLL,CLL,LE,LE).

derive_bdd_nodes_groupatoms_output_atoms([H|T],M,ExData,O,E,G,Nodes0,Nodes,CLL0,CLL,LE0,LE):-  
  generate_goal(O,M,H,[],GL),
  length(GL,NA),
  (M:prob(H,P)->
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
  (M:prob(H,P)->
    CardEx is P*E/NA
  ;
    CardEx is 1.0
  ),
  get_node_list_groupatoms(GL,M,ExData,BDDs,CardEx,G,CLL0,CLL1,LE0,LE1),
  append(Nodes0,BDDs,Nodes1),
  derive_bdd_nodes_groupatoms(T,M,ExData,E,G,Nodes1,Nodes,CLL1,CLL,LE1,LE).

get_node_list_groupatoms([],_M,_ExData,[],_CE,_Gmax,CLL,CLL,LE,LE).

get_node_list_groupatoms([H|T],M,ExData,[[BDD,CE1]|BDDT],CE,Gmax,CLL0,CLL,LE0,LE):-
  init_bdd(ExData,Env),  
  one(Env,One),
  get_bdd_group([H|T],M,Env,T1,Gmax,G,One,BDD,CE,LE0,LE1),  %output:BDD,CLL
  CE1 is CE*(Gmax-G),
  ret_prob(Env,BDD,HP),
  end_bdd(ExData),
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
  get_node(H,M,Env,BDD1),  		%creates the BDD for atom H
  and(Env,BDD0,BDD1,BDD2),
  G is Gmax-1,
  get_bdd_group(T,M,Env,T1,G,G1,BDD2,BDD,CE,LE0,LE).


/* EM start */
random_restarts(0,_ExData,_Nodes,Score,Score,Par,Par,_LE):-!.

random_restarts(N,ExData,Nodes,Score0,Score,Par0,Par,LE):-
  input_mod(M),
  M:local_setting(random_restarts_number,NMax),
  Num is NMax-N+1,
  format3("Restart number ~d~n~n",[Num]),
  randomize(ExData),
  M:local_setting(epsilon_em,EA),
  M:local_setting(epsilon_em_fraction,ER),
  length(Nodes,L),
  M:local_setting(iter,Iter),
  em(ExData,Nodes,EA,ER,L,Iter,CLL,Par1,ExP),  
  score(LE,ExP,CLL,ScoreR),
  format3("Random_restart: Score ~f~n",[ScoreR]),
  N1 is N-1,
  (ScoreR>Score0->     
    random_restarts(N1,ExData,Nodes,ScoreR,Score,Par1,Par,LE)
  ;
    random_restarts(N1,ExData,Nodes,Score0,Score,Par0,Par,LE)
  ).

random_restarts_ref(0,_ExData,_Nodes,Score,Score,Par,Par,_LE):-!.

random_restarts_ref(N,ExData,Nodes,Score0,Score,Par0,Par,LE):-
  input_mod(M),
  M:local_setting(random_restarts_REFnumber,NMax),
  Num is NMax-N+1,
  format3("Restart number ~d~n~n",[Num]),
  M:local_setting(epsilon_em,EA),
  M:local_setting(epsilon_em_fraction,ER),
  length(Nodes,L),
  M:local_setting(iterREF,Iter),
  em(ExData,Nodes,EA,ER,L,Iter,CLLR,Par1,ExP),  
  score(LE,ExP,CLLR,ScoreR),
  format3("Random_restart: Score ~f~n",[ScoreR]),
  N1 is N-1,
  (ScoreR>Score0->  
    random_restarts_ref(N1,ExData,Nodes,ScoreR,Score,Par1,Par,LE)
  ;
    random_restarts_ref(N1,ExData,Nodes,Score0,Score,Par0,Par,LE)
  ).


score(_LE,_ExP,CLL,CLL):-
  input_mod(M),
  M:local_setting(score,ll),!.

score(LE,ExP,_CLL,Score):-
  compute_prob(LE,ExP,LPU,0,Pos,0,Neg),
  keysort(LPU,LPO),
  reverse(LPO,LP),
  compute_aucpr(LP,Pos,Neg,Score).


compute_prob([],[],[],Pos,Pos,Neg,Neg).

compute_prob([\+ HE|TE],[HP|TP],[P- (\+ HE)|T],Pos0,Pos,Neg0,Neg):-!,
  P is 1-HP,
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
    FPB is TPB*(1-P0)/P0,
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


randomize([],[]):-!.

randomize([rule(N,V,NH,HL,BL,LogF)|T],[rule(N,V,NH,HL1,BL,LogF)|T1]):-
  length(HL,L),
  Int is 1.0/L,
  randomize_head(Int,HL,0,HL1),
  randomize(T,T1).

randomize_head(_Int,['':_],P,['':PNull1]):-!,
  PNull is 1.0-P,
  (PNull>=0.0->
    PNull1 =PNull
  ;
    PNull1=0.0
  ).
  
randomize_head(Int,[H:_|T],P,[H:PH1|NT]):-
  PMax is 1.0-P,
  random(0,PMax,PH1),
  P1 is P+PH1,  
  randomize_head(Int,T,P1,NT).



update_head([],[],_N,[]):-!.  

update_head([H:_P|T],[PU|TP],N,[H:P|T1]):-
  P is PU/N,
  update_head(T,TP,N,T1).


/* EM end */    
  
  
/* utilities */

generate_file_names(File,FileKB,FileIn,FileBG,FileOut,FileL):-
  atom_concat(File,'.kb',FileKB),
  atom_concat(File,'.cpl',FileIn),
  atom_concat(File,'.rules',FileOut),
  atom_concat(File,'.bg',FileBG),
  atom_concat(File,'.l',FileL).

   
load_models(File,ModulesList):-  %carica le interpretazioni, 1 alla volta
  open(File,read,Stream),
  read_models(Stream,ModulesList),
  close(Stream).


read_models(Stream,[Name1|Names]):-
  read(Stream,begin(model(Name))),!,
  (number(Name)->
     name(Name,NameStr),
     append("i",NameStr,Name1Str),
     name(Name1,Name1Str)
  ;
     Name1=Name
  ),
  read_all_atoms(Stream,Name1),
  read_models(Stream,Names).

read_models(_S,[]).


read_all_atoms(Stream,Name):-
  read(Stream,At),
  At \=end(model(_Name)),!,
  (At=neg(Atom)->    
    Atom=..[Pred|Args],
    Atom1=..[Pred,Name|Args],
    assertz(neg(Atom1))
  ;
    (At=prob(Pr)->
      assertz(prob(Name,Pr))
    ;
      At=..[Pred|Args],
      Atom1=..[Pred,Name|Args],
      assertz(Atom1)
    )
  ),
  read_all_atoms(Stream,Name).    

read_all_atoms(_S,_N).


rules2terms(R,T):-
  maplist(rule2term,R,T).

rule2term(rule(_N,HL,BL,_Lit),(H:-B)):-
  list2or(HL,H),
  list2and(BL,B).

rule2term(def_rule(H,BL,_Lit),((H:1.0):-B)):-
  list2and(BL,B).


write_rules([],_S).

write_rules([rule(_N,HL,BL,Lit)|T],S):-!,
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
    ( HL \== [] ->
       (generate_body(HL,Mod,InTheory1),
    	append(InTheory0,InTheory1,InTheory2),
    	NM1 is NM-1,
    	deduct(NM1,Mod,DB1,InTheory2,InTheory)
       )
      ;
       deduct(NM,Mod,DB,InTheory0,InTheory)
    )
  ;
    InTheory=InTheory0
  ).


get_head_atoms(O,M):-
  findall(A,M:modeh(_,A),O0),
  findall((A,B,D),M:modeh(_,A,B,D),O1),
  append(O0,O1,O).

generate_top_cl([],[]):-!.

generate_top_cl([A|T],[(rule(R,[A1:0.5,'':0.5],[],true),-1e20)|TR]):-
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  A1=..[F|Arg],
  get_next_rule_number(R),
  generate_top_cl(T,TR).


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

keep_const([H|T],[H|T1]):-
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
  append(B0,BL,B1),
  get_modeb(T,Mod,B1,B).

generate_body([],_Mod,[]):-!.

generate_body([(A,H,Det)|T],Mod,[(rule(R,HP,[],BodyList),-1e20)|CL0]):-!,
  get_modeb(Det,Mod,[],BL),
  get_args(A,H,Pairs,[],Args,[],ArgsTypes,M),
  Mod:local_setting(d,D),
  cycle_modeb(ArgsTypes,Args,[],[],Mod,BL,a,[],BLout0,D,M),
    variabilize((Pairs:-BLout0),CLV),  %+(Head):-Bodylist;  -CLV:(Head):-Bodylist with variables _num in place of constants
  CLV=(Head1:-BodyList1),
  remove_int_atom_list(Head1,Head),
  remove_int_atom_list(BodyList1,BodyList2),
  remove_duplicates(BodyList2,BodyList),
  get_next_rule_number(R),
  length(Head,LH),
  Prob is 1/(LH+1),
  gen_head(Head,Prob,HP),
  copy_term((HP,BodyList),(HeadV,BodyListV)),
  numbervars((HeadV,BodyListV),0,_V),
  format2("Bottom clause: example ~q~nClause~n",[H]),
  write_disj_clause2(user_output,(HeadV:-BodyListV)),
  generate_body(T,Mod,CL0).

generate_body([(A,H)|T],Mod,[(rule(R,[Head:0.5,'':0.5],[],BodyList),-1e20)|CL0]):-
  functor(A,F,AA),
  findall((R,B),(Mod:modeb(R,B),functor(B,FB,AB),Mod:determination(F/AA,FB/AB)),BL),
  A=..[F|ArgsTypes],
  H=..[F,M|Args],
  Mod:local_setting(d,D),
  cycle_modeb(ArgsTypes,Args,[],[],Mod,BL,a,[],BLout0,D,M),
  variabilize(([(H,A)]:-BLout0),CLV),  %+(Head):-Bodylist;  -CLV:(Head):-Bodylist with variables _num in place of constants
  CLV=([Head1]:-BodyList1),
  remove_int_atom(Head1,Head),
  remove_int_atom_list(BodyList1,BodyList2),
  remove_duplicates(BodyList2,BodyList),
  get_next_rule_number(R),
  copy_term((Head,BodyList),(HeadV,BodyListV)),
  numbervars((HeadV,BodyListV),0,_V),
  format2("Bottom clause: example ~q~nClause~n~q:0.5 :-~n",[H,HeadV]),
  write_body2(user_output,BodyListV),
  generate_body(T,Mod,CL0).


variabilize((H:-B),(H1:-B1)):-
  variabilize_list(H,H1,[],AS,M),
  variabilize_list(B,B1,AS,_AS,M).


variabilize_list([],[],A,A,_M).

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
  (Ty = +Ty1;Ty = -Ty1),
  variabilize_args(T,TT,TV,[C/Ty1/V|A0],A).


cycle_modeb(ArgsTypes,Args,ArgsTypes,Args,_Mod,_BL,L,L,L,_,_M):-!.

cycle_modeb(_ArgsTypes,_Args,_ArgsTypes1,_Args1,_Mod,_BL,_L,L,L,0,_M):-!.

cycle_modeb(ArgsTypes,Args,_ArgsTypes0,_Args0,Mod,BL,_L0,L1,L,D,M):-
  find_atoms(BL,Mod,ArgsTypes,Args,ArgsTypes1,Args1,L1,L2,M),
  D1 is D-1,
  cycle_modeb(ArgsTypes1,Args1,ArgsTypes,Args,Mod,BL,L1,L2,L,D1,M).


find_atoms([],_Mod,ArgsTypes,Args,ArgsTypes,Args,L,L,_M).

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
  add_const(T,TT,ArgsTypes0,Args0,ArgsTypes,Args).


already_present([+T|_TT],[C|_TC],C,T):-!.

already_present([_|TT],[_|TC],C,T):-
  already_present(TT,TC,C,T).


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


instantiate_input([C|T],AT,A,[C|TA]):-
  instantiate_input(T,AT,A,TA).


find_val([T|_TT],[A|_TA],T,A).

find_val([_T|TT],[_A|TA],T,A):-
  find_val(TT,TA,T,A).


get_output_atoms(O,M):-
  findall((A/Ar),M:output((A/Ar)),O).


generate_goal([],_M,_H,G,G):-!.

generate_goal([P/A|T],M,H,G0,G1):-
  functor(Pred,P,A),
  Pred=..[P|Rest],
  Pred1=..[P,H|Rest],
  findall(Pred1,call(M:Pred1),L),
  findall(\+ Pred1,call(M:neg(Pred1)),LN),
  append(G0,L,G2),
  append(G2,LN,G3),
  generate_goal(T,M,H,G3,G1).

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


theory_revisions_op(Theory,TheoryRevs):-
  setof(RevOp, Theory^revise_theory(Theory,RevOp), TheoryRevs),!.

theory_revisions_op(_Theory,[]).


theory_revisions(Theory,TheoryRevs):-
  theory_revisions_op(Theory,TheoryRevs1),
  apply_operators(TheoryRevs1,Theory,TheoryRevs).


apply_operators([],_Theory,[]).

apply_operators([add(Rule)|RestOps],Theory,[NewTheory|RestTheory]) :-
  append(Theory, [Rule], NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([add_body(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove_body(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([add_head(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove_head(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove(Rule)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule,NewTheory),
  apply_operators(RestOps,Theory,RestTheory).


revise_theory(Theory,Ref):-
  specialize_theory(Theory,Ref).

revise_theory(Theory,Ref):-
  generalize_theory(Theory,Ref).


generalize_theory(Theory,Ref):-
  Theory \== [],
  choose_rule(Theory,Rule),
  generalize_rule(Rule,Ref).

generalize_theory(Theory,Ref):-
  length(Theory,LT),
  setting_sc(max_rules,MR),
  LT<MR,
  add_rule(Ref).


generalize_rule(Rule,Ref):-
  generalize_head(Rule,Ref).

generalize_rule(Rule,Ref):-
  generalize_body(Rule,Ref).


add_rule(add(rule(ID,Head,[],Lits))):-
  setting_sc(specialization,bottom),!,
  database(DB),
  sample(1,DB,[M]),
  get_head_atoms(O),
  member(A,O),
  functor(A,F,N),    
  functor(F1,F,N),   
  F1=..[F|Arg],
  Pred1=..[F,M|Arg],
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  findall((A,Pred1),call(Pred1),L),
  sample(1,L,LH),
  generate_body(LH,[rule(ID,Head,[],Lits)]).

add_rule(add(rule(ID,Head,[],true))):-
  findall(HL , modeh(_,HL), HLS),
  length(HLS,L),
  L1 is L+1,
  P is 1/L1,
  generate_head(HLS,P,Head),
  get_next_rule_number(ID).


generate_head([H|_T],_P,[H1:0.5,'':0.5]):-
  H=..[Pred|Args],
  length(Args,LA),
  length(Args1,LA),
  H1=..[Pred|Args1].

generate_head([_H|T],P,Head):-
  generate_head(T,P,Head).


generalize_head(Rule,Ref):-
  Rule = rule(ID,LH,BL),
  generalize_head1(LH,LH1,NewAt),
  Ref = add_head(Rule,rule(ID,LH1,BL),NewAt).


generalize_head1(LH,LH1,NH):-
  findall(HL , modeh(_,HL), HLS),
  generalize_head2(HLS,LH,LH1,NH).


generalize_head2([X|_R],LH,LH1,PH) :-
  X =.. [P|A],
  length(A,LA),
  length(A1,LA),
  PH =.. [P|A1],
  \+ member(PH:_, LH),
  (setting_sc(new_head_atoms_zero_prob,true)->
    delete_matching(LH,'':PNull,LH0),
    append(LH0,[PH:0.0,'':PNull],LH1)
  ;
    length(LH,NH),
    add_to_head(LH,NH,PH,LH1)
  ).

generalize_head2([_X|R],LH,LH1) :-
  generalize_head2(R,LH,LH1).


add_to_head(['':PN],NH,At,[At:PA,'':PN1]):-!,
  PN1 is PN*NH/(NH+1),
  PA is 1/(NH+1).

add_to_head([H:PH|T],NH,At,[H:PH1|T1]):-
  PH1 is PH*NH/(NH+1),
  add_to_head(T,NH,At,T1).
  

get_module_var(LH,Module):-
  member(H:_,LH),!,
  H=..[_F,Module|_].


generalize_body(Rule,Ref):-
  Rule = rule(ID,LH,BL),
  delete_one(BL,BL1,A),
  remove_prob(LH,LH1),
  delete(LH1,'',LH2),
  linked_clause(BL1,LH2),
  Ref = remove_body(Rule,rule(ID,LH,BL1),A).
  

specialize_theory(Theory,Ref):-
  Theory \== [],
  choose_rule(Theory,Rule),
  specialize_rule(Rule,SpecRule,Lit),
  Ref = add_body(Rule,SpecRule,Lit).

%used by cycle_clauses in slipcover.pl
specialize_rule(Rule,M,SpecRule,Lit):-
  M:local_setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
  delete_one(Lits,RLits,Lit),
  \+ M:lookahead_cons(Lit,_),
  \+ M:lookahead_cons_var(Lit,_),
  \+ member_eq(Lit,BL), 
  append(BL,[Lit],BL1),
  remove_prob(LH,LH1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  dv(LH2,BL1,M,DList), 	%-DList: list of couples (variable,depth)
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,M,LH2),
  M:local_setting(maxdepth_var,MD),
  exceed_depth(DList,MD),  
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,RLits).

specialize_rule(Rule,M,SpecRule,Lit):-
  M:local_setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
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
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,RLits1).

specialize_rule(Rule,M,SpecRule,Lit):-
  M:local_setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
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
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,[]).

specialize_rule(Rule,M,SpecRule,Lit):-
  M:local_setting(specialization,mode),%!,
  findall(BL , M:modeb(_,BL), BLS),
  specialize_rule(BLS,Rule,M,SpecRule,Lit).

%specializes the clause's head
specialize_rule(rule(ID,LH,BL,Lits),M,rule(ID,LH2,BL,Lits),Lit):-
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


banned_clause(H,B):-
  input_mod(M),
  numbervars((H,B),0,_N),
  M:banned(H2,B2),
  mysublist(H2,H),
  mysublist(B2,B).


mysublist([],_).

mysublist([H|T],L):-
  member(H,L),
  mysublist(T,L).


specialize_rule([Lit|_RLit],Rule,M,SpecRul,SLit):-
  Rule = rule(ID,LH,BL,true),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,ALL,SLit),
  append(BL,[SLit],BL1),
  (M:lookahead(SLit,LLit1);M:lookahead_cons(SLit,LLit1)),
  specialize_rule_la(LLit1,LH1,BL1,BL2),
  append(LH1,BL2,ALL2),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  SpecRul = rule(ID,LH,BL2,true).

specialize_rule([Lit|_RLit],Rule,M,SpecRul,SLit):-
  Rule = rule(ID,LH,BL,true),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,ALL,SLit),
  \+ M:lookahead_cons(SLit,_),
  append(BL,[SLit],BL1),
  append(LH1,BL1,ALL1),
  extract_fancy_vars(ALL1,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  SpecRul = rule(ID,LH,BL1,true).

specialize_rule([_|RLit],Rule,M,SpecRul,Lit):-
  specialize_rule(RLit,Rule,M,SpecRul,Lit).


specialize_rule_la([],_LH1,BL1,BL1).

specialize_rule_la([Lit1|T],LH1,BL1,BL3):-
  copy_term(Lit1,Lit2),
  input_mod(M),
  M:modeb(_,Lit2),
  append(LH1,BL1,ALL1),
  specialize_rule1(Lit2,ALL1,SLit1),
  append(BL1,[SLit1],BL2),
  specialize_rule_la(T,LH1,BL2,BL3).


specialize_rule_la_bot([],Bot,Bot,BL,BL).

specialize_rule_la_bot([Lit|T],Bot0,Bot,BL1,BL3):-
  delete_one(Bot0,Bot1,Lit),
  \+ member_eq(Lit,BL1),
  append(BL1,[Lit],BL2),
  specialize_rule_la_bot(T,Bot1,Bot,BL2,BL3).


remove_prob(['':_P],[]):-!.

remove_prob([X:_|R],[X|R1]):-
  remove_prob(R,R1).


specialize_rule1(Lit,Lits,SpecLit):-
  Lit =.. [Pred|Args],
  exctract_type_vars(Lits,TypeVars0),  
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


linked_clause(X):-
  linked_clause(X,[]).

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
  copy_term(LitM,Lit0),
  M:modeb(_,Lit1),
  Lit1 =.. [P|Args1],
  convert_to_input_vars(Args1,Args2),
  Lit2 =.. [P|Args2],
  input_vars(Lit0,Lit2,InputVars).

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


exctract_type_vars([],[]).

exctract_type_vars([Lit|RestLit],TypeVars):-
  Lit =.. [Pred|Args],
  length(Args,L),
  length(Args1,L),
  Lit1 =.. [Pred|Args1],
  take_mode(Lit1),
  type_vars(Args,Args1,Types),
  exctract_type_vars(RestLit,TypeVars0),
  !,
  append(Types,TypeVars0,TypeVars).


take_mode(Lit):-
  input_mod(M),
  M:modeh(_,Lit),!.

take_mode(Lit):-
  input_mod(M),
  M:modeb(_,Lit),!.

take_mode(Lit):-
  input_mod(M),
  M:mode(_,Lit),!.


type_vars([],[],[]).

type_vars([V|RV],[+T|RT],[V=T|RTV]):-
  !,
  type_vars(RV,RT,RTV).

type_vars([V|RV],[-T|RT],[V=T|RTV]):-atom(T),!,
  type_vars(RV,RT,RTV).

type_vars([_V|RV],[_T|RT],RTV):-
  type_vars(RV,RT,RTV).


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


choose_rule(Theory,Rule):-
  member(Rule,Theory).


add_rule(Theory,add(rule(ID,H,[],true))):-
  new_id(ID),
  findall(HL , modeh(_,HL), HLS),
  length(HLS,NH),
  P is 1/(NH+1),
  add_probs(HLS,H,P),
  \+ member(rule(_,H,[],true),Theory).

add_rule(Theory,TheoryGen):-
  findall(HL , modeh(_,HL), HLS),
  add_rule(HLS,Theory,TheoryGen).

add_rule([X|_R],Theory,TheoryGen) :-
  new_id(ID),
  X =.. [P|A],
  length(A,LA),
  length(A1,LA),
  PH =.. [P|A1],
  TheoryGen = add(rule(ID,[PH:0.5,'':0.5],[],true)),
  \+ member(rule(_,[PH:_,'':_],[],true),Theory).

add_rule([_X|R],Theory,TheoryGen) :-
  add_rule(R,Theory,TheoryGen).


add_probs([],['':P],P):-!.

add_probs([H|T],[H:P|T1],P):-
  add_probs(T,T1,P).


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


delete_one([X|R],R,X).

delete_one([X|R],[X|R1],D):-
  delete_one(R,R1,D).


remove_last([_X],[]) :-
  !.

remove_last([X|R],[X|R1]):-
  remove_last(R,R1).


delete_matching([],_El,[]).

delete_matching([El|T],El,T1):-!,
  delete_matching(T,El,T1).

delete_matching([H|T],El,[H|T1]):-
  delete_matching(T,El,T1).
 

%Computation of the depth of the variables in the clause's head/body
dv(H,B,M,DV1):-			%DV1: returns a list of couples (Variable, Max depth)
	term_variables(H,V),
	head_depth(V,DV0),
	findall((MD-DV),var_depth(B,M,DV0,DV,0,MD),LDs), 
        get_max(LDs,-1,-,DV1).
	

input_variables_b(LitM,M,InputVars):-
	  LitM=..[P|Args],
	  length(Args,LA),
	  length(Args1,LA),
	  Lit1=..[P|Args1],
	  M:modeb(_,Lit1),
	  input_vars(LitM,Lit1,InputVars).



%associates depth 0 to each variable in the clause's head
head_depth([],[]).
head_depth([V|R],[[V,0]|R1]):-
  head_depth(R,R1).

%associates a depth to each variable in the clause's body
var_depth([],_M,PrevDs1,PrevDs1,MD,MD):-!.

var_depth([L|R],M,PrevDs,PrevDs1,_MD,MD):-    		%L = a body literal, MD = maximum depth set by the user
  input_variables_b(L,M,InputVars),          	
  term_variables(L, BodyAtomVars),   		   
  output_vars(BodyAtomVars,InputVars,OutputVars),       
  depth_InputVars(InputVars,PrevDs,0,MaxD),   		%MaxD: maximum depth of the input variables in the body literal
  D is MaxD+1,
  compute_depth(OutputVars,D,PrevDs,PrevDs0), 		%Computes the depth for the output variables in the body literal
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

 

%checks if a variable's depth exceeds the setting_sc
exceed_depth([],_):-!.
exceed_depth([H|T],MD):-
	nth1(2,H,Dep),	
	Dep<MD, %setting_sc(maxdepth_var,MD),
	exceed_depth(T,MD).

/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzi and Elena Bellodi

*/




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


assert_all([],_M,[]).

assert_all([H|T],M,[HRef|TRef]):-
  assertz(M:H,HRef),
  assert_all(T,M,TRef).

assert_all([],[]).

assert_all([H|T],[HRef|TRef]):-
  assertz(slipcover:H,HRef),
  assert_all(T,TRef).


retract_all([],_):-!.

retract_all([H|T],M):-
  erase(M,H),
  retract_all(T,M).

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
  input_mod(M),
  retract(M:rule_sc_n(R)),
  R1 is R+1,
  assert(M:rule_sc_n(R1)).


get_node(\+ Goal,M,Env,BDD):-
  M:local_setting(depth_bound,true),!,
  M:local_setting(depth,DB),
  retractall(pita:v(_,_,_)),
  add_bdd_arg_db(Goal,Env,BDD,DB,Goal1),
  (bagof(BDD,M:Goal1,L)->
    or_list(L,Env,B)
  ;
    zero(Env,B)
  ),
  bdd_not(Env,B,BDD).

get_node(\+ Goal,M,Env,BDD):-!,
  retractall(pita:v(_,_,_)),
  add_bdd_arg(Goal,Env,BDD,Goal1),
  (bagof(BDD,M:Goal1,L)->
    or_list(L,Env,B)
  ;
    zero(Env,B)
  ),
  bdd_not(Env,B,BDD).

get_node(Goal,M,Env,B):-
  M:local_setting(depth_bound,true),!,
  M:local_setting(depth,DB),
  retractall(pita:v(_,_,_)),
  add_bdd_arg_db(Goal,Env,BDD,DB,Goal1),%DB=depth bound
  (bagof(BDD,M:Goal1,L)->
    or_list(L,Env,B)
  ;
    zero(Env,B)
  ).

get_node(Goal,M,Env,B):- %with DB=false
  retractall(pita:v(_,_,_)),
  add_bdd_arg(Goal,Env,BDD,Goal1),
  (bagof(BDD,M:Goal1,L)->
    or_list(L,Env,B)
  ;  
    zero(Env,B)
  ).


s(Goal,P,CPUTime1,0,WallTime1,0):-
  statistics(cputime,[_,_]),
  statistics(walltime,[_,_]),
    init,
    retractall(pita:v(_,_,_)),
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


/*get_var_n(Env,R,S,Probs,V):-
  (v(R,S,V)->
    true
  ;
    length(Probs,L),
    add_var(Env,L,Probs,R,V),    
    assert(v(R,S,V))
  ).
*/
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


generate_rules_fact([],_Env,_VC,_R,_Probs,_N,[],_Module).

generate_rules_fact([Head:_P1,'':_P2],Env,VC,R,Probs,N,[Clause],Module):-!,
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,N,BDD))).

generate_rules_fact([Head:_P|T],Env,VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,N,BDD))),
  N1 is N+1,
  generate_rules_fact(T,Env,VC,R,Probs,N1,Clauses,Module).


generate_rules_fact_db([],_Env,_VC,_R,_Probs,_N,[],_Module).

generate_rules_fact_db([Head:_P1,'':_P2],Env,VC,R,Probs,N,[Clause],Module):-!,
  add_bdd_arg_db(Head,Env,BDD,_DB,Module,Head1),
  Clause=(Head1:-(pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,N,BDD))).

generate_rules_fact_db([Head:_P|T],Env,VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg_db(Head,Env,BDD,_DB,Module,Head1),
  Clause=(Head1:-(pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,N,BDD))),
  N1 is N+1,
  generate_rules_fact_db(T,Env,VC,R,Probs,N1,Clauses,Module).


generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module):-
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(Body,pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,N,B),pita:and(Env,BDDAnd,B,BDD))).


generate_clause_db(Head,Env,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module):-
  add_bdd_arg_db(Head,Env,BDD,DBH,Module,Head1),
  Clause=(Head1:-(DBH>=1,DB is DBH-1,Body,pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,N,B),pita:and(Env,BDDAnd,B,BDD))).


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



process_body([],BDD,BDD,Vars,Vars,[],_Env,_Module).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Env,Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).
  
process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Env,Module):-
  db(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[
(((neg(H1);\+ H1),pita:one(Env,BDDN));
  (bagof(BDDH,H2,L)->pita:or_list(L,Env,BDDL),pita:bdd_not(Env,BDDL,BDDN);
  pita:one(Env,BDDN))),
  pita:and(Env,BDD,BDDN,BDD2)
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
[(bagof(BDDH,H1,L)->pita:or_list(L,Env,BDDL),pita:bdd_not(Env,BDDL,BDDN);pita:one(Env,BDDN)),
  pita:and(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
  add_bdd_arg(H,Env,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Env,Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Env,Module):-
  db(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,
[((H1,pita:one(Env,BDDH));H2),pita:and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-
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
[H1,pita:and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-
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
  (((neg(H1);\+ H1),pita:one(Env,BDDN));
    (bagof(BDDH,H2,L)->pita:or_list(L,Env,BDDL),pita:bdd_not(Env,BDDL,BDDN);
      pita:one(Env,BDDN))),
  pita:and(Env,BDD,BDDN,BDD2)
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
[(bagof(BDDH,H1,L)->pita:or_list(L,Env,BDDL),pita:bdd_not(Env,BDDL,BDDN);pita:one(Env,BDDN)),
  pita:and(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
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
(((neg(H1);\+ H1),pita:one(Env,BDDN));
  (bagof(BDDH,H2,L)->pita:or_list(L,Env,BDDL),pita:bdd_not(Env,BDDL,BDDN);
    pita:one(Env,BDDN))),
  pita:and(Env,BDD,BDDN,BDD2)
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
[(bagof(BDDH,H1,L)->pita:or_list(L,Env,BDDL),pita:bdd_not(Env,BDDL,BDDN);pita:one(Env,BDDN)),
  pita:and(Env,BDD,BDDN,BDD2)|Rest],Env,Module):-!,
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  db(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,
[((H1,pita:one(Env,BDDH));H2),pita:and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-
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
[H1,pita:and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-!, %agg. cut
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  db(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,
[((H1,pita:one(Env,BDDH));H2),pita:and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-
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
[H1,pita:and(Env,BDD,BDDH,BDD2)|Rest],Env,Module):-!, %agg. cut
  add_bdd_arg_db(H,Env,BDDH,DB,Module,H1),
  process_body_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Env,Module).



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
(((neg(H1);\+ H1),pita:one(BDDN));(bagof(BDDH,H2,L)->pita:or_list(L,BDDL),pita:bdd_not(BDDL,BDDN);
  pita:one(BDDN))),
  pita:and(BDD,BDDN,BDD2)
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
[(bagof(BDDH,H1,L)->pita:or_list(L,BDDL),pita:bdd_not(BDDL,BDDN);pita:one(BDDN)),
  pita:and(BDD,BDDN,BDD2)|Rest],Module):-!,
  add_bdd_arg(H,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Module):-
  db(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,
[((H1,pita:one(BDDH));H2),pita:and(BDD,BDDH,BDD2)|Rest],Module):-
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
[H1,pita:and(BDD,BDDH,BDD2)|Rest],Module):-
  add_bdd_arg(H,BDDH,Module,H1),
  process_body(T,BDD2,BDD1,Vars,Vars1,Rest,Module).


given(H):-
  input_mod(M),
  functor(H,P,Ar),
  (M:input(P/Ar)).


given_cw(H):-
  input_mod(M),
  functor(H,P,Ar),
  (M:input_cw(P/Ar)).


and_list([],B,B).

and_list([H|T],B0,B1):-
  and(B0,H,B2),
  and_list(T,B2,B1).

/*
or_list([H],_Env,H):-!.

or_list([H|T],Env,B):-
  or_list1(T,Env,H,B).


or_list1([],_Env,B,B).

or_list1([H|T],Env,B0,B1):-
  or(Env,B0,H,B2),
  or_list1(T,Env,B2,B1).
*/

/** 
 * set_sc(+Parameter:atom,+Value:term) is det
 *
 * The predicate sets the value of a parameter
 * For a list of parameters see 
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or 
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
set_sc(Parameter,Value):-
  input_mod(M),
  retract(M:local_setting(Parameter,_)),
  assert(M:local_setting(Parameter,Value)).

/** 
 * setting_sc(+Parameter:atom,-Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see 
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or 
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_sc(P,V):-
  input_mod(M),
  M:local_setting(P,V).

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
  input_mod(M),
  M:local_setting(epsilon_parsing, Eps), 
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
  append([pita:one(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  input_mod(M),
  (M:local_setting(single_var,true)->
    generate_rules(HeadList,Env,Body1,[],N,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_rules(HeadList,Env,Body1,VC,N,Probs,BDDAnd,0,Clauses,Module)
  ),
  N1 is N+1.

gen_clause_cw(def_rule(H,BodyList,Lit),N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
% disjunctive clause with a single head atom senza depth_bound con prob =1
  process_body_cw(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Module),
  append([pita:one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,Env,BDDAnd,Module,Head1),
  Clauses=[(Head1 :- Body1)].


generate_clauses([],[],_N,C,C):-!.

generate_clauses([H|T],[H1|T1],N,C0,C):-
  gen_clause(H,N,N1,H1,CL),!,  %agg.cut
  append(C0,CL,C1),
  generate_clauses(T,T1,N1,C1,C).


gen_clause((H :- Body),N,N,(H :- Body),[(H :- Body)]):-!.
  
gen_clause(rule(_R,HeadList,BodyList,Lit),N,N1,
  rule(N,HeadList,BodyList,Lit),Clauses):-
  input_mod(M),
  M:local_setting(depth_bound,true),!,
% disjunctive clause with more than one head atom e depth_bound
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,Env,Module),
  append([pita:one(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules_db(HeadList,Env,Body1,[],N,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_rules_db(HeadList,Env,Body1,VC,N,Probs,DB,BDDAnd,0,Clauses,Module)
   ),
  N1 is N+1.
  
gen_clause(rule(_R,HeadList,BodyList,Lit),N,N1,
  rule(N,HeadList,BodyList,Lit),Clauses):-!,
  input_mod(M),
% disjunctive clause with more than one head atom senza depth_bound
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Env,Module),
  append([pita:one(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules(HeadList,Env,Body1,[],N,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_rules(HeadList,Env,Body1,VC,N,Probs,BDDAnd,0,Clauses,Module)
  ),
  N1 is N+1.

gen_clause(def_rule(H,BodyList,Lit),N,N,def_rule(H,BodyList,Lit),Clauses) :- 
% disjunctive clause with a single head atom e depth_bound
  input_mod(M),
  M:local_setting(depth_bound,true),!,
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([pita:one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(H,Env,BDDAnd,DBH,Module,Head1),
  Clauses=[(Head1 :- (DBH>=1,DB is DBH-1,Body1))].

gen_clause(def_rule(H,BodyList,Lit),N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
% disjunctive clause with a single head atom senza depth_bound con prob =1
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module),
  append([pita:one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,Env,BDDAnd,Module,Head1),
  Clauses=[(Head1 :- Body1)].


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

term_expansion_int((Head :- Body), ((H :- Body),[])):-
  Head=db(H),!.
  
term_expansion_int((Head :- Body), (Clauses,[rule(R,HeadList,BodyList,true)])):-
  input_mod(M),
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive clause with more than one head atom e depth_bound
  Head = (_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,Env,Module),
  append([pita:one(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules_db(HeadList,Env,Body1,[],R,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_rules_db(HeadList,Env,Body1,VC,R,Probs,DB,BDDAnd,0,Clauses,Module)
   ).
  
term_expansion_int((Head :- Body), (Clauses,[rule(R,HeadList,BodyList,true)])):-
  input_mod(M),
  M:local_setting(compiling,on),
% disjunctive clause with more than one head atom senza depth_bound
  Head = (_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Env,Module),
  append([pita:one(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules(HeadList,Env,Body1,[],R,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_rules(HeadList,Env,Body1,VC,R,Probs,BDDAnd,0,Clauses,Module)
  ).

term_expansion_int((Head :- Body), ([],[])) :- 
% disjunctive clause with a single head atom con prob. 0 senza depth_bound --> la regola non è caricata nella teoria e non è conteggiata in NR
  input_mod(M),
  M:local_setting(compiling,on),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  Head = (_H:P),P=:=0.0, !. 

term_expansion_int((Head :- Body), (Clauses,[def_rule(H,BodyList,true)])) :- 
% disjunctive clause with a single head atom e depth_bound
  input_mod(M),
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([pita:one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(H,Env,BDDAnd,DBH,Module,Head1),
  Clauses=(Head1 :- (DBH>=1,DB is DBH-1,Body1)).

term_expansion_int((Head :- Body), (Clauses,[def_rule(H,BodyList,true)])) :- 
% disjunctive clause with a single head atom senza depth_bound con prob =1
  input_mod(M),
  M:local_setting(compiling,on),
   ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  HeadList=[H:_],!,
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module),
  append([pita:one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,Env,BDDAnd,Module,Head1),
  Clauses=(Head1 :- Body1).

term_expansion_int((Head :- Body), (Clauses,[rule(R,HeadList,BodyList,true)])) :- 
% disjunctive clause with a single head atom e DB, con prob. diversa da 1
  input_mod(M),
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  Head = (H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([pita:one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),%***test single_var
  (M:local_setting(single_var,true)->
    generate_clause_db(H,Env,Body2,[],R,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_clause_db(H,Env,Body2,VC,R,Probs,DB,BDDAnd,0,Clauses,Module)
  ).

term_expansion_int((Head :- Body), (Clauses,[rule(R,HeadList,BodyList,true)])) :- 
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  input_mod(M),
  M:local_setting(compiling,on),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  Head = (H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module),
  append([pita:one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),%***test single_vars
  (M:local_setting(single_var,true)->
    generate_clause(H,Env,Body2,[],R,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_clause(H,Env,Body2,VC,R,Probs,BDDAnd,0,Clauses,Module)
  ).
  
term_expansion_int((Head :- Body),(Clauses,[])) :- 
% definite clause for db facts
  input_mod(M),
  M:local_setting(compiling,on),  
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),
  Head=db(Head1),!,
  Clauses=(Head1 :- Body).

term_expansion_int((Head :- Body),(Clauses,[def_rule(Head,BodyList,true)])) :- 
% definite clause with depth_bound
  input_mod(M),
  M:local_setting(compiling,on),  
  M:local_setting(depth_bound,true),
   ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body), 
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([pita:one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(Head,Env,BDDAnd,DBH,Module,Head1),
  Clauses=(Head1 :- (DBH>=1,DB is DBH-1,Body1)).
  
term_expansion_int((Head :- Body),(Clauses,[def_rule(Head,BodyList,true)])) :- 
% definite clause senza DB
  input_mod(M),
  M:local_setting(compiling,on),  
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module),
  append([pita:one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Head,Env,BDDAnd,Module,Head1),
  Clauses=(Head1 :- Body2).

term_expansion_int(Head,(Clauses,[rule(R,HeadList,[],true)])) :- 
  input_mod(M),
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive FACT with more than one head atom e db
  Head=(_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  (M:local_setting(single_var,true)->
    generate_rules_fact_db(HeadList,_Env,[],R,Probs,0,Clauses,_Module)
  ;
    generate_rules_fact_db(HeadList,_Env,VC,R,Probs,0,Clauses,_Module)
  ).

term_expansion_int(Head,(Clauses,[rule(R,HeadList,[],true)])) :- 
  input_mod(M),
  M:local_setting(compiling,on),
% disjunctive fact with more than one head atom senza db
  Head=(_;_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs), %**** test single_var
  (M:local_setting(single_var,true)->
    generate_rules_fact(HeadList,_Env,[],R,Probs,0,Clauses,_Module)
  ;
    generate_rules_fact(HeadList,_Env,VC,R,Probs,0,Clauses,_Module)
  ).

term_expansion_int(Head,([],[])) :- 
  input_mod(M),
  M:local_setting(compiling,on),
% disjunctive fact with a single head atom con prob. 0
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (_H:P),P=:=0.0, !.
  
term_expansion_int(Head,(Clause,[def_rule(H,[],true)])) :- 
  input_mod(M),
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive fact with a single head atom con prob.1 e db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),P=:=1.0, !,
  list2and([pita:one(Env,BDD)],Body1),
  add_bdd_arg_db(H,Env,BDD,_DB,_Module,Head1),
  Clause=(Head1 :- Body1).

term_expansion_int(Head,(Clause,[def_rule(H,[],true)])) :- 
  input_mod(M),
  M:local_setting(compiling,on),
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),P=:=1.0, !,
  list2and([pita:one(Env,BDD)],Body1),
  add_bdd_arg(H,Env,BDD,_Module,Head1),
  Clause=(Head1 :- Body1).

term_expansion_int(Head,(Clause,[rule(R,HeadList,[],true)])) :- 
  input_mod(M),
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive fact with a single head atom e prob. generiche, con db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  add_bdd_arg_db(H,Env,BDD,_DB,_Module,Head1),
  (M:local_setting(single_var,true)->
    Clause=(Head1:-(pita:get_var_n(Env,R,[],Probs,V),pita:equality(Env,V,0,BDD)))
  ;
    Clause=(Head1:-(pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,0,BDD)))
  ).

term_expansion_int(Head,(Clause,[rule(R,HeadList,[],true)])) :- 
  input_mod(M),
  M:local_setting(compiling,on),
% disjunctive fact with a single head atom e prob. generiche, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(H:_), !, 
  list2or(HeadListOr, Head), 
  process_head(HeadListOr, HeadList), 
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(R),
  get_probs(HeadList,Probs),
  add_bdd_arg(H,Env,BDD,_Module,Head1),%***test single_var
  (M:local_setting(single_var,true)->
    Clause=(Head1:-(pita:get_var_n(Env,R,[],Probs,V),pita:equality(Env,V,0,BDD)))
  ;
    Clause=(Head1:-(pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,0,BDD)))
  ).

term_expansion_int(Head, ((Head1:-pita:one(Env,One)),[def_rule(Head,[],true)])) :- 
  input_mod(M),
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% definite fact with db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg_db(Head,Env,One,_DB,_Module,Head1).

term_expansion_int(Head, ((Head1:-pita:one(Env,One)),[def_rule(Head,[],true)])) :- 
  input_mod(M),
  M:local_setting(compiling,on),
% definite fact without db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg(Head,Env,One,_Module,Head1).

/*-----------*/
:- multifile sandbox:safe_primitive/1.

%sandbox:safe_primitive(write(_)).
%sandbox:safe_primitive(random:setrand(_)).

sandbox:safe_primitive(slipcover:induce_par(_,_)).
sandbox:safe_primitive(slipcover:induce(_,_)).
sandbox:safe_primitive(slipcover:test(_,_,_,_,_,_,_)).
sandbox:safe_primitive(slipcover:test_prob(_,_,_,_,_,_)).
sandbox:safe_primitive(slipcover:set_sc(_,_)).

%sandbox:safe_primitive(prolog_load_context(_,_)).
%sandbox:safe_primitive(random:setran(_)).
/*
:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(slipcover:set_sc(_,_)).
sandbox:safe_primitive(slipcover:setting_sc(_,_)).
sandbox:safe_primitive(slipcover:init(_,_,_)).
sandbox:safe_primitive(slipcover:init_bdd(_,_)).
sandbox:safe_primitive(slipcover:init_test(_)).
sandbox:safe_primitive(slipcover:ret_prob(_,_)).
sandbox:safe_primitive(slipcover:end(_)).
sandbox:safe_primitive(slipcover:end_bdd(_)).
sandbox:safe_primitive(slipcover:end_test).
sandbox:safe_primitive(slipcover:one(_)).
sandbox:safe_primitive(slipcover:zero(_)).
sandbox:safe_primitive(slipcover:and(_,_,_)).
sandbox:safe_primitive(slipcover:or(_,_,_)).
sandbox:safe_primitive(slipcover:bdd_not(_,_)).
sandbox:safe_primitive(slipcover:get_var_n(_,_,_,_)).
sandbox:safe_primitive(slipcover:add_var(_,_,_,_)).
sandbox:safe_primitive(slipcover:equality(_,_,_)).

:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(slipcover:get_node(_,_), []).

*/
test_no_area(TestSet,NPos,NNeg,CLL,Results):-
%  S= user_output,
%  SA= user_output,
%  format(SA,"Fold;\tCLL;\t AUCROC;\t AUCPR~n",[]),
  %gtrace,
  test_folds(TestSet,[],Results,0,NPos,0,NNeg,0,CLL).


test(TestSet,CLL,AUCROC,ROC,AUCPR,PR):-
%  S= user_output,
%  SA= user_output,
%  format(SA,"Fold;\tCLL;\t AUCROC;\t AUCPR~n",[]),
  test_folds(TestSet,[],LG,0,_Pos,0,_Neg,0,CLL),
%  format(S,"cll(all,post,~d,~d,[",[Pos,Neg]),
%  write_prob(LG),
  compute_areas_diagrams(LG,AUCROC,ROC,AUCPR,PR).
/*
  ROC = c3{data:_{x:x, rows:[x-'ROC'|ROC0]},
    axis:_{x:_{min:0.0,max:1.0,padding:0.0,
        tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}},
           y:_{min:0.0,max:1.0,padding:_{bottom:0.0,top:0.0},
        tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}}},
  PR = c3{data:_{x:x, rows:[x-'PR'|PR0]},
    axis:_{x:_{min:0.0,max:1.0,padding:0.0,
        tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}},
           y:_{min:0.0,max:1.0,padding:_{bottom:0.0,top:0.0},
        tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}}}.
*/
test_folds([],LG,LG,Pos,Pos,Neg,Neg,CLL,CLL).

test_folds([HT|TT],LG0,LG,Pos0,Pos,Neg0,Neg,CLL0,CLL):-
  test_1fold(HT,LG1,Pos1,Neg1,CLL1),
  append(LG0,LG1,LG2),
  Pos2 is Pos0+Pos1,
  Neg2 is Neg0+Neg1,
  CLL2 is CLL0+CLL1,
  test_folds(TT,LG2,LG,Pos2,Pos,Neg2,Neg,CLL2,CLL).

test_1fold(F,LGOrd,Pos,Neg,CLL1):-
  find_ex(F,LG,Pos,Neg),
  compute_CLL_atoms(LG,0,0,CLL1,LG1),
  keysort(LG1,LGOrd).
/*
compute_areas(LG,Pos,Neg,AUCROC,ROC,AUCPR,PR):-
  compute_pointsroc(LG,+1e20,0,0,Pos,Neg,[],ROC),
  hull(ROC,0,0,0,AUCROC),
%  SC=user_output,
%  write_p(ROC,SC),
  compute_aucpr(LG,Pos,Neg,AUCPR,PR).
%  SPR=user_output,
%  write_ppr(PR,SPR).

compute_pointsroc([],_P0,_TP,_FP,_FN,_TN,P0,P1):-!,
  append(P0,[1.0-1.0],P1).

compute_pointsroc([P- (\+ _)|T],P0,TP,FP,FN,TN,Po0,Po1):-!,
  (P<P0->
    FPR is FP/(FP+TN),
    TPR is TP/(TP+FN),
    append(Po0,[(FPR-TPR)],Po2),
    P1=P
  ;		
    Po2=Po0,
    P1=P0
  ),
  FP1 is FP+1,
  TN1 is TN-1,
  compute_pointsroc(T,P1,TP,FP1,FN,TN1,Po2,Po1).

compute_pointsroc([P- _|T],P0,TP,FP,FN,TN,Po0,Po1):-!,
  (P<P0->
    FPR is FP/(FP+TN),
    TPR is TP/(TP+FN),
    append(Po0,[FPR-TPR],Po2),
    P1=P
  ;
    Po2=Po0,
    P1=P0
  ),
  TP1 is TP+1,
  FN1 is FN-1,
  compute_pointsroc(T,P1,TP1,FP,FN1,TN,Po2,Po1).


hull([],FPR,TPR,AUC0,AUC1):-
  AUC1 is AUC0+(1-FPR)*(1+TPR)/2.


hull([FPR1-TPR1|T],FPR,TPR,AUC0,AUC1):-
  AUC2 is AUC0+(FPR1-FPR)*(TPR1+TPR)/2,
  hull(T,FPR1,TPR1,AUC2,AUC1).

compute_aucpr(L,Pos,Neg,A,PR):-
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
  area(Points,Flag,Pos,0,0,0,A,[],PR).

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

area([],_Flag,_Pos,_TPA,_FPA,A,A,PR,PR).

area([R0-P0|T],Flag,Pos,TPA,FPA,A0,A,PR0,PR):-
 TPB is R0*Pos,
  (TPB=:=0->
    A1=A0,
    FPB=0,
    PR2=PR0,
    PR=[R0-P0|PR3]
  ;
    R_1 is TPA/Pos,
    (TPA=:=0->
      (Flag=false->
        P_1=P0,
	PR=[0.0-P0|PR3]
      ;
        P_1=0.0,
	PR=[0.0-0.0|PR3]
      )
    ;
      P_1 is TPA/(TPA+FPA),
      PR=PR3
    ),
    FPB is TPB*(1-P0)/P0,
    N is TPB-TPA+0.5,
    (N<1.0->
      append(PR0,[R0-P0],PR2),
      A1=A0
    ;
      interpolate(1,N,Pos,R_1,P_1,TPA,FPA,TPB,FPB,A0,A1,[],PR1),
      append(PR0,PR1,PR2)
    )
  ),
  area(T,Flag,Pos,TPB,FPB,A1,A,PR2,PR3).

interpolate(I,N,_Pos,_R0,_P0,_TPA,_FPA,_TPB,_FPB,A,A,PR,PR):-I>N,!.

interpolate(I,N,Pos,R0,P0,TPA,FPA,TPB,FPB,A0,A,PR0,[R-P|PR]):-
  R is (TPA+I)/Pos,
  P is (TPA+I)/(TPA+I+FPA+(FPB-FPA)/(TPB-TPA)*I),
  A1 is A0+(R-R0)*(P+P0)/2,
  I1 is I+1,
  interpolate(I1,N,Pos,R,P,TPA,FPA,TPB,FPB,A1,A,PR0,PR).
*/

find_ex(DB,LG,Pos,Neg):-
  input_mod(M),
  findall(P/A,M:output(P/A),LP),
  M:local_setting(neg_ex,given),!,
  find_ex_pred(LP,DB,[],LG,0,Pos,0,Neg).

find_ex(DB,LG,Pos,Neg):-
  input_mod(M),
  findall(P/A,M:output(P/A),LP),
  M:local_setting(neg_ex,cw),
  find_ex_pred_cw(LP,DB,[],LG,0,Pos,0,Neg).


find_ex_pred([],_DB,LG,LG,Pos,Pos,Neg,Neg).

find_ex_pred([P/A|T],DB,LG0,LG,Pos0,Pos,Neg0,Neg):-
  functor(At,P,A),
  find_ex_db(DB,At,LG0,LG1,Pos0,Pos1,Neg0,Neg1),
  find_ex_pred(T,DB,LG1,LG,Pos1,Pos,Neg1,Neg).

find_ex_db([],_At,LG,LG,Pos,Pos,Neg,Neg).

find_ex_db([H|T],At,LG0,LG,Pos0,Pos,Neg0,Neg):-
  At=..[P|L],
  At1=..[P,H|L],
  input_mod(M),
  findall(At1,M:At1,LP),
  findall(\+ At1,M:neg(At1),LN),
  length(LP,NP),
  length(LN,NN),
  append([LG0,LP,LN],LG1),
  Pos1 is Pos0+NP,
  Neg1 is Neg0+NN,
  find_ex_db(T,At,LG1,LG,Pos1,Pos,Neg1,Neg).


find_ex_pred_cw([],_DB,LG,LG,Pos,Pos,Neg,Neg).

find_ex_pred_cw([P/A|T],DB,LG0,LG,Pos0,Pos,Neg0,Neg):-
  functor(At,P,A),
  findall(Types,get_types(At,Types),LT),
  append(LT,LLT),
  remove_duplicates(LLT,Types1),
  find_ex_db_cw(DB,At,Types1,LG0,LG1,Pos0,Pos1,Neg0,Neg1),
  find_ex_pred_cw(T,DB,LG1,LG,Pos1,Pos,Neg1,Neg).

get_types(At,[]):-
  At=..[_],!.

get_types(At,Types):-
  input_mod(M),
  M:modeh(_,At),
  At=..[_|Args],
  get_args(Args,Types).

get_types(At,Types):-
  input_mod(M),
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




get_constants([],_M,[]).

get_constants([Type|T],M,[(Type,Co)|C]):-
  find_pred_using_type(Type,LP),
  find_constants(LP,M,[],Co),
  get_constants(T,M,C).

find_pred_using_type(T,L):-
  (setof((P,Ar,A),pred_type(T,P,Ar,A),L)->
    true
  ;
    L=[]
  ).

pred_type(T,P,Ar,A):-
  input_mod(M),
  M:modeh(_,S),
  S=..[P|Args],
  length(Args,Ar),
  scan_args(Args,T,1,A).

pred_type(T,P,Ar,A):-
  input_mod(M),
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

find_constants([],_M,C,C).

find_constants([(P,Ar,A)|T],M,C0,C):-
  input_mod(Mod),
  gen_goal(1,Ar,A,Args,ArgsNoV,V),
  G=..[P,M|Args],
  (setof(V,ArgsNoV^call_goal(Mod,G),LC)->
    true
  ;
    LC=[]
  ),
  append(C0,LC,C1),
  remove_duplicates(C1,C2),
  find_constants(T,M,C2,C).

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
  


find_ex_db_cw([],_At,_Ty,LG,LG,Pos,Pos,Neg,Neg).

find_ex_db_cw([H|T],At,Types,LG0,LG,Pos0,Pos,Neg0,Neg):-
  input_mod(M),
  get_constants(Types,H,C),
  At=..[P|L],
  get_types(At,TypesA),!,
  length(L,N),
  length(LN,N),
  At1=..[P,H|LN],
  findall(At1,M:At1,LP),
  (setof(\+ At1,neg_ex(LN,TypesA,At1,C),LNeg)->true;LNeg=[]),
  length(LP,NP),
  length(LNeg,NN),
  append([LG0,LP,LNeg],LG1),
  Pos1 is Pos0+NP,
  Neg1 is Neg0+NN,
  find_ex_db_cw(T,At,Types,LG1,LG,Pos1,Pos,Neg1,Neg).

neg_ex([],[],At1,_C):-
  input_mod(M),
  \+ M:At1.

neg_ex([H|T],[HT|TT],At1,C):-
  member((HT,Co),C),
  member(H,Co),
  neg_ex(T,TT,At1,C).

compute_CLL_atoms([],_N,CLL,CLL,[]):-!.

compute_CLL_atoms([\+ H|T],N,CLL0,CLL1,[PG- (\+ H)|T1]):-!,
%  write(\+ H),nl,
  input_mod(M),
  findall(R,M:rule(R,_HL,_BL,_Lit),LR),
  length(LR,NR),
  init_test(NR,Env),
  get_node(H,M,Env,BDD),!,
  ret_prob(Env,BDD,PG),
  end_test(Env),!,
  PG1 is 1-PG,
  (PG1=:=0.0->
    setting_sc(logzero,LZ),
    CLL2 is CLL0+LZ
  ;
    CLL2 is CLL0+ log(PG1)
  ),		
  N1 is N+1,
  compute_CLL_atoms(T,N1,CLL2,CLL1,T1).	

compute_CLL_atoms([H|T],N,CLL0,CLL1,[PG-H|T1]):-
%  write(H),nl,
  input_mod(M),
  findall(R,M:rule(R,_HL,_BL,_Lit),LR),
  length(LR,NR),
  init_test(NR,Env),
  get_node(H,M,Env,BDD),!,
  ret_prob(Env,BDD,PG),
  end_test(Env),!,
  (PG=:=0.0->
    setting_sc(logzero,LZ),
    CLL2 is CLL0+LZ
  ;	
    CLL2 is CLL0+ log(PG)
  ),
  N1 is N+1,
  compute_CLL_atoms(T,N1,CLL2,CLL1,T1).		


writes([H-H1],S):-
  format(S,"~f - (~q)]).~n~n",[H,H1]).

writes([H-H1|T],S):-
  format(S,"~f - (~q),~n",[H,H1]),
  writes(T,S).


write_p(P,S):-
  get_xy(P,PX,PY),
  format(S,"x=[",[]),
  writesf(PX,S),
  format(S,"y=[",[]),
  writesf(PY,S),
  format(S,"
figure('Name','roc','NumberTitle','off')
set(gca,'XLim',[0.0 1.0])
set(gca,'YLim',[0.0 1.0])
x=[x 1.0]
y=[y 0.0]
k=convhull(x,y)
plot(x(k),y(k),'r-',x,y,'--b+')
%A = polyarea(x,y)~n~n
%save area_roc.csv  A -ascii -append
",
  []).

get_xy([],[],[]).

get_xy([X-Y|T],[X|TX],[Y|TY]):-
  get_xy(T,TX,TY).


writesf([H],S):-
  format(S,"~f]~n",[H]).

writesf([H|T],S):-
  format(S,"~f ",[H]),
  writesf(T,S).

write_ppr(P,S):-
  get_xy(P,PX,PY),
  format(S,"rec=[",[A]),
  writesf(PX,S),
  format(S,"prec=[",[A]),
  writesf(PY,S),
  format(S,"
figure('Name','pr','NumberTitle','off')
set(gca,'XLim',[0.0 1.0])
set(gca,'YLim',[0.0 1.0])
rec=[0.0  rec 1.0];
prec=[0.0 prec 0.0];
plot(rec,prec,'--*k')
%A=polyarea(rec,prec)
%save area_pr.csv  A -ascii -append
~n~n",
  []).

write2(A):-
  input_mod(M),
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write(A)
  ;
    true
  ).

write3(A):-	
  input_mod(M),
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write(A)
  ;
    true
  ).

format2(A,B):-	
  input_mod(M),
  M:local_setting(verbosity,Ver),
  (Ver>1->
    format(A,B)
  ;
    true
  ).

format3(A,B):-	
  input_mod(M),
  M:local_setting(verbosity,Ver),
  (Ver>2->
    format(A,B)
  ;
    true
  ).

write_rules2(A,B):-	
  input_mod(M),
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_rules(A,B)
  ;
    true
  ).

write_rules3(A,B):-	
  input_mod(M),
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_rules(A,B)
  ;
    true
  ).


write_disj_clause2(A,B):-	
  input_mod(M),
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_disj_clause(A,B)
  ;
    true
  ).

write_disj_clause3(A,B):-	
  input_mod(M),
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_disj_clause(A,B)
  ;
    true
  ).

write_body2(A,B):-	
  input_mod(M),
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_body(A,B)
  ;
    true
  ).

write_body3(A,B):-	
  input_mod(M),
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_body(A,B)
  ;
    true
  ).


%:-style_check(-discontiguous).
:- dynamic input_mod/1.

/* input_mod(0).
:-prolog_load_context(module, M),
  retract(input_mod(_)),
  write(M),
  assert(input_mod(M)).
*/

user:term_expansion((:- sc), []) :-!,
  prolog_load_context(module, M),
%  retractall(input_mod(_)),
%  M:dynamic(model/1),
%  M:set_prolog_flag(unkonw,fail),
  findall(local_setting(P,V),default_setting_sc(P,V),L),
  assert_all(L,M,_),
  assert(input_mod(M)),
  retractall(M:rule_sc_n(_)),
  assert(M:rule_sc_n(0)),
  M:dynamic((modeh/2,modeh/4,fixed_rule/3,banned/2,lookahead/2,
    lookahead_cons/2,lookahead_cons_var/2,prob/2,input/1,input_cw/1,
    ref_clause/1,ref/1,model/1,neg/1,rule/4,determination/2,
    bg_on/0,bg/1,bgc/1,in_on/0,in/1,inc/1,int/1)),
  style_check(-discontiguous).

user:term_expansion((:- begin_bg), []) :-
  input_mod(M),!,
  assert(M:bg_on).

user:term_expansion(C, M:bgc(C)) :-
  C\= (:- end_bg),
  input_mod(M),
  M:bg_on,!.

user:term_expansion((:- end_bg), []) :-
  input_mod(M),!,
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

user:term_expansion((:- begin_in), []) :-
  input_mod(M),!,
  assert(M:in_on).

user:term_expansion(C, M:inc(C)) :-
  C\= (:- end_in),
  input_mod(M),
  M:in_on,!.

user:term_expansion((:- end_in), []) :-
  input_mod(M),!,
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


%
user:term_expansion(begin(model(I)), []) :-!,
  input_mod(M),
  retractall(M:model(_)),
  assert(M:model(I)),
  assert(M:int(I)).

user:term_expansion(end(model(_I)), []) :-!,
  input_mod(M),
  retractall(M:model(_)).

user:term_expansion(At, A) :-
%  write(At),nl,
  input_mod(M),
  M:model(Name),
  At \= (_ :- _),
  At \= end_of_file,
  (At=neg(Atom)->    
    Atom=..[Pred|Args],
    Atom1=..[Pred,Name|Args],
    A=neg(Atom1)
  ;
    (At=prob(Pr)->
      A=prob(Name,Pr)
    ;
      At=..[Pred|Args],
      Atom1=..[Pred,Name|Args],
      A=Atom1
    )
  ).





