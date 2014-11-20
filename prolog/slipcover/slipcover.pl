/** <module> slipcover

This module performs learning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It allows to perform both parameter and structure learning.

See http://ds.ing.unife.it/~friguzzi/software/cplint/manual.html for
details

@author Fabrizio Riguzzi
@license Artistic License 2.0
*/

/*

SLIPCOVER

Copyright (c) 2013, Fabrizio Riguzzi and Elena Bellodi

*/
:-module(slipcover,[sl/1,em/1,set/2,setting/2]).
:- meta_predicate get_node(:,-).
:-use_module(library(lists)).
:-use_module(library(random)).
:-use_module(library(system)).
:- use_module(library(terms)).
:-use_module(library(rbtrees)).
:-use_foreign_library(foreign(bddem),install).
:-set_prolog_flag(unknown,warning).

:-multifile setting/2.
%:-use_module(library(sandbox)).

:-dynamic p/2,rule_n/1.
:-dynamic setting/2,last_id/1, rule/5.
:- expects_dialect(yap).

:- op(500,fx,#).
:- op(500,fx,'-#').


setting(epsilon_em,0.0001).
setting(epsilon_em_fraction,0.00001).
setting(eps,0.0001).
setting(eps_f,0.00001).

/* if the difference in log likelihood in two successive em iteration is smaller
than epsilon_em, then EM stops */
setting(epsilon_sem,2).

/* number of random restarts of em */
setting(random_restarts_REFnumber,1).
setting(random_restarts_number,1).
setting(iterREF,-1).
setting(iter,-1).
setting(examples,atoms).
setting(group,1).
setting(d,1).  
setting(verbosity,1).
setting(logzero,log(0.000001)).
setting(megaex_bottom,1). 
setting(initial_clauses_per_megaex,1).  
setting(max_iter,10).
setting(max_iter_structure,10000).
setting(max_var,4).
setting(max_rules,10).
setting(maxdepth_var,2).
setting(beamsize,100).
setting(background_clauses,50).

setting(specialization,bottom).
%setting(specialization,mode).
/* allowed values: mode,bottom */

setting(seed,rand(10,1231,30322)).  
setting(score,ll).
/* allowed values: ll aucpr */
/** 
 * sl(+FileStem:atom) is det
 *
 * The predicate performs structure learning for the problem stored in
 * the files FileStem.l (language bias), FileStem.kb (dataset), 
 * FileStem.bg (optional, background theory), FileStem.cpl (optional,
 * initial theory).
 * The result is stored in FileStem.rules
 */

sl(File):-
  setting(seed,Seed),
  setrand(Seed),
  generate_file_names(File,FileKB,FileIn,FileBG,FileOut,FileL),
  reconsult(FileL),
  load_models(FileKB,DB),  
  assert(database(DB)),
  statistics(walltime,[_,_]),
  (exists_file(FileBG)->
    set(compiling,on),
    load(FileBG,_ThBG,RBG),
    set(compiling,off),
    generate_clauses(RBG,_RBG1,0,[],ThBG), 
    assert_all(ThBG,_ThBGRef)
  ;
    true
  ),
  (exists_file(FileIn)->
    set(compiling,on),
    load(FileIn,_Th1,R1),
    set(compiling,off)
  ;
    (setting(specialization,bottom)->
      setting(megaex_bottom,MB),
      deduct(MB,DB,[],InitialTheory),   
      length(InitialTheory,_LI),  
      remove_duplicates(InitialTheory,R1)
    ;
      get_head_atoms(O),
      generate_top_cl(O,R1)
    )
  ),
%  write('Initial theory'),nl,
%  write_rules(R1,user_output),
  learn_struct(DB,R1,R2,Score2), 
  learn_params(DB,R2,R,Score),  
  statistics(walltime,[_,WT]),
  WTS is WT/1000,
  format("~nRefinement score  ~f - score after EMBLEM ~f~n",[Score2,Score]),
  format("Total execution time ~f~n~n",[WTS]),
  write_rules(R,user_output),
  listing(setting/2),
  open(FileOut,write,Stream),
  format(Stream,'/* SLIPCOVER Final score ~f~n',[Score]),
  format(Stream,'Execution time ~f~n',[WTS]),
  tell(Stream),
  listing(setting/2),
  format(Stream,'*/~n~n',[]),
  told, 
  open(FileOut,append,Stream1),
  write_rules(R,Stream1),
  close(Stream1).

gen_fixed([],[]).

gen_fixed([(H,B,BL)|T],[rule(R,H,B,BL)|T1]):-
  get_next_rule_number(R), 
  gen_fixed(T,T1).

learn_struct_only(DB,R1,R,Score):-   %+R1:initial theory of the form [rule(NR,[h],[b]],...], -R:final theory of the same form, -CLL
  format("Clause search~n~n",[]),
  setting(max_iter,M),
  setting(depth_bound,DepthB),
  set(depth_bound,false),
  findall((H,B,BL),fixed_rule(H,B,BL),LF),
  length(LF,LLF),
  gen_fixed(LF,LFR),
  format("Scoring fixed clauses: ~d clauses~n~n",[LLF]),
  score_clause_refinements(LFR,1,LLF,DB,[],NB1,[],CL0,[],CLBG0),
  append(NB1,R1,Beam),
  cycle_beam(Beam,DB,CL0,[(HCL,S)|TCL],CLBG0,BG,M),
  set(depth_bound,DepthB),
  format("Theory search~n~n",[]),
  setting(max_iter_structure,MS),
  cycle_structure(TCL,[HCL],S,-1e20,DB,R2,Score,MS),
  format("Best target theory~n~n",[]),
  write_rules(R2,user_output),
  length(BG,NBG),
  format("Background search: ~d clauses~n~n",[NBG]),
  remove_score(BG,BG2),
  append(R2,BG2,R).


learn_struct(DB,R1,R,Score):-   %+R1:initial theory of the form [rule(NR,[h],[b]],...], -R:final theory of the same form, -CLL
  format("Clause search~n~n",[]),
  setting(max_iter,M),
  setting(depth_bound,DepthB),
  set(depth_bound,false),
  findall((H,B,BL),fixed_rule(H,B,BL),LF),
  length(LF,LLF),
  gen_fixed(LF,LFR),
  format("Scoring fixed clauses: ~d clauses~n~n",[LLF]),
  score_clause_refinements(LFR,1,LLF,DB,[],NB1,[],CL0,[],CLBG0),
  append(NB1,R1,Beam),
  cycle_beam(Beam,DB,CL0,[(HCL,S)|TCL],CLBG0,BG,M),
  set(depth_bound,DepthB),
  format("Theory search~n~n",[]),
  setting(max_iter_structure,MS),
  cycle_structure(TCL,[HCL],S,-1e20,DB,R2,Score,MS),
  format("Best target theory~n~n",[]),
  write_rules(R2,user_output),
  setting(background_clauses,NBG1),
  length(BG,NBG),
  format("Background search: ~d of ~d clauses~n~n",[NBG1,NBG]),
  pick_first(NBG1,BG,BG1),
  remove_score(BG,BG2),
  write_rules(BG2,user_output),nl,
  append(R2,BG1,R).

pick_first(0,_,[]):-!.

pick_first(_,[],[]):-!.

pick_first(N,[(H,_S)|T],[H|T1]):-
  N1 is N-1,
  pick_first(N1,T,T1).

remove_score([],[]).

remove_score([(H,_S)|T],[H|T1]):-
  remove_score(T,T1).

cycle_structure([],R,S,_SP,_DB,R,S,_I):-!.  %empty beam

cycle_structure(_CL,R,S,_SP,_DB,R,S,0):-!.  %0 iterations

cycle_structure([(RH,_CLL)|RT],R0,S0,SP0,DB,R,S,M):-
  already_scored([RH|R0],R3,Score),!,
  format("Theory iteration ~d",[M]),nl,nl,
  write('Already scored, updated refinement'),nl,
  write_rules(R3,user_output), 
  write('Score '),write(Score),nl,nl,nl,
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
  cycle_structure(RT,R4,S4,SP1,DB,R,S,M1). 

cycle_structure([(RH,_Score)|RT],R0,S0,SP0,DB,R,S,M):-
  format("Theory iteration ~d",[M]),nl,nl,
  generate_clauses([RH|R0],R2,0,[],Th1),
  format("Initial theory~n~n",[]),
  write_rules([RH|R0],user_output),
  assert_all(Th1,Th1Ref),  
  assert_all(R2,R2Ref),!,
  findall(R-HN,(rule(R,HL,_BL,_Lit),length(HL,HN)),L),  
  keysort(L,LS),
  get_heads(LS,LSH),  
  length(LSH,NR),
  init(NR,LSH,ExData),
  retractall(v(_,_,_)),
  length(DB,NEx),  
  (setting(examples,atoms)->
    setting(group,G),
    derive_bdd_nodes_groupatoms(DB,ExData,NEx,G,[],Nodes,0,CLL0,LE,[]),!   % 1 BDD per example if G=1
  ;
    derive_bdd_nodes(DB,ExData,NEx,[],Nodes,0,CLL0),! % 1 BDD per model
  ),
  setting(random_restarts_number,N),
  format("~nInitial CLL ~f~n~n",[CLL0]),
  random_restarts(N,ExData,Nodes,CLL0,Score,initial,Par,LE),   %output:CLL,Par
  format("Score after EMBLEM = ~f~n",[Score]),
  retract_all(Th1Ref),
  retract_all(R2Ref),!,
  end(ExData),  
  update_theory(R2,Par,R3), 
  write('updated Theory'),nl,
  write_rules(R3,user_output),   %definite rules without probabilities in the head are not written
  (Score>S0->
    R4=R3,
    S4=Score,
    SP1=S0,
    write('New best score'),nl
  ;
    R4=R0,
    S4=S0,
    SP1=SP0
  ),
  store_refinement([RH|R0],R3,Score),
  M1 is M-1,
  cycle_structure(RT,R4,S4,SP1,DB,R,S,M1). 

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
    set(compiling,on),
    load(FileBG,_ThBG,RBG),
    set(compiling,off),
    generate_clauses(RBG,_RBG1,0,[],ThBG), 
    assert_all(ThBG,_ThBGRef)
  ;
    true
  ),
  set(compiling,on),
  load(FileIn,_TH,R0),
  set(compiling,off),
  set(verbosity,3),
  statistics(walltime,[_,_]),      
  learn_params(DB,R0,R,Score),
  statistics(walltime,[_,CT]),
  CTS is CT/1000,
  format("EM: Final score ~f~n",[Score]),
  format("Execution time ~f~n~n",[CTS]),
  write_rules(R,user_output),
  listing(setting/2),
  open(FileOut,write,Stream),
  format(Stream,'/* EMBLEM Final score ~f~n',[Score]),
  format(Stream,'Execution time ~f~n',[CTS]),
  tell(Stream),
  listing(setting/2),
  format(Stream,'*/~n~n',[]),
  told,
  open(FileOut,append,Stream1),
  write_rules(R,Stream1),
  close(Stream1).

learn_params(DB,R0,R,Score):-  %Parameter Learning
  generate_clauses(R0,R1,0,[],Th0), 
  assert_all(Th0,Th0Ref),
  assert_all(R1,R1Ref),!,
  findall(R-HN,(rule(R,HL,_BL,_Lit),length(HL,HN)),L),
  keysort(L,LS),
  get_heads(LS,LSH),
  length(LSH,NR),
  init(NR,LSH,ExData),
  retractall(v(_,_,_)),
  length(DB,NEx),
  (setting(examples,atoms)->
    setting(group,G),  
    derive_bdd_nodes_groupatoms(DB,ExData,NEx,G,[],Nodes,0,_CLL0,LE,[]),!   
  ; 
   derive_bdd_nodes(DB,ExData,NEx,[],Nodes,0,_CLL0),!      
  ),
  setting(random_restarts_number,N),
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

update_theory_par([rule(N,_H,_B,_L)|T0],Par,T):-
  member([N,[1.0|_T]],Par),!,  
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
  
cycle_beam([],_DB,CL,CL,CLBG,CLBG,_M):-!.

cycle_beam(_Beam,_DB,CL,CL,CLBG,CLBG,0):-!.

cycle_beam(Beam,DB,CL0,CL,CLBG0,CLBG,M):-
  format("Clause iteration ~d",[M]),nl,nl,
  cycle_clauses(Beam,DB,[],NB,CL0,CL1,CLBG0,CLBG1),
  M1 is M-1,%decreases the number of max_iter M
  cycle_beam(NB,DB,CL1,CL,CLBG1,CLBG,M1).

cycle_clauses([],_DB,NB,NB,CL,CL,CLBG,CLBG):-!.

cycle_clauses([(RH,_ScoreH)|T],DB,NB0,NB,CL0,CL,CLBG0,CLBG):-
  findall(RS,specialize_rule(RH,RS,_L),LR),!,   %-LR:list of lists, each one correponding to a different revised theory; specialize_rule defined in revise.pl
  length(LR,NR),
  write('Number of revisions '),write(NR),nl,
  score_clause_refinements(LR,1,NR,DB,NB0,NB1,CL0,CL1,CLBG0,CLBG1),
  cycle_clauses(T,DB,NB1,NB,CL1,CL,CLBG1,CLBG).

score_clause_refinements([],_N,_NR,_DB,NB,NB,CL,CL,CLBG,CLBG).

score_clause_refinements([R1|T],Nrev,NRef,DB,NB0,NB,CL0,CL,CLBG0,CLBG):-  %scans the list of revised theories
  already_scored_clause(R1,R3,Score),!,
  format('Score ref.  ~d of ~d~n',[Nrev,NRef]),
  write('Already scored, updated refinement'),nl,
  write_rules([R3],user_output), 
  write('Score '),write(Score),nl,nl,nl,
  setting(beamsize,BS),
  insert_in_order(NB0,(R3,Score),BS,NB1),
  Nrev1 is Nrev+1,  
  score_clause_refinements(T,Nrev1,NRef,DB,NB1,NB,CL0,CL,CLBG0,CLBG).

score_clause_refinements([R1|T],Nrev,NRef,DB,NB0,NB,CL0,CL,CLBG0,CLBG):- 
  format('Score ref.  ~d of ~d~n',[Nrev,NRef]),
  write_rules([R1],user_output),   
  generate_clauses_cw([R1],[R2],0,[],Th1),
  assert_all(Th1,Th1Ref),
  assert_all([R2],[R2Ref]),!,
  findall(RN-HN,(rule(RN,HL,_BL,_Lit),length(HL,HN)),L),  
  keysort(L,LS),
  get_heads(LS,LSH),
  length(LSH,NR),
  init(NR,LSH,ExData),
  retractall(v(_,_,_)),
  length(DB,NEx),
  get_output_preds(R1,O),
  (setting(examples,atoms)->
    setting(group,G),  
    derive_bdd_nodes_groupatoms_output_atoms(DB,ExData,O,NEx,G,[],Nodes,0,CLL0,LE,[]),!
  ; 
    derive_bdd_nodes(DB,ExData,NEx,[],Nodes,0,CLL0),!
  ),
  format("Initial CLL ~f~n",[CLL0]),
  setting(random_restarts_REFnumber,N),
  random_restarts_ref(N,ExData,Nodes,CLL0,Score,initial,Par,LE),  
  end(ExData),
  update_theory([R2],Par,[R3]),
  write('Updated refinement'),nl,
  write_rules([R3],user_output), 
  write('Score (CLL) '),write(Score),nl,nl,nl,
  retract_all(Th1Ref),
  retract_all([R2Ref]),!,
  setting(beamsize,BS),
  insert_in_order(NB0,(R3,Score),BS,NB1),
  (target(R3)->
    insert_in_order(CL0,(R3,Score),+1e20,CL1),
    length(CL1,LCL1),
    format("N. of target clauses ~d~n~n",[LCL1]),
    CLBG1=CLBG0
  ;
    (range_restricted(R3)->
      insert_in_order(CLBG0,(R3,Score),+1e20,CLBG1),
      length(CLBG1,LCL1),
      format("N. of background clauses ~d~n~n",[LCL1]),
      CL1=CL0
    ;
      format("Not range restricted~n~n",[]),
      CL1=CL0,
      CLBG1=CLBG0
    )
  ),
  store_clause_refinement(R1,R3,Score),
  Nrev1 is Nrev+1,  
  score_clause_refinements(T,Nrev1,NRef,DB,NB1,NB,CL1,CL,CLBG1,CLBG).

range_restricted(rule(_N,HL,BL,_Lit)):-
  term_variables(HL,VH),
  term_variables(BL,VB),
  sublisteq(VH,VB).

sublisteq([],_).

sublisteq([H|T],L):-
  member_eq(H,L),
  sublisteq(T,L).

target(R):-
  get_output_preds(R,O),
  member(T,O),
  output(T),!.

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
  recorda(ref_clause,r(Ref1,RefP,Score),_).

store_refinement(Ref,RefP,Score):-
  elab_ref(Ref,Ref1),
  recorda(ref,r(Ref1,RefP,Score),_).

already_scored_clause(R,R1,Score):-
  elab_ref([R],[rule(H,B)]),
  recorded(ref_clause,r(rule(H,B1),R1,Score),_),
  permutation(B,B1).

already_scored(R,R1,Score):-
  elab_ref(R,RR),
  recorded(ref,r(RR,R1,Score),_).


elab_clause_ref(rule(_NR,H,B,_Lits),rule(H1,B1)):-
  copy_term((H,B),(H1,B1)).

elab_ref([],[]).

elab_ref([rule(_NR,H,B,_Lits)|T],[rule(H1,B1)|T1]):-
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
    setting(logzero,LZ),
    CLL1 is CLL0+LZ*CardEx
  ;
    CLL1 is CLL0+log(HP)*CardEx
  ),
  end_bdd(ExData),
  append(Nodes0,[[BDD,CardEx]],Nodes1),
  derive_bdd_nodes(T,ExData,E,Nodes1,Nodes,CLL1,CLL).


get_node_list([],Env,BDD,BDD,_CE).


get_node_list([H|T],Env,BDD0,BDD,CE):-
  get_node(H,Env,BDD1),
  and(Env,BDD0,BDD1,BDD2),
  get_node_list(T,Env,BDD2,BDD,CE).


derive_bdd_nodes_groupatoms_output_atoms([],_ExData,_O,_E,_G,Nodes,Nodes,CLL,CLL,LE,LE).

derive_bdd_nodes_groupatoms_output_atoms([H|T],ExDataO,E,G,Nodes0,Nodes,CLL0,CLL,LE0,LE):-  
  generate_goal(O,H,[],GL),
  length(GL,NA),
  (prob(H,P)->
    CardEx is P*E/NA
  ;
    CardEx is 1.0
  ),
  get_node_list_groupatoms(GL,ExData,BDDs,CardEx,G,CLL0,CLL1,LE0,LE1),
  append(Nodes0,BDDs,Nodes1),
  derive_bdd_nodes_groupatoms_output_atoms(T,ExData,O,E,G,Nodes1,Nodes,CLL1,CLL,LE1,LE).

  
derive_bdd_nodes_groupatoms([],_ExData,_E,_G,Nodes,Nodes,CLL,CLL,LE,LE).

derive_bdd_nodes_groupatoms([H|T],ExData,E,G,Nodes0,Nodes,CLL0,CLL,LE0,LE):-  
  get_output_atoms(O),
  generate_goal(O,H,[],GL),
  length(GL,NA),
  (prob(H,P)->
    CardEx is P*E/NA
  ;
    CardEx is 1.0
  ),
  get_node_list_groupatoms(GL,ExData,BDDs,CardEx,G,CLL0,CLL1,LE0,LE1),
  append(Nodes0,BDDs,Nodes1),
  derive_bdd_nodes_groupatoms(T,ExData,E,G,Nodes1,Nodes,CLL1,CLL,LE1,LE).

get_node_list_groupatoms([],_ExData,[],_CE,_Gmax,CLL,CLL,LE,LE).

get_node_list_groupatoms([H|T],ExData,[[BDD,CE1]|BDDT],CE,Gmax,CLL0,CLL,LE0,LE):-
  init_bdd(ExData,Env),  
  one(Env,One),
  get_bdd_group([H|T],Env,T1,Gmax,G,One,BDD,CE,LE0,LE1),  %output:BDD,CLL
  CE1 is CE*(Gmax-G),
  ret_prob(Env,BDD,HP),
  end_bdd(ExData),
  (HP =:=0.0->
    setting(logzero,LZ),
    CLL2 is CLL0+LZ*CE1
  ;
    CLL2 is CLL0+log(HP)*CE1
  ),
  get_node_list_groupatoms(T1,ExData,BDDT,CE,Gmax,CLL2,CLL,LE1,LE).


get_bdd_group([],_Env,[],G,G,BDD,BDD,_CE,LE,LE):-!.

get_bdd_group(T,_Env,T,0,0,BDD,BDD,_CE,LE,LE):- !.

get_bdd_group([H|T],Env,T1,Gmax,G1,BDD0,BDD,CE,[H|LE0],LE):-
%  write(H),flush_output,
  get_node(H,Env,BDD1),  		%creates the BDD for atom H
  and(Env,BDD0,BDD1,BDD2),
  G is Gmax-1,
  get_bdd_group(T,Env,T1,G,G1,BDD2,BDD,CE,LE0,LE).


/* EM start */
random_restarts(0,_ExData,_Nodes,Score,Score,Par,Par,_LE):-!.

random_restarts(N,ExData,Nodes,Score0,Score,Par0,Par,LE):-
  setting(verbosity,Ver),
  (Ver>2->
    setting(random_restarts_number,NMax),
    Num is NMax-N+1,
    format("Restart number ~d~n~n",[Num]),
    flush_output
  ;
    true
  ),
  randomize(ExData),
  setting(epsilon_em,EA),
  setting(epsilon_em_fraction,ER),
  length(Nodes,L),
  setting(iter,Iter),
  em(ExData,odes,EA,ER,L,Iter,CLL,Par1,ExP),  
  score(LE,ExP,CLL,ScoreR),
  setting(verbosity,Ver),
  (Ver>2->
    format("Random_restart: Score ~f~n",[ScoreR])
  ;
    true
  ),
  N1 is N-1,
  (ScoreR>Score0->     
    random_restarts(N1,ExData,Nodes,ScoreR,Score,Par1,Par,LE)
  ;
    random_restarts(N1,ExData,Nodes,Score0,Score,Par0,Par,LE)
  ).

random_restarts_ref(0,_ExData,_Nodes,Score,Score,Par,Par,_LE):-!.

random_restarts_ref(N,ExData,Nodes,Score0,Score,Par0,Par,LE):-
  setting(verbosity,Ver),
  (Ver>2->
    setting(random_restarts_REFnumber,NMax),
    Num is NMax-N+1,
    format("Restart number ~d~n~n",[Num]),
    flush_output
  ;
    true
  ),
  setting(epsilon_em,EA),
  setting(epsilon_em_fraction,ER),
  length(Nodes,L),
  setting(iterREF,Iter),
  em(ExData,Nodes,EA,ER,L,Iter,CLLR,Par1,ExP),  
  score(LE,ExP,CLLR,ScoreR),
  setting(verbosity,Ver),
  (Ver>2->
    format("Random_restart: Score ~f~n",[ScoreR])
  ;
    true
  ),
  N1 is N-1,
  (ScoreR>Score0->  
    random_restarts_ref(N1,ExData,Nodes,ScoreR,Score,Par1,Par,LE)
  ;
    random_restarts_ref(N1,ExData,Nodes,Score0,Score,Par0,Par,LE)
  ).


score(_LE,_ExP,CLL,CLL):-
  setting(score,ll),!.

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
  generate_file_name(File,".kb",FileKB),
  generate_file_name(File,".cpl",FileIn),
  generate_file_name(File,".rules",FileOut),
  generate_file_name(File,".bg",FileBG),
  generate_file_name(File,".l",FileL).
        
generate_file_name(File,Ext,FileExt):-
  name(File,FileString),
  append(FileString,Ext,FileStringExt),
  name(FileExt,FileStringExt).
   
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


write_param(initial,S):-!,
  format("~nInitial parameters~n",[]),
  findall(rule(R,H,B,Lit),rule(R,H,B,Lit),LDis),
  findall(rule(d,[H:1.0],B,Lit),def_rule(H,B,Lit),LDef),
  append(LDis,LDef,L),
  write_model(L,S).

write_param(L,S):-
  reverse(L,L1),
  write_par(L1,S).


write_par([],S):-
  findall(rule(d,[H:1.0],B,Lit),def_rule(H,B,Lit),L),
  write_model(L,S).

write_par([[N,P]|T],S):-
  rule(N,HL0,BL),
  reverse(P,PR),
  new_par(PR,HL0,HL),
  copy_term((HL,BL),(HL1,BL1)),
  numbervars((HL1,BL1),0,_M),
  write_disj_clause(S,(HL1:-BL1)),
  write_par(T,S).


write_rules([],_S).

write_rules([rule(_N,HL,BL,Lit)|T],S):-
  copy_term((HL,BL,Lit),(HL1,BL1,Lit1)),
  numbervars((HL1,BL1,Lit1),0,_M),
  write_disj_clause(S,(HL1:-BL1)),
%  write(Lit1),nl,
  write_rules(T,S).


new_par([],[],[]).

new_par([HP|TP],[Head:_|TO],[Head:HP|TN]):-
  new_par(TP,TO,TN).


write_model([],_Stream):-!.

write_model([rule(_N,HL,BL)|Rest],Stream):-
  copy_term((HL,BL),(HL1,BL1)),
  numbervars((HL1,BL1),0,_M),
  write_disj_clause(Stream,(HL1:-BL1)),
  write_model(Rest,Stream).


write_disj_clause(S,(H:-[])):-!,
  write_head(S,H),
  format(S,".~n~n",[]).
    
write_disj_clause(S,(H:-B)):-
  write_head(S,H),
  write(S,' :-'),
  nl(S),
  write_body(S,B).



write_head(S,[A:1.0|_Rest]):-!,
%  write(_Rest),nl,
  format(S,"~p:1.0",[A]).
  
write_head(S,[A:P,'':_P]):-!, 
  format(S,"~p:~g",[A,P]).

write_head(S,[A:P]):-!,
  format(S,"~p:~g",[A,P]).

write_head(S,[A:P|Rest]):-
  format(S,"~p:~g ; ",[A,P]),
  write_head(S,Rest).

write_body(S,[]):-
  format(S,"\ttrue.~n~n",[]).

write_body(S,[A]):-!,
  format(S,"\t~p.~n~n",[A]).
    
write_body(S,[A|T]):-
  format(S,"\t~p,~n",[A]),
  write_body(S,T).


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
 

deduct(0,_DB,Th,Th):-!.

deduct(NM,DB,InTheory0,InTheory):-
  get_head_atoms(O),
  sample(1,DB,[M],DB1),
  generate_head(O,M,[],HL),
  generate_body(HL,InTheory1),
  append(InTheory0,InTheory1,InTheory2),
  NM1 is NM-1,
  deduct(NM1,DB1,InTheory2,InTheory).

        
get_head_atoms(O):-
  findall(A,modeh(_,A),O0),
  findall((A,B,D),modeh(_,A,B,D),O1),
  append(O0,O1,O).

generate_top_cl([],[]):-!.

generate_top_cl([A|T],[(rule(R,[A1:0.5,'':0.5],[],true),-1e20)|TR]):-
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  A1=..[F|Arg],
  get_next_rule_number(R),
  generate_top_cl(T,TR).


generate_head([],_M,HL,HL):-!.

generate_head([(A,G,D)|T],M,H0,H1):-!,
  generate_head_goal(G,M,Goals),
  findall((A,Goals,D),(member(Goal,Goals),call(Goal),ground(Goals)),L),
  setting(initial_clauses_per_megaex,IC),   %IC: represents how many samples are extracted from the list L of example
  sample(IC,L,L1),   
  append(H0,L1,H2),
  generate_head(T,M,H2,H1).

generate_head([A|T],M,H0,H1):-
  functor(A,F,N),    
  functor(F1,F,N),   
  F1=..[F|Arg],
  Pred1=..[F,M|Arg],
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  findall((A,Pred1),call(Pred1),L),
  setting(initial_clauses_per_megaex,IC),   
  sample(IC,L,L1),   
  append(H0,L1,H2),
  generate_head(T,M,H2,H1).

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

get_modeb([],B,B).

get_modeb([F/AA|T],B0,B):-
  findall((R,B),(modeb(R,B),functor(B,F,AA)),BL),
  append(B0,BL,B1),
  get_modeb(T,B1,B).

generate_body([],[]):-!.

generate_body([(A,H,Det)|T],[(rule(R,HP,[],BodyList),-1e20)|CL0]):-!,
  get_modeb(Det,[],BL),
  get_args(A,H,Pairs,[],Args,[],ArgsTypes,M),
  setting(d,D),
  cycle_modeb(ArgsTypes,Args,[],[],BL,a,[],BLout0,D,M),
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
  format("Bottom clause: example ~p~nClause~n",[H]),
  write_disj_clause(user_output,(HeadV:-BodyListV)),
  generate_body(T,CL0).

generate_body([(A,H)|T],[(rule(R,[Head:0.5,'':0.5],[],BodyList),-1e20)|CL0]):-
  functor(A,F,AA),
  findall((R,B),(modeb(R,B),functor(B,FB,AB),determination(F/AA,FB/AB)),BL),
  A=..[F|ArgsTypes],
  H=..[F,M|Args],
  setting(d,D),
  cycle_modeb(ArgsTypes,Args,[],[],BL,a,[],BLout0,D,M),
  variabilize(([(H,A)]:-BLout0),CLV),  %+(Head):-Bodylist;  -CLV:(Head):-Bodylist with variables _num in place of constants
  CLV=([Head1]:-BodyList1),
  remove_int_atom(Head1,Head),
  remove_int_atom_list(BodyList1,BodyList2),
  remove_duplicates(BodyList2,BodyList),
  get_next_rule_number(R),
  copy_term((Head,BodyList),(HeadV,BodyListV)),
  numbervars((HeadV,BodyListV),0,_V),
  format("Bottom clause: example ~p~nClause~n~p:0.5 :-~n",[H,HeadV]),
  write_body(user_output,BodyListV),
  generate_body(T,CL0).


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

variabilize_args([C|T],[_Ty|TT],[V|TV],A0,A):-
  member(C/V,A0),!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[_Ty|TT],[V|TV],A0,A):-
  variabilize_args(T,TT,TV,[C/V|A0],A).


cycle_modeb(ArgsTypes,Args,ArgsTypes,Args,_BL,L,L,L,_,_M):-!.

cycle_modeb(_ArgsTypes,_Args,_ArgsTypes1,_Args1,_BL,_L,L,L,0,_M):-!.

cycle_modeb(ArgsTypes,Args,_ArgsTypes0,_Args0,BL,_L0,L1,L,D,M):-
  find_atoms(BL,ArgsTypes,Args,ArgsTypes1,Args1,L1,L2,M),
  D1 is D-1,
  cycle_modeb(ArgsTypes1,Args1,ArgsTypes,Args,BL,L1,L2,L,D1,M).


find_atoms([],ArgsTypes,Args,ArgsTypes,Args,L,L,_M).

find_atoms([(R,H)|T],ArgsTypes0,Args0,ArgsTypes,Args,L0,L1,M):-
  H=..[F|ArgsT],
  findall((A,H),instantiate_query(ArgsT,ArgsTypes0,Args0,F,M,A),L),
  call_atoms(L,[],At),
  remove_duplicates(At,At1),
  (R = '*' ->
    R1= +1e20
  ;
    R1=R
  ),
  sample(R1,At1,At2),
  extract_output_args(At2,ArgsT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  append(L0,At2,L2),
  find_atoms(T,ArgsTypes1,Args1,ArgsTypes,Args,L2,L1,M).


call_atoms([],A,A).

call_atoms([(H,M)|T],A0,A):-
  findall((H,M),H,L),
  append(A0,L,A1),
  call_atoms(T,A1,A).


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


get_output_atoms(O):-
  findall((A/Ar),output((A/Ar)),O).


generate_goal([],_H,G,G):-!.

generate_goal([P/A|T],H,G0,G1):-
  functor(Pred,P,A),
  Pred=..[P|Rest],
  Pred1=..[P,H|Rest],
  findall(Pred1,call(Pred1),L),
  findall(\+ Pred1,call(neg(Pred1)),LN),
  append(G0,L,G2),
  append(G2,LN,G3),
  generate_goal(T,H,G3,G1).
  
remove_duplicates([],[]).

remove_duplicates([H|T],T1):-
  member_eq(H,T),!,
  remove_duplicates(T,T1).

remove_duplicates([H|T],[H|T1]):-
  remove_duplicates(T,T1).


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
%  nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([add_body(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
%  nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove_body(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
%  nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([add_head(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
%  nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove_head(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
%  nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove(Rule)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule,NewTheory),
%  nl,write(NewTheory),
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
  setting(max_rules,MR),
  LT<MR,
  add_rule(Ref).


generalize_rule(Rule,Ref):-
  generalize_head(Rule,Ref).

generalize_rule(Rule,Ref):-
  generalize_body(Rule,Ref).


add_rule(add(rule(ID,Head,[],Lits))):-
  setting(specialization,bottom),!,
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
  (setting(new_head_atoms_zero_prob,true)->
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
specialize_rule(Rule,SpecRule,Lit):-
  setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
  delete_one(Lits,RLits,Lit),
  \+ lookahead_cons(Lit,_),
  \+ lookahead_cons_var(Lit,_),
  \+ member_eq(Lit,BL), 
  append(BL,[Lit],BL1),
  remove_prob(LH,LH1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  dv(LH2,BL1,DList), 	%-DList: list of couples (variable,depth)
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,LH2),
  setting(maxdepth_var,MD),
  exceed_depth(DList,MD),  
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,RLits).

specialize_rule(Rule,SpecRule,Lit):-
  setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
  delete_one(Lits,RLits,Lit),
  \+ member_eq(Lit,BL),
  append(BL,[Lit],BL0),
  \+lookahead_cons_var(Lit,_),
  (lookahead(Lit,LLit1);lookahead_cons(Lit,LLit1)),  
  copy_term(LLit1,LLit2),
  specialize_rule_la_bot(LLit2,RLits,RLits1,BL0,BL1),
  remove_prob(LH,LH1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  dv(LH2,BL1,DList), 
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,LH2),
  setting(maxdepth_var,MD),
  exceed_depth(DList,MD),  
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,RLits1).

specialize_rule(Rule,SpecRule,Lit):-
  setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
  delete_one(Lits,RLits,Lit),
  \+ member_eq(Lit,BL),
  append(BL,[Lit],BL0),
  lookahead_cons_var(Lit,LLit2),
  specialize_rule_la_bot(LLit2,RLits,_RLits1,BL0,BL1),
  remove_prob(LH,LH1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  dv(LH2,BL1,DList), 
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,LH2),
  setting(maxdepth_var,MD),
  exceed_depth(DList,MD),  
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,[]).

specialize_rule(Rule,SpecRule,Lit):-
  setting(specialization,mode),%!,
  findall(BL , modeb(_,BL), BLS),
  specialize_rule(BLS,Rule,SpecRule,Lit).

%specializes the clause's head
specialize_rule(rule(ID,LH,BL,Lits),rule(ID,LH2,BL,Lits),Lit):-
	length(LH,L), 
	L>2,
	delete_one(LH,LH1,Lit),  %deletes Lit
	Lit\=' ',
	update_head1(LH1,L-1,LH2).  %updates parameters

update_head1([],_N,[]):-!.

update_head1([H:_P|T],N,[H:P|T1]):-
	       P is 1/N,
	       update_head1(T,N,T1).

write_list([A]):-!,
               format("\t~p.~n~n",[A]).
write_list([A|T]):-
            format("\t~p,",[A]),
	          write_list(T).


banned_clause(H,B):-
  numbervars((H,B),0,_N),
  banned(H2,B2),
  mysublist(H2,H),
  mysublist(B2,B).


mysublist([],_).

mysublist([H|T],L):-
  member(H,L),
  mysublist(T,L).


check_ref(H,B):-
  copy_term((H,B),(H1,B1)),
  numbervars((H1,B1),0,_N),
  (ref(H1,B1)->
    fail
  ;
    assert(ref(H1,B1))
  ).

specialize_rule([Lit|_RLit],Rule,SpecRul,SLit):-
  Rule = rule(ID,LH,BL,true),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,ALL,SLit),
  append(BL,[SLit],BL1),
  (lookahead(SLit,LLit1);lookahead_cons(SLit,LLit1)),
  specialize_rule_la(LLit1,LH1,BL1,BL2),
  append(LH1,BL2,ALL2),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  setting(max_var,MV),
  NV=<MV,
  SpecRul = rule(ID,LH,BL2,true).

specialize_rule([Lit|_RLit],Rule,SpecRul,SLit):-
  Rule = rule(ID,LH,BL,true),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,ALL,SLit),
  \+ lookahead_cons(SLit,_),
  append(BL,[SLit],BL1),
  append(LH1,BL1,ALL1),
  extract_fancy_vars(ALL1,Vars1),
  length(Vars1,NV),
  setting(max_var,MV),
  NV=<MV,
  SpecRul = rule(ID,LH,BL1,true).

specialize_rule([_|RLit],Rule,SpecRul,Lit):-
  specialize_rule(RLit,Rule,SpecRul,Lit).


specialize_rule_la([],_LH1,BL1,BL1).

specialize_rule_la([Lit1|T],LH1,BL1,BL3):-
  copy_term(Lit1,Lit2),
  modeb(_,Lit2),
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

linked_clause([],_).

linked_clause([L|R],PrevLits):-
  term_variables(PrevLits,PrevVars),
  input_variables(L,InputVars),
  linked(InputVars,PrevVars),!,
  linked_clause(R,[L|PrevLits]).


linked([],_).

linked([X|R],L) :-
  member_eq(X,L),
  !,
  linked(R,L).
  

input_variables(\+ LitM,InputVars):-
  !,
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  copy_term(LitM,Lit0),
  modeb(_,Lit1),
  Lit1 =.. [P|Args1],
  convert_to_input_vars(Args1,Args2),
  Lit2 =.. [P|Args2],
  input_vars(Lit0,Lit2,InputVars).

input_variables(LitM,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  modeb(_,Lit1),
  input_vars(LitM,Lit1,InputVars).

input_variables(LitM,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  modeh(_,Lit1),
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
  modeh(_,Lit),!.

take_mode(Lit):-
  modeb(_,Lit),!.

take_mode(Lit):-
  mode(_,Lit),!.


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
dv(H,B,DV1):-			%DV1: returns a list of couples (Variable, Max depth)
	term_variables(H,V),
	head_depth(V,DV0),
	findall((MD-DV),var_depth(B,DV0,DV,0,MD),LDs), 
        get_max(LDs,-1,-,DV1).
	

input_variables_b(LitM,InputVars):-
	  LitM=..[P|Args],
	  length(Args,LA),
	  length(Args1,LA),
	  Lit1=..[P|Args1],
	  modeb(_,Lit1),
	  input_vars(LitM,Lit1,InputVars).



%associates depth 0 to each variable in the clause's head
head_depth([],[]).
head_depth([V|R],[[V,0]|R1]):-
  head_depth(R,R1).

%associates a depth to each variable in the clause's body
var_depth([],PrevDs1,PrevDs1,MD,MD):-!.

var_depth([L|R],PrevDs,PrevDs1,_MD,MD):-    		%L = a body literal, MD = maximum depth set by the user
  input_variables_b(L,InputVars),          	
  term_variables(L, BodyAtomVars),   		   
  output_vars(BodyAtomVars,InputVars,OutputVars),       
  depth_InputVars(InputVars,PrevDs,0,MaxD),   		%MaxD: maximum depth of the input variables in the body literal
  D is MaxD+1,
  compute_depth(OutputVars,D,PrevDs,PrevDs0), 		%Computes the depth for the output variables in the body literal
  var_depth(R,PrevDs0,PrevDs1,D,MD).

get_max([],_,Ds,Ds).

get_max([(MD-DsH)|T],MD0,_Ds0,Ds):-
  MD>MD0,!,
  get_max(T,MD,DsH,Ds).

get_max([_H|T],MD,Ds0,Ds):-
	get_max(T,MD,Ds0,Ds).


output_vars(OutVars,[],OutVars):-!.
output_vars(BodyAtomVars,[I|InputVars],OutVars):-	
  delete(BodyAtomVars, I, Residue),   			
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

 

%checks if a variable's depth exceeds the setting
exceed_depth([],_):-!.
exceed_depth([H|T],MD):-
	nth1(2,H,Dep),	
	Dep<MD, %setting(maxdepth_var,MD),
	exceed_depth(T,MD).

/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzi and Elena Bellodi

*/



rule_n(0).

setting(epsilon_parsing, 1e-5).
setting(tabling, off).
/* on, off */

setting(bagof,false).
/* values: false, intermediate, all, extra */

setting(compiling,off).


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


get_node(\+ Goal,Env,BDD):-
  setting(depth_bound,true),!,
  setting(depth,DB),
  retractall(v(_,_,_)),
  add_bdd_arg_db(Goal,Env,BDD,DB,Goal1),
  (bagof(BDD,Goal1,L)->
    or_list(L,Env,B)
  ;
    zero(Env,B)
  ),
  bdd_not(Env,B,BDD).

get_node(\+ Goal,Env,BDD):-!,
  retractall(v(_,_,_)),
  add_bdd_arg(Goal,Env,BDD,Goal1),
  (bagof(BDD,Goal1,L)->
    or_list(L,Env,B)
  ;
    zero(Env,B)
  ),
  bdd_not(Env,B,BDD).

get_node(Goal,Env,B):-
  setting(depth_bound,true),!,
  setting(depth,DB),
  retractall(v(_,_,_)),
  add_bdd_arg_db(Goal,Env,BDD,DB,Goal1),%DB=depth bound
  (bagof(BDD,Goal1,L)->
    or_list(L,Env,B)
  ;
    zero(Env,B)
  ).

get_node(Goal,Env,B):- %with DB=false
  retractall(v(_,_,_)),
  add_bdd_arg(Goal,Env,BDD,Goal1),
  (bagof(BDD,Goal1,L)->
    or_list(L,Env,B)
  ;  
    zero(Env,B)
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


get_var_n(Env,R,S,Probs,V):-
  (v(R,S,V)->
    true
  ;
    length(Probs,L),
    add_var(Env,L,Probs,R,V),    
    assert(v(R,S,V))
  ).

add_bdd_arg(M:A,Env,BDD,M:A1):-
  A=..[P|Args],
  append(Args,[BDD],Args1),
  A1=..[P,Env|Args1].


add_bdd_arg_db(M:A,Env,BDD,DB,M:A1):-
  A=..[P|Args],
  append(Args,[DB,BDD],Args1),
  A1=..[P,Env|Args1].


add_bdd_arg(A,Env,BDD,_Module,A1):-
  A=..[P|Args],
  append(Args,[BDD],Args1),
  A1=..[P,Env|Args1].


add_bdd_arg_db(A,Env,BDD,DB,_Module,A1):-
  A=..[P|Args],
  append(Args,[DB,BDD],Args1),
  A1=..[P,Env|Args1].

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


or_list([H],_Env,H):-!.

or_list([H|T],Env,B):-
  or_list1(T,Env,H,B).


or_list1([],_Env,B,B).

or_list1([H|T],Env,B0,B1):-
  or(Env,B0,H,B2),
  or_list1(T,Env,B2,B1).


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


add_mod_arg(A,Module,A1):-
  A=..[P|Args],
  A1=..[P,Module|Args].



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
  append([one(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  (setting(single_var,true)->
    generate_rules(HeadList,Env,Body1,[],N,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_rules(HeadList,Env,Body1,VC,N,Probs,BDDAnd,0,Clauses,Module)
  ),
  N1 is N+1.

gen_clause_cw(def_rule(H,BodyList,Lit),N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
% disjunctive clause with a single head atom senza depth_bound con prob =1
  process_body_cw(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
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
  setting(depth_bound,true),!,
% disjunctive clause with more than one head atom e depth_bound
  process_body_db(BodyList,BDD,BDDAnd, DB,[],_Vars,BodyList1,Env,Module),
  append([one(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  (setting(single_var,true)->
    generate_rules_db(HeadList,Env,Body1,[],N,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_rules_db(HeadList,Env,Body1,VC,N,Probs,DB,BDDAnd,0,Clauses,Module)
   ),
  N1 is N+1.
  
gen_clause(rule(_R,HeadList,BodyList,Lit),N,N1,
  rule(N,HeadList,BodyList,Lit),Clauses):-!,
% disjunctive clause with more than one head atom senza depth_bound
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Env,Module),
  append([one(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  (setting(single_var,true)->
    generate_rules(HeadList,Env,Body1,[],N,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_rules(HeadList,Env,Body1,VC,N,Probs,BDDAnd,0,Clauses,Module)
  ),
  N1 is N+1.

gen_clause(def_rule(H,BodyList,Lit),N,N,def_rule(H,BodyList,Lit),Clauses) :- 
% disjunctive clause with a single head atom e depth_bound
  setting(depth_bound,true),!,
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(H,Env,BDDAnd,DB,Module,Head1),
  Clauses=[(Head1 :- Body1)].

gen_clause(def_rule(H,BodyList,Lit),N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
% disjunctive clause with a single head atom senza depth_bound con prob =1
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg(H,Env,BDDAnd,Module,Head1),
  Clauses=[(Head1 :- Body1)].

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
  setting(compiling,on),
  setting(depth_bound,true),
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
  (setting(single_var,true)->
    generate_rules_db(HeadList,Env,Body1,[],R,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_rules_db(HeadList,Env,Body1,VC,R,Probs,DB,BDDAnd,0,Clauses,Module)
   ).
  
term_expansion_int((Head :- Body), (Clauses,[rule(R,HeadList,BodyList,true)])):-
  setting(compiling,on),
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
  (setting(single_var,true)->
    generate_rules(HeadList,Env,Body1,[],R,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_rules(HeadList,Env,Body1,VC,R,Probs,BDDAnd,0,Clauses,Module)
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
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(H,Env,BDDAnd,DB,Module,Head1),
  Clauses=(Head1 :- Body1).

term_expansion_int((Head :- Body), (Clauses,[def_rule(H,BodyList,true)])) :- 
% disjunctive clause with a single head atom senza depth_bound con prob =1
  setting(compiling,on),
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

term_expansion_int((Head :- Body), (Clauses,[rule(R,HeadList,BodyList,true)])) :- 
% disjunctive clause with a single head atom e DB, con prob. diversa da 1
  setting(compiling,on),
  setting(depth_bound,true),
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
  (setting(single_var,true)->
    generate_clause_db(H,Env,Body2,[],R,Probs,DB,BDDAnd,0,Clauses,Module)
  ;
    generate_clause_db(H,Env,Body2,VC,R,Probs,DB,BDDAnd,0,Clauses,Module)
  ).

term_expansion_int((Head :- Body), (Clauses,[rule(R,HeadList,BodyList,true)])) :- 
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  setting(compiling,on),
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
  (setting(single_var,true)->
    generate_clause(H,Env,Body2,[],R,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_clause(H,Env,Body2,VC,R,Probs,BDDAnd,0,Clauses,Module)
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
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(Head,Env,BDDAnd,DB,Module,Head1),
  Clauses=(Head1 :- Body1).
  
term_expansion_int((Head :- Body),(Clauses,[def_rule(Head,BodyList,true)])) :- 
% definite clause senza DB
  setting(compiling,on),  
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body), 
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module),
  append([one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Head,Env,BDDAnd,Module,Head1),
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
    generate_rules_fact_db(HeadList,_Env,[],R,Probs,0,Clauses,_Module)
  ;
    generate_rules_fact_db(HeadList,_Env,VC,R,Probs,0,Clauses,_Module)
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
    generate_rules_fact(HeadList,_Env,[],R,Probs,0,Clauses,_Module)
  ;
    generate_rules_fact(HeadList,_Env,VC,R,Probs,0,Clauses,_Module)
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
  list2and([one(Env,BDD)],Body1),
  add_bdd_arg_db(H,Env,BDD,_DB,_Module,Head1),
  Clause=(Head1 :- Body1).

term_expansion_int(Head,(Clause,[def_rule(H,[],true)])) :- 
  setting(compiling,on),
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),P=:=1.0, !,
  list2and([one(Env,BDD)],Body1),
  add_bdd_arg(H,Env,BDD,_Module,Head1),
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
  add_bdd_arg_db(H,Env,BDD,_DB,_Module,Head1),
  (setting(single_var,true)->
    Clause=(Head1:-(get_var_n(Env,R,[],Probs,V),equality(Env,V,0,BDD)))
  ;
    Clause=(Head1:-(get_var_n(Env,R,VC,Probs,V),equality(Env,V,0,BDD)))
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
  add_bdd_arg(H,Env,BDD,_Module,Head1),%***test single_var
  (setting(single_var,true)->
    Clause=(Head1:-(get_var_n(Env,R,[],Probs,V),equality(Env,V,0,BDD)))
  ;
    Clause=(Head1:-(get_var_n(Env,R,VC,Probs,V),equality(Env,V,0,BDD)))
  ).

term_expansion_int(Head, ((Head1:-one(Env,One)),[def_rule(Head,[],true)])) :- 
  setting(compiling,on),
  setting(depth_bound,true),
% definite fact with db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg_db(Head,Env,One,_DB,_Module,Head1).

term_expansion_int(Head, ((Head1:-one(Env,One)),[def_rule(Head,[],true)])) :- 
  setting(compiling,on),
% definite fact without db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg(Head,Env,One,_Module,Head1).

/*-----------*/

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(slipcover:set(_,_)).
sandbox:safe_primitive(slipcover:setting(_,_)).
sandbox:safe_primitive(slipcover:init_test(_)).
sandbox:safe_primitive(slipcover:ret_prob(_,_)).
sandbox:safe_primitive(slipcover:end_test).
sandbox:safe_primitive(slipcover:one(_)).
sandbox:safe_primitive(slipcover:zero(_)).
sandbox:safe_primitive(slipcover:and(_,_,_)).
sandbox:safe_primitive(slipcover:or(_,_,_)).
sandbox:safe_primitive(slipcover:bdd_not(_,_)).
sandbox:safe_primitive(slipcover:get_var_n(_,_,_,_)).
sandbox:safe_primitive(slipcover:add_var(_,_,_,_)).
sandbox:safe_primitive(slipcover:equality(_,_,_)).

sandbox:safe_meta(slipcover:get_node(_,_), []).


:-style_check(-discontiguous).


