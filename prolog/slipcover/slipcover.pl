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
:-module(slipcover,[sl/1,em/1,load/3,assert_all/2,retract_all/1,rule_n/1,set/2,
  init_test/1,get_node/2,ret_prob/2,end_test/0]).
:-use_module(library(lists)).
:-use_module(library(random)).
:-use_module(library(system)).
:-multifile setting/2.

:-dynamic setting/2,last_id/1, rule/5.
:- expects_dialect(yap).

:- op(500,fx,#).
:- op(500,fx,'-#').

:-[revise_sl].

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
  init(NR,LSH),
  retractall(v(_,_,_)),
  length(DB,NEx),  
  (setting(examples,atoms)->
    setting(group,G),
    derive_bdd_nodes_groupatoms(DB,NEx,G,[],Nodes,0,CLL0,LE,[]),!   % 1 BDD per example if G=1
  ;
    derive_bdd_nodes(DB,NEx,[],Nodes,0,CLL0),! % 1 BDD per model
  ),
  setting(random_restarts_number,N),
  format("~nInitial CLL ~f~n~n",[CLL0]),
  random_restarts(N,Nodes,CLL0,Score,initial,Par,LE),   %output:CLL,Par
  format("Score after EMBLEM = ~f~n",[Score]),
  retract_all(Th1Ref),
  retract_all(R2Ref),!,
  end,  
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
  init(NR,LSH),
  retractall(v(_,_,_)),
  length(DB,NEx),
  (setting(examples,atoms)->
    setting(group,G),  
    derive_bdd_nodes_groupatoms(DB,NEx,G,[],Nodes,0,_CLL0,LE,[]),!   
  ; 
   derive_bdd_nodes(DB,NEx,[],Nodes,0,_CLL0),!      
  ),
  setting(random_restarts_number,N),
  random_restarts(N,Nodes,-1e20,Score,initial,Par,LE),  %computes new parameters Par
  end,
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
  init(NR,LSH),
  retractall(v(_,_,_)),
  length(DB,NEx),
  get_output_preds(R1,O),
  (setting(examples,atoms)->
    setting(group,G),  
    derive_bdd_nodes_groupatoms_output_atoms(DB,O,NEx,G,[],Nodes,0,CLL0,LE,[]),!
  ; 
    derive_bdd_nodes(DB,NEx,[],Nodes,0,CLL0),!
  ),
  format("Initial CLL ~f~n",[CLL0]),
  setting(random_restarts_REFnumber,N),
  random_restarts_ref(N,Nodes,CLL0,Score,initial,Par,LE),  
  end,
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

derive_bdd_nodes([],_E,Nodes,Nodes,CLL,CLL).

derive_bdd_nodes([H|T],E,Nodes0,Nodes,CLL0,CLL):-
  get_output_atoms(O),
  generate_goal(O,H,[],GL),
  (prob(H,P)->
    CardEx is P*E
  
  ;
    CardEx is 1.0
  ),
  init_bdd,
  one(One),
  get_node_list(GL,One,BDD,CardEx),
  ret_prob(BDD,HP),
  (HP=:=0.0->
    setting(logzero,LZ),
    CLL1 is CLL0+LZ*CardEx
  ;
    CLL1 is CLL0+log(HP)*CardEx
  ),
  end_bdd,
  append(Nodes0,[[BDD,CardEx]],Nodes1),
  derive_bdd_nodes(T,E,Nodes1,Nodes,CLL1,CLL).


get_node_list([],BDD,BDD,_CE).


get_node_list([H|T],BDD0,BDD,CE):-
  get_node(H,BDD1),
  and(BDD0,BDD1,BDD2),
  get_node_list(T,BDD2,BDD,CE).


derive_bdd_nodes_groupatoms_output_atoms([],_O,_E,_G,Nodes,Nodes,CLL,CLL,LE,LE).

derive_bdd_nodes_groupatoms_output_atoms([H|T],O,E,G,Nodes0,Nodes,CLL0,CLL,LE0,LE):-  
  generate_goal(O,H,[],GL),
  length(GL,NA),
  (prob(H,P)->
    CardEx is P*E/NA
  ;
    CardEx is 1.0
  ),
  get_node_list_groupatoms(GL,BDDs,CardEx,G,CLL0,CLL1,LE0,LE1),
  append(Nodes0,BDDs,Nodes1),
  derive_bdd_nodes_groupatoms_output_atoms(T,O,E,G,Nodes1,Nodes,CLL1,CLL,LE1,LE).

  
derive_bdd_nodes_groupatoms([],_E,_G,Nodes,Nodes,CLL,CLL,LE,LE).

derive_bdd_nodes_groupatoms([H|T],E,G,Nodes0,Nodes,CLL0,CLL,LE0,LE):-  
  get_output_atoms(O),
  generate_goal(O,H,[],GL),
  length(GL,NA),
  (prob(H,P)->
    CardEx is P*E/NA
  ;
    CardEx is 1.0
  ),
  get_node_list_groupatoms(GL,BDDs,CardEx,G,CLL0,CLL1,LE0,LE1),
  append(Nodes0,BDDs,Nodes1),
  derive_bdd_nodes_groupatoms(T,E,G,Nodes1,Nodes,CLL1,CLL,LE1,LE).

get_node_list_groupatoms([],[],_CE,_Gmax,CLL,CLL,LE,LE).

get_node_list_groupatoms([H|T],[[BDD,CE1]|BDDT],CE,Gmax,CLL0,CLL,LE0,LE):-
  init_bdd,  
  one(One),
  get_bdd_group([H|T],T1,Gmax,G,One,BDD,CE,LE0,LE1),  %output:BDD,CLL
  CE1 is CE*(Gmax-G),
  ret_prob(BDD,HP),
  end_bdd,
  (HP =:=0.0->
    setting(logzero,LZ),
    CLL2 is CLL0+LZ*CE1
  ;
    CLL2 is CLL0+log(HP)*CE1
  ),
  get_node_list_groupatoms(T1,BDDT,CE,Gmax,CLL2,CLL,LE1,LE).


get_bdd_group([],[],G,G,BDD,BDD,_CE,LE,LE):-!.

get_bdd_group(T,T,0,0,BDD,BDD,_CE,LE,LE):- !.

get_bdd_group([H|T],T1,Gmax,G1,BDD0,BDD,CE,[H|LE0],LE):-
%  write(H),flush_output,
  get_node(H,BDD1),  		%creates the BDD for atom H
  and(BDD0,BDD1,BDD2),
  G is Gmax-1,
  get_bdd_group(T,T1,G,G1,BDD2,BDD,CE,LE0,LE).
  
/* EM start */
random_restarts(0,_Nodes,Score,Score,Par,Par,_LE):-!.

random_restarts(N,Nodes,Score0,Score,Par0,Par,LE):-
  setting(verbosity,Ver),
  (Ver>2->
    setting(random_restarts_number,NMax),
    Num is NMax-N+1,
    format("Restart number ~d~n~n",[Num]),
    flush_output
  ;
    true
  ),
  randomize,
  setting(epsilon_em,EA),
  setting(epsilon_em_fraction,ER),
  length(Nodes,L),
  setting(iter,Iter),
  em(Nodes,EA,ER,L,Iter,CLL,Par1,ExP),  
  score(LE,ExP,CLL,ScoreR),
  setting(verbosity,Ver),
  (Ver>2->
    format("Random_restart: Score ~f~n",[ScoreR])
  ;
    true
  ),
  N1 is N-1,
  (ScoreR>Score0->     
    random_restarts(N1,Nodes,ScoreR,Score,Par1,Par,LE)
  ;
    random_restarts(N1,Nodes,Score0,Score,Par0,Par,LE)
  ).

random_restarts_ref(0,_Nodes,Score,Score,Par,Par,_LE):-!.

random_restarts_ref(N,Nodes,Score0,Score,Par0,Par,LE):-
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
  em(Nodes,EA,ER,L,Iter,CLLR,Par1,ExP),  
  score(LE,ExP,CLLR,ScoreR),
  setting(verbosity,Ver),
  (Ver>2->
    format("Random_restart: Score ~f~n",[ScoreR])
  ;
    true
  ),
  N1 is N-1,
  (ScoreR>Score0->  
    random_restarts_ref(N1,Nodes,ScoreR,Score,Par1,Par,LE)
  ;
    random_restarts_ref(N1,Nodes,Score0,Score,Par0,Par,LE)
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


:-[inference_sl_cover].
