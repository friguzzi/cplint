/***************************************************************************************************
  MCLPADS
	http://www.di.uniba.it/~ndm/mclpads/

	Copyright (c) 2013 University of Bari "Aldo Moro"
  Author: Nicola Di Mauro

	**************************************************************************************************

	This code is part of the SLIPCOVER code https://sites.google.com/a/unife.it/ml/slipcover
	Copyright (c) 2011, Fabrizio Riguzzi and Elena Bellodi
	Parts of this code are thaken from the SLIPCOVER source code

	***************************************************************************************************

  The MCLPADS Software is made available under the terms and conditions of the Artistic License 2.0.
	LICENSEE shall acknowledge University of Bari "Aldo Moro" as the provider of the Software.

***************************************************************************************************/

/*:- include(slipcover_lemur).*/

/**************************************
	 __BEGIN__
	 New source code for MCLPADS
 **************************************/

:-module(lemur,[set_lm/2,setting_lm/2,
  induce_lm/2, sample/4,test_lm/7,test_lm_r/5,test_prob_lm/6,
  op(500,fx,#),op(500,fx,'-#')]).

/*slipcover_lemur.pl declarations start*/
:-use_module(library(lists)).
:-use_module(library(random)).
:-use_module(library(system)).
:-use_module(library(terms)).
:-use_module(library(rbtrees)).
:-use_module(library(pita)).
:-use_module(library(slipcover)).

/* :-[revise_lemur].
 declarations start*/
:- set_prolog_flag(discontiguous_warnings,on).
:- set_prolog_flag(single_var_warnings,on).
:- set_prolog_flag(unknown,warning).

/* :- [dv_lemur].
	declarations start
define the depth of a variable appearing in a clause A B ^ : : : ^ Br as follows.

Variables appearing in the head of a clause have depth zero.
Otherwise, let Bi be the first literal containing the variable V, and let d be the maximal depth of the input variables of Bi
then the depth of V is d + 1. The depth of a clause is the maximal depth of any variable in the clause.

In questo modo possiamo lasciare il numero massimo di variabili a 4 (e cosi' impara la regola con taughtby) e riduciamo la profondita' massima delle variabili a 2 (in questo modo dovremmo imparare la regola con i due publication nel body e anche quella con taughtby).
Bisogna modificare revise.pl per controllare che gli atomi che si aggiungono nel body non abbiano variabili oltre la profondita' massima.
*/

%:-['graphics_train.l'].

%setting(maxdepth_var,1).
%funzionamento
%?- t(DV).
%DV = [[_A,1],[_B,0],[_C,0]]	- lista di coppie [variabile,profondità massima]

default_setting_lm(epsilon_em,0.0001).
default_setting_lm(epsilon_em_fraction,0.00001).
default_setting_lm(eps,0.0001).
default_setting_lm(eps_f,0.00001).

/* if the difference in log likelihood in two successive em iteration is smaller
than epsilon_em, then EM stops */
default_setting_lm(epsilon_sem,2).

/* number of random restarts of em */
default_setting_lm(random_restarts_REFnumber,1).
default_setting_lm(random_restarts_number,1).
default_setting_lm(iterREF,-1).
default_setting_lm(iter,-1).
default_setting_lm(examples,atoms).
default_setting_lm(group,1).
default_setting_lm(d,1).
default_setting_lm(verbosity,1).
default_setting_lm(logzero,log(0.000001)).
default_setting_lm(megaex_bottom,1).
default_setting_lm(initial_clauses_per_megaex,1).
default_setting_lm(max_iter,10).
default_setting_lm(max_iter_structure,10000).
default_setting_lm(maxdepth_var,2).
default_setting_lm(beamsize,100).
default_setting_lm(background_clauses,50).
default_setting_lm(neg_ex,cw).

%setting(specialization,bottom).
default_setting_lm(specialization,mode).
/* allowed values: mode,bottom */

default_setting_lm(seed,seed(3032)).
default_setting_lm(c_seed,21344).
default_setting_lm(score,ll).
/* allowed values: ll aucpr */

default_setting_lm(mcts_beamsize,3).
default_setting_lm(mcts_visits,1e20).
default_setting_lm(max_var,4).
default_setting_lm(mcts_max_depth,8).
default_setting_lm(mcts_c,0.7).
default_setting_lm(mcts_iter,20).
default_setting_lm(mcts_maxrestarts,20).
default_setting_lm(mcts_covering,true).
default_setting_lm(max_rules,1).
default_setting_lm(epsilon_parsing, 1e-5).
default_setting_lm(tabling, off).
default_setting_lm(bagof,false).
default_setting_lm(compiling,off).
default_setting_lm(depth_bound,false).  %if true, it limits the derivation of the example to the value of 'depth'
default_setting_lm(depth,2).
default_setting_lm(single_var,false). %false:1 variable for every grounding of a rule; true: 1 variable for rule (even if a rule has more groundings),simpler.

:- thread_local database/1, mcts_modeb/1, mcts_restart/1, mcts_best_score/1, mcts_best_theory/1, mcts_theories/1, mcts_best_theories_iteration/1, node/7, lastid/1, mcts_iteration/1, v/3, rule_sc_n/1, input_mod/1, local_setting/2, in_on/0, in/1, model/1, int/1.

/**
 * induce_lm(+TrainFolds:list_of_atoms,-P:probabilistic_program) is det
 *
 * The predicate performs structure learning using the folds indicated in
 * TrainFolds for training.
 * It returns in P the learned probabilistic program.
 */
induce_lm(TrainFolds,P):-
  input_mod(M),
  assert(slipcover:input_mod(M)),
  induce_rules(TrainFolds,P0),
  rules2terms(P0,P),
  retract((slipcover:input_mod(M))).
  %generate_clauses(P0,P,0,[],_Th).

/**
 * induce_lm(+TrainFolds:list_of_atoms,+TestFolds:list_of_atoms,-P:probabilistic_program,-LL:float,-AUCROC:float,-ROC:dict,-AUCPR:float,-PR:dict) is det
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
induce_lm(TrainFolds,TestFolds,ROut,LL,AUCROC,ROC,AUCPR,PR):-
  induce_rules(TrainFolds,R),
  rules2terms(R,ROut),
  test(ROut,TestFolds,LL,AUCROC,ROC,AUCPR,PR).

/**
 * test_lm(+P:probabilistic_program,+TestFolds:list_of_atoms,-LL:float,-AUCROC:float,-ROC:dict,-AUCPR:float,-PR:dict) is det
 *
 * The predicate takes as input in P a probabilistic program,
 * tests P on the folds indicated in TestFolds and returns the
 * log likelihood of the test examples in LL, the area under the Receiver
 * Operating Characteristic curve in AUCROC, a dict containing the points
 * of the ROC curve in ROC, the area under the Precision Recall curve in AUCPR
 * and a dict containing the points of the PR curve in PR
 */
test_lm(P,TestFolds,LL,AUCROC,ROC,AUCPR,PR):-
  input_mod(M),
  assert(slipcover:input_mod(M)),
  test(P,TestFolds,LL,AUCROC,ROC,AUCPR,PR),
  retract((slipcover:input_mod(M))).

/**
 * test_lm_r(+P:probabilistic_program,+TestFolds:list_of_atoms,-LL:float,-AUCROC:float,-AUCPR:float) is det
 *
 * The predicate takes as input in P a probabilistic program,
 * tests P on the folds indicated in TestFolds and returns the
 * log likelihood of the test examples in LL, the area under the Receiver
 * Operating Characteristic curve in AUCROC, the area under the Precision Recall
 * curve in AUCPR and draws R diagrams of the curves.
 */
test_lm_r(P,TestFolds,LL,AUCROC,AUCPR):-
  input_mod(M),
  assert(slipcover:input_mod(M)),
  test_r(P,TestFolds,LL,AUCROC,AUCPR),
  retract((slipcover:input_mod(M))).

/**
 * test_prob_lm(+P:probabilistic_program,+TestFolds:list_of_atoms,-NPos:int,
 * -NNeg:int,-LL:float,-Results:list) is det
 *
 * The predicate takes as input in P a probabilistic program,
 * tests P on the folds indicated in TestFolds and returns
 * the number of positive examples in NPos, the number of negative examples
 * in NNeg, the log likelihood in LL
 * and in Results a list containing the probabilistic result for each query cont
ained in TestFolds.
 */
test_prob_lm(P,TestFolds,NPos,NNeg,CLL,Results) :-
  input_mod(M),
  assert(slipcover:input_mod(M)),
  test_prob(P,TestFolds,NPos,NNeg,CLL,Results),
  retract((slipcover:input_mod(M))).

induce_rules(Folds,R):-
  %tell(ciao),
  input_module(M),
  make_dynamic(M),
  set_lm(compiling,on),
  M:local_setting(seed,Seed),
  set_random(Seed),
  M:local_setting(c_seed,CSeed),
  rand_seed(CSeed),
  %set_prolog_flag(unknown,warning),
  findall(Exs,(member(F,Folds),M:fold(F,Exs)),L),
  append(L,DB),
  assert(M:database(DB)),
  %gtrace,
  %find_ex(DB,_LG,NPos,_Neg),

  statistics(walltime,[_,_]),
  format2("\nMonte Carlo Tree Search for LPAD Structure Learning\n",[]),
  %assert(filedot('bdd.dot')),

  %findall(C,M:bg(C),RBG),
  (M:bg(RBG0)->
    process_clauses(RBG0,[],_,[],RBG),
    generate_clauses(RBG,_RBG1,0,[],ThBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ThBG,M,ThBGRef),
    assert_all(ClBG,M,ClBGRef)
  ;
    true
  ),
  findall(BL , M:modeb(_,BL), BLS0),
  sort(BLS0,BSL),
  assert(mcts_modeb(BSL)),

  assert(mcts_restart(1)),
  learn_struct_mcts(DB,[],R2,Score2),
  retract(mcts_restart(_)),
  learn_params(DB,M,R2,R,Score),

  format2("~nRefinement score  ~f - score after EMBLEM ~f~n",[Score2,Score]),
  statistics(walltime,[_,WT]),
  WTS is WT/1000,
  write2('\n\n'),
  format2('Wall time ~f */~n',[WTS]),
  write_rules2(R,user_output),
  %told,
  set_lm(compiling,off),
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


learn_struct_mcts(DB,R1,R,CLL1):-
  input_module(M),
  learn_params(DB,M, R1, R3, CLL),
  /*generate_clauses(R1,R2,0,[],Th1),
  assert_all(Th1),
  assert_all(R2),
  !,
  findall(R-HN,(rule(R,HL,_BL,_Lit),length(HL,HN)),L),
  keysort(L,LS),
  get_heads(LS,LSH),
  length(LSH,NR),
  init(NR,LSH),
  retractall(v(_,_,_)),
  length(DB,NEx),
  (setting(examples,atoms) ->
   setting(group,G),
   derive_bdd_nodes_groupatoms(DB,NEx,G,[],Nodes,0,CLL0,LE,[]),
   !
  ;
   derive_bdd_nodes(DB,NEx,[],Nodes,0,CLL0),
   !
  ),
  setting(random_restarts_number,N),
  format("~nInitial CLL ~f~n~n",[CLL0]),
  random_restarts(N,Nodes,CLL0,CLL,initial,Par,LE),
  format("CLL after EMBLEM = ~f~n",[CLL]),
  retract_all(Th1),
  retract_all(R2),
  !,
  end,
  update_theory(R2,Par,R3),*/
  write2('updated Theory'),nl2,
  write_rules2(R3,user_output),

  assert(mcts_best_score(CLL)),
  assert(mcts_best_theory(R3)),
  assert(mcts_theories(0)),

  assert(mcts_best_theories_iteration([])),

  mcts(R3,CLL,DB),
  %assert(mcts_best_by_cll(-1e20)),
  %assert(mcts_best_theory_by_cll([])),
  %assert(mcts_best_by_visits(-1e20)),
  %select_the_best_bycll,
  %select_the_best_byvisits,

  retract(mcts_best_theories_iteration(BestsIter)),
  format2("\nBests found at : ~w",[BestsIter]),

  retract(mcts_theories(_)),
  retract(mcts_best_score(CLLNew)),
  retract(mcts_best_theory(RNew)),

  (M:local_setting(mcts_covering,true) ->

    M:local_setting(mcts_maxrestarts,MctsRestarts),
    mcts_restart(CurrentRestart),

    Improvement is CLLNew - CLL,
    ( (CLLNew > CLL, Improvement > 0.1, CurrentRestart =< MctsRestarts) ->

      format2("\n---------------- Improvement ~w",[Improvement]),
      retractall(node(_, _, _, _, _, _, _)),
      retract(M:local_setting(max_rules,ParRules)),
      ParRules1 is ParRules + 1,
      assert(M:local_setting(max_rules,ParRules1)),
      retract(mcts_restart(Restart)),
      Restart1 is Restart + 1,
      assert(mcts_restart(Restart1)),
      learn_struct_mcts(DB,RNew,R,CLL1)
    ;
      CLL1 = CLLNew,
      R = RNew
    )
  ;
    CLL1 = CLLNew,
    R = RNew
  ).



mcts(InitialTheory,InitialScore,DB):-
  % node(ID, CHILDRENS, PARENT, CLL, Theory, VISITED, BACKSCORE)
  input_module(M),
  assert(node(1, [], 0, InitialScore , InitialTheory, 0 , 0)),
  assert(lastid(1)),
  M:local_setting(mcts_iter,I),
  assert(mcts_iteration(0)),
  cycle_mcts(I,DB),
  retract(mcts_iteration(_)),
  retract(lastid(Nodes)),
  %print_graph,
  format2("\nTree size: ~w nodes.",[Nodes]).


backup_amaf(1,_Reward,_):-
  !,
  (retract(node(1, Childs, Parent , PSLL, MLN, Visited, Backscore)) ->
    true
  ;
    format(user_error,"\nNo node with ID ~w in backup",[NodeID]),
    throw(no_node_id(NodeID))
  ),
  Visited1 is Visited + 1,
  assert(node(1, Childs, Parent , PSLL, MLN, Visited1, Backscore)).

backup_amaf(NodeID,Reward,ParentsTranspose):-
  (retract(node(NodeID, Childs, Parent , PSLL, MLN, Visited, Backscore)) ->
    true
  ;
    format(user_error,"\nNo node with ID ~w in backup",[NodeID]),
    throw(no_node_id(NodeID))
  ),
  (member(NodeID,ParentsTranspose) ->
    Backscore1 is Backscore,
    Visited1 is Visited
    %format("~w- ",[NodeID])
  ;
    (PSLL =:= 1  ->
      Backscore1 is Backscore + Reward
    ;
      SigmoidValue is 1 / (1 - PSLL),
      ( Reward > SigmoidValue ->
        Backscore1 is Backscore + Reward
      ;
        Backscore1 is Backscore + SigmoidValue
        %Backscore1 is Backscore + Reward
      )
    ),

    Visited1 is Visited + 1
    %format("~w+ ",[NodeID])
  ),
  assert(node(NodeID, Childs, Parent , PSLL, MLN, Visited1, Backscore1)).
  %backup_amaf(Parent,Reward,ParentsTranspose).


check_amaf(NodeID,Theory,SigmoidValue,ParentsTranspose):-
  lastid(Nodes),
  format2("\nChecking amaf: node ~w, parents ~w: ",[NodeID,ParentsTranspose]),
  check_amaf(Nodes,NodeID,Theory,SigmoidValue,ParentsTranspose).

check_amaf(1,_NodeID,_,_SigmoidValue,_ParentsTranspose):-
  retract(node(1, Childs, Parent , PSLL, MLN, Visited, Backscore)),
  Visited1 is Visited + 1,
  assert(node(1, Childs, Parent , PSLL, MLN, Visited1, Backscore)),
  !.

check_amaf(Node,NodeID,Theory,SigmoidValue,ParentsTranspose):-
  Node \== NodeID,
  !,
  node(Node, _Childs, _Parent , _CLL, TheoryN, _Visited, _Backscore),
  ( subsume_theory(TheoryN,Theory) ->
    %format("\n\t ~w ~w: ",[TheoryN,Theory]),
    backup_amaf(Node,SigmoidValue,ParentsTranspose)
  ;
    true
  ),
  Node1 is Node - 1,
  check_amaf(Node1,NodeID,Theory,SigmoidValue,ParentsTranspose).

check_amaf(Node,NodeID,Theory,SigmoidValue,ParentsTranspose):-
  Node1 is Node - 1,
  check_amaf(Node1,NodeID,Theory,SigmoidValue,ParentsTranspose).


subsume_theory(Theory,TheoryN):-
  copy_term(Theory,Theory1),
  skolemize(TheoryN,TheoryN1),
  subsume_theory1(Theory1,TheoryN1),
  !.


subsume_theory1([],_).

subsume_theory1([Rule|R],TheoryN):-
  subsume_theory2(Rule,TheoryN,NewTheoryN),
  subsume_theory1(R,NewTheoryN).


subsume_theory2(Rule,[Rule1|R],R):-
  Rule = rule(_,[H: _, _: _],Body,_),
  Rule1 = rule(_,[H1: _, _: _],Body1,_),
  H = H1,
  subsume_body(Body,Body1),
  !.

subsume_theory2(Rule,[Rule1|R],[Rule1|R1]):-
  subsume_theory2(Rule,R,R1).


subsume_body(Body,Body1):-
  length(Body,L),
  length(Body1,L1),
  L =< L1,
  subsume_body1(Body,Body1).


subsume_body1([],_).

subsume_body1([L|R],Body):-
  nth1(_,Body,L,Rest),
  subsume_body1(R,Rest).


same_theory(Theory0,TheoryN):-
  copy_term(Theory0,Theory),
  length(Theory,L),
  length(TheoryN,L),
  same_theory1(Theory,TheoryN),
  !.


same_theory1([],[]).

same_theory1([Rule|R],TheoryN):-
  same_theory2(Rule,TheoryN,NewTheoryN),
  same_theory1(R,NewTheoryN).


same_theory2(Rule,[Rule1|R],R):-
  Rule = rule(_,[H: _, _: _],Body,_),
  Rule1 = rule(_,[H1: _, _: _],Body1,_),
  H = H1,
  same_body(Body,Body1),
  !.

same_theory2(Rule,[Rule1|R],[Rule1|R1]):-
  same_theory2(Rule,R,R1).


same_body(Body,Body1):-
  length(Body,L),
  length(Body1,L),
  same_body1(Body,Body1).


same_body1([],[]).

same_body1([L|R],Body):-
  nth1(_,Body,L,Rest),
  same_body1(R,Rest).


cycle_mcts(0,_):-
  !.

cycle_mcts(K,DB):-
  input_module(M),
  M:local_setting(mcts_iter,MaxI),
  Iteration is MaxI - K + 1,
  retract(mcts_iteration(_)),
  assert(mcts_iteration(Iteration)),
  format2("\nIteration ~w",[Iteration]),
  tree_policy(1,NodeID,DB,1,_Depth),
  ( node(NodeID, _Childs, _Parent , _CLL, Theory, _Visited, _Backscore) ->
  % do update with the sigmoid of the Score
  % SigmoidValue is ((1 / (1 + exp(-PSLL)))/0.5),
  % format("\n~w: ~w ~w Sigmoid ~w",[K,MLN,PSLL,SigmoidValue]),
    M:local_setting(mcts_max_depth, MaxDepth),
    random(1,MaxDepth,MaxDepth1),
    default_policy(Theory,-1e20,Reward,_,BestDefaultTheory,DB,1,MaxDepth1),
  % do update with the sigmoid of the Score
  % SigmoidValue is ((1 / (1 + exp(-Reward)))/0.5),

    (Reward=:=1->
      SigmoidValue=1e20
    ;
      SigmoidValue is 1 / (1 -  Reward)
    ),
    ( Reward =\= -1e20 ->

  % (Reward > CLL ->
  % SigmoidValue = 1
  % ;
  % SigmoidValue = 0
  % ),

      % format("\n~w: ~w \nReward ~w Sigmoid ~w",[K,Theory,Reward,SigmoidValue]),
      format2("\n[Backup reward ~w]",[SigmoidValue]),
      backup(NodeID,SigmoidValue,Parents),
      %	check_transposition(NodeID,Theory,SigmoidValue,Parents),
      check_amaf(NodeID,BestDefaultTheory,SigmoidValue,Parents)
    ;
      format2("\n--> no default policy expansion",[])
    ),
    K1 is K - 1,
    %read(_),
    cycle_mcts(K1,DB)
  ;
    format2("\n--> tree policy end",[])
  ).


prune([],_Childs1).

prune([ID|R],Childs1):-
  member(ID,Childs1),
  !,
  prune(R,Childs1).

prune([ID|R],Childs1):-
  prune_sub_tree(ID),
  prune(R,Childs1).


prune_sub_tree(ID):-
  retract(node(ID, Childs, _Parent , _CLL, _Theory, _VISITED, _BACKSCORE)),
  prune_sub_tree1(Childs).


prune_sub_tree1([]).

prune_sub_tree1([ID|R]):-
  retract(node(ID, Childs, _Parent , _CLL, _Theory, _VISITED, _BACKSCORE)),
  prune_sub_tree1(Childs),
  prune_sub_tree1(R).


tree_policy(ID,NodeID,DB,Od,Nd):-
  input_module(M),
  %check_pruning(ID),

  (retract(node(ID, Childs, Parent , CLL, Theory, VISITED, BACKSCORE)) ->
    true
  ;
    throw(no_node_id(ID))
  ),
  %format("\n Tree policy ~w ~w ~w",[Theory,VISITED, BACKSCORE]),
  format2("\n[Tree Policy ~w, ~w, ~w] ",[ID,VISITED,BACKSCORE]), flush_output,
  %( VISITED = 0, ID \= 1 ->
  ( CLL = 1, ID \= 1 ->
    score_theory(Theory,DB,CLL1,BestTheory,NewTheory),
    mcts_best_score(BestScore),

    ( M:local_setting(mcts_covering,true) ->
      length(NewTheory,NewTheoryL),	%lemurc
      length(Theory,TheoryL),
      ( NewTheoryL = TheoryL ->
        LengthCondition = true
      ;
        LengthCondition = false
      )
    ;
      LengthCondition = true
    ),


    ( ( CLL1 > BestScore, LengthCondition = true) ->
        format2("\n[New best score: ~w ~w]",[CLL1, BestTheory]),flush_output,


      retract(mcts_best_score(_)),
      retract(mcts_best_theory(_)),
      assert(mcts_best_score(CLL1)),
      assert(mcts_best_theory(NewTheory)),

      retract(mcts_best_theories_iteration(BestsIter)),
      mcts_iteration(Iteration),
      append(BestsIter,[Iteration],BestsIter1),
      assert(mcts_best_theories_iteration(BestsIter1)),

      retract(mcts_theories(Mlns)),
      Mlns1 is Mlns + 1,
      assert(mcts_theories(Mlns1))
    ;
      true
    )
 ;
    CLL1 = CLL,
    NewTheory = Theory
  ),

  Visited1 is VISITED + 1,

  % (CLL = 1 ->
  % Visited2 = Visited1,
  %	(Visited2 == 2 -> Backscore1 = BACKSCORE ; Backscore1 = 0) % in this case the node has been visited by transposition
  %	;
  %	Visited2 = Visited1,
  %	Backscore1 = BACKSCORE
  %	),

  Visited2 = Visited1,
  Backscore1 = BACKSCORE,


  (Childs == [] ->
    Nd = Od,
    expand(ID, Theory, CLL1, DB, NodeID, Childs1),
    assert(node(ID, Childs1, Parent , CLL1, NewTheory, Visited2, Backscore1))
  ;
    Od1 is Od + 1,
    minmaxvalue(Childs,MinV,MaxV),
    %mean_value_level(Childs,Mvl),
    once(uct(Childs, VISITED, MinV, MaxV, BestChild)),
    %once(uct(Childs, VISITED, BestChild)),
    tree_policy(BestChild,NodeID,DB,Od1, Nd),
    assert(node(ID, Childs, Parent , CLL1, NewTheory, Visited2, Backscore1))
  ).


default_policy(_Theory, Reward, Reward, BestDefaultTheory,BestDefaultTheory,_DB, Depth, MaxDepth):-
  Depth > MaxDepth,
  !.

default_policy(Theory,PrevR,Reward,PrevBestDefaultTheory,BestDefaultTheory,DB,Depth,MaxDepth):-
  input_module(M),
  %format("\nDefault policy",[]),flush_output,
  format2("\n[Default Policy ~w]",[Depth]),
  theory_revisions_r(Theory,Revisions),
  ( Revisions \== [] ->
    length(Revisions,L),
    random(0,L,K),
    nth0(K, Revisions,Spec),
    Depth1 is Depth + 1,
    score_theory(Spec,DB,Score,BestTheory,NewTheory),
    ( M:local_setting(mcts_covering,true) ->
      length(NewTheory,NewTheoryL),	%lemurc
      length(Spec,TheoryL),
      ( NewTheoryL = TheoryL ->
        LengthCondition = true
      ;
        LengthCondition = false
      )
    ;
      LengthCondition = true
    ),


    (( Score > PrevR, LengthCondition = true) ->
       Reward1 = Score,
       BestDefaultTheory1 = NewTheory
  ;
       Reward1 = PrevR,
       BestDefaultTheory1 = PrevBestDefaultTheory
    ),

    format2(" cll-reward ~w",[Reward1]),

    mcts_best_score(BestScore),


    ((Score > BestScore, LengthCondition = true) ->
      format2("\n[New best score: ~w ~w]",[Score, BestTheory]),flush_output,


      retract(mcts_best_score(_)),
      retract(mcts_best_theory(_)),
      assert(mcts_best_score(Score)),
      assert(mcts_best_theory(NewTheory)),

      retract(mcts_best_theories_iteration(BestsIter)),
      mcts_iteration(Iteration),
      append(BestsIter,[Iteration],BestsIter1),
      assert(mcts_best_theories_iteration(BestsIter1)),


      retract(mcts_theories(Mlns)),
      Mlns1 is Mlns + 1,
      assert(mcts_theories(Mlns1))
    ;
      true
    ),

    default_policy(Spec, Reward1,Reward, BestDefaultTheory1,BestDefaultTheory,DB, Depth1,MaxDepth)

  ;
    Reward = PrevR,
    BestDefaultTheory = PrevBestDefaultTheory

    /*
    format("\n\t Default ~w",[Theory]),
    score_theory(Theory,DB,Score,BestTheory),

    ( Score > PrevR ->
      Reward = Score
    ;
      Reward = PrevR
    ),

    mcts_best_score(BestScore),


    ( Score > BestScore ->
      format("\n[New best score: ~w ~w]",[Score, BestTheory]),flush_output,

      retract(mcts_best_score(_)),
      retract(mcts_best_theory(_)),
      assert(mcts_best_score(Score)),
      assert(mcts_best_theory(BestTheory)),

      retract(mcts_best_theories_iteration(BestsIter)),
      mcts_iteration(Iteration),
      append(BestsIter,[Iteration],BestsIter1),
      assert(mcts_best_theories_iteration(BestsIter1)),

      retract(mcts_theories(Mlns)),
      Mlns1 is Mlns + 1,
      assert(mcts_theories(Mlns1))
    ;
      true
    )
    */

  ).


minmaxvalue(Childs,MinV,MaxV):-
  Childs = [F|R],
  node(F, _, _ , _, _, Visits, Reward),
  ( Visits=:=0->
    V is sign(Reward)*1e20
  ;
    V is Reward / Visits
  ),
  minmaxvalue(R,V,V,MinV,MaxV).

minmaxvalue([],Min,Max,Min,Max).

minmaxvalue([C|R],PrevMin,PrevMax,MinV,MaxV):-
  node(C, _, _ , _, _, Visits, Reward),
  ( Visits=:=0->
    V is sign(Reward)*1e20
  ;
    V is Reward / Visits
  ),
  ( V > PrevMax ->
    Max1 is V
  ;
    Max1 is PrevMax
  ),
  ( V < PrevMin ->
    Min1 is V
  ;
    Min1 is PrevMin
  ),
  minmaxvalue(R,Min1,Max1,MinV,MaxV).


mean_value_level(Cs,M):-
  mean_value_level1(Cs,Me),
  length(Me,L),
  sum_list(Me,S),
  ( L=:=0->
    M is sign(S)*1e20
  ;
    M is S / L
  ).


mean_value_level1([],[]).

mean_value_level1([C|R],M1):-
  node(C, _, _ , 1, _, _Visits, _Reward),
  !,
  mean_value_level1(R,M1).

mean_value_level1([C|R],[M|Rm]):-
  node(C, _, _ , _, _, Visits, Reward),
  !,
  mean_value_level1(R,Rm),
  ( Visits=:=0->
    M is sign(Reward)*1e20
  ;
    M is (Reward / Visits)
  ).


uct(Childs, ParentVisits, Min, Max, BestChild):-
  input_module(M),
  Childs = [FirstChild|RestChilds],
  node(FirstChild, _, _ , _Score, _Theory, Visits, Reward),
  ( Visits == 0 ->
    BestChild = FirstChild
  ;
    M:local_setting(mcts_c,C),
%    ( Score == 1 ->
%      R is Mvl
%    ;
%      R is Reward
%    ),
    ( Max-Min=:=0->
      UCT is sign(Reward/Visits-Min)*1e20
    ;
      R is Reward,
      %AA is ((R / Visits) - Min ) / (Max-Min),
      %BB is 2 * C * sqrt(2 * log(ParentVisits) / Visits),
      UCT is ((R / Visits) - Min ) / (Max-Min) + 2 * C * sqrt(2 * log(ParentVisits) / Visits)
    ),
%%%    format("\nID ~w UCT ~w ~w/~w=~w ~w",[FirstChild,UCT,R,Visits,AA,BB]),
%%%    format("\n\t ~w ~w",[Score,Theory]),
%%%    format("~w ",[UCT]),
    uct(RestChilds, UCT, ParentVisits, FirstChild, Min,Max, BestChild)
  ).


uct([], _CurrentBestUCT, _ParentVisits, BestChild, _, _,BestChild).

uct([Child|RestChilds], CurrentBestUCT, ParentVisits, CurrentBestChild, Min, Max,BestChild) :-
  input_module(M),
  node(Child, _, _ , _Score, _Theory, Visits, Reward),
  ( Visits == 0 ->
    BestChild = Child
  ;
    M:local_setting(mcts_c,C),
%   ( Score == 1 ->
%     R is Mvl
%   ;
%     R is Reward
%   ),
    ( Max-Min=:=0->
      UCT is sign(Reward/Visits-Min)*1e20
    ;
      R is Reward,
      %AA is ((R / Visits) - Min ) / (Max-Min),
      %BB is 2 * C * sqrt(2 * log(ParentVisits) / Visits),
      UCT is ((R / Visits) - Min ) / (Max-Min) + 2 * C * sqrt(2 * log(ParentVisits) / Visits)
    ),
%%%   format("\nID ~w UCT ~w ~w/~w=~w ~w",[Child,UCT,R,Visits,AA,BB]),
%%%   format("\n\t ~w ~w",[Score,Theory]),
%%%   format("~w ",[UCT]),flush_output,
    ( UCT > CurrentBestUCT ->
      uct(RestChilds, UCT, ParentVisits, Child, Min, Max, BestChild)
    ;
      uct(RestChilds, CurrentBestUCT, ParentVisits, CurrentBestChild, Min, Max, BestChild)
    )
  ).


expand(ID, Theory, ParentCLL, DB, NodeID, Childs):-
  input_module(M),
  %format("  expanding...",[]),flush_output,
  theory_revisions(Theory,Revisions),
  !,
  assert_childs(Revisions,ID,ParentCLL,Childs),
  ( Childs \= [] ->
    Childs = [NodeID|_],
    retract(node(NodeID, Childs1, Parent , _, Theory1, Visited, Backscore)),
    format2("\n[Expand ~w]",[NodeID]),
    Visited1 is Visited + 1,
    score_theory(Theory1,DB,CLL,BestTheory,NewTheory),
    format2(" CLL: ~w]",[CLL]),
    %format("\nTree policy: ~w ~w]",[Score, Theory1]),
    mcts_best_score(BestScore),

    %Ratio is BestScore / CLL,
    %( Ratio > 1.001 ->

    ( M:local_setting(mcts_covering,true) ->
      length(NewTheory,NewTheoryL), %lemurc
      length(Theory1,Theory1L),
      ( NewTheoryL = Theory1L ->
        LengthCondition = true
      ;
        LengthCondition = false
      )
    ;
      LengthCondition = true
    ),


    ( ( CLL > BestScore, LengthCondition = true) ->
        format2("\n[New best score: ~w ~w]",[CLL, BestTheory]),flush_output,
        retract(mcts_best_score(_)),
        retract(mcts_best_theory(_)),
        assert(mcts_best_score(CLL)),
        assert(mcts_best_theory(NewTheory)),

        retract(mcts_best_theories_iteration(BestsIter)),
        mcts_iteration(Iteration),
        append(BestsIter,[Iteration],BestsIter1),
        assert(mcts_best_theories_iteration(BestsIter1)),


        retract(mcts_theories(Mlns)),
        Mlns1 is Mlns + 1,
        assert(mcts_theories(Mlns1))
    ;
      true
  ),
    assert(node(NodeID, Childs1, Parent , CLL, NewTheory, Visited1, Backscore))
  ;
    NodeID = -1
  ).


assert_childs([],_,_,[]).

assert_childs([Spec|Rest],P,PCLL,[ID1|Childs]):-
  %node(ID, CHILDRENS, PARENT, PSLL, MLN, VISITED, BACKSCORE)
  retract(lastid(ID)),
  %format(" ~w",[ID]),flush_output,
  ID1 is ID + 1,
  assert(lastid(ID1)),
  %SigmoidValue is ((1 / (1 + exp(-PCLL)))/0.5),
  (PCLL=:=1->
    SigmoidValue=1e20
  ;
    SigmoidValue is 1 / (1 -  PCLL)
  ),
  %format(" ~w",[ID1]),
  %score_theory(Spec,DB,CLL),
  assert(node(ID1, [], P, 1 , Spec, 1 , SigmoidValue)),
  %assert(node(ID1, [], P, 1 , Spec, 0 , 0)),
  assert_childs(Rest,P,PCLL,Childs).


theory_length([],X,X).

theory_length([T|R],K,K1):-
  theory_length(R,K,K0),
  T = rule(_,_,B,_),
  length(B,L),
  ( L > K0 ->
    K1 = L
  ;
    K1 = K0
  ).

score_theory(Theory0,DB,Score,Theory,R3):-
  ( mcts_theories(0) ->
    Theory = Theory0
  ;
    theory_length(Theory0,0,Le),
    ( Le > 1 ->
%     mcts_best_theory(TheoryBest),
%     append(TheoryBest,Theory0,Theory)
      Theory = Theory0
    ;
      Theory = Theory0
    )
  ),
  input_module(M),
  learn_params(DB, M, Theory, R3, CLL),
  write3('Updated refinement'),write3('\n'),
  write_rules3(R3,user_output),
  Score = CLL,
  !.


backup(1,_Reward,[]):-
  !.

backup(NodeID,Reward,[Parent|R]):-
  ( retract(node(NodeID, Childs, Parent , PSLL, MLN, Visited, Backscore)) ->
    true
  ;
    format2(user_error,"\nNo node with ID ~w in backup",[NodeID]),
    throw(no_node_id(NodeID))
  ),
  ( PSLL=:=1->
    SigmoidValue=1e20
  ;
    SigmoidValue is 1 / (1 -  PSLL)
  ),
  ( Reward > SigmoidValue ->
    Backscore1 is Backscore + Reward,
    Reward1 is Reward
  ;
    Backscore1 is Backscore + SigmoidValue,
    Reward1 is SigmoidValue
    %Backscore1 is Backscore + Reward,
    %Reward1 is Reward
    ),
    %format("\n backup ~w ~w",[NodeID,MLN]),
    assert(node(NodeID, Childs, Parent , PSLL, MLN, Visited, Backscore1)),
    backup(Parent,Reward1,R).



/* slipcover_lemur.pl START*/
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
  input_module(M),
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
    M:local_setting(logzero,LZ),
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
  input_module(M),
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
  input_module(M),
  M:local_setting(random_restarts_number,NMax),
  Num is NMax-N+1,
  format3("Restart number ~d~n~n",[Num]),
  randomize(ExData),
  M:local_setting(epsilon_em,EA),
  M:local_setting(epsilon_em_fraction,ER),
  M:local_setting(iter,Iter),
  em(ExData,Nodes,EA,ER,Iter,CLL,Par1,ExP),
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
  input_module(M),
  M:local_setting(random_restarts_REFnumber,NMax),
  Num is NMax-N+1,
  format3("Restart number ~d~n~n",[Num]),
  M:local_setting(epsilon_em,EA),
  M:local_setting(epsilon_em_fraction,ER),
  M:local_setting(iterREF,Iter),
  em(ExData,Nodes,EA,ER,Iter,CLLR,Par1,ExP),
  score(LE,ExP,CLLR,ScoreR),
  format3("Random_restart: Score ~f~n",[ScoreR]),
  N1 is N-1,
  (ScoreR>Score0->
    random_restarts_ref(N1,ExData,Nodes,ScoreR,Score,Par1,Par,LE)
  ;
    random_restarts_ref(N1,ExData,Nodes,Score0,Score,Par0,Par,LE)
  ).


score(_LE,_ExP,CLL,CLL):-
  input_module(M),
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
  ( E= (\+ _ )->
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
  ( R0=:=0,P0=:=0->
    Flag=true
  ;
    Flag=false
  ),
  area(Points,Flag,Pos,0,0,0,A).


compute_curve_points([],_P0,TP,FP,_FN,_TN,[1.0-Prec]):-!,
  Prec is TP/(TP+FP).

compute_curve_points([P- (\+ _)|T],P0,TP,FP,FN,TN,Pr):-!,
  ( P<P0->
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
  ( P<P0->
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
  ( TPB=:=0->
    A1=A0,
    FPB=0
  ;
    R_1 is TPA/Pos,
    ( TPA=:=0->
      ( Flag=false->
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

generate_file_name(File,Ext,FileExt):-
  name(File,FileString),
  append(FileString,Ext,FileStringExt),
  name(FileExt,FileStringExt).


write_param(initial,S):-!,
  format2("~nInitial parameters~n",[]),
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
	%write(Lit1),nl,
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
	%write(_Rest),nl,
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


deduct(0,_Mod,_DB,Th,Th):-!.

deduct(NM,Mod,DB,InTheory0,InTheory):-
  get_head_atoms(O,Mod),
  sample(1,DB,Sampled,DB1),
  (Sampled=[M]->
    generate_head(O,M,Mod,[],HL),
    %gtrace,
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


scan_pred_list([],[],[]).

scan_pred_list([P/Ar|T],[(P/Ar,LHP)|LH],[(P/Ar,[])|T1]):-
  output(P/Ar),!,
  findall(A,C^(modeh(C,A),functor(A,P,Ar)),LHS),
  findall((A,B,Cons,D),(C,At)^(modeh(C,A,B,Cons,D),member(At,A),functor(At,P,Ar)),LHC),
  append(LHS,LHC,LHP),
  scan_pred_list(T,LH,T1).

scan_pred_list([P/Ar|T],[(P/Ar,LHP)|LH],[(P/Ar,[])|T1]):-
  findall(A,C^(modeh(C,A),functor(A,P,Ar)),LHS),
  findall((A,B,Cons,D),(C,At,At1,P1,Ar1)^(
    modeh(C,A,B,Cons,D),member(At,A),functor(At,P,Ar),
    member(At1,A),functor(At1,P1,Ar1),\+ output(P1/Ar1)),LHC),
  append(LHS,LHC,LHP),
  scan_pred_list(T,LH,T1).


head_predicate(P/Ar):-
  modeh(_C,A),
  functor(A,P,Ar).

head_predicate(P/Ar):-
  modeh(_C,A,_B,_Cons,_D),
  member(At,A),
  functor(At,P,Ar).


generate_top_cl([],[]):-!.

generate_top_cl([A|T],[(rule(R,[A1:0.5,'':0.5],[],true),-1e20)|TR]):-
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  A1=..[F|Arg],
  get_next_rule_number(R),
  generate_top_cl(T,TR).


generate_head_ex([],_M,[],[]).

generate_head_ex([(P/A,L)|T],M,[(P/A,LH)|LHT],[(P/A,LH1)|LHT1]):-
  generate_head_pred(L,M,[],LHP),
  append(LH,LHP,LH1),
  generate_head_ex(T,M,LHT,LHT1).


generate_head_pred([],_M,HL,HL):-!.

generate_head_pred([(A,G,Cons,D)|T],M,H0,H1):-!,
  input_module(M),
  generate_head_goal(G,M,Goals),
  findall((A,Goals,D),(member(Goal,Goals),call(Goal),call(Cons),ground(Goals)),L),
  M:local_setting(initial_clauses_per_megaex,IC),   %IC: represents how many samples are extracted from the list L of example
  sample(IC,L,L1),   %+IC,L, -L1
  append(H0,L1,H2),
  generate_head_pred(T,M,H2,H1).

generate_head_pred([A|T],M,H0,H1):-
  input_module(M),
  functor(A,F,N),
  functor(F1,F,N),
  F1=..[F|Arg],
  Pred1=..[F,M|Arg],
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  findall((A,Pred1),call(Pred1),L),
  M:local_setting(initial_clauses_per_megaex,IC),   %IC: represents how many samples are extracted from the list L of example
  sample(IC,L,L1),   %+IC,L, -L1
  append(H0,L1,H2),
  generate_head_pred(T,M,H2,H1).


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


generate_body([],[]).

generate_body([(P/A,LH)|T],[(P/A,LR)|TR]):-
  generate_body_pred(LH,LR),
  generate_body(T,TR).


generate_body_pred([],[]):-!.

generate_body_pred([(A,H,Det)|T],[(rule(R,HP,[],BodyList),-1e20)|CL0]):-!,
  input_module(M),
  get_modeb(Det,[],BL),
  get_args(A,H,Pairs,[],Args,[],ArgsTypes,M),
  M:local_setting(d,D),
  cycle_modeb(ArgsTypes,Args,[],[],BL,a,[],BLout0,D,M),
  remove_duplicates(BLout0,BLout),
  variabilize((Pairs:-BLout),CLV),  %+(Head):-Bodylist;  -CLV:(Head):-Bodylist with variables _num in place of constants
  CLV=(Head1:-BodyList1),
  remove_int_atom_list(Head1,Head),
  remove_int_atom_list(BodyList1,BodyList),
  get_next_rule_number(R),
  length(Head,LH),
  Prob is 1/(LH+1),
  gen_head(Head,Prob,HP),
  copy_term((HP,BodyList),(HeadV,BodyListV)),
  numbervars((HeadV,BodyListV),0,_V),
  format("Bottom clause: example ~p~nClause~n",[H]),
  write_disj_clause(user_output,(HeadV:-BodyListV)),
  generate_body_pred(T,CL0).

generate_body_pred([(A,H)|T],[(rule(R,[Head:0.5,'':0.5],[],BodyList),-1e20)|CL0]):-
  input_module(M),
  functor(A,F,AA),
  findall((R,B),(modeb(R,B),functor(B,FB,AB),determination(F/AA,FB/AB)),BL),
  A=..[F|ArgsTypes],
  H=..[F,M|Args],
  M:local_setting(d,D),
  cycle_modeb(ArgsTypes,Args,[],[],BL,a,[],BLout0,D,M),
  remove_duplicates(BLout0,BLout),
  variabilize(([(H,A)]:-BLout),CLV),  %+(Head):-Bodylist;  -CLV:(Head):-Bodylist with variables _num in place of constants
  CLV=([Head1]:-BodyList1),
  remove_int_atom(Head1,Head),
  remove_int_atom_list(BodyList1,BodyList),
  get_next_rule_number(R),
  copy_term((Head,BodyList),(HeadV,BodyListV)),
  numbervars((HeadV,BodyListV),0,_V),
  format("Bottom clause: example ~p~nClause~n~p:0.5 :-~n",[H,HeadV]),
  write_body(user_output,BodyListV),
  generate_body_pred(T,CL0).


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
    R1= 1e20
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


/*:-[inference_lemur]. */
/* inference_lemur.pl START */

:-dynamic p/2,rule_n/1,setting/2.


rule_n(0).


local_setting(A,B):-
	setting(A,B).


load(FileIn,C1,R):-
  open(FileIn,read,SI),
  read_clauses_dir(SI,C),
  close(SI),
  process_clauses(C,[],C1,[],R).


add_inter_cl(CL):-
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
  input_module(M),
  retract(M:rule_sc_n(R)),
  R1 is R+1,
  assert(M:rule_sc_n(R1)).


get_node(\+ Goal,M,Env,BDD):-
  input_module(M),
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
  input_module(M),
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

generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module):-
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(Body,pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,N,B),pita:and(Env,BDDAnd,B,BDD))).


generate_rules([],_Env,_Body,_VC,_R,_Probs,_BDDAnd,_N,[],_Module).

generate_rules([Head:_P1,'':_P2],Env,Body,VC,R,Probs,BDDAnd,N,[Clause],Module):-!,
  generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module).

generate_rules([Head:_P|T],Env,Body,VC,R,Probs,BDDAnd,N,[Clause|Clauses],Module):-
  generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module),
  N1 is N+1,
  generate_rules(T,Env,Body,VC,R,Probs,BDDAnd,N1,Clauses,Module).


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

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,
[neg(H1)|Rest],Module):-
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



process_body_def_db([],BDD,BDD,_DB,Vars,Vars,[],_Module).

process_body_def_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Module):-
  builtin(H),!,
  process_body_def_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_def_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Module):-
  db(H),!,
  process_body_def_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_def_db([\+H|T],BDD,BDD1,DB,Vars,Vars1,
[(((neg(H1);\+ H1),one(BDDN));(bagof(BDDH,H2,L)->or_list(L,BDDL),bdd_not(BDDL,BDDN);one(BDDN))),
  and(BDD,BDDN,BDD2)|Rest],Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg_db(H,BDDH,DB,Module,H2),
  process_body_def_db(T,BDD2,BDD1,DB,Vars,Vars1,Rest,Module).

process_body_def_db([\+H|T],BDD,BDD1,DB,Vars,Vars1,
[neg(H1)|Rest],Module):-
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
  and(BDD,BDDN,BDD2)|Rest],Module):-
  given(H),!,
  add_mod_arg(H,Module,H1),
  add_bdd_arg(H,BDDH,Module,H2),
  process_body_def(T,BDD2,BDD1,Vars,Vars1,Rest,Module).

process_body_def([\+H|T],BDD,BDD1,Vars,Vars1,
[neg(H1)|Rest],Module):-
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


given(H):-
  input_module(M),
  functor(H,P,Ar),
  (M:input(P/Ar)).


given_cw(H):-
  input_module(M),
  functor(H,P,Ar),
  (M:input_cw(P/Ar)).


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
  input_module(M),
  retract(M:local_setting(Parameter,_)),
  assert(M:local_setting(Parameter,Value)).


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


add_bdd_arg(A,Env,BDD,A1):-
  A=..[P|Args],
  append(Args,[Env,BDD],Args1),
  A1=..[P|Args1].

add_bdd_arg(A,Env,BDD,Module,A1):-
  A=..[P|Args],
  append(Args,[Env,BDD],Args1),
  A1=..[P,Module|Args1].


add_bdd_arg_db(A,Env,BDD,DB,A1):-
  A=..[P|Args],
  append(Args,[DB,Env,BDD],Args1),
  A1=..[P|Args1].

add_bdd_arg_db(A,Env,BDD,DB,Module,A1):-
  A=..[P|Args],
  append(Args,[DB,Env,BDD],Args1),
  A1=..[P,Module|Args1].


add_mod_arg(A,Module,A1):-
  A=..[P|Args],
  A1=..[P,Module|Args].


process_head(HeadList, GroundHeadList) :-
  ground_prob(HeadList), !,
  process_head_ground(HeadList, 0, GroundHeadList).

process_head(HeadList, HeadList).


process_head_ground([Head:ProbHead], Prob, [Head:ProbHead1|Null]) :-!,
  input_module(M),
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
  %disjunctive clause with more than one head atom senza depth_bound
  process_body_cw(BodyList,BDD,BDDAnd,[],_Vars,BodyList1,Module),
  append([pita:one(Env,BDD)],BodyList1,BodyList2),
  list2and(BodyList2,Body1),
  append(HeadList,BodyList,List),
  extract_vars_list(List,[],VC),
  get_probs(HeadList,Probs),
  input_module(M),
  (M:local_setting(single_var,true)->
    generate_rules(HeadList,Env,Body1,[],N,Probs,BDDAnd,0,Clauses,Module)
  ;
    generate_rules(HeadList,Env,Body1,VC,N,Probs,BDDAnd,0,Clauses,Module)
  ),
  N1 is N+1.

gen_clause_cw(def_rule(H,BodyList,Lit),N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
  %disjunctive clause with a single head atom senza depth_bound con prob =1
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
  input_module(M),
  M:local_setting(depth_bound,true),!,
  %disjunctive clause with more than one head atom e depth_bound
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
  input_module(M),
  %disjunctive clause with more than one head atom senza depth_bound
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
  %disjunctive clause with a single head atom e depth_bound
  input_module(M),
  M:local_setting(depth_bound,true),!,
  process_body_db(BodyList,BDD,BDDAnd,DB,[],_Vars,BodyList2,Env,Module),
  append([pita:one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body1),
  add_bdd_arg_db(H,Env,BDDAnd,DBH,Module,Head1),
  Clauses=[(Head1 :- (DBH>=1,DB is DBH-1,Body1))].

gen_clause(def_rule(H,BodyList,Lit),N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
  %disjunctive clause with a single head atom senza depth_bound con prob =1
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
builtin(nth1(_,_,_)).
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
  input_module(M),
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
  input_module(M),
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
  input_module(M),
  M:local_setting(compiling,on),
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  Head = (_H:P),P=:=0.0, !.

term_expansion_int((Head :- Body), (Clauses,[def_rule(H,BodyList,true)])) :-
% disjunctive clause with a single head atom e depth_bound
  input_module(M),
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
  input_module(M),
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
  input_module(M),
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
  input_module(M),
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
  input_module(M),
  M:local_setting(compiling,on),
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),
  Head=db(Head1),!,
  Clauses=(Head1 :- Body).

term_expansion_int((Head :- Body),(Clauses,[def_rule(Head,BodyList,true)])) :-
% definite clause with depth_bound
  input_module(M),
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
  input_module(M),
  M:local_setting(compiling,on),
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module),
  append([pita:one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Head,Env,BDDAnd,Module,Head1),
  Clauses=(Head1 :- Body2).

term_expansion_int(Head,(Clauses,[rule(R,HeadList,[],true)])) :-
  input_module(M),
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
  input_module(M),
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
  input_module(M),
  M:local_setting(compiling,on),
% disjunctive fact with a single head atom con prob. 0
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (_H:P),P=:=0.0, !.

term_expansion_int(Head,(Clause,[def_rule(H,[],true)])) :-
  input_module(M),
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% disjunctive fact with a single head atom con prob.1 e db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),P=:=1.0, !,
  list2and([pita:one(Env,BDD)],Body1),
  add_bdd_arg_db(H,Env,BDD,_DB,_Module,Head1),
  Clause=(Head1 :- Body1).

term_expansion_int(Head,(Clause,[def_rule(H,[],true)])) :-
  input_module(M),
  M:local_setting(compiling,on),
% disjunctive fact with a single head atom con prob. 1, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head = (H:P),P=:=1.0, !,
  list2and([pita:one(Env,BDD)],Body1),
  add_bdd_arg(H,Env,BDD,_Module,Head1),
  Clause=(Head1 :- Body1).

term_expansion_int(Head,(Clause,[rule(R,HeadList,[],true)])) :-
  input_module(M),
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
  input_module(M),
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
  input_module(M),
  M:local_setting(compiling,on),
  M:local_setting(depth_bound,true),
% definite fact with db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg_db(Head,Env,One,_DB,_Module,Head1).

term_expansion_int(Head, ((Head1:-pita:one(Env,One)),[def_rule(Head,[],true)])) :-
  input_module(M),
  M:local_setting(compiling,on),
% definite fact without db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg(Head,Env,One,_Module,Head1).


/* inference_lemur.pl END */


theory_revisions_op(Theory,TheoryRevs):-
  setof(RevOp, Theory^revise_theory(Theory,RevOp), TheoryRevs),!.

theory_revisions_op(_Theory,[]).


theory_revisions_r(Theory,TheoryRevs):-
  theory_revisions_op(Theory,TheoryRevs1),
  %filter_add_rule(TheoryRevs11,TheoryRevs1),

  ( TheoryRevs1 == [] ->
    TheoryRevs = []
  ;
    length(TheoryRevs1,L),
    random(0,L,K),
    nth0(K, TheoryRevs1,Revision),
    apply_operators([Revision],Theory,TheoryRevs)
  ).


theory_revisions(Theory,TheoryRevs):-
  theory_revisions_op(Theory,TheoryRevs1),
  apply_operators(TheoryRevs1,Theory,TheoryRevs).


apply_operators([],_Theory,[]).

apply_operators([add(Rule)|RestOps],Theory,[NewTheory|RestTheory]) :-!,
  append(Theory, [Rule], NewTheory),
  %nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([add_body(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-!,
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
  %nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove_body(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-!,
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
  %nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([add_head(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-!,
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
  %nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove_head(Rule1,Rule2,_A)|RestOps],Theory,[NewTheory|RestTheory]) :-!,
  delete_matching(Theory,Rule1,Theory1),
  append(Theory1, [Rule2], NewTheory),
  %nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).

apply_operators([remove(Rule)|RestOps],Theory,[NewTheory|RestTheory]) :-
  delete_matching(Theory,Rule,NewTheory),
  %nl,write(NewTheory),
  apply_operators(RestOps,Theory,RestTheory).


revise_theory(Theory,Ref):-
  specialize_theory(Theory,Ref).

revise_theory(Theory,Ref):-
  generalize_theory(Theory,Ref).


generalize_theory(Theory,Ref):-
  input_module(M),
  length(Theory,LT),
  M:local_setting(max_rules,MR),
  LT<MR,
  add_rule(Ref).


add_rule(add(rule(ID,Head,[],Lits))):-
  input_module(M),
  M:local_setting(specialization,bottom),!,
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

add_rule(add(SpecRule)):-
  input_module(M),
  findall(HL , M:modeh(_,HL), HLS),
  length(HLS,L),
  L1 is L+1,
  P is 1/L1,
  generate_head(HLS,P,Head),
  get_next_rule_number(ID),
  Rule0 = rule(ID,Head,[],true),
  specialize_rule(Rule0,SpecRule,_Lit).


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

generate_head([H|_T],_P,[H1:0.5,'':0.5]):-
  H=..[Pred|Args],
  length(Args,LA),
  length(Args1,LA),
  H1=..[Pred|Args1].

generate_head([_H|T],P,Head):-
  generate_head(T,P,Head).


generalize_head1(LH,LH1,NH):-
  input_module(M),
  findall(HL , M:modeh(_,HL), HLS),
  generalize_head2(HLS,LH,LH1,NH).


generalize_head2([X|_R],LH,LH1,PH) :-
  input_module(M),
  X =.. [P|A],
  length(A,LA),
  length(A1,LA),
  PH =.. [P|A1],
  \+ member(PH:_, LH),
  (M:local_setting(new_head_atoms_zero_prob,true)->
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
  Ref = add_body(Rule,SpecRule,Lit),
  SpecRule = rule(_,_,_B,_).


specialize_rule(Rule,SpecRule,Lit):-
  input_module(M),
  M:local_setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
  delete_one(Lits,RLits,Lit),
  \+ lookahead_cons(Lit,_),
  append(BL,[Lit],BL1),
  remove_prob(LH,LH1),
  %check_ref(LH1,BL1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,LH2),
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,RLits).

specialize_rule(Rule,SpecRule,Lit):-
  input_module(M),
  M:local_setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
  delete_one(Lits,RLits,Lit),
  append(BL,[Lit],BL0),
  (lookahead(Lit,LLit1);lookahead_cons(Lit,LLit1)),  % lookahead_cons serve a dire che rating(_A,_B,_C) e aggiunto solo  insieme ai letterali indicati nella lista, mai da solo.
  copy_term(LLit1,LLit2),
  specialize_rule_la_bot(LLit2,RLits,RLits1,BL0,BL1),
  remove_prob(LH,LH1),
  %check_ref(LH1,BL1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,LH2),
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,RLits1).

specialize_rule(Rule,SpecRule,Lit):-
  input_module(M),
  M:local_setting(specialization,mode),!,
  %findall(BL , modeb(_,BL), BLS),
  mcts_modeb(BSL0),
  Rule = rule(_ID,_LH,BL,_),
  ( BL \= [] ->
    %last(BL,LastLit),
    %LastLit =.. [Pred|_],
    %filter_modeb(BSL0,LastLit,BSL)
    BSL = BSL0
  ;
    BSL = BSL0
  ),
  specialize_rule(BSL,Rule,SpecRule,Lit).


filter_modeb([],_Pred,[]).

filter_modeb([Modeb|RestModeb],Pred,[Modeb|RestBSL]):-
  Modeb =.. [_PredMode|_],
  Modeb @>= Pred,
  !,
  filter_modeb(RestModeb,Pred,RestBSL).

filter_modeb([_|RestModeb],Pred,RestBSL):-
  filter_modeb(RestModeb,Pred,RestBSL).


skolemize(Theory,Theory1):-
  copy_term(Theory,Theory1),
  term_variables(Theory1,Vars),
  skolemize1(Vars,1).


skolemize1([],_).

skolemize1([Var|R],K):-
  atomic_list_concat([s,K],Skolem),
  Var = Skolem,
  K1 is K + 1,
  skolemize1(R,K1).


banned_clause(H,B):-
  skolemize([H,B],[H1,B1]),
  banned(H2,B2),
  mysublist(H2,H1),
  mysublist(B2,B1).


mysublist([],_).

mysublist([A\==B|T],L):-
  !,
  A\==B,
  mysublist(T,L).

mysublist([H|T],L):-
  nth1(_,L,H,R),
  mysublist(T,R).


specialize_rule([Lit|_RLit],Rule,SpecRul,SLit):-
  input_module(M),
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
  M:local_setting(max_var,MV),
  NV=<MV,
  \+ banned_clause(LH1,BL2),
  SpecRul = rule(ID,LH,BL2,true).

specialize_rule([Lit|_RLit],Rule,SpecRul,SLit):-
  input_module(M),
  Rule = rule(ID,LH,BL,true),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,ALL,SLit),

  %\+ member(SLit,LH1)

  \+ lookahead_cons(SLit,_),

  append(BL,[SLit],BL1),
  append(LH1,BL1,ALL1),
  extract_fancy_vars(ALL1,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  M:local_setting(maxdepth_var,_MD),
  \+ banned_clause(LH1,BL1),
  SpecRul = rule(ID,LH,BL1,true).


specialize_rule([_|RLit],Rule,SpecRul,Lit):-
  specialize_rule(RLit,Rule,SpecRul,Lit).


specialize_rule_la([],_LH1,BL1,BL1).

specialize_rule_la([Lit1|T],LH1,BL1,BL3):-
  copy_term(Lit1,Lit2),
  input_module(M),
  M:modeb(_,Lit2),
  append(LH1,BL1,ALL1),
  specialize_rule1(Lit2,ALL1,SLit1),
  append(BL1,[SLit1],BL2),
  specialize_rule_la(T,LH1,BL2,BL3).


specialize_rule_la_bot([],Bot,Bot,BL,BL).

specialize_rule_la_bot([Lit|T],Bot0,Bot,BL1,BL3):-
  delete_one(Bot0,Bot1,Lit),
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
  input_module(M),
  M:modeb(_,Lit1),
  Lit1 =.. [P|Args1],
  convert_to_input_vars(Args1,Args2),
  Lit2 =.. [P|Args2],
  input_vars(Lit0,Lit2,InputVars).

input_variables(LitM,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  input_module(M),
  M:modeb(_,Lit1),
  input_vars(LitM,Lit1,InputVars).

input_variables(LitM,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  input_module(M),
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
  input_module(M),
  M:modeh(_,Lit),!.

take_mode(Lit):-
  input_module(M),
  M:modeb(_,Lit),!.

take_mode(Lit):-
  input_module(M),
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
  input_module(M),
  ( M:local_setting(mcts_covering,true)	->
    mcts_restart(Restart),
    nth1(K,Theory,Rule),
    K >= Restart
  ;
    member(Rule,Theory)
  ).
  %last(Theory,Rule).


add_rule(Theory,add(rule(ID,H,[],true))):-
  input_module(M),
  new_id(ID),
  findall(HL , M:modeh(_,HL), HLS),
  length(HLS,NH),
  P is 1/(NH+1),
  add_probs(HLS,H,P),
  \+ member(rule(_,H,[],true),Theory).

add_rule(Theory,TheoryGen):-
  input_module(M),
  findall(HL , M;modeh(_,HL), HLS),
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


/* dv_lemur.pl START*/
t(DV):- % dv([advisedby(A,B)],[taughtby(C,B,D),ta(C,A,D)],DV).
  dv([advisedby(A,B)],[publication(C,B),publication(C,A),professor(B),student(A)],DV).

  %dv([professor(A)],[taughtby(B,A,C),taughtby(D,A,E),taughtby(D,A,E)],DV).  %max_var 5


dv(H,B,DV1):-			%-DV1
  term_variables(H,V),
  head_depth(V,DV0),
  findall((MD-DV),var_depth(B,DV0,DV,0,MD),LDs), % cerchiamo tutte le possibili liste di coppie var-prof che si possono generare in base alle scelte del modeb e poi prendiamo la lista che porta al massimo della profondita massima
  get_max(LDs,-1,-,DV1).


input_variables_b(LitM,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  input_module(M),
  M:modeb(_,Lit1),
  input_vars(LitM,Lit1,InputVars).


depth_var_head(List,VarsD):-   % exit:depth_var_head([professor(_G131537)],[[_G131537,0]]) ?
  term_variables(List,Vars0),   %List = lista atomi testa, Vars0 = lista variabili estratte dalla testa (term_variables  _710033,_710237,_711016,_710969).
  head_depth(Vars0,VarsD).	%aggiunge la profondità 0 ad ogni variabile, creando sottoliste


head_depth([],[]).

head_depth([V|R],[[V,0]|R1]):-
  head_depth(R,R1).


var_depth([],PrevDs1,PrevDs1,MD,MD):-!.

var_depth([L|R],PrevDs,PrevDs1,_MD,MD):-    		%L=body atom
  %MD e' la profondita' massima a cui si e' arrivati
  input_variables_b(L,InputVars),          	%variabili di input nell'atomo L

  %write(L),format("~n variabili di input:",[]),write_list(InputVars),  %L=letterale del body=ta(_710237,_710858,_711331) InputVars = variabili di input nel letterale=_710237,_710858.
  term_variables(L, BodyAtomVars),   		   %BodyAtomVars: estrae dal letterale Lit del body la lista di variabili
  output_vars(BodyAtomVars,InputVars,OutputVars),  %OutputVars = BodyAtomVars-InputVars
  depth_InputVars(InputVars,PrevDs,0,MaxD),   %MaxD: massima profondita' delle variabili di input presenti nel letterale
  D is MaxD+1,
  compute_depth(OutputVars,D,PrevDs,PrevDs0),  %Ds: lista di liste [v,d] per tutte le  variabili (assegna D a tutte le variabili)

  %term_variables(PrevLits,PrevVars),     	%PrevVars: lista variabili nella testa
  %write(BodyD),
  %PrevDs1 = [BodyD|PrevDs].
  var_depth(R,PrevDs0,PrevDs1,D,MD).


get_max([],_,Ds,Ds).

get_max([(MD-DsH)|T],MD0,_Ds0,Ds):-
  MD>MD0,!,
  get_max(T,MD,DsH,Ds).

get_max([_H|T],MD,Ds0,Ds):-
  get_max(T,MD,Ds0,Ds).


output_vars(OutVars,[],OutVars):-!.

output_vars(BodyAtomVars,[I|InputVars],OutVars):-	%esclude le variabili di input dalla lista di var del letterale del body
  delete(BodyAtomVars, I, Residue),   			%cancella I da BodyAtomVars
  output_vars(Residue,InputVars, OutVars).


% restituisce in D la profondita massima delle variabili presenti nella lista passata come primo argomento
depth_InputVars([],_,D,D).

depth_InputVars([I|Input],PrevDs,D0,D):-
  member_l(PrevDs,I,MD),
  ( MD>D0->
    D1=MD
  ;
    D1=D0
  ),
  depth_InputVars(Input,PrevDs,D1,D).


member_l([[L,D]|_P],I,D):-   %resituisce in output la profondita della variabile I
  I==L,!.

member_l([_|P],I,D):-
  member_l(P,I,D).


compute_depth([],_,PD,PD):-!.   %LVarD

compute_depth([O|Output],D,PD,RestO):-   %LVarD
  member_l(PD,O,_),!, % variabile gia presente
  compute_depth(Output,D,PD,RestO).

compute_depth([O|Output],D,PD,[[O,D]|RestO]):-   %LVarD
  compute_depth(Output,D,PD,RestO).


exceed_depth([],_):-!.

exceed_depth([H|T],MD):-
  nth1(2,H,Dep),	%estrae la profondità
  %setting(maxdepth_var,MD),
  %(Dep>=MD ->
  %		format("*****************depth exceeded ~n")
  %	;
  %		true
  %	),
  Dep<MD,
  exceed_depth(T,MD).

/* dv_lemur END */


write2(A):-
  input_module(M),
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write(A)
  ;
    true
  ).


write3(A):-
  input_module(M),
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write(A)
  ;
    true
  ).

nl2:-
  input_module(M),
  M:local_setting(verbosity,Ver),
  (Ver>1->
    nl
  ;
    true
  ).

nl3:-
  input_module(M),
  M:local_setting(verbosity,Ver),
  (Ver>2->
    nl
  ;
    true
  ).


format2(A,B):-
  input_module(M),
  M:local_setting(verbosity,Ver),
  (Ver>1->
    format(A,B)
  ;
    true
  ).


format3(A,B):-
	input_module(M),
  M:local_setting(verbosity,Ver),
  (Ver>2->
    format(A,B)
  ;
    true
  ).


write_rules2(A,B):-
  input_module(M),
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_rules(A,B)
  ;
    true
  ).


write_rules3(A,B):-
  input_module(M),
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_rules(A,B)
  ;
    true
  ).


write_disj_clause2(A,B):-
  input_module(M),
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_disj_clause(A,B)
  ;
    true
  ).


write_disj_clause3(A,B):-
  input_module(M),
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_disj_clause(A,B)
  ;
    true
  ).


write_body2(A,B):-
  input_module(M),
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_body(A,B)
  ;
    true
  ).


write_body3(A,B):-
  input_module(M),
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_body(A,B)
  ;
    true
  ).


remove_duplicates(L0,L):-
  remove_duplicates(L0,[],L1),
  reverse(L1,L).

remove_duplicates([],L,L).

remove_duplicates([H|T],L0,L):-
  member_eq(H,L0),!,
  remove_duplicates(T,L0,L).

remove_duplicates([H|T],L0,L):-
  remove_duplicates(T,[H|L0],L).


find_ex(DB,LG,Pos,Neg):-
  input_module(M),
  findall(P/A,M:output(P/A),LP),
  M:local_setting(neg_ex,given),!,
  find_ex_pred(LP,DB,[],LG,0,Pos,0,Neg).

find_ex(DB,LG,Pos,Neg):-
  input_module(M),
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
  input_module(M),
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
  input_module(M),
  M:modeh(_,At),
  At=..[_|Args],
  get_args(Args,Types).

get_types(At,Types):-
  input_module(M),
  M:modeh(_,HT,_,_),
  member(At,HT),
  At=..[_|Args],
  get_args(Args,Types).


find_ex_db_cw([],_At,_Ty,LG,LG,Pos,Pos,Neg,Neg).

find_ex_db_cw([H|T],At,Types,LG0,LG,Pos0,Pos,Neg0,Neg):-
  input_module(M),
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
  input_module(M),
  \+ M:At1.

neg_ex([H|T],[HT|TT],At1,C):-
  member((HT,Co),C),
  member(H,Co),
  neg_ex(T,TT,At1,C).


set_lm(Parameter,Value):-
  input_module(M),
  retract(M:local_setting(Parameter,_)),
  assert(M:local_setting(Parameter,Value)).

setting_lm(P,V):-
  input_module(M),
  M:local_setting(P,V).

input_module(M):-
  input_mod(M).


%:-style_check(-discontiguous).
:- dynamic input_mod/1.

user:term_expansion((:- lemur), []) :-!,
  %write(ciao),nl,
  prolog_load_context(module, M),
  %retractall(input_mod(_)),
  %M:dynamic(model/1),
  %M:set_prolog_flag(unkonw,fail),
  findall(local_setting(P,V),default_setting_lm(P,V),L),
  assert_all(L,M,_),
  assert(input_mod(M)),
  retractall(M:rule_sc_n(_)),
  assert(M:rule_sc_n(0)),

  M:dynamic((modeh/2,modeh/4,fixed_rule/3,banned/2,lookahead/2,
    lookahead_cons/2,lookahead_cons_var/2,prob/2,output/1,input/1,input_cw/1,
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

user:term_expansion(begin(model(I)), []) :-
  input_mod(M),!,
  retractall(M:model(_)),
  assert(M:model(I)),
  assert(M:int(I)).

user:term_expansion(end(model(_I)), []) :-
  input_mod(M),!,
  retractall(M:model(_)).

user:term_expansion(At, A) :-
  %write(At),nl,
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

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(lemur:induce_lm(_,_)).
sandbox:safe_primitive(lemur:test_lm(_,_,_,_,_,_,_)).
sandbox:safe_primitive(lemur:test_lm_r(_,_,_,_,_)).
sandbox:safe_primitive(lemur:test_prob_lm(_,_,_,_,_,_)).
sandbox:safe_primitive(lemur:set_lm(_,_)).
sandbox:safe_primitive(lemur:setting_lm(_,_)).
