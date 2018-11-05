
:-module(lemur,[set_lm/2,setting_lm/2,
  induce_lm/2]).


/** <module> lemur

This module performs learning over Logic Programs with Annotated
Disjunctions and CP-Logic programs using the LEMUR algorithm of

Nicola Di Mauro, Elena Bellodi, and Fabrizio Riguzzi. 
Bandit-based Monte-Carlo structure learning of probabilistic logic programs. 
Machine Learning, 100(1):127-156, July 2015

See https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html for
details.

@author Nicola di Mauro, Fabrizio Riguzzi, Elena Bellodi
@license Artistic License 2.0
@copyright Nicola di Mauro, Fabrizio Riguzzi, Elena Bellodi
*/
:-reexport(library(slipcover)).

:-use_module(library(lists)).
:-use_module(library(random)).
:-use_module(library(system)).
:-use_module(library(terms)).
:-use_module(library(rbtrees)).


:- set_prolog_flag(discontiguous_warnings,on).
:- set_prolog_flag(single_var_warnings,on).
:- set_prolog_flag(unknown,warning).

:- dynamic db/1.
:- dynamic lm_input_mod/1.


:- meta_predicate induce_lm(:,-).
:- meta_predicate induce_rules(:,-).
:- meta_predicate set_lm(:,+).
:- meta_predicate setting_lm(:,-).
/*
	declarations start
define the depth of a variable appearing in a clause A B ^ : : : ^ Br as follows.

Variables appearing in the head of a clause have depth zero.
Otherwise, let Bi be the first literal containing the variable V, and let d be the maximal depth of the input variables of Bi
then the depth of V is d + 1. The depth of a clause is the maximal depth of any variable in the clause.

In questo modo possiamo lasciare il numero massimo di variabili a 4 (e cosi' impara la regola con taughtby) e riduciamo la profondita' massima delle variabili a 2 (in questo modo dovremmo imparare la regola con i due publication nel body e anche quella con taughtby).
Bisogna modificare revise.pl per controllare che gli atomi che si aggiungono nel body non abbiano variabili oltre la profondita' massima.
*/


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
default_setting_lm(max_iter,10).
default_setting_lm(max_iter_structure,10000).
default_setting_lm(maxdepth_var,2).
default_setting_lm(beamsize,100).
default_setting_lm(background_clauses,50).
default_setting_lm(neg_ex,cw).


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
default_setting_lm(bagof,false).
default_setting_lm(compiling,off).
default_setting_lm(depth_bound,false).  %if true, it limits the derivation of the example to the value of 'depth'
default_setting_lm(depth,2).
default_setting_lm(single_var,false). %false:1 variable for every grounding of a rule; true: 1 variable for rule (even if a rule has more groundings),simpler.
default_setting_lm(tabling,auto).
/* values:
  auto
  explicit
*/

:- thread_local database/1, lm_input_mod/1.

/**
 * induce_lm(:TrainFolds:list_of_atoms,-P:probabilistic_program) is det
 *
 * The predicate performs structure learning using the folds indicated in
 * TrainFolds for training.
 * It returns in P the learned probabilistic program.
 */
induce_lm(TrainFolds,P):-
  induce_rules(TrainFolds,P0),
  rules2terms(P0,P).



induce_rules(M:Folds,R):-
  set_lm(M:compiling,on),
  M:local_setting(seed,Seed),
  set_random(Seed),
  M:local_setting(c_seed,CSeed),
  rand_seed(CSeed),
  findall(Exs,(member(F,Folds),M:fold(F,Exs)),L),
  append(L,DB),
  assert(M:database(DB)),

  statistics(walltime,[_,_]),
  format2(M,"\nMonte Carlo Tree Search for LPAD Structure Learning\n",[]),

  (M:bg(RBG0)->
    process_clauses(RBG0,M,[],_,[],RBG),
    generate_clauses(RBG,M,_RBG1,0,[],ThBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ThBG,M,ThBGRef),
    assert_all(ClBG,M,ClBGRef)
  ;
    true
  ),
  findall(BL , M:modeb(_,BL), BLS0),
  sort(BLS0,BSL),
  assert(M:mcts_modeb(BSL)),

  assert(M:mcts_restart(1)),
  learn_struct_mcts(DB,M,[],R2,Score2),
  retract(M:mcts_restart(_)),
  learn_params(DB,M,R2,R,Score),

  format2(M,"~nRefinement score  ~f - score after EMBLEM ~f~n",[Score2,Score]),
  statistics(walltime,[_,WT]),
  WTS is WT/1000,
  write2(M,'\n\n'),
  format2(M,'Wall time ~f */~n',[WTS]),
  write_rules2(M,R,user_output),
  set_lm(M:compiling,off),
  (M:bg(RBG0)->
    retract_all(ThBGRef),
    retract_all(ClBGRef)
  ;
    true
  ).


learn_struct_mcts(DB,M,R1,R,CLL1):-
  learn_params(DB,M, R1, R3, CLL),
  write2(M,'updated Theory'),nl2(M),
  write_rules2(M,R3,user_output),

  assert(M:mcts_best_score(CLL)),
  assert(M:mcts_best_theory(R3)),
  assert(M:mcts_theories(0)),

  assert(M:mcts_best_theories_iteration([])),

  mcts(R3,M,CLL,DB),
  retract(M:mcts_best_theories_iteration(BestsIter)),
  format2(M,"\nBests found at : ~w",[BestsIter]),

  retract(M:mcts_theories(_)),
  retract(M:mcts_best_score(CLLNew)),
  retract(M:mcts_best_theory(RNew)),

  (M:local_setting(mcts_covering,true) ->

    M:local_setting(mcts_maxrestarts,MctsRestarts),
    M:mcts_restart(CurrentRestart),

    Improvement is CLLNew - CLL,
    ( (CLLNew > CLL, Improvement > 0.1, CurrentRestart =< MctsRestarts) ->

      format2(M,"\n---------------- Improvement ~w",[Improvement]),
      retractall(M:node(_, _, _, _, _, _, _)),
      retract(M:local_setting(max_rules,ParRules)),
      ParRules1 is ParRules + 1,
      assert(M:local_setting(max_rules,ParRules1)),
      retract(M:mcts_restart(Restart)),
      Restart1 is Restart + 1,
      assert(M:mcts_restart(Restart1)),
      learn_struct_mcts(DB,M,RNew,R,CLL1)
    ;
      CLL1 = CLLNew,
      R = RNew
    )
  ;
    CLL1 = CLLNew,
    R = RNew
  ).



mcts(InitialTheory,M,InitialScore,DB):-
  % node(ID, CHILDRENS, PARENT, CLL, Theory, VISITED, BACKSCORE)
  assert(M:node(1, [], 0, InitialScore , InitialTheory, 0 , 0)),
  assert(M:lastid(1)),
  M:local_setting(mcts_iter,I),
  assert(M:mcts_iteration(0)),
  cycle_mcts(I,M,DB),
  retract(M:mcts_iteration(_)),
  retract(M:lastid(Nodes)),
  %print_graph,
  format2(M,"\nTree size: ~w nodes.",[Nodes]).


backup_amaf(1,M,_Reward,_):-
  !,
  (retract(M:node(1, Childs, Parent , PSLL, MLN, Visited, Backscore)) ->
    true
  ;
    format(user_error,"\nNo node with ID ~w in backup",[NodeID]),
    throw(no_node_id(NodeID))
  ),
  Visited1 is Visited + 1,
  assert(M:node(1, Childs, Parent , PSLL, MLN, Visited1, Backscore)).

backup_amaf(NodeID,M,Reward,ParentsTranspose):-
  (retract(M:node(NodeID, Childs, Parent , PSLL, MLN, Visited, Backscore)) ->
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
  assert(M:node(NodeID, Childs, Parent , PSLL, MLN, Visited1, Backscore1)).


check_amaf(NodeID,M,Theory,SigmoidValue,ParentsTranspose):-
  M:lastid(Nodes),
  format2(M,"\nChecking amaf: node ~w, parents ~w: ",[NodeID,ParentsTranspose]),
  check_amaf(Nodes,M,NodeID,Theory,SigmoidValue,ParentsTranspose).

check_amaf(1,M,_NodeID,_,_SigmoidValue,_ParentsTranspose):-
  retract(M:node(1, Childs, Parent , PSLL, MLN, Visited, Backscore)),
  Visited1 is Visited + 1,
  assert(M:node(1, Childs, Parent , PSLL, MLN, Visited1, Backscore)),
  !.

check_amaf(Node,M,NodeID,Theory,SigmoidValue,ParentsTranspose):-
  Node \== NodeID,
  !,
  M:node(Node, _Childs, _Parent , _CLL, TheoryN, _Visited, _Backscore),
  ( subsume_theory(TheoryN,Theory) ->
    %format("\n\t ~w ~w: ",[TheoryN,Theory]),
    backup_amaf(Node,M,SigmoidValue,ParentsTranspose)
  ;
    true
  ),
  Node1 is Node - 1,
  check_amaf(Node1,M,NodeID,Theory,SigmoidValue,ParentsTranspose).

check_amaf(Node,M,NodeID,Theory,SigmoidValue,ParentsTranspose):-
  Node1 is Node - 1,
  check_amaf(Node1,M,NodeID,Theory,SigmoidValue,ParentsTranspose).


subsume_theory(Theory,TheoryN):-
  copy_term(Theory,Theory1),
  skolemize(TheoryN,TheoryN1),
  subsume_theory1(Theory1,TheoryN1),
  !.

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

subsume_theory1([],_).

subsume_theory1([Rule|R],TheoryN):-
  subsume_theory2(Rule,TheoryN,NewTheoryN),
  subsume_theory1(R,NewTheoryN).


subsume_theory2(Rule,[Rule1|R],R):-
  Rule = rule(_,[H: _, _: _],Body,_,_),
  Rule1 = rule(_,[H1: _, _: _],Body1,_,_),
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
  Rule = rule(_,[H: _, _: _],Body,_,_),
  Rule1 = rule(_,[H1: _, _: _],Body1,_,_),
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


cycle_mcts(0,_M,_):-
  !.

cycle_mcts(K,M,DB):-
  M:local_setting(mcts_iter,MaxI),
  Iteration is MaxI - K + 1,
  retract(M:mcts_iteration(_)),
  assert(M:mcts_iteration(Iteration)),
  format2(M,"\nIteration ~w",[Iteration]),
  tree_policy(1,M,NodeID,DB,1,_Depth),
  ( M:node(NodeID, _Childs, _Parent , _CLL, Theory, _Visited, _Backscore) ->
  % do update with the sigmoid of the Score
  % SigmoidValue is ((1 / (1 + exp(-PSLL)))/0.5),
  % format("\n~w: ~w ~w Sigmoid ~w",[K,MLN,PSLL,SigmoidValue]),
    M:local_setting(mcts_max_depth, MaxDepth),
    random(1,MaxDepth,MaxDepth1),
    default_policy(Theory,M,-1e20,Reward,_,BestDefaultTheory,DB,1,MaxDepth1),
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
      format2(M,"\n[Backup reward ~w]",[SigmoidValue]),
      backup(NodeID,M,SigmoidValue,Parents),
      %	check_transposition(NodeID,Theory,SigmoidValue,Parents),
      check_amaf(NodeID,M,BestDefaultTheory,SigmoidValue,Parents)
    ;
      format2(M,"\n--> no default policy expansion",[])
    ),
    K1 is K - 1,
    %read(_),
    cycle_mcts(K1,M,DB)
  ;
    format2(M,"\n--> tree policy end",[])
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


tree_policy(ID,M,NodeID,DB,Od,Nd):-
  %check_pruning(ID),

  (retract(M:node(ID, Childs, Parent , CLL, Theory, VISITED, BACKSCORE)) ->
    true
  ;
    throw(no_node_id(ID))
  ),
  %format("\n Tree policy ~w ~w ~w",[Theory,VISITED, BACKSCORE]),
  format2(M,"\n[Tree Policy ~w, ~w, ~w] ",[ID,VISITED,BACKSCORE]), flush_output,
  %( VISITED = 0, ID \= 1 ->
  ( CLL = 1, ID \= 1 ->
    score_theory(Theory,M,DB,CLL1,BestTheory,NewTheory),
    M:mcts_best_score(BestScore),

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
        format2(M,"\n[New best score: ~w ~w]",[CLL1, BestTheory]),flush_output,


      retract(M:mcts_best_score(_)),
      retract(M:mcts_best_theory(_)),
      assert(M:mcts_best_score(CLL1)),
      assert(M:mcts_best_theory(NewTheory)),

      retract(M:mcts_best_theories_iteration(BestsIter)),
      M:mcts_iteration(Iteration),
      append(BestsIter,[Iteration],BestsIter1),
      assert(M:mcts_best_theories_iteration(BestsIter1)),

      retract(M:mcts_theories(Mlns)),
      Mlns1 is Mlns + 1,
      assert(M:mcts_theories(Mlns1))
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
    expand(ID,M, Theory, CLL1, DB, NodeID, Childs1),
    assert(M:node(ID, Childs1, Parent , CLL1, NewTheory, Visited2, Backscore1))
  ;
    Od1 is Od + 1,
    minmaxvalue(Childs,M,MinV,MaxV),
    %mean_value_level(Childs,M,Mvl),
    once(uct(Childs, M,VISITED, MinV, MaxV, BestChild)),
    %once(uct(Childs, VISITED, BestChild)),
    tree_policy(BestChild,M,NodeID,DB,Od1, Nd),
    assert(M:node(ID, Childs, Parent , CLL1, NewTheory, Visited2, Backscore1))
  ).


default_policy(_Theory, _M,Reward, Reward, BestDefaultTheory,BestDefaultTheory,_DB, Depth, MaxDepth):-
  Depth > MaxDepth,
  !.

default_policy(Theory,M,PrevR,Reward,PrevBestDefaultTheory,BestDefaultTheory,DB,Depth,MaxDepth):-
  %format("\nDefault policy",[]),flush_output,
  format2(M,"\n[Default Policy ~w]",[Depth]),
  theory_revisions_r(Theory,M,Revisions),
  ( Revisions \== [] ->
    length(Revisions,L),
    random(0,L,K),
    nth0(K, Revisions,Spec),
    Depth1 is Depth + 1,
    score_theory(Spec,M,DB,Score,BestTheory,NewTheory),
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

    format2(M," cll-reward ~w",[Reward1]),

    M:mcts_best_score(BestScore),


    ((Score > BestScore, LengthCondition = true) ->
      format2(M,"\n[New best score: ~w ~w]",[Score, BestTheory]),flush_output,


      retract(M:mcts_best_score(_)),
      retract(M:mcts_best_theory(_)),
      assert(M:mcts_best_score(Score)),
      assert(M:mcts_best_theory(NewTheory)),

      retract(M:mcts_best_theories_iteration(BestsIter)),
      M:mcts_iteration(Iteration),
      append(BestsIter,[Iteration],BestsIter1),
      assert(M:mcts_best_theories_iteration(BestsIter1)),


      retract(M:mcts_theories(Mlns)),
      Mlns1 is Mlns + 1,
      assert(M:mcts_theories(Mlns1))
    ;
      true
    ),

    default_policy(Spec,M, Reward1,Reward, BestDefaultTheory1,BestDefaultTheory,DB, Depth1,MaxDepth)

  ;
    Reward = PrevR,
    BestDefaultTheory = PrevBestDefaultTheory
  ).


minmaxvalue(Childs,M,MinV,MaxV):-
  Childs = [F|R],
  M:node(F, _, _ , _, _, Visits, Reward),
  ( Visits=:=0->
    V is sign(Reward)*1e20
  ;
    V is Reward / Visits
  ),
  minmaxvalue(R,M,V,V,MinV,MaxV).

minmaxvalue([],_M,Min,Max,Min,Max).

minmaxvalue([C|R],M,PrevMin,PrevMax,MinV,MaxV):-
  M:node(C, _, _ , _, _, Visits, Reward),
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
  minmaxvalue(R,M,Min1,Max1,MinV,MaxV).


mean_value_level(Cs,Mod,M):-
  mean_value_level1(Cs,Mod,Me),
  length(Me,L),
  sum_list(Me,S),
  ( L=:=0->
    M is sign(S)*1e20
  ;
    M is S / L
  ).


mean_value_level1([],_Mod,[]).

mean_value_level1([C|R],Mod,M1):-
  Mod:node(C, _, _ , 1, _, _Visits, _Reward),
  !,
  mean_value_level1(R,Mod,M1).

mean_value_level1([C|R],Mod,[M|Rm]):-
  Mod:node(C, _, _ , _, _, Visits, Reward),
  !,
  mean_value_level1(R,Mod,Rm),
  ( Visits=:=0->
    M is sign(Reward)*1e20
  ;
    M is (Reward / Visits)
  ).


uct(Childs, M,ParentVisits, Min, Max, BestChild):-
  Childs = [FirstChild|RestChilds],
  M:node(FirstChild, _, _ , _Score, _Theory, Visits, Reward),
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
    uct(RestChilds,M, UCT, ParentVisits, FirstChild, Min,Max, BestChild)
  ).


uct([],_M, _CurrentBestUCT, _ParentVisits, BestChild, _, _,BestChild).

uct([Child|RestChilds], M,CurrentBestUCT, ParentVisits, CurrentBestChild, Min, Max,BestChild) :-
  M:node(Child, _, _ , _Score, _Theory, Visits, Reward),
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
      uct(RestChilds,M, UCT, ParentVisits, Child, Min, Max, BestChild)
    ;
      uct(RestChilds,M, CurrentBestUCT, ParentVisits, CurrentBestChild, Min, Max, BestChild)
    )
  ).


expand(ID, M,Theory, ParentCLL, DB, NodeID, Childs):-
  %format("  expanding...",[]),flush_output,
  theory_revisions(Theory,M,Revisions),
  !,
  assert_childs(Revisions,M,ID,ParentCLL,Childs),
  ( Childs \= [] ->
    Childs = [NodeID|_],
    retract(M:node(NodeID, Childs1, Parent , _, Theory1, Visited, Backscore)),
    format2(M,"\n[Expand ~w]",[NodeID]),
    Visited1 is Visited + 1,
    score_theory(Theory1,M,DB,CLL,BestTheory,NewTheory),
    format2(M," CLL: ~w]",[CLL]),
    %format("\nTree policy: ~w ~w]",[Score, Theory1]),
    M:mcts_best_score(BestScore),

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
        format2(M,"\n[New best score: ~w ~w]",[CLL, BestTheory]),flush_output,
        retract(M:mcts_best_score(_)),
        retract(M:mcts_best_theory(_)),
        assert(M:mcts_best_score(CLL)),
        assert(M:mcts_best_theory(NewTheory)),

        retract(M:mcts_best_theories_iteration(BestsIter)),
        M:mcts_iteration(Iteration),
        append(BestsIter,[Iteration],BestsIter1),
        assert(M:mcts_best_theories_iteration(BestsIter1)),


        retract(M:mcts_theories(Mlns)),
        Mlns1 is Mlns + 1,
        assert(M:mcts_theories(Mlns1))
    ;
      true
  ),
    assert(M:node(NodeID, Childs1, Parent , CLL, NewTheory, Visited1, Backscore))
  ;
    NodeID = -1
  ).


assert_childs([],_M,_,_,[]).

assert_childs([Spec|Rest],M,P,PCLL,[ID1|Childs]):-
  %node(ID, CHILDRENS, PARENT, PSLL, MLN, VISITED, BACKSCORE)
  retract(M:lastid(ID)),
  %format(" ~w",[ID]),flush_output,
  ID1 is ID + 1,
  assert(M:lastid(ID1)),
  %SigmoidValue is ((1 / (1 + exp(-PCLL)))/0.5),
  (PCLL=:=1->
    SigmoidValue=1e20
  ;
    SigmoidValue is 1 / (1 -  PCLL)
  ),
  assert(M:node(ID1, [], P, 1 , Spec, 1 , SigmoidValue)),
  %assert(node(ID1, [], P, 1 , Spec, 0 , 0)),
  assert_childs(Rest,M,P,PCLL,Childs).


theory_length([],X,X).

theory_length([T|R],K,K1):-
  theory_length(R,K,K0),
  T = rule(_,_,B,_,_),
  length(B,L),
  ( L > K0 ->
    K1 = L
  ;
    K1 = K0
  ).

score_theory(Theory0,M,DB,Score,Theory,R3):-
  ( M:mcts_theories(0) ->
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
  learn_params(DB, M, Theory, R3, CLL),
  write3(M,'Updated refinement'),write3(M,'\n'),
  write_rules3(M,R3,user_output),
  Score = CLL,
  !.


backup(1,_M,_Reward,[]):-
  !.

backup(NodeID,M,Reward,[Parent|R]):-
  ( retract(M:node(NodeID, Childs, Parent , PSLL, MLN, Visited, Backscore)) ->
    true
  ;
    format2(M,"\nNo node with ID ~w in backup",[NodeID]),
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
    assert(M:node(NodeID, Childs, Parent , PSLL, MLN, Visited, Backscore1)),
    backup(Parent,M,Reward1,R).


theory_revisions_op(Theory,M,TheoryRevs):-
  setof(RevOp, Theory^revise_theory(Theory,M,RevOp), TheoryRevs),!.

theory_revisions_op(_Theory,_M,[]).


theory_revisions_r(Theory,M,TheoryRevs):-
  theory_revisions_op(Theory,M,TheoryRevs1),
  %filter_add_rule(TheoryRevs11,TheoryRevs1),

  ( TheoryRevs1 == [] ->
    TheoryRevs = []
  ;
    length(TheoryRevs1,L),
    random(0,L,K),
    nth0(K, TheoryRevs1,Revision),
    apply_operators([Revision],Theory,TheoryRevs)
  ).


theory_revisions(Theory,M,TheoryRevs):-
  theory_revisions_op(Theory,M,TheoryRevs1),
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

revise_theory(Theory,M,Ref):-
  specialize_theory(Theory,M,Ref).

revise_theory(Theory,M,Ref):-
  generalize_theory(Theory,M,Ref).


generalize_theory(Theory,M,Ref):-
  length(Theory,LT),
  M:local_setting(max_rules,MR),
  LT<MR,
  add_rule(M,Ref).


add_rule(M,add(SpecRule)):-
  findall(HL , M:modeh(_,HL), HLS),
  length(HLS,L),
  L1 is L+1,
  P is 1/L1,
  generate_head(HLS,P,Head),
  get_next_rule_number(M,ID),
  Rule0 = rule(ID,Head,[],true,_),
  specialize_rule(Rule0,M,SpecRule,_Lit).

generate_head([H|_T],_P,[H1:0.5,'':0.5]):-
  H=..[Pred|Args],
  length(Args,LA),
  length(Args1,LA),
  H1=..[Pred|Args1].

generate_head([_H|T],P,Head):-
  generate_head(T,P,Head).


specialize_theory(Theory,M,Ref):-
  Theory \== [],
  choose_rule(Theory,M,Rule),
  specialize_rule(Rule,M,SpecRule,Lit),
  Ref = add_body(Rule,SpecRule,Lit),
  SpecRule = rule(_,_,_B,_,_).


specialize_rule(Rule,M,SpecRule,Lit):-
  M:mcts_modeb(BSL),
  specialize_rule_bl(BSL,M,Rule,SpecRule,Lit).





specialize_rule_bl([Lit|_RLit],M,Rule,SpecRul,SLit):-
  Rule = rule(ID,LH,BL,true,Tun),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule_lit(Lit,M,ALL,SLit),
  append(BL,[SLit],BL1),
  (M:lookahead(SLit,LLit1);M:lookahead_cons(SLit,LLit1)),
  specialize_rule_la(LLit1,M,LH1,BL1,BL2),
  append(LH1,BL2,ALL2),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  \+ banned_clause(M,LH1,BL2),
  SpecRul = rule(ID,LH,BL2,true,Tun).

specialize_rule_bl([Lit|_RLit],M,Rule,SpecRul,SLit):-
  Rule = rule(ID,LH,BL,true,Tun),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule_lit(Lit,M,ALL,SLit),

  \+ M:lookahead_cons(SLit,_),

  append(BL,[SLit],BL1),
  append(LH1,BL1,ALL1),
  extract_fancy_vars(ALL1,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  M:local_setting(maxdepth_var,_MD),
  \+ banned_clause(M,LH1,BL1),
  SpecRul = rule(ID,LH,BL1,true,Tun).


specialize_rule_bl([_|RLit],M,Rule,SpecRul,Lit):-
  specialize_rule_bl(RLit,M,Rule,SpecRul,Lit).


specialize_rule_la([],_M,_LH1,BL1,BL1).

specialize_rule_la([Lit1|T],M,LH1,BL1,BL3):-
  copy_term(Lit1,Lit2),
  M:modeb(_,Lit2),
  append(LH1,BL1,ALL1),
  specialize_rule_lit(Lit2,M,ALL1,SLit1),
  append(BL1,[SLit1],BL2),
  specialize_rule_la(T,M,LH1,BL2,BL3).



remove_prob(['':_P],[]):-!.

remove_prob([X:_|R],[X|R1]):-
  remove_prob(R,R1).


specialize_rule_lit(Lit,M,Lits,SpecLit):-
  Lit =.. [Pred|Args],
  extract_type_vars(Lits,M,TypeVars0),
  remove_duplicates(TypeVars0,TypeVars),
  take_var_args(Args,TypeVars,Args1),
  SpecLit =.. [Pred|Args1],
  \+ member_eq(SpecLit,Lits).

choose_rule(Theory,M,Rule):-
  ( M:local_setting(mcts_covering,true)	->
    M:mcts_restart(Restart),
    nth1(K,Theory,Rule),
    K >= Restart
  ;
    member(Rule,Theory)
  ).



delete_matching([],_El,[]).

delete_matching([El|T],El,T1):-!,
  delete_matching(T,El,T1).

delete_matching([H|T],El,[H|T1]):-
  delete_matching(T,El,T1).


/**
 * set_lm(:Parameter:atom,+Value:term) is det
 *
 * The predicate sets the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
set_lm(M:Parameter,Value):-
  retract(M:local_setting(Parameter,_)),
  assert(M:local_setting(Parameter,Value)).

/**
 * setting_lm(:Parameter:atom,-Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_lm(M:P,V):-
  M:local_setting(P,V).




user:term_expansion((:- lemur), []) :-!,
  prolog_load_context(module, M),
  retractall(M:local_setting(_,_)),
  findall(local_setting(P,V),default_setting_lm(P,V),L),
  assert_all(L,M,_),
  assert(lm_input_mod(M)),
  retractall(M:rule_sc_n(_)),
  assert(M:rule_sc_n(0)),
  retractall(M:rule_ng_sc_n(_)),
  assert(M:rule_ng_sc_n(0)),
    M:dynamic((modeh/2,modeh/4,fixed_rule/3,banned/2,lookahead/2,
    lookahead_cons/2,lookahead_cons_var/2,'$prob'/2,output/1,input/1,input_cw/1,
    ref_clause/1,ref/1,model/1,neg/1,rule/5,determination/2,
    bg_on/0,bg/1,bgc/1,in_on/0,in/1,inc/1,int/1,
    query_rule/4,
    zero_clauses/1,tabled/1)),
  retractall(M:tabled(_)),
  style_check(-discontiguous).

user:term_expansion(end_of_file, end_of_file) :-
  prolog_load_context(module, M),
  lm_input_mod(M),!,
  make_dynamic(M),
  retractall(lm_input_mod(M)),
  style_check(+discontiguous).

user:term_expansion((:- begin_bg), []) :-
  prolog_load_context(module, M),
  lm_input_mod(M),!,
  assert(M:bg_on).

user:term_expansion(C, M:bgc(C)) :-
  prolog_load_context(module, M),
  C\= (:- end_bg),
  lm_input_mod(M),
  M:bg_on,!.

user:term_expansion((:- end_bg), []) :-
  prolog_load_context(module, M),
  lm_input_mod(M),!,
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
  prolog_load_context(module, M),
  lm_input_mod(M),!,
  assert(M:in_on).

user:term_expansion(C, M:inc(C)) :-
  prolog_load_context(module, M),
  C\= (:- end_in),
  lm_input_mod(M),
  M:in_on,!.

user:term_expansion((:- end_in), []) :-
  prolog_load_context(module, M),
  lm_input_mod(M),!,
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

user:term_expansion(output(P/A), [(:- table P1),output(P/A)]) :-
  prolog_load_context(module, M),
  lm_input_mod(M),
  M:local_setting(tabling,auto),!,
  tab(M,P/A,P1),
  zero_clause(M,P/A,Z),
  assert(M:zero_clauses([Z])).

user:term_expansion(input(P/A), [(:- table P1),input(P/A)]) :-
  prolog_load_context(module, M),
  lm_input_mod(M),
  M:local_setting(tabling,auto),!,
  tab(M,P/A,P1),
  zero_clause(M,P/A,Z),
  assert(M:zero_clauses([Z])).

user:term_expansion(begin(model(I)), []) :-
  prolog_load_context(module, M),
  lm_input_mod(M),!,
  retractall(M:model(_)),
  assert(M:model(I)),
  assert(M:int(I)).

user:term_expansion(end(model(_I)), []) :-
  prolog_load_context(module, M),
  lm_input_mod(M),!,
  retractall(M:model(_)).

user:term_expansion(At, A) :-
  prolog_load_context(module, M),
  lm_input_mod(M),
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


:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(lemur:induce_lm(_,_),[]).
sandbox:safe_meta(lemur:set_lm(_,_), []).
sandbox:safe_meta(lemur:setting_lm(_,_), []).
