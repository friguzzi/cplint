/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzi, Nicola di Mauro and Elena Bellodi

*/
:- use_module(library(terms)).
:- use_module(library(lists)).

:- set_prolog_flag(discontiguous_warnings,on).
:- set_prolog_flag(single_var_warnings,on).


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

