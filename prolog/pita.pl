/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzi and Elena Bellodi

*/
:-use_module(library(lists)).

:-[slipcover].

:-dynamic previous_rules/2.




previous_rules([],[]).
/* p(File) parses the file File.cpl. It can be called more than once without 
exiting yap */
p(File):-
	parse(File).

parse(File):-
  retract(rule_n(_)),
  assert(rule_n(0)),
  previous_rules(A,B),
  retract_all(A),
  retract_all(B),
  atom_concat(File,'.cpl',FilePl),
  set(compiling,on),
  load(FilePl,R,Th),
  set(compiling,off),
  assert_all(Th,ThRef),  
  assert_all(R,RRef),
  retract(previous_rules(_,_)),
  assert(previous_rules(RRef,ThRef)).
  

s(Goal,P):-
  rule_n(NR),
  init_test(NR),
  Goal=..[F|Args],
  Goal1=..[F,1|Args],
  get_node(Goal1,BDD),
  ret_prob(BDD,P),
  end_test.


