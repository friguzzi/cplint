/** <module> PITA

This module performs reassoning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It allows to parse probabilistic program and
compute the probability of queris.

See http://ds.ing.unife.it/~friguzzi/software/cplint/manual.html for
the syntax of programs.

@author Fabrizio Riguzzi
@license Artistic License 2.0
*/


/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzi and Elena Bellodi

*/
:- module(pita,[p/1,parse/1,s/2]).

:-use_module(library(lists)).

:-use_module(library(slipcover/slipcover)).

:-dynamic previous_rules/2.




previous_rules([],[]).
/** 
 * p(+File:atom) is det
 * An alias for parse/1
 */
/* p(File) parses the file File.cpl. It can be called more than once without 
exiting yap */
p(File):-
	parse(File).

/** 
 * parse(+File:atom) is det
 * The predicate paeses the file with name File.cpl and loads the program
 * in memory.
 */
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
  
/** 
 * s(+Query:atom,-Probability:float) is det
 * The predicate computes the probability of the ground query Query
 */

s(Goal,P):-
  rule_n(NR),
  init_test(NR),
  Goal=..[F|Args],
  Goal1=..[F,1|Args],
  get_node(Goal1,BDD),
  ret_prob(BDD,P),
  end_test.


