:- use_module(library(pita)).

setting(check,true).

main:-
	format("~nTesting pita~n",[]),
	setof(File,A^test(A,File),F),
	statistics(runtime,[_,_]),
	test_files(F),
	statistics(runtime,[_,T]),
	T1 is T /1000,
	format("Test successful, time ~f secs.~n",[T1]).

t:-
	format("Test unsuccessful.",[]).

test_files([]).

test_files([H|T]):-
	format("~n~a.pl~n",[H]),
	load_files([H]),
%	library_directory(LD),
%	atom_concat(LD,'/cplint/examples/',ExDir),
%	atom_concat(ExDir,H,NH),
%	p(NH),!,
	findall(A,test(A,H),L),
	test_all(L,H),
	unload_file(H),
	test_files(T).

test_all([],_F).

test_all([H|T],F):-
	copy_term(H,NH),
	numbervars(NH),
	format("~a ~p.~n",[F,NH]),
	(H=(G,R)),
	call(G),!,
	format("\t~p.~n",[G]),
	(setting(check,true)->
	  call(R),!
        ;
          true),
	test_all(T,F).


epsilon(0.001).

close_to(V,T):-
	epsilon(E),
	TLow is T-E,
	THigh is T+E,
	TLow<V,
	V<THigh.

test((abd_prob(a,P,Exp),close_to(P,0.72),Exp=[c, e]),abd1).
test((abd_bdd_dot_string(a,_BDD,_Var,_VarA,P,Exp),close_to(P,0.72),Exp=[c, e]),abd1).

test((abd_prob(a,P,Exp),close_to(P,0.72),Exp=[c, d, f, (\+g)]),abd2).
test((abd_bdd_dot_string(a,_BDD,_Var,_VarA,P,Exp),close_to(P,0.72),
  Exp=[c, d, f, (\+g)]),abd2).

test((abd_prob(a,P,Exp),close_to(P,0.42),Exp=[(\+ c), f, (\+g)]),abd3).
test((abd_bdd_dot_string(a,_BDD,_Var,_VarA,P,Exp),close_to(P,0.42),
  Exp=[(\+ c),f, (\+g)]),abd3).

test((map_bdd_dot_string(win,_BDD,_Var,_VarA,P,Exp),close_to(P,0.162),
  Exp=[
	  rule(0, '', [red:0.4, '':0.6], true),
	  rule(1, green, [green:0.9, '':0.09999999999999998], true),
		rule(2, blue, [blue:0.5, '':0.5], true),
		rule(3, yellow, [yellow:0.6, '':0.4], true)
	 ]),map1).

test((map_bdd_dot_string(ev,_BDD,_Var,_VarA,P,Exp),close_to(P,0.27),
	  Exp=[
		  rule(2,pf(2,1),[pf(2,1):0.6,'':0.4],true),
			rule(1,pf(1,2),[pf(1,2):0.75,'':0.25],true),
			rule(0,pf(1,1),[pf(1,1):0.6,'':0.4],true)
		  ]),map_es3).

test((map_bdd_dot_string(ev,_BDD,_Var,_VarA,P,Exp),close_to(P,0.6),
	  Exp=[
		  rule(0, red(b1), [red(b1):0.6, green(b1):0.3, blue(b1):0.1], pick(b1))
		  ]),map_es21).

test((map_bdd_dot_string(ev,_BDD,_Var,_VarA,P,Exp),close_to(P,0.36),
	  Exp=[
		  rule(1, pick(b1), [pick(b1):0.6, no_pick(b1):0.4], true),
			rule(0, red(b1), [red(b1):0.6, green(b1):0.3, blue(b1):0.1], pick(b1))
		  ]),map_es2).

test((map_bdd_dot_string(ev,_BDD,_Var,_VarA,P,Exp),close_to(P,0.54),
	  Exp=[
		  rule(1, pick(b1), [pick(b1):0.6, no_pick(b1):0.4], true)
		  ]),map_es2map).

test((map_bdd_dot_string(ev,_BDD,_Var,_VarA,P,Exp),close_to(P,0.6),
	  Exp=[
		  rule(0, red(b1), [red(b1):0.6, green(b1):0.3, blue(b1):0.1], pick(b1))
		  ]),map_es2map1).
