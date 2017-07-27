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

test((map_bdd_dot_string(win,_BDD,_Var,_VarA,P,Exp),close_to(P,0.162),
  Exp=['', green, blue, yellow]),map1).

test((map_bdd_dot_string(ev,_BDD,_Var,_VarA,P,Exp),close_to(P,0.27),
	  Exp=[pf(2, 1), pf(1, 2), pf(1, 1)]),map_es3).
