

main:-
	format("~nTesting slipcover~n",[]),
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
	format("~a ~q.~n",[F,NH]),
	call(H),!,
	test_all(T,F).


test((induce_par([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),close_to(LL,-4.468541553948988),close_to(AUCROC,0.92),close_to(AUCPR,0.33333333333333326)),mach).
test((in(P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),close_to(LL, -21.286207461851408),close_to(AUCROC,0.7733333333333333),close_to(AUCPR,0.3333333333333332)),mach).
%test((induce([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),close_to(LL,-41.44653167389282),close_to(AUCROC,0.7),close_to(AUCPR,0.3333333333333332)),mach).

epsilon(0.001).

close_to(V,T):-
	epsilon(E),
	A is abs(T),
	VA is abs(V),
	TLow is A*(1-E),
	THigh is A*(1+E),
	TLow<VA,
	VA<THigh.

