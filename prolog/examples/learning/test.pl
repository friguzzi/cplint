

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
	%format("Result: ~a ~q.~n~n",[F,NH]),
	nl,
	test_all(T,F).



epsilon(0.001).

close_to(V,T):-
	epsilon(E),
	A is abs(T),
	VA is abs(V),
	TLow is A*(1-E),
	THigh is A*(1+E),
	TLow<VA,
	VA<THigh.
