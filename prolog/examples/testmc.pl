

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
	NH=(_Query,close_to('P',_Prob)),
	format("~a ~p.~n",[F,NH]),
	call(H),!,
	test_all(T,F).


test((mc_sample(heads(coin),1000,P),close_to(P,0.51)),coinmc).
test((mc_sample(tails(coin),1000,P),close_to(P,0.49)),coinmc).

test((mc_prob(heads(coin),P),close_to(P,0.51)),coinmc).
test((mc_prob(tails(coin),P),close_to(P,0.49)),coinmc).



test((mc_sample(on(0,1),1000,P),close_to(P,0.333333333333333)),threesideddicemc).
test((mc_sample(on(1,1),1000,P),close_to(P,0.222222222222222)),threesideddicemc).
test((mc_sample(on(2,1),1000,P),close_to(P,0.148148147703704)),threesideddicemc).

epsilon(0.05).

close_to(V,T):-
	epsilon(E),
	TLow is T-E,
	THigh is T+E,
	TLow<V,
	V<THigh.

