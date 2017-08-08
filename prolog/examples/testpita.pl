
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
/*
test((prob(heads(coin),P),close_to(P,0.51)),coin).
test((prob((heads(coin),biased(coin)),P),close_to(P,0.06)),coin).
test((prob(tails(coin),P),close_to(P,0.49)),coin).
test((prob(heads(coin),biased(coin),P),close_to(P,0.6)),coin).
test((prob((heads(coin), biased(coin)),P),close_to(P,0.06)),coin).
test((prob((heads(coin),\+ biased(coin)),P),close_to(P,0.45)),coin).
test((prob(\+ heads(coin),P),close_to(P,0.49)),coin).
*/
test((prob(res(coin,heads),P),close_to(P,0.51)),coinmsw).
test((prob(res(coin,tails),P),close_to(P,0.49)),coinmsw).

/*test((prob(on(0,1),P),close_to(P,0.16666666666666666)),dice).
test((prob(on(1,1),P),close_to(P,0.13888888888888887)),dice).
test((prob(on(2,1),P),close_to(P,0.11574074074074071)),dice).
test((prob(on(2,1),on(0,1),P),close_to(P,0.13888888888888887)),dice).
test((prob(on(2,1),evidence,P),close_to(P,0.16666666666666666)),dice).
*/

test((prob(epidemic,P),close_to(P,0.588)),epidemic).
test((prob(pandemic,P),close_to(P,0.357)),epidemic).

test((prob(earthquake(stromboli,strong),P),close_to(P,0.43999999999999995)),earthquake).
test((prob(earthquake(stromboli,moderate),P),close_to(P,0.7999999999999998)),earthquake).
test((prob(earthquake(eyjafjallajkull,strong),P),close_to(P,0.2)),earthquake).
test((prob(earthquake(eyjafjallajkull,moderate),P),close_to(P,0.6)),earthquake).


test((prob(strong_sneezing(bob),P),close_to(P,0.43999999999999995)),sneezing).
test((prob(moderate_sneezing(bob),P),close_to(P,0.7999999999999998)),sneezing).

test((prob(death,P),close_to(P,0.305555555555556)),trigger).

/*test((prob(light,P),close_to(P,0.4)),light).
test((prob(replace,P),close_to(P,0.6)),light).
*/

/*
test((prob(on(0,1),P),close_to(P,0.333333333333333)),threesideddice).
test((prob(on(1,1),P),close_to(P,0.222222222222222)),threesideddice).
test((prob(on(2,1),P),close_to(P,0.148148147703704)),threesideddice).

test((prob(on(2,1),on(0,1),P),close_to(P,0.222222222222222)),threesideddice).
test((prob(on(2,1),on(1,1),P),close_to(P,0.333333333333333)),threesideddice).
*/

test((prob(cg(s,1,p),P),close_to(P,0.5)),mendel).
test((prob(cg(s,1,w),P),close_to(P,0.5)),mendel).
test((prob(cg(s,2,p),P),close_to(P,0.5)),mendel).
test((prob(cg(s,2,w),P),close_to(P,0.5)),mendel).


test((prob(heads(coin1),P),close_to(P,0.51)),coin2).
test((prob(heads(coin2),P),close_to(P,0.51)),coin2).

test((prob(tails(coin1),P),close_to(P,0.49)),coin2).
test((prob(tails(coin2),P),close_to(P,0.49)),coin2).
/*
test((prob(recovery,drug,P),close_to(P,0.5)),simpson).
test((prob(recovery,\+ drug,P),close_to(P,0.4)),simpson).
test((prob(recovery,(drug,female),P),close_to(P,0.2)),simpson).
test((prob(recovery,(\+drug,female),P),close_to(P,0.3)),simpson).
test((prob(recovery,(drug,\+female),P),close_to(P,0.6)),simpson).
test((prob(recovery,(\+ drug,\+female),P),close_to(P,0.7)),simpson).
test((prob(recovery,do(drug),P),close_to(P,0.4)),simpson).
test((prob(recovery,do(\+ drug),P),close_to(P,0.5)),simpson).
test((prob(recovery,(do(drug),female),P),close_to(P,0.2)),simpson).
test((prob(recovery,(do(\+drug),female),P),close_to(P,0.3)),simpson).
test((prob(recovery,(do(drug),\+ female),P),close_to(P,0.6)),simpson).
test((prob(recovery,(do(\+ drug),\+ female),P),close_to(P,0.7)),simpson).
*/
/*
test((prob(has(2),has(3),P),close_to(P,0.4065135474609725)),viral).
test((prob(has(2),do(has(3)),P),close_to(P,0.136)),viral).
*/
