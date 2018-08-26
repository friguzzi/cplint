

:-module(cplint_test,
  [close_to/2,close_to/3,relatively_close_to/2,relatively_close_to/3,run/1,
  perm/2]).

:- meta_predicate run(:).

perm(A,B):-
	permutation(A,B),!.

run(M:H):-
	copy_term(H,NH),
	numbervars(NH),
%	NH=(_Query,close_to('P',_Prob)),
	format("~p.~n",[NH]),
	(H=(G,R)),
	time(call(M:G)),!,
	format("\t~p.~n~n",[G]),
	call(R).

epsilon(0.09).

close_to(V,T):-
	epsilon(E),
	TLow is T-E,
	THigh is T+E,
	TLow=<V,
	V=<THigh.

close_to(V,T,E):-
	TLow is T-E,
	THigh is T+E,
	TLow=<V,
	V=<THigh.

relative_epsilon(0.1).

relatively_close_to(V,T):-
	relative_epsilon(E),
	TLow is T*(1-E),
	THigh is T*(1+E),
	TLow=<V,
	V=<THigh.

relatively_close_to(V,T,E):-
	TLow is T*(1-E),
	THigh is T*(1+E),
	TLow=<V,
	V=<THigh.
