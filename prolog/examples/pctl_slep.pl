/*
Model checking of the Synchronous Leader Election Protocol expressed in 
Probabilistic Computation Tree Logic (PCTL).
From
Gorlin, Andrey, C. R. Ramakrishnan, and Scott A. Smolka. "Model checking with probabilistic tabled logic programming." Theory and Practice of Logic Programming 12.4-5 (2012): 681-700.

See also http://www.prismmodelchecker.org/casestudies/synchronous_leader.php
*/

:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- mc.

:- begin_lpad.

% State Formulae 
models(_S, tt).
models(S, prop(P)) :-
	proposition(P, S).
models(S, and(F1, F2)) :-
	models(S, F1), models(S, F2).
models(S, or(F1, _F2)) :-
	models(S, F1).
models(S, or(F1, F2)) :-
	\+ models(S, F1),
	models(S, F2).
models(S, not(F)) :-
	\+ models(S, F).
models(S, prob_until(comp(Op, P), F1, F2)) :-
	mc_sample(pmodels(S, until(F1, F2)),20, Q),
	comp(Q, Op, P).
models(S, prob_next(comp(Op, P), F)) :-
	mc_sample(pmodels(S, next(F)),20, Q),
	comp(Q, Op, P).

comp(Q,>,P):-
  Q>P.

comp(Q,>=,P):-
  Q>=P.

comp(Q,<,P):-
  Q<P.

comp(Q,=<,P):-
  Q=<P.


% Path Formulae
pmodels(S, PF) :-
  pmodels(S, PF,[]).

pmodels(S, until(_F1, F2),_Hist) :-
	models(S, F2).
	
pmodels(S, until(F1, F2),Hist0) :-
	models(S, F1),
	ctrans(S, _, T, Hist0, Hist),
	pmodels(T, until(F1,F2), Hist).

pmodels(S, next(F), Hist) :-
	ctrans(S, _, T, Hist, _),
	models(T, F).

top_models(PCTLF) :-
	init(S),
	pctlspec(PCTLF, F),
	pmodels(S, F).

top_pmodels(PCTLPF, P) :-
	init(S),
	pctlspec(PCTLPF, PF),
	prob(pmodels(S, PF, _),P).

pctlspec(X, until(tt, prop(X))).
proposition(P, S) :- final(P, S).

final(elect, [_|L]) :-
	num(N),
	gen_elected_state(N, L).

gen_elected_state(J, L) :-
	(J==0
	->    L=[]
	;     L = [state(3,_,_,_)|Rest],
	      J1 is J-1,
	      gen_elected_state(J1,Rest)
	).
	

% transitions
% module counter
% [read] c<N-1 -> (c'=c+1);
% reading
trans(counter, counter(C), read, counter(D),_S,H,H) :-
  num(N),
  C < N-1,
  D is C+1.

% [read] c=N-1 -> (c'=c);
% finished reading
trans(counter, counter(C), read, counter(C),_S,H,H) :-
  num(N),
  C =:= N-1.

% [done] u1 | u2 | u3 | u4 -> (c'=c);
% done
trans(counter, counter(C), done, counter(C),S,H,H) :-
  get_processid(P), 
  nonlocal(process(P,_), uniqueid, 1,S).
   

% [retry] !(u1 | u2 | u3 | u4) -> (c'=1);
% pick again reset counter 
trans(counter, counter(_C), retry, counter(1),S,H,H) :-
        findall(P,get_processid(P),PL),
	maplist(nl(S),PL).

% [loop] s1=3 -> (c'=c);
% loop (when finished to avoid deadlocks)
trans(counter, counter(C), loop, counter(C),S,H,H) :-
  nonlocal(process(1,_), state, 3,S).

% module process
% local state
% s1=0 make random choice
% s1=1 reading
% s1=2 deciding
% s1=3 finished

% [pick] s1=0 -> 1/K : (s1'=1) & (p1'=0) & (v1'=0) & (u1'=true) + ...;
% pick value
trans(process(_N,_Next), state(0,_,_,_), pick, state(1,1,R,R),_S,H,[pick(R)|H]) :-
  pick(H,R).

%read 
% [read] s1=1 &  u1 & !p1=v2 & c<N-1 -> (u1'=true) & (v1'=v2);
trans(process(_N,Next), state(1,1,_,P), read, state(1,1,V,P),S,H,H) :-
  nonlocal(counter, counter, C,S),
  num(CN),
  C < CN - 1,
  nonlocal(process(Next,_), value, V,S),
  P \== V.

% [read] s1=1 &  u1 &  p1=v2 & c<N-1 -> (u1'=false) & (v1'=v2) & (p1'=0);
trans(process(_N,Next), state(1,1,_,P), read, state(1,0,V,0),S,H,H) :-
  nonlocal(counter, counter, C,S),
  num(CN),
  C < CN - 1,
  nonlocal(process(Next,_), value, V,S),
  P == V.

% [read] s1=1 & !u1 &  c<N-1 -> (u1'=false) & (v1'=v2);
trans(process(_N,Next), state(1,0,_,P), read, state(1,0,V,P),S,H,H) :-
  nonlocal(counter, counter, C,S),
  num(CN),
  C < CN - 1,
  nonlocal(process(Next,_), value, V,S).
 
% read and move to decide 
% [read] s1=1 &  u1 & !p1=v2 & c=N-1 -> (s1'=2) & (u1'=true) & (v1'=0) & (p1'=0);
trans(process(_N,Next), state(1,1,_,P), read, state(2,1,0,0),S,H,H) :-
  nonlocal(counter, counter, C,S),
  num(CN),
  C =:= CN - 1,
  nonlocal(process(Next,_), value, V,S),
  P \== V.

% [read] s1=1 &  u1 &  p1=v2 & c=N-1 -> (u1'=false) & (v1'=0) & (p1'=0);
trans(process(_N,Next), state(1,1,_,P), read, state(2,0,0,0),S,H,H) :-
  nonlocal(counter, counter, C,S),
  num(CN),
  C =:= CN - 1,
  nonlocal(process(Next,_), value, V,S),
  P == V.

% [read] s1=1 & !u1 &  c=N-1 -> (s1'=2) & (u1'=false) & (v1'=0);
trans(process(_N,_Next), state(1,0,_,P), read, state(2,0,0,P),S,H,H) :-
  nonlocal(counter, counter, C,S),
  num(CN),
  C =:= CN - 1.

% done
% [done] s1=2 -> (s1'=3) & (u1'=false) & (v1'=0) & (p1'=0);
trans(process(_N,_Next), state(2,_,_,_), done, state(3,0,0,0),_S,H,H).

% retry
% [retry] s1=2 -> (s1'=0) & (u1'=false) & (v1'=0) & (p1'=0);
trans(process(_N,_Next), state(2,_,_,_), retry, state(0,0,0,0),_S,H,H).

% loop (when finished to avoid deadlocks)
% [loop] s1=3 -> (s1'=3);
trans(process(_N,_Next), state(3,U,V,P), loop, state(3,U,V,P),_S,H,H).

pick(H,V):-
  kr(K),
  K1 is K-1,
  PH is 1/K,
  findall(I,between(0,K1,I),L),
  foldl(pick_value(H,PH),L,(1,_),(_,V)).

pick_value(_H,_PH,_I,(P0,V0),(P0,V0)):-
  nonvar(V0).

pick_value(H,PH,I,(P0,V0),(P1,V1)):-
  var(V0),
  PF is PH/P0,
  (pick_fact(H,V0,PF)->
    P1=PF,
    V1=I
  ;
    P1 is P0*(1-PF),
    V1=V0
  ).

pick_fact(_,_,P):P.

%pick(H,0):0.5; pick(H,1):0.5.

num(4).
kr(4).

ctrans(S, A, T, Hi, Ho) :-
	config(P),
	find_matching_trans(P,S,S,[],A,T,Hi,Ho).

find_matching_trans([], [], _CS, _PA, A, [], H,H) :- nonvar(A).
find_matching_trans([P|Ps], [S|Ss], CS, PA, A, [T|Ts],Hi,Ho) :-
	pick_trans(P, S, CS, PA, A, T, Hi,H1),
	find_matching_trans(Ps, Ss, CS, PA, A, Ts,H1,Ho).
find_matching_trans([P|Ps], [S|Ss], CS, PA, A, [S|Ts], Hi,Ho) :-
	% skip current process; but then all transitions enabled in the current
	% process will be prohibited for selection in later processes.
	enabled_trans(P,L),
	(nonvar(A) -> \+ member(A,L); true),
	append(L, PA, PA1),
	find_matching_trans(Ps, Ss, CS, PA1, A, Ts, Hi, Ho).

pick_trans(P, S, CS, PA, A, T, H0,H) :-
	etrans(P, S, PA, A, T,CS, H0,H).

etrans(P, S, PA, A, T, CS,H0,H) :-
	trans(P, S, A, T,CS,H0,H),
	A \= epsilon,
	\+ member(A, PA).

enabled_trans(P, L) :-
	setof(A, enabled_trans_in_process(P,A), L).

enabled_trans_in_process(P,A) :-
	clause(trans(P,_,A,_,_,_,_),_),
	A \= epsilon.

nonlocal(Proc, Var, Val,CS) :-
	getpid(Proc, Var, Pid, Idx),
	nth1(Pid, CS, State),
	arg(Idx, State, Val).
%	writeln(nonlocal_read(Proc, State, Var, Val)).

getpid(Proc, Var, Pid, Idx) :-
	config(Config),
	nth1(Pid, Config, Proc),
	nonlocal_access(Proc, Var, Idx).

get_processid(P):-
  num(N),
  between(1,N,P).

config([counter| L]) :-
	findall(P,get_processid(P),PL),
	maplist(neighbor,PL,L).

neighbor(I,process(I,J)) :-
	num(N),
	(I<N
	->  J is I+1
	;   J = 1
	).

%config([counter, process(1,2), process(2,3), process(3,4), process(4,1)]).

init(S) :-
	config(P),
	maplist(init_state,P,S).

init_state(counter, counter(1)).
init_state(process(_,_), state(0,0,0,0)).

nonlocal_access(counter, counter, 1).
nonlocal_access(process(_,_), state, 1).
nonlocal_access(process(_,_), uniqueid, 2).
nonlocal_access(process(_,_), value, 3).

nl(S,P):-
  nonlocal(process(P, _), uniqueid, 0,S).

:- end_lpad.



/** <examples>
% see http://www.prismmodelchecker.org/casestudies/synchronous_leader.php
% What is the probability that eventually a leader is elected?
?-  mc_sample( top_`models(elect),1,P).
% expected result 1

% What is the probability of electing a leader within L rounds?

% What is the expected number of rounds to elect a leader?
*/

