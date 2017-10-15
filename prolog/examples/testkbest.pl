
setting(check,true).

main:-
	format("~nTesting viterbi~n",[]),
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
test(alarm,alarm(t),0.144).

test(light,light,0.5).
test(light,replace,0.5).

test(hmm,hmm([a]),0.083333333).
test(hmm,hmm([a,a]),0.006944444).
test(hmm,hmm([a,a,a]),0.000578704).

test(mendel,cg(s,1,p),0.125).
test(mendel,cg(s,1,w),0.125).
test(mendel,cg(s,2,p),0.125).
test(mendel,cg(s,2,w),0.125).
test(mendel,cg(f,2,w),0.5).
test(mendel,cg(s,2,w),0.125).

test(coin,heads(coin),0.45).

test(coin2,heads(coin1),0.45).
test(coin2,heads(coin2),0.45).
test(coin2,tails(coin1),0.45).
test(coin2,tails(coin2),0.45).
*/
test((kbest(win,1,Exp),
Exp=[0.36000000000000004-[rule(0, red, [red:0.4, '':0.6], []),
rule(1, green, [green:0.9, '':0.09999999999999998], [])]]),kbest_win).

test((kbest(win,1,P,Exp),close_to(P,0.36),
Exp=[0.36000000000000004-[rule(0, red, [red:0.4, '':0.6], []),
rule(1, green, [green:0.9, '':0.09999999999999998], [])]]),kbest_win).

test((kbest(win,2,Exp),
Exp=[0.36000000000000004-[rule(0, red, [red:0.4, '':0.6], []),
   rule(1, green, [green:0.9, '':0.09999999999999998], [])],
 0.30000000000000004-[rule(2, blue, [blue:0.5, '':0.5], []),
 rule(3, yellow, [yellow:0.6, '':0.4], [])]]),kbest_win).

test((kbest(win,2,P,Exp),close_to(P,0.552),
Exp=[0.36000000000000004-[rule(0, red, [red:0.4, '':0.6], []),
   rule(1, green, [green:0.9, '':0.09999999999999998], [])],
 0.30000000000000004-[rule(2, blue, [blue:0.5, '':0.5], []),
 rule(3, yellow, [yellow:0.6, '':0.4], [])]]),kbest_win).


test((kbest(hmm([a,g,g]),1,P,_Exp),close_to(P,0.000405)
  ),kbest_hmm).

test((kbest(hmm([a,a,a]),1,P,_Exp),close_to(P,0.0008000000000000003)
  ),kbest_hmm).

test((kbest(heads(coin),1,Prob,Exp),close_to(Prob,0.45),
 Exp = [0.45000000000000007-[rule(0, heads(coin), [heads(coin):0.5, tails(coin):0.5], [toss(coin), \+biased(coin)]),
 	rule(2, fair(coin), [fair(coin):0.9, biased(coin):0.1], [])]]),
 	kbest_coin).

test((kbest(color(s,purple),1,Prob,Exp),close_to(Prob,0.5),
	Exp = [0.5-[rule(0, cg(s, 1, p), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
	 [mother(m, s), cg(m, 1, p), cg(m, 2, w)])]]),
 	kbest_mendel).


test((kbest(color(s,white),1,Prob,Exp),close_to(Prob,0.25),
  Exp = [0.25-[rule(0, cg(s, 1, w), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
     [mother(m, s), cg(m, 1, p), cg(m, 2, w)]),
	 rule(1, cg(s, 2, w), [cg(s, 2, w):0.5, cg(s, 2, p):0.5],
	   [father(f, s), cg(f, 1, w), cg(f, 2, p)])]]),
	kbest_mendel).

test((kbest(color(s,purple),2,Prob,Exp),close_to(Prob,0.75),
	Exp = [0.5-[rule(1, cg(s, 2, p), [cg(s, 2, w):0.5, cg(s, 2, p):0.5],
	[father(f, s), cg(f, 1, w), cg(f, 2, p)])],
	0.5-[rule(0, cg(s, 1, p), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
	[mother(m, s), cg(m, 1, p), cg(m, 2, w)])]]),
 	kbest_mendel).

test((kbest(color(s,white),2,Prob,Exp),close_to(Prob,0.25),
  Exp = [0.25-[rule(0, cg(s, 1, w), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
     [mother(m, s), cg(m, 1, p), cg(m, 2, w)]),
	 rule(1, cg(s, 2, w), [cg(s, 2, w):0.5, cg(s, 2, p):0.5],
	   [father(f, s), cg(f, 1, w), cg(f, 2, p)])]]),
	kbest_mendel).
