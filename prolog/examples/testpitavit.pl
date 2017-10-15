
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
test((vit_prob(win,P,Exp),close_to(P,0.36),
Exp=[
  rule(0, red, [red:0.4, '':0.6], []),
  rule(1, green, [green:0.9, '':0.09999999999999998], [])]),pitavit_win).

test((vit_prob(hmm([a,g,g]),P,Exp),close_to(P,0.000405),
  Exp= [rule(0,next_state(q1,q2,[]),[next_state(q1,q1,[]):0.5,
	    next_state(q1,q2,[]):0.45,next_state(q1,end,[]):0.05],[]),
		rule(2,letter(q1,a,[]),[letter(q1,a,[]):0.4,letter(q1,c,[]):0.3,
		  letter(q1,g,[]):0.2,letter(q1,t,[]):0.1],[]),
		rule(1,next_state(q2,q2,[q1]),[next_state(q2,q1,[q1]):0.45,
		  next_state(q2,q2,[q1]):0.5,next_state(q2,end,[q1]):0.05],[]),
		rule(3,letter(q2,g,[q1]),[letter(q2,a,[q1]):0.1,letter(q2,c,[q1]):0.2,
		  letter(q2,g,[q1]):0.3,letter(q2,t,[q1]):0.4],[]),
		rule(1,next_state(q2,end,[q2,q1]),[next_state(q2,q1,[q2,q1]):0.45,
		  next_state(q2,q2,[q2,q1]):0.5,next_state(q2,end,[q2,q1]):0.05],[]),
		rule(3,letter(q2,g,[q2,q1]),[letter(q2,a,[q2,q1]):0.1,
		  letter(q2,c,[q2,q1]):0.2,letter(q2,g,[q2,q1]):0.3,
			letter(q2,t,[q2,q1]):0.4],[])]
  ),pitavit_hmm).

test((vit_prob(hmm([a,a,a]),P,Exp),close_to(P,0.0008000000000000003),
  Exp = [rule(0,next_state(q1,q1,[]),[next_state(q1,q1,[]):0.5,
	  next_state(q1,q2,[]):0.45,next_state(q1,end,[]):0.05],[]),
	rule(2,letter(q1,a,[]),[letter(q1,a,[]):0.4,letter(q1,c,[]):0.3,
	  letter(q1,g,[]):0.2,letter(q1,t,[]):0.1],[]),
	rule(0,next_state(q1,q1,[q1]),[next_state(q1,q1,[q1]):0.5,
	  next_state(q1,q2,[q1]):0.45,next_state(q1,end,[q1]):0.05],[]),
	rule(2,letter(q1,a,[q1]),[letter(q1,a,[q1]):0.4,letter(q1,c,[q1]):0.3,
	  letter(q1,g,[q1]):0.2,letter(q1,t,[q1]):0.1],[]),
	rule(0,next_state(q1,end,[q1,q1]),[next_state(q1,q1,[q1,q1]):0.5,
	  next_state(q1,q2,[q1,q1]):0.45,next_state(q1,end,[q1,q1]):0.05],[]),
	rule(2,letter(q1,a,[q1,q1]),[letter(q1,a,[q1,q1]):0.4,
	  letter(q1,c,[q1,q1]):0.3,letter(q1,g,[q1,q1]):0.2,
	  letter(q1,t,[q1,q1]):0.1],[])]
  ),pitavit_hmm).

test((vit_prob(heads(coin),Prob,Exp),close_to(Prob,0.45),
  Exp = [rule(2, fair(coin), [fair(coin):0.9, biased(coin):0.1], []),
    rule(0, heads(coin), [heads(coin):0.5, tails(coin):0.5],
    [toss(coin), \+biased(coin)])]
  ),
	pitavit_coin).

test((vit_prob(color(s,purple),Prob,Exp),close_to(Prob,0.5),
	Exp = [rule(0, cg(s, 1, p), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
	 [mother(m, s), cg(m, 1, p), cg(m, 2, w)])]),
 	pitavit_mendel).


test((vit_prob(color(s,white),Prob,Exp),close_to(Prob,0.25),
  Exp = [rule(0, cg(s, 1, w), [cg(s, 1, p):0.5, cg(s, 1, w):0.5],
     [mother(m, s), cg(m, 1, p), cg(m, 2, w)]),
	 rule(1, cg(s, 2, w), [cg(s, 2, w):0.5, cg(s, 2, p):0.5],
	   [father(f, s), cg(f, 1, w), cg(f, 2, p)])]),
	pitavit_mendel).
