/*
Computing the probability of mining the next block with a two-phase
Proof-of-Work protocol in a Bitcoin network
From Damiano Azzolini, Fabrizio Riguzzi, Evelina Lamma, Elena Bellodi,
and Riccardo Zese.
Modeling bitcoin protocols with probabilistic logic programming.
PLP 2018 http://ceur-ws.org/Vol-2219/paper6.pdf
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.

:- mc.

:- begin_lpad.

% http://referaat.cs.utwente.nl/conference/22/paper/7473/preventingthe-51-attack%20-a-stochastic-analysis-of-two-phase-proof-of-work-in-bitcoin.pdf
/*
 * 2-phase PoW. 
 * This new method consists of a PoW organized in 2 steps: 
 * the ﬁrst step is to ﬁnd a double hash of the header SHA256(SHA256(header)) 
 * that is smaller than a certain value X (this is the current PoW). 
 * The second one consists in signing the header with the private key of the 
 * address that will receive the mining reward and then ﬁnding a new hash 
 * SHA256(SIG(header,privateKey)) smaller than a value Y 
 * a peer can be in one of the following 4 states:
 * – State 0 (a0): a miner generates a hash using a certain nonce. 
 *		If this hash is correct, it will move to state 1, 
 *  	if it’s not, it will stay in a0.
 * – State 1 (a1): the miner has already solved the ﬁrst hash 
 *		puzzle (he has found a nonce value such that SHA256(SHA256(header)) 
 *	   	is less than a diﬃculty parameter X) and now needs to solve 
 *		the second one (ﬁnd a hash value such that SHA256(SIG(header,privateKey)) 
 *      is less than a diﬃculty parameter Y). 
 * – State 2 (a2): the miner has solved both hashes, is rewarded and now 
 *  	is ready to mine another block (back to state 0). 
 * – State 3 (a3): another miner has solved the second hash, 
 * 		so the ﬁrst miner can now stop working on this hash and move 
 *	    to another one (back to state 0).
*/

reach(S, I, T) :-
  	trans(S, I, U),
  	reach(U, next(I), T).
reach(S, _, S).

a_found_y(_):0.15.
b_found_y(_):0.25.
b_found_x(_):0.10.
found_y(S):- a_found_y(S); b_found_y(S).

trans(a0,S,a1):1.0/50; trans(a0,S,a0):1.0-1.0/50:- \+b_found_x(S).


trans(a1,S,a2):0.15;trans(a1,S,a1):1.0-0.15:- \+b_found_y(S).

trans(a0,S,a3):- b_found_x(S).
trans(a1,S,a3):- b_found_y(S).

trans(a2,S,a0):- a_found_y(S).
trans(a3,S,a0):- b_found_y(S).

% same for B
trans(b0,S,b1):1.0/28; trans(b0,S,b0):1.0-1.0/28:- \+a_found_y(S).
trans(b1,S,b2):0.25;trans(b1,S,b1):1.0-0.25:- \+a_found_y(S).
trans(b0,S,b3):- a_found_y(S).
trans(b1,S,b3):- a_found_y(S).

trans(b2,S,b0):- a_found_y(S).
trans(b3,S,b0):- b_found_y(S).

:- end_lpad.

draw(D):-
    runModel(1000,PA,_),
    histogram(PA,D,[nbins(100)]).

% used to draw the graph
runModel(0,[],[]).
runModel(I,[[PA]-1|TA],[[_]-1|TB]):-
    I > 0,
    mc_sample(reach(a0,0,a2),100,PA),
    %mc_sample(reach(b0,0,b2),100,PB),
    I1 is I-1,
    runModel(I1,TA,TB).
markov_chain(digraph(G)):-
    findall(edge(A -> B,[label=P]),
            (clause(trans(A,_,B),
                    (sample_head(_,_,Probs,N))),
                nth0(N,Probs,_:P)),
            G0),
    findall(edge(A -> B,[label=1.0]),
            clause(trans(A,_,B),_),
            G1),
    append(G0,G1,G).

/** <examples>
?- draw(D).
?- mc_sample(reach(a0,0,a2),100,PA).
?- mc_sample(reach(b0,0,b2),100,PB).
?- mc_prob(reach(a0,0,a2),PA).
*/
