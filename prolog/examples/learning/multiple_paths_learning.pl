:- use_module(library(slipcover)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(lpad).
:- endif.

/** <examples>
?- induce_par([train],P),test_prob(P,[train],NP,NN,LL,L).
?- induce_par([train],P).  % learn the parameteters
*/

:-sc.

:- set_sc(verbosity,1).
:- set_sc(depth_bound,false).
:- set_sc(neg_ex,given).

bg([]).

fold(train,[train1,train2]).

output(p/0).

:- begin_in.
p:- c,\+ d,a,b.

p:- d, \+ a,\+b.

a:0.3.

b:0.4.

c:0.2.

d:0.1.

:- end_in.

begin(model(train1)).
p.
end(model(train1)).
begin(model(train2)).
neg(p).
end(model(train2)).
