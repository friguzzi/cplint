/*
The task is to decide whether a machine should be fixed, sent back or is ok.
Machines dataset from The ACE Data Mining System User's Manual
https://dtai.cs.kuleuven.be/ACE/doc/ACEuser-1.2.16.pdf

Downloaded from
https://dtai.cs.kuleuven.be/static/ACE/doc/
*/

/** <examples>
?- in(P),test(P,[all],LL,AUCROC,ROC,AUCPR,PR).
*/
:-use_module(library(slipcover)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(lpad).
:- endif.

:-sc.

:- set_sc(depth_bound,false).
:- set_sc(neg_ex,given).
:- set_sc(megaex_bottom,15).
:- set_sc(max_iter,10).
:- set_sc(max_iter_structure,50).
:- set_sc(verbosity,1).


:- begin_in.
class(sendback):-
  sendback.

class(fix):-
  \+ sendback,
  fix.

class(ok):-
  \+ sendback,
  \+ fix.

sendback:-
  worn(_A), 
  not_replaceable(_B), 
  not_replaceable(C), 
  worn(C).

fix:-
  worn(_A).

:- end_in.

:- begin_bg.
component(C):-
  replaceable(C).
component(C):-
  not_replaceable(C).
replaceable(gear).
replaceable(wheel).
replaceable(chain).
not_replaceable(engine).
not_replaceable(control_unit).
not_worn(C):-
  component(C),
  \+ worn(C).
one_worn:-
  worn(_).
none_worn:-
  \+ one_worn.
:- end_bg.

fold(train,[1,2,3,4,5,6,7,8,9,10]).

fold(test,[11,12,13,14,15]).

fold(all,F):-
  fold(train,FTr),
  fold(test,FTe),
  append(FTr,FTe,F).

:- fold(all,F),
   sample(10,F,FTr,FTe),
   assert(fold(rand_train,FTr)),
   assert(fold(rand_test,FTe)).

output(class/1).

input_cw(replaceable/1).
input_cw(not_replaceable/1).
input_cw(worn/1).
input_cw(not_worn/1).
input_cw(none_worn/0).



begin(model(1)).
testnr(1).
class(sendback).
neg(class(fix)).
neg(class(ok)).
worn(gear).
worn(engine).
end(model(1)).

begin(model(2)).
testnr(2).
class(ok).
neg(class(sendback)).
neg(class(fix)).
end(model(2)).

begin(model(3)).
testnr(3).
class(fix).
neg(class(sendback)).
neg(class(ok)).
worn(gear).
end(model(3)).

begin(model(4)).
testnr(4).
class(sendback).
neg(class(fix)).
neg(class(ok)).
worn(engine).
end(model(4)).

begin(model(5)).
testnr(5).
class(fix).
neg(class(sendback)).
neg(class(ok)).
worn(gear).
worn(chain).
end(model(5)).

begin(model(6)).
testnr(6).
class(fix).
neg(class(sendback)).
neg(class(ok)).
worn(wheel).
end(model(6)).

begin(model(7)).
testnr(7).
class(sendback).
neg(class(fix)).
neg(class(ok)).
worn(wheel).
worn(control_unit).
end(model(7)).

begin(model(8)).
testnr(8).
class(ok).
neg(class(sendback)).
neg(class(fix)).
end(model(8)).

begin(model(9)).
testnr(9).
class(fix).
neg(class(sendback)).
neg(class(ok)).
worn(wheel).
worn(chain).
end(model(9)).

begin(model(10)).
testnr(10).
class(sendback).
neg(class(fix)).
neg(class(ok)).
worn(engine).
worn(chain).
end(model(10)).

begin(model(11)).
testnr(11).
class(sendback).
neg(class(fix)).
neg(class(ok)).
worn(engine).
worn(control_unit).
end(model(11)).

begin(model(12)).
testnr(12).
class(fix).
neg(class(sendback)).
neg(class(ok)).
worn(chain).
worn(wheel).
worn(gear).
end(model(12)).

begin(model(13)).
testnr(13).
class(sendback).
neg(class(fix)).
neg(class(ok)).
worn(chain).
worn(wheel).
worn(gear).
worn(engine).
end(model(13)).

begin(model(14)).
testnr(14).
class(ok).
neg(class(sendback)).
neg(class(fix)).
end(model(14)).

begin(model(15)).
testnr(15).
class(fix).
neg(class(sendback)).
neg(class(ok)).
worn(wheel).
worn(gear).
end(model(15)).
