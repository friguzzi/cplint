:- module(test_cplint_util,
  [test_util/0]).
:- use_module(library(plunit)).


:-use_module(library(cplint_test/cplint_test)).

test_util:-
	run_tests([
    beta,av
  ]).



:- begin_tests(beta, []).

:-ensure_loaded(library(cplint_util)).

test(beta_1_1):-
  run((beta([1,1],B),
  close_to(B,1.0))).


test(beta_2_2):-
  run((beta([2,2],B),
  close_to(B,0.16666666666666663))).

test(beta_1_2):-
  run((beta([1,2],B),
  close_to(B,0.49999999999999994))).


test(beta_05_05):-
  run((beta([0.5,0.5],B),
  close_to(B,3.1415926535897927))).

test(beta_03_03):-
  run((beta([0.3,0.3],B),
  close_to(B,6.009623683731014))).

test(bar1):-
  bar1(0.5,C),C=c3{axis:_4862{rotated:true,x:_4878{type:category},
  y:_4886{max:1.0,min:0.0,padding:_4918{bottom:0.0,top:0.0},
  tick:_4942{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}},
  data:_4822{rows:[elem-prob,'T'-0.50],type:bar,x:elem},
  legend:_5082{show:false},size:_4814{height:100}}.
test(bar):-
  bar(0.5,C),C=c3{axis:_4862{rotated:true,x:_4878{type:category},
  y:_4886{max:1.0,min:0.0,padding:_4918{bottom:0.0,top:0.0},
  tick:_4942{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}},
  data:_4822{rows:[elem-prob,'T'-0.50,'F'-0.50],type:bar,x:elem},
  legend:_5082{show:false},size:_4814{height:100}}.

:- end_tests(beta).

:- begin_tests(av, []).

:-ensure_loaded(library(cplint_util)).

test(av1):-
  average([1,1],1).

test(av2):-
  average([1-1,1-1],1).

test(av3):-
  average([[1,1]-1,[1,1]-1],[1,1]).

:- end_tests(av).