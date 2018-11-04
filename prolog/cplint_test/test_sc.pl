
:- module(test_sc,
  [test_sc/0,test_all/0,test_par/0,test_stru/0]).
:- use_module(library(plunit)).

test_sc:-
  test_all.

test_all:-
  par(P),
  stru(S),
  append(P,S,A),
  run_tests(A).

test_par:-
  par(P),
  run_tests(P).

test_stru:-
  stru(S),
  run_tests(S).

par([
	  bongard,
		bongardkeys,
		hmmlearn,
		shop,
		multiple_paths_simple_learning,
		multiple_paths_learning,
    bongard_ind,
    bongard_fixed,
    bongard_initial]).

stru([mach,
		registration]).


:- begin_tests(mach, []).
:-ensure_loaded(library(examples/learning/mach)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_par):-
  induce_par([train],P),
  writeln('Result:'),
  writeln(P),
  writeln('Expected:'),
  writeln([(class(sendback):1.0;'':0.0:-worn(A), not_replaceable(A)),
   (class(fix):0.5714285714285714;'':0.4285714285714286:-worn(B), replaceable(B)),
   (class(ok):0.2;'':0.8:-not_worn(_120))]).

test(test_in):-
  in(P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),close_to(LL, -21.286207461851408),close_to(AUCROC,0.7733333333333333),close_to(AUCPR, 0.5527564018467214).
test(induce):-
  induce([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),close_to(LL,-18.554628716462105),close_to(AUCROC,0.77),close_to(AUCPR,0.600952380952381).
:- end_tests(mach).

:- begin_tests(bongard, []).
:-ensure_loaded(library(examples/learning/bongard)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_par):-
induce_par([train],P),
writeln('Result:'),
writeln(P),
writeln('Expected:'),
writeln([(pos:0.08375986464331642;'':0.9162401353566836:-circle(A), in(_96, A)),  (pos:0.41285257298968425;'':0.5871474270103157:-circle(_92), triangle(_97))]).


:- end_tests(bongard).

:- begin_tests(bongardkeys, []).
:-ensure_loaded(library(examples/learning/bongardkeys)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_par):-
induce_par([train],P),
writeln('Result:'),
writeln(P),
writeln('Expected:'),
writeln([(pos:0.08375986464331642;'':0.9162401353566836:-circle(A), in(_96, A)),  (pos:0.41285257298968425;'':0.5871474270103157:-circle(_92), triangle(_97))]).
:- end_tests(bongardkeys).

:- begin_tests(registration, []).
:-ensure_loaded(library(examples/learning/registration)).
:-use_module(library(cplint_test/cplint_test)).


test(in):-
  in(P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\nP =', "[(party(yes):0.5:-company_type(commercial)),  (party(no):0.5:-subscription(C), course_len(C, 4), \\+company_type(commercial))]",
'\nLL =', -17.281246460764,
'\nAUCROC =',0.9166666666666667,
'\nAUCPR =', 0.9583333333333334],St1),
writeln(St1).

test(induce):-
  induce([all],P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\nP =',
"[(party(yes):0.015604109954480928;:0.9843958900455191:-company_type(university),job(researcher)),(party(yes):0.3224437982952064;:0.6775562017047936:-job(researcher)),(party(yes):0.9999999983379018;:1.6620982368209525e-9:-company_type(commercial)),(party(yes):0.6233797589501375;:0.37662024104986247:-not_company_type(university)),(party(yes):0.0003946210127296068;:0.9996053789872704:-subscription(_814)),(party(no):1.0;:0.0:-not_company_type(commercial),subscription(_1844),subscription(_1854),subscription(_1864),course_len(_1864,4))]",
'\nLL =', -1.909937224474644,
'\nAUCROC =', 0.9722222222222223,
'\nAUCPR =', 0.9791666666666667],St1),
writeln(St1).

:- end_tests(registration).

:- begin_tests(hmmlearn, []).
:-ensure_loaded(library(examples/learning/hmmlearn)).
:-use_module(library(cplint_test/cplint_test)).
%:- style_check(-singleton).

test(induce_par):-
  induce_par([train],P),
writeln('Result:'),
writeln(P),
writeln('Expected:'),
writeln([(success(_3868):1.0:-hmmf(_3868, [r0], 3)),  (hmmf([p|_3868], _3888, _3890):0.397642938480145;hmmf([s|_3868], _3888, _3890):0.602357061519855:-_3890>0, _3888=[r0|_3928], tr(_3888, _3934), _3938 is _3890-1, hmmf(_3868, [_3934|_3888], _3938)),  (hmmf([s|_3868], _3888, _3890):0.52143903729156;hmmf([h|_3868], _3888, _3890):0.47856096270843995:-_3890>0, _3888=[r1|_4006], tr(_3888, _3934), _3938 is _3890-1, hmmf(_3868, [_3934|_3888], _3938)),  (hmmf([], _3888, 0):1.0:-true),
(tr([r0|_4058], r0):0.6858291009862068;tr([r0|_4058], r1):0.31417089901379325:-true),  (tr([r1|_4058], r0):0.2719403559829671;tr([r1|_4058], r1):0.7280596440170328:-true)]).
:- end_tests(hmmlearn).

:- begin_tests(shop, []).
:-ensure_loaded(library(examples/learning/shop)).
%:- style_check(-singleton).

test(induce_par):-
  induce_par([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\nP =', "[(shops(john):0.20548261545513422;'':0.7945173845448658:-true),  (shops(mary):0.8982135971596719;'':0.10178640284032814:-true),  (bought(spaghetti):0.5448836590413033;bought(steak):0.4551163409586967:-shops(john)),  (bought(spaghetti):0.29413365772975464;bought(fish):0.7058663422702454:-shops(mary))]",
'\nLL =', -3137.1337240554694,
'\nAUCROC =', 0.8035006664184584,
'\nAUCPR =', 0.752406766447365,'\n'],St1),
writeln(St1).

test(in):-
  in(P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
P = [shops(john):0.2, shops(mary):0.9,  (bought(spaghetti):0.5;bought(steak):0.5:-shops(john)),  (bought(spaghetti):0.3;bought(fish):0.7:-shops(mary))],
LL = -3127.225749380786,
AUCROC = 0.8035006664184584,
AUCPR = 0.752406766447365.
:- end_tests(shop).

:- begin_tests(multiple_paths_simple_learning, []).
:-ensure_loaded(library(examples/learning/multiple_paths_simple_learning)).

:-use_module(library(cplint_test/cplint_test)).


test(induce_par):-
  induce_par([train],P), test_prob(P,[train],_NP,_NN,LL,_EL),
writeln(P),
writeln(
[(p:1.0:-a, b),  (p:1.0:- \+a, \+b),  (a:0.14098536470750564;'':0.8590146352924943:-true),
(b:0.2686080357352204;'':0.7313919642647796:-true)]),
writeln([0.6661462033202675-p(train1), 0.6661462033202675-(\+p(train2)),
0.6661462033202675-p(train3)]),close_to(LL, -1.9095443323886057).

:- end_tests(multiple_paths_simple_learning).

:- begin_tests(multiple_paths_learning, []).
:-ensure_loaded(library(examples/learning/multiple_paths_learning)).


test(induce_par):-
  induce_par([train],P),writeln(P),
writeln( [(p:1.0:-c, \+d, a, b),  (p:1.0:-d, \+a, \+b),
(a:0.11345438264374658;'':0.8865456173562534:-true),
  (b:0.24757444431927064;'':0.7524255556807293:-true),
    (c:0.6699713764888383;'':0.3300286235111617:-true),
      (d:0.7420553493044134;'':0.2579446506955866:-true)]).

:- end_tests(multiple_paths_learning).

:- begin_tests(bongard_ind, []).
:-ensure_loaded(library(examples/learning/bongard_ind)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_par):-
induce_par([train],P),
writeln('Result:'),
writeln(P),
writeln('Expected:'),
writeln([(pos:0.0032009782151307584;'':0.9967990217848692:-circle(_24), in(_28, _24)), 
  (pos:0.36069511297793233;'':0.6393048870220677:-triangle(_90), config(_90, down)),
  (pos:0.4178555801893244;'':0.5821444198106756:-triangle(_144), config(_144, up))]).
:- end_tests(bongard_ind).

:- begin_tests(bongard_fixed, []).
:-ensure_loaded(library(examples/learning/bongard_fixed)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_par):-
induce_par([train],P),
writeln('Result:'),
writeln(P),
writeln('Expected:'),
writeln( [(pos:0.0021000155528233407;'':0.9978999844471766:-circle(_24), in(_28, _24)),  (pos:0.5;'':0.5:-circle(_24), triangle(_28))]).
:- end_tests(bongard_fixed).

:- begin_tests(bongard_initial, []).
:-ensure_loaded(library(examples/learning/bongard_initial)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_par):-
induce_par([train],P),
writeln('Result:'),
writeln(P),
writeln('Expected:'),
writeln([(pos:0.07507285519157605;'':0.924927144808424:-circle(_24), in(_28, _24)),  (pos:0.41706649095461357;'':0.5829335090453864:-circle(_88), triangle(_98))]).

:- end_tests(bongard_initial).

