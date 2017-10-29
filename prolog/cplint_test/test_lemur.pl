:- module(test_lemur,
  [test_lemur/0]).
:- use_module(library(plunit)).

test_lemur:-
  run_tests([
    mach_lm,
    bongard_lm,
    bongardkeys_lm,
    registration_lm
  ]).


:- begin_tests(mach_lm, []).
:-ensure_loaded(library(examples/lemur/mach)).
test(induce_lm):-
  induce_lm([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  writeln((LL,AUCROC,AUCPR)),
  writeln('Expected:'),
  writeln([(class_fix:0.167671594180088;'':0.832328405819912:-not_replaceable(_92), replaceable(_96), replaceable(_100), worn(_96), not_worn(_92)),
  (class_sendback:1.0;'':0.0:-not_replaceable(_112), worn(_112)),
  (class_ok:1.0;'':0.0:-none_worn),
  (class_ok:7.426381092891156e-11;'':0.9999999999257362:-not_replaceable(_120))]),
  writeln((-1.8063458978498406,1.0,1.0)).
:- end_tests(mach_lm).


:- begin_tests(bongard_lm, []).
:-ensure_loaded(library(examples/lemur/bongard)).
test(induce_lm):-
  induce_lm([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  writeln((LL,AUCROC,AUCPR)),
  writeln('Expected:'),
  writeln([(pos:0.23497241271435504;'':0.7650275872856449:-triangle(_92), triangle(_96), triangle(_100), in(_96, _106)),
  (pos:0.09907287899912748;'':0.9009271210008725:-circle(_110), in(_114, _110), circle(_120), in(_114, _120), square(_130)),
  (pos:2.400626683893628e-8;'':0.9999999759937331:-square(_134))]),
  writeln((-66.18600681193735,0.9038314176245211,0.7937421909836889)).

:- end_tests(bongard_lm).

:- begin_tests(bongardkeys_lm, []).
:-ensure_loaded(library(examples/lemur/bongardkeys)).


test(induce_lm):-
  induce_lm([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  writeln((LL,AUCROC,AUCPR)),
  writeln('Expected:'),
  writeln([(pos:0.23497241271435504;'':0.7650275872856449:-triangle(_92), triangle(_96), triangle(_100), in(_96, _106)),
  (pos:0.09907287899912748;'':0.9009271210008725:-circle(_110), in(_114, _110), circle(_120), in(_114, _120), square(_130)),
  (pos:2.400626683893628e-8;'':0.9999999759937331:-square(_134))]),
  writeln((-66.18600681193735,0.9038314176245211,0.7937421909836889)).

:- end_tests(bongardkeys_lm).

:- begin_tests(registration_lm, []).
:-ensure_loaded(library(examples/lemur/registration)).

test(induce_lm):-
  induce_lm([all],P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  writeln((LL,AUCROC,AUCPR)),
  writeln('Expected:'),
  writeln([(party(_92):0.06663081956298464;'':0.9333691804370153:-subscription(_96)),
  (party(_100):0.20556786978108324;'':0.7944321302189168:-subscription(_104))]),
  writeln((-8.818022622164117,0.5,0.5)).

:- end_tests(registration_lm).
