

test((induce_lm([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  writeln((LL,AUCROC,AUCPR)),
  writeln('Expected:'),
  writeln([(class_fix:0.167671594180088;'':0.832328405819912:-not_replaceable(_92), replaceable(_96), replaceable(_100), worn(_96), not_worn(_92)),
  (class_sendback:1.0;'':0.0:-not_replaceable(_112), worn(_112)),
  (class_ok:1.0;'':0.0:-none_worn),
  (class_ok:7.426381092891156e-11;'':0.9999999999257362:-not_replaceable(_120))]),
  writeln((-1.8063458978498406,1.0,1.0))),
mach).
