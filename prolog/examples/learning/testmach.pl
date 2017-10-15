


test((induce_par([train],P),
  writeln('Result:'),
  writeln(P),
  writeln('Expected:'),
  writeln([(class(sendback):1.0;'':0.0:-worn(A), not_replaceable(A)),
   (class(fix):0.5714285714285714;'':0.4285714285714286:-worn(B), replaceable(B)),
   (class(ok):0.2;'':0.8:-not_worn(_120))])),mach).
test((in(P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),close_to(LL, -21.286207461851408),close_to(AUCROC,0.7733333333333333),close_to(AUCPR, 0.5527564018467214)),mach).
test((induce([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),close_to(LL,-18.554628716462105),close_to(AUCROC,0.77),close_to(AUCPR,0.600952380952381)),mach).
