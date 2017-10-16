
test((induce_lm([all],P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  writeln((LL,AUCROC,AUCPR)),
  writeln('Expected:'),
  writeln([(party(_92):0.06663081956298464;'':0.9333691804370153:-subscription(_96)),
  (party(_100):0.20556786978108324;'':0.7944321302189168:-subscription(_104))]),
  writeln((-8.818022622164117,0.5,0.5))),
  registration).
