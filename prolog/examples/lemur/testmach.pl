
:- style_check(-singleton).


test((induce_lm([train],P),test_lm(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
P = [(class_sendback:1.0;'':0.0:-not_replaceable(_92), worn(_92)),  (class_fix:0.07541910269950489;'':0.9245808973004951:-replaceable(_100), worn(_100), replaceable(_108), not_worn(_112)),  (class_ok:1.0;'':0.0:-none_worn),  (class_ok:1.2980416325929554e-8;'':0.9999999870195837:-not_worn(_116))],
LL = -1.2645940192432141,
AUCROC = AUCPR, AUCPR = 1.0),
mach).
