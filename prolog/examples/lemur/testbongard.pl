
:- style_check(-singleton).

test((induce_lm([train],P),test_lm(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
P = [(pos:0.23497241271435504;'':0.7650275872856449:-triangle(_92), triangle(_96), triangle(_100), in(_96, _106)),
  (pos:0.09907287899912748;'':0.9009271210008725:-circle(_110), in(_114, _110), circle(_120), in(_114, _120), square(_130)),
  (pos:2.400626683893628e-8;'':0.9999999759937331:-square(_134))],
LL = -66.18600681193735,
AUCROC = 0.9038314176245211,
AUCPR = 0.7937421909836889),
bongard).
