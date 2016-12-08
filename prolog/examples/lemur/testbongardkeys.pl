
:- style_check(-singleton).

test((induce_lm([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
P = [(pos:0.5441685114747136;'':0.45583148852528643:-triangle(_92), in(_92, _98)),  (pos:1.0;'':0.0:-circle(_102), triangle(_106), in(_110, _102), square(_110)),  (pos:0.025412522467442973;'':0.974587477532557:-triangle(_120), triangle(_124))],
LL = -50.080247980893986,
AUCROC = 0.9592592592592593,
AUCPR = 0.9147618914232571),
bongardkeys).

