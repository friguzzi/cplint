
:- style_check(-singleton).

test((induce_par([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['P =', "[(shops(john):0.20548261545513422;'':0.7945173845448658:-true),  (shops(mary):0.8982135971596719;'':0.10178640284032814:-true),  (bought(spaghetti):0.5448836590413033;bought(steak):0.4551163409586967:-shops(john)),  (bought(spaghetti):0.29413365772975464;bought(fish):0.7058663422702454:-shops(mary))]",
'\nLL =', -3137.1337240554694,
'\nAUCROC =', 0.8035006664184584,
'\nAUCPR =', 0.752406766447365,'\n'],St1),
writeln(St1)),shop).

test((in(P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
P = [shops(john):0.2, shops(mary):0.9,  (bought(spaghetti):0.5;bought(steak):0.5:-shops(john)),  (bought(spaghetti):0.3;bought(fish):0.7:-shops(mary))],
LL = -3127.225749380786,
AUCROC = 0.8035006664184584,
AUCPR = 0.752406766447365),shop).


