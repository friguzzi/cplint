

test((in(P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['P =', "[(party(yes):0.5:-company_type(commercial)),  (party(no):0.5:-subscription(C), course_len(C, 4), \\+company_type(commercial))]",
'\nLL =', -17.281246460764,
'\nAUCROC =',0.9166666666666667,
'\nAUCPR =', 0.9583333333333334],St1),
writeln(St1)),registration).

test((induce([all],P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['P =',
"[(party(yes):0.9991328314090373;'':0.0008671685909626969:-not_company_type(university)),  (party(yes):0.7535193156573855;'':0.2464806843426145:-company_type(commercial)),  (party(yes):0.25002777076736327;'':0.7499722292326367:-subscription(_646)),  (party(no):1.0;'':0.0:-company_type(university), subscription(_1262), subscription(C), course_len(C, 4))]",
'\nLL =', -2.249661210989883,
'\nAUCROC =', 0.9583333333333334,
'\nAUCPR =', 0.9722222222222222],St1),
writeln(St1)),registration).



