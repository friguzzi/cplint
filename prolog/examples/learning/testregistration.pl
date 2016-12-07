

test((in(P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
P = [(party(yes):0.5:-company_type(commercial)),  (party(no):0.5:-subscription(C), course_len(C, 4), \+company_type(commercial))],
LL = -56.64833659297699,
AUCROC = 0.6666666666666666,
AUCPR = 0.7524018379281537),registration).

test((induce([all],P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
P = [(party(yes):0.9991328314090373;'':0.0008671685909626969:-not_company_type(university)),  (party(yes):0.7535193156573855;'':0.2464806843426145:-company_type(commercial)),  (party(yes):0.25002777076736327;'':0.7499722292326367:-subscription(_646)),  (party(no):1.0;'':0.0:-company_type(university), subscription(_1262), subscription(C), course_len(C, 4))],
LL = -2.249661210989883,
AUCROC = 0.9583333333333334,
AUCPR = 0.9722222222222222),registration).



