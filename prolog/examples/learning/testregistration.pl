

test((in(P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\nP =', "[(party(yes):0.5:-company_type(commercial)),  (party(no):0.5:-subscription(C), course_len(C, 4), \\+company_type(commercial))]",
'\nLL =', -17.281246460764,
'\nAUCROC =',0.9166666666666667,
'\nAUCPR =', 0.9583333333333334],St1),
writeln(St1)),registration).

test((induce([all],P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\nP =',
"[(party(yes):0.015604109954480928;:0.9843958900455191:-company_type(university),job(researcher)),(party(yes):0.3224437982952064;:0.6775562017047936:-job(researcher)),(party(yes):0.9999999983379018;:1.6620982368209525e-9:-company_type(commercial)),(party(yes):0.6233797589501375;:0.37662024104986247:-not_company_type(university)),(party(yes):0.0003946210127296068;:0.9996053789872704:-subscription(_814)),(party(no):1.0;:0.0:-not_company_type(commercial),subscription(_1844),subscription(_1854),subscription(_1864),course_len(_1864,4))]",
'\nLL =', -1.909937224474644,
'\nAUCROC =', 0.9722222222222223,
'\nAUCPR =', 0.9791666666666667],St1),
writeln(St1)),registration).
