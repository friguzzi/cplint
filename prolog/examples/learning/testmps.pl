


test((induce_par([train],P), test_prob(P,[train],_NP,_NN,LL,_EL),
writeln(P),
writeln(
[(p:1.0:-a, b),  (p:1.0:- \+a, \+b),  (a:0.14098536470750564;'':0.8590146352924943:-true),
(b:0.2686080357352204;'':0.7313919642647796:-true)]),
writeln([0.6661462033202675-p(train1), 0.6661462033202675-(\+p(train2)),
0.6661462033202675-p(train3)]),close_to(LL, -1.9095443323886057)
),multiple_paths_simple_learning).
