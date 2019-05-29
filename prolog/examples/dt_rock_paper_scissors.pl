:- use_module(library(pita)).

:- pita.

:- begin_lpad.

? :: rock_a.
? :: paper_a. 
? :: scissor_a.

0.34::rock_b.
0.33::paper_b.
0.33::scissor_b.

loose:- paper_b,rock_a.
loose:- rock_b,scissor_a.
loose:- scissor_b,paper_a.

win:- paper_b,scissor_a.
win:- scissor_b,rock_a.
win:- rock_b,paper_a.

tie:- paper_b,paper_a.
tie:- scissor_b,scissor_a.
tie:- rock_b,rock_a.

utility(win,1).
utility(tie,0).
utility(loose,-1).

:- end_lpad.

/*
 * ?- dt_solve(Strategy,Value).
 * Expected result:
 * Strategy = paper_a
 * Value = 0.01
*/