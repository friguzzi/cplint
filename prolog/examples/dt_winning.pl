:- use_module(library(pita)).

:- pita.

:- begin_lpad.
%%% mut_exl2.pl
% ? :: play.

% ? :: bet(heads):- play.
% ? :: bet(tails):- play.
% % ?::bet(tails); ?::bet(heads) :- play. %% versione vera
% % ?::bet(tails); ?::bet(heads). % test con disj


% 0.45::toss(heads); 0.55::toss(tails).


% win :- play,  toss(X), bet(X).

% utility(play, -5).
% utility(bet(heads), -1).
% utility(bet(tails), -1).
% utility(win, 10).
%%%%%%%%%%%

%%%% winning
0.3::solo_win1.
0.3::solo_win2.

0.5::both_win :- \+(solo_win1), \+(solo_win2).

win1 :- play1, solo_win1, \+play2.
win1 :- play1, both_win, \+play2.

win2 :- play2, solo_win2, \+play1.
win2 :- play2, both_win, \+play1.

decision(play1).
decision(play2).

utility(play1, -10).
utility(play2, -10).
utility(win1, 50).
utility(win2, 50).

% ?- dt_solve(A,B)
% A = [play1],
% B = 17.25

:- end_lpad.