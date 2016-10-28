:- use_module(library(pita)).


:- pita.

:- begin_lpad.

intelligent(_):0.5.

good_marks(_):0.5.

friends(_,_):0.5.

% 1.5  Intelligent(x) => GoodMarks(x) 
factor1(X): 0.31025740516:- \+intelligent(X), \+good_marks(X).
factor1(X): 0.31025740516:- \+intelligent(X), good_marks(X).
factor1(X): 0.0692277845:- intelligent(X), \+good_marks(X).
factor1(X): 0.31025740516:- intelligent(X), good_marks(X).

% 1.1 Friends(x, y) => (Intelligent(x) <=> Intelligent(y)) 1.1
factor2(X,Y): 0.150020804:- \+friends(X,Y).
factor2(X,Y): 0.150020804:- friends(X,Y), intelligent(X),intelligent(Y).
factor2(X,Y): 0.150020804:- friends(X,Y), \+intelligent(X),\+intelligent(Y).
factor2(X,Y): 0.049937588:- friends(X,Y), intelligent(X),\+intelligent(Y).
factor2(X,Y): 0.049937588:- friends(X,Y), \+intelligent(X),intelligent(Y).

student(anna).
student(bob).
evidence:- factor1(anna),factor1(bob),factor2(anna,anna),
    factor2(anna,bob),factor2(bob,anna),factor2(bob,bob).

ev_intelligent_bob_friends_anna_bob:-
    intelligent(bob),friends(anna,bob),evidence.

:- end_lpad.

/** <examples>

?- prob(intelligent(anna),P).
?- prob(intelligent(anna),ev_intelligent_bob_friends_anna_bob,P).

*/
