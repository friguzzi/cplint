
:- use_module(library(kbest)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- kbest.

:- begin_lpad.

win :- red, green.
win :- blue, yellow.

 0.4::red.
 0.9::green.
 0.5::blue.
 0.6::yellow.


:- end_lpad.

/** <examples>
?- kbest(win,2,Exp).
Exp = [0.36000000000000004-[rule(0, red, [red:0.4, '':0.6], []),
   rule(1, green, [green:0.9, '':0.09999999999999998], [])],
 0.30000000000000004-[rule(2, blue, [blue:0.5, '':0.5], []),
 rule(3, yellow, [yellow:0.6, '':0.4], [])]].

?- kbest(win,2,P,Exp).
P = 0.552,
Exp = [0.36000000000000004-[rule(0, red, [red:0.4, '':0.6], []),
   rule(1, green, [green:0.9, '':0.09999999999999998], [])],
 0.30000000000000004-[rule(2, blue, [blue:0.5, '':0.5], []),
 rule(3, yellow, [yellow:0.6, '':0.4], [])]].




*/
