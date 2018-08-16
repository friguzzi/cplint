:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.


card(_, Card): uniform(Card, [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]):-
body.

body.

pair :- card(deck1, C), card(deck2, C).

:- end_lpad.
