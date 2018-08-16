:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.


card(_, Card): discrete(Card, [2:1/13, 3:1/13, 4:1/13, 5:1/13, 6:1/13, 7:1/13,
     8:1/13, 9:1/13, 10:1/13, 11:1/13, 12:1/13, 13:1/13, 14:1/13]).

pair :- card(deck1, C), card(deck2, C).

:- end_lpad.
