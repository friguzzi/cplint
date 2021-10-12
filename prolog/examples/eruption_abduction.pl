:- use_module(library(pita)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.

:- begin_lpad.

eruption : 0.6; earthquake : 0.3 :- sudden_energy_release, fault_rupture(_).
sudden_energy_release : 0.7.
abducible fault_rupture(southwest_northeast).
abducible fault_rupture(east_west).

% :- fault_rupture(southwest_northeast), fault_rupture(east_west).

ev :- eruption.
:- end_lpad.

/** 

?- abd_prob(ev,P,A).
P = 0.588,
A = [fault_rupture(southwest_northeast), fault_rupture(east_west)].

*/
