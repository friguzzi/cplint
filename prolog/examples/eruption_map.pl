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
map_query fault_rupture(southwest_northeast) : 0.5.
map_query fault_rupture(east_west): 0.4.

ev :- eruption.

:- end_lpad.

/** <examples> Your example queries go here, e.g.
?- map(ev,P,Exp).
P = 0.588,
Exp = [rule(1, sudden_energy_release, [sudden_energy_release:0.7, '':0.30000000000000004], true)].

*/