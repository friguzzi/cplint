% Empty with pita loaded
:- use_module(library(pita)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.

:- begin_lpad.

eruption : 0.6; earthquake : 0.3 :- 
    sudden_energy_release, fault_rupture(_).
sudden_energy_release : 0.7.
fault_rupture(southwest_northeast) : 0.6.
fault_rupture(east_west) : 0.55.

ev :- eruption.

:- end_lpad.

/** <examples> Your example queries go here, e.g.
?- mpe_bdd_dot_string(ev,G,LV,P,MAP).
?- mpe(ev,P,MPE).
MPE = [rule(1, sudden_energy_release, [sudden_energy_release:0.7, '' : 0.30000000000000004], []), 
rule(2, fault_rupture(southwest_northeast), [fault_rupture(southwest_northeast):0.6, '' : 0.4], []), 
rule(3, fault_rupture(east_west), [fault_rupture(east_west):0.55, '' : 0.44999999999999996], []), 
rule(0, eruption, [eruption:0.6, earthquake:0.3, '' : 0.10000000000000003], [sudden_energy_release, fault_rupture(southwest_northeast)])],
P = 0.1386
it is not the viterbi explanation
*/