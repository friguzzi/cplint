% Empty with pita loaded
:- use_module(library(pita)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.

:- begin_lpad.

map_query eruption : 0.6; earthquake : 0.3 :- 
    sudden_energy_release, fault_rupture(_).
map_query sudden_energy_release : 0.7.
map_query fault_rupture(southwest_northeast) : 0.6.
map_query fault_rupture(east_west) : 0.55.

:- end_lpad.

/** <examples> Your example queries go here, e.g.
?- map(eruption,P,MPE).
MPE = [rule(1,sudden_energy_release,[sudden_energy_release:0.7,'':0.30000000000000004],true),
rule(2,fault_rupture(southwest_northeast),[fault_rupture(southwest_northeast):0.6,'':0.4],true),
rule(3,fault_rupture(east_west),[fault_rupture(east_west):0.55,'':0.44999999999999996],true),
rule(0,eruption,[eruption:0.6,earthquake:0.3,'':0.10000000000000003],(sudden_energy_release,fault_rupture(southwest_northeast))),
rule(0,eruption,[eruption:0.6,earthquake:0.3,'':0.10000000000000003],(sudden_energy_release,fault_rupture(east_west)))]
P = 0.08316,
*/