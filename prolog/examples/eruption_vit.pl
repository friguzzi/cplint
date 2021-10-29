/*
This program is inspired by the morphological characteristics of the Stromboli
Italian island.
The Stromboli island is located at the intersection of two geological faults, 
one in the southwest-northeast direction, the other in the east-west direction,
and contains one of the three volcanoes that are active in Italy. 
This program models the possibility that an eruption or an earthquake occurs at Stromboli.

From
Elena Bellodi and Fabrizio Riguzzi. Structure learning of probabilistic logic 
programs by searching the clause space. Theory and Practice of Logic 
Programming, FirstView Articles, 2014
*/
:- use_module(library(viterbi)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- viterbi.

:- begin_lpad.

eruption : 0.6 ; earthquake : 0.3 :- 
  sudden_energy_release,
  fault_rupture(_).
% If there is a sudden energy release under the island and there is a fault 
% rupture, then there can be an eruption of the volcano on the island with 
% probability 0.6 or an earthquake in the area with probability 0.3

sudden_energy_release : 0.7.
% The energy release occurs with probability 0.7 

fault_rupture(southwest_northeast) : 0.6.
fault_rupture(east_west) : 0.55.

:- end_lpad.

/** <examples>

?- viterbi(eruption,P,E).
P = 0.252,
E = [rule(0, eruption, [eruption:0.6, earthquake:0.3, '':0.10000000000000003], [sudden_energy_release, fault_rupture(southwest_northeast)]), 
rule(1, sudden_energy_release, [sudden_energy_release:0.7, '':0.30000000000000004], []), 
rule(2, fault_rupture(southwest_northeast), [fault_rupture(southwest_northeast):0.6, '':0.4], [])].

*/
