:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

mvg(G):gaussian(G,[0,0],[[1,0.6],[0.6,2]]).

mvg1(_,G):gaussian(G,[M1,M2],[[1,0.6],[0.6,2]]):-
  mean(1,M1),
  mean(2,M2).

mean(_,M):gaussian(M,0,1).

mvg2(_,G):gaussian(G,[0,0],[[1,0.6],[0.6,2]]).
mvg3(G):gaussian(G,[0,0,0],[[2,-1,0],[-1,2,-1],[0,-1,2]]).
:- end_lpad.

chart(Chart):-
  mc_sample_arg_first(mvg(G),1000,G,V),
  maplist(val,V,ValList),
  Chart = c3{data:_{x:x, rows:[x-y|ValList], type:scatter},
  legend:_{show: false},
  axis:_{ x:_{ tick:_{fit:false}}}}.

val([X,Y]-_,X-Y).

obs(L):-
  mc_lw_sample_arg(mvg1(1,G),mvg1(1,[1,1]),2,G,L).

gv3(L):-
  mc_sample_arg_first(mvg3(G),10,G,L).

/** <examples>
?- chart(G).
?- obs(L).
?- gv3(L).
*/
