/*
One-dimensional  Kalman filter. Hidden Markov model with a real
value as state and a real value as output. The next state is given by
the current state plus Gaussian noise (mean 0 and variance 2 in this example)
and the output is given by the current state plus Gaussian noise (mean
0 and variance 1 in this example). 
This example can be considered as modeling a random walk of a single continuous 
state variable with noisy observations. 
Given that at time 0 the value 2.5 was
observed, what is the distribution of the state at time 1 (filtering problem)?
The distribution of the state is plotted in the case of having (posterior) or 
not having the observation (prior).
Liklihood weighing is used to condition the distribution on evidence on
a continuous random variable (evidence with probability 0).
CLP(R) constraints allow both sampling and weighing samples with the same
program.
Filtering can be used to estimate the sequence of state variables
given a sequence of observations. Either likelihood weighting or particle 
filtering can be used for this purpose.
From
Islam, Muhammad Asiful, C. R. Ramakrishnan, and I. V. Ramakrishnan. 
"Inference in probabilistic logic programs with continuous random variables." 
Theory and Practice of Logic Programming 12.4-5 (2012): 505-523.
http://arxiv.org/pdf/1112.2681v3.pdf
Russell, S. and Norvig, P. 2010. Arficial Intelligence: A Modern Approach. 
Third Edition, Prentice Hall, Figure 15.10 page 587

*/

/** <examples>
?- filter_sampled_par(100,C).
?- filter_par(100,C).
?- filter_sampled(1000,C).
?- filter(1000,C).
?- dens_par(1000,40,G).
?- dens_lw(1000,40,G).
% plot the density of the state at time 1 in case of no observation (prior)
% and in case of observing 2.5 by taking 1000 samples and dividing the domain
% in 40 bins
?- hist(1000,40,G).
% plot the density of the state at time 1 in case of no observation
% by taking 1000 samples and dividing the domain
% in 40 bins

*/
:- use_module(library(mcintyre)).
:- use_module(library(clpr)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

kf_fin(N, T) :-
  kf_fin(N,_O,T).

kf_fin(N,O, T) :-
  init(S),
  kf_part(0, N, S,O,_LS,T).


kf(N,O,LS) :-
  init(S),
  kf_part(0, N, S,O,LS,_T).

kf_o(N,ON):-
  init(S),
  N1 is N-1,
  kf_part(0,N1,S,_O,_LS,T),
  emit(T,N,ON).

kf_part(I, N, S, [V|RO], [S|LS], T) :-
  I < N, 
  NextI is I+1,
  trans(S,I,NextS),
  emit(NextS,I,V),
  kf_part(NextI, N, NextS, RO, LS, T).

kf_part(N, N, S, [], [], S).

trans(S,I,NextS) :-
  {NextS =:= E + S},
  trans_err(I,E).

emit(NextS,I,V) :-
  {V =:= NextS +X},
  obs_err(I,X).

init(S):gaussian(S,0,1).
% prior as in Russel and Norvig 2010, Fig 15.10
trans_err(_,E):gaussian(E,0,2).
% transition noise as in Russel and Norvig 2010, Fig 15.10
obs_err(_,E):gaussian(E,0,1).
% observation noise as in Russel and Norvig 2010, Fig 15.10

:- end_lpad.

%! hist(+S:int,+Bins:int,-C:dict) is det
% Plots a histogram of the density of the state at time 1 in case of 
% no observation
hist(Samples,NBins,Chart):-
  mc_sample_arg(kf_fin(1,_O1,Y),Samples,Y,L0),
  histogram(L0,Chart,[nbins(NBins)]).

%! dens_lw(+S:int,+Bins:int,-C:dict) is det
% Plots the density of the state at time 1 in case of no observation (prior)
% and in case of observing 2.5.
% Observation as in Russel and Norvig 2010, Fig 15.10
dens_lw(Samples,NBins,Chart):-
  mc_sample_arg(kf_fin(1,_O1,Y),Samples,Y,L0),
  mc_lw_sample_arg(kf_fin(1,_O2,T),kf_fin(1,[2.5],_T),Samples,T,L),
  densities(L0,L,Chart,[nbins(NBins)]).

dens_par(Samples,NBins,Chart):-
  mc_sample_arg(kf_fin(1,_O1,Y),Samples,Y,L0),
  mc_particle_sample_arg(kf_fin(1,_O2,T),[kf_fin(1,[2.5],_T)],Samples,T,L),
  densities(L0,L,Chart,[nbins(NBins)]).


%! filter_par(+S:int,-C:dict) is det
% Draws a sample trajectory for 4 time points and performs particle filtering
filter_par(Samples,C):-
  sample_trajectory(4,O,St),
  filter_par(Samples,O,St,C).

%! filter_sampled_par(+S:int,-C:dict) is det
% Considers a sampled trajectory for 4 time points and performs particle filtering
filter_sampled_par(Samples,C):-
  o(O),
  st(St),
  filter_par(Samples,O,St,C).

%! filter_par(+S:int,+O:list,+St:list,-C:dict) is det
% Takes observations O and true states St for 4 time points 
% and performs filtering on the trajectory: given O, computes the
% distribution of the state for each time point.
% It uses particle filtering with S particles.
% Returns a graph C with the distributions of the state variable 
% at time 1, 2, 3 and 4 
% (S1, S2, S3, S4, density on the left y axis) 
% and with O and St (time on the right y axis).
filter_par(Samples,O,St,C):-
  O=[O1,O2,O3,O4],
  NBins=20,
  mc_particle_sample_arg([kf_fin(1,T1),kf_fin(2,T2),kf_fin(3,T3),kf_fin(4,T4)],
  [kf_o(1,O1),kf_o(2,O2),kf_o(3,O3),kf_o(4,O4)],Samples,[T1,T2,T3,T4],[F1,F2,F3,F4]),
  density(F1,C1,[nbins(NBins)]),
  density(F2,C2,[nbins(NBins)]),
  density(F3,C3,[nbins(NBins)]),
  density(F4,C4,[nbins(NBins)]),
  [[x|X1],[dens|S1]]=C1.data.columns,
  [[x|X2],[dens|S2]]=C2.data.columns,
  [[x|X3],[dens|S3]]=C3.data.columns,
  [[x|X4],[dens|S4]]=C4.data.columns,
  Y=[1,2,3,4],
  C = c3{data:_{xs:_{'True State':xt,'Obs':xo,'S1':x1,'S2':x2,'S3':x3,'S4':x4},
  columns:[[xt|St],['True State'|Y],
    [xo|O],['Obs'|Y],
    [x1|X1],['S1'|S1],
    [x2|X2],['S2'|S2],
    [x3|X3],['S3'|S3],
    [x4|X4],['S4'|S4]],
    types:_{'S1': spline,'S2': spline,'S3': spline,'S4': spline,'True State':scatter,'Obs':scatter},
    axes:_{'S1':y,'S2':y,'S3':y,'S4':y,'True State':y2,'Obs':y2}},
    axis:_{ x:_{ tick:_{fit:false}},
      y2:_{
            show: 'true',
                label: 'Time',
                min: -6
       },
       y:_{label:'Density'}}
  }.

%! filter(+S:int,-C:dict) is det
% Draws a sample trajectory for 4 time points and performs filtering with
% likelihood weighting
filter(Samples,C):-
  sample_trajectory(4,O,St),
  filter(Samples,O,St,C).

%! filter_sampled(+S:int,-C:dict) is det
% Considers a sampled trajectory for 4 time points and performs filtering
% with likelihood weighting
filter_sampled(Samples,C):-
  o(O),
  st(St),
  filter(Samples,O,St,C).

o([-0.13382010096024688, -1.1832019975321675, -3.2127809027386567, -4.586259511038596]).
st([-0.18721387460211258, -2.187978176930458, -1.5472275345566668, -2.9840114021132713]).

%! filter(+S:int,+O:list,+St:list,-C:dict) is det
% Takes observations O and true states St for 4 time points 
% and performs filtering on the trajectory: given O, computes the
% distribution of the state for each time point by taking S samples
% using likelihood weighting.
% Returns a graph C with the distributions of the state variable 
% at time 1, 2, 3 and 4 
% (S1, S2, S3, S4, density on the left y axis) 
% and with O and St (time on the right y axis).
filter(Samples,O,St,C):-
  mc_lw_sample_arg(kf(4,_O,T),kf_fin(4,O,_T),Samples,T,L),
  maplist(separate,L,T1,T2,T3,T4),
  NBins=20,
  density(T1,C1,[nbins(NBins)]),
  density(T2,C2,[nbins(NBins)]),
  density(T3,C3,[nbins(NBins)]),
  density(T4,C4,[nbins(NBins)]),
  [[x|X1],[dens|S1]]=C1.data.columns,
  [[x|X2],[dens|S2]]=C2.data.columns,
  [[x|X3],[dens|S3]]=C3.data.columns,
  [[x|X4],[dens|S4]]=C4.data.columns,
  Y=[1,2,3,4],
  C = c3{data:_{xs:_{'True State':xt,'Obs':xo,'S1':x1,'S2':x2,'S3':x3,'S4':x4},
  columns:[[xt|St],['True State'|Y],
    [xo|O],['Obs'|Y],
    [x1|X1],['S1'|S1],
    [x2|X2],['S2'|S2],
    [x3|X3],['S3'|S3],
    [x4|X4],['S4'|S4]],
    types:_{'S1': spline,'S2': spline,'S3': spline,'S4': spline,'True State':scatter,'Obs':scatter},
    axes:_{'S1':y,'S2':y,'S3':y,'S4':y,'True State':y2,'Obs':y2}},
 % legend:_{show: false},
    axis:_{ x:_{ tick:_{fit:false}},
      y2:_{
            show: 'true',
                label: 'Time',
                min: -6
       },
       y:_{label:'Density'}}
  }.

separate([S1,S2,S3,S4]-W,S1-W,S2-W,S3-W,S4-W).

sample_trajectory(N,Ob,St):-
  mc_sample_arg(kf(N,O,T),1,(O,T),L),
  L=[[(Ob,St)]-_].

