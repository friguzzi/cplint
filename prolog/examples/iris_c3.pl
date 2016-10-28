/*
EM clustering on the iris dataset
*/
:- use_module(library(clpfd), [transpose/2]).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.


em(It,True,Post):-
  findall([A,B,C,D,Type],data(A,B,C,D,Type),LA),
  pca(LA,True),
  data_mean_var(1,DM1,V1),
  data_mean_var(2,DM2,V2),
  data_mean_var(3,DM3,V3),
  data_mean_var(4,DM4,V4),
  findall(M,(between(1,3,_),gauss(DM1,1.0,M)),[M11,M21,M31]),
  findall(M,(between(1,3,_),gauss(DM2,1.0,M)),[M12,M22,M32]),
  findall(M,(between(1,3,_),gauss(DM3,1.0,M)),[M13,M23,M33]),
  findall(M,(between(1,3,_),gauss(DM4,1.0,M)),[M14,M24,M34]),
  em_it(0,It,LA,par([c1:0.2,c2:0.5,c3:0.3],
    [M11,M12,M13,M14,V1,V2,V3,V4],[M21,M22,M23,M24,V1,V2,V3,V4],
    [M31,M32,M33,M34,V1,V2,V3,V4]),_Par,_,LA1),
  find_most_likely(LA1,LPostCat),
  maplist(replace_cat,LA,LPostCat,LPost),
  pca(LPost,Post).

em_it(It,It,_LA,Par,Par,LAOut,LAOut):-!.

em_it(It0,It,LA,Par0,Par,_,LAOut):-
  expect(LA,Par0,LA1,LL),
  write('LL '),write(LL),nl,
  maxim(LA1,Par1),
  It1 is It0+1,
  em_it(It1,It,LA,Par1,Par,LA1,LAOut).

expect(LA,par([c1:P1,c2:P2,c3:P3],
  G1,G2,G3),[L1,L2,L3],LL):-
  maplist(weight(G1,P1),LA,L01),
  maplist(weight(G2,P2),LA,L02),
  maplist(weight(G3,P3),LA,L03),
  normal(L01,L02,L03,L1,L2,L3),
  log_lik(L01,L02,L3,P1,P2,P3,LL).

maxim([LA1,LA2,LA3],par([c1:P1,c2:P2,c3:P3],C1,C2,C3)):-
  stats(LA1,W1,C1),
  stats(LA2,W2,C2),
  stats(LA3,W3,C3),
  SW is W1+W2+W3,
  P1 is W1/SW,
  P2 is W2/SW,
  P3 is W3/SW.

find_most_likely([L1,L2,L3],LC):-
  maplist(classify,L1,L2,L3,LC).

classify(_-W1,_-W2,_-W3,Cat):-
  find_max([W1,W2,W3],Cat).

normal(L01,L02,L03,L1,L2,L3):-
  maplist(px,L01,L02,L03,L1,L2,L3).

px(X-W01,X-W02,X-W03,X-W1,X-W2,X-W3):-
  S is W01+W02+W03,
  W1 is W01/S,
  W2 is W02/S,
  W3 is W03/S.

weight([M1,M2,M3,M4,V1,V2,V3,V4],P,[A,B,C,D,_],[A,B,C,D]-W):-
  gauss_density_0(M1,V1,A,W1),
  gauss_density_0(M2,V2,B,W2),
  gauss_density_0(M3,V3,C,W3),
  gauss_density_0(M4,V4,D,W4),
  W is W1*W2*W3*W4*P.

log_lik(L1,L2,L3,P1,P2,P3,LL):-
  foldl(combine(P1,P2,P3),L1,L2,L3,0,LL).

combine(P1,P2,P3,_-W1,_-W2,_-W3,LL0,LL):-
  LLs is log(P1*W1+P2*W2+P3*W3),
  LL is LL0+LLs.



find_max(Counts,MaxC):-
  max_list(Counts,MV),
  nth1(Max,Counts,MV),
   concat_atom(['c',Max],MaxC).

replace_cat([A,B,C,D,_],Cat,[A,B,C,D,Cat]).


pcac(LA,P):-
  length(LA,NP),
  maplist(add_cat,LA,LCat,L),
  L=[H|_],
  length(H,Comp),
  list_to_set(LCat,Cats),
  append(L,LLin),
  D =..[c|LLin],
  data<- matrix(D,ncol=Comp,byrow='TRUE'),
  pc<- prcomp(data),
  Data0<-pc["x"],
  Data0=[Data1],
  foldl(getn(NP),Data2,Data1,[]),
  transpose(Data2,Data),
  maplist(add_cat,DC,LCat,Data),
  maplist(separate(DC),Cats,Mat1),
  maplist(keep2,Mat1,Mat2),
  maplist(axis,Cats,Axis,LAxis),
  dict_create(Ax,_,LAxis),
  maplist(tocol,Mat2,Axis,ColData),
  append(ColData,Cols),
  P = c3{data:_{
    xs:Ax, columns:Cols,type:scatter},
    axis:_{x:_{ tick:_{fit: false}}}
    }.

:- <- library("ggplot2").



pca(LA,_P):-
  length(LA,NP),
  maplist(add_cat,LA,LCat,L),
  L=[H|_],
  length(H,Comp),
  append(L,LLin),
  D =..[c|LLin],
  data<- matrix(D,ncol=Comp,byrow='TRUE'),
  pc<- prcomp(data),
  Data0<-pc["x"],
  Data0=[Data1],
  foldl(getn(NP),Data2,Data1,[]),
  transpose(Data2,Data),
  maplist(getx,Data,X),
  maplist(gety,Data,Y),
  x<- X,
  y<-Y,
  class<-LCat,
  <-qplot(x, y, colour=class),
  r_download. 

getn(N,LN,L,Rest):-
    length(LN,N),
    append(LN,Rest,L).

tocol(Data,[X,Y],[[X|DX],[Y|DY]]):-
  maplist(xy,Data,DX,DY).

getx([X,_,_,_],X).

gety([_,Y,_,_],Y).

ab([X,Y,_,_,_],[X,Y]).

xy([X,Y],X,Y).

add_cat([X,Y,Z,W,C],C,[X,Y,Z,W]).

cat(Cat,[_,_,_,_,Cat]).

group([A,B],[C,D],[E,F],g(A,B,C,D,E,F)).

axis(N,[NX,N],NA-NX):-
  (number(N)->
    atom_number(NA,N)
  ;
    NA=N
  ),
  atom_concat(x,NA,NX).

keep2(Quad,Cou):-
  maplist(ab,Quad,Cou).


separate(DC,Cat,DataClass):-
  include(cat(Cat),DC,DataClass).


assert_data_means:-
  findall([A,B,C,D],data(A,B,C,D,_Type),LA),
  maplist(component,LA,CA,CB,CC,CD),
  mean(CA,M1),
  mean(CB,M2),
  mean(CC,M3),
  mean(CD,M4),
  variance(CA,M1,V1),
  variance(CB,M2,V2),
  variance(CC,M3,V3),
  variance(CD,M4,V4),
  assert(data_mean_var(1,M1,V1)),
  assert(data_mean_var(2,M2,V2)),
  assert(data_mean_var(3,M3,V3)),
  assert(data_mean_var(4,M4,V4)).

mean(L,M):-
  length(L,N),
  sum_list(L,S),
  M is S/N.

variance(L,M,Var):-
  length(L,N),
  foldl(agg_var(M),L,0,S),
  Var is S/N.

stats(LA,SW,[M1,M2,M3,M4,V1,V2,V3,V4]):-
  maplist(component_weight,LA,CA,CB,CC,CD),
  weighted_mean(CA,M1,SW),
  weighted_mean(CB,M2,_),
  weighted_mean(CC,M3,_),
  weighted_mean(CD,M4,_),
  weighted_var(CA,M1,V1),
  weighted_var(CB,M2,V2),
  weighted_var(CC,M3,V3),
  weighted_var(CD,M4,V4).
 
weighted_var(L,M,Var):-
  foldl(agg_val_var(M),L,(0,0),(S,SW0)),
  SW is SW0,
  (SW=:=0.0->
    write(zero_var),nl,
    Var=1.0
  ;
    Var is S/SW
  ).

weighted_mean(L,M,SW):-
  foldl(agg_val,L,(0,0),(S,SW0)),
  SW is SW0,
  (SW =:=0.0->
    write(zero_mean),nl,
    M is 0
  ;
    M is S/SW
  ).

agg_val(V -N,(S,SW),(S+V*N,SW+N)).
agg_val_var(M,V -N,(S,SW),(S+(M-V)^2*N,SW+N)).
agg_var(M,V,S,S+(M-V)^2).


component([A,B,C,D],A,B,C,D).
component_weight([A,B,C,D]-W,A-W,B-W,C-W,D-W).

gauss_density_0(M,V,X,W):-
  (V=:=0.0->
   write(zero_var_gauss),
    W=0.0
  ;
    gauss_density(M,V,X,W)
  ).

gauss_density(Mean,Variance,S,D):-
  StdDev is sqrt(Variance),
  D is 1/(StdDev*sqrt(2*pi))*exp(-(S-Mean)*(S-Mean)/(2*Variance)).

gauss(Mean,Variance,S):-
  number(Mean),!,
  random(U1),
  random(U2),
  R is sqrt(-2*log(U1)),
  Theta is 2*pi*U2,
  S0 is R*cos(Theta),
  StdDev is sqrt(Variance),
  S is StdDev*S0+Mean.
/** <examples>
?- findall([A,B,C,D,Type],data(A,B,C,D,Type),LA),
  pca(LA,True).
?- em(10,T,P).


*/
% Iris dataset
data(5.1,3.5,1.4,0.2,'Iris-setosa').
data(4.9,3.0,1.4,0.2,'Iris-setosa').
data(4.7,3.2,1.3,0.2,'Iris-setosa').
data(4.6,3.1,1.5,0.2,'Iris-setosa').
data(5.0,3.6,1.4,0.2,'Iris-setosa').
data(5.4,3.9,1.7,0.4,'Iris-setosa').
data(4.6,3.4,1.4,0.3,'Iris-setosa').
data(5.0,3.4,1.5,0.2,'Iris-setosa').
data(4.4,2.9,1.4,0.2,'Iris-setosa').
data(4.9,3.1,1.5,0.1,'Iris-setosa').
data(5.4,3.7,1.5,0.2,'Iris-setosa').
data(4.8,3.4,1.6,0.2,'Iris-setosa').
data(4.8,3.0,1.4,0.1,'Iris-setosa').
data(4.3,3.0,1.1,0.1,'Iris-setosa').
data(5.8,4.0,1.2,0.2,'Iris-setosa').
data(5.7,4.4,1.5,0.4,'Iris-setosa').
data(5.4,3.9,1.3,0.4,'Iris-setosa').
data(5.1,3.5,1.4,0.3,'Iris-setosa').
data(5.7,3.8,1.7,0.3,'Iris-setosa').
data(5.1,3.8,1.5,0.3,'Iris-setosa').
data(5.4,3.4,1.7,0.2,'Iris-setosa').
data(5.1,3.7,1.5,0.4,'Iris-setosa').
data(4.6,3.6,1.0,0.2,'Iris-setosa').
data(5.1,3.3,1.7,0.5,'Iris-setosa').
data(4.8,3.4,1.9,0.2,'Iris-setosa').
data(5.0,3.0,1.6,0.2,'Iris-setosa').
data(5.0,3.4,1.6,0.4,'Iris-setosa').
data(5.2,3.5,1.5,0.2,'Iris-setosa').
data(5.2,3.4,1.4,0.2,'Iris-setosa').
data(4.7,3.2,1.6,0.2,'Iris-setosa').
data(4.8,3.1,1.6,0.2,'Iris-setosa').
data(5.4,3.4,1.5,0.4,'Iris-setosa').
data(5.2,4.1,1.5,0.1,'Iris-setosa').
data(5.5,4.2,1.4,0.2,'Iris-setosa').
data(4.9,3.1,1.5,0.1,'Iris-setosa').
data(5.0,3.2,1.2,0.2,'Iris-setosa').
data(5.5,3.5,1.3,0.2,'Iris-setosa').
data(4.9,3.1,1.5,0.1,'Iris-setosa').
data(4.4,3.0,1.3,0.2,'Iris-setosa').
data(5.1,3.4,1.5,0.2,'Iris-setosa').
data(5.0,3.5,1.3,0.3,'Iris-setosa').
data(4.5,2.3,1.3,0.3,'Iris-setosa').
data(4.4,3.2,1.3,0.2,'Iris-setosa').
data(5.0,3.5,1.6,0.6,'Iris-setosa').
data(5.1,3.8,1.9,0.4,'Iris-setosa').
data(4.8,3.0,1.4,0.3,'Iris-setosa').
data(5.1,3.8,1.6,0.2,'Iris-setosa').
data(4.6,3.2,1.4,0.2,'Iris-setosa').
data(5.3,3.7,1.5,0.2,'Iris-setosa').
data(5.0,3.3,1.4,0.2,'Iris-setosa').
data(7.0,3.2,4.7,1.4,'Iris-versicolor').
data(6.4,3.2,4.5,1.5,'Iris-versicolor').
data(6.9,3.1,4.9,1.5,'Iris-versicolor').
data(5.5,2.3,4.0,1.3,'Iris-versicolor').
data(6.5,2.8,4.6,1.5,'Iris-versicolor').
data(5.7,2.8,4.5,1.3,'Iris-versicolor').
data(6.3,3.3,4.7,1.6,'Iris-versicolor').
data(4.9,2.4,3.3,1.0,'Iris-versicolor').
data(6.6,2.9,4.6,1.3,'Iris-versicolor').
data(5.2,2.7,3.9,1.4,'Iris-versicolor').
data(5.0,2.0,3.5,1.0,'Iris-versicolor').
data(5.9,3.0,4.2,1.5,'Iris-versicolor').
data(6.0,2.2,4.0,1.0,'Iris-versicolor').
data(6.1,2.9,4.7,1.4,'Iris-versicolor').
data(5.6,2.9,3.6,1.3,'Iris-versicolor').
data(6.7,3.1,4.4,1.4,'Iris-versicolor').
data(5.6,3.0,4.5,1.5,'Iris-versicolor').
data(5.8,2.7,4.1,1.0,'Iris-versicolor').
data(6.2,2.2,4.5,1.5,'Iris-versicolor').
data(5.6,2.5,3.9,1.1,'Iris-versicolor').
data(5.9,3.2,4.8,1.8,'Iris-versicolor').
data(6.1,2.8,4.0,1.3,'Iris-versicolor').
data(6.3,2.5,4.9,1.5,'Iris-versicolor').
data(6.1,2.8,4.7,1.2,'Iris-versicolor').
data(6.4,2.9,4.3,1.3,'Iris-versicolor').
data(6.6,3.0,4.4,1.4,'Iris-versicolor').
data(6.8,2.8,4.8,1.4,'Iris-versicolor').
data(6.7,3.0,5.0,1.7,'Iris-versicolor').
data(6.0,2.9,4.5,1.5,'Iris-versicolor').
data(5.7,2.6,3.5,1.0,'Iris-versicolor').
data(5.5,2.4,3.8,1.1,'Iris-versicolor').
data(5.5,2.4,3.7,1.0,'Iris-versicolor').
data(5.8,2.7,3.9,1.2,'Iris-versicolor').
data(6.0,2.7,5.1,1.6,'Iris-versicolor').
data(5.4,3.0,4.5,1.5,'Iris-versicolor').
data(6.0,3.4,4.5,1.6,'Iris-versicolor').
data(6.7,3.1,4.7,1.5,'Iris-versicolor').
data(6.3,2.3,4.4,1.3,'Iris-versicolor').
data(5.6,3.0,4.1,1.3,'Iris-versicolor').
data(5.5,2.5,4.0,1.3,'Iris-versicolor').
data(5.5,2.6,4.4,1.2,'Iris-versicolor').
data(6.1,3.0,4.6,1.4,'Iris-versicolor').
data(5.8,2.6,4.0,1.2,'Iris-versicolor').
data(5.0,2.3,3.3,1.0,'Iris-versicolor').
data(5.6,2.7,4.2,1.3,'Iris-versicolor').
data(5.7,3.0,4.2,1.2,'Iris-versicolor').
data(5.7,2.9,4.2,1.3,'Iris-versicolor').
data(6.2,2.9,4.3,1.3,'Iris-versicolor').
data(5.1,2.5,3.0,1.1,'Iris-versicolor').
data(5.7,2.8,4.1,1.3,'Iris-versicolor').
data(6.3,3.3,6.0,2.5,'Iris-virginica').
data(5.8,2.7,5.1,1.9,'Iris-virginica').
data(7.1,3.0,5.9,2.1,'Iris-virginica').
data(6.3,2.9,5.6,1.8,'Iris-virginica').
data(6.5,3.0,5.8,2.2,'Iris-virginica').
data(7.6,3.0,6.6,2.1,'Iris-virginica').
data(4.9,2.5,4.5,1.7,'Iris-virginica').
data(7.3,2.9,6.3,1.8,'Iris-virginica').
data(6.7,2.5,5.8,1.8,'Iris-virginica').
data(7.2,3.6,6.1,2.5,'Iris-virginica').
data(6.5,3.2,5.1,2.0,'Iris-virginica').
data(6.4,2.7,5.3,1.9,'Iris-virginica').
data(6.8,3.0,5.5,2.1,'Iris-virginica').
data(5.7,2.5,5.0,2.0,'Iris-virginica').
data(5.8,2.8,5.1,2.4,'Iris-virginica').
data(6.4,3.2,5.3,2.3,'Iris-virginica').
data(6.5,3.0,5.5,1.8,'Iris-virginica').
data(7.7,3.8,6.7,2.2,'Iris-virginica').
data(7.7,2.6,6.9,2.3,'Iris-virginica').
data(6.0,2.2,5.0,1.5,'Iris-virginica').
data(6.9,3.2,5.7,2.3,'Iris-virginica').
data(5.6,2.8,4.9,2.0,'Iris-virginica').
data(7.7,2.8,6.7,2.0,'Iris-virginica').
data(6.3,2.7,4.9,1.8,'Iris-virginica').
data(6.7,3.3,5.7,2.1,'Iris-virginica').
data(7.2,3.2,6.0,1.8,'Iris-virginica').
data(6.2,2.8,4.8,1.8,'Iris-virginica').
data(6.1,3.0,4.9,1.8,'Iris-virginica').
data(6.4,2.8,5.6,2.1,'Iris-virginica').
data(7.2,3.0,5.8,1.6,'Iris-virginica').
data(7.4,2.8,6.1,1.9,'Iris-virginica').
data(7.9,3.8,6.4,2.0,'Iris-virginica').
data(6.4,2.8,5.6,2.2,'Iris-virginica').
data(6.3,2.8,5.1,1.5,'Iris-virginica').
data(6.1,2.6,5.6,1.4,'Iris-virginica').
data(7.7,3.0,6.1,2.3,'Iris-virginica').
data(6.3,3.4,5.6,2.4,'Iris-virginica').
data(6.4,3.1,5.5,1.8,'Iris-virginica').
data(6.0,3.0,4.8,1.8,'Iris-virginica').
data(6.9,3.1,5.4,2.1,'Iris-virginica').
data(6.7,3.1,5.6,2.4,'Iris-virginica').
data(6.9,3.1,5.1,2.3,'Iris-virginica').
data(5.8,2.7,5.1,1.9,'Iris-virginica').
data(6.8,3.2,5.9,2.3,'Iris-virginica').
data(6.7,3.3,5.7,2.5,'Iris-virginica').
data(6.7,3.0,5.2,2.3,'Iris-virginica').
data(6.3,2.5,5.0,1.9,'Iris-virginica').
data(6.5,3.0,5.2,2.0,'Iris-virginica').
data(6.2,3.4,5.4,2.3,'Iris-virginica').
data(5.9,3.0,5.1,1.8,'Iris-virginica').

:- assert_data_means.
 