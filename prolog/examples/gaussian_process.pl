:- use_module(library(mcintyre)).
:- use_module(library(matrix)).
:- use_module(library(clpfd)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

% http://arxiv.org/pdf/1505.02965v2.pdf

gp(X,Kernel,Y):-
  compute_cov(X,Kernel,C),
  gp(C,Y).

gp(Cov,Y):gaussian(Y,Mean,Cov):-
  length(Cov,N),
  list0(N,Mean).

compute_cov(X,Kernel,C):-
  length(X,N),
  cov(X,N,Kernel,CT,CND),
  transpose(CND,CNDT),
  matrix_sum(CT,CNDT,C).

cov([],_,_,[],[]).

cov([XH|XT],N,Ker,[KH|KY],[KHND|KYND]):-
  length(XT,LX),
  N1 is N-LX-1,
  list0(N1,KH0),
  maplist(call(Ker,XH),XT,KH1),
  call(Ker,XH,XH,KXH),
%  kernel(XH,XH,KXH),
  append([KH0,[KXH],KH1],KH),
  append([KH0,[0],KH1],KHND),
  cov(XT,N,Ker,KY,KYND).


sq_exp_p(X,XP,K):-
  sigma(Sigma),
  l(L),
  K is Sigma^2*exp(-((X-XP)^2)/2/(L^2)).


l(L):uniform(L,[1,2,3]).

sigma(Sigma):uniform(Sigma,-2,2).

:- end_lpad.

%kernel(X,X,K):-!,
%  K is 1.27^2+0.3^2.

sq_exp(X,XP,K):-
  K is exp(-((X-XP)^2)/2).
  %K is (1.27^2)*exp(-((X-XP)^2)/2).

min(X,XP,K):-
  K is min(X,XP).

lin(X,XP,K):-
  K is (X*XP+5)^2.

ou(X,XP,K):-
  K is exp(-abs(X-XP)).
draw_fun(Kernel,C):-
%  X=[-1.50,-1.00,-0.75,-0.40,-0.25,0.00],
%  X=[-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1.00,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4,4.5],
  X=[-3,-2,-1,0,1,2,3],
  compute_cov(X,Kernel,K),
  mc_sample_arg_first(gp(K,Y),5,Y,L),
  numlist(1,5,LD),
  maplist(name_s,L,LD,L1),
  C = c3{data:_{x:x, columns:[[x|X]|L1] },
 % legend:_{show: false},
  axis:_{ x:_{ tick:_{fit:false}}}}.

draw_fun_post(Kernel,LSR):-
%  X=[-1.50,-1.00,-0.75,-0.40,-0.25,0.00],
%  X=[-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1.00,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4,4.5],
  numlist(0,10,X),
  XT=[2,4,6,8,9],
  YT=[1,-0.4,-0.8,0.25,0.6],
  mc_lw_sample_arg(gp(X,Kernel,Y),gp(XT,Kernel,YT),10,Y,L),
  keysort(L,LS),
  reverse(LS,LSR).
  /*
  numlist(1,5,LD),
  maplist(name_s,L,LD,L1),
  C = c3{data:_{x:x, columns:[[x|X]|L1] },
 % legend:_{show: false},
  axis:_{ x:_{ tick:_{fit:false}}}}.
*/
name_s(V-_,N,[ND|V]):-
  atomic_concat(f,N,ND).

/** <examples>
?- draw_fun(sq_exp,C).
?- draw_fun(min,C).
?- draw_fun(lin,nC).
*/
