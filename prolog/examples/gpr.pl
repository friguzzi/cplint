/*
Gaussian process (GP), see https://en.wikipedia.org/wiki/Gaussian_process
and http://arxiv.org/pdf/1505.02965v2.pdf and
Christopher M. Bishop, 
Pattern Recognition and Machine Learning, Springer, 2006, section 6.4

A Gaussian Process defines a probability distribution over functions. 
This distribution has the property that, given N values,
their image through a function sampled from the GP follows a multivariate 
normal with mean 0 and covariance matrix K.
A GP is defined by a kernel function k that determines K in this way
K_nm=k(x_n,x_m)
GPs can be used for regression: the random functions 
predict the y value corresponding to a x value given a set X and Y of
observed values. It can be proven that y is Gaussian distributed with mean
m(y)=k*C^-1*Y 
where k is the row vector with elements k(X_i, x) and C has elements
C_ij=k(x_i,x_j)+sigma*delta_ij, with sigma a user defined parameter (variance
over the observed values) and delta_ij is the Kronecker function (delta_ij=1
if i=j and 0 otherwise).
When performing GP regression, you choose the kernel and you want to estimate
the parameters of the kernel. You can define a prior distribution over the 
parameters. In this program, you can sample kernels and thus functions and 
predictions and you can computed the expected value of the predictions for 
a squared exponential kernel with l uniformly
distributed in {1,2,3} and sigma uniformly distributed in [-2,2].

*/

/** <examples>
?- draw_fun(sq_exp_p,C).
% draw 5 functions from a GP with a squared exponential kernel with a prior 
% over the parameters sigma and l
?- draw_fun(sq_exp_const_lin(1.0,4.0,0.0,5.00),C).
% draw 5 functions from a GP with a squared exponential, constant, linear 
% kernel (see Bishop page 308 Figure 6.5, bottom right
??- draw_fun(sq_exp,C).
% draw 5 functions from a GP with a squared exponential kernel with 
% parameters sigma=1 and l=1
?- draw_fun(ou,C).
% draw 5 functions from a GP with a Ornstein-Uhlenbeck kernel 
?- draw_fun(lin,C).
% draw 5 functions from a GP with a linear kernel 
?- draw_fun([1,2,3,4,5,6],min,C).
% draw 5 functions from a GP with a min kernel 
?- draw_fun_pred(sq_exp_p,C).
% Given the three points
% XT=[2.5,6.5,8.5]
% YT=[1,-0.8,0.6]
% draws 5 functions predicting points with X=[0,...,10] with a 
% squared exponential kernel. The graphs shows as dots the given points.
?- draw_fun_pred_exp(sq_exp_p,C).
% Given the three points
% XT=[2.5,6.5,8.5]
% YT=[1,-0.8,0.6]
% draws the expected prediction for points with X=[0,...,10] with a
% squared exponential kernel. The graphs shows as dots the given points.
*/

:- use_module(library(mcintyre)).
:- use_module(library(matrix)).
:- use_module(library(clpfd)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.


%! gp(+X:list,+Kernel:atom,-Y:list) is det
% gp, given a list of values X and a kernel name, returns in Y
% the list of values of type f(x) where x belongs to X and f is 
% a function sampled from the Gaussian process.
gp(X,Kernel,Y):-
  compute_cov(X,Kernel,0,C),
  gp(C,Y).

gp(Cov,Y):gaussian(Y,Mean,Cov):-
  length(Cov,N),
  list0(N,Mean).

compute_cov(X,Kernel,Var,C):-
  length(X,N),
  cov(X,N,Kernel,Var,CT,CND),
  transpose(CND,CNDT),
  matrix_sum(CT,CNDT,C).

cov([],_,_,_,[],[]).

cov([XH|XT],N,Ker,Var,[KH|KY],[KHND|KYND]):-
  length(XT,LX),
  N1 is N-LX-1,
  list0(N1,KH0),
  cov_row(XT,XH,Ker,KH1),
  call(Ker,XH,XH,KXH0),
  KXH is KXH0+Var,
  append([KH0,[KXH],KH1],KH),
  append([KH0,[0],KH1],KHND),
  cov(XT,N,Ker,Var,KY,KYND).

cov_row([],_,_,[]).

cov_row([H|T],XH,Ker,[KH|KT]):-
  call(Ker,H,XH,KH),
  cov_row(T,XH,Ker,KT).

%! gp_predict(+XP:list,+Kernel:atom,+XT:list,+YT:list,-YP:list) is det
% Given the points described by the lists XT and YT and a Kernel,
% predict the Y values of points with X values in XP and returns them in YP.
% Prediction is performed by Gaussian process regression.
gp_predict(XP,Kernel,Var,XT,YT,YP):-
  compute_cov(XT,Kernel,Var,C),
  matrix_inversion(C,C_1),
  transpose([YT],YST),
  matrix_multiply(C_1,YST,C_1T),
  gp_predict_single(XP,Kernel,XT,C_1T,YP).

gp_predict_single([],_,_,_,[]).

gp_predict_single([XH|XT],Kernel,X,C_1T,[YH|YT]):-
  compute_k(X,XH,Kernel,K),
  matrix_multiply([K],C_1T,[[YH]]),
  gp_predict_single(XT,Kernel,X,C_1T,YT).

compute_k([],_,_,[]).

compute_k([XH|XT],X,Ker,[HK|TK]):-
  call(Ker,XH,X,HK),
  compute_k(XT,X,Ker,TK).
  

% list of kernels

% squared exponential kernel with a prior on its parameters: l is uniformly
% distributed in {1,2,3} and sigma is uniformly distributed in [-2,2].
sq_exp_p(X,XP,K):-
  sigma(Sigma),
  l(L),
  K is Sigma^2*exp(-((X-XP)^2)/2/(L^2)).


l(L):uniform(L,[1,2,3]).

sigma(Sigma):uniform(Sigma,-2,2).

% squared exponential kernel with fixed parameters: sigma=1, l=1.
sq_exp(X,XP,K):-
  K is exp(-((X-XP)^2)/2).

% squared exponential with linear and constant compoonent, 
% from Bishop, page 307 eq 6.63.
sq_exp_const_lin(Theta0,Theta1,Theta2,Theta3,X,XP,K):-
  K is Theta0*exp(-((X-XP)^2)*Theta1/2)+Theta2+Theta3*X*XP.

% min kernel
min(X,XP,K):-
  K is min(X,XP).

% linear kernel with an additional term for the diagonal to ensure positive
% definedness.
lin(X,X,K):-!,
  K is (X*X)+1.

lin(X,XP,K):-
  K is (X*XP).

% Ornstein-Uhlenbeck kernel
ou(X,XP,K):-
  K is exp(-abs(X-XP)).

:- end_lpad.

%! draw_fun(+Kernel:atom,-C:dict) is det
% draws 5 functions sampled from the Gaussian process with kernel Kernel
% at points X=[-3,-2,-1,0,1,2,3].
draw_fun(Kernel,C):-
  X=[-3,-2,-1,0,1,2,3],
  draw_fun(X,Kernel,C).
  
%! draw_fun(+X:list,+Kernel:atom,-C:dict) is det
% draws 5 functions sampled from the Gaussian process with kernel Kernel
% at points X.
draw_fun(X,Kernel,C):-
  mc_sample_arg_first(gp(X,Kernel,Y),5,Y,L),
  numlist(1,5,LD),
  maplist(name_s,L,LD,L1),
  C = c3{data:_{x:x, columns:[[x|X]|L1] ,type:spline},
  axis:_{ x:_{ tick:_{fit:false}}}}.

%! draw_fun_pred(+Kernel:atom,-C:dict) is det
% Given the three points
% XT=[2.5,6.5,8.5]
% YT=[1,-0.8,0.6]
% draws 5 functions predicting points with X=[0,...,10].
draw_fun_pred(Kernel,C):-
  numlist(0,10,X),
  XT=[2.5,6.5,8.5],
  YT=[1,-0.8,0.6],
  mc_lw_sample_arg(gp_predict(X,Kernel,0.3,XT,YT,Y),gp(XT,Kernel,YT),5,Y,L),
  keysort(L,LS),
  reverse(LS,[Y1-_,Y2-_,Y3-_|_]),
  C = c3{data:_{xs:_{y:xt,f1:x,f2:x,f3:x}, 
  columns:[[y|YT],[xt|XT],[x|X],[f1|Y1],[f2|Y2],[f3|Y3]],
    types:_{f1: spline,f2:spline,f3:spline,y:scatter}},
  axis:_{ x:_{ tick:_{fit:false}}}}.

%! draw_fun_pred_exp(+Kernel:atom,-C:dict) is det
% Given the three points
% XT=[2.5,6.5,8.5]
% YT=[1,-0.8,0.6]
% draws the expected prediction for points with X=[0,...,10].
draw_fun_pred_exp(Kernel,C):-
  numlist(0,10,X),
  XT=[2.5,6.5,8.5],
  YT=[1,-0.8,0.6],
  compute_e(X,Kernel,XT,YT,Y),
  C = c3{data:_{xs:_{y:xt,f:x}, 
  columns:[[y|YT],[xt|XT],[x|X],[f|Y]],
    types:_{f: spline,y:scatter}},
  axis:_{ x:_{ tick:_{fit:false}}}}.

compute_e([],_,_,_,[]).
compute_e([X|T],Kernel,XT,YT,[YE|TYE]):-
  mc_lw_expectation(gp_predict([X],Kernel,0.3,XT,YT,[Y]),gp(XT,Kernel,YT),5,Y,YE),
  compute_e(T,Kernel,XT,YT,TYE).

name_s(V-_,N,[ND|V]):-
  atomic_concat(f,N,ND).


