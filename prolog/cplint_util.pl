

:- module(cplint_util,[
  bar/2,
  bar/3,
  bar1/2,
  argbar/2,
  histogram/2,
  histogram/3,
  densities/3,
  densities/4,
  density/2,
  density/3,
  density2d/2,
  density2d/3,
  to_pair/2,
  key/2,
  value/2,
  bin/5,
  beta/2,
  to_atom/2,
  average/2,
  variance/2,
  variance/3,
  std_dev/2,
  std_dev/3,
  agg_val/3,
  swi_builtin/1]).


:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(clp/clpr)).
:- use_module(library(matrix)).
:- use_module(library(clpfd)).

:- use_module(library(matrix)).
:- use_module(highlight).
/** <module> cplint_util

Utility module for cplint

@author Fabrizio Riguzzi
@license Artistic License 2.0 https://opensource.org/licenses/Artistic-2.0
@copyright Fabrizio Riguzzi
*/
/**
 * bar(+Probability:float,-Chart:dict) is det
 *
 * The predicate returns a dict for rendering with c3 as a bar chart with
 * a bar for the probability and a bar for one minus the probability.
 */
bar(P,Chart):-
  PF is 1.0-P,
  Chart = c3{data:_{x:elem, rows:[elem-prob,'T'-P,'F' -PF], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,max:1.0,padding:_{bottom:0.0,top:0.0},
             tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}},
	           size:_{height: 100},
	          legend:_{show: false}}.

/**
 * bar1(+Probability:float,-Chart:dict) is det
 *
 * The predicate returns a dict for rendering with c3 as a bar chart with
 * a bar for the probability
 */
bar1(P,Chart):-
  Chart = c3{data:_{x:elem, rows:[elem-prob,'T'-P], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,max:1.0,padding:_{bottom:0.0,top:0.0},
             tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}},
	           size:_{height: 100},
	          legend:_{show: false}}.
/**
 * bar(+Successes:int,+Failures:int,-Chart:dict) is det
 *
 * The predicate returns a dict for rendering with c3 as a bar chart with
 * a bar for the number of successes and a bar for the number of failures
 */
bar(S,F,Chart):-
  Chart = c3{data:_{x:elem, rows:[elem-prob,'T'-S,'F' -F], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,padding:_{bottom:0.0}}},
	           size:_{height: 100},
	          legend:_{show: false}}.
/**
 * argbar(+Values:list,-Chart:dict) is det
 *
 * Values is a list of couples V-N where
 * V is the value and N is the number of samples
 * returning that value.
 * The predicate returns a dict for rendering with c3 as a bar chart with
 * a bar each value V.
 * The size of the bar is given by N.
 */
argbar(ValList,Chart):-
        maplist(to_atom,ValList,ValList1),
        Chart = c3{data:_{x:elem, rows:[elem-prob|ValList1], type:bar},
            axis:_{x:_{type:category}, rotated: true,
                   y:_{min:0.0,padding:_{bottom:0.0}}},
             %  size:_{height: 100},
              legend:_{show: false}}.


/**
 * to_atom(+In:pair,-Out:pair) is det
 *
 * Given In=A0-N, to_atom/2 returns Out=A-N
 * where A is an atom representing A0
 */
to_atom(A0-N,A-N):-
  term_to_atom(A0,A).

/**
 * histogram(+List:list,-Chart:dict) is det
 *
 * Equivalent to histogram/3 with an empty option list.
 */
histogram(L0,Chart):-
  histogram(L0,Chart,[]).

/**
 * histogram(+List:list,-Chart:dict,+Options:list) is det
 *
 * Draws a histogram of the samples in List. List must be a list of couples of the form [V]-W or V-W
 * where V is a sampled value and W is its weight.
 *
 * Options is a list of options, the following are recognised by histogram/3:
 * * min(+Min:float)
 *   the minimum value of domain, default value the minimum in List
 * * max(+Max:float)
 *   the maximum value of domain, default value the maximum in List
 * * nbins(+NBins:int)
 *   the number of bins for dividing the domain, default value 40
 */
histogram(L0,Chart,Options):-
  maplist(to_pair,L0,L1),
  maplist(key,L1,L2),
  max_list(L2,DMax0),
  min_list(L2,DMin),
  DMax is DMax0+(DMax0-DMin)/12,
  option(max(Max),Options,DMax),
  option(min(Min),Options,DMin),
  option(nbins(NBins),Options,40),
  histogram(L0,NBins,Min,Max,Chart).

/**
 * histogram(+List:list,+NBins:int,+Min:float,+Max:float,-Chart:dict) is det
 *
 * Draws a histogram of the samples in List dividing the domain in
 * NBins bins. List must be a list of couples of the form [V]-W or V-W
 * where V is a sampled value and W is its weight. The minimum and maximum
 * values of the domains must be provided.
 */
histogram(L0,NBins,Min,Max,Chart):-
  maplist(to_pair,L0,L1),
  keysort(L1,L),
  D is Max-Min,
  BinWidth is D/NBins,
  bin(NBins,L,Min,BinWidth,LB),
  maplist(key,LB,X),
  maplist(value,LB,Y),
  Chart = c3{data:_{x:x,
    columns:[[x|X],[freq|Y]], type:bar},
    axis:_{ x:_{ tick:_{fit:false}}},
     bar:_{
     width:_{ ratio: 1.0 }},
     legend:_{show: false}}.

/**
 * densities(+PriorList:list,+PostList:list,-Chart:dict) is det
 *
 * Equivalent to densities/4 with an empty option list.
 */
densities(Pri,Post,Chart):-
  densities(Pri,Post,Chart,[]).

/**
 * densities(+PriorList:list,+PostList:list,-Chart:dict,+Options:list) is det
 *
 * Draws a line chart of the density of two sets of samples, usually
 * prior and post observations. The samples from the prior are in PriorList
 * while the samples from the posterior are in PostList
 * as couples [V]-W or V-W where V is a value and W its weigth.
 * Options is a list of options, the following are recognised by histogram/3:
 * * nbins(+NBins:int)
 *   the number of bins for dividing the domain, default value 40
 * */
densities(Pri0,Post0,Chart,Options):-
  option(nbins(NBins),Options,40),
  maplist(to_pair,Pri0,Pri1),
  maplist(to_pair,Post0,Post1),
  maplist(key,Pri1,Pri),
  maplist(key,Post1,Post),
  append(Pri,Post,All),
  max_list(All,DMax0),
  min_list(All,DMin),
  DMax is DMax0+(DMax0-DMin)/12,
  option(max(Max),Options,DMax),
  option(min(Min),Options,DMin),
  D is Max-Min,
  BinWidth is D/NBins,
  keysort(Pri1,Pr),
  keysort(Post1,Po),
  bin(NBins,Pr,Min,BinWidth,LPr),
  bin(NBins,Po,Min,BinWidth,LPo),
  maplist(key,LPr,X),
  maplist(value,LPr,YPr),
  maplist(value,LPo,YPo),
  Chart = c3{data:_{x: x,
  columns: [[x|X],
    [pre|YPr],[post|YPo]]},
   axis:_{ x:_{ tick:_{fit:false}}}
  }.
/**
 * density(+List:list,+NBins:int,+Min:float,+Max:float,-Chart:dict) is det
 *
 * Draws a line chart of the density of a sets of samples.
 * The samples are in List
 * as couples [V]-W or V-W where V is a value and W its weigth.
 * The lines are drawn dividing the domain in
 * NBins bins. The X axis goes from Min to Max.
 */
density(Post,NBins,Min,Max,Chart):-
  D is Max-Min,
  BinWidth is D/NBins,
  keysort(Post,Po),
  bin(NBins,Po,Min,BinWidth,LPo),
  maplist(key,LPo,X),
  maplist(value,LPo,YPo),
  Chart = c3{data:_{x: x,
  columns: [[x|X],
    [dens|YPo]]},
   axis:_{ x:_{ tick:_{fit:false}}}
  }.

/**
 * density2d(+List:list,+NBins:int,+XMin:float,+XMax:float,+YMin:float,+YMax:float,-Dist:list) is det
 *
 * Returns the density of a sets of two dimensional samples.
 * The samples are in List
 * as couples [V]-W or V-W where V is a value and W its weigth.
 * The lines are drawn dividing the domain in
 * NBins bins. The X axis goes from Min to Max.
 */
density2d(Post0,NBins,XMin,XMax,YMin,YMax,D):-
  maplist(to_pair,Post0,Post),
  DX is XMax-XMin,
  XBinWidth is DX/NBins,
  DY is YMax-YMin,
  YBinWidth is DY/NBins,
  bin2D(NBins,Post,XMin,YMin,XBinWidth,YBinWidth,D).

/**
 * density(+List:list,-Chart:dict) is det
 *
 * Equivalent to density/3 with an empty option list.
 */
density(Post,Chart):-
  density(Post,Chart,[]).

/**
 * density(+List:list,-Chart:dict,+Options:list) is det
 *
 * Draws a line chart of the density of a sets of samples.
 * The samples are in List
 * as couples [V]-W or V-W where V is a value and W its weigth.
 *
 * Options is a list of options, the following are recognised by density/3:
 * * min(+Min:float)
 *   the minimum value of domain, default value the minimum in List
 * * max(+Max:float)
 *   the maximum value of domain, default value the maximum in List
 * * nbins(+NBins:int)
 *   the number of bins for dividing the domain, default value 40
 */
density(Post,Chart,Options):-
  maplist(to_pair,Post,Post0),
  maplist(key,Post0,PoK),
  max_list(PoK,DMax0),
  min_list(PoK,DMin),
  DMax is DMax0+(DMax0-DMin)/12,
  option(max(Max),Options,DMax),
  option(min(Min),Options,DMin),
  option(nbins(NBins),Options,40),
  density(Post0,NBins,Min,Max,Chart).

/**
 * density2d(+List:list,-Chart:dict) is det
 *
 *  Equivalent to density2d/3 with an empty option list.
 */
density2d(Post0,D):-
  density2d(Post0,D,[]).

/**
 * density2d(+List:list,-Chart:dict,+Options:list) is det
 *
 * Draws a line chart of the density of a sets of samples.
 * The samples are in List
 * as couples [V]-W or V-W where V is a value and W its weigth.
 *
 * Options is a list of options, the following are recognised by density2d/3:
 * * xmin(+XMin:float)
 *   the minimum value of the X domain, default value the minimum in List
 * * xmax(-XMax:float)
 *   the maximum value of the X domain, default value the maximum in List
 * * ymin(-YMin:float)
 *   the minimum value of the Y domain, default value the minimum in List
 * * ymax(-YMax:float)
 *   the maximum value of the Y domain, default value the maximum in List
 * * nbins(+NBins:int)
 *   the number of bins for dividing the X and Y domains, default value 40
 */
density2d(Post0,D,Options):-
  maplist(key_x_y,Post0,X,Y),
  max_list(X,DxMax),
  min_list(X,DxMin),
  max_list(Y,DyMax),
  min_list(Y,DyMin),
  option(xmax(XMax),Options,DxMax),
  option(xmin(XMin),Options,DxMin),
  option(ymax(YMax),Options,DyMax),
  option(ymin(YMin),Options,DyMin),
  option(nbins(NBins),Options,40),
  density2d(Post0,NBins,XMin,XMax,YMin,YMax,D).

/**
 * to_pair(+Pair:pair,-FlattenedPair:pair) is det
 *
 * Given a pair E-W, returns a pair Ep-W where
 * Ep=EE if E=[EE], otherwise Ep=E
 */
to_pair([E]-W,E-W):- !.
to_pair(E-W,E-W).

/**
 * key(+Pair:pair,-Key:term) is det
 *
 * Given a pair Key-Vaule, returns its first element Key
 */
key(K-_,K).

/**
 * value(+Pair:pair,-Value:term) is det
 *
 * Given a pair Key-Vaule, returns its second element Value
 */
value(_ - Y,Y).

key_x_y([X,Y]-_,X,Y).


bin2D(NBins,Post,XMin,YMin,XBinWidth,YBinWidth,D):-
  binX(NBins,NBins,Post,XMin,YMin,XBinWidth,YBinWidth,D).

binX(0,_NBins,_Post,_XMin,_YMin,_XBinWidth,_YBinWidth,[]):-!.

binX(N,NBins,L,XLower,YMin,XBW,YBW,[R|D]):-
  V is XLower+XBW/2,
  XUpper is XLower+XBW,
  binY(NBins,L,V,XLower,XUpper,YMin,YBW,R),
  N1 is N-1,
  binX(N1,NBins,L,XUpper,YMin,XBW,YBW,D).

binY(0,_Post,_XV,_XMin,_YMin,_XBinWidth,_YBinWidth,[]):-!.

binY(N,L,XV,XLower,XUpper,YLower,YBW,[[XV,YV]-Freq|D]):-
    YV is YLower+YBW/2,
    YUpper is YLower+YBW,
    count_bin2d(L,XLower,XUpper,YLower,YUpper,0,Freq),
    N1 is N-1,
    binY(N1,L,XV,XLower,XUpper,YUpper,YBW,D).

count_bin2d([],_XL,_XU,_YL,_YU,F,F).

count_bin2d([[X,Y]-W|T0],XL,XU,YL,YU,F0,F):-
  ((X>=XL,X<XU,Y>=YL,Y<YU)->
    F1 is F0+W
  ;
    F1 = F0
  ),
  count_bin2d(T0,XL,XU,YL,YU,F1,F).

/**
 * bin(+N:int,+Values:list,+Lower:number,+BinWidth:number,-Couples:list) is det
 *
 * Given a list of numeric Values, a Lower value and BinWidth, returns in Couples
 * a list of N pairs V-Freq where V is the midpoint of a bin and Freq is the number
 * of values that are inside the bin interval [V-BinWidth/2,V+BinWidth/2)
 * starting with the bin where V-BinWidth/2=Lower
 */
bin(0,_L,_Min,_BW,[]):-!.

bin(N,L,Lower,BW,[V-Freq|T]):-
  V is Lower+BW/2,
  Upper is Lower+BW,
  count_bin(L,Lower,Upper,0,Freq,L1),
  N1 is N-1,
  bin(N1,L1,Upper,BW,T).

count_bin([],_L,_U,F,F,[]).

count_bin([H-_W|T0],L,U,F0,F,T):-
  H<L,!,
  count_bin(T0,L,U,F0,F,T).

count_bin([H-W|T0],L,U,F0,F,T):-
  (H>=U->
    F=F0,
    T=[H-W|T0]
  ;
    F1 is F0+W,
    count_bin(T0,L,U,F1,F,T)
  ).

/**
 * beta(+Alphas:list,-Beta:float) is det
 *
 * Computes the value of the multivariate beta function for vector Alphas
 * https://en.wikipedia.org/wiki/Beta_function#Multivariate_beta_function
 * Alphas is a list of floats
 */
beta(Par,B):-
  sumlist(Par,Sum),
  maplist(comp_lgamma,Par,LnGPar),
  LnG is lgamma(Sum),
  sumlist([-LnG|LnGPar],Exp),
  B is exp(Exp).

comp_lgamma(X,LnG):-
  LnG is lgamma(X).

/**
 * average(+Values:list,-Average:float) is det
 *
 * Computes the average of Values.
 * Values can be
 *
 * * a list of numbers
 * * a list of couples number-weight, in which case each number is multiplied by the weight
 *   before being summed
 * * a list of lists,  in which case lists are considered as matrices of numbers and averaged
 *   element-wise
 * * a list of couples list-weight, in which case the list is considered as a matrix of numbers.
 *   The matrix in each element of List must have the same dimension and are aggregated element-
 *   wise
 */
average([H|T],Av):-
  number(H),!,
  sum_list([H|T],Sum),
  length([H|T],N),
  Av is Sum/N.

average([H|T],E):-
  is_list(H),!,
  length(H,N),
  list0(N,L0),
  foldl(vector_sum,[H|T],L0,Sum),
  length([H|T],NV),
  matrix_div_scal([Sum],NV,[E]).

average([H-W|T],E):-
  is_list(H),!,
  length(H,N),
  list0(N,L0),
  foldl(single_value_vect,[H-W|T],L0,Sum),
  foldl(agg_val,[H-W|T],0,SW),
  matrix_div_scal([Sum],SW,[E]).

average(ValList,E):-
  foldl(single_value_cont,ValList,0,Sum),
  foldl(agg_val,ValList,0,SW),
  E is Sum/SW.


single_value_cont(H-N,S,S+N*H).

single_value_vect(H-N,S0,S):-
  matrix_mult_scal([H],N,[H1]),
  matrix_sum([H1],[S0],[S]).

/**
 * agg_val(+Couple:atom,+PartialSum:number,-Sum:number) is det
 *
 * Aggregate values by summation. The first argument is a couple _-N with
 * N the new value to sum to PartialSum
 */
agg_val(_ -N,S,S+N).

vector_sum(A,B,C):-
  matrix_sum([A],[B],[C]).

/**
 * variance(+Values:list,-Average:float,-Variance:float) is det.
 * variance(+Values:list,-Variance:float) is det.
 * std_dev(+Values:list,-Average:float,-Dev:float) is det.
 * std_dev(+Values:list,-Dev:float) is det.
 *
 * Computes the variance or standard deviation (and the average) of Values.
 * Values can be
 *
 * * a list of numbers
 * * a list of couples number-weight, in which case each number is multiplied by the weight
 *   before being considered
 * * a list of couples list-weight, in which case list is considered as a matrix of numbers.
 *   The matrix in each element of List must have the same dimension and are aggregated element-
 *   wise
 */
variance(L,Var):-
  variance(L,_Av,Var).

variance(L,Av,Var):-
  average(L,Av),
  maplist(sq_diff(Av),L,LS),
  average(LS,Var).

std_dev(L,Dev):-
  std_dev(L,_Av,Dev).

std_dev(L,Av,Dev):-
  variance(L,Av,Var),
  root(Var,Dev).

root(Var,Dev):-
  number(Var),!,
  Dev is sqrt(Var).

root(Var,Dev):-
  maplist(sqroot,Var,Dev).

sqroot(A,B):-
  B is sqrt(A).

sq_diff(Av,A,S):-
  number(A),!,
  S is (A-Av)^2.

sq_diff(Av,A-W,S):-
  number(A),!,
  S is W*(A-Av)^2.

sq_diff(Av,A-W,S):-
  maplist(sq_diff,Av,A,S0),
  matrix_mult_scal([S0],W,[S]).

/**
 * swi_builtin(+Goal:atom) is det
 *
 * Succeeds if Goal is an atom whose predicate is defined in Prolog
 * (either builtin or defined in a standard library).
 */
:- dynamic
  builtin_cache/2.

swi_builtin(G):-
  builtin_cache(G, IsBuiltin),
  !,
  IsBuiltin == true.
swi_builtin(G):-
  functor(G, Name, Arity),
  functor(Gen, Name, Arity),
  (   builtin_int(Gen)
  ->  asserta(builtin_cache(Gen, true))
  ;   asserta(builtin_cache(Gen, false)),
      fail
  ).

builtin_int(G):-
  predicate_property(G,built_in).
builtin_int(G):-
  predicate_property(G,imported_from(lists)).
builtin_int(G):-
  predicate_property(G,imported_from(apply)).
builtin_int(G):-
  predicate_property(G,imported_from(nf_r)).
builtin_int(G):-
  predicate_property(G,imported_from(matrix)).
builtin_int(G):-
  predicate_property(G,imported_from(clpfd)).


:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(cplint_util:bar(_,_)).
sandbox:safe_primitive(cplint_util:bar(_,_,_)).
sandbox:safe_primitive(cplint_util:bar1(_,_)).
sandbox:safe_primitive(cplint_util:argbar(_,_)).
sandbox:safe_primitive(cplint_util:histogram(_,_)).
sandbox:safe_primitive(cplint_util:histogram(_,_,_)).
sandbox:safe_primitive(cplint_util:densities(_,_,_)).
sandbox:safe_primitive(cplint_util:densities(_,_,_,_)).
sandbox:safe_primitive(cplint_util:density(_,_)).
sandbox:safe_primitive(cplint_util:density(_,_,_)).
sandbox:safe_primitive(cplint_util:density2d(_,_)).
sandbox:safe_primitive(cplint_util:density2d(_,_,_)).
sandbox:safe_primitive(cplint_util:to_pair(_,_)).
sandbox:safe_primitive(cplint_util:key(_,_)).
sandbox:safe_primitive(cplint_util:value(_,_)).
sandbox:safe_primitive(cplint_util:value(_,_)).
sandbox:safe_primitive(cplint_util:bin(_,_,_,_,_)).
sandbox:safe_primitive(cplint_util:beta(_,_)).



:- multifile license:license/3.

license:license(artisticv2, permissive,
                [ comment('Artistic License 2.0'),
                  url('https://opensource.org/licenses/Artistic-2.0')
                ]).

:- license(artisticv2).
