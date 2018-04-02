/** <module> cplint_util

Utility module for cplint

@author Fabrizio Riguzzi
@license Artistic License 2.0 https://opensource.org/licenses/Artistic-2.0
@copyright Fabrizio Riguzzi
*/

:- module(cplint_util,[
  bar/2,
  bar/3,
  bar2/2,
  argbar/2,
  histogram/3,
  densities/4,
  density/3,
  density2d/3,
  to_pair/2,
  key/2,
  y/2,
  bin/5]).

/**
 * bar2(+Probability:float,-Chart:dict) is nondet
 *
 * The predicate returns a dict for rendering with c3 as a bar chart with
 * a bar for the probability and a bar for one minus the probability.
 */
bar2(P,Chart):-
  PF is 1.0-P,
  Chart = c3{data:_{x:elem, rows:[elem-prob,'T'-P,'F' -PF], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,max:1.0,padding:_{bottom:0.0,top:0.0},
             tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}},
	           size:_{height: 100},
	          legend:_{show: false}}.

/**
 * bar(+Probability:float,-Chart:dict) is nondet
 *
 * The predicate returns a dict for rendering with c3 as a bar chart with
 * a bar for the probability
 */
bar(P,Chart):-
  Chart = c3{data:_{x:elem, rows:[elem-prob,'T'-P], type:bar},
          axis:_{x:_{type:category}, rotated: true,
                 y:_{min:0.0,max:1.0,padding:_{bottom:0.0,top:0.0},
             tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}},
	           size:_{height: 100},
	          legend:_{show: false}}.
/**
 * bar(+Successes:int,+Failures:int,-Chart:dict) is nondet
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
 * argbar(+Values:list,-Chart:dict) is nondet
 *
 * Values is a list of couples V-N where
 * V is the value and N is the number of samples
 * returning that value.
 * The predicate returns a dict for rendering with c3 as a bar chart with
 * a bar each value.
 */
argbar(ValList,Chart):-
        maplist(to_atom,ValList,ValList1),
        Chart = c3{data:_{x:elem, rows:[elem-prob|ValList1], type:bar},
            axis:_{x:_{type:category}, rotated: true,
                   y:_{min:0.0,padding:_{bottom:0.0}}},
             %  size:_{height: 100},
              legend:_{show: false}}.


to_atom(A0-N,A-N):-
  term_to_atom(A0,A).

/**
 * histogram(+List:list,-Chart:dict,+Options:list) is det
 *
 * Draws a histogram of the samples in List. List must be a list of couples of the form [V]-W or V-W
 * where V is a sampled value and W is its weight.
 *
 * Options is a list of options, the following are recognised by histogram/4:
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
  max_list(L2,DMax),
  min_list(L2,DMin),
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
  maplist(y,LB,Y),
  Chart = c3{data:_{x:x,
    columns:[[x|X],[freq|Y]], type:bar},
    axis:_{ x:_{ tick:_{fit:false}}},
     bar:_{
     width:_{ ratio: 1.0 }},
     legend:_{show: false}}.

/**
 * densities(+PriorList:list,+PostList:list,+NBins:int,-Chart:dict) is det
 *
 * Draws a line chart of the density of two sets of samples, usually
 * prior and post observations. The samples from the prior are in PriorList
 * while the samples from the posterior are in PostList
 * as couples [V]-W or V-W where V is a value and W its weigth.
 * The lines are drawn dividing the domain in
 * NBins bins.
 */
densities(Pri0,Post0,NBins,Chart):-
  maplist(to_pair,Pri0,Pri1),
  maplist(to_pair,Post0,Post1),
  maplist(key,Pri1,Pri),
  maplist(key,Post1,Post),
  append(Pri,Post,All),
  max_list(All,Max),
  min_list(All,Min),
  D is Max-Min,
  BinWidth is D/NBins,
  keysort(Pri1,Pr),
  keysort(Post1,Po),
  bin(NBins,Pr,Min,BinWidth,LPr),
  bin(NBins,Po,Min,BinWidth,LPo),
  maplist(key,LPr,X),
  maplist(y,LPr,YPr),
  maplist(y,LPo,YPo),
  Chart = c3{data:_{x: x,
  columns: [[x|X],
    [pre|YPr],[post|YPo]]},
   axis:_{ x:_{ tick:_{fit:false}}}
  }.

/**
 * densities(+PriorList:list,+PostList:list,+NBins:int,-Chart:dict) is det
 *
 * Draws a line chart of the density of two sets of samples, usually
 * prior and post observations. The samples from the prior are in PriorList
 * while the samples from the posterior are in PostList
 * as couples [V]-W or V-W where V is a value and W its weigth.
 * The lines are drawn dividing the domain in
 * NBins bins.
 */
densities(Pri0,Post0,NBins,Chart):-
  maplist(to_pair,Pri0,Pri1),
  maplist(to_pair,Post0,Post1),
  maplist(key,Pri1,Pri),
  maplist(key,Post1,Post),
  append(Pri,Post,All),
  max_list(All,Max),
  min_list(All,Min),
  D is Max-Min,
  BinWidth is D/NBins,
  keysort(Pri1,Pr),
  keysort(Post1,Po),
  bin(NBins,Pr,Min,BinWidth,LPr),
  bin(NBins,Po,Min,BinWidth,LPo),
  maplist(key,LPr,X),
  maplist(y,LPr,YPr),
  maplist(y,LPo,YPo),
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
density(Post0,NBins,Min,Max,Chart):-
  maplist(to_pair,Post0,Post),
  D is Max-Min,
  BinWidth is D/NBins,
  keysort(Post,Po),
  bin(NBins,Po,Min,BinWidth,LPo),
  maplist(key,LPo,X),
  maplist(y,LPo,YPo),
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
 * density(+List:list,-Chart:dict,+Options:list) is det
 *
 * Draws a line chart of the density of a sets of samples.
 * The samples are in List
 * as couples [V]-W or V-W where V is a value and W its weigth.
 *
 * Options is a list of options, the following are recognised by density/4:
 * * min(+Min:float)
 *   the minimum value of domain, default value the minimum in List
 * * max(+Max:float)
 *   the maximum value of domain, default value the maximum in List
 * * nbins(+NBins:int)
 *   the number of bins for dividing the domain, default value 40
 */
density(Post0,Chart,Options):-
  maplist(key,Post0,PoK),
  max_list(PoK,DMax),
  min_list(PoK,DMin),
  option(max(Max),Options,DMax),
  option(min(Min),Options,DMin),
  option(nbins(NBins),Options,40),
  density(Post0,NBins,Min,Max,Chart).

/**
 * density2d(+List:list,-Chart:dict,+Options:list) is det
 *
 * Draws a line chart of the density of a sets of samples.
 * The samples are in List
 * as couples [V]-W or V-W where V is a value and W its weigth.
 *
 * Options is a list of options, the following are recognised by density2d/4:
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

to_pair([E]-W,E-W):- !.
to_pair(E-W,E-W).

key(K-_,K).

key_x_y([X,Y]-_,X,Y).
y(_ - Y,Y).


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

:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(mcintyre:bar(_,_)).
sandbox:safe_primitive(mcintyre:bar2(_,_)).

:- multifile license:license/3.

license:license(artisticv2, permissive,
                [ comment('Artistic License 2.0'),
                  url('https://opensource.org/licenses/Artistic-2.0')
                ]).

:- license(artisticv2).