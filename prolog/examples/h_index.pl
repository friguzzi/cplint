/* Computation of the h-index of an author given his number of papers
assuming that the citations per paper follow a Poisson distribution with 
given average.
It shows that the expected h-index is directly proportional to the
average number of citations (expected h-index=avg. n. cit. +5)
*/
:- use_module(library(mcintyre)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.

:- begin_lpad.

citations(AverageCit,_,Cit):poisson(Cit,AverageCit).

h_index(NumPapers,AverageCit,HIndex):-
    numlist(1,NumPapers,Papers),
    maplist(citations(AverageCit),Papers,Citations),
    sort(0,  @>=, Citations,  Sorted),
    compute_index(Sorted,1,HIndex).

compute_index([0|_],_,0):-!.

compute_index([],I0,I):-!,
    I is I0-1.

compute_index([I|_T],I,I):-!.

compute_index([H|_T],I0,I):-
    H<I0,!,
    I is I0-1.

compute_index([H|T],I0,I):-
    H>I0,
    I1 is I0+1,
    compute_index(T,I1,I).


    
:- end_lpad.

h_vs_avg_cit(Papers,MaxAvg,Chart):-
  findall(E,(
    between(1,MaxAvg,N),mc_expectation(h_index(Papers,N,T),10,T,E)
  ),V),
  numlist(1,MaxAvg,X),
  findall(N,(between(1,MaxAvg,N0),N is 5+N0),Dep),
  Chart=c3{data:_{x:x, columns:[[x|X],['Expected h-index'|V],['Dependency'|Dep]]}}.

/** <examples> Your example queries go here, e.g.

?- mc_sample_arg_first(h_index(200,10,H),1000,H,HList),density(HList,Dens,[nbins(20)]).
compute the distribution of the h_index given that the authors wrote
200 papers and each paper receives on average 10 citations
?- mc_expectation(h_index(200,10,H),1000,H,HExp).
compute the expected value of the h_index given that the authors wrote
200 papers and each paper receives on average 10 citations
?- mc_sample_arg_first(citations(10,200,Cit),1000,Cit,CitList),density(CitList,Dens,[nbins(20)]).
?- h_vs_avg_cit(100,40,Chart).
Plot of the depedency of the expected h-index as a function of the average
number of citations per article (max 40) given that there are 100 papers.
The graphs shows that the expected h-index is directly proportional to the
average number of citations (expected h-index=avg. n. cit. +5)
*/

