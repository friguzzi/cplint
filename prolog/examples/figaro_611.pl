:- use_module(library(mcintyre)).

:- mc.

:- begin_lpad.

flip(h,P,N):P; flip(t,P,N):1-P.
bias(X): beta(X,2,5).

next_toss(T):-
    bias(Bias),
	flip(T,Bias,0).

previous_tosses(Tosses):-
    length(Tosses,NumTosses),
    tosses(Tosses,NumTosses).

tosses([],0).
tosses([H|T],NT):-
    bias(Bias),
    flip(H,Bias,NT),
    NT1 is NT-1,
    tosses(T,NT1).

:- end_lpad.

/** <examples> Your example queries go here, e.g.

?-mc_lw_sample(next_toss(h),
previous_tosses([h,t,h,h,h,h,h,t,h,t,h,h,h,t,h,h,h,h,h,h]),
1000,P).
?-mc_lw_expectation(bias(B),
previous_tosses([h,t,h,h,h,h,h,t,h,t,h,h,h,t,h,h,h,h,h,h]),
1000,B,E).
*/
