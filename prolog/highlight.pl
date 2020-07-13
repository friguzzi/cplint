:- module(cplint_highligh, []).
:- op(600, xfy, ::).

/** <module> SWI-Prolog IDE and SWISH highlighting

This module defines highlight  support  for   both  the  offline  tools,
notably  PceEmacs  and  the   graphical    debugger,   and   the  online
[cplint-on-SWISH](http://cplint.ml.unife.it).
*/

:- multifile prolog_colour:term_colours/2.

prolog_colour:term_colours((:- begin_lpad),
             neck(directive)-[cplint_directive]):-!.

prolog_colour:term_colours((:- end_lpad),
             neck(directive)-[cplint_directive]):-!.

prolog_colour:term_colours((:- begin_plp),
             neck(directive)-[cplint_directive]):-!.

prolog_colour:term_colours((:- end_plp),
             neck(directive)-[cplint_directive]):-!.

prolog_colour:term_colours((:- pita),
             neck(directive)-[cplint_directive]):-!.

prolog_colour:term_colours((:- mc),
             neck(directive)-[cplint_directive]):-!.

prolog_colour:term_colours((:- sc),
             neck(directive)-[cplint_directive]):-!.

prolog_colour:term_colours((:- lemur),
             neck(directive)-[cplint_directive]):-!.

prolog_colour:term_colours((:- begin_in),
             neck(directive)-[cplint_directive]):-!.

prolog_colour:term_colours((:- end_in),
             neck(directive)-[cplint_directive]):-!.

prolog_colour:term_colours((:- begin_bg),
             neck(directive)-[cplint_directive]):-!.

prolog_colour:term_colours((:- end_bg),
             neck(directive)-[cplint_directive]):-!.

prolog_colour:term_colours(begin(model(_)), model_delim - [model_delim - [classify]]):-!.

prolog_colour:term_colours(end(model(_)), model_delim - [model_delim - [classify]]):-!.


prolog_colour:term_colours((H:-Body), neck(clause)-
  [C,body(Body)]):-
	(H=(_:_;_);(H=(_:P),is_annotation(P))),!,
	build_color(H,C).

prolog_colour:term_colours(H,C):-
	(H=(_:_;_);(H=(_:P),is_annotation(P))),!,
	build_color(H,C).

prolog_colour:term_colours((H:-Body), neck(clause)-
  [C,body(Body)]):-
	(H=(_::_;_);H=(_::_)),!,
	build_color_pb(H,C).

prolog_colour:term_colours(H,C):-
	(H=(_::_;_);H=(_::_)),!,
	build_color_pb(H,C).

is_annotation(A):-
	number(A),!.

is_annotation(A):-
	var(A),!.

is_annotation(A):-
	functor(A,F,_Ar),
	is_func(F),!.

is_annotation(A):-
	functor(A,F,_Ar),
	is_cont_ann(F).


is_cont_ann(F):-
	member(F,[
	  uniform,gaussian,dirichlet,discrete,
		gamma,beta,poisson,binomial,geometric]),!.

is_func(F):-
	member(F,[/,+,-,*,**,^]),!.

build_color(H:P,annotation_symbol-[head(head,H),A]):-!,
  ann_colour(P,A).

build_color((H:P;Rest),disjunction-[annotation_symbol-[head(head,H),A],RC]):-
  ann_colour(P,A),
	build_color(Rest,RC).

build_color_pb(P::H,annotation_symbol-[A,head(head,H)]):-!,
  ann_colour(P,A).

build_color_pb((P::H;Rest),disjunction-[annotation_symbol-[A,head(head,H)],RC]):-
  ann_colour(P,A),
	build_color_pb(Rest,RC).

ann_colour(A,annotation):-
	number(A),!.

ann_colour(A,annotation_function):-
	var(A),!.

ann_colour(A,annotation_function):-
	functor(A,F,_Ar),
	is_cont_ann(F),!.

ann_colour(A,annotation_function-Cols):-
	A=..[F|Ar],
	is_func(F),!,
	maplist(exp_col,Ar,Cols).

exp_col(A,annotation):-
	number(A),!.

exp_col(A,annotation_function):-
	var(A),!.

exp_col(A,annotation_function-Cols):-
	A=..[F|Ar],
	is_func(F),!,
	maplist(exp_col,Ar,Cols).

:- multifile prolog_colour:style/2.

prolog_colour:style(annotation,                  [colour(maroon), bold(true)]).
prolog_colour:style(annotation_function,                  [colour(maroon), bold(true)]).
prolog_colour:style(annotation_symbol,                  [colour(dark_red)]).
prolog_colour:style(disjunction,                  [colour(deep_pink),bold(true)]).
prolog_colour:style(cplint_directive,                  [colour(firebrick),bold(true)]).
prolog_colour:style(model_delim,                  [colour(firebrick),bold(true)]).

:- multifile swish_highlight:style/3.

swish_highlight:style(annotation,  annotation, [base(number)]).
swish_highlight:style(annotation_function,   annotation_function,  [text, base(functor)]).
swish_highlight:style(annotation_symbol,   annotation_symbol,  [text, base(symbol)]).
swish_highlight:style(disjunction,  disjunction, [text, base(symbol)]).
swish_highlight:style(cplint_directive,  cplint_directive, [text, base(atom)]).
swish_highlight:style(model_delim,  model_delim, [text, base(symbol)]).
