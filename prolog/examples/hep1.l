:-set(max_iter,2).
:-set(beamsize,20).

output(type/2).

input(fibros/2).
input(activity/2).
input(sex/2).
input(action/2).
input(age/2).
input(type/2).
input(got/2).
input(gpt/2).
input(tbil/2).
input(dbil/2).
input(che/2).
input(ttt/2).
input(ztt/2).
input(tcho/2).
input(tp/2).
input(dur/2).
input(b_rel11/2).
input(b_rel12/2).
input(b_rel13/2).


modeh(*,type(+m,type_b)).
modeh(*,type(+m,type_c)).

determination(type/2,sex/2).
determination(type/2,age/2).

determination(type/2,b_rel11/2).
determination(type/2,b_rel12/2).
determination(type/2,b_rel13/2).

determination(type/2,fibros/2).
determination(type/2,activity/2).
determination(type/2,got/2).
determination(type/2,gpt/2).
determination(type/2,tbil/2).
determination(type/2,dbil/2).
determination(type/2,che/2).
determination(type/2,ttt/2).
determination(type/2,ztt/2).
determination(type/2,tcho/2).
determination(type/2,tp/2).
determination(type/2,dur/2).

modeb(*,b_rel11(-b,+m)).
modeb(*,b_rel12(-i,+m)).
modeb(*,b_rel13(-a,+m)).

modeb(*,b_rel11(+b,-m)).
modeb(*,b_rel12(+i,-m)).
modeb(*,b_rel13(+a,-m)).


modeb(*,fibros(+b,-#f)).
modeb(*,activity(+b,-#a)).
modeb(*,sex(+m,-#sex)).
modeb(*,age(+m,-#age)).
modeb(*,got(+i,-#got)).
modeb(*,gpt(+i,-#gpt)).
modeb(*,tbil(+i,-#tbil)).
modeb(*,dbil(+i,-#dbil)).
modeb(*,che(+i,-#che)).
modeb(*,ttt(+i,-#ttt)).
modeb(*,ztt(+i,-#ztt)).
modeb(*,tcho(+i,-#tcho)).
modeb(*,tp(+i,-#tp)).
modeb(*,dur(+a,#dur)).
modeb(*,fibros(+b,-f)).
modeb(*,activity(+b,-a)).
modeb(*,sex(+m,-sex)).
modeb(*,age(+m,-age)).
modeb(*,got(+i,-got)).
modeb(*,gpt(+i,-gpt)).
modeb(*,tbil(+i,-tbil)).
modeb(*,dbil(+i,-dbil)).
modeb(*,che(+i,-che)).
modeb(*,ttt(+i,-ttt)).
modeb(*,ztt(+i,-ztt)).
modeb(*,tcho(+i,-tcho)).
modeb(*,tp(+i,-tp)).
modeb(*,dur(+a,dur)).

lookahead(b_rel11(A,_),[fibros(A,_)]).
lookahead(b_rel11(A,_),[activity(A,_)]).

lookahead(b_rel12(A,_),[got(A,_)]).
lookahead(b_rel11(A,_B),[gpt(A,_)]).
lookahead(b_rel11(A,_B),[tbil(A,_)]).
lookahead(b_rel11(A,_B),[dbil(A,_)]).
lookahead(b_rel11(A,_B),[che(A,_)]).
lookahead(b_rel11(A,_B),[ttt(A,_)]).
lookahead(b_rel11(A,_B),[ztt(A,_)]).
lookahead(b_rel11(A,_B),[tcho(A,_)]).
lookahead(b_rel11(A,_B),[tp(A,_)]).
lookahead(b_rel13(A,_B),[dur(A,_)]).

