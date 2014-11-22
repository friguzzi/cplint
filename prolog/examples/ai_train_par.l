output(advisedby/2).


input(student/1).

input(professor/1).

input(inphase/2).

input(hasposition/2).

input(publication/2).

input(yearsinprogram/2).

input(taughtby/3).

input(ta/3).

input(courselevel/2).

input(tempadvisedby/2).


determination(professor/1,student/1).
determination(professor/1,hasposition/2).
determination(professor/1,publication/2).
determination(professor/1,courselevel/2).
determination(professor/1,inphase/2).
determination(professor/1,advisedby/2).
determination(professor/1,tempadvisedby/2).
determination(professor/1,yearsinprogram/2).
determination(professor/1,taughtby/3).
determination(professor/1,ta/3).

determination(student/1,professor/1).
determination(student/1,hasposition/2).
determination(student/1,publication/2).
determination(student/1,courselevel/2).
determination(student/1,inphase/2).
determination(student/1,advisedby/2).
determination(student/1,tempadvisedby/2).
determination(student/1,yearsinprogram/2).
determination(student/1,taughtby/3).
determination(student/1,ta/3).

determination(hasposition/2,professor/1).
determination(hasposition/2,student/1).
determination(hasposition/2,publication/2).
determination(hasposition/2,courselevel/2).
determination(hasposition/2,inphase/2).
determination(hasposition/2,advisedby/2).
determination(hasposition/2,tempadvisedby/2).
determination(hasposition/2,yearsinprogram/2).
determination(hasposition/2,taughtby/3).
determination(hasposition/2,ta/3).

determination(inphase/2,professor/1).
determination(inphase/2,student/1).
determination(inphase/2,hasposition/2).
determination(inphase/2,publication/2).
determination(inphase/2,courselevel/2).
determination(inphase/2,advisedby/2).
determination(inphase/2,tempadvisedby/2).
determination(inphase/2,yearsinprogram/2).
determination(inphase/2,taughtby/3).
determination(inphase/2,ta/3).

determination(taughtby/3,professor/1).
determination(taughtby/3,student/1).
determination(taughtby/3,hasposition/2).
determination(taughtby/3,publication/2).
determination(taughtby/3,courselevel/2).
determination(taughtby/3,inphase/2).
determination(taughtby/3,advisedby/2).
determination(taughtby/3,tempadvisedby/2).
determination(taughtby/3,yearsinprogram/2).
determination(taughtby/3,ta/3).

determination(advisedby/2,professor/1).
determination(advisedby/2,student/1).
determination(advisedby/2,hasposition/2).
determination(advisedby/2,publication/2).
determination(advisedby/2,courselevel/2).
determination(advisedby/2,inphase/2).
determination(advisedby/2,yearsinprogram/2).
determination(advisedby/2,taughtby/3).
determination(advisedby/2,ta/3).

determination(tempadvisedby/2,professor/1).
determination(tempadvisedby/2,student/1).
determination(tempadvisedby/2,hasposition/2).
determination(tempadvisedby/2,publication/2).
determination(tempadvisedby/2,courselevel/2).
determination(tempadvisedby/2,inphase/2).
determination(tempadvisedby/2,yearsinprogram/2).
determination(tempadvisedby/2,taughtby/3).
determination(tempadvisedby/2,ta/3).

determination(ta/3,professor/1).
determination(ta/3,student/1).
determination(ta/3,hasposition/2).
determination(ta/3,publication/2).
determination(ta/3,courselevel/2).
determination(ta/3,inphase/2).
determination(ta/3,advisedby/2).
determination(ta/3,tempadvisedby/2).
determination(ta/3,yearsinprogram/2).
determination(ta/3,taughtby/3).

determination(yearsinprogram/2,professor/1).
determination(yearsinprogram/2,student/1).
determination(yearsinprogram/2,hasposition/2).
determination(yearsinprogram/2,publication/2).
determination(yearsinprogram/2,courselevel/2).
determination(yearsinprogram/2,inphase/2).
determination(yearsinprogram/2,advisedby/2).
determination(yearsinprogram/2,tempadvisedby/2).
determination(yearsinprogram/2,taughtby/3).
determination(yearsinprogram/2,ta/3).

determination(courselevel/2,professor/1).
determination(courselevel/2,student/1).
determination(courselevel/2,hasposition/2).
determination(courselevel/2,publication/2).
determination(courselevel/2,inphase/2).
determination(courselevel/2,advisedby/2).
determination(courselevel/2,tempadvisedby/2).
determination(courselevel/2,yearsinprogram/2).
determination(courselevel/2,taughtby/3).
determination(courselevel/2,ta/3).

determination(publication/2,professor/1).
determination(publication/2,student/1).
determination(publication/2,hasposition/2).
determination(publication/2,courselevel/2).
determination(publication/2,inphase/2).
determination(publication/2,advisedby/2).
determination(publication/2,tempadvisedby/2).
determination(publication/2,yearsinprogram/2).
determination(publication/2,taughtby/3).
determination(publication/2,ta/3).


modeh(*,professor(+person)).
modeh(*,student(+person)).
modeh(*,hasposition(+person, faculty)).
modeh(*,inphase(+person, pre_quals)).
modeh(*,taughtby(+course, +person, +quarter)).
modeh(*,advisedby(+person,+person)). 
modeh(*,tempadvisedby(+person,+person)). 
modeh(*,ta(+course,+person,+quarter)).
modeh(*,courselevel(+course,#level)).
modeh(*,yearsinprogram(+person,#year)).


modeh(*,[advisedby(+person,+person),tempadvisedby(+person,+person)],[advisedby(A,B),tempadvisedby(A,B)],[professor/1,student/1,hasposition/2,inphase/2,publication/2,taughtby/3,ta/3,courselevel/2,yearsinprogram/2]).

modeh(*,[student(+person),professor(+person)],[student(P),professor(P)],[hasposition/2,inphase/2,taughtby/3,ta/3,courselevel/2,yearsinprogram/2,advisedby/2,tempadvisedby/2,publication/2]).

modeh(*,
  [hasposition(+person, faculty),hasposition(+person, faculty_affiliate),hasposition(+person, faculty_adjunct),hasposition(+person, faculty_emeritus),hasposition(+person, faculty_visiting)],
  [hasposition(P, faculty),hasposition(P, faculty_affiliate),hasposition(P, faculty_adjunct),hasposition(P, faculty_emeritus),hasposition(P, faculty_visiting)],
  [professor/1,student/1,inphase/2,taughtby/3,ta/3,courselevel/2,yearsinprogram/2,advisedby/2,tempadvisedby/2]).

modeh(*,
  [yearsinprogram(+person, year_1) , ta(+course, +person, +quarter)],
  [yearsinprogram(P, year_1) , ta(_C, P, _Q)],
  [professor/1,student/1,inphase/2,taughtby/3,courselevel/2,advisedby/2,tempadvisedby/2,hasposition/2]).

modeh(*,[yearsinprogram(+person, year_1) ,yearsinprogram(+person, year_2)],[yearsinprogram(P, year_1) ,yearsinprogram(P, year_2)],
  [professor/1,student/1,inphase/2,taughtby/3,ta/3,courselevel/2,advisedby/2,tempadvisedby/2,hasposition/2]).

modeh(*,[ inphase(+person,pre_quals) , inphase(+person,post_quals) , inphase(+person,post_generals)],[ inphase(P,pre_quals) , inphase(P,post_quals) , inphase(P,post_generals)],
  [professor/1,student/1,taughtby/3,ta/3,courselevel/2,yearsinprogram/2,advisedby/2,tempadvisedby/2,hasposition/2]).

modeh(*,[courselevel(+course,level_100) , advisedby(+person,+person)],[courselevel(_C,level_100) , advisedby(_S,_P)],
  [professor/1,student/1,inphase/2,taughtby/3,ta/3,yearsinprogram/2,tempadvisedby/2,hasposition/2,publication/2]).

modeh(*,[ta(+course,+person,+quarter) , courselevel(+course,level_100)],[ta(C,_P,_Q) , courselevel(C,level_100)],
  [professor/1,student/1,inphase/2,taughtby/3,yearsinprogram/2,advisedby/2,tempadvisedby/2,hasposition/2,publication/2]).

modeh(*,[taughtby(+course,+person,+quarter) , courselevel(+course,level_100)],[taughtby(C,_P,_Q) , courselevel(C,level_100)],
  [professor/1,student/1,inphase/2,ta/3,yearsinprogram/2,advisedby/2,tempadvisedby/2,hasposition/2,publication/2]).


modeb(*,publication(-title, +person)).
modeb(*,publication(+title, -person)).
modeb(*,professor(+person)).
modeb(*,student(+person)).
modeb(*,taughtby(-course, +person, -quarter)).
modeb(*,taughtby(+course, -person, -quarter)).
modeb(*,ta(+course, -person, +quarter)).
modeb(*,ta(-course, +person, -quarter)).
modeb(*,hasposition(+person, -position)).
modeb(*,hasposition(+person, #position)).
modeb(*,inphase(+person, -phase)).
modeb(*,inphase(+person, #phase)).
modeb(*,tempadvisedby(+person, -person)).
modeb(*,tempadvisedby(-person, +person)).
modeb(*,yearsinprogram(+person, -year)).
modeb(*,courselevel(+course, #level)).

lookahead(ta(_A,_B,_C),[taughtby(_A,_D,_C)]).
lookahead(publication(_A,_B),[publication(_A,_C), professor(_B),student(_C)]). 
