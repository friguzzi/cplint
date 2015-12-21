
:-use_module(slipcover).

fold(ai,[ai]).
fold(graphics,[graphics]).
fold(language,[language]).
fold(systems,[systems]).
fold(theory,[theory]).

:-set(max_iter,2).
:-set(beamsize,20).

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

%lookahead(ta(A,_B,C),[taughtby(A,_D,C)]).
%lookahead(publication(A,B),[publication(A,C), professor(B),student(C)]). 
lookahead(ta(_A,_B,_C),[taughtby(_A,_D,_C)]).
lookahead(publication(_A,_B),[publication(_A,_C), professor(_B),student(_C)]). 
% database
taughtby(ai,course44, person171, autumn_0001).
taughtby(ai,course24, person240, autumn_0001).
taughtby(ai,course12, person211, autumn_0001).
taughtby(ai,course123, person150, autumn_0001).
taughtby(ai,course44, person293, winter_0001).
taughtby(ai,course143, person211, winter_0001).
taughtby(ai,course50, person171, winter_0001).
taughtby(ai,course170, person79, winter_0001).
taughtby(ai,course15, person292, winter_0001).
taughtby(ai,course32, person319, winter_0001).
taughtby(ai,course158, person240, winter_0001).
taughtby(ai,course24, person150, spring_0001).
taughtby(ai,course52, person168, spring_0001).
taughtby(ai,course16, person240, spring_0001).
taughtby(ai,course173, person171, spring_0001).
taughtby(ai,course64, person79, spring_0001).
taughtby(ai,course44, person171, autumn_0102).
taughtby(ai,course24, person211, autumn_0102).
taughtby(ai,course156, person240, autumn_0102).
taughtby(ai,course12, person79, autumn_0102).
taughtby(ai,course143, person407, winter_0102).
taughtby(ai,course170, person211, winter_0102).
taughtby(ai,course44, person415, spring_0102).
taughtby(ai,course24, person240, spring_0102).
taughtby(ai,course52, person168, spring_0102).
taughtby(ai,course50, person171, spring_0102).
taughtby(ai,course39, person415, spring_0102).
taughtby(ai,course123, person150, spring_0102).
taughtby(ai,course76, person319, spring_0102).
taughtby(ai,course44, person171, autumn_0203).
taughtby(ai,course24, person240, autumn_0203).
taughtby(ai,course44, person415, winter_0203).
taughtby(ai,course52, person168, winter_0203).
taughtby(ai,course141, person150, winter_0203).
taughtby(ai,course12, person211, winter_0203).
taughtby(ai,course16, person79, winter_0203).
taughtby(ai,course24, person211, spring_0203).
taughtby(ai,course170, person407, spring_0203).
taughtby(ai,course15, person292, spring_0203).
taughtby(ai,course168, person240, spring_0203).
taughtby(ai,course64, person79, spring_0203).
taughtby(ai,course44, person171, autumn_0304).
taughtby(ai,course24, person79, autumn_0304).
taughtby(ai,course156, person240, autumn_0304).
taughtby(ai,course12, person407, autumn_0304).
taughtby(ai,course76, person319, autumn_0304).
taughtby(ai,course44, person415, winter_0304).
taughtby(ai,course57, person150, winter_0304).
taughtby(ai,course52, person168, winter_0304).
taughtby(ai,course170, person79, winter_0304).
taughtby(ai,course24, person407, spring_0304).
taughtby(ai,course50, person171, spring_0304).
taughtby(ai,course158, person240, spring_0304).
taughtby(ai,course7, person415, spring_0304).
courselevel(ai,course52, level_400).
courselevel(ai,course44, level_400).
courselevel(ai,course24, level_400).
courselevel(ai,course128, level_400).
courselevel(ai,course57, level_400).
courselevel(ai,course82, level_400).
courselevel(ai,course143, level_400).
courselevel(ai,course50, level_500).
courselevel(ai,course156, level_500).
courselevel(ai,course141, level_500).
courselevel(ai,course12, level_500).
courselevel(ai,course170, level_500).
courselevel(ai,course65, level_500).
courselevel(ai,course123, level_500).
courselevel(ai,course173, level_500).
courselevel(ai,course86, level_500).
courselevel(ai,course131, level_500).
courselevel(ai,course85, level_500).
courselevel(ai,course64, level_500).
courselevel(ai,course168, level_500).
courselevel(ai,course158, level_500).
courselevel(ai,course132, level_500).
courselevel(ai,course76, level_500).
courselevel(ai,course16, level_500).
courselevel(ai,course15, level_500).
courselevel(ai,course39, level_500).
courselevel(ai,course32, level_500).
courselevel(ai,course7, level_500).
courselevel(ai,course134, level_500).
courselevel(ai,course135, level_500).
hasposition(ai,person292, faculty_affiliate).
hasposition(ai,person293, faculty_affiliate).
hasposition(ai,person240, faculty).
hasposition(ai,person211, faculty).
hasposition(ai,person150, faculty).
hasposition(ai,person415, faculty).
hasposition(ai,person79, faculty).
hasposition(ai,person349, faculty_adjunct).
hasposition(ai,person7, faculty_adjunct).
hasposition(ai,person319, faculty).
hasposition(ai,person185, faculty_adjunct).
hasposition(ai,person171, faculty).
hasposition(ai,person168, faculty).
hasposition(ai,person407, faculty).
projectmember(ai,project62, person319).
advisedby(ai,person265, person168).
advisedby(ai,person381, person168).
advisedby(ai,person176, person407).
advisedby(ai,person272, person7).
advisedby(ai,person37, person79).
advisedby(ai,person353, person319).
advisedby(ai,person432, person240).
advisedby(ai,person239, person171).
advisedby(ai,person13, person240).
advisedby(ai,person286, person171).
advisedby(ai,person418, person171).
advisedby(ai,person14, person150).
advisedby(ai,person320, person150).
advisedby(ai,person352, person415).
advisedby(ai,person352, person292).
advisedby(ai,person276, person407).
advisedby(ai,person45, person415).
advisedby(ai,person45, person211).
advisedby(ai,person148, person171).
advisedby(ai,person314, person415).
advisedby(ai,person275, person79).
advisedby(ai,person21, person211).
advisedby(ai,person262, person415).
advisedby(ai,person262, person292).
advisedby(ai,person257, person240).
advisedby(ai,person380, person79).
advisedby(ai,person384, person240).
advisedby(ai,person384, person407).
advisedby(ai,person266, person7).
advisedby(ai,person312, person319).
advisedby(ai,person208, person319).
advisedby(ai,person63, person415).
advisedby(ai,person318, person185).
advisedby(ai,person318, person319).
advisedby(ai,person83, person349).
inphase(ai,person408, pre_quals).
inphase(ai,person265, post_generals).
inphase(ai,person70, pre_quals).
inphase(ai,person381, post_generals).
inphase(ai,person139, post_quals).
inphase(ai,person382, post_quals).
inphase(ai,person333, pre_quals).
inphase(ai,person94, pre_quals).
inphase(ai,person176, post_quals).
inphase(ai,person272, post_quals).
inphase(ai,person37, pre_quals).
inphase(ai,person353, post_quals).
inphase(ai,person432, post_quals).
inphase(ai,person377, pre_quals).
inphase(ai,person239, post_quals).
inphase(ai,person13, post_generals).
inphase(ai,person286, post_quals).
inphase(ai,person412, post_quals).
inphase(ai,person418, post_quals).
inphase(ai,person14, post_generals).
inphase(ai,person320, post_quals).
inphase(ai,person42, pre_quals).
inphase(ai,person20, pre_quals).
inphase(ai,person352, post_generals).
inphase(ai,person276, pre_quals).
inphase(ai,person45, post_generals).
inphase(ai,person233, pre_quals).
inphase(ai,person148, post_quals).
inphase(ai,person193, pre_quals).
inphase(ai,person314, post_generals).
inphase(ai,person275, post_generals).
inphase(ai,person21, post_generals).
inphase(ai,person262, post_generals).
inphase(ai,person257, post_generals).
inphase(ai,person73, post_quals).
inphase(ai,person380, post_generals).
inphase(ai,person384, post_quals).
inphase(ai,person406, post_generals).
inphase(ai,person266, post_quals).
inphase(ai,person312, pre_quals).
inphase(ai,person208, post_quals).
inphase(ai,person311, post_quals).
inphase(ai,person63, post_generals).
inphase(ai,person318, pre_quals).
inphase(ai,person83, post_quals).
inphase(ai,person161, post_generals).
inphase(ai,person284, post_quals).
tempadvisedby(ai,person408, person150).
tempadvisedby(ai,person382, person415).
tempadvisedby(ai,person333, person211).
tempadvisedby(ai,person94, person79).
tempadvisedby(ai,person377, person292).
tempadvisedby(ai,person412, person168).
tempadvisedby(ai,person42, person150).
tempadvisedby(ai,person20, person240).
tempadvisedby(ai,person233, person319).
tempadvisedby(ai,person193, person415).
tempadvisedby(ai,person284, person211).
yearsinprogram(ai,person408, year_2).
yearsinprogram(ai,person265, year_9).
yearsinprogram(ai,person70, year_1).
yearsinprogram(ai,person381, year_10).
yearsinprogram(ai,person139, year_3).
yearsinprogram(ai,person382, year_3).
yearsinprogram(ai,person333, year_2).
yearsinprogram(ai,person94, year_1).
yearsinprogram(ai,person176, year_2).
yearsinprogram(ai,person272, year_2).
yearsinprogram(ai,person37, year_1).
yearsinprogram(ai,person353, year_4).
yearsinprogram(ai,person432, year_5).
yearsinprogram(ai,person377, year_1).
yearsinprogram(ai,person239, year_4).
yearsinprogram(ai,person13, year_7).
yearsinprogram(ai,person286, year_3).
yearsinprogram(ai,person412, year_3).
yearsinprogram(ai,person418, year_3).
yearsinprogram(ai,person14, year_10).
yearsinprogram(ai,person320, year_3).
yearsinprogram(ai,person42, year_1).
yearsinprogram(ai,person20, year_1).
yearsinprogram(ai,person352, year_5).
yearsinprogram(ai,person276, year_3).
yearsinprogram(ai,person45, year_5).
yearsinprogram(ai,person233, year_1).
yearsinprogram(ai,person148, year_5).
yearsinprogram(ai,person193, year_1).
yearsinprogram(ai,person314, year_4).
yearsinprogram(ai,person275, year_5).
yearsinprogram(ai,person21, year_5).
yearsinprogram(ai,person262, year_7).
yearsinprogram(ai,person257, year_7).
yearsinprogram(ai,person73, year_4).
yearsinprogram(ai,person380, year_6).
yearsinprogram(ai,person384, year_3).
yearsinprogram(ai,person406, year_5).
yearsinprogram(ai,person266, year_5).
yearsinprogram(ai,person312, year_4).
yearsinprogram(ai,person208, year_4).
yearsinprogram(ai,person311, year_3).
yearsinprogram(ai,person63, year_5).
yearsinprogram(ai,person318, year_5).
yearsinprogram(ai,person83, year_5).
yearsinprogram(ai,person161, year_7).
yearsinprogram(ai,person284, year_3).
ta(ai,course52, person70, winter_0304).
ta(ai,course44, person193, winter_0304).
ta(ai,course128, person271, winter_0304).
ta(ai,course128, person392, winter_0304).
ta(ai,course44, person377, autumn_0304).
ta(ai,course24, person70, autumn_0304).
ta(ai,course156, person257, autumn_0304).
ta(ai,course132, person94, autumn_0304).
ta(ai,course24, person21, spring_0203).
ta(ai,course44, person420, winter_0203).
ta(ai,course44, person382, winter_0203).
ta(ai,course141, person14, winter_0203).
ta(ai,course12, person21, winter_0203).
ta(ai,course44, person286, autumn_0203).
ta(ai,course52, person318, spring_0102).
ta(ai,course44, person382, spring_0102).
ta(ai,course44, person86, spring_0102).
ta(ai,course50, person314, spring_0102).
ta(ai,course39, person73, spring_0102).
ta(ai,course82, person381, winter_0102).
taughtby(ai,course128 , person150, winter_0304).
taughtby(ai,course132 , person319, autumn_0304).
taughtby(ai,course134, person240, spring_0203).
taughtby(ai,course82 , person407, winter_0102).
professor(ai,person319).
student(ai,person284).
student(ai,person311).
student(ai,person14).
student(ai,person275).
student(ai,person259).
student(ai,person139).
student(ai,person176).
student(ai,person400).
student(ai,person318).
student(ai,person161).
student(ai,person347).
professor(ai,person292).
professor(ai,person293).
professor(ai,person240).
professor(ai,person211).
professor(ai,person150).
professor(ai,person415).
professor(ai,person79).
professor(ai,person349).
professor(ai,person7).
professor(ai,person185).
professor(ai,person171).
professor(ai,person168).
professor(ai,person407).
student(ai,person408).
student(ai,person265).
student(ai,person70).
student(ai,person381).
student(ai,person382).
student(ai,person333).
student(ai,person94).
student(ai,person272).
student(ai,person37).
student(ai,person353).
student(ai,person432).
student(ai,person377).
student(ai,person239).
student(ai,person13).
student(ai,person286).
student(ai,person412).
student(ai,person418).
student(ai,person320).
student(ai,person42).
student(ai,person20).
student(ai,person352).
student(ai,person276).
student(ai,person45).
student(ai,person233).
student(ai,person148).
student(ai,person193).
student(ai,person314).
student(ai,person21).
student(ai,person262).
student(ai,person257).
student(ai,person73).
student(ai,person380).
student(ai,person384).
student(ai,person406).
student(ai,person266).
student(ai,person312).
student(ai,person208).
student(ai,person63).
student(ai,person83).
student(ai,person271).
student(ai,person392).
student(ai,person420).
student(ai,person86).
sameperson(ai,person319, person319).
sameperson(ai,person284, person284).
sameperson(ai,person311, person311).
sameperson(ai,person14, person14).
sameperson(ai,person275, person275).
sameperson(ai,person259, person259).
sameperson(ai,person139, person139).
sameperson(ai,person176, person176).
sameperson(ai,person400, person400).
sameperson(ai,person318, person318).
sameperson(ai,person161, person161).
sameperson(ai,person347, person347).
sameperson(ai,person292, person292).
sameperson(ai,person293, person293).
sameperson(ai,person240, person240).
sameperson(ai,person211, person211).
sameperson(ai,person150, person150).
sameperson(ai,person415, person415).
sameperson(ai,person79, person79).
sameperson(ai,person349, person349).
sameperson(ai,person7, person7).
sameperson(ai,person185, person185).
sameperson(ai,person171, person171).
sameperson(ai,person168, person168).
sameperson(ai,person407, person407).
sameperson(ai,person408, person408).
sameperson(ai,person265, person265).
sameperson(ai,person70, person70).
sameperson(ai,person381, person381).
sameperson(ai,person382, person382).
sameperson(ai,person333, person333).
sameperson(ai,person94, person94).
sameperson(ai,person272, person272).
sameperson(ai,person37, person37).
sameperson(ai,person353, person353).
sameperson(ai,person432, person432).
sameperson(ai,person377, person377).
sameperson(ai,person239, person239).
sameperson(ai,person13, person13).
sameperson(ai,person286, person286).
sameperson(ai,person412, person412).
sameperson(ai,person418, person418).
sameperson(ai,person320, person320).
sameperson(ai,person42, person42).
sameperson(ai,person20, person20).
sameperson(ai,person352, person352).
sameperson(ai,person276, person276).
sameperson(ai,person45, person45).
sameperson(ai,person233, person233).
sameperson(ai,person148, person148).
sameperson(ai,person193, person193).
sameperson(ai,person314, person314).
sameperson(ai,person21, person21).
sameperson(ai,person262, person262).
sameperson(ai,person257, person257).
sameperson(ai,person73, person73).
sameperson(ai,person380, person380).
sameperson(ai,person384, person384).
sameperson(ai,person406, person406).
sameperson(ai,person266, person266).
sameperson(ai,person312, person312).
sameperson(ai,person208, person208).
sameperson(ai,person63, person63).
sameperson(ai,person83, person83).
sameperson(ai,person271, person271).
sameperson(ai,person392, person392).
sameperson(ai,person420, person420).
sameperson(ai,person86, person86).
samecourse(ai,course52, course52).
samecourse(ai,course44, course44).
samecourse(ai,course24, course24).
samecourse(ai,course57, course57).
samecourse(ai,course143, course143).
samecourse(ai,course50, course50).
samecourse(ai,course156, course156).
samecourse(ai,course141, course141).
samecourse(ai,course12, course12).
samecourse(ai,course170, course170).
samecourse(ai,course123, course123).
samecourse(ai,course173, course173).
samecourse(ai,course85, course85).
samecourse(ai,course64, course64).
samecourse(ai,course168, course168).
samecourse(ai,course158, course158).
samecourse(ai,course76, course76).
samecourse(ai,course16, course16).
samecourse(ai,course15, course15).
samecourse(ai,course39, course39).
samecourse(ai,course32, course32).
samecourse(ai,course7, course7).
samecourse(ai,course134, course134).
samecourse(ai,course135, course135).
samecourse(ai,course65, course65).
samecourse(ai,course86, course86).
samecourse(ai,course131, course131).
samecourse(ai,course128, course128).
samecourse(ai,course82, course82).
samecourse(ai,course132, course132).
sameproject(ai,project50, project50).
sameproject(ai,project58, project58).
sameproject(ai,project104, project104).
sameproject(ai,project20, project20).
sameproject(ai,project74, project74).
sameproject(ai,project62, project62).
sameproject(ai,project17, project17).
sameproject(ai,project82, project82).
sameproject(ai,project24, project24).
sameproject(ai,project127, project127).
sameproject(ai,project131, project131).
sameproject(ai,project38, project38).
sameproject(ai,project41, project41).
sameproject(ai,project100, project100).
sameproject(ai,project76, project76).
sameproject(ai,project111, project111).
sameproject(ai,project124, project124).
sameproject(ai,project77, project77).
sameproject(ai,project11, project11).
sameproject(ai,project52, project52).
sameproject(ai,project51, project51).
sameproject(ai,project115, project115).
sameproject(ai,project141, project141).
sameproject(ai,project70, project70).
sameproject(ai,project146, project146).
sameproject(ai,project150, project150).
sameproject(ai,project73, project73).
sameproject(ai,project42, project42).
sameproject(ai,project36, project36).
sameproject(ai,project13, project13).
sameproject(ai,project85, project85).
sameproject(ai,project7, project7).
sameproject(ai,project121, project121).
sameproject(ai,project0, project0).
sameproject(ai,project33, project33).
sameproject(ai,project29, project29).
sameproject(ai,project84, project84).
sameproject(ai,project90, project90).
sameproject(ai,project83, project83).
sameproject(ai,project97, project97).
sameproject(ai,project113, project113).
sameproject(ai,project116, project116).
sameproject(ai,project143, project143).
sameproject(ai,project66, project66).
sameproject(ai,project101, project101).
publication(ai,title25 , person284).
publication(ai,title284 , person14).
publication(ai,title110 , person14).
publication(ai,title118 , person14).
publication(ai,title71 , person14).
publication(ai,title316 , person14).
publication(ai,title118 , person318).
publication(ai,title217 , person161).
publication(ai,title55 , person161).
publication(ai,title331 , person161).
publication(ai,title250 , person161).
publication(ai,title268 , person161).
publication(ai,title271 , person161).
publication(ai,title171 , person161).
publication(ai,title120 , person347).
publication(ai,title86 , person347).
publication(ai,title338 , person347).
publication(ai,title224 , person347).
publication(ai,title260 , person347).
publication(ai,title112 , person347).
publication(ai,title97 , person347).
publication(ai,title50 , person292).
publication(ai,title103 , person292).
publication(ai,title166 , person292).
publication(ai,title72 , person292).
publication(ai,title47 , person292).
publication(ai,title41 , person292).
publication(ai,title40 , person293).
publication(ai,title13 , person240).
publication(ai,title140 , person240).
publication(ai,title217 , person240).
publication(ai,title92 , person240).
publication(ai,title167 , person240).
publication(ai,title331 , person240).
publication(ai,title26 , person240).
publication(ai,title275 , person240).
publication(ai,title333 , person240).
publication(ai,title270 , person240).
publication(ai,title208 , person240).
publication(ai,title103 , person240).
publication(ai,title268 , person240).
publication(ai,title340 , person240).
publication(ai,title192 , person240).
publication(ai,title54 , person240).
publication(ai,title177 , person240).
publication(ai,title33 , person240).
publication(ai,title10 , person240).
publication(ai,title84 , person240).
publication(ai,title161 , person240).
publication(ai,title248 , person240).
publication(ai,title102 , person240).
publication(ai,title274 , person240).
publication(ai,title47 , person240).
publication(ai,title0 , person240).
publication(ai,title82 , person240).
publication(ai,title337 , person240).
publication(ai,title344 , person240).
publication(ai,title254 , person240).
publication(ai,title119 , person240).
publication(ai,title114 , person211).
publication(ai,title259 , person211).
publication(ai,title59 , person211).
publication(ai,title160 , person211).
publication(ai,title88 , person211).
publication(ai,title24 , person211).
publication(ai,title323 , person211).
publication(ai,title190 , person211).
publication(ai,title11 , person211).
publication(ai,title199 , person211).
publication(ai,title240 , person211).
publication(ai,title335 , person211).
publication(ai,title241 , person211).
publication(ai,title212 , person211).
publication(ai,title228 , person211).
publication(ai,title345 , person211).
publication(ai,title89 , person211).
publication(ai,title165 , person211).
publication(ai,title113 , person211).
publication(ai,title233 , person211).
publication(ai,title132 , person211).
publication(ai,title310 , person211).
publication(ai,title218 , person211).
publication(ai,title71 , person211).
publication(ai,title341 , person211).
publication(ai,title207 , person211).
publication(ai,title229 , person211).
publication(ai,title292 , person211).
publication(ai,title49 , person211).
publication(ai,title238 , person211).
publication(ai,title255 , person211).
publication(ai,title329 , person211).
publication(ai,title79 , person211).
publication(ai,title325 , person211).
publication(ai,title44 , person211).
publication(ai,title25 , person211).
publication(ai,title118 , person150).
publication(ai,title140 , person415).
publication(ai,title12 , person415).
publication(ai,title182 , person415).
publication(ai,title122 , person415).
publication(ai,title208 , person415).
publication(ai,title103 , person415).
publication(ai,title347 , person415).
publication(ai,title266 , person415).
publication(ai,title340 , person415).
publication(ai,title269 , person415).
publication(ai,title5 , person415).
publication(ai,title70 , person415).
publication(ai,title179 , person415).
publication(ai,title29 , person415).
publication(ai,title72 , person415).
publication(ai,title47 , person415).
publication(ai,title0 , person415).
publication(ai,title38 , person415).
publication(ai,title290 , person415).
publication(ai,title63 , person415).
publication(ai,title82 , person415).
publication(ai,title283 , person415).
publication(ai,title337 , person415).
publication(ai,title94 , person415).
publication(ai,title147 , person415).
publication(ai,title329 , person415).
publication(ai,title297 , person415).
publication(ai,title79 , person415).
publication(ai,title312 , person415).
publication(ai,title107 , person415).
publication(ai,title273 , person415).
publication(ai,title172 , person415).
publication(ai,title295 , person415).
publication(ai,title41 , person415).
publication(ai,title325 , person415).
publication(ai,title44 , person415).
publication(ai,title87 , person415).
publication(ai,title222 , person415).
publication(ai,title236 , person415).
publication(ai,title258 , person415).
publication(ai,title301 , person415).
publication(ai,title318 , person79).
publication(ai,title115 , person79).
publication(ai,title231 , person79).
publication(ai,title226 , person79).
publication(ai,title195 , person79).
publication(ai,title162 , person185).
publication(ai,title178 , person171).
publication(ai,title225 , person171).
publication(ai,title269 , person171).
publication(ai,title150 , person171).
publication(ai,title70 , person171).
publication(ai,title63 , person171).
publication(ai,title94 , person171).
publication(ai,title147 , person171).
publication(ai,title170 , person171).
publication(ai,title125 , person171).
publication(ai,title90 , person171).
publication(ai,title114 , person407).
publication(ai,title12 , person407).
publication(ai,title259 , person407).
publication(ai,title217 , person407).
publication(ai,title92 , person407).
publication(ai,title182 , person407).
publication(ai,title59 , person407).
publication(ai,title160 , person407).
publication(ai,title55 , person407).
publication(ai,title88 , person407).
publication(ai,title167 , person407).
publication(ai,title24 , person407).
publication(ai,title323 , person407).
publication(ai,title331 , person407).
publication(ai,title190 , person407).
publication(ai,title120 , person407).
publication(ai,title250 , person407).
publication(ai,title11 , person407).
publication(ai,title284 , person407).
publication(ai,title199 , person407).
publication(ai,title240 , person407).
publication(ai,title335 , person407).
publication(ai,title270 , person407).
publication(ai,title241 , person407).
publication(ai,title212 , person407).
publication(ai,title110 , person407).
publication(ai,title268 , person407).
publication(ai,title228 , person407).
publication(ai,title347 , person407).
publication(ai,title266 , person407).
publication(ai,title192 , person407).
publication(ai,title345 , person407).
publication(ai,title5 , person407).
publication(ai,title271 , person407).
publication(ai,title89 , person407).
publication(ai,title165 , person407).
publication(ai,title113 , person407).
publication(ai,title233 , person407).
publication(ai,title179 , person407).
publication(ai,title132 , person407).
publication(ai,title177 , person407).
publication(ai,title310 , person407).
publication(ai,title171 , person407).
publication(ai,title33 , person407).
publication(ai,title218 , person407).
publication(ai,title71 , person407).
publication(ai,title341 , person407).
publication(ai,title207 , person407).
publication(ai,title229 , person407).
publication(ai,title292 , person407).
publication(ai,title316 , person407).
publication(ai,title49 , person407).
publication(ai,title38 , person407).
publication(ai,title238 , person407).
publication(ai,title283 , person407).
publication(ai,title255 , person407).
publication(ai,title224 , person407).
publication(ai,title260 , person407).
publication(ai,title297 , person407).
publication(ai,title312 , person407).
publication(ai,title273 , person407).
publication(ai,title25 , person407).
publication(ai,title258 , person407).
publication(ai,title118 , person408).
publication(ai,title118 , person353).
publication(ai,title40 , person239).
publication(ai,title13 , person13).
publication(ai,title26 , person13).
publication(ai,title275 , person13).
publication(ai,title333 , person13).
publication(ai,title54 , person13).
publication(ai,title10 , person13).
publication(ai,title84 , person13).
publication(ai,title161 , person13).
publication(ai,title248 , person13).
publication(ai,title344 , person13).
publication(ai,title50 , person352).
publication(ai,title208 , person352).
publication(ai,title103 , person352).
publication(ai,title166 , person352).
publication(ai,title314 , person352).
publication(ai,title47 , person352).
publication(ai,title86 , person352).
publication(ai,title82 , person352).
publication(ai,title79 , person352).
publication(ai,title261 , person352).
publication(ai,title87 , person352).
publication(ai,title329 , person45).
publication(ai,title79 , person45).
publication(ai,title325 , person45).
publication(ai,title44 , person45).
publication(ai,title150 , person148).
publication(ai,title125 , person148).
publication(ai,title90 , person148).
publication(ai,title162 , person193).
publication(ai,title170 , person314).
publication(ai,title107 , person314).
publication(ai,title172 , person314).
publication(ai,title295 , person314).
publication(ai,title222 , person314).
publication(ai,title301 , person314).
publication(ai,title25 , person21).
publication(ai,title122 , person262).
publication(ai,title314 , person262).
publication(ai,title29 , person262).
publication(ai,title72 , person262).
publication(ai,title290 , person262).
publication(ai,title86 , person262).
publication(ai,title261 , person262).
publication(ai,title41 , person262).
publication(ai,title102 , person257).
publication(ai,title274 , person257).
publication(ai,title254 , person257).
publication(ai,title119 , person257).
publication(ai,title269 , person73).
publication(ai,title63 , person73).
publication(ai,title318 , person380).
publication(ai,title115 , person380).
publication(ai,title231 , person380).
publication(ai,title226 , person380).
publication(ai,title195 , person380).
publication(ai,title314 , person406).
publication(ai,title86 , person406).
publication(ai,title261 , person406).
publication(ai,title118 , person208).
publication(ai,title182 , person63).
publication(ai,title178 , person63).
publication(ai,title225 , person63).
publication(ai,title5 , person63).
publication(ai,title314 , person63).
publication(ai,title86 , person63).
publication(ai,title147 , person63).
publication(ai,title261 , person63).
publication(ai,title97 , person63).
publication(ai,title222 , person63).
publication(ai,title236 , person63).
publication(ai,title301 , person63).
publication(ai,title325 , person83).
neg(advisedby(ai,person13,person13)).
neg(advisedby(ai,person13,person14)).
neg(advisedby(ai,person13,person148)).
neg(advisedby(ai,person13,person176)).
neg(advisedby(ai,person13,person208)).
neg(advisedby(ai,person13,person21)).
neg(advisedby(ai,person13,person239)).
neg(advisedby(ai,person13,person257)).
neg(advisedby(ai,person13,person262)).
neg(advisedby(ai,person13,person265)).
neg(advisedby(ai,person13,person266)).
neg(advisedby(ai,person13,person272)).
neg(advisedby(ai,person13,person275)).
neg(advisedby(ai,person13,person276)).
neg(advisedby(ai,person13,person286)).
neg(advisedby(ai,person13,person312)).
neg(advisedby(ai,person13,person314)).
neg(advisedby(ai,person13,person318)).
neg(advisedby(ai,person13,person320)).
neg(advisedby(ai,person13,person352)).
neg(advisedby(ai,person13,person353)).
neg(advisedby(ai,person13,person37)).
neg(advisedby(ai,person13,person380)).
neg(advisedby(ai,person13,person381)).
neg(advisedby(ai,person13,person384)).
neg(advisedby(ai,person13,person418)).
neg(advisedby(ai,person13,person432)).
neg(advisedby(ai,person13,person45)).
neg(advisedby(ai,person13,person63)).
neg(advisedby(ai,person13,person83)).
neg(advisedby(ai,person13,person150)).
neg(advisedby(ai,person13,person168)).
neg(advisedby(ai,person13,person171)).
neg(advisedby(ai,person13,person185)).
neg(advisedby(ai,person13,person211)).
neg(advisedby(ai,person13,person292)).
neg(advisedby(ai,person13,person319)).
neg(advisedby(ai,person13,person349)).
neg(advisedby(ai,person13,person407)).
neg(advisedby(ai,person13,person415)).
neg(advisedby(ai,person13,person7)).
neg(advisedby(ai,person13,person79)).
neg(advisedby(ai,person14,person13)).
neg(advisedby(ai,person14,person14)).
neg(advisedby(ai,person14,person148)).
neg(advisedby(ai,person14,person176)).
neg(advisedby(ai,person14,person208)).
neg(advisedby(ai,person14,person21)).
neg(advisedby(ai,person14,person239)).
neg(advisedby(ai,person14,person257)).
neg(advisedby(ai,person14,person262)).
neg(advisedby(ai,person14,person265)).
neg(advisedby(ai,person14,person266)).
neg(advisedby(ai,person14,person272)).
neg(advisedby(ai,person14,person275)).
neg(advisedby(ai,person14,person276)).
neg(advisedby(ai,person14,person286)).
neg(advisedby(ai,person14,person312)).
neg(advisedby(ai,person14,person314)).
neg(advisedby(ai,person14,person318)).
neg(advisedby(ai,person14,person320)).
neg(advisedby(ai,person14,person352)).
neg(advisedby(ai,person14,person353)).
neg(advisedby(ai,person14,person37)).
neg(advisedby(ai,person14,person380)).
neg(advisedby(ai,person14,person381)).
neg(advisedby(ai,person14,person384)).
neg(advisedby(ai,person14,person418)).
neg(advisedby(ai,person14,person432)).
neg(advisedby(ai,person14,person45)).
neg(advisedby(ai,person14,person63)).
neg(advisedby(ai,person14,person83)).
neg(advisedby(ai,person14,person168)).
neg(advisedby(ai,person14,person171)).
neg(advisedby(ai,person14,person185)).
neg(advisedby(ai,person14,person211)).
neg(advisedby(ai,person14,person240)).
neg(advisedby(ai,person14,person292)).
neg(advisedby(ai,person14,person319)).
neg(advisedby(ai,person14,person349)).
neg(advisedby(ai,person14,person407)).
neg(advisedby(ai,person14,person415)).
neg(advisedby(ai,person14,person7)).
neg(advisedby(ai,person14,person79)).
neg(advisedby(ai,person148,person13)).
neg(advisedby(ai,person148,person14)).
neg(advisedby(ai,person148,person148)).
neg(advisedby(ai,person148,person176)).
neg(advisedby(ai,person148,person208)).
neg(advisedby(ai,person148,person21)).
neg(advisedby(ai,person148,person239)).
neg(advisedby(ai,person148,person257)).
neg(advisedby(ai,person148,person262)).
neg(advisedby(ai,person148,person265)).
neg(advisedby(ai,person148,person266)).
neg(advisedby(ai,person148,person272)).
neg(advisedby(ai,person148,person275)).
neg(advisedby(ai,person148,person276)).
neg(advisedby(ai,person148,person286)).
neg(advisedby(ai,person148,person312)).
neg(advisedby(ai,person148,person314)).
neg(advisedby(ai,person148,person318)).
neg(advisedby(ai,person148,person320)).
neg(advisedby(ai,person148,person352)).
neg(advisedby(ai,person148,person353)).
neg(advisedby(ai,person148,person37)).
neg(advisedby(ai,person148,person380)).
neg(advisedby(ai,person148,person381)).
neg(advisedby(ai,person148,person384)).
neg(advisedby(ai,person148,person418)).
neg(advisedby(ai,person148,person432)).
neg(advisedby(ai,person148,person45)).
neg(advisedby(ai,person148,person63)).
neg(advisedby(ai,person148,person83)).
neg(advisedby(ai,person148,person150)).
neg(advisedby(ai,person148,person168)).
neg(advisedby(ai,person148,person185)).
neg(advisedby(ai,person148,person211)).
neg(advisedby(ai,person148,person240)).
neg(advisedby(ai,person148,person292)).
neg(advisedby(ai,person148,person319)).
neg(advisedby(ai,person148,person349)).
neg(advisedby(ai,person148,person407)).
neg(advisedby(ai,person148,person415)).
neg(advisedby(ai,person148,person7)).
neg(advisedby(ai,person148,person79)).
neg(advisedby(ai,person176,person13)).
neg(advisedby(ai,person176,person14)).
neg(advisedby(ai,person176,person148)).
neg(advisedby(ai,person176,person176)).
neg(advisedby(ai,person176,person208)).
neg(advisedby(ai,person176,person21)).
neg(advisedby(ai,person176,person239)).
neg(advisedby(ai,person176,person257)).
neg(advisedby(ai,person176,person262)).
neg(advisedby(ai,person176,person265)).
neg(advisedby(ai,person176,person266)).
neg(advisedby(ai,person176,person272)).
neg(advisedby(ai,person176,person275)).
neg(advisedby(ai,person176,person276)).
neg(advisedby(ai,person176,person286)).
neg(advisedby(ai,person176,person312)).
neg(advisedby(ai,person176,person314)).
neg(advisedby(ai,person176,person318)).
neg(advisedby(ai,person176,person320)).
neg(advisedby(ai,person176,person352)).
neg(advisedby(ai,person176,person353)).
neg(advisedby(ai,person176,person37)).
neg(advisedby(ai,person176,person380)).
neg(advisedby(ai,person176,person381)).
neg(advisedby(ai,person176,person384)).
neg(advisedby(ai,person176,person418)).
neg(advisedby(ai,person176,person432)).
neg(advisedby(ai,person176,person45)).
neg(advisedby(ai,person176,person63)).
neg(advisedby(ai,person176,person83)).
neg(advisedby(ai,person176,person150)).
neg(advisedby(ai,person176,person168)).
neg(advisedby(ai,person176,person171)).
neg(advisedby(ai,person176,person185)).
neg(advisedby(ai,person176,person211)).
neg(advisedby(ai,person176,person240)).
neg(advisedby(ai,person176,person292)).
neg(advisedby(ai,person176,person319)).
neg(advisedby(ai,person176,person349)).
neg(advisedby(ai,person176,person415)).
neg(advisedby(ai,person176,person7)).
neg(advisedby(ai,person176,person79)).
neg(advisedby(ai,person208,person13)).
neg(advisedby(ai,person208,person14)).
neg(advisedby(ai,person208,person148)).
neg(advisedby(ai,person208,person176)).
neg(advisedby(ai,person208,person208)).
neg(advisedby(ai,person208,person21)).
neg(advisedby(ai,person208,person239)).
neg(advisedby(ai,person208,person257)).
neg(advisedby(ai,person208,person262)).
neg(advisedby(ai,person208,person265)).
neg(advisedby(ai,person208,person266)).
neg(advisedby(ai,person208,person272)).
neg(advisedby(ai,person208,person275)).
neg(advisedby(ai,person208,person276)).
neg(advisedby(ai,person208,person286)).
neg(advisedby(ai,person208,person312)).
neg(advisedby(ai,person208,person314)).
neg(advisedby(ai,person208,person318)).
neg(advisedby(ai,person208,person320)).
neg(advisedby(ai,person208,person352)).
neg(advisedby(ai,person208,person353)).
neg(advisedby(ai,person208,person37)).
neg(advisedby(ai,person208,person380)).
neg(advisedby(ai,person208,person381)).
neg(advisedby(ai,person208,person384)).
neg(advisedby(ai,person208,person418)).
neg(advisedby(ai,person208,person432)).
neg(advisedby(ai,person208,person45)).
neg(advisedby(ai,person208,person63)).
neg(advisedby(ai,person208,person83)).
neg(advisedby(ai,person208,person150)).
neg(advisedby(ai,person208,person168)).
neg(advisedby(ai,person208,person171)).
neg(advisedby(ai,person208,person185)).
neg(advisedby(ai,person208,person211)).
neg(advisedby(ai,person208,person240)).
neg(advisedby(ai,person208,person292)).
neg(advisedby(ai,person208,person349)).
neg(advisedby(ai,person208,person407)).
neg(advisedby(ai,person208,person415)).
neg(advisedby(ai,person208,person7)).
neg(advisedby(ai,person208,person79)).
neg(advisedby(ai,person21,person13)).
neg(advisedby(ai,person21,person14)).
neg(advisedby(ai,person21,person148)).
neg(advisedby(ai,person21,person176)).
neg(advisedby(ai,person21,person208)).
neg(advisedby(ai,person21,person21)).
neg(advisedby(ai,person21,person239)).
neg(advisedby(ai,person21,person257)).
neg(advisedby(ai,person21,person262)).
neg(advisedby(ai,person21,person265)).
neg(advisedby(ai,person21,person266)).
neg(advisedby(ai,person21,person272)).
neg(advisedby(ai,person21,person275)).
neg(advisedby(ai,person21,person276)).
neg(advisedby(ai,person21,person286)).
neg(advisedby(ai,person21,person312)).
neg(advisedby(ai,person21,person314)).
neg(advisedby(ai,person21,person318)).
neg(advisedby(ai,person21,person320)).
neg(advisedby(ai,person21,person352)).
neg(advisedby(ai,person21,person353)).
neg(advisedby(ai,person21,person37)).
neg(advisedby(ai,person21,person380)).
neg(advisedby(ai,person21,person381)).
neg(advisedby(ai,person21,person384)).
neg(advisedby(ai,person21,person418)).
neg(advisedby(ai,person21,person432)).
neg(advisedby(ai,person21,person45)).
neg(advisedby(ai,person21,person63)).
neg(advisedby(ai,person21,person83)).
neg(advisedby(ai,person21,person150)).
neg(advisedby(ai,person21,person168)).
neg(advisedby(ai,person21,person171)).
neg(advisedby(ai,person21,person185)).
neg(advisedby(ai,person21,person240)).
neg(advisedby(ai,person21,person292)).
neg(advisedby(ai,person21,person319)).
neg(advisedby(ai,person21,person349)).
neg(advisedby(ai,person21,person407)).
neg(advisedby(ai,person21,person415)).
neg(advisedby(ai,person21,person7)).
neg(advisedby(ai,person21,person79)).
neg(advisedby(ai,person239,person13)).
neg(advisedby(ai,person239,person14)).
neg(advisedby(ai,person239,person148)).
neg(advisedby(ai,person239,person176)).
neg(advisedby(ai,person239,person208)).
neg(advisedby(ai,person239,person21)).
neg(advisedby(ai,person239,person239)).
neg(advisedby(ai,person239,person257)).
neg(advisedby(ai,person239,person262)).
neg(advisedby(ai,person239,person265)).
neg(advisedby(ai,person239,person266)).
neg(advisedby(ai,person239,person272)).
neg(advisedby(ai,person239,person275)).
neg(advisedby(ai,person239,person276)).
neg(advisedby(ai,person239,person286)).
neg(advisedby(ai,person239,person312)).
neg(advisedby(ai,person239,person314)).
neg(advisedby(ai,person239,person318)).
neg(advisedby(ai,person239,person320)).
neg(advisedby(ai,person239,person352)).
neg(advisedby(ai,person239,person353)).
neg(advisedby(ai,person239,person37)).
neg(advisedby(ai,person239,person380)).
neg(advisedby(ai,person239,person381)).
neg(advisedby(ai,person239,person384)).
neg(advisedby(ai,person239,person418)).
neg(advisedby(ai,person239,person432)).
neg(advisedby(ai,person239,person45)).
neg(advisedby(ai,person239,person63)).
neg(advisedby(ai,person239,person83)).
neg(advisedby(ai,person239,person150)).
neg(advisedby(ai,person239,person168)).
neg(advisedby(ai,person239,person185)).
neg(advisedby(ai,person239,person211)).
neg(advisedby(ai,person239,person240)).
neg(advisedby(ai,person239,person292)).
neg(advisedby(ai,person239,person319)).
neg(advisedby(ai,person239,person349)).
neg(advisedby(ai,person239,person407)).
neg(advisedby(ai,person239,person415)).
neg(advisedby(ai,person239,person7)).
neg(advisedby(ai,person239,person79)).
neg(advisedby(ai,person257,person13)).
neg(advisedby(ai,person257,person14)).
neg(advisedby(ai,person257,person148)).
neg(advisedby(ai,person257,person176)).
neg(advisedby(ai,person257,person208)).
neg(advisedby(ai,person257,person21)).
neg(advisedby(ai,person257,person239)).
neg(advisedby(ai,person257,person257)).
neg(advisedby(ai,person257,person262)).
neg(advisedby(ai,person257,person265)).
neg(advisedby(ai,person257,person266)).
neg(advisedby(ai,person257,person272)).
neg(advisedby(ai,person257,person275)).
neg(advisedby(ai,person257,person276)).
neg(advisedby(ai,person257,person286)).
neg(advisedby(ai,person257,person312)).
neg(advisedby(ai,person257,person314)).
neg(advisedby(ai,person257,person318)).
neg(advisedby(ai,person257,person320)).
neg(advisedby(ai,person257,person352)).
neg(advisedby(ai,person257,person353)).
neg(advisedby(ai,person257,person37)).
neg(advisedby(ai,person257,person380)).
neg(advisedby(ai,person257,person381)).
neg(advisedby(ai,person257,person384)).
neg(advisedby(ai,person257,person418)).
neg(advisedby(ai,person257,person432)).
neg(advisedby(ai,person257,person45)).
neg(advisedby(ai,person257,person63)).
neg(advisedby(ai,person257,person83)).
neg(advisedby(ai,person257,person150)).
neg(advisedby(ai,person257,person168)).
neg(advisedby(ai,person257,person171)).
neg(advisedby(ai,person257,person185)).
neg(advisedby(ai,person257,person211)).
neg(advisedby(ai,person257,person292)).
neg(advisedby(ai,person257,person319)).
neg(advisedby(ai,person257,person349)).
neg(advisedby(ai,person257,person407)).
neg(advisedby(ai,person257,person415)).
neg(advisedby(ai,person257,person7)).
neg(advisedby(ai,person257,person79)).
neg(advisedby(ai,person262,person13)).
neg(advisedby(ai,person262,person14)).
neg(advisedby(ai,person262,person148)).
neg(advisedby(ai,person262,person176)).
neg(advisedby(ai,person262,person208)).
neg(advisedby(ai,person262,person21)).
neg(advisedby(ai,person262,person239)).
neg(advisedby(ai,person262,person257)).
neg(advisedby(ai,person262,person262)).
neg(advisedby(ai,person262,person265)).
neg(advisedby(ai,person262,person266)).
neg(advisedby(ai,person262,person272)).
neg(advisedby(ai,person262,person275)).
neg(advisedby(ai,person262,person276)).
neg(advisedby(ai,person262,person286)).
neg(advisedby(ai,person262,person312)).
neg(advisedby(ai,person262,person314)).
neg(advisedby(ai,person262,person318)).
neg(advisedby(ai,person262,person320)).
neg(advisedby(ai,person262,person352)).
neg(advisedby(ai,person262,person353)).
neg(advisedby(ai,person262,person37)).
neg(advisedby(ai,person262,person380)).
neg(advisedby(ai,person262,person381)).
neg(advisedby(ai,person262,person384)).
neg(advisedby(ai,person262,person418)).
neg(advisedby(ai,person262,person432)).
neg(advisedby(ai,person262,person45)).
neg(advisedby(ai,person262,person63)).
neg(advisedby(ai,person262,person83)).
neg(advisedby(ai,person262,person150)).
neg(advisedby(ai,person262,person168)).
neg(advisedby(ai,person262,person171)).
neg(advisedby(ai,person262,person185)).
neg(advisedby(ai,person262,person211)).
neg(advisedby(ai,person262,person240)).
neg(advisedby(ai,person262,person319)).
neg(advisedby(ai,person262,person349)).
neg(advisedby(ai,person262,person407)).
neg(advisedby(ai,person262,person7)).
neg(advisedby(ai,person262,person79)).
neg(advisedby(ai,person265,person13)).
neg(advisedby(ai,person265,person14)).
neg(advisedby(ai,person265,person148)).
neg(advisedby(ai,person265,person176)).
neg(advisedby(ai,person265,person208)).
neg(advisedby(ai,person265,person21)).
neg(advisedby(ai,person265,person239)).
neg(advisedby(ai,person265,person257)).
neg(advisedby(ai,person265,person262)).
neg(advisedby(ai,person265,person265)).
neg(advisedby(ai,person265,person266)).
neg(advisedby(ai,person265,person272)).
neg(advisedby(ai,person265,person275)).
neg(advisedby(ai,person265,person276)).
neg(advisedby(ai,person265,person286)).
neg(advisedby(ai,person265,person312)).
neg(advisedby(ai,person265,person314)).
neg(advisedby(ai,person265,person318)).
neg(advisedby(ai,person265,person320)).
neg(advisedby(ai,person265,person352)).
neg(advisedby(ai,person265,person353)).
neg(advisedby(ai,person265,person37)).
neg(advisedby(ai,person265,person380)).
neg(advisedby(ai,person265,person381)).
neg(advisedby(ai,person265,person384)).
neg(advisedby(ai,person265,person418)).
neg(advisedby(ai,person265,person432)).
neg(advisedby(ai,person265,person45)).
neg(advisedby(ai,person265,person63)).
neg(advisedby(ai,person265,person83)).
neg(advisedby(ai,person265,person150)).
neg(advisedby(ai,person265,person171)).
neg(advisedby(ai,person265,person185)).
neg(advisedby(ai,person265,person211)).
neg(advisedby(ai,person265,person240)).
neg(advisedby(ai,person265,person292)).
neg(advisedby(ai,person265,person319)).
neg(advisedby(ai,person265,person349)).
neg(advisedby(ai,person265,person407)).
neg(advisedby(ai,person265,person415)).
neg(advisedby(ai,person265,person7)).
neg(advisedby(ai,person265,person79)).
neg(advisedby(ai,person266,person13)).
neg(advisedby(ai,person266,person14)).
neg(advisedby(ai,person266,person148)).
neg(advisedby(ai,person266,person176)).
neg(advisedby(ai,person266,person208)).
neg(advisedby(ai,person266,person21)).
neg(advisedby(ai,person266,person239)).
neg(advisedby(ai,person266,person257)).
neg(advisedby(ai,person266,person262)).
neg(advisedby(ai,person266,person265)).
neg(advisedby(ai,person266,person266)).
neg(advisedby(ai,person266,person272)).
neg(advisedby(ai,person266,person275)).
neg(advisedby(ai,person266,person276)).
neg(advisedby(ai,person266,person286)).
neg(advisedby(ai,person266,person312)).
neg(advisedby(ai,person266,person314)).
neg(advisedby(ai,person266,person318)).
neg(advisedby(ai,person266,person320)).
neg(advisedby(ai,person266,person352)).
neg(advisedby(ai,person266,person353)).
neg(advisedby(ai,person266,person37)).
neg(advisedby(ai,person266,person380)).
neg(advisedby(ai,person266,person381)).
neg(advisedby(ai,person266,person384)).
neg(advisedby(ai,person266,person418)).
neg(advisedby(ai,person266,person432)).
neg(advisedby(ai,person266,person45)).
neg(advisedby(ai,person266,person63)).
neg(advisedby(ai,person266,person83)).
neg(advisedby(ai,person266,person150)).
neg(advisedby(ai,person266,person168)).
neg(advisedby(ai,person266,person171)).
neg(advisedby(ai,person266,person185)).
neg(advisedby(ai,person266,person211)).
neg(advisedby(ai,person266,person240)).
neg(advisedby(ai,person266,person292)).
neg(advisedby(ai,person266,person319)).
neg(advisedby(ai,person266,person349)).
neg(advisedby(ai,person266,person407)).
neg(advisedby(ai,person266,person415)).
neg(advisedby(ai,person266,person79)).
neg(advisedby(ai,person272,person13)).
neg(advisedby(ai,person272,person14)).
neg(advisedby(ai,person272,person148)).
neg(advisedby(ai,person272,person176)).
neg(advisedby(ai,person272,person208)).
neg(advisedby(ai,person272,person21)).
neg(advisedby(ai,person272,person239)).
neg(advisedby(ai,person272,person257)).
neg(advisedby(ai,person272,person262)).
neg(advisedby(ai,person272,person265)).
neg(advisedby(ai,person272,person266)).
neg(advisedby(ai,person272,person272)).
neg(advisedby(ai,person272,person275)).
neg(advisedby(ai,person272,person276)).
neg(advisedby(ai,person272,person286)).
neg(advisedby(ai,person272,person312)).
neg(advisedby(ai,person272,person314)).
neg(advisedby(ai,person272,person318)).
neg(advisedby(ai,person272,person320)).
neg(advisedby(ai,person272,person352)).
neg(advisedby(ai,person272,person353)).
neg(advisedby(ai,person272,person37)).
neg(advisedby(ai,person272,person380)).
neg(advisedby(ai,person272,person381)).
neg(advisedby(ai,person272,person384)).
neg(advisedby(ai,person272,person418)).
neg(advisedby(ai,person272,person432)).
neg(advisedby(ai,person272,person45)).
neg(advisedby(ai,person272,person63)).
neg(advisedby(ai,person272,person83)).
neg(advisedby(ai,person272,person150)).
neg(advisedby(ai,person272,person168)).
neg(advisedby(ai,person272,person171)).
neg(advisedby(ai,person272,person185)).
neg(advisedby(ai,person272,person211)).
neg(advisedby(ai,person272,person240)).
neg(advisedby(ai,person272,person292)).
neg(advisedby(ai,person272,person319)).
neg(advisedby(ai,person272,person349)).
neg(advisedby(ai,person272,person407)).
neg(advisedby(ai,person272,person415)).
neg(advisedby(ai,person272,person79)).
neg(advisedby(ai,person275,person13)).
neg(advisedby(ai,person275,person14)).
neg(advisedby(ai,person275,person148)).
neg(advisedby(ai,person275,person176)).
neg(advisedby(ai,person275,person208)).
neg(advisedby(ai,person275,person21)).
neg(advisedby(ai,person275,person239)).
neg(advisedby(ai,person275,person257)).
neg(advisedby(ai,person275,person262)).
neg(advisedby(ai,person275,person265)).
neg(advisedby(ai,person275,person266)).
neg(advisedby(ai,person275,person272)).
neg(advisedby(ai,person275,person275)).
neg(advisedby(ai,person275,person276)).
neg(advisedby(ai,person275,person286)).
neg(advisedby(ai,person275,person312)).
neg(advisedby(ai,person275,person314)).
neg(advisedby(ai,person275,person318)).
neg(advisedby(ai,person275,person320)).
neg(advisedby(ai,person275,person352)).
neg(advisedby(ai,person275,person353)).
neg(advisedby(ai,person275,person37)).
neg(advisedby(ai,person275,person380)).
neg(advisedby(ai,person275,person381)).
neg(advisedby(ai,person275,person384)).
neg(advisedby(ai,person275,person418)).
neg(advisedby(ai,person275,person432)).
neg(advisedby(ai,person275,person45)).
neg(advisedby(ai,person275,person63)).
neg(advisedby(ai,person275,person83)).
neg(advisedby(ai,person275,person150)).
neg(advisedby(ai,person275,person168)).
neg(advisedby(ai,person275,person171)).
neg(advisedby(ai,person275,person185)).
neg(advisedby(ai,person275,person211)).
neg(advisedby(ai,person275,person240)).
neg(advisedby(ai,person275,person292)).
neg(advisedby(ai,person275,person319)).
neg(advisedby(ai,person275,person349)).
neg(advisedby(ai,person275,person407)).
neg(advisedby(ai,person275,person415)).
neg(advisedby(ai,person275,person7)).
neg(advisedby(ai,person276,person13)).
neg(advisedby(ai,person276,person14)).
neg(advisedby(ai,person276,person148)).
neg(advisedby(ai,person276,person176)).
neg(advisedby(ai,person276,person208)).
neg(advisedby(ai,person276,person21)).
neg(advisedby(ai,person276,person239)).
neg(advisedby(ai,person276,person257)).
neg(advisedby(ai,person276,person262)).
neg(advisedby(ai,person276,person265)).
neg(advisedby(ai,person276,person266)).
neg(advisedby(ai,person276,person272)).
neg(advisedby(ai,person276,person275)).
neg(advisedby(ai,person276,person276)).
neg(advisedby(ai,person276,person286)).
neg(advisedby(ai,person276,person312)).
neg(advisedby(ai,person276,person314)).
neg(advisedby(ai,person276,person318)).
neg(advisedby(ai,person276,person320)).
neg(advisedby(ai,person276,person352)).
neg(advisedby(ai,person276,person353)).
neg(advisedby(ai,person276,person37)).
neg(advisedby(ai,person276,person380)).
neg(advisedby(ai,person276,person381)).
neg(advisedby(ai,person276,person384)).
neg(advisedby(ai,person276,person418)).
neg(advisedby(ai,person276,person432)).
neg(advisedby(ai,person276,person45)).
neg(advisedby(ai,person276,person63)).
neg(advisedby(ai,person276,person83)).
neg(advisedby(ai,person276,person150)).
neg(advisedby(ai,person276,person168)).
neg(advisedby(ai,person276,person171)).
neg(advisedby(ai,person276,person185)).
neg(advisedby(ai,person276,person211)).
neg(advisedby(ai,person276,person240)).
neg(advisedby(ai,person276,person292)).
neg(advisedby(ai,person276,person319)).
neg(advisedby(ai,person276,person349)).
neg(advisedby(ai,person276,person415)).
neg(advisedby(ai,person276,person7)).
neg(advisedby(ai,person276,person79)).
neg(advisedby(ai,person286,person13)).
neg(advisedby(ai,person286,person14)).
neg(advisedby(ai,person286,person148)).
neg(advisedby(ai,person286,person176)).
neg(advisedby(ai,person286,person208)).
neg(advisedby(ai,person286,person21)).
neg(advisedby(ai,person286,person239)).
neg(advisedby(ai,person286,person257)).
neg(advisedby(ai,person286,person262)).
neg(advisedby(ai,person286,person265)).
neg(advisedby(ai,person286,person266)).
neg(advisedby(ai,person286,person272)).
neg(advisedby(ai,person286,person275)).
neg(advisedby(ai,person286,person276)).
neg(advisedby(ai,person286,person286)).
neg(advisedby(ai,person286,person312)).
neg(advisedby(ai,person286,person314)).
neg(advisedby(ai,person286,person318)).
neg(advisedby(ai,person286,person320)).
neg(advisedby(ai,person286,person352)).
neg(advisedby(ai,person286,person353)).
neg(advisedby(ai,person286,person37)).
neg(advisedby(ai,person286,person380)).
neg(advisedby(ai,person286,person381)).
neg(advisedby(ai,person286,person384)).
neg(advisedby(ai,person286,person418)).
neg(advisedby(ai,person286,person432)).
neg(advisedby(ai,person286,person45)).
neg(advisedby(ai,person286,person63)).
neg(advisedby(ai,person286,person83)).
neg(advisedby(ai,person286,person150)).
neg(advisedby(ai,person286,person168)).
neg(advisedby(ai,person286,person185)).
neg(advisedby(ai,person286,person211)).
neg(advisedby(ai,person286,person240)).
neg(advisedby(ai,person286,person292)).
neg(advisedby(ai,person286,person319)).
neg(advisedby(ai,person286,person349)).
neg(advisedby(ai,person286,person407)).
neg(advisedby(ai,person286,person415)).
neg(advisedby(ai,person286,person7)).
neg(advisedby(ai,person286,person79)).
neg(advisedby(ai,person312,person13)).
neg(advisedby(ai,person312,person14)).
neg(advisedby(ai,person312,person148)).
neg(advisedby(ai,person312,person176)).
neg(advisedby(ai,person312,person208)).
neg(advisedby(ai,person312,person21)).
neg(advisedby(ai,person312,person239)).
neg(advisedby(ai,person312,person257)).
neg(advisedby(ai,person312,person262)).
neg(advisedby(ai,person312,person265)).
neg(advisedby(ai,person312,person266)).
neg(advisedby(ai,person312,person272)).
neg(advisedby(ai,person312,person275)).
neg(advisedby(ai,person312,person276)).
neg(advisedby(ai,person312,person286)).
neg(advisedby(ai,person312,person312)).
neg(advisedby(ai,person312,person314)).
neg(advisedby(ai,person312,person318)).
neg(advisedby(ai,person312,person320)).
neg(advisedby(ai,person312,person352)).
neg(advisedby(ai,person312,person353)).
neg(advisedby(ai,person312,person37)).
neg(advisedby(ai,person312,person380)).
neg(advisedby(ai,person312,person381)).
neg(advisedby(ai,person312,person384)).
neg(advisedby(ai,person312,person418)).
neg(advisedby(ai,person312,person432)).
neg(advisedby(ai,person312,person45)).
neg(advisedby(ai,person312,person63)).
neg(advisedby(ai,person312,person83)).
neg(advisedby(ai,person312,person150)).
neg(advisedby(ai,person312,person168)).
neg(advisedby(ai,person312,person171)).
neg(advisedby(ai,person312,person185)).
neg(advisedby(ai,person312,person211)).
neg(advisedby(ai,person312,person240)).
neg(advisedby(ai,person312,person292)).
neg(advisedby(ai,person312,person349)).
neg(advisedby(ai,person312,person407)).
neg(advisedby(ai,person312,person415)).
neg(advisedby(ai,person312,person7)).
neg(advisedby(ai,person312,person79)).
neg(advisedby(ai,person314,person13)).
neg(advisedby(ai,person314,person14)).
neg(advisedby(ai,person314,person148)).
neg(advisedby(ai,person314,person176)).
neg(advisedby(ai,person314,person208)).
neg(advisedby(ai,person314,person21)).
neg(advisedby(ai,person314,person239)).
neg(advisedby(ai,person314,person257)).
neg(advisedby(ai,person314,person262)).
neg(advisedby(ai,person314,person265)).
neg(advisedby(ai,person314,person266)).
neg(advisedby(ai,person314,person272)).
neg(advisedby(ai,person314,person275)).
neg(advisedby(ai,person314,person276)).
neg(advisedby(ai,person314,person286)).
neg(advisedby(ai,person314,person312)).
neg(advisedby(ai,person314,person314)).
neg(advisedby(ai,person314,person318)).
neg(advisedby(ai,person314,person320)).
neg(advisedby(ai,person314,person352)).
neg(advisedby(ai,person314,person353)).
neg(advisedby(ai,person314,person37)).
neg(advisedby(ai,person314,person380)).
neg(advisedby(ai,person314,person381)).
neg(advisedby(ai,person314,person384)).
neg(advisedby(ai,person314,person418)).
neg(advisedby(ai,person314,person432)).
neg(advisedby(ai,person314,person45)).
neg(advisedby(ai,person314,person63)).
neg(advisedby(ai,person314,person83)).
neg(advisedby(ai,person314,person150)).
neg(advisedby(ai,person314,person168)).
neg(advisedby(ai,person314,person171)).
neg(advisedby(ai,person314,person185)).
neg(advisedby(ai,person314,person211)).
neg(advisedby(ai,person314,person240)).
neg(advisedby(ai,person314,person292)).
neg(advisedby(ai,person314,person319)).
neg(advisedby(ai,person314,person349)).
neg(advisedby(ai,person314,person407)).
neg(advisedby(ai,person314,person7)).
neg(advisedby(ai,person314,person79)).
neg(advisedby(ai,person318,person13)).
neg(advisedby(ai,person318,person14)).
neg(advisedby(ai,person318,person148)).
neg(advisedby(ai,person318,person176)).
neg(advisedby(ai,person318,person208)).
neg(advisedby(ai,person318,person21)).
neg(advisedby(ai,person318,person239)).
neg(advisedby(ai,person318,person257)).
neg(advisedby(ai,person318,person262)).
neg(advisedby(ai,person318,person265)).
neg(advisedby(ai,person318,person266)).
neg(advisedby(ai,person318,person272)).
neg(advisedby(ai,person318,person275)).
neg(advisedby(ai,person318,person276)).
neg(advisedby(ai,person318,person286)).
neg(advisedby(ai,person318,person312)).
neg(advisedby(ai,person318,person314)).
neg(advisedby(ai,person318,person318)).
neg(advisedby(ai,person318,person320)).
neg(advisedby(ai,person318,person352)).
neg(advisedby(ai,person318,person353)).
neg(advisedby(ai,person318,person37)).
neg(advisedby(ai,person318,person380)).
neg(advisedby(ai,person318,person381)).
neg(advisedby(ai,person318,person384)).
neg(advisedby(ai,person318,person418)).
neg(advisedby(ai,person318,person432)).
neg(advisedby(ai,person318,person45)).
neg(advisedby(ai,person318,person63)).
neg(advisedby(ai,person318,person83)).
neg(advisedby(ai,person318,person150)).
neg(advisedby(ai,person318,person168)).
neg(advisedby(ai,person318,person171)).
neg(advisedby(ai,person318,person211)).
neg(advisedby(ai,person318,person240)).
neg(advisedby(ai,person318,person292)).
neg(advisedby(ai,person318,person349)).
neg(advisedby(ai,person318,person407)).
neg(advisedby(ai,person318,person415)).
neg(advisedby(ai,person318,person7)).
neg(advisedby(ai,person318,person79)).
neg(advisedby(ai,person320,person13)).
neg(advisedby(ai,person320,person14)).
neg(advisedby(ai,person320,person148)).
neg(advisedby(ai,person320,person176)).
neg(advisedby(ai,person320,person208)).
neg(advisedby(ai,person320,person21)).
neg(advisedby(ai,person320,person239)).
neg(advisedby(ai,person320,person257)).
neg(advisedby(ai,person320,person262)).
neg(advisedby(ai,person320,person265)).
neg(advisedby(ai,person320,person266)).
neg(advisedby(ai,person320,person272)).
neg(advisedby(ai,person320,person275)).
neg(advisedby(ai,person320,person276)).
neg(advisedby(ai,person320,person286)).
neg(advisedby(ai,person320,person312)).
neg(advisedby(ai,person320,person314)).
neg(advisedby(ai,person320,person318)).
neg(advisedby(ai,person320,person320)).
neg(advisedby(ai,person320,person352)).
neg(advisedby(ai,person320,person353)).
neg(advisedby(ai,person320,person37)).
neg(advisedby(ai,person320,person380)).
neg(advisedby(ai,person320,person381)).
neg(advisedby(ai,person320,person384)).
neg(advisedby(ai,person320,person418)).
neg(advisedby(ai,person320,person432)).
neg(advisedby(ai,person320,person45)).
neg(advisedby(ai,person320,person63)).
neg(advisedby(ai,person320,person83)).
neg(advisedby(ai,person320,person168)).
neg(advisedby(ai,person320,person171)).
neg(advisedby(ai,person320,person185)).
neg(advisedby(ai,person320,person211)).
neg(advisedby(ai,person320,person240)).
neg(advisedby(ai,person320,person292)).
neg(advisedby(ai,person320,person319)).
neg(advisedby(ai,person320,person349)).
neg(advisedby(ai,person320,person407)).
neg(advisedby(ai,person320,person415)).
neg(advisedby(ai,person320,person7)).
neg(advisedby(ai,person320,person79)).
neg(advisedby(ai,person352,person13)).
neg(advisedby(ai,person352,person14)).
neg(advisedby(ai,person352,person148)).
neg(advisedby(ai,person352,person176)).
neg(advisedby(ai,person352,person208)).
neg(advisedby(ai,person352,person21)).
neg(advisedby(ai,person352,person239)).
neg(advisedby(ai,person352,person257)).
neg(advisedby(ai,person352,person262)).
neg(advisedby(ai,person352,person265)).
neg(advisedby(ai,person352,person266)).
neg(advisedby(ai,person352,person272)).
neg(advisedby(ai,person352,person275)).
neg(advisedby(ai,person352,person276)).
neg(advisedby(ai,person352,person286)).
neg(advisedby(ai,person352,person312)).
neg(advisedby(ai,person352,person314)).
neg(advisedby(ai,person352,person318)).
neg(advisedby(ai,person352,person320)).
neg(advisedby(ai,person352,person352)).
neg(advisedby(ai,person352,person353)).
neg(advisedby(ai,person352,person37)).
neg(advisedby(ai,person352,person380)).
neg(advisedby(ai,person352,person381)).
neg(advisedby(ai,person352,person384)).
neg(advisedby(ai,person352,person418)).
neg(advisedby(ai,person352,person432)).
neg(advisedby(ai,person352,person45)).
neg(advisedby(ai,person352,person63)).
neg(advisedby(ai,person352,person83)).
neg(advisedby(ai,person352,person150)).
neg(advisedby(ai,person352,person168)).
neg(advisedby(ai,person352,person171)).
neg(advisedby(ai,person352,person185)).
neg(advisedby(ai,person352,person211)).
neg(advisedby(ai,person352,person240)).
neg(advisedby(ai,person352,person319)).
neg(advisedby(ai,person352,person349)).
neg(advisedby(ai,person352,person407)).
neg(advisedby(ai,person352,person7)).
neg(advisedby(ai,person352,person79)).
neg(advisedby(ai,person353,person13)).
neg(advisedby(ai,person353,person14)).
neg(advisedby(ai,person353,person148)).
neg(advisedby(ai,person353,person176)).
neg(advisedby(ai,person353,person208)).
neg(advisedby(ai,person353,person21)).
neg(advisedby(ai,person353,person239)).
neg(advisedby(ai,person353,person257)).
neg(advisedby(ai,person353,person262)).
neg(advisedby(ai,person353,person265)).
neg(advisedby(ai,person353,person266)).
neg(advisedby(ai,person353,person272)).
neg(advisedby(ai,person353,person275)).
neg(advisedby(ai,person353,person276)).
neg(advisedby(ai,person353,person286)).
neg(advisedby(ai,person353,person312)).
neg(advisedby(ai,person353,person314)).
neg(advisedby(ai,person353,person318)).
neg(advisedby(ai,person353,person320)).
neg(advisedby(ai,person353,person352)).
neg(advisedby(ai,person353,person353)).
neg(advisedby(ai,person353,person37)).
neg(advisedby(ai,person353,person380)).
neg(advisedby(ai,person353,person381)).
neg(advisedby(ai,person353,person384)).
neg(advisedby(ai,person353,person418)).
neg(advisedby(ai,person353,person432)).
neg(advisedby(ai,person353,person45)).
neg(advisedby(ai,person353,person63)).
neg(advisedby(ai,person353,person83)).
neg(advisedby(ai,person353,person150)).
neg(advisedby(ai,person353,person168)).
neg(advisedby(ai,person353,person171)).
neg(advisedby(ai,person353,person185)).
neg(advisedby(ai,person353,person211)).
neg(advisedby(ai,person353,person240)).
neg(advisedby(ai,person353,person292)).
neg(advisedby(ai,person353,person349)).
neg(advisedby(ai,person353,person407)).
neg(advisedby(ai,person353,person415)).
neg(advisedby(ai,person353,person7)).
neg(advisedby(ai,person353,person79)).
neg(advisedby(ai,person37,person13)).
neg(advisedby(ai,person37,person14)).
neg(advisedby(ai,person37,person148)).
neg(advisedby(ai,person37,person176)).
neg(advisedby(ai,person37,person208)).
neg(advisedby(ai,person37,person21)).
neg(advisedby(ai,person37,person239)).
neg(advisedby(ai,person37,person257)).
neg(advisedby(ai,person37,person262)).
neg(advisedby(ai,person37,person265)).
neg(advisedby(ai,person37,person266)).
neg(advisedby(ai,person37,person272)).
neg(advisedby(ai,person37,person275)).
neg(advisedby(ai,person37,person276)).
neg(advisedby(ai,person37,person286)).
neg(advisedby(ai,person37,person312)).
neg(advisedby(ai,person37,person314)).
neg(advisedby(ai,person37,person318)).
neg(advisedby(ai,person37,person320)).
neg(advisedby(ai,person37,person352)).
neg(advisedby(ai,person37,person353)).
neg(advisedby(ai,person37,person37)).
neg(advisedby(ai,person37,person380)).
neg(advisedby(ai,person37,person381)).
neg(advisedby(ai,person37,person384)).
neg(advisedby(ai,person37,person418)).
neg(advisedby(ai,person37,person432)).
neg(advisedby(ai,person37,person45)).
neg(advisedby(ai,person37,person63)).
neg(advisedby(ai,person37,person83)).
neg(advisedby(ai,person37,person150)).
neg(advisedby(ai,person37,person168)).
neg(advisedby(ai,person37,person171)).
neg(advisedby(ai,person37,person185)).
neg(advisedby(ai,person37,person211)).
neg(advisedby(ai,person37,person240)).
neg(advisedby(ai,person37,person292)).
neg(advisedby(ai,person37,person319)).
neg(advisedby(ai,person37,person349)).
neg(advisedby(ai,person37,person407)).
neg(advisedby(ai,person37,person415)).
neg(advisedby(ai,person37,person7)).
neg(advisedby(ai,person380,person13)).
neg(advisedby(ai,person380,person14)).
neg(advisedby(ai,person380,person148)).
neg(advisedby(ai,person380,person176)).
neg(advisedby(ai,person380,person208)).
neg(advisedby(ai,person380,person21)).
neg(advisedby(ai,person380,person239)).
neg(advisedby(ai,person380,person257)).
neg(advisedby(ai,person380,person262)).
neg(advisedby(ai,person380,person265)).
neg(advisedby(ai,person380,person266)).
neg(advisedby(ai,person380,person272)).
neg(advisedby(ai,person380,person275)).
neg(advisedby(ai,person380,person276)).
neg(advisedby(ai,person380,person286)).
neg(advisedby(ai,person380,person312)).
neg(advisedby(ai,person380,person314)).
neg(advisedby(ai,person380,person318)).
neg(advisedby(ai,person380,person320)).
neg(advisedby(ai,person380,person352)).
neg(advisedby(ai,person380,person353)).
neg(advisedby(ai,person380,person37)).
neg(advisedby(ai,person380,person380)).
neg(advisedby(ai,person380,person381)).
neg(advisedby(ai,person380,person384)).
neg(advisedby(ai,person380,person418)).
neg(advisedby(ai,person380,person432)).
neg(advisedby(ai,person380,person45)).
neg(advisedby(ai,person380,person63)).
neg(advisedby(ai,person380,person83)).
neg(advisedby(ai,person380,person150)).
neg(advisedby(ai,person380,person168)).
neg(advisedby(ai,person380,person171)).
neg(advisedby(ai,person380,person185)).
neg(advisedby(ai,person380,person211)).
neg(advisedby(ai,person380,person240)).
neg(advisedby(ai,person380,person292)).
neg(advisedby(ai,person380,person319)).
neg(advisedby(ai,person380,person349)).
neg(advisedby(ai,person380,person407)).
neg(advisedby(ai,person380,person415)).
neg(advisedby(ai,person380,person7)).
neg(advisedby(ai,person381,person13)).
neg(advisedby(ai,person381,person14)).
neg(advisedby(ai,person381,person148)).
neg(advisedby(ai,person381,person176)).
neg(advisedby(ai,person381,person208)).
neg(advisedby(ai,person381,person21)).
neg(advisedby(ai,person381,person239)).
neg(advisedby(ai,person381,person257)).
neg(advisedby(ai,person381,person262)).
neg(advisedby(ai,person381,person265)).
neg(advisedby(ai,person381,person266)).
neg(advisedby(ai,person381,person272)).
neg(advisedby(ai,person381,person275)).
neg(advisedby(ai,person381,person276)).
neg(advisedby(ai,person381,person286)).
neg(advisedby(ai,person381,person312)).
neg(advisedby(ai,person381,person314)).
neg(advisedby(ai,person381,person318)).
neg(advisedby(ai,person381,person320)).
neg(advisedby(ai,person381,person352)).
neg(advisedby(ai,person381,person353)).
neg(advisedby(ai,person381,person37)).
neg(advisedby(ai,person381,person380)).
neg(advisedby(ai,person381,person381)).
neg(advisedby(ai,person381,person384)).
neg(advisedby(ai,person381,person418)).
neg(advisedby(ai,person381,person432)).
neg(advisedby(ai,person381,person45)).
neg(advisedby(ai,person381,person63)).
neg(advisedby(ai,person381,person83)).
neg(advisedby(ai,person381,person150)).
neg(advisedby(ai,person381,person171)).
neg(advisedby(ai,person381,person185)).
neg(advisedby(ai,person381,person211)).
neg(advisedby(ai,person381,person240)).
neg(advisedby(ai,person381,person292)).
neg(advisedby(ai,person381,person319)).
neg(advisedby(ai,person381,person349)).
neg(advisedby(ai,person381,person407)).
neg(advisedby(ai,person381,person415)).
neg(advisedby(ai,person381,person7)).
neg(advisedby(ai,person381,person79)).
neg(advisedby(ai,person384,person13)).
neg(advisedby(ai,person384,person14)).
neg(advisedby(ai,person384,person148)).
neg(advisedby(ai,person384,person176)).
neg(advisedby(ai,person384,person208)).
neg(advisedby(ai,person384,person21)).
neg(advisedby(ai,person384,person239)).
neg(advisedby(ai,person384,person257)).
neg(advisedby(ai,person384,person262)).
neg(advisedby(ai,person384,person265)).
neg(advisedby(ai,person384,person266)).
neg(advisedby(ai,person384,person272)).
neg(advisedby(ai,person384,person275)).
neg(advisedby(ai,person384,person276)).
neg(advisedby(ai,person384,person286)).
neg(advisedby(ai,person384,person312)).
neg(advisedby(ai,person384,person314)).
neg(advisedby(ai,person384,person318)).
neg(advisedby(ai,person384,person320)).
neg(advisedby(ai,person384,person352)).
neg(advisedby(ai,person384,person353)).
neg(advisedby(ai,person384,person37)).
neg(advisedby(ai,person384,person380)).
neg(advisedby(ai,person384,person381)).
neg(advisedby(ai,person384,person384)).
neg(advisedby(ai,person384,person418)).
neg(advisedby(ai,person384,person432)).
neg(advisedby(ai,person384,person45)).
neg(advisedby(ai,person384,person63)).
neg(advisedby(ai,person384,person83)).
neg(advisedby(ai,person384,person150)).
neg(advisedby(ai,person384,person168)).
neg(advisedby(ai,person384,person171)).
neg(advisedby(ai,person384,person185)).
neg(advisedby(ai,person384,person211)).
neg(advisedby(ai,person384,person292)).
neg(advisedby(ai,person384,person319)).
neg(advisedby(ai,person384,person349)).
neg(advisedby(ai,person384,person415)).
neg(advisedby(ai,person384,person7)).
neg(advisedby(ai,person384,person79)).
neg(advisedby(ai,person418,person13)).
neg(advisedby(ai,person418,person14)).
neg(advisedby(ai,person418,person148)).
neg(advisedby(ai,person418,person176)).
neg(advisedby(ai,person418,person208)).
neg(advisedby(ai,person418,person21)).
neg(advisedby(ai,person418,person239)).
neg(advisedby(ai,person418,person257)).
neg(advisedby(ai,person418,person262)).
neg(advisedby(ai,person418,person265)).
neg(advisedby(ai,person418,person266)).
neg(advisedby(ai,person418,person272)).
neg(advisedby(ai,person418,person275)).
neg(advisedby(ai,person418,person276)).
neg(advisedby(ai,person418,person286)).
neg(advisedby(ai,person418,person312)).
neg(advisedby(ai,person418,person314)).
neg(advisedby(ai,person418,person318)).
neg(advisedby(ai,person418,person320)).
neg(advisedby(ai,person418,person352)).
neg(advisedby(ai,person418,person353)).
neg(advisedby(ai,person418,person37)).
neg(advisedby(ai,person418,person380)).
neg(advisedby(ai,person418,person381)).
neg(advisedby(ai,person418,person384)).
neg(advisedby(ai,person418,person418)).
neg(advisedby(ai,person418,person432)).
neg(advisedby(ai,person418,person45)).
neg(advisedby(ai,person418,person63)).
neg(advisedby(ai,person418,person83)).
neg(advisedby(ai,person418,person150)).
neg(advisedby(ai,person418,person168)).
neg(advisedby(ai,person418,person185)).
neg(advisedby(ai,person418,person211)).
neg(advisedby(ai,person418,person240)).
neg(advisedby(ai,person418,person292)).
neg(advisedby(ai,person418,person319)).
neg(advisedby(ai,person418,person349)).
neg(advisedby(ai,person418,person407)).
neg(advisedby(ai,person418,person415)).
neg(advisedby(ai,person418,person7)).
neg(advisedby(ai,person418,person79)).
neg(advisedby(ai,person432,person13)).
neg(advisedby(ai,person432,person14)).
neg(advisedby(ai,person432,person148)).
neg(advisedby(ai,person432,person176)).
neg(advisedby(ai,person432,person208)).
neg(advisedby(ai,person432,person21)).
neg(advisedby(ai,person432,person239)).
neg(advisedby(ai,person432,person257)).
neg(advisedby(ai,person432,person262)).
neg(advisedby(ai,person432,person265)).
neg(advisedby(ai,person432,person266)).
neg(advisedby(ai,person432,person272)).
neg(advisedby(ai,person432,person275)).
neg(advisedby(ai,person432,person276)).
neg(advisedby(ai,person432,person286)).
neg(advisedby(ai,person432,person312)).
neg(advisedby(ai,person432,person314)).
neg(advisedby(ai,person432,person318)).
neg(advisedby(ai,person432,person320)).
neg(advisedby(ai,person432,person352)).
neg(advisedby(ai,person432,person353)).
neg(advisedby(ai,person432,person37)).
neg(advisedby(ai,person432,person380)).
neg(advisedby(ai,person432,person381)).
neg(advisedby(ai,person432,person384)).
neg(advisedby(ai,person432,person418)).
neg(advisedby(ai,person432,person432)).
neg(advisedby(ai,person432,person45)).
neg(advisedby(ai,person432,person63)).
neg(advisedby(ai,person432,person83)).
neg(advisedby(ai,person432,person150)).
neg(advisedby(ai,person432,person168)).
neg(advisedby(ai,person432,person171)).
neg(advisedby(ai,person432,person185)).
neg(advisedby(ai,person432,person211)).
neg(advisedby(ai,person432,person292)).
neg(advisedby(ai,person432,person319)).
neg(advisedby(ai,person432,person349)).
neg(advisedby(ai,person432,person407)).
neg(advisedby(ai,person432,person415)).
neg(advisedby(ai,person432,person7)).
neg(advisedby(ai,person432,person79)).
neg(advisedby(ai,person45,person13)).
neg(advisedby(ai,person45,person14)).
neg(advisedby(ai,person45,person148)).
neg(advisedby(ai,person45,person176)).
neg(advisedby(ai,person45,person208)).
neg(advisedby(ai,person45,person21)).
neg(advisedby(ai,person45,person239)).
neg(advisedby(ai,person45,person257)).
neg(advisedby(ai,person45,person262)).
neg(advisedby(ai,person45,person265)).
neg(advisedby(ai,person45,person266)).
neg(advisedby(ai,person45,person272)).
neg(advisedby(ai,person45,person275)).
neg(advisedby(ai,person45,person276)).
neg(advisedby(ai,person45,person286)).
neg(advisedby(ai,person45,person312)).
neg(advisedby(ai,person45,person314)).
neg(advisedby(ai,person45,person318)).
neg(advisedby(ai,person45,person320)).
neg(advisedby(ai,person45,person352)).
neg(advisedby(ai,person45,person353)).
neg(advisedby(ai,person45,person37)).
neg(advisedby(ai,person45,person380)).
neg(advisedby(ai,person45,person381)).
neg(advisedby(ai,person45,person384)).
neg(advisedby(ai,person45,person418)).
neg(advisedby(ai,person45,person432)).
neg(advisedby(ai,person45,person45)).
neg(advisedby(ai,person45,person63)).
neg(advisedby(ai,person45,person83)).
neg(advisedby(ai,person45,person150)).
neg(advisedby(ai,person45,person168)).
neg(advisedby(ai,person45,person171)).
neg(advisedby(ai,person45,person185)).
neg(advisedby(ai,person45,person240)).
neg(advisedby(ai,person45,person292)).
neg(advisedby(ai,person45,person319)).
neg(advisedby(ai,person45,person349)).
neg(advisedby(ai,person45,person407)).
neg(advisedby(ai,person45,person7)).
neg(advisedby(ai,person45,person79)).
neg(advisedby(ai,person63,person13)).
neg(advisedby(ai,person63,person14)).
neg(advisedby(ai,person63,person148)).
neg(advisedby(ai,person63,person176)).
neg(advisedby(ai,person63,person208)).
neg(advisedby(ai,person63,person21)).
neg(advisedby(ai,person63,person239)).
neg(advisedby(ai,person63,person257)).
neg(advisedby(ai,person63,person262)).
neg(advisedby(ai,person63,person265)).
neg(advisedby(ai,person63,person266)).
neg(advisedby(ai,person63,person272)).
neg(advisedby(ai,person63,person275)).
neg(advisedby(ai,person63,person276)).
neg(advisedby(ai,person63,person286)).
neg(advisedby(ai,person63,person312)).
neg(advisedby(ai,person63,person314)).
neg(advisedby(ai,person63,person318)).
neg(advisedby(ai,person63,person320)).
neg(advisedby(ai,person63,person352)).
neg(advisedby(ai,person63,person353)).
neg(advisedby(ai,person63,person37)).
neg(advisedby(ai,person63,person380)).
neg(advisedby(ai,person63,person381)).
neg(advisedby(ai,person63,person384)).
neg(advisedby(ai,person63,person418)).
neg(advisedby(ai,person63,person432)).
neg(advisedby(ai,person63,person45)).
neg(advisedby(ai,person63,person63)).
neg(advisedby(ai,person63,person83)).
neg(advisedby(ai,person63,person150)).
neg(advisedby(ai,person63,person168)).
neg(advisedby(ai,person63,person171)).
neg(advisedby(ai,person63,person185)).
neg(advisedby(ai,person63,person211)).
neg(advisedby(ai,person63,person240)).
neg(advisedby(ai,person63,person292)).
neg(advisedby(ai,person63,person319)).
neg(advisedby(ai,person63,person349)).
neg(advisedby(ai,person63,person407)).
neg(advisedby(ai,person63,person7)).
neg(advisedby(ai,person63,person79)).
neg(advisedby(ai,person83,person13)).
neg(advisedby(ai,person83,person14)).
neg(advisedby(ai,person83,person148)).
neg(advisedby(ai,person83,person176)).
neg(advisedby(ai,person83,person208)).
neg(advisedby(ai,person83,person21)).
neg(advisedby(ai,person83,person239)).
neg(advisedby(ai,person83,person257)).
neg(advisedby(ai,person83,person262)).
neg(advisedby(ai,person83,person265)).
neg(advisedby(ai,person83,person266)).
neg(advisedby(ai,person83,person272)).
neg(advisedby(ai,person83,person275)).
neg(advisedby(ai,person83,person276)).
neg(advisedby(ai,person83,person286)).
neg(advisedby(ai,person83,person312)).
neg(advisedby(ai,person83,person314)).
neg(advisedby(ai,person83,person318)).
neg(advisedby(ai,person83,person320)).
neg(advisedby(ai,person83,person352)).
neg(advisedby(ai,person83,person353)).
neg(advisedby(ai,person83,person37)).
neg(advisedby(ai,person83,person380)).
neg(advisedby(ai,person83,person381)).
neg(advisedby(ai,person83,person384)).
neg(advisedby(ai,person83,person418)).
neg(advisedby(ai,person83,person432)).
neg(advisedby(ai,person83,person45)).
neg(advisedby(ai,person83,person63)).
neg(advisedby(ai,person83,person83)).
neg(advisedby(ai,person83,person150)).
neg(advisedby(ai,person83,person168)).
neg(advisedby(ai,person83,person171)).
neg(advisedby(ai,person83,person185)).
neg(advisedby(ai,person83,person211)).
neg(advisedby(ai,person83,person240)).
neg(advisedby(ai,person83,person292)).
neg(advisedby(ai,person83,person319)).
neg(advisedby(ai,person83,person407)).
neg(advisedby(ai,person83,person415)).
neg(advisedby(ai,person83,person7)).
neg(advisedby(ai,person83,person79)).
neg(advisedby(ai,person150,person13)).
neg(advisedby(ai,person150,person14)).
neg(advisedby(ai,person150,person148)).
neg(advisedby(ai,person150,person176)).
neg(advisedby(ai,person150,person208)).
neg(advisedby(ai,person150,person21)).
neg(advisedby(ai,person150,person239)).
neg(advisedby(ai,person150,person257)).
neg(advisedby(ai,person150,person262)).
neg(advisedby(ai,person150,person265)).
neg(advisedby(ai,person150,person266)).
neg(advisedby(ai,person150,person272)).
neg(advisedby(ai,person150,person275)).
neg(advisedby(ai,person150,person276)).
neg(advisedby(ai,person150,person286)).
neg(advisedby(ai,person150,person312)).
neg(advisedby(ai,person150,person314)).
neg(advisedby(ai,person150,person318)).
neg(advisedby(ai,person150,person320)).
neg(advisedby(ai,person150,person352)).
neg(advisedby(ai,person150,person353)).
neg(advisedby(ai,person150,person37)).
neg(advisedby(ai,person150,person380)).
neg(advisedby(ai,person150,person381)).
neg(advisedby(ai,person150,person384)).
neg(advisedby(ai,person150,person418)).
neg(advisedby(ai,person150,person432)).
neg(advisedby(ai,person150,person45)).
neg(advisedby(ai,person150,person63)).
neg(advisedby(ai,person150,person83)).
neg(advisedby(ai,person150,person150)).
neg(advisedby(ai,person150,person168)).
neg(advisedby(ai,person150,person171)).
neg(advisedby(ai,person150,person185)).
neg(advisedby(ai,person150,person211)).
neg(advisedby(ai,person150,person240)).
neg(advisedby(ai,person150,person292)).
neg(advisedby(ai,person150,person319)).
neg(advisedby(ai,person150,person349)).
neg(advisedby(ai,person150,person407)).
neg(advisedby(ai,person150,person415)).
neg(advisedby(ai,person150,person7)).
neg(advisedby(ai,person150,person79)).
neg(advisedby(ai,person168,person13)).
neg(advisedby(ai,person168,person14)).
neg(advisedby(ai,person168,person148)).
neg(advisedby(ai,person168,person176)).
neg(advisedby(ai,person168,person208)).
neg(advisedby(ai,person168,person21)).
neg(advisedby(ai,person168,person239)).
neg(advisedby(ai,person168,person257)).
neg(advisedby(ai,person168,person262)).
neg(advisedby(ai,person168,person265)).
neg(advisedby(ai,person168,person266)).
neg(advisedby(ai,person168,person272)).
neg(advisedby(ai,person168,person275)).
neg(advisedby(ai,person168,person276)).
neg(advisedby(ai,person168,person286)).
neg(advisedby(ai,person168,person312)).
neg(advisedby(ai,person168,person314)).
neg(advisedby(ai,person168,person318)).
neg(advisedby(ai,person168,person320)).
neg(advisedby(ai,person168,person352)).
neg(advisedby(ai,person168,person353)).
neg(advisedby(ai,person168,person37)).
neg(advisedby(ai,person168,person380)).
neg(advisedby(ai,person168,person381)).
neg(advisedby(ai,person168,person384)).
neg(advisedby(ai,person168,person418)).
neg(advisedby(ai,person168,person432)).
neg(advisedby(ai,person168,person45)).
neg(advisedby(ai,person168,person63)).
neg(advisedby(ai,person168,person83)).
neg(advisedby(ai,person168,person150)).
neg(advisedby(ai,person168,person168)).
neg(advisedby(ai,person168,person171)).
neg(advisedby(ai,person168,person185)).
neg(advisedby(ai,person168,person211)).
neg(advisedby(ai,person168,person240)).
neg(advisedby(ai,person168,person292)).
neg(advisedby(ai,person168,person319)).
neg(advisedby(ai,person168,person349)).
neg(advisedby(ai,person168,person407)).
neg(advisedby(ai,person168,person415)).
neg(advisedby(ai,person168,person7)).
neg(advisedby(ai,person168,person79)).
neg(advisedby(ai,person171,person13)).
neg(advisedby(ai,person171,person14)).
neg(advisedby(ai,person171,person148)).
neg(advisedby(ai,person171,person176)).
neg(advisedby(ai,person171,person208)).
neg(advisedby(ai,person171,person21)).
neg(advisedby(ai,person171,person239)).
neg(advisedby(ai,person171,person257)).
neg(advisedby(ai,person171,person262)).
neg(advisedby(ai,person171,person265)).
neg(advisedby(ai,person171,person266)).
neg(advisedby(ai,person171,person272)).
neg(advisedby(ai,person171,person275)).
neg(advisedby(ai,person171,person276)).
neg(advisedby(ai,person171,person286)).
neg(advisedby(ai,person171,person312)).
neg(advisedby(ai,person171,person314)).
neg(advisedby(ai,person171,person318)).
neg(advisedby(ai,person171,person320)).
neg(advisedby(ai,person171,person352)).
neg(advisedby(ai,person171,person353)).
neg(advisedby(ai,person171,person37)).
neg(advisedby(ai,person171,person380)).
neg(advisedby(ai,person171,person381)).
neg(advisedby(ai,person171,person384)).
neg(advisedby(ai,person171,person418)).
neg(advisedby(ai,person171,person432)).
neg(advisedby(ai,person171,person45)).
neg(advisedby(ai,person171,person63)).
neg(advisedby(ai,person171,person83)).
neg(advisedby(ai,person171,person150)).
neg(advisedby(ai,person171,person168)).
neg(advisedby(ai,person171,person171)).
neg(advisedby(ai,person171,person185)).
neg(advisedby(ai,person171,person211)).
neg(advisedby(ai,person171,person240)).
neg(advisedby(ai,person171,person292)).
neg(advisedby(ai,person171,person319)).
neg(advisedby(ai,person171,person349)).
neg(advisedby(ai,person171,person407)).
neg(advisedby(ai,person171,person415)).
neg(advisedby(ai,person171,person7)).
neg(advisedby(ai,person171,person79)).
neg(advisedby(ai,person185,person13)).
neg(advisedby(ai,person185,person14)).
neg(advisedby(ai,person185,person148)).
neg(advisedby(ai,person185,person176)).
neg(advisedby(ai,person185,person208)).
neg(advisedby(ai,person185,person21)).
neg(advisedby(ai,person185,person239)).
neg(advisedby(ai,person185,person257)).
neg(advisedby(ai,person185,person262)).
neg(advisedby(ai,person185,person265)).
neg(advisedby(ai,person185,person266)).
neg(advisedby(ai,person185,person272)).
neg(advisedby(ai,person185,person275)).
neg(advisedby(ai,person185,person276)).
neg(advisedby(ai,person185,person286)).
neg(advisedby(ai,person185,person312)).
neg(advisedby(ai,person185,person314)).
neg(advisedby(ai,person185,person318)).
neg(advisedby(ai,person185,person320)).
neg(advisedby(ai,person185,person352)).
neg(advisedby(ai,person185,person353)).
neg(advisedby(ai,person185,person37)).
neg(advisedby(ai,person185,person380)).
neg(advisedby(ai,person185,person381)).
neg(advisedby(ai,person185,person384)).
neg(advisedby(ai,person185,person418)).
neg(advisedby(ai,person185,person432)).
neg(advisedby(ai,person185,person45)).
neg(advisedby(ai,person185,person63)).
neg(advisedby(ai,person185,person83)).
neg(advisedby(ai,person185,person150)).
neg(advisedby(ai,person185,person168)).
neg(advisedby(ai,person185,person171)).
neg(advisedby(ai,person185,person185)).
neg(advisedby(ai,person185,person211)).
neg(advisedby(ai,person185,person240)).
neg(advisedby(ai,person185,person292)).
neg(advisedby(ai,person185,person319)).
neg(advisedby(ai,person185,person349)).
neg(advisedby(ai,person185,person407)).
neg(advisedby(ai,person185,person415)).
neg(advisedby(ai,person185,person7)).
neg(advisedby(ai,person185,person79)).
neg(advisedby(ai,person211,person13)).
neg(advisedby(ai,person211,person14)).
neg(advisedby(ai,person211,person148)).
neg(advisedby(ai,person211,person176)).
neg(advisedby(ai,person211,person208)).
neg(advisedby(ai,person211,person21)).
neg(advisedby(ai,person211,person239)).
neg(advisedby(ai,person211,person257)).
neg(advisedby(ai,person211,person262)).
neg(advisedby(ai,person211,person265)).
neg(advisedby(ai,person211,person266)).
neg(advisedby(ai,person211,person272)).
neg(advisedby(ai,person211,person275)).
neg(advisedby(ai,person211,person276)).
neg(advisedby(ai,person211,person286)).
neg(advisedby(ai,person211,person312)).
neg(advisedby(ai,person211,person314)).
neg(advisedby(ai,person211,person318)).
neg(advisedby(ai,person211,person320)).
neg(advisedby(ai,person211,person352)).
neg(advisedby(ai,person211,person353)).
neg(advisedby(ai,person211,person37)).
neg(advisedby(ai,person211,person380)).
neg(advisedby(ai,person211,person381)).
neg(advisedby(ai,person211,person384)).
neg(advisedby(ai,person211,person418)).
neg(advisedby(ai,person211,person432)).
neg(advisedby(ai,person211,person45)).
neg(advisedby(ai,person211,person63)).
neg(advisedby(ai,person211,person83)).
neg(advisedby(ai,person211,person150)).
neg(advisedby(ai,person211,person168)).
neg(advisedby(ai,person211,person171)).
neg(advisedby(ai,person211,person185)).
neg(advisedby(ai,person211,person211)).
neg(advisedby(ai,person211,person240)).
neg(advisedby(ai,person211,person292)).
neg(advisedby(ai,person211,person319)).
neg(advisedby(ai,person211,person349)).
neg(advisedby(ai,person211,person407)).
neg(advisedby(ai,person211,person415)).
neg(advisedby(ai,person211,person7)).
neg(advisedby(ai,person211,person79)).
neg(advisedby(ai,person240,person13)).
neg(advisedby(ai,person240,person14)).
neg(advisedby(ai,person240,person148)).
neg(advisedby(ai,person240,person176)).
neg(advisedby(ai,person240,person208)).
neg(advisedby(ai,person240,person21)).
neg(advisedby(ai,person240,person239)).
neg(advisedby(ai,person240,person257)).
neg(advisedby(ai,person240,person262)).
neg(advisedby(ai,person240,person265)).
neg(advisedby(ai,person240,person266)).
neg(advisedby(ai,person240,person272)).
neg(advisedby(ai,person240,person275)).
neg(advisedby(ai,person240,person276)).
neg(advisedby(ai,person240,person286)).
neg(advisedby(ai,person240,person312)).
neg(advisedby(ai,person240,person314)).
neg(advisedby(ai,person240,person318)).
neg(advisedby(ai,person240,person320)).
neg(advisedby(ai,person240,person352)).
neg(advisedby(ai,person240,person353)).
neg(advisedby(ai,person240,person37)).
neg(advisedby(ai,person240,person380)).
neg(advisedby(ai,person240,person381)).
neg(advisedby(ai,person240,person384)).
neg(advisedby(ai,person240,person418)).
neg(advisedby(ai,person240,person432)).
neg(advisedby(ai,person240,person45)).
neg(advisedby(ai,person240,person63)).
neg(advisedby(ai,person240,person83)).
neg(advisedby(ai,person240,person150)).
neg(advisedby(ai,person240,person168)).
neg(advisedby(ai,person240,person171)).
neg(advisedby(ai,person240,person185)).
neg(advisedby(ai,person240,person211)).
neg(advisedby(ai,person240,person240)).
neg(advisedby(ai,person240,person292)).
neg(advisedby(ai,person240,person319)).
neg(advisedby(ai,person240,person349)).
neg(advisedby(ai,person240,person407)).
neg(advisedby(ai,person240,person415)).
neg(advisedby(ai,person240,person7)).
neg(advisedby(ai,person240,person79)).
neg(advisedby(ai,person292,person13)).
neg(advisedby(ai,person292,person14)).
neg(advisedby(ai,person292,person148)).
neg(advisedby(ai,person292,person176)).
neg(advisedby(ai,person292,person208)).
neg(advisedby(ai,person292,person21)).
neg(advisedby(ai,person292,person239)).
neg(advisedby(ai,person292,person257)).
neg(advisedby(ai,person292,person262)).
neg(advisedby(ai,person292,person265)).
neg(advisedby(ai,person292,person266)).
neg(advisedby(ai,person292,person272)).
neg(advisedby(ai,person292,person275)).
neg(advisedby(ai,person292,person276)).
neg(advisedby(ai,person292,person286)).
neg(advisedby(ai,person292,person312)).
neg(advisedby(ai,person292,person314)).
neg(advisedby(ai,person292,person318)).
neg(advisedby(ai,person292,person320)).
neg(advisedby(ai,person292,person352)).
neg(advisedby(ai,person292,person353)).
neg(advisedby(ai,person292,person37)).
neg(advisedby(ai,person292,person380)).
neg(advisedby(ai,person292,person381)).
neg(advisedby(ai,person292,person384)).
neg(advisedby(ai,person292,person418)).
neg(advisedby(ai,person292,person432)).
neg(advisedby(ai,person292,person45)).
neg(advisedby(ai,person292,person63)).
neg(advisedby(ai,person292,person83)).
neg(advisedby(ai,person292,person150)).
neg(advisedby(ai,person292,person168)).
neg(advisedby(ai,person292,person171)).
neg(advisedby(ai,person292,person185)).
neg(advisedby(ai,person292,person211)).
neg(advisedby(ai,person292,person240)).
neg(advisedby(ai,person292,person292)).
neg(advisedby(ai,person292,person319)).
neg(advisedby(ai,person292,person349)).
neg(advisedby(ai,person292,person407)).
neg(advisedby(ai,person292,person415)).
neg(advisedby(ai,person292,person7)).
neg(advisedby(ai,person292,person79)).
neg(advisedby(ai,person319,person13)).
neg(advisedby(ai,person319,person14)).
neg(advisedby(ai,person319,person148)).
neg(advisedby(ai,person319,person176)).
neg(advisedby(ai,person319,person208)).
neg(advisedby(ai,person319,person21)).
neg(advisedby(ai,person319,person239)).
neg(advisedby(ai,person319,person257)).
neg(advisedby(ai,person319,person262)).
neg(advisedby(ai,person319,person265)).
neg(advisedby(ai,person319,person266)).
neg(advisedby(ai,person319,person272)).
neg(advisedby(ai,person319,person275)).
neg(advisedby(ai,person319,person276)).
neg(advisedby(ai,person319,person286)).
neg(advisedby(ai,person319,person312)).
neg(advisedby(ai,person319,person314)).
neg(advisedby(ai,person319,person318)).
neg(advisedby(ai,person319,person320)).
neg(advisedby(ai,person319,person352)).
neg(advisedby(ai,person319,person353)).
neg(advisedby(ai,person319,person37)).
neg(advisedby(ai,person319,person380)).
neg(advisedby(ai,person319,person381)).
neg(advisedby(ai,person319,person384)).
neg(advisedby(ai,person319,person418)).
neg(advisedby(ai,person319,person432)).
neg(advisedby(ai,person319,person45)).
neg(advisedby(ai,person319,person63)).
neg(advisedby(ai,person319,person83)).
neg(advisedby(ai,person319,person150)).
neg(advisedby(ai,person319,person168)).
neg(advisedby(ai,person319,person171)).
neg(advisedby(ai,person319,person185)).
neg(advisedby(ai,person319,person211)).
neg(advisedby(ai,person319,person240)).
neg(advisedby(ai,person319,person292)).
neg(advisedby(ai,person319,person319)).
neg(advisedby(ai,person319,person349)).
neg(advisedby(ai,person319,person407)).
neg(advisedby(ai,person319,person415)).
neg(advisedby(ai,person319,person7)).
neg(advisedby(ai,person319,person79)).
neg(advisedby(ai,person349,person13)).
neg(advisedby(ai,person349,person14)).
neg(advisedby(ai,person349,person148)).
neg(advisedby(ai,person349,person176)).
neg(advisedby(ai,person349,person208)).
neg(advisedby(ai,person349,person21)).
neg(advisedby(ai,person349,person239)).
neg(advisedby(ai,person349,person257)).
neg(advisedby(ai,person349,person262)).
neg(advisedby(ai,person349,person265)).
neg(advisedby(ai,person349,person266)).
neg(advisedby(ai,person349,person272)).
neg(advisedby(ai,person349,person275)).
neg(advisedby(ai,person349,person276)).
neg(advisedby(ai,person349,person286)).
neg(advisedby(ai,person349,person312)).
neg(advisedby(ai,person349,person314)).
neg(advisedby(ai,person349,person318)).
neg(advisedby(ai,person349,person320)).
neg(advisedby(ai,person349,person352)).
neg(advisedby(ai,person349,person353)).
neg(advisedby(ai,person349,person37)).
neg(advisedby(ai,person349,person380)).
neg(advisedby(ai,person349,person381)).
neg(advisedby(ai,person349,person384)).
neg(advisedby(ai,person349,person418)).
neg(advisedby(ai,person349,person432)).
neg(advisedby(ai,person349,person45)).
neg(advisedby(ai,person349,person63)).
neg(advisedby(ai,person349,person83)).
neg(advisedby(ai,person349,person150)).
neg(advisedby(ai,person349,person168)).
neg(advisedby(ai,person349,person171)).
neg(advisedby(ai,person349,person185)).
neg(advisedby(ai,person349,person211)).
neg(advisedby(ai,person349,person240)).
neg(advisedby(ai,person349,person292)).
neg(advisedby(ai,person349,person319)).
neg(advisedby(ai,person349,person349)).
neg(advisedby(ai,person349,person407)).
neg(advisedby(ai,person349,person415)).
neg(advisedby(ai,person349,person7)).
neg(advisedby(ai,person349,person79)).
neg(advisedby(ai,person407,person13)).
neg(advisedby(ai,person407,person14)).
neg(advisedby(ai,person407,person148)).
neg(advisedby(ai,person407,person176)).
neg(advisedby(ai,person407,person208)).
neg(advisedby(ai,person407,person21)).
neg(advisedby(ai,person407,person239)).
neg(advisedby(ai,person407,person257)).
neg(advisedby(ai,person407,person262)).
neg(advisedby(ai,person407,person265)).
neg(advisedby(ai,person407,person266)).
neg(advisedby(ai,person407,person272)).
neg(advisedby(ai,person407,person275)).
neg(advisedby(ai,person407,person276)).
neg(advisedby(ai,person407,person286)).
neg(advisedby(ai,person407,person312)).
neg(advisedby(ai,person407,person314)).
neg(advisedby(ai,person407,person318)).
neg(advisedby(ai,person407,person320)).
neg(advisedby(ai,person407,person352)).
neg(advisedby(ai,person407,person353)).
neg(advisedby(ai,person407,person37)).
neg(advisedby(ai,person407,person380)).
neg(advisedby(ai,person407,person381)).
neg(advisedby(ai,person407,person384)).
neg(advisedby(ai,person407,person418)).
neg(advisedby(ai,person407,person432)).
neg(advisedby(ai,person407,person45)).
neg(advisedby(ai,person407,person63)).
neg(advisedby(ai,person407,person83)).
neg(advisedby(ai,person407,person150)).
neg(advisedby(ai,person407,person168)).
neg(advisedby(ai,person407,person171)).
neg(advisedby(ai,person407,person185)).
neg(advisedby(ai,person407,person211)).
neg(advisedby(ai,person407,person240)).
neg(advisedby(ai,person407,person292)).
neg(advisedby(ai,person407,person319)).
neg(advisedby(ai,person407,person349)).
neg(advisedby(ai,person407,person407)).
neg(advisedby(ai,person407,person415)).
neg(advisedby(ai,person407,person7)).
neg(advisedby(ai,person407,person79)).
neg(advisedby(ai,person415,person13)).
neg(advisedby(ai,person415,person14)).
neg(advisedby(ai,person415,person148)).
neg(advisedby(ai,person415,person176)).
neg(advisedby(ai,person415,person208)).
neg(advisedby(ai,person415,person21)).
neg(advisedby(ai,person415,person239)).
neg(advisedby(ai,person415,person257)).
neg(advisedby(ai,person415,person262)).
neg(advisedby(ai,person415,person265)).
neg(advisedby(ai,person415,person266)).
neg(advisedby(ai,person415,person272)).
neg(advisedby(ai,person415,person275)).
neg(advisedby(ai,person415,person276)).
neg(advisedby(ai,person415,person286)).
neg(advisedby(ai,person415,person312)).
neg(advisedby(ai,person415,person314)).
neg(advisedby(ai,person415,person318)).
neg(advisedby(ai,person415,person320)).
neg(advisedby(ai,person415,person352)).
neg(advisedby(ai,person415,person353)).
neg(advisedby(ai,person415,person37)).
neg(advisedby(ai,person415,person380)).
neg(advisedby(ai,person415,person381)).
neg(advisedby(ai,person415,person384)).
neg(advisedby(ai,person415,person418)).
neg(advisedby(ai,person415,person432)).
neg(advisedby(ai,person415,person45)).
neg(advisedby(ai,person415,person63)).
neg(advisedby(ai,person415,person83)).
neg(advisedby(ai,person415,person150)).
neg(advisedby(ai,person415,person168)).
neg(advisedby(ai,person415,person171)).
neg(advisedby(ai,person415,person185)).
neg(advisedby(ai,person415,person211)).
neg(advisedby(ai,person415,person240)).
neg(advisedby(ai,person415,person292)).
neg(advisedby(ai,person415,person319)).
neg(advisedby(ai,person415,person349)).
neg(advisedby(ai,person415,person407)).
neg(advisedby(ai,person415,person415)).
neg(advisedby(ai,person415,person7)).
neg(advisedby(ai,person415,person79)).
neg(advisedby(ai,person7,person13)).
neg(advisedby(ai,person7,person14)).
neg(advisedby(ai,person7,person148)).
neg(advisedby(ai,person7,person176)).
neg(advisedby(ai,person7,person208)).
neg(advisedby(ai,person7,person21)).
neg(advisedby(ai,person7,person239)).
neg(advisedby(ai,person7,person257)).
neg(advisedby(ai,person7,person262)).
neg(advisedby(ai,person7,person265)).
neg(advisedby(ai,person7,person266)).
neg(advisedby(ai,person7,person272)).
neg(advisedby(ai,person7,person275)).
neg(advisedby(ai,person7,person276)).
neg(advisedby(ai,person7,person286)).
neg(advisedby(ai,person7,person312)).
neg(advisedby(ai,person7,person314)).
neg(advisedby(ai,person7,person318)).
neg(advisedby(ai,person7,person320)).
neg(advisedby(ai,person7,person352)).
neg(advisedby(ai,person7,person353)).
neg(advisedby(ai,person7,person37)).
neg(advisedby(ai,person7,person380)).
neg(advisedby(ai,person7,person381)).
neg(advisedby(ai,person7,person384)).
neg(advisedby(ai,person7,person418)).
neg(advisedby(ai,person7,person432)).
neg(advisedby(ai,person7,person45)).
neg(advisedby(ai,person7,person63)).
neg(advisedby(ai,person7,person83)).
neg(advisedby(ai,person7,person150)).
neg(advisedby(ai,person7,person168)).
neg(advisedby(ai,person7,person171)).
neg(advisedby(ai,person7,person185)).
neg(advisedby(ai,person7,person211)).
neg(advisedby(ai,person7,person240)).
neg(advisedby(ai,person7,person292)).
neg(advisedby(ai,person7,person319)).
neg(advisedby(ai,person7,person349)).
neg(advisedby(ai,person7,person407)).
neg(advisedby(ai,person7,person415)).
neg(advisedby(ai,person7,person7)).
neg(advisedby(ai,person7,person79)).
neg(advisedby(ai,person79,person13)).
neg(advisedby(ai,person79,person14)).
neg(advisedby(ai,person79,person148)).
neg(advisedby(ai,person79,person176)).
neg(advisedby(ai,person79,person208)).
neg(advisedby(ai,person79,person21)).
neg(advisedby(ai,person79,person239)).
neg(advisedby(ai,person79,person257)).
neg(advisedby(ai,person79,person262)).
neg(advisedby(ai,person79,person265)).
neg(advisedby(ai,person79,person266)).
neg(advisedby(ai,person79,person272)).
neg(advisedby(ai,person79,person275)).
neg(advisedby(ai,person79,person276)).
neg(advisedby(ai,person79,person286)).
neg(advisedby(ai,person79,person312)).
neg(advisedby(ai,person79,person314)).
neg(advisedby(ai,person79,person318)).
neg(advisedby(ai,person79,person320)).
neg(advisedby(ai,person79,person352)).
neg(advisedby(ai,person79,person353)).
neg(advisedby(ai,person79,person37)).
neg(advisedby(ai,person79,person380)).
neg(advisedby(ai,person79,person381)).
neg(advisedby(ai,person79,person384)).
neg(advisedby(ai,person79,person418)).
neg(advisedby(ai,person79,person432)).
neg(advisedby(ai,person79,person45)).
neg(advisedby(ai,person79,person63)).
neg(advisedby(ai,person79,person83)).
neg(advisedby(ai,person79,person150)).
neg(advisedby(ai,person79,person168)).
neg(advisedby(ai,person79,person171)).
neg(advisedby(ai,person79,person185)).
neg(advisedby(ai,person79,person211)).
neg(advisedby(ai,person79,person240)).
neg(advisedby(ai,person79,person292)).
neg(advisedby(ai,person79,person319)).
neg(advisedby(ai,person79,person349)).
neg(advisedby(ai,person79,person407)).
neg(advisedby(ai,person79,person415)).
neg(advisedby(ai,person79,person7)).
neg(advisedby(ai,person79,person79)).

taughtby(graphics,course157, person342, autumn_0001).
taughtby(graphics,course110, person351, winter_0001).
taughtby(graphics,course13, person72, winter_0001).
taughtby(graphics,course67, person394, winter_0001).
taughtby(graphics,course157, person72, spring_0001).
taughtby(graphics,course164, person351, spring_0001).
taughtby(graphics,course0, person40, spring_0001).
taughtby(graphics,course115, person342, spring_0001).
taughtby(graphics,course101, person279, spring_0001).
taughtby(graphics,course153, person394, spring_0001).
taughtby(graphics,course157, person72, autumn_0102).
taughtby(graphics,course110, person351, autumn_0102).
taughtby(graphics,course125, person351, winter_0102).
taughtby(graphics,course28, person394, winter_0102).
taughtby(graphics,course13, person342, winter_0102).
taughtby(graphics,course1, person40, winter_0102).
taughtby(graphics,course157, person394, spring_0102).
taughtby(graphics,course164, person351, spring_0102).
taughtby(graphics,course115, person72, spring_0102).
taughtby(graphics,course153, person342, spring_0102).
taughtby(graphics,course157, person72, autumn_0203).
taughtby(graphics,course110, person351, autumn_0203).
taughtby(graphics,course108, person279, autumn_0203).
taughtby(graphics,course89, person394, winter_0203).
taughtby(graphics,course125, person351, winter_0203).
taughtby(graphics,course13, person342, winter_0203).
taughtby(graphics,course157, person72, spring_0203).
taughtby(graphics,course164, person351, spring_0203).
taughtby(graphics,course115, person342, spring_0203).
taughtby(graphics,course101, person394, spring_0203).
taughtby(graphics,course110, person351, autumn_0304).
taughtby(graphics,course79, person72, autumn_0304).
taughtby(graphics,course89, person394, winter_0304).
taughtby(graphics,course125, person351, winter_0304).
taughtby(graphics,course13, person342, winter_0304).
taughtby(graphics,course157, person342, spring_0304).
taughtby(graphics,course164, person351, spring_0304).
taughtby(graphics,course101, person279, spring_0304).
taughtby(graphics,course136, person394, spring_0304).
courselevel(graphics,course89, level_400).
courselevel(graphics,course157, level_400).
courselevel(graphics,course110, level_400).
courselevel(graphics,course41, level_400).
courselevel(graphics,course148, level_400).
courselevel(graphics,course125, level_400).
courselevel(graphics,course93, level_400).
courselevel(graphics,course164, level_400).
courselevel(graphics,course159, level_400).
courselevel(graphics,course28, level_400).
courselevel(graphics,course154, level_400).
courselevel(graphics,course118, level_400).
courselevel(graphics,course107, level_400).
courselevel(graphics,course0, level_500).
courselevel(graphics,course13, level_500).
courselevel(graphics,course115, level_500).
courselevel(graphics,course101, level_500).
courselevel(graphics,course136, level_500).
courselevel(graphics,course150, level_500).
courselevel(graphics,course109, level_500).
courselevel(graphics,course3, level_500).
courselevel(graphics,course108, level_500).
courselevel(graphics,course56, level_500).
courselevel(graphics,course67, level_500).
courselevel(graphics,course153, level_500).
courselevel(graphics,course1, level_500).
courselevel(graphics,course83, level_500).
courselevel(graphics,course79, level_500).
courselevel(graphics,course114, level_500).
hasposition(graphics,person40, faculty).
hasposition(graphics,person342, faculty).
hasposition(graphics,person111, faculty_adjunct).
hasposition(graphics,person115, faculty).
hasposition(graphics,person351, faculty).
hasposition(graphics,person72, faculty).
hasposition(graphics,person393, faculty).
hasposition(graphics,person394, faculty).
hasposition(graphics,person279, faculty).
advisedby(graphics,person217, person342).
advisedby(graphics,person217, person72).
advisedby(graphics,person206, person342).
advisedby(graphics,person206, person72).
advisedby(graphics,person81, person342).
advisedby(graphics,person81, person393).
advisedby(graphics,person122, person72).
advisedby(graphics,person228, person342).
advisedby(graphics,person228, person393).
advisedby(graphics,person228, person394).
advisedby(graphics,person41, person394).
advisedby(graphics,person163, person393).
advisedby(graphics,person435, person279).
advisedby(graphics,person404, person72).
advisedby(graphics,person142, person342).
advisedby(graphics,person300, person342).
advisedby(graphics,person200, person72).
advisedby(graphics,person157, person72).
advisedby(graphics,person113, person394).
advisedby(graphics,person113, person342).
inphase(graphics,person241, post_quals).
inphase(graphics,person217, post_generals).
inphase(graphics,person270, pre_quals).
inphase(graphics,person206, post_generals).
inphase(graphics,person81, post_generals).
inphase(graphics,person122, post_quals).
inphase(graphics,person228, post_quals).
inphase(graphics,person51, pre_quals).
inphase(graphics,person41, post_quals).
inphase(graphics,person163, post_quals).
inphase(graphics,person435, post_quals).
inphase(graphics,person404, post_generals).
inphase(graphics,person363, pre_quals).
inphase(graphics,person427, post_quals).
inphase(graphics,person142, post_generals).
inphase(graphics,person431, pre_quals).
inphase(graphics,person283, pre_quals).
inphase(graphics,person149, post_quals).
inphase(graphics,person300, post_generals).
inphase(graphics,person200, post_quals).
inphase(graphics,person157, post_quals).
inphase(graphics,person113, post_generals).
tempadvisedby(graphics,person241, person393).
tempadvisedby(graphics,person270, person393).
tempadvisedby(graphics,person51, person72).
tempadvisedby(graphics,person363, person72).
tempadvisedby(graphics,person427, person393).
tempadvisedby(graphics,person431, person393).
tempadvisedby(graphics,person283, person394).
yearsinprogram(graphics,person241, year_3).
yearsinprogram(graphics,person217, year_5).
yearsinprogram(graphics,person270, year_1).
yearsinprogram(graphics,person206, year_6).
yearsinprogram(graphics,person81, year_6).
yearsinprogram(graphics,person122, year_4).
yearsinprogram(graphics,person228, year_3).
yearsinprogram(graphics,person51, year_2).
yearsinprogram(graphics,person41, year_5).
yearsinprogram(graphics,person163, year_4).
yearsinprogram(graphics,person435, year_4).
yearsinprogram(graphics,person404, year_4).
yearsinprogram(graphics,person363, year_3).
yearsinprogram(graphics,person427, year_4).
yearsinprogram(graphics,person142, year_9).
yearsinprogram(graphics,person431, year_2).
yearsinprogram(graphics,person283, year_1).
yearsinprogram(graphics,person149, year_5).
yearsinprogram(graphics,person300, year_8).
yearsinprogram(graphics,person200, year_4).
yearsinprogram(graphics,person157, year_4).
yearsinprogram(graphics,person113, year_4).
ta(graphics,course89, person228, winter_0304).
ta(graphics,course41, person296, winter_0304).
ta(graphics,course41, person36, winter_0304).
ta(graphics,course13, person431, winter_0304).
ta(graphics,course157, person328, autumn_0304).
ta(graphics,course157, person31, autumn_0304).
ta(graphics,course110, person61, autumn_0304).
ta(graphics,course110, person36, autumn_0304).
ta(graphics,course79, person157, autumn_0304).
ta(graphics,course79, person119, autumn_0304).
ta(graphics,course118, person296, summer_0203).
ta(graphics,course118, person317, summer_0203).
ta(graphics,course157, person119, spring_0203).
ta(graphics,course157, person230, spring_0203).
ta(graphics,course157, person3, spring_0203).
ta(graphics,course157, person258, spring_0203).
ta(graphics,course101, person241, spring_0203).
ta(graphics,course89, person188, winter_0203).
ta(graphics,course89, person41, winter_0203).
ta(graphics,course148, person327, winter_0203).
ta(graphics,course148, person140, winter_0203).
ta(graphics,course13, person51, winter_0203).
ta(graphics,course157, person321, autumn_0203).
ta(graphics,course157, person428, autumn_0203).
ta(graphics,course157, person3, autumn_0203).
ta(graphics,course157, person158, autumn_0203).
ta(graphics,course110, person327, autumn_0203).
ta(graphics,course110, person317, autumn_0203).
ta(graphics,course3, person431, autumn_0203).
ta(graphics,course150, person327, summer_0102).
ta(graphics,course150, person102, summer_0102).
ta(graphics,course157, person90, spring_0102).
ta(graphics,course157, person214, spring_0102).
ta(graphics,course157, person146, spring_0102).
ta(graphics,course157, person88, spring_0102).
ta(graphics,course93, person228, spring_0102).
ta(graphics,course93, person31, spring_0102).
ta(graphics,course93, person178, spring_0102).
ta(graphics,course153, person195, spring_0102).
ta(graphics,course153, person428, spring_0102).
ta(graphics,course148, person31, winter_0102).
ta(graphics,course159, person113, winter_0102).
ta(graphics,course13, person217, winter_0102).
taughtby(graphics,course41 , person351, winter_0304).
taughtby(graphics,course118, person351, summer_0203).
taughtby(graphics,course148, person351, winter_0203).
taughtby(graphics,course3 , person279, autumn_0203).
taughtby(graphics,course150 , person351, summer_0102).
taughtby(graphics,course93, person351, spring_0102).
taughtby(graphics,course148, person351, winter_0102).
taughtby(graphics,course159, person394, winter_0102).
professor(graphics,person40).
professor(graphics,person279).
professor(graphics,person394).
student(graphics,person38).
student(graphics,person261).
student(graphics,person149).
student(graphics,person306).
student(graphics,person410).
student(graphics,person157).
student(graphics,person200).
student(graphics,person404).
student(graphics,person122).
student(graphics,person322).
student(graphics,person131).
student(graphics,person85).
professor(graphics,person342).
professor(graphics,person111).
professor(graphics,person115).
professor(graphics,person351).
professor(graphics,person72).
professor(graphics,person393).
student(graphics,person241).
student(graphics,person217).
student(graphics,person270).
student(graphics,person206).
student(graphics,person81).
student(graphics,person228).
student(graphics,person51).
student(graphics,person41).
student(graphics,person163).
student(graphics,person435).
student(graphics,person363).
student(graphics,person427).
student(graphics,person142).
student(graphics,person431).
student(graphics,person283).
student(graphics,person300).
student(graphics,person113).
student(graphics,person296).
student(graphics,person36).
student(graphics,person328).
student(graphics,person31).
student(graphics,person61).
student(graphics,person119).
student(graphics,person3).
student(graphics,person317).
student(graphics,person230).
student(graphics,person258).
student(graphics,person188).
student(graphics,person327).
student(graphics,person140).
student(graphics,person321).
student(graphics,person428).
student(graphics,person158).
student(graphics,person102).
student(graphics,person90).
student(graphics,person214).
student(graphics,person146).
student(graphics,person88).
student(graphics,person178).
student(graphics,person195).
sameperson(graphics,person40, person40).
sameperson(graphics,person279, person279).
sameperson(graphics,person394, person394).
sameperson(graphics,person38, person38).
sameperson(graphics,person261, person261).
sameperson(graphics,person149, person149).
sameperson(graphics,person306, person306).
sameperson(graphics,person410, person410).
sameperson(graphics,person157, person157).
sameperson(graphics,person200, person200).
sameperson(graphics,person404, person404).
sameperson(graphics,person122, person122).
sameperson(graphics,person322, person322).
sameperson(graphics,person131, person131).
sameperson(graphics,person85, person85).
sameperson(graphics,person342, person342).
sameperson(graphics,person111, person111).
sameperson(graphics,person115, person115).
sameperson(graphics,person351, person351).
sameperson(graphics,person72, person72).
sameperson(graphics,person393, person393).
sameperson(graphics,person241, person241).
sameperson(graphics,person217, person217).
sameperson(graphics,person270, person270).
sameperson(graphics,person206, person206).
sameperson(graphics,person81, person81).
sameperson(graphics,person228, person228).
sameperson(graphics,person51, person51).
sameperson(graphics,person41, person41).
sameperson(graphics,person163, person163).
sameperson(graphics,person435, person435).
sameperson(graphics,person363, person363).
sameperson(graphics,person427, person427).
sameperson(graphics,person142, person142).
sameperson(graphics,person431, person431).
sameperson(graphics,person283, person283).
sameperson(graphics,person300, person300).
sameperson(graphics,person113, person113).
sameperson(graphics,person296, person296).
sameperson(graphics,person36, person36).
sameperson(graphics,person328, person328).
sameperson(graphics,person31, person31).
sameperson(graphics,person61, person61).
sameperson(graphics,person119, person119).
sameperson(graphics,person3, person3).
sameperson(graphics,person317, person317).
sameperson(graphics,person230, person230).
sameperson(graphics,person258, person258).
sameperson(graphics,person188, person188).
sameperson(graphics,person327, person327).
sameperson(graphics,person140, person140).
sameperson(graphics,person321, person321).
sameperson(graphics,person428, person428).
sameperson(graphics,person158, person158).
sameperson(graphics,person102, person102).
sameperson(graphics,person90, person90).
sameperson(graphics,person214, person214).
sameperson(graphics,person146, person146).
sameperson(graphics,person88, person88).
sameperson(graphics,person178, person178).
sameperson(graphics,person195, person195).
samecourse(graphics,course89, course89).
samecourse(graphics,course157, course157).
samecourse(graphics,course110, course110).
samecourse(graphics,course125, course125).
samecourse(graphics,course164, course164).
samecourse(graphics,course28, course28).
samecourse(graphics,course107, course107).
samecourse(graphics,course0, course0).
samecourse(graphics,course13, course13).
samecourse(graphics,course115, course115).
samecourse(graphics,course101, course101).
samecourse(graphics,course136, course136).
samecourse(graphics,course108, course108).
samecourse(graphics,course67, course67).
samecourse(graphics,course153, course153).
samecourse(graphics,course1, course1).
samecourse(graphics,course83, course83).
samecourse(graphics,course79, course79).
samecourse(graphics,course114, course114).
samecourse(graphics,course148, course148).
samecourse(graphics,course93, course93).
samecourse(graphics,course159, course159).
samecourse(graphics,course154, course154).
samecourse(graphics,course118, course118).
samecourse(graphics,course109, course109).
samecourse(graphics,course56, course56).
samecourse(graphics,course41, course41).
samecourse(graphics,course150, course150).
samecourse(graphics,course3, course3).
sameproject(graphics,project103, project103).
sameproject(graphics,project91, project91).
sameproject(graphics,project96, project96).
sameproject(graphics,project15, project15).
sameproject(graphics,project140, project140).
sameproject(graphics,project78, project78).
sameproject(graphics,project47, project47).
sameproject(graphics,project106, project106).
sameproject(graphics,project118, project118).
sameproject(graphics,project133, project133).
sameproject(graphics,project34, project34).
sameproject(graphics,project151, project151).
sameproject(graphics,project81, project81).
sameproject(graphics,project123, project123).
sameproject(graphics,project23, project23).
sameproject(graphics,project132, project132).
sameproject(graphics,project71, project71).
sameproject(graphics,project135, project135).
sameproject(graphics,project149, project149).
sameproject(graphics,project25, project25).
sameproject(graphics,project65, project65).
sameproject(graphics,project6, project6).
sameproject(graphics,project88, project88).
sameproject(graphics,project54, project54).
sameproject(graphics,project105, project105).
sameproject(graphics,project46, project46).
sameproject(graphics,project142, project142).
sameproject(graphics,project48, project48).
sameproject(graphics,project2, project2).
sameproject(graphics,project92, project92).
sameproject(graphics,project86, project86).
sameproject(graphics,project19, project19).
sameproject(graphics,project139, project139).
sameproject(graphics,project117, project117).
sameproject(graphics,project98, project98).
sameproject(graphics,project59, project59).
sameproject(graphics,project145, project145).
sameproject(graphics,project69, project69).
sameproject(graphics,project53, project53).
sameproject(graphics,project107, project107).
sameproject(graphics,project136, project136).
sameproject(graphics,project87, project87).
sameproject(graphics,project45, project45).
sameproject(graphics,project79, project79).
sameproject(graphics,project35, project35).
sameproject(graphics,project49, project49).
publication(graphics,title322 , person40).
publication(graphics,title346 , person40).
publication(graphics,title1 , person40).
publication(graphics,title183 , person394).
publication(graphics,title30 , person394).
publication(graphics,title153 , person306).
publication(graphics,title111 , person306).
publication(graphics,title15 , person410).
publication(graphics,title45 , person410).
publication(graphics,title60 , person410).
publication(graphics,title93 , person410).
publication(graphics,title272 , person410).
publication(graphics,title116 , person410).
publication(graphics,title203 , person404).
publication(graphics,title185 , person404).
publication(graphics,title201 , person404).
publication(graphics,title137 , person322).
publication(graphics,title109 , person342).
publication(graphics,title181 , person342).
publication(graphics,title135 , person342).
publication(graphics,title15 , person342).
publication(graphics,title45 , person342).
publication(graphics,title263 , person342).
publication(graphics,title35 , person342).
publication(graphics,title272 , person342).
publication(graphics,title116 , person342).
publication(graphics,title85 , person342).
publication(graphics,title304 , person342).
publication(graphics,title302 , person342).
publication(graphics,title137 , person342).
publication(graphics,title183 , person72).
publication(graphics,title30 , person72).
publication(graphics,title201 , person72).
publication(graphics,title302 , person72).
publication(graphics,title137 , person72).
publication(graphics,title109 , person393).
publication(graphics,title153 , person393).
publication(graphics,title123 , person393).
publication(graphics,title181 , person393).
publication(graphics,title135 , person393).
publication(graphics,title45 , person393).
publication(graphics,title60 , person393).
publication(graphics,title93 , person393).
publication(graphics,title35 , person393).
publication(graphics,title220 , person393).
publication(graphics,title272 , person393).
publication(graphics,title116 , person393).
publication(graphics,title111 , person393).
publication(graphics,title4 , person393).
publication(graphics,title85 , person393).
publication(graphics,title304 , person393).
publication(graphics,title322 , person393).
publication(graphics,title346 , person393).
publication(graphics,title1 , person393).
publication(graphics,title304 , person241).
publication(graphics,title302 , person217).
publication(graphics,title137 , person206).
publication(graphics,title109 , person81).
publication(graphics,title45 , person81).
publication(graphics,title116 , person81).
publication(graphics,title304 , person81).
publication(graphics,title100 , person41).
publication(graphics,title99 , person435).
publication(graphics,title123 , person142).
publication(graphics,title263 , person142).
publication(graphics,title220 , person142).
publication(graphics,title99 , person300).
publication(graphics,title181 , person300).
publication(graphics,title35 , person300).
publication(graphics,title4 , person300).
publication(graphics,title203 , person113).
publication(graphics,title185 , person113).
publication(graphics,title100 , person113).
neg(advisedby(graphics,person113,person113)).
neg(advisedby(graphics,person113,person122)).
neg(advisedby(graphics,person113,person142)).
neg(advisedby(graphics,person113,person157)).
neg(advisedby(graphics,person113,person163)).
neg(advisedby(graphics,person113,person200)).
neg(advisedby(graphics,person113,person206)).
neg(advisedby(graphics,person113,person217)).
neg(advisedby(graphics,person113,person228)).
neg(advisedby(graphics,person113,person300)).
neg(advisedby(graphics,person113,person404)).
neg(advisedby(graphics,person113,person41)).
neg(advisedby(graphics,person113,person435)).
neg(advisedby(graphics,person113,person81)).
neg(advisedby(graphics,person113,person279)).
neg(advisedby(graphics,person113,person393)).
neg(advisedby(graphics,person113,person72)).
neg(advisedby(graphics,person122,person113)).
neg(advisedby(graphics,person122,person122)).
neg(advisedby(graphics,person122,person142)).
neg(advisedby(graphics,person122,person157)).
neg(advisedby(graphics,person122,person163)).
neg(advisedby(graphics,person122,person200)).
neg(advisedby(graphics,person122,person206)).
neg(advisedby(graphics,person122,person217)).
neg(advisedby(graphics,person122,person228)).
neg(advisedby(graphics,person122,person300)).
neg(advisedby(graphics,person122,person404)).
neg(advisedby(graphics,person122,person41)).
neg(advisedby(graphics,person122,person435)).
neg(advisedby(graphics,person122,person81)).
neg(advisedby(graphics,person122,person279)).
neg(advisedby(graphics,person122,person342)).
neg(advisedby(graphics,person122,person393)).
neg(advisedby(graphics,person122,person394)).
neg(advisedby(graphics,person142,person113)).
neg(advisedby(graphics,person142,person122)).
neg(advisedby(graphics,person142,person142)).
neg(advisedby(graphics,person142,person157)).
neg(advisedby(graphics,person142,person163)).
neg(advisedby(graphics,person142,person200)).
neg(advisedby(graphics,person142,person206)).
neg(advisedby(graphics,person142,person217)).
neg(advisedby(graphics,person142,person228)).
neg(advisedby(graphics,person142,person300)).
neg(advisedby(graphics,person142,person404)).
neg(advisedby(graphics,person142,person41)).
neg(advisedby(graphics,person142,person435)).
neg(advisedby(graphics,person142,person81)).
neg(advisedby(graphics,person142,person279)).
neg(advisedby(graphics,person142,person393)).
neg(advisedby(graphics,person142,person394)).
neg(advisedby(graphics,person142,person72)).
neg(advisedby(graphics,person157,person113)).
neg(advisedby(graphics,person157,person122)).
neg(advisedby(graphics,person157,person142)).
neg(advisedby(graphics,person157,person157)).
neg(advisedby(graphics,person157,person163)).
neg(advisedby(graphics,person157,person200)).
neg(advisedby(graphics,person157,person206)).
neg(advisedby(graphics,person157,person217)).
neg(advisedby(graphics,person157,person228)).
neg(advisedby(graphics,person157,person300)).
neg(advisedby(graphics,person157,person404)).
neg(advisedby(graphics,person157,person41)).
neg(advisedby(graphics,person157,person435)).
neg(advisedby(graphics,person157,person81)).
neg(advisedby(graphics,person157,person279)).
neg(advisedby(graphics,person157,person342)).
neg(advisedby(graphics,person157,person393)).
neg(advisedby(graphics,person157,person394)).
neg(advisedby(graphics,person163,person113)).
neg(advisedby(graphics,person163,person122)).
neg(advisedby(graphics,person163,person142)).
neg(advisedby(graphics,person163,person157)).
neg(advisedby(graphics,person163,person163)).
neg(advisedby(graphics,person163,person200)).
neg(advisedby(graphics,person163,person206)).
neg(advisedby(graphics,person163,person217)).
neg(advisedby(graphics,person163,person228)).
neg(advisedby(graphics,person163,person300)).
neg(advisedby(graphics,person163,person404)).
neg(advisedby(graphics,person163,person41)).
neg(advisedby(graphics,person163,person435)).
neg(advisedby(graphics,person163,person81)).
neg(advisedby(graphics,person163,person279)).
neg(advisedby(graphics,person163,person342)).
neg(advisedby(graphics,person163,person394)).
neg(advisedby(graphics,person163,person72)).
neg(advisedby(graphics,person200,person113)).
neg(advisedby(graphics,person200,person122)).
neg(advisedby(graphics,person200,person142)).
neg(advisedby(graphics,person200,person157)).
neg(advisedby(graphics,person200,person163)).
neg(advisedby(graphics,person200,person200)).
neg(advisedby(graphics,person200,person206)).
neg(advisedby(graphics,person200,person217)).
neg(advisedby(graphics,person200,person228)).
neg(advisedby(graphics,person200,person300)).
neg(advisedby(graphics,person200,person404)).
neg(advisedby(graphics,person200,person41)).
neg(advisedby(graphics,person200,person435)).
neg(advisedby(graphics,person200,person81)).
neg(advisedby(graphics,person200,person279)).
neg(advisedby(graphics,person200,person342)).
neg(advisedby(graphics,person200,person393)).
neg(advisedby(graphics,person200,person394)).
neg(advisedby(graphics,person206,person113)).
neg(advisedby(graphics,person206,person122)).
neg(advisedby(graphics,person206,person142)).
neg(advisedby(graphics,person206,person157)).
neg(advisedby(graphics,person206,person163)).
neg(advisedby(graphics,person206,person200)).
neg(advisedby(graphics,person206,person206)).
neg(advisedby(graphics,person206,person217)).
neg(advisedby(graphics,person206,person228)).
neg(advisedby(graphics,person206,person300)).
neg(advisedby(graphics,person206,person404)).
neg(advisedby(graphics,person206,person41)).
neg(advisedby(graphics,person206,person435)).
neg(advisedby(graphics,person206,person81)).
neg(advisedby(graphics,person206,person279)).
neg(advisedby(graphics,person206,person393)).
neg(advisedby(graphics,person206,person394)).
neg(advisedby(graphics,person217,person113)).
neg(advisedby(graphics,person217,person122)).
neg(advisedby(graphics,person217,person142)).
neg(advisedby(graphics,person217,person157)).
neg(advisedby(graphics,person217,person163)).
neg(advisedby(graphics,person217,person200)).
neg(advisedby(graphics,person217,person206)).
neg(advisedby(graphics,person217,person217)).
neg(advisedby(graphics,person217,person228)).
neg(advisedby(graphics,person217,person300)).
neg(advisedby(graphics,person217,person404)).
neg(advisedby(graphics,person217,person41)).
neg(advisedby(graphics,person217,person435)).
neg(advisedby(graphics,person217,person81)).
neg(advisedby(graphics,person217,person279)).
neg(advisedby(graphics,person217,person393)).
neg(advisedby(graphics,person217,person394)).
neg(advisedby(graphics,person228,person113)).
neg(advisedby(graphics,person228,person122)).
neg(advisedby(graphics,person228,person142)).
neg(advisedby(graphics,person228,person157)).
neg(advisedby(graphics,person228,person163)).
neg(advisedby(graphics,person228,person200)).
neg(advisedby(graphics,person228,person206)).
neg(advisedby(graphics,person228,person217)).
neg(advisedby(graphics,person228,person228)).
neg(advisedby(graphics,person228,person300)).
neg(advisedby(graphics,person228,person404)).
neg(advisedby(graphics,person228,person41)).
neg(advisedby(graphics,person228,person435)).
neg(advisedby(graphics,person228,person81)).
neg(advisedby(graphics,person228,person279)).
neg(advisedby(graphics,person228,person72)).
neg(advisedby(graphics,person300,person113)).
neg(advisedby(graphics,person300,person122)).
neg(advisedby(graphics,person300,person142)).
neg(advisedby(graphics,person300,person157)).
neg(advisedby(graphics,person300,person163)).
neg(advisedby(graphics,person300,person200)).
neg(advisedby(graphics,person300,person206)).
neg(advisedby(graphics,person300,person217)).
neg(advisedby(graphics,person300,person228)).
neg(advisedby(graphics,person300,person300)).
neg(advisedby(graphics,person300,person404)).
neg(advisedby(graphics,person300,person41)).
neg(advisedby(graphics,person300,person435)).
neg(advisedby(graphics,person300,person81)).
neg(advisedby(graphics,person300,person279)).
neg(advisedby(graphics,person300,person393)).
neg(advisedby(graphics,person300,person394)).
neg(advisedby(graphics,person300,person72)).
neg(advisedby(graphics,person404,person113)).
neg(advisedby(graphics,person404,person122)).
neg(advisedby(graphics,person404,person142)).
neg(advisedby(graphics,person404,person157)).
neg(advisedby(graphics,person404,person163)).
neg(advisedby(graphics,person404,person200)).
neg(advisedby(graphics,person404,person206)).
neg(advisedby(graphics,person404,person217)).
neg(advisedby(graphics,person404,person228)).
neg(advisedby(graphics,person404,person300)).
neg(advisedby(graphics,person404,person404)).
neg(advisedby(graphics,person404,person41)).
neg(advisedby(graphics,person404,person435)).
neg(advisedby(graphics,person404,person81)).
neg(advisedby(graphics,person404,person279)).
neg(advisedby(graphics,person404,person342)).
neg(advisedby(graphics,person404,person393)).
neg(advisedby(graphics,person404,person394)).
neg(advisedby(graphics,person41,person113)).
neg(advisedby(graphics,person41,person122)).
neg(advisedby(graphics,person41,person142)).
neg(advisedby(graphics,person41,person157)).
neg(advisedby(graphics,person41,person163)).
neg(advisedby(graphics,person41,person200)).
neg(advisedby(graphics,person41,person206)).
neg(advisedby(graphics,person41,person217)).
neg(advisedby(graphics,person41,person228)).
neg(advisedby(graphics,person41,person300)).
neg(advisedby(graphics,person41,person404)).
neg(advisedby(graphics,person41,person41)).
neg(advisedby(graphics,person41,person435)).
neg(advisedby(graphics,person41,person81)).
neg(advisedby(graphics,person41,person279)).
neg(advisedby(graphics,person41,person342)).
neg(advisedby(graphics,person41,person393)).
neg(advisedby(graphics,person41,person72)).
neg(advisedby(graphics,person435,person113)).
neg(advisedby(graphics,person435,person122)).
neg(advisedby(graphics,person435,person142)).
neg(advisedby(graphics,person435,person157)).
neg(advisedby(graphics,person435,person163)).
neg(advisedby(graphics,person435,person200)).
neg(advisedby(graphics,person435,person206)).
neg(advisedby(graphics,person435,person217)).
neg(advisedby(graphics,person435,person228)).
neg(advisedby(graphics,person435,person300)).
neg(advisedby(graphics,person435,person404)).
neg(advisedby(graphics,person435,person41)).
neg(advisedby(graphics,person435,person435)).
neg(advisedby(graphics,person435,person81)).
neg(advisedby(graphics,person435,person342)).
neg(advisedby(graphics,person435,person393)).
neg(advisedby(graphics,person435,person394)).
neg(advisedby(graphics,person435,person72)).
neg(advisedby(graphics,person81,person113)).
neg(advisedby(graphics,person81,person122)).
neg(advisedby(graphics,person81,person142)).
neg(advisedby(graphics,person81,person157)).
neg(advisedby(graphics,person81,person163)).
neg(advisedby(graphics,person81,person200)).
neg(advisedby(graphics,person81,person206)).
neg(advisedby(graphics,person81,person217)).
neg(advisedby(graphics,person81,person228)).
neg(advisedby(graphics,person81,person300)).
neg(advisedby(graphics,person81,person404)).
neg(advisedby(graphics,person81,person41)).
neg(advisedby(graphics,person81,person435)).
neg(advisedby(graphics,person81,person81)).
neg(advisedby(graphics,person81,person279)).
neg(advisedby(graphics,person81,person394)).
neg(advisedby(graphics,person81,person72)).
neg(advisedby(graphics,person279,person113)).
neg(advisedby(graphics,person279,person122)).
neg(advisedby(graphics,person279,person142)).
neg(advisedby(graphics,person279,person157)).
neg(advisedby(graphics,person279,person163)).
neg(advisedby(graphics,person279,person200)).
neg(advisedby(graphics,person279,person206)).
neg(advisedby(graphics,person279,person217)).
neg(advisedby(graphics,person279,person228)).
neg(advisedby(graphics,person279,person300)).
neg(advisedby(graphics,person279,person404)).
neg(advisedby(graphics,person279,person41)).
neg(advisedby(graphics,person279,person435)).
neg(advisedby(graphics,person279,person81)).
neg(advisedby(graphics,person279,person279)).
neg(advisedby(graphics,person279,person342)).
neg(advisedby(graphics,person279,person393)).
neg(advisedby(graphics,person279,person394)).
neg(advisedby(graphics,person279,person72)).
neg(advisedby(graphics,person342,person113)).
neg(advisedby(graphics,person342,person122)).
neg(advisedby(graphics,person342,person142)).
neg(advisedby(graphics,person342,person157)).
neg(advisedby(graphics,person342,person163)).
neg(advisedby(graphics,person342,person200)).
neg(advisedby(graphics,person342,person206)).
neg(advisedby(graphics,person342,person217)).
neg(advisedby(graphics,person342,person228)).
neg(advisedby(graphics,person342,person300)).
neg(advisedby(graphics,person342,person404)).
neg(advisedby(graphics,person342,person41)).
neg(advisedby(graphics,person342,person435)).
neg(advisedby(graphics,person342,person81)).
neg(advisedby(graphics,person342,person279)).
neg(advisedby(graphics,person342,person342)).
neg(advisedby(graphics,person342,person393)).
neg(advisedby(graphics,person342,person394)).
neg(advisedby(graphics,person342,person72)).
neg(advisedby(graphics,person393,person113)).
neg(advisedby(graphics,person393,person122)).
neg(advisedby(graphics,person393,person142)).
neg(advisedby(graphics,person393,person157)).
neg(advisedby(graphics,person393,person163)).
neg(advisedby(graphics,person393,person200)).
neg(advisedby(graphics,person393,person206)).
neg(advisedby(graphics,person393,person217)).
neg(advisedby(graphics,person393,person228)).
neg(advisedby(graphics,person393,person300)).
neg(advisedby(graphics,person393,person404)).
neg(advisedby(graphics,person393,person41)).
neg(advisedby(graphics,person393,person435)).
neg(advisedby(graphics,person393,person81)).
neg(advisedby(graphics,person393,person279)).
neg(advisedby(graphics,person393,person342)).
neg(advisedby(graphics,person393,person393)).
neg(advisedby(graphics,person393,person394)).
neg(advisedby(graphics,person393,person72)).
neg(advisedby(graphics,person394,person113)).
neg(advisedby(graphics,person394,person122)).
neg(advisedby(graphics,person394,person142)).
neg(advisedby(graphics,person394,person157)).
neg(advisedby(graphics,person394,person163)).
neg(advisedby(graphics,person394,person200)).
neg(advisedby(graphics,person394,person206)).
neg(advisedby(graphics,person394,person217)).
neg(advisedby(graphics,person394,person228)).
neg(advisedby(graphics,person394,person300)).
neg(advisedby(graphics,person394,person404)).
neg(advisedby(graphics,person394,person41)).
neg(advisedby(graphics,person394,person435)).
neg(advisedby(graphics,person394,person81)).
neg(advisedby(graphics,person394,person279)).
neg(advisedby(graphics,person394,person342)).
neg(advisedby(graphics,person394,person393)).
neg(advisedby(graphics,person394,person394)).
neg(advisedby(graphics,person394,person72)).
neg(advisedby(graphics,person72,person113)).
neg(advisedby(graphics,person72,person122)).
neg(advisedby(graphics,person72,person142)).
neg(advisedby(graphics,person72,person157)).
neg(advisedby(graphics,person72,person163)).
neg(advisedby(graphics,person72,person200)).
neg(advisedby(graphics,person72,person206)).
neg(advisedby(graphics,person72,person217)).
neg(advisedby(graphics,person72,person228)).
neg(advisedby(graphics,person72,person300)).
neg(advisedby(graphics,person72,person404)).
neg(advisedby(graphics,person72,person41)).
neg(advisedby(graphics,person72,person435)).
neg(advisedby(graphics,person72,person81)).
neg(advisedby(graphics,person72,person279)).
neg(advisedby(graphics,person72,person342)).
neg(advisedby(graphics,person72,person393)).
neg(advisedby(graphics,person72,person394)).
neg(advisedby(graphics,person72,person72)).

taughtby(language,course51, person5, autumn_0001).
taughtby(language,course172, person335, autumn_0001).
taughtby(language,course46, person335, winter_0001).
taughtby(language,course71, person5, winter_0001).
taughtby(language,course124, person335, spring_0001).
taughtby(language,course51, person166, autumn_0102).
taughtby(language,course49, person263, winter_0102).
taughtby(language,course19, person5, winter_0102).
taughtby(language,course63, person335, spring_0102).
taughtby(language,course51, person18, autumn_0203).
taughtby(language,course53, person248, autumn_0203).
taughtby(language,course172, person335, autumn_0203).
taughtby(language,course49, person248, winter_0203).
taughtby(language,course46, person335, winter_0203).
taughtby(language,course146, person335, spring_0203).
taughtby(language,course49, person248, spring_0203).
taughtby(language,course53, person189, autumn_0304).
taughtby(language,course172, person46, autumn_0304).
taughtby(language,course138, person335, autumn_0304).
taughtby(language,course124, person9, winter_0304).
taughtby(language,course49, person64, winter_0304).
taughtby(language,course46, person335, winter_0304).
taughtby(language,course146, person335, spring_0304).
taughtby(language,course124, person46, spring_0304).
taughtby(language,course49, person189, spring_0304).
taughtby(language,course19, person370, spring_0304).
courselevel(language,course146, level_300).
courselevel(language,course124, level_300).
courselevel(language,course51, level_400).
courselevel(language,course49, level_400).
courselevel(language,course53, level_400).
courselevel(language,course46, level_500).
courselevel(language,course19, level_500).
courselevel(language,course172, level_500).
courselevel(language,course71, level_500).
courselevel(language,course63, level_500).
courselevel(language,course152, level_500).
courselevel(language,course54, level_500).
courselevel(language,course138, level_500).
courselevel(language,course35, level_500).
hasposition(language,person335, faculty).
hasposition(language,person46, faculty).
hasposition(language,person189, faculty_adjunct).
hasposition(language,person5, faculty).
advisedby(language,person18, person335).
advisedby(language,person9, person335).
advisedby(language,person429, person335).
advisedby(language,person362, person5).
advisedby(language,person362, person335).
advisedby(language,person96, person5).
advisedby(language,person263, person5).
advisedby(language,person183, person5).
advisedby(language,person118, person5).
inphase(language,person18, pre_quals).
inphase(language,person9, post_generals).
inphase(language,person429, post_quals).
inphase(language,person27, pre_quals).
inphase(language,person362, post_quals).
inphase(language,person96, post_generals).
inphase(language,person361, post_generals).
inphase(language,person263, post_generals).
inphase(language,person183, pre_quals).
inphase(language,person118, post_generals).
tempadvisedby(language,person27, person335).
yearsinprogram(language,person18, year_3).
yearsinprogram(language,person9, year_5).
yearsinprogram(language,person429, year_5).
yearsinprogram(language,person27, year_1).
yearsinprogram(language,person362, year_3).
yearsinprogram(language,person96, year_5).
yearsinprogram(language,person361, year_6).
yearsinprogram(language,person263, year_6).
yearsinprogram(language,person183, year_4).
yearsinprogram(language,person118, year_4).
ta(language,course49, person361, winter_0304).
ta(language,course46, person429, winter_0304).
ta(language,course124, person105, autumn_0304).
ta(language,course51, person27, autumn_0304).
ta(language,course138, person18, autumn_0304).
ta(language,course49, person361, summer_0203).
ta(language,course124, person108, spring_0203).
ta(language,course124, person203, spring_0203).
ta(language,course51, person96, spring_0203).
ta(language,course49, person287, spring_0203).
ta(language,course49, person87, spring_0203).
ta(language,course124, person18, winter_0203).
ta(language,course124, person35, winter_0203).
ta(language,course49, person287, winter_0203).
ta(language,course49, person87, winter_0203).
ta(language,course46, person429, winter_0203).
ta(language,course124, person108, autumn_0203).
ta(language,course124, person203, autumn_0203).
ta(language,course53, person287, autumn_0203).
ta(language,course172, person325, autumn_0203).
ta(language,course49, person361, summer_0102).
ta(language,course51, person39, spring_0102).
ta(language,course124, person76, winter_0102).
ta(language,course124, person9, winter_0102).
ta(language,course49, person96, winter_0102).
ta(language,course19, person232, winter_0102).
taughtby(language,course53, person248, autumn_0304).
taughtby(language,course49, person64, summer_0203).
taughtby(language,course49, person64, summer_0102).
professor(language,person248).
professor(language,person64).
professor(language,person166).
professor(language,person370).
professor(language,person335).
professor(language,person46).
professor(language,person189).
professor(language,person5).
student(language,person18).
student(language,person9).
student(language,person429).
student(language,person27).
student(language,person362).
student(language,person96).
student(language,person361).
student(language,person263).
student(language,person183).
student(language,person118).
student(language,person105).
student(language,person108).
student(language,person203).
student(language,person287).
student(language,person87).
student(language,person39).
student(language,person35).
student(language,person325).
student(language,person76).
student(language,person232).
sameperson(language,person248, person248).
sameperson(language,person64, person64).
sameperson(language,person166, person166).
sameperson(language,person370, person370).
sameperson(language,person335, person335).
sameperson(language,person46, person46).
sameperson(language,person189, person189).
sameperson(language,person5, person5).
sameperson(language,person18, person18).
sameperson(language,person9, person9).
sameperson(language,person429, person429).
sameperson(language,person27, person27).
sameperson(language,person362, person362).
sameperson(language,person96, person96).
sameperson(language,person361, person361).
sameperson(language,person263, person263).
sameperson(language,person183, person183).
sameperson(language,person118, person118).
sameperson(language,person105, person105).
sameperson(language,person108, person108).
sameperson(language,person203, person203).
sameperson(language,person287, person287).
sameperson(language,person87, person87).
sameperson(language,person39, person39).
sameperson(language,person35, person35).
sameperson(language,person325, person325).
sameperson(language,person76, person76).
sameperson(language,person232, person232).
samecourse(language,course146, course146).
samecourse(language,course124, course124).
samecourse(language,course51, course51).
samecourse(language,course49, course49).
samecourse(language,course53, course53).
samecourse(language,course46, course46).
samecourse(language,course19, course19).
samecourse(language,course172, course172).
samecourse(language,course71, course71).
samecourse(language,course63, course63).
samecourse(language,course152, course152).
samecourse(language,course54, course54).
samecourse(language,course138, course138).
samecourse(language,course35, course35).
sameproject(language,project9, project9).
sameproject(language,project102, project102).
sameproject(language,project108, project108).
sameproject(language,project89, project89).
sameproject(language,project147, project147).
sameproject(language,project3, project3).
sameproject(language,project95, project95).
sameproject(language,project120, project120).
publication(language,title106 , person335).
publication(language,title14 , person335).
publication(language,title130 , person335).
publication(language,title106 , person5).
publication(language,title130 , person5).
publication(language,title257 , person429).
publication(language,title142 , person429).
publication(language,title14 , person429).
publication(language,title257 , person183).
publication(language,title142 , person183).
neg(advisedby(language,person118,person118)).
neg(advisedby(language,person118,person18)).
neg(advisedby(language,person118,person183)).
neg(advisedby(language,person118,person263)).
neg(advisedby(language,person118,person362)).
neg(advisedby(language,person118,person429)).
neg(advisedby(language,person118,person9)).
neg(advisedby(language,person118,person96)).
neg(advisedby(language,person118,person335)).
neg(advisedby(language,person18,person118)).
neg(advisedby(language,person18,person18)).
neg(advisedby(language,person18,person183)).
neg(advisedby(language,person18,person263)).
neg(advisedby(language,person18,person362)).
neg(advisedby(language,person18,person429)).
neg(advisedby(language,person18,person9)).
neg(advisedby(language,person18,person96)).
neg(advisedby(language,person18,person5)).
neg(advisedby(language,person183,person118)).
neg(advisedby(language,person183,person18)).
neg(advisedby(language,person183,person183)).
neg(advisedby(language,person183,person263)).
neg(advisedby(language,person183,person362)).
neg(advisedby(language,person183,person429)).
neg(advisedby(language,person183,person9)).
neg(advisedby(language,person183,person96)).
neg(advisedby(language,person183,person335)).
neg(advisedby(language,person263,person118)).
neg(advisedby(language,person263,person18)).
neg(advisedby(language,person263,person183)).
neg(advisedby(language,person263,person263)).
neg(advisedby(language,person263,person362)).
neg(advisedby(language,person263,person429)).
neg(advisedby(language,person263,person9)).
neg(advisedby(language,person263,person96)).
neg(advisedby(language,person263,person335)).
neg(advisedby(language,person362,person118)).
neg(advisedby(language,person362,person18)).
neg(advisedby(language,person362,person183)).
neg(advisedby(language,person362,person263)).
neg(advisedby(language,person362,person362)).
neg(advisedby(language,person362,person429)).
neg(advisedby(language,person362,person9)).
neg(advisedby(language,person362,person96)).
neg(advisedby(language,person429,person118)).
neg(advisedby(language,person429,person18)).
neg(advisedby(language,person429,person183)).
neg(advisedby(language,person429,person263)).
neg(advisedby(language,person429,person362)).
neg(advisedby(language,person429,person429)).
neg(advisedby(language,person429,person9)).
neg(advisedby(language,person429,person96)).
neg(advisedby(language,person429,person5)).
neg(advisedby(language,person9,person118)).
neg(advisedby(language,person9,person18)).
neg(advisedby(language,person9,person183)).
neg(advisedby(language,person9,person263)).
neg(advisedby(language,person9,person362)).
neg(advisedby(language,person9,person429)).
neg(advisedby(language,person9,person9)).
neg(advisedby(language,person9,person96)).
neg(advisedby(language,person9,person5)).
neg(advisedby(language,person96,person118)).
neg(advisedby(language,person96,person18)).
neg(advisedby(language,person96,person183)).
neg(advisedby(language,person96,person263)).
neg(advisedby(language,person96,person362)).
neg(advisedby(language,person96,person429)).
neg(advisedby(language,person96,person9)).
neg(advisedby(language,person96,person96)).
neg(advisedby(language,person96,person335)).
neg(advisedby(language,person335,person118)).
neg(advisedby(language,person335,person18)).
neg(advisedby(language,person335,person183)).
neg(advisedby(language,person335,person263)).
neg(advisedby(language,person335,person362)).
neg(advisedby(language,person335,person429)).
neg(advisedby(language,person335,person9)).
neg(advisedby(language,person335,person96)).
neg(advisedby(language,person335,person335)).
neg(advisedby(language,person335,person5)).
neg(advisedby(language,person5,person118)).
neg(advisedby(language,person5,person18)).
neg(advisedby(language,person5,person183)).
neg(advisedby(language,person5,person263)).
neg(advisedby(language,person5,person362)).
neg(advisedby(language,person5,person429)).
neg(advisedby(language,person5,person9)).
neg(advisedby(language,person5,person96)).
neg(advisedby(language,person5,person335)).
neg(advisedby(language,person5,person5)).

taughtby(systems,course18, person373, autumn_0001).
taughtby(systems,course151, person290, autumn_0001).
taughtby(systems,course38, person204, autumn_0001).
taughtby(systems,course48, person107, autumn_0001).
taughtby(systems,course21, person99, autumn_0001).
taughtby(systems,course18, person326, winter_0001).
taughtby(systems,course151, person235, winter_0001).
taughtby(systems,course38, person104, winter_0001).
taughtby(systems,course20, person180, winter_0001).
taughtby(systems,course62, person101, winter_0001).
taughtby(systems,course129, person373, winter_0001).
taughtby(systems,course2, person180, winter_0001).
taughtby(systems,course18, person107, spring_0001).
taughtby(systems,course151, person267, spring_0001).
taughtby(systems,course80, person180, spring_0001).
taughtby(systems,course30, person290, spring_0001).
taughtby(systems,course8, person297, spring_0001).
taughtby(systems,course120, person235, spring_0001).
taughtby(systems,course74, person124, spring_0001).
taughtby(systems,course18, person213, autumn_0102).
taughtby(systems,course151, person179, autumn_0102).
taughtby(systems,course38, person104, autumn_0102).
taughtby(systems,course48, person375, autumn_0102).
taughtby(systems,course4, person107, autumn_0102).
taughtby(systems,course18, person107, winter_0102).
taughtby(systems,course151, person290, winter_0102).
taughtby(systems,course38, person124, winter_0102).
taughtby(systems,course20, person180, winter_0102).
taughtby(systems,course62, person101, winter_0102).
taughtby(systems,course129, person213, winter_0102).
taughtby(systems,course166, person235, winter_0102).
taughtby(systems,course2, person180, winter_0102).
taughtby(systems,course34, person179, winter_0102).
taughtby(systems,course18, person326, spring_0102).
taughtby(systems,course151, person234, spring_0102).
taughtby(systems,course80, person98, spring_0102).
taughtby(systems,course30, person290, spring_0102).
taughtby(systems,course75, person267, spring_0102).
taughtby(systems,course8, person297, spring_0102).
taughtby(systems,course116, person375, spring_0102).
taughtby(systems,course120, person235, spring_0102).
taughtby(systems,course74, person104, spring_0102).
taughtby(systems,course14, person124, spring_0102).
taughtby(systems,course162, person213, spring_0102).
taughtby(systems,course18, person107, autumn_0203).
taughtby(systems,course151, person267, autumn_0203).
taughtby(systems,course38, person104, autumn_0203).
taughtby(systems,course48, person375, autumn_0203).
taughtby(systems,course30, person290, autumn_0203).
taughtby(systems,course129, person213, autumn_0203).
taughtby(systems,course74, person124, autumn_0203).
taughtby(systems,course18, person290, winter_0203).
taughtby(systems,course151, person179, winter_0203).
taughtby(systems,course38, person104, winter_0203).
taughtby(systems,course75, person267, winter_0203).
taughtby(systems,course139, person235, winter_0203).
taughtby(systems,course167, person98, winter_0203).
taughtby(systems,course18, person375, spring_0203).
taughtby(systems,course151, person234, spring_0203).
taughtby(systems,course80, person98, spring_0203).
taughtby(systems,course30, person290, spring_0203).
taughtby(systems,course21, person22, spring_0203).
taughtby(systems,course120, person235, spring_0203).
taughtby(systems,course4, person107, spring_0203).
taughtby(systems,course151, person179, autumn_0304).
taughtby(systems,course38, person124, autumn_0304).
taughtby(systems,course48, person213, autumn_0304).
taughtby(systems,course74, person104, autumn_0304).
taughtby(systems,course18, person290, winter_0304).
taughtby(systems,course151, person82, winter_0304).
taughtby(systems,course38, person255, winter_0304).
taughtby(systems,course20, person180, winter_0304).
taughtby(systems,course75, person267, winter_0304).
taughtby(systems,course129, person213, winter_0304).
taughtby(systems,course23, person179, winter_0304).
taughtby(systems,course9, person235, winter_0304).
taughtby(systems,course18, person375, spring_0304).
taughtby(systems,course151, person234, spring_0304).
taughtby(systems,course80, person101, spring_0304).
taughtby(systems,course30, person290, spring_0304).
taughtby(systems,course120, person235, spring_0304).
taughtby(systems,course120, person82, spring_0304).
courselevel(systems,course5, level_300).
courselevel(systems,course18, level_300).
courselevel(systems,course21, level_400).
courselevel(systems,course151, level_400).
courselevel(systems,course38, level_400).
courselevel(systems,course45, level_400).
courselevel(systems,course20, level_400).
courselevel(systems,course48, level_400).
courselevel(systems,course62, level_400).
courselevel(systems,course80, level_400).
courselevel(systems,course30, level_400).
courselevel(systems,course174, level_400).
courselevel(systems,course75, level_400).
courselevel(systems,course8, level_400).
courselevel(systems,course129, level_500).
courselevel(systems,course116, level_500).
courselevel(systems,course120, level_500).
courselevel(systems,course166, level_500).
courselevel(systems,course74, level_500).
courselevel(systems,course2, level_500).
courselevel(systems,course4, level_500).
courselevel(systems,course34, level_500).
courselevel(systems,course14, level_500).
courselevel(systems,course167, level_500).
courselevel(systems,course139, level_500).
courselevel(systems,course162, level_500).
courselevel(systems,course61, level_500).
courselevel(systems,course23, level_500).
courselevel(systems,course9, level_500).
courselevel(systems,course87, level_500).
courselevel(systems,course88, level_500).
hasposition(systems,person124, faculty).
hasposition(systems,person375, faculty_emeritus).
hasposition(systems,person234, faculty).
hasposition(systems,person101, faculty).
hasposition(systems,person180, faculty).
hasposition(systems,person98, faculty).
hasposition(systems,person107, faculty).
hasposition(systems,person235, faculty).
hasposition(systems,person297, faculty_emeritus).
hasposition(systems,person82, faculty).
hasposition(systems,person179, faculty).
hasposition(systems,person213, faculty).
hasposition(systems,person22, faculty_emeritus).
hasposition(systems,person373, faculty).
hasposition(systems,person104, faculty).
hasposition(systems,person290, faculty).
advisedby(systems,person368, person180).
advisedby(systems,person130, person124).
advisedby(systems,person411, person373).
advisedby(systems,person426, person179).
advisedby(systems,person426, person235).
advisedby(systems,person99, person104).
advisedby(systems,person212, person180).
advisedby(systems,person403, person234).
advisedby(systems,person391, person235).
advisedby(systems,person253, person101).
advisedby(systems,person280, person101).
advisedby(systems,person92, person101).
advisedby(systems,person419, person101).
advisedby(systems,person357, person124).
advisedby(systems,person67, person375).
advisedby(systems,person67, person98).
advisedby(systems,person89, person104).
advisedby(systems,person80, person234).
advisedby(systems,person376, person179).
advisedby(systems,person376, person107).
advisedby(systems,person62, person104).
advisedby(systems,person218, person101).
advisedby(systems,person154, person124).
advisedby(systems,person154, person235).
advisedby(systems,person204, person104).
advisedby(systems,person126, person213).
advisedby(systems,person129, person179).
advisedby(systems,person129, person234).
advisedby(systems,person374, person179).
advisedby(systems,person155, person101).
advisedby(systems,person100, person104).
advisedby(systems,person100, person235).
advisedby(systems,person116, person124).
inphase(systems,person19, pre_quals).
inphase(systems,person398, pre_quals).
inphase(systems,person368, post_generals).
inphase(systems,person130, post_generals).
inphase(systems,person299, pre_quals).
inphase(systems,person175, post_generals).
inphase(systems,person255, post_generals).
inphase(systems,person411, post_generals).
inphase(systems,person426, post_quals).
inphase(systems,person99, post_quals).
inphase(systems,person212, post_generals).
inphase(systems,person403, post_generals).
inphase(systems,person402, pre_quals).
inphase(systems,person391, post_quals).
inphase(systems,person253, post_generals).
inphase(systems,person280, pre_quals).
inphase(systems,person417, pre_quals).
inphase(systems,person92, post_generals).
inphase(systems,person419, post_generals).
inphase(systems,person357, post_quals).
inphase(systems,person67, post_generals).
inphase(systems,person222, pre_quals).
inphase(systems,person89, post_generals).
inphase(systems,person277, pre_quals).
inphase(systems,person15, post_quals).
inphase(systems,person80, post_generals).
inphase(systems,person376, post_quals).
inphase(systems,person62, pre_quals).
inphase(systems,person218, post_generals).
inphase(systems,person186, pre_quals).
inphase(systems,person187, pre_quals).
inphase(systems,person343, pre_quals).
inphase(systems,person154, post_quals).
inphase(systems,person204, post_generals).
inphase(systems,person126, post_quals).
inphase(systems,person129, post_generals).
inphase(systems,person374, post_generals).
inphase(systems,person155, pre_quals).
inphase(systems,person100, post_quals).
inphase(systems,person116, pre_quals).
tempadvisedby(systems,person19, person98).
tempadvisedby(systems,person398, person213).
tempadvisedby(systems,person299, person235).
tempadvisedby(systems,person175, person107).
tempadvisedby(systems,person402, person234).
tempadvisedby(systems,person417, person104).
tempadvisedby(systems,person277, person235).
tempadvisedby(systems,person186, person290).
tempadvisedby(systems,person187, person180).
tempadvisedby(systems,person343, person213).
yearsinprogram(systems,person19, year_1).
yearsinprogram(systems,person398, year_1).
yearsinprogram(systems,person368, year_4).
yearsinprogram(systems,person130, year_8).
yearsinprogram(systems,person299, year_3).
yearsinprogram(systems,person175, year_2).
yearsinprogram(systems,person255, year_5).
yearsinprogram(systems,person411, year_6).
yearsinprogram(systems,person426, year_5).
yearsinprogram(systems,person99, year_2).
yearsinprogram(systems,person212, year_7).
yearsinprogram(systems,person403, year_12).
yearsinprogram(systems,person402, year_2).
yearsinprogram(systems,person391, year_4).
yearsinprogram(systems,person253, year_5).
yearsinprogram(systems,person280, year_3).
yearsinprogram(systems,person417, year_1).
yearsinprogram(systems,person92, year_5).
yearsinprogram(systems,person419, year_7).
yearsinprogram(systems,person357, year_4).
yearsinprogram(systems,person67, year_6).
yearsinprogram(systems,person222, year_1).
yearsinprogram(systems,person89, year_5).
yearsinprogram(systems,person277, year_1).
yearsinprogram(systems,person15, year_3).
yearsinprogram(systems,person80, year_6).
yearsinprogram(systems,person376, year_4).
yearsinprogram(systems,person62, year_2).
yearsinprogram(systems,person218, year_12).
yearsinprogram(systems,person186, year_1).
yearsinprogram(systems,person187, year_1).
yearsinprogram(systems,person343, year_1).
yearsinprogram(systems,person154, year_4).
yearsinprogram(systems,person204, year_6).
yearsinprogram(systems,person126, year_5).
yearsinprogram(systems,person129, year_6).
yearsinprogram(systems,person374, year_12).
yearsinprogram(systems,person155, year_2).
yearsinprogram(systems,person100, year_5).
yearsinprogram(systems,person116, year_3).
ta(systems,course18, person398, winter_0304).
ta(systems,course18, person274, winter_0304).
ta(systems,course151, person4, winter_0304).
ta(systems,course151, person299, winter_0304).
ta(systems,course151, person71, winter_0304).
ta(systems,course38, person222, winter_0304).
ta(systems,course38, person207, winter_0304).
ta(systems,course20, person368, winter_0304).
ta(systems,course129, person67, winter_0304).
ta(systems,course23, person116, winter_0304).
ta(systems,course88, person130, winter_0304).
ta(systems,course18, person277, autumn_0304).
ta(systems,course18, person67, autumn_0304).
ta(systems,course151, person4, autumn_0304).
ta(systems,course151, person129, autumn_0304).
ta(systems,course38, person190, autumn_0304).
ta(systems,course38, person222, autumn_0304).
ta(systems,course38, person207, autumn_0304).
ta(systems,course45, person155, autumn_0304).
ta(systems,course45, person71, autumn_0304).
ta(systems,course48, person155, autumn_0304).
ta(systems,course18, person274, spring_0203).
ta(systems,course21, person198, spring_0203).
ta(systems,course151, person269, spring_0203).
ta(systems,course80, person358, spring_0203).
ta(systems,course61, person155, spring_0203).
ta(systems,course18, person116, winter_0203).
ta(systems,course151, person155, winter_0203).
ta(systems,course38, person62, winter_0203).
ta(systems,course18, person354, autumn_0203).
ta(systems,course18, person155, autumn_0203).
ta(systems,course151, person167, autumn_0203).
ta(systems,course151, person186, autumn_0203).
ta(systems,course38, person154, autumn_0203).
ta(systems,course45, person358, autumn_0203).
ta(systems,course74, person255, autumn_0203).
ta(systems,course18, person67, spring_0102).
ta(systems,course151, person299, spring_0102).
ta(systems,course30, person116, spring_0102).
ta(systems,course174, person123, spring_0102).
ta(systems,course74, person204, spring_0102).
ta(systems,course14, person15, spring_0102).
ta(systems,course18, person15, winter_0102).
ta(systems,course18, person280, winter_0102).
ta(systems,course151, person223, winter_0102).
ta(systems,course151, person299, winter_0102).
ta(systems,course38, person357, winter_0102).
ta(systems,course38, person255, winter_0102).
ta(systems,course38, person92, winter_0102).
ta(systems,course20, person84, winter_0102).
ta(systems,course62, person126, winter_0102).
ta(systems,course129, person340, winter_0102).
ta(systems,course166, person100, winter_0102).
taughtby(systems,course88 , person235, winter_0304).
taughtby(systems,course61, person107, spring_0203).
taughtby(systems,course174 , person267, spring_0102).
professor(systems,person22).
professor(systems,person124).
professor(systems,person375).
professor(systems,person179).
professor(systems,person297).
professor(systems,person326).
professor(systems,person267).
professor(systems,person234).
professor(systems,person101).
professor(systems,person180).
professor(systems,person98).
professor(systems,person107).
professor(systems,person235).
professor(systems,person82).
professor(systems,person213).
professor(systems,person373).
professor(systems,person104).
professor(systems,person290).
student(systems,person19).
student(systems,person398).
student(systems,person368).
student(systems,person130).
student(systems,person299).
student(systems,person175).
student(systems,person255).
student(systems,person411).
student(systems,person426).
student(systems,person99).
student(systems,person212).
student(systems,person403).
student(systems,person402).
student(systems,person391).
student(systems,person253).
student(systems,person280).
student(systems,person417).
student(systems,person92).
student(systems,person419).
student(systems,person357).
student(systems,person67).
student(systems,person222).
student(systems,person89).
student(systems,person277).
student(systems,person15).
student(systems,person80).
student(systems,person376).
student(systems,person62).
student(systems,person218).
student(systems,person186).
student(systems,person187).
student(systems,person343).
student(systems,person154).
student(systems,person204).
student(systems,person126).
student(systems,person129).
student(systems,person374).
student(systems,person155).
student(systems,person100).
student(systems,person116).
student(systems,person274).
student(systems,person4).
student(systems,person71).
student(systems,person207).
student(systems,person190).
student(systems,person198).
student(systems,person269).
student(systems,person358).
student(systems,person354).
student(systems,person167).
student(systems,person123).
student(systems,person223).
student(systems,person84).
student(systems,person340).
sameperson(systems,person22, person22).
sameperson(systems,person124, person124).
sameperson(systems,person375, person375).
sameperson(systems,person179, person179).
sameperson(systems,person297, person297).
sameperson(systems,person326, person326).
sameperson(systems,person267, person267).
sameperson(systems,person234, person234).
sameperson(systems,person101, person101).
sameperson(systems,person180, person180).
sameperson(systems,person98, person98).
sameperson(systems,person107, person107).
sameperson(systems,person235, person235).
sameperson(systems,person82, person82).
sameperson(systems,person213, person213).
sameperson(systems,person373, person373).
sameperson(systems,person104, person104).
sameperson(systems,person290, person290).
sameperson(systems,person19, person19).
sameperson(systems,person398, person398).
sameperson(systems,person368, person368).
sameperson(systems,person130, person130).
sameperson(systems,person299, person299).
sameperson(systems,person175, person175).
sameperson(systems,person255, person255).
sameperson(systems,person411, person411).
sameperson(systems,person426, person426).
sameperson(systems,person99, person99).
sameperson(systems,person212, person212).
sameperson(systems,person403, person403).
sameperson(systems,person402, person402).
sameperson(systems,person391, person391).
sameperson(systems,person253, person253).
sameperson(systems,person280, person280).
sameperson(systems,person417, person417).
sameperson(systems,person92, person92).
sameperson(systems,person419, person419).
sameperson(systems,person357, person357).
sameperson(systems,person67, person67).
sameperson(systems,person222, person222).
sameperson(systems,person89, person89).
sameperson(systems,person277, person277).
sameperson(systems,person15, person15).
sameperson(systems,person80, person80).
sameperson(systems,person376, person376).
sameperson(systems,person62, person62).
sameperson(systems,person218, person218).
sameperson(systems,person186, person186).
sameperson(systems,person187, person187).
sameperson(systems,person343, person343).
sameperson(systems,person154, person154).
sameperson(systems,person204, person204).
sameperson(systems,person126, person126).
sameperson(systems,person129, person129).
sameperson(systems,person374, person374).
sameperson(systems,person155, person155).
sameperson(systems,person100, person100).
sameperson(systems,person116, person116).
sameperson(systems,person274, person274).
sameperson(systems,person4, person4).
sameperson(systems,person71, person71).
sameperson(systems,person207, person207).
sameperson(systems,person190, person190).
sameperson(systems,person198, person198).
sameperson(systems,person269, person269).
sameperson(systems,person358, person358).
sameperson(systems,person354, person354).
sameperson(systems,person167, person167).
sameperson(systems,person123, person123).
sameperson(systems,person223, person223).
sameperson(systems,person84, person84).
sameperson(systems,person340, person340).
samecourse(systems,course5, course5).
samecourse(systems,course18, course18).
samecourse(systems,course21, course21).
samecourse(systems,course151, course151).
samecourse(systems,course38, course38).
samecourse(systems,course45, course45).
samecourse(systems,course20, course20).
samecourse(systems,course48, course48).
samecourse(systems,course62, course62).
samecourse(systems,course80, course80).
samecourse(systems,course30, course30).
samecourse(systems,course75, course75).
samecourse(systems,course8, course8).
samecourse(systems,course129, course129).
samecourse(systems,course116, course116).
samecourse(systems,course120, course120).
samecourse(systems,course166, course166).
samecourse(systems,course74, course74).
samecourse(systems,course2, course2).
samecourse(systems,course4, course4).
samecourse(systems,course34, course34).
samecourse(systems,course14, course14).
samecourse(systems,course167, course167).
samecourse(systems,course139, course139).
samecourse(systems,course162, course162).
samecourse(systems,course61, course61).
samecourse(systems,course23, course23).
samecourse(systems,course9, course9).
samecourse(systems,course87, course87).
samecourse(systems,course174, course174).
samecourse(systems,course88, course88).
sameproject(systems,project27, project27).
sameproject(systems,project16, project16).
sameproject(systems,project60, project60).
sameproject(systems,project125, project125).
sameproject(systems,project39, project39).
sameproject(systems,project32, project32).
sameproject(systems,project44, project44).
sameproject(systems,project14, project14).
sameproject(systems,project114, project114).
sameproject(systems,project80, project80).
sameproject(systems,project43, project43).
sameproject(systems,project110, project110).
sameproject(systems,project68, project68).
sameproject(systems,project75, project75).
sameproject(systems,project128, project128).
sameproject(systems,project112, project112).
sameproject(systems,project37, project37).
sameproject(systems,project93, project93).
sameproject(systems,project40, project40).
sameproject(systems,project148, project148).
sameproject(systems,project26, project26).
sameproject(systems,project122, project122).
sameproject(systems,project4, project4).
sameproject(systems,project30, project30).
sameproject(systems,project67, project67).
sameproject(systems,project55, project55).
sameproject(systems,project31, project31).
sameproject(systems,project99, project99).
sameproject(systems,project134, project134).
sameproject(systems,project109, project109).
sameproject(systems,project72, project72).
sameproject(systems,project8, project8).
sameproject(systems,project28, project28).
sameproject(systems,project144, project144).
sameproject(systems,project10, project10).
sameproject(systems,project138, project138).
publication(systems,title294 , person124).
publication(systems,title214 , person124).
publication(systems,title186 , person124).
publication(systems,title141 , person124).
publication(systems,title246 , person124).
publication(systems,title253 , person124).
publication(systems,title227 , person124).
publication(systems,title48 , person124).
publication(systems,title282 , person124).
publication(systems,title267 , person124).
publication(systems,title133 , person124).
publication(systems,title245 , person124).
publication(systems,title213 , person375).
publication(systems,title91 , person375).
publication(systems,title74 , person375).
publication(systems,title9 , person375).
publication(systems,title117 , person375).
publication(systems,title239 , person375).
publication(systems,title194 , person375).
publication(systems,title64 , person179).
publication(systems,title143 , person179).
publication(systems,title338 , person179).
publication(systems,title51 , person179).
publication(systems,title294 , person234).
publication(systems,title213 , person234).
publication(systems,title168 , person234).
publication(systems,title96 , person234).
publication(systems,title3 , person234).
publication(systems,title189 , person234).
publication(systems,title46 , person234).
publication(systems,title28 , person234).
publication(systems,title141 , person234).
publication(systems,title53 , person234).
publication(systems,title176 , person234).
publication(systems,title58 , person234).
publication(systems,title65 , person234).
publication(systems,title198 , person234).
publication(systems,title315 , person234).
publication(systems,title196 , person234).
publication(systems,title91 , person234).
publication(systems,title289 , person234).
publication(systems,title43 , person234).
publication(systems,title48 , person234).
publication(systems,title74 , person234).
publication(systems,title117 , person234).
publication(systems,title239 , person234).
publication(systems,title194 , person234).
publication(systems,title237 , person234).
publication(systems,title204 , person234).
publication(systems,title121 , person234).
publication(systems,title209 , person234).
publication(systems,title332 , person234).
publication(systems,title247 , person234).
publication(systems,title342 , person234).
publication(systems,title169 , person234).
publication(systems,title144 , person234).
publication(systems,title17 , person234).
publication(systems,title80 , person234).
publication(systems,title7 , person234).
publication(systems,title234 , person234).
publication(systems,title339 , person234).
publication(systems,title69 , person234).
publication(systems,title249 , person234).
publication(systems,title76 , person234).
publication(systems,title81 , person234).
publication(systems,title285 , person234).
publication(systems,title101 , person234).
publication(systems,title223 , person101).
publication(systems,title56 , person101).
publication(systems,title294 , person101).
publication(systems,title214 , person101).
publication(systems,title157 , person101).
publication(systems,title68 , person101).
publication(systems,title197 , person101).
publication(systems,title139 , person101).
publication(systems,title141 , person101).
publication(systems,title131 , person101).
publication(systems,title2 , person101).
publication(systems,title75 , person101).
publication(systems,title174 , person101).
publication(systems,title148 , person101).
publication(systems,title8 , person101).
publication(systems,title282 , person101).
publication(systems,title52 , person101).
publication(systems,title31 , person101).
publication(systems,title133 , person101).
publication(systems,title245 , person101).
publication(systems,title67 , person101).
publication(systems,title173 , person180).
publication(systems,title223 , person98).
publication(systems,title197 , person98).
publication(systems,title139 , person98).
publication(systems,title2 , person98).
publication(systems,title174 , person98).
publication(systems,title148 , person98).
publication(systems,title8 , person98).
publication(systems,title244 , person107).
publication(systems,title300 , person107).
publication(systems,title124 , person107).
publication(systems,title96 , person107).
publication(systems,title176 , person107).
publication(systems,title198 , person107).
publication(systems,title303 , person107).
publication(systems,title209 , person107).
publication(systems,title320 , person107).
publication(systems,title169 , person107).
publication(systems,title17 , person107).
publication(systems,title264 , person107).
publication(systems,title294 , person235).
publication(systems,title108 , person235).
publication(systems,title141 , person235).
publication(systems,title42 , person235).
publication(systems,title338 , person235).
publication(systems,title51 , person235).
publication(systems,title37 , person235).
publication(systems,title281 , person235).
publication(systems,title315 , person82).
publication(systems,title196 , person82).
publication(systems,title221 , person82).
publication(systems,title232 , person82).
publication(systems,title9 , person82).
publication(systems,title32 , person82).
publication(systems,title251 , person82).
publication(systems,title211 , person82).
publication(systems,title23 , person82).
publication(systems,title204 , person82).
publication(systems,title121 , person82).
publication(systems,title332 , person82).
publication(systems,title144 , person82).
publication(systems,title163 , person82).
publication(systems,title306 , person82).
publication(systems,title80 , person82).
publication(systems,title234 , person82).
publication(systems,title256 , person82).
publication(systems,title61 , person82).
publication(systems,title343 , person82).
publication(systems,title187 , person82).
publication(systems,title249 , person82).
publication(systems,title6 , person82).
publication(systems,title76 , person82).
publication(systems,title299 , person82).
publication(systems,title34 , person82).
publication(systems,title280 , person82).
publication(systems,title36 , person82).
publication(systems,title81 , person82).
publication(systems,title146 , person373).
publication(systems,title186 , person104).
publication(systems,title277 , person104).
publication(systems,title180 , person104).
publication(systems,title141 , person104).
publication(systems,title246 , person104).
publication(systems,title227 , person104).
publication(systems,title48 , person104).
publication(systems,title267 , person104).
publication(systems,title149 , person104).
publication(systems,title253 , person290).
publication(systems,title221 , person290).
publication(systems,title232 , person290).
publication(systems,title9 , person290).
publication(systems,title32 , person290).
publication(systems,title251 , person290).
publication(systems,title211 , person290).
publication(systems,title23 , person290).
publication(systems,title163 , person290).
publication(systems,title306 , person290).
publication(systems,title256 , person290).
publication(systems,title61 , person290).
publication(systems,title343 , person290).
publication(systems,title187 , person290).
publication(systems,title6 , person290).
publication(systems,title299 , person290).
publication(systems,title34 , person290).
publication(systems,title280 , person290).
publication(systems,title36 , person290).
publication(systems,title294 , person255).
publication(systems,title141 , person255).
publication(systems,title146 , person411).
publication(systems,title186 , person99).
publication(systems,title180 , person99).
publication(systems,title286 , person99).
publication(systems,title173 , person212).
publication(systems,title96 , person403).
publication(systems,title46 , person403).
publication(systems,title53 , person403).
publication(systems,title58 , person403).
publication(systems,title289 , person403).
publication(systems,title43 , person403).
publication(systems,title169 , person403).
publication(systems,title7 , person403).
publication(systems,title339 , person403).
publication(systems,title108 , person402).
publication(systems,title42 , person402).
publication(systems,title56 , person253).
publication(systems,title214 , person253).
publication(systems,title157 , person253).
publication(systems,title68 , person253).
publication(systems,title131 , person253).
publication(systems,title75 , person253).
publication(systems,title282 , person253).
publication(systems,title245 , person253).
publication(systems,title206 , person92).
publication(systems,title242 , person92).
publication(systems,title31 , person419).
publication(systems,title64 , person419).
publication(systems,title143 , person419).
publication(systems,title294 , person357).
publication(systems,title141 , person357).
publication(systems,title262 , person89).
publication(systems,title206 , person89).
publication(systems,title242 , person89).
publication(systems,title206 , person15).
publication(systems,title168 , person80).
publication(systems,title300 , person80).
publication(systems,title96 , person80).
publication(systems,title189 , person80).
publication(systems,title46 , person80).
publication(systems,title28 , person80).
publication(systems,title176 , person80).
publication(systems,title65 , person80).
publication(systems,title237 , person80).
publication(systems,title247 , person80).
publication(systems,title342 , person80).
publication(systems,title169 , person80).
publication(systems,title17 , person80).
publication(systems,title339 , person80).
publication(systems,title285 , person80).
publication(systems,title101 , person80).
publication(systems,title244 , person376).
publication(systems,title124 , person376).
publication(systems,title303 , person376).
publication(systems,title320 , person376).
publication(systems,title264 , person376).
publication(systems,title52 , person218).
publication(systems,title67 , person218).
publication(systems,title37 , person154).
publication(systems,title281 , person154).
publication(systems,title186 , person204).
publication(systems,title277 , person204).
publication(systems,title3 , person204).
publication(systems,title286 , person204).
publication(systems,title149 , person204).
publication(systems,title69 , person204).
publication(systems,title294 , person126).
publication(systems,title141 , person126).
publication(systems,title262 , person100).
publication(systems,title37 , person100).
publication(systems,title281 , person100).
neg(advisedby(systems,person100,person100)).
neg(advisedby(systems,person100,person116)).
neg(advisedby(systems,person100,person126)).
neg(advisedby(systems,person100,person129)).
neg(advisedby(systems,person100,person130)).
neg(advisedby(systems,person100,person154)).
neg(advisedby(systems,person100,person155)).
neg(advisedby(systems,person100,person204)).
neg(advisedby(systems,person100,person212)).
neg(advisedby(systems,person100,person218)).
neg(advisedby(systems,person100,person253)).
neg(advisedby(systems,person100,person280)).
neg(advisedby(systems,person100,person357)).
neg(advisedby(systems,person100,person368)).
neg(advisedby(systems,person100,person374)).
neg(advisedby(systems,person100,person376)).
neg(advisedby(systems,person100,person391)).
neg(advisedby(systems,person100,person403)).
neg(advisedby(systems,person100,person411)).
neg(advisedby(systems,person100,person419)).
neg(advisedby(systems,person100,person426)).
neg(advisedby(systems,person100,person62)).
neg(advisedby(systems,person100,person67)).
neg(advisedby(systems,person100,person80)).
neg(advisedby(systems,person100,person89)).
neg(advisedby(systems,person100,person92)).
neg(advisedby(systems,person100,person99)).
neg(advisedby(systems,person100,person101)).
neg(advisedby(systems,person100,person107)).
neg(advisedby(systems,person100,person124)).
neg(advisedby(systems,person100,person179)).
neg(advisedby(systems,person100,person180)).
neg(advisedby(systems,person100,person213)).
neg(advisedby(systems,person100,person234)).
neg(advisedby(systems,person100,person373)).
neg(advisedby(systems,person100,person375)).
neg(advisedby(systems,person100,person98)).
neg(advisedby(systems,person116,person100)).
neg(advisedby(systems,person116,person116)).
neg(advisedby(systems,person116,person126)).
neg(advisedby(systems,person116,person129)).
neg(advisedby(systems,person116,person130)).
neg(advisedby(systems,person116,person154)).
neg(advisedby(systems,person116,person155)).
neg(advisedby(systems,person116,person204)).
neg(advisedby(systems,person116,person212)).
neg(advisedby(systems,person116,person218)).
neg(advisedby(systems,person116,person253)).
neg(advisedby(systems,person116,person280)).
neg(advisedby(systems,person116,person357)).
neg(advisedby(systems,person116,person368)).
neg(advisedby(systems,person116,person374)).
neg(advisedby(systems,person116,person376)).
neg(advisedby(systems,person116,person391)).
neg(advisedby(systems,person116,person403)).
neg(advisedby(systems,person116,person411)).
neg(advisedby(systems,person116,person419)).
neg(advisedby(systems,person116,person426)).
neg(advisedby(systems,person116,person62)).
neg(advisedby(systems,person116,person67)).
neg(advisedby(systems,person116,person80)).
neg(advisedby(systems,person116,person89)).
neg(advisedby(systems,person116,person92)).
neg(advisedby(systems,person116,person99)).
neg(advisedby(systems,person116,person101)).
neg(advisedby(systems,person116,person104)).
neg(advisedby(systems,person116,person107)).
neg(advisedby(systems,person116,person179)).
neg(advisedby(systems,person116,person180)).
neg(advisedby(systems,person116,person213)).
neg(advisedby(systems,person116,person234)).
neg(advisedby(systems,person116,person235)).
neg(advisedby(systems,person116,person373)).
neg(advisedby(systems,person116,person375)).
neg(advisedby(systems,person116,person98)).
neg(advisedby(systems,person126,person100)).
neg(advisedby(systems,person126,person116)).
neg(advisedby(systems,person126,person126)).
neg(advisedby(systems,person126,person129)).
neg(advisedby(systems,person126,person130)).
neg(advisedby(systems,person126,person154)).
neg(advisedby(systems,person126,person155)).
neg(advisedby(systems,person126,person204)).
neg(advisedby(systems,person126,person212)).
neg(advisedby(systems,person126,person218)).
neg(advisedby(systems,person126,person253)).
neg(advisedby(systems,person126,person280)).
neg(advisedby(systems,person126,person357)).
neg(advisedby(systems,person126,person368)).
neg(advisedby(systems,person126,person374)).
neg(advisedby(systems,person126,person376)).
neg(advisedby(systems,person126,person391)).
neg(advisedby(systems,person126,person403)).
neg(advisedby(systems,person126,person411)).
neg(advisedby(systems,person126,person419)).
neg(advisedby(systems,person126,person426)).
neg(advisedby(systems,person126,person62)).
neg(advisedby(systems,person126,person67)).
neg(advisedby(systems,person126,person80)).
neg(advisedby(systems,person126,person89)).
neg(advisedby(systems,person126,person92)).
neg(advisedby(systems,person126,person99)).
neg(advisedby(systems,person126,person101)).
neg(advisedby(systems,person126,person104)).
neg(advisedby(systems,person126,person107)).
neg(advisedby(systems,person126,person124)).
neg(advisedby(systems,person126,person179)).
neg(advisedby(systems,person126,person180)).
neg(advisedby(systems,person126,person234)).
neg(advisedby(systems,person126,person235)).
neg(advisedby(systems,person126,person373)).
neg(advisedby(systems,person126,person375)).
neg(advisedby(systems,person126,person98)).
neg(advisedby(systems,person129,person100)).
neg(advisedby(systems,person129,person116)).
neg(advisedby(systems,person129,person126)).
neg(advisedby(systems,person129,person129)).
neg(advisedby(systems,person129,person130)).
neg(advisedby(systems,person129,person154)).
neg(advisedby(systems,person129,person155)).
neg(advisedby(systems,person129,person204)).
neg(advisedby(systems,person129,person212)).
neg(advisedby(systems,person129,person218)).
neg(advisedby(systems,person129,person253)).
neg(advisedby(systems,person129,person280)).
neg(advisedby(systems,person129,person357)).
neg(advisedby(systems,person129,person368)).
neg(advisedby(systems,person129,person374)).
neg(advisedby(systems,person129,person376)).
neg(advisedby(systems,person129,person391)).
neg(advisedby(systems,person129,person403)).
neg(advisedby(systems,person129,person411)).
neg(advisedby(systems,person129,person419)).
neg(advisedby(systems,person129,person426)).
neg(advisedby(systems,person129,person62)).
neg(advisedby(systems,person129,person67)).
neg(advisedby(systems,person129,person80)).
neg(advisedby(systems,person129,person89)).
neg(advisedby(systems,person129,person92)).
neg(advisedby(systems,person129,person99)).
neg(advisedby(systems,person129,person101)).
neg(advisedby(systems,person129,person104)).
neg(advisedby(systems,person129,person107)).
neg(advisedby(systems,person129,person124)).
neg(advisedby(systems,person129,person180)).
neg(advisedby(systems,person129,person213)).
neg(advisedby(systems,person129,person235)).
neg(advisedby(systems,person129,person373)).
neg(advisedby(systems,person129,person375)).
neg(advisedby(systems,person129,person98)).
neg(advisedby(systems,person130,person100)).
neg(advisedby(systems,person130,person116)).
neg(advisedby(systems,person130,person126)).
neg(advisedby(systems,person130,person129)).
neg(advisedby(systems,person130,person130)).
neg(advisedby(systems,person130,person154)).
neg(advisedby(systems,person130,person155)).
neg(advisedby(systems,person130,person204)).
neg(advisedby(systems,person130,person212)).
neg(advisedby(systems,person130,person218)).
neg(advisedby(systems,person130,person253)).
neg(advisedby(systems,person130,person280)).
neg(advisedby(systems,person130,person357)).
neg(advisedby(systems,person130,person368)).
neg(advisedby(systems,person130,person374)).
neg(advisedby(systems,person130,person376)).
neg(advisedby(systems,person130,person391)).
neg(advisedby(systems,person130,person403)).
neg(advisedby(systems,person130,person411)).
neg(advisedby(systems,person130,person419)).
neg(advisedby(systems,person130,person426)).
neg(advisedby(systems,person130,person62)).
neg(advisedby(systems,person130,person67)).
neg(advisedby(systems,person130,person80)).
neg(advisedby(systems,person130,person89)).
neg(advisedby(systems,person130,person92)).
neg(advisedby(systems,person130,person99)).
neg(advisedby(systems,person130,person101)).
neg(advisedby(systems,person130,person104)).
neg(advisedby(systems,person130,person107)).
neg(advisedby(systems,person130,person179)).
neg(advisedby(systems,person130,person180)).
neg(advisedby(systems,person130,person213)).
neg(advisedby(systems,person130,person234)).
neg(advisedby(systems,person130,person235)).
neg(advisedby(systems,person130,person373)).
neg(advisedby(systems,person130,person375)).
neg(advisedby(systems,person130,person98)).
neg(advisedby(systems,person154,person100)).
neg(advisedby(systems,person154,person116)).
neg(advisedby(systems,person154,person126)).
neg(advisedby(systems,person154,person129)).
neg(advisedby(systems,person154,person130)).
neg(advisedby(systems,person154,person154)).
neg(advisedby(systems,person154,person155)).
neg(advisedby(systems,person154,person204)).
neg(advisedby(systems,person154,person212)).
neg(advisedby(systems,person154,person218)).
neg(advisedby(systems,person154,person253)).
neg(advisedby(systems,person154,person280)).
neg(advisedby(systems,person154,person357)).
neg(advisedby(systems,person154,person368)).
neg(advisedby(systems,person154,person374)).
neg(advisedby(systems,person154,person376)).
neg(advisedby(systems,person154,person391)).
neg(advisedby(systems,person154,person403)).
neg(advisedby(systems,person154,person411)).
neg(advisedby(systems,person154,person419)).
neg(advisedby(systems,person154,person426)).
neg(advisedby(systems,person154,person62)).
neg(advisedby(systems,person154,person67)).
neg(advisedby(systems,person154,person80)).
neg(advisedby(systems,person154,person89)).
neg(advisedby(systems,person154,person92)).
neg(advisedby(systems,person154,person99)).
neg(advisedby(systems,person154,person101)).
neg(advisedby(systems,person154,person104)).
neg(advisedby(systems,person154,person107)).
neg(advisedby(systems,person154,person179)).
neg(advisedby(systems,person154,person180)).
neg(advisedby(systems,person154,person213)).
neg(advisedby(systems,person154,person234)).
neg(advisedby(systems,person154,person373)).
neg(advisedby(systems,person154,person375)).
neg(advisedby(systems,person154,person98)).
neg(advisedby(systems,person155,person100)).
neg(advisedby(systems,person155,person116)).
neg(advisedby(systems,person155,person126)).
neg(advisedby(systems,person155,person129)).
neg(advisedby(systems,person155,person130)).
neg(advisedby(systems,person155,person154)).
neg(advisedby(systems,person155,person155)).
neg(advisedby(systems,person155,person204)).
neg(advisedby(systems,person155,person212)).
neg(advisedby(systems,person155,person218)).
neg(advisedby(systems,person155,person253)).
neg(advisedby(systems,person155,person280)).
neg(advisedby(systems,person155,person357)).
neg(advisedby(systems,person155,person368)).
neg(advisedby(systems,person155,person374)).
neg(advisedby(systems,person155,person376)).
neg(advisedby(systems,person155,person391)).
neg(advisedby(systems,person155,person403)).
neg(advisedby(systems,person155,person411)).
neg(advisedby(systems,person155,person419)).
neg(advisedby(systems,person155,person426)).
neg(advisedby(systems,person155,person62)).
neg(advisedby(systems,person155,person67)).
neg(advisedby(systems,person155,person80)).
neg(advisedby(systems,person155,person89)).
neg(advisedby(systems,person155,person92)).
neg(advisedby(systems,person155,person99)).
neg(advisedby(systems,person155,person104)).
neg(advisedby(systems,person155,person107)).
neg(advisedby(systems,person155,person124)).
neg(advisedby(systems,person155,person179)).
neg(advisedby(systems,person155,person180)).
neg(advisedby(systems,person155,person213)).
neg(advisedby(systems,person155,person234)).
neg(advisedby(systems,person155,person235)).
neg(advisedby(systems,person155,person373)).
neg(advisedby(systems,person155,person375)).
neg(advisedby(systems,person155,person98)).
neg(advisedby(systems,person204,person100)).
neg(advisedby(systems,person204,person116)).
neg(advisedby(systems,person204,person126)).
neg(advisedby(systems,person204,person129)).
neg(advisedby(systems,person204,person130)).
neg(advisedby(systems,person204,person154)).
neg(advisedby(systems,person204,person155)).
neg(advisedby(systems,person204,person204)).
neg(advisedby(systems,person204,person212)).
neg(advisedby(systems,person204,person218)).
neg(advisedby(systems,person204,person253)).
neg(advisedby(systems,person204,person280)).
neg(advisedby(systems,person204,person357)).
neg(advisedby(systems,person204,person368)).
neg(advisedby(systems,person204,person374)).
neg(advisedby(systems,person204,person376)).
neg(advisedby(systems,person204,person391)).
neg(advisedby(systems,person204,person403)).
neg(advisedby(systems,person204,person411)).
neg(advisedby(systems,person204,person419)).
neg(advisedby(systems,person204,person426)).
neg(advisedby(systems,person204,person62)).
neg(advisedby(systems,person204,person67)).
neg(advisedby(systems,person204,person80)).
neg(advisedby(systems,person204,person89)).
neg(advisedby(systems,person204,person92)).
neg(advisedby(systems,person204,person99)).
neg(advisedby(systems,person204,person101)).
neg(advisedby(systems,person204,person107)).
neg(advisedby(systems,person204,person124)).
neg(advisedby(systems,person204,person179)).
neg(advisedby(systems,person204,person180)).
neg(advisedby(systems,person204,person213)).
neg(advisedby(systems,person204,person234)).
neg(advisedby(systems,person204,person235)).
neg(advisedby(systems,person204,person373)).
neg(advisedby(systems,person204,person375)).
neg(advisedby(systems,person204,person98)).
neg(advisedby(systems,person212,person100)).
neg(advisedby(systems,person212,person116)).
neg(advisedby(systems,person212,person126)).
neg(advisedby(systems,person212,person129)).
neg(advisedby(systems,person212,person130)).
neg(advisedby(systems,person212,person154)).
neg(advisedby(systems,person212,person155)).
neg(advisedby(systems,person212,person204)).
neg(advisedby(systems,person212,person212)).
neg(advisedby(systems,person212,person218)).
neg(advisedby(systems,person212,person253)).
neg(advisedby(systems,person212,person280)).
neg(advisedby(systems,person212,person357)).
neg(advisedby(systems,person212,person368)).
neg(advisedby(systems,person212,person374)).
neg(advisedby(systems,person212,person376)).
neg(advisedby(systems,person212,person391)).
neg(advisedby(systems,person212,person403)).
neg(advisedby(systems,person212,person411)).
neg(advisedby(systems,person212,person419)).
neg(advisedby(systems,person212,person426)).
neg(advisedby(systems,person212,person62)).
neg(advisedby(systems,person212,person67)).
neg(advisedby(systems,person212,person80)).
neg(advisedby(systems,person212,person89)).
neg(advisedby(systems,person212,person92)).
neg(advisedby(systems,person212,person99)).
neg(advisedby(systems,person212,person101)).
neg(advisedby(systems,person212,person104)).
neg(advisedby(systems,person212,person107)).
neg(advisedby(systems,person212,person124)).
neg(advisedby(systems,person212,person179)).
neg(advisedby(systems,person212,person213)).
neg(advisedby(systems,person212,person234)).
neg(advisedby(systems,person212,person235)).
neg(advisedby(systems,person212,person373)).
neg(advisedby(systems,person212,person375)).
neg(advisedby(systems,person212,person98)).
neg(advisedby(systems,person218,person100)).
neg(advisedby(systems,person218,person116)).
neg(advisedby(systems,person218,person126)).
neg(advisedby(systems,person218,person129)).
neg(advisedby(systems,person218,person130)).
neg(advisedby(systems,person218,person154)).
neg(advisedby(systems,person218,person155)).
neg(advisedby(systems,person218,person204)).
neg(advisedby(systems,person218,person212)).
neg(advisedby(systems,person218,person218)).
neg(advisedby(systems,person218,person253)).
neg(advisedby(systems,person218,person280)).
neg(advisedby(systems,person218,person357)).
neg(advisedby(systems,person218,person368)).
neg(advisedby(systems,person218,person374)).
neg(advisedby(systems,person218,person376)).
neg(advisedby(systems,person218,person391)).
neg(advisedby(systems,person218,person403)).
neg(advisedby(systems,person218,person411)).
neg(advisedby(systems,person218,person419)).
neg(advisedby(systems,person218,person426)).
neg(advisedby(systems,person218,person62)).
neg(advisedby(systems,person218,person67)).
neg(advisedby(systems,person218,person80)).
neg(advisedby(systems,person218,person89)).
neg(advisedby(systems,person218,person92)).
neg(advisedby(systems,person218,person99)).
neg(advisedby(systems,person218,person104)).
neg(advisedby(systems,person218,person107)).
neg(advisedby(systems,person218,person124)).
neg(advisedby(systems,person218,person179)).
neg(advisedby(systems,person218,person180)).
neg(advisedby(systems,person218,person213)).
neg(advisedby(systems,person218,person234)).
neg(advisedby(systems,person218,person235)).
neg(advisedby(systems,person218,person373)).
neg(advisedby(systems,person218,person375)).
neg(advisedby(systems,person218,person98)).
neg(advisedby(systems,person253,person100)).
neg(advisedby(systems,person253,person116)).
neg(advisedby(systems,person253,person126)).
neg(advisedby(systems,person253,person129)).
neg(advisedby(systems,person253,person130)).
neg(advisedby(systems,person253,person154)).
neg(advisedby(systems,person253,person155)).
neg(advisedby(systems,person253,person204)).
neg(advisedby(systems,person253,person212)).
neg(advisedby(systems,person253,person218)).
neg(advisedby(systems,person253,person253)).
neg(advisedby(systems,person253,person280)).
neg(advisedby(systems,person253,person357)).
neg(advisedby(systems,person253,person368)).
neg(advisedby(systems,person253,person374)).
neg(advisedby(systems,person253,person376)).
neg(advisedby(systems,person253,person391)).
neg(advisedby(systems,person253,person403)).
neg(advisedby(systems,person253,person411)).
neg(advisedby(systems,person253,person419)).
neg(advisedby(systems,person253,person426)).
neg(advisedby(systems,person253,person62)).
neg(advisedby(systems,person253,person67)).
neg(advisedby(systems,person253,person80)).
neg(advisedby(systems,person253,person89)).
neg(advisedby(systems,person253,person92)).
neg(advisedby(systems,person253,person99)).
neg(advisedby(systems,person253,person104)).
neg(advisedby(systems,person253,person107)).
neg(advisedby(systems,person253,person124)).
neg(advisedby(systems,person253,person179)).
neg(advisedby(systems,person253,person180)).
neg(advisedby(systems,person253,person213)).
neg(advisedby(systems,person253,person234)).
neg(advisedby(systems,person253,person235)).
neg(advisedby(systems,person253,person373)).
neg(advisedby(systems,person253,person375)).
neg(advisedby(systems,person253,person98)).
neg(advisedby(systems,person280,person100)).
neg(advisedby(systems,person280,person116)).
neg(advisedby(systems,person280,person126)).
neg(advisedby(systems,person280,person129)).
neg(advisedby(systems,person280,person130)).
neg(advisedby(systems,person280,person154)).
neg(advisedby(systems,person280,person155)).
neg(advisedby(systems,person280,person204)).
neg(advisedby(systems,person280,person212)).
neg(advisedby(systems,person280,person218)).
neg(advisedby(systems,person280,person253)).
neg(advisedby(systems,person280,person280)).
neg(advisedby(systems,person280,person357)).
neg(advisedby(systems,person280,person368)).
neg(advisedby(systems,person280,person374)).
neg(advisedby(systems,person280,person376)).
neg(advisedby(systems,person280,person391)).
neg(advisedby(systems,person280,person403)).
neg(advisedby(systems,person280,person411)).
neg(advisedby(systems,person280,person419)).
neg(advisedby(systems,person280,person426)).
neg(advisedby(systems,person280,person62)).
neg(advisedby(systems,person280,person67)).
neg(advisedby(systems,person280,person80)).
neg(advisedby(systems,person280,person89)).
neg(advisedby(systems,person280,person92)).
neg(advisedby(systems,person280,person99)).
neg(advisedby(systems,person280,person104)).
neg(advisedby(systems,person280,person107)).
neg(advisedby(systems,person280,person124)).
neg(advisedby(systems,person280,person179)).
neg(advisedby(systems,person280,person180)).
neg(advisedby(systems,person280,person213)).
neg(advisedby(systems,person280,person234)).
neg(advisedby(systems,person280,person235)).
neg(advisedby(systems,person280,person373)).
neg(advisedby(systems,person280,person375)).
neg(advisedby(systems,person280,person98)).
neg(advisedby(systems,person357,person100)).
neg(advisedby(systems,person357,person116)).
neg(advisedby(systems,person357,person126)).
neg(advisedby(systems,person357,person129)).
neg(advisedby(systems,person357,person130)).
neg(advisedby(systems,person357,person154)).
neg(advisedby(systems,person357,person155)).
neg(advisedby(systems,person357,person204)).
neg(advisedby(systems,person357,person212)).
neg(advisedby(systems,person357,person218)).
neg(advisedby(systems,person357,person253)).
neg(advisedby(systems,person357,person280)).
neg(advisedby(systems,person357,person357)).
neg(advisedby(systems,person357,person368)).
neg(advisedby(systems,person357,person374)).
neg(advisedby(systems,person357,person376)).
neg(advisedby(systems,person357,person391)).
neg(advisedby(systems,person357,person403)).
neg(advisedby(systems,person357,person411)).
neg(advisedby(systems,person357,person419)).
neg(advisedby(systems,person357,person426)).
neg(advisedby(systems,person357,person62)).
neg(advisedby(systems,person357,person67)).
neg(advisedby(systems,person357,person80)).
neg(advisedby(systems,person357,person89)).
neg(advisedby(systems,person357,person92)).
neg(advisedby(systems,person357,person99)).
neg(advisedby(systems,person357,person101)).
neg(advisedby(systems,person357,person104)).
neg(advisedby(systems,person357,person107)).
neg(advisedby(systems,person357,person179)).
neg(advisedby(systems,person357,person180)).
neg(advisedby(systems,person357,person213)).
neg(advisedby(systems,person357,person234)).
neg(advisedby(systems,person357,person235)).
neg(advisedby(systems,person357,person373)).
neg(advisedby(systems,person357,person375)).
neg(advisedby(systems,person357,person98)).
neg(advisedby(systems,person368,person100)).
neg(advisedby(systems,person368,person116)).
neg(advisedby(systems,person368,person126)).
neg(advisedby(systems,person368,person129)).
neg(advisedby(systems,person368,person130)).
neg(advisedby(systems,person368,person154)).
neg(advisedby(systems,person368,person155)).
neg(advisedby(systems,person368,person204)).
neg(advisedby(systems,person368,person212)).
neg(advisedby(systems,person368,person218)).
neg(advisedby(systems,person368,person253)).
neg(advisedby(systems,person368,person280)).
neg(advisedby(systems,person368,person357)).
neg(advisedby(systems,person368,person368)).
neg(advisedby(systems,person368,person374)).
neg(advisedby(systems,person368,person376)).
neg(advisedby(systems,person368,person391)).
neg(advisedby(systems,person368,person403)).
neg(advisedby(systems,person368,person411)).
neg(advisedby(systems,person368,person419)).
neg(advisedby(systems,person368,person426)).
neg(advisedby(systems,person368,person62)).
neg(advisedby(systems,person368,person67)).
neg(advisedby(systems,person368,person80)).
neg(advisedby(systems,person368,person89)).
neg(advisedby(systems,person368,person92)).
neg(advisedby(systems,person368,person99)).
neg(advisedby(systems,person368,person101)).
neg(advisedby(systems,person368,person104)).
neg(advisedby(systems,person368,person107)).
neg(advisedby(systems,person368,person124)).
neg(advisedby(systems,person368,person179)).
neg(advisedby(systems,person368,person213)).
neg(advisedby(systems,person368,person234)).
neg(advisedby(systems,person368,person235)).
neg(advisedby(systems,person368,person373)).
neg(advisedby(systems,person368,person375)).
neg(advisedby(systems,person368,person98)).
neg(advisedby(systems,person374,person100)).
neg(advisedby(systems,person374,person116)).
neg(advisedby(systems,person374,person126)).
neg(advisedby(systems,person374,person129)).
neg(advisedby(systems,person374,person130)).
neg(advisedby(systems,person374,person154)).
neg(advisedby(systems,person374,person155)).
neg(advisedby(systems,person374,person204)).
neg(advisedby(systems,person374,person212)).
neg(advisedby(systems,person374,person218)).
neg(advisedby(systems,person374,person253)).
neg(advisedby(systems,person374,person280)).
neg(advisedby(systems,person374,person357)).
neg(advisedby(systems,person374,person368)).
neg(advisedby(systems,person374,person374)).
neg(advisedby(systems,person374,person376)).
neg(advisedby(systems,person374,person391)).
neg(advisedby(systems,person374,person403)).
neg(advisedby(systems,person374,person411)).
neg(advisedby(systems,person374,person419)).
neg(advisedby(systems,person374,person426)).
neg(advisedby(systems,person374,person62)).
neg(advisedby(systems,person374,person67)).
neg(advisedby(systems,person374,person80)).
neg(advisedby(systems,person374,person89)).
neg(advisedby(systems,person374,person92)).
neg(advisedby(systems,person374,person99)).
neg(advisedby(systems,person374,person101)).
neg(advisedby(systems,person374,person104)).
neg(advisedby(systems,person374,person107)).
neg(advisedby(systems,person374,person124)).
neg(advisedby(systems,person374,person180)).
neg(advisedby(systems,person374,person213)).
neg(advisedby(systems,person374,person234)).
neg(advisedby(systems,person374,person235)).
neg(advisedby(systems,person374,person373)).
neg(advisedby(systems,person374,person375)).
neg(advisedby(systems,person374,person98)).
neg(advisedby(systems,person376,person100)).
neg(advisedby(systems,person376,person116)).
neg(advisedby(systems,person376,person126)).
neg(advisedby(systems,person376,person129)).
neg(advisedby(systems,person376,person130)).
neg(advisedby(systems,person376,person154)).
neg(advisedby(systems,person376,person155)).
neg(advisedby(systems,person376,person204)).
neg(advisedby(systems,person376,person212)).
neg(advisedby(systems,person376,person218)).
neg(advisedby(systems,person376,person253)).
neg(advisedby(systems,person376,person280)).
neg(advisedby(systems,person376,person357)).
neg(advisedby(systems,person376,person368)).
neg(advisedby(systems,person376,person374)).
neg(advisedby(systems,person376,person376)).
neg(advisedby(systems,person376,person391)).
neg(advisedby(systems,person376,person403)).
neg(advisedby(systems,person376,person411)).
neg(advisedby(systems,person376,person419)).
neg(advisedby(systems,person376,person426)).
neg(advisedby(systems,person376,person62)).
neg(advisedby(systems,person376,person67)).
neg(advisedby(systems,person376,person80)).
neg(advisedby(systems,person376,person89)).
neg(advisedby(systems,person376,person92)).
neg(advisedby(systems,person376,person99)).
neg(advisedby(systems,person376,person101)).
neg(advisedby(systems,person376,person104)).
neg(advisedby(systems,person376,person124)).
neg(advisedby(systems,person376,person180)).
neg(advisedby(systems,person376,person213)).
neg(advisedby(systems,person376,person234)).
neg(advisedby(systems,person376,person235)).
neg(advisedby(systems,person376,person373)).
neg(advisedby(systems,person376,person375)).
neg(advisedby(systems,person376,person98)).
neg(advisedby(systems,person391,person100)).
neg(advisedby(systems,person391,person116)).
neg(advisedby(systems,person391,person126)).
neg(advisedby(systems,person391,person129)).
neg(advisedby(systems,person391,person130)).
neg(advisedby(systems,person391,person154)).
neg(advisedby(systems,person391,person155)).
neg(advisedby(systems,person391,person204)).
neg(advisedby(systems,person391,person212)).
neg(advisedby(systems,person391,person218)).
neg(advisedby(systems,person391,person253)).
neg(advisedby(systems,person391,person280)).
neg(advisedby(systems,person391,person357)).
neg(advisedby(systems,person391,person368)).
neg(advisedby(systems,person391,person374)).
neg(advisedby(systems,person391,person376)).
neg(advisedby(systems,person391,person391)).
neg(advisedby(systems,person391,person403)).
neg(advisedby(systems,person391,person411)).
neg(advisedby(systems,person391,person419)).
neg(advisedby(systems,person391,person426)).
neg(advisedby(systems,person391,person62)).
neg(advisedby(systems,person391,person67)).
neg(advisedby(systems,person391,person80)).
neg(advisedby(systems,person391,person89)).
neg(advisedby(systems,person391,person92)).
neg(advisedby(systems,person391,person99)).
neg(advisedby(systems,person391,person101)).
neg(advisedby(systems,person391,person104)).
neg(advisedby(systems,person391,person107)).
neg(advisedby(systems,person391,person124)).
neg(advisedby(systems,person391,person179)).
neg(advisedby(systems,person391,person180)).
neg(advisedby(systems,person391,person213)).
neg(advisedby(systems,person391,person234)).
neg(advisedby(systems,person391,person373)).
neg(advisedby(systems,person391,person375)).
neg(advisedby(systems,person391,person98)).
neg(advisedby(systems,person403,person100)).
neg(advisedby(systems,person403,person116)).
neg(advisedby(systems,person403,person126)).
neg(advisedby(systems,person403,person129)).
neg(advisedby(systems,person403,person130)).
neg(advisedby(systems,person403,person154)).
neg(advisedby(systems,person403,person155)).
neg(advisedby(systems,person403,person204)).
neg(advisedby(systems,person403,person212)).
neg(advisedby(systems,person403,person218)).
neg(advisedby(systems,person403,person253)).
neg(advisedby(systems,person403,person280)).
neg(advisedby(systems,person403,person357)).
neg(advisedby(systems,person403,person368)).
neg(advisedby(systems,person403,person374)).
neg(advisedby(systems,person403,person376)).
neg(advisedby(systems,person403,person391)).
neg(advisedby(systems,person403,person403)).
neg(advisedby(systems,person403,person411)).
neg(advisedby(systems,person403,person419)).
neg(advisedby(systems,person403,person426)).
neg(advisedby(systems,person403,person62)).
neg(advisedby(systems,person403,person67)).
neg(advisedby(systems,person403,person80)).
neg(advisedby(systems,person403,person89)).
neg(advisedby(systems,person403,person92)).
neg(advisedby(systems,person403,person99)).
neg(advisedby(systems,person403,person101)).
neg(advisedby(systems,person403,person104)).
neg(advisedby(systems,person403,person107)).
neg(advisedby(systems,person403,person124)).
neg(advisedby(systems,person403,person179)).
neg(advisedby(systems,person403,person180)).
neg(advisedby(systems,person403,person213)).
neg(advisedby(systems,person403,person235)).
neg(advisedby(systems,person403,person373)).
neg(advisedby(systems,person403,person375)).
neg(advisedby(systems,person403,person98)).
neg(advisedby(systems,person411,person100)).
neg(advisedby(systems,person411,person116)).
neg(advisedby(systems,person411,person126)).
neg(advisedby(systems,person411,person129)).
neg(advisedby(systems,person411,person130)).
neg(advisedby(systems,person411,person154)).
neg(advisedby(systems,person411,person155)).
neg(advisedby(systems,person411,person204)).
neg(advisedby(systems,person411,person212)).
neg(advisedby(systems,person411,person218)).
neg(advisedby(systems,person411,person253)).
neg(advisedby(systems,person411,person280)).
neg(advisedby(systems,person411,person357)).
neg(advisedby(systems,person411,person368)).
neg(advisedby(systems,person411,person374)).
neg(advisedby(systems,person411,person376)).
neg(advisedby(systems,person411,person391)).
neg(advisedby(systems,person411,person403)).
neg(advisedby(systems,person411,person411)).
neg(advisedby(systems,person411,person419)).
neg(advisedby(systems,person411,person426)).
neg(advisedby(systems,person411,person62)).
neg(advisedby(systems,person411,person67)).
neg(advisedby(systems,person411,person80)).
neg(advisedby(systems,person411,person89)).
neg(advisedby(systems,person411,person92)).
neg(advisedby(systems,person411,person99)).
neg(advisedby(systems,person411,person101)).
neg(advisedby(systems,person411,person104)).
neg(advisedby(systems,person411,person107)).
neg(advisedby(systems,person411,person124)).
neg(advisedby(systems,person411,person179)).
neg(advisedby(systems,person411,person180)).
neg(advisedby(systems,person411,person213)).
neg(advisedby(systems,person411,person234)).
neg(advisedby(systems,person411,person235)).
neg(advisedby(systems,person411,person375)).
neg(advisedby(systems,person411,person98)).
neg(advisedby(systems,person419,person100)).
neg(advisedby(systems,person419,person116)).
neg(advisedby(systems,person419,person126)).
neg(advisedby(systems,person419,person129)).
neg(advisedby(systems,person419,person130)).
neg(advisedby(systems,person419,person154)).
neg(advisedby(systems,person419,person155)).
neg(advisedby(systems,person419,person204)).
neg(advisedby(systems,person419,person212)).
neg(advisedby(systems,person419,person218)).
neg(advisedby(systems,person419,person253)).
neg(advisedby(systems,person419,person280)).
neg(advisedby(systems,person419,person357)).
neg(advisedby(systems,person419,person368)).
neg(advisedby(systems,person419,person374)).
neg(advisedby(systems,person419,person376)).
neg(advisedby(systems,person419,person391)).
neg(advisedby(systems,person419,person403)).
neg(advisedby(systems,person419,person411)).
neg(advisedby(systems,person419,person419)).
neg(advisedby(systems,person419,person426)).
neg(advisedby(systems,person419,person62)).
neg(advisedby(systems,person419,person67)).
neg(advisedby(systems,person419,person80)).
neg(advisedby(systems,person419,person89)).
neg(advisedby(systems,person419,person92)).
neg(advisedby(systems,person419,person99)).
neg(advisedby(systems,person419,person104)).
neg(advisedby(systems,person419,person107)).
neg(advisedby(systems,person419,person124)).
neg(advisedby(systems,person419,person179)).
neg(advisedby(systems,person419,person180)).
neg(advisedby(systems,person419,person213)).
neg(advisedby(systems,person419,person234)).
neg(advisedby(systems,person419,person235)).
neg(advisedby(systems,person419,person373)).
neg(advisedby(systems,person419,person375)).
neg(advisedby(systems,person419,person98)).
neg(advisedby(systems,person426,person100)).
neg(advisedby(systems,person426,person116)).
neg(advisedby(systems,person426,person126)).
neg(advisedby(systems,person426,person129)).
neg(advisedby(systems,person426,person130)).
neg(advisedby(systems,person426,person154)).
neg(advisedby(systems,person426,person155)).
neg(advisedby(systems,person426,person204)).
neg(advisedby(systems,person426,person212)).
neg(advisedby(systems,person426,person218)).
neg(advisedby(systems,person426,person253)).
neg(advisedby(systems,person426,person280)).
neg(advisedby(systems,person426,person357)).
neg(advisedby(systems,person426,person368)).
neg(advisedby(systems,person426,person374)).
neg(advisedby(systems,person426,person376)).
neg(advisedby(systems,person426,person391)).
neg(advisedby(systems,person426,person403)).
neg(advisedby(systems,person426,person411)).
neg(advisedby(systems,person426,person419)).
neg(advisedby(systems,person426,person426)).
neg(advisedby(systems,person426,person62)).
neg(advisedby(systems,person426,person67)).
neg(advisedby(systems,person426,person80)).
neg(advisedby(systems,person426,person89)).
neg(advisedby(systems,person426,person92)).
neg(advisedby(systems,person426,person99)).
neg(advisedby(systems,person426,person101)).
neg(advisedby(systems,person426,person104)).
neg(advisedby(systems,person426,person107)).
neg(advisedby(systems,person426,person124)).
neg(advisedby(systems,person426,person180)).
neg(advisedby(systems,person426,person213)).
neg(advisedby(systems,person426,person234)).
neg(advisedby(systems,person426,person373)).
neg(advisedby(systems,person426,person375)).
neg(advisedby(systems,person426,person98)).
neg(advisedby(systems,person62,person100)).
neg(advisedby(systems,person62,person116)).
neg(advisedby(systems,person62,person126)).
neg(advisedby(systems,person62,person129)).
neg(advisedby(systems,person62,person130)).
neg(advisedby(systems,person62,person154)).
neg(advisedby(systems,person62,person155)).
neg(advisedby(systems,person62,person204)).
neg(advisedby(systems,person62,person212)).
neg(advisedby(systems,person62,person218)).
neg(advisedby(systems,person62,person253)).
neg(advisedby(systems,person62,person280)).
neg(advisedby(systems,person62,person357)).
neg(advisedby(systems,person62,person368)).
neg(advisedby(systems,person62,person374)).
neg(advisedby(systems,person62,person376)).
neg(advisedby(systems,person62,person391)).
neg(advisedby(systems,person62,person403)).
neg(advisedby(systems,person62,person411)).
neg(advisedby(systems,person62,person419)).
neg(advisedby(systems,person62,person426)).
neg(advisedby(systems,person62,person62)).
neg(advisedby(systems,person62,person67)).
neg(advisedby(systems,person62,person80)).
neg(advisedby(systems,person62,person89)).
neg(advisedby(systems,person62,person92)).
neg(advisedby(systems,person62,person99)).
neg(advisedby(systems,person62,person101)).
neg(advisedby(systems,person62,person107)).
neg(advisedby(systems,person62,person124)).
neg(advisedby(systems,person62,person179)).
neg(advisedby(systems,person62,person180)).
neg(advisedby(systems,person62,person213)).
neg(advisedby(systems,person62,person234)).
neg(advisedby(systems,person62,person235)).
neg(advisedby(systems,person62,person373)).
neg(advisedby(systems,person62,person375)).
neg(advisedby(systems,person62,person98)).
neg(advisedby(systems,person67,person100)).
neg(advisedby(systems,person67,person116)).
neg(advisedby(systems,person67,person126)).
neg(advisedby(systems,person67,person129)).
neg(advisedby(systems,person67,person130)).
neg(advisedby(systems,person67,person154)).
neg(advisedby(systems,person67,person155)).
neg(advisedby(systems,person67,person204)).
neg(advisedby(systems,person67,person212)).
neg(advisedby(systems,person67,person218)).
neg(advisedby(systems,person67,person253)).
neg(advisedby(systems,person67,person280)).
neg(advisedby(systems,person67,person357)).
neg(advisedby(systems,person67,person368)).
neg(advisedby(systems,person67,person374)).
neg(advisedby(systems,person67,person376)).
neg(advisedby(systems,person67,person391)).
neg(advisedby(systems,person67,person403)).
neg(advisedby(systems,person67,person411)).
neg(advisedby(systems,person67,person419)).
neg(advisedby(systems,person67,person426)).
neg(advisedby(systems,person67,person62)).
neg(advisedby(systems,person67,person67)).
neg(advisedby(systems,person67,person80)).
neg(advisedby(systems,person67,person89)).
neg(advisedby(systems,person67,person92)).
neg(advisedby(systems,person67,person99)).
neg(advisedby(systems,person67,person101)).
neg(advisedby(systems,person67,person104)).
neg(advisedby(systems,person67,person107)).
neg(advisedby(systems,person67,person124)).
neg(advisedby(systems,person67,person179)).
neg(advisedby(systems,person67,person180)).
neg(advisedby(systems,person67,person213)).
neg(advisedby(systems,person67,person234)).
neg(advisedby(systems,person67,person235)).
neg(advisedby(systems,person67,person373)).
neg(advisedby(systems,person80,person100)).
neg(advisedby(systems,person80,person116)).
neg(advisedby(systems,person80,person126)).
neg(advisedby(systems,person80,person129)).
neg(advisedby(systems,person80,person130)).
neg(advisedby(systems,person80,person154)).
neg(advisedby(systems,person80,person155)).
neg(advisedby(systems,person80,person204)).
neg(advisedby(systems,person80,person212)).
neg(advisedby(systems,person80,person218)).
neg(advisedby(systems,person80,person253)).
neg(advisedby(systems,person80,person280)).
neg(advisedby(systems,person80,person357)).
neg(advisedby(systems,person80,person368)).
neg(advisedby(systems,person80,person374)).
neg(advisedby(systems,person80,person376)).
neg(advisedby(systems,person80,person391)).
neg(advisedby(systems,person80,person403)).
neg(advisedby(systems,person80,person411)).
neg(advisedby(systems,person80,person419)).
neg(advisedby(systems,person80,person426)).
neg(advisedby(systems,person80,person62)).
neg(advisedby(systems,person80,person67)).
neg(advisedby(systems,person80,person80)).
neg(advisedby(systems,person80,person89)).
neg(advisedby(systems,person80,person92)).
neg(advisedby(systems,person80,person99)).
neg(advisedby(systems,person80,person101)).
neg(advisedby(systems,person80,person104)).
neg(advisedby(systems,person80,person107)).
neg(advisedby(systems,person80,person124)).
neg(advisedby(systems,person80,person179)).
neg(advisedby(systems,person80,person180)).
neg(advisedby(systems,person80,person213)).
neg(advisedby(systems,person80,person235)).
neg(advisedby(systems,person80,person373)).
neg(advisedby(systems,person80,person375)).
neg(advisedby(systems,person80,person98)).
neg(advisedby(systems,person89,person100)).
neg(advisedby(systems,person89,person116)).
neg(advisedby(systems,person89,person126)).
neg(advisedby(systems,person89,person129)).
neg(advisedby(systems,person89,person130)).
neg(advisedby(systems,person89,person154)).
neg(advisedby(systems,person89,person155)).
neg(advisedby(systems,person89,person204)).
neg(advisedby(systems,person89,person212)).
neg(advisedby(systems,person89,person218)).
neg(advisedby(systems,person89,person253)).
neg(advisedby(systems,person89,person280)).
neg(advisedby(systems,person89,person357)).
neg(advisedby(systems,person89,person368)).
neg(advisedby(systems,person89,person374)).
neg(advisedby(systems,person89,person376)).
neg(advisedby(systems,person89,person391)).
neg(advisedby(systems,person89,person403)).
neg(advisedby(systems,person89,person411)).
neg(advisedby(systems,person89,person419)).
neg(advisedby(systems,person89,person426)).
neg(advisedby(systems,person89,person62)).
neg(advisedby(systems,person89,person67)).
neg(advisedby(systems,person89,person80)).
neg(advisedby(systems,person89,person89)).
neg(advisedby(systems,person89,person92)).
neg(advisedby(systems,person89,person99)).
neg(advisedby(systems,person89,person101)).
neg(advisedby(systems,person89,person107)).
neg(advisedby(systems,person89,person124)).
neg(advisedby(systems,person89,person179)).
neg(advisedby(systems,person89,person180)).
neg(advisedby(systems,person89,person213)).
neg(advisedby(systems,person89,person234)).
neg(advisedby(systems,person89,person235)).
neg(advisedby(systems,person89,person373)).
neg(advisedby(systems,person89,person375)).
neg(advisedby(systems,person89,person98)).
neg(advisedby(systems,person92,person100)).
neg(advisedby(systems,person92,person116)).
neg(advisedby(systems,person92,person126)).
neg(advisedby(systems,person92,person129)).
neg(advisedby(systems,person92,person130)).
neg(advisedby(systems,person92,person154)).
neg(advisedby(systems,person92,person155)).
neg(advisedby(systems,person92,person204)).
neg(advisedby(systems,person92,person212)).
neg(advisedby(systems,person92,person218)).
neg(advisedby(systems,person92,person253)).
neg(advisedby(systems,person92,person280)).
neg(advisedby(systems,person92,person357)).
neg(advisedby(systems,person92,person368)).
neg(advisedby(systems,person92,person374)).
neg(advisedby(systems,person92,person376)).
neg(advisedby(systems,person92,person391)).
neg(advisedby(systems,person92,person403)).
neg(advisedby(systems,person92,person411)).
neg(advisedby(systems,person92,person419)).
neg(advisedby(systems,person92,person426)).
neg(advisedby(systems,person92,person62)).
neg(advisedby(systems,person92,person67)).
neg(advisedby(systems,person92,person80)).
neg(advisedby(systems,person92,person89)).
neg(advisedby(systems,person92,person92)).
neg(advisedby(systems,person92,person99)).
neg(advisedby(systems,person92,person104)).
neg(advisedby(systems,person92,person107)).
neg(advisedby(systems,person92,person124)).
neg(advisedby(systems,person92,person179)).
neg(advisedby(systems,person92,person180)).
neg(advisedby(systems,person92,person213)).
neg(advisedby(systems,person92,person234)).
neg(advisedby(systems,person92,person235)).
neg(advisedby(systems,person92,person373)).
neg(advisedby(systems,person92,person375)).
neg(advisedby(systems,person92,person98)).
neg(advisedby(systems,person99,person100)).
neg(advisedby(systems,person99,person116)).
neg(advisedby(systems,person99,person126)).
neg(advisedby(systems,person99,person129)).
neg(advisedby(systems,person99,person130)).
neg(advisedby(systems,person99,person154)).
neg(advisedby(systems,person99,person155)).
neg(advisedby(systems,person99,person204)).
neg(advisedby(systems,person99,person212)).
neg(advisedby(systems,person99,person218)).
neg(advisedby(systems,person99,person253)).
neg(advisedby(systems,person99,person280)).
neg(advisedby(systems,person99,person357)).
neg(advisedby(systems,person99,person368)).
neg(advisedby(systems,person99,person374)).
neg(advisedby(systems,person99,person376)).
neg(advisedby(systems,person99,person391)).
neg(advisedby(systems,person99,person403)).
neg(advisedby(systems,person99,person411)).
neg(advisedby(systems,person99,person419)).
neg(advisedby(systems,person99,person426)).
neg(advisedby(systems,person99,person62)).
neg(advisedby(systems,person99,person67)).
neg(advisedby(systems,person99,person80)).
neg(advisedby(systems,person99,person89)).
neg(advisedby(systems,person99,person92)).
neg(advisedby(systems,person99,person99)).
neg(advisedby(systems,person99,person101)).
neg(advisedby(systems,person99,person107)).
neg(advisedby(systems,person99,person124)).
neg(advisedby(systems,person99,person179)).
neg(advisedby(systems,person99,person180)).
neg(advisedby(systems,person99,person213)).
neg(advisedby(systems,person99,person234)).
neg(advisedby(systems,person99,person235)).
neg(advisedby(systems,person99,person373)).
neg(advisedby(systems,person99,person375)).
neg(advisedby(systems,person99,person98)).
neg(advisedby(systems,person101,person100)).
neg(advisedby(systems,person101,person116)).
neg(advisedby(systems,person101,person126)).
neg(advisedby(systems,person101,person129)).
neg(advisedby(systems,person101,person130)).
neg(advisedby(systems,person101,person154)).
neg(advisedby(systems,person101,person155)).
neg(advisedby(systems,person101,person204)).
neg(advisedby(systems,person101,person212)).
neg(advisedby(systems,person101,person218)).
neg(advisedby(systems,person101,person253)).
neg(advisedby(systems,person101,person280)).
neg(advisedby(systems,person101,person357)).
neg(advisedby(systems,person101,person368)).
neg(advisedby(systems,person101,person374)).
neg(advisedby(systems,person101,person376)).
neg(advisedby(systems,person101,person391)).
neg(advisedby(systems,person101,person403)).
neg(advisedby(systems,person101,person411)).
neg(advisedby(systems,person101,person419)).
neg(advisedby(systems,person101,person426)).
neg(advisedby(systems,person101,person62)).
neg(advisedby(systems,person101,person67)).
neg(advisedby(systems,person101,person80)).
neg(advisedby(systems,person101,person89)).
neg(advisedby(systems,person101,person92)).
neg(advisedby(systems,person101,person99)).
neg(advisedby(systems,person101,person101)).
neg(advisedby(systems,person101,person104)).
neg(advisedby(systems,person101,person107)).
neg(advisedby(systems,person101,person124)).
neg(advisedby(systems,person101,person179)).
neg(advisedby(systems,person101,person180)).
neg(advisedby(systems,person101,person213)).
neg(advisedby(systems,person101,person234)).
neg(advisedby(systems,person101,person235)).
neg(advisedby(systems,person101,person373)).
neg(advisedby(systems,person101,person375)).
neg(advisedby(systems,person101,person98)).
neg(advisedby(systems,person104,person100)).
neg(advisedby(systems,person104,person116)).
neg(advisedby(systems,person104,person126)).
neg(advisedby(systems,person104,person129)).
neg(advisedby(systems,person104,person130)).
neg(advisedby(systems,person104,person154)).
neg(advisedby(systems,person104,person155)).
neg(advisedby(systems,person104,person204)).
neg(advisedby(systems,person104,person212)).
neg(advisedby(systems,person104,person218)).
neg(advisedby(systems,person104,person253)).
neg(advisedby(systems,person104,person280)).
neg(advisedby(systems,person104,person357)).
neg(advisedby(systems,person104,person368)).
neg(advisedby(systems,person104,person374)).
neg(advisedby(systems,person104,person376)).
neg(advisedby(systems,person104,person391)).
neg(advisedby(systems,person104,person403)).
neg(advisedby(systems,person104,person411)).
neg(advisedby(systems,person104,person419)).
neg(advisedby(systems,person104,person426)).
neg(advisedby(systems,person104,person62)).
neg(advisedby(systems,person104,person67)).
neg(advisedby(systems,person104,person80)).
neg(advisedby(systems,person104,person89)).
neg(advisedby(systems,person104,person92)).
neg(advisedby(systems,person104,person99)).
neg(advisedby(systems,person104,person101)).
neg(advisedby(systems,person104,person104)).
neg(advisedby(systems,person104,person107)).
neg(advisedby(systems,person104,person124)).
neg(advisedby(systems,person104,person179)).
neg(advisedby(systems,person104,person180)).
neg(advisedby(systems,person104,person213)).
neg(advisedby(systems,person104,person234)).
neg(advisedby(systems,person104,person235)).
neg(advisedby(systems,person104,person373)).
neg(advisedby(systems,person104,person375)).
neg(advisedby(systems,person104,person98)).
neg(advisedby(systems,person107,person100)).
neg(advisedby(systems,person107,person116)).
neg(advisedby(systems,person107,person126)).
neg(advisedby(systems,person107,person129)).
neg(advisedby(systems,person107,person130)).
neg(advisedby(systems,person107,person154)).
neg(advisedby(systems,person107,person155)).
neg(advisedby(systems,person107,person204)).
neg(advisedby(systems,person107,person212)).
neg(advisedby(systems,person107,person218)).
neg(advisedby(systems,person107,person253)).
neg(advisedby(systems,person107,person280)).
neg(advisedby(systems,person107,person357)).
neg(advisedby(systems,person107,person368)).
neg(advisedby(systems,person107,person374)).
neg(advisedby(systems,person107,person376)).
neg(advisedby(systems,person107,person391)).
neg(advisedby(systems,person107,person403)).
neg(advisedby(systems,person107,person411)).
neg(advisedby(systems,person107,person419)).
neg(advisedby(systems,person107,person426)).
neg(advisedby(systems,person107,person62)).
neg(advisedby(systems,person107,person67)).
neg(advisedby(systems,person107,person80)).
neg(advisedby(systems,person107,person89)).
neg(advisedby(systems,person107,person92)).
neg(advisedby(systems,person107,person99)).
neg(advisedby(systems,person107,person101)).
neg(advisedby(systems,person107,person104)).
neg(advisedby(systems,person107,person107)).
neg(advisedby(systems,person107,person124)).
neg(advisedby(systems,person107,person179)).
neg(advisedby(systems,person107,person180)).
neg(advisedby(systems,person107,person213)).
neg(advisedby(systems,person107,person234)).
neg(advisedby(systems,person107,person235)).
neg(advisedby(systems,person107,person373)).
neg(advisedby(systems,person107,person375)).
neg(advisedby(systems,person107,person98)).
neg(advisedby(systems,person124,person100)).
neg(advisedby(systems,person124,person116)).
neg(advisedby(systems,person124,person126)).
neg(advisedby(systems,person124,person129)).
neg(advisedby(systems,person124,person130)).
neg(advisedby(systems,person124,person154)).
neg(advisedby(systems,person124,person155)).
neg(advisedby(systems,person124,person204)).
neg(advisedby(systems,person124,person212)).
neg(advisedby(systems,person124,person218)).
neg(advisedby(systems,person124,person253)).
neg(advisedby(systems,person124,person280)).
neg(advisedby(systems,person124,person357)).
neg(advisedby(systems,person124,person368)).
neg(advisedby(systems,person124,person374)).
neg(advisedby(systems,person124,person376)).
neg(advisedby(systems,person124,person391)).
neg(advisedby(systems,person124,person403)).
neg(advisedby(systems,person124,person411)).
neg(advisedby(systems,person124,person419)).
neg(advisedby(systems,person124,person426)).
neg(advisedby(systems,person124,person62)).
neg(advisedby(systems,person124,person67)).
neg(advisedby(systems,person124,person80)).
neg(advisedby(systems,person124,person89)).
neg(advisedby(systems,person124,person92)).
neg(advisedby(systems,person124,person99)).
neg(advisedby(systems,person124,person101)).
neg(advisedby(systems,person124,person104)).
neg(advisedby(systems,person124,person107)).
neg(advisedby(systems,person124,person124)).
neg(advisedby(systems,person124,person179)).
neg(advisedby(systems,person124,person180)).
neg(advisedby(systems,person124,person213)).
neg(advisedby(systems,person124,person234)).
neg(advisedby(systems,person124,person235)).
neg(advisedby(systems,person124,person373)).
neg(advisedby(systems,person124,person375)).
neg(advisedby(systems,person124,person98)).
neg(advisedby(systems,person179,person100)).
neg(advisedby(systems,person179,person116)).
neg(advisedby(systems,person179,person126)).
neg(advisedby(systems,person179,person129)).
neg(advisedby(systems,person179,person130)).
neg(advisedby(systems,person179,person154)).
neg(advisedby(systems,person179,person155)).
neg(advisedby(systems,person179,person204)).
neg(advisedby(systems,person179,person212)).
neg(advisedby(systems,person179,person218)).
neg(advisedby(systems,person179,person253)).
neg(advisedby(systems,person179,person280)).
neg(advisedby(systems,person179,person357)).
neg(advisedby(systems,person179,person368)).
neg(advisedby(systems,person179,person374)).
neg(advisedby(systems,person179,person376)).
neg(advisedby(systems,person179,person391)).
neg(advisedby(systems,person179,person403)).
neg(advisedby(systems,person179,person411)).
neg(advisedby(systems,person179,person419)).
neg(advisedby(systems,person179,person426)).
neg(advisedby(systems,person179,person62)).
neg(advisedby(systems,person179,person67)).
neg(advisedby(systems,person179,person80)).
neg(advisedby(systems,person179,person89)).
neg(advisedby(systems,person179,person92)).
neg(advisedby(systems,person179,person99)).
neg(advisedby(systems,person179,person101)).
neg(advisedby(systems,person179,person104)).
neg(advisedby(systems,person179,person107)).
neg(advisedby(systems,person179,person124)).
neg(advisedby(systems,person179,person179)).
neg(advisedby(systems,person179,person180)).
neg(advisedby(systems,person179,person213)).
neg(advisedby(systems,person179,person234)).
neg(advisedby(systems,person179,person235)).
neg(advisedby(systems,person179,person373)).
neg(advisedby(systems,person179,person375)).
neg(advisedby(systems,person179,person98)).
neg(advisedby(systems,person180,person100)).
neg(advisedby(systems,person180,person116)).
neg(advisedby(systems,person180,person126)).
neg(advisedby(systems,person180,person129)).
neg(advisedby(systems,person180,person130)).
neg(advisedby(systems,person180,person154)).
neg(advisedby(systems,person180,person155)).
neg(advisedby(systems,person180,person204)).
neg(advisedby(systems,person180,person212)).
neg(advisedby(systems,person180,person218)).
neg(advisedby(systems,person180,person253)).
neg(advisedby(systems,person180,person280)).
neg(advisedby(systems,person180,person357)).
neg(advisedby(systems,person180,person368)).
neg(advisedby(systems,person180,person374)).
neg(advisedby(systems,person180,person376)).
neg(advisedby(systems,person180,person391)).
neg(advisedby(systems,person180,person403)).
neg(advisedby(systems,person180,person411)).
neg(advisedby(systems,person180,person419)).
neg(advisedby(systems,person180,person426)).
neg(advisedby(systems,person180,person62)).
neg(advisedby(systems,person180,person67)).
neg(advisedby(systems,person180,person80)).
neg(advisedby(systems,person180,person89)).
neg(advisedby(systems,person180,person92)).
neg(advisedby(systems,person180,person99)).
neg(advisedby(systems,person180,person101)).
neg(advisedby(systems,person180,person104)).
neg(advisedby(systems,person180,person107)).
neg(advisedby(systems,person180,person124)).
neg(advisedby(systems,person180,person179)).
neg(advisedby(systems,person180,person180)).
neg(advisedby(systems,person180,person213)).
neg(advisedby(systems,person180,person234)).
neg(advisedby(systems,person180,person235)).
neg(advisedby(systems,person180,person373)).
neg(advisedby(systems,person180,person375)).
neg(advisedby(systems,person180,person98)).
neg(advisedby(systems,person213,person100)).
neg(advisedby(systems,person213,person116)).
neg(advisedby(systems,person213,person126)).
neg(advisedby(systems,person213,person129)).
neg(advisedby(systems,person213,person130)).
neg(advisedby(systems,person213,person154)).
neg(advisedby(systems,person213,person155)).
neg(advisedby(systems,person213,person204)).
neg(advisedby(systems,person213,person212)).
neg(advisedby(systems,person213,person218)).
neg(advisedby(systems,person213,person253)).
neg(advisedby(systems,person213,person280)).
neg(advisedby(systems,person213,person357)).
neg(advisedby(systems,person213,person368)).
neg(advisedby(systems,person213,person374)).
neg(advisedby(systems,person213,person376)).
neg(advisedby(systems,person213,person391)).
neg(advisedby(systems,person213,person403)).
neg(advisedby(systems,person213,person411)).
neg(advisedby(systems,person213,person419)).
neg(advisedby(systems,person213,person426)).
neg(advisedby(systems,person213,person62)).
neg(advisedby(systems,person213,person67)).
neg(advisedby(systems,person213,person80)).
neg(advisedby(systems,person213,person89)).
neg(advisedby(systems,person213,person92)).
neg(advisedby(systems,person213,person99)).
neg(advisedby(systems,person213,person101)).
neg(advisedby(systems,person213,person104)).
neg(advisedby(systems,person213,person107)).
neg(advisedby(systems,person213,person124)).
neg(advisedby(systems,person213,person179)).
neg(advisedby(systems,person213,person180)).
neg(advisedby(systems,person213,person213)).
neg(advisedby(systems,person213,person234)).
neg(advisedby(systems,person213,person235)).
neg(advisedby(systems,person213,person373)).
neg(advisedby(systems,person213,person375)).
neg(advisedby(systems,person213,person98)).
neg(advisedby(systems,person234,person100)).
neg(advisedby(systems,person234,person116)).
neg(advisedby(systems,person234,person126)).
neg(advisedby(systems,person234,person129)).
neg(advisedby(systems,person234,person130)).
neg(advisedby(systems,person234,person154)).
neg(advisedby(systems,person234,person155)).
neg(advisedby(systems,person234,person204)).
neg(advisedby(systems,person234,person212)).
neg(advisedby(systems,person234,person218)).
neg(advisedby(systems,person234,person253)).
neg(advisedby(systems,person234,person280)).
neg(advisedby(systems,person234,person357)).
neg(advisedby(systems,person234,person368)).
neg(advisedby(systems,person234,person374)).
neg(advisedby(systems,person234,person376)).
neg(advisedby(systems,person234,person391)).
neg(advisedby(systems,person234,person403)).
neg(advisedby(systems,person234,person411)).
neg(advisedby(systems,person234,person419)).
neg(advisedby(systems,person234,person426)).
neg(advisedby(systems,person234,person62)).
neg(advisedby(systems,person234,person67)).
neg(advisedby(systems,person234,person80)).
neg(advisedby(systems,person234,person89)).
neg(advisedby(systems,person234,person92)).
neg(advisedby(systems,person234,person99)).
neg(advisedby(systems,person234,person101)).
neg(advisedby(systems,person234,person104)).
neg(advisedby(systems,person234,person107)).
neg(advisedby(systems,person234,person124)).
neg(advisedby(systems,person234,person179)).
neg(advisedby(systems,person234,person180)).
neg(advisedby(systems,person234,person213)).
neg(advisedby(systems,person234,person234)).
neg(advisedby(systems,person234,person235)).
neg(advisedby(systems,person234,person373)).
neg(advisedby(systems,person234,person375)).
neg(advisedby(systems,person234,person98)).
neg(advisedby(systems,person235,person100)).
neg(advisedby(systems,person235,person116)).
neg(advisedby(systems,person235,person126)).
neg(advisedby(systems,person235,person129)).
neg(advisedby(systems,person235,person130)).
neg(advisedby(systems,person235,person154)).
neg(advisedby(systems,person235,person155)).
neg(advisedby(systems,person235,person204)).
neg(advisedby(systems,person235,person212)).
neg(advisedby(systems,person235,person218)).
neg(advisedby(systems,person235,person253)).
neg(advisedby(systems,person235,person280)).
neg(advisedby(systems,person235,person357)).
neg(advisedby(systems,person235,person368)).
neg(advisedby(systems,person235,person374)).
neg(advisedby(systems,person235,person376)).
neg(advisedby(systems,person235,person391)).
neg(advisedby(systems,person235,person403)).
neg(advisedby(systems,person235,person411)).
neg(advisedby(systems,person235,person419)).
neg(advisedby(systems,person235,person426)).
neg(advisedby(systems,person235,person62)).
neg(advisedby(systems,person235,person67)).
neg(advisedby(systems,person235,person80)).
neg(advisedby(systems,person235,person89)).
neg(advisedby(systems,person235,person92)).
neg(advisedby(systems,person235,person99)).
neg(advisedby(systems,person235,person101)).
neg(advisedby(systems,person235,person104)).
neg(advisedby(systems,person235,person107)).
neg(advisedby(systems,person235,person124)).
neg(advisedby(systems,person235,person179)).
neg(advisedby(systems,person235,person180)).
neg(advisedby(systems,person235,person213)).
neg(advisedby(systems,person235,person234)).
neg(advisedby(systems,person235,person235)).
neg(advisedby(systems,person235,person373)).
neg(advisedby(systems,person235,person375)).
neg(advisedby(systems,person235,person98)).
neg(advisedby(systems,person373,person100)).
neg(advisedby(systems,person373,person116)).
neg(advisedby(systems,person373,person126)).
neg(advisedby(systems,person373,person129)).
neg(advisedby(systems,person373,person130)).
neg(advisedby(systems,person373,person154)).
neg(advisedby(systems,person373,person155)).
neg(advisedby(systems,person373,person204)).
neg(advisedby(systems,person373,person212)).
neg(advisedby(systems,person373,person218)).
neg(advisedby(systems,person373,person253)).
neg(advisedby(systems,person373,person280)).
neg(advisedby(systems,person373,person357)).
neg(advisedby(systems,person373,person368)).
neg(advisedby(systems,person373,person374)).
neg(advisedby(systems,person373,person376)).
neg(advisedby(systems,person373,person391)).
neg(advisedby(systems,person373,person403)).
neg(advisedby(systems,person373,person411)).
neg(advisedby(systems,person373,person419)).
neg(advisedby(systems,person373,person426)).
neg(advisedby(systems,person373,person62)).
neg(advisedby(systems,person373,person67)).
neg(advisedby(systems,person373,person80)).
neg(advisedby(systems,person373,person89)).
neg(advisedby(systems,person373,person92)).
neg(advisedby(systems,person373,person99)).
neg(advisedby(systems,person373,person101)).
neg(advisedby(systems,person373,person104)).
neg(advisedby(systems,person373,person107)).
neg(advisedby(systems,person373,person124)).
neg(advisedby(systems,person373,person179)).
neg(advisedby(systems,person373,person180)).
neg(advisedby(systems,person373,person213)).
neg(advisedby(systems,person373,person234)).
neg(advisedby(systems,person373,person235)).
neg(advisedby(systems,person373,person373)).
neg(advisedby(systems,person373,person375)).
neg(advisedby(systems,person373,person98)).
neg(advisedby(systems,person375,person100)).
neg(advisedby(systems,person375,person116)).
neg(advisedby(systems,person375,person126)).
neg(advisedby(systems,person375,person129)).
neg(advisedby(systems,person375,person130)).
neg(advisedby(systems,person375,person154)).
neg(advisedby(systems,person375,person155)).
neg(advisedby(systems,person375,person204)).
neg(advisedby(systems,person375,person212)).
neg(advisedby(systems,person375,person218)).
neg(advisedby(systems,person375,person253)).
neg(advisedby(systems,person375,person280)).
neg(advisedby(systems,person375,person357)).
neg(advisedby(systems,person375,person368)).
neg(advisedby(systems,person375,person374)).
neg(advisedby(systems,person375,person376)).
neg(advisedby(systems,person375,person391)).
neg(advisedby(systems,person375,person403)).
neg(advisedby(systems,person375,person411)).
neg(advisedby(systems,person375,person419)).
neg(advisedby(systems,person375,person426)).
neg(advisedby(systems,person375,person62)).
neg(advisedby(systems,person375,person67)).
neg(advisedby(systems,person375,person80)).
neg(advisedby(systems,person375,person89)).
neg(advisedby(systems,person375,person92)).
neg(advisedby(systems,person375,person99)).
neg(advisedby(systems,person375,person101)).
neg(advisedby(systems,person375,person104)).
neg(advisedby(systems,person375,person107)).
neg(advisedby(systems,person375,person124)).
neg(advisedby(systems,person375,person179)).
neg(advisedby(systems,person375,person180)).
neg(advisedby(systems,person375,person213)).
neg(advisedby(systems,person375,person234)).
neg(advisedby(systems,person375,person235)).
neg(advisedby(systems,person375,person373)).
neg(advisedby(systems,person375,person375)).
neg(advisedby(systems,person375,person98)).
neg(advisedby(systems,person98,person100)).
neg(advisedby(systems,person98,person116)).
neg(advisedby(systems,person98,person126)).
neg(advisedby(systems,person98,person129)).
neg(advisedby(systems,person98,person130)).
neg(advisedby(systems,person98,person154)).
neg(advisedby(systems,person98,person155)).
neg(advisedby(systems,person98,person204)).
neg(advisedby(systems,person98,person212)).
neg(advisedby(systems,person98,person218)).
neg(advisedby(systems,person98,person253)).
neg(advisedby(systems,person98,person280)).
neg(advisedby(systems,person98,person357)).
neg(advisedby(systems,person98,person368)).
neg(advisedby(systems,person98,person374)).
neg(advisedby(systems,person98,person376)).
neg(advisedby(systems,person98,person391)).
neg(advisedby(systems,person98,person403)).
neg(advisedby(systems,person98,person411)).
neg(advisedby(systems,person98,person419)).
neg(advisedby(systems,person98,person426)).
neg(advisedby(systems,person98,person62)).
neg(advisedby(systems,person98,person67)).
neg(advisedby(systems,person98,person80)).
neg(advisedby(systems,person98,person89)).
neg(advisedby(systems,person98,person92)).
neg(advisedby(systems,person98,person99)).
neg(advisedby(systems,person98,person101)).
neg(advisedby(systems,person98,person104)).
neg(advisedby(systems,person98,person107)).
neg(advisedby(systems,person98,person124)).
neg(advisedby(systems,person98,person179)).
neg(advisedby(systems,person98,person180)).
neg(advisedby(systems,person98,person213)).
neg(advisedby(systems,person98,person234)).
neg(advisedby(systems,person98,person235)).
neg(advisedby(systems,person98,person373)).
neg(advisedby(systems,person98,person375)).
neg(advisedby(systems,person98,person98)).

taughtby(theory,course11, person57, autumn_0001).
taughtby(theory,course147, person201, autumn_0001).
taughtby(theory,course77, person165, autumn_0001).
taughtby(theory,course160, person331, autumn_0001).
taughtby(theory,course66, person298, autumn_0001).
taughtby(theory,course11, person298, winter_0001).
taughtby(theory,course147, person165, winter_0001).
taughtby(theory,course165, person364, winter_0001).
taughtby(theory,course161, person201, winter_0001).
taughtby(theory,course68, person331, winter_0001).
taughtby(theory,course29, person298, winter_0001).
taughtby(theory,course11, person331, spring_0001).
taughtby(theory,course147, person57, spring_0001).
taughtby(theory,course27, person165, spring_0001).
taughtby(theory,course40, person378, spring_0001).
taughtby(theory,course165, person231, autumn_0102).
taughtby(theory,course104, person364, autumn_0102).
taughtby(theory,course103, person201, autumn_0102).
taughtby(theory,course77, person324, autumn_0102).
taughtby(theory,course66, person165, autumn_0102).
taughtby(theory,course147, person324, winter_0102).
taughtby(theory,course161, person298, winter_0102).
taughtby(theory,course126, person165, winter_0102).
taughtby(theory,course68, person201, winter_0102).
taughtby(theory,course11, person324, spring_0102).
taughtby(theory,course147, person364, spring_0102).
taughtby(theory,course165, person141, spring_0102).
taughtby(theory,course27, person165, spring_0102).
taughtby(theory,course40, person298, spring_0102).
taughtby(theory,course104, person165, autumn_0203).
taughtby(theory,course68, person331, autumn_0203).
taughtby(theory,course77, person52, autumn_0203).
taughtby(theory,course147, person165, winter_0203).
taughtby(theory,course161, person331, winter_0203).
taughtby(theory,course119, person324, winter_0203).
taughtby(theory,course121, person52, winter_0203).
taughtby(theory,course11, person324, spring_0203).
taughtby(theory,course147, person52, spring_0203).
taughtby(theory,course27, person331, spring_0203).
taughtby(theory,course40, person165, spring_0203).
taughtby(theory,course117, person181, spring_0203).
taughtby(theory,course11, person298, autumn_0304).
taughtby(theory,course147, person165, autumn_0304).
taughtby(theory,course165, person75, autumn_0304).
taughtby(theory,course104, person181, autumn_0304).
taughtby(theory,course103, person201, autumn_0304).
taughtby(theory,course77, person52, autumn_0304).
taughtby(theory,course144, person331, autumn_0304).
taughtby(theory,course147, person331, winter_0304).
taughtby(theory,course165, person181, winter_0304).
taughtby(theory,course161, person201, winter_0304).
taughtby(theory,course126, person165, winter_0304).
taughtby(theory,course68, person324, winter_0304).
taughtby(theory,course40, person298, winter_0304).
taughtby(theory,course11, person52, spring_0304).
taughtby(theory,course27, person165, spring_0304).
taughtby(theory,course97, person324, spring_0304).
taughtby(theory,course91, person331, spring_0304).
courselevel(theory,course11, level_300).
courselevel(theory,course147, level_300).
courselevel(theory,course165, level_300).
courselevel(theory,course104, level_300).
courselevel(theory,course68, level_400).
courselevel(theory,course161, level_400).
courselevel(theory,course27, level_400).
courselevel(theory,course137, level_400).
courselevel(theory,course126, level_400).
courselevel(theory,course97, level_400).
courselevel(theory,course122, level_400).
courselevel(theory,course40, level_500).
courselevel(theory,course29, level_500).
courselevel(theory,course103, level_500).
courselevel(theory,course77, level_500).
courselevel(theory,course91, level_500).
courselevel(theory,course160, level_500).
courselevel(theory,course155, level_500).
courselevel(theory,course66, level_500).
courselevel(theory,course169, level_500).
courselevel(theory,course119, level_500).
courselevel(theory,course84, level_500).
courselevel(theory,course121, level_500).
courselevel(theory,course98, level_500).
courselevel(theory,course117, level_500).
courselevel(theory,course36, level_500).
courselevel(theory,course144, level_500).
courselevel(theory,course149, level_500).
hasposition(theory,person378, faculty).
hasposition(theory,person331, faculty).
hasposition(theory,person103, faculty_affiliate).
hasposition(theory,person52, faculty).
hasposition(theory,person298, faculty).
hasposition(theory,person165, faculty).
hasposition(theory,person29, faculty_adjunct).
hasposition(theory,person201, faculty).
hasposition(theory,person324, faculty).
projectmember(theory,project130, person324).
projectmember(theory,project119, person201).
projectmember(theory,project152, person201).
projectmember(theory,project94, person324).
advisedby(theory,person309, person378).
advisedby(theory,person141, person331).
advisedby(theory,person288, person165).
advisedby(theory,person159, person201).
advisedby(theory,person159, person57).
advisedby(theory,person226, person324).
advisedby(theory,person242, person29).
advisedby(theory,person242, person165).
advisedby(theory,person348, person324).
advisedby(theory,person6, person29).
advisedby(theory,person6, person165).
advisedby(theory,person75, person331).
advisedby(theory,person303, person165).
advisedby(theory,person303, person29).
advisedby(theory,person249, person331).
advisedby(theory,person68, person201).
inphase(theory,person309, post_quals).
inphase(theory,person141, post_generals).
inphase(theory,person383, pre_quals).
inphase(theory,person422, post_quals).
inphase(theory,person390, pre_quals).
inphase(theory,person288, post_generals).
inphase(theory,person159, post_quals).
inphase(theory,person172, pre_quals).
inphase(theory,person226, post_quals).
inphase(theory,person242, post_generals).
inphase(theory,person191, post_quals).
inphase(theory,person416, pre_quals).
inphase(theory,person348, post_quals).
inphase(theory,person278, pre_quals).
inphase(theory,person6, post_quals).
inphase(theory,person75, post_generals).
inphase(theory,person303, post_quals).
inphase(theory,person249, post_generals).
inphase(theory,person68, post_generals).
inphase(theory,person205, pre_quals).
inphase(theory,person182, post_quals).
tempadvisedby(theory,person383, person165).
tempadvisedby(theory,person390, person331).
tempadvisedby(theory,person172, person331).
tempadvisedby(theory,person191, person298).
tempadvisedby(theory,person416, person52).
tempadvisedby(theory,person278, person378).
tempadvisedby(theory,person205, person324).
tempadvisedby(theory,person182, person201).
yearsinprogram(theory,person309, year_3).
yearsinprogram(theory,person141, year_6).
yearsinprogram(theory,person383, year_2).
yearsinprogram(theory,person422, year_3).
yearsinprogram(theory,person390, year_2).
yearsinprogram(theory,person288, year_5).
yearsinprogram(theory,person159, year_2).
yearsinprogram(theory,person172, year_1).
yearsinprogram(theory,person226, year_4).
yearsinprogram(theory,person242, year_5).
yearsinprogram(theory,person191, year_4).
yearsinprogram(theory,person416, year_1).
yearsinprogram(theory,person348, year_3).
yearsinprogram(theory,person278, year_2).
yearsinprogram(theory,person6, year_2).
yearsinprogram(theory,person75, year_6).
yearsinprogram(theory,person303, year_4).
yearsinprogram(theory,person249, year_7).
yearsinprogram(theory,person68, year_5).
yearsinprogram(theory,person205, year_1).
yearsinprogram(theory,person182, year_3).
ta(theory,course147, person23, winter_0304).
ta(theory,course165, person141, winter_0304).
ta(theory,course104, person424, winter_0304).
ta(theory,course68, person416, winter_0304).
ta(theory,course161, person191, winter_0304).
ta(theory,course137, person383, winter_0304).
ta(theory,course40, person390, winter_0304).
ta(theory,course11, person205, autumn_0304).
ta(theory,course11, person172, autumn_0304).
ta(theory,course147, person310, autumn_0304).
ta(theory,course165, person416, autumn_0304).
ta(theory,course104, person401, autumn_0304).
ta(theory,course103, person182, autumn_0304).
ta(theory,course149, person390, autumn_0304).
ta(theory,course11, person58, spring_0203).
ta(theory,course11, person144, spring_0203).
ta(theory,course147, person390, spring_0203).
ta(theory,course147, person310, spring_0203).
ta(theory,course27, person249, spring_0203).
ta(theory,course40, person303, spring_0203).
ta(theory,course36, person278, spring_0203).
ta(theory,course147, person125, winter_0203).
ta(theory,course147, person6, winter_0203).
ta(theory,course165, person58, winter_0203).
ta(theory,course165, person422, winter_0203).
ta(theory,course104, person237, winter_0203).
ta(theory,course161, person390, winter_0203).
ta(theory,course161, person350, winter_0203).
ta(theory,course84, person141, winter_0203).
ta(theory,course11, person125, autumn_0203).
ta(theory,course11, person390, autumn_0203).
ta(theory,course11, person310, autumn_0203).
ta(theory,course165, person191, autumn_0203).
ta(theory,course104, person278, autumn_0203).
ta(theory,course104, person237, autumn_0203).
ta(theory,course68, person356, autumn_0203).
ta(theory,course155, person226, autumn_0203).
ta(theory,course11, person422, spring_0102).
ta(theory,course147, person309, spring_0102).
ta(theory,course147, person356, spring_0102).
ta(theory,course165, person294, spring_0102).
ta(theory,course104, person191, spring_0102).
ta(theory,course104, person182, spring_0102).
ta(theory,course27, person75, spring_0102).
ta(theory,course40, person303, spring_0102).
ta(theory,course147, person75, winter_0102).
ta(theory,course147, person356, winter_0102).
ta(theory,course165, person315, winter_0102).
ta(theory,course68, person191, winter_0102).
ta(theory,course68, person309, winter_0102).
ta(theory,course161, person249, winter_0102).
ta(theory,course137, person288, winter_0102).
ta(theory,course98, person303, winter_0102).
taughtby(theory,course137 , person165, winter_0304).
taughtby(theory,course122 , person378, autumn_0304).
taughtby(theory,course149 , person331, autumn_0304).
taughtby(theory,course144, person278, summer_0203).
taughtby(theory,course122 , person378, spring_0203).
taughtby(theory,course36, person181, spring_0203).
taughtby(theory,course84, person324, winter_0203).
taughtby(theory,course137 , person165, winter_0102).
taughtby(theory,course98 , person103, winter_0102).
professor(theory,person378).
professor(theory,person298).
professor(theory,person52).
professor(theory,person57).
professor(theory,person231).
professor(theory,person181).
professor(theory,person364).
student(theory,person191).
student(theory,person397).
student(theory,person138).
student(theory,person303).
student(theory,person77).
student(theory,person141).
professor(theory,person331).
professor(theory,person103).
professor(theory,person165).
professor(theory,person29).
professor(theory,person201).
professor(theory,person324).
student(theory,person309).
student(theory,person383).
student(theory,person422).
student(theory,person390).
student(theory,person288).
student(theory,person159).
student(theory,person172).
student(theory,person226).
student(theory,person242).
student(theory,person416).
student(theory,person348).
student(theory,person278).
student(theory,person6).
student(theory,person75).
student(theory,person249).
student(theory,person68).
student(theory,person205).
student(theory,person182).
student(theory,person23).
student(theory,person310).
student(theory,person424).
student(theory,person401).
student(theory,person237).
student(theory,person58).
student(theory,person144).
student(theory,person125).
student(theory,person350).
student(theory,person356).
student(theory,person294).
student(theory,person315).
sameperson(theory,person378, person378).
sameperson(theory,person298, person298).
sameperson(theory,person52, person52).
sameperson(theory,person57, person57).
sameperson(theory,person231, person231).
sameperson(theory,person181, person181).
sameperson(theory,person364, person364).
sameperson(theory,person191, person191).
sameperson(theory,person397, person397).
sameperson(theory,person138, person138).
sameperson(theory,person303, person303).
sameperson(theory,person77, person77).
sameperson(theory,person141, person141).
sameperson(theory,person331, person331).
sameperson(theory,person103, person103).
sameperson(theory,person165, person165).
sameperson(theory,person29, person29).
sameperson(theory,person201, person201).
sameperson(theory,person324, person324).
sameperson(theory,person309, person309).
sameperson(theory,person383, person383).
sameperson(theory,person422, person422).
sameperson(theory,person390, person390).
sameperson(theory,person288, person288).
sameperson(theory,person159, person159).
sameperson(theory,person172, person172).
sameperson(theory,person226, person226).
sameperson(theory,person242, person242).
sameperson(theory,person416, person416).
sameperson(theory,person348, person348).
sameperson(theory,person278, person278).
sameperson(theory,person6, person6).
sameperson(theory,person75, person75).
sameperson(theory,person249, person249).
sameperson(theory,person68, person68).
sameperson(theory,person205, person205).
sameperson(theory,person182, person182).
sameperson(theory,person23, person23).
sameperson(theory,person310, person310).
sameperson(theory,person424, person424).
sameperson(theory,person401, person401).
sameperson(theory,person237, person237).
sameperson(theory,person58, person58).
sameperson(theory,person144, person144).
sameperson(theory,person125, person125).
sameperson(theory,person350, person350).
sameperson(theory,person356, person356).
sameperson(theory,person294, person294).
sameperson(theory,person315, person315).
samecourse(theory,course144, course144).
samecourse(theory,course165, course165).
samecourse(theory,course11, course11).
samecourse(theory,course147, course147).
samecourse(theory,course104, course104).
samecourse(theory,course68, course68).
samecourse(theory,course161, course161).
samecourse(theory,course27, course27).
samecourse(theory,course126, course126).
samecourse(theory,course97, course97).
samecourse(theory,course40, course40).
samecourse(theory,course29, course29).
samecourse(theory,course103, course103).
samecourse(theory,course77, course77).
samecourse(theory,course91, course91).
samecourse(theory,course160, course160).
samecourse(theory,course155, course155).
samecourse(theory,course66, course66).
samecourse(theory,course119, course119).
samecourse(theory,course121, course121).
samecourse(theory,course117, course117).
samecourse(theory,course36, course36).
samecourse(theory,course169, course169).
samecourse(theory,course84, course84).
samecourse(theory,course137, course137).
samecourse(theory,course122, course122).
samecourse(theory,course98, course98).
samecourse(theory,course149, course149).
sameproject(theory,project12, project12).
sameproject(theory,project1, project1).
sameproject(theory,project119, project119).
sameproject(theory,project63, project63).
sameproject(theory,project5, project5).
sameproject(theory,project152, project152).
sameproject(theory,project18, project18).
sameproject(theory,project56, project56).
sameproject(theory,project130, project130).
sameproject(theory,project61, project61).
sameproject(theory,project126, project126).
sameproject(theory,project94, project94).
sameproject(theory,project57, project57).
sameproject(theory,project22, project22).
sameproject(theory,project21, project21).
sameproject(theory,project64, project64).
publication(theory,title164 , person378).
publication(theory,title202 , person378).
publication(theory,title152 , person378).
publication(theory,title154 , person378).
publication(theory,title334 , person378).
publication(theory,title193 , person378).
publication(theory,title326 , person378).
publication(theory,title328 , person378).
publication(theory,title327 , person378).
publication(theory,title308 , person378).
publication(theory,title136 , person378).
publication(theory,title243 , person378).
publication(theory,title127 , person378).
publication(theory,title317 , person298).
publication(theory,title18 , person298).
publication(theory,title126 , person298).
publication(theory,title309 , person298).
publication(theory,title128 , person298).
publication(theory,title77 , person298).
publication(theory,title216 , person298).
publication(theory,title235 , person298).
publication(theory,title311 , person298).
publication(theory,title298 , person298).
publication(theory,title326 , person397).
publication(theory,title62 , person138).
publication(theory,title210 , person138).
publication(theory,title287 , person138).
publication(theory,title317 , person77).
publication(theory,title18 , person77).
publication(theory,title126 , person77).
publication(theory,title309 , person77).
publication(theory,title128 , person77).
publication(theory,title77 , person77).
publication(theory,title216 , person77).
publication(theory,title287 , person77).
publication(theory,title235 , person77).
publication(theory,title311 , person77).
publication(theory,title298 , person77).
publication(theory,title164 , person331).
publication(theory,title202 , person331).
publication(theory,title95 , person331).
publication(theory,title152 , person331).
publication(theory,title154 , person331).
publication(theory,title158 , person331).
publication(theory,title73 , person331).
publication(theory,title19 , person331).
publication(theory,title159 , person331).
publication(theory,title334 , person331).
publication(theory,title276 , person331).
publication(theory,title328 , person331).
publication(theory,title327 , person331).
publication(theory,title308 , person331).
publication(theory,title136 , person331).
publication(theory,title243 , person331).
publication(theory,title138 , person331).
publication(theory,title127 , person331).
publication(theory,title20 , person331).
publication(theory,title21 , person331).
publication(theory,title27 , person331).
publication(theory,title105 , person331).
publication(theory,title330 , person165).
publication(theory,title200 , person165).
publication(theory,title129 , person165).
publication(theory,title155 , person165).
publication(theory,title104 , person165).
publication(theory,title324 , person165).
publication(theory,title215 , person165).
publication(theory,title205 , person165).
publication(theory,title291 , person165).
publication(theory,title321 , person165).
publication(theory,title175 , person165).
publication(theory,title307 , person165).
publication(theory,title193 , person165).
publication(theory,title184 , person165).
publication(theory,title138 , person165).
publication(theory,title330 , person29).
publication(theory,title200 , person29).
publication(theory,title129 , person29).
publication(theory,title155 , person29).
publication(theory,title104 , person29).
publication(theory,title324 , person29).
publication(theory,title215 , person29).
publication(theory,title205 , person29).
publication(theory,title279 , person29).
publication(theory,title291 , person29).
publication(theory,title66 , person29).
publication(theory,title321 , person29).
publication(theory,title175 , person29).
publication(theory,title307 , person29).
publication(theory,title184 , person29).
publication(theory,title62 , person324).
publication(theory,title158 , person324).
publication(theory,title19 , person324).
publication(theory,title210 , person324).
publication(theory,title21 , person324).
publication(theory,title27 , person324).
publication(theory,title105 , person324).
publication(theory,title330 , person242).
publication(theory,title104 , person242).
publication(theory,title215 , person242).
publication(theory,title205 , person242).
publication(theory,title279 , person242).
publication(theory,title291 , person242).
publication(theory,title66 , person242).
publication(theory,title321 , person242).
publication(theory,title175 , person242).
publication(theory,title287 , person242).
publication(theory,title159 , person75).
publication(theory,title20 , person75).
publication(theory,title95 , person249).
publication(theory,title73 , person249).
publication(theory,title276 , person249).
neg(advisedby(theory,person141,person141)).
neg(advisedby(theory,person141,person159)).
neg(advisedby(theory,person141,person226)).
neg(advisedby(theory,person141,person242)).
neg(advisedby(theory,person141,person249)).
neg(advisedby(theory,person141,person288)).
neg(advisedby(theory,person141,person303)).
neg(advisedby(theory,person141,person309)).
neg(advisedby(theory,person141,person348)).
neg(advisedby(theory,person141,person6)).
neg(advisedby(theory,person141,person68)).
neg(advisedby(theory,person141,person75)).
neg(advisedby(theory,person141,person165)).
neg(advisedby(theory,person141,person201)).
neg(advisedby(theory,person141,person29)).
neg(advisedby(theory,person141,person324)).
neg(advisedby(theory,person141,person378)).
neg(advisedby(theory,person141,person57)).
neg(advisedby(theory,person159,person141)).
neg(advisedby(theory,person159,person159)).
neg(advisedby(theory,person159,person226)).
neg(advisedby(theory,person159,person242)).
neg(advisedby(theory,person159,person249)).
neg(advisedby(theory,person159,person288)).
neg(advisedby(theory,person159,person303)).
neg(advisedby(theory,person159,person309)).
neg(advisedby(theory,person159,person348)).
neg(advisedby(theory,person159,person6)).
neg(advisedby(theory,person159,person68)).
neg(advisedby(theory,person159,person75)).
neg(advisedby(theory,person159,person165)).
neg(advisedby(theory,person159,person29)).
neg(advisedby(theory,person159,person324)).
neg(advisedby(theory,person159,person331)).
neg(advisedby(theory,person159,person378)).
neg(advisedby(theory,person226,person141)).
neg(advisedby(theory,person226,person159)).
neg(advisedby(theory,person226,person226)).
neg(advisedby(theory,person226,person242)).
neg(advisedby(theory,person226,person249)).
neg(advisedby(theory,person226,person288)).
neg(advisedby(theory,person226,person303)).
neg(advisedby(theory,person226,person309)).
neg(advisedby(theory,person226,person348)).
neg(advisedby(theory,person226,person6)).
neg(advisedby(theory,person226,person68)).
neg(advisedby(theory,person226,person75)).
neg(advisedby(theory,person226,person165)).
neg(advisedby(theory,person226,person201)).
neg(advisedby(theory,person226,person29)).
neg(advisedby(theory,person226,person331)).
neg(advisedby(theory,person226,person378)).
neg(advisedby(theory,person226,person57)).
neg(advisedby(theory,person242,person141)).
neg(advisedby(theory,person242,person159)).
neg(advisedby(theory,person242,person226)).
neg(advisedby(theory,person242,person242)).
neg(advisedby(theory,person242,person249)).
neg(advisedby(theory,person242,person288)).
neg(advisedby(theory,person242,person303)).
neg(advisedby(theory,person242,person309)).
neg(advisedby(theory,person242,person348)).
neg(advisedby(theory,person242,person6)).
neg(advisedby(theory,person242,person68)).
neg(advisedby(theory,person242,person75)).
neg(advisedby(theory,person242,person201)).
neg(advisedby(theory,person242,person324)).
neg(advisedby(theory,person242,person331)).
neg(advisedby(theory,person242,person378)).
neg(advisedby(theory,person242,person57)).
neg(advisedby(theory,person249,person141)).
neg(advisedby(theory,person249,person159)).
neg(advisedby(theory,person249,person226)).
neg(advisedby(theory,person249,person242)).
neg(advisedby(theory,person249,person249)).
neg(advisedby(theory,person249,person288)).
neg(advisedby(theory,person249,person303)).
neg(advisedby(theory,person249,person309)).
neg(advisedby(theory,person249,person348)).
neg(advisedby(theory,person249,person6)).
neg(advisedby(theory,person249,person68)).
neg(advisedby(theory,person249,person75)).
neg(advisedby(theory,person249,person165)).
neg(advisedby(theory,person249,person201)).
neg(advisedby(theory,person249,person29)).
neg(advisedby(theory,person249,person324)).
neg(advisedby(theory,person249,person378)).
neg(advisedby(theory,person249,person57)).
neg(advisedby(theory,person288,person141)).
neg(advisedby(theory,person288,person159)).
neg(advisedby(theory,person288,person226)).
neg(advisedby(theory,person288,person242)).
neg(advisedby(theory,person288,person249)).
neg(advisedby(theory,person288,person288)).
neg(advisedby(theory,person288,person303)).
neg(advisedby(theory,person288,person309)).
neg(advisedby(theory,person288,person348)).
neg(advisedby(theory,person288,person6)).
neg(advisedby(theory,person288,person68)).
neg(advisedby(theory,person288,person75)).
neg(advisedby(theory,person288,person201)).
neg(advisedby(theory,person288,person29)).
neg(advisedby(theory,person288,person324)).
neg(advisedby(theory,person288,person331)).
neg(advisedby(theory,person288,person378)).
neg(advisedby(theory,person288,person57)).
neg(advisedby(theory,person303,person141)).
neg(advisedby(theory,person303,person159)).
neg(advisedby(theory,person303,person226)).
neg(advisedby(theory,person303,person242)).
neg(advisedby(theory,person303,person249)).
neg(advisedby(theory,person303,person288)).
neg(advisedby(theory,person303,person303)).
neg(advisedby(theory,person303,person309)).
neg(advisedby(theory,person303,person348)).
neg(advisedby(theory,person303,person6)).
neg(advisedby(theory,person303,person68)).
neg(advisedby(theory,person303,person75)).
neg(advisedby(theory,person303,person201)).
neg(advisedby(theory,person303,person324)).
neg(advisedby(theory,person303,person331)).
neg(advisedby(theory,person303,person378)).
neg(advisedby(theory,person303,person57)).
neg(advisedby(theory,person309,person141)).
neg(advisedby(theory,person309,person159)).
neg(advisedby(theory,person309,person226)).
neg(advisedby(theory,person309,person242)).
neg(advisedby(theory,person309,person249)).
neg(advisedby(theory,person309,person288)).
neg(advisedby(theory,person309,person303)).
neg(advisedby(theory,person309,person309)).
neg(advisedby(theory,person309,person348)).
neg(advisedby(theory,person309,person6)).
neg(advisedby(theory,person309,person68)).
neg(advisedby(theory,person309,person75)).
neg(advisedby(theory,person309,person165)).
neg(advisedby(theory,person309,person201)).
neg(advisedby(theory,person309,person29)).
neg(advisedby(theory,person309,person324)).
neg(advisedby(theory,person309,person331)).
neg(advisedby(theory,person309,person57)).
neg(advisedby(theory,person348,person141)).
neg(advisedby(theory,person348,person159)).
neg(advisedby(theory,person348,person226)).
neg(advisedby(theory,person348,person242)).
neg(advisedby(theory,person348,person249)).
neg(advisedby(theory,person348,person288)).
neg(advisedby(theory,person348,person303)).
neg(advisedby(theory,person348,person309)).
neg(advisedby(theory,person348,person348)).
neg(advisedby(theory,person348,person6)).
neg(advisedby(theory,person348,person68)).
neg(advisedby(theory,person348,person75)).
neg(advisedby(theory,person348,person165)).
neg(advisedby(theory,person348,person201)).
neg(advisedby(theory,person348,person29)).
neg(advisedby(theory,person348,person331)).
neg(advisedby(theory,person348,person378)).
neg(advisedby(theory,person348,person57)).
neg(advisedby(theory,person6,person141)).
neg(advisedby(theory,person6,person159)).
neg(advisedby(theory,person6,person226)).
neg(advisedby(theory,person6,person242)).
neg(advisedby(theory,person6,person249)).
neg(advisedby(theory,person6,person288)).
neg(advisedby(theory,person6,person303)).
neg(advisedby(theory,person6,person309)).
neg(advisedby(theory,person6,person348)).
neg(advisedby(theory,person6,person6)).
neg(advisedby(theory,person6,person68)).
neg(advisedby(theory,person6,person75)).
neg(advisedby(theory,person6,person201)).
neg(advisedby(theory,person6,person324)).
neg(advisedby(theory,person6,person331)).
neg(advisedby(theory,person6,person378)).
neg(advisedby(theory,person6,person57)).
neg(advisedby(theory,person68,person141)).
neg(advisedby(theory,person68,person159)).
neg(advisedby(theory,person68,person226)).
neg(advisedby(theory,person68,person242)).
neg(advisedby(theory,person68,person249)).
neg(advisedby(theory,person68,person288)).
neg(advisedby(theory,person68,person303)).
neg(advisedby(theory,person68,person309)).
neg(advisedby(theory,person68,person348)).
neg(advisedby(theory,person68,person6)).
neg(advisedby(theory,person68,person68)).
neg(advisedby(theory,person68,person75)).
neg(advisedby(theory,person68,person165)).
neg(advisedby(theory,person68,person29)).
neg(advisedby(theory,person68,person324)).
neg(advisedby(theory,person68,person331)).
neg(advisedby(theory,person68,person378)).
neg(advisedby(theory,person68,person57)).
neg(advisedby(theory,person75,person141)).
neg(advisedby(theory,person75,person159)).
neg(advisedby(theory,person75,person226)).
neg(advisedby(theory,person75,person242)).
neg(advisedby(theory,person75,person249)).
neg(advisedby(theory,person75,person288)).
neg(advisedby(theory,person75,person303)).
neg(advisedby(theory,person75,person309)).
neg(advisedby(theory,person75,person348)).
neg(advisedby(theory,person75,person6)).
neg(advisedby(theory,person75,person68)).
neg(advisedby(theory,person75,person75)).
neg(advisedby(theory,person75,person165)).
neg(advisedby(theory,person75,person201)).
neg(advisedby(theory,person75,person29)).
neg(advisedby(theory,person75,person324)).
neg(advisedby(theory,person75,person378)).
neg(advisedby(theory,person75,person57)).
neg(advisedby(theory,person165,person141)).
neg(advisedby(theory,person165,person159)).
neg(advisedby(theory,person165,person226)).
neg(advisedby(theory,person165,person242)).
neg(advisedby(theory,person165,person249)).
neg(advisedby(theory,person165,person288)).
neg(advisedby(theory,person165,person303)).
neg(advisedby(theory,person165,person309)).
neg(advisedby(theory,person165,person348)).
neg(advisedby(theory,person165,person6)).
neg(advisedby(theory,person165,person68)).
neg(advisedby(theory,person165,person75)).
neg(advisedby(theory,person165,person165)).
neg(advisedby(theory,person165,person201)).
neg(advisedby(theory,person165,person29)).
neg(advisedby(theory,person165,person324)).
neg(advisedby(theory,person165,person331)).
neg(advisedby(theory,person165,person378)).
neg(advisedby(theory,person165,person57)).
neg(advisedby(theory,person201,person141)).
neg(advisedby(theory,person201,person159)).
neg(advisedby(theory,person201,person226)).
neg(advisedby(theory,person201,person242)).
neg(advisedby(theory,person201,person249)).
neg(advisedby(theory,person201,person288)).
neg(advisedby(theory,person201,person303)).
neg(advisedby(theory,person201,person309)).
neg(advisedby(theory,person201,person348)).
neg(advisedby(theory,person201,person6)).
neg(advisedby(theory,person201,person68)).
neg(advisedby(theory,person201,person75)).
neg(advisedby(theory,person201,person165)).
neg(advisedby(theory,person201,person201)).
neg(advisedby(theory,person201,person29)).
neg(advisedby(theory,person201,person324)).
neg(advisedby(theory,person201,person331)).
neg(advisedby(theory,person201,person378)).
neg(advisedby(theory,person201,person57)).
neg(advisedby(theory,person29,person141)).
neg(advisedby(theory,person29,person159)).
neg(advisedby(theory,person29,person226)).
neg(advisedby(theory,person29,person242)).
neg(advisedby(theory,person29,person249)).
neg(advisedby(theory,person29,person288)).
neg(advisedby(theory,person29,person303)).
neg(advisedby(theory,person29,person309)).
neg(advisedby(theory,person29,person348)).
neg(advisedby(theory,person29,person6)).
neg(advisedby(theory,person29,person68)).
neg(advisedby(theory,person29,person75)).
neg(advisedby(theory,person29,person165)).
neg(advisedby(theory,person29,person201)).
neg(advisedby(theory,person29,person29)).
neg(advisedby(theory,person29,person324)).
neg(advisedby(theory,person29,person331)).
neg(advisedby(theory,person29,person378)).
neg(advisedby(theory,person29,person57)).
neg(advisedby(theory,person324,person141)).
neg(advisedby(theory,person324,person159)).
neg(advisedby(theory,person324,person226)).
neg(advisedby(theory,person324,person242)).
neg(advisedby(theory,person324,person249)).
neg(advisedby(theory,person324,person288)).
neg(advisedby(theory,person324,person303)).
neg(advisedby(theory,person324,person309)).
neg(advisedby(theory,person324,person348)).
neg(advisedby(theory,person324,person6)).
neg(advisedby(theory,person324,person68)).
neg(advisedby(theory,person324,person75)).
neg(advisedby(theory,person324,person165)).
neg(advisedby(theory,person324,person201)).
neg(advisedby(theory,person324,person29)).
neg(advisedby(theory,person324,person324)).
neg(advisedby(theory,person324,person331)).
neg(advisedby(theory,person324,person378)).
neg(advisedby(theory,person324,person57)).
neg(advisedby(theory,person331,person141)).
neg(advisedby(theory,person331,person159)).
neg(advisedby(theory,person331,person226)).
neg(advisedby(theory,person331,person242)).
neg(advisedby(theory,person331,person249)).
neg(advisedby(theory,person331,person288)).
neg(advisedby(theory,person331,person303)).
neg(advisedby(theory,person331,person309)).
neg(advisedby(theory,person331,person348)).
neg(advisedby(theory,person331,person6)).
neg(advisedby(theory,person331,person68)).
neg(advisedby(theory,person331,person75)).
neg(advisedby(theory,person331,person165)).
neg(advisedby(theory,person331,person201)).
neg(advisedby(theory,person331,person29)).
neg(advisedby(theory,person331,person324)).
neg(advisedby(theory,person331,person331)).
neg(advisedby(theory,person331,person378)).
neg(advisedby(theory,person331,person57)).
neg(advisedby(theory,person378,person141)).
neg(advisedby(theory,person378,person159)).
neg(advisedby(theory,person378,person226)).
neg(advisedby(theory,person378,person242)).
neg(advisedby(theory,person378,person249)).
neg(advisedby(theory,person378,person288)).
neg(advisedby(theory,person378,person303)).
neg(advisedby(theory,person378,person309)).
neg(advisedby(theory,person378,person348)).
neg(advisedby(theory,person378,person6)).
neg(advisedby(theory,person378,person68)).
neg(advisedby(theory,person378,person75)).
neg(advisedby(theory,person378,person165)).
neg(advisedby(theory,person378,person201)).
neg(advisedby(theory,person378,person29)).
neg(advisedby(theory,person378,person324)).
neg(advisedby(theory,person378,person331)).
neg(advisedby(theory,person378,person378)).
neg(advisedby(theory,person378,person57)).
neg(advisedby(theory,person57,person141)).
neg(advisedby(theory,person57,person159)).
neg(advisedby(theory,person57,person226)).
neg(advisedby(theory,person57,person242)).
neg(advisedby(theory,person57,person249)).
neg(advisedby(theory,person57,person288)).
neg(advisedby(theory,person57,person303)).
neg(advisedby(theory,person57,person309)).
neg(advisedby(theory,person57,person348)).
neg(advisedby(theory,person57,person6)).
neg(advisedby(theory,person57,person68)).
neg(advisedby(theory,person57,person75)).
neg(advisedby(theory,person57,person165)).
neg(advisedby(theory,person57,person201)).
neg(advisedby(theory,person57,person29)).
neg(advisedby(theory,person57,person324)).
neg(advisedby(theory,person57,person331)).
neg(advisedby(theory,person57,person378)).
neg(advisedby(theory,person57,person57)).

