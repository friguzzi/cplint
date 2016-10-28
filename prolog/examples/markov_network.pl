:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(graphviz).
:- endif.

:- pita.

:- begin_lpad.

intelligent:0.5.

good_marks:0.5.

difficult_course:0.5.

good_teacher:0.5.

% factor1(intelligent,good_marks)=3/10 if not intelligent or good_marks, 
% 1/10 otherwise
factor1: 3/10:- \+intelligent, \+good_marks.
factor1: 3/10:- \+intelligent, good_marks.
factor1: 1/10:- intelligent, \+good_marks.
factor1: 3/10:- intelligent, good_marks.

% factor2(difficult_course,good_marks)=3/10 if 
% not difficult_course or not good_marks, 
% 1/10 otherwise
factor2: 3/10:- \+difficult_course, \+good_marks.
factor2: 3/10:- \+difficult_course, good_marks.
factor2: 3/10:- difficult_course, \+good_marks.
factor2: 1/10:- difficult_course, good_marks.

% factor3(good_teacher,good_marks)=3/10 if not good_teacher or good_marks, 
% 1/10 otherwise
factor3: 3/10:- \+good_teacher, \+good_marks.
factor3: 3/10:- \+good_teacher, good_marks.
factor3: 1/10:- good_teacher, \+good_marks.
factor3: 3/10:- good_teacher, good_marks.

% factor4(good_teacher,difficult_course)=3/10 
% if not good_teacher or not good_marks, 
% 1/10 otherwise
factor4: 3/10:- \+difficult_course, \+good_teacher.
factor4: 3/10:- \+difficult_course, good_teacher.
factor4: 3/10:- difficult_course, \+good_teacher.
factor4: 1/10:- difficult_course, good_teacher.

evidence:- factor1,factor2,factor3,factor4.

intelligent_ev:-intelligent,evidence.

good_teacher_int_ev:-intelligent_ev,good_teacher.
:- end_lpad.
network_structure(graph([
  intelligent-good_marks,good_marks-difficult_course,
  difficult_course-good_teacher,
  good_teacher-good_marks])).

/** <examples>

?- prob(good_marks,evidence,P).
?- prob(good_marks,intelligent_ev,P).
?- prob(good_marks,good_teacher_int_ev,P).
?- network_structure(G).

*/

