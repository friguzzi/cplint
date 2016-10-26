:-use_module(library(slipcover)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(lpad).
:- endif.

:-sc.

:- set_sc(megaex_bottom,2).
:- set_sc(max_iter,3).
:- set_sc(max_iter_structure,10).
:- set_sc(maxdepth_var,4).
:- set_sc(verbosity,3).
:- set_sc(seed,rand(192,3412,1230)).


:- begin_bg.
car(car_11).  car(car_12).  car(car_13).  car(car_14).
car(car_21).  car(car_22).  car(car_23).
car(car_31).  car(car_32).  car(car_33).
car(car_41).  car(car_42).  car(car_43).  car(car_44).
car(car_51).  car(car_52).  car(car_53).
car(car_61).  car(car_62).
car(car_71).  car(car_72).  car(car_73).
car(car_81).  car(car_82).
car(car_91).  car(car_92).  car(car_93).  car(car_94).
car(car_101).  car(car_102).

shape(elipse).  shape(hexagon).  shape(rectangle).  shape(u_shaped).
shape(triangle). shape(circle). shape(nil).

train(east1).  train(east2).  train(east3).  train(east4).  train(east5).
train(west6).  train(west7).  train(west8).  train(west9).  train(west10).

%eastbound train 1
short(car_12).		% 0
closed(car_12).		% 1
long(car_11).		% 2
long(car_13).
short(car_14).
open_car(car_11).		% 3
open_car(car_13).
open_car(car_14).
shape(car_11,rectangle). % 4,5
shape(car_12,rectangle).
shape(car_13,rectangle).
shape(car_14,rectangle).
load(car_11,rectangle,3). % 6,7,8
load(car_12,triangle,1).
load(car_13,hexagon,1).
load(car_14,circle,1).
wheels(car_11,2).	  % 9,10
wheels(car_12,2).
wheels(car_13,3).
wheels(car_14,2).

%eastbound train 2
short(car_21).
short(car_22).
short(car_23).
shape(car_21,u_shaped).
shape(car_22,u_shaped).
shape(car_23,rectangle).
open_car(car_21).
open_car(car_22).
closed(car_23).
load(car_21,triangle,1).
load(car_22,rectangle,1).
load(car_23,circle,2).
wheels(car_21,2).
wheels(car_22,2).
wheels(car_23,2).

%eastbound train 3
short(car_31).
short(car_32).
long(car_33).
shape(car_31,rectangle).
shape(car_32,hexagon).
shape(car_33,rectangle).
open_car(car_31).
closed(car_32).
closed(car_33).
load(car_31,circle,1).
load(car_32,triangle,1).
load(car_33,triangle,1).
wheels(car_31,2).
wheels(car_32,2).
wheels(car_33,3).

%eastbound train 4
short(car_41).
short(car_42).
short(car_43).
short(car_44).
shape(car_41,u_shaped).
shape(car_42,rectangle).
shape(car_43,elipse).
shape(car_44,rectangle).
double(car_42).
open_car(car_41).
open_car(car_42).
closed(car_43).
open_car(car_44).
load(car_41,triangle,1).
load(car_42,triangle,1).
load(car_43,rectangle,1).
load(car_44,rectangle,1).
wheels(car_41,2).
wheels(car_42,2).
wheels(car_43,2).
wheels(car_44,2).

%eastbound train 5
short(car_51).
short(car_52).
short(car_53).
shape(car_51,rectangle).
shape(car_52,rectangle).
shape(car_53,rectangle).
double(car_51).
open_car(car_51).
closed(car_52).
closed(car_53).
load(car_51,triangle,1).
load(car_52,rectangle,1).
load(car_53,circle,1).
wheels(car_51,2).
wheels(car_52,3).
wheels(car_53,2).

%westbound train 6
long(car_61).
short(car_62).
shape(car_61,rectangle).
shape(car_62,rectangle).
closed(car_61).
open_car(car_62).
load(car_61,circle,3).
load(car_62,triangle,1).
wheels(car_61,2).
wheels(car_62,2).

%westbound train 7
short(car_71).
short(car_72).
long(car_73).
shape(car_71,rectangle).
shape(car_72,u_shaped).
shape(car_73,rectangle).
double(car_71).
open_car(car_71).
open_car(car_72).
jagged(car_73).
load(car_71,circle,1).
load(car_72,triangle,1).
load(car_73,nil,0).
wheels(car_71,2).
wheels(car_72,2).
wheels(car_73,2).

%westbound train 8
long(car_81).
short(car_82).
shape(car_81,rectangle).
shape(car_82,u_shaped).
closed(car_81).
open_car(car_82).
load(car_81,rectangle,1).
load(car_82,circle,1).
wheels(car_81,3).
wheels(car_82,2).

%westbound train 9
short(car_91).
long(car_92).
short(car_93).
short(car_94).
shape(car_91,u_shaped).
shape(car_92,rectangle).
shape(car_93,rectangle).
shape(car_94,u_shaped).
open_car(car_91).
jagged(car_92).
open_car(car_93).
open_car(car_94).
load(car_91,circle,1).
load(car_92,rectangle,1).
load(car_93,rectangle,1).
load(car_93,circle,1).
wheels(car_91,2).
wheels(car_92,2).
wheels(car_93,2).
wheels(car_94,2).

%westbound train 10
short(car_101).
long(car_102).
shape(car_101,u_shaped).
shape(car_102,rectangle).
open_car(car_101).
open_car(car_102).
load(car_101,rectangle,1).
load(car_102,rectangle,2).
wheels(car_101,2).
wheels(car_102,2).
:- end_bg.

in([]).

fold(train,[east1, east2, east3, east4, east5, west6, west7, west8, west9, west10]).

output(eastbound/0).    %ariety is 1 parameter less

input_cw(short/1).
input_cw(long/1).
input_cw(closed/1).
input_cw(open_car/1).
input_cw(double/1).
input_cw(jagged/1).
input_cw(shape/2).
input_cw(load/3).
input_cw(wheels/2).
input_cw(has_car/1).    %ariety is 1 parameter less

modeh(1,eastbound).
modeb(*,has_car(-car)).
modeb(1,short(+car)).
modeb(1,closed(+car)).
modeb(1,long(+car)).
modeb(1,open_car(+car)).
modeb(1,double(+car)).
modeb(1,jagged(+car)).
modeb(1,shape(+car,#shape)).
modeb(1,load(+car,#shape,#int)).
modeb(1,wheels(+car,#int)).

determination(eastbound/0,short/1).
determination(eastbound/0,closed/1).
determination(eastbound/0,long/1).
determination(eastbound/0,open_car/1).
determination(eastbound/0,double/1).
determination(eastbound/0,jagged/1).
determination(eastbound/0,shape/2).
determination(eastbound/0,wheels/2).
determination(eastbound/0,has_car/1).
determination(eastbound/0,load/3).

%%%%%%%%%%
%database%
%%%%%%%%%%

% eastbound train 1
has_car(east1,car_11). % 11,12
has_car(east1,car_12).
has_car(east1,car_13).
has_car(east1,car_14).

% eastbound train 2
has_car(east2,car_21).
has_car(east2,car_22).
has_car(east2,car_23).

% eastbound train 3
has_car(east3,car_31).
has_car(east3,car_32).
has_car(east3,car_33).

% eastbound train 4
has_car(east4,car_41).
has_car(east4,car_42).
has_car(east4,car_43).
has_car(east4,car_44).

% eastbound train 5
has_car(east5,car_51).
has_car(east5,car_52).
has_car(east5,car_53).

% westbound train 6
has_car(west6,car_61).
has_car(west6,car_62).

% westbound train 7
has_car(west7,car_71).
has_car(west7,car_72).
has_car(west7,car_73).

% westbound train 8
has_car(west8,car_81).
has_car(west8,car_82).

% westbound train 9
has_car(west9,car_91).
has_car(west9,car_92).
has_car(west9,car_93).
has_car(west9,car_94).

% westbound train 10
has_car(west10,car_101).
has_car(west10,car_102).

eastbound(east1).
eastbound(east2).
eastbound(east3).
eastbound(east4).
eastbound(east5).
neg(eastbound(west6)).
neg(eastbound(west7)).
neg(eastbound(west8)).
neg(eastbound(west9)).
neg(eastbound(west10)).

