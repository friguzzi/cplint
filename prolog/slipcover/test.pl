:-multifile setting/2.
:-source.
:-use_module(library('cplint/slipcover')).

setting(neg_ex,cw).
/* allowed values: given, cw */

main(TrainP,TestSets):-
  system('rm -f areas.csv'),
  system('rm -f curve_roc.m'),
  system('rm -f curve_pr.m'),
  open('cll1.pl',write,S),
  open('areas.csv',append,SA),
  format(SA,"Fold;\tCLL;\t AUCROC;\t AUCPR~n",[]),
  close(SA),
  test(TrainP,TestSets,S,[],LG,0,Pos,0,Neg,0,CLL),
  keysort(LG,LG1),
  format(S,"cll(all,post,~d,~d,[",[Pos,Neg]),
  writes(LG1,S),
  reverse(LG1,LGR1),
  compute_areas(LGR1,Pos,Neg,AUCROC,AUCPR),
  open('areas.csv',append,SA1),
  format(SA1,"~a;\t ~f;\t ~f;\t ~f~n",[all,CLL,AUCROC,AUCPR]),
  close(SA1),
  close(S).
  
test([],[],_S,LG,LG,Pos,Pos,Neg,Neg,CLL,CLL).

test([HP|TP],[HT|TT],S,LG0,LG,Pos0,Pos,Neg0,Neg,CLL0,CLL):-
  test_fold(HP,HT,S,LG1,Pos1,Neg1,CLL1),
  append(LG0,LG1,LG2),
  Pos2 is Pos0+Pos1,
  Neg2 is Neg0+Neg1,
  CLL2 is CLL0+CLL1,
  test(TP,TT,S,LG2,LG,Pos2,Pos,Neg2,Neg,CLL2,CLL).

test_fold(P,F,S,LGOrd,Pos,Neg,CLL1):-
  atom_concat([P,'.rules'],PR),
  atom_concat([P,'.bg'],PBG),
  atom_concat([P,'.l'],FL),
  atom_concat([F,'.kb'],TKB),
  reconsult(FL),
  (file_exists(PBG)->
    set(compiling,on),
    load(PBG,_ThBG,RBG),
    set(compiling,off),
    generate_clauses(RBG,_RBG1,0,[],ThBG),
    assert_all(ThBG)
  ;
    true
  ),
  format("~a~n",[TKB]),
  load_models(TKB,DB),
  set(compiling,on),
  load(PR,Th1,R1),  
  set(compiling,off),
  assert_all(Th1),
  assert_all(R1),
  find_ex(DB,LG,Pos,Neg),
  compute_CLL_atoms(LG,0,0,CLL1,LG1),
  (file_exists(PBG)->
    retract_all(ThBG)
  ;
    true
  ),
  retract_all(Th1),
  retract_all(R1),
  keysort(LG1,LGOrd),
  reverse(LGOrd,LGROrd),
  compute_areas(LGROrd,Pos,Neg,AUCROC,AUCPR), 
  format(S,"cll(~a,post,~d,~d,[",[F,Pos,Neg]),
  writes(LGOrd,S),
  open('areas.csv',append,SA),
  format(SA,"~a;\t ~f;\t ~f;\t ~f~n",[F,CLL1,AUCROC,AUCPR]),
  close(SA).

compute_areas(LG,Pos,Neg,AUCROC,AUCPR):-
  compute_pointsroc(LG,+inf,0,0,Pos,Neg,[],ROC),
  hull(ROC,0,0,0,AUCROC),
  open('curve_roc.m',append,SC),
  write_p(ROC,SC),
  close(SC),
  compute_aucpr(LG,Pos,Neg,AUCPR,PR),
  open('curve_pr.m',append,SPR),
  write_ppr(PR,SPR),
  close(SPR).

compute_pointsroc([],_P0,_TP,_FP,_FN,_TN,P0,P1):-!,
  append(P0,[1.0-1.0],P1).

compute_pointsroc([P- (\+ _)|T],P0,TP,FP,FN,TN,Po0,Po1):-!,
  (P<P0->
    FPR is FP/(FP+TN),
    TPR is TP/(TP+FN),
    append(Po0,[(FPR-TPR)],Po2),
    P1=P
  ;		
    Po2=Po0,
    P1=P0
  ),
  FP1 is FP+1,
  TN1 is TN-1,
  compute_pointsroc(T,P1,TP,FP1,FN,TN1,Po2,Po1).

compute_pointsroc([P- _|T],P0,TP,FP,FN,TN,Po0,Po1):-!,
  (P<P0->
    FPR is FP/(FP+TN),
    TPR is TP/(TP+FN),
    append(Po0,[FPR-TPR],Po2),
    P1=P
  ;
    Po2=Po0,
    P1=P0
  ),
  TP1 is TP+1,
  FN1 is FN-1,
  compute_pointsroc(T,P1,TP1,FP,FN1,TN,Po2,Po1).


hull([],FPR,TPR,AUC0,AUC1):-
  AUC1 is AUC0+(1-FPR)*(1+TPR)/2.


hull([FPR1-TPR1|T],FPR,TPR,AUC0,AUC1):-
  AUC2 is AUC0+(FPR1-FPR)*(TPR1+TPR)/2,
  hull(T,FPR1,TPR1,AUC2,AUC1).

compute_aucpr(L,Pos,Neg,A,PR):-
  L=[P_0-E|TL],
  (E= (\+ _ )->
    FP=1,
    TP=0,
    FN=Pos,
    TN is Neg -1
  ;
    FP=0,
    TP=1,
    FN is Pos -1,
    TN=Neg
  ),
  compute_curve_points(TL,P_0,TP,FP,FN,TN,Points),
  Points=[R0-P0|_TPoints],
  (R0=:=0,P0=:=0->
    Flag=true
  ;
    Flag=false
  ),
  area(Points,Flag,Pos,0,0,0,A,[],PR).

compute_curve_points([],_P0,TP,FP,_FN,_TN,[1.0-Prec]):-!,
  Prec is TP/(TP+FP).

compute_curve_points([P- (\+ _)|T],P0,TP,FP,FN,TN,Pr):-!,
  (P<P0->
    Prec is TP/(TP+FP),
    Rec is TP/(TP+FN),
    Pr=[Rec-Prec|Pr1],
    P1=P
  ;
    Pr=Pr1,
    P1=P0
  ),
  FP1 is FP+1,
  TN1 is TN-1,
  compute_curve_points(T,P1,TP,FP1,FN,TN1,Pr1).

compute_curve_points([P- _|T],P0,TP,FP,FN,TN,Pr):-!,
  (P<P0->
    Prec is TP/(TP+FP),
    Rec is TP/(TP+FN),
    Pr=[Rec-Prec|Pr1],
    P1=P
  ;
    Pr=Pr1,
    P1=P0
  ),
  TP1 is TP+1,
  FN1 is FN-1,
  compute_curve_points(T,P1,TP1,FP,FN1,TN,Pr1).

area([],_Flag,_Pos,_TPA,_FPA,A,A,PR,PR).

area([R0-P0|T],Flag,Pos,TPA,FPA,A0,A,PR0,PR):-
 TPB is R0*Pos,
  (TPB=:=0->
    A1=A0,
    FPB=0,
    PR2=PR0,
    PR=[R0-P0|PR3]
  ;
    R_1 is TPA/Pos,
    (TPA=:=0->
      (Flag=false->
        P_1=P0,
	PR=[0.0-P0|PR3]
      ;
        P_1=0.0,
	PR=[0.0-0.0|PR3]
      )
    ;
      P_1 is TPA/(TPA+FPA),
      PR=PR3
    ),
    FPB is TPB*(1-P0)/P0,
    N is TPB-TPA+0.5,
    (N<1.0->
      append(PR0,[R0-P0],PR2),
      A1=A0
    ;
      interpolate(1,N,Pos,R_1,P_1,TPA,FPA,TPB,FPB,A0,A1,[],PR1),
      append(PR0,PR1,PR2)
    )
  ),
  area(T,Flag,Pos,TPB,FPB,A1,A,PR2,PR3).

interpolate(I,N,_Pos,_R0,_P0,_TPA,_FPA,_TPB,_FPB,A,A,PR,PR):-I>N,!.

interpolate(I,N,Pos,R0,P0,TPA,FPA,TPB,FPB,A0,A,PR0,[R-P|PR]):-
  R is (TPA+I)/Pos,
  P is (TPA+I)/(TPA+I+FPA+(FPB-FPA)/(TPB-TPA)*I),
  A1 is A0+(R-R0)*(P+P0)/2,
  I1 is I+1,
  interpolate(I1,N,Pos,R,P,TPA,FPA,TPB,FPB,A1,A,PR0,PR).


find_ex(DB,LG,Pos,Neg):-
  findall(P/A,output(P/A),LP),
  setting(neg_ex,given),!,
  find_ex_pred(LP,DB,[],LG,0,Pos,0,Neg).

find_ex(DB,LG,Pos,Neg):-
  findall(P/A,output(P/A),LP),
  setting(neg_ex,cw),
  find_ex_pred_cw(LP,DB,[],LG,0,Pos,0,Neg).


find_ex_pred([],_DB,LG,LG,Pos,Pos,Neg,Neg).

find_ex_pred([P/A|T],DB,LG0,LG,Pos0,Pos,Neg0,Neg):-
  functor(At,P,A),
  find_ex_db(DB,At,LG0,LG1,Pos0,Pos1,Neg0,Neg1),
  find_ex_pred(T,DB,LG1,LG,Pos1,Pos,Neg1,Neg).

find_ex_db([],_At,LG,LG,Pos,Pos,Neg,Neg).

find_ex_db([H|T],At,LG0,LG,Pos0,Pos,Neg0,Neg):-
  At=..[P|L],
  At1=..[P,H|L],
  findall(At1,At1,LP),
  findall(\+ At1,neg(At1),LN),
  length(LP,NP),
  length(LN,NN),
  append([LG0,LP,LN],LG1),
  Pos1 is Pos0+NP,
  Neg1 is Neg0+NN,
  find_ex_db(T,At,LG1,LG,Pos1,Pos,Neg1,Neg).


find_ex_pred_cw([],_DB,LG,LG,Pos,Pos,Neg,Neg).

find_ex_pred_cw([P/A|T],DB,LG0,LG,Pos0,Pos,Neg0,Neg):-
  functor(At,P,A),
  get_types(At,Types),
  remove_duplicates(Types,Types1),
  find_ex_db_cw(DB,At,Types1,LG0,LG1,Pos0,Pos1,Neg0,Neg1),
  find_ex_pred_cw(T,DB,LG1,LG,Pos1,Pos,Neg1,Neg).

get_types(At,Types):-
  modeh(_,At),
  At=..[_|Args],
  get_args(Args,Types).

get_args([],[]).

get_args([+H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([-H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([#H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([-#H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([H|T],[H|T1]):-
  get_args(T,T1).




get_constants([],_M,[]).

get_constants([Type|T],M,[(Type,Co)|C]):-
  find_pred_using_type(Type,LP),
  find_constants(LP,M,[],Co),
  get_constants(T,M,C).

find_pred_using_type(T,L):-
  setof((P,Ar,A),pred_type(T,P,Ar,A),L).

pred_type(T,P,Ar,A):-
  modeh(_,S),
  S=..[P|Args],
  length(Args,Ar),
  scan_args(Args,T,1,A).

pred_type(T,P,Ar,A):-
  modeb(_,S),
  S=..[P|Args],
  length(Args,Ar),
  scan_args(Args,T,1,A).

scan_args([+T|_],T,A,A):-!.

scan_args([-T|_],T,A,A):-!.

scan_args([#T|_],T,A,A):-!.

scan_args([-#T|_],T,A,A):-!.

scan_args([_|Tail],T,A0,A):-
  A1 is A0+1,
  scan_args(Tail,T,A1,A).

find_constants([],_M,C,C).

find_constants([(P,Ar,A)|T],M,C0,C):-
  gen_goal(1,Ar,A,Args,ArgsNoV,V),
  G=..[P,M|Args],
  setof(V,ArgsNoV^G,LC),
  append(C0,LC,C1),
  remove_duplicates(C1,C2),
  find_constants(T,M,C2,C).


gen_goal(Arg,Ar,_A,[],[],_):-
  Arg =:= Ar+1,!.

gen_goal(A,Ar,A,[V|Args],ArgsNoV,V):-!,
  Arg1 is A+1,
  gen_goal(Arg1,Ar,A,Args,ArgsNoV,V).
  
gen_goal(Arg,Ar,A,[ArgV|Args],[ArgV|ArgsNoV],V):-
  Arg1 is Arg+1,
  gen_goal(Arg1,Ar,A,Args,ArgsNoV,V).
  


find_ex_db_cw([],_At,_Ty,LG,LG,Pos,Pos,Neg,Neg).

find_ex_db_cw([H|T],At,Types,LG0,LG,Pos0,Pos,Neg0,Neg):-
  get_constants(Types,H,C),
  At=..[P|L],
  get_types(At,TypesA),
  length(L,N),
  length(LN,N),
  At1=..[P,H|LN],
  findall(At1,At1,LP),
  setof(\+ At1,neg_ex(LN,TypesA,At1,C),LNeg),
  length(LP,NP),
  length(LNeg,NN),
  append([LG0,LP,LNeg],LG1),
  Pos1 is Pos0+NP,
  Neg1 is Neg0+NN,
  find_ex_db_cw(T,At,Types,LG1,LG,Pos1,Pos,Neg1,Neg).

neg_ex([],[],At1,_C):-
  \+ At1.

neg_ex([H|T],[HT|TT],At1,C):-
  member((HT,Co),C),
  member(H,Co),
  neg_ex(T,TT,At1,C).

compute_CLL_atoms([],_N,CLL,CLL,[]):-!.

compute_CLL_atoms([\+ H|T],N,CLL0,CLL1,[PG- (\+ H)|T1]):-!,
  rule_n(NR),
  init_test(NR),
%  write(\+ H),
  get_node(H,BDD),!,
  ret_prob(BDD,PG),
%  write(PG),nl,
  end_test,!,
  PG1 is 1-PG,
  (PG1=:=0.0->
    CLL2 is CLL0-10
  ;
    CLL2 is CLL0+ log(PG1)
  ),		
  N1 is N+1,
  compute_CLL_atoms(T,N1,CLL2,CLL1,T1).	

compute_CLL_atoms([H|T],N,CLL0,CLL1,[PG-H|T1]):-
  rule_n(NR),
  init_test(NR),
%  write(H),
  get_node(H,BDD),!,
  ret_prob(BDD,PG),
%  write(PG),nl,
  end_test,!,
  (PG=:=0.0->
    CLL2 is CLL0-10
  ;	
    CLL2 is CLL0+ log(PG)
  ),
  N1 is N+1,
  compute_CLL_atoms(T,N1,CLL2,CLL1,T1).		


writes([H-H1],S):-
  format(S,"~f - (~p)]).~n~n",[H,H1]).

writes([H-H1|T],S):-
  format(S,"~f - (~p),~n",[H,H1]),
  writes(T,S).

write_p(P,S):-
  get_xy(P,PX,PY),
  format(S,"x=[",[]),
  writesf(PX,S),
  format(S,"y=[",[]),
  writesf(PY,S),
  format(S,"
figure('Name','roc','NumberTitle','off')
set(gca,'XLim',[0.0 1.0])
set(gca,'YLim',[0.0 1.0])
x=[x 1.0]
y=[y 0.0]
k=convhull(x,y)
plot(x(k),y(k),'r-',x,y,'--b+')
%A = polyarea(x,y)~n~n
%save area_roc.csv  A -ascii -append
",
  []).

get_xy([],[],[]).

get_xy([X-Y|T],[X|TX],[Y|TY]):-
  get_xy(T,TX,TY).


writesf([H],S):-
  format(S,"~f]~n",[H]).

writesf([H|T],S):-
  format(S,"~f ",[H]),
  writesf(T,S).

write_ppr(P,S):-
  get_xy(P,PX,PY),
  format(S,"rec=[",[A]),
  writesf(PX,S),
  format(S,"prec=[",[A]),
  writesf(PY,S),
  format(S,"
figure('Name','pr','NumberTitle','off')
set(gca,'XLim',[0.0 1.0])
set(gca,'YLim',[0.0 1.0])
rec=[0.0  rec 1.0];
prec=[0.0 prec 0.0];
plot(rec,prec,'--*k')
%A=polyarea(rec,prec)
%save area_pr.csv  A -ascii -append
~n~n",
  []).
	
