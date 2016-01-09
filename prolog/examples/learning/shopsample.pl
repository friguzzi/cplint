/* Shop dataset from
Fabrizio Riguzzi and Nicola Di Mauro. Applying the information bottleneck
to statistical relational learning. Machine Learning, 86(1):89-114, 2012.
Meert, W., Struyf, J., and Blockeel, H. 2008.
Learning ground CP-Logic theories by leveraging Bayesian network learning
techniques. Fundamenta Informaticae 89, 131-160

Examples generated randomly from the input program (shop4). 
The task is to recover
the values of the parameters of the input program. When learning, the initial
parameters are randomly set. A test set is also provided generated randomly from
the target program.

*/
/** <examples>
?- induce_par([train],P).
?- induce_par([train],[test],P,LL,AUCROC,ROC,AUCPR,PR).
*/

:- use_module(library(slipcover)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(lpad).
:- endif.

:-sc.

:- set_sc(verbosity,1).
:- set_sc(depth_bound,false).
:- set_sc(neg_ex,given).

bg([]).

fold(train,L):-
  findall(A,(between(1,1000,V),atom_concat(train,V,A)),L).

fold(test,L):-
  findall(V,between(1,10000,V),L).

output(bought/1).

output(shops/1).


in([
(shops(john) : 0.2),
(shops(mary) : 0.9),
(bought(spaghetti ) : 0.5; bought(steak) : 0.5 :- shops(john)),
(bought(spaghetti ) : 0.3; bought(fish) : 0.7:-  shops(mary))]).

neg(shops(A,john)):-
  \+ shops(A,john).

neg(shops(A,mary)):-
  \+ shops(A,mary).

neg(bought(A,spaghetti)):-
  \+ shops(A,spaghetti).

neg(bought(A,fish)):-
  \+ shops(A,fish).

neg(bought(A,steak)):-
  \+ shops(A,steak).




begin(model(train1)).
bought(fish).
shops(mary).
end(model(train1)).

begin(model(train2)).
bought(spaghetti).
shops(mary).
end(model(train2)).

begin(model(train3)).
bought(fish).
shops(mary).
end(model(train3)).

begin(model(train4)).
bought(fish).
shops(mary).
end(model(train4)).

begin(model(train5)).
bought(fish).
shops(mary).
end(model(train5)).

begin(model(train6)).
bought(fish).
shops(mary).
end(model(train6)).

begin(model(train7)).
bought(fish).
shops(mary).
end(model(train7)).

begin(model(train8)).
bought(fish).
shops(mary).
end(model(train8)).

begin(model(train9)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train9)).

begin(model(train10)).
bought(fish).
shops(mary).
end(model(train10)).

begin(model(train11)).
bought(fish).
shops(mary).
end(model(train11)).

begin(model(train12)).
bought(fish).
shops(mary).
end(model(train12)).

begin(model(train13)).
bought(spaghetti).
shops(mary).
end(model(train13)).

begin(model(train14)).
bought(fish).
shops(mary).
end(model(train14)).

begin(model(train15)).
bought(spaghetti).
shops(john).
end(model(train15)).

begin(model(train16)).
end(model(train16)).

begin(model(train17)).
bought(fish).
shops(mary).
end(model(train17)).

begin(model(train18)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train18)).

begin(model(train19)).
bought(spaghetti).
shops(mary).
end(model(train19)).

begin(model(train20)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train20)).

begin(model(train21)).
bought(fish).
shops(mary).
end(model(train21)).

begin(model(train22)).
bought(fish).
shops(mary).
end(model(train22)).

begin(model(train23)).
bought(fish).
shops(mary).
end(model(train23)).

begin(model(train24)).
bought(fish).
shops(mary).
end(model(train24)).

begin(model(train25)).
bought(fish).
shops(mary).
end(model(train25)).

begin(model(train26)).
end(model(train26)).

begin(model(train27)).
bought(fish).
shops(mary).
end(model(train27)).

begin(model(train28)).
bought(fish).
shops(mary).
end(model(train28)).

begin(model(train29)).
bought(spaghetti).
shops(mary).
end(model(train29)).

begin(model(train30)).
bought(fish).
shops(mary).
end(model(train30)).

begin(model(train31)).
bought(fish).
shops(mary).
end(model(train31)).

begin(model(train32)).
bought(fish).
shops(mary).
end(model(train32)).

begin(model(train33)).
bought(fish).
shops(mary).
end(model(train33)).

begin(model(train34)).
bought(spaghetti).
shops(mary).
end(model(train34)).

begin(model(train35)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train35)).

begin(model(train36)).
bought(spaghetti).
shops(mary).
end(model(train36)).

begin(model(train37)).
bought(fish).
shops(mary).
end(model(train37)).

begin(model(train38)).
bought(spaghetti).
shops(mary).
end(model(train38)).

begin(model(train39)).
bought(fish).
shops(mary).
end(model(train39)).

begin(model(train40)).
bought(fish).
shops(mary).
end(model(train40)).

begin(model(train41)).
bought(spaghetti).
shops(mary).
end(model(train41)).

begin(model(train42)).
bought(spaghetti).
shops(mary).
end(model(train42)).

begin(model(train43)).
bought(spaghetti).
shops(mary).
end(model(train43)).

begin(model(train44)).
bought(spaghetti).
shops(john).
end(model(train44)).

begin(model(train45)).
bought(fish).
shops(mary).
end(model(train45)).

begin(model(train46)).
end(model(train46)).

begin(model(train47)).
bought(fish).
shops(mary).
end(model(train47)).

begin(model(train48)).
bought(fish).
shops(mary).
end(model(train48)).

begin(model(train49)).
bought(fish).
shops(mary).
end(model(train49)).

begin(model(train50)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train50)).

begin(model(train51)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train51)).

begin(model(train52)).
bought(fish).
shops(mary).
end(model(train52)).

begin(model(train53)).
bought(fish).
shops(mary).
end(model(train53)).

begin(model(train54)).
bought(fish).
shops(mary).
end(model(train54)).

begin(model(train55)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train55)).

begin(model(train56)).
bought(fish).
shops(mary).
end(model(train56)).

begin(model(train57)).
bought(fish).
shops(mary).
end(model(train57)).

begin(model(train58)).
bought(spaghetti).
shops(mary).
end(model(train58)).

begin(model(train59)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train59)).

begin(model(train60)).
bought(spaghetti).
shops(mary).
end(model(train60)).

begin(model(train61)).
bought(spaghetti).
shops(mary).
end(model(train61)).

begin(model(train62)).
bought(spaghetti).
shops(mary).
end(model(train62)).

begin(model(train63)).
bought(fish).
shops(mary).
end(model(train63)).

begin(model(train64)).
bought(spaghetti).
shops(mary).
end(model(train64)).

begin(model(train65)).
bought(fish).
shops(mary).
end(model(train65)).

begin(model(train66)).
bought(fish).
shops(mary).
end(model(train66)).

begin(model(train67)).
bought(fish).
shops(mary).
end(model(train67)).

begin(model(train68)).
bought(spaghetti).
shops(mary).
end(model(train68)).

begin(model(train69)).
bought(fish).
shops(mary).
end(model(train69)).

begin(model(train70)).
bought(fish).
shops(mary).
end(model(train70)).

begin(model(train71)).
bought(fish).
shops(mary).
end(model(train71)).

begin(model(train72)).
bought(fish).
shops(mary).
end(model(train72)).

begin(model(train73)).
bought(fish).
shops(mary).
end(model(train73)).

begin(model(train74)).
bought(fish).
shops(mary).
end(model(train74)).

begin(model(train75)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train75)).

begin(model(train76)).
bought(steak).
shops(john).
end(model(train76)).

begin(model(train77)).
bought(fish).
shops(mary).
end(model(train77)).

begin(model(train78)).
bought(spaghetti).
shops(mary).
end(model(train78)).

begin(model(train79)).
bought(steak).
shops(john).
end(model(train79)).

begin(model(train80)).
bought(fish).
shops(mary).
end(model(train80)).

begin(model(train81)).
bought(spaghetti).
shops(mary).
end(model(train81)).

begin(model(train82)).
bought(fish).
shops(mary).
end(model(train82)).

begin(model(train83)).
bought(spaghetti).
shops(mary).
end(model(train83)).

begin(model(train84)).
bought(fish).
shops(mary).
end(model(train84)).

begin(model(train85)).
bought(fish).
shops(mary).
end(model(train85)).

begin(model(train86)).
bought(fish).
shops(mary).
end(model(train86)).

begin(model(train87)).
bought(spaghetti).
shops(mary).
end(model(train87)).

begin(model(train88)).
bought(spaghetti).
shops(john).
end(model(train88)).

begin(model(train89)).
bought(spaghetti).
shops(mary).
end(model(train89)).

begin(model(train90)).
bought(fish).
shops(mary).
end(model(train90)).

begin(model(train91)).
bought(spaghetti).
shops(mary).
end(model(train91)).

begin(model(train92)).
bought(fish).
shops(mary).
end(model(train92)).

begin(model(train93)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train93)).

begin(model(train94)).
bought(spaghetti).
shops(mary).
end(model(train94)).

begin(model(train95)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train95)).

begin(model(train96)).
bought(fish).
shops(mary).
end(model(train96)).

begin(model(train97)).
bought(fish).
shops(mary).
end(model(train97)).

begin(model(train98)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train98)).

begin(model(train99)).
bought(spaghetti).
shops(mary).
end(model(train99)).

begin(model(train100)).
bought(spaghetti).
shops(mary).
end(model(train100)).

begin(model(train101)).
bought(fish).
shops(mary).
end(model(train101)).

begin(model(train102)).
bought(fish).
shops(mary).
end(model(train102)).

begin(model(train103)).
bought(fish).
shops(mary).
end(model(train103)).

begin(model(train104)).
bought(fish).
shops(mary).
end(model(train104)).

begin(model(train105)).
end(model(train105)).

begin(model(train106)).
bought(fish).
shops(mary).
end(model(train106)).

begin(model(train107)).
bought(spaghetti).
shops(mary).
end(model(train107)).

begin(model(train108)).
bought(fish).
shops(mary).
end(model(train108)).

begin(model(train109)).
bought(fish).
shops(mary).
end(model(train109)).

begin(model(train110)).
bought(fish).
shops(mary).
end(model(train110)).

begin(model(train111)).
bought(fish).
shops(mary).
end(model(train111)).

begin(model(train112)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train112)).

begin(model(train113)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train113)).

begin(model(train114)).
bought(fish).
shops(mary).
end(model(train114)).

begin(model(train115)).
bought(spaghetti).
shops(mary).
end(model(train115)).

begin(model(train116)).
bought(fish).
shops(mary).
end(model(train116)).

begin(model(train117)).
bought(spaghetti).
shops(mary).
end(model(train117)).

begin(model(train118)).
bought(fish).
shops(mary).
end(model(train118)).

begin(model(train119)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train119)).

begin(model(train120)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train120)).

begin(model(train121)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train121)).

begin(model(train122)).
bought(fish).
shops(mary).
end(model(train122)).

begin(model(train123)).
end(model(train123)).

begin(model(train124)).
bought(fish).
shops(mary).
end(model(train124)).

begin(model(train125)).
bought(fish).
shops(mary).
end(model(train125)).

begin(model(train126)).
bought(spaghetti).
shops(mary).
end(model(train126)).

begin(model(train127)).
bought(fish).
shops(mary).
end(model(train127)).

begin(model(train128)).
bought(fish).
shops(mary).
end(model(train128)).

begin(model(train129)).
bought(fish).
shops(mary).
end(model(train129)).

begin(model(train130)).
bought(fish).
shops(mary).
end(model(train130)).

begin(model(train131)).
bought(fish).
shops(mary).
end(model(train131)).

begin(model(train132)).
bought(fish).
shops(mary).
end(model(train132)).

begin(model(train133)).
bought(fish).
shops(mary).
end(model(train133)).

begin(model(train134)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train134)).

begin(model(train135)).
bought(fish).
shops(mary).
end(model(train135)).

begin(model(train136)).
bought(fish).
shops(mary).
end(model(train136)).

begin(model(train137)).
bought(fish).
shops(mary).
end(model(train137)).

begin(model(train138)).
bought(fish).
shops(mary).
end(model(train138)).

begin(model(train139)).
bought(fish).
shops(mary).
end(model(train139)).

begin(model(train140)).
bought(fish).
shops(mary).
end(model(train140)).

begin(model(train141)).
end(model(train141)).

begin(model(train142)).
bought(spaghetti).
shops(mary).
end(model(train142)).

begin(model(train143)).
bought(spaghetti).
shops(mary).
end(model(train143)).

begin(model(train144)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train144)).

begin(model(train145)).
bought(spaghetti).
shops(mary).
end(model(train145)).

begin(model(train146)).
end(model(train146)).

begin(model(train147)).
bought(spaghetti).
shops(mary).
end(model(train147)).

begin(model(train148)).
bought(fish).
shops(mary).
end(model(train148)).

begin(model(train149)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train149)).

begin(model(train150)).
end(model(train150)).

begin(model(train151)).
bought(steak).
shops(john).
end(model(train151)).

begin(model(train152)).
bought(spaghetti).
shops(mary).
end(model(train152)).

begin(model(train153)).
bought(fish).
shops(mary).
end(model(train153)).

begin(model(train154)).
bought(fish).
shops(mary).
end(model(train154)).

begin(model(train155)).
bought(fish).
shops(mary).
end(model(train155)).

begin(model(train156)).
bought(fish).
shops(mary).
end(model(train156)).

begin(model(train157)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train157)).

begin(model(train158)).
bought(spaghetti).
shops(mary).
end(model(train158)).

begin(model(train159)).
bought(fish).
shops(mary).
end(model(train159)).

begin(model(train160)).
bought(spaghetti).
shops(mary).
end(model(train160)).

begin(model(train161)).
bought(fish).
shops(mary).
end(model(train161)).

begin(model(train162)).
bought(fish).
shops(mary).
end(model(train162)).

begin(model(train163)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train163)).

begin(model(train164)).
bought(fish).
shops(mary).
end(model(train164)).

begin(model(train165)).
bought(spaghetti).
shops(mary).
end(model(train165)).

begin(model(train166)).
bought(spaghetti).
shops(mary).
end(model(train166)).

begin(model(train167)).
bought(fish).
shops(mary).
end(model(train167)).

begin(model(train168)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train168)).

begin(model(train169)).
bought(fish).
shops(mary).
end(model(train169)).

begin(model(train170)).
bought(spaghetti).
shops(mary).
end(model(train170)).

begin(model(train171)).
bought(spaghetti).
shops(mary).
end(model(train171)).

begin(model(train172)).
bought(fish).
shops(mary).
end(model(train172)).

begin(model(train173)).
bought(fish).
shops(mary).
end(model(train173)).

begin(model(train174)).
bought(fish).
shops(mary).
end(model(train174)).

begin(model(train175)).
bought(fish).
shops(mary).
end(model(train175)).

begin(model(train176)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train176)).

begin(model(train177)).
bought(fish).
shops(mary).
end(model(train177)).

begin(model(train178)).
bought(spaghetti).
shops(mary).
end(model(train178)).

begin(model(train179)).
bought(fish).
shops(mary).
end(model(train179)).

begin(model(train180)).
bought(fish).
shops(mary).
end(model(train180)).

begin(model(train181)).
bought(fish).
shops(mary).
end(model(train181)).

begin(model(train182)).
bought(fish).
shops(mary).
end(model(train182)).

begin(model(train183)).
bought(fish).
shops(mary).
end(model(train183)).

begin(model(train184)).
bought(spaghetti).
shops(mary).
end(model(train184)).

begin(model(train185)).
bought(fish).
shops(mary).
end(model(train185)).

begin(model(train186)).
bought(spaghetti).
shops(mary).
end(model(train186)).

begin(model(train187)).
bought(fish).
shops(mary).
end(model(train187)).

begin(model(train188)).
bought(spaghetti).
shops(mary).
end(model(train188)).

begin(model(train189)).
bought(spaghetti).
shops(mary).
end(model(train189)).

begin(model(train190)).
bought(spaghetti).
shops(mary).
end(model(train190)).

begin(model(train191)).
end(model(train191)).

begin(model(train192)).
bought(spaghetti).
shops(mary).
end(model(train192)).

begin(model(train193)).
bought(fish).
shops(mary).
end(model(train193)).

begin(model(train194)).
bought(fish).
shops(mary).
end(model(train194)).

begin(model(train195)).
bought(fish).
shops(mary).
end(model(train195)).

begin(model(train196)).
bought(fish).
shops(mary).
end(model(train196)).

begin(model(train197)).
bought(fish).
shops(mary).
end(model(train197)).

begin(model(train198)).
bought(fish).
shops(mary).
end(model(train198)).

begin(model(train199)).
bought(fish).
shops(mary).
end(model(train199)).

begin(model(train200)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train200)).

begin(model(train201)).
bought(fish).
shops(mary).
end(model(train201)).

begin(model(train202)).
bought(fish).
shops(mary).
end(model(train202)).

begin(model(train203)).
bought(fish).
shops(mary).
end(model(train203)).

begin(model(train204)).
bought(fish).
shops(mary).
end(model(train204)).

begin(model(train205)).
bought(fish).
shops(mary).
end(model(train205)).

begin(model(train206)).
bought(fish).
shops(mary).
end(model(train206)).

begin(model(train207)).
bought(spaghetti).
shops(mary).
end(model(train207)).

begin(model(train208)).
bought(fish).
shops(mary).
end(model(train208)).

begin(model(train209)).
end(model(train209)).

begin(model(train210)).
bought(fish).
shops(mary).
end(model(train210)).

begin(model(train211)).
bought(fish).
shops(mary).
end(model(train211)).

begin(model(train212)).
bought(spaghetti).
shops(mary).
end(model(train212)).

begin(model(train213)).
bought(spaghetti).
shops(mary).
end(model(train213)).

begin(model(train214)).
bought(fish).
shops(mary).
end(model(train214)).

begin(model(train215)).
bought(fish).
shops(mary).
end(model(train215)).

begin(model(train216)).
bought(fish).
shops(mary).
end(model(train216)).

begin(model(train217)).
bought(fish).
shops(mary).
end(model(train217)).

begin(model(train218)).
bought(spaghetti).
shops(mary).
end(model(train218)).

begin(model(train219)).
bought(spaghetti).
shops(mary).
end(model(train219)).

begin(model(train220)).
bought(fish).
shops(mary).
end(model(train220)).

begin(model(train221)).
bought(fish).
shops(mary).
end(model(train221)).

begin(model(train222)).
bought(spaghetti).
shops(mary).
end(model(train222)).

begin(model(train223)).
bought(fish).
shops(mary).
end(model(train223)).

begin(model(train224)).
end(model(train224)).

begin(model(train225)).
end(model(train225)).

begin(model(train226)).
bought(fish).
shops(mary).
end(model(train226)).

begin(model(train227)).
bought(fish).
shops(mary).
end(model(train227)).

begin(model(train228)).
bought(fish).
shops(mary).
end(model(train228)).

begin(model(train229)).
bought(fish).
shops(mary).
end(model(train229)).

begin(model(train230)).
bought(spaghetti).
shops(mary).
end(model(train230)).

begin(model(train231)).
bought(spaghetti).
shops(mary).
end(model(train231)).

begin(model(train232)).
bought(fish).
shops(mary).
end(model(train232)).

begin(model(train233)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train233)).

begin(model(train234)).
bought(spaghetti).
shops(mary).
end(model(train234)).

begin(model(train235)).
bought(fish).
shops(mary).
end(model(train235)).

begin(model(train236)).
bought(fish).
shops(mary).
end(model(train236)).

begin(model(train237)).
bought(fish).
shops(mary).
end(model(train237)).

begin(model(train238)).
bought(fish).
shops(mary).
end(model(train238)).

begin(model(train239)).
bought(fish).
shops(mary).
end(model(train239)).

begin(model(train240)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train240)).

begin(model(train241)).
bought(fish).
shops(mary).
end(model(train241)).

begin(model(train242)).
bought(spaghetti).
shops(mary).
end(model(train242)).

begin(model(train243)).
bought(fish).
shops(mary).
end(model(train243)).

begin(model(train244)).
bought(spaghetti).
shops(mary).
end(model(train244)).

begin(model(train245)).
bought(fish).
shops(mary).
end(model(train245)).

begin(model(train246)).
bought(fish).
shops(mary).
end(model(train246)).

begin(model(train247)).
bought(fish).
shops(mary).
end(model(train247)).

begin(model(train248)).
bought(spaghetti).
shops(mary).
end(model(train248)).

begin(model(train249)).
bought(fish).
shops(mary).
end(model(train249)).

begin(model(train250)).
bought(spaghetti).
shops(mary).
end(model(train250)).

begin(model(train251)).
bought(spaghetti).
shops(mary).
end(model(train251)).

begin(model(train252)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train252)).

begin(model(train253)).
bought(fish).
shops(mary).
end(model(train253)).

begin(model(train254)).
bought(fish).
shops(mary).
end(model(train254)).

begin(model(train255)).
bought(fish).
shops(mary).
end(model(train255)).

begin(model(train256)).
bought(fish).
shops(mary).
end(model(train256)).

begin(model(train257)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train257)).

begin(model(train258)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train258)).

begin(model(train259)).
bought(spaghetti).
shops(mary).
end(model(train259)).

begin(model(train260)).
bought(fish).
shops(mary).
end(model(train260)).

begin(model(train261)).
bought(fish).
shops(mary).
end(model(train261)).

begin(model(train262)).
bought(fish).
shops(mary).
end(model(train262)).

begin(model(train263)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train263)).

begin(model(train264)).
end(model(train264)).

begin(model(train265)).
end(model(train265)).

begin(model(train266)).
bought(spaghetti).
shops(mary).
end(model(train266)).

begin(model(train267)).
bought(fish).
shops(mary).
end(model(train267)).

begin(model(train268)).
bought(fish).
shops(mary).
end(model(train268)).

begin(model(train269)).
bought(fish).
shops(mary).
end(model(train269)).

begin(model(train270)).
bought(spaghetti).
shops(mary).
end(model(train270)).

begin(model(train271)).
bought(fish).
shops(mary).
end(model(train271)).

begin(model(train272)).
bought(fish).
shops(mary).
end(model(train272)).

begin(model(train273)).
bought(fish).
shops(mary).
end(model(train273)).

begin(model(train274)).
bought(fish).
shops(mary).
end(model(train274)).

begin(model(train275)).
bought(fish).
shops(mary).
end(model(train275)).

begin(model(train276)).
bought(steak).
shops(john).
end(model(train276)).

begin(model(train277)).
bought(spaghetti).
shops(mary).
end(model(train277)).

begin(model(train278)).
bought(fish).
shops(mary).
end(model(train278)).

begin(model(train279)).
bought(fish).
shops(mary).
end(model(train279)).

begin(model(train280)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train280)).

begin(model(train281)).
bought(fish).
shops(mary).
end(model(train281)).

begin(model(train282)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train282)).

begin(model(train283)).
bought(spaghetti).
shops(mary).
end(model(train283)).

begin(model(train284)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train284)).

begin(model(train285)).
bought(fish).
shops(mary).
end(model(train285)).

begin(model(train286)).
bought(fish).
shops(mary).
end(model(train286)).

begin(model(train287)).
bought(fish).
shops(mary).
end(model(train287)).

begin(model(train288)).
bought(fish).
shops(mary).
end(model(train288)).

begin(model(train289)).
bought(fish).
shops(mary).
end(model(train289)).

begin(model(train290)).
bought(fish).
shops(mary).
end(model(train290)).

begin(model(train291)).
end(model(train291)).

begin(model(train292)).
bought(fish).
shops(mary).
end(model(train292)).

begin(model(train293)).
bought(fish).
shops(mary).
end(model(train293)).

begin(model(train294)).
bought(steak).
shops(john).
end(model(train294)).

begin(model(train295)).
bought(fish).
shops(mary).
end(model(train295)).

begin(model(train296)).
bought(spaghetti).
shops(mary).
end(model(train296)).

begin(model(train297)).
bought(fish).
shops(mary).
end(model(train297)).

begin(model(train298)).
bought(spaghetti).
shops(mary).
end(model(train298)).

begin(model(train299)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train299)).

begin(model(train300)).
bought(fish).
shops(mary).
end(model(train300)).

begin(model(train301)).
bought(fish).
shops(mary).
end(model(train301)).

begin(model(train302)).
bought(fish).
shops(mary).
end(model(train302)).

begin(model(train303)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train303)).

begin(model(train304)).
bought(fish).
shops(mary).
end(model(train304)).

begin(model(train305)).
bought(fish).
shops(mary).
end(model(train305)).

begin(model(train306)).
bought(fish).
shops(mary).
end(model(train306)).

begin(model(train307)).
bought(fish).
shops(mary).
end(model(train307)).

begin(model(train308)).
bought(fish).
shops(mary).
end(model(train308)).

begin(model(train309)).
bought(fish).
shops(mary).
end(model(train309)).

begin(model(train310)).
bought(fish).
shops(mary).
end(model(train310)).

begin(model(train311)).
bought(spaghetti).
shops(mary).
end(model(train311)).

begin(model(train312)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train312)).

begin(model(train313)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train313)).

begin(model(train314)).
bought(spaghetti).
shops(john).
end(model(train314)).

begin(model(train315)).
bought(fish).
shops(mary).
end(model(train315)).

begin(model(train316)).
bought(fish).
shops(mary).
end(model(train316)).

begin(model(train317)).
bought(spaghetti).
shops(mary).
end(model(train317)).

begin(model(train318)).
bought(spaghetti).
shops(mary).
end(model(train318)).

begin(model(train319)).
bought(spaghetti).
shops(mary).
end(model(train319)).

begin(model(train320)).
bought(fish).
shops(mary).
end(model(train320)).

begin(model(train321)).
bought(fish).
shops(mary).
end(model(train321)).

begin(model(train322)).
bought(fish).
shops(mary).
end(model(train322)).

begin(model(train323)).
bought(fish).
shops(mary).
end(model(train323)).

begin(model(train324)).
bought(fish).
shops(mary).
end(model(train324)).

begin(model(train325)).
bought(fish).
shops(mary).
end(model(train325)).

begin(model(train326)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train326)).

begin(model(train327)).
bought(fish).
shops(mary).
end(model(train327)).

begin(model(train328)).
bought(spaghetti).
shops(mary).
end(model(train328)).

begin(model(train329)).
bought(fish).
shops(mary).
end(model(train329)).

begin(model(train330)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train330)).

begin(model(train331)).
bought(spaghetti).
shops(mary).
end(model(train331)).

begin(model(train332)).
bought(fish).
shops(mary).
end(model(train332)).

begin(model(train333)).
bought(spaghetti).
shops(mary).
end(model(train333)).

begin(model(train334)).
bought(spaghetti).
shops(mary).
end(model(train334)).

begin(model(train335)).
bought(fish).
shops(mary).
end(model(train335)).

begin(model(train336)).
bought(spaghetti).
shops(mary).
end(model(train336)).

begin(model(train337)).
bought(fish).
shops(mary).
end(model(train337)).

begin(model(train338)).
bought(fish).
shops(mary).
end(model(train338)).

begin(model(train339)).
bought(spaghetti).
shops(mary).
end(model(train339)).

begin(model(train340)).
bought(fish).
shops(mary).
end(model(train340)).

begin(model(train341)).
bought(fish).
shops(mary).
end(model(train341)).

begin(model(train342)).
bought(fish).
shops(mary).
end(model(train342)).

begin(model(train343)).
bought(fish).
shops(mary).
end(model(train343)).

begin(model(train344)).
bought(fish).
shops(mary).
end(model(train344)).

begin(model(train345)).
bought(fish).
shops(mary).
end(model(train345)).

begin(model(train346)).
bought(fish).
shops(mary).
end(model(train346)).

begin(model(train347)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train347)).

begin(model(train348)).
bought(fish).
shops(mary).
end(model(train348)).

begin(model(train349)).
bought(fish).
shops(mary).
end(model(train349)).

begin(model(train350)).
bought(fish).
shops(mary).
end(model(train350)).

begin(model(train351)).
bought(spaghetti).
shops(mary).
end(model(train351)).

begin(model(train352)).
bought(fish).
shops(mary).
end(model(train352)).

begin(model(train353)).
bought(fish).
shops(mary).
end(model(train353)).

begin(model(train354)).
bought(fish).
shops(mary).
end(model(train354)).

begin(model(train355)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train355)).

begin(model(train356)).
bought(fish).
shops(mary).
end(model(train356)).

begin(model(train357)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train357)).

begin(model(train358)).
bought(fish).
shops(mary).
end(model(train358)).

begin(model(train359)).
bought(fish).
shops(mary).
end(model(train359)).

begin(model(train360)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train360)).

begin(model(train361)).
bought(spaghetti).
shops(mary).
end(model(train361)).

begin(model(train362)).
bought(spaghetti).
shops(mary).
end(model(train362)).

begin(model(train363)).
bought(fish).
shops(mary).
end(model(train363)).

begin(model(train364)).
bought(spaghetti).
shops(mary).
end(model(train364)).

begin(model(train365)).
bought(fish).
shops(mary).
end(model(train365)).

begin(model(train366)).
bought(fish).
shops(mary).
end(model(train366)).

begin(model(train367)).
bought(fish).
shops(mary).
end(model(train367)).

begin(model(train368)).
bought(fish).
shops(mary).
end(model(train368)).

begin(model(train369)).
end(model(train369)).

begin(model(train370)).
bought(fish).
shops(mary).
end(model(train370)).

begin(model(train371)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train371)).

begin(model(train372)).
bought(fish).
shops(mary).
end(model(train372)).

begin(model(train373)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train373)).

begin(model(train374)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train374)).

begin(model(train375)).
bought(fish).
shops(mary).
end(model(train375)).

begin(model(train376)).
bought(fish).
shops(mary).
end(model(train376)).

begin(model(train377)).
bought(fish).
shops(mary).
end(model(train377)).

begin(model(train378)).
bought(fish).
shops(mary).
end(model(train378)).

begin(model(train379)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train379)).

begin(model(train380)).
bought(spaghetti).
shops(mary).
end(model(train380)).

begin(model(train381)).
bought(fish).
shops(mary).
end(model(train381)).

begin(model(train382)).
bought(fish).
shops(mary).
end(model(train382)).

begin(model(train383)).
bought(fish).
shops(mary).
end(model(train383)).

begin(model(train384)).
bought(fish).
shops(mary).
end(model(train384)).

begin(model(train385)).
bought(fish).
shops(mary).
end(model(train385)).

begin(model(train386)).
bought(fish).
shops(mary).
end(model(train386)).

begin(model(train387)).
bought(spaghetti).
shops(john).
end(model(train387)).

begin(model(train388)).
bought(fish).
shops(mary).
end(model(train388)).

begin(model(train389)).
bought(fish).
shops(mary).
end(model(train389)).

begin(model(train390)).
bought(fish).
shops(mary).
end(model(train390)).

begin(model(train391)).
bought(spaghetti).
shops(mary).
end(model(train391)).

begin(model(train392)).
bought(spaghetti).
shops(john).
end(model(train392)).

begin(model(train393)).
bought(fish).
shops(mary).
end(model(train393)).

begin(model(train394)).
bought(fish).
shops(mary).
end(model(train394)).

begin(model(train395)).
bought(spaghetti).
shops(mary).
end(model(train395)).

begin(model(train396)).
bought(fish).
shops(mary).
end(model(train396)).

begin(model(train397)).
bought(fish).
shops(mary).
end(model(train397)).

begin(model(train398)).
bought(fish).
shops(mary).
end(model(train398)).

begin(model(train399)).
bought(spaghetti).
shops(mary).
end(model(train399)).

begin(model(train400)).
bought(fish).
shops(mary).
end(model(train400)).

begin(model(train401)).
bought(fish).
shops(mary).
end(model(train401)).

begin(model(train402)).
bought(spaghetti).
shops(mary).
end(model(train402)).

begin(model(train403)).
bought(fish).
shops(mary).
end(model(train403)).

begin(model(train404)).
bought(fish).
shops(mary).
end(model(train404)).

begin(model(train405)).
bought(spaghetti).
shops(mary).
end(model(train405)).

begin(model(train406)).
bought(fish).
shops(mary).
end(model(train406)).

begin(model(train407)).
bought(fish).
shops(mary).
end(model(train407)).

begin(model(train408)).
bought(fish).
shops(mary).
end(model(train408)).

begin(model(train409)).
bought(steak).
shops(john).
end(model(train409)).

begin(model(train410)).
bought(fish).
shops(mary).
end(model(train410)).

begin(model(train411)).
bought(fish).
shops(mary).
end(model(train411)).

begin(model(train412)).
bought(fish).
shops(mary).
end(model(train412)).

begin(model(train413)).
bought(fish).
shops(mary).
end(model(train413)).

begin(model(train414)).
bought(fish).
shops(mary).
end(model(train414)).

begin(model(train415)).
bought(fish).
shops(mary).
end(model(train415)).

begin(model(train416)).
end(model(train416)).

begin(model(train417)).
bought(spaghetti).
shops(mary).
end(model(train417)).

begin(model(train418)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train418)).

begin(model(train419)).
bought(spaghetti).
shops(mary).
end(model(train419)).

begin(model(train420)).
bought(spaghetti).
shops(mary).
end(model(train420)).

begin(model(train421)).
bought(fish).
shops(mary).
end(model(train421)).

begin(model(train422)).
bought(spaghetti).
shops(mary).
end(model(train422)).

begin(model(train423)).
bought(fish).
shops(mary).
end(model(train423)).

begin(model(train424)).
bought(fish).
shops(mary).
end(model(train424)).

begin(model(train425)).
bought(fish).
shops(mary).
end(model(train425)).

begin(model(train426)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train426)).

begin(model(train427)).
bought(spaghetti).
shops(mary).
end(model(train427)).

begin(model(train428)).
bought(fish).
shops(mary).
end(model(train428)).

begin(model(train429)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train429)).

begin(model(train430)).
bought(spaghetti).
shops(mary).
end(model(train430)).

begin(model(train431)).
bought(fish).
shops(mary).
end(model(train431)).

begin(model(train432)).
bought(fish).
shops(mary).
end(model(train432)).

begin(model(train433)).
bought(fish).
shops(mary).
end(model(train433)).

begin(model(train434)).
bought(fish).
shops(mary).
end(model(train434)).

begin(model(train435)).
bought(fish).
shops(mary).
end(model(train435)).

begin(model(train436)).
bought(fish).
shops(mary).
end(model(train436)).

begin(model(train437)).
bought(fish).
shops(mary).
end(model(train437)).

begin(model(train438)).
bought(fish).
shops(mary).
end(model(train438)).

begin(model(train439)).
bought(fish).
shops(mary).
end(model(train439)).

begin(model(train440)).
bought(spaghetti).
shops(mary).
end(model(train440)).

begin(model(train441)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train441)).

begin(model(train442)).
bought(fish).
shops(mary).
end(model(train442)).

begin(model(train443)).
bought(spaghetti).
shops(mary).
end(model(train443)).

begin(model(train444)).
bought(fish).
shops(mary).
end(model(train444)).

begin(model(train445)).
bought(fish).
shops(mary).
end(model(train445)).

begin(model(train446)).
bought(spaghetti).
shops(mary).
end(model(train446)).

begin(model(train447)).
bought(fish).
shops(mary).
end(model(train447)).

begin(model(train448)).
bought(fish).
shops(mary).
end(model(train448)).

begin(model(train449)).
bought(fish).
shops(mary).
end(model(train449)).

begin(model(train450)).
bought(fish).
shops(mary).
end(model(train450)).

begin(model(train451)).
bought(fish).
shops(mary).
end(model(train451)).

begin(model(train452)).
bought(spaghetti).
shops(mary).
end(model(train452)).

begin(model(train453)).
bought(fish).
shops(mary).
end(model(train453)).

begin(model(train454)).
bought(fish).
shops(mary).
end(model(train454)).

begin(model(train455)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train455)).

begin(model(train456)).
bought(spaghetti).
shops(mary).
end(model(train456)).

begin(model(train457)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train457)).

begin(model(train458)).
bought(fish).
shops(mary).
end(model(train458)).

begin(model(train459)).
bought(fish).
shops(mary).
end(model(train459)).

begin(model(train460)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train460)).

begin(model(train461)).
bought(spaghetti).
shops(mary).
end(model(train461)).

begin(model(train462)).
bought(fish).
shops(mary).
end(model(train462)).

begin(model(train463)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train463)).

begin(model(train464)).
bought(spaghetti).
shops(mary).
end(model(train464)).

begin(model(train465)).
bought(fish).
shops(mary).
end(model(train465)).

begin(model(train466)).
bought(fish).
shops(mary).
end(model(train466)).

begin(model(train467)).
bought(fish).
shops(mary).
end(model(train467)).

begin(model(train468)).
bought(steak).
shops(john).
end(model(train468)).

begin(model(train469)).
bought(steak).
shops(john).
end(model(train469)).

begin(model(train470)).
bought(fish).
shops(mary).
end(model(train470)).

begin(model(train471)).
bought(fish).
shops(mary).
end(model(train471)).

begin(model(train472)).
bought(fish).
shops(mary).
end(model(train472)).

begin(model(train473)).
bought(fish).
shops(mary).
end(model(train473)).

begin(model(train474)).
bought(spaghetti).
shops(mary).
end(model(train474)).

begin(model(train475)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train475)).

begin(model(train476)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train476)).

begin(model(train477)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train477)).

begin(model(train478)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train478)).

begin(model(train479)).
bought(spaghetti).
shops(mary).
end(model(train479)).

begin(model(train480)).
bought(fish).
shops(mary).
end(model(train480)).

begin(model(train481)).
bought(fish).
shops(mary).
end(model(train481)).

begin(model(train482)).
bought(fish).
shops(mary).
end(model(train482)).

begin(model(train483)).
bought(fish).
shops(mary).
end(model(train483)).

begin(model(train484)).
bought(fish).
shops(mary).
end(model(train484)).

begin(model(train485)).
bought(spaghetti).
shops(mary).
end(model(train485)).

begin(model(train486)).
bought(spaghetti).
shops(mary).
end(model(train486)).

begin(model(train487)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train487)).

begin(model(train488)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train488)).

begin(model(train489)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train489)).

begin(model(train490)).
bought(fish).
shops(mary).
end(model(train490)).

begin(model(train491)).
bought(spaghetti).
shops(mary).
end(model(train491)).

begin(model(train492)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train492)).

begin(model(train493)).
bought(fish).
shops(mary).
end(model(train493)).

begin(model(train494)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train494)).

begin(model(train495)).
bought(spaghetti).
shops(mary).
end(model(train495)).

begin(model(train496)).
bought(spaghetti).
shops(mary).
end(model(train496)).

begin(model(train497)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train497)).

begin(model(train498)).
bought(fish).
shops(mary).
end(model(train498)).

begin(model(train499)).
bought(fish).
shops(mary).
end(model(train499)).

begin(model(train500)).
bought(fish).
shops(mary).
end(model(train500)).

begin(model(train501)).
bought(spaghetti).
shops(mary).
end(model(train501)).

begin(model(train502)).
bought(fish).
shops(mary).
end(model(train502)).

begin(model(train503)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train503)).

begin(model(train504)).
bought(fish).
shops(mary).
end(model(train504)).

begin(model(train505)).
bought(spaghetti).
shops(mary).
end(model(train505)).

begin(model(train506)).
bought(fish).
shops(mary).
end(model(train506)).

begin(model(train507)).
bought(fish).
shops(mary).
end(model(train507)).

begin(model(train508)).
bought(spaghetti).
shops(mary).
end(model(train508)).

begin(model(train509)).
bought(fish).
shops(mary).
end(model(train509)).

begin(model(train510)).
bought(fish).
shops(mary).
end(model(train510)).

begin(model(train511)).
bought(spaghetti).
shops(mary).
end(model(train511)).

begin(model(train512)).
bought(fish).
shops(mary).
end(model(train512)).

begin(model(train513)).
bought(fish).
shops(mary).
end(model(train513)).

begin(model(train514)).
bought(fish).
shops(mary).
end(model(train514)).

begin(model(train515)).
bought(spaghetti).
shops(mary).
end(model(train515)).

begin(model(train516)).
bought(spaghetti).
shops(john).
end(model(train516)).

begin(model(train517)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train517)).

begin(model(train518)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train518)).

begin(model(train519)).
bought(fish).
shops(mary).
end(model(train519)).

begin(model(train520)).
bought(fish).
shops(mary).
end(model(train520)).

begin(model(train521)).
bought(fish).
shops(mary).
end(model(train521)).

begin(model(train522)).
bought(fish).
shops(mary).
end(model(train522)).

begin(model(train523)).
bought(fish).
shops(mary).
end(model(train523)).

begin(model(train524)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train524)).

begin(model(train525)).
bought(spaghetti).
shops(mary).
end(model(train525)).

begin(model(train526)).
bought(fish).
shops(mary).
end(model(train526)).

begin(model(train527)).
bought(fish).
shops(mary).
end(model(train527)).

begin(model(train528)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train528)).

begin(model(train529)).
bought(spaghetti).
shops(mary).
end(model(train529)).

begin(model(train530)).
bought(spaghetti).
shops(mary).
end(model(train530)).

begin(model(train531)).
bought(fish).
shops(mary).
end(model(train531)).

begin(model(train532)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train532)).

begin(model(train533)).
end(model(train533)).

begin(model(train534)).
bought(fish).
shops(mary).
end(model(train534)).

begin(model(train535)).
bought(spaghetti).
shops(mary).
end(model(train535)).

begin(model(train536)).
bought(spaghetti).
shops(mary).
end(model(train536)).

begin(model(train537)).
bought(fish).
shops(mary).
end(model(train537)).

begin(model(train538)).
bought(fish).
shops(mary).
end(model(train538)).

begin(model(train539)).
bought(fish).
shops(mary).
end(model(train539)).

begin(model(train540)).
bought(fish).
shops(mary).
end(model(train540)).

begin(model(train541)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train541)).

begin(model(train542)).
bought(fish).
shops(mary).
end(model(train542)).

begin(model(train543)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train543)).

begin(model(train544)).
bought(spaghetti).
shops(mary).
end(model(train544)).

begin(model(train545)).
bought(fish).
shops(mary).
end(model(train545)).

begin(model(train546)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train546)).

begin(model(train547)).
bought(fish).
shops(mary).
end(model(train547)).

begin(model(train548)).
bought(spaghetti).
shops(mary).
end(model(train548)).

begin(model(train549)).
bought(fish).
shops(mary).
end(model(train549)).

begin(model(train550)).
bought(fish).
shops(mary).
end(model(train550)).

begin(model(train551)).
bought(fish).
shops(mary).
end(model(train551)).

begin(model(train552)).
bought(spaghetti).
shops(mary).
end(model(train552)).

begin(model(train553)).
bought(spaghetti).
shops(mary).
end(model(train553)).

begin(model(train554)).
bought(fish).
shops(mary).
end(model(train554)).

begin(model(train555)).
bought(fish).
shops(mary).
end(model(train555)).

begin(model(train556)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train556)).

begin(model(train557)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train557)).

begin(model(train558)).
bought(fish).
shops(mary).
end(model(train558)).

begin(model(train559)).
bought(spaghetti).
shops(mary).
end(model(train559)).

begin(model(train560)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train560)).

begin(model(train561)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train561)).

begin(model(train562)).
bought(spaghetti).
shops(mary).
end(model(train562)).

begin(model(train563)).
bought(spaghetti).
shops(mary).
end(model(train563)).

begin(model(train564)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train564)).

begin(model(train565)).
bought(fish).
shops(mary).
end(model(train565)).

begin(model(train566)).
bought(spaghetti).
shops(mary).
end(model(train566)).

begin(model(train567)).
bought(spaghetti).
shops(mary).
end(model(train567)).

begin(model(train568)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train568)).

begin(model(train569)).
bought(fish).
shops(mary).
end(model(train569)).

begin(model(train570)).
bought(fish).
shops(mary).
end(model(train570)).

begin(model(train571)).
bought(fish).
shops(mary).
end(model(train571)).

begin(model(train572)).
bought(fish).
shops(mary).
end(model(train572)).

begin(model(train573)).
bought(fish).
shops(mary).
end(model(train573)).

begin(model(train574)).
bought(fish).
shops(mary).
end(model(train574)).

begin(model(train575)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train575)).

begin(model(train576)).
bought(fish).
shops(mary).
end(model(train576)).

begin(model(train577)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train577)).

begin(model(train578)).
bought(fish).
shops(mary).
end(model(train578)).

begin(model(train579)).
bought(spaghetti).
shops(mary).
end(model(train579)).

begin(model(train580)).
bought(fish).
shops(mary).
end(model(train580)).

begin(model(train581)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train581)).

begin(model(train582)).
bought(spaghetti).
shops(mary).
end(model(train582)).

begin(model(train583)).
bought(fish).
shops(mary).
end(model(train583)).

begin(model(train584)).
bought(fish).
shops(mary).
end(model(train584)).

begin(model(train585)).
bought(fish).
shops(mary).
end(model(train585)).

begin(model(train586)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train586)).

begin(model(train587)).
bought(fish).
shops(mary).
end(model(train587)).

begin(model(train588)).
bought(fish).
shops(mary).
end(model(train588)).

begin(model(train589)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train589)).

begin(model(train590)).
bought(fish).
shops(mary).
end(model(train590)).

begin(model(train591)).
bought(fish).
shops(mary).
end(model(train591)).

begin(model(train592)).
bought(spaghetti).
shops(mary).
end(model(train592)).

begin(model(train593)).
bought(spaghetti).
shops(mary).
end(model(train593)).

begin(model(train594)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train594)).

begin(model(train595)).
bought(fish).
shops(mary).
end(model(train595)).

begin(model(train596)).
bought(spaghetti).
shops(mary).
end(model(train596)).

begin(model(train597)).
bought(spaghetti).
shops(mary).
end(model(train597)).

begin(model(train598)).
end(model(train598)).

begin(model(train599)).
bought(fish).
shops(mary).
end(model(train599)).

begin(model(train600)).
bought(fish).
shops(mary).
end(model(train600)).

begin(model(train601)).
bought(spaghetti).
shops(mary).
end(model(train601)).

begin(model(train602)).
bought(fish).
shops(mary).
end(model(train602)).

begin(model(train603)).
bought(fish).
shops(mary).
end(model(train603)).

begin(model(train604)).
bought(fish).
shops(mary).
end(model(train604)).

begin(model(train605)).
bought(fish).
shops(mary).
end(model(train605)).

begin(model(train606)).
end(model(train606)).

begin(model(train607)).
bought(spaghetti).
shops(mary).
end(model(train607)).

begin(model(train608)).
bought(spaghetti).
shops(mary).
end(model(train608)).

begin(model(train609)).
bought(fish).
shops(mary).
end(model(train609)).

begin(model(train610)).
bought(spaghetti).
shops(john).
end(model(train610)).

begin(model(train611)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train611)).

begin(model(train612)).
bought(spaghetti).
shops(mary).
end(model(train612)).

begin(model(train613)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train613)).

begin(model(train614)).
bought(fish).
shops(mary).
end(model(train614)).

begin(model(train615)).
bought(fish).
shops(mary).
end(model(train615)).

begin(model(train616)).
bought(fish).
shops(mary).
end(model(train616)).

begin(model(train617)).
bought(spaghetti).
shops(mary).
end(model(train617)).

begin(model(train618)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train618)).

begin(model(train619)).
end(model(train619)).

begin(model(train620)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train620)).

begin(model(train621)).
bought(fish).
shops(mary).
end(model(train621)).

begin(model(train622)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train622)).

begin(model(train623)).
bought(spaghetti).
shops(mary).
end(model(train623)).

begin(model(train624)).
bought(fish).
shops(mary).
end(model(train624)).

begin(model(train625)).
bought(steak).
shops(john).
end(model(train625)).

begin(model(train626)).
bought(fish).
shops(mary).
end(model(train626)).

begin(model(train627)).
bought(fish).
shops(mary).
end(model(train627)).

begin(model(train628)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train628)).

begin(model(train629)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train629)).

begin(model(train630)).
bought(spaghetti).
shops(mary).
end(model(train630)).

begin(model(train631)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train631)).

begin(model(train632)).
bought(spaghetti).
shops(mary).
end(model(train632)).

begin(model(train633)).
bought(spaghetti).
shops(mary).
end(model(train633)).

begin(model(train634)).
bought(fish).
shops(mary).
end(model(train634)).

begin(model(train635)).
bought(fish).
shops(mary).
end(model(train635)).

begin(model(train636)).
end(model(train636)).

begin(model(train637)).
bought(fish).
shops(mary).
end(model(train637)).

begin(model(train638)).
bought(fish).
shops(mary).
end(model(train638)).

begin(model(train639)).
bought(spaghetti).
shops(mary).
end(model(train639)).

begin(model(train640)).
end(model(train640)).

begin(model(train641)).
bought(fish).
shops(mary).
end(model(train641)).

begin(model(train642)).
bought(fish).
shops(mary).
end(model(train642)).

begin(model(train643)).
bought(fish).
shops(mary).
end(model(train643)).

begin(model(train644)).
bought(spaghetti).
shops(mary).
end(model(train644)).

begin(model(train645)).
bought(spaghetti).
shops(mary).
end(model(train645)).

begin(model(train646)).
bought(spaghetti).
shops(mary).
end(model(train646)).

begin(model(train647)).
bought(fish).
shops(mary).
end(model(train647)).

begin(model(train648)).
bought(fish).
shops(mary).
end(model(train648)).

begin(model(train649)).
bought(fish).
shops(mary).
end(model(train649)).

begin(model(train650)).
bought(fish).
shops(mary).
end(model(train650)).

begin(model(train651)).
bought(fish).
shops(mary).
end(model(train651)).

begin(model(train652)).
bought(fish).
shops(mary).
end(model(train652)).

begin(model(train653)).
bought(fish).
shops(mary).
end(model(train653)).

begin(model(train654)).
bought(fish).
shops(mary).
end(model(train654)).

begin(model(train655)).
bought(spaghetti).
shops(john).
end(model(train655)).

begin(model(train656)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train656)).

begin(model(train657)).
bought(fish).
shops(mary).
end(model(train657)).

begin(model(train658)).
bought(fish).
shops(mary).
end(model(train658)).

begin(model(train659)).
bought(spaghetti).
shops(mary).
end(model(train659)).

begin(model(train660)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train660)).

begin(model(train661)).
bought(fish).
shops(mary).
end(model(train661)).

begin(model(train662)).
bought(fish).
shops(mary).
end(model(train662)).

begin(model(train663)).
bought(spaghetti).
shops(mary).
end(model(train663)).

begin(model(train664)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train664)).

begin(model(train665)).
bought(fish).
shops(mary).
end(model(train665)).

begin(model(train666)).
bought(spaghetti).
shops(mary).
end(model(train666)).

begin(model(train667)).
bought(spaghetti).
shops(mary).
end(model(train667)).

begin(model(train668)).
bought(fish).
shops(mary).
end(model(train668)).

begin(model(train669)).
bought(spaghetti).
shops(mary).
end(model(train669)).

begin(model(train670)).
bought(fish).
shops(mary).
end(model(train670)).

begin(model(train671)).
bought(fish).
shops(mary).
end(model(train671)).

begin(model(train672)).
bought(fish).
shops(mary).
end(model(train672)).

begin(model(train673)).
bought(fish).
shops(mary).
end(model(train673)).

begin(model(train674)).
bought(fish).
shops(mary).
end(model(train674)).

begin(model(train675)).
bought(spaghetti).
shops(mary).
end(model(train675)).

begin(model(train676)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train676)).

begin(model(train677)).
bought(spaghetti).
shops(mary).
end(model(train677)).

begin(model(train678)).
bought(fish).
shops(mary).
end(model(train678)).

begin(model(train679)).
bought(fish).
shops(mary).
end(model(train679)).

begin(model(train680)).
bought(spaghetti).
shops(mary).
end(model(train680)).

begin(model(train681)).
bought(fish).
shops(mary).
end(model(train681)).

begin(model(train682)).
bought(fish).
shops(mary).
end(model(train682)).

begin(model(train683)).
bought(fish).
shops(mary).
end(model(train683)).

begin(model(train684)).
bought(fish).
shops(mary).
end(model(train684)).

begin(model(train685)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train685)).

begin(model(train686)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train686)).

begin(model(train687)).
bought(fish).
shops(mary).
end(model(train687)).

begin(model(train688)).
bought(fish).
shops(mary).
end(model(train688)).

begin(model(train689)).
bought(fish).
shops(mary).
end(model(train689)).

begin(model(train690)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train690)).

begin(model(train691)).
bought(fish).
shops(mary).
end(model(train691)).

begin(model(train692)).
bought(fish).
shops(mary).
end(model(train692)).

begin(model(train693)).
bought(spaghetti).
shops(mary).
end(model(train693)).

begin(model(train694)).
bought(spaghetti).
shops(mary).
end(model(train694)).

begin(model(train695)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train695)).

begin(model(train696)).
bought(fish).
shops(mary).
end(model(train696)).

begin(model(train697)).
bought(spaghetti).
shops(mary).
end(model(train697)).

begin(model(train698)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train698)).

begin(model(train699)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train699)).

begin(model(train700)).
bought(fish).
shops(mary).
end(model(train700)).

begin(model(train701)).
bought(fish).
shops(mary).
end(model(train701)).

begin(model(train702)).
bought(fish).
shops(mary).
end(model(train702)).

begin(model(train703)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train703)).

begin(model(train704)).
bought(fish).
shops(mary).
end(model(train704)).

begin(model(train705)).
bought(spaghetti).
shops(mary).
end(model(train705)).

begin(model(train706)).
bought(spaghetti).
shops(mary).
end(model(train706)).

begin(model(train707)).
bought(fish).
shops(mary).
end(model(train707)).

begin(model(train708)).
bought(spaghetti).
shops(mary).
end(model(train708)).

begin(model(train709)).
bought(fish).
shops(mary).
end(model(train709)).

begin(model(train710)).
bought(fish).
shops(mary).
end(model(train710)).

begin(model(train711)).
end(model(train711)).

begin(model(train712)).
bought(fish).
shops(mary).
end(model(train712)).

begin(model(train713)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train713)).

begin(model(train714)).
bought(fish).
shops(mary).
end(model(train714)).

begin(model(train715)).
bought(spaghetti).
shops(mary).
end(model(train715)).

begin(model(train716)).
bought(fish).
shops(mary).
end(model(train716)).

begin(model(train717)).
bought(spaghetti).
shops(john).
end(model(train717)).

begin(model(train718)).
bought(fish).
shops(mary).
end(model(train718)).

begin(model(train719)).
bought(fish).
shops(mary).
end(model(train719)).

begin(model(train720)).
bought(fish).
shops(mary).
end(model(train720)).

begin(model(train721)).
bought(fish).
shops(mary).
end(model(train721)).

begin(model(train722)).
bought(spaghetti).
shops(mary).
end(model(train722)).

begin(model(train723)).
bought(fish).
shops(mary).
end(model(train723)).

begin(model(train724)).
bought(spaghetti).
shops(mary).
end(model(train724)).

begin(model(train725)).
bought(fish).
shops(mary).
end(model(train725)).

begin(model(train726)).
bought(fish).
shops(mary).
end(model(train726)).

begin(model(train727)).
bought(spaghetti).
shops(mary).
end(model(train727)).

begin(model(train728)).
bought(fish).
shops(mary).
end(model(train728)).

begin(model(train729)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train729)).

begin(model(train730)).
bought(spaghetti).
shops(mary).
end(model(train730)).

begin(model(train731)).
bought(spaghetti).
shops(mary).
end(model(train731)).

begin(model(train732)).
bought(fish).
shops(mary).
end(model(train732)).

begin(model(train733)).
bought(fish).
shops(mary).
end(model(train733)).

begin(model(train734)).
bought(fish).
shops(mary).
end(model(train734)).

begin(model(train735)).
bought(spaghetti).
shops(mary).
end(model(train735)).

begin(model(train736)).
bought(fish).
shops(mary).
end(model(train736)).

begin(model(train737)).
bought(fish).
shops(mary).
end(model(train737)).

begin(model(train738)).
bought(spaghetti).
shops(mary).
end(model(train738)).

begin(model(train739)).
bought(spaghetti).
shops(mary).
end(model(train739)).

begin(model(train740)).
bought(fish).
shops(mary).
end(model(train740)).

begin(model(train741)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train741)).

begin(model(train742)).
bought(fish).
shops(mary).
end(model(train742)).

begin(model(train743)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train743)).

begin(model(train744)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train744)).

begin(model(train745)).
bought(spaghetti).
shops(mary).
end(model(train745)).

begin(model(train746)).
bought(spaghetti).
shops(mary).
end(model(train746)).

begin(model(train747)).
bought(fish).
shops(mary).
end(model(train747)).

begin(model(train748)).
bought(spaghetti).
shops(mary).
end(model(train748)).

begin(model(train749)).
bought(fish).
shops(mary).
end(model(train749)).

begin(model(train750)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train750)).

begin(model(train751)).
bought(spaghetti).
shops(mary).
end(model(train751)).

begin(model(train752)).
bought(spaghetti).
shops(mary).
end(model(train752)).

begin(model(train753)).
bought(fish).
shops(mary).
end(model(train753)).

begin(model(train754)).
bought(fish).
shops(mary).
end(model(train754)).

begin(model(train755)).
bought(fish).
shops(mary).
end(model(train755)).

begin(model(train756)).
bought(fish).
shops(mary).
end(model(train756)).

begin(model(train757)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train757)).

begin(model(train758)).
bought(fish).
shops(mary).
end(model(train758)).

begin(model(train759)).
bought(fish).
shops(mary).
end(model(train759)).

begin(model(train760)).
bought(fish).
shops(mary).
end(model(train760)).

begin(model(train761)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train761)).

begin(model(train762)).
bought(spaghetti).
shops(mary).
end(model(train762)).

begin(model(train763)).
bought(fish).
shops(mary).
end(model(train763)).

begin(model(train764)).
bought(fish).
shops(mary).
end(model(train764)).

begin(model(train765)).
bought(fish).
shops(mary).
end(model(train765)).

begin(model(train766)).
bought(fish).
shops(mary).
end(model(train766)).

begin(model(train767)).
bought(fish).
shops(mary).
end(model(train767)).

begin(model(train768)).
bought(spaghetti).
shops(mary).
end(model(train768)).

begin(model(train769)).
bought(fish).
shops(mary).
end(model(train769)).

begin(model(train770)).
bought(spaghetti).
shops(mary).
end(model(train770)).

begin(model(train771)).
bought(fish).
shops(mary).
end(model(train771)).

begin(model(train772)).
bought(fish).
shops(mary).
end(model(train772)).

begin(model(train773)).
bought(spaghetti).
shops(mary).
end(model(train773)).

begin(model(train774)).
bought(fish).
shops(mary).
end(model(train774)).

begin(model(train775)).
bought(fish).
shops(mary).
end(model(train775)).

begin(model(train776)).
bought(spaghetti).
shops(mary).
end(model(train776)).

begin(model(train777)).
bought(spaghetti).
shops(mary).
end(model(train777)).

begin(model(train778)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train778)).

begin(model(train779)).
bought(spaghetti).
shops(john).
end(model(train779)).

begin(model(train780)).
bought(fish).
shops(mary).
end(model(train780)).

begin(model(train781)).
bought(spaghetti).
shops(mary).
end(model(train781)).

begin(model(train782)).
bought(fish).
shops(mary).
end(model(train782)).

begin(model(train783)).
bought(fish).
shops(mary).
end(model(train783)).

begin(model(train784)).
bought(fish).
shops(mary).
end(model(train784)).

begin(model(train785)).
bought(spaghetti).
shops(mary).
end(model(train785)).

begin(model(train786)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train786)).

begin(model(train787)).
bought(fish).
shops(mary).
end(model(train787)).

begin(model(train788)).
bought(spaghetti).
shops(mary).
end(model(train788)).

begin(model(train789)).
end(model(train789)).

begin(model(train790)).
bought(fish).
shops(mary).
end(model(train790)).

begin(model(train791)).
bought(fish).
shops(mary).
end(model(train791)).

begin(model(train792)).
bought(fish).
shops(mary).
end(model(train792)).

begin(model(train793)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train793)).

begin(model(train794)).
bought(spaghetti).
shops(mary).
end(model(train794)).

begin(model(train795)).
bought(fish).
shops(mary).
end(model(train795)).

begin(model(train796)).
bought(fish).
shops(mary).
end(model(train796)).

begin(model(train797)).
bought(fish).
shops(mary).
end(model(train797)).

begin(model(train798)).
bought(spaghetti).
shops(mary).
end(model(train798)).

begin(model(train799)).
bought(fish).
shops(mary).
end(model(train799)).

begin(model(train800)).
bought(fish).
shops(mary).
end(model(train800)).

begin(model(train801)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train801)).

begin(model(train802)).
bought(spaghetti).
shops(mary).
end(model(train802)).

begin(model(train803)).
bought(fish).
shops(mary).
end(model(train803)).

begin(model(train804)).
bought(spaghetti).
shops(mary).
end(model(train804)).

begin(model(train805)).
bought(spaghetti).
shops(mary).
end(model(train805)).

begin(model(train806)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train806)).

begin(model(train807)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train807)).

begin(model(train808)).
bought(fish).
shops(mary).
end(model(train808)).

begin(model(train809)).
bought(fish).
shops(mary).
end(model(train809)).

begin(model(train810)).
bought(fish).
shops(mary).
end(model(train810)).

begin(model(train811)).
bought(fish).
shops(mary).
end(model(train811)).

begin(model(train812)).
bought(fish).
shops(mary).
end(model(train812)).

begin(model(train813)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train813)).

begin(model(train814)).
bought(fish).
shops(mary).
end(model(train814)).

begin(model(train815)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train815)).

begin(model(train816)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train816)).

begin(model(train817)).
bought(fish).
shops(mary).
end(model(train817)).

begin(model(train818)).
bought(spaghetti).
shops(mary).
end(model(train818)).

begin(model(train819)).
bought(spaghetti).
shops(mary).
end(model(train819)).

begin(model(train820)).
bought(fish).
shops(mary).
end(model(train820)).

begin(model(train821)).
bought(spaghetti).
shops(john).
end(model(train821)).

begin(model(train822)).
bought(fish).
shops(mary).
end(model(train822)).

begin(model(train823)).
bought(fish).
shops(mary).
end(model(train823)).

begin(model(train824)).
bought(fish).
shops(mary).
end(model(train824)).

begin(model(train825)).
bought(fish).
shops(mary).
end(model(train825)).

begin(model(train826)).
bought(fish).
shops(mary).
end(model(train826)).

begin(model(train827)).
bought(fish).
shops(mary).
end(model(train827)).

begin(model(train828)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train828)).

begin(model(train829)).
bought(fish).
shops(mary).
end(model(train829)).

begin(model(train830)).
bought(fish).
shops(mary).
end(model(train830)).

begin(model(train831)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train831)).

begin(model(train832)).
bought(spaghetti).
shops(mary).
end(model(train832)).

begin(model(train833)).
bought(fish).
shops(mary).
end(model(train833)).

begin(model(train834)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train834)).

begin(model(train835)).
bought(fish).
shops(mary).
end(model(train835)).

begin(model(train836)).
bought(fish).
shops(mary).
end(model(train836)).

begin(model(train837)).
bought(spaghetti).
shops(mary).
end(model(train837)).

begin(model(train838)).
bought(fish).
shops(mary).
end(model(train838)).

begin(model(train839)).
bought(fish).
shops(mary).
end(model(train839)).

begin(model(train840)).
bought(fish).
shops(mary).
end(model(train840)).

begin(model(train841)).
bought(fish).
shops(mary).
end(model(train841)).

begin(model(train842)).
bought(fish).
shops(mary).
end(model(train842)).

begin(model(train843)).
bought(fish).
shops(mary).
end(model(train843)).

begin(model(train844)).
bought(fish).
shops(mary).
end(model(train844)).

begin(model(train845)).
bought(fish).
shops(mary).
end(model(train845)).

begin(model(train846)).
bought(fish).
shops(mary).
end(model(train846)).

begin(model(train847)).
bought(spaghetti).
shops(mary).
end(model(train847)).

begin(model(train848)).
bought(spaghetti).
shops(mary).
end(model(train848)).

begin(model(train849)).
bought(fish).
shops(mary).
end(model(train849)).

begin(model(train850)).
bought(spaghetti).
shops(mary).
end(model(train850)).

begin(model(train851)).
bought(fish).
shops(mary).
end(model(train851)).

begin(model(train852)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train852)).

begin(model(train853)).
bought(fish).
shops(mary).
end(model(train853)).

begin(model(train854)).
bought(spaghetti).
shops(mary).
end(model(train854)).

begin(model(train855)).
bought(fish).
shops(mary).
end(model(train855)).

begin(model(train856)).
bought(fish).
shops(mary).
end(model(train856)).

begin(model(train857)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train857)).

begin(model(train858)).
bought(fish).
shops(mary).
end(model(train858)).

begin(model(train859)).
bought(spaghetti).
shops(mary).
end(model(train859)).

begin(model(train860)).
bought(spaghetti).
shops(mary).
end(model(train860)).

begin(model(train861)).
bought(fish).
shops(mary).
end(model(train861)).

begin(model(train862)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train862)).

begin(model(train863)).
bought(fish).
shops(mary).
end(model(train863)).

begin(model(train864)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train864)).

begin(model(train865)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train865)).

begin(model(train866)).
bought(spaghetti).
shops(mary).
end(model(train866)).

begin(model(train867)).
bought(fish).
shops(mary).
end(model(train867)).

begin(model(train868)).
bought(fish).
shops(mary).
end(model(train868)).

begin(model(train869)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train869)).

begin(model(train870)).
bought(fish).
shops(mary).
end(model(train870)).

begin(model(train871)).
bought(spaghetti).
shops(mary).
end(model(train871)).

begin(model(train872)).
bought(fish).
shops(mary).
end(model(train872)).

begin(model(train873)).
bought(spaghetti).
shops(mary).
end(model(train873)).

begin(model(train874)).
bought(spaghetti).
shops(mary).
end(model(train874)).

begin(model(train875)).
bought(spaghetti).
shops(mary).
end(model(train875)).

begin(model(train876)).
bought(fish).
shops(mary).
end(model(train876)).

begin(model(train877)).
bought(steak).
shops(john).
end(model(train877)).

begin(model(train878)).
bought(steak).
shops(john).
end(model(train878)).

begin(model(train879)).
bought(fish).
shops(mary).
end(model(train879)).

begin(model(train880)).
bought(spaghetti).
shops(mary).
end(model(train880)).

begin(model(train881)).
bought(spaghetti).
shops(john).
end(model(train881)).

begin(model(train882)).
bought(fish).
shops(mary).
end(model(train882)).

begin(model(train883)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train883)).

begin(model(train884)).
bought(fish).
shops(mary).
end(model(train884)).

begin(model(train885)).
bought(fish).
shops(mary).
end(model(train885)).

begin(model(train886)).
bought(fish).
shops(mary).
end(model(train886)).

begin(model(train887)).
bought(fish).
shops(mary).
end(model(train887)).

begin(model(train888)).
bought(spaghetti).
shops(mary).
end(model(train888)).

begin(model(train889)).
bought(fish).
shops(mary).
end(model(train889)).

begin(model(train890)).
bought(fish).
shops(mary).
end(model(train890)).

begin(model(train891)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train891)).

begin(model(train892)).
bought(spaghetti).
shops(mary).
end(model(train892)).

begin(model(train893)).
bought(fish).
shops(mary).
end(model(train893)).

begin(model(train894)).
bought(fish).
shops(mary).
end(model(train894)).

begin(model(train895)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train895)).

begin(model(train896)).
bought(spaghetti).
shops(mary).
end(model(train896)).

begin(model(train897)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train897)).

begin(model(train898)).
bought(fish).
shops(mary).
end(model(train898)).

begin(model(train899)).
bought(fish).
shops(mary).
end(model(train899)).

begin(model(train900)).
bought(fish).
shops(mary).
end(model(train900)).

begin(model(train901)).
bought(fish).
shops(mary).
end(model(train901)).

begin(model(train902)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train902)).

begin(model(train903)).
bought(steak).
shops(john).
end(model(train903)).

begin(model(train904)).
bought(fish).
shops(mary).
end(model(train904)).

begin(model(train905)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train905)).

begin(model(train906)).
bought(spaghetti).
shops(mary).
end(model(train906)).

begin(model(train907)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train907)).

begin(model(train908)).
bought(fish).
shops(mary).
end(model(train908)).

begin(model(train909)).
bought(fish).
shops(mary).
end(model(train909)).

begin(model(train910)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train910)).

begin(model(train911)).
bought(fish).
shops(mary).
end(model(train911)).

begin(model(train912)).
bought(fish).
shops(mary).
end(model(train912)).

begin(model(train913)).
bought(fish).
shops(mary).
end(model(train913)).

begin(model(train914)).
bought(fish).
shops(mary).
end(model(train914)).

begin(model(train915)).
bought(fish).
shops(mary).
end(model(train915)).

begin(model(train916)).
bought(fish).
shops(mary).
end(model(train916)).

begin(model(train917)).
bought(spaghetti).
shops(mary).
end(model(train917)).

begin(model(train918)).
end(model(train918)).

begin(model(train919)).
bought(fish).
shops(mary).
end(model(train919)).

begin(model(train920)).
bought(fish).
shops(mary).
end(model(train920)).

begin(model(train921)).
bought(fish).
shops(mary).
end(model(train921)).

begin(model(train922)).
bought(fish).
shops(mary).
end(model(train922)).

begin(model(train923)).
bought(steak).
shops(john).
end(model(train923)).

begin(model(train924)).
bought(fish).
shops(mary).
end(model(train924)).

begin(model(train925)).
bought(spaghetti).
shops(mary).
end(model(train925)).

begin(model(train926)).
bought(fish).
shops(mary).
end(model(train926)).

begin(model(train927)).
bought(fish).
shops(mary).
end(model(train927)).

begin(model(train928)).
bought(spaghetti).
shops(mary).
end(model(train928)).

begin(model(train929)).
bought(fish).
shops(mary).
end(model(train929)).

begin(model(train930)).
end(model(train930)).

begin(model(train931)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train931)).

begin(model(train932)).
bought(fish).
shops(mary).
end(model(train932)).

begin(model(train933)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train933)).

begin(model(train934)).
bought(fish).
shops(mary).
end(model(train934)).

begin(model(train935)).
bought(fish).
shops(mary).
end(model(train935)).

begin(model(train936)).
bought(fish).
shops(mary).
end(model(train936)).

begin(model(train937)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train937)).

begin(model(train938)).
end(model(train938)).

begin(model(train939)).
bought(spaghetti).
shops(mary).
end(model(train939)).

begin(model(train940)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train940)).

begin(model(train941)).
bought(fish).
shops(mary).
end(model(train941)).

begin(model(train942)).
bought(fish).
shops(mary).
end(model(train942)).

begin(model(train943)).
bought(spaghetti).
shops(mary).
end(model(train943)).

begin(model(train944)).
bought(spaghetti).
shops(mary).
end(model(train944)).

begin(model(train945)).
bought(spaghetti).
shops(mary).
end(model(train945)).

begin(model(train946)).
bought(spaghetti).
shops(mary).
end(model(train946)).

begin(model(train947)).
bought(fish).
shops(mary).
end(model(train947)).

begin(model(train948)).
bought(spaghetti).
shops(mary).
end(model(train948)).

begin(model(train949)).
bought(fish).
shops(mary).
end(model(train949)).

begin(model(train950)).
bought(spaghetti).
shops(mary).
end(model(train950)).

begin(model(train951)).
bought(spaghetti).
shops(mary).
end(model(train951)).

begin(model(train952)).
bought(fish).
shops(mary).
end(model(train952)).

begin(model(train953)).
bought(fish).
shops(mary).
end(model(train953)).

begin(model(train954)).
bought(fish).
shops(mary).
end(model(train954)).

begin(model(train955)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train955)).

begin(model(train956)).
bought(fish).
shops(mary).
end(model(train956)).

begin(model(train957)).
bought(fish).
shops(mary).
end(model(train957)).

begin(model(train958)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train958)).

begin(model(train959)).
bought(fish).
shops(mary).
end(model(train959)).

begin(model(train960)).
bought(fish).
shops(mary).
end(model(train960)).

begin(model(train961)).
bought(fish).
shops(mary).
end(model(train961)).

begin(model(train962)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train962)).

begin(model(train963)).
bought(fish).
shops(mary).
end(model(train963)).

begin(model(train964)).
bought(fish).
shops(mary).
end(model(train964)).

begin(model(train965)).
bought(fish).
shops(mary).
end(model(train965)).

begin(model(train966)).
bought(fish).
shops(mary).
end(model(train966)).

begin(model(train967)).
bought(spaghetti).
shops(mary).
end(model(train967)).

begin(model(train968)).
bought(fish).
shops(mary).
end(model(train968)).

begin(model(train969)).
bought(fish).
shops(mary).
end(model(train969)).

begin(model(train970)).
bought(spaghetti).
shops(mary).
end(model(train970)).

begin(model(train971)).
bought(spaghetti).
shops(mary).
end(model(train971)).

begin(model(train972)).
bought(spaghetti).
shops(mary).
end(model(train972)).

begin(model(train973)).
bought(spaghetti).
shops(mary).
end(model(train973)).

begin(model(train974)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train974)).

begin(model(train975)).
bought(fish).
shops(mary).
end(model(train975)).

begin(model(train976)).
bought(fish).
shops(mary).
end(model(train976)).

begin(model(train977)).
bought(fish).
shops(mary).
end(model(train977)).

begin(model(train978)).
end(model(train978)).

begin(model(train979)).
bought(fish).
shops(mary).
end(model(train979)).

begin(model(train980)).
bought(fish).
shops(mary).
end(model(train980)).

begin(model(train981)).
bought(spaghetti).
shops(mary).
end(model(train981)).

begin(model(train982)).
bought(fish).
shops(mary).
end(model(train982)).

begin(model(train983)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(train983)).

begin(model(train984)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train984)).

begin(model(train985)).
bought(spaghetti).
shops(mary).
end(model(train985)).

begin(model(train986)).
bought(fish).
shops(mary).
end(model(train986)).

begin(model(train987)).
bought(fish).
shops(mary).
end(model(train987)).

begin(model(train988)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(train988)).

begin(model(train989)).
bought(spaghetti).
shops(john).
end(model(train989)).

begin(model(train990)).
bought(fish).
shops(mary).
end(model(train990)).

begin(model(train991)).
bought(fish).
shops(mary).
end(model(train991)).

begin(model(train992)).
bought(spaghetti).
shops(mary).
end(model(train992)).

begin(model(train993)).
bought(fish).
shops(mary).
end(model(train993)).

begin(model(train994)).
bought(spaghetti).
shops(mary).
end(model(train994)).

begin(model(train995)).
bought(fish).
shops(mary).
end(model(train995)).

begin(model(train996)).
bought(fish).
shops(mary).
end(model(train996)).

begin(model(train997)).
bought(fish).
shops(mary).
end(model(train997)).

begin(model(train998)).
bought(fish).
shops(mary).
end(model(train998)).

begin(model(train999)).
bought(fish).
shops(mary).
end(model(train999)).

begin(model(train1000)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(train1000)).

%rand(7372,21245,12212) 
begin(model(1)).
bought(fish).
shops(mary).
end(model(1)).

begin(model(2)).
bought(fish).
shops(mary).
end(model(2)).

begin(model(3)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3)).

begin(model(4)).
bought(fish).
shops(mary).
end(model(4)).

begin(model(5)).
bought(spaghetti).
shops(mary).
end(model(5)).

begin(model(6)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6)).

begin(model(7)).
bought(fish).
shops(mary).
end(model(7)).

begin(model(8)).
bought(fish).
shops(mary).
end(model(8)).

begin(model(9)).
end(model(9)).

begin(model(10)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(10)).

begin(model(11)).
bought(fish).
shops(mary).
end(model(11)).

begin(model(12)).
bought(spaghetti).
shops(mary).
end(model(12)).

begin(model(13)).
bought(fish).
shops(mary).
end(model(13)).

begin(model(14)).
bought(fish).
shops(mary).
end(model(14)).

begin(model(15)).
bought(fish).
shops(mary).
end(model(15)).

begin(model(16)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(16)).

begin(model(17)).
bought(fish).
shops(mary).
end(model(17)).

begin(model(18)).
bought(fish).
shops(mary).
end(model(18)).

begin(model(19)).
bought(fish).
shops(mary).
end(model(19)).

begin(model(20)).
end(model(20)).

begin(model(21)).
bought(spaghetti).
shops(mary).
end(model(21)).

begin(model(22)).
bought(fish).
shops(mary).
end(model(22)).

begin(model(23)).
bought(fish).
shops(mary).
end(model(23)).

begin(model(24)).
bought(spaghetti).
shops(mary).
end(model(24)).

begin(model(25)).
bought(fish).
shops(mary).
end(model(25)).

begin(model(26)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(26)).

begin(model(27)).
bought(spaghetti).
shops(mary).
end(model(27)).

begin(model(28)).
bought(spaghetti).
shops(mary).
end(model(28)).

begin(model(29)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(29)).

begin(model(30)).
bought(fish).
shops(mary).
end(model(30)).

begin(model(31)).
bought(spaghetti).
shops(john).
end(model(31)).

begin(model(32)).
bought(fish).
shops(mary).
end(model(32)).

begin(model(33)).
bought(spaghetti).
shops(mary).
end(model(33)).

begin(model(34)).
bought(spaghetti).
shops(mary).
end(model(34)).

begin(model(35)).
bought(fish).
shops(mary).
end(model(35)).

begin(model(36)).
bought(fish).
shops(mary).
end(model(36)).

begin(model(37)).
bought(spaghetti).
shops(mary).
end(model(37)).

begin(model(38)).
bought(fish).
shops(mary).
end(model(38)).

begin(model(39)).
bought(fish).
shops(mary).
end(model(39)).

begin(model(40)).
bought(fish).
shops(mary).
end(model(40)).

begin(model(41)).
bought(spaghetti).
shops(john).
end(model(41)).

begin(model(42)).
bought(spaghetti).
shops(mary).
end(model(42)).

begin(model(43)).
bought(spaghetti).
shops(mary).
end(model(43)).

begin(model(44)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(44)).

begin(model(45)).
bought(fish).
shops(mary).
end(model(45)).

begin(model(46)).
bought(fish).
shops(mary).
end(model(46)).

begin(model(47)).
bought(spaghetti).
shops(mary).
end(model(47)).

begin(model(48)).
bought(fish).
shops(mary).
end(model(48)).

begin(model(49)).
bought(fish).
shops(mary).
end(model(49)).

begin(model(50)).
bought(fish).
shops(mary).
end(model(50)).

begin(model(51)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(51)).

begin(model(52)).
end(model(52)).

begin(model(53)).
bought(spaghetti).
shops(mary).
end(model(53)).

begin(model(54)).
bought(fish).
shops(mary).
end(model(54)).

begin(model(55)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(55)).

begin(model(56)).
bought(spaghetti).
shops(mary).
end(model(56)).

begin(model(57)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(57)).

begin(model(58)).
bought(fish).
shops(mary).
end(model(58)).

begin(model(59)).
bought(spaghetti).
shops(mary).
end(model(59)).

begin(model(60)).
bought(fish).
shops(mary).
end(model(60)).

begin(model(61)).
bought(fish).
shops(mary).
end(model(61)).

begin(model(62)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(62)).

begin(model(63)).
bought(fish).
shops(mary).
end(model(63)).

begin(model(64)).
bought(fish).
shops(mary).
end(model(64)).

begin(model(65)).
bought(spaghetti).
shops(mary).
end(model(65)).

begin(model(66)).
bought(spaghetti).
shops(mary).
end(model(66)).

begin(model(67)).
bought(fish).
shops(mary).
end(model(67)).

begin(model(68)).
bought(fish).
shops(mary).
end(model(68)).

begin(model(69)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(69)).

begin(model(70)).
bought(spaghetti).
shops(mary).
end(model(70)).

begin(model(71)).
bought(fish).
shops(mary).
end(model(71)).

begin(model(72)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(72)).

begin(model(73)).
bought(fish).
shops(mary).
end(model(73)).

begin(model(74)).
bought(spaghetti).
shops(mary).
end(model(74)).

begin(model(75)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(75)).

begin(model(76)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(76)).

begin(model(77)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(77)).

begin(model(78)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(78)).

begin(model(79)).
bought(spaghetti).
shops(mary).
end(model(79)).

begin(model(80)).
bought(fish).
shops(mary).
end(model(80)).

begin(model(81)).
bought(spaghetti).
shops(mary).
end(model(81)).

begin(model(82)).
bought(fish).
shops(mary).
end(model(82)).

begin(model(83)).
bought(fish).
shops(mary).
end(model(83)).

begin(model(84)).
bought(fish).
shops(mary).
end(model(84)).

begin(model(85)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(85)).

begin(model(86)).
bought(fish).
shops(mary).
end(model(86)).

begin(model(87)).
bought(spaghetti).
shops(mary).
end(model(87)).

begin(model(88)).
bought(fish).
shops(mary).
end(model(88)).

begin(model(89)).
bought(spaghetti).
shops(mary).
end(model(89)).

begin(model(90)).
end(model(90)).

begin(model(91)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(91)).

begin(model(92)).
bought(spaghetti).
shops(mary).
end(model(92)).

begin(model(93)).
bought(fish).
shops(mary).
end(model(93)).

begin(model(94)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(94)).

begin(model(95)).
bought(fish).
shops(mary).
end(model(95)).

begin(model(96)).
end(model(96)).

begin(model(97)).
bought(fish).
shops(mary).
end(model(97)).

begin(model(98)).
bought(fish).
shops(mary).
end(model(98)).

begin(model(99)).
bought(fish).
shops(mary).
end(model(99)).

begin(model(100)).
bought(fish).
shops(mary).
end(model(100)).

begin(model(101)).
bought(fish).
shops(mary).
end(model(101)).

begin(model(102)).
bought(fish).
shops(mary).
end(model(102)).

begin(model(103)).
bought(fish).
shops(mary).
end(model(103)).

begin(model(104)).
bought(spaghetti).
shops(mary).
end(model(104)).

begin(model(105)).
bought(fish).
shops(mary).
end(model(105)).

begin(model(106)).
bought(spaghetti).
shops(mary).
end(model(106)).

begin(model(107)).
end(model(107)).

begin(model(108)).
bought(fish).
shops(mary).
end(model(108)).

begin(model(109)).
bought(spaghetti).
shops(mary).
end(model(109)).

begin(model(110)).
bought(spaghetti).
shops(mary).
end(model(110)).

begin(model(111)).
bought(fish).
shops(mary).
end(model(111)).

begin(model(112)).
bought(fish).
shops(mary).
end(model(112)).

begin(model(113)).
bought(fish).
shops(mary).
end(model(113)).

begin(model(114)).
bought(fish).
shops(mary).
end(model(114)).

begin(model(115)).
end(model(115)).

begin(model(116)).
bought(fish).
shops(mary).
end(model(116)).

begin(model(117)).
bought(fish).
shops(mary).
end(model(117)).

begin(model(118)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(118)).

begin(model(119)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(119)).

begin(model(120)).
bought(fish).
shops(mary).
end(model(120)).

begin(model(121)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(121)).

begin(model(122)).
end(model(122)).

begin(model(123)).
bought(fish).
shops(mary).
end(model(123)).

begin(model(124)).
bought(fish).
shops(mary).
end(model(124)).

begin(model(125)).
bought(spaghetti).
shops(mary).
end(model(125)).

begin(model(126)).
bought(fish).
shops(mary).
end(model(126)).

begin(model(127)).
bought(fish).
shops(mary).
end(model(127)).

begin(model(128)).
bought(fish).
shops(mary).
end(model(128)).

begin(model(129)).
bought(spaghetti).
shops(mary).
end(model(129)).

begin(model(130)).
bought(fish).
shops(mary).
end(model(130)).

begin(model(131)).
end(model(131)).

begin(model(132)).
bought(spaghetti).
shops(mary).
end(model(132)).

begin(model(133)).
bought(fish).
shops(mary).
end(model(133)).

begin(model(134)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(134)).

begin(model(135)).
bought(fish).
shops(mary).
end(model(135)).

begin(model(136)).
bought(fish).
shops(mary).
end(model(136)).

begin(model(137)).
bought(fish).
shops(mary).
end(model(137)).

begin(model(138)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(138)).

begin(model(139)).
end(model(139)).

begin(model(140)).
bought(fish).
shops(mary).
end(model(140)).

begin(model(141)).
bought(spaghetti).
shops(mary).
end(model(141)).

begin(model(142)).
bought(fish).
shops(mary).
end(model(142)).

begin(model(143)).
end(model(143)).

begin(model(144)).
bought(fish).
shops(mary).
end(model(144)).

begin(model(145)).
bought(fish).
shops(mary).
end(model(145)).

begin(model(146)).
bought(spaghetti).
shops(mary).
end(model(146)).

begin(model(147)).
bought(fish).
shops(mary).
end(model(147)).

begin(model(148)).
bought(spaghetti).
shops(mary).
end(model(148)).

begin(model(149)).
bought(fish).
shops(mary).
end(model(149)).

begin(model(150)).
bought(spaghetti).
shops(mary).
end(model(150)).

begin(model(151)).
bought(spaghetti).
shops(mary).
end(model(151)).

begin(model(152)).
bought(spaghetti).
shops(mary).
end(model(152)).

begin(model(153)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(153)).

begin(model(154)).
bought(fish).
shops(mary).
end(model(154)).

begin(model(155)).
bought(fish).
shops(mary).
end(model(155)).

begin(model(156)).
bought(fish).
shops(mary).
end(model(156)).

begin(model(157)).
bought(spaghetti).
shops(mary).
end(model(157)).

begin(model(158)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(158)).

begin(model(159)).
bought(fish).
shops(mary).
end(model(159)).

begin(model(160)).
bought(steak).
shops(john).
end(model(160)).

begin(model(161)).
end(model(161)).

begin(model(162)).
bought(fish).
shops(mary).
end(model(162)).

begin(model(163)).
bought(fish).
shops(mary).
end(model(163)).

begin(model(164)).
bought(spaghetti).
shops(mary).
end(model(164)).

begin(model(165)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(165)).

begin(model(166)).
bought(fish).
shops(mary).
end(model(166)).

begin(model(167)).
bought(spaghetti).
shops(mary).
end(model(167)).

begin(model(168)).
bought(fish).
shops(mary).
end(model(168)).

begin(model(169)).
bought(spaghetti).
shops(mary).
end(model(169)).

begin(model(170)).
bought(spaghetti).
shops(mary).
end(model(170)).

begin(model(171)).
bought(fish).
shops(mary).
end(model(171)).

begin(model(172)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(172)).

begin(model(173)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(173)).

begin(model(174)).
bought(spaghetti).
shops(mary).
end(model(174)).

begin(model(175)).
bought(fish).
shops(mary).
end(model(175)).

begin(model(176)).
bought(fish).
shops(mary).
end(model(176)).

begin(model(177)).
bought(fish).
shops(mary).
end(model(177)).

begin(model(178)).
end(model(178)).

begin(model(179)).
bought(spaghetti).
shops(mary).
end(model(179)).

begin(model(180)).
bought(spaghetti).
shops(mary).
end(model(180)).

begin(model(181)).
bought(fish).
shops(mary).
end(model(181)).

begin(model(182)).
end(model(182)).

begin(model(183)).
bought(spaghetti).
shops(mary).
end(model(183)).

begin(model(184)).
end(model(184)).

begin(model(185)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(185)).

begin(model(186)).
bought(spaghetti).
shops(mary).
end(model(186)).

begin(model(187)).
bought(spaghetti).
shops(john).
end(model(187)).

begin(model(188)).
bought(fish).
shops(mary).
end(model(188)).

begin(model(189)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(189)).

begin(model(190)).
bought(spaghetti).
shops(mary).
end(model(190)).

begin(model(191)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(191)).

begin(model(192)).
bought(spaghetti).
shops(mary).
end(model(192)).

begin(model(193)).
bought(fish).
shops(mary).
end(model(193)).

begin(model(194)).
bought(fish).
shops(mary).
end(model(194)).

begin(model(195)).
bought(fish).
shops(mary).
end(model(195)).

begin(model(196)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(196)).

begin(model(197)).
bought(spaghetti).
shops(john).
end(model(197)).

begin(model(198)).
end(model(198)).

begin(model(199)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(199)).

begin(model(200)).
bought(spaghetti).
shops(mary).
end(model(200)).

begin(model(201)).
bought(spaghetti).
shops(mary).
end(model(201)).

begin(model(202)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(202)).

begin(model(203)).
bought(spaghetti).
shops(mary).
end(model(203)).

begin(model(204)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(204)).

begin(model(205)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(205)).

begin(model(206)).
bought(fish).
shops(mary).
end(model(206)).

begin(model(207)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(207)).

begin(model(208)).
bought(fish).
shops(mary).
end(model(208)).

begin(model(209)).
bought(fish).
shops(mary).
end(model(209)).

begin(model(210)).
bought(fish).
shops(mary).
end(model(210)).

begin(model(211)).
bought(fish).
shops(mary).
end(model(211)).

begin(model(212)).
bought(fish).
shops(mary).
end(model(212)).

begin(model(213)).
bought(spaghetti).
shops(mary).
end(model(213)).

begin(model(214)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(214)).

begin(model(215)).
end(model(215)).

begin(model(216)).
bought(fish).
shops(mary).
end(model(216)).

begin(model(217)).
bought(fish).
shops(mary).
end(model(217)).

begin(model(218)).
bought(fish).
shops(mary).
end(model(218)).

begin(model(219)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(219)).

begin(model(220)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(220)).

begin(model(221)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(221)).

begin(model(222)).
bought(fish).
shops(mary).
end(model(222)).

begin(model(223)).
end(model(223)).

begin(model(224)).
bought(spaghetti).
shops(mary).
end(model(224)).

begin(model(225)).
bought(spaghetti).
shops(mary).
end(model(225)).

begin(model(226)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(226)).

begin(model(227)).
bought(fish).
shops(mary).
end(model(227)).

begin(model(228)).
bought(spaghetti).
shops(mary).
end(model(228)).

begin(model(229)).
bought(fish).
shops(mary).
end(model(229)).

begin(model(230)).
bought(spaghetti).
shops(mary).
end(model(230)).

begin(model(231)).
bought(fish).
shops(mary).
end(model(231)).

begin(model(232)).
bought(fish).
shops(mary).
end(model(232)).

begin(model(233)).
bought(spaghetti).
shops(mary).
end(model(233)).

begin(model(234)).
bought(spaghetti).
shops(mary).
end(model(234)).

begin(model(235)).
bought(fish).
shops(mary).
end(model(235)).

begin(model(236)).
bought(fish).
shops(mary).
end(model(236)).

begin(model(237)).
bought(fish).
shops(mary).
end(model(237)).

begin(model(238)).
bought(fish).
shops(mary).
end(model(238)).

begin(model(239)).
bought(spaghetti).
shops(mary).
end(model(239)).

begin(model(240)).
bought(spaghetti).
shops(john).
end(model(240)).

begin(model(241)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(241)).

begin(model(242)).
end(model(242)).

begin(model(243)).
bought(fish).
shops(mary).
end(model(243)).

begin(model(244)).
bought(spaghetti).
shops(mary).
end(model(244)).

begin(model(245)).
bought(fish).
shops(mary).
end(model(245)).

begin(model(246)).
bought(fish).
shops(mary).
end(model(246)).

begin(model(247)).
bought(fish).
shops(mary).
end(model(247)).

begin(model(248)).
bought(fish).
shops(mary).
end(model(248)).

begin(model(249)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(249)).

begin(model(250)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(250)).

begin(model(251)).
end(model(251)).

begin(model(252)).
bought(spaghetti).
shops(mary).
end(model(252)).

begin(model(253)).
bought(spaghetti).
shops(mary).
end(model(253)).

begin(model(254)).
bought(fish).
shops(mary).
end(model(254)).

begin(model(255)).
bought(fish).
shops(mary).
end(model(255)).

begin(model(256)).
bought(spaghetti).
shops(mary).
end(model(256)).

begin(model(257)).
bought(fish).
shops(mary).
end(model(257)).

begin(model(258)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(258)).

begin(model(259)).
bought(fish).
shops(mary).
end(model(259)).

begin(model(260)).
bought(fish).
shops(mary).
end(model(260)).

begin(model(261)).
bought(fish).
shops(mary).
end(model(261)).

begin(model(262)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(262)).

begin(model(263)).
bought(fish).
shops(mary).
end(model(263)).

begin(model(264)).
bought(fish).
shops(mary).
end(model(264)).

begin(model(265)).
bought(fish).
shops(mary).
end(model(265)).

begin(model(266)).
bought(spaghetti).
shops(mary).
end(model(266)).

begin(model(267)).
bought(spaghetti).
shops(mary).
end(model(267)).

begin(model(268)).
bought(fish).
shops(mary).
end(model(268)).

begin(model(269)).
bought(fish).
shops(mary).
end(model(269)).

begin(model(270)).
bought(fish).
shops(mary).
end(model(270)).

begin(model(271)).
end(model(271)).

begin(model(272)).
bought(fish).
shops(mary).
end(model(272)).

begin(model(273)).
bought(fish).
shops(mary).
end(model(273)).

begin(model(274)).
end(model(274)).

begin(model(275)).
bought(spaghetti).
shops(mary).
end(model(275)).

begin(model(276)).
bought(fish).
shops(mary).
end(model(276)).

begin(model(277)).
bought(spaghetti).
shops(mary).
end(model(277)).

begin(model(278)).
bought(fish).
shops(mary).
end(model(278)).

begin(model(279)).
bought(spaghetti).
shops(mary).
end(model(279)).

begin(model(280)).
bought(fish).
shops(mary).
end(model(280)).

begin(model(281)).
bought(fish).
shops(mary).
end(model(281)).

begin(model(282)).
bought(spaghetti).
shops(mary).
end(model(282)).

begin(model(283)).
bought(spaghetti).
shops(mary).
end(model(283)).

begin(model(284)).
bought(fish).
shops(mary).
end(model(284)).

begin(model(285)).
bought(fish).
shops(mary).
end(model(285)).

begin(model(286)).
bought(fish).
shops(mary).
end(model(286)).

begin(model(287)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(287)).

begin(model(288)).
bought(spaghetti).
shops(mary).
end(model(288)).

begin(model(289)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(289)).

begin(model(290)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(290)).

begin(model(291)).
bought(steak).
shops(john).
end(model(291)).

begin(model(292)).
bought(spaghetti).
shops(mary).
end(model(292)).

begin(model(293)).
bought(spaghetti).
shops(mary).
end(model(293)).

begin(model(294)).
bought(fish).
shops(mary).
end(model(294)).

begin(model(295)).
end(model(295)).

begin(model(296)).
end(model(296)).

begin(model(297)).
bought(spaghetti).
shops(mary).
end(model(297)).

begin(model(298)).
bought(spaghetti).
shops(mary).
end(model(298)).

begin(model(299)).
bought(spaghetti).
shops(mary).
end(model(299)).

begin(model(300)).
bought(spaghetti).
shops(mary).
end(model(300)).

begin(model(301)).
bought(fish).
shops(mary).
end(model(301)).

begin(model(302)).
bought(fish).
shops(mary).
end(model(302)).

begin(model(303)).
end(model(303)).

begin(model(304)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(304)).

begin(model(305)).
end(model(305)).

begin(model(306)).
bought(fish).
shops(mary).
end(model(306)).

begin(model(307)).
bought(fish).
shops(mary).
end(model(307)).

begin(model(308)).
bought(fish).
shops(mary).
end(model(308)).

begin(model(309)).
bought(fish).
shops(mary).
end(model(309)).

begin(model(310)).
bought(fish).
shops(mary).
end(model(310)).

begin(model(311)).
bought(spaghetti).
shops(mary).
end(model(311)).

begin(model(312)).
bought(fish).
shops(mary).
end(model(312)).

begin(model(313)).
bought(fish).
shops(mary).
end(model(313)).

begin(model(314)).
bought(spaghetti).
shops(mary).
end(model(314)).

begin(model(315)).
bought(spaghetti).
shops(john).
end(model(315)).

begin(model(316)).
bought(fish).
shops(mary).
end(model(316)).

begin(model(317)).
bought(fish).
shops(mary).
end(model(317)).

begin(model(318)).
bought(fish).
shops(mary).
end(model(318)).

begin(model(319)).
bought(fish).
shops(mary).
end(model(319)).

begin(model(320)).
bought(spaghetti).
shops(mary).
end(model(320)).

begin(model(321)).
bought(fish).
shops(mary).
end(model(321)).

begin(model(322)).
bought(spaghetti).
shops(mary).
end(model(322)).

begin(model(323)).
bought(spaghetti).
shops(mary).
end(model(323)).

begin(model(324)).
bought(fish).
shops(mary).
end(model(324)).

begin(model(325)).
bought(fish).
shops(mary).
end(model(325)).

begin(model(326)).
end(model(326)).

begin(model(327)).
bought(spaghetti).
shops(mary).
end(model(327)).

begin(model(328)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(328)).

begin(model(329)).
bought(fish).
shops(mary).
end(model(329)).

begin(model(330)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(330)).

begin(model(331)).
bought(fish).
shops(mary).
end(model(331)).

begin(model(332)).
bought(fish).
shops(mary).
end(model(332)).

begin(model(333)).
bought(spaghetti).
shops(mary).
end(model(333)).

begin(model(334)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(334)).

begin(model(335)).
bought(fish).
shops(mary).
end(model(335)).

begin(model(336)).
bought(fish).
shops(mary).
end(model(336)).

begin(model(337)).
bought(fish).
shops(mary).
end(model(337)).

begin(model(338)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(338)).

begin(model(339)).
bought(fish).
shops(mary).
end(model(339)).

begin(model(340)).
bought(fish).
shops(mary).
end(model(340)).

begin(model(341)).
bought(fish).
shops(mary).
end(model(341)).

begin(model(342)).
bought(fish).
shops(mary).
end(model(342)).

begin(model(343)).
bought(fish).
shops(mary).
end(model(343)).

begin(model(344)).
bought(fish).
shops(mary).
end(model(344)).

begin(model(345)).
end(model(345)).

begin(model(346)).
end(model(346)).

begin(model(347)).
bought(spaghetti).
shops(mary).
end(model(347)).

begin(model(348)).
bought(fish).
shops(mary).
end(model(348)).

begin(model(349)).
bought(spaghetti).
shops(mary).
end(model(349)).

begin(model(350)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(350)).

begin(model(351)).
bought(fish).
shops(mary).
end(model(351)).

begin(model(352)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(352)).

begin(model(353)).
end(model(353)).

begin(model(354)).
bought(fish).
shops(mary).
end(model(354)).

begin(model(355)).
bought(fish).
shops(mary).
end(model(355)).

begin(model(356)).
bought(spaghetti).
shops(mary).
end(model(356)).

begin(model(357)).
bought(fish).
shops(mary).
end(model(357)).

begin(model(358)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(358)).

begin(model(359)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(359)).

begin(model(360)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(360)).

begin(model(361)).
bought(fish).
shops(mary).
end(model(361)).

begin(model(362)).
bought(fish).
shops(mary).
end(model(362)).

begin(model(363)).
bought(spaghetti).
shops(mary).
end(model(363)).

begin(model(364)).
bought(fish).
shops(mary).
end(model(364)).

begin(model(365)).
bought(spaghetti).
shops(john).
end(model(365)).

begin(model(366)).
bought(fish).
shops(mary).
end(model(366)).

begin(model(367)).
bought(fish).
shops(mary).
end(model(367)).

begin(model(368)).
bought(spaghetti).
shops(mary).
end(model(368)).

begin(model(369)).
end(model(369)).

begin(model(370)).
bought(fish).
shops(mary).
end(model(370)).

begin(model(371)).
bought(spaghetti).
shops(mary).
end(model(371)).

begin(model(372)).
bought(spaghetti).
shops(mary).
end(model(372)).

begin(model(373)).
bought(fish).
shops(mary).
end(model(373)).

begin(model(374)).
bought(fish).
shops(mary).
end(model(374)).

begin(model(375)).
bought(spaghetti).
shops(mary).
end(model(375)).

begin(model(376)).
bought(fish).
shops(mary).
end(model(376)).

begin(model(377)).
bought(fish).
shops(mary).
end(model(377)).

begin(model(378)).
bought(fish).
shops(mary).
end(model(378)).

begin(model(379)).
bought(fish).
shops(mary).
end(model(379)).

begin(model(380)).
bought(fish).
shops(mary).
end(model(380)).

begin(model(381)).
bought(fish).
shops(mary).
end(model(381)).

begin(model(382)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(382)).

begin(model(383)).
bought(spaghetti).
shops(mary).
end(model(383)).

begin(model(384)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(384)).

begin(model(385)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(385)).

begin(model(386)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(386)).

begin(model(387)).
bought(fish).
shops(mary).
end(model(387)).

begin(model(388)).
bought(fish).
shops(mary).
end(model(388)).

begin(model(389)).
bought(spaghetti).
shops(mary).
end(model(389)).

begin(model(390)).
bought(spaghetti).
shops(mary).
end(model(390)).

begin(model(391)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(391)).

begin(model(392)).
bought(fish).
shops(mary).
end(model(392)).

begin(model(393)).
bought(fish).
shops(mary).
end(model(393)).

begin(model(394)).
bought(fish).
shops(mary).
end(model(394)).

begin(model(395)).
bought(fish).
shops(mary).
end(model(395)).

begin(model(396)).
bought(fish).
shops(mary).
end(model(396)).

begin(model(397)).
bought(fish).
shops(mary).
end(model(397)).

begin(model(398)).
bought(fish).
shops(mary).
end(model(398)).

begin(model(399)).
bought(fish).
shops(mary).
end(model(399)).

begin(model(400)).
bought(spaghetti).
shops(mary).
end(model(400)).

begin(model(401)).
bought(spaghetti).
shops(mary).
end(model(401)).

begin(model(402)).
bought(fish).
shops(mary).
end(model(402)).

begin(model(403)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(403)).

begin(model(404)).
bought(spaghetti).
shops(mary).
end(model(404)).

begin(model(405)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(405)).

begin(model(406)).
bought(fish).
shops(mary).
end(model(406)).

begin(model(407)).
bought(fish).
shops(mary).
end(model(407)).

begin(model(408)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(408)).

begin(model(409)).
bought(fish).
shops(mary).
end(model(409)).

begin(model(410)).
bought(fish).
shops(mary).
end(model(410)).

begin(model(411)).
bought(fish).
shops(mary).
end(model(411)).

begin(model(412)).
bought(spaghetti).
shops(mary).
end(model(412)).

begin(model(413)).
bought(spaghetti).
shops(mary).
end(model(413)).

begin(model(414)).
bought(spaghetti).
shops(mary).
end(model(414)).

begin(model(415)).
bought(fish).
shops(mary).
end(model(415)).

begin(model(416)).
bought(spaghetti).
shops(mary).
end(model(416)).

begin(model(417)).
bought(fish).
shops(mary).
end(model(417)).

begin(model(418)).
bought(fish).
shops(mary).
end(model(418)).

begin(model(419)).
bought(fish).
shops(mary).
end(model(419)).

begin(model(420)).
end(model(420)).

begin(model(421)).
bought(fish).
shops(mary).
end(model(421)).

begin(model(422)).
bought(fish).
shops(mary).
end(model(422)).

begin(model(423)).
bought(fish).
shops(mary).
end(model(423)).

begin(model(424)).
bought(fish).
shops(mary).
end(model(424)).

begin(model(425)).
bought(fish).
shops(mary).
end(model(425)).

begin(model(426)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(426)).

begin(model(427)).
bought(fish).
shops(mary).
end(model(427)).

begin(model(428)).
bought(spaghetti).
shops(mary).
end(model(428)).

begin(model(429)).
bought(spaghetti).
shops(mary).
end(model(429)).

begin(model(430)).
end(model(430)).

begin(model(431)).
bought(fish).
shops(mary).
end(model(431)).

begin(model(432)).
bought(spaghetti).
shops(mary).
end(model(432)).

begin(model(433)).
bought(fish).
shops(mary).
end(model(433)).

begin(model(434)).
bought(spaghetti).
shops(mary).
end(model(434)).

begin(model(435)).
bought(spaghetti).
shops(mary).
end(model(435)).

begin(model(436)).
end(model(436)).

begin(model(437)).
bought(fish).
shops(mary).
end(model(437)).

begin(model(438)).
bought(fish).
shops(mary).
end(model(438)).

begin(model(439)).
bought(fish).
shops(mary).
end(model(439)).

begin(model(440)).
bought(spaghetti).
shops(mary).
end(model(440)).

begin(model(441)).
bought(fish).
shops(mary).
end(model(441)).

begin(model(442)).
bought(spaghetti).
shops(mary).
end(model(442)).

begin(model(443)).
bought(fish).
shops(mary).
end(model(443)).

begin(model(444)).
bought(fish).
shops(mary).
end(model(444)).

begin(model(445)).
bought(spaghetti).
shops(mary).
end(model(445)).

begin(model(446)).
bought(fish).
shops(mary).
end(model(446)).

begin(model(447)).
end(model(447)).

begin(model(448)).
bought(spaghetti).
shops(mary).
end(model(448)).

begin(model(449)).
bought(spaghetti).
shops(mary).
end(model(449)).

begin(model(450)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(450)).

begin(model(451)).
bought(spaghetti).
shops(mary).
end(model(451)).

begin(model(452)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(452)).

begin(model(453)).
bought(fish).
shops(mary).
end(model(453)).

begin(model(454)).
bought(spaghetti).
shops(mary).
end(model(454)).

begin(model(455)).
end(model(455)).

begin(model(456)).
bought(fish).
shops(mary).
end(model(456)).

begin(model(457)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(457)).

begin(model(458)).
bought(fish).
shops(mary).
end(model(458)).

begin(model(459)).
bought(fish).
shops(mary).
end(model(459)).

begin(model(460)).
bought(fish).
shops(mary).
end(model(460)).

begin(model(461)).
bought(spaghetti).
shops(mary).
end(model(461)).

begin(model(462)).
bought(fish).
shops(mary).
end(model(462)).

begin(model(463)).
bought(fish).
shops(mary).
end(model(463)).

begin(model(464)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(464)).

begin(model(465)).
bought(fish).
shops(mary).
end(model(465)).

begin(model(466)).
bought(fish).
shops(mary).
end(model(466)).

begin(model(467)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(467)).

begin(model(468)).
bought(spaghetti).
shops(mary).
end(model(468)).

begin(model(469)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(469)).

begin(model(470)).
bought(fish).
shops(mary).
end(model(470)).

begin(model(471)).
bought(fish).
shops(mary).
end(model(471)).

begin(model(472)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(472)).

begin(model(473)).
bought(fish).
shops(mary).
end(model(473)).

begin(model(474)).
bought(fish).
shops(mary).
end(model(474)).

begin(model(475)).
bought(fish).
shops(mary).
end(model(475)).

begin(model(476)).
bought(fish).
shops(mary).
end(model(476)).

begin(model(477)).
bought(fish).
shops(mary).
end(model(477)).

begin(model(478)).
bought(fish).
shops(mary).
end(model(478)).

begin(model(479)).
bought(spaghetti).
shops(mary).
end(model(479)).

begin(model(480)).
end(model(480)).

begin(model(481)).
bought(fish).
shops(mary).
end(model(481)).

begin(model(482)).
bought(spaghetti).
shops(mary).
end(model(482)).

begin(model(483)).
bought(fish).
shops(mary).
end(model(483)).

begin(model(484)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(484)).

begin(model(485)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(485)).

begin(model(486)).
bought(spaghetti).
shops(mary).
end(model(486)).

begin(model(487)).
bought(spaghetti).
shops(mary).
end(model(487)).

begin(model(488)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(488)).

begin(model(489)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(489)).

begin(model(490)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(490)).

begin(model(491)).
bought(spaghetti).
shops(mary).
end(model(491)).

begin(model(492)).
bought(spaghetti).
shops(mary).
end(model(492)).

begin(model(493)).
bought(fish).
shops(mary).
end(model(493)).

begin(model(494)).
bought(fish).
shops(mary).
end(model(494)).

begin(model(495)).
bought(fish).
shops(mary).
end(model(495)).

begin(model(496)).
bought(spaghetti).
shops(mary).
end(model(496)).

begin(model(497)).
bought(fish).
shops(mary).
end(model(497)).

begin(model(498)).
bought(spaghetti).
shops(mary).
end(model(498)).

begin(model(499)).
bought(fish).
shops(mary).
end(model(499)).

begin(model(500)).
bought(fish).
shops(mary).
end(model(500)).

begin(model(501)).
bought(fish).
shops(mary).
end(model(501)).

begin(model(502)).
bought(spaghetti).
shops(john).
end(model(502)).

begin(model(503)).
bought(spaghetti).
shops(mary).
end(model(503)).

begin(model(504)).
bought(fish).
shops(mary).
end(model(504)).

begin(model(505)).
bought(spaghetti).
shops(mary).
end(model(505)).

begin(model(506)).
bought(fish).
shops(mary).
end(model(506)).

begin(model(507)).
bought(fish).
shops(mary).
end(model(507)).

begin(model(508)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(508)).

begin(model(509)).
bought(fish).
shops(mary).
end(model(509)).

begin(model(510)).
bought(fish).
shops(mary).
end(model(510)).

begin(model(511)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(511)).

begin(model(512)).
bought(fish).
shops(mary).
end(model(512)).

begin(model(513)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(513)).

begin(model(514)).
bought(fish).
shops(mary).
end(model(514)).

begin(model(515)).
bought(fish).
shops(mary).
end(model(515)).

begin(model(516)).
bought(fish).
shops(mary).
end(model(516)).

begin(model(517)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(517)).

begin(model(518)).
bought(fish).
shops(mary).
end(model(518)).

begin(model(519)).
bought(fish).
shops(mary).
end(model(519)).

begin(model(520)).
bought(fish).
shops(mary).
end(model(520)).

begin(model(521)).
bought(fish).
shops(mary).
end(model(521)).

begin(model(522)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(522)).

begin(model(523)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(523)).

begin(model(524)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(524)).

begin(model(525)).
bought(fish).
shops(mary).
end(model(525)).

begin(model(526)).
bought(fish).
shops(mary).
end(model(526)).

begin(model(527)).
bought(fish).
shops(mary).
end(model(527)).

begin(model(528)).
bought(fish).
shops(mary).
end(model(528)).

begin(model(529)).
bought(spaghetti).
shops(mary).
end(model(529)).

begin(model(530)).
bought(fish).
shops(mary).
end(model(530)).

begin(model(531)).
bought(fish).
shops(mary).
end(model(531)).

begin(model(532)).
bought(fish).
shops(mary).
end(model(532)).

begin(model(533)).
bought(spaghetti).
shops(mary).
end(model(533)).

begin(model(534)).
end(model(534)).

begin(model(535)).
bought(fish).
shops(mary).
end(model(535)).

begin(model(536)).
bought(spaghetti).
shops(mary).
end(model(536)).

begin(model(537)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(537)).

begin(model(538)).
bought(fish).
shops(mary).
end(model(538)).

begin(model(539)).
bought(fish).
shops(mary).
end(model(539)).

begin(model(540)).
bought(fish).
shops(mary).
end(model(540)).

begin(model(541)).
bought(fish).
shops(mary).
end(model(541)).

begin(model(542)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(542)).

begin(model(543)).
bought(fish).
shops(mary).
end(model(543)).

begin(model(544)).
bought(fish).
shops(mary).
end(model(544)).

begin(model(545)).
bought(spaghetti).
shops(mary).
end(model(545)).

begin(model(546)).
bought(fish).
shops(mary).
end(model(546)).

begin(model(547)).
bought(fish).
shops(mary).
end(model(547)).

begin(model(548)).
bought(spaghetti).
shops(mary).
end(model(548)).

begin(model(549)).
bought(fish).
shops(mary).
end(model(549)).

begin(model(550)).
bought(fish).
shops(mary).
end(model(550)).

begin(model(551)).
bought(fish).
shops(mary).
end(model(551)).

begin(model(552)).
end(model(552)).

begin(model(553)).
bought(spaghetti).
shops(mary).
end(model(553)).

begin(model(554)).
bought(fish).
shops(mary).
end(model(554)).

begin(model(555)).
bought(fish).
shops(mary).
end(model(555)).

begin(model(556)).
bought(fish).
shops(mary).
end(model(556)).

begin(model(557)).
bought(spaghetti).
shops(mary).
end(model(557)).

begin(model(558)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(558)).

begin(model(559)).
bought(fish).
shops(mary).
end(model(559)).

begin(model(560)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(560)).

begin(model(561)).
bought(fish).
shops(mary).
end(model(561)).

begin(model(562)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(562)).

begin(model(563)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(563)).

begin(model(564)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(564)).

begin(model(565)).
bought(fish).
shops(mary).
end(model(565)).

begin(model(566)).
bought(fish).
shops(mary).
end(model(566)).

begin(model(567)).
bought(fish).
shops(mary).
end(model(567)).

begin(model(568)).
bought(spaghetti).
shops(mary).
end(model(568)).

begin(model(569)).
bought(fish).
shops(mary).
end(model(569)).

begin(model(570)).
bought(fish).
shops(mary).
end(model(570)).

begin(model(571)).
end(model(571)).

begin(model(572)).
bought(spaghetti).
shops(mary).
end(model(572)).

begin(model(573)).
bought(fish).
shops(mary).
end(model(573)).

begin(model(574)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(574)).

begin(model(575)).
bought(fish).
shops(mary).
end(model(575)).

begin(model(576)).
bought(fish).
shops(mary).
end(model(576)).

begin(model(577)).
bought(fish).
shops(mary).
end(model(577)).

begin(model(578)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(578)).

begin(model(579)).
bought(spaghetti).
shops(mary).
end(model(579)).

begin(model(580)).
end(model(580)).

begin(model(581)).
bought(fish).
shops(mary).
end(model(581)).

begin(model(582)).
end(model(582)).

begin(model(583)).
bought(spaghetti).
shops(mary).
end(model(583)).

begin(model(584)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(584)).

begin(model(585)).
bought(fish).
shops(mary).
end(model(585)).

begin(model(586)).
bought(spaghetti).
shops(mary).
end(model(586)).

begin(model(587)).
bought(fish).
shops(mary).
end(model(587)).

begin(model(588)).
bought(fish).
shops(mary).
end(model(588)).

begin(model(589)).
bought(fish).
shops(mary).
end(model(589)).

begin(model(590)).
bought(fish).
shops(mary).
end(model(590)).

begin(model(591)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(591)).

begin(model(592)).
end(model(592)).

begin(model(593)).
bought(fish).
shops(mary).
end(model(593)).

begin(model(594)).
bought(fish).
shops(mary).
end(model(594)).

begin(model(595)).
end(model(595)).

begin(model(596)).
bought(fish).
shops(mary).
end(model(596)).

begin(model(597)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(597)).

begin(model(598)).
bought(fish).
shops(mary).
end(model(598)).

begin(model(599)).
bought(fish).
shops(mary).
end(model(599)).

begin(model(600)).
bought(fish).
shops(mary).
end(model(600)).

begin(model(601)).
bought(spaghetti).
shops(mary).
end(model(601)).

begin(model(602)).
bought(fish).
shops(mary).
end(model(602)).

begin(model(603)).
bought(spaghetti).
shops(mary).
end(model(603)).

begin(model(604)).
bought(fish).
shops(mary).
end(model(604)).

begin(model(605)).
end(model(605)).

begin(model(606)).
bought(fish).
shops(mary).
end(model(606)).

begin(model(607)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(607)).

begin(model(608)).
bought(spaghetti).
shops(mary).
end(model(608)).

begin(model(609)).
bought(spaghetti).
shops(mary).
end(model(609)).

begin(model(610)).
bought(fish).
shops(mary).
end(model(610)).

begin(model(611)).
bought(fish).
shops(mary).
end(model(611)).

begin(model(612)).
bought(spaghetti).
shops(mary).
end(model(612)).

begin(model(613)).
end(model(613)).

begin(model(614)).
bought(fish).
shops(mary).
end(model(614)).

begin(model(615)).
bought(fish).
shops(mary).
end(model(615)).

begin(model(616)).
bought(spaghetti).
shops(mary).
end(model(616)).

begin(model(617)).
bought(spaghetti).
shops(mary).
end(model(617)).

begin(model(618)).
bought(fish).
shops(mary).
end(model(618)).

begin(model(619)).
bought(spaghetti).
shops(mary).
end(model(619)).

begin(model(620)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(620)).

begin(model(621)).
bought(fish).
shops(mary).
end(model(621)).

begin(model(622)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(622)).

begin(model(623)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(623)).

begin(model(624)).
end(model(624)).

begin(model(625)).
end(model(625)).

begin(model(626)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(626)).

begin(model(627)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(627)).

begin(model(628)).
bought(fish).
shops(mary).
end(model(628)).

begin(model(629)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(629)).

begin(model(630)).
bought(fish).
shops(mary).
end(model(630)).

begin(model(631)).
bought(spaghetti).
shops(mary).
end(model(631)).

begin(model(632)).
bought(fish).
shops(mary).
end(model(632)).

begin(model(633)).
bought(spaghetti).
shops(mary).
end(model(633)).

begin(model(634)).
bought(fish).
shops(mary).
end(model(634)).

begin(model(635)).
bought(fish).
shops(mary).
end(model(635)).

begin(model(636)).
bought(fish).
shops(mary).
end(model(636)).

begin(model(637)).
bought(fish).
shops(mary).
end(model(637)).

begin(model(638)).
bought(fish).
shops(mary).
end(model(638)).

begin(model(639)).
bought(spaghetti).
shops(mary).
end(model(639)).

begin(model(640)).
bought(fish).
shops(mary).
end(model(640)).

begin(model(641)).
bought(spaghetti).
shops(mary).
end(model(641)).

begin(model(642)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(642)).

begin(model(643)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(643)).

begin(model(644)).
bought(fish).
shops(mary).
end(model(644)).

begin(model(645)).
bought(fish).
shops(mary).
end(model(645)).

begin(model(646)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(646)).

begin(model(647)).
bought(fish).
shops(mary).
end(model(647)).

begin(model(648)).
bought(fish).
shops(mary).
end(model(648)).

begin(model(649)).
bought(fish).
shops(mary).
end(model(649)).

begin(model(650)).
bought(spaghetti).
shops(mary).
end(model(650)).

begin(model(651)).
bought(spaghetti).
shops(mary).
end(model(651)).

begin(model(652)).
bought(spaghetti).
shops(mary).
end(model(652)).

begin(model(653)).
bought(spaghetti).
shops(mary).
end(model(653)).

begin(model(654)).
bought(fish).
shops(mary).
end(model(654)).

begin(model(655)).
bought(fish).
shops(mary).
end(model(655)).

begin(model(656)).
bought(fish).
shops(mary).
end(model(656)).

begin(model(657)).
bought(fish).
shops(mary).
end(model(657)).

begin(model(658)).
bought(steak).
shops(john).
end(model(658)).

begin(model(659)).
bought(fish).
shops(mary).
end(model(659)).

begin(model(660)).
end(model(660)).

begin(model(661)).
bought(spaghetti).
shops(mary).
end(model(661)).

begin(model(662)).
bought(fish).
shops(mary).
end(model(662)).

begin(model(663)).
bought(fish).
shops(mary).
end(model(663)).

begin(model(664)).
bought(fish).
shops(mary).
end(model(664)).

begin(model(665)).
end(model(665)).

begin(model(666)).
bought(fish).
shops(mary).
end(model(666)).

begin(model(667)).
bought(fish).
shops(mary).
end(model(667)).

begin(model(668)).
bought(fish).
shops(mary).
end(model(668)).

begin(model(669)).
bought(spaghetti).
shops(mary).
end(model(669)).

begin(model(670)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(670)).

begin(model(671)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(671)).

begin(model(672)).
bought(spaghetti).
shops(mary).
end(model(672)).

begin(model(673)).
bought(fish).
shops(mary).
end(model(673)).

begin(model(674)).
bought(fish).
shops(mary).
end(model(674)).

begin(model(675)).
end(model(675)).

begin(model(676)).
bought(steak).
shops(john).
end(model(676)).

begin(model(677)).
bought(fish).
shops(mary).
end(model(677)).

begin(model(678)).
bought(spaghetti).
shops(john).
end(model(678)).

begin(model(679)).
bought(fish).
shops(mary).
end(model(679)).

begin(model(680)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(680)).

begin(model(681)).
bought(spaghetti).
shops(mary).
end(model(681)).

begin(model(682)).
end(model(682)).

begin(model(683)).
bought(fish).
shops(mary).
end(model(683)).

begin(model(684)).
bought(steak).
shops(john).
end(model(684)).

begin(model(685)).
bought(fish).
shops(mary).
end(model(685)).

begin(model(686)).
bought(fish).
shops(mary).
end(model(686)).

begin(model(687)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(687)).

begin(model(688)).
bought(fish).
shops(mary).
end(model(688)).

begin(model(689)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(689)).

begin(model(690)).
bought(spaghetti).
shops(mary).
end(model(690)).

begin(model(691)).
bought(spaghetti).
shops(mary).
end(model(691)).

begin(model(692)).
bought(spaghetti).
shops(mary).
end(model(692)).

begin(model(693)).
bought(fish).
shops(mary).
end(model(693)).

begin(model(694)).
bought(fish).
shops(mary).
end(model(694)).

begin(model(695)).
bought(fish).
shops(mary).
end(model(695)).

begin(model(696)).
bought(fish).
shops(mary).
end(model(696)).

begin(model(697)).
bought(spaghetti).
shops(mary).
end(model(697)).

begin(model(698)).
bought(fish).
shops(mary).
end(model(698)).

begin(model(699)).
bought(fish).
shops(mary).
end(model(699)).

begin(model(700)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(700)).

begin(model(701)).
bought(fish).
shops(mary).
end(model(701)).

begin(model(702)).
bought(fish).
shops(mary).
end(model(702)).

begin(model(703)).
bought(fish).
shops(mary).
end(model(703)).

begin(model(704)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(704)).

begin(model(705)).
bought(spaghetti).
shops(mary).
end(model(705)).

begin(model(706)).
bought(spaghetti).
shops(mary).
end(model(706)).

begin(model(707)).
bought(steak).
shops(john).
end(model(707)).

begin(model(708)).
bought(spaghetti).
shops(mary).
end(model(708)).

begin(model(709)).
bought(spaghetti).
shops(mary).
end(model(709)).

begin(model(710)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(710)).

begin(model(711)).
bought(spaghetti).
shops(mary).
end(model(711)).

begin(model(712)).
end(model(712)).

begin(model(713)).
bought(fish).
shops(mary).
end(model(713)).

begin(model(714)).
bought(fish).
shops(mary).
end(model(714)).

begin(model(715)).
bought(fish).
shops(mary).
end(model(715)).

begin(model(716)).
bought(fish).
shops(mary).
end(model(716)).

begin(model(717)).
bought(spaghetti).
shops(mary).
end(model(717)).

begin(model(718)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(718)).

begin(model(719)).
bought(fish).
shops(mary).
end(model(719)).

begin(model(720)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(720)).

begin(model(721)).
bought(fish).
shops(mary).
end(model(721)).

begin(model(722)).
bought(spaghetti).
shops(mary).
end(model(722)).

begin(model(723)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(723)).

begin(model(724)).
bought(spaghetti).
shops(john).
end(model(724)).

begin(model(725)).
end(model(725)).

begin(model(726)).
end(model(726)).

begin(model(727)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(727)).

begin(model(728)).
bought(spaghetti).
shops(mary).
end(model(728)).

begin(model(729)).
bought(fish).
shops(mary).
end(model(729)).

begin(model(730)).
bought(fish).
shops(mary).
end(model(730)).

begin(model(731)).
bought(fish).
shops(mary).
end(model(731)).

begin(model(732)).
end(model(732)).

begin(model(733)).
bought(fish).
shops(mary).
end(model(733)).

begin(model(734)).
bought(fish).
shops(mary).
end(model(734)).

begin(model(735)).
bought(fish).
shops(mary).
end(model(735)).

begin(model(736)).
bought(spaghetti).
shops(john).
end(model(736)).

begin(model(737)).
bought(spaghetti).
shops(mary).
end(model(737)).

begin(model(738)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(738)).

begin(model(739)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(739)).

begin(model(740)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(740)).

begin(model(741)).
end(model(741)).

begin(model(742)).
bought(fish).
shops(mary).
end(model(742)).

begin(model(743)).
bought(fish).
shops(mary).
end(model(743)).

begin(model(744)).
bought(spaghetti).
shops(mary).
end(model(744)).

begin(model(745)).
bought(fish).
shops(mary).
end(model(745)).

begin(model(746)).
bought(fish).
shops(mary).
end(model(746)).

begin(model(747)).
bought(fish).
shops(mary).
end(model(747)).

begin(model(748)).
bought(fish).
shops(mary).
end(model(748)).

begin(model(749)).
bought(fish).
shops(mary).
end(model(749)).

begin(model(750)).
bought(fish).
shops(mary).
end(model(750)).

begin(model(751)).
bought(fish).
shops(mary).
end(model(751)).

begin(model(752)).
end(model(752)).

begin(model(753)).
bought(spaghetti).
shops(mary).
end(model(753)).

begin(model(754)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(754)).

begin(model(755)).
bought(fish).
shops(mary).
end(model(755)).

begin(model(756)).
bought(fish).
shops(mary).
end(model(756)).

begin(model(757)).
bought(fish).
shops(mary).
end(model(757)).

begin(model(758)).
bought(spaghetti).
shops(mary).
end(model(758)).

begin(model(759)).
bought(spaghetti).
shops(mary).
end(model(759)).

begin(model(760)).
bought(fish).
shops(mary).
end(model(760)).

begin(model(761)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(761)).

begin(model(762)).
bought(fish).
shops(mary).
end(model(762)).

begin(model(763)).
bought(fish).
shops(mary).
end(model(763)).

begin(model(764)).
end(model(764)).

begin(model(765)).
bought(fish).
shops(mary).
end(model(765)).

begin(model(766)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(766)).

begin(model(767)).
bought(fish).
shops(mary).
end(model(767)).

begin(model(768)).
bought(spaghetti).
shops(mary).
end(model(768)).

begin(model(769)).
bought(fish).
shops(mary).
end(model(769)).

begin(model(770)).
bought(spaghetti).
shops(mary).
end(model(770)).

begin(model(771)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(771)).

begin(model(772)).
bought(fish).
shops(mary).
end(model(772)).

begin(model(773)).
bought(spaghetti).
shops(mary).
end(model(773)).

begin(model(774)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(774)).

begin(model(775)).
bought(spaghetti).
shops(mary).
end(model(775)).

begin(model(776)).
bought(fish).
shops(mary).
end(model(776)).

begin(model(777)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(777)).

begin(model(778)).
bought(fish).
shops(mary).
end(model(778)).

begin(model(779)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(779)).

begin(model(780)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(780)).

begin(model(781)).
bought(fish).
shops(mary).
end(model(781)).

begin(model(782)).
bought(spaghetti).
shops(john).
end(model(782)).

begin(model(783)).
bought(fish).
shops(mary).
end(model(783)).

begin(model(784)).
bought(fish).
shops(mary).
end(model(784)).

begin(model(785)).
bought(fish).
shops(mary).
end(model(785)).

begin(model(786)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(786)).

begin(model(787)).
end(model(787)).

begin(model(788)).
bought(fish).
shops(mary).
end(model(788)).

begin(model(789)).
bought(spaghetti).
shops(mary).
end(model(789)).

begin(model(790)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(790)).

begin(model(791)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(791)).

begin(model(792)).
end(model(792)).

begin(model(793)).
bought(fish).
shops(mary).
end(model(793)).

begin(model(794)).
bought(fish).
shops(mary).
end(model(794)).

begin(model(795)).
bought(spaghetti).
shops(mary).
end(model(795)).

begin(model(796)).
bought(fish).
shops(mary).
end(model(796)).

begin(model(797)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(797)).

begin(model(798)).
bought(fish).
shops(mary).
end(model(798)).

begin(model(799)).
bought(fish).
shops(mary).
end(model(799)).

begin(model(800)).
bought(spaghetti).
shops(mary).
end(model(800)).

begin(model(801)).
bought(fish).
shops(mary).
end(model(801)).

begin(model(802)).
bought(spaghetti).
shops(mary).
end(model(802)).

begin(model(803)).
bought(spaghetti).
shops(mary).
end(model(803)).

begin(model(804)).
end(model(804)).

begin(model(805)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(805)).

begin(model(806)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(806)).

begin(model(807)).
bought(fish).
shops(mary).
end(model(807)).

begin(model(808)).
bought(spaghetti).
shops(mary).
end(model(808)).

begin(model(809)).
bought(spaghetti).
shops(mary).
end(model(809)).

begin(model(810)).
bought(spaghetti).
shops(mary).
end(model(810)).

begin(model(811)).
bought(spaghetti).
shops(mary).
end(model(811)).

begin(model(812)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(812)).

begin(model(813)).
bought(fish).
shops(mary).
end(model(813)).

begin(model(814)).
bought(fish).
shops(mary).
end(model(814)).

begin(model(815)).
bought(spaghetti).
shops(mary).
end(model(815)).

begin(model(816)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(816)).

begin(model(817)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(817)).

begin(model(818)).
bought(fish).
shops(mary).
end(model(818)).

begin(model(819)).
bought(spaghetti).
shops(mary).
end(model(819)).

begin(model(820)).
end(model(820)).

begin(model(821)).
end(model(821)).

begin(model(822)).
bought(fish).
shops(mary).
end(model(822)).

begin(model(823)).
bought(spaghetti).
shops(mary).
end(model(823)).

begin(model(824)).
bought(fish).
shops(mary).
end(model(824)).

begin(model(825)).
bought(fish).
shops(mary).
end(model(825)).

begin(model(826)).
bought(spaghetti).
shops(mary).
end(model(826)).

begin(model(827)).
bought(fish).
shops(mary).
end(model(827)).

begin(model(828)).
bought(fish).
shops(mary).
end(model(828)).

begin(model(829)).
bought(fish).
shops(mary).
end(model(829)).

begin(model(830)).
bought(spaghetti).
shops(mary).
end(model(830)).

begin(model(831)).
end(model(831)).

begin(model(832)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(832)).

begin(model(833)).
bought(fish).
shops(mary).
end(model(833)).

begin(model(834)).
bought(fish).
shops(mary).
end(model(834)).

begin(model(835)).
bought(fish).
shops(mary).
end(model(835)).

begin(model(836)).
bought(spaghetti).
shops(mary).
end(model(836)).

begin(model(837)).
bought(fish).
shops(mary).
end(model(837)).

begin(model(838)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(838)).

begin(model(839)).
bought(fish).
shops(mary).
end(model(839)).

begin(model(840)).
bought(fish).
shops(mary).
end(model(840)).

begin(model(841)).
end(model(841)).

begin(model(842)).
bought(fish).
shops(mary).
end(model(842)).

begin(model(843)).
bought(fish).
shops(mary).
end(model(843)).

begin(model(844)).
end(model(844)).

begin(model(845)).
bought(spaghetti).
shops(mary).
end(model(845)).

begin(model(846)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(846)).

begin(model(847)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(847)).

begin(model(848)).
bought(spaghetti).
shops(mary).
end(model(848)).

begin(model(849)).
bought(spaghetti).
shops(mary).
end(model(849)).

begin(model(850)).
bought(fish).
shops(mary).
end(model(850)).

begin(model(851)).
bought(fish).
shops(mary).
end(model(851)).

begin(model(852)).
bought(fish).
shops(mary).
end(model(852)).

begin(model(853)).
bought(spaghetti).
shops(mary).
end(model(853)).

begin(model(854)).
bought(fish).
shops(mary).
end(model(854)).

begin(model(855)).
bought(fish).
shops(mary).
end(model(855)).

begin(model(856)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(856)).

begin(model(857)).
bought(fish).
shops(mary).
end(model(857)).

begin(model(858)).
bought(spaghetti).
shops(mary).
end(model(858)).

begin(model(859)).
bought(fish).
shops(mary).
end(model(859)).

begin(model(860)).
bought(fish).
shops(mary).
end(model(860)).

begin(model(861)).
bought(fish).
shops(mary).
end(model(861)).

begin(model(862)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(862)).

begin(model(863)).
bought(fish).
shops(mary).
end(model(863)).

begin(model(864)).
bought(spaghetti).
shops(mary).
end(model(864)).

begin(model(865)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(865)).

begin(model(866)).
bought(fish).
shops(mary).
end(model(866)).

begin(model(867)).
bought(spaghetti).
shops(mary).
end(model(867)).

begin(model(868)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(868)).

begin(model(869)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(869)).

begin(model(870)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(870)).

begin(model(871)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(871)).

begin(model(872)).
bought(fish).
shops(mary).
end(model(872)).

begin(model(873)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(873)).

begin(model(874)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(874)).

begin(model(875)).
bought(fish).
shops(mary).
end(model(875)).

begin(model(876)).
bought(fish).
shops(mary).
end(model(876)).

begin(model(877)).
bought(fish).
shops(mary).
end(model(877)).

begin(model(878)).
bought(spaghetti).
shops(mary).
end(model(878)).

begin(model(879)).
bought(spaghetti).
shops(mary).
end(model(879)).

begin(model(880)).
bought(spaghetti).
shops(mary).
end(model(880)).

begin(model(881)).
bought(fish).
shops(mary).
end(model(881)).

begin(model(882)).
bought(fish).
shops(mary).
end(model(882)).

begin(model(883)).
bought(fish).
shops(mary).
end(model(883)).

begin(model(884)).
bought(fish).
shops(mary).
end(model(884)).

begin(model(885)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(885)).

begin(model(886)).
bought(spaghetti).
shops(john).
end(model(886)).

begin(model(887)).
bought(spaghetti).
shops(mary).
end(model(887)).

begin(model(888)).
bought(spaghetti).
shops(mary).
end(model(888)).

begin(model(889)).
bought(spaghetti).
shops(mary).
end(model(889)).

begin(model(890)).
bought(fish).
shops(mary).
end(model(890)).

begin(model(891)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(891)).

begin(model(892)).
bought(spaghetti).
shops(mary).
end(model(892)).

begin(model(893)).
bought(fish).
shops(mary).
end(model(893)).

begin(model(894)).
bought(fish).
shops(mary).
end(model(894)).

begin(model(895)).
bought(fish).
shops(mary).
end(model(895)).

begin(model(896)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(896)).

begin(model(897)).
bought(spaghetti).
shops(mary).
end(model(897)).

begin(model(898)).
bought(fish).
shops(mary).
end(model(898)).

begin(model(899)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(899)).

begin(model(900)).
bought(spaghetti).
shops(mary).
end(model(900)).

begin(model(901)).
bought(fish).
shops(mary).
end(model(901)).

begin(model(902)).
bought(fish).
shops(mary).
end(model(902)).

begin(model(903)).
bought(fish).
shops(mary).
end(model(903)).

begin(model(904)).
bought(fish).
shops(mary).
end(model(904)).

begin(model(905)).
end(model(905)).

begin(model(906)).
bought(spaghetti).
shops(john).
end(model(906)).

begin(model(907)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(907)).

begin(model(908)).
bought(fish).
shops(mary).
end(model(908)).

begin(model(909)).
bought(fish).
shops(mary).
end(model(909)).

begin(model(910)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(910)).

begin(model(911)).
bought(spaghetti).
shops(mary).
end(model(911)).

begin(model(912)).
bought(fish).
shops(mary).
end(model(912)).

begin(model(913)).
bought(spaghetti).
shops(mary).
end(model(913)).

begin(model(914)).
bought(spaghetti).
shops(mary).
end(model(914)).

begin(model(915)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(915)).

begin(model(916)).
bought(fish).
shops(mary).
end(model(916)).

begin(model(917)).
bought(spaghetti).
shops(mary).
end(model(917)).

begin(model(918)).
bought(fish).
shops(mary).
end(model(918)).

begin(model(919)).
bought(fish).
shops(mary).
end(model(919)).

begin(model(920)).
end(model(920)).

begin(model(921)).
bought(fish).
shops(mary).
end(model(921)).

begin(model(922)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(922)).

begin(model(923)).
bought(fish).
shops(mary).
end(model(923)).

begin(model(924)).
bought(spaghetti).
shops(mary).
end(model(924)).

begin(model(925)).
bought(fish).
shops(mary).
end(model(925)).

begin(model(926)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(926)).

begin(model(927)).
bought(spaghetti).
shops(mary).
end(model(927)).

begin(model(928)).
bought(fish).
shops(mary).
end(model(928)).

begin(model(929)).
bought(fish).
shops(mary).
end(model(929)).

begin(model(930)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(930)).

begin(model(931)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(931)).

begin(model(932)).
bought(spaghetti).
shops(mary).
end(model(932)).

begin(model(933)).
bought(fish).
shops(mary).
end(model(933)).

begin(model(934)).
bought(fish).
shops(mary).
end(model(934)).

begin(model(935)).
bought(fish).
shops(mary).
end(model(935)).

begin(model(936)).
end(model(936)).

begin(model(937)).
end(model(937)).

begin(model(938)).
bought(fish).
shops(mary).
end(model(938)).

begin(model(939)).
bought(fish).
shops(mary).
end(model(939)).

begin(model(940)).
bought(spaghetti).
shops(mary).
end(model(940)).

begin(model(941)).
bought(spaghetti).
shops(john).
end(model(941)).

begin(model(942)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(942)).

begin(model(943)).
bought(fish).
shops(mary).
end(model(943)).

begin(model(944)).
bought(fish).
shops(mary).
end(model(944)).

begin(model(945)).
bought(fish).
shops(mary).
end(model(945)).

begin(model(946)).
bought(fish).
shops(mary).
end(model(946)).

begin(model(947)).
bought(fish).
shops(mary).
end(model(947)).

begin(model(948)).
bought(spaghetti).
shops(mary).
end(model(948)).

begin(model(949)).
end(model(949)).

begin(model(950)).
bought(spaghetti).
shops(mary).
end(model(950)).

begin(model(951)).
end(model(951)).

begin(model(952)).
bought(spaghetti).
shops(mary).
end(model(952)).

begin(model(953)).
bought(fish).
shops(mary).
end(model(953)).

begin(model(954)).
bought(spaghetti).
shops(mary).
end(model(954)).

begin(model(955)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(955)).

begin(model(956)).
bought(spaghetti).
shops(mary).
end(model(956)).

begin(model(957)).
bought(fish).
shops(mary).
end(model(957)).

begin(model(958)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(958)).

begin(model(959)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(959)).

begin(model(960)).
bought(fish).
shops(mary).
end(model(960)).

begin(model(961)).
bought(fish).
shops(mary).
end(model(961)).

begin(model(962)).
bought(fish).
shops(mary).
end(model(962)).

begin(model(963)).
bought(fish).
shops(mary).
end(model(963)).

begin(model(964)).
bought(fish).
shops(mary).
end(model(964)).

begin(model(965)).
bought(spaghetti).
shops(mary).
end(model(965)).

begin(model(966)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(966)).

begin(model(967)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(967)).

begin(model(968)).
bought(fish).
shops(mary).
end(model(968)).

begin(model(969)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(969)).

begin(model(970)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(970)).

begin(model(971)).
bought(fish).
shops(mary).
end(model(971)).

begin(model(972)).
bought(fish).
shops(mary).
end(model(972)).

begin(model(973)).
bought(fish).
shops(mary).
end(model(973)).

begin(model(974)).
bought(fish).
shops(mary).
end(model(974)).

begin(model(975)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(975)).

begin(model(976)).
bought(spaghetti).
shops(mary).
end(model(976)).

begin(model(977)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(977)).

begin(model(978)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(978)).

begin(model(979)).
bought(spaghetti).
shops(mary).
end(model(979)).

begin(model(980)).
bought(spaghetti).
shops(mary).
end(model(980)).

begin(model(981)).
bought(fish).
shops(mary).
end(model(981)).

begin(model(982)).
bought(spaghetti).
shops(mary).
end(model(982)).

begin(model(983)).
bought(fish).
shops(mary).
end(model(983)).

begin(model(984)).
bought(fish).
shops(mary).
end(model(984)).

begin(model(985)).
bought(fish).
shops(mary).
end(model(985)).

begin(model(986)).
bought(fish).
shops(mary).
end(model(986)).

begin(model(987)).
bought(fish).
shops(mary).
end(model(987)).

begin(model(988)).
bought(fish).
shops(mary).
end(model(988)).

begin(model(989)).
bought(fish).
shops(mary).
end(model(989)).

begin(model(990)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(990)).

begin(model(991)).
bought(spaghetti).
shops(mary).
end(model(991)).

begin(model(992)).
bought(spaghetti).
shops(john).
end(model(992)).

begin(model(993)).
end(model(993)).

begin(model(994)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(994)).

begin(model(995)).
bought(spaghetti).
shops(john).
end(model(995)).

begin(model(996)).
bought(spaghetti).
shops(mary).
end(model(996)).

begin(model(997)).
bought(spaghetti).
shops(mary).
end(model(997)).

begin(model(998)).
bought(fish).
shops(mary).
end(model(998)).

begin(model(999)).
bought(fish).
shops(mary).
end(model(999)).

begin(model(1000)).
bought(fish).
shops(mary).
end(model(1000)).

begin(model(1001)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1001)).

begin(model(1002)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1002)).

begin(model(1003)).
bought(fish).
shops(mary).
end(model(1003)).

begin(model(1004)).
bought(spaghetti).
shops(mary).
end(model(1004)).

begin(model(1005)).
bought(spaghetti).
shops(mary).
end(model(1005)).

begin(model(1006)).
end(model(1006)).

begin(model(1007)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1007)).

begin(model(1008)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1008)).

begin(model(1009)).
bought(fish).
shops(mary).
end(model(1009)).

begin(model(1010)).
bought(fish).
shops(mary).
end(model(1010)).

begin(model(1011)).
bought(fish).
shops(mary).
end(model(1011)).

begin(model(1012)).
bought(fish).
shops(mary).
end(model(1012)).

begin(model(1013)).
bought(fish).
shops(mary).
end(model(1013)).

begin(model(1014)).
bought(fish).
shops(mary).
end(model(1014)).

begin(model(1015)).
bought(fish).
shops(mary).
end(model(1015)).

begin(model(1016)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1016)).

begin(model(1017)).
bought(fish).
shops(mary).
end(model(1017)).

begin(model(1018)).
bought(fish).
shops(mary).
end(model(1018)).

begin(model(1019)).
bought(spaghetti).
shops(mary).
end(model(1019)).

begin(model(1020)).
bought(fish).
shops(mary).
end(model(1020)).

begin(model(1021)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1021)).

begin(model(1022)).
bought(spaghetti).
shops(mary).
end(model(1022)).

begin(model(1023)).
bought(fish).
shops(mary).
end(model(1023)).

begin(model(1024)).
bought(fish).
shops(mary).
end(model(1024)).

begin(model(1025)).
bought(fish).
shops(mary).
end(model(1025)).

begin(model(1026)).
bought(fish).
shops(mary).
end(model(1026)).

begin(model(1027)).
bought(spaghetti).
shops(mary).
end(model(1027)).

begin(model(1028)).
bought(fish).
shops(mary).
end(model(1028)).

begin(model(1029)).
end(model(1029)).

begin(model(1030)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1030)).

begin(model(1031)).
bought(fish).
shops(mary).
end(model(1031)).

begin(model(1032)).
bought(fish).
shops(mary).
end(model(1032)).

begin(model(1033)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1033)).

begin(model(1034)).
bought(fish).
shops(mary).
end(model(1034)).

begin(model(1035)).
bought(spaghetti).
shops(mary).
end(model(1035)).

begin(model(1036)).
bought(spaghetti).
shops(mary).
end(model(1036)).

begin(model(1037)).
end(model(1037)).

begin(model(1038)).
bought(fish).
shops(mary).
end(model(1038)).

begin(model(1039)).
bought(fish).
shops(mary).
end(model(1039)).

begin(model(1040)).
end(model(1040)).

begin(model(1041)).
bought(fish).
shops(mary).
end(model(1041)).

begin(model(1042)).
bought(fish).
shops(mary).
end(model(1042)).

begin(model(1043)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1043)).

begin(model(1044)).
end(model(1044)).

begin(model(1045)).
end(model(1045)).

begin(model(1046)).
bought(spaghetti).
shops(mary).
end(model(1046)).

begin(model(1047)).
bought(fish).
shops(mary).
end(model(1047)).

begin(model(1048)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1048)).

begin(model(1049)).
bought(spaghetti).
shops(mary).
end(model(1049)).

begin(model(1050)).
bought(fish).
shops(mary).
end(model(1050)).

begin(model(1051)).
bought(spaghetti).
shops(mary).
end(model(1051)).

begin(model(1052)).
bought(fish).
shops(mary).
end(model(1052)).

begin(model(1053)).
bought(fish).
shops(mary).
end(model(1053)).

begin(model(1054)).
bought(fish).
shops(mary).
end(model(1054)).

begin(model(1055)).
bought(spaghetti).
shops(mary).
end(model(1055)).

begin(model(1056)).
end(model(1056)).

begin(model(1057)).
bought(fish).
shops(mary).
end(model(1057)).

begin(model(1058)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1058)).

begin(model(1059)).
bought(spaghetti).
shops(mary).
end(model(1059)).

begin(model(1060)).
bought(fish).
shops(mary).
end(model(1060)).

begin(model(1061)).
bought(spaghetti).
shops(mary).
end(model(1061)).

begin(model(1062)).
bought(spaghetti).
shops(mary).
end(model(1062)).

begin(model(1063)).
bought(fish).
shops(mary).
end(model(1063)).

begin(model(1064)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1064)).

begin(model(1065)).
bought(fish).
shops(mary).
end(model(1065)).

begin(model(1066)).
bought(spaghetti).
shops(mary).
end(model(1066)).

begin(model(1067)).
bought(fish).
shops(mary).
end(model(1067)).

begin(model(1068)).
bought(fish).
shops(mary).
end(model(1068)).

begin(model(1069)).
end(model(1069)).

begin(model(1070)).
bought(spaghetti).
shops(mary).
end(model(1070)).

begin(model(1071)).
bought(spaghetti).
shops(mary).
end(model(1071)).

begin(model(1072)).
bought(fish).
shops(mary).
end(model(1072)).

begin(model(1073)).
bought(fish).
shops(mary).
end(model(1073)).

begin(model(1074)).
bought(spaghetti).
shops(mary).
end(model(1074)).

begin(model(1075)).
bought(fish).
shops(mary).
end(model(1075)).

begin(model(1076)).
bought(spaghetti).
shops(mary).
end(model(1076)).

begin(model(1077)).
bought(fish).
shops(mary).
end(model(1077)).

begin(model(1078)).
bought(fish).
shops(mary).
end(model(1078)).

begin(model(1079)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1079)).

begin(model(1080)).
bought(fish).
shops(mary).
end(model(1080)).

begin(model(1081)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1081)).

begin(model(1082)).
bought(fish).
shops(mary).
end(model(1082)).

begin(model(1083)).
bought(fish).
shops(mary).
end(model(1083)).

begin(model(1084)).
bought(fish).
shops(mary).
end(model(1084)).

begin(model(1085)).
bought(fish).
shops(mary).
end(model(1085)).

begin(model(1086)).
bought(spaghetti).
shops(mary).
end(model(1086)).

begin(model(1087)).
end(model(1087)).

begin(model(1088)).
bought(fish).
shops(mary).
end(model(1088)).

begin(model(1089)).
bought(fish).
shops(mary).
end(model(1089)).

begin(model(1090)).
bought(spaghetti).
shops(mary).
end(model(1090)).

begin(model(1091)).
bought(spaghetti).
shops(john).
end(model(1091)).

begin(model(1092)).
bought(fish).
shops(mary).
end(model(1092)).

begin(model(1093)).
bought(fish).
shops(mary).
end(model(1093)).

begin(model(1094)).
end(model(1094)).

begin(model(1095)).
bought(fish).
shops(mary).
end(model(1095)).

begin(model(1096)).
bought(spaghetti).
shops(mary).
end(model(1096)).

begin(model(1097)).
bought(fish).
shops(mary).
end(model(1097)).

begin(model(1098)).
bought(fish).
shops(mary).
end(model(1098)).

begin(model(1099)).
bought(spaghetti).
shops(mary).
end(model(1099)).

begin(model(1100)).
end(model(1100)).

begin(model(1101)).
bought(fish).
shops(mary).
end(model(1101)).

begin(model(1102)).
bought(spaghetti).
shops(mary).
end(model(1102)).

begin(model(1103)).
bought(spaghetti).
shops(mary).
end(model(1103)).

begin(model(1104)).
bought(fish).
shops(mary).
end(model(1104)).

begin(model(1105)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1105)).

begin(model(1106)).
bought(fish).
shops(mary).
end(model(1106)).

begin(model(1107)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1107)).

begin(model(1108)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1108)).

begin(model(1109)).
bought(spaghetti).
shops(mary).
end(model(1109)).

begin(model(1110)).
end(model(1110)).

begin(model(1111)).
bought(spaghetti).
shops(mary).
end(model(1111)).

begin(model(1112)).
bought(fish).
shops(mary).
end(model(1112)).

begin(model(1113)).
bought(spaghetti).
shops(mary).
end(model(1113)).

begin(model(1114)).
bought(spaghetti).
shops(mary).
end(model(1114)).

begin(model(1115)).
bought(fish).
shops(mary).
end(model(1115)).

begin(model(1116)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1116)).

begin(model(1117)).
bought(spaghetti).
shops(mary).
end(model(1117)).

begin(model(1118)).
bought(fish).
shops(mary).
end(model(1118)).

begin(model(1119)).
end(model(1119)).

begin(model(1120)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1120)).

begin(model(1121)).
bought(fish).
shops(mary).
end(model(1121)).

begin(model(1122)).
bought(spaghetti).
shops(mary).
end(model(1122)).

begin(model(1123)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1123)).

begin(model(1124)).
bought(fish).
shops(mary).
end(model(1124)).

begin(model(1125)).
bought(fish).
shops(mary).
end(model(1125)).

begin(model(1126)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1126)).

begin(model(1127)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1127)).

begin(model(1128)).
bought(fish).
shops(mary).
end(model(1128)).

begin(model(1129)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1129)).

begin(model(1130)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1130)).

begin(model(1131)).
bought(spaghetti).
shops(mary).
end(model(1131)).

begin(model(1132)).
bought(fish).
shops(mary).
end(model(1132)).

begin(model(1133)).
end(model(1133)).

begin(model(1134)).
bought(spaghetti).
shops(mary).
end(model(1134)).

begin(model(1135)).
bought(fish).
shops(mary).
end(model(1135)).

begin(model(1136)).
bought(fish).
shops(mary).
end(model(1136)).

begin(model(1137)).
bought(fish).
shops(mary).
end(model(1137)).

begin(model(1138)).
bought(spaghetti).
shops(mary).
end(model(1138)).

begin(model(1139)).
bought(fish).
shops(mary).
end(model(1139)).

begin(model(1140)).
bought(spaghetti).
shops(mary).
end(model(1140)).

begin(model(1141)).
bought(fish).
shops(mary).
end(model(1141)).

begin(model(1142)).
bought(fish).
shops(mary).
end(model(1142)).

begin(model(1143)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1143)).

begin(model(1144)).
bought(fish).
shops(mary).
end(model(1144)).

begin(model(1145)).
bought(spaghetti).
shops(mary).
end(model(1145)).

begin(model(1146)).
end(model(1146)).

begin(model(1147)).
bought(fish).
shops(mary).
end(model(1147)).

begin(model(1148)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1148)).

begin(model(1149)).
bought(fish).
shops(mary).
end(model(1149)).

begin(model(1150)).
bought(spaghetti).
shops(mary).
end(model(1150)).

begin(model(1151)).
bought(spaghetti).
shops(mary).
end(model(1151)).

begin(model(1152)).
bought(fish).
shops(mary).
end(model(1152)).

begin(model(1153)).
bought(fish).
shops(mary).
end(model(1153)).

begin(model(1154)).
end(model(1154)).

begin(model(1155)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1155)).

begin(model(1156)).
bought(fish).
shops(mary).
end(model(1156)).

begin(model(1157)).
bought(fish).
shops(mary).
end(model(1157)).

begin(model(1158)).
bought(fish).
shops(mary).
end(model(1158)).

begin(model(1159)).
bought(spaghetti).
shops(mary).
end(model(1159)).

begin(model(1160)).
bought(fish).
shops(mary).
end(model(1160)).

begin(model(1161)).
bought(fish).
shops(mary).
end(model(1161)).

begin(model(1162)).
bought(fish).
shops(mary).
end(model(1162)).

begin(model(1163)).
bought(fish).
shops(mary).
end(model(1163)).

begin(model(1164)).
end(model(1164)).

begin(model(1165)).
bought(fish).
shops(mary).
end(model(1165)).

begin(model(1166)).
bought(fish).
shops(mary).
end(model(1166)).

begin(model(1167)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1167)).

begin(model(1168)).
bought(fish).
shops(mary).
end(model(1168)).

begin(model(1169)).
bought(fish).
shops(mary).
end(model(1169)).

begin(model(1170)).
bought(fish).
shops(mary).
end(model(1170)).

begin(model(1171)).
bought(fish).
shops(mary).
end(model(1171)).

begin(model(1172)).
bought(spaghetti).
shops(mary).
end(model(1172)).

begin(model(1173)).
bought(spaghetti).
shops(mary).
end(model(1173)).

begin(model(1174)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1174)).

begin(model(1175)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1175)).

begin(model(1176)).
bought(fish).
shops(mary).
end(model(1176)).

begin(model(1177)).
bought(spaghetti).
shops(mary).
end(model(1177)).

begin(model(1178)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1178)).

begin(model(1179)).
bought(fish).
shops(mary).
end(model(1179)).

begin(model(1180)).
bought(fish).
shops(mary).
end(model(1180)).

begin(model(1181)).
bought(fish).
shops(mary).
end(model(1181)).

begin(model(1182)).
bought(fish).
shops(mary).
end(model(1182)).

begin(model(1183)).
bought(fish).
shops(mary).
end(model(1183)).

begin(model(1184)).
bought(fish).
shops(mary).
end(model(1184)).

begin(model(1185)).
bought(fish).
shops(mary).
end(model(1185)).

begin(model(1186)).
bought(fish).
shops(mary).
end(model(1186)).

begin(model(1187)).
bought(fish).
shops(mary).
end(model(1187)).

begin(model(1188)).
bought(fish).
shops(mary).
end(model(1188)).

begin(model(1189)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1189)).

begin(model(1190)).
bought(fish).
shops(mary).
end(model(1190)).

begin(model(1191)).
bought(fish).
shops(mary).
end(model(1191)).

begin(model(1192)).
end(model(1192)).

begin(model(1193)).
bought(fish).
shops(mary).
end(model(1193)).

begin(model(1194)).
end(model(1194)).

begin(model(1195)).
end(model(1195)).

begin(model(1196)).
bought(spaghetti).
shops(mary).
end(model(1196)).

begin(model(1197)).
bought(fish).
shops(mary).
end(model(1197)).

begin(model(1198)).
bought(steak).
shops(john).
end(model(1198)).

begin(model(1199)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1199)).

begin(model(1200)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1200)).

begin(model(1201)).
end(model(1201)).

begin(model(1202)).
bought(spaghetti).
shops(mary).
end(model(1202)).

begin(model(1203)).
bought(fish).
shops(mary).
end(model(1203)).

begin(model(1204)).
bought(fish).
shops(mary).
end(model(1204)).

begin(model(1205)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1205)).

begin(model(1206)).
bought(fish).
shops(mary).
end(model(1206)).

begin(model(1207)).
bought(fish).
shops(mary).
end(model(1207)).

begin(model(1208)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1208)).

begin(model(1209)).
end(model(1209)).

begin(model(1210)).
bought(fish).
shops(mary).
end(model(1210)).

begin(model(1211)).
bought(fish).
shops(mary).
end(model(1211)).

begin(model(1212)).
end(model(1212)).

begin(model(1213)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1213)).

begin(model(1214)).
bought(fish).
shops(mary).
end(model(1214)).

begin(model(1215)).
bought(spaghetti).
shops(mary).
end(model(1215)).

begin(model(1216)).
bought(fish).
shops(mary).
end(model(1216)).

begin(model(1217)).
bought(spaghetti).
shops(mary).
end(model(1217)).

begin(model(1218)).
bought(spaghetti).
shops(mary).
end(model(1218)).

begin(model(1219)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1219)).

begin(model(1220)).
bought(spaghetti).
shops(mary).
end(model(1220)).

begin(model(1221)).
bought(fish).
shops(mary).
end(model(1221)).

begin(model(1222)).
bought(fish).
shops(mary).
end(model(1222)).

begin(model(1223)).
end(model(1223)).

begin(model(1224)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1224)).

begin(model(1225)).
bought(fish).
shops(mary).
end(model(1225)).

begin(model(1226)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1226)).

begin(model(1227)).
bought(fish).
shops(mary).
end(model(1227)).

begin(model(1228)).
bought(fish).
shops(mary).
end(model(1228)).

begin(model(1229)).
bought(fish).
shops(mary).
end(model(1229)).

begin(model(1230)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1230)).

begin(model(1231)).
bought(fish).
shops(mary).
end(model(1231)).

begin(model(1232)).
bought(spaghetti).
shops(mary).
end(model(1232)).

begin(model(1233)).
end(model(1233)).

begin(model(1234)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1234)).

begin(model(1235)).
bought(spaghetti).
shops(mary).
end(model(1235)).

begin(model(1236)).
bought(fish).
shops(mary).
end(model(1236)).

begin(model(1237)).
bought(fish).
shops(mary).
end(model(1237)).

begin(model(1238)).
bought(fish).
shops(mary).
end(model(1238)).

begin(model(1239)).
bought(fish).
shops(mary).
end(model(1239)).

begin(model(1240)).
bought(fish).
shops(mary).
end(model(1240)).

begin(model(1241)).
bought(spaghetti).
shops(mary).
end(model(1241)).

begin(model(1242)).
bought(fish).
shops(mary).
end(model(1242)).

begin(model(1243)).
bought(fish).
shops(mary).
end(model(1243)).

begin(model(1244)).
end(model(1244)).

begin(model(1245)).
bought(fish).
shops(mary).
end(model(1245)).

begin(model(1246)).
bought(fish).
shops(mary).
end(model(1246)).

begin(model(1247)).
bought(fish).
shops(mary).
end(model(1247)).

begin(model(1248)).
bought(spaghetti).
shops(mary).
end(model(1248)).

begin(model(1249)).
bought(fish).
shops(mary).
end(model(1249)).

begin(model(1250)).
bought(spaghetti).
shops(mary).
end(model(1250)).

begin(model(1251)).
bought(fish).
shops(mary).
end(model(1251)).

begin(model(1252)).
bought(fish).
shops(mary).
end(model(1252)).

begin(model(1253)).
bought(spaghetti).
shops(mary).
end(model(1253)).

begin(model(1254)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1254)).

begin(model(1255)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1255)).

begin(model(1256)).
bought(spaghetti).
shops(mary).
end(model(1256)).

begin(model(1257)).
end(model(1257)).

begin(model(1258)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1258)).

begin(model(1259)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1259)).

begin(model(1260)).
bought(fish).
shops(mary).
end(model(1260)).

begin(model(1261)).
bought(fish).
shops(mary).
end(model(1261)).

begin(model(1262)).
bought(spaghetti).
shops(mary).
end(model(1262)).

begin(model(1263)).
end(model(1263)).

begin(model(1264)).
bought(fish).
shops(mary).
end(model(1264)).

begin(model(1265)).
bought(spaghetti).
shops(mary).
end(model(1265)).

begin(model(1266)).
bought(fish).
shops(mary).
end(model(1266)).

begin(model(1267)).
bought(fish).
shops(mary).
end(model(1267)).

begin(model(1268)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1268)).

begin(model(1269)).
bought(fish).
shops(mary).
end(model(1269)).

begin(model(1270)).
bought(fish).
shops(mary).
end(model(1270)).

begin(model(1271)).
bought(spaghetti).
shops(mary).
end(model(1271)).

begin(model(1272)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1272)).

begin(model(1273)).
end(model(1273)).

begin(model(1274)).
bought(fish).
shops(mary).
end(model(1274)).

begin(model(1275)).
bought(fish).
shops(mary).
end(model(1275)).

begin(model(1276)).
bought(fish).
shops(mary).
end(model(1276)).

begin(model(1277)).
bought(spaghetti).
shops(mary).
end(model(1277)).

begin(model(1278)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1278)).

begin(model(1279)).
bought(fish).
shops(mary).
end(model(1279)).

begin(model(1280)).
bought(fish).
shops(mary).
end(model(1280)).

begin(model(1281)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1281)).

begin(model(1282)).
bought(fish).
shops(mary).
end(model(1282)).

begin(model(1283)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1283)).

begin(model(1284)).
bought(fish).
shops(mary).
end(model(1284)).

begin(model(1285)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1285)).

begin(model(1286)).
bought(fish).
shops(mary).
end(model(1286)).

begin(model(1287)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1287)).

begin(model(1288)).
bought(fish).
shops(mary).
end(model(1288)).

begin(model(1289)).
bought(spaghetti).
shops(mary).
end(model(1289)).

begin(model(1290)).
bought(fish).
shops(mary).
end(model(1290)).

begin(model(1291)).
bought(fish).
shops(mary).
end(model(1291)).

begin(model(1292)).
bought(fish).
shops(mary).
end(model(1292)).

begin(model(1293)).
bought(fish).
shops(mary).
end(model(1293)).

begin(model(1294)).
bought(fish).
shops(mary).
end(model(1294)).

begin(model(1295)).
bought(fish).
shops(mary).
end(model(1295)).

begin(model(1296)).
bought(fish).
shops(mary).
end(model(1296)).

begin(model(1297)).
bought(spaghetti).
shops(mary).
end(model(1297)).

begin(model(1298)).
bought(fish).
shops(mary).
end(model(1298)).

begin(model(1299)).
bought(spaghetti).
shops(mary).
end(model(1299)).

begin(model(1300)).
bought(fish).
shops(mary).
end(model(1300)).

begin(model(1301)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1301)).

begin(model(1302)).
bought(spaghetti).
shops(mary).
end(model(1302)).

begin(model(1303)).
bought(fish).
shops(mary).
end(model(1303)).

begin(model(1304)).
bought(fish).
shops(mary).
end(model(1304)).

begin(model(1305)).
bought(fish).
shops(mary).
end(model(1305)).

begin(model(1306)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1306)).

begin(model(1307)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1307)).

begin(model(1308)).
bought(fish).
shops(mary).
end(model(1308)).

begin(model(1309)).
bought(spaghetti).
shops(mary).
end(model(1309)).

begin(model(1310)).
bought(fish).
shops(mary).
end(model(1310)).

begin(model(1311)).
bought(spaghetti).
shops(mary).
end(model(1311)).

begin(model(1312)).
bought(fish).
shops(mary).
end(model(1312)).

begin(model(1313)).
end(model(1313)).

begin(model(1314)).
bought(fish).
shops(mary).
end(model(1314)).

begin(model(1315)).
bought(fish).
shops(mary).
end(model(1315)).

begin(model(1316)).
bought(spaghetti).
shops(mary).
end(model(1316)).

begin(model(1317)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1317)).

begin(model(1318)).
bought(spaghetti).
shops(mary).
end(model(1318)).

begin(model(1319)).
bought(spaghetti).
shops(mary).
end(model(1319)).

begin(model(1320)).
bought(spaghetti).
shops(mary).
end(model(1320)).

begin(model(1321)).
bought(fish).
shops(mary).
end(model(1321)).

begin(model(1322)).
bought(spaghetti).
shops(mary).
end(model(1322)).

begin(model(1323)).
bought(fish).
shops(mary).
end(model(1323)).

begin(model(1324)).
bought(spaghetti).
shops(mary).
end(model(1324)).

begin(model(1325)).
bought(fish).
shops(mary).
end(model(1325)).

begin(model(1326)).
bought(fish).
shops(mary).
end(model(1326)).

begin(model(1327)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1327)).

begin(model(1328)).
end(model(1328)).

begin(model(1329)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1329)).

begin(model(1330)).
bought(fish).
shops(mary).
end(model(1330)).

begin(model(1331)).
bought(spaghetti).
shops(mary).
end(model(1331)).

begin(model(1332)).
bought(fish).
shops(mary).
end(model(1332)).

begin(model(1333)).
bought(spaghetti).
shops(mary).
end(model(1333)).

begin(model(1334)).
bought(fish).
shops(mary).
end(model(1334)).

begin(model(1335)).
bought(fish).
shops(mary).
end(model(1335)).

begin(model(1336)).
bought(fish).
shops(mary).
end(model(1336)).

begin(model(1337)).
bought(spaghetti).
shops(mary).
end(model(1337)).

begin(model(1338)).
bought(fish).
shops(mary).
end(model(1338)).

begin(model(1339)).
bought(spaghetti).
shops(mary).
end(model(1339)).

begin(model(1340)).
bought(spaghetti).
shops(mary).
end(model(1340)).

begin(model(1341)).
bought(fish).
shops(mary).
end(model(1341)).

begin(model(1342)).
bought(spaghetti).
shops(mary).
end(model(1342)).

begin(model(1343)).
bought(fish).
shops(mary).
end(model(1343)).

begin(model(1344)).
bought(spaghetti).
shops(mary).
end(model(1344)).

begin(model(1345)).
bought(fish).
shops(mary).
end(model(1345)).

begin(model(1346)).
bought(spaghetti).
shops(mary).
end(model(1346)).

begin(model(1347)).
end(model(1347)).

begin(model(1348)).
bought(fish).
shops(mary).
end(model(1348)).

begin(model(1349)).
bought(fish).
shops(mary).
end(model(1349)).

begin(model(1350)).
bought(fish).
shops(mary).
end(model(1350)).

begin(model(1351)).
bought(fish).
shops(mary).
end(model(1351)).

begin(model(1352)).
bought(fish).
shops(mary).
end(model(1352)).

begin(model(1353)).
bought(fish).
shops(mary).
end(model(1353)).

begin(model(1354)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1354)).

begin(model(1355)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1355)).

begin(model(1356)).
bought(fish).
shops(mary).
end(model(1356)).

begin(model(1357)).
bought(fish).
shops(mary).
end(model(1357)).

begin(model(1358)).
end(model(1358)).

begin(model(1359)).
bought(fish).
shops(mary).
end(model(1359)).

begin(model(1360)).
bought(fish).
shops(mary).
end(model(1360)).

begin(model(1361)).
bought(fish).
shops(mary).
end(model(1361)).

begin(model(1362)).
bought(fish).
shops(mary).
end(model(1362)).

begin(model(1363)).
bought(spaghetti).
shops(mary).
end(model(1363)).

begin(model(1364)).
bought(spaghetti).
shops(mary).
end(model(1364)).

begin(model(1365)).
bought(fish).
shops(mary).
end(model(1365)).

begin(model(1366)).
bought(fish).
shops(mary).
end(model(1366)).

begin(model(1367)).
bought(fish).
shops(mary).
end(model(1367)).

begin(model(1368)).
bought(spaghetti).
shops(mary).
end(model(1368)).

begin(model(1369)).
bought(spaghetti).
shops(mary).
end(model(1369)).

begin(model(1370)).
bought(spaghetti).
shops(mary).
end(model(1370)).

begin(model(1371)).
bought(spaghetti).
shops(mary).
end(model(1371)).

begin(model(1372)).
bought(fish).
shops(mary).
end(model(1372)).

begin(model(1373)).
bought(fish).
shops(mary).
end(model(1373)).

begin(model(1374)).
bought(fish).
shops(mary).
end(model(1374)).

begin(model(1375)).
bought(spaghetti).
shops(mary).
end(model(1375)).

begin(model(1376)).
bought(spaghetti).
shops(mary).
end(model(1376)).

begin(model(1377)).
bought(fish).
shops(mary).
end(model(1377)).

begin(model(1378)).
bought(fish).
shops(mary).
end(model(1378)).

begin(model(1379)).
bought(fish).
shops(mary).
end(model(1379)).

begin(model(1380)).
bought(fish).
shops(mary).
end(model(1380)).

begin(model(1381)).
bought(fish).
shops(mary).
end(model(1381)).

begin(model(1382)).
bought(spaghetti).
shops(mary).
end(model(1382)).

begin(model(1383)).
bought(fish).
shops(mary).
end(model(1383)).

begin(model(1384)).
bought(fish).
shops(mary).
end(model(1384)).

begin(model(1385)).
bought(spaghetti).
shops(mary).
end(model(1385)).

begin(model(1386)).
bought(spaghetti).
shops(mary).
end(model(1386)).

begin(model(1387)).
bought(spaghetti).
shops(mary).
end(model(1387)).

begin(model(1388)).
bought(fish).
shops(mary).
end(model(1388)).

begin(model(1389)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1389)).

begin(model(1390)).
bought(fish).
shops(mary).
end(model(1390)).

begin(model(1391)).
bought(fish).
shops(mary).
end(model(1391)).

begin(model(1392)).
bought(fish).
shops(mary).
end(model(1392)).

begin(model(1393)).
bought(fish).
shops(mary).
end(model(1393)).

begin(model(1394)).
bought(fish).
shops(mary).
end(model(1394)).

begin(model(1395)).
bought(spaghetti).
shops(mary).
end(model(1395)).

begin(model(1396)).
bought(fish).
shops(mary).
end(model(1396)).

begin(model(1397)).
bought(fish).
shops(mary).
end(model(1397)).

begin(model(1398)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1398)).

begin(model(1399)).
bought(fish).
shops(mary).
end(model(1399)).

begin(model(1400)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1400)).

begin(model(1401)).
bought(fish).
shops(mary).
end(model(1401)).

begin(model(1402)).
bought(fish).
shops(mary).
end(model(1402)).

begin(model(1403)).
bought(fish).
shops(mary).
end(model(1403)).

begin(model(1404)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1404)).

begin(model(1405)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1405)).

begin(model(1406)).
bought(fish).
shops(mary).
end(model(1406)).

begin(model(1407)).
bought(fish).
shops(mary).
end(model(1407)).

begin(model(1408)).
bought(spaghetti).
shops(mary).
end(model(1408)).

begin(model(1409)).
bought(spaghetti).
shops(mary).
end(model(1409)).

begin(model(1410)).
bought(spaghetti).
shops(mary).
end(model(1410)).

begin(model(1411)).
bought(fish).
shops(mary).
end(model(1411)).

begin(model(1412)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1412)).

begin(model(1413)).
bought(spaghetti).
shops(mary).
end(model(1413)).

begin(model(1414)).
bought(fish).
shops(mary).
end(model(1414)).

begin(model(1415)).
bought(spaghetti).
shops(john).
end(model(1415)).

begin(model(1416)).
bought(fish).
shops(mary).
end(model(1416)).

begin(model(1417)).
bought(spaghetti).
shops(john).
end(model(1417)).

begin(model(1418)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1418)).

begin(model(1419)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1419)).

begin(model(1420)).
bought(fish).
shops(mary).
end(model(1420)).

begin(model(1421)).
bought(fish).
shops(mary).
end(model(1421)).

begin(model(1422)).
bought(fish).
shops(mary).
end(model(1422)).

begin(model(1423)).
bought(spaghetti).
shops(mary).
end(model(1423)).

begin(model(1424)).
bought(fish).
shops(mary).
end(model(1424)).

begin(model(1425)).
bought(fish).
shops(mary).
end(model(1425)).

begin(model(1426)).
bought(fish).
shops(mary).
end(model(1426)).

begin(model(1427)).
bought(fish).
shops(mary).
end(model(1427)).

begin(model(1428)).
bought(fish).
shops(mary).
end(model(1428)).

begin(model(1429)).
bought(fish).
shops(mary).
end(model(1429)).

begin(model(1430)).
bought(fish).
shops(mary).
end(model(1430)).

begin(model(1431)).
bought(fish).
shops(mary).
end(model(1431)).

begin(model(1432)).
bought(fish).
shops(mary).
end(model(1432)).

begin(model(1433)).
bought(fish).
shops(mary).
end(model(1433)).

begin(model(1434)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1434)).

begin(model(1435)).
bought(fish).
shops(mary).
end(model(1435)).

begin(model(1436)).
bought(spaghetti).
shops(mary).
end(model(1436)).

begin(model(1437)).
bought(spaghetti).
shops(mary).
end(model(1437)).

begin(model(1438)).
bought(fish).
shops(mary).
end(model(1438)).

begin(model(1439)).
bought(fish).
shops(mary).
end(model(1439)).

begin(model(1440)).
bought(spaghetti).
shops(mary).
end(model(1440)).

begin(model(1441)).
bought(spaghetti).
shops(mary).
end(model(1441)).

begin(model(1442)).
end(model(1442)).

begin(model(1443)).
bought(fish).
shops(mary).
end(model(1443)).

begin(model(1444)).
bought(spaghetti).
shops(mary).
end(model(1444)).

begin(model(1445)).
bought(spaghetti).
shops(mary).
end(model(1445)).

begin(model(1446)).
end(model(1446)).

begin(model(1447)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1447)).

begin(model(1448)).
end(model(1448)).

begin(model(1449)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1449)).

begin(model(1450)).
bought(spaghetti).
shops(john).
end(model(1450)).

begin(model(1451)).
bought(spaghetti).
shops(mary).
end(model(1451)).

begin(model(1452)).
bought(fish).
shops(mary).
end(model(1452)).

begin(model(1453)).
bought(fish).
shops(mary).
end(model(1453)).

begin(model(1454)).
bought(fish).
shops(mary).
end(model(1454)).

begin(model(1455)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1455)).

begin(model(1456)).
bought(fish).
shops(mary).
end(model(1456)).

begin(model(1457)).
bought(fish).
shops(mary).
end(model(1457)).

begin(model(1458)).
bought(fish).
shops(mary).
end(model(1458)).

begin(model(1459)).
end(model(1459)).

begin(model(1460)).
end(model(1460)).

begin(model(1461)).
bought(spaghetti).
shops(mary).
end(model(1461)).

begin(model(1462)).
bought(fish).
shops(mary).
end(model(1462)).

begin(model(1463)).
bought(fish).
shops(mary).
end(model(1463)).

begin(model(1464)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1464)).

begin(model(1465)).
bought(spaghetti).
shops(mary).
end(model(1465)).

begin(model(1466)).
bought(fish).
shops(mary).
end(model(1466)).

begin(model(1467)).
bought(fish).
shops(mary).
end(model(1467)).

begin(model(1468)).
bought(fish).
shops(mary).
end(model(1468)).

begin(model(1469)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1469)).

begin(model(1470)).
bought(fish).
shops(mary).
end(model(1470)).

begin(model(1471)).
bought(fish).
shops(mary).
end(model(1471)).

begin(model(1472)).
bought(spaghetti).
shops(mary).
end(model(1472)).

begin(model(1473)).
end(model(1473)).

begin(model(1474)).
bought(fish).
shops(mary).
end(model(1474)).

begin(model(1475)).
bought(spaghetti).
shops(mary).
end(model(1475)).

begin(model(1476)).
bought(spaghetti).
shops(mary).
end(model(1476)).

begin(model(1477)).
bought(fish).
shops(mary).
end(model(1477)).

begin(model(1478)).
bought(spaghetti).
shops(mary).
end(model(1478)).

begin(model(1479)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1479)).

begin(model(1480)).
end(model(1480)).

begin(model(1481)).
end(model(1481)).

begin(model(1482)).
bought(fish).
shops(mary).
end(model(1482)).

begin(model(1483)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1483)).

begin(model(1484)).
bought(fish).
shops(mary).
end(model(1484)).

begin(model(1485)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1485)).

begin(model(1486)).
bought(fish).
shops(mary).
end(model(1486)).

begin(model(1487)).
bought(fish).
shops(mary).
end(model(1487)).

begin(model(1488)).
bought(fish).
shops(mary).
end(model(1488)).

begin(model(1489)).
end(model(1489)).

begin(model(1490)).
bought(spaghetti).
shops(mary).
end(model(1490)).

begin(model(1491)).
end(model(1491)).

begin(model(1492)).
bought(fish).
shops(mary).
end(model(1492)).

begin(model(1493)).
bought(fish).
shops(mary).
end(model(1493)).

begin(model(1494)).
bought(fish).
shops(mary).
end(model(1494)).

begin(model(1495)).
bought(spaghetti).
shops(mary).
end(model(1495)).

begin(model(1496)).
end(model(1496)).

begin(model(1497)).
bought(spaghetti).
shops(mary).
end(model(1497)).

begin(model(1498)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1498)).

begin(model(1499)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1499)).

begin(model(1500)).
bought(fish).
shops(mary).
end(model(1500)).

begin(model(1501)).
bought(fish).
shops(mary).
end(model(1501)).

begin(model(1502)).
bought(fish).
shops(mary).
end(model(1502)).

begin(model(1503)).
bought(fish).
shops(mary).
end(model(1503)).

begin(model(1504)).
bought(fish).
shops(mary).
end(model(1504)).

begin(model(1505)).
bought(fish).
shops(mary).
end(model(1505)).

begin(model(1506)).
bought(fish).
shops(mary).
end(model(1506)).

begin(model(1507)).
bought(spaghetti).
shops(mary).
end(model(1507)).

begin(model(1508)).
bought(fish).
shops(mary).
end(model(1508)).

begin(model(1509)).
bought(fish).
shops(mary).
end(model(1509)).

begin(model(1510)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1510)).

begin(model(1511)).
bought(fish).
shops(mary).
end(model(1511)).

begin(model(1512)).
bought(spaghetti).
shops(mary).
end(model(1512)).

begin(model(1513)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1513)).

begin(model(1514)).
bought(fish).
shops(mary).
end(model(1514)).

begin(model(1515)).
bought(fish).
shops(mary).
end(model(1515)).

begin(model(1516)).
bought(spaghetti).
shops(mary).
end(model(1516)).

begin(model(1517)).
end(model(1517)).

begin(model(1518)).
bought(fish).
shops(mary).
end(model(1518)).

begin(model(1519)).
bought(spaghetti).
shops(mary).
end(model(1519)).

begin(model(1520)).
bought(spaghetti).
shops(mary).
end(model(1520)).

begin(model(1521)).
bought(spaghetti).
shops(mary).
end(model(1521)).

begin(model(1522)).
bought(fish).
shops(mary).
end(model(1522)).

begin(model(1523)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1523)).

begin(model(1524)).
bought(spaghetti).
shops(mary).
end(model(1524)).

begin(model(1525)).
bought(fish).
shops(mary).
end(model(1525)).

begin(model(1526)).
bought(fish).
shops(mary).
end(model(1526)).

begin(model(1527)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1527)).

begin(model(1528)).
bought(fish).
shops(mary).
end(model(1528)).

begin(model(1529)).
bought(fish).
shops(mary).
end(model(1529)).

begin(model(1530)).
bought(fish).
shops(mary).
end(model(1530)).

begin(model(1531)).
bought(fish).
shops(mary).
end(model(1531)).

begin(model(1532)).
bought(spaghetti).
shops(mary).
end(model(1532)).

begin(model(1533)).
end(model(1533)).

begin(model(1534)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1534)).

begin(model(1535)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1535)).

begin(model(1536)).
bought(fish).
shops(mary).
end(model(1536)).

begin(model(1537)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1537)).

begin(model(1538)).
bought(fish).
shops(mary).
end(model(1538)).

begin(model(1539)).
end(model(1539)).

begin(model(1540)).
bought(fish).
shops(mary).
end(model(1540)).

begin(model(1541)).
bought(fish).
shops(mary).
end(model(1541)).

begin(model(1542)).
bought(fish).
shops(mary).
end(model(1542)).

begin(model(1543)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1543)).

begin(model(1544)).
bought(fish).
shops(mary).
end(model(1544)).

begin(model(1545)).
end(model(1545)).

begin(model(1546)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1546)).

begin(model(1547)).
bought(spaghetti).
shops(john).
end(model(1547)).

begin(model(1548)).
bought(fish).
shops(mary).
end(model(1548)).

begin(model(1549)).
bought(spaghetti).
shops(mary).
end(model(1549)).

begin(model(1550)).
bought(fish).
shops(mary).
end(model(1550)).

begin(model(1551)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1551)).

begin(model(1552)).
bought(fish).
shops(mary).
end(model(1552)).

begin(model(1553)).
bought(fish).
shops(mary).
end(model(1553)).

begin(model(1554)).
bought(spaghetti).
shops(mary).
end(model(1554)).

begin(model(1555)).
bought(spaghetti).
shops(mary).
end(model(1555)).

begin(model(1556)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1556)).

begin(model(1557)).
bought(fish).
shops(mary).
end(model(1557)).

begin(model(1558)).
bought(spaghetti).
shops(mary).
end(model(1558)).

begin(model(1559)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1559)).

begin(model(1560)).
bought(spaghetti).
shops(mary).
end(model(1560)).

begin(model(1561)).
bought(fish).
shops(mary).
end(model(1561)).

begin(model(1562)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1562)).

begin(model(1563)).
end(model(1563)).

begin(model(1564)).
bought(fish).
shops(mary).
end(model(1564)).

begin(model(1565)).
bought(fish).
shops(mary).
end(model(1565)).

begin(model(1566)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1566)).

begin(model(1567)).
bought(fish).
shops(mary).
end(model(1567)).

begin(model(1568)).
bought(fish).
shops(mary).
end(model(1568)).

begin(model(1569)).
end(model(1569)).

begin(model(1570)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1570)).

begin(model(1571)).
bought(fish).
shops(mary).
end(model(1571)).

begin(model(1572)).
bought(fish).
shops(mary).
end(model(1572)).

begin(model(1573)).
bought(spaghetti).
shops(john).
end(model(1573)).

begin(model(1574)).
bought(fish).
shops(mary).
end(model(1574)).

begin(model(1575)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1575)).

begin(model(1576)).
bought(fish).
shops(mary).
end(model(1576)).

begin(model(1577)).
bought(fish).
shops(mary).
end(model(1577)).

begin(model(1578)).
bought(fish).
shops(mary).
end(model(1578)).

begin(model(1579)).
bought(fish).
shops(mary).
end(model(1579)).

begin(model(1580)).
end(model(1580)).

begin(model(1581)).
bought(fish).
shops(mary).
end(model(1581)).

begin(model(1582)).
bought(spaghetti).
shops(mary).
end(model(1582)).

begin(model(1583)).
bought(fish).
shops(mary).
end(model(1583)).

begin(model(1584)).
bought(fish).
shops(mary).
end(model(1584)).

begin(model(1585)).
bought(spaghetti).
shops(mary).
end(model(1585)).

begin(model(1586)).
bought(fish).
shops(mary).
end(model(1586)).

begin(model(1587)).
bought(spaghetti).
shops(mary).
end(model(1587)).

begin(model(1588)).
bought(fish).
shops(mary).
end(model(1588)).

begin(model(1589)).
end(model(1589)).

begin(model(1590)).
end(model(1590)).

begin(model(1591)).
bought(spaghetti).
shops(mary).
end(model(1591)).

begin(model(1592)).
bought(fish).
shops(mary).
end(model(1592)).

begin(model(1593)).
bought(fish).
shops(mary).
end(model(1593)).

begin(model(1594)).
bought(spaghetti).
shops(mary).
end(model(1594)).

begin(model(1595)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1595)).

begin(model(1596)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1596)).

begin(model(1597)).
bought(fish).
shops(mary).
end(model(1597)).

begin(model(1598)).
end(model(1598)).

begin(model(1599)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1599)).

begin(model(1600)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1600)).

begin(model(1601)).
bought(spaghetti).
shops(mary).
end(model(1601)).

begin(model(1602)).
bought(fish).
shops(mary).
end(model(1602)).

begin(model(1603)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1603)).

begin(model(1604)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1604)).

begin(model(1605)).
bought(fish).
shops(mary).
end(model(1605)).

begin(model(1606)).
bought(spaghetti).
shops(mary).
end(model(1606)).

begin(model(1607)).
bought(fish).
shops(mary).
end(model(1607)).

begin(model(1608)).
bought(fish).
shops(mary).
end(model(1608)).

begin(model(1609)).
bought(spaghetti).
shops(mary).
end(model(1609)).

begin(model(1610)).
bought(fish).
shops(mary).
end(model(1610)).

begin(model(1611)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1611)).

begin(model(1612)).
bought(fish).
shops(mary).
end(model(1612)).

begin(model(1613)).
bought(fish).
shops(mary).
end(model(1613)).

begin(model(1614)).
bought(spaghetti).
shops(mary).
end(model(1614)).

begin(model(1615)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1615)).

begin(model(1616)).
bought(fish).
shops(mary).
end(model(1616)).

begin(model(1617)).
bought(fish).
shops(mary).
end(model(1617)).

begin(model(1618)).
bought(fish).
shops(mary).
end(model(1618)).

begin(model(1619)).
bought(fish).
shops(mary).
end(model(1619)).

begin(model(1620)).
bought(fish).
shops(mary).
end(model(1620)).

begin(model(1621)).
bought(fish).
shops(mary).
end(model(1621)).

begin(model(1622)).
bought(fish).
shops(mary).
end(model(1622)).

begin(model(1623)).
bought(fish).
shops(mary).
end(model(1623)).

begin(model(1624)).
bought(fish).
shops(mary).
end(model(1624)).

begin(model(1625)).
bought(fish).
shops(mary).
end(model(1625)).

begin(model(1626)).
bought(fish).
shops(mary).
end(model(1626)).

begin(model(1627)).
end(model(1627)).

begin(model(1628)).
bought(fish).
shops(mary).
end(model(1628)).

begin(model(1629)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1629)).

begin(model(1630)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1630)).

begin(model(1631)).
bought(fish).
shops(mary).
end(model(1631)).

begin(model(1632)).
bought(fish).
shops(mary).
end(model(1632)).

begin(model(1633)).
bought(fish).
shops(mary).
end(model(1633)).

begin(model(1634)).
bought(fish).
shops(mary).
end(model(1634)).

begin(model(1635)).
bought(spaghetti).
shops(mary).
end(model(1635)).

begin(model(1636)).
bought(fish).
shops(mary).
end(model(1636)).

begin(model(1637)).
bought(fish).
shops(mary).
end(model(1637)).

begin(model(1638)).
bought(fish).
shops(mary).
end(model(1638)).

begin(model(1639)).
bought(fish).
shops(mary).
end(model(1639)).

begin(model(1640)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1640)).

begin(model(1641)).
bought(spaghetti).
shops(mary).
end(model(1641)).

begin(model(1642)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1642)).

begin(model(1643)).
bought(fish).
shops(mary).
end(model(1643)).

begin(model(1644)).
bought(fish).
shops(mary).
end(model(1644)).

begin(model(1645)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1645)).

begin(model(1646)).
bought(fish).
shops(mary).
end(model(1646)).

begin(model(1647)).
bought(fish).
shops(mary).
end(model(1647)).

begin(model(1648)).
bought(fish).
shops(mary).
end(model(1648)).

begin(model(1649)).
bought(spaghetti).
shops(mary).
end(model(1649)).

begin(model(1650)).
bought(fish).
shops(mary).
end(model(1650)).

begin(model(1651)).
end(model(1651)).

begin(model(1652)).
bought(spaghetti).
shops(mary).
end(model(1652)).

begin(model(1653)).
bought(fish).
shops(mary).
end(model(1653)).

begin(model(1654)).
bought(spaghetti).
shops(mary).
end(model(1654)).

begin(model(1655)).
bought(fish).
shops(mary).
end(model(1655)).

begin(model(1656)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1656)).

begin(model(1657)).
bought(fish).
shops(mary).
end(model(1657)).

begin(model(1658)).
bought(spaghetti).
shops(mary).
end(model(1658)).

begin(model(1659)).
bought(fish).
shops(mary).
end(model(1659)).

begin(model(1660)).
bought(spaghetti).
shops(mary).
end(model(1660)).

begin(model(1661)).
bought(fish).
shops(mary).
end(model(1661)).

begin(model(1662)).
bought(fish).
shops(mary).
end(model(1662)).

begin(model(1663)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1663)).

begin(model(1664)).
bought(spaghetti).
shops(mary).
end(model(1664)).

begin(model(1665)).
bought(fish).
shops(mary).
end(model(1665)).

begin(model(1666)).
bought(spaghetti).
shops(mary).
end(model(1666)).

begin(model(1667)).
bought(fish).
shops(mary).
end(model(1667)).

begin(model(1668)).
bought(spaghetti).
shops(mary).
end(model(1668)).

begin(model(1669)).
bought(fish).
shops(mary).
end(model(1669)).

begin(model(1670)).
bought(fish).
shops(mary).
end(model(1670)).

begin(model(1671)).
bought(fish).
shops(mary).
end(model(1671)).

begin(model(1672)).
bought(fish).
shops(mary).
end(model(1672)).

begin(model(1673)).
bought(fish).
shops(mary).
end(model(1673)).

begin(model(1674)).
bought(spaghetti).
shops(mary).
end(model(1674)).

begin(model(1675)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1675)).

begin(model(1676)).
bought(fish).
shops(mary).
end(model(1676)).

begin(model(1677)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1677)).

begin(model(1678)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1678)).

begin(model(1679)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1679)).

begin(model(1680)).
end(model(1680)).

begin(model(1681)).
end(model(1681)).

begin(model(1682)).
bought(spaghetti).
shops(mary).
end(model(1682)).

begin(model(1683)).
bought(spaghetti).
shops(mary).
end(model(1683)).

begin(model(1684)).
bought(fish).
shops(mary).
end(model(1684)).

begin(model(1685)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1685)).

begin(model(1686)).
bought(spaghetti).
shops(mary).
end(model(1686)).

begin(model(1687)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1687)).

begin(model(1688)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1688)).

begin(model(1689)).
bought(spaghetti).
shops(mary).
end(model(1689)).

begin(model(1690)).
bought(spaghetti).
shops(mary).
end(model(1690)).

begin(model(1691)).
bought(fish).
shops(mary).
end(model(1691)).

begin(model(1692)).
bought(spaghetti).
shops(mary).
end(model(1692)).

begin(model(1693)).
bought(fish).
shops(mary).
end(model(1693)).

begin(model(1694)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1694)).

begin(model(1695)).
bought(spaghetti).
shops(mary).
end(model(1695)).

begin(model(1696)).
bought(fish).
shops(mary).
end(model(1696)).

begin(model(1697)).
bought(spaghetti).
shops(mary).
end(model(1697)).

begin(model(1698)).
bought(fish).
shops(mary).
end(model(1698)).

begin(model(1699)).
end(model(1699)).

begin(model(1700)).
bought(spaghetti).
shops(mary).
end(model(1700)).

begin(model(1701)).
bought(fish).
shops(mary).
end(model(1701)).

begin(model(1702)).
bought(spaghetti).
shops(mary).
end(model(1702)).

begin(model(1703)).
bought(fish).
shops(mary).
end(model(1703)).

begin(model(1704)).
bought(spaghetti).
shops(mary).
end(model(1704)).

begin(model(1705)).
bought(fish).
shops(mary).
end(model(1705)).

begin(model(1706)).
bought(fish).
shops(mary).
end(model(1706)).

begin(model(1707)).
bought(fish).
shops(mary).
end(model(1707)).

begin(model(1708)).
bought(spaghetti).
shops(john).
end(model(1708)).

begin(model(1709)).
bought(fish).
shops(mary).
end(model(1709)).

begin(model(1710)).
bought(fish).
shops(mary).
end(model(1710)).

begin(model(1711)).
bought(spaghetti).
shops(mary).
end(model(1711)).

begin(model(1712)).
bought(fish).
shops(mary).
end(model(1712)).

begin(model(1713)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1713)).

begin(model(1714)).
bought(fish).
shops(mary).
end(model(1714)).

begin(model(1715)).
bought(spaghetti).
shops(mary).
end(model(1715)).

begin(model(1716)).
bought(fish).
shops(mary).
end(model(1716)).

begin(model(1717)).
bought(fish).
shops(mary).
end(model(1717)).

begin(model(1718)).
bought(fish).
shops(mary).
end(model(1718)).

begin(model(1719)).
bought(fish).
shops(mary).
end(model(1719)).

begin(model(1720)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1720)).

begin(model(1721)).
bought(fish).
shops(mary).
end(model(1721)).

begin(model(1722)).
bought(fish).
shops(mary).
end(model(1722)).

begin(model(1723)).
bought(fish).
shops(mary).
end(model(1723)).

begin(model(1724)).
bought(fish).
shops(mary).
end(model(1724)).

begin(model(1725)).
bought(fish).
shops(mary).
end(model(1725)).

begin(model(1726)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1726)).

begin(model(1727)).
bought(fish).
shops(mary).
end(model(1727)).

begin(model(1728)).
bought(fish).
shops(mary).
end(model(1728)).

begin(model(1729)).
end(model(1729)).

begin(model(1730)).
bought(spaghetti).
shops(mary).
end(model(1730)).

begin(model(1731)).
bought(fish).
shops(mary).
end(model(1731)).

begin(model(1732)).
bought(fish).
shops(mary).
end(model(1732)).

begin(model(1733)).
bought(fish).
shops(mary).
end(model(1733)).

begin(model(1734)).
bought(fish).
shops(mary).
end(model(1734)).

begin(model(1735)).
bought(fish).
shops(mary).
end(model(1735)).

begin(model(1736)).
bought(spaghetti).
shops(mary).
end(model(1736)).

begin(model(1737)).
bought(fish).
shops(mary).
end(model(1737)).

begin(model(1738)).
bought(fish).
shops(mary).
end(model(1738)).

begin(model(1739)).
bought(spaghetti).
shops(mary).
end(model(1739)).

begin(model(1740)).
bought(spaghetti).
shops(mary).
end(model(1740)).

begin(model(1741)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1741)).

begin(model(1742)).
bought(fish).
shops(mary).
end(model(1742)).

begin(model(1743)).
bought(spaghetti).
shops(mary).
end(model(1743)).

begin(model(1744)).
bought(spaghetti).
shops(mary).
end(model(1744)).

begin(model(1745)).
bought(spaghetti).
shops(mary).
end(model(1745)).

begin(model(1746)).
bought(spaghetti).
shops(mary).
end(model(1746)).

begin(model(1747)).
bought(fish).
shops(mary).
end(model(1747)).

begin(model(1748)).
bought(spaghetti).
shops(mary).
end(model(1748)).

begin(model(1749)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1749)).

begin(model(1750)).
bought(fish).
shops(mary).
end(model(1750)).

begin(model(1751)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1751)).

begin(model(1752)).
bought(spaghetti).
shops(mary).
end(model(1752)).

begin(model(1753)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1753)).

begin(model(1754)).
bought(fish).
shops(mary).
end(model(1754)).

begin(model(1755)).
bought(fish).
shops(mary).
end(model(1755)).

begin(model(1756)).
end(model(1756)).

begin(model(1757)).
bought(fish).
shops(mary).
end(model(1757)).

begin(model(1758)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1758)).

begin(model(1759)).
bought(spaghetti).
shops(mary).
end(model(1759)).

begin(model(1760)).
bought(fish).
shops(mary).
end(model(1760)).

begin(model(1761)).
bought(fish).
shops(mary).
end(model(1761)).

begin(model(1762)).
bought(spaghetti).
shops(mary).
end(model(1762)).

begin(model(1763)).
bought(fish).
shops(mary).
end(model(1763)).

begin(model(1764)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1764)).

begin(model(1765)).
bought(fish).
shops(mary).
end(model(1765)).

begin(model(1766)).
bought(fish).
shops(mary).
end(model(1766)).

begin(model(1767)).
bought(spaghetti).
shops(mary).
end(model(1767)).

begin(model(1768)).
bought(spaghetti).
shops(mary).
end(model(1768)).

begin(model(1769)).
bought(fish).
shops(mary).
end(model(1769)).

begin(model(1770)).
bought(fish).
shops(mary).
end(model(1770)).

begin(model(1771)).
bought(fish).
shops(mary).
end(model(1771)).

begin(model(1772)).
bought(spaghetti).
shops(mary).
end(model(1772)).

begin(model(1773)).
bought(fish).
shops(mary).
end(model(1773)).

begin(model(1774)).
bought(fish).
shops(mary).
end(model(1774)).

begin(model(1775)).
bought(fish).
shops(mary).
end(model(1775)).

begin(model(1776)).
bought(fish).
shops(mary).
end(model(1776)).

begin(model(1777)).
bought(spaghetti).
shops(mary).
end(model(1777)).

begin(model(1778)).
bought(fish).
shops(mary).
end(model(1778)).

begin(model(1779)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1779)).

begin(model(1780)).
bought(fish).
shops(mary).
end(model(1780)).

begin(model(1781)).
bought(fish).
shops(mary).
end(model(1781)).

begin(model(1782)).
end(model(1782)).

begin(model(1783)).
bought(fish).
shops(mary).
end(model(1783)).

begin(model(1784)).
bought(fish).
shops(mary).
end(model(1784)).

begin(model(1785)).
bought(fish).
shops(mary).
end(model(1785)).

begin(model(1786)).
bought(spaghetti).
shops(mary).
end(model(1786)).

begin(model(1787)).
bought(fish).
shops(mary).
end(model(1787)).

begin(model(1788)).
bought(fish).
shops(mary).
end(model(1788)).

begin(model(1789)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1789)).

begin(model(1790)).
bought(spaghetti).
shops(mary).
end(model(1790)).

begin(model(1791)).
bought(fish).
shops(mary).
end(model(1791)).

begin(model(1792)).
bought(fish).
shops(mary).
end(model(1792)).

begin(model(1793)).
bought(fish).
shops(mary).
end(model(1793)).

begin(model(1794)).
bought(fish).
shops(mary).
end(model(1794)).

begin(model(1795)).
bought(fish).
shops(mary).
end(model(1795)).

begin(model(1796)).
bought(spaghetti).
shops(mary).
end(model(1796)).

begin(model(1797)).
bought(spaghetti).
shops(mary).
end(model(1797)).

begin(model(1798)).
bought(spaghetti).
shops(mary).
end(model(1798)).

begin(model(1799)).
bought(fish).
shops(mary).
end(model(1799)).

begin(model(1800)).
bought(spaghetti).
shops(mary).
end(model(1800)).

begin(model(1801)).
bought(fish).
shops(mary).
end(model(1801)).

begin(model(1802)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1802)).

begin(model(1803)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1803)).

begin(model(1804)).
bought(fish).
shops(mary).
end(model(1804)).

begin(model(1805)).
bought(fish).
shops(mary).
end(model(1805)).

begin(model(1806)).
bought(fish).
shops(mary).
end(model(1806)).

begin(model(1807)).
bought(fish).
shops(mary).
end(model(1807)).

begin(model(1808)).
bought(fish).
shops(mary).
end(model(1808)).

begin(model(1809)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1809)).

begin(model(1810)).
bought(fish).
shops(mary).
end(model(1810)).

begin(model(1811)).
bought(spaghetti).
shops(mary).
end(model(1811)).

begin(model(1812)).
bought(fish).
shops(mary).
end(model(1812)).

begin(model(1813)).
bought(fish).
shops(mary).
end(model(1813)).

begin(model(1814)).
bought(fish).
shops(mary).
end(model(1814)).

begin(model(1815)).
bought(fish).
shops(mary).
end(model(1815)).

begin(model(1816)).
bought(fish).
shops(mary).
end(model(1816)).

begin(model(1817)).
bought(spaghetti).
shops(mary).
end(model(1817)).

begin(model(1818)).
end(model(1818)).

begin(model(1819)).
bought(fish).
shops(mary).
end(model(1819)).

begin(model(1820)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1820)).

begin(model(1821)).
bought(spaghetti).
shops(mary).
end(model(1821)).

begin(model(1822)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1822)).

begin(model(1823)).
bought(spaghetti).
shops(mary).
end(model(1823)).

begin(model(1824)).
end(model(1824)).

begin(model(1825)).
bought(fish).
shops(mary).
end(model(1825)).

begin(model(1826)).
bought(fish).
shops(mary).
end(model(1826)).

begin(model(1827)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1827)).

begin(model(1828)).
bought(spaghetti).
shops(mary).
end(model(1828)).

begin(model(1829)).
end(model(1829)).

begin(model(1830)).
bought(spaghetti).
shops(mary).
end(model(1830)).

begin(model(1831)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1831)).

begin(model(1832)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1832)).

begin(model(1833)).
bought(fish).
shops(mary).
end(model(1833)).

begin(model(1834)).
bought(fish).
shops(mary).
end(model(1834)).

begin(model(1835)).
bought(spaghetti).
shops(mary).
end(model(1835)).

begin(model(1836)).
bought(fish).
shops(mary).
end(model(1836)).

begin(model(1837)).
bought(spaghetti).
shops(mary).
end(model(1837)).

begin(model(1838)).
bought(fish).
shops(mary).
end(model(1838)).

begin(model(1839)).
end(model(1839)).

begin(model(1840)).
bought(fish).
shops(mary).
end(model(1840)).

begin(model(1841)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1841)).

begin(model(1842)).
bought(fish).
shops(mary).
end(model(1842)).

begin(model(1843)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1843)).

begin(model(1844)).
bought(fish).
shops(mary).
end(model(1844)).

begin(model(1845)).
bought(spaghetti).
shops(mary).
end(model(1845)).

begin(model(1846)).
bought(fish).
shops(mary).
end(model(1846)).

begin(model(1847)).
bought(fish).
shops(mary).
end(model(1847)).

begin(model(1848)).
bought(spaghetti).
shops(mary).
end(model(1848)).

begin(model(1849)).
bought(spaghetti).
shops(mary).
end(model(1849)).

begin(model(1850)).
bought(fish).
shops(mary).
end(model(1850)).

begin(model(1851)).
bought(fish).
shops(mary).
end(model(1851)).

begin(model(1852)).
bought(fish).
shops(mary).
end(model(1852)).

begin(model(1853)).
bought(fish).
shops(mary).
end(model(1853)).

begin(model(1854)).
end(model(1854)).

begin(model(1855)).
bought(fish).
shops(mary).
end(model(1855)).

begin(model(1856)).
bought(spaghetti).
shops(mary).
end(model(1856)).

begin(model(1857)).
bought(fish).
shops(mary).
end(model(1857)).

begin(model(1858)).
bought(spaghetti).
shops(mary).
end(model(1858)).

begin(model(1859)).
bought(spaghetti).
shops(mary).
end(model(1859)).

begin(model(1860)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1860)).

begin(model(1861)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1861)).

begin(model(1862)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1862)).

begin(model(1863)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1863)).

begin(model(1864)).
end(model(1864)).

begin(model(1865)).
end(model(1865)).

begin(model(1866)).
bought(fish).
shops(mary).
end(model(1866)).

begin(model(1867)).
bought(fish).
shops(mary).
end(model(1867)).

begin(model(1868)).
bought(fish).
shops(mary).
end(model(1868)).

begin(model(1869)).
bought(fish).
shops(mary).
end(model(1869)).

begin(model(1870)).
bought(spaghetti).
shops(mary).
end(model(1870)).

begin(model(1871)).
bought(fish).
shops(mary).
end(model(1871)).

begin(model(1872)).
end(model(1872)).

begin(model(1873)).
end(model(1873)).

begin(model(1874)).
bought(spaghetti).
shops(mary).
end(model(1874)).

begin(model(1875)).
bought(fish).
shops(mary).
end(model(1875)).

begin(model(1876)).
bought(spaghetti).
shops(mary).
end(model(1876)).

begin(model(1877)).
bought(fish).
shops(mary).
end(model(1877)).

begin(model(1878)).
bought(spaghetti).
shops(mary).
end(model(1878)).

begin(model(1879)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1879)).

begin(model(1880)).
bought(spaghetti).
shops(mary).
end(model(1880)).

begin(model(1881)).
bought(spaghetti).
shops(mary).
end(model(1881)).

begin(model(1882)).
end(model(1882)).

begin(model(1883)).
end(model(1883)).

begin(model(1884)).
bought(fish).
shops(mary).
end(model(1884)).

begin(model(1885)).
bought(fish).
shops(mary).
end(model(1885)).

begin(model(1886)).
bought(spaghetti).
shops(mary).
end(model(1886)).

begin(model(1887)).
bought(fish).
shops(mary).
end(model(1887)).

begin(model(1888)).
end(model(1888)).

begin(model(1889)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1889)).

begin(model(1890)).
bought(fish).
shops(mary).
end(model(1890)).

begin(model(1891)).
end(model(1891)).

begin(model(1892)).
bought(spaghetti).
shops(mary).
end(model(1892)).

begin(model(1893)).
bought(spaghetti).
shops(mary).
end(model(1893)).

begin(model(1894)).
bought(fish).
shops(mary).
end(model(1894)).

begin(model(1895)).
bought(spaghetti).
shops(mary).
end(model(1895)).

begin(model(1896)).
bought(spaghetti).
shops(mary).
end(model(1896)).

begin(model(1897)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1897)).

begin(model(1898)).
bought(fish).
shops(mary).
end(model(1898)).

begin(model(1899)).
bought(fish).
shops(mary).
end(model(1899)).

begin(model(1900)).
end(model(1900)).

begin(model(1901)).
bought(fish).
shops(mary).
end(model(1901)).

begin(model(1902)).
bought(fish).
shops(mary).
end(model(1902)).

begin(model(1903)).
bought(spaghetti).
shops(mary).
end(model(1903)).

begin(model(1904)).
bought(fish).
shops(mary).
end(model(1904)).

begin(model(1905)).
bought(spaghetti).
shops(mary).
end(model(1905)).

begin(model(1906)).
bought(fish).
shops(mary).
end(model(1906)).

begin(model(1907)).
bought(fish).
shops(mary).
end(model(1907)).

begin(model(1908)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1908)).

begin(model(1909)).
end(model(1909)).

begin(model(1910)).
bought(spaghetti).
shops(mary).
end(model(1910)).

begin(model(1911)).
bought(spaghetti).
shops(mary).
end(model(1911)).

begin(model(1912)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1912)).

begin(model(1913)).
bought(fish).
shops(mary).
end(model(1913)).

begin(model(1914)).
bought(fish).
shops(mary).
end(model(1914)).

begin(model(1915)).
bought(fish).
shops(mary).
end(model(1915)).

begin(model(1916)).
bought(fish).
shops(mary).
end(model(1916)).

begin(model(1917)).
bought(fish).
shops(mary).
end(model(1917)).

begin(model(1918)).
bought(fish).
shops(mary).
end(model(1918)).

begin(model(1919)).
bought(spaghetti).
shops(mary).
end(model(1919)).

begin(model(1920)).
end(model(1920)).

begin(model(1921)).
bought(fish).
shops(mary).
end(model(1921)).

begin(model(1922)).
bought(fish).
shops(mary).
end(model(1922)).

begin(model(1923)).
bought(spaghetti).
shops(mary).
end(model(1923)).

begin(model(1924)).
bought(fish).
shops(mary).
end(model(1924)).

begin(model(1925)).
bought(spaghetti).
shops(mary).
end(model(1925)).

begin(model(1926)).
end(model(1926)).

begin(model(1927)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1927)).

begin(model(1928)).
bought(fish).
shops(mary).
end(model(1928)).

begin(model(1929)).
bought(fish).
shops(mary).
end(model(1929)).

begin(model(1930)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1930)).

begin(model(1931)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1931)).

begin(model(1932)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(1932)).

begin(model(1933)).
bought(fish).
shops(mary).
end(model(1933)).

begin(model(1934)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1934)).

begin(model(1935)).
bought(fish).
shops(mary).
end(model(1935)).

begin(model(1936)).
bought(fish).
shops(mary).
end(model(1936)).

begin(model(1937)).
bought(spaghetti).
shops(mary).
end(model(1937)).

begin(model(1938)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1938)).

begin(model(1939)).
bought(steak).
shops(john).
end(model(1939)).

begin(model(1940)).
bought(fish).
shops(mary).
end(model(1940)).

begin(model(1941)).
end(model(1941)).

begin(model(1942)).
bought(fish).
shops(mary).
end(model(1942)).

begin(model(1943)).
bought(fish).
shops(mary).
end(model(1943)).

begin(model(1944)).
bought(fish).
shops(mary).
end(model(1944)).

begin(model(1945)).
bought(spaghetti).
shops(mary).
end(model(1945)).

begin(model(1946)).
bought(fish).
shops(mary).
end(model(1946)).

begin(model(1947)).
bought(fish).
shops(mary).
end(model(1947)).

begin(model(1948)).
bought(fish).
shops(mary).
end(model(1948)).

begin(model(1949)).
bought(fish).
shops(mary).
end(model(1949)).

begin(model(1950)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1950)).

begin(model(1951)).
bought(fish).
shops(mary).
end(model(1951)).

begin(model(1952)).
bought(fish).
shops(mary).
end(model(1952)).

begin(model(1953)).
bought(spaghetti).
shops(mary).
end(model(1953)).

begin(model(1954)).
end(model(1954)).

begin(model(1955)).
bought(spaghetti).
shops(mary).
end(model(1955)).

begin(model(1956)).
bought(spaghetti).
shops(mary).
end(model(1956)).

begin(model(1957)).
bought(spaghetti).
shops(mary).
end(model(1957)).

begin(model(1958)).
bought(fish).
shops(mary).
end(model(1958)).

begin(model(1959)).
bought(fish).
shops(mary).
end(model(1959)).

begin(model(1960)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1960)).

begin(model(1961)).
bought(fish).
shops(mary).
end(model(1961)).

begin(model(1962)).
bought(fish).
shops(mary).
end(model(1962)).

begin(model(1963)).
bought(fish).
shops(mary).
end(model(1963)).

begin(model(1964)).
end(model(1964)).

begin(model(1965)).
bought(fish).
shops(mary).
end(model(1965)).

begin(model(1966)).
end(model(1966)).

begin(model(1967)).
bought(spaghetti).
shops(mary).
end(model(1967)).

begin(model(1968)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1968)).

begin(model(1969)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1969)).

begin(model(1970)).
end(model(1970)).

begin(model(1971)).
end(model(1971)).

begin(model(1972)).
bought(spaghetti).
shops(mary).
end(model(1972)).

begin(model(1973)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1973)).

begin(model(1974)).
bought(fish).
shops(mary).
end(model(1974)).

begin(model(1975)).
bought(fish).
shops(mary).
end(model(1975)).

begin(model(1976)).
bought(fish).
shops(mary).
end(model(1976)).

begin(model(1977)).
bought(steak).
shops(john).
end(model(1977)).

begin(model(1978)).
bought(fish).
shops(mary).
end(model(1978)).

begin(model(1979)).
bought(fish).
shops(mary).
end(model(1979)).

begin(model(1980)).
end(model(1980)).

begin(model(1981)).
bought(fish).
shops(mary).
end(model(1981)).

begin(model(1982)).
bought(fish).
shops(mary).
end(model(1982)).

begin(model(1983)).
bought(fish).
shops(mary).
end(model(1983)).

begin(model(1984)).
bought(spaghetti).
shops(mary).
end(model(1984)).

begin(model(1985)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1985)).

begin(model(1986)).
bought(fish).
shops(mary).
end(model(1986)).

begin(model(1987)).
bought(spaghetti).
shops(mary).
end(model(1987)).

begin(model(1988)).
bought(fish).
shops(mary).
end(model(1988)).

begin(model(1989)).
bought(fish).
shops(mary).
end(model(1989)).

begin(model(1990)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1990)).

begin(model(1991)).
bought(spaghetti).
shops(mary).
end(model(1991)).

begin(model(1992)).
bought(spaghetti).
shops(mary).
end(model(1992)).

begin(model(1993)).
bought(fish).
shops(mary).
end(model(1993)).

begin(model(1994)).
bought(spaghetti).
shops(mary).
end(model(1994)).

begin(model(1995)).
bought(fish).
shops(mary).
end(model(1995)).

begin(model(1996)).
bought(spaghetti).
shops(mary).
end(model(1996)).

begin(model(1997)).
bought(fish).
shops(mary).
end(model(1997)).

begin(model(1998)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(1998)).

begin(model(1999)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1999)).

begin(model(2000)).
bought(fish).
shops(mary).
end(model(2000)).

begin(model(2001)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2001)).

begin(model(2002)).
bought(spaghetti).
shops(mary).
end(model(2002)).

begin(model(2003)).
end(model(2003)).

begin(model(2004)).
bought(spaghetti).
shops(mary).
end(model(2004)).

begin(model(2005)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2005)).

begin(model(2006)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2006)).

begin(model(2007)).
bought(spaghetti).
shops(john).
end(model(2007)).

begin(model(2008)).
bought(fish).
shops(mary).
end(model(2008)).

begin(model(2009)).
end(model(2009)).

begin(model(2010)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2010)).

begin(model(2011)).
bought(fish).
shops(mary).
end(model(2011)).

begin(model(2012)).
bought(spaghetti).
shops(mary).
end(model(2012)).

begin(model(2013)).
bought(spaghetti).
shops(mary).
end(model(2013)).

begin(model(2014)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2014)).

begin(model(2015)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2015)).

begin(model(2016)).
bought(spaghetti).
shops(mary).
end(model(2016)).

begin(model(2017)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2017)).

begin(model(2018)).
bought(fish).
shops(mary).
end(model(2018)).

begin(model(2019)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2019)).

begin(model(2020)).
bought(fish).
shops(mary).
end(model(2020)).

begin(model(2021)).
bought(spaghetti).
shops(mary).
end(model(2021)).

begin(model(2022)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2022)).

begin(model(2023)).
bought(fish).
shops(mary).
end(model(2023)).

begin(model(2024)).
bought(steak).
shops(john).
end(model(2024)).

begin(model(2025)).
bought(spaghetti).
shops(john).
end(model(2025)).

begin(model(2026)).
bought(spaghetti).
shops(mary).
end(model(2026)).

begin(model(2027)).
bought(fish).
shops(mary).
end(model(2027)).

begin(model(2028)).
bought(fish).
shops(mary).
end(model(2028)).

begin(model(2029)).
bought(fish).
shops(mary).
end(model(2029)).

begin(model(2030)).
end(model(2030)).

begin(model(2031)).
bought(spaghetti).
shops(mary).
end(model(2031)).

begin(model(2032)).
bought(fish).
shops(mary).
end(model(2032)).

begin(model(2033)).
bought(fish).
shops(mary).
end(model(2033)).

begin(model(2034)).
bought(fish).
shops(mary).
end(model(2034)).

begin(model(2035)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2035)).

begin(model(2036)).
bought(fish).
shops(mary).
end(model(2036)).

begin(model(2037)).
bought(spaghetti).
shops(mary).
end(model(2037)).

begin(model(2038)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2038)).

begin(model(2039)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2039)).

begin(model(2040)).
bought(fish).
shops(mary).
end(model(2040)).

begin(model(2041)).
bought(fish).
shops(mary).
end(model(2041)).

begin(model(2042)).
bought(spaghetti).
shops(mary).
end(model(2042)).

begin(model(2043)).
bought(spaghetti).
shops(mary).
end(model(2043)).

begin(model(2044)).
bought(fish).
shops(mary).
end(model(2044)).

begin(model(2045)).
bought(fish).
shops(mary).
end(model(2045)).

begin(model(2046)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2046)).

begin(model(2047)).
bought(fish).
shops(mary).
end(model(2047)).

begin(model(2048)).
bought(spaghetti).
shops(mary).
end(model(2048)).

begin(model(2049)).
bought(spaghetti).
shops(mary).
end(model(2049)).

begin(model(2050)).
bought(fish).
shops(mary).
end(model(2050)).

begin(model(2051)).
bought(spaghetti).
shops(mary).
end(model(2051)).

begin(model(2052)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2052)).

begin(model(2053)).
bought(fish).
shops(mary).
end(model(2053)).

begin(model(2054)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2054)).

begin(model(2055)).
bought(fish).
shops(mary).
end(model(2055)).

begin(model(2056)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2056)).

begin(model(2057)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2057)).

begin(model(2058)).
bought(fish).
shops(mary).
end(model(2058)).

begin(model(2059)).
bought(fish).
shops(mary).
end(model(2059)).

begin(model(2060)).
bought(spaghetti).
shops(mary).
end(model(2060)).

begin(model(2061)).
bought(fish).
shops(mary).
end(model(2061)).

begin(model(2062)).
bought(fish).
shops(mary).
end(model(2062)).

begin(model(2063)).
bought(fish).
shops(mary).
end(model(2063)).

begin(model(2064)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2064)).

begin(model(2065)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2065)).

begin(model(2066)).
bought(fish).
shops(mary).
end(model(2066)).

begin(model(2067)).
bought(spaghetti).
shops(mary).
end(model(2067)).

begin(model(2068)).
bought(fish).
shops(mary).
end(model(2068)).

begin(model(2069)).
bought(fish).
shops(mary).
end(model(2069)).

begin(model(2070)).
bought(spaghetti).
shops(mary).
end(model(2070)).

begin(model(2071)).
end(model(2071)).

begin(model(2072)).
bought(fish).
shops(mary).
end(model(2072)).

begin(model(2073)).
end(model(2073)).

begin(model(2074)).
bought(fish).
shops(mary).
end(model(2074)).

begin(model(2075)).
bought(spaghetti).
shops(mary).
end(model(2075)).

begin(model(2076)).
bought(fish).
shops(mary).
end(model(2076)).

begin(model(2077)).
bought(fish).
shops(mary).
end(model(2077)).

begin(model(2078)).
bought(fish).
shops(mary).
end(model(2078)).

begin(model(2079)).
bought(fish).
shops(mary).
end(model(2079)).

begin(model(2080)).
bought(fish).
shops(mary).
end(model(2080)).

begin(model(2081)).
end(model(2081)).

begin(model(2082)).
end(model(2082)).

begin(model(2083)).
bought(fish).
shops(mary).
end(model(2083)).

begin(model(2084)).
bought(fish).
shops(mary).
end(model(2084)).

begin(model(2085)).
bought(spaghetti).
shops(mary).
end(model(2085)).

begin(model(2086)).
bought(spaghetti).
shops(john).
end(model(2086)).

begin(model(2087)).
end(model(2087)).

begin(model(2088)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2088)).

begin(model(2089)).
bought(fish).
shops(mary).
end(model(2089)).

begin(model(2090)).
end(model(2090)).

begin(model(2091)).
bought(fish).
shops(mary).
end(model(2091)).

begin(model(2092)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2092)).

begin(model(2093)).
bought(spaghetti).
shops(john).
end(model(2093)).

begin(model(2094)).
bought(spaghetti).
shops(mary).
end(model(2094)).

begin(model(2095)).
bought(fish).
shops(mary).
end(model(2095)).

begin(model(2096)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2096)).

begin(model(2097)).
bought(fish).
shops(mary).
end(model(2097)).

begin(model(2098)).
end(model(2098)).

begin(model(2099)).
bought(spaghetti).
shops(mary).
end(model(2099)).

begin(model(2100)).
bought(fish).
shops(mary).
end(model(2100)).

begin(model(2101)).
bought(fish).
shops(mary).
end(model(2101)).

begin(model(2102)).
bought(fish).
shops(mary).
end(model(2102)).

begin(model(2103)).
bought(fish).
shops(mary).
end(model(2103)).

begin(model(2104)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2104)).

begin(model(2105)).
bought(fish).
shops(mary).
end(model(2105)).

begin(model(2106)).
bought(fish).
shops(mary).
end(model(2106)).

begin(model(2107)).
bought(spaghetti).
shops(mary).
end(model(2107)).

begin(model(2108)).
bought(fish).
shops(mary).
end(model(2108)).

begin(model(2109)).
bought(fish).
shops(mary).
end(model(2109)).

begin(model(2110)).
bought(fish).
shops(mary).
end(model(2110)).

begin(model(2111)).
bought(fish).
shops(mary).
end(model(2111)).

begin(model(2112)).
bought(spaghetti).
shops(mary).
end(model(2112)).

begin(model(2113)).
bought(fish).
shops(mary).
end(model(2113)).

begin(model(2114)).
bought(spaghetti).
shops(mary).
end(model(2114)).

begin(model(2115)).
bought(fish).
shops(mary).
end(model(2115)).

begin(model(2116)).
bought(spaghetti).
shops(mary).
end(model(2116)).

begin(model(2117)).
bought(fish).
shops(mary).
end(model(2117)).

begin(model(2118)).
bought(spaghetti).
shops(mary).
end(model(2118)).

begin(model(2119)).
bought(fish).
shops(mary).
end(model(2119)).

begin(model(2120)).
bought(spaghetti).
shops(mary).
end(model(2120)).

begin(model(2121)).
bought(spaghetti).
shops(mary).
end(model(2121)).

begin(model(2122)).
bought(spaghetti).
shops(mary).
end(model(2122)).

begin(model(2123)).
bought(fish).
shops(mary).
end(model(2123)).

begin(model(2124)).
bought(spaghetti).
shops(mary).
end(model(2124)).

begin(model(2125)).
bought(fish).
shops(mary).
end(model(2125)).

begin(model(2126)).
end(model(2126)).

begin(model(2127)).
bought(fish).
shops(mary).
end(model(2127)).

begin(model(2128)).
bought(spaghetti).
shops(mary).
end(model(2128)).

begin(model(2129)).
bought(fish).
shops(mary).
end(model(2129)).

begin(model(2130)).
bought(spaghetti).
shops(mary).
end(model(2130)).

begin(model(2131)).
bought(spaghetti).
shops(mary).
end(model(2131)).

begin(model(2132)).
bought(fish).
shops(mary).
end(model(2132)).

begin(model(2133)).
bought(fish).
shops(mary).
end(model(2133)).

begin(model(2134)).
bought(spaghetti).
shops(mary).
end(model(2134)).

begin(model(2135)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2135)).

begin(model(2136)).
bought(fish).
shops(mary).
end(model(2136)).

begin(model(2137)).
bought(spaghetti).
shops(mary).
end(model(2137)).

begin(model(2138)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2138)).

begin(model(2139)).
bought(fish).
shops(mary).
end(model(2139)).

begin(model(2140)).
bought(fish).
shops(mary).
end(model(2140)).

begin(model(2141)).
end(model(2141)).

begin(model(2142)).
bought(fish).
shops(mary).
end(model(2142)).

begin(model(2143)).
end(model(2143)).

begin(model(2144)).
bought(fish).
shops(mary).
end(model(2144)).

begin(model(2145)).
bought(fish).
shops(mary).
end(model(2145)).

begin(model(2146)).
bought(fish).
shops(mary).
end(model(2146)).

begin(model(2147)).
bought(fish).
shops(mary).
end(model(2147)).

begin(model(2148)).
bought(fish).
shops(mary).
end(model(2148)).

begin(model(2149)).
bought(fish).
shops(mary).
end(model(2149)).

begin(model(2150)).
bought(fish).
shops(mary).
end(model(2150)).

begin(model(2151)).
bought(fish).
shops(mary).
end(model(2151)).

begin(model(2152)).
bought(fish).
shops(mary).
end(model(2152)).

begin(model(2153)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2153)).

begin(model(2154)).
bought(fish).
shops(mary).
end(model(2154)).

begin(model(2155)).
end(model(2155)).

begin(model(2156)).
bought(fish).
shops(mary).
end(model(2156)).

begin(model(2157)).
bought(fish).
shops(mary).
end(model(2157)).

begin(model(2158)).
bought(fish).
shops(mary).
end(model(2158)).

begin(model(2159)).
bought(spaghetti).
shops(mary).
end(model(2159)).

begin(model(2160)).
bought(spaghetti).
shops(mary).
end(model(2160)).

begin(model(2161)).
bought(fish).
shops(mary).
end(model(2161)).

begin(model(2162)).
bought(fish).
shops(mary).
end(model(2162)).

begin(model(2163)).
bought(fish).
shops(mary).
end(model(2163)).

begin(model(2164)).
bought(fish).
shops(mary).
end(model(2164)).

begin(model(2165)).
bought(fish).
shops(mary).
end(model(2165)).

begin(model(2166)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2166)).

begin(model(2167)).
bought(fish).
shops(mary).
end(model(2167)).

begin(model(2168)).
bought(fish).
shops(mary).
end(model(2168)).

begin(model(2169)).
bought(fish).
shops(mary).
end(model(2169)).

begin(model(2170)).
bought(spaghetti).
shops(mary).
end(model(2170)).

begin(model(2171)).
bought(fish).
shops(mary).
end(model(2171)).

begin(model(2172)).
bought(fish).
shops(mary).
end(model(2172)).

begin(model(2173)).
end(model(2173)).

begin(model(2174)).
bought(fish).
shops(mary).
end(model(2174)).

begin(model(2175)).
bought(fish).
shops(mary).
end(model(2175)).

begin(model(2176)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2176)).

begin(model(2177)).
bought(spaghetti).
shops(mary).
end(model(2177)).

begin(model(2178)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2178)).

begin(model(2179)).
bought(spaghetti).
shops(mary).
end(model(2179)).

begin(model(2180)).
bought(fish).
shops(mary).
end(model(2180)).

begin(model(2181)).
end(model(2181)).

begin(model(2182)).
bought(fish).
shops(mary).
end(model(2182)).

begin(model(2183)).
bought(spaghetti).
shops(mary).
end(model(2183)).

begin(model(2184)).
bought(spaghetti).
shops(mary).
end(model(2184)).

begin(model(2185)).
bought(spaghetti).
shops(mary).
end(model(2185)).

begin(model(2186)).
bought(fish).
shops(mary).
end(model(2186)).

begin(model(2187)).
bought(fish).
shops(mary).
end(model(2187)).

begin(model(2188)).
bought(spaghetti).
shops(mary).
end(model(2188)).

begin(model(2189)).
bought(fish).
shops(mary).
end(model(2189)).

begin(model(2190)).
bought(fish).
shops(mary).
end(model(2190)).

begin(model(2191)).
bought(fish).
shops(mary).
end(model(2191)).

begin(model(2192)).
bought(fish).
shops(mary).
end(model(2192)).

begin(model(2193)).
bought(fish).
shops(mary).
end(model(2193)).

begin(model(2194)).
bought(spaghetti).
shops(mary).
end(model(2194)).

begin(model(2195)).
bought(spaghetti).
shops(mary).
end(model(2195)).

begin(model(2196)).
bought(steak).
shops(john).
end(model(2196)).

begin(model(2197)).
bought(fish).
shops(mary).
end(model(2197)).

begin(model(2198)).
end(model(2198)).

begin(model(2199)).
bought(fish).
shops(mary).
end(model(2199)).

begin(model(2200)).
bought(spaghetti).
shops(mary).
end(model(2200)).

begin(model(2201)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2201)).

begin(model(2202)).
bought(spaghetti).
shops(mary).
end(model(2202)).

begin(model(2203)).
bought(fish).
shops(mary).
end(model(2203)).

begin(model(2204)).
bought(fish).
shops(mary).
end(model(2204)).

begin(model(2205)).
bought(fish).
shops(mary).
end(model(2205)).

begin(model(2206)).
bought(fish).
shops(mary).
end(model(2206)).

begin(model(2207)).
bought(fish).
shops(mary).
end(model(2207)).

begin(model(2208)).
bought(fish).
shops(mary).
end(model(2208)).

begin(model(2209)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2209)).

begin(model(2210)).
bought(fish).
shops(mary).
end(model(2210)).

begin(model(2211)).
bought(spaghetti).
shops(mary).
end(model(2211)).

begin(model(2212)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2212)).

begin(model(2213)).
bought(spaghetti).
shops(mary).
end(model(2213)).

begin(model(2214)).
bought(fish).
shops(mary).
end(model(2214)).

begin(model(2215)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2215)).

begin(model(2216)).
end(model(2216)).

begin(model(2217)).
bought(fish).
shops(mary).
end(model(2217)).

begin(model(2218)).
bought(fish).
shops(mary).
end(model(2218)).

begin(model(2219)).
bought(fish).
shops(mary).
end(model(2219)).

begin(model(2220)).
bought(spaghetti).
shops(mary).
end(model(2220)).

begin(model(2221)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2221)).

begin(model(2222)).
bought(fish).
shops(mary).
end(model(2222)).

begin(model(2223)).
bought(fish).
shops(mary).
end(model(2223)).

begin(model(2224)).
bought(fish).
shops(mary).
end(model(2224)).

begin(model(2225)).
end(model(2225)).

begin(model(2226)).
bought(fish).
shops(mary).
end(model(2226)).

begin(model(2227)).
bought(fish).
shops(mary).
end(model(2227)).

begin(model(2228)).
bought(fish).
shops(mary).
end(model(2228)).

begin(model(2229)).
bought(fish).
shops(mary).
end(model(2229)).

begin(model(2230)).
bought(fish).
shops(mary).
end(model(2230)).

begin(model(2231)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2231)).

begin(model(2232)).
bought(fish).
shops(mary).
end(model(2232)).

begin(model(2233)).
bought(fish).
shops(mary).
end(model(2233)).

begin(model(2234)).
bought(fish).
shops(mary).
end(model(2234)).

begin(model(2235)).
bought(spaghetti).
shops(mary).
end(model(2235)).

begin(model(2236)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2236)).

begin(model(2237)).
bought(fish).
shops(mary).
end(model(2237)).

begin(model(2238)).
bought(spaghetti).
shops(mary).
end(model(2238)).

begin(model(2239)).
bought(spaghetti).
shops(mary).
end(model(2239)).

begin(model(2240)).
bought(spaghetti).
shops(mary).
end(model(2240)).

begin(model(2241)).
bought(fish).
shops(mary).
end(model(2241)).

begin(model(2242)).
bought(spaghetti).
shops(mary).
end(model(2242)).

begin(model(2243)).
bought(spaghetti).
shops(mary).
end(model(2243)).

begin(model(2244)).
bought(fish).
shops(mary).
end(model(2244)).

begin(model(2245)).
bought(fish).
shops(mary).
end(model(2245)).

begin(model(2246)).
bought(spaghetti).
shops(mary).
end(model(2246)).

begin(model(2247)).
bought(fish).
shops(mary).
end(model(2247)).

begin(model(2248)).
bought(fish).
shops(mary).
end(model(2248)).

begin(model(2249)).
bought(fish).
shops(mary).
end(model(2249)).

begin(model(2250)).
bought(spaghetti).
shops(mary).
end(model(2250)).

begin(model(2251)).
bought(spaghetti).
shops(mary).
end(model(2251)).

begin(model(2252)).
bought(spaghetti).
shops(mary).
end(model(2252)).

begin(model(2253)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2253)).

begin(model(2254)).
end(model(2254)).

begin(model(2255)).
end(model(2255)).

begin(model(2256)).
bought(fish).
shops(mary).
end(model(2256)).

begin(model(2257)).
bought(fish).
shops(mary).
end(model(2257)).

begin(model(2258)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2258)).

begin(model(2259)).
bought(fish).
shops(mary).
end(model(2259)).

begin(model(2260)).
bought(spaghetti).
shops(mary).
end(model(2260)).

begin(model(2261)).
bought(fish).
shops(mary).
end(model(2261)).

begin(model(2262)).
end(model(2262)).

begin(model(2263)).
bought(fish).
shops(mary).
end(model(2263)).

begin(model(2264)).
end(model(2264)).

begin(model(2265)).
bought(fish).
shops(mary).
end(model(2265)).

begin(model(2266)).
bought(fish).
shops(mary).
end(model(2266)).

begin(model(2267)).
bought(fish).
shops(mary).
end(model(2267)).

begin(model(2268)).
bought(fish).
shops(mary).
end(model(2268)).

begin(model(2269)).
bought(spaghetti).
shops(mary).
end(model(2269)).

begin(model(2270)).
bought(fish).
shops(mary).
end(model(2270)).

begin(model(2271)).
bought(fish).
shops(mary).
end(model(2271)).

begin(model(2272)).
bought(fish).
shops(mary).
end(model(2272)).

begin(model(2273)).
bought(fish).
shops(mary).
end(model(2273)).

begin(model(2274)).
bought(fish).
shops(mary).
end(model(2274)).

begin(model(2275)).
bought(spaghetti).
shops(mary).
end(model(2275)).

begin(model(2276)).
bought(fish).
shops(mary).
end(model(2276)).

begin(model(2277)).
bought(fish).
shops(mary).
end(model(2277)).

begin(model(2278)).
bought(fish).
shops(mary).
end(model(2278)).

begin(model(2279)).
bought(fish).
shops(mary).
end(model(2279)).

begin(model(2280)).
bought(fish).
shops(mary).
end(model(2280)).

begin(model(2281)).
end(model(2281)).

begin(model(2282)).
bought(spaghetti).
shops(mary).
end(model(2282)).

begin(model(2283)).
bought(fish).
shops(mary).
end(model(2283)).

begin(model(2284)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2284)).

begin(model(2285)).
bought(fish).
shops(mary).
end(model(2285)).

begin(model(2286)).
bought(fish).
shops(mary).
end(model(2286)).

begin(model(2287)).
bought(fish).
shops(mary).
end(model(2287)).

begin(model(2288)).
bought(fish).
shops(mary).
end(model(2288)).

begin(model(2289)).
bought(fish).
shops(mary).
end(model(2289)).

begin(model(2290)).
bought(fish).
shops(mary).
end(model(2290)).

begin(model(2291)).
bought(fish).
shops(mary).
end(model(2291)).

begin(model(2292)).
bought(fish).
shops(mary).
end(model(2292)).

begin(model(2293)).
bought(spaghetti).
shops(mary).
end(model(2293)).

begin(model(2294)).
bought(spaghetti).
shops(mary).
end(model(2294)).

begin(model(2295)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2295)).

begin(model(2296)).
bought(spaghetti).
shops(mary).
end(model(2296)).

begin(model(2297)).
bought(fish).
shops(mary).
end(model(2297)).

begin(model(2298)).
bought(fish).
shops(mary).
end(model(2298)).

begin(model(2299)).
bought(spaghetti).
shops(mary).
end(model(2299)).

begin(model(2300)).
end(model(2300)).

begin(model(2301)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2301)).

begin(model(2302)).
bought(spaghetti).
shops(mary).
end(model(2302)).

begin(model(2303)).
bought(spaghetti).
shops(mary).
end(model(2303)).

begin(model(2304)).
bought(fish).
shops(mary).
end(model(2304)).

begin(model(2305)).
bought(fish).
shops(mary).
end(model(2305)).

begin(model(2306)).
bought(spaghetti).
shops(mary).
end(model(2306)).

begin(model(2307)).
bought(spaghetti).
shops(mary).
end(model(2307)).

begin(model(2308)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2308)).

begin(model(2309)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2309)).

begin(model(2310)).
bought(fish).
shops(mary).
end(model(2310)).

begin(model(2311)).
end(model(2311)).

begin(model(2312)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2312)).

begin(model(2313)).
bought(spaghetti).
shops(mary).
end(model(2313)).

begin(model(2314)).
bought(spaghetti).
shops(mary).
end(model(2314)).

begin(model(2315)).
bought(spaghetti).
shops(mary).
end(model(2315)).

begin(model(2316)).
bought(fish).
shops(mary).
end(model(2316)).

begin(model(2317)).
bought(fish).
shops(mary).
end(model(2317)).

begin(model(2318)).
bought(fish).
shops(mary).
end(model(2318)).

begin(model(2319)).
end(model(2319)).

begin(model(2320)).
bought(fish).
shops(mary).
end(model(2320)).

begin(model(2321)).
bought(fish).
shops(mary).
end(model(2321)).

begin(model(2322)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2322)).

begin(model(2323)).
bought(fish).
shops(mary).
end(model(2323)).

begin(model(2324)).
bought(fish).
shops(mary).
end(model(2324)).

begin(model(2325)).
bought(fish).
shops(mary).
end(model(2325)).

begin(model(2326)).
end(model(2326)).

begin(model(2327)).
bought(fish).
shops(mary).
end(model(2327)).

begin(model(2328)).
bought(fish).
shops(mary).
end(model(2328)).

begin(model(2329)).
bought(fish).
shops(mary).
end(model(2329)).

begin(model(2330)).
bought(fish).
shops(mary).
end(model(2330)).

begin(model(2331)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2331)).

begin(model(2332)).
bought(fish).
shops(mary).
end(model(2332)).

begin(model(2333)).
bought(fish).
shops(mary).
end(model(2333)).

begin(model(2334)).
end(model(2334)).

begin(model(2335)).
bought(fish).
shops(mary).
end(model(2335)).

begin(model(2336)).
bought(fish).
shops(mary).
end(model(2336)).

begin(model(2337)).
bought(spaghetti).
shops(mary).
end(model(2337)).

begin(model(2338)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2338)).

begin(model(2339)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2339)).

begin(model(2340)).
bought(spaghetti).
shops(mary).
end(model(2340)).

begin(model(2341)).
bought(fish).
shops(mary).
end(model(2341)).

begin(model(2342)).
bought(fish).
shops(mary).
end(model(2342)).

begin(model(2343)).
bought(spaghetti).
shops(mary).
end(model(2343)).

begin(model(2344)).
bought(spaghetti).
shops(mary).
end(model(2344)).

begin(model(2345)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2345)).

begin(model(2346)).
end(model(2346)).

begin(model(2347)).
bought(fish).
shops(mary).
end(model(2347)).

begin(model(2348)).
bought(fish).
shops(mary).
end(model(2348)).

begin(model(2349)).
bought(fish).
shops(mary).
end(model(2349)).

begin(model(2350)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2350)).

begin(model(2351)).
bought(spaghetti).
shops(mary).
end(model(2351)).

begin(model(2352)).
bought(spaghetti).
shops(mary).
end(model(2352)).

begin(model(2353)).
bought(fish).
shops(mary).
end(model(2353)).

begin(model(2354)).
bought(spaghetti).
shops(mary).
end(model(2354)).

begin(model(2355)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2355)).

begin(model(2356)).
end(model(2356)).

begin(model(2357)).
bought(fish).
shops(mary).
end(model(2357)).

begin(model(2358)).
bought(fish).
shops(mary).
end(model(2358)).

begin(model(2359)).
end(model(2359)).

begin(model(2360)).
bought(fish).
shops(mary).
end(model(2360)).

begin(model(2361)).
bought(fish).
shops(mary).
end(model(2361)).

begin(model(2362)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2362)).

begin(model(2363)).
bought(spaghetti).
shops(mary).
end(model(2363)).

begin(model(2364)).
end(model(2364)).

begin(model(2365)).
bought(spaghetti).
shops(john).
end(model(2365)).

begin(model(2366)).
bought(fish).
shops(mary).
end(model(2366)).

begin(model(2367)).
bought(fish).
shops(mary).
end(model(2367)).

begin(model(2368)).
bought(fish).
shops(mary).
end(model(2368)).

begin(model(2369)).
bought(fish).
shops(mary).
end(model(2369)).

begin(model(2370)).
bought(fish).
shops(mary).
end(model(2370)).

begin(model(2371)).
bought(spaghetti).
shops(mary).
end(model(2371)).

begin(model(2372)).
bought(spaghetti).
shops(mary).
end(model(2372)).

begin(model(2373)).
bought(fish).
shops(mary).
end(model(2373)).

begin(model(2374)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2374)).

begin(model(2375)).
bought(fish).
shops(mary).
end(model(2375)).

begin(model(2376)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2376)).

begin(model(2377)).
end(model(2377)).

begin(model(2378)).
bought(fish).
shops(mary).
end(model(2378)).

begin(model(2379)).
end(model(2379)).

begin(model(2380)).
bought(fish).
shops(mary).
end(model(2380)).

begin(model(2381)).
bought(fish).
shops(mary).
end(model(2381)).

begin(model(2382)).
bought(fish).
shops(mary).
end(model(2382)).

begin(model(2383)).
bought(spaghetti).
shops(mary).
end(model(2383)).

begin(model(2384)).
bought(fish).
shops(mary).
end(model(2384)).

begin(model(2385)).
bought(spaghetti).
shops(mary).
end(model(2385)).

begin(model(2386)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2386)).

begin(model(2387)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2387)).

begin(model(2388)).
bought(fish).
shops(mary).
end(model(2388)).

begin(model(2389)).
bought(spaghetti).
shops(mary).
end(model(2389)).

begin(model(2390)).
bought(spaghetti).
shops(mary).
end(model(2390)).

begin(model(2391)).
bought(spaghetti).
shops(mary).
end(model(2391)).

begin(model(2392)).
bought(spaghetti).
shops(mary).
end(model(2392)).

begin(model(2393)).
bought(fish).
shops(mary).
end(model(2393)).

begin(model(2394)).
bought(fish).
shops(mary).
end(model(2394)).

begin(model(2395)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2395)).

begin(model(2396)).
bought(spaghetti).
shops(mary).
end(model(2396)).

begin(model(2397)).
bought(spaghetti).
shops(mary).
end(model(2397)).

begin(model(2398)).
bought(fish).
shops(mary).
end(model(2398)).

begin(model(2399)).
bought(spaghetti).
shops(mary).
end(model(2399)).

begin(model(2400)).
end(model(2400)).

begin(model(2401)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2401)).

begin(model(2402)).
bought(fish).
shops(mary).
end(model(2402)).

begin(model(2403)).
bought(fish).
shops(mary).
end(model(2403)).

begin(model(2404)).
bought(fish).
shops(mary).
end(model(2404)).

begin(model(2405)).
bought(fish).
shops(mary).
end(model(2405)).

begin(model(2406)).
bought(spaghetti).
shops(john).
end(model(2406)).

begin(model(2407)).
bought(fish).
shops(mary).
end(model(2407)).

begin(model(2408)).
bought(spaghetti).
shops(mary).
end(model(2408)).

begin(model(2409)).
bought(fish).
shops(mary).
end(model(2409)).

begin(model(2410)).
bought(spaghetti).
shops(mary).
end(model(2410)).

begin(model(2411)).
bought(fish).
shops(mary).
end(model(2411)).

begin(model(2412)).
end(model(2412)).

begin(model(2413)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2413)).

begin(model(2414)).
bought(spaghetti).
shops(mary).
end(model(2414)).

begin(model(2415)).
end(model(2415)).

begin(model(2416)).
bought(fish).
shops(mary).
end(model(2416)).

begin(model(2417)).
bought(fish).
shops(mary).
end(model(2417)).

begin(model(2418)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2418)).

begin(model(2419)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2419)).

begin(model(2420)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2420)).

begin(model(2421)).
end(model(2421)).

begin(model(2422)).
bought(steak).
shops(john).
end(model(2422)).

begin(model(2423)).
bought(spaghetti).
shops(mary).
end(model(2423)).

begin(model(2424)).
bought(fish).
shops(mary).
end(model(2424)).

begin(model(2425)).
bought(fish).
shops(mary).
end(model(2425)).

begin(model(2426)).
bought(spaghetti).
shops(mary).
end(model(2426)).

begin(model(2427)).
end(model(2427)).

begin(model(2428)).
bought(fish).
shops(mary).
end(model(2428)).

begin(model(2429)).
bought(fish).
shops(mary).
end(model(2429)).

begin(model(2430)).
bought(spaghetti).
shops(mary).
end(model(2430)).

begin(model(2431)).
bought(fish).
shops(mary).
end(model(2431)).

begin(model(2432)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2432)).

begin(model(2433)).
bought(fish).
shops(mary).
end(model(2433)).

begin(model(2434)).
bought(fish).
shops(mary).
end(model(2434)).

begin(model(2435)).
end(model(2435)).

begin(model(2436)).
bought(fish).
shops(mary).
end(model(2436)).

begin(model(2437)).
bought(spaghetti).
shops(mary).
end(model(2437)).

begin(model(2438)).
bought(fish).
shops(mary).
end(model(2438)).

begin(model(2439)).
bought(spaghetti).
shops(mary).
end(model(2439)).

begin(model(2440)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2440)).

begin(model(2441)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2441)).

begin(model(2442)).
bought(fish).
shops(mary).
end(model(2442)).

begin(model(2443)).
bought(fish).
shops(mary).
end(model(2443)).

begin(model(2444)).
bought(fish).
shops(mary).
end(model(2444)).

begin(model(2445)).
bought(fish).
shops(mary).
end(model(2445)).

begin(model(2446)).
bought(spaghetti).
shops(mary).
end(model(2446)).

begin(model(2447)).
bought(fish).
shops(mary).
end(model(2447)).

begin(model(2448)).
bought(fish).
shops(mary).
end(model(2448)).

begin(model(2449)).
bought(fish).
shops(mary).
end(model(2449)).

begin(model(2450)).
bought(fish).
shops(mary).
end(model(2450)).

begin(model(2451)).
bought(spaghetti).
shops(mary).
end(model(2451)).

begin(model(2452)).
bought(fish).
shops(mary).
end(model(2452)).

begin(model(2453)).
bought(fish).
shops(mary).
end(model(2453)).

begin(model(2454)).
bought(fish).
shops(mary).
end(model(2454)).

begin(model(2455)).
bought(spaghetti).
shops(mary).
end(model(2455)).

begin(model(2456)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2456)).

begin(model(2457)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2457)).

begin(model(2458)).
bought(spaghetti).
shops(mary).
end(model(2458)).

begin(model(2459)).
bought(fish).
shops(mary).
end(model(2459)).

begin(model(2460)).
bought(spaghetti).
shops(john).
end(model(2460)).

begin(model(2461)).
bought(fish).
shops(mary).
end(model(2461)).

begin(model(2462)).
bought(fish).
shops(mary).
end(model(2462)).

begin(model(2463)).
bought(fish).
shops(mary).
end(model(2463)).

begin(model(2464)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2464)).

begin(model(2465)).
bought(fish).
shops(mary).
end(model(2465)).

begin(model(2466)).
end(model(2466)).

begin(model(2467)).
bought(fish).
shops(mary).
end(model(2467)).

begin(model(2468)).
bought(fish).
shops(mary).
end(model(2468)).

begin(model(2469)).
bought(fish).
shops(mary).
end(model(2469)).

begin(model(2470)).
bought(fish).
shops(mary).
end(model(2470)).

begin(model(2471)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2471)).

begin(model(2472)).
end(model(2472)).

begin(model(2473)).
bought(fish).
shops(mary).
end(model(2473)).

begin(model(2474)).
end(model(2474)).

begin(model(2475)).
bought(fish).
shops(mary).
end(model(2475)).

begin(model(2476)).
bought(fish).
shops(mary).
end(model(2476)).

begin(model(2477)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2477)).

begin(model(2478)).
bought(spaghetti).
shops(mary).
end(model(2478)).

begin(model(2479)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2479)).

begin(model(2480)).
bought(spaghetti).
shops(mary).
end(model(2480)).

begin(model(2481)).
bought(fish).
shops(mary).
end(model(2481)).

begin(model(2482)).
bought(fish).
shops(mary).
end(model(2482)).

begin(model(2483)).
bought(fish).
shops(mary).
end(model(2483)).

begin(model(2484)).
bought(fish).
shops(mary).
end(model(2484)).

begin(model(2485)).
end(model(2485)).

begin(model(2486)).
bought(fish).
shops(mary).
end(model(2486)).

begin(model(2487)).
bought(fish).
shops(mary).
end(model(2487)).

begin(model(2488)).
bought(spaghetti).
shops(mary).
end(model(2488)).

begin(model(2489)).
bought(spaghetti).
shops(mary).
end(model(2489)).

begin(model(2490)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2490)).

begin(model(2491)).
bought(fish).
shops(mary).
end(model(2491)).

begin(model(2492)).
bought(spaghetti).
shops(mary).
end(model(2492)).

begin(model(2493)).
bought(fish).
shops(mary).
end(model(2493)).

begin(model(2494)).
bought(fish).
shops(mary).
end(model(2494)).

begin(model(2495)).
bought(fish).
shops(mary).
end(model(2495)).

begin(model(2496)).
bought(fish).
shops(mary).
end(model(2496)).

begin(model(2497)).
bought(fish).
shops(mary).
end(model(2497)).

begin(model(2498)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2498)).

begin(model(2499)).
bought(fish).
shops(mary).
end(model(2499)).

begin(model(2500)).
bought(fish).
shops(mary).
end(model(2500)).

begin(model(2501)).
bought(fish).
shops(mary).
end(model(2501)).

begin(model(2502)).
bought(spaghetti).
shops(mary).
end(model(2502)).

begin(model(2503)).
bought(fish).
shops(mary).
end(model(2503)).

begin(model(2504)).
end(model(2504)).

begin(model(2505)).
bought(spaghetti).
shops(mary).
end(model(2505)).

begin(model(2506)).
bought(fish).
shops(mary).
end(model(2506)).

begin(model(2507)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2507)).

begin(model(2508)).
bought(fish).
shops(mary).
end(model(2508)).

begin(model(2509)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2509)).

begin(model(2510)).
bought(fish).
shops(mary).
end(model(2510)).

begin(model(2511)).
bought(fish).
shops(mary).
end(model(2511)).

begin(model(2512)).
bought(fish).
shops(mary).
end(model(2512)).

begin(model(2513)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2513)).

begin(model(2514)).
bought(fish).
shops(mary).
end(model(2514)).

begin(model(2515)).
bought(fish).
shops(mary).
end(model(2515)).

begin(model(2516)).
bought(spaghetti).
shops(mary).
end(model(2516)).

begin(model(2517)).
bought(spaghetti).
shops(mary).
end(model(2517)).

begin(model(2518)).
end(model(2518)).

begin(model(2519)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2519)).

begin(model(2520)).
bought(fish).
shops(mary).
end(model(2520)).

begin(model(2521)).
bought(spaghetti).
shops(mary).
end(model(2521)).

begin(model(2522)).
bought(spaghetti).
shops(mary).
end(model(2522)).

begin(model(2523)).
bought(fish).
shops(mary).
end(model(2523)).

begin(model(2524)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2524)).

begin(model(2525)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2525)).

begin(model(2526)).
bought(fish).
shops(mary).
end(model(2526)).

begin(model(2527)).
bought(fish).
shops(mary).
end(model(2527)).

begin(model(2528)).
bought(fish).
shops(mary).
end(model(2528)).

begin(model(2529)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2529)).

begin(model(2530)).
bought(spaghetti).
shops(mary).
end(model(2530)).

begin(model(2531)).
bought(fish).
shops(mary).
end(model(2531)).

begin(model(2532)).
bought(fish).
shops(mary).
end(model(2532)).

begin(model(2533)).
bought(spaghetti).
shops(mary).
end(model(2533)).

begin(model(2534)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2534)).

begin(model(2535)).
bought(spaghetti).
shops(mary).
end(model(2535)).

begin(model(2536)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2536)).

begin(model(2537)).
bought(spaghetti).
shops(mary).
end(model(2537)).

begin(model(2538)).
bought(fish).
shops(mary).
end(model(2538)).

begin(model(2539)).
bought(fish).
shops(mary).
end(model(2539)).

begin(model(2540)).
end(model(2540)).

begin(model(2541)).
bought(fish).
shops(mary).
end(model(2541)).

begin(model(2542)).
bought(spaghetti).
shops(mary).
end(model(2542)).

begin(model(2543)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2543)).

begin(model(2544)).
bought(fish).
shops(mary).
end(model(2544)).

begin(model(2545)).
end(model(2545)).

begin(model(2546)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2546)).

begin(model(2547)).
bought(spaghetti).
shops(mary).
end(model(2547)).

begin(model(2548)).
bought(fish).
shops(mary).
end(model(2548)).

begin(model(2549)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2549)).

begin(model(2550)).
bought(spaghetti).
shops(mary).
end(model(2550)).

begin(model(2551)).
bought(spaghetti).
shops(mary).
end(model(2551)).

begin(model(2552)).
bought(spaghetti).
shops(mary).
end(model(2552)).

begin(model(2553)).
bought(spaghetti).
shops(mary).
end(model(2553)).

begin(model(2554)).
bought(spaghetti).
shops(mary).
end(model(2554)).

begin(model(2555)).
bought(spaghetti).
shops(mary).
end(model(2555)).

begin(model(2556)).
bought(fish).
shops(mary).
end(model(2556)).

begin(model(2557)).
bought(spaghetti).
shops(mary).
end(model(2557)).

begin(model(2558)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2558)).

begin(model(2559)).
bought(fish).
shops(mary).
end(model(2559)).

begin(model(2560)).
bought(spaghetti).
shops(mary).
end(model(2560)).

begin(model(2561)).
bought(fish).
shops(mary).
end(model(2561)).

begin(model(2562)).
bought(fish).
shops(mary).
end(model(2562)).

begin(model(2563)).
end(model(2563)).

begin(model(2564)).
bought(fish).
shops(mary).
end(model(2564)).

begin(model(2565)).
bought(fish).
shops(mary).
end(model(2565)).

begin(model(2566)).
bought(spaghetti).
shops(mary).
end(model(2566)).

begin(model(2567)).
bought(fish).
shops(mary).
end(model(2567)).

begin(model(2568)).
bought(fish).
shops(mary).
end(model(2568)).

begin(model(2569)).
bought(spaghetti).
shops(mary).
end(model(2569)).

begin(model(2570)).
bought(fish).
shops(mary).
end(model(2570)).

begin(model(2571)).
bought(fish).
shops(mary).
end(model(2571)).

begin(model(2572)).
bought(spaghetti).
shops(mary).
end(model(2572)).

begin(model(2573)).
bought(spaghetti).
shops(mary).
end(model(2573)).

begin(model(2574)).
bought(spaghetti).
shops(mary).
end(model(2574)).

begin(model(2575)).
bought(fish).
shops(mary).
end(model(2575)).

begin(model(2576)).
bought(fish).
shops(mary).
end(model(2576)).

begin(model(2577)).
bought(fish).
shops(mary).
end(model(2577)).

begin(model(2578)).
bought(fish).
shops(mary).
end(model(2578)).

begin(model(2579)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2579)).

begin(model(2580)).
bought(fish).
shops(mary).
end(model(2580)).

begin(model(2581)).
bought(fish).
shops(mary).
end(model(2581)).

begin(model(2582)).
bought(fish).
shops(mary).
end(model(2582)).

begin(model(2583)).
bought(fish).
shops(mary).
end(model(2583)).

begin(model(2584)).
bought(spaghetti).
shops(mary).
end(model(2584)).

begin(model(2585)).
bought(spaghetti).
shops(mary).
end(model(2585)).

begin(model(2586)).
bought(fish).
shops(mary).
end(model(2586)).

begin(model(2587)).
end(model(2587)).

begin(model(2588)).
bought(fish).
shops(mary).
end(model(2588)).

begin(model(2589)).
bought(fish).
shops(mary).
end(model(2589)).

begin(model(2590)).
bought(fish).
shops(mary).
end(model(2590)).

begin(model(2591)).
bought(spaghetti).
shops(mary).
end(model(2591)).

begin(model(2592)).
bought(fish).
shops(mary).
end(model(2592)).

begin(model(2593)).
bought(fish).
shops(mary).
end(model(2593)).

begin(model(2594)).
bought(fish).
shops(mary).
end(model(2594)).

begin(model(2595)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2595)).

begin(model(2596)).
bought(spaghetti).
shops(mary).
end(model(2596)).

begin(model(2597)).
bought(spaghetti).
shops(mary).
end(model(2597)).

begin(model(2598)).
bought(fish).
shops(mary).
end(model(2598)).

begin(model(2599)).
bought(spaghetti).
shops(mary).
end(model(2599)).

begin(model(2600)).
bought(fish).
shops(mary).
end(model(2600)).

begin(model(2601)).
bought(fish).
shops(mary).
end(model(2601)).

begin(model(2602)).
bought(fish).
shops(mary).
end(model(2602)).

begin(model(2603)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2603)).

begin(model(2604)).
bought(fish).
shops(mary).
end(model(2604)).

begin(model(2605)).
bought(fish).
shops(mary).
end(model(2605)).

begin(model(2606)).
bought(fish).
shops(mary).
end(model(2606)).

begin(model(2607)).
bought(fish).
shops(mary).
end(model(2607)).

begin(model(2608)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2608)).

begin(model(2609)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2609)).

begin(model(2610)).
bought(fish).
shops(mary).
end(model(2610)).

begin(model(2611)).
bought(spaghetti).
shops(mary).
end(model(2611)).

begin(model(2612)).
end(model(2612)).

begin(model(2613)).
bought(spaghetti).
shops(mary).
end(model(2613)).

begin(model(2614)).
bought(fish).
shops(mary).
end(model(2614)).

begin(model(2615)).
bought(fish).
shops(mary).
end(model(2615)).

begin(model(2616)).
bought(fish).
shops(mary).
end(model(2616)).

begin(model(2617)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2617)).

begin(model(2618)).
bought(spaghetti).
shops(mary).
end(model(2618)).

begin(model(2619)).
bought(spaghetti).
shops(mary).
end(model(2619)).

begin(model(2620)).
bought(spaghetti).
shops(mary).
end(model(2620)).

begin(model(2621)).
bought(fish).
shops(mary).
end(model(2621)).

begin(model(2622)).
end(model(2622)).

begin(model(2623)).
bought(fish).
shops(mary).
end(model(2623)).

begin(model(2624)).
bought(spaghetti).
shops(mary).
end(model(2624)).

begin(model(2625)).
bought(fish).
shops(mary).
end(model(2625)).

begin(model(2626)).
bought(spaghetti).
shops(mary).
end(model(2626)).

begin(model(2627)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2627)).

begin(model(2628)).
bought(fish).
shops(mary).
end(model(2628)).

begin(model(2629)).
bought(fish).
shops(mary).
end(model(2629)).

begin(model(2630)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2630)).

begin(model(2631)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2631)).

begin(model(2632)).
bought(fish).
shops(mary).
end(model(2632)).

begin(model(2633)).
bought(spaghetti).
shops(mary).
end(model(2633)).

begin(model(2634)).
bought(spaghetti).
shops(mary).
end(model(2634)).

begin(model(2635)).
bought(fish).
shops(mary).
end(model(2635)).

begin(model(2636)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2636)).

begin(model(2637)).
bought(fish).
shops(mary).
end(model(2637)).

begin(model(2638)).
bought(fish).
shops(mary).
end(model(2638)).

begin(model(2639)).
bought(fish).
shops(mary).
end(model(2639)).

begin(model(2640)).
bought(spaghetti).
shops(mary).
end(model(2640)).

begin(model(2641)).
bought(fish).
shops(mary).
end(model(2641)).

begin(model(2642)).
end(model(2642)).

begin(model(2643)).
bought(fish).
shops(mary).
end(model(2643)).

begin(model(2644)).
bought(fish).
shops(mary).
end(model(2644)).

begin(model(2645)).
bought(fish).
shops(mary).
end(model(2645)).

begin(model(2646)).
bought(fish).
shops(mary).
end(model(2646)).

begin(model(2647)).
bought(fish).
shops(mary).
end(model(2647)).

begin(model(2648)).
bought(spaghetti).
shops(mary).
end(model(2648)).

begin(model(2649)).
end(model(2649)).

begin(model(2650)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2650)).

begin(model(2651)).
bought(fish).
shops(mary).
end(model(2651)).

begin(model(2652)).
bought(fish).
shops(mary).
end(model(2652)).

begin(model(2653)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2653)).

begin(model(2654)).
end(model(2654)).

begin(model(2655)).
bought(fish).
shops(mary).
end(model(2655)).

begin(model(2656)).
bought(fish).
shops(mary).
end(model(2656)).

begin(model(2657)).
bought(fish).
shops(mary).
end(model(2657)).

begin(model(2658)).
bought(fish).
shops(mary).
end(model(2658)).

begin(model(2659)).
bought(fish).
shops(mary).
end(model(2659)).

begin(model(2660)).
bought(fish).
shops(mary).
end(model(2660)).

begin(model(2661)).
bought(spaghetti).
shops(mary).
end(model(2661)).

begin(model(2662)).
bought(fish).
shops(mary).
end(model(2662)).

begin(model(2663)).
bought(fish).
shops(mary).
end(model(2663)).

begin(model(2664)).
bought(spaghetti).
shops(mary).
end(model(2664)).

begin(model(2665)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2665)).

begin(model(2666)).
bought(fish).
shops(mary).
end(model(2666)).

begin(model(2667)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2667)).

begin(model(2668)).
bought(fish).
shops(mary).
end(model(2668)).

begin(model(2669)).
bought(fish).
shops(mary).
end(model(2669)).

begin(model(2670)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2670)).

begin(model(2671)).
bought(fish).
shops(mary).
end(model(2671)).

begin(model(2672)).
bought(fish).
shops(mary).
end(model(2672)).

begin(model(2673)).
bought(fish).
shops(mary).
end(model(2673)).

begin(model(2674)).
bought(fish).
shops(mary).
end(model(2674)).

begin(model(2675)).
bought(fish).
shops(mary).
end(model(2675)).

begin(model(2676)).
end(model(2676)).

begin(model(2677)).
bought(fish).
shops(mary).
end(model(2677)).

begin(model(2678)).
bought(fish).
shops(mary).
end(model(2678)).

begin(model(2679)).
bought(spaghetti).
shops(mary).
end(model(2679)).

begin(model(2680)).
bought(fish).
shops(mary).
end(model(2680)).

begin(model(2681)).
bought(spaghetti).
shops(john).
end(model(2681)).

begin(model(2682)).
bought(spaghetti).
shops(mary).
end(model(2682)).

begin(model(2683)).
bought(fish).
shops(mary).
end(model(2683)).

begin(model(2684)).
bought(fish).
shops(mary).
end(model(2684)).

begin(model(2685)).
bought(fish).
shops(mary).
end(model(2685)).

begin(model(2686)).
bought(fish).
shops(mary).
end(model(2686)).

begin(model(2687)).
bought(fish).
shops(mary).
end(model(2687)).

begin(model(2688)).
bought(fish).
shops(mary).
end(model(2688)).

begin(model(2689)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2689)).

begin(model(2690)).
bought(fish).
shops(mary).
end(model(2690)).

begin(model(2691)).
bought(fish).
shops(mary).
end(model(2691)).

begin(model(2692)).
bought(spaghetti).
shops(mary).
end(model(2692)).

begin(model(2693)).
bought(fish).
shops(mary).
end(model(2693)).

begin(model(2694)).
end(model(2694)).

begin(model(2695)).
bought(spaghetti).
shops(john).
end(model(2695)).

begin(model(2696)).
bought(fish).
shops(mary).
end(model(2696)).

begin(model(2697)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2697)).

begin(model(2698)).
bought(spaghetti).
shops(mary).
end(model(2698)).

begin(model(2699)).
bought(spaghetti).
shops(mary).
end(model(2699)).

begin(model(2700)).
bought(fish).
shops(mary).
end(model(2700)).

begin(model(2701)).
bought(fish).
shops(mary).
end(model(2701)).

begin(model(2702)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2702)).

begin(model(2703)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2703)).

begin(model(2704)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2704)).

begin(model(2705)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2705)).

begin(model(2706)).
bought(fish).
shops(mary).
end(model(2706)).

begin(model(2707)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2707)).

begin(model(2708)).
bought(spaghetti).
shops(mary).
end(model(2708)).

begin(model(2709)).
bought(fish).
shops(mary).
end(model(2709)).

begin(model(2710)).
bought(fish).
shops(mary).
end(model(2710)).

begin(model(2711)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2711)).

begin(model(2712)).
end(model(2712)).

begin(model(2713)).
bought(fish).
shops(mary).
end(model(2713)).

begin(model(2714)).
bought(spaghetti).
shops(mary).
end(model(2714)).

begin(model(2715)).
bought(fish).
shops(mary).
end(model(2715)).

begin(model(2716)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2716)).

begin(model(2717)).
bought(fish).
shops(mary).
end(model(2717)).

begin(model(2718)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2718)).

begin(model(2719)).
bought(spaghetti).
shops(mary).
end(model(2719)).

begin(model(2720)).
bought(fish).
shops(mary).
end(model(2720)).

begin(model(2721)).
bought(fish).
shops(mary).
end(model(2721)).

begin(model(2722)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2722)).

begin(model(2723)).
end(model(2723)).

begin(model(2724)).
bought(fish).
shops(mary).
end(model(2724)).

begin(model(2725)).
bought(fish).
shops(mary).
end(model(2725)).

begin(model(2726)).
bought(fish).
shops(mary).
end(model(2726)).

begin(model(2727)).
bought(fish).
shops(mary).
end(model(2727)).

begin(model(2728)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2728)).

begin(model(2729)).
end(model(2729)).

begin(model(2730)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2730)).

begin(model(2731)).
bought(fish).
shops(mary).
end(model(2731)).

begin(model(2732)).
bought(fish).
shops(mary).
end(model(2732)).

begin(model(2733)).
bought(fish).
shops(mary).
end(model(2733)).

begin(model(2734)).
bought(fish).
shops(mary).
end(model(2734)).

begin(model(2735)).
end(model(2735)).

begin(model(2736)).
bought(fish).
shops(mary).
end(model(2736)).

begin(model(2737)).
bought(fish).
shops(mary).
end(model(2737)).

begin(model(2738)).
bought(spaghetti).
shops(mary).
end(model(2738)).

begin(model(2739)).
bought(fish).
shops(mary).
end(model(2739)).

begin(model(2740)).
bought(fish).
shops(mary).
end(model(2740)).

begin(model(2741)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2741)).

begin(model(2742)).
bought(fish).
shops(mary).
end(model(2742)).

begin(model(2743)).
bought(fish).
shops(mary).
end(model(2743)).

begin(model(2744)).
bought(fish).
shops(mary).
end(model(2744)).

begin(model(2745)).
bought(fish).
shops(mary).
end(model(2745)).

begin(model(2746)).
bought(fish).
shops(mary).
end(model(2746)).

begin(model(2747)).
bought(fish).
shops(mary).
end(model(2747)).

begin(model(2748)).
bought(fish).
shops(mary).
end(model(2748)).

begin(model(2749)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2749)).

begin(model(2750)).
bought(fish).
shops(mary).
end(model(2750)).

begin(model(2751)).
end(model(2751)).

begin(model(2752)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2752)).

begin(model(2753)).
bought(fish).
shops(mary).
end(model(2753)).

begin(model(2754)).
bought(fish).
shops(mary).
end(model(2754)).

begin(model(2755)).
bought(fish).
shops(mary).
end(model(2755)).

begin(model(2756)).
end(model(2756)).

begin(model(2757)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2757)).

begin(model(2758)).
bought(spaghetti).
shops(mary).
end(model(2758)).

begin(model(2759)).
bought(fish).
shops(mary).
end(model(2759)).

begin(model(2760)).
bought(fish).
shops(mary).
end(model(2760)).

begin(model(2761)).
bought(fish).
shops(mary).
end(model(2761)).

begin(model(2762)).
bought(fish).
shops(mary).
end(model(2762)).

begin(model(2763)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2763)).

begin(model(2764)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2764)).

begin(model(2765)).
bought(spaghetti).
shops(mary).
end(model(2765)).

begin(model(2766)).
bought(fish).
shops(mary).
end(model(2766)).

begin(model(2767)).
end(model(2767)).

begin(model(2768)).
bought(fish).
shops(mary).
end(model(2768)).

begin(model(2769)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2769)).

begin(model(2770)).
bought(fish).
shops(mary).
end(model(2770)).

begin(model(2771)).
bought(fish).
shops(mary).
end(model(2771)).

begin(model(2772)).
bought(fish).
shops(mary).
end(model(2772)).

begin(model(2773)).
bought(spaghetti).
shops(mary).
end(model(2773)).

begin(model(2774)).
bought(fish).
shops(mary).
end(model(2774)).

begin(model(2775)).
bought(fish).
shops(mary).
end(model(2775)).

begin(model(2776)).
bought(spaghetti).
shops(mary).
end(model(2776)).

begin(model(2777)).
bought(fish).
shops(mary).
end(model(2777)).

begin(model(2778)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2778)).

begin(model(2779)).
bought(spaghetti).
shops(mary).
end(model(2779)).

begin(model(2780)).
bought(spaghetti).
shops(john).
end(model(2780)).

begin(model(2781)).
bought(fish).
shops(mary).
end(model(2781)).

begin(model(2782)).
bought(spaghetti).
shops(john).
end(model(2782)).

begin(model(2783)).
bought(fish).
shops(mary).
end(model(2783)).

begin(model(2784)).
end(model(2784)).

begin(model(2785)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2785)).

begin(model(2786)).
bought(spaghetti).
shops(mary).
end(model(2786)).

begin(model(2787)).
bought(fish).
shops(mary).
end(model(2787)).

begin(model(2788)).
bought(fish).
shops(mary).
end(model(2788)).

begin(model(2789)).
bought(fish).
shops(mary).
end(model(2789)).

begin(model(2790)).
end(model(2790)).

begin(model(2791)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2791)).

begin(model(2792)).
bought(fish).
shops(mary).
end(model(2792)).

begin(model(2793)).
bought(fish).
shops(mary).
end(model(2793)).

begin(model(2794)).
bought(fish).
shops(mary).
end(model(2794)).

begin(model(2795)).
end(model(2795)).

begin(model(2796)).
bought(spaghetti).
shops(mary).
end(model(2796)).

begin(model(2797)).
bought(fish).
shops(mary).
end(model(2797)).

begin(model(2798)).
bought(spaghetti).
shops(mary).
end(model(2798)).

begin(model(2799)).
bought(fish).
shops(mary).
end(model(2799)).

begin(model(2800)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2800)).

begin(model(2801)).
bought(fish).
shops(mary).
end(model(2801)).

begin(model(2802)).
bought(fish).
shops(mary).
end(model(2802)).

begin(model(2803)).
bought(fish).
shops(mary).
end(model(2803)).

begin(model(2804)).
bought(fish).
shops(mary).
end(model(2804)).

begin(model(2805)).
bought(fish).
shops(mary).
end(model(2805)).

begin(model(2806)).
bought(fish).
shops(mary).
end(model(2806)).

begin(model(2807)).
bought(spaghetti).
shops(mary).
end(model(2807)).

begin(model(2808)).
bought(fish).
shops(mary).
end(model(2808)).

begin(model(2809)).
bought(fish).
shops(mary).
end(model(2809)).

begin(model(2810)).
bought(spaghetti).
shops(mary).
end(model(2810)).

begin(model(2811)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2811)).

begin(model(2812)).
bought(fish).
shops(mary).
end(model(2812)).

begin(model(2813)).
bought(fish).
shops(mary).
end(model(2813)).

begin(model(2814)).
bought(fish).
shops(mary).
end(model(2814)).

begin(model(2815)).
bought(fish).
shops(mary).
end(model(2815)).

begin(model(2816)).
bought(fish).
shops(mary).
end(model(2816)).

begin(model(2817)).
bought(spaghetti).
shops(mary).
end(model(2817)).

begin(model(2818)).
bought(fish).
shops(mary).
end(model(2818)).

begin(model(2819)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2819)).

begin(model(2820)).
bought(fish).
shops(mary).
end(model(2820)).

begin(model(2821)).
bought(fish).
shops(mary).
end(model(2821)).

begin(model(2822)).
bought(spaghetti).
shops(mary).
end(model(2822)).

begin(model(2823)).
bought(spaghetti).
shops(mary).
end(model(2823)).

begin(model(2824)).
bought(fish).
shops(mary).
end(model(2824)).

begin(model(2825)).
bought(fish).
shops(mary).
end(model(2825)).

begin(model(2826)).
bought(fish).
shops(mary).
end(model(2826)).

begin(model(2827)).
bought(fish).
shops(mary).
end(model(2827)).

begin(model(2828)).
bought(spaghetti).
shops(mary).
end(model(2828)).

begin(model(2829)).
bought(fish).
shops(mary).
end(model(2829)).

begin(model(2830)).
bought(fish).
shops(mary).
end(model(2830)).

begin(model(2831)).
end(model(2831)).

begin(model(2832)).
bought(fish).
shops(mary).
end(model(2832)).

begin(model(2833)).
bought(spaghetti).
shops(mary).
end(model(2833)).

begin(model(2834)).
bought(spaghetti).
shops(mary).
end(model(2834)).

begin(model(2835)).
bought(spaghetti).
shops(mary).
end(model(2835)).

begin(model(2836)).
bought(spaghetti).
shops(mary).
end(model(2836)).

begin(model(2837)).
end(model(2837)).

begin(model(2838)).
end(model(2838)).

begin(model(2839)).
bought(fish).
shops(mary).
end(model(2839)).

begin(model(2840)).
bought(spaghetti).
shops(mary).
end(model(2840)).

begin(model(2841)).
bought(spaghetti).
shops(mary).
end(model(2841)).

begin(model(2842)).
bought(fish).
shops(mary).
end(model(2842)).

begin(model(2843)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2843)).

begin(model(2844)).
bought(fish).
shops(mary).
end(model(2844)).

begin(model(2845)).
bought(spaghetti).
shops(mary).
end(model(2845)).

begin(model(2846)).
bought(fish).
shops(mary).
end(model(2846)).

begin(model(2847)).
bought(spaghetti).
shops(mary).
end(model(2847)).

begin(model(2848)).
bought(fish).
shops(mary).
end(model(2848)).

begin(model(2849)).
bought(fish).
shops(mary).
end(model(2849)).

begin(model(2850)).
bought(fish).
shops(mary).
end(model(2850)).

begin(model(2851)).
bought(fish).
shops(mary).
end(model(2851)).

begin(model(2852)).
bought(spaghetti).
shops(mary).
end(model(2852)).

begin(model(2853)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2853)).

begin(model(2854)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2854)).

begin(model(2855)).
bought(spaghetti).
shops(john).
end(model(2855)).

begin(model(2856)).
end(model(2856)).

begin(model(2857)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2857)).

begin(model(2858)).
bought(spaghetti).
shops(mary).
end(model(2858)).

begin(model(2859)).
bought(spaghetti).
shops(mary).
end(model(2859)).

begin(model(2860)).
bought(spaghetti).
shops(mary).
end(model(2860)).

begin(model(2861)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2861)).

begin(model(2862)).
bought(fish).
shops(mary).
end(model(2862)).

begin(model(2863)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2863)).

begin(model(2864)).
bought(fish).
shops(mary).
end(model(2864)).

begin(model(2865)).
bought(fish).
shops(mary).
end(model(2865)).

begin(model(2866)).
bought(fish).
shops(mary).
end(model(2866)).

begin(model(2867)).
end(model(2867)).

begin(model(2868)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2868)).

begin(model(2869)).
end(model(2869)).

begin(model(2870)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(2870)).

begin(model(2871)).
bought(fish).
shops(mary).
end(model(2871)).

begin(model(2872)).
bought(spaghetti).
shops(mary).
end(model(2872)).

begin(model(2873)).
bought(fish).
shops(mary).
end(model(2873)).

begin(model(2874)).
end(model(2874)).

begin(model(2875)).
bought(fish).
shops(mary).
end(model(2875)).

begin(model(2876)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2876)).

begin(model(2877)).
bought(fish).
shops(mary).
end(model(2877)).

begin(model(2878)).
bought(fish).
shops(mary).
end(model(2878)).

begin(model(2879)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2879)).

begin(model(2880)).
bought(spaghetti).
shops(mary).
end(model(2880)).

begin(model(2881)).
bought(spaghetti).
shops(mary).
end(model(2881)).

begin(model(2882)).
end(model(2882)).

begin(model(2883)).
end(model(2883)).

begin(model(2884)).
bought(fish).
shops(mary).
end(model(2884)).

begin(model(2885)).
bought(fish).
shops(mary).
end(model(2885)).

begin(model(2886)).
bought(fish).
shops(mary).
end(model(2886)).

begin(model(2887)).
bought(fish).
shops(mary).
end(model(2887)).

begin(model(2888)).
bought(fish).
shops(mary).
end(model(2888)).

begin(model(2889)).
bought(fish).
shops(mary).
end(model(2889)).

begin(model(2890)).
bought(fish).
shops(mary).
end(model(2890)).

begin(model(2891)).
bought(spaghetti).
shops(mary).
end(model(2891)).

begin(model(2892)).
bought(fish).
shops(mary).
end(model(2892)).

begin(model(2893)).
bought(fish).
shops(mary).
end(model(2893)).

begin(model(2894)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2894)).

begin(model(2895)).
bought(spaghetti).
shops(mary).
end(model(2895)).

begin(model(2896)).
bought(spaghetti).
shops(mary).
end(model(2896)).

begin(model(2897)).
bought(spaghetti).
shops(mary).
end(model(2897)).

begin(model(2898)).
bought(spaghetti).
shops(mary).
end(model(2898)).

begin(model(2899)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2899)).

begin(model(2900)).
bought(fish).
shops(mary).
end(model(2900)).

begin(model(2901)).
bought(fish).
shops(mary).
end(model(2901)).

begin(model(2902)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2902)).

begin(model(2903)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2903)).

begin(model(2904)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2904)).

begin(model(2905)).
bought(fish).
shops(mary).
end(model(2905)).

begin(model(2906)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2906)).

begin(model(2907)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2907)).

begin(model(2908)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2908)).

begin(model(2909)).
bought(fish).
shops(mary).
end(model(2909)).

begin(model(2910)).
end(model(2910)).

begin(model(2911)).
bought(fish).
shops(mary).
end(model(2911)).

begin(model(2912)).
end(model(2912)).

begin(model(2913)).
bought(fish).
shops(mary).
end(model(2913)).

begin(model(2914)).
bought(fish).
shops(mary).
end(model(2914)).

begin(model(2915)).
bought(fish).
shops(mary).
end(model(2915)).

begin(model(2916)).
bought(fish).
shops(mary).
end(model(2916)).

begin(model(2917)).
bought(fish).
shops(mary).
end(model(2917)).

begin(model(2918)).
bought(fish).
shops(mary).
end(model(2918)).

begin(model(2919)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2919)).

begin(model(2920)).
bought(spaghetti).
shops(mary).
end(model(2920)).

begin(model(2921)).
bought(fish).
shops(mary).
end(model(2921)).

begin(model(2922)).
bought(spaghetti).
shops(mary).
end(model(2922)).

begin(model(2923)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2923)).

begin(model(2924)).
bought(fish).
shops(mary).
end(model(2924)).

begin(model(2925)).
bought(fish).
shops(mary).
end(model(2925)).

begin(model(2926)).
bought(spaghetti).
shops(mary).
end(model(2926)).

begin(model(2927)).
end(model(2927)).

begin(model(2928)).
bought(spaghetti).
shops(mary).
end(model(2928)).

begin(model(2929)).
bought(fish).
shops(mary).
end(model(2929)).

begin(model(2930)).
bought(fish).
shops(mary).
end(model(2930)).

begin(model(2931)).
bought(fish).
shops(mary).
end(model(2931)).

begin(model(2932)).
bought(fish).
shops(mary).
end(model(2932)).

begin(model(2933)).
bought(fish).
shops(mary).
end(model(2933)).

begin(model(2934)).
bought(fish).
shops(mary).
end(model(2934)).

begin(model(2935)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2935)).

begin(model(2936)).
end(model(2936)).

begin(model(2937)).
bought(fish).
shops(mary).
end(model(2937)).

begin(model(2938)).
bought(fish).
shops(mary).
end(model(2938)).

begin(model(2939)).
bought(fish).
shops(mary).
end(model(2939)).

begin(model(2940)).
bought(spaghetti).
shops(mary).
end(model(2940)).

begin(model(2941)).
bought(fish).
shops(mary).
end(model(2941)).

begin(model(2942)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2942)).

begin(model(2943)).
bought(fish).
shops(mary).
end(model(2943)).

begin(model(2944)).
bought(fish).
shops(mary).
end(model(2944)).

begin(model(2945)).
bought(spaghetti).
shops(mary).
end(model(2945)).

begin(model(2946)).
bought(spaghetti).
shops(mary).
end(model(2946)).

begin(model(2947)).
bought(fish).
shops(mary).
end(model(2947)).

begin(model(2948)).
end(model(2948)).

begin(model(2949)).
bought(fish).
shops(mary).
end(model(2949)).

begin(model(2950)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2950)).

begin(model(2951)).
bought(fish).
shops(mary).
end(model(2951)).

begin(model(2952)).
bought(spaghetti).
shops(mary).
end(model(2952)).

begin(model(2953)).
end(model(2953)).

begin(model(2954)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2954)).

begin(model(2955)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2955)).

begin(model(2956)).
bought(spaghetti).
shops(mary).
end(model(2956)).

begin(model(2957)).
bought(fish).
shops(mary).
end(model(2957)).

begin(model(2958)).
bought(fish).
shops(mary).
end(model(2958)).

begin(model(2959)).
bought(fish).
shops(mary).
end(model(2959)).

begin(model(2960)).
bought(spaghetti).
shops(mary).
end(model(2960)).

begin(model(2961)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2961)).

begin(model(2962)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2962)).

begin(model(2963)).
bought(fish).
shops(mary).
end(model(2963)).

begin(model(2964)).
bought(fish).
shops(mary).
end(model(2964)).

begin(model(2965)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2965)).

begin(model(2966)).
bought(fish).
shops(mary).
end(model(2966)).

begin(model(2967)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2967)).

begin(model(2968)).
bought(fish).
shops(mary).
end(model(2968)).

begin(model(2969)).
bought(spaghetti).
shops(mary).
end(model(2969)).

begin(model(2970)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2970)).

begin(model(2971)).
bought(spaghetti).
shops(mary).
end(model(2971)).

begin(model(2972)).
bought(fish).
shops(mary).
end(model(2972)).

begin(model(2973)).
bought(fish).
shops(mary).
end(model(2973)).

begin(model(2974)).
bought(fish).
shops(mary).
end(model(2974)).

begin(model(2975)).
bought(fish).
shops(mary).
end(model(2975)).

begin(model(2976)).
end(model(2976)).

begin(model(2977)).
bought(fish).
shops(mary).
end(model(2977)).

begin(model(2978)).
bought(fish).
shops(mary).
end(model(2978)).

begin(model(2979)).
bought(spaghetti).
shops(mary).
end(model(2979)).

begin(model(2980)).
bought(spaghetti).
shops(mary).
end(model(2980)).

begin(model(2981)).
bought(fish).
shops(mary).
end(model(2981)).

begin(model(2982)).
bought(spaghetti).
shops(mary).
end(model(2982)).

begin(model(2983)).
end(model(2983)).

begin(model(2984)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2984)).

begin(model(2985)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2985)).

begin(model(2986)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(2986)).

begin(model(2987)).
end(model(2987)).

begin(model(2988)).
bought(spaghetti).
shops(mary).
end(model(2988)).

begin(model(2989)).
bought(fish).
shops(mary).
end(model(2989)).

begin(model(2990)).
bought(spaghetti).
shops(mary).
end(model(2990)).

begin(model(2991)).
bought(fish).
shops(mary).
end(model(2991)).

begin(model(2992)).
bought(fish).
shops(mary).
end(model(2992)).

begin(model(2993)).
bought(fish).
shops(mary).
end(model(2993)).

begin(model(2994)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(2994)).

begin(model(2995)).
bought(fish).
shops(mary).
end(model(2995)).

begin(model(2996)).
bought(fish).
shops(mary).
end(model(2996)).

begin(model(2997)).
bought(fish).
shops(mary).
end(model(2997)).

begin(model(2998)).
bought(spaghetti).
shops(mary).
end(model(2998)).

begin(model(2999)).
bought(fish).
shops(mary).
end(model(2999)).

begin(model(3000)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3000)).

begin(model(3001)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3001)).

begin(model(3002)).
bought(spaghetti).
shops(mary).
end(model(3002)).

begin(model(3003)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3003)).

begin(model(3004)).
bought(fish).
shops(mary).
end(model(3004)).

begin(model(3005)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3005)).

begin(model(3006)).
bought(fish).
shops(mary).
end(model(3006)).

begin(model(3007)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3007)).

begin(model(3008)).
bought(fish).
shops(mary).
end(model(3008)).

begin(model(3009)).
bought(spaghetti).
shops(mary).
end(model(3009)).

begin(model(3010)).
bought(spaghetti).
shops(mary).
end(model(3010)).

begin(model(3011)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3011)).

begin(model(3012)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3012)).

begin(model(3013)).
bought(fish).
shops(mary).
end(model(3013)).

begin(model(3014)).
bought(fish).
shops(mary).
end(model(3014)).

begin(model(3015)).
bought(spaghetti).
shops(john).
end(model(3015)).

begin(model(3016)).
end(model(3016)).

begin(model(3017)).
bought(fish).
shops(mary).
end(model(3017)).

begin(model(3018)).
bought(spaghetti).
shops(mary).
end(model(3018)).

begin(model(3019)).
bought(fish).
shops(mary).
end(model(3019)).

begin(model(3020)).
bought(fish).
shops(mary).
end(model(3020)).

begin(model(3021)).
end(model(3021)).

begin(model(3022)).
bought(fish).
shops(mary).
end(model(3022)).

begin(model(3023)).
bought(fish).
shops(mary).
end(model(3023)).

begin(model(3024)).
bought(fish).
shops(mary).
end(model(3024)).

begin(model(3025)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3025)).

begin(model(3026)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3026)).

begin(model(3027)).
bought(fish).
shops(mary).
end(model(3027)).

begin(model(3028)).
bought(spaghetti).
shops(mary).
end(model(3028)).

begin(model(3029)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3029)).

begin(model(3030)).
bought(spaghetti).
shops(mary).
end(model(3030)).

begin(model(3031)).
bought(steak).
shops(john).
end(model(3031)).

begin(model(3032)).
bought(fish).
shops(mary).
end(model(3032)).

begin(model(3033)).
bought(fish).
shops(mary).
end(model(3033)).

begin(model(3034)).
end(model(3034)).

begin(model(3035)).
bought(fish).
shops(mary).
end(model(3035)).

begin(model(3036)).
bought(spaghetti).
shops(mary).
end(model(3036)).

begin(model(3037)).
bought(steak).
shops(john).
end(model(3037)).

begin(model(3038)).
bought(fish).
shops(mary).
end(model(3038)).

begin(model(3039)).
bought(spaghetti).
shops(mary).
end(model(3039)).

begin(model(3040)).
bought(fish).
shops(mary).
end(model(3040)).

begin(model(3041)).
bought(spaghetti).
shops(mary).
end(model(3041)).

begin(model(3042)).
bought(spaghetti).
shops(mary).
end(model(3042)).

begin(model(3043)).
bought(fish).
shops(mary).
end(model(3043)).

begin(model(3044)).
bought(fish).
shops(mary).
end(model(3044)).

begin(model(3045)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3045)).

begin(model(3046)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3046)).

begin(model(3047)).
bought(fish).
shops(mary).
end(model(3047)).

begin(model(3048)).
bought(spaghetti).
shops(mary).
end(model(3048)).

begin(model(3049)).
bought(fish).
shops(mary).
end(model(3049)).

begin(model(3050)).
bought(fish).
shops(mary).
end(model(3050)).

begin(model(3051)).
end(model(3051)).

begin(model(3052)).
bought(fish).
shops(mary).
end(model(3052)).

begin(model(3053)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3053)).

begin(model(3054)).
bought(fish).
shops(mary).
end(model(3054)).

begin(model(3055)).
end(model(3055)).

begin(model(3056)).
bought(fish).
shops(mary).
end(model(3056)).

begin(model(3057)).
bought(spaghetti).
shops(mary).
end(model(3057)).

begin(model(3058)).
bought(fish).
shops(mary).
end(model(3058)).

begin(model(3059)).
bought(spaghetti).
shops(mary).
end(model(3059)).

begin(model(3060)).
bought(fish).
shops(mary).
end(model(3060)).

begin(model(3061)).
bought(spaghetti).
shops(mary).
end(model(3061)).

begin(model(3062)).
bought(fish).
shops(mary).
end(model(3062)).

begin(model(3063)).
bought(fish).
shops(mary).
end(model(3063)).

begin(model(3064)).
bought(spaghetti).
shops(mary).
end(model(3064)).

begin(model(3065)).
bought(fish).
shops(mary).
end(model(3065)).

begin(model(3066)).
bought(spaghetti).
shops(mary).
end(model(3066)).

begin(model(3067)).
bought(spaghetti).
shops(mary).
end(model(3067)).

begin(model(3068)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3068)).

begin(model(3069)).
bought(spaghetti).
shops(mary).
end(model(3069)).

begin(model(3070)).
bought(spaghetti).
shops(mary).
end(model(3070)).

begin(model(3071)).
bought(fish).
shops(mary).
end(model(3071)).

begin(model(3072)).
bought(spaghetti).
shops(mary).
end(model(3072)).

begin(model(3073)).
end(model(3073)).

begin(model(3074)).
bought(spaghetti).
shops(john).
end(model(3074)).

begin(model(3075)).
bought(fish).
shops(mary).
end(model(3075)).

begin(model(3076)).
bought(spaghetti).
shops(mary).
end(model(3076)).

begin(model(3077)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3077)).

begin(model(3078)).
bought(fish).
shops(mary).
end(model(3078)).

begin(model(3079)).
bought(spaghetti).
shops(john).
end(model(3079)).

begin(model(3080)).
end(model(3080)).

begin(model(3081)).
bought(fish).
shops(mary).
end(model(3081)).

begin(model(3082)).
bought(fish).
shops(mary).
end(model(3082)).

begin(model(3083)).
bought(spaghetti).
shops(john).
end(model(3083)).

begin(model(3084)).
bought(fish).
shops(mary).
end(model(3084)).

begin(model(3085)).
bought(fish).
shops(mary).
end(model(3085)).

begin(model(3086)).
bought(spaghetti).
shops(mary).
end(model(3086)).

begin(model(3087)).
bought(spaghetti).
shops(mary).
end(model(3087)).

begin(model(3088)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3088)).

begin(model(3089)).
bought(steak).
shops(john).
end(model(3089)).

begin(model(3090)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3090)).

begin(model(3091)).
end(model(3091)).

begin(model(3092)).
bought(fish).
shops(mary).
end(model(3092)).

begin(model(3093)).
bought(fish).
shops(mary).
end(model(3093)).

begin(model(3094)).
bought(fish).
shops(mary).
end(model(3094)).

begin(model(3095)).
bought(fish).
shops(mary).
end(model(3095)).

begin(model(3096)).
bought(fish).
shops(mary).
end(model(3096)).

begin(model(3097)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3097)).

begin(model(3098)).
bought(spaghetti).
shops(mary).
end(model(3098)).

begin(model(3099)).
bought(fish).
shops(mary).
end(model(3099)).

begin(model(3100)).
bought(fish).
shops(mary).
end(model(3100)).

begin(model(3101)).
bought(fish).
shops(mary).
end(model(3101)).

begin(model(3102)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3102)).

begin(model(3103)).
bought(fish).
shops(mary).
end(model(3103)).

begin(model(3104)).
bought(fish).
shops(mary).
end(model(3104)).

begin(model(3105)).
bought(fish).
shops(mary).
end(model(3105)).

begin(model(3106)).
bought(fish).
shops(mary).
end(model(3106)).

begin(model(3107)).
bought(fish).
shops(mary).
end(model(3107)).

begin(model(3108)).
bought(fish).
shops(mary).
end(model(3108)).

begin(model(3109)).
bought(spaghetti).
shops(john).
end(model(3109)).

begin(model(3110)).
bought(fish).
shops(mary).
end(model(3110)).

begin(model(3111)).
bought(fish).
shops(mary).
end(model(3111)).

begin(model(3112)).
bought(fish).
shops(mary).
end(model(3112)).

begin(model(3113)).
bought(spaghetti).
shops(mary).
end(model(3113)).

begin(model(3114)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3114)).

begin(model(3115)).
bought(fish).
shops(mary).
end(model(3115)).

begin(model(3116)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3116)).

begin(model(3117)).
bought(fish).
shops(mary).
end(model(3117)).

begin(model(3118)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3118)).

begin(model(3119)).
bought(fish).
shops(mary).
end(model(3119)).

begin(model(3120)).
bought(fish).
shops(mary).
end(model(3120)).

begin(model(3121)).
bought(spaghetti).
shops(mary).
end(model(3121)).

begin(model(3122)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3122)).

begin(model(3123)).
bought(spaghetti).
shops(mary).
end(model(3123)).

begin(model(3124)).
bought(fish).
shops(mary).
end(model(3124)).

begin(model(3125)).
bought(fish).
shops(mary).
end(model(3125)).

begin(model(3126)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3126)).

begin(model(3127)).
end(model(3127)).

begin(model(3128)).
bought(spaghetti).
shops(mary).
end(model(3128)).

begin(model(3129)).
bought(fish).
shops(mary).
end(model(3129)).

begin(model(3130)).
bought(spaghetti).
shops(mary).
end(model(3130)).

begin(model(3131)).
bought(spaghetti).
shops(mary).
end(model(3131)).

begin(model(3132)).
bought(fish).
shops(mary).
end(model(3132)).

begin(model(3133)).
bought(fish).
shops(mary).
end(model(3133)).

begin(model(3134)).
bought(spaghetti).
shops(mary).
end(model(3134)).

begin(model(3135)).
bought(fish).
shops(mary).
end(model(3135)).

begin(model(3136)).
bought(spaghetti).
shops(mary).
end(model(3136)).

begin(model(3137)).
bought(fish).
shops(mary).
end(model(3137)).

begin(model(3138)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3138)).

begin(model(3139)).
bought(fish).
shops(mary).
end(model(3139)).

begin(model(3140)).
end(model(3140)).

begin(model(3141)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3141)).

begin(model(3142)).
bought(spaghetti).
shops(mary).
end(model(3142)).

begin(model(3143)).
bought(fish).
shops(mary).
end(model(3143)).

begin(model(3144)).
bought(spaghetti).
shops(mary).
end(model(3144)).

begin(model(3145)).
bought(fish).
shops(mary).
end(model(3145)).

begin(model(3146)).
bought(fish).
shops(mary).
end(model(3146)).

begin(model(3147)).
bought(spaghetti).
shops(mary).
end(model(3147)).

begin(model(3148)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3148)).

begin(model(3149)).
bought(fish).
shops(mary).
end(model(3149)).

begin(model(3150)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3150)).

begin(model(3151)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3151)).

begin(model(3152)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3152)).

begin(model(3153)).
bought(fish).
shops(mary).
end(model(3153)).

begin(model(3154)).
bought(fish).
shops(mary).
end(model(3154)).

begin(model(3155)).
bought(spaghetti).
shops(mary).
end(model(3155)).

begin(model(3156)).
bought(fish).
shops(mary).
end(model(3156)).

begin(model(3157)).
bought(spaghetti).
shops(mary).
end(model(3157)).

begin(model(3158)).
end(model(3158)).

begin(model(3159)).
bought(fish).
shops(mary).
end(model(3159)).

begin(model(3160)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3160)).

begin(model(3161)).
bought(fish).
shops(mary).
end(model(3161)).

begin(model(3162)).
end(model(3162)).

begin(model(3163)).
bought(fish).
shops(mary).
end(model(3163)).

begin(model(3164)).
bought(fish).
shops(mary).
end(model(3164)).

begin(model(3165)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3165)).

begin(model(3166)).
bought(fish).
shops(mary).
end(model(3166)).

begin(model(3167)).
bought(fish).
shops(mary).
end(model(3167)).

begin(model(3168)).
bought(spaghetti).
shops(mary).
end(model(3168)).

begin(model(3169)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3169)).

begin(model(3170)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3170)).

begin(model(3171)).
end(model(3171)).

begin(model(3172)).
bought(fish).
shops(mary).
end(model(3172)).

begin(model(3173)).
bought(fish).
shops(mary).
end(model(3173)).

begin(model(3174)).
end(model(3174)).

begin(model(3175)).
bought(fish).
shops(mary).
end(model(3175)).

begin(model(3176)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3176)).

begin(model(3177)).
bought(fish).
shops(mary).
end(model(3177)).

begin(model(3178)).
end(model(3178)).

begin(model(3179)).
bought(spaghetti).
shops(mary).
end(model(3179)).

begin(model(3180)).
bought(spaghetti).
shops(mary).
end(model(3180)).

begin(model(3181)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3181)).

begin(model(3182)).
bought(fish).
shops(mary).
end(model(3182)).

begin(model(3183)).
bought(fish).
shops(mary).
end(model(3183)).

begin(model(3184)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3184)).

begin(model(3185)).
bought(spaghetti).
shops(mary).
end(model(3185)).

begin(model(3186)).
end(model(3186)).

begin(model(3187)).
end(model(3187)).

begin(model(3188)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3188)).

begin(model(3189)).
bought(spaghetti).
shops(mary).
end(model(3189)).

begin(model(3190)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3190)).

begin(model(3191)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3191)).

begin(model(3192)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3192)).

begin(model(3193)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3193)).

begin(model(3194)).
bought(fish).
shops(mary).
end(model(3194)).

begin(model(3195)).
bought(spaghetti).
shops(mary).
end(model(3195)).

begin(model(3196)).
bought(fish).
shops(mary).
end(model(3196)).

begin(model(3197)).
bought(spaghetti).
shops(mary).
end(model(3197)).

begin(model(3198)).
bought(spaghetti).
shops(mary).
end(model(3198)).

begin(model(3199)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3199)).

begin(model(3200)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3200)).

begin(model(3201)).
bought(fish).
shops(mary).
end(model(3201)).

begin(model(3202)).
bought(fish).
shops(mary).
end(model(3202)).

begin(model(3203)).
bought(steak).
shops(john).
end(model(3203)).

begin(model(3204)).
bought(fish).
shops(mary).
end(model(3204)).

begin(model(3205)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3205)).

begin(model(3206)).
bought(fish).
shops(mary).
end(model(3206)).

begin(model(3207)).
bought(spaghetti).
shops(mary).
end(model(3207)).

begin(model(3208)).
bought(spaghetti).
shops(mary).
end(model(3208)).

begin(model(3209)).
bought(fish).
shops(mary).
end(model(3209)).

begin(model(3210)).
bought(spaghetti).
shops(mary).
end(model(3210)).

begin(model(3211)).
end(model(3211)).

begin(model(3212)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3212)).

begin(model(3213)).
end(model(3213)).

begin(model(3214)).
bought(fish).
shops(mary).
end(model(3214)).

begin(model(3215)).
end(model(3215)).

begin(model(3216)).
bought(fish).
shops(mary).
end(model(3216)).

begin(model(3217)).
bought(fish).
shops(mary).
end(model(3217)).

begin(model(3218)).
bought(fish).
shops(mary).
end(model(3218)).

begin(model(3219)).
bought(spaghetti).
shops(mary).
end(model(3219)).

begin(model(3220)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3220)).

begin(model(3221)).
bought(fish).
shops(mary).
end(model(3221)).

begin(model(3222)).
bought(fish).
shops(mary).
end(model(3222)).

begin(model(3223)).
bought(spaghetti).
shops(mary).
end(model(3223)).

begin(model(3224)).
bought(fish).
shops(mary).
end(model(3224)).

begin(model(3225)).
bought(fish).
shops(mary).
end(model(3225)).

begin(model(3226)).
bought(fish).
shops(mary).
end(model(3226)).

begin(model(3227)).
bought(fish).
shops(mary).
end(model(3227)).

begin(model(3228)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3228)).

begin(model(3229)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3229)).

begin(model(3230)).
bought(spaghetti).
shops(mary).
end(model(3230)).

begin(model(3231)).
bought(spaghetti).
shops(mary).
end(model(3231)).

begin(model(3232)).
bought(spaghetti).
shops(mary).
end(model(3232)).

begin(model(3233)).
bought(fish).
shops(mary).
end(model(3233)).

begin(model(3234)).
bought(spaghetti).
shops(mary).
end(model(3234)).

begin(model(3235)).
end(model(3235)).

begin(model(3236)).
bought(fish).
shops(mary).
end(model(3236)).

begin(model(3237)).
bought(spaghetti).
shops(mary).
end(model(3237)).

begin(model(3238)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3238)).

begin(model(3239)).
bought(fish).
shops(mary).
end(model(3239)).

begin(model(3240)).
end(model(3240)).

begin(model(3241)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3241)).

begin(model(3242)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3242)).

begin(model(3243)).
bought(fish).
shops(mary).
end(model(3243)).

begin(model(3244)).
bought(fish).
shops(mary).
end(model(3244)).

begin(model(3245)).
bought(fish).
shops(mary).
end(model(3245)).

begin(model(3246)).
bought(spaghetti).
shops(john).
end(model(3246)).

begin(model(3247)).
bought(fish).
shops(mary).
end(model(3247)).

begin(model(3248)).
bought(fish).
shops(mary).
end(model(3248)).

begin(model(3249)).
bought(fish).
shops(mary).
end(model(3249)).

begin(model(3250)).
bought(fish).
shops(mary).
end(model(3250)).

begin(model(3251)).
bought(fish).
shops(mary).
end(model(3251)).

begin(model(3252)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3252)).

begin(model(3253)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3253)).

begin(model(3254)).
bought(spaghetti).
shops(mary).
end(model(3254)).

begin(model(3255)).
bought(fish).
shops(mary).
end(model(3255)).

begin(model(3256)).
bought(fish).
shops(mary).
end(model(3256)).

begin(model(3257)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3257)).

begin(model(3258)).
bought(spaghetti).
shops(mary).
end(model(3258)).

begin(model(3259)).
bought(fish).
shops(mary).
end(model(3259)).

begin(model(3260)).
end(model(3260)).

begin(model(3261)).
bought(fish).
shops(mary).
end(model(3261)).

begin(model(3262)).
bought(spaghetti).
shops(john).
end(model(3262)).

begin(model(3263)).
bought(fish).
shops(mary).
end(model(3263)).

begin(model(3264)).
bought(fish).
shops(mary).
end(model(3264)).

begin(model(3265)).
end(model(3265)).

begin(model(3266)).
bought(fish).
shops(mary).
end(model(3266)).

begin(model(3267)).
bought(spaghetti).
shops(mary).
end(model(3267)).

begin(model(3268)).
end(model(3268)).

begin(model(3269)).
bought(fish).
shops(mary).
end(model(3269)).

begin(model(3270)).
bought(spaghetti).
shops(mary).
end(model(3270)).

begin(model(3271)).
bought(spaghetti).
shops(mary).
end(model(3271)).

begin(model(3272)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3272)).

begin(model(3273)).
bought(spaghetti).
shops(mary).
end(model(3273)).

begin(model(3274)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3274)).

begin(model(3275)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3275)).

begin(model(3276)).
bought(spaghetti).
shops(mary).
end(model(3276)).

begin(model(3277)).
bought(fish).
shops(mary).
end(model(3277)).

begin(model(3278)).
bought(fish).
shops(mary).
end(model(3278)).

begin(model(3279)).
bought(fish).
shops(mary).
end(model(3279)).

begin(model(3280)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3280)).

begin(model(3281)).
bought(fish).
shops(mary).
end(model(3281)).

begin(model(3282)).
bought(spaghetti).
shops(mary).
end(model(3282)).

begin(model(3283)).
bought(fish).
shops(mary).
end(model(3283)).

begin(model(3284)).
bought(fish).
shops(mary).
end(model(3284)).

begin(model(3285)).
bought(fish).
shops(mary).
end(model(3285)).

begin(model(3286)).
bought(fish).
shops(mary).
end(model(3286)).

begin(model(3287)).
bought(spaghetti).
shops(mary).
end(model(3287)).

begin(model(3288)).
bought(fish).
shops(mary).
end(model(3288)).

begin(model(3289)).
bought(spaghetti).
shops(mary).
end(model(3289)).

begin(model(3290)).
bought(fish).
shops(mary).
end(model(3290)).

begin(model(3291)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3291)).

begin(model(3292)).
bought(fish).
shops(mary).
end(model(3292)).

begin(model(3293)).
bought(fish).
shops(mary).
end(model(3293)).

begin(model(3294)).
bought(fish).
shops(mary).
end(model(3294)).

begin(model(3295)).
bought(fish).
shops(mary).
end(model(3295)).

begin(model(3296)).
bought(fish).
shops(mary).
end(model(3296)).

begin(model(3297)).
end(model(3297)).

begin(model(3298)).
bought(fish).
shops(mary).
end(model(3298)).

begin(model(3299)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3299)).

begin(model(3300)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3300)).

begin(model(3301)).
bought(fish).
shops(mary).
end(model(3301)).

begin(model(3302)).
bought(spaghetti).
shops(mary).
end(model(3302)).

begin(model(3303)).
bought(fish).
shops(mary).
end(model(3303)).

begin(model(3304)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3304)).

begin(model(3305)).
bought(spaghetti).
shops(mary).
end(model(3305)).

begin(model(3306)).
bought(fish).
shops(mary).
end(model(3306)).

begin(model(3307)).
bought(fish).
shops(mary).
end(model(3307)).

begin(model(3308)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3308)).

begin(model(3309)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3309)).

begin(model(3310)).
bought(fish).
shops(mary).
end(model(3310)).

begin(model(3311)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3311)).

begin(model(3312)).
bought(fish).
shops(mary).
end(model(3312)).

begin(model(3313)).
bought(fish).
shops(mary).
end(model(3313)).

begin(model(3314)).
bought(fish).
shops(mary).
end(model(3314)).

begin(model(3315)).
bought(fish).
shops(mary).
end(model(3315)).

begin(model(3316)).
bought(fish).
shops(mary).
end(model(3316)).

begin(model(3317)).
bought(fish).
shops(mary).
end(model(3317)).

begin(model(3318)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3318)).

begin(model(3319)).
end(model(3319)).

begin(model(3320)).
bought(fish).
shops(mary).
end(model(3320)).

begin(model(3321)).
bought(fish).
shops(mary).
end(model(3321)).

begin(model(3322)).
bought(fish).
shops(mary).
end(model(3322)).

begin(model(3323)).
bought(fish).
shops(mary).
end(model(3323)).

begin(model(3324)).
end(model(3324)).

begin(model(3325)).
bought(fish).
shops(mary).
end(model(3325)).

begin(model(3326)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3326)).

begin(model(3327)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3327)).

begin(model(3328)).
bought(spaghetti).
shops(mary).
end(model(3328)).

begin(model(3329)).
bought(fish).
shops(mary).
end(model(3329)).

begin(model(3330)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3330)).

begin(model(3331)).
end(model(3331)).

begin(model(3332)).
bought(fish).
shops(mary).
end(model(3332)).

begin(model(3333)).
end(model(3333)).

begin(model(3334)).
bought(spaghetti).
shops(mary).
end(model(3334)).

begin(model(3335)).
bought(spaghetti).
shops(mary).
end(model(3335)).

begin(model(3336)).
bought(fish).
shops(mary).
end(model(3336)).

begin(model(3337)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3337)).

begin(model(3338)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3338)).

begin(model(3339)).
bought(fish).
shops(mary).
end(model(3339)).

begin(model(3340)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3340)).

begin(model(3341)).
bought(fish).
shops(mary).
end(model(3341)).

begin(model(3342)).
end(model(3342)).

begin(model(3343)).
bought(fish).
shops(mary).
end(model(3343)).

begin(model(3344)).
bought(fish).
shops(mary).
end(model(3344)).

begin(model(3345)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3345)).

begin(model(3346)).
bought(fish).
shops(mary).
end(model(3346)).

begin(model(3347)).
bought(fish).
shops(mary).
end(model(3347)).

begin(model(3348)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3348)).

begin(model(3349)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3349)).

begin(model(3350)).
bought(steak).
shops(john).
end(model(3350)).

begin(model(3351)).
bought(fish).
shops(mary).
end(model(3351)).

begin(model(3352)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3352)).

begin(model(3353)).
bought(fish).
shops(mary).
end(model(3353)).

begin(model(3354)).
bought(fish).
shops(mary).
end(model(3354)).

begin(model(3355)).
bought(fish).
shops(mary).
end(model(3355)).

begin(model(3356)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3356)).

begin(model(3357)).
bought(fish).
shops(mary).
end(model(3357)).

begin(model(3358)).
bought(fish).
shops(mary).
end(model(3358)).

begin(model(3359)).
bought(spaghetti).
shops(mary).
end(model(3359)).

begin(model(3360)).
end(model(3360)).

begin(model(3361)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3361)).

begin(model(3362)).
bought(spaghetti).
shops(mary).
end(model(3362)).

begin(model(3363)).
bought(fish).
shops(mary).
end(model(3363)).

begin(model(3364)).
bought(fish).
shops(mary).
end(model(3364)).

begin(model(3365)).
bought(spaghetti).
shops(mary).
end(model(3365)).

begin(model(3366)).
bought(fish).
shops(mary).
end(model(3366)).

begin(model(3367)).
bought(spaghetti).
shops(mary).
end(model(3367)).

begin(model(3368)).
bought(spaghetti).
shops(mary).
end(model(3368)).

begin(model(3369)).
bought(fish).
shops(mary).
end(model(3369)).

begin(model(3370)).
bought(fish).
shops(mary).
end(model(3370)).

begin(model(3371)).
bought(spaghetti).
shops(mary).
end(model(3371)).

begin(model(3372)).
bought(spaghetti).
shops(mary).
end(model(3372)).

begin(model(3373)).
bought(fish).
shops(mary).
end(model(3373)).

begin(model(3374)).
bought(fish).
shops(mary).
end(model(3374)).

begin(model(3375)).
bought(fish).
shops(mary).
end(model(3375)).

begin(model(3376)).
bought(fish).
shops(mary).
end(model(3376)).

begin(model(3377)).
bought(fish).
shops(mary).
end(model(3377)).

begin(model(3378)).
bought(fish).
shops(mary).
end(model(3378)).

begin(model(3379)).
bought(fish).
shops(mary).
end(model(3379)).

begin(model(3380)).
bought(fish).
shops(mary).
end(model(3380)).

begin(model(3381)).
end(model(3381)).

begin(model(3382)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3382)).

begin(model(3383)).
bought(spaghetti).
shops(mary).
end(model(3383)).

begin(model(3384)).
bought(fish).
shops(mary).
end(model(3384)).

begin(model(3385)).
bought(spaghetti).
shops(mary).
end(model(3385)).

begin(model(3386)).
bought(fish).
shops(mary).
end(model(3386)).

begin(model(3387)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3387)).

begin(model(3388)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3388)).

begin(model(3389)).
bought(fish).
shops(mary).
end(model(3389)).

begin(model(3390)).
bought(fish).
shops(mary).
end(model(3390)).

begin(model(3391)).
bought(fish).
shops(mary).
end(model(3391)).

begin(model(3392)).
bought(fish).
shops(mary).
end(model(3392)).

begin(model(3393)).
bought(fish).
shops(mary).
end(model(3393)).

begin(model(3394)).
end(model(3394)).

begin(model(3395)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3395)).

begin(model(3396)).
end(model(3396)).

begin(model(3397)).
bought(fish).
shops(mary).
end(model(3397)).

begin(model(3398)).
bought(spaghetti).
shops(mary).
end(model(3398)).

begin(model(3399)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3399)).

begin(model(3400)).
bought(fish).
shops(mary).
end(model(3400)).

begin(model(3401)).
bought(fish).
shops(mary).
end(model(3401)).

begin(model(3402)).
bought(fish).
shops(mary).
end(model(3402)).

begin(model(3403)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3403)).

begin(model(3404)).
bought(fish).
shops(mary).
end(model(3404)).

begin(model(3405)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3405)).

begin(model(3406)).
bought(fish).
shops(mary).
end(model(3406)).

begin(model(3407)).
bought(spaghetti).
shops(mary).
end(model(3407)).

begin(model(3408)).
bought(fish).
shops(mary).
end(model(3408)).

begin(model(3409)).
bought(fish).
shops(mary).
end(model(3409)).

begin(model(3410)).
end(model(3410)).

begin(model(3411)).
bought(fish).
shops(mary).
end(model(3411)).

begin(model(3412)).
bought(fish).
shops(mary).
end(model(3412)).

begin(model(3413)).
bought(spaghetti).
shops(mary).
end(model(3413)).

begin(model(3414)).
bought(fish).
shops(mary).
end(model(3414)).

begin(model(3415)).
bought(fish).
shops(mary).
end(model(3415)).

begin(model(3416)).
bought(fish).
shops(mary).
end(model(3416)).

begin(model(3417)).
bought(fish).
shops(mary).
end(model(3417)).

begin(model(3418)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3418)).

begin(model(3419)).
bought(fish).
shops(mary).
end(model(3419)).

begin(model(3420)).
bought(fish).
shops(mary).
end(model(3420)).

begin(model(3421)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3421)).

begin(model(3422)).
bought(spaghetti).
shops(mary).
end(model(3422)).

begin(model(3423)).
bought(fish).
shops(mary).
end(model(3423)).

begin(model(3424)).
bought(spaghetti).
shops(mary).
end(model(3424)).

begin(model(3425)).
bought(fish).
shops(mary).
end(model(3425)).

begin(model(3426)).
bought(spaghetti).
shops(mary).
end(model(3426)).

begin(model(3427)).
bought(fish).
shops(mary).
end(model(3427)).

begin(model(3428)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3428)).

begin(model(3429)).
bought(fish).
shops(mary).
end(model(3429)).

begin(model(3430)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3430)).

begin(model(3431)).
bought(fish).
shops(mary).
end(model(3431)).

begin(model(3432)).
bought(spaghetti).
shops(mary).
end(model(3432)).

begin(model(3433)).
bought(fish).
shops(mary).
end(model(3433)).

begin(model(3434)).
bought(fish).
shops(mary).
end(model(3434)).

begin(model(3435)).
bought(spaghetti).
shops(mary).
end(model(3435)).

begin(model(3436)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3436)).

begin(model(3437)).
bought(fish).
shops(mary).
end(model(3437)).

begin(model(3438)).
bought(spaghetti).
shops(mary).
end(model(3438)).

begin(model(3439)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3439)).

begin(model(3440)).
bought(fish).
shops(mary).
end(model(3440)).

begin(model(3441)).
bought(spaghetti).
shops(mary).
end(model(3441)).

begin(model(3442)).
bought(fish).
shops(mary).
end(model(3442)).

begin(model(3443)).
bought(fish).
shops(mary).
end(model(3443)).

begin(model(3444)).
bought(spaghetti).
shops(john).
end(model(3444)).

begin(model(3445)).
bought(spaghetti).
shops(mary).
end(model(3445)).

begin(model(3446)).
bought(fish).
shops(mary).
end(model(3446)).

begin(model(3447)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3447)).

begin(model(3448)).
bought(spaghetti).
shops(mary).
end(model(3448)).

begin(model(3449)).
bought(fish).
shops(mary).
end(model(3449)).

begin(model(3450)).
bought(fish).
shops(mary).
end(model(3450)).

begin(model(3451)).
bought(fish).
shops(mary).
end(model(3451)).

begin(model(3452)).
end(model(3452)).

begin(model(3453)).
bought(spaghetti).
shops(mary).
end(model(3453)).

begin(model(3454)).
bought(fish).
shops(mary).
end(model(3454)).

begin(model(3455)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3455)).

begin(model(3456)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3456)).

begin(model(3457)).
bought(fish).
shops(mary).
end(model(3457)).

begin(model(3458)).
bought(fish).
shops(mary).
end(model(3458)).

begin(model(3459)).
bought(fish).
shops(mary).
end(model(3459)).

begin(model(3460)).
bought(fish).
shops(mary).
end(model(3460)).

begin(model(3461)).
bought(fish).
shops(mary).
end(model(3461)).

begin(model(3462)).
end(model(3462)).

begin(model(3463)).
bought(fish).
shops(mary).
end(model(3463)).

begin(model(3464)).
bought(fish).
shops(mary).
end(model(3464)).

begin(model(3465)).
bought(fish).
shops(mary).
end(model(3465)).

begin(model(3466)).
bought(spaghetti).
shops(mary).
end(model(3466)).

begin(model(3467)).
bought(fish).
shops(mary).
end(model(3467)).

begin(model(3468)).
bought(spaghetti).
shops(mary).
end(model(3468)).

begin(model(3469)).
bought(spaghetti).
shops(mary).
end(model(3469)).

begin(model(3470)).
bought(fish).
shops(mary).
end(model(3470)).

begin(model(3471)).
bought(spaghetti).
shops(mary).
end(model(3471)).

begin(model(3472)).
bought(spaghetti).
shops(mary).
end(model(3472)).

begin(model(3473)).
bought(fish).
shops(mary).
end(model(3473)).

begin(model(3474)).
bought(fish).
shops(mary).
end(model(3474)).

begin(model(3475)).
bought(fish).
shops(mary).
end(model(3475)).

begin(model(3476)).
bought(spaghetti).
shops(mary).
end(model(3476)).

begin(model(3477)).
bought(spaghetti).
shops(mary).
end(model(3477)).

begin(model(3478)).
bought(spaghetti).
shops(mary).
end(model(3478)).

begin(model(3479)).
bought(spaghetti).
shops(mary).
end(model(3479)).

begin(model(3480)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3480)).

begin(model(3481)).
bought(spaghetti).
shops(mary).
end(model(3481)).

begin(model(3482)).
bought(spaghetti).
shops(mary).
end(model(3482)).

begin(model(3483)).
bought(fish).
shops(mary).
end(model(3483)).

begin(model(3484)).
bought(fish).
shops(mary).
end(model(3484)).

begin(model(3485)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3485)).

begin(model(3486)).
end(model(3486)).

begin(model(3487)).
bought(spaghetti).
shops(mary).
end(model(3487)).

begin(model(3488)).
bought(fish).
shops(mary).
end(model(3488)).

begin(model(3489)).
bought(spaghetti).
shops(mary).
end(model(3489)).

begin(model(3490)).
bought(fish).
shops(mary).
end(model(3490)).

begin(model(3491)).
bought(fish).
shops(mary).
end(model(3491)).

begin(model(3492)).
bought(spaghetti).
shops(mary).
end(model(3492)).

begin(model(3493)).
bought(fish).
shops(mary).
end(model(3493)).

begin(model(3494)).
bought(spaghetti).
shops(mary).
end(model(3494)).

begin(model(3495)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3495)).

begin(model(3496)).
bought(fish).
shops(mary).
end(model(3496)).

begin(model(3497)).
bought(fish).
shops(mary).
end(model(3497)).

begin(model(3498)).
end(model(3498)).

begin(model(3499)).
bought(spaghetti).
shops(mary).
end(model(3499)).

begin(model(3500)).
bought(fish).
shops(mary).
end(model(3500)).

begin(model(3501)).
bought(fish).
shops(mary).
end(model(3501)).

begin(model(3502)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3502)).

begin(model(3503)).
bought(fish).
shops(mary).
end(model(3503)).

begin(model(3504)).
bought(fish).
shops(mary).
end(model(3504)).

begin(model(3505)).
end(model(3505)).

begin(model(3506)).
bought(fish).
shops(mary).
end(model(3506)).

begin(model(3507)).
bought(fish).
shops(mary).
end(model(3507)).

begin(model(3508)).
bought(fish).
shops(mary).
end(model(3508)).

begin(model(3509)).
bought(fish).
shops(mary).
end(model(3509)).

begin(model(3510)).
bought(fish).
shops(mary).
end(model(3510)).

begin(model(3511)).
bought(fish).
shops(mary).
end(model(3511)).

begin(model(3512)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3512)).

begin(model(3513)).
bought(steak).
shops(john).
end(model(3513)).

begin(model(3514)).
end(model(3514)).

begin(model(3515)).
bought(fish).
shops(mary).
end(model(3515)).

begin(model(3516)).
bought(fish).
shops(mary).
end(model(3516)).

begin(model(3517)).
bought(fish).
shops(mary).
end(model(3517)).

begin(model(3518)).
bought(fish).
shops(mary).
end(model(3518)).

begin(model(3519)).
bought(spaghetti).
shops(mary).
end(model(3519)).

begin(model(3520)).
bought(fish).
shops(mary).
end(model(3520)).

begin(model(3521)).
bought(fish).
shops(mary).
end(model(3521)).

begin(model(3522)).
end(model(3522)).

begin(model(3523)).
end(model(3523)).

begin(model(3524)).
end(model(3524)).

begin(model(3525)).
bought(spaghetti).
shops(mary).
end(model(3525)).

begin(model(3526)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3526)).

begin(model(3527)).
bought(fish).
shops(mary).
end(model(3527)).

begin(model(3528)).
bought(fish).
shops(mary).
end(model(3528)).

begin(model(3529)).
bought(fish).
shops(mary).
end(model(3529)).

begin(model(3530)).
bought(fish).
shops(mary).
end(model(3530)).

begin(model(3531)).
bought(fish).
shops(mary).
end(model(3531)).

begin(model(3532)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3532)).

begin(model(3533)).
bought(fish).
shops(mary).
end(model(3533)).

begin(model(3534)).
bought(fish).
shops(mary).
end(model(3534)).

begin(model(3535)).
bought(fish).
shops(mary).
end(model(3535)).

begin(model(3536)).
bought(fish).
shops(mary).
end(model(3536)).

begin(model(3537)).
bought(fish).
shops(mary).
end(model(3537)).

begin(model(3538)).
bought(fish).
shops(mary).
end(model(3538)).

begin(model(3539)).
bought(fish).
shops(mary).
end(model(3539)).

begin(model(3540)).
bought(fish).
shops(mary).
end(model(3540)).

begin(model(3541)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3541)).

begin(model(3542)).
bought(fish).
shops(mary).
end(model(3542)).

begin(model(3543)).
bought(fish).
shops(mary).
end(model(3543)).

begin(model(3544)).
bought(fish).
shops(mary).
end(model(3544)).

begin(model(3545)).
bought(spaghetti).
shops(mary).
end(model(3545)).

begin(model(3546)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3546)).

begin(model(3547)).
bought(fish).
shops(mary).
end(model(3547)).

begin(model(3548)).
bought(fish).
shops(mary).
end(model(3548)).

begin(model(3549)).
end(model(3549)).

begin(model(3550)).
bought(spaghetti).
shops(mary).
end(model(3550)).

begin(model(3551)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3551)).

begin(model(3552)).
bought(spaghetti).
shops(mary).
end(model(3552)).

begin(model(3553)).
bought(fish).
shops(mary).
end(model(3553)).

begin(model(3554)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3554)).

begin(model(3555)).
bought(fish).
shops(mary).
end(model(3555)).

begin(model(3556)).
bought(spaghetti).
shops(mary).
end(model(3556)).

begin(model(3557)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3557)).

begin(model(3558)).
bought(fish).
shops(mary).
end(model(3558)).

begin(model(3559)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3559)).

begin(model(3560)).
end(model(3560)).

begin(model(3561)).
bought(fish).
shops(mary).
end(model(3561)).

begin(model(3562)).
bought(spaghetti).
shops(mary).
end(model(3562)).

begin(model(3563)).
bought(fish).
shops(mary).
end(model(3563)).

begin(model(3564)).
bought(fish).
shops(mary).
end(model(3564)).

begin(model(3565)).
bought(fish).
shops(mary).
end(model(3565)).

begin(model(3566)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3566)).

begin(model(3567)).
bought(spaghetti).
shops(mary).
end(model(3567)).

begin(model(3568)).
bought(spaghetti).
shops(mary).
end(model(3568)).

begin(model(3569)).
bought(spaghetti).
shops(mary).
end(model(3569)).

begin(model(3570)).
bought(fish).
shops(mary).
end(model(3570)).

begin(model(3571)).
end(model(3571)).

begin(model(3572)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3572)).

begin(model(3573)).
bought(fish).
shops(mary).
end(model(3573)).

begin(model(3574)).
bought(spaghetti).
shops(mary).
end(model(3574)).

begin(model(3575)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3575)).

begin(model(3576)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3576)).

begin(model(3577)).
bought(fish).
shops(mary).
end(model(3577)).

begin(model(3578)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3578)).

begin(model(3579)).
bought(spaghetti).
shops(mary).
end(model(3579)).

begin(model(3580)).
bought(spaghetti).
shops(mary).
end(model(3580)).

begin(model(3581)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3581)).

begin(model(3582)).
bought(fish).
shops(mary).
end(model(3582)).

begin(model(3583)).
bought(fish).
shops(mary).
end(model(3583)).

begin(model(3584)).
bought(fish).
shops(mary).
end(model(3584)).

begin(model(3585)).
bought(fish).
shops(mary).
end(model(3585)).

begin(model(3586)).
bought(spaghetti).
shops(mary).
end(model(3586)).

begin(model(3587)).
bought(fish).
shops(mary).
end(model(3587)).

begin(model(3588)).
bought(fish).
shops(mary).
end(model(3588)).

begin(model(3589)).
bought(fish).
shops(mary).
end(model(3589)).

begin(model(3590)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3590)).

begin(model(3591)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3591)).

begin(model(3592)).
bought(fish).
shops(mary).
end(model(3592)).

begin(model(3593)).
bought(fish).
shops(mary).
end(model(3593)).

begin(model(3594)).
bought(spaghetti).
shops(mary).
end(model(3594)).

begin(model(3595)).
bought(spaghetti).
shops(mary).
end(model(3595)).

begin(model(3596)).
bought(fish).
shops(mary).
end(model(3596)).

begin(model(3597)).
bought(fish).
shops(mary).
end(model(3597)).

begin(model(3598)).
bought(steak).
shops(john).
end(model(3598)).

begin(model(3599)).
bought(spaghetti).
shops(mary).
end(model(3599)).

begin(model(3600)).
bought(fish).
shops(mary).
end(model(3600)).

begin(model(3601)).
bought(fish).
shops(mary).
end(model(3601)).

begin(model(3602)).
bought(fish).
shops(mary).
end(model(3602)).

begin(model(3603)).
bought(spaghetti).
shops(mary).
end(model(3603)).

begin(model(3604)).
end(model(3604)).

begin(model(3605)).
bought(fish).
shops(mary).
end(model(3605)).

begin(model(3606)).
end(model(3606)).

begin(model(3607)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3607)).

begin(model(3608)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3608)).

begin(model(3609)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3609)).

begin(model(3610)).
bought(fish).
shops(mary).
end(model(3610)).

begin(model(3611)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3611)).

begin(model(3612)).
bought(spaghetti).
shops(mary).
end(model(3612)).

begin(model(3613)).
bought(fish).
shops(mary).
end(model(3613)).

begin(model(3614)).
bought(spaghetti).
shops(mary).
end(model(3614)).

begin(model(3615)).
bought(fish).
shops(mary).
end(model(3615)).

begin(model(3616)).
bought(spaghetti).
shops(mary).
end(model(3616)).

begin(model(3617)).
bought(fish).
shops(mary).
end(model(3617)).

begin(model(3618)).
bought(fish).
shops(mary).
end(model(3618)).

begin(model(3619)).
bought(fish).
shops(mary).
end(model(3619)).

begin(model(3620)).
bought(spaghetti).
shops(mary).
end(model(3620)).

begin(model(3621)).
bought(fish).
shops(mary).
end(model(3621)).

begin(model(3622)).
bought(fish).
shops(mary).
end(model(3622)).

begin(model(3623)).
bought(fish).
shops(mary).
end(model(3623)).

begin(model(3624)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3624)).

begin(model(3625)).
bought(spaghetti).
shops(mary).
end(model(3625)).

begin(model(3626)).
bought(spaghetti).
shops(mary).
end(model(3626)).

begin(model(3627)).
bought(fish).
shops(mary).
end(model(3627)).

begin(model(3628)).
bought(spaghetti).
shops(mary).
end(model(3628)).

begin(model(3629)).
end(model(3629)).

begin(model(3630)).
bought(fish).
shops(mary).
end(model(3630)).

begin(model(3631)).
bought(spaghetti).
shops(mary).
end(model(3631)).

begin(model(3632)).
bought(spaghetti).
shops(mary).
end(model(3632)).

begin(model(3633)).
bought(fish).
shops(mary).
end(model(3633)).

begin(model(3634)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3634)).

begin(model(3635)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3635)).

begin(model(3636)).
bought(spaghetti).
shops(john).
end(model(3636)).

begin(model(3637)).
bought(spaghetti).
shops(mary).
end(model(3637)).

begin(model(3638)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3638)).

begin(model(3639)).
bought(fish).
shops(mary).
end(model(3639)).

begin(model(3640)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3640)).

begin(model(3641)).
bought(fish).
shops(mary).
end(model(3641)).

begin(model(3642)).
bought(fish).
shops(mary).
end(model(3642)).

begin(model(3643)).
bought(fish).
shops(mary).
end(model(3643)).

begin(model(3644)).
bought(spaghetti).
shops(mary).
end(model(3644)).

begin(model(3645)).
bought(fish).
shops(mary).
end(model(3645)).

begin(model(3646)).
bought(spaghetti).
shops(mary).
end(model(3646)).

begin(model(3647)).
bought(fish).
shops(mary).
end(model(3647)).

begin(model(3648)).
bought(fish).
shops(mary).
end(model(3648)).

begin(model(3649)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3649)).

begin(model(3650)).
bought(fish).
shops(mary).
end(model(3650)).

begin(model(3651)).
bought(fish).
shops(mary).
end(model(3651)).

begin(model(3652)).
bought(fish).
shops(mary).
end(model(3652)).

begin(model(3653)).
end(model(3653)).

begin(model(3654)).
bought(spaghetti).
shops(mary).
end(model(3654)).

begin(model(3655)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3655)).

begin(model(3656)).
bought(spaghetti).
shops(mary).
end(model(3656)).

begin(model(3657)).
bought(fish).
shops(mary).
end(model(3657)).

begin(model(3658)).
bought(fish).
shops(mary).
end(model(3658)).

begin(model(3659)).
bought(spaghetti).
shops(mary).
end(model(3659)).

begin(model(3660)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3660)).

begin(model(3661)).
bought(spaghetti).
shops(mary).
end(model(3661)).

begin(model(3662)).
bought(spaghetti).
shops(mary).
end(model(3662)).

begin(model(3663)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3663)).

begin(model(3664)).
end(model(3664)).

begin(model(3665)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3665)).

begin(model(3666)).
bought(spaghetti).
shops(mary).
end(model(3666)).

begin(model(3667)).
bought(fish).
shops(mary).
end(model(3667)).

begin(model(3668)).
end(model(3668)).

begin(model(3669)).
bought(fish).
shops(mary).
end(model(3669)).

begin(model(3670)).
bought(fish).
shops(mary).
end(model(3670)).

begin(model(3671)).
bought(spaghetti).
shops(mary).
end(model(3671)).

begin(model(3672)).
bought(fish).
shops(mary).
end(model(3672)).

begin(model(3673)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3673)).

begin(model(3674)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3674)).

begin(model(3675)).
bought(spaghetti).
shops(mary).
end(model(3675)).

begin(model(3676)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3676)).

begin(model(3677)).
bought(spaghetti).
shops(mary).
end(model(3677)).

begin(model(3678)).
bought(spaghetti).
shops(mary).
end(model(3678)).

begin(model(3679)).
bought(spaghetti).
shops(mary).
end(model(3679)).

begin(model(3680)).
bought(fish).
shops(mary).
end(model(3680)).

begin(model(3681)).
bought(spaghetti).
shops(mary).
end(model(3681)).

begin(model(3682)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3682)).

begin(model(3683)).
bought(spaghetti).
shops(mary).
end(model(3683)).

begin(model(3684)).
bought(spaghetti).
shops(mary).
end(model(3684)).

begin(model(3685)).
end(model(3685)).

begin(model(3686)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3686)).

begin(model(3687)).
bought(fish).
shops(mary).
end(model(3687)).

begin(model(3688)).
end(model(3688)).

begin(model(3689)).
bought(fish).
shops(mary).
end(model(3689)).

begin(model(3690)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3690)).

begin(model(3691)).
bought(fish).
shops(mary).
end(model(3691)).

begin(model(3692)).
bought(spaghetti).
shops(mary).
end(model(3692)).

begin(model(3693)).
end(model(3693)).

begin(model(3694)).
bought(spaghetti).
shops(mary).
end(model(3694)).

begin(model(3695)).
bought(fish).
shops(mary).
end(model(3695)).

begin(model(3696)).
bought(fish).
shops(mary).
end(model(3696)).

begin(model(3697)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3697)).

begin(model(3698)).
bought(fish).
shops(mary).
end(model(3698)).

begin(model(3699)).
bought(spaghetti).
shops(mary).
end(model(3699)).

begin(model(3700)).
bought(fish).
shops(mary).
end(model(3700)).

begin(model(3701)).
bought(steak).
shops(john).
end(model(3701)).

begin(model(3702)).
bought(fish).
shops(mary).
end(model(3702)).

begin(model(3703)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3703)).

begin(model(3704)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3704)).

begin(model(3705)).
bought(fish).
shops(mary).
end(model(3705)).

begin(model(3706)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3706)).

begin(model(3707)).
bought(fish).
shops(mary).
end(model(3707)).

begin(model(3708)).
bought(spaghetti).
shops(mary).
end(model(3708)).

begin(model(3709)).
bought(fish).
shops(mary).
end(model(3709)).

begin(model(3710)).
bought(spaghetti).
shops(mary).
end(model(3710)).

begin(model(3711)).
bought(fish).
shops(mary).
end(model(3711)).

begin(model(3712)).
bought(spaghetti).
shops(mary).
end(model(3712)).

begin(model(3713)).
bought(spaghetti).
shops(mary).
end(model(3713)).

begin(model(3714)).
end(model(3714)).

begin(model(3715)).
bought(fish).
shops(mary).
end(model(3715)).

begin(model(3716)).
bought(fish).
shops(mary).
end(model(3716)).

begin(model(3717)).
bought(fish).
shops(mary).
end(model(3717)).

begin(model(3718)).
bought(fish).
shops(mary).
end(model(3718)).

begin(model(3719)).
bought(spaghetti).
shops(mary).
end(model(3719)).

begin(model(3720)).
bought(spaghetti).
shops(mary).
end(model(3720)).

begin(model(3721)).
bought(fish).
shops(mary).
end(model(3721)).

begin(model(3722)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3722)).

begin(model(3723)).
bought(fish).
shops(mary).
end(model(3723)).

begin(model(3724)).
bought(fish).
shops(mary).
end(model(3724)).

begin(model(3725)).
bought(fish).
shops(mary).
end(model(3725)).

begin(model(3726)).
bought(fish).
shops(mary).
end(model(3726)).

begin(model(3727)).
bought(fish).
shops(mary).
end(model(3727)).

begin(model(3728)).
bought(fish).
shops(mary).
end(model(3728)).

begin(model(3729)).
bought(fish).
shops(mary).
end(model(3729)).

begin(model(3730)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3730)).

begin(model(3731)).
bought(spaghetti).
shops(mary).
end(model(3731)).

begin(model(3732)).
bought(fish).
shops(mary).
end(model(3732)).

begin(model(3733)).
bought(fish).
shops(mary).
end(model(3733)).

begin(model(3734)).
end(model(3734)).

begin(model(3735)).
bought(spaghetti).
shops(mary).
end(model(3735)).

begin(model(3736)).
bought(spaghetti).
shops(mary).
end(model(3736)).

begin(model(3737)).
bought(fish).
shops(mary).
end(model(3737)).

begin(model(3738)).
bought(fish).
shops(mary).
end(model(3738)).

begin(model(3739)).
bought(fish).
shops(mary).
end(model(3739)).

begin(model(3740)).
end(model(3740)).

begin(model(3741)).
end(model(3741)).

begin(model(3742)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3742)).

begin(model(3743)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3743)).

begin(model(3744)).
end(model(3744)).

begin(model(3745)).
bought(fish).
shops(mary).
end(model(3745)).

begin(model(3746)).
bought(fish).
shops(mary).
end(model(3746)).

begin(model(3747)).
bought(fish).
shops(mary).
end(model(3747)).

begin(model(3748)).
bought(fish).
shops(mary).
end(model(3748)).

begin(model(3749)).
bought(fish).
shops(mary).
end(model(3749)).

begin(model(3750)).
bought(spaghetti).
shops(mary).
end(model(3750)).

begin(model(3751)).
bought(fish).
shops(mary).
end(model(3751)).

begin(model(3752)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3752)).

begin(model(3753)).
bought(fish).
shops(mary).
end(model(3753)).

begin(model(3754)).
bought(spaghetti).
shops(mary).
end(model(3754)).

begin(model(3755)).
bought(spaghetti).
shops(mary).
end(model(3755)).

begin(model(3756)).
bought(fish).
shops(mary).
end(model(3756)).

begin(model(3757)).
bought(fish).
shops(mary).
end(model(3757)).

begin(model(3758)).
bought(fish).
shops(mary).
end(model(3758)).

begin(model(3759)).
bought(fish).
shops(mary).
end(model(3759)).

begin(model(3760)).
bought(spaghetti).
shops(mary).
end(model(3760)).

begin(model(3761)).
bought(fish).
shops(mary).
end(model(3761)).

begin(model(3762)).
bought(spaghetti).
shops(mary).
end(model(3762)).

begin(model(3763)).
bought(fish).
shops(mary).
end(model(3763)).

begin(model(3764)).
bought(spaghetti).
shops(mary).
end(model(3764)).

begin(model(3765)).
bought(fish).
shops(mary).
end(model(3765)).

begin(model(3766)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3766)).

begin(model(3767)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3767)).

begin(model(3768)).
bought(fish).
shops(mary).
end(model(3768)).

begin(model(3769)).
bought(spaghetti).
shops(mary).
end(model(3769)).

begin(model(3770)).
bought(spaghetti).
shops(mary).
end(model(3770)).

begin(model(3771)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3771)).

begin(model(3772)).
bought(fish).
shops(mary).
end(model(3772)).

begin(model(3773)).
bought(fish).
shops(mary).
end(model(3773)).

begin(model(3774)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3774)).

begin(model(3775)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3775)).

begin(model(3776)).
bought(fish).
shops(mary).
end(model(3776)).

begin(model(3777)).
bought(fish).
shops(mary).
end(model(3777)).

begin(model(3778)).
bought(fish).
shops(mary).
end(model(3778)).

begin(model(3779)).
bought(spaghetti).
shops(john).
end(model(3779)).

begin(model(3780)).
end(model(3780)).

begin(model(3781)).
bought(spaghetti).
shops(mary).
end(model(3781)).

begin(model(3782)).
end(model(3782)).

begin(model(3783)).
bought(fish).
shops(mary).
end(model(3783)).

begin(model(3784)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3784)).

begin(model(3785)).
bought(spaghetti).
shops(mary).
end(model(3785)).

begin(model(3786)).
bought(fish).
shops(mary).
end(model(3786)).

begin(model(3787)).
bought(spaghetti).
shops(mary).
end(model(3787)).

begin(model(3788)).
bought(spaghetti).
shops(mary).
end(model(3788)).

begin(model(3789)).
bought(fish).
shops(mary).
end(model(3789)).

begin(model(3790)).
bought(spaghetti).
shops(mary).
end(model(3790)).

begin(model(3791)).
bought(fish).
shops(mary).
end(model(3791)).

begin(model(3792)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3792)).

begin(model(3793)).
bought(spaghetti).
shops(mary).
end(model(3793)).

begin(model(3794)).
bought(fish).
shops(mary).
end(model(3794)).

begin(model(3795)).
bought(fish).
shops(mary).
end(model(3795)).

begin(model(3796)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3796)).

begin(model(3797)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3797)).

begin(model(3798)).
bought(spaghetti).
shops(mary).
end(model(3798)).

begin(model(3799)).
bought(spaghetti).
shops(mary).
end(model(3799)).

begin(model(3800)).
bought(fish).
shops(mary).
end(model(3800)).

begin(model(3801)).
end(model(3801)).

begin(model(3802)).
bought(fish).
shops(mary).
end(model(3802)).

begin(model(3803)).
bought(fish).
shops(mary).
end(model(3803)).

begin(model(3804)).
bought(fish).
shops(mary).
end(model(3804)).

begin(model(3805)).
bought(fish).
shops(mary).
end(model(3805)).

begin(model(3806)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3806)).

begin(model(3807)).
bought(spaghetti).
shops(mary).
end(model(3807)).

begin(model(3808)).
bought(fish).
shops(mary).
end(model(3808)).

begin(model(3809)).
end(model(3809)).

begin(model(3810)).
bought(fish).
shops(mary).
end(model(3810)).

begin(model(3811)).
bought(fish).
shops(mary).
end(model(3811)).

begin(model(3812)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3812)).

begin(model(3813)).
bought(spaghetti).
shops(mary).
end(model(3813)).

begin(model(3814)).
bought(fish).
shops(mary).
end(model(3814)).

begin(model(3815)).
bought(fish).
shops(mary).
end(model(3815)).

begin(model(3816)).
bought(spaghetti).
shops(mary).
end(model(3816)).

begin(model(3817)).
bought(fish).
shops(mary).
end(model(3817)).

begin(model(3818)).
bought(spaghetti).
shops(mary).
end(model(3818)).

begin(model(3819)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3819)).

begin(model(3820)).
bought(spaghetti).
shops(mary).
end(model(3820)).

begin(model(3821)).
bought(fish).
shops(mary).
end(model(3821)).

begin(model(3822)).
bought(fish).
shops(mary).
end(model(3822)).

begin(model(3823)).
bought(fish).
shops(mary).
end(model(3823)).

begin(model(3824)).
bought(fish).
shops(mary).
end(model(3824)).

begin(model(3825)).
bought(fish).
shops(mary).
end(model(3825)).

begin(model(3826)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3826)).

begin(model(3827)).
bought(spaghetti).
shops(mary).
end(model(3827)).

begin(model(3828)).
bought(fish).
shops(mary).
end(model(3828)).

begin(model(3829)).
bought(fish).
shops(mary).
end(model(3829)).

begin(model(3830)).
end(model(3830)).

begin(model(3831)).
bought(fish).
shops(mary).
end(model(3831)).

begin(model(3832)).
bought(spaghetti).
shops(mary).
end(model(3832)).

begin(model(3833)).
bought(fish).
shops(mary).
end(model(3833)).

begin(model(3834)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3834)).

begin(model(3835)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3835)).

begin(model(3836)).
bought(fish).
shops(mary).
end(model(3836)).

begin(model(3837)).
bought(fish).
shops(mary).
end(model(3837)).

begin(model(3838)).
bought(fish).
shops(mary).
end(model(3838)).

begin(model(3839)).
bought(spaghetti).
shops(mary).
end(model(3839)).

begin(model(3840)).
bought(fish).
shops(mary).
end(model(3840)).

begin(model(3841)).
bought(fish).
shops(mary).
end(model(3841)).

begin(model(3842)).
bought(fish).
shops(mary).
end(model(3842)).

begin(model(3843)).
bought(fish).
shops(mary).
end(model(3843)).

begin(model(3844)).
bought(spaghetti).
shops(mary).
end(model(3844)).

begin(model(3845)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3845)).

begin(model(3846)).
bought(fish).
shops(mary).
end(model(3846)).

begin(model(3847)).
bought(fish).
shops(mary).
end(model(3847)).

begin(model(3848)).
bought(spaghetti).
shops(mary).
end(model(3848)).

begin(model(3849)).
end(model(3849)).

begin(model(3850)).
bought(fish).
shops(mary).
end(model(3850)).

begin(model(3851)).
end(model(3851)).

begin(model(3852)).
bought(fish).
shops(mary).
end(model(3852)).

begin(model(3853)).
bought(fish).
shops(mary).
end(model(3853)).

begin(model(3854)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3854)).

begin(model(3855)).
bought(fish).
shops(mary).
end(model(3855)).

begin(model(3856)).
bought(spaghetti).
shops(mary).
end(model(3856)).

begin(model(3857)).
bought(spaghetti).
shops(mary).
end(model(3857)).

begin(model(3858)).
bought(spaghetti).
shops(mary).
end(model(3858)).

begin(model(3859)).
bought(fish).
shops(mary).
end(model(3859)).

begin(model(3860)).
bought(fish).
shops(mary).
end(model(3860)).

begin(model(3861)).
bought(fish).
shops(mary).
end(model(3861)).

begin(model(3862)).
bought(fish).
shops(mary).
end(model(3862)).

begin(model(3863)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3863)).

begin(model(3864)).
bought(fish).
shops(mary).
end(model(3864)).

begin(model(3865)).
bought(spaghetti).
shops(mary).
end(model(3865)).

begin(model(3866)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3866)).

begin(model(3867)).
bought(fish).
shops(mary).
end(model(3867)).

begin(model(3868)).
bought(fish).
shops(mary).
end(model(3868)).

begin(model(3869)).
bought(fish).
shops(mary).
end(model(3869)).

begin(model(3870)).
bought(fish).
shops(mary).
end(model(3870)).

begin(model(3871)).
bought(spaghetti).
shops(mary).
end(model(3871)).

begin(model(3872)).
bought(spaghetti).
shops(mary).
end(model(3872)).

begin(model(3873)).
bought(fish).
shops(mary).
end(model(3873)).

begin(model(3874)).
end(model(3874)).

begin(model(3875)).
bought(fish).
shops(mary).
end(model(3875)).

begin(model(3876)).
bought(fish).
shops(mary).
end(model(3876)).

begin(model(3877)).
bought(spaghetti).
shops(john).
end(model(3877)).

begin(model(3878)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3878)).

begin(model(3879)).
bought(spaghetti).
shops(mary).
end(model(3879)).

begin(model(3880)).
end(model(3880)).

begin(model(3881)).
bought(spaghetti).
shops(mary).
end(model(3881)).

begin(model(3882)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3882)).

begin(model(3883)).
bought(fish).
shops(mary).
end(model(3883)).

begin(model(3884)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3884)).

begin(model(3885)).
bought(spaghetti).
shops(mary).
end(model(3885)).

begin(model(3886)).
end(model(3886)).

begin(model(3887)).
bought(fish).
shops(mary).
end(model(3887)).

begin(model(3888)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3888)).

begin(model(3889)).
bought(spaghetti).
shops(mary).
end(model(3889)).

begin(model(3890)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3890)).

begin(model(3891)).
bought(spaghetti).
shops(mary).
end(model(3891)).

begin(model(3892)).
end(model(3892)).

begin(model(3893)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3893)).

begin(model(3894)).
bought(fish).
shops(mary).
end(model(3894)).

begin(model(3895)).
bought(fish).
shops(mary).
end(model(3895)).

begin(model(3896)).
bought(fish).
shops(mary).
end(model(3896)).

begin(model(3897)).
bought(fish).
shops(mary).
end(model(3897)).

begin(model(3898)).
bought(spaghetti).
shops(mary).
end(model(3898)).

begin(model(3899)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3899)).

begin(model(3900)).
bought(fish).
shops(mary).
end(model(3900)).

begin(model(3901)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3901)).

begin(model(3902)).
bought(fish).
shops(mary).
end(model(3902)).

begin(model(3903)).
bought(spaghetti).
shops(mary).
end(model(3903)).

begin(model(3904)).
bought(fish).
shops(mary).
end(model(3904)).

begin(model(3905)).
bought(spaghetti).
shops(mary).
end(model(3905)).

begin(model(3906)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3906)).

begin(model(3907)).
bought(spaghetti).
shops(mary).
end(model(3907)).

begin(model(3908)).
bought(fish).
shops(mary).
end(model(3908)).

begin(model(3909)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3909)).

begin(model(3910)).
bought(fish).
shops(mary).
end(model(3910)).

begin(model(3911)).
bought(fish).
shops(mary).
end(model(3911)).

begin(model(3912)).
bought(fish).
shops(mary).
end(model(3912)).

begin(model(3913)).
bought(fish).
shops(mary).
end(model(3913)).

begin(model(3914)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3914)).

begin(model(3915)).
bought(fish).
shops(mary).
end(model(3915)).

begin(model(3916)).
bought(fish).
shops(mary).
end(model(3916)).

begin(model(3917)).
bought(spaghetti).
shops(mary).
end(model(3917)).

begin(model(3918)).
bought(fish).
shops(mary).
end(model(3918)).

begin(model(3919)).
bought(fish).
shops(mary).
end(model(3919)).

begin(model(3920)).
end(model(3920)).

begin(model(3921)).
bought(fish).
shops(mary).
end(model(3921)).

begin(model(3922)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3922)).

begin(model(3923)).
bought(fish).
shops(mary).
end(model(3923)).

begin(model(3924)).
bought(spaghetti).
shops(mary).
end(model(3924)).

begin(model(3925)).
bought(fish).
shops(mary).
end(model(3925)).

begin(model(3926)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3926)).

begin(model(3927)).
bought(spaghetti).
shops(mary).
end(model(3927)).

begin(model(3928)).
bought(fish).
shops(mary).
end(model(3928)).

begin(model(3929)).
bought(fish).
shops(mary).
end(model(3929)).

begin(model(3930)).
bought(spaghetti).
shops(mary).
end(model(3930)).

begin(model(3931)).
end(model(3931)).

begin(model(3932)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3932)).

begin(model(3933)).
bought(spaghetti).
shops(mary).
end(model(3933)).

begin(model(3934)).
bought(fish).
shops(mary).
end(model(3934)).

begin(model(3935)).
bought(steak).
shops(john).
end(model(3935)).

begin(model(3936)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3936)).

begin(model(3937)).
bought(fish).
shops(mary).
end(model(3937)).

begin(model(3938)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3938)).

begin(model(3939)).
bought(spaghetti).
shops(mary).
end(model(3939)).

begin(model(3940)).
bought(fish).
shops(mary).
end(model(3940)).

begin(model(3941)).
bought(fish).
shops(mary).
end(model(3941)).

begin(model(3942)).
bought(fish).
shops(mary).
end(model(3942)).

begin(model(3943)).
bought(fish).
shops(mary).
end(model(3943)).

begin(model(3944)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3944)).

begin(model(3945)).
bought(spaghetti).
shops(mary).
end(model(3945)).

begin(model(3946)).
bought(fish).
shops(mary).
end(model(3946)).

begin(model(3947)).
bought(fish).
shops(mary).
end(model(3947)).

begin(model(3948)).
bought(fish).
shops(mary).
end(model(3948)).

begin(model(3949)).
bought(spaghetti).
shops(mary).
end(model(3949)).

begin(model(3950)).
bought(spaghetti).
shops(mary).
end(model(3950)).

begin(model(3951)).
bought(fish).
shops(mary).
end(model(3951)).

begin(model(3952)).
bought(fish).
shops(mary).
end(model(3952)).

begin(model(3953)).
bought(fish).
shops(mary).
end(model(3953)).

begin(model(3954)).
bought(fish).
shops(mary).
end(model(3954)).

begin(model(3955)).
bought(fish).
shops(mary).
end(model(3955)).

begin(model(3956)).
bought(spaghetti).
shops(mary).
end(model(3956)).

begin(model(3957)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3957)).

begin(model(3958)).
bought(spaghetti).
shops(mary).
end(model(3958)).

begin(model(3959)).
bought(spaghetti).
shops(john).
end(model(3959)).

begin(model(3960)).
bought(fish).
shops(mary).
end(model(3960)).

begin(model(3961)).
end(model(3961)).

begin(model(3962)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3962)).

begin(model(3963)).
bought(fish).
shops(mary).
end(model(3963)).

begin(model(3964)).
bought(fish).
shops(mary).
end(model(3964)).

begin(model(3965)).
bought(fish).
shops(mary).
end(model(3965)).

begin(model(3966)).
bought(fish).
shops(mary).
end(model(3966)).

begin(model(3967)).
bought(fish).
shops(mary).
end(model(3967)).

begin(model(3968)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3968)).

begin(model(3969)).
bought(fish).
shops(mary).
end(model(3969)).

begin(model(3970)).
bought(fish).
shops(mary).
end(model(3970)).

begin(model(3971)).
bought(fish).
shops(mary).
end(model(3971)).

begin(model(3972)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3972)).

begin(model(3973)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3973)).

begin(model(3974)).
bought(spaghetti).
shops(mary).
end(model(3974)).

begin(model(3975)).
bought(fish).
shops(mary).
end(model(3975)).

begin(model(3976)).
end(model(3976)).

begin(model(3977)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3977)).

begin(model(3978)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3978)).

begin(model(3979)).
bought(spaghetti).
shops(mary).
end(model(3979)).

begin(model(3980)).
bought(spaghetti).
shops(mary).
end(model(3980)).

begin(model(3981)).
bought(fish).
shops(mary).
end(model(3981)).

begin(model(3982)).
end(model(3982)).

begin(model(3983)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(3983)).

begin(model(3984)).
end(model(3984)).

begin(model(3985)).
bought(fish).
shops(mary).
end(model(3985)).

begin(model(3986)).
bought(fish).
shops(mary).
end(model(3986)).

begin(model(3987)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(3987)).

begin(model(3988)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3988)).

begin(model(3989)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3989)).

begin(model(3990)).
bought(spaghetti).
shops(mary).
end(model(3990)).

begin(model(3991)).
bought(fish).
shops(mary).
end(model(3991)).

begin(model(3992)).
bought(fish).
shops(mary).
end(model(3992)).

begin(model(3993)).
bought(spaghetti).
shops(mary).
end(model(3993)).

begin(model(3994)).
bought(fish).
shops(mary).
end(model(3994)).

begin(model(3995)).
bought(spaghetti).
shops(mary).
end(model(3995)).

begin(model(3996)).
bought(spaghetti).
shops(mary).
end(model(3996)).

begin(model(3997)).
bought(fish).
shops(mary).
end(model(3997)).

begin(model(3998)).
bought(fish).
shops(mary).
end(model(3998)).

begin(model(3999)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(3999)).

begin(model(4000)).
bought(spaghetti).
shops(mary).
end(model(4000)).

begin(model(4001)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4001)).

begin(model(4002)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4002)).

begin(model(4003)).
bought(fish).
shops(mary).
end(model(4003)).

begin(model(4004)).
bought(spaghetti).
shops(mary).
end(model(4004)).

begin(model(4005)).
bought(fish).
shops(mary).
end(model(4005)).

begin(model(4006)).
bought(fish).
shops(mary).
end(model(4006)).

begin(model(4007)).
bought(fish).
shops(mary).
end(model(4007)).

begin(model(4008)).
bought(fish).
shops(mary).
end(model(4008)).

begin(model(4009)).
bought(spaghetti).
shops(mary).
end(model(4009)).

begin(model(4010)).
bought(fish).
shops(mary).
end(model(4010)).

begin(model(4011)).
bought(fish).
shops(mary).
end(model(4011)).

begin(model(4012)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4012)).

begin(model(4013)).
end(model(4013)).

begin(model(4014)).
bought(fish).
shops(mary).
end(model(4014)).

begin(model(4015)).
bought(fish).
shops(mary).
end(model(4015)).

begin(model(4016)).
end(model(4016)).

begin(model(4017)).
bought(fish).
shops(mary).
end(model(4017)).

begin(model(4018)).
bought(fish).
shops(mary).
end(model(4018)).

begin(model(4019)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4019)).

begin(model(4020)).
bought(fish).
shops(mary).
end(model(4020)).

begin(model(4021)).
bought(fish).
shops(mary).
end(model(4021)).

begin(model(4022)).
bought(fish).
shops(mary).
end(model(4022)).

begin(model(4023)).
end(model(4023)).

begin(model(4024)).
bought(fish).
shops(mary).
end(model(4024)).

begin(model(4025)).
bought(spaghetti).
shops(mary).
end(model(4025)).

begin(model(4026)).
bought(fish).
shops(mary).
end(model(4026)).

begin(model(4027)).
bought(fish).
shops(mary).
end(model(4027)).

begin(model(4028)).
bought(fish).
shops(mary).
end(model(4028)).

begin(model(4029)).
bought(fish).
shops(mary).
end(model(4029)).

begin(model(4030)).
bought(spaghetti).
shops(mary).
end(model(4030)).

begin(model(4031)).
end(model(4031)).

begin(model(4032)).
bought(fish).
shops(mary).
end(model(4032)).

begin(model(4033)).
bought(spaghetti).
shops(mary).
end(model(4033)).

begin(model(4034)).
bought(fish).
shops(mary).
end(model(4034)).

begin(model(4035)).
end(model(4035)).

begin(model(4036)).
end(model(4036)).

begin(model(4037)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4037)).

begin(model(4038)).
bought(fish).
shops(mary).
end(model(4038)).

begin(model(4039)).
bought(fish).
shops(mary).
end(model(4039)).

begin(model(4040)).
bought(fish).
shops(mary).
end(model(4040)).

begin(model(4041)).
bought(spaghetti).
shops(mary).
end(model(4041)).

begin(model(4042)).
bought(fish).
shops(mary).
end(model(4042)).

begin(model(4043)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4043)).

begin(model(4044)).
bought(fish).
shops(mary).
end(model(4044)).

begin(model(4045)).
bought(fish).
shops(mary).
end(model(4045)).

begin(model(4046)).
bought(fish).
shops(mary).
end(model(4046)).

begin(model(4047)).
bought(fish).
shops(mary).
end(model(4047)).

begin(model(4048)).
bought(spaghetti).
shops(mary).
end(model(4048)).

begin(model(4049)).
bought(fish).
shops(mary).
end(model(4049)).

begin(model(4050)).
bought(fish).
shops(mary).
end(model(4050)).

begin(model(4051)).
bought(fish).
shops(mary).
end(model(4051)).

begin(model(4052)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4052)).

begin(model(4053)).
bought(fish).
shops(mary).
end(model(4053)).

begin(model(4054)).
end(model(4054)).

begin(model(4055)).
bought(fish).
shops(mary).
end(model(4055)).

begin(model(4056)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4056)).

begin(model(4057)).
bought(fish).
shops(mary).
end(model(4057)).

begin(model(4058)).
bought(steak).
shops(john).
end(model(4058)).

begin(model(4059)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4059)).

begin(model(4060)).
bought(fish).
shops(mary).
end(model(4060)).

begin(model(4061)).
end(model(4061)).

begin(model(4062)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4062)).

begin(model(4063)).
bought(fish).
shops(mary).
end(model(4063)).

begin(model(4064)).
bought(fish).
shops(mary).
end(model(4064)).

begin(model(4065)).
bought(fish).
shops(mary).
end(model(4065)).

begin(model(4066)).
end(model(4066)).

begin(model(4067)).
end(model(4067)).

begin(model(4068)).
bought(fish).
shops(mary).
end(model(4068)).

begin(model(4069)).
bought(fish).
shops(mary).
end(model(4069)).

begin(model(4070)).
bought(spaghetti).
shops(mary).
end(model(4070)).

begin(model(4071)).
end(model(4071)).

begin(model(4072)).
bought(fish).
shops(mary).
end(model(4072)).

begin(model(4073)).
bought(spaghetti).
shops(mary).
end(model(4073)).

begin(model(4074)).
bought(fish).
shops(mary).
end(model(4074)).

begin(model(4075)).
bought(fish).
shops(mary).
end(model(4075)).

begin(model(4076)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4076)).

begin(model(4077)).
bought(fish).
shops(mary).
end(model(4077)).

begin(model(4078)).
bought(fish).
shops(mary).
end(model(4078)).

begin(model(4079)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4079)).

begin(model(4080)).
bought(spaghetti).
shops(mary).
end(model(4080)).

begin(model(4081)).
bought(spaghetti).
shops(mary).
end(model(4081)).

begin(model(4082)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4082)).

begin(model(4083)).
bought(fish).
shops(mary).
end(model(4083)).

begin(model(4084)).
end(model(4084)).

begin(model(4085)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4085)).

begin(model(4086)).
bought(spaghetti).
shops(mary).
end(model(4086)).

begin(model(4087)).
bought(fish).
shops(mary).
end(model(4087)).

begin(model(4088)).
end(model(4088)).

begin(model(4089)).
bought(fish).
shops(mary).
end(model(4089)).

begin(model(4090)).
end(model(4090)).

begin(model(4091)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4091)).

begin(model(4092)).
bought(fish).
shops(mary).
end(model(4092)).

begin(model(4093)).
bought(fish).
shops(mary).
end(model(4093)).

begin(model(4094)).
bought(fish).
shops(mary).
end(model(4094)).

begin(model(4095)).
bought(fish).
shops(mary).
end(model(4095)).

begin(model(4096)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4096)).

begin(model(4097)).
bought(fish).
shops(mary).
end(model(4097)).

begin(model(4098)).
bought(fish).
shops(mary).
end(model(4098)).

begin(model(4099)).
bought(fish).
shops(mary).
end(model(4099)).

begin(model(4100)).
bought(fish).
shops(mary).
end(model(4100)).

begin(model(4101)).
end(model(4101)).

begin(model(4102)).
bought(fish).
shops(mary).
end(model(4102)).

begin(model(4103)).
bought(fish).
shops(mary).
end(model(4103)).

begin(model(4104)).
bought(fish).
shops(mary).
end(model(4104)).

begin(model(4105)).
bought(spaghetti).
shops(mary).
end(model(4105)).

begin(model(4106)).
end(model(4106)).

begin(model(4107)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4107)).

begin(model(4108)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4108)).

begin(model(4109)).
bought(spaghetti).
shops(mary).
end(model(4109)).

begin(model(4110)).
bought(fish).
shops(mary).
end(model(4110)).

begin(model(4111)).
bought(fish).
shops(mary).
end(model(4111)).

begin(model(4112)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4112)).

begin(model(4113)).
bought(fish).
shops(mary).
end(model(4113)).

begin(model(4114)).
bought(spaghetti).
shops(mary).
end(model(4114)).

begin(model(4115)).
bought(fish).
shops(mary).
end(model(4115)).

begin(model(4116)).
bought(fish).
shops(mary).
end(model(4116)).

begin(model(4117)).
bought(spaghetti).
shops(mary).
end(model(4117)).

begin(model(4118)).
bought(fish).
shops(mary).
end(model(4118)).

begin(model(4119)).
bought(spaghetti).
shops(mary).
end(model(4119)).

begin(model(4120)).
bought(fish).
shops(mary).
end(model(4120)).

begin(model(4121)).
bought(spaghetti).
shops(mary).
end(model(4121)).

begin(model(4122)).
bought(spaghetti).
shops(mary).
end(model(4122)).

begin(model(4123)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4123)).

begin(model(4124)).
bought(fish).
shops(mary).
end(model(4124)).

begin(model(4125)).
bought(fish).
shops(mary).
end(model(4125)).

begin(model(4126)).
bought(fish).
shops(mary).
end(model(4126)).

begin(model(4127)).
bought(spaghetti).
shops(mary).
end(model(4127)).

begin(model(4128)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4128)).

begin(model(4129)).
bought(spaghetti).
shops(mary).
end(model(4129)).

begin(model(4130)).
bought(spaghetti).
shops(mary).
end(model(4130)).

begin(model(4131)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4131)).

begin(model(4132)).
bought(spaghetti).
shops(mary).
end(model(4132)).

begin(model(4133)).
bought(fish).
shops(mary).
end(model(4133)).

begin(model(4134)).
bought(fish).
shops(mary).
end(model(4134)).

begin(model(4135)).
bought(spaghetti).
shops(mary).
end(model(4135)).

begin(model(4136)).
bought(fish).
shops(mary).
end(model(4136)).

begin(model(4137)).
bought(spaghetti).
shops(mary).
end(model(4137)).

begin(model(4138)).
bought(fish).
shops(mary).
end(model(4138)).

begin(model(4139)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4139)).

begin(model(4140)).
bought(spaghetti).
shops(john).
end(model(4140)).

begin(model(4141)).
bought(spaghetti).
shops(mary).
end(model(4141)).

begin(model(4142)).
bought(spaghetti).
shops(mary).
end(model(4142)).

begin(model(4143)).
bought(fish).
shops(mary).
end(model(4143)).

begin(model(4144)).
bought(fish).
shops(mary).
end(model(4144)).

begin(model(4145)).
bought(spaghetti).
shops(mary).
end(model(4145)).

begin(model(4146)).
bought(fish).
shops(mary).
end(model(4146)).

begin(model(4147)).
bought(fish).
shops(mary).
end(model(4147)).

begin(model(4148)).
bought(fish).
shops(mary).
end(model(4148)).

begin(model(4149)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4149)).

begin(model(4150)).
bought(fish).
shops(mary).
end(model(4150)).

begin(model(4151)).
bought(spaghetti).
shops(mary).
end(model(4151)).

begin(model(4152)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4152)).

begin(model(4153)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4153)).

begin(model(4154)).
bought(fish).
shops(mary).
end(model(4154)).

begin(model(4155)).
bought(fish).
shops(mary).
end(model(4155)).

begin(model(4156)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4156)).

begin(model(4157)).
bought(fish).
shops(mary).
end(model(4157)).

begin(model(4158)).
bought(fish).
shops(mary).
end(model(4158)).

begin(model(4159)).
bought(spaghetti).
shops(mary).
end(model(4159)).

begin(model(4160)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4160)).

begin(model(4161)).
bought(spaghetti).
shops(mary).
end(model(4161)).

begin(model(4162)).
bought(fish).
shops(mary).
end(model(4162)).

begin(model(4163)).
bought(fish).
shops(mary).
end(model(4163)).

begin(model(4164)).
bought(spaghetti).
shops(mary).
end(model(4164)).

begin(model(4165)).
end(model(4165)).

begin(model(4166)).
bought(fish).
shops(mary).
end(model(4166)).

begin(model(4167)).
bought(fish).
shops(mary).
end(model(4167)).

begin(model(4168)).
bought(fish).
shops(mary).
end(model(4168)).

begin(model(4169)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4169)).

begin(model(4170)).
bought(fish).
shops(mary).
end(model(4170)).

begin(model(4171)).
bought(fish).
shops(mary).
end(model(4171)).

begin(model(4172)).
bought(spaghetti).
shops(mary).
end(model(4172)).

begin(model(4173)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4173)).

begin(model(4174)).
bought(fish).
shops(mary).
end(model(4174)).

begin(model(4175)).
bought(fish).
shops(mary).
end(model(4175)).

begin(model(4176)).
bought(spaghetti).
shops(mary).
end(model(4176)).

begin(model(4177)).
bought(fish).
shops(mary).
end(model(4177)).

begin(model(4178)).
bought(fish).
shops(mary).
end(model(4178)).

begin(model(4179)).
end(model(4179)).

begin(model(4180)).
bought(spaghetti).
shops(mary).
end(model(4180)).

begin(model(4181)).
bought(fish).
shops(mary).
end(model(4181)).

begin(model(4182)).
bought(fish).
shops(mary).
end(model(4182)).

begin(model(4183)).
bought(fish).
shops(mary).
end(model(4183)).

begin(model(4184)).
end(model(4184)).

begin(model(4185)).
bought(fish).
shops(mary).
end(model(4185)).

begin(model(4186)).
bought(fish).
shops(mary).
end(model(4186)).

begin(model(4187)).
end(model(4187)).

begin(model(4188)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4188)).

begin(model(4189)).
bought(fish).
shops(mary).
end(model(4189)).

begin(model(4190)).
end(model(4190)).

begin(model(4191)).
bought(fish).
shops(mary).
end(model(4191)).

begin(model(4192)).
bought(fish).
shops(mary).
end(model(4192)).

begin(model(4193)).
bought(spaghetti).
shops(mary).
end(model(4193)).

begin(model(4194)).
bought(fish).
shops(mary).
end(model(4194)).

begin(model(4195)).
bought(spaghetti).
shops(mary).
end(model(4195)).

begin(model(4196)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4196)).

begin(model(4197)).
bought(fish).
shops(mary).
end(model(4197)).

begin(model(4198)).
bought(spaghetti).
shops(mary).
end(model(4198)).

begin(model(4199)).
bought(spaghetti).
shops(mary).
end(model(4199)).

begin(model(4200)).
bought(fish).
shops(mary).
end(model(4200)).

begin(model(4201)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4201)).

begin(model(4202)).
bought(spaghetti).
shops(mary).
end(model(4202)).

begin(model(4203)).
end(model(4203)).

begin(model(4204)).
bought(steak).
shops(john).
end(model(4204)).

begin(model(4205)).
bought(fish).
shops(mary).
end(model(4205)).

begin(model(4206)).
bought(fish).
shops(mary).
end(model(4206)).

begin(model(4207)).
bought(fish).
shops(mary).
end(model(4207)).

begin(model(4208)).
bought(fish).
shops(mary).
end(model(4208)).

begin(model(4209)).
bought(fish).
shops(mary).
end(model(4209)).

begin(model(4210)).
bought(fish).
shops(mary).
end(model(4210)).

begin(model(4211)).
bought(fish).
shops(mary).
end(model(4211)).

begin(model(4212)).
bought(fish).
shops(mary).
end(model(4212)).

begin(model(4213)).
end(model(4213)).

begin(model(4214)).
bought(fish).
shops(mary).
end(model(4214)).

begin(model(4215)).
bought(spaghetti).
shops(mary).
end(model(4215)).

begin(model(4216)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4216)).

begin(model(4217)).
bought(spaghetti).
shops(mary).
end(model(4217)).

begin(model(4218)).
bought(fish).
shops(mary).
end(model(4218)).

begin(model(4219)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4219)).

begin(model(4220)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4220)).

begin(model(4221)).
bought(steak).
shops(john).
end(model(4221)).

begin(model(4222)).
bought(fish).
shops(mary).
end(model(4222)).

begin(model(4223)).
bought(fish).
shops(mary).
end(model(4223)).

begin(model(4224)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4224)).

begin(model(4225)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4225)).

begin(model(4226)).
bought(steak).
shops(john).
end(model(4226)).

begin(model(4227)).
bought(fish).
shops(mary).
end(model(4227)).

begin(model(4228)).
end(model(4228)).

begin(model(4229)).
bought(spaghetti).
shops(mary).
end(model(4229)).

begin(model(4230)).
bought(fish).
shops(mary).
end(model(4230)).

begin(model(4231)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4231)).

begin(model(4232)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4232)).

begin(model(4233)).
bought(fish).
shops(mary).
end(model(4233)).

begin(model(4234)).
bought(fish).
shops(mary).
end(model(4234)).

begin(model(4235)).
bought(spaghetti).
shops(mary).
end(model(4235)).

begin(model(4236)).
bought(spaghetti).
shops(mary).
end(model(4236)).

begin(model(4237)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4237)).

begin(model(4238)).
bought(fish).
shops(mary).
end(model(4238)).

begin(model(4239)).
bought(spaghetti).
shops(mary).
end(model(4239)).

begin(model(4240)).
bought(spaghetti).
shops(mary).
end(model(4240)).

begin(model(4241)).
bought(fish).
shops(mary).
end(model(4241)).

begin(model(4242)).
end(model(4242)).

begin(model(4243)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4243)).

begin(model(4244)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4244)).

begin(model(4245)).
bought(fish).
shops(mary).
end(model(4245)).

begin(model(4246)).
bought(fish).
shops(mary).
end(model(4246)).

begin(model(4247)).
bought(spaghetti).
shops(mary).
end(model(4247)).

begin(model(4248)).
bought(fish).
shops(mary).
end(model(4248)).

begin(model(4249)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4249)).

begin(model(4250)).
bought(fish).
shops(mary).
end(model(4250)).

begin(model(4251)).
bought(fish).
shops(mary).
end(model(4251)).

begin(model(4252)).
bought(fish).
shops(mary).
end(model(4252)).

begin(model(4253)).
bought(spaghetti).
shops(mary).
end(model(4253)).

begin(model(4254)).
bought(spaghetti).
shops(mary).
end(model(4254)).

begin(model(4255)).
bought(spaghetti).
shops(mary).
end(model(4255)).

begin(model(4256)).
bought(fish).
shops(mary).
end(model(4256)).

begin(model(4257)).
bought(fish).
shops(mary).
end(model(4257)).

begin(model(4258)).
bought(spaghetti).
shops(mary).
end(model(4258)).

begin(model(4259)).
bought(spaghetti).
shops(mary).
end(model(4259)).

begin(model(4260)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4260)).

begin(model(4261)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4261)).

begin(model(4262)).
bought(spaghetti).
shops(mary).
end(model(4262)).

begin(model(4263)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4263)).

begin(model(4264)).
bought(spaghetti).
shops(mary).
end(model(4264)).

begin(model(4265)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4265)).

begin(model(4266)).
bought(fish).
shops(mary).
end(model(4266)).

begin(model(4267)).
bought(fish).
shops(mary).
end(model(4267)).

begin(model(4268)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4268)).

begin(model(4269)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4269)).

begin(model(4270)).
bought(fish).
shops(mary).
end(model(4270)).

begin(model(4271)).
bought(fish).
shops(mary).
end(model(4271)).

begin(model(4272)).
bought(spaghetti).
shops(mary).
end(model(4272)).

begin(model(4273)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4273)).

begin(model(4274)).
bought(fish).
shops(mary).
end(model(4274)).

begin(model(4275)).
bought(fish).
shops(mary).
end(model(4275)).

begin(model(4276)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4276)).

begin(model(4277)).
bought(fish).
shops(mary).
end(model(4277)).

begin(model(4278)).
bought(fish).
shops(mary).
end(model(4278)).

begin(model(4279)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4279)).

begin(model(4280)).
bought(fish).
shops(mary).
end(model(4280)).

begin(model(4281)).
bought(spaghetti).
shops(mary).
end(model(4281)).

begin(model(4282)).
bought(steak).
shops(john).
end(model(4282)).

begin(model(4283)).
end(model(4283)).

begin(model(4284)).
bought(fish).
shops(mary).
end(model(4284)).

begin(model(4285)).
end(model(4285)).

begin(model(4286)).
bought(steak).
shops(john).
end(model(4286)).

begin(model(4287)).
bought(fish).
shops(mary).
end(model(4287)).

begin(model(4288)).
bought(spaghetti).
shops(mary).
end(model(4288)).

begin(model(4289)).
bought(spaghetti).
shops(mary).
end(model(4289)).

begin(model(4290)).
bought(fish).
shops(mary).
end(model(4290)).

begin(model(4291)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4291)).

begin(model(4292)).
bought(spaghetti).
shops(mary).
end(model(4292)).

begin(model(4293)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4293)).

begin(model(4294)).
bought(fish).
shops(mary).
end(model(4294)).

begin(model(4295)).
bought(fish).
shops(mary).
end(model(4295)).

begin(model(4296)).
end(model(4296)).

begin(model(4297)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4297)).

begin(model(4298)).
bought(fish).
shops(mary).
end(model(4298)).

begin(model(4299)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4299)).

begin(model(4300)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4300)).

begin(model(4301)).
bought(fish).
shops(mary).
end(model(4301)).

begin(model(4302)).
bought(fish).
shops(mary).
end(model(4302)).

begin(model(4303)).
bought(spaghetti).
shops(mary).
end(model(4303)).

begin(model(4304)).
bought(fish).
shops(mary).
end(model(4304)).

begin(model(4305)).
bought(fish).
shops(mary).
end(model(4305)).

begin(model(4306)).
bought(fish).
shops(mary).
end(model(4306)).

begin(model(4307)).
bought(spaghetti).
shops(mary).
end(model(4307)).

begin(model(4308)).
bought(spaghetti).
shops(mary).
end(model(4308)).

begin(model(4309)).
bought(fish).
shops(mary).
end(model(4309)).

begin(model(4310)).
bought(spaghetti).
shops(mary).
end(model(4310)).

begin(model(4311)).
bought(steak).
shops(john).
end(model(4311)).

begin(model(4312)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4312)).

begin(model(4313)).
bought(spaghetti).
shops(mary).
end(model(4313)).

begin(model(4314)).
bought(fish).
shops(mary).
end(model(4314)).

begin(model(4315)).
bought(spaghetti).
shops(mary).
end(model(4315)).

begin(model(4316)).
bought(fish).
shops(mary).
end(model(4316)).

begin(model(4317)).
bought(spaghetti).
shops(mary).
end(model(4317)).

begin(model(4318)).
bought(fish).
shops(mary).
end(model(4318)).

begin(model(4319)).
bought(fish).
shops(mary).
end(model(4319)).

begin(model(4320)).
bought(spaghetti).
shops(mary).
end(model(4320)).

begin(model(4321)).
bought(fish).
shops(mary).
end(model(4321)).

begin(model(4322)).
bought(spaghetti).
shops(mary).
end(model(4322)).

begin(model(4323)).
bought(fish).
shops(mary).
end(model(4323)).

begin(model(4324)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4324)).

begin(model(4325)).
bought(fish).
shops(mary).
end(model(4325)).

begin(model(4326)).
end(model(4326)).

begin(model(4327)).
bought(fish).
shops(mary).
end(model(4327)).

begin(model(4328)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4328)).

begin(model(4329)).
bought(fish).
shops(mary).
end(model(4329)).

begin(model(4330)).
bought(fish).
shops(mary).
end(model(4330)).

begin(model(4331)).
end(model(4331)).

begin(model(4332)).
bought(fish).
shops(mary).
end(model(4332)).

begin(model(4333)).
bought(fish).
shops(mary).
end(model(4333)).

begin(model(4334)).
bought(fish).
shops(mary).
end(model(4334)).

begin(model(4335)).
bought(fish).
shops(mary).
end(model(4335)).

begin(model(4336)).
bought(fish).
shops(mary).
end(model(4336)).

begin(model(4337)).
bought(fish).
shops(mary).
end(model(4337)).

begin(model(4338)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4338)).

begin(model(4339)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4339)).

begin(model(4340)).
bought(fish).
shops(mary).
end(model(4340)).

begin(model(4341)).
bought(spaghetti).
shops(mary).
end(model(4341)).

begin(model(4342)).
bought(spaghetti).
shops(mary).
end(model(4342)).

begin(model(4343)).
bought(spaghetti).
shops(mary).
end(model(4343)).

begin(model(4344)).
end(model(4344)).

begin(model(4345)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4345)).

begin(model(4346)).
bought(fish).
shops(mary).
end(model(4346)).

begin(model(4347)).
bought(fish).
shops(mary).
end(model(4347)).

begin(model(4348)).
bought(fish).
shops(mary).
end(model(4348)).

begin(model(4349)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4349)).

begin(model(4350)).
bought(spaghetti).
shops(mary).
end(model(4350)).

begin(model(4351)).
bought(spaghetti).
shops(mary).
end(model(4351)).

begin(model(4352)).
bought(fish).
shops(mary).
end(model(4352)).

begin(model(4353)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4353)).

begin(model(4354)).
bought(spaghetti).
shops(mary).
end(model(4354)).

begin(model(4355)).
bought(spaghetti).
shops(mary).
end(model(4355)).

begin(model(4356)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4356)).

begin(model(4357)).
bought(spaghetti).
shops(mary).
end(model(4357)).

begin(model(4358)).
bought(fish).
shops(mary).
end(model(4358)).

begin(model(4359)).
bought(fish).
shops(mary).
end(model(4359)).

begin(model(4360)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4360)).

begin(model(4361)).
bought(spaghetti).
shops(mary).
end(model(4361)).

begin(model(4362)).
bought(fish).
shops(mary).
end(model(4362)).

begin(model(4363)).
end(model(4363)).

begin(model(4364)).
bought(spaghetti).
shops(mary).
end(model(4364)).

begin(model(4365)).
bought(spaghetti).
shops(mary).
end(model(4365)).

begin(model(4366)).
bought(fish).
shops(mary).
end(model(4366)).

begin(model(4367)).
bought(fish).
shops(mary).
end(model(4367)).

begin(model(4368)).
end(model(4368)).

begin(model(4369)).
bought(fish).
shops(mary).
end(model(4369)).

begin(model(4370)).
bought(fish).
shops(mary).
end(model(4370)).

begin(model(4371)).
bought(fish).
shops(mary).
end(model(4371)).

begin(model(4372)).
bought(fish).
shops(mary).
end(model(4372)).

begin(model(4373)).
bought(fish).
shops(mary).
end(model(4373)).

begin(model(4374)).
bought(fish).
shops(mary).
end(model(4374)).

begin(model(4375)).
bought(spaghetti).
shops(mary).
end(model(4375)).

begin(model(4376)).
bought(fish).
shops(mary).
end(model(4376)).

begin(model(4377)).
bought(fish).
shops(mary).
end(model(4377)).

begin(model(4378)).
bought(fish).
shops(mary).
end(model(4378)).

begin(model(4379)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4379)).

begin(model(4380)).
bought(spaghetti).
shops(mary).
end(model(4380)).

begin(model(4381)).
bought(fish).
shops(mary).
end(model(4381)).

begin(model(4382)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4382)).

begin(model(4383)).
bought(fish).
shops(mary).
end(model(4383)).

begin(model(4384)).
bought(fish).
shops(mary).
end(model(4384)).

begin(model(4385)).
bought(spaghetti).
shops(mary).
end(model(4385)).

begin(model(4386)).
bought(fish).
shops(mary).
end(model(4386)).

begin(model(4387)).
bought(fish).
shops(mary).
end(model(4387)).

begin(model(4388)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4388)).

begin(model(4389)).
bought(fish).
shops(mary).
end(model(4389)).

begin(model(4390)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4390)).

begin(model(4391)).
bought(fish).
shops(mary).
end(model(4391)).

begin(model(4392)).
bought(fish).
shops(mary).
end(model(4392)).

begin(model(4393)).
bought(fish).
shops(mary).
end(model(4393)).

begin(model(4394)).
bought(spaghetti).
shops(mary).
end(model(4394)).

begin(model(4395)).
bought(fish).
shops(mary).
end(model(4395)).

begin(model(4396)).
bought(fish).
shops(mary).
end(model(4396)).

begin(model(4397)).
bought(fish).
shops(mary).
end(model(4397)).

begin(model(4398)).
bought(fish).
shops(mary).
end(model(4398)).

begin(model(4399)).
bought(spaghetti).
shops(mary).
end(model(4399)).

begin(model(4400)).
bought(fish).
shops(mary).
end(model(4400)).

begin(model(4401)).
bought(steak).
shops(john).
end(model(4401)).

begin(model(4402)).
bought(fish).
shops(mary).
end(model(4402)).

begin(model(4403)).
bought(fish).
shops(mary).
end(model(4403)).

begin(model(4404)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4404)).

begin(model(4405)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4405)).

begin(model(4406)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4406)).

begin(model(4407)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4407)).

begin(model(4408)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4408)).

begin(model(4409)).
bought(fish).
shops(mary).
end(model(4409)).

begin(model(4410)).
bought(spaghetti).
shops(mary).
end(model(4410)).

begin(model(4411)).
bought(fish).
shops(mary).
end(model(4411)).

begin(model(4412)).
end(model(4412)).

begin(model(4413)).
bought(spaghetti).
shops(mary).
end(model(4413)).

begin(model(4414)).
bought(fish).
shops(mary).
end(model(4414)).

begin(model(4415)).
bought(fish).
shops(mary).
end(model(4415)).

begin(model(4416)).
bought(spaghetti).
shops(mary).
end(model(4416)).

begin(model(4417)).
bought(fish).
shops(mary).
end(model(4417)).

begin(model(4418)).
bought(fish).
shops(mary).
end(model(4418)).

begin(model(4419)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4419)).

begin(model(4420)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4420)).

begin(model(4421)).
bought(fish).
shops(mary).
end(model(4421)).

begin(model(4422)).
bought(spaghetti).
shops(mary).
end(model(4422)).

begin(model(4423)).
bought(fish).
shops(mary).
end(model(4423)).

begin(model(4424)).
bought(fish).
shops(mary).
end(model(4424)).

begin(model(4425)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4425)).

begin(model(4426)).
bought(fish).
shops(mary).
end(model(4426)).

begin(model(4427)).
bought(spaghetti).
shops(mary).
end(model(4427)).

begin(model(4428)).
bought(fish).
shops(mary).
end(model(4428)).

begin(model(4429)).
bought(spaghetti).
shops(mary).
end(model(4429)).

begin(model(4430)).
bought(spaghetti).
shops(mary).
end(model(4430)).

begin(model(4431)).
bought(spaghetti).
shops(mary).
end(model(4431)).

begin(model(4432)).
end(model(4432)).

begin(model(4433)).
bought(fish).
shops(mary).
end(model(4433)).

begin(model(4434)).
bought(spaghetti).
shops(mary).
end(model(4434)).

begin(model(4435)).
bought(fish).
shops(mary).
end(model(4435)).

begin(model(4436)).
bought(fish).
shops(mary).
end(model(4436)).

begin(model(4437)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4437)).

begin(model(4438)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4438)).

begin(model(4439)).
bought(fish).
shops(mary).
end(model(4439)).

begin(model(4440)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4440)).

begin(model(4441)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4441)).

begin(model(4442)).
bought(fish).
shops(mary).
end(model(4442)).

begin(model(4443)).
bought(fish).
shops(mary).
end(model(4443)).

begin(model(4444)).
bought(fish).
shops(mary).
end(model(4444)).

begin(model(4445)).
bought(fish).
shops(mary).
end(model(4445)).

begin(model(4446)).
bought(fish).
shops(mary).
end(model(4446)).

begin(model(4447)).
bought(fish).
shops(mary).
end(model(4447)).

begin(model(4448)).
bought(fish).
shops(mary).
end(model(4448)).

begin(model(4449)).
bought(fish).
shops(mary).
end(model(4449)).

begin(model(4450)).
bought(fish).
shops(mary).
end(model(4450)).

begin(model(4451)).
end(model(4451)).

begin(model(4452)).
bought(spaghetti).
shops(mary).
end(model(4452)).

begin(model(4453)).
bought(fish).
shops(mary).
end(model(4453)).

begin(model(4454)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4454)).

begin(model(4455)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4455)).

begin(model(4456)).
bought(fish).
shops(mary).
end(model(4456)).

begin(model(4457)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4457)).

begin(model(4458)).
bought(fish).
shops(mary).
end(model(4458)).

begin(model(4459)).
end(model(4459)).

begin(model(4460)).
bought(spaghetti).
shops(mary).
end(model(4460)).

begin(model(4461)).
bought(fish).
shops(mary).
end(model(4461)).

begin(model(4462)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4462)).

begin(model(4463)).
bought(fish).
shops(mary).
end(model(4463)).

begin(model(4464)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4464)).

begin(model(4465)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4465)).

begin(model(4466)).
bought(fish).
shops(mary).
end(model(4466)).

begin(model(4467)).
bought(fish).
shops(mary).
end(model(4467)).

begin(model(4468)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4468)).

begin(model(4469)).
bought(fish).
shops(mary).
end(model(4469)).

begin(model(4470)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4470)).

begin(model(4471)).
end(model(4471)).

begin(model(4472)).
bought(fish).
shops(mary).
end(model(4472)).

begin(model(4473)).
bought(fish).
shops(mary).
end(model(4473)).

begin(model(4474)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4474)).

begin(model(4475)).
bought(spaghetti).
shops(mary).
end(model(4475)).

begin(model(4476)).
bought(fish).
shops(mary).
end(model(4476)).

begin(model(4477)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4477)).

begin(model(4478)).
bought(fish).
shops(mary).
end(model(4478)).

begin(model(4479)).
bought(fish).
shops(mary).
end(model(4479)).

begin(model(4480)).
bought(fish).
shops(mary).
end(model(4480)).

begin(model(4481)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4481)).

begin(model(4482)).
bought(fish).
shops(mary).
end(model(4482)).

begin(model(4483)).
bought(fish).
shops(mary).
end(model(4483)).

begin(model(4484)).
bought(fish).
shops(mary).
end(model(4484)).

begin(model(4485)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4485)).

begin(model(4486)).
bought(spaghetti).
shops(mary).
end(model(4486)).

begin(model(4487)).
bought(spaghetti).
shops(mary).
end(model(4487)).

begin(model(4488)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4488)).

begin(model(4489)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4489)).

begin(model(4490)).
bought(fish).
shops(mary).
end(model(4490)).

begin(model(4491)).
bought(spaghetti).
shops(mary).
end(model(4491)).

begin(model(4492)).
bought(fish).
shops(mary).
end(model(4492)).

begin(model(4493)).
bought(spaghetti).
shops(mary).
end(model(4493)).

begin(model(4494)).
bought(spaghetti).
shops(mary).
end(model(4494)).

begin(model(4495)).
bought(spaghetti).
shops(mary).
end(model(4495)).

begin(model(4496)).
end(model(4496)).

begin(model(4497)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4497)).

begin(model(4498)).
bought(fish).
shops(mary).
end(model(4498)).

begin(model(4499)).
bought(fish).
shops(mary).
end(model(4499)).

begin(model(4500)).
bought(fish).
shops(mary).
end(model(4500)).

begin(model(4501)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4501)).

begin(model(4502)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4502)).

begin(model(4503)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4503)).

begin(model(4504)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4504)).

begin(model(4505)).
end(model(4505)).

begin(model(4506)).
bought(spaghetti).
shops(mary).
end(model(4506)).

begin(model(4507)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4507)).

begin(model(4508)).
bought(spaghetti).
shops(mary).
end(model(4508)).

begin(model(4509)).
bought(fish).
shops(mary).
end(model(4509)).

begin(model(4510)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4510)).

begin(model(4511)).
bought(fish).
shops(mary).
end(model(4511)).

begin(model(4512)).
end(model(4512)).

begin(model(4513)).
end(model(4513)).

begin(model(4514)).
bought(fish).
shops(mary).
end(model(4514)).

begin(model(4515)).
bought(spaghetti).
shops(mary).
end(model(4515)).

begin(model(4516)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4516)).

begin(model(4517)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4517)).

begin(model(4518)).
bought(spaghetti).
shops(mary).
end(model(4518)).

begin(model(4519)).
bought(fish).
shops(mary).
end(model(4519)).

begin(model(4520)).
bought(fish).
shops(mary).
end(model(4520)).

begin(model(4521)).
bought(fish).
shops(mary).
end(model(4521)).

begin(model(4522)).
bought(fish).
shops(mary).
end(model(4522)).

begin(model(4523)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4523)).

begin(model(4524)).
bought(spaghetti).
shops(mary).
end(model(4524)).

begin(model(4525)).
bought(fish).
shops(mary).
end(model(4525)).

begin(model(4526)).
bought(spaghetti).
shops(mary).
end(model(4526)).

begin(model(4527)).
end(model(4527)).

begin(model(4528)).
bought(fish).
shops(mary).
end(model(4528)).

begin(model(4529)).
bought(fish).
shops(mary).
end(model(4529)).

begin(model(4530)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4530)).

begin(model(4531)).
bought(spaghetti).
shops(mary).
end(model(4531)).

begin(model(4532)).
bought(fish).
shops(mary).
end(model(4532)).

begin(model(4533)).
bought(spaghetti).
shops(mary).
end(model(4533)).

begin(model(4534)).
bought(spaghetti).
shops(mary).
end(model(4534)).

begin(model(4535)).
bought(fish).
shops(mary).
end(model(4535)).

begin(model(4536)).
bought(fish).
shops(mary).
end(model(4536)).

begin(model(4537)).
end(model(4537)).

begin(model(4538)).
bought(spaghetti).
shops(mary).
end(model(4538)).

begin(model(4539)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4539)).

begin(model(4540)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4540)).

begin(model(4541)).
bought(spaghetti).
shops(mary).
end(model(4541)).

begin(model(4542)).
bought(fish).
shops(mary).
end(model(4542)).

begin(model(4543)).
bought(fish).
shops(mary).
end(model(4543)).

begin(model(4544)).
bought(fish).
shops(mary).
end(model(4544)).

begin(model(4545)).
end(model(4545)).

begin(model(4546)).
bought(spaghetti).
shops(mary).
end(model(4546)).

begin(model(4547)).
bought(fish).
shops(mary).
end(model(4547)).

begin(model(4548)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4548)).

begin(model(4549)).
bought(fish).
shops(mary).
end(model(4549)).

begin(model(4550)).
bought(fish).
shops(mary).
end(model(4550)).

begin(model(4551)).
bought(fish).
shops(mary).
end(model(4551)).

begin(model(4552)).
bought(fish).
shops(mary).
end(model(4552)).

begin(model(4553)).
bought(fish).
shops(mary).
end(model(4553)).

begin(model(4554)).
bought(fish).
shops(mary).
end(model(4554)).

begin(model(4555)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4555)).

begin(model(4556)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4556)).

begin(model(4557)).
end(model(4557)).

begin(model(4558)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4558)).

begin(model(4559)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4559)).

begin(model(4560)).
end(model(4560)).

begin(model(4561)).
bought(fish).
shops(mary).
end(model(4561)).

begin(model(4562)).
bought(fish).
shops(mary).
end(model(4562)).

begin(model(4563)).
bought(fish).
shops(mary).
end(model(4563)).

begin(model(4564)).
bought(fish).
shops(mary).
end(model(4564)).

begin(model(4565)).
bought(fish).
shops(mary).
end(model(4565)).

begin(model(4566)).
bought(spaghetti).
shops(mary).
end(model(4566)).

begin(model(4567)).
bought(fish).
shops(mary).
end(model(4567)).

begin(model(4568)).
bought(fish).
shops(mary).
end(model(4568)).

begin(model(4569)).
end(model(4569)).

begin(model(4570)).
bought(fish).
shops(mary).
end(model(4570)).

begin(model(4571)).
bought(spaghetti).
shops(mary).
end(model(4571)).

begin(model(4572)).
bought(fish).
shops(mary).
end(model(4572)).

begin(model(4573)).
bought(fish).
shops(mary).
end(model(4573)).

begin(model(4574)).
bought(fish).
shops(mary).
end(model(4574)).

begin(model(4575)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4575)).

begin(model(4576)).
bought(spaghetti).
shops(mary).
end(model(4576)).

begin(model(4577)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4577)).

begin(model(4578)).
bought(fish).
shops(mary).
end(model(4578)).

begin(model(4579)).
bought(spaghetti).
shops(mary).
end(model(4579)).

begin(model(4580)).
bought(spaghetti).
shops(mary).
end(model(4580)).

begin(model(4581)).
bought(spaghetti).
shops(mary).
end(model(4581)).

begin(model(4582)).
bought(spaghetti).
shops(john).
end(model(4582)).

begin(model(4583)).
bought(spaghetti).
shops(mary).
end(model(4583)).

begin(model(4584)).
bought(fish).
shops(mary).
end(model(4584)).

begin(model(4585)).
bought(fish).
shops(mary).
end(model(4585)).

begin(model(4586)).
end(model(4586)).

begin(model(4587)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4587)).

begin(model(4588)).
bought(fish).
shops(mary).
end(model(4588)).

begin(model(4589)).
bought(spaghetti).
shops(mary).
end(model(4589)).

begin(model(4590)).
bought(fish).
shops(mary).
end(model(4590)).

begin(model(4591)).
bought(spaghetti).
shops(mary).
end(model(4591)).

begin(model(4592)).
bought(spaghetti).
shops(mary).
end(model(4592)).

begin(model(4593)).
bought(spaghetti).
shops(mary).
end(model(4593)).

begin(model(4594)).
bought(fish).
shops(mary).
end(model(4594)).

begin(model(4595)).
bought(fish).
shops(mary).
end(model(4595)).

begin(model(4596)).
end(model(4596)).

begin(model(4597)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4597)).

begin(model(4598)).
bought(fish).
shops(mary).
end(model(4598)).

begin(model(4599)).
bought(fish).
shops(mary).
end(model(4599)).

begin(model(4600)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4600)).

begin(model(4601)).
bought(fish).
shops(mary).
end(model(4601)).

begin(model(4602)).
bought(fish).
shops(mary).
end(model(4602)).

begin(model(4603)).
bought(spaghetti).
shops(mary).
end(model(4603)).

begin(model(4604)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4604)).

begin(model(4605)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4605)).

begin(model(4606)).
bought(spaghetti).
shops(mary).
end(model(4606)).

begin(model(4607)).
bought(fish).
shops(mary).
end(model(4607)).

begin(model(4608)).
bought(fish).
shops(mary).
end(model(4608)).

begin(model(4609)).
bought(fish).
shops(mary).
end(model(4609)).

begin(model(4610)).
bought(fish).
shops(mary).
end(model(4610)).

begin(model(4611)).
bought(fish).
shops(mary).
end(model(4611)).

begin(model(4612)).
bought(spaghetti).
shops(mary).
end(model(4612)).

begin(model(4613)).
bought(spaghetti).
shops(mary).
end(model(4613)).

begin(model(4614)).
bought(fish).
shops(mary).
end(model(4614)).

begin(model(4615)).
bought(spaghetti).
shops(mary).
end(model(4615)).

begin(model(4616)).
bought(fish).
shops(mary).
end(model(4616)).

begin(model(4617)).
bought(fish).
shops(mary).
end(model(4617)).

begin(model(4618)).
bought(fish).
shops(mary).
end(model(4618)).

begin(model(4619)).
bought(spaghetti).
shops(mary).
end(model(4619)).

begin(model(4620)).
bought(fish).
shops(mary).
end(model(4620)).

begin(model(4621)).
bought(fish).
shops(mary).
end(model(4621)).

begin(model(4622)).
bought(fish).
shops(mary).
end(model(4622)).

begin(model(4623)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4623)).

begin(model(4624)).
bought(fish).
shops(mary).
end(model(4624)).

begin(model(4625)).
bought(fish).
shops(mary).
end(model(4625)).

begin(model(4626)).
bought(spaghetti).
shops(mary).
end(model(4626)).

begin(model(4627)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4627)).

begin(model(4628)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4628)).

begin(model(4629)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4629)).

begin(model(4630)).
bought(fish).
shops(mary).
end(model(4630)).

begin(model(4631)).
bought(fish).
shops(mary).
end(model(4631)).

begin(model(4632)).
bought(fish).
shops(mary).
end(model(4632)).

begin(model(4633)).
bought(steak).
shops(john).
end(model(4633)).

begin(model(4634)).
bought(fish).
shops(mary).
end(model(4634)).

begin(model(4635)).
bought(spaghetti).
shops(mary).
end(model(4635)).

begin(model(4636)).
end(model(4636)).

begin(model(4637)).
bought(fish).
shops(mary).
end(model(4637)).

begin(model(4638)).
bought(fish).
shops(mary).
end(model(4638)).

begin(model(4639)).
bought(fish).
shops(mary).
end(model(4639)).

begin(model(4640)).
bought(fish).
shops(mary).
end(model(4640)).

begin(model(4641)).
bought(fish).
shops(mary).
end(model(4641)).

begin(model(4642)).
bought(fish).
shops(mary).
end(model(4642)).

begin(model(4643)).
end(model(4643)).

begin(model(4644)).
bought(spaghetti).
shops(john).
end(model(4644)).

begin(model(4645)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4645)).

begin(model(4646)).
bought(fish).
shops(mary).
end(model(4646)).

begin(model(4647)).
bought(fish).
shops(mary).
end(model(4647)).

begin(model(4648)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4648)).

begin(model(4649)).
bought(fish).
shops(mary).
end(model(4649)).

begin(model(4650)).
end(model(4650)).

begin(model(4651)).
bought(fish).
shops(mary).
end(model(4651)).

begin(model(4652)).
bought(fish).
shops(mary).
end(model(4652)).

begin(model(4653)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4653)).

begin(model(4654)).
bought(fish).
shops(mary).
end(model(4654)).

begin(model(4655)).
bought(fish).
shops(mary).
end(model(4655)).

begin(model(4656)).
bought(spaghetti).
shops(mary).
end(model(4656)).

begin(model(4657)).
bought(spaghetti).
shops(mary).
end(model(4657)).

begin(model(4658)).
bought(fish).
shops(mary).
end(model(4658)).

begin(model(4659)).
end(model(4659)).

begin(model(4660)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4660)).

begin(model(4661)).
bought(fish).
shops(mary).
end(model(4661)).

begin(model(4662)).
bought(fish).
shops(mary).
end(model(4662)).

begin(model(4663)).
bought(fish).
shops(mary).
end(model(4663)).

begin(model(4664)).
bought(fish).
shops(mary).
end(model(4664)).

begin(model(4665)).
bought(fish).
shops(mary).
end(model(4665)).

begin(model(4666)).
bought(fish).
shops(mary).
end(model(4666)).

begin(model(4667)).
bought(spaghetti).
shops(mary).
end(model(4667)).

begin(model(4668)).
bought(fish).
shops(mary).
end(model(4668)).

begin(model(4669)).
bought(fish).
shops(mary).
end(model(4669)).

begin(model(4670)).
bought(spaghetti).
shops(mary).
end(model(4670)).

begin(model(4671)).
bought(fish).
shops(mary).
end(model(4671)).

begin(model(4672)).
bought(fish).
shops(mary).
end(model(4672)).

begin(model(4673)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4673)).

begin(model(4674)).
bought(fish).
shops(mary).
end(model(4674)).

begin(model(4675)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4675)).

begin(model(4676)).
bought(spaghetti).
shops(mary).
end(model(4676)).

begin(model(4677)).
end(model(4677)).

begin(model(4678)).
bought(spaghetti).
shops(john).
end(model(4678)).

begin(model(4679)).
bought(spaghetti).
shops(mary).
end(model(4679)).

begin(model(4680)).
bought(fish).
shops(mary).
end(model(4680)).

begin(model(4681)).
bought(spaghetti).
shops(mary).
end(model(4681)).

begin(model(4682)).
end(model(4682)).

begin(model(4683)).
bought(fish).
shops(mary).
end(model(4683)).

begin(model(4684)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4684)).

begin(model(4685)).
bought(spaghetti).
shops(mary).
end(model(4685)).

begin(model(4686)).
bought(fish).
shops(mary).
end(model(4686)).

begin(model(4687)).
bought(fish).
shops(mary).
end(model(4687)).

begin(model(4688)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4688)).

begin(model(4689)).
bought(fish).
shops(mary).
end(model(4689)).

begin(model(4690)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4690)).

begin(model(4691)).
bought(spaghetti).
shops(mary).
end(model(4691)).

begin(model(4692)).
bought(fish).
shops(mary).
end(model(4692)).

begin(model(4693)).
bought(fish).
shops(mary).
end(model(4693)).

begin(model(4694)).
bought(fish).
shops(mary).
end(model(4694)).

begin(model(4695)).
bought(fish).
shops(mary).
end(model(4695)).

begin(model(4696)).
bought(spaghetti).
shops(mary).
end(model(4696)).

begin(model(4697)).
bought(fish).
shops(mary).
end(model(4697)).

begin(model(4698)).
bought(fish).
shops(mary).
end(model(4698)).

begin(model(4699)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4699)).

begin(model(4700)).
bought(fish).
shops(mary).
end(model(4700)).

begin(model(4701)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4701)).

begin(model(4702)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4702)).

begin(model(4703)).
bought(spaghetti).
shops(mary).
end(model(4703)).

begin(model(4704)).
bought(fish).
shops(mary).
end(model(4704)).

begin(model(4705)).
bought(spaghetti).
shops(mary).
end(model(4705)).

begin(model(4706)).
bought(spaghetti).
shops(mary).
end(model(4706)).

begin(model(4707)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4707)).

begin(model(4708)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4708)).

begin(model(4709)).
bought(fish).
shops(mary).
end(model(4709)).

begin(model(4710)).
bought(fish).
shops(mary).
end(model(4710)).

begin(model(4711)).
bought(fish).
shops(mary).
end(model(4711)).

begin(model(4712)).
bought(fish).
shops(mary).
end(model(4712)).

begin(model(4713)).
bought(fish).
shops(mary).
end(model(4713)).

begin(model(4714)).
bought(fish).
shops(mary).
end(model(4714)).

begin(model(4715)).
bought(fish).
shops(mary).
end(model(4715)).

begin(model(4716)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4716)).

begin(model(4717)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4717)).

begin(model(4718)).
bought(fish).
shops(mary).
end(model(4718)).

begin(model(4719)).
bought(fish).
shops(mary).
end(model(4719)).

begin(model(4720)).
bought(fish).
shops(mary).
end(model(4720)).

begin(model(4721)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4721)).

begin(model(4722)).
bought(fish).
shops(mary).
end(model(4722)).

begin(model(4723)).
bought(spaghetti).
shops(mary).
end(model(4723)).

begin(model(4724)).
bought(steak).
shops(john).
end(model(4724)).

begin(model(4725)).
bought(fish).
shops(mary).
end(model(4725)).

begin(model(4726)).
bought(fish).
shops(mary).
end(model(4726)).

begin(model(4727)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4727)).

begin(model(4728)).
end(model(4728)).

begin(model(4729)).
bought(fish).
shops(mary).
end(model(4729)).

begin(model(4730)).
bought(spaghetti).
shops(john).
end(model(4730)).

begin(model(4731)).
bought(spaghetti).
shops(mary).
end(model(4731)).

begin(model(4732)).
bought(fish).
shops(mary).
end(model(4732)).

begin(model(4733)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4733)).

begin(model(4734)).
bought(fish).
shops(mary).
end(model(4734)).

begin(model(4735)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4735)).

begin(model(4736)).
bought(fish).
shops(mary).
end(model(4736)).

begin(model(4737)).
bought(fish).
shops(mary).
end(model(4737)).

begin(model(4738)).
bought(spaghetti).
shops(mary).
end(model(4738)).

begin(model(4739)).
bought(fish).
shops(mary).
end(model(4739)).

begin(model(4740)).
bought(fish).
shops(mary).
end(model(4740)).

begin(model(4741)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4741)).

begin(model(4742)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4742)).

begin(model(4743)).
bought(spaghetti).
shops(mary).
end(model(4743)).

begin(model(4744)).
bought(spaghetti).
shops(mary).
end(model(4744)).

begin(model(4745)).
bought(spaghetti).
shops(mary).
end(model(4745)).

begin(model(4746)).
bought(fish).
shops(mary).
end(model(4746)).

begin(model(4747)).
bought(fish).
shops(mary).
end(model(4747)).

begin(model(4748)).
bought(fish).
shops(mary).
end(model(4748)).

begin(model(4749)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4749)).

begin(model(4750)).
bought(spaghetti).
shops(mary).
end(model(4750)).

begin(model(4751)).
bought(fish).
shops(mary).
end(model(4751)).

begin(model(4752)).
bought(fish).
shops(mary).
end(model(4752)).

begin(model(4753)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4753)).

begin(model(4754)).
bought(fish).
shops(mary).
end(model(4754)).

begin(model(4755)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4755)).

begin(model(4756)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4756)).

begin(model(4757)).
bought(fish).
shops(mary).
end(model(4757)).

begin(model(4758)).
bought(fish).
shops(mary).
end(model(4758)).

begin(model(4759)).
end(model(4759)).

begin(model(4760)).
bought(fish).
shops(mary).
end(model(4760)).

begin(model(4761)).
bought(fish).
shops(mary).
end(model(4761)).

begin(model(4762)).
bought(fish).
shops(mary).
end(model(4762)).

begin(model(4763)).
bought(fish).
shops(mary).
end(model(4763)).

begin(model(4764)).
bought(fish).
shops(mary).
end(model(4764)).

begin(model(4765)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4765)).

begin(model(4766)).
bought(spaghetti).
shops(mary).
end(model(4766)).

begin(model(4767)).
bought(fish).
shops(mary).
end(model(4767)).

begin(model(4768)).
bought(spaghetti).
shops(mary).
end(model(4768)).

begin(model(4769)).
bought(spaghetti).
shops(mary).
end(model(4769)).

begin(model(4770)).
bought(spaghetti).
shops(mary).
end(model(4770)).

begin(model(4771)).
bought(fish).
shops(mary).
end(model(4771)).

begin(model(4772)).
bought(fish).
shops(mary).
end(model(4772)).

begin(model(4773)).
bought(fish).
shops(mary).
end(model(4773)).

begin(model(4774)).
bought(fish).
shops(mary).
end(model(4774)).

begin(model(4775)).
bought(fish).
shops(mary).
end(model(4775)).

begin(model(4776)).
bought(fish).
shops(mary).
end(model(4776)).

begin(model(4777)).
bought(spaghetti).
shops(mary).
end(model(4777)).

begin(model(4778)).
bought(fish).
shops(mary).
end(model(4778)).

begin(model(4779)).
bought(fish).
shops(mary).
end(model(4779)).

begin(model(4780)).
bought(fish).
shops(mary).
end(model(4780)).

begin(model(4781)).
bought(spaghetti).
shops(mary).
end(model(4781)).

begin(model(4782)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4782)).

begin(model(4783)).
bought(spaghetti).
shops(mary).
end(model(4783)).

begin(model(4784)).
bought(spaghetti).
shops(mary).
end(model(4784)).

begin(model(4785)).
end(model(4785)).

begin(model(4786)).
bought(spaghetti).
shops(mary).
end(model(4786)).

begin(model(4787)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4787)).

begin(model(4788)).
bought(fish).
shops(mary).
end(model(4788)).

begin(model(4789)).
bought(fish).
shops(mary).
end(model(4789)).

begin(model(4790)).
bought(fish).
shops(mary).
end(model(4790)).

begin(model(4791)).
bought(fish).
shops(mary).
end(model(4791)).

begin(model(4792)).
bought(spaghetti).
shops(mary).
end(model(4792)).

begin(model(4793)).
bought(fish).
shops(mary).
end(model(4793)).

begin(model(4794)).
bought(fish).
shops(mary).
end(model(4794)).

begin(model(4795)).
bought(spaghetti).
shops(mary).
end(model(4795)).

begin(model(4796)).
bought(fish).
shops(mary).
end(model(4796)).

begin(model(4797)).
bought(fish).
shops(mary).
end(model(4797)).

begin(model(4798)).
bought(spaghetti).
shops(mary).
end(model(4798)).

begin(model(4799)).
bought(spaghetti).
shops(mary).
end(model(4799)).

begin(model(4800)).
bought(fish).
shops(mary).
end(model(4800)).

begin(model(4801)).
bought(fish).
shops(mary).
end(model(4801)).

begin(model(4802)).
bought(fish).
shops(mary).
end(model(4802)).

begin(model(4803)).
bought(fish).
shops(mary).
end(model(4803)).

begin(model(4804)).
bought(spaghetti).
shops(mary).
end(model(4804)).

begin(model(4805)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4805)).

begin(model(4806)).
end(model(4806)).

begin(model(4807)).
bought(fish).
shops(mary).
end(model(4807)).

begin(model(4808)).
bought(fish).
shops(mary).
end(model(4808)).

begin(model(4809)).
bought(fish).
shops(mary).
end(model(4809)).

begin(model(4810)).
bought(fish).
shops(mary).
end(model(4810)).

begin(model(4811)).
bought(fish).
shops(mary).
end(model(4811)).

begin(model(4812)).
bought(fish).
shops(mary).
end(model(4812)).

begin(model(4813)).
bought(fish).
shops(mary).
end(model(4813)).

begin(model(4814)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4814)).

begin(model(4815)).
end(model(4815)).

begin(model(4816)).
bought(spaghetti).
shops(john).
end(model(4816)).

begin(model(4817)).
bought(spaghetti).
shops(mary).
end(model(4817)).

begin(model(4818)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4818)).

begin(model(4819)).
bought(fish).
shops(mary).
end(model(4819)).

begin(model(4820)).
bought(spaghetti).
shops(mary).
end(model(4820)).

begin(model(4821)).
bought(steak).
shops(john).
end(model(4821)).

begin(model(4822)).
bought(fish).
shops(mary).
end(model(4822)).

begin(model(4823)).
bought(fish).
shops(mary).
end(model(4823)).

begin(model(4824)).
bought(fish).
shops(mary).
end(model(4824)).

begin(model(4825)).
bought(spaghetti).
shops(mary).
end(model(4825)).

begin(model(4826)).
bought(fish).
shops(mary).
end(model(4826)).

begin(model(4827)).
bought(spaghetti).
shops(mary).
end(model(4827)).

begin(model(4828)).
bought(fish).
shops(mary).
end(model(4828)).

begin(model(4829)).
bought(spaghetti).
shops(mary).
end(model(4829)).

begin(model(4830)).
bought(fish).
shops(mary).
end(model(4830)).

begin(model(4831)).
bought(fish).
shops(mary).
end(model(4831)).

begin(model(4832)).
end(model(4832)).

begin(model(4833)).
bought(fish).
shops(mary).
end(model(4833)).

begin(model(4834)).
bought(fish).
shops(mary).
end(model(4834)).

begin(model(4835)).
bought(spaghetti).
shops(mary).
end(model(4835)).

begin(model(4836)).
end(model(4836)).

begin(model(4837)).
bought(fish).
shops(mary).
end(model(4837)).

begin(model(4838)).
bought(fish).
shops(mary).
end(model(4838)).

begin(model(4839)).
bought(spaghetti).
shops(mary).
end(model(4839)).

begin(model(4840)).
bought(fish).
shops(mary).
end(model(4840)).

begin(model(4841)).
bought(fish).
shops(mary).
end(model(4841)).

begin(model(4842)).
bought(fish).
shops(mary).
end(model(4842)).

begin(model(4843)).
bought(fish).
shops(mary).
end(model(4843)).

begin(model(4844)).
bought(fish).
shops(mary).
end(model(4844)).

begin(model(4845)).
bought(spaghetti).
shops(mary).
end(model(4845)).

begin(model(4846)).
bought(spaghetti).
shops(mary).
end(model(4846)).

begin(model(4847)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4847)).

begin(model(4848)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4848)).

begin(model(4849)).
bought(fish).
shops(mary).
end(model(4849)).

begin(model(4850)).
bought(fish).
shops(mary).
end(model(4850)).

begin(model(4851)).
bought(fish).
shops(mary).
end(model(4851)).

begin(model(4852)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4852)).

begin(model(4853)).
bought(fish).
shops(mary).
end(model(4853)).

begin(model(4854)).
bought(spaghetti).
shops(mary).
end(model(4854)).

begin(model(4855)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4855)).

begin(model(4856)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4856)).

begin(model(4857)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4857)).

begin(model(4858)).
bought(fish).
shops(mary).
end(model(4858)).

begin(model(4859)).
bought(steak).
shops(john).
end(model(4859)).

begin(model(4860)).
bought(fish).
shops(mary).
end(model(4860)).

begin(model(4861)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4861)).

begin(model(4862)).
bought(fish).
shops(mary).
end(model(4862)).

begin(model(4863)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4863)).

begin(model(4864)).
bought(fish).
shops(mary).
end(model(4864)).

begin(model(4865)).
bought(fish).
shops(mary).
end(model(4865)).

begin(model(4866)).
bought(spaghetti).
shops(mary).
end(model(4866)).

begin(model(4867)).
bought(fish).
shops(mary).
end(model(4867)).

begin(model(4868)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4868)).

begin(model(4869)).
bought(fish).
shops(mary).
end(model(4869)).

begin(model(4870)).
bought(spaghetti).
shops(mary).
end(model(4870)).

begin(model(4871)).
bought(spaghetti).
shops(mary).
end(model(4871)).

begin(model(4872)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4872)).

begin(model(4873)).
bought(fish).
shops(mary).
end(model(4873)).

begin(model(4874)).
bought(fish).
shops(mary).
end(model(4874)).

begin(model(4875)).
bought(fish).
shops(mary).
end(model(4875)).

begin(model(4876)).
bought(spaghetti).
shops(mary).
end(model(4876)).

begin(model(4877)).
bought(fish).
shops(mary).
end(model(4877)).

begin(model(4878)).
end(model(4878)).

begin(model(4879)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4879)).

begin(model(4880)).
bought(fish).
shops(mary).
end(model(4880)).

begin(model(4881)).
bought(fish).
shops(mary).
end(model(4881)).

begin(model(4882)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4882)).

begin(model(4883)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4883)).

begin(model(4884)).
bought(spaghetti).
shops(mary).
end(model(4884)).

begin(model(4885)).
bought(fish).
shops(mary).
end(model(4885)).

begin(model(4886)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4886)).

begin(model(4887)).
bought(spaghetti).
shops(mary).
end(model(4887)).

begin(model(4888)).
bought(spaghetti).
shops(mary).
end(model(4888)).

begin(model(4889)).
bought(spaghetti).
shops(mary).
end(model(4889)).

begin(model(4890)).
bought(fish).
shops(mary).
end(model(4890)).

begin(model(4891)).
bought(fish).
shops(mary).
end(model(4891)).

begin(model(4892)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4892)).

begin(model(4893)).
end(model(4893)).

begin(model(4894)).
bought(fish).
shops(mary).
end(model(4894)).

begin(model(4895)).
bought(fish).
shops(mary).
end(model(4895)).

begin(model(4896)).
end(model(4896)).

begin(model(4897)).
bought(fish).
shops(mary).
end(model(4897)).

begin(model(4898)).
end(model(4898)).

begin(model(4899)).
bought(spaghetti).
shops(mary).
end(model(4899)).

begin(model(4900)).
bought(fish).
shops(mary).
end(model(4900)).

begin(model(4901)).
bought(fish).
shops(mary).
end(model(4901)).

begin(model(4902)).
bought(fish).
shops(mary).
end(model(4902)).

begin(model(4903)).
bought(fish).
shops(mary).
end(model(4903)).

begin(model(4904)).
bought(fish).
shops(mary).
end(model(4904)).

begin(model(4905)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4905)).

begin(model(4906)).
bought(fish).
shops(mary).
end(model(4906)).

begin(model(4907)).
bought(spaghetti).
shops(mary).
end(model(4907)).

begin(model(4908)).
bought(fish).
shops(mary).
end(model(4908)).

begin(model(4909)).
end(model(4909)).

begin(model(4910)).
bought(fish).
shops(mary).
end(model(4910)).

begin(model(4911)).
bought(fish).
shops(mary).
end(model(4911)).

begin(model(4912)).
bought(fish).
shops(mary).
end(model(4912)).

begin(model(4913)).
bought(fish).
shops(mary).
end(model(4913)).

begin(model(4914)).
bought(fish).
shops(mary).
end(model(4914)).

begin(model(4915)).
bought(fish).
shops(mary).
end(model(4915)).

begin(model(4916)).
bought(fish).
shops(mary).
end(model(4916)).

begin(model(4917)).
bought(spaghetti).
shops(mary).
end(model(4917)).

begin(model(4918)).
bought(fish).
shops(mary).
end(model(4918)).

begin(model(4919)).
bought(spaghetti).
shops(mary).
end(model(4919)).

begin(model(4920)).
bought(fish).
shops(mary).
end(model(4920)).

begin(model(4921)).
bought(fish).
shops(mary).
end(model(4921)).

begin(model(4922)).
bought(spaghetti).
shops(john).
end(model(4922)).

begin(model(4923)).
end(model(4923)).

begin(model(4924)).
bought(spaghetti).
shops(mary).
end(model(4924)).

begin(model(4925)).
end(model(4925)).

begin(model(4926)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4926)).

begin(model(4927)).
end(model(4927)).

begin(model(4928)).
bought(spaghetti).
shops(mary).
end(model(4928)).

begin(model(4929)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4929)).

begin(model(4930)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4930)).

begin(model(4931)).
bought(fish).
shops(mary).
end(model(4931)).

begin(model(4932)).
bought(fish).
shops(mary).
end(model(4932)).

begin(model(4933)).
bought(fish).
shops(mary).
end(model(4933)).

begin(model(4934)).
bought(spaghetti).
shops(mary).
end(model(4934)).

begin(model(4935)).
bought(fish).
shops(mary).
end(model(4935)).

begin(model(4936)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4936)).

begin(model(4937)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4937)).

begin(model(4938)).
bought(fish).
shops(mary).
end(model(4938)).

begin(model(4939)).
bought(fish).
shops(mary).
end(model(4939)).

begin(model(4940)).
bought(fish).
shops(mary).
end(model(4940)).

begin(model(4941)).
end(model(4941)).

begin(model(4942)).
bought(fish).
shops(mary).
end(model(4942)).

begin(model(4943)).
bought(fish).
shops(mary).
end(model(4943)).

begin(model(4944)).
bought(spaghetti).
shops(mary).
end(model(4944)).

begin(model(4945)).
bought(fish).
shops(mary).
end(model(4945)).

begin(model(4946)).
bought(spaghetti).
shops(mary).
end(model(4946)).

begin(model(4947)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4947)).

begin(model(4948)).
bought(spaghetti).
shops(mary).
end(model(4948)).

begin(model(4949)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4949)).

begin(model(4950)).
bought(spaghetti).
shops(mary).
end(model(4950)).

begin(model(4951)).
end(model(4951)).

begin(model(4952)).
bought(spaghetti).
shops(mary).
end(model(4952)).

begin(model(4953)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4953)).

begin(model(4954)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4954)).

begin(model(4955)).
end(model(4955)).

begin(model(4956)).
bought(spaghetti).
shops(mary).
end(model(4956)).

begin(model(4957)).
bought(fish).
shops(mary).
end(model(4957)).

begin(model(4958)).
bought(spaghetti).
shops(mary).
end(model(4958)).

begin(model(4959)).
bought(fish).
shops(mary).
end(model(4959)).

begin(model(4960)).
bought(fish).
shops(mary).
end(model(4960)).

begin(model(4961)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4961)).

begin(model(4962)).
bought(spaghetti).
shops(mary).
end(model(4962)).

begin(model(4963)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4963)).

begin(model(4964)).
bought(spaghetti).
shops(mary).
end(model(4964)).

begin(model(4965)).
bought(spaghetti).
shops(mary).
end(model(4965)).

begin(model(4966)).
bought(fish).
shops(mary).
end(model(4966)).

begin(model(4967)).
bought(fish).
shops(mary).
end(model(4967)).

begin(model(4968)).
bought(fish).
shops(mary).
end(model(4968)).

begin(model(4969)).
bought(fish).
shops(mary).
end(model(4969)).

begin(model(4970)).
bought(spaghetti).
shops(mary).
end(model(4970)).

begin(model(4971)).
end(model(4971)).

begin(model(4972)).
bought(steak).
shops(john).
end(model(4972)).

begin(model(4973)).
end(model(4973)).

begin(model(4974)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4974)).

begin(model(4975)).
bought(fish).
shops(mary).
end(model(4975)).

begin(model(4976)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4976)).

begin(model(4977)).
bought(fish).
shops(mary).
end(model(4977)).

begin(model(4978)).
bought(spaghetti).
shops(mary).
end(model(4978)).

begin(model(4979)).
bought(fish).
shops(mary).
end(model(4979)).

begin(model(4980)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4980)).

begin(model(4981)).
bought(fish).
shops(mary).
end(model(4981)).

begin(model(4982)).
bought(spaghetti).
shops(mary).
end(model(4982)).

begin(model(4983)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4983)).

begin(model(4984)).
bought(spaghetti).
shops(mary).
end(model(4984)).

begin(model(4985)).
bought(spaghetti).
shops(mary).
end(model(4985)).

begin(model(4986)).
end(model(4986)).

begin(model(4987)).
end(model(4987)).

begin(model(4988)).
bought(fish).
shops(mary).
end(model(4988)).

begin(model(4989)).
bought(fish).
shops(mary).
end(model(4989)).

begin(model(4990)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(4990)).

begin(model(4991)).
end(model(4991)).

begin(model(4992)).
bought(spaghetti).
shops(mary).
end(model(4992)).

begin(model(4993)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4993)).

begin(model(4994)).
bought(spaghetti).
shops(mary).
end(model(4994)).

begin(model(4995)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4995)).

begin(model(4996)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(4996)).

begin(model(4997)).
bought(fish).
shops(mary).
end(model(4997)).

begin(model(4998)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(4998)).

begin(model(4999)).
bought(spaghetti).
shops(mary).
end(model(4999)).

begin(model(5000)).
bought(spaghetti).
shops(mary).
end(model(5000)).

begin(model(5001)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5001)).

begin(model(5002)).
bought(fish).
shops(mary).
end(model(5002)).

begin(model(5003)).
bought(fish).
shops(mary).
end(model(5003)).

begin(model(5004)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5004)).

begin(model(5005)).
bought(fish).
shops(mary).
end(model(5005)).

begin(model(5006)).
bought(fish).
shops(mary).
end(model(5006)).

begin(model(5007)).
bought(spaghetti).
shops(mary).
end(model(5007)).

begin(model(5008)).
bought(fish).
shops(mary).
end(model(5008)).

begin(model(5009)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5009)).

begin(model(5010)).
bought(fish).
shops(mary).
end(model(5010)).

begin(model(5011)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5011)).

begin(model(5012)).
bought(fish).
shops(mary).
end(model(5012)).

begin(model(5013)).
bought(fish).
shops(mary).
end(model(5013)).

begin(model(5014)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5014)).

begin(model(5015)).
bought(fish).
shops(mary).
end(model(5015)).

begin(model(5016)).
bought(spaghetti).
shops(mary).
end(model(5016)).

begin(model(5017)).
bought(spaghetti).
shops(mary).
end(model(5017)).

begin(model(5018)).
bought(fish).
shops(mary).
end(model(5018)).

begin(model(5019)).
bought(fish).
shops(mary).
end(model(5019)).

begin(model(5020)).
bought(fish).
shops(mary).
end(model(5020)).

begin(model(5021)).
bought(fish).
shops(mary).
end(model(5021)).

begin(model(5022)).
bought(fish).
shops(mary).
end(model(5022)).

begin(model(5023)).
bought(fish).
shops(mary).
end(model(5023)).

begin(model(5024)).
end(model(5024)).

begin(model(5025)).
end(model(5025)).

begin(model(5026)).
bought(fish).
shops(mary).
end(model(5026)).

begin(model(5027)).
bought(fish).
shops(mary).
end(model(5027)).

begin(model(5028)).
bought(spaghetti).
shops(mary).
end(model(5028)).

begin(model(5029)).
bought(fish).
shops(mary).
end(model(5029)).

begin(model(5030)).
bought(spaghetti).
shops(mary).
end(model(5030)).

begin(model(5031)).
bought(spaghetti).
shops(mary).
end(model(5031)).

begin(model(5032)).
bought(spaghetti).
shops(mary).
end(model(5032)).

begin(model(5033)).
bought(fish).
shops(mary).
end(model(5033)).

begin(model(5034)).
bought(fish).
shops(mary).
end(model(5034)).

begin(model(5035)).
bought(fish).
shops(mary).
end(model(5035)).

begin(model(5036)).
bought(steak).
shops(john).
end(model(5036)).

begin(model(5037)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5037)).

begin(model(5038)).
end(model(5038)).

begin(model(5039)).
end(model(5039)).

begin(model(5040)).
bought(fish).
shops(mary).
end(model(5040)).

begin(model(5041)).
bought(spaghetti).
shops(mary).
end(model(5041)).

begin(model(5042)).
end(model(5042)).

begin(model(5043)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5043)).

begin(model(5044)).
bought(spaghetti).
shops(mary).
end(model(5044)).

begin(model(5045)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5045)).

begin(model(5046)).
bought(spaghetti).
shops(mary).
end(model(5046)).

begin(model(5047)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5047)).

begin(model(5048)).
bought(fish).
shops(mary).
end(model(5048)).

begin(model(5049)).
bought(fish).
shops(mary).
end(model(5049)).

begin(model(5050)).
end(model(5050)).

begin(model(5051)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5051)).

begin(model(5052)).
bought(fish).
shops(mary).
end(model(5052)).

begin(model(5053)).
bought(fish).
shops(mary).
end(model(5053)).

begin(model(5054)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5054)).

begin(model(5055)).
bought(spaghetti).
shops(mary).
end(model(5055)).

begin(model(5056)).
bought(fish).
shops(mary).
end(model(5056)).

begin(model(5057)).
bought(spaghetti).
shops(mary).
end(model(5057)).

begin(model(5058)).
end(model(5058)).

begin(model(5059)).
bought(fish).
shops(mary).
end(model(5059)).

begin(model(5060)).
bought(fish).
shops(mary).
end(model(5060)).

begin(model(5061)).
end(model(5061)).

begin(model(5062)).
end(model(5062)).

begin(model(5063)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5063)).

begin(model(5064)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5064)).

begin(model(5065)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5065)).

begin(model(5066)).
bought(fish).
shops(mary).
end(model(5066)).

begin(model(5067)).
bought(fish).
shops(mary).
end(model(5067)).

begin(model(5068)).
bought(fish).
shops(mary).
end(model(5068)).

begin(model(5069)).
bought(fish).
shops(mary).
end(model(5069)).

begin(model(5070)).
bought(fish).
shops(mary).
end(model(5070)).

begin(model(5071)).
bought(steak).
shops(john).
end(model(5071)).

begin(model(5072)).
end(model(5072)).

begin(model(5073)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5073)).

begin(model(5074)).
bought(fish).
shops(mary).
end(model(5074)).

begin(model(5075)).
bought(spaghetti).
shops(mary).
end(model(5075)).

begin(model(5076)).
bought(spaghetti).
shops(mary).
end(model(5076)).

begin(model(5077)).
end(model(5077)).

begin(model(5078)).
bought(fish).
shops(mary).
end(model(5078)).

begin(model(5079)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5079)).

begin(model(5080)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5080)).

begin(model(5081)).
bought(fish).
shops(mary).
end(model(5081)).

begin(model(5082)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5082)).

begin(model(5083)).
bought(fish).
shops(mary).
end(model(5083)).

begin(model(5084)).
bought(fish).
shops(mary).
end(model(5084)).

begin(model(5085)).
bought(fish).
shops(mary).
end(model(5085)).

begin(model(5086)).
bought(spaghetti).
shops(mary).
end(model(5086)).

begin(model(5087)).
bought(fish).
shops(mary).
end(model(5087)).

begin(model(5088)).
bought(fish).
shops(mary).
end(model(5088)).

begin(model(5089)).
bought(spaghetti).
shops(mary).
end(model(5089)).

begin(model(5090)).
bought(fish).
shops(mary).
end(model(5090)).

begin(model(5091)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5091)).

begin(model(5092)).
bought(fish).
shops(mary).
end(model(5092)).

begin(model(5093)).
bought(spaghetti).
shops(mary).
end(model(5093)).

begin(model(5094)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5094)).

begin(model(5095)).
bought(fish).
shops(mary).
end(model(5095)).

begin(model(5096)).
bought(spaghetti).
shops(mary).
end(model(5096)).

begin(model(5097)).
end(model(5097)).

begin(model(5098)).
bought(spaghetti).
shops(mary).
end(model(5098)).

begin(model(5099)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5099)).

begin(model(5100)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5100)).

begin(model(5101)).
end(model(5101)).

begin(model(5102)).
bought(fish).
shops(mary).
end(model(5102)).

begin(model(5103)).
bought(spaghetti).
shops(mary).
end(model(5103)).

begin(model(5104)).
bought(fish).
shops(mary).
end(model(5104)).

begin(model(5105)).
bought(fish).
shops(mary).
end(model(5105)).

begin(model(5106)).
bought(fish).
shops(mary).
end(model(5106)).

begin(model(5107)).
bought(fish).
shops(mary).
end(model(5107)).

begin(model(5108)).
bought(steak).
shops(john).
end(model(5108)).

begin(model(5109)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5109)).

begin(model(5110)).
bought(fish).
shops(mary).
end(model(5110)).

begin(model(5111)).
bought(fish).
shops(mary).
end(model(5111)).

begin(model(5112)).
bought(fish).
shops(mary).
end(model(5112)).

begin(model(5113)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5113)).

begin(model(5114)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5114)).

begin(model(5115)).
bought(fish).
shops(mary).
end(model(5115)).

begin(model(5116)).
bought(spaghetti).
shops(mary).
end(model(5116)).

begin(model(5117)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5117)).

begin(model(5118)).
bought(spaghetti).
shops(mary).
end(model(5118)).

begin(model(5119)).
bought(fish).
shops(mary).
end(model(5119)).

begin(model(5120)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5120)).

begin(model(5121)).
bought(spaghetti).
shops(mary).
end(model(5121)).

begin(model(5122)).
bought(fish).
shops(mary).
end(model(5122)).

begin(model(5123)).
bought(fish).
shops(mary).
end(model(5123)).

begin(model(5124)).
bought(fish).
shops(mary).
end(model(5124)).

begin(model(5125)).
bought(spaghetti).
shops(mary).
end(model(5125)).

begin(model(5126)).
bought(fish).
shops(mary).
end(model(5126)).

begin(model(5127)).
bought(fish).
shops(mary).
end(model(5127)).

begin(model(5128)).
bought(fish).
shops(mary).
end(model(5128)).

begin(model(5129)).
bought(fish).
shops(mary).
end(model(5129)).

begin(model(5130)).
bought(fish).
shops(mary).
end(model(5130)).

begin(model(5131)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5131)).

begin(model(5132)).
end(model(5132)).

begin(model(5133)).
bought(spaghetti).
shops(mary).
end(model(5133)).

begin(model(5134)).
bought(fish).
shops(mary).
end(model(5134)).

begin(model(5135)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5135)).

begin(model(5136)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5136)).

begin(model(5137)).
bought(spaghetti).
shops(mary).
end(model(5137)).

begin(model(5138)).
bought(fish).
shops(mary).
end(model(5138)).

begin(model(5139)).
bought(spaghetti).
shops(mary).
end(model(5139)).

begin(model(5140)).
bought(spaghetti).
shops(mary).
end(model(5140)).

begin(model(5141)).
bought(spaghetti).
shops(mary).
end(model(5141)).

begin(model(5142)).
bought(fish).
shops(mary).
end(model(5142)).

begin(model(5143)).
bought(fish).
shops(mary).
end(model(5143)).

begin(model(5144)).
bought(fish).
shops(mary).
end(model(5144)).

begin(model(5145)).
bought(spaghetti).
shops(mary).
end(model(5145)).

begin(model(5146)).
bought(fish).
shops(mary).
end(model(5146)).

begin(model(5147)).
bought(spaghetti).
shops(mary).
end(model(5147)).

begin(model(5148)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5148)).

begin(model(5149)).
bought(fish).
shops(mary).
end(model(5149)).

begin(model(5150)).
bought(fish).
shops(mary).
end(model(5150)).

begin(model(5151)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5151)).

begin(model(5152)).
bought(fish).
shops(mary).
end(model(5152)).

begin(model(5153)).
bought(fish).
shops(mary).
end(model(5153)).

begin(model(5154)).
bought(fish).
shops(mary).
end(model(5154)).

begin(model(5155)).
bought(spaghetti).
shops(mary).
end(model(5155)).

begin(model(5156)).
bought(spaghetti).
shops(mary).
end(model(5156)).

begin(model(5157)).
bought(spaghetti).
shops(mary).
end(model(5157)).

begin(model(5158)).
bought(fish).
shops(mary).
end(model(5158)).

begin(model(5159)).
bought(fish).
shops(mary).
end(model(5159)).

begin(model(5160)).
bought(fish).
shops(mary).
end(model(5160)).

begin(model(5161)).
bought(fish).
shops(mary).
end(model(5161)).

begin(model(5162)).
bought(fish).
shops(mary).
end(model(5162)).

begin(model(5163)).
bought(fish).
shops(mary).
end(model(5163)).

begin(model(5164)).
bought(spaghetti).
shops(mary).
end(model(5164)).

begin(model(5165)).
bought(spaghetti).
shops(mary).
end(model(5165)).

begin(model(5166)).
bought(fish).
shops(mary).
end(model(5166)).

begin(model(5167)).
bought(fish).
shops(mary).
end(model(5167)).

begin(model(5168)).
bought(spaghetti).
shops(mary).
end(model(5168)).

begin(model(5169)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5169)).

begin(model(5170)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5170)).

begin(model(5171)).
bought(fish).
shops(mary).
end(model(5171)).

begin(model(5172)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5172)).

begin(model(5173)).
bought(fish).
shops(mary).
end(model(5173)).

begin(model(5174)).
bought(fish).
shops(mary).
end(model(5174)).

begin(model(5175)).
bought(fish).
shops(mary).
end(model(5175)).

begin(model(5176)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5176)).

begin(model(5177)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5177)).

begin(model(5178)).
bought(spaghetti).
shops(mary).
end(model(5178)).

begin(model(5179)).
bought(fish).
shops(mary).
end(model(5179)).

begin(model(5180)).
bought(fish).
shops(mary).
end(model(5180)).

begin(model(5181)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5181)).

begin(model(5182)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5182)).

begin(model(5183)).
bought(fish).
shops(mary).
end(model(5183)).

begin(model(5184)).
bought(fish).
shops(mary).
end(model(5184)).

begin(model(5185)).
end(model(5185)).

begin(model(5186)).
bought(fish).
shops(mary).
end(model(5186)).

begin(model(5187)).
end(model(5187)).

begin(model(5188)).
bought(spaghetti).
shops(mary).
end(model(5188)).

begin(model(5189)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5189)).

begin(model(5190)).
bought(fish).
shops(mary).
end(model(5190)).

begin(model(5191)).
bought(fish).
shops(mary).
end(model(5191)).

begin(model(5192)).
bought(spaghetti).
shops(mary).
end(model(5192)).

begin(model(5193)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5193)).

begin(model(5194)).
bought(fish).
shops(mary).
end(model(5194)).

begin(model(5195)).
bought(spaghetti).
shops(mary).
end(model(5195)).

begin(model(5196)).
bought(fish).
shops(mary).
end(model(5196)).

begin(model(5197)).
bought(spaghetti).
shops(mary).
end(model(5197)).

begin(model(5198)).
bought(spaghetti).
shops(mary).
end(model(5198)).

begin(model(5199)).
bought(fish).
shops(mary).
end(model(5199)).

begin(model(5200)).
end(model(5200)).

begin(model(5201)).
bought(spaghetti).
shops(mary).
end(model(5201)).

begin(model(5202)).
bought(fish).
shops(mary).
end(model(5202)).

begin(model(5203)).
bought(fish).
shops(mary).
end(model(5203)).

begin(model(5204)).
end(model(5204)).

begin(model(5205)).
bought(fish).
shops(mary).
end(model(5205)).

begin(model(5206)).
end(model(5206)).

begin(model(5207)).
bought(spaghetti).
shops(mary).
end(model(5207)).

begin(model(5208)).
bought(spaghetti).
shops(mary).
end(model(5208)).

begin(model(5209)).
bought(fish).
shops(mary).
end(model(5209)).

begin(model(5210)).
end(model(5210)).

begin(model(5211)).
bought(fish).
shops(mary).
end(model(5211)).

begin(model(5212)).
bought(fish).
shops(mary).
end(model(5212)).

begin(model(5213)).
bought(fish).
shops(mary).
end(model(5213)).

begin(model(5214)).
bought(spaghetti).
shops(mary).
end(model(5214)).

begin(model(5215)).
bought(fish).
shops(mary).
end(model(5215)).

begin(model(5216)).
bought(spaghetti).
shops(mary).
end(model(5216)).

begin(model(5217)).
bought(fish).
shops(mary).
end(model(5217)).

begin(model(5218)).
bought(fish).
shops(mary).
end(model(5218)).

begin(model(5219)).
bought(spaghetti).
shops(mary).
end(model(5219)).

begin(model(5220)).
bought(fish).
shops(mary).
end(model(5220)).

begin(model(5221)).
bought(fish).
shops(mary).
end(model(5221)).

begin(model(5222)).
bought(fish).
shops(mary).
end(model(5222)).

begin(model(5223)).
bought(fish).
shops(mary).
end(model(5223)).

begin(model(5224)).
bought(spaghetti).
shops(mary).
end(model(5224)).

begin(model(5225)).
bought(fish).
shops(mary).
end(model(5225)).

begin(model(5226)).
bought(fish).
shops(mary).
end(model(5226)).

begin(model(5227)).
bought(fish).
shops(mary).
end(model(5227)).

begin(model(5228)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5228)).

begin(model(5229)).
bought(fish).
shops(mary).
end(model(5229)).

begin(model(5230)).
bought(spaghetti).
shops(mary).
end(model(5230)).

begin(model(5231)).
bought(spaghetti).
shops(mary).
end(model(5231)).

begin(model(5232)).
bought(fish).
shops(mary).
end(model(5232)).

begin(model(5233)).
bought(fish).
shops(mary).
end(model(5233)).

begin(model(5234)).
bought(fish).
shops(mary).
end(model(5234)).

begin(model(5235)).
bought(spaghetti).
shops(mary).
end(model(5235)).

begin(model(5236)).
bought(fish).
shops(mary).
end(model(5236)).

begin(model(5237)).
bought(spaghetti).
shops(mary).
end(model(5237)).

begin(model(5238)).
bought(fish).
shops(mary).
end(model(5238)).

begin(model(5239)).
bought(fish).
shops(mary).
end(model(5239)).

begin(model(5240)).
bought(fish).
shops(mary).
end(model(5240)).

begin(model(5241)).
end(model(5241)).

begin(model(5242)).
bought(spaghetti).
shops(mary).
end(model(5242)).

begin(model(5243)).
bought(fish).
shops(mary).
end(model(5243)).

begin(model(5244)).
bought(fish).
shops(mary).
end(model(5244)).

begin(model(5245)).
bought(fish).
shops(mary).
end(model(5245)).

begin(model(5246)).
bought(fish).
shops(mary).
end(model(5246)).

begin(model(5247)).
end(model(5247)).

begin(model(5248)).
end(model(5248)).

begin(model(5249)).
bought(spaghetti).
shops(mary).
end(model(5249)).

begin(model(5250)).
bought(fish).
shops(mary).
end(model(5250)).

begin(model(5251)).
end(model(5251)).

begin(model(5252)).
bought(fish).
shops(mary).
end(model(5252)).

begin(model(5253)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5253)).

begin(model(5254)).
bought(fish).
shops(mary).
end(model(5254)).

begin(model(5255)).
bought(spaghetti).
shops(mary).
end(model(5255)).

begin(model(5256)).
bought(fish).
shops(mary).
end(model(5256)).

begin(model(5257)).
bought(fish).
shops(mary).
end(model(5257)).

begin(model(5258)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5258)).

begin(model(5259)).
bought(spaghetti).
shops(mary).
end(model(5259)).

begin(model(5260)).
bought(spaghetti).
shops(mary).
end(model(5260)).

begin(model(5261)).
end(model(5261)).

begin(model(5262)).
end(model(5262)).

begin(model(5263)).
bought(fish).
shops(mary).
end(model(5263)).

begin(model(5264)).
bought(fish).
shops(mary).
end(model(5264)).

begin(model(5265)).
bought(spaghetti).
shops(mary).
end(model(5265)).

begin(model(5266)).
bought(fish).
shops(mary).
end(model(5266)).

begin(model(5267)).
bought(spaghetti).
shops(mary).
end(model(5267)).

begin(model(5268)).
bought(spaghetti).
shops(mary).
end(model(5268)).

begin(model(5269)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5269)).

begin(model(5270)).
bought(fish).
shops(mary).
end(model(5270)).

begin(model(5271)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5271)).

begin(model(5272)).
bought(fish).
shops(mary).
end(model(5272)).

begin(model(5273)).
bought(spaghetti).
shops(mary).
end(model(5273)).

begin(model(5274)).
bought(fish).
shops(mary).
end(model(5274)).

begin(model(5275)).
bought(fish).
shops(mary).
end(model(5275)).

begin(model(5276)).
bought(fish).
shops(mary).
end(model(5276)).

begin(model(5277)).
bought(fish).
shops(mary).
end(model(5277)).

begin(model(5278)).
bought(fish).
shops(mary).
end(model(5278)).

begin(model(5279)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5279)).

begin(model(5280)).
bought(fish).
shops(mary).
end(model(5280)).

begin(model(5281)).
bought(spaghetti).
shops(john).
end(model(5281)).

begin(model(5282)).
bought(fish).
shops(mary).
end(model(5282)).

begin(model(5283)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5283)).

begin(model(5284)).
bought(fish).
shops(mary).
end(model(5284)).

begin(model(5285)).
bought(spaghetti).
shops(mary).
end(model(5285)).

begin(model(5286)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5286)).

begin(model(5287)).
bought(spaghetti).
shops(mary).
end(model(5287)).

begin(model(5288)).
end(model(5288)).

begin(model(5289)).
bought(fish).
shops(mary).
end(model(5289)).

begin(model(5290)).
bought(spaghetti).
shops(mary).
end(model(5290)).

begin(model(5291)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5291)).

begin(model(5292)).
end(model(5292)).

begin(model(5293)).
bought(fish).
shops(mary).
end(model(5293)).

begin(model(5294)).
bought(fish).
shops(mary).
end(model(5294)).

begin(model(5295)).
end(model(5295)).

begin(model(5296)).
bought(fish).
shops(mary).
end(model(5296)).

begin(model(5297)).
bought(fish).
shops(mary).
end(model(5297)).

begin(model(5298)).
bought(fish).
shops(mary).
end(model(5298)).

begin(model(5299)).
bought(fish).
shops(mary).
end(model(5299)).

begin(model(5300)).
end(model(5300)).

begin(model(5301)).
bought(fish).
shops(mary).
end(model(5301)).

begin(model(5302)).
bought(spaghetti).
shops(mary).
end(model(5302)).

begin(model(5303)).
bought(fish).
shops(mary).
end(model(5303)).

begin(model(5304)).
bought(spaghetti).
shops(mary).
end(model(5304)).

begin(model(5305)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5305)).

begin(model(5306)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5306)).

begin(model(5307)).
bought(fish).
shops(mary).
end(model(5307)).

begin(model(5308)).
bought(fish).
shops(mary).
end(model(5308)).

begin(model(5309)).
bought(fish).
shops(mary).
end(model(5309)).

begin(model(5310)).
end(model(5310)).

begin(model(5311)).
bought(fish).
shops(mary).
end(model(5311)).

begin(model(5312)).
bought(fish).
shops(mary).
end(model(5312)).

begin(model(5313)).
bought(fish).
shops(mary).
end(model(5313)).

begin(model(5314)).
bought(spaghetti).
shops(mary).
end(model(5314)).

begin(model(5315)).
bought(spaghetti).
shops(mary).
end(model(5315)).

begin(model(5316)).
bought(spaghetti).
shops(mary).
end(model(5316)).

begin(model(5317)).
bought(fish).
shops(mary).
end(model(5317)).

begin(model(5318)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5318)).

begin(model(5319)).
bought(fish).
shops(mary).
end(model(5319)).

begin(model(5320)).
bought(spaghetti).
shops(mary).
end(model(5320)).

begin(model(5321)).
bought(fish).
shops(mary).
end(model(5321)).

begin(model(5322)).
bought(spaghetti).
shops(mary).
end(model(5322)).

begin(model(5323)).
bought(fish).
shops(mary).
end(model(5323)).

begin(model(5324)).
bought(spaghetti).
shops(mary).
end(model(5324)).

begin(model(5325)).
bought(spaghetti).
shops(mary).
end(model(5325)).

begin(model(5326)).
end(model(5326)).

begin(model(5327)).
bought(fish).
shops(mary).
end(model(5327)).

begin(model(5328)).
bought(spaghetti).
shops(mary).
end(model(5328)).

begin(model(5329)).
bought(spaghetti).
shops(mary).
end(model(5329)).

begin(model(5330)).
bought(fish).
shops(mary).
end(model(5330)).

begin(model(5331)).
bought(fish).
shops(mary).
end(model(5331)).

begin(model(5332)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5332)).

begin(model(5333)).
bought(fish).
shops(mary).
end(model(5333)).

begin(model(5334)).
bought(spaghetti).
shops(mary).
end(model(5334)).

begin(model(5335)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5335)).

begin(model(5336)).
bought(spaghetti).
shops(mary).
end(model(5336)).

begin(model(5337)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5337)).

begin(model(5338)).
end(model(5338)).

begin(model(5339)).
bought(fish).
shops(mary).
end(model(5339)).

begin(model(5340)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5340)).

begin(model(5341)).
bought(fish).
shops(mary).
end(model(5341)).

begin(model(5342)).
bought(spaghetti).
shops(mary).
end(model(5342)).

begin(model(5343)).
bought(fish).
shops(mary).
end(model(5343)).

begin(model(5344)).
end(model(5344)).

begin(model(5345)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5345)).

begin(model(5346)).
end(model(5346)).

begin(model(5347)).
bought(spaghetti).
shops(mary).
end(model(5347)).

begin(model(5348)).
bought(spaghetti).
shops(mary).
end(model(5348)).

begin(model(5349)).
bought(fish).
shops(mary).
end(model(5349)).

begin(model(5350)).
bought(spaghetti).
shops(mary).
end(model(5350)).

begin(model(5351)).
bought(spaghetti).
shops(john).
end(model(5351)).

begin(model(5352)).
bought(fish).
shops(mary).
end(model(5352)).

begin(model(5353)).
bought(spaghetti).
shops(mary).
end(model(5353)).

begin(model(5354)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5354)).

begin(model(5355)).
end(model(5355)).

begin(model(5356)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5356)).

begin(model(5357)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5357)).

begin(model(5358)).
bought(fish).
shops(mary).
end(model(5358)).

begin(model(5359)).
bought(spaghetti).
shops(mary).
end(model(5359)).

begin(model(5360)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5360)).

begin(model(5361)).
bought(spaghetti).
shops(mary).
end(model(5361)).

begin(model(5362)).
bought(fish).
shops(mary).
end(model(5362)).

begin(model(5363)).
bought(fish).
shops(mary).
end(model(5363)).

begin(model(5364)).
bought(spaghetti).
shops(mary).
end(model(5364)).

begin(model(5365)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5365)).

begin(model(5366)).
bought(fish).
shops(mary).
end(model(5366)).

begin(model(5367)).
bought(spaghetti).
shops(mary).
end(model(5367)).

begin(model(5368)).
bought(fish).
shops(mary).
end(model(5368)).

begin(model(5369)).
end(model(5369)).

begin(model(5370)).
bought(fish).
shops(mary).
end(model(5370)).

begin(model(5371)).
bought(spaghetti).
shops(mary).
end(model(5371)).

begin(model(5372)).
bought(fish).
shops(mary).
end(model(5372)).

begin(model(5373)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5373)).

begin(model(5374)).
bought(fish).
shops(mary).
end(model(5374)).

begin(model(5375)).
bought(spaghetti).
shops(mary).
end(model(5375)).

begin(model(5376)).
bought(fish).
shops(mary).
end(model(5376)).

begin(model(5377)).
bought(fish).
shops(mary).
end(model(5377)).

begin(model(5378)).
bought(spaghetti).
shops(mary).
end(model(5378)).

begin(model(5379)).
bought(fish).
shops(mary).
end(model(5379)).

begin(model(5380)).
bought(fish).
shops(mary).
end(model(5380)).

begin(model(5381)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5381)).

begin(model(5382)).
bought(spaghetti).
shops(mary).
end(model(5382)).

begin(model(5383)).
bought(spaghetti).
shops(mary).
end(model(5383)).

begin(model(5384)).
bought(spaghetti).
shops(mary).
end(model(5384)).

begin(model(5385)).
bought(fish).
shops(mary).
end(model(5385)).

begin(model(5386)).
bought(fish).
shops(mary).
end(model(5386)).

begin(model(5387)).
end(model(5387)).

begin(model(5388)).
bought(fish).
shops(mary).
end(model(5388)).

begin(model(5389)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5389)).

begin(model(5390)).
bought(fish).
shops(mary).
end(model(5390)).

begin(model(5391)).
bought(fish).
shops(mary).
end(model(5391)).

begin(model(5392)).
bought(fish).
shops(mary).
end(model(5392)).

begin(model(5393)).
bought(fish).
shops(mary).
end(model(5393)).

begin(model(5394)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5394)).

begin(model(5395)).
bought(fish).
shops(mary).
end(model(5395)).

begin(model(5396)).
bought(fish).
shops(mary).
end(model(5396)).

begin(model(5397)).
bought(fish).
shops(mary).
end(model(5397)).

begin(model(5398)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5398)).

begin(model(5399)).
bought(fish).
shops(mary).
end(model(5399)).

begin(model(5400)).
bought(fish).
shops(mary).
end(model(5400)).

begin(model(5401)).
bought(steak).
shops(john).
end(model(5401)).

begin(model(5402)).
bought(spaghetti).
shops(mary).
end(model(5402)).

begin(model(5403)).
bought(fish).
shops(mary).
end(model(5403)).

begin(model(5404)).
bought(spaghetti).
shops(mary).
end(model(5404)).

begin(model(5405)).
bought(fish).
shops(mary).
end(model(5405)).

begin(model(5406)).
bought(fish).
shops(mary).
end(model(5406)).

begin(model(5407)).
bought(spaghetti).
shops(mary).
end(model(5407)).

begin(model(5408)).
bought(fish).
shops(mary).
end(model(5408)).

begin(model(5409)).
bought(spaghetti).
shops(mary).
end(model(5409)).

begin(model(5410)).
bought(fish).
shops(mary).
end(model(5410)).

begin(model(5411)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5411)).

begin(model(5412)).
bought(fish).
shops(mary).
end(model(5412)).

begin(model(5413)).
bought(fish).
shops(mary).
end(model(5413)).

begin(model(5414)).
bought(fish).
shops(mary).
end(model(5414)).

begin(model(5415)).
bought(fish).
shops(mary).
end(model(5415)).

begin(model(5416)).
bought(spaghetti).
shops(mary).
end(model(5416)).

begin(model(5417)).
bought(spaghetti).
shops(mary).
end(model(5417)).

begin(model(5418)).
bought(fish).
shops(mary).
end(model(5418)).

begin(model(5419)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5419)).

begin(model(5420)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5420)).

begin(model(5421)).
bought(fish).
shops(mary).
end(model(5421)).

begin(model(5422)).
end(model(5422)).

begin(model(5423)).
bought(spaghetti).
shops(mary).
end(model(5423)).

begin(model(5424)).
bought(fish).
shops(mary).
end(model(5424)).

begin(model(5425)).
bought(spaghetti).
shops(mary).
end(model(5425)).

begin(model(5426)).
bought(spaghetti).
shops(mary).
end(model(5426)).

begin(model(5427)).
bought(spaghetti).
shops(mary).
end(model(5427)).

begin(model(5428)).
bought(spaghetti).
shops(mary).
end(model(5428)).

begin(model(5429)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5429)).

begin(model(5430)).
bought(fish).
shops(mary).
end(model(5430)).

begin(model(5431)).
bought(fish).
shops(mary).
end(model(5431)).

begin(model(5432)).
bought(fish).
shops(mary).
end(model(5432)).

begin(model(5433)).
bought(spaghetti).
shops(john).
end(model(5433)).

begin(model(5434)).
bought(fish).
shops(mary).
end(model(5434)).

begin(model(5435)).
bought(fish).
shops(mary).
end(model(5435)).

begin(model(5436)).
bought(fish).
shops(mary).
end(model(5436)).

begin(model(5437)).
bought(spaghetti).
shops(mary).
end(model(5437)).

begin(model(5438)).
bought(fish).
shops(mary).
end(model(5438)).

begin(model(5439)).
bought(fish).
shops(mary).
end(model(5439)).

begin(model(5440)).
bought(fish).
shops(mary).
end(model(5440)).

begin(model(5441)).
bought(fish).
shops(mary).
end(model(5441)).

begin(model(5442)).
end(model(5442)).

begin(model(5443)).
bought(fish).
shops(mary).
end(model(5443)).

begin(model(5444)).
bought(spaghetti).
shops(mary).
end(model(5444)).

begin(model(5445)).
bought(fish).
shops(mary).
end(model(5445)).

begin(model(5446)).
bought(fish).
shops(mary).
end(model(5446)).

begin(model(5447)).
end(model(5447)).

begin(model(5448)).
bought(fish).
shops(mary).
end(model(5448)).

begin(model(5449)).
bought(fish).
shops(mary).
end(model(5449)).

begin(model(5450)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5450)).

begin(model(5451)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5451)).

begin(model(5452)).
end(model(5452)).

begin(model(5453)).
bought(fish).
shops(mary).
end(model(5453)).

begin(model(5454)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5454)).

begin(model(5455)).
bought(fish).
shops(mary).
end(model(5455)).

begin(model(5456)).
bought(fish).
shops(mary).
end(model(5456)).

begin(model(5457)).
bought(spaghetti).
shops(john).
end(model(5457)).

begin(model(5458)).
bought(fish).
shops(mary).
end(model(5458)).

begin(model(5459)).
bought(spaghetti).
shops(mary).
end(model(5459)).

begin(model(5460)).
bought(fish).
shops(mary).
end(model(5460)).

begin(model(5461)).
end(model(5461)).

begin(model(5462)).
bought(spaghetti).
shops(mary).
end(model(5462)).

begin(model(5463)).
bought(fish).
shops(mary).
end(model(5463)).

begin(model(5464)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5464)).

begin(model(5465)).
bought(fish).
shops(mary).
end(model(5465)).

begin(model(5466)).
bought(spaghetti).
shops(mary).
end(model(5466)).

begin(model(5467)).
bought(spaghetti).
shops(mary).
end(model(5467)).

begin(model(5468)).
bought(spaghetti).
shops(mary).
end(model(5468)).

begin(model(5469)).
bought(fish).
shops(mary).
end(model(5469)).

begin(model(5470)).
bought(spaghetti).
shops(mary).
end(model(5470)).

begin(model(5471)).
bought(spaghetti).
shops(mary).
end(model(5471)).

begin(model(5472)).
bought(spaghetti).
shops(john).
end(model(5472)).

begin(model(5473)).
bought(fish).
shops(mary).
end(model(5473)).

begin(model(5474)).
bought(fish).
shops(mary).
end(model(5474)).

begin(model(5475)).
bought(fish).
shops(mary).
end(model(5475)).

begin(model(5476)).
bought(spaghetti).
shops(mary).
end(model(5476)).

begin(model(5477)).
bought(fish).
shops(mary).
end(model(5477)).

begin(model(5478)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5478)).

begin(model(5479)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5479)).

begin(model(5480)).
bought(fish).
shops(mary).
end(model(5480)).

begin(model(5481)).
bought(spaghetti).
shops(john).
end(model(5481)).

begin(model(5482)).
bought(fish).
shops(mary).
end(model(5482)).

begin(model(5483)).
bought(fish).
shops(mary).
end(model(5483)).

begin(model(5484)).
bought(spaghetti).
shops(mary).
end(model(5484)).

begin(model(5485)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5485)).

begin(model(5486)).
bought(spaghetti).
shops(mary).
end(model(5486)).

begin(model(5487)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5487)).

begin(model(5488)).
bought(fish).
shops(mary).
end(model(5488)).

begin(model(5489)).
bought(fish).
shops(mary).
end(model(5489)).

begin(model(5490)).
bought(fish).
shops(mary).
end(model(5490)).

begin(model(5491)).
bought(fish).
shops(mary).
end(model(5491)).

begin(model(5492)).
bought(fish).
shops(mary).
end(model(5492)).

begin(model(5493)).
end(model(5493)).

begin(model(5494)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5494)).

begin(model(5495)).
bought(spaghetti).
shops(mary).
end(model(5495)).

begin(model(5496)).
bought(fish).
shops(mary).
end(model(5496)).

begin(model(5497)).
bought(spaghetti).
shops(mary).
end(model(5497)).

begin(model(5498)).
bought(fish).
shops(mary).
end(model(5498)).

begin(model(5499)).
bought(spaghetti).
shops(mary).
end(model(5499)).

begin(model(5500)).
bought(fish).
shops(mary).
end(model(5500)).

begin(model(5501)).
bought(fish).
shops(mary).
end(model(5501)).

begin(model(5502)).
end(model(5502)).

begin(model(5503)).
bought(fish).
shops(mary).
end(model(5503)).

begin(model(5504)).
bought(spaghetti).
shops(mary).
end(model(5504)).

begin(model(5505)).
bought(fish).
shops(mary).
end(model(5505)).

begin(model(5506)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5506)).

begin(model(5507)).
end(model(5507)).

begin(model(5508)).
bought(fish).
shops(mary).
end(model(5508)).

begin(model(5509)).
bought(fish).
shops(mary).
end(model(5509)).

begin(model(5510)).
bought(fish).
shops(mary).
end(model(5510)).

begin(model(5511)).
bought(fish).
shops(mary).
end(model(5511)).

begin(model(5512)).
bought(spaghetti).
shops(mary).
end(model(5512)).

begin(model(5513)).
bought(spaghetti).
shops(mary).
end(model(5513)).

begin(model(5514)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5514)).

begin(model(5515)).
bought(fish).
shops(mary).
end(model(5515)).

begin(model(5516)).
bought(spaghetti).
shops(mary).
end(model(5516)).

begin(model(5517)).
bought(fish).
shops(mary).
end(model(5517)).

begin(model(5518)).
bought(fish).
shops(mary).
end(model(5518)).

begin(model(5519)).
bought(steak).
shops(john).
end(model(5519)).

begin(model(5520)).
end(model(5520)).

begin(model(5521)).
bought(fish).
shops(mary).
end(model(5521)).

begin(model(5522)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5522)).

begin(model(5523)).
bought(fish).
shops(mary).
end(model(5523)).

begin(model(5524)).
bought(fish).
shops(mary).
end(model(5524)).

begin(model(5525)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5525)).

begin(model(5526)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5526)).

begin(model(5527)).
end(model(5527)).

begin(model(5528)).
bought(fish).
shops(mary).
end(model(5528)).

begin(model(5529)).
bought(spaghetti).
shops(mary).
end(model(5529)).

begin(model(5530)).
bought(fish).
shops(mary).
end(model(5530)).

begin(model(5531)).
bought(spaghetti).
shops(mary).
end(model(5531)).

begin(model(5532)).
bought(spaghetti).
shops(mary).
end(model(5532)).

begin(model(5533)).
bought(fish).
shops(mary).
end(model(5533)).

begin(model(5534)).
bought(fish).
shops(mary).
end(model(5534)).

begin(model(5535)).
bought(fish).
shops(mary).
end(model(5535)).

begin(model(5536)).
end(model(5536)).

begin(model(5537)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5537)).

begin(model(5538)).
bought(fish).
shops(mary).
end(model(5538)).

begin(model(5539)).
bought(steak).
shops(john).
end(model(5539)).

begin(model(5540)).
bought(spaghetti).
shops(mary).
end(model(5540)).

begin(model(5541)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5541)).

begin(model(5542)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5542)).

begin(model(5543)).
end(model(5543)).

begin(model(5544)).
bought(fish).
shops(mary).
end(model(5544)).

begin(model(5545)).
bought(fish).
shops(mary).
end(model(5545)).

begin(model(5546)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5546)).

begin(model(5547)).
end(model(5547)).

begin(model(5548)).
bought(fish).
shops(mary).
end(model(5548)).

begin(model(5549)).
bought(spaghetti).
shops(mary).
end(model(5549)).

begin(model(5550)).
bought(fish).
shops(mary).
end(model(5550)).

begin(model(5551)).
bought(fish).
shops(mary).
end(model(5551)).

begin(model(5552)).
bought(fish).
shops(mary).
end(model(5552)).

begin(model(5553)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5553)).

begin(model(5554)).
bought(spaghetti).
shops(mary).
end(model(5554)).

begin(model(5555)).
bought(fish).
shops(mary).
end(model(5555)).

begin(model(5556)).
bought(spaghetti).
shops(mary).
end(model(5556)).

begin(model(5557)).
bought(fish).
shops(mary).
end(model(5557)).

begin(model(5558)).
bought(fish).
shops(mary).
end(model(5558)).

begin(model(5559)).
bought(fish).
shops(mary).
end(model(5559)).

begin(model(5560)).
bought(fish).
shops(mary).
end(model(5560)).

begin(model(5561)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5561)).

begin(model(5562)).
end(model(5562)).

begin(model(5563)).
end(model(5563)).

begin(model(5564)).
bought(fish).
shops(mary).
end(model(5564)).

begin(model(5565)).
bought(fish).
shops(mary).
end(model(5565)).

begin(model(5566)).
bought(spaghetti).
shops(mary).
end(model(5566)).

begin(model(5567)).
bought(fish).
shops(mary).
end(model(5567)).

begin(model(5568)).
bought(steak).
shops(john).
end(model(5568)).

begin(model(5569)).
bought(fish).
shops(mary).
end(model(5569)).

begin(model(5570)).
bought(fish).
shops(mary).
end(model(5570)).

begin(model(5571)).
end(model(5571)).

begin(model(5572)).
bought(fish).
shops(mary).
end(model(5572)).

begin(model(5573)).
bought(steak).
shops(john).
end(model(5573)).

begin(model(5574)).
bought(fish).
shops(mary).
end(model(5574)).

begin(model(5575)).
bought(spaghetti).
shops(mary).
end(model(5575)).

begin(model(5576)).
bought(spaghetti).
shops(mary).
end(model(5576)).

begin(model(5577)).
bought(fish).
shops(mary).
end(model(5577)).

begin(model(5578)).
bought(fish).
shops(mary).
end(model(5578)).

begin(model(5579)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5579)).

begin(model(5580)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5580)).

begin(model(5581)).
bought(fish).
shops(mary).
end(model(5581)).

begin(model(5582)).
bought(fish).
shops(mary).
end(model(5582)).

begin(model(5583)).
bought(fish).
shops(mary).
end(model(5583)).

begin(model(5584)).
bought(fish).
shops(mary).
end(model(5584)).

begin(model(5585)).
bought(fish).
shops(mary).
end(model(5585)).

begin(model(5586)).
bought(fish).
shops(mary).
end(model(5586)).

begin(model(5587)).
bought(fish).
shops(mary).
end(model(5587)).

begin(model(5588)).
bought(fish).
shops(mary).
end(model(5588)).

begin(model(5589)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5589)).

begin(model(5590)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5590)).

begin(model(5591)).
bought(fish).
shops(mary).
end(model(5591)).

begin(model(5592)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5592)).

begin(model(5593)).
bought(fish).
shops(mary).
end(model(5593)).

begin(model(5594)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5594)).

begin(model(5595)).
bought(fish).
shops(mary).
end(model(5595)).

begin(model(5596)).
bought(fish).
shops(mary).
end(model(5596)).

begin(model(5597)).
bought(spaghetti).
shops(mary).
end(model(5597)).

begin(model(5598)).
bought(spaghetti).
shops(mary).
end(model(5598)).

begin(model(5599)).
bought(fish).
shops(mary).
end(model(5599)).

begin(model(5600)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5600)).

begin(model(5601)).
bought(fish).
shops(mary).
end(model(5601)).

begin(model(5602)).
bought(fish).
shops(mary).
end(model(5602)).

begin(model(5603)).
bought(fish).
shops(mary).
end(model(5603)).

begin(model(5604)).
bought(fish).
shops(mary).
end(model(5604)).

begin(model(5605)).
bought(fish).
shops(mary).
end(model(5605)).

begin(model(5606)).
bought(fish).
shops(mary).
end(model(5606)).

begin(model(5607)).
bought(spaghetti).
shops(mary).
end(model(5607)).

begin(model(5608)).
bought(fish).
shops(mary).
end(model(5608)).

begin(model(5609)).
bought(fish).
shops(mary).
end(model(5609)).

begin(model(5610)).
bought(spaghetti).
shops(mary).
end(model(5610)).

begin(model(5611)).
bought(fish).
shops(mary).
end(model(5611)).

begin(model(5612)).
bought(fish).
shops(mary).
end(model(5612)).

begin(model(5613)).
bought(spaghetti).
shops(mary).
end(model(5613)).

begin(model(5614)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5614)).

begin(model(5615)).
bought(fish).
shops(mary).
end(model(5615)).

begin(model(5616)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5616)).

begin(model(5617)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5617)).

begin(model(5618)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5618)).

begin(model(5619)).
bought(spaghetti).
shops(mary).
end(model(5619)).

begin(model(5620)).
bought(spaghetti).
shops(mary).
end(model(5620)).

begin(model(5621)).
bought(spaghetti).
shops(mary).
end(model(5621)).

begin(model(5622)).
bought(fish).
shops(mary).
end(model(5622)).

begin(model(5623)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5623)).

begin(model(5624)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5624)).

begin(model(5625)).
bought(fish).
shops(mary).
end(model(5625)).

begin(model(5626)).
bought(spaghetti).
shops(john).
end(model(5626)).

begin(model(5627)).
bought(fish).
shops(mary).
end(model(5627)).

begin(model(5628)).
end(model(5628)).

begin(model(5629)).
end(model(5629)).

begin(model(5630)).
bought(fish).
shops(mary).
end(model(5630)).

begin(model(5631)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5631)).

begin(model(5632)).
bought(fish).
shops(mary).
end(model(5632)).

begin(model(5633)).
bought(spaghetti).
shops(mary).
end(model(5633)).

begin(model(5634)).
bought(spaghetti).
shops(mary).
end(model(5634)).

begin(model(5635)).
bought(fish).
shops(mary).
end(model(5635)).

begin(model(5636)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5636)).

begin(model(5637)).
bought(fish).
shops(mary).
end(model(5637)).

begin(model(5638)).
bought(fish).
shops(mary).
end(model(5638)).

begin(model(5639)).
bought(fish).
shops(mary).
end(model(5639)).

begin(model(5640)).
bought(spaghetti).
shops(mary).
end(model(5640)).

begin(model(5641)).
bought(fish).
shops(mary).
end(model(5641)).

begin(model(5642)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5642)).

begin(model(5643)).
bought(spaghetti).
shops(mary).
end(model(5643)).

begin(model(5644)).
bought(spaghetti).
shops(mary).
end(model(5644)).

begin(model(5645)).
bought(fish).
shops(mary).
end(model(5645)).

begin(model(5646)).
bought(fish).
shops(mary).
end(model(5646)).

begin(model(5647)).
bought(fish).
shops(mary).
end(model(5647)).

begin(model(5648)).
bought(fish).
shops(mary).
end(model(5648)).

begin(model(5649)).
bought(fish).
shops(mary).
end(model(5649)).

begin(model(5650)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5650)).

begin(model(5651)).
bought(fish).
shops(mary).
end(model(5651)).

begin(model(5652)).
bought(spaghetti).
shops(john).
end(model(5652)).

begin(model(5653)).
bought(fish).
shops(mary).
end(model(5653)).

begin(model(5654)).
bought(fish).
shops(mary).
end(model(5654)).

begin(model(5655)).
bought(spaghetti).
shops(mary).
end(model(5655)).

begin(model(5656)).
bought(spaghetti).
shops(mary).
end(model(5656)).

begin(model(5657)).
bought(fish).
shops(mary).
end(model(5657)).

begin(model(5658)).
bought(fish).
shops(mary).
end(model(5658)).

begin(model(5659)).
bought(spaghetti).
shops(mary).
end(model(5659)).

begin(model(5660)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5660)).

begin(model(5661)).
bought(fish).
shops(mary).
end(model(5661)).

begin(model(5662)).
bought(fish).
shops(mary).
end(model(5662)).

begin(model(5663)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5663)).

begin(model(5664)).
bought(fish).
shops(mary).
end(model(5664)).

begin(model(5665)).
bought(fish).
shops(mary).
end(model(5665)).

begin(model(5666)).
end(model(5666)).

begin(model(5667)).
end(model(5667)).

begin(model(5668)).
bought(spaghetti).
shops(mary).
end(model(5668)).

begin(model(5669)).
end(model(5669)).

begin(model(5670)).
bought(fish).
shops(mary).
end(model(5670)).

begin(model(5671)).
bought(fish).
shops(mary).
end(model(5671)).

begin(model(5672)).
bought(fish).
shops(mary).
end(model(5672)).

begin(model(5673)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5673)).

begin(model(5674)).
bought(fish).
shops(mary).
end(model(5674)).

begin(model(5675)).
bought(spaghetti).
shops(mary).
end(model(5675)).

begin(model(5676)).
bought(spaghetti).
shops(mary).
end(model(5676)).

begin(model(5677)).
bought(spaghetti).
shops(mary).
end(model(5677)).

begin(model(5678)).
bought(fish).
shops(mary).
end(model(5678)).

begin(model(5679)).
bought(fish).
shops(mary).
end(model(5679)).

begin(model(5680)).
bought(fish).
shops(mary).
end(model(5680)).

begin(model(5681)).
end(model(5681)).

begin(model(5682)).
bought(fish).
shops(mary).
end(model(5682)).

begin(model(5683)).
bought(fish).
shops(mary).
end(model(5683)).

begin(model(5684)).
bought(spaghetti).
shops(mary).
end(model(5684)).

begin(model(5685)).
bought(fish).
shops(mary).
end(model(5685)).

begin(model(5686)).
bought(fish).
shops(mary).
end(model(5686)).

begin(model(5687)).
bought(fish).
shops(mary).
end(model(5687)).

begin(model(5688)).
bought(spaghetti).
shops(mary).
end(model(5688)).

begin(model(5689)).
bought(fish).
shops(mary).
end(model(5689)).

begin(model(5690)).
bought(fish).
shops(mary).
end(model(5690)).

begin(model(5691)).
end(model(5691)).

begin(model(5692)).
bought(fish).
shops(mary).
end(model(5692)).

begin(model(5693)).
bought(spaghetti).
shops(mary).
end(model(5693)).

begin(model(5694)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5694)).

begin(model(5695)).
bought(fish).
shops(mary).
end(model(5695)).

begin(model(5696)).
bought(fish).
shops(mary).
end(model(5696)).

begin(model(5697)).
bought(fish).
shops(mary).
end(model(5697)).

begin(model(5698)).
bought(spaghetti).
shops(mary).
end(model(5698)).

begin(model(5699)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5699)).

begin(model(5700)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5700)).

begin(model(5701)).
bought(fish).
shops(mary).
end(model(5701)).

begin(model(5702)).
bought(fish).
shops(mary).
end(model(5702)).

begin(model(5703)).
bought(spaghetti).
shops(mary).
end(model(5703)).

begin(model(5704)).
bought(fish).
shops(mary).
end(model(5704)).

begin(model(5705)).
bought(fish).
shops(mary).
end(model(5705)).

begin(model(5706)).
bought(spaghetti).
shops(mary).
end(model(5706)).

begin(model(5707)).
bought(spaghetti).
shops(mary).
end(model(5707)).

begin(model(5708)).
bought(spaghetti).
shops(mary).
end(model(5708)).

begin(model(5709)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5709)).

begin(model(5710)).
bought(fish).
shops(mary).
end(model(5710)).

begin(model(5711)).
bought(spaghetti).
shops(mary).
end(model(5711)).

begin(model(5712)).
end(model(5712)).

begin(model(5713)).
bought(fish).
shops(mary).
end(model(5713)).

begin(model(5714)).
bought(spaghetti).
shops(mary).
end(model(5714)).

begin(model(5715)).
bought(fish).
shops(mary).
end(model(5715)).

begin(model(5716)).
bought(fish).
shops(mary).
end(model(5716)).

begin(model(5717)).
bought(fish).
shops(mary).
end(model(5717)).

begin(model(5718)).
bought(fish).
shops(mary).
end(model(5718)).

begin(model(5719)).
bought(fish).
shops(mary).
end(model(5719)).

begin(model(5720)).
bought(fish).
shops(mary).
end(model(5720)).

begin(model(5721)).
bought(fish).
shops(mary).
end(model(5721)).

begin(model(5722)).
bought(fish).
shops(mary).
end(model(5722)).

begin(model(5723)).
bought(fish).
shops(mary).
end(model(5723)).

begin(model(5724)).
bought(spaghetti).
shops(mary).
end(model(5724)).

begin(model(5725)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5725)).

begin(model(5726)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5726)).

begin(model(5727)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5727)).

begin(model(5728)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5728)).

begin(model(5729)).
bought(fish).
shops(mary).
end(model(5729)).

begin(model(5730)).
bought(fish).
shops(mary).
end(model(5730)).

begin(model(5731)).
bought(fish).
shops(mary).
end(model(5731)).

begin(model(5732)).
end(model(5732)).

begin(model(5733)).
bought(fish).
shops(mary).
end(model(5733)).

begin(model(5734)).
bought(fish).
shops(mary).
end(model(5734)).

begin(model(5735)).
bought(fish).
shops(mary).
end(model(5735)).

begin(model(5736)).
bought(fish).
shops(mary).
end(model(5736)).

begin(model(5737)).
bought(fish).
shops(mary).
end(model(5737)).

begin(model(5738)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5738)).

begin(model(5739)).
bought(fish).
shops(mary).
end(model(5739)).

begin(model(5740)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5740)).

begin(model(5741)).
bought(fish).
shops(mary).
end(model(5741)).

begin(model(5742)).
bought(fish).
shops(mary).
end(model(5742)).

begin(model(5743)).
end(model(5743)).

begin(model(5744)).
bought(spaghetti).
shops(mary).
end(model(5744)).

begin(model(5745)).
bought(spaghetti).
shops(mary).
end(model(5745)).

begin(model(5746)).
bought(fish).
shops(mary).
end(model(5746)).

begin(model(5747)).
bought(spaghetti).
shops(mary).
end(model(5747)).

begin(model(5748)).
bought(fish).
shops(mary).
end(model(5748)).

begin(model(5749)).
bought(fish).
shops(mary).
end(model(5749)).

begin(model(5750)).
bought(spaghetti).
shops(mary).
end(model(5750)).

begin(model(5751)).
bought(fish).
shops(mary).
end(model(5751)).

begin(model(5752)).
bought(fish).
shops(mary).
end(model(5752)).

begin(model(5753)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5753)).

begin(model(5754)).
bought(spaghetti).
shops(mary).
end(model(5754)).

begin(model(5755)).
bought(fish).
shops(mary).
end(model(5755)).

begin(model(5756)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5756)).

begin(model(5757)).
bought(fish).
shops(mary).
end(model(5757)).

begin(model(5758)).
bought(fish).
shops(mary).
end(model(5758)).

begin(model(5759)).
end(model(5759)).

begin(model(5760)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5760)).

begin(model(5761)).
bought(fish).
shops(mary).
end(model(5761)).

begin(model(5762)).
bought(fish).
shops(mary).
end(model(5762)).

begin(model(5763)).
bought(spaghetti).
shops(mary).
end(model(5763)).

begin(model(5764)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5764)).

begin(model(5765)).
bought(fish).
shops(mary).
end(model(5765)).

begin(model(5766)).
bought(spaghetti).
shops(mary).
end(model(5766)).

begin(model(5767)).
bought(fish).
shops(mary).
end(model(5767)).

begin(model(5768)).
bought(fish).
shops(mary).
end(model(5768)).

begin(model(5769)).
bought(fish).
shops(mary).
end(model(5769)).

begin(model(5770)).
bought(fish).
shops(mary).
end(model(5770)).

begin(model(5771)).
bought(fish).
shops(mary).
end(model(5771)).

begin(model(5772)).
bought(fish).
shops(mary).
end(model(5772)).

begin(model(5773)).
bought(fish).
shops(mary).
end(model(5773)).

begin(model(5774)).
bought(spaghetti).
shops(mary).
end(model(5774)).

begin(model(5775)).
bought(spaghetti).
shops(mary).
end(model(5775)).

begin(model(5776)).
bought(fish).
shops(mary).
end(model(5776)).

begin(model(5777)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5777)).

begin(model(5778)).
bought(fish).
shops(mary).
end(model(5778)).

begin(model(5779)).
bought(spaghetti).
shops(mary).
end(model(5779)).

begin(model(5780)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5780)).

begin(model(5781)).
bought(fish).
shops(mary).
end(model(5781)).

begin(model(5782)).
bought(fish).
shops(mary).
end(model(5782)).

begin(model(5783)).
bought(fish).
shops(mary).
end(model(5783)).

begin(model(5784)).
bought(fish).
shops(mary).
end(model(5784)).

begin(model(5785)).
end(model(5785)).

begin(model(5786)).
bought(fish).
shops(mary).
end(model(5786)).

begin(model(5787)).
bought(fish).
shops(mary).
end(model(5787)).

begin(model(5788)).
bought(spaghetti).
shops(mary).
end(model(5788)).

begin(model(5789)).
bought(fish).
shops(mary).
end(model(5789)).

begin(model(5790)).
bought(fish).
shops(mary).
end(model(5790)).

begin(model(5791)).
bought(spaghetti).
shops(mary).
end(model(5791)).

begin(model(5792)).
bought(fish).
shops(mary).
end(model(5792)).

begin(model(5793)).
bought(spaghetti).
shops(mary).
end(model(5793)).

begin(model(5794)).
bought(fish).
shops(mary).
end(model(5794)).

begin(model(5795)).
end(model(5795)).

begin(model(5796)).
bought(spaghetti).
shops(mary).
end(model(5796)).

begin(model(5797)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5797)).

begin(model(5798)).
bought(fish).
shops(mary).
end(model(5798)).

begin(model(5799)).
bought(fish).
shops(mary).
end(model(5799)).

begin(model(5800)).
bought(fish).
shops(mary).
end(model(5800)).

begin(model(5801)).
bought(fish).
shops(mary).
end(model(5801)).

begin(model(5802)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5802)).

begin(model(5803)).
bought(spaghetti).
shops(mary).
end(model(5803)).

begin(model(5804)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5804)).

begin(model(5805)).
bought(fish).
shops(mary).
end(model(5805)).

begin(model(5806)).
bought(fish).
shops(mary).
end(model(5806)).

begin(model(5807)).
bought(fish).
shops(mary).
end(model(5807)).

begin(model(5808)).
bought(spaghetti).
shops(mary).
end(model(5808)).

begin(model(5809)).
end(model(5809)).

begin(model(5810)).
bought(fish).
shops(mary).
end(model(5810)).

begin(model(5811)).
bought(fish).
shops(mary).
end(model(5811)).

begin(model(5812)).
bought(fish).
shops(mary).
end(model(5812)).

begin(model(5813)).
bought(fish).
shops(mary).
end(model(5813)).

begin(model(5814)).
bought(spaghetti).
shops(john).
end(model(5814)).

begin(model(5815)).
bought(fish).
shops(mary).
end(model(5815)).

begin(model(5816)).
end(model(5816)).

begin(model(5817)).
bought(spaghetti).
shops(mary).
end(model(5817)).

begin(model(5818)).
bought(fish).
shops(mary).
end(model(5818)).

begin(model(5819)).
bought(spaghetti).
shops(mary).
end(model(5819)).

begin(model(5820)).
bought(fish).
shops(mary).
end(model(5820)).

begin(model(5821)).
bought(spaghetti).
shops(mary).
end(model(5821)).

begin(model(5822)).
bought(fish).
shops(mary).
end(model(5822)).

begin(model(5823)).
bought(spaghetti).
shops(mary).
end(model(5823)).

begin(model(5824)).
bought(spaghetti).
shops(mary).
end(model(5824)).

begin(model(5825)).
end(model(5825)).

begin(model(5826)).
bought(fish).
shops(mary).
end(model(5826)).

begin(model(5827)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5827)).

begin(model(5828)).
bought(spaghetti).
shops(mary).
end(model(5828)).

begin(model(5829)).
bought(fish).
shops(mary).
end(model(5829)).

begin(model(5830)).
bought(fish).
shops(mary).
end(model(5830)).

begin(model(5831)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5831)).

begin(model(5832)).
bought(spaghetti).
shops(mary).
end(model(5832)).

begin(model(5833)).
bought(fish).
shops(mary).
end(model(5833)).

begin(model(5834)).
bought(steak).
shops(john).
end(model(5834)).

begin(model(5835)).
bought(fish).
shops(mary).
end(model(5835)).

begin(model(5836)).
bought(spaghetti).
shops(john).
end(model(5836)).

begin(model(5837)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5837)).

begin(model(5838)).
bought(fish).
shops(mary).
end(model(5838)).

begin(model(5839)).
bought(fish).
shops(mary).
end(model(5839)).

begin(model(5840)).
end(model(5840)).

begin(model(5841)).
bought(spaghetti).
shops(mary).
end(model(5841)).

begin(model(5842)).
bought(fish).
shops(mary).
end(model(5842)).

begin(model(5843)).
bought(fish).
shops(mary).
end(model(5843)).

begin(model(5844)).
bought(fish).
shops(mary).
end(model(5844)).

begin(model(5845)).
bought(fish).
shops(mary).
end(model(5845)).

begin(model(5846)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5846)).

begin(model(5847)).
bought(spaghetti).
shops(john).
end(model(5847)).

begin(model(5848)).
bought(fish).
shops(mary).
end(model(5848)).

begin(model(5849)).
bought(fish).
shops(mary).
end(model(5849)).

begin(model(5850)).
bought(spaghetti).
shops(mary).
end(model(5850)).

begin(model(5851)).
bought(fish).
shops(mary).
end(model(5851)).

begin(model(5852)).
bought(fish).
shops(mary).
end(model(5852)).

begin(model(5853)).
bought(fish).
shops(mary).
end(model(5853)).

begin(model(5854)).
bought(fish).
shops(mary).
end(model(5854)).

begin(model(5855)).
bought(fish).
shops(mary).
end(model(5855)).

begin(model(5856)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5856)).

begin(model(5857)).
bought(fish).
shops(mary).
end(model(5857)).

begin(model(5858)).
end(model(5858)).

begin(model(5859)).
bought(fish).
shops(mary).
end(model(5859)).

begin(model(5860)).
bought(spaghetti).
shops(mary).
end(model(5860)).

begin(model(5861)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5861)).

begin(model(5862)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5862)).

begin(model(5863)).
bought(fish).
shops(mary).
end(model(5863)).

begin(model(5864)).
bought(fish).
shops(mary).
end(model(5864)).

begin(model(5865)).
bought(spaghetti).
shops(mary).
end(model(5865)).

begin(model(5866)).
end(model(5866)).

begin(model(5867)).
bought(fish).
shops(mary).
end(model(5867)).

begin(model(5868)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5868)).

begin(model(5869)).
end(model(5869)).

begin(model(5870)).
end(model(5870)).

begin(model(5871)).
bought(fish).
shops(mary).
end(model(5871)).

begin(model(5872)).
bought(spaghetti).
shops(mary).
end(model(5872)).

begin(model(5873)).
bought(fish).
shops(mary).
end(model(5873)).

begin(model(5874)).
bought(fish).
shops(mary).
end(model(5874)).

begin(model(5875)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5875)).

begin(model(5876)).
bought(spaghetti).
shops(mary).
end(model(5876)).

begin(model(5877)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5877)).

begin(model(5878)).
bought(spaghetti).
shops(mary).
end(model(5878)).

begin(model(5879)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5879)).

begin(model(5880)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5880)).

begin(model(5881)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5881)).

begin(model(5882)).
bought(fish).
shops(mary).
end(model(5882)).

begin(model(5883)).
bought(fish).
shops(mary).
end(model(5883)).

begin(model(5884)).
bought(fish).
shops(mary).
end(model(5884)).

begin(model(5885)).
bought(spaghetti).
shops(mary).
end(model(5885)).

begin(model(5886)).
end(model(5886)).

begin(model(5887)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5887)).

begin(model(5888)).
bought(spaghetti).
shops(mary).
end(model(5888)).

begin(model(5889)).
bought(spaghetti).
shops(mary).
end(model(5889)).

begin(model(5890)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5890)).

begin(model(5891)).
bought(fish).
shops(mary).
end(model(5891)).

begin(model(5892)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5892)).

begin(model(5893)).
bought(fish).
shops(mary).
end(model(5893)).

begin(model(5894)).
bought(spaghetti).
shops(mary).
end(model(5894)).

begin(model(5895)).
bought(fish).
shops(mary).
end(model(5895)).

begin(model(5896)).
bought(fish).
shops(mary).
end(model(5896)).

begin(model(5897)).
bought(spaghetti).
shops(mary).
end(model(5897)).

begin(model(5898)).
bought(fish).
shops(mary).
end(model(5898)).

begin(model(5899)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5899)).

begin(model(5900)).
bought(fish).
shops(mary).
end(model(5900)).

begin(model(5901)).
bought(fish).
shops(mary).
end(model(5901)).

begin(model(5902)).
bought(spaghetti).
shops(mary).
end(model(5902)).

begin(model(5903)).
bought(spaghetti).
shops(mary).
end(model(5903)).

begin(model(5904)).
bought(spaghetti).
shops(john).
end(model(5904)).

begin(model(5905)).
bought(fish).
shops(mary).
end(model(5905)).

begin(model(5906)).
bought(spaghetti).
shops(mary).
end(model(5906)).

begin(model(5907)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5907)).

begin(model(5908)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5908)).

begin(model(5909)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5909)).

begin(model(5910)).
bought(fish).
shops(mary).
end(model(5910)).

begin(model(5911)).
bought(fish).
shops(mary).
end(model(5911)).

begin(model(5912)).
bought(fish).
shops(mary).
end(model(5912)).

begin(model(5913)).
bought(spaghetti).
shops(mary).
end(model(5913)).

begin(model(5914)).
bought(spaghetti).
shops(mary).
end(model(5914)).

begin(model(5915)).
bought(fish).
shops(mary).
end(model(5915)).

begin(model(5916)).
bought(spaghetti).
shops(mary).
end(model(5916)).

begin(model(5917)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5917)).

begin(model(5918)).
bought(fish).
shops(mary).
end(model(5918)).

begin(model(5919)).
bought(fish).
shops(mary).
end(model(5919)).

begin(model(5920)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5920)).

begin(model(5921)).
bought(spaghetti).
shops(mary).
end(model(5921)).

begin(model(5922)).
bought(fish).
shops(mary).
end(model(5922)).

begin(model(5923)).
bought(fish).
shops(mary).
end(model(5923)).

begin(model(5924)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5924)).

begin(model(5925)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5925)).

begin(model(5926)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5926)).

begin(model(5927)).
bought(fish).
shops(mary).
end(model(5927)).

begin(model(5928)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5928)).

begin(model(5929)).
bought(fish).
shops(mary).
end(model(5929)).

begin(model(5930)).
bought(fish).
shops(mary).
end(model(5930)).

begin(model(5931)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5931)).

begin(model(5932)).
bought(fish).
shops(mary).
end(model(5932)).

begin(model(5933)).
bought(fish).
shops(mary).
end(model(5933)).

begin(model(5934)).
bought(fish).
shops(mary).
end(model(5934)).

begin(model(5935)).
bought(fish).
shops(mary).
end(model(5935)).

begin(model(5936)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5936)).

begin(model(5937)).
bought(fish).
shops(mary).
end(model(5937)).

begin(model(5938)).
bought(fish).
shops(mary).
end(model(5938)).

begin(model(5939)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5939)).

begin(model(5940)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5940)).

begin(model(5941)).
bought(fish).
shops(mary).
end(model(5941)).

begin(model(5942)).
bought(fish).
shops(mary).
end(model(5942)).

begin(model(5943)).
end(model(5943)).

begin(model(5944)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5944)).

begin(model(5945)).
bought(spaghetti).
shops(mary).
end(model(5945)).

begin(model(5946)).
bought(spaghetti).
shops(mary).
end(model(5946)).

begin(model(5947)).
bought(spaghetti).
shops(mary).
end(model(5947)).

begin(model(5948)).
bought(fish).
shops(mary).
end(model(5948)).

begin(model(5949)).
bought(spaghetti).
shops(mary).
end(model(5949)).

begin(model(5950)).
bought(fish).
shops(mary).
end(model(5950)).

begin(model(5951)).
bought(fish).
shops(mary).
end(model(5951)).

begin(model(5952)).
end(model(5952)).

begin(model(5953)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5953)).

begin(model(5954)).
bought(spaghetti).
shops(mary).
end(model(5954)).

begin(model(5955)).
bought(fish).
shops(mary).
end(model(5955)).

begin(model(5956)).
end(model(5956)).

begin(model(5957)).
bought(spaghetti).
shops(mary).
end(model(5957)).

begin(model(5958)).
end(model(5958)).

begin(model(5959)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5959)).

begin(model(5960)).
end(model(5960)).

begin(model(5961)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(5961)).

begin(model(5962)).
end(model(5962)).

begin(model(5963)).
bought(fish).
shops(mary).
end(model(5963)).

begin(model(5964)).
bought(spaghetti).
shops(mary).
end(model(5964)).

begin(model(5965)).
bought(spaghetti).
shops(mary).
end(model(5965)).

begin(model(5966)).
bought(fish).
shops(mary).
end(model(5966)).

begin(model(5967)).
bought(fish).
shops(mary).
end(model(5967)).

begin(model(5968)).
bought(spaghetti).
shops(mary).
end(model(5968)).

begin(model(5969)).
bought(fish).
shops(mary).
end(model(5969)).

begin(model(5970)).
bought(fish).
shops(mary).
end(model(5970)).

begin(model(5971)).
bought(fish).
shops(mary).
end(model(5971)).

begin(model(5972)).
bought(fish).
shops(mary).
end(model(5972)).

begin(model(5973)).
end(model(5973)).

begin(model(5974)).
bought(fish).
shops(mary).
end(model(5974)).

begin(model(5975)).
bought(fish).
shops(mary).
end(model(5975)).

begin(model(5976)).
bought(fish).
shops(mary).
end(model(5976)).

begin(model(5977)).
bought(fish).
shops(mary).
end(model(5977)).

begin(model(5978)).
bought(spaghetti).
shops(mary).
end(model(5978)).

begin(model(5979)).
bought(fish).
shops(mary).
end(model(5979)).

begin(model(5980)).
bought(fish).
shops(mary).
end(model(5980)).

begin(model(5981)).
bought(spaghetti).
shops(mary).
end(model(5981)).

begin(model(5982)).
bought(steak).
shops(john).
end(model(5982)).

begin(model(5983)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5983)).

begin(model(5984)).
bought(fish).
shops(mary).
end(model(5984)).

begin(model(5985)).
bought(spaghetti).
shops(mary).
end(model(5985)).

begin(model(5986)).
bought(fish).
shops(mary).
end(model(5986)).

begin(model(5987)).
bought(spaghetti).
shops(mary).
end(model(5987)).

begin(model(5988)).
bought(fish).
shops(mary).
end(model(5988)).

begin(model(5989)).
bought(fish).
shops(mary).
end(model(5989)).

begin(model(5990)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(5990)).

begin(model(5991)).
end(model(5991)).

begin(model(5992)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5992)).

begin(model(5993)).
bought(spaghetti).
shops(mary).
end(model(5993)).

begin(model(5994)).
bought(fish).
shops(mary).
end(model(5994)).

begin(model(5995)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(5995)).

begin(model(5996)).
bought(fish).
shops(mary).
end(model(5996)).

begin(model(5997)).
bought(fish).
shops(mary).
end(model(5997)).

begin(model(5998)).
end(model(5998)).

begin(model(5999)).
bought(fish).
shops(mary).
end(model(5999)).

begin(model(6000)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6000)).

begin(model(6001)).
bought(spaghetti).
shops(mary).
end(model(6001)).

begin(model(6002)).
bought(fish).
shops(mary).
end(model(6002)).

begin(model(6003)).
end(model(6003)).

begin(model(6004)).
end(model(6004)).

begin(model(6005)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6005)).

begin(model(6006)).
bought(fish).
shops(mary).
end(model(6006)).

begin(model(6007)).
bought(fish).
shops(mary).
end(model(6007)).

begin(model(6008)).
bought(fish).
shops(mary).
end(model(6008)).

begin(model(6009)).
bought(spaghetti).
shops(mary).
end(model(6009)).

begin(model(6010)).
end(model(6010)).

begin(model(6011)).
bought(fish).
shops(mary).
end(model(6011)).

begin(model(6012)).
end(model(6012)).

begin(model(6013)).
bought(fish).
shops(mary).
end(model(6013)).

begin(model(6014)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6014)).

begin(model(6015)).
bought(fish).
shops(mary).
end(model(6015)).

begin(model(6016)).
bought(spaghetti).
shops(john).
end(model(6016)).

begin(model(6017)).
bought(fish).
shops(mary).
end(model(6017)).

begin(model(6018)).
bought(fish).
shops(mary).
end(model(6018)).

begin(model(6019)).
bought(fish).
shops(mary).
end(model(6019)).

begin(model(6020)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6020)).

begin(model(6021)).
end(model(6021)).

begin(model(6022)).
bought(fish).
shops(mary).
end(model(6022)).

begin(model(6023)).
bought(spaghetti).
shops(mary).
end(model(6023)).

begin(model(6024)).
bought(fish).
shops(mary).
end(model(6024)).

begin(model(6025)).
bought(spaghetti).
shops(mary).
end(model(6025)).

begin(model(6026)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6026)).

begin(model(6027)).
bought(fish).
shops(mary).
end(model(6027)).

begin(model(6028)).
bought(fish).
shops(mary).
end(model(6028)).

begin(model(6029)).
bought(spaghetti).
shops(mary).
end(model(6029)).

begin(model(6030)).
bought(spaghetti).
shops(john).
end(model(6030)).

begin(model(6031)).
bought(fish).
shops(mary).
end(model(6031)).

begin(model(6032)).
bought(fish).
shops(mary).
end(model(6032)).

begin(model(6033)).
end(model(6033)).

begin(model(6034)).
bought(fish).
shops(mary).
end(model(6034)).

begin(model(6035)).
bought(fish).
shops(mary).
end(model(6035)).

begin(model(6036)).
bought(fish).
shops(mary).
end(model(6036)).

begin(model(6037)).
bought(fish).
shops(mary).
end(model(6037)).

begin(model(6038)).
bought(fish).
shops(mary).
end(model(6038)).

begin(model(6039)).
bought(fish).
shops(mary).
end(model(6039)).

begin(model(6040)).
bought(fish).
shops(mary).
end(model(6040)).

begin(model(6041)).
bought(fish).
shops(mary).
end(model(6041)).

begin(model(6042)).
bought(fish).
shops(mary).
end(model(6042)).

begin(model(6043)).
bought(fish).
shops(mary).
end(model(6043)).

begin(model(6044)).
bought(fish).
shops(mary).
end(model(6044)).

begin(model(6045)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6045)).

begin(model(6046)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6046)).

begin(model(6047)).
bought(spaghetti).
shops(mary).
end(model(6047)).

begin(model(6048)).
bought(fish).
shops(mary).
end(model(6048)).

begin(model(6049)).
bought(fish).
shops(mary).
end(model(6049)).

begin(model(6050)).
bought(spaghetti).
shops(mary).
end(model(6050)).

begin(model(6051)).
bought(fish).
shops(mary).
end(model(6051)).

begin(model(6052)).
bought(fish).
shops(mary).
end(model(6052)).

begin(model(6053)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6053)).

begin(model(6054)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6054)).

begin(model(6055)).
bought(fish).
shops(mary).
end(model(6055)).

begin(model(6056)).
bought(fish).
shops(mary).
end(model(6056)).

begin(model(6057)).
bought(fish).
shops(mary).
end(model(6057)).

begin(model(6058)).
bought(fish).
shops(mary).
end(model(6058)).

begin(model(6059)).
bought(spaghetti).
shops(mary).
end(model(6059)).

begin(model(6060)).
bought(spaghetti).
shops(mary).
end(model(6060)).

begin(model(6061)).
bought(fish).
shops(mary).
end(model(6061)).

begin(model(6062)).
bought(spaghetti).
shops(mary).
end(model(6062)).

begin(model(6063)).
bought(fish).
shops(mary).
end(model(6063)).

begin(model(6064)).
bought(fish).
shops(mary).
end(model(6064)).

begin(model(6065)).
bought(fish).
shops(mary).
end(model(6065)).

begin(model(6066)).
bought(fish).
shops(mary).
end(model(6066)).

begin(model(6067)).
end(model(6067)).

begin(model(6068)).
end(model(6068)).

begin(model(6069)).
bought(spaghetti).
shops(mary).
end(model(6069)).

begin(model(6070)).
bought(fish).
shops(mary).
end(model(6070)).

begin(model(6071)).
bought(fish).
shops(mary).
end(model(6071)).

begin(model(6072)).
end(model(6072)).

begin(model(6073)).
bought(fish).
shops(mary).
end(model(6073)).

begin(model(6074)).
bought(spaghetti).
shops(mary).
end(model(6074)).

begin(model(6075)).
bought(fish).
shops(mary).
end(model(6075)).

begin(model(6076)).
bought(fish).
shops(mary).
end(model(6076)).

begin(model(6077)).
bought(fish).
shops(mary).
end(model(6077)).

begin(model(6078)).
bought(spaghetti).
shops(mary).
end(model(6078)).

begin(model(6079)).
bought(fish).
shops(mary).
end(model(6079)).

begin(model(6080)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6080)).

begin(model(6081)).
bought(fish).
shops(mary).
end(model(6081)).

begin(model(6082)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6082)).

begin(model(6083)).
bought(fish).
shops(mary).
end(model(6083)).

begin(model(6084)).
bought(fish).
shops(mary).
end(model(6084)).

begin(model(6085)).
bought(spaghetti).
shops(john).
end(model(6085)).

begin(model(6086)).
bought(fish).
shops(mary).
end(model(6086)).

begin(model(6087)).
bought(spaghetti).
shops(mary).
end(model(6087)).

begin(model(6088)).
bought(spaghetti).
shops(mary).
end(model(6088)).

begin(model(6089)).
bought(spaghetti).
shops(mary).
end(model(6089)).

begin(model(6090)).
bought(fish).
shops(mary).
end(model(6090)).

begin(model(6091)).
bought(fish).
shops(mary).
end(model(6091)).

begin(model(6092)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6092)).

begin(model(6093)).
bought(spaghetti).
shops(mary).
end(model(6093)).

begin(model(6094)).
bought(fish).
shops(mary).
end(model(6094)).

begin(model(6095)).
bought(fish).
shops(mary).
end(model(6095)).

begin(model(6096)).
bought(fish).
shops(mary).
end(model(6096)).

begin(model(6097)).
bought(fish).
shops(mary).
end(model(6097)).

begin(model(6098)).
bought(fish).
shops(mary).
end(model(6098)).

begin(model(6099)).
bought(spaghetti).
shops(mary).
end(model(6099)).

begin(model(6100)).
bought(fish).
shops(mary).
end(model(6100)).

begin(model(6101)).
bought(spaghetti).
shops(mary).
end(model(6101)).

begin(model(6102)).
end(model(6102)).

begin(model(6103)).
bought(fish).
shops(mary).
end(model(6103)).

begin(model(6104)).
bought(spaghetti).
shops(mary).
end(model(6104)).

begin(model(6105)).
bought(fish).
shops(mary).
end(model(6105)).

begin(model(6106)).
bought(fish).
shops(mary).
end(model(6106)).

begin(model(6107)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6107)).

begin(model(6108)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6108)).

begin(model(6109)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6109)).

begin(model(6110)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6110)).

begin(model(6111)).
bought(spaghetti).
shops(mary).
end(model(6111)).

begin(model(6112)).
bought(fish).
shops(mary).
end(model(6112)).

begin(model(6113)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6113)).

begin(model(6114)).
bought(fish).
shops(mary).
end(model(6114)).

begin(model(6115)).
end(model(6115)).

begin(model(6116)).
bought(fish).
shops(mary).
end(model(6116)).

begin(model(6117)).
bought(fish).
shops(mary).
end(model(6117)).

begin(model(6118)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6118)).

begin(model(6119)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6119)).

begin(model(6120)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6120)).

begin(model(6121)).
bought(fish).
shops(mary).
end(model(6121)).

begin(model(6122)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6122)).

begin(model(6123)).
end(model(6123)).

begin(model(6124)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6124)).

begin(model(6125)).
bought(spaghetti).
shops(mary).
end(model(6125)).

begin(model(6126)).
bought(spaghetti).
shops(mary).
end(model(6126)).

begin(model(6127)).
end(model(6127)).

begin(model(6128)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6128)).

begin(model(6129)).
bought(fish).
shops(mary).
end(model(6129)).

begin(model(6130)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6130)).

begin(model(6131)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6131)).

begin(model(6132)).
bought(fish).
shops(mary).
end(model(6132)).

begin(model(6133)).
bought(spaghetti).
shops(mary).
end(model(6133)).

begin(model(6134)).
bought(spaghetti).
shops(mary).
end(model(6134)).

begin(model(6135)).
bought(fish).
shops(mary).
end(model(6135)).

begin(model(6136)).
bought(fish).
shops(mary).
end(model(6136)).

begin(model(6137)).
bought(spaghetti).
shops(mary).
end(model(6137)).

begin(model(6138)).
bought(fish).
shops(mary).
end(model(6138)).

begin(model(6139)).
bought(spaghetti).
shops(mary).
end(model(6139)).

begin(model(6140)).
bought(spaghetti).
shops(mary).
end(model(6140)).

begin(model(6141)).
bought(spaghetti).
shops(mary).
end(model(6141)).

begin(model(6142)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6142)).

begin(model(6143)).
bought(spaghetti).
shops(mary).
end(model(6143)).

begin(model(6144)).
bought(spaghetti).
shops(mary).
end(model(6144)).

begin(model(6145)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6145)).

begin(model(6146)).
bought(fish).
shops(mary).
end(model(6146)).

begin(model(6147)).
bought(spaghetti).
shops(mary).
end(model(6147)).

begin(model(6148)).
bought(fish).
shops(mary).
end(model(6148)).

begin(model(6149)).
bought(fish).
shops(mary).
end(model(6149)).

begin(model(6150)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6150)).

begin(model(6151)).
bought(fish).
shops(mary).
end(model(6151)).

begin(model(6152)).
bought(fish).
shops(mary).
end(model(6152)).

begin(model(6153)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6153)).

begin(model(6154)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6154)).

begin(model(6155)).
bought(fish).
shops(mary).
end(model(6155)).

begin(model(6156)).
bought(spaghetti).
shops(mary).
end(model(6156)).

begin(model(6157)).
bought(fish).
shops(mary).
end(model(6157)).

begin(model(6158)).
bought(fish).
shops(mary).
end(model(6158)).

begin(model(6159)).
bought(spaghetti).
shops(mary).
end(model(6159)).

begin(model(6160)).
bought(fish).
shops(mary).
end(model(6160)).

begin(model(6161)).
bought(fish).
shops(mary).
end(model(6161)).

begin(model(6162)).
end(model(6162)).

begin(model(6163)).
bought(spaghetti).
shops(mary).
end(model(6163)).

begin(model(6164)).
bought(fish).
shops(mary).
end(model(6164)).

begin(model(6165)).
bought(fish).
shops(mary).
end(model(6165)).

begin(model(6166)).
bought(fish).
shops(mary).
end(model(6166)).

begin(model(6167)).
bought(fish).
shops(mary).
end(model(6167)).

begin(model(6168)).
bought(fish).
shops(mary).
end(model(6168)).

begin(model(6169)).
bought(fish).
shops(mary).
end(model(6169)).

begin(model(6170)).
bought(fish).
shops(mary).
end(model(6170)).

begin(model(6171)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6171)).

begin(model(6172)).
bought(fish).
shops(mary).
end(model(6172)).

begin(model(6173)).
bought(spaghetti).
shops(mary).
end(model(6173)).

begin(model(6174)).
bought(fish).
shops(mary).
end(model(6174)).

begin(model(6175)).
bought(fish).
shops(mary).
end(model(6175)).

begin(model(6176)).
end(model(6176)).

begin(model(6177)).
bought(fish).
shops(mary).
end(model(6177)).

begin(model(6178)).
bought(spaghetti).
shops(mary).
end(model(6178)).

begin(model(6179)).
bought(fish).
shops(mary).
end(model(6179)).

begin(model(6180)).
bought(fish).
shops(mary).
end(model(6180)).

begin(model(6181)).
bought(spaghetti).
shops(mary).
end(model(6181)).

begin(model(6182)).
bought(fish).
shops(mary).
end(model(6182)).

begin(model(6183)).
bought(fish).
shops(mary).
end(model(6183)).

begin(model(6184)).
bought(fish).
shops(mary).
end(model(6184)).

begin(model(6185)).
bought(fish).
shops(mary).
end(model(6185)).

begin(model(6186)).
bought(fish).
shops(mary).
end(model(6186)).

begin(model(6187)).
bought(spaghetti).
shops(mary).
end(model(6187)).

begin(model(6188)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6188)).

begin(model(6189)).
bought(fish).
shops(mary).
end(model(6189)).

begin(model(6190)).
bought(spaghetti).
shops(mary).
end(model(6190)).

begin(model(6191)).
end(model(6191)).

begin(model(6192)).
bought(fish).
shops(mary).
end(model(6192)).

begin(model(6193)).
bought(fish).
shops(mary).
end(model(6193)).

begin(model(6194)).
end(model(6194)).

begin(model(6195)).
bought(spaghetti).
shops(mary).
end(model(6195)).

begin(model(6196)).
bought(spaghetti).
shops(mary).
end(model(6196)).

begin(model(6197)).
bought(fish).
shops(mary).
end(model(6197)).

begin(model(6198)).
bought(fish).
shops(mary).
end(model(6198)).

begin(model(6199)).
bought(spaghetti).
shops(mary).
end(model(6199)).

begin(model(6200)).
bought(spaghetti).
shops(mary).
end(model(6200)).

begin(model(6201)).
bought(fish).
shops(mary).
end(model(6201)).

begin(model(6202)).
bought(fish).
shops(mary).
end(model(6202)).

begin(model(6203)).
bought(steak).
shops(john).
end(model(6203)).

begin(model(6204)).
bought(spaghetti).
shops(mary).
end(model(6204)).

begin(model(6205)).
bought(spaghetti).
shops(mary).
end(model(6205)).

begin(model(6206)).
bought(fish).
shops(mary).
end(model(6206)).

begin(model(6207)).
bought(fish).
shops(mary).
end(model(6207)).

begin(model(6208)).
bought(spaghetti).
shops(mary).
end(model(6208)).

begin(model(6209)).
bought(fish).
shops(mary).
end(model(6209)).

begin(model(6210)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6210)).

begin(model(6211)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6211)).

begin(model(6212)).
bought(fish).
shops(mary).
end(model(6212)).

begin(model(6213)).
bought(fish).
shops(mary).
end(model(6213)).

begin(model(6214)).
bought(fish).
shops(mary).
end(model(6214)).

begin(model(6215)).
bought(fish).
shops(mary).
end(model(6215)).

begin(model(6216)).
bought(fish).
shops(mary).
end(model(6216)).

begin(model(6217)).
bought(fish).
shops(mary).
end(model(6217)).

begin(model(6218)).
bought(fish).
shops(mary).
end(model(6218)).

begin(model(6219)).
end(model(6219)).

begin(model(6220)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6220)).

begin(model(6221)).
bought(spaghetti).
shops(mary).
end(model(6221)).

begin(model(6222)).
bought(fish).
shops(mary).
end(model(6222)).

begin(model(6223)).
bought(fish).
shops(mary).
end(model(6223)).

begin(model(6224)).
bought(fish).
shops(mary).
end(model(6224)).

begin(model(6225)).
bought(spaghetti).
shops(mary).
end(model(6225)).

begin(model(6226)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6226)).

begin(model(6227)).
bought(spaghetti).
shops(mary).
end(model(6227)).

begin(model(6228)).
bought(fish).
shops(mary).
end(model(6228)).

begin(model(6229)).
bought(fish).
shops(mary).
end(model(6229)).

begin(model(6230)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6230)).

begin(model(6231)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6231)).

begin(model(6232)).
bought(spaghetti).
shops(mary).
end(model(6232)).

begin(model(6233)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6233)).

begin(model(6234)).
bought(spaghetti).
shops(mary).
end(model(6234)).

begin(model(6235)).
bought(spaghetti).
shops(mary).
end(model(6235)).

begin(model(6236)).
bought(spaghetti).
shops(mary).
end(model(6236)).

begin(model(6237)).
bought(fish).
shops(mary).
end(model(6237)).

begin(model(6238)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6238)).

begin(model(6239)).
bought(spaghetti).
shops(mary).
end(model(6239)).

begin(model(6240)).
bought(fish).
shops(mary).
end(model(6240)).

begin(model(6241)).
bought(spaghetti).
shops(mary).
end(model(6241)).

begin(model(6242)).
bought(spaghetti).
shops(mary).
end(model(6242)).

begin(model(6243)).
bought(spaghetti).
shops(mary).
end(model(6243)).

begin(model(6244)).
bought(fish).
shops(mary).
end(model(6244)).

begin(model(6245)).
end(model(6245)).

begin(model(6246)).
bought(fish).
shops(mary).
end(model(6246)).

begin(model(6247)).
bought(fish).
shops(mary).
end(model(6247)).

begin(model(6248)).
bought(fish).
shops(mary).
end(model(6248)).

begin(model(6249)).
bought(fish).
shops(mary).
end(model(6249)).

begin(model(6250)).
bought(fish).
shops(mary).
end(model(6250)).

begin(model(6251)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6251)).

begin(model(6252)).
bought(fish).
shops(mary).
end(model(6252)).

begin(model(6253)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6253)).

begin(model(6254)).
bought(fish).
shops(mary).
end(model(6254)).

begin(model(6255)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6255)).

begin(model(6256)).
bought(spaghetti).
shops(mary).
end(model(6256)).

begin(model(6257)).
bought(fish).
shops(mary).
end(model(6257)).

begin(model(6258)).
bought(fish).
shops(mary).
end(model(6258)).

begin(model(6259)).
end(model(6259)).

begin(model(6260)).
bought(fish).
shops(mary).
end(model(6260)).

begin(model(6261)).
bought(fish).
shops(mary).
end(model(6261)).

begin(model(6262)).
bought(fish).
shops(mary).
end(model(6262)).

begin(model(6263)).
bought(fish).
shops(mary).
end(model(6263)).

begin(model(6264)).
bought(fish).
shops(mary).
end(model(6264)).

begin(model(6265)).
bought(fish).
shops(mary).
end(model(6265)).

begin(model(6266)).
bought(spaghetti).
shops(mary).
end(model(6266)).

begin(model(6267)).
bought(fish).
shops(mary).
end(model(6267)).

begin(model(6268)).
bought(fish).
shops(mary).
end(model(6268)).

begin(model(6269)).
bought(fish).
shops(mary).
end(model(6269)).

begin(model(6270)).
bought(fish).
shops(mary).
end(model(6270)).

begin(model(6271)).
bought(steak).
shops(john).
end(model(6271)).

begin(model(6272)).
bought(fish).
shops(mary).
end(model(6272)).

begin(model(6273)).
bought(spaghetti).
shops(mary).
end(model(6273)).

begin(model(6274)).
bought(spaghetti).
shops(mary).
end(model(6274)).

begin(model(6275)).
bought(fish).
shops(mary).
end(model(6275)).

begin(model(6276)).
bought(spaghetti).
shops(mary).
end(model(6276)).

begin(model(6277)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6277)).

begin(model(6278)).
bought(spaghetti).
shops(mary).
end(model(6278)).

begin(model(6279)).
bought(fish).
shops(mary).
end(model(6279)).

begin(model(6280)).
bought(fish).
shops(mary).
end(model(6280)).

begin(model(6281)).
bought(fish).
shops(mary).
end(model(6281)).

begin(model(6282)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6282)).

begin(model(6283)).
bought(spaghetti).
shops(mary).
end(model(6283)).

begin(model(6284)).
bought(fish).
shops(mary).
end(model(6284)).

begin(model(6285)).
bought(fish).
shops(mary).
end(model(6285)).

begin(model(6286)).
bought(fish).
shops(mary).
end(model(6286)).

begin(model(6287)).
end(model(6287)).

begin(model(6288)).
bought(fish).
shops(mary).
end(model(6288)).

begin(model(6289)).
end(model(6289)).

begin(model(6290)).
bought(fish).
shops(mary).
end(model(6290)).

begin(model(6291)).
bought(spaghetti).
shops(mary).
end(model(6291)).

begin(model(6292)).
bought(fish).
shops(mary).
end(model(6292)).

begin(model(6293)).
bought(fish).
shops(mary).
end(model(6293)).

begin(model(6294)).
bought(fish).
shops(mary).
end(model(6294)).

begin(model(6295)).
bought(fish).
shops(mary).
end(model(6295)).

begin(model(6296)).
bought(fish).
shops(mary).
end(model(6296)).

begin(model(6297)).
bought(fish).
shops(mary).
end(model(6297)).

begin(model(6298)).
bought(spaghetti).
shops(mary).
end(model(6298)).

begin(model(6299)).
bought(fish).
shops(mary).
end(model(6299)).

begin(model(6300)).
bought(spaghetti).
shops(mary).
end(model(6300)).

begin(model(6301)).
bought(fish).
shops(mary).
end(model(6301)).

begin(model(6302)).
bought(spaghetti).
shops(mary).
end(model(6302)).

begin(model(6303)).
end(model(6303)).

begin(model(6304)).
bought(fish).
shops(mary).
end(model(6304)).

begin(model(6305)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6305)).

begin(model(6306)).
end(model(6306)).

begin(model(6307)).
bought(spaghetti).
shops(mary).
end(model(6307)).

begin(model(6308)).
bought(fish).
shops(mary).
end(model(6308)).

begin(model(6309)).
bought(spaghetti).
shops(mary).
end(model(6309)).

begin(model(6310)).
bought(spaghetti).
shops(mary).
end(model(6310)).

begin(model(6311)).
bought(fish).
shops(mary).
end(model(6311)).

begin(model(6312)).
bought(spaghetti).
shops(mary).
end(model(6312)).

begin(model(6313)).
bought(fish).
shops(mary).
end(model(6313)).

begin(model(6314)).
bought(fish).
shops(mary).
end(model(6314)).

begin(model(6315)).
bought(fish).
shops(mary).
end(model(6315)).

begin(model(6316)).
bought(fish).
shops(mary).
end(model(6316)).

begin(model(6317)).
end(model(6317)).

begin(model(6318)).
end(model(6318)).

begin(model(6319)).
bought(spaghetti).
shops(mary).
end(model(6319)).

begin(model(6320)).
end(model(6320)).

begin(model(6321)).
bought(fish).
shops(mary).
end(model(6321)).

begin(model(6322)).
bought(fish).
shops(mary).
end(model(6322)).

begin(model(6323)).
bought(fish).
shops(mary).
end(model(6323)).

begin(model(6324)).
end(model(6324)).

begin(model(6325)).
bought(spaghetti).
shops(mary).
end(model(6325)).

begin(model(6326)).
end(model(6326)).

begin(model(6327)).
bought(fish).
shops(mary).
end(model(6327)).

begin(model(6328)).
bought(spaghetti).
shops(john).
end(model(6328)).

begin(model(6329)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6329)).

begin(model(6330)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6330)).

begin(model(6331)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6331)).

begin(model(6332)).
bought(spaghetti).
shops(mary).
end(model(6332)).

begin(model(6333)).
bought(fish).
shops(mary).
end(model(6333)).

begin(model(6334)).
bought(spaghetti).
shops(mary).
end(model(6334)).

begin(model(6335)).
bought(fish).
shops(mary).
end(model(6335)).

begin(model(6336)).
bought(spaghetti).
shops(mary).
end(model(6336)).

begin(model(6337)).
bought(fish).
shops(mary).
end(model(6337)).

begin(model(6338)).
bought(spaghetti).
shops(mary).
end(model(6338)).

begin(model(6339)).
bought(spaghetti).
shops(mary).
end(model(6339)).

begin(model(6340)).
bought(fish).
shops(mary).
end(model(6340)).

begin(model(6341)).
bought(fish).
shops(mary).
end(model(6341)).

begin(model(6342)).
bought(spaghetti).
shops(mary).
end(model(6342)).

begin(model(6343)).
bought(spaghetti).
shops(mary).
end(model(6343)).

begin(model(6344)).
end(model(6344)).

begin(model(6345)).
bought(fish).
shops(mary).
end(model(6345)).

begin(model(6346)).
bought(fish).
shops(mary).
end(model(6346)).

begin(model(6347)).
bought(fish).
shops(mary).
end(model(6347)).

begin(model(6348)).
bought(fish).
shops(mary).
end(model(6348)).

begin(model(6349)).
bought(fish).
shops(mary).
end(model(6349)).

begin(model(6350)).
bought(fish).
shops(mary).
end(model(6350)).

begin(model(6351)).
bought(spaghetti).
shops(mary).
end(model(6351)).

begin(model(6352)).
bought(fish).
shops(mary).
end(model(6352)).

begin(model(6353)).
bought(fish).
shops(mary).
end(model(6353)).

begin(model(6354)).
bought(fish).
shops(mary).
end(model(6354)).

begin(model(6355)).
bought(fish).
shops(mary).
end(model(6355)).

begin(model(6356)).
bought(fish).
shops(mary).
end(model(6356)).

begin(model(6357)).
bought(spaghetti).
shops(mary).
end(model(6357)).

begin(model(6358)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6358)).

begin(model(6359)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6359)).

begin(model(6360)).
end(model(6360)).

begin(model(6361)).
bought(fish).
shops(mary).
end(model(6361)).

begin(model(6362)).
bought(spaghetti).
shops(mary).
end(model(6362)).

begin(model(6363)).
bought(fish).
shops(mary).
end(model(6363)).

begin(model(6364)).
bought(fish).
shops(mary).
end(model(6364)).

begin(model(6365)).
bought(spaghetti).
shops(mary).
end(model(6365)).

begin(model(6366)).
bought(fish).
shops(mary).
end(model(6366)).

begin(model(6367)).
bought(fish).
shops(mary).
end(model(6367)).

begin(model(6368)).
end(model(6368)).

begin(model(6369)).
bought(fish).
shops(mary).
end(model(6369)).

begin(model(6370)).
end(model(6370)).

begin(model(6371)).
bought(spaghetti).
shops(mary).
end(model(6371)).

begin(model(6372)).
bought(spaghetti).
shops(mary).
end(model(6372)).

begin(model(6373)).
bought(fish).
shops(mary).
end(model(6373)).

begin(model(6374)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6374)).

begin(model(6375)).
bought(fish).
shops(mary).
end(model(6375)).

begin(model(6376)).
bought(spaghetti).
shops(mary).
end(model(6376)).

begin(model(6377)).
bought(fish).
shops(mary).
end(model(6377)).

begin(model(6378)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6378)).

begin(model(6379)).
bought(fish).
shops(mary).
end(model(6379)).

begin(model(6380)).
bought(fish).
shops(mary).
end(model(6380)).

begin(model(6381)).
bought(spaghetti).
shops(mary).
end(model(6381)).

begin(model(6382)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6382)).

begin(model(6383)).
bought(fish).
shops(mary).
end(model(6383)).

begin(model(6384)).
bought(fish).
shops(mary).
end(model(6384)).

begin(model(6385)).
bought(spaghetti).
shops(mary).
end(model(6385)).

begin(model(6386)).
bought(fish).
shops(mary).
end(model(6386)).

begin(model(6387)).
bought(fish).
shops(mary).
end(model(6387)).

begin(model(6388)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6388)).

begin(model(6389)).
bought(fish).
shops(mary).
end(model(6389)).

begin(model(6390)).
end(model(6390)).

begin(model(6391)).
bought(fish).
shops(mary).
end(model(6391)).

begin(model(6392)).
bought(fish).
shops(mary).
end(model(6392)).

begin(model(6393)).
bought(fish).
shops(mary).
end(model(6393)).

begin(model(6394)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6394)).

begin(model(6395)).
bought(fish).
shops(mary).
end(model(6395)).

begin(model(6396)).
bought(spaghetti).
shops(mary).
end(model(6396)).

begin(model(6397)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6397)).

begin(model(6398)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6398)).

begin(model(6399)).
bought(fish).
shops(mary).
end(model(6399)).

begin(model(6400)).
bought(fish).
shops(mary).
end(model(6400)).

begin(model(6401)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6401)).

begin(model(6402)).
bought(fish).
shops(mary).
end(model(6402)).

begin(model(6403)).
end(model(6403)).

begin(model(6404)).
bought(fish).
shops(mary).
end(model(6404)).

begin(model(6405)).
bought(fish).
shops(mary).
end(model(6405)).

begin(model(6406)).
bought(fish).
shops(mary).
end(model(6406)).

begin(model(6407)).
bought(fish).
shops(mary).
end(model(6407)).

begin(model(6408)).
end(model(6408)).

begin(model(6409)).
end(model(6409)).

begin(model(6410)).
bought(fish).
shops(mary).
end(model(6410)).

begin(model(6411)).
bought(fish).
shops(mary).
end(model(6411)).

begin(model(6412)).
bought(fish).
shops(mary).
end(model(6412)).

begin(model(6413)).
bought(fish).
shops(mary).
end(model(6413)).

begin(model(6414)).
end(model(6414)).

begin(model(6415)).
bought(fish).
shops(mary).
end(model(6415)).

begin(model(6416)).
bought(fish).
shops(mary).
end(model(6416)).

begin(model(6417)).
bought(fish).
shops(mary).
end(model(6417)).

begin(model(6418)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6418)).

begin(model(6419)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6419)).

begin(model(6420)).
bought(fish).
shops(mary).
end(model(6420)).

begin(model(6421)).
bought(fish).
shops(mary).
end(model(6421)).

begin(model(6422)).
bought(fish).
shops(mary).
end(model(6422)).

begin(model(6423)).
bought(spaghetti).
shops(mary).
end(model(6423)).

begin(model(6424)).
bought(fish).
shops(mary).
end(model(6424)).

begin(model(6425)).
bought(spaghetti).
shops(mary).
end(model(6425)).

begin(model(6426)).
bought(spaghetti).
shops(mary).
end(model(6426)).

begin(model(6427)).
bought(spaghetti).
shops(mary).
end(model(6427)).

begin(model(6428)).
bought(spaghetti).
shops(mary).
end(model(6428)).

begin(model(6429)).
bought(fish).
shops(mary).
end(model(6429)).

begin(model(6430)).
bought(fish).
shops(mary).
end(model(6430)).

begin(model(6431)).
bought(fish).
shops(mary).
end(model(6431)).

begin(model(6432)).
end(model(6432)).

begin(model(6433)).
bought(fish).
shops(mary).
end(model(6433)).

begin(model(6434)).
end(model(6434)).

begin(model(6435)).
bought(fish).
shops(mary).
end(model(6435)).

begin(model(6436)).
bought(spaghetti).
shops(mary).
end(model(6436)).

begin(model(6437)).
bought(fish).
shops(mary).
end(model(6437)).

begin(model(6438)).
bought(spaghetti).
shops(mary).
end(model(6438)).

begin(model(6439)).
bought(spaghetti).
shops(mary).
end(model(6439)).

begin(model(6440)).
bought(spaghetti).
shops(mary).
end(model(6440)).

begin(model(6441)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6441)).

begin(model(6442)).
end(model(6442)).

begin(model(6443)).
bought(fish).
shops(mary).
end(model(6443)).

begin(model(6444)).
bought(fish).
shops(mary).
end(model(6444)).

begin(model(6445)).
end(model(6445)).

begin(model(6446)).
bought(spaghetti).
shops(mary).
end(model(6446)).

begin(model(6447)).
end(model(6447)).

begin(model(6448)).
bought(fish).
shops(mary).
end(model(6448)).

begin(model(6449)).
bought(fish).
shops(mary).
end(model(6449)).

begin(model(6450)).
bought(fish).
shops(mary).
end(model(6450)).

begin(model(6451)).
bought(fish).
shops(mary).
end(model(6451)).

begin(model(6452)).
end(model(6452)).

begin(model(6453)).
bought(fish).
shops(mary).
end(model(6453)).

begin(model(6454)).
bought(fish).
shops(mary).
end(model(6454)).

begin(model(6455)).
bought(fish).
shops(mary).
end(model(6455)).

begin(model(6456)).
bought(fish).
shops(mary).
end(model(6456)).

begin(model(6457)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6457)).

begin(model(6458)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6458)).

begin(model(6459)).
bought(fish).
shops(mary).
end(model(6459)).

begin(model(6460)).
bought(spaghetti).
shops(mary).
end(model(6460)).

begin(model(6461)).
bought(fish).
shops(mary).
end(model(6461)).

begin(model(6462)).
end(model(6462)).

begin(model(6463)).
bought(spaghetti).
shops(mary).
end(model(6463)).

begin(model(6464)).
end(model(6464)).

begin(model(6465)).
bought(fish).
shops(mary).
end(model(6465)).

begin(model(6466)).
bought(fish).
shops(mary).
end(model(6466)).

begin(model(6467)).
bought(spaghetti).
shops(mary).
end(model(6467)).

begin(model(6468)).
bought(fish).
shops(mary).
end(model(6468)).

begin(model(6469)).
bought(fish).
shops(mary).
end(model(6469)).

begin(model(6470)).
bought(fish).
shops(mary).
end(model(6470)).

begin(model(6471)).
bought(fish).
shops(mary).
end(model(6471)).

begin(model(6472)).
bought(spaghetti).
shops(mary).
end(model(6472)).

begin(model(6473)).
bought(fish).
shops(mary).
end(model(6473)).

begin(model(6474)).
bought(fish).
shops(mary).
end(model(6474)).

begin(model(6475)).
bought(fish).
shops(mary).
end(model(6475)).

begin(model(6476)).
bought(fish).
shops(mary).
end(model(6476)).

begin(model(6477)).
bought(fish).
shops(mary).
end(model(6477)).

begin(model(6478)).
bought(fish).
shops(mary).
end(model(6478)).

begin(model(6479)).
bought(fish).
shops(mary).
end(model(6479)).

begin(model(6480)).
bought(fish).
shops(mary).
end(model(6480)).

begin(model(6481)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6481)).

begin(model(6482)).
end(model(6482)).

begin(model(6483)).
bought(fish).
shops(mary).
end(model(6483)).

begin(model(6484)).
bought(spaghetti).
shops(mary).
end(model(6484)).

begin(model(6485)).
bought(fish).
shops(mary).
end(model(6485)).

begin(model(6486)).
bought(fish).
shops(mary).
end(model(6486)).

begin(model(6487)).
end(model(6487)).

begin(model(6488)).
bought(fish).
shops(mary).
end(model(6488)).

begin(model(6489)).
bought(fish).
shops(mary).
end(model(6489)).

begin(model(6490)).
bought(fish).
shops(mary).
end(model(6490)).

begin(model(6491)).
bought(fish).
shops(mary).
end(model(6491)).

begin(model(6492)).
bought(fish).
shops(mary).
end(model(6492)).

begin(model(6493)).
bought(fish).
shops(mary).
end(model(6493)).

begin(model(6494)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6494)).

begin(model(6495)).
bought(spaghetti).
shops(mary).
end(model(6495)).

begin(model(6496)).
bought(fish).
shops(mary).
end(model(6496)).

begin(model(6497)).
end(model(6497)).

begin(model(6498)).
bought(fish).
shops(mary).
end(model(6498)).

begin(model(6499)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6499)).

begin(model(6500)).
bought(fish).
shops(mary).
end(model(6500)).

begin(model(6501)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6501)).

begin(model(6502)).
bought(spaghetti).
shops(mary).
end(model(6502)).

begin(model(6503)).
bought(fish).
shops(mary).
end(model(6503)).

begin(model(6504)).
bought(fish).
shops(mary).
end(model(6504)).

begin(model(6505)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6505)).

begin(model(6506)).
end(model(6506)).

begin(model(6507)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6507)).

begin(model(6508)).
end(model(6508)).

begin(model(6509)).
bought(fish).
shops(mary).
end(model(6509)).

begin(model(6510)).
bought(fish).
shops(mary).
end(model(6510)).

begin(model(6511)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6511)).

begin(model(6512)).
bought(spaghetti).
shops(mary).
end(model(6512)).

begin(model(6513)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6513)).

begin(model(6514)).
bought(fish).
shops(mary).
end(model(6514)).

begin(model(6515)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6515)).

begin(model(6516)).
bought(fish).
shops(mary).
end(model(6516)).

begin(model(6517)).
end(model(6517)).

begin(model(6518)).
bought(fish).
shops(mary).
end(model(6518)).

begin(model(6519)).
end(model(6519)).

begin(model(6520)).
bought(fish).
shops(mary).
end(model(6520)).

begin(model(6521)).
bought(fish).
shops(mary).
end(model(6521)).

begin(model(6522)).
bought(fish).
shops(mary).
end(model(6522)).

begin(model(6523)).
end(model(6523)).

begin(model(6524)).
bought(spaghetti).
shops(mary).
end(model(6524)).

begin(model(6525)).
end(model(6525)).

begin(model(6526)).
bought(spaghetti).
shops(mary).
end(model(6526)).

begin(model(6527)).
end(model(6527)).

begin(model(6528)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6528)).

begin(model(6529)).
bought(fish).
shops(mary).
end(model(6529)).

begin(model(6530)).
bought(fish).
shops(mary).
end(model(6530)).

begin(model(6531)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6531)).

begin(model(6532)).
bought(fish).
shops(mary).
end(model(6532)).

begin(model(6533)).
bought(fish).
shops(mary).
end(model(6533)).

begin(model(6534)).
bought(fish).
shops(mary).
end(model(6534)).

begin(model(6535)).
bought(fish).
shops(mary).
end(model(6535)).

begin(model(6536)).
bought(fish).
shops(mary).
end(model(6536)).

begin(model(6537)).
bought(spaghetti).
shops(mary).
end(model(6537)).

begin(model(6538)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6538)).

begin(model(6539)).
bought(fish).
shops(mary).
end(model(6539)).

begin(model(6540)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6540)).

begin(model(6541)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6541)).

begin(model(6542)).
bought(fish).
shops(mary).
end(model(6542)).

begin(model(6543)).
bought(spaghetti).
shops(mary).
end(model(6543)).

begin(model(6544)).
bought(spaghetti).
shops(mary).
end(model(6544)).

begin(model(6545)).
bought(fish).
shops(mary).
end(model(6545)).

begin(model(6546)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6546)).

begin(model(6547)).
bought(fish).
shops(mary).
end(model(6547)).

begin(model(6548)).
end(model(6548)).

begin(model(6549)).
bought(fish).
shops(mary).
end(model(6549)).

begin(model(6550)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6550)).

begin(model(6551)).
bought(spaghetti).
shops(john).
end(model(6551)).

begin(model(6552)).
bought(steak).
shops(john).
end(model(6552)).

begin(model(6553)).
bought(spaghetti).
shops(mary).
end(model(6553)).

begin(model(6554)).
bought(fish).
shops(mary).
end(model(6554)).

begin(model(6555)).
bought(spaghetti).
shops(mary).
end(model(6555)).

begin(model(6556)).
bought(fish).
shops(mary).
end(model(6556)).

begin(model(6557)).
end(model(6557)).

begin(model(6558)).
bought(fish).
shops(mary).
end(model(6558)).

begin(model(6559)).
bought(spaghetti).
shops(mary).
end(model(6559)).

begin(model(6560)).
bought(fish).
shops(mary).
end(model(6560)).

begin(model(6561)).
bought(fish).
shops(mary).
end(model(6561)).

begin(model(6562)).
bought(fish).
shops(mary).
end(model(6562)).

begin(model(6563)).
bought(fish).
shops(mary).
end(model(6563)).

begin(model(6564)).
bought(spaghetti).
shops(mary).
end(model(6564)).

begin(model(6565)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6565)).

begin(model(6566)).
bought(steak).
shops(john).
end(model(6566)).

begin(model(6567)).
bought(fish).
shops(mary).
end(model(6567)).

begin(model(6568)).
end(model(6568)).

begin(model(6569)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6569)).

begin(model(6570)).
bought(fish).
shops(mary).
end(model(6570)).

begin(model(6571)).
bought(spaghetti).
shops(mary).
end(model(6571)).

begin(model(6572)).
bought(fish).
shops(mary).
end(model(6572)).

begin(model(6573)).
end(model(6573)).

begin(model(6574)).
end(model(6574)).

begin(model(6575)).
bought(fish).
shops(mary).
end(model(6575)).

begin(model(6576)).
bought(fish).
shops(mary).
end(model(6576)).

begin(model(6577)).
bought(spaghetti).
shops(mary).
end(model(6577)).

begin(model(6578)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6578)).

begin(model(6579)).
bought(fish).
shops(mary).
end(model(6579)).

begin(model(6580)).
bought(fish).
shops(mary).
end(model(6580)).

begin(model(6581)).
bought(fish).
shops(mary).
end(model(6581)).

begin(model(6582)).
bought(fish).
shops(mary).
end(model(6582)).

begin(model(6583)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6583)).

begin(model(6584)).
end(model(6584)).

begin(model(6585)).
bought(spaghetti).
shops(mary).
end(model(6585)).

begin(model(6586)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6586)).

begin(model(6587)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6587)).

begin(model(6588)).
bought(spaghetti).
shops(mary).
end(model(6588)).

begin(model(6589)).
bought(spaghetti).
shops(mary).
end(model(6589)).

begin(model(6590)).
bought(fish).
shops(mary).
end(model(6590)).

begin(model(6591)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6591)).

begin(model(6592)).
bought(fish).
shops(mary).
end(model(6592)).

begin(model(6593)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6593)).

begin(model(6594)).
bought(spaghetti).
shops(mary).
end(model(6594)).

begin(model(6595)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6595)).

begin(model(6596)).
bought(fish).
shops(mary).
end(model(6596)).

begin(model(6597)).
bought(spaghetti).
shops(mary).
end(model(6597)).

begin(model(6598)).
bought(fish).
shops(mary).
end(model(6598)).

begin(model(6599)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6599)).

begin(model(6600)).
bought(fish).
shops(mary).
end(model(6600)).

begin(model(6601)).
bought(fish).
shops(mary).
end(model(6601)).

begin(model(6602)).
bought(fish).
shops(mary).
end(model(6602)).

begin(model(6603)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6603)).

begin(model(6604)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6604)).

begin(model(6605)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6605)).

begin(model(6606)).
bought(spaghetti).
shops(mary).
end(model(6606)).

begin(model(6607)).
bought(fish).
shops(mary).
end(model(6607)).

begin(model(6608)).
bought(spaghetti).
shops(mary).
end(model(6608)).

begin(model(6609)).
end(model(6609)).

begin(model(6610)).
bought(spaghetti).
shops(mary).
end(model(6610)).

begin(model(6611)).
bought(fish).
shops(mary).
end(model(6611)).

begin(model(6612)).
bought(fish).
shops(mary).
end(model(6612)).

begin(model(6613)).
bought(fish).
shops(mary).
end(model(6613)).

begin(model(6614)).
bought(spaghetti).
shops(mary).
end(model(6614)).

begin(model(6615)).
end(model(6615)).

begin(model(6616)).
bought(fish).
shops(mary).
end(model(6616)).

begin(model(6617)).
bought(fish).
shops(mary).
end(model(6617)).

begin(model(6618)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6618)).

begin(model(6619)).
bought(fish).
shops(mary).
end(model(6619)).

begin(model(6620)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6620)).

begin(model(6621)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6621)).

begin(model(6622)).
bought(fish).
shops(mary).
end(model(6622)).

begin(model(6623)).
bought(spaghetti).
shops(mary).
end(model(6623)).

begin(model(6624)).
bought(fish).
shops(mary).
end(model(6624)).

begin(model(6625)).
bought(spaghetti).
shops(john).
end(model(6625)).

begin(model(6626)).
bought(spaghetti).
shops(mary).
end(model(6626)).

begin(model(6627)).
bought(fish).
shops(mary).
end(model(6627)).

begin(model(6628)).
bought(spaghetti).
shops(mary).
end(model(6628)).

begin(model(6629)).
bought(fish).
shops(mary).
end(model(6629)).

begin(model(6630)).
bought(fish).
shops(mary).
end(model(6630)).

begin(model(6631)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6631)).

begin(model(6632)).
bought(spaghetti).
shops(mary).
end(model(6632)).

begin(model(6633)).
end(model(6633)).

begin(model(6634)).
bought(spaghetti).
shops(mary).
end(model(6634)).

begin(model(6635)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6635)).

begin(model(6636)).
bought(fish).
shops(mary).
end(model(6636)).

begin(model(6637)).
bought(spaghetti).
shops(mary).
end(model(6637)).

begin(model(6638)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6638)).

begin(model(6639)).
end(model(6639)).

begin(model(6640)).
bought(spaghetti).
shops(mary).
end(model(6640)).

begin(model(6641)).
bought(fish).
shops(mary).
end(model(6641)).

begin(model(6642)).
bought(fish).
shops(mary).
end(model(6642)).

begin(model(6643)).
end(model(6643)).

begin(model(6644)).
bought(fish).
shops(mary).
end(model(6644)).

begin(model(6645)).
bought(fish).
shops(mary).
end(model(6645)).

begin(model(6646)).
bought(fish).
shops(mary).
end(model(6646)).

begin(model(6647)).
bought(fish).
shops(mary).
end(model(6647)).

begin(model(6648)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6648)).

begin(model(6649)).
bought(spaghetti).
shops(john).
end(model(6649)).

begin(model(6650)).
bought(spaghetti).
shops(mary).
end(model(6650)).

begin(model(6651)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6651)).

begin(model(6652)).
bought(fish).
shops(mary).
end(model(6652)).

begin(model(6653)).
bought(spaghetti).
shops(mary).
end(model(6653)).

begin(model(6654)).
bought(fish).
shops(mary).
end(model(6654)).

begin(model(6655)).
bought(fish).
shops(mary).
end(model(6655)).

begin(model(6656)).
bought(fish).
shops(mary).
end(model(6656)).

begin(model(6657)).
bought(fish).
shops(mary).
end(model(6657)).

begin(model(6658)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6658)).

begin(model(6659)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6659)).

begin(model(6660)).
bought(fish).
shops(mary).
end(model(6660)).

begin(model(6661)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6661)).

begin(model(6662)).
bought(spaghetti).
shops(mary).
end(model(6662)).

begin(model(6663)).
bought(fish).
shops(mary).
end(model(6663)).

begin(model(6664)).
bought(spaghetti).
shops(mary).
end(model(6664)).

begin(model(6665)).
bought(fish).
shops(mary).
end(model(6665)).

begin(model(6666)).
bought(fish).
shops(mary).
end(model(6666)).

begin(model(6667)).
bought(fish).
shops(mary).
end(model(6667)).

begin(model(6668)).
bought(fish).
shops(mary).
end(model(6668)).

begin(model(6669)).
bought(fish).
shops(mary).
end(model(6669)).

begin(model(6670)).
bought(fish).
shops(mary).
end(model(6670)).

begin(model(6671)).
bought(fish).
shops(mary).
end(model(6671)).

begin(model(6672)).
bought(fish).
shops(mary).
end(model(6672)).

begin(model(6673)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6673)).

begin(model(6674)).
bought(fish).
shops(mary).
end(model(6674)).

begin(model(6675)).
bought(fish).
shops(mary).
end(model(6675)).

begin(model(6676)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6676)).

begin(model(6677)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6677)).

begin(model(6678)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6678)).

begin(model(6679)).
bought(spaghetti).
shops(mary).
end(model(6679)).

begin(model(6680)).
bought(spaghetti).
shops(mary).
end(model(6680)).

begin(model(6681)).
bought(fish).
shops(mary).
end(model(6681)).

begin(model(6682)).
bought(spaghetti).
shops(mary).
end(model(6682)).

begin(model(6683)).
bought(fish).
shops(mary).
end(model(6683)).

begin(model(6684)).
end(model(6684)).

begin(model(6685)).
bought(spaghetti).
shops(mary).
end(model(6685)).

begin(model(6686)).
bought(spaghetti).
shops(mary).
end(model(6686)).

begin(model(6687)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6687)).

begin(model(6688)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6688)).

begin(model(6689)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6689)).

begin(model(6690)).
bought(fish).
shops(mary).
end(model(6690)).

begin(model(6691)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6691)).

begin(model(6692)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6692)).

begin(model(6693)).
bought(fish).
shops(mary).
end(model(6693)).

begin(model(6694)).
bought(fish).
shops(mary).
end(model(6694)).

begin(model(6695)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6695)).

begin(model(6696)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6696)).

begin(model(6697)).
bought(spaghetti).
shops(mary).
end(model(6697)).

begin(model(6698)).
bought(spaghetti).
shops(john).
end(model(6698)).

begin(model(6699)).
bought(fish).
shops(mary).
end(model(6699)).

begin(model(6700)).
bought(spaghetti).
shops(mary).
end(model(6700)).

begin(model(6701)).
end(model(6701)).

begin(model(6702)).
bought(fish).
shops(mary).
end(model(6702)).

begin(model(6703)).
bought(fish).
shops(mary).
end(model(6703)).

begin(model(6704)).
bought(fish).
shops(mary).
end(model(6704)).

begin(model(6705)).
bought(fish).
shops(mary).
end(model(6705)).

begin(model(6706)).
bought(spaghetti).
shops(mary).
end(model(6706)).

begin(model(6707)).
bought(fish).
shops(mary).
end(model(6707)).

begin(model(6708)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6708)).

begin(model(6709)).
bought(spaghetti).
shops(mary).
end(model(6709)).

begin(model(6710)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6710)).

begin(model(6711)).
end(model(6711)).

begin(model(6712)).
bought(fish).
shops(mary).
end(model(6712)).

begin(model(6713)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6713)).

begin(model(6714)).
bought(spaghetti).
shops(mary).
end(model(6714)).

begin(model(6715)).
bought(fish).
shops(mary).
end(model(6715)).

begin(model(6716)).
bought(fish).
shops(mary).
end(model(6716)).

begin(model(6717)).
bought(fish).
shops(mary).
end(model(6717)).

begin(model(6718)).
end(model(6718)).

begin(model(6719)).
bought(fish).
shops(mary).
end(model(6719)).

begin(model(6720)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6720)).

begin(model(6721)).
bought(fish).
shops(mary).
end(model(6721)).

begin(model(6722)).
bought(fish).
shops(mary).
end(model(6722)).

begin(model(6723)).
bought(fish).
shops(mary).
end(model(6723)).

begin(model(6724)).
bought(fish).
shops(mary).
end(model(6724)).

begin(model(6725)).
end(model(6725)).

begin(model(6726)).
bought(spaghetti).
shops(mary).
end(model(6726)).

begin(model(6727)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6727)).

begin(model(6728)).
bought(fish).
shops(mary).
end(model(6728)).

begin(model(6729)).
bought(fish).
shops(mary).
end(model(6729)).

begin(model(6730)).
bought(fish).
shops(mary).
end(model(6730)).

begin(model(6731)).
bought(fish).
shops(mary).
end(model(6731)).

begin(model(6732)).
bought(fish).
shops(mary).
end(model(6732)).

begin(model(6733)).
end(model(6733)).

begin(model(6734)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6734)).

begin(model(6735)).
bought(spaghetti).
shops(mary).
end(model(6735)).

begin(model(6736)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6736)).

begin(model(6737)).
bought(fish).
shops(mary).
end(model(6737)).

begin(model(6738)).
bought(spaghetti).
shops(mary).
end(model(6738)).

begin(model(6739)).
bought(spaghetti).
shops(mary).
end(model(6739)).

begin(model(6740)).
bought(spaghetti).
shops(mary).
end(model(6740)).

begin(model(6741)).
bought(fish).
shops(mary).
end(model(6741)).

begin(model(6742)).
bought(fish).
shops(mary).
end(model(6742)).

begin(model(6743)).
bought(spaghetti).
shops(mary).
end(model(6743)).

begin(model(6744)).
bought(spaghetti).
shops(mary).
end(model(6744)).

begin(model(6745)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6745)).

begin(model(6746)).
bought(spaghetti).
shops(mary).
end(model(6746)).

begin(model(6747)).
bought(fish).
shops(mary).
end(model(6747)).

begin(model(6748)).
bought(fish).
shops(mary).
end(model(6748)).

begin(model(6749)).
bought(spaghetti).
shops(mary).
end(model(6749)).

begin(model(6750)).
bought(fish).
shops(mary).
end(model(6750)).

begin(model(6751)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6751)).

begin(model(6752)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6752)).

begin(model(6753)).
bought(fish).
shops(mary).
end(model(6753)).

begin(model(6754)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6754)).

begin(model(6755)).
end(model(6755)).

begin(model(6756)).
bought(spaghetti).
shops(mary).
end(model(6756)).

begin(model(6757)).
bought(fish).
shops(mary).
end(model(6757)).

begin(model(6758)).
bought(fish).
shops(mary).
end(model(6758)).

begin(model(6759)).
bought(fish).
shops(mary).
end(model(6759)).

begin(model(6760)).
bought(fish).
shops(mary).
end(model(6760)).

begin(model(6761)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6761)).

begin(model(6762)).
bought(fish).
shops(mary).
end(model(6762)).

begin(model(6763)).
end(model(6763)).

begin(model(6764)).
bought(fish).
shops(mary).
end(model(6764)).

begin(model(6765)).
bought(steak).
shops(john).
end(model(6765)).

begin(model(6766)).
end(model(6766)).

begin(model(6767)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6767)).

begin(model(6768)).
end(model(6768)).

begin(model(6769)).
bought(fish).
shops(mary).
end(model(6769)).

begin(model(6770)).
end(model(6770)).

begin(model(6771)).
bought(fish).
shops(mary).
end(model(6771)).

begin(model(6772)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6772)).

begin(model(6773)).
bought(fish).
shops(mary).
end(model(6773)).

begin(model(6774)).
bought(fish).
shops(mary).
end(model(6774)).

begin(model(6775)).
end(model(6775)).

begin(model(6776)).
bought(fish).
shops(mary).
end(model(6776)).

begin(model(6777)).
bought(fish).
shops(mary).
end(model(6777)).

begin(model(6778)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6778)).

begin(model(6779)).
bought(fish).
shops(mary).
end(model(6779)).

begin(model(6780)).
bought(fish).
shops(mary).
end(model(6780)).

begin(model(6781)).
bought(fish).
shops(mary).
end(model(6781)).

begin(model(6782)).
bought(fish).
shops(mary).
end(model(6782)).

begin(model(6783)).
bought(fish).
shops(mary).
end(model(6783)).

begin(model(6784)).
bought(fish).
shops(mary).
end(model(6784)).

begin(model(6785)).
bought(fish).
shops(mary).
end(model(6785)).

begin(model(6786)).
bought(spaghetti).
shops(mary).
end(model(6786)).

begin(model(6787)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6787)).

begin(model(6788)).
bought(fish).
shops(mary).
end(model(6788)).

begin(model(6789)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6789)).

begin(model(6790)).
bought(spaghetti).
shops(mary).
end(model(6790)).

begin(model(6791)).
bought(fish).
shops(mary).
end(model(6791)).

begin(model(6792)).
bought(fish).
shops(mary).
end(model(6792)).

begin(model(6793)).
bought(fish).
shops(mary).
end(model(6793)).

begin(model(6794)).
bought(fish).
shops(mary).
end(model(6794)).

begin(model(6795)).
bought(fish).
shops(mary).
end(model(6795)).

begin(model(6796)).
bought(spaghetti).
shops(mary).
end(model(6796)).

begin(model(6797)).
bought(fish).
shops(mary).
end(model(6797)).

begin(model(6798)).
bought(fish).
shops(mary).
end(model(6798)).

begin(model(6799)).
bought(fish).
shops(mary).
end(model(6799)).

begin(model(6800)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6800)).

begin(model(6801)).
bought(fish).
shops(mary).
end(model(6801)).

begin(model(6802)).
bought(fish).
shops(mary).
end(model(6802)).

begin(model(6803)).
bought(spaghetti).
shops(mary).
end(model(6803)).

begin(model(6804)).
bought(fish).
shops(mary).
end(model(6804)).

begin(model(6805)).
bought(fish).
shops(mary).
end(model(6805)).

begin(model(6806)).
bought(fish).
shops(mary).
end(model(6806)).

begin(model(6807)).
bought(fish).
shops(mary).
end(model(6807)).

begin(model(6808)).
bought(steak).
shops(john).
end(model(6808)).

begin(model(6809)).
bought(spaghetti).
shops(mary).
end(model(6809)).

begin(model(6810)).
bought(fish).
shops(mary).
end(model(6810)).

begin(model(6811)).
bought(spaghetti).
shops(mary).
end(model(6811)).

begin(model(6812)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6812)).

begin(model(6813)).
bought(spaghetti).
shops(mary).
end(model(6813)).

begin(model(6814)).
bought(fish).
shops(mary).
end(model(6814)).

begin(model(6815)).
bought(fish).
shops(mary).
end(model(6815)).

begin(model(6816)).
bought(fish).
shops(mary).
end(model(6816)).

begin(model(6817)).
bought(fish).
shops(mary).
end(model(6817)).

begin(model(6818)).
bought(fish).
shops(mary).
end(model(6818)).

begin(model(6819)).
end(model(6819)).

begin(model(6820)).
bought(fish).
shops(mary).
end(model(6820)).

begin(model(6821)).
bought(fish).
shops(mary).
end(model(6821)).

begin(model(6822)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6822)).

begin(model(6823)).
end(model(6823)).

begin(model(6824)).
bought(spaghetti).
shops(mary).
end(model(6824)).

begin(model(6825)).
bought(fish).
shops(mary).
end(model(6825)).

begin(model(6826)).
end(model(6826)).

begin(model(6827)).
bought(fish).
shops(mary).
end(model(6827)).

begin(model(6828)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6828)).

begin(model(6829)).
bought(fish).
shops(mary).
end(model(6829)).

begin(model(6830)).
bought(spaghetti).
shops(mary).
end(model(6830)).

begin(model(6831)).
bought(spaghetti).
shops(mary).
end(model(6831)).

begin(model(6832)).
bought(fish).
shops(mary).
end(model(6832)).

begin(model(6833)).
bought(fish).
shops(mary).
end(model(6833)).

begin(model(6834)).
bought(fish).
shops(mary).
end(model(6834)).

begin(model(6835)).
bought(fish).
shops(mary).
end(model(6835)).

begin(model(6836)).
bought(fish).
shops(mary).
end(model(6836)).

begin(model(6837)).
bought(fish).
shops(mary).
end(model(6837)).

begin(model(6838)).
bought(fish).
shops(mary).
end(model(6838)).

begin(model(6839)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6839)).

begin(model(6840)).
bought(fish).
shops(mary).
end(model(6840)).

begin(model(6841)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6841)).

begin(model(6842)).
bought(spaghetti).
shops(mary).
end(model(6842)).

begin(model(6843)).
bought(fish).
shops(mary).
end(model(6843)).

begin(model(6844)).
end(model(6844)).

begin(model(6845)).
bought(fish).
shops(mary).
end(model(6845)).

begin(model(6846)).
bought(fish).
shops(mary).
end(model(6846)).

begin(model(6847)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6847)).

begin(model(6848)).
bought(spaghetti).
shops(mary).
end(model(6848)).

begin(model(6849)).
bought(fish).
shops(mary).
end(model(6849)).

begin(model(6850)).
bought(spaghetti).
shops(john).
end(model(6850)).

begin(model(6851)).
bought(fish).
shops(mary).
end(model(6851)).

begin(model(6852)).
bought(fish).
shops(mary).
end(model(6852)).

begin(model(6853)).
bought(fish).
shops(mary).
end(model(6853)).

begin(model(6854)).
end(model(6854)).

begin(model(6855)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6855)).

begin(model(6856)).
end(model(6856)).

begin(model(6857)).
end(model(6857)).

begin(model(6858)).
bought(fish).
shops(mary).
end(model(6858)).

begin(model(6859)).
end(model(6859)).

begin(model(6860)).
bought(fish).
shops(mary).
end(model(6860)).

begin(model(6861)).
bought(fish).
shops(mary).
end(model(6861)).

begin(model(6862)).
bought(fish).
shops(mary).
end(model(6862)).

begin(model(6863)).
end(model(6863)).

begin(model(6864)).
bought(fish).
shops(mary).
end(model(6864)).

begin(model(6865)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6865)).

begin(model(6866)).
end(model(6866)).

begin(model(6867)).
bought(fish).
shops(mary).
end(model(6867)).

begin(model(6868)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6868)).

begin(model(6869)).
bought(fish).
shops(mary).
end(model(6869)).

begin(model(6870)).
bought(fish).
shops(mary).
end(model(6870)).

begin(model(6871)).
bought(spaghetti).
shops(mary).
end(model(6871)).

begin(model(6872)).
bought(fish).
shops(mary).
end(model(6872)).

begin(model(6873)).
bought(fish).
shops(mary).
end(model(6873)).

begin(model(6874)).
end(model(6874)).

begin(model(6875)).
bought(fish).
shops(mary).
end(model(6875)).

begin(model(6876)).
bought(fish).
shops(mary).
end(model(6876)).

begin(model(6877)).
bought(fish).
shops(mary).
end(model(6877)).

begin(model(6878)).
bought(fish).
shops(mary).
end(model(6878)).

begin(model(6879)).
bought(spaghetti).
shops(mary).
end(model(6879)).

begin(model(6880)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6880)).

begin(model(6881)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6881)).

begin(model(6882)).
bought(fish).
shops(mary).
end(model(6882)).

begin(model(6883)).
bought(fish).
shops(mary).
end(model(6883)).

begin(model(6884)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6884)).

begin(model(6885)).
bought(fish).
shops(mary).
end(model(6885)).

begin(model(6886)).
bought(spaghetti).
shops(mary).
end(model(6886)).

begin(model(6887)).
bought(fish).
shops(mary).
end(model(6887)).

begin(model(6888)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6888)).

begin(model(6889)).
bought(fish).
shops(mary).
end(model(6889)).

begin(model(6890)).
bought(spaghetti).
shops(mary).
end(model(6890)).

begin(model(6891)).
bought(spaghetti).
shops(mary).
end(model(6891)).

begin(model(6892)).
bought(fish).
shops(mary).
end(model(6892)).

begin(model(6893)).
bought(spaghetti).
shops(mary).
end(model(6893)).

begin(model(6894)).
bought(fish).
shops(mary).
end(model(6894)).

begin(model(6895)).
bought(fish).
shops(mary).
end(model(6895)).

begin(model(6896)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6896)).

begin(model(6897)).
bought(fish).
shops(mary).
end(model(6897)).

begin(model(6898)).
end(model(6898)).

begin(model(6899)).
bought(fish).
shops(mary).
end(model(6899)).

begin(model(6900)).
bought(fish).
shops(mary).
end(model(6900)).

begin(model(6901)).
bought(fish).
shops(mary).
end(model(6901)).

begin(model(6902)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6902)).

begin(model(6903)).
bought(fish).
shops(mary).
end(model(6903)).

begin(model(6904)).
bought(fish).
shops(mary).
end(model(6904)).

begin(model(6905)).
bought(spaghetti).
shops(mary).
end(model(6905)).

begin(model(6906)).
bought(fish).
shops(mary).
end(model(6906)).

begin(model(6907)).
bought(spaghetti).
shops(mary).
end(model(6907)).

begin(model(6908)).
bought(fish).
shops(mary).
end(model(6908)).

begin(model(6909)).
bought(spaghetti).
shops(mary).
end(model(6909)).

begin(model(6910)).
end(model(6910)).

begin(model(6911)).
bought(fish).
shops(mary).
end(model(6911)).

begin(model(6912)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6912)).

begin(model(6913)).
bought(fish).
shops(mary).
end(model(6913)).

begin(model(6914)).
bought(fish).
shops(mary).
end(model(6914)).

begin(model(6915)).
bought(spaghetti).
shops(mary).
end(model(6915)).

begin(model(6916)).
bought(fish).
shops(mary).
end(model(6916)).

begin(model(6917)).
end(model(6917)).

begin(model(6918)).
bought(spaghetti).
shops(mary).
end(model(6918)).

begin(model(6919)).
bought(fish).
shops(mary).
end(model(6919)).

begin(model(6920)).
bought(fish).
shops(mary).
end(model(6920)).

begin(model(6921)).
bought(fish).
shops(mary).
end(model(6921)).

begin(model(6922)).
bought(fish).
shops(mary).
end(model(6922)).

begin(model(6923)).
bought(fish).
shops(mary).
end(model(6923)).

begin(model(6924)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6924)).

begin(model(6925)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6925)).

begin(model(6926)).
bought(fish).
shops(mary).
end(model(6926)).

begin(model(6927)).
end(model(6927)).

begin(model(6928)).
bought(spaghetti).
shops(mary).
end(model(6928)).

begin(model(6929)).
bought(steak).
shops(john).
end(model(6929)).

begin(model(6930)).
bought(fish).
shops(mary).
end(model(6930)).

begin(model(6931)).
bought(spaghetti).
shops(mary).
end(model(6931)).

begin(model(6932)).
end(model(6932)).

begin(model(6933)).
bought(fish).
shops(mary).
end(model(6933)).

begin(model(6934)).
bought(spaghetti).
shops(mary).
end(model(6934)).

begin(model(6935)).
bought(fish).
shops(mary).
end(model(6935)).

begin(model(6936)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6936)).

begin(model(6937)).
bought(spaghetti).
shops(mary).
end(model(6937)).

begin(model(6938)).
bought(fish).
shops(mary).
end(model(6938)).

begin(model(6939)).
bought(fish).
shops(mary).
end(model(6939)).

begin(model(6940)).
end(model(6940)).

begin(model(6941)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6941)).

begin(model(6942)).
bought(fish).
shops(mary).
end(model(6942)).

begin(model(6943)).
bought(fish).
shops(mary).
end(model(6943)).

begin(model(6944)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6944)).

begin(model(6945)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6945)).

begin(model(6946)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6946)).

begin(model(6947)).
bought(spaghetti).
shops(mary).
end(model(6947)).

begin(model(6948)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6948)).

begin(model(6949)).
bought(spaghetti).
shops(mary).
end(model(6949)).

begin(model(6950)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6950)).

begin(model(6951)).
bought(fish).
shops(mary).
end(model(6951)).

begin(model(6952)).
bought(fish).
shops(mary).
end(model(6952)).

begin(model(6953)).
bought(fish).
shops(mary).
end(model(6953)).

begin(model(6954)).
end(model(6954)).

begin(model(6955)).
bought(fish).
shops(mary).
end(model(6955)).

begin(model(6956)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6956)).

begin(model(6957)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6957)).

begin(model(6958)).
bought(spaghetti).
shops(mary).
end(model(6958)).

begin(model(6959)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6959)).

begin(model(6960)).
bought(fish).
shops(mary).
end(model(6960)).

begin(model(6961)).
bought(fish).
shops(mary).
end(model(6961)).

begin(model(6962)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6962)).

begin(model(6963)).
bought(fish).
shops(mary).
end(model(6963)).

begin(model(6964)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(6964)).

begin(model(6965)).
end(model(6965)).

begin(model(6966)).
bought(spaghetti).
shops(mary).
end(model(6966)).

begin(model(6967)).
bought(fish).
shops(mary).
end(model(6967)).

begin(model(6968)).
bought(spaghetti).
shops(mary).
end(model(6968)).

begin(model(6969)).
bought(fish).
shops(mary).
end(model(6969)).

begin(model(6970)).
bought(fish).
shops(mary).
end(model(6970)).

begin(model(6971)).
bought(fish).
shops(mary).
end(model(6971)).

begin(model(6972)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6972)).

begin(model(6973)).
bought(fish).
shops(mary).
end(model(6973)).

begin(model(6974)).
bought(fish).
shops(mary).
end(model(6974)).

begin(model(6975)).
bought(spaghetti).
shops(mary).
end(model(6975)).

begin(model(6976)).
bought(spaghetti).
shops(mary).
end(model(6976)).

begin(model(6977)).
end(model(6977)).

begin(model(6978)).
bought(spaghetti).
shops(mary).
end(model(6978)).

begin(model(6979)).
bought(fish).
shops(mary).
end(model(6979)).

begin(model(6980)).
bought(fish).
shops(mary).
end(model(6980)).

begin(model(6981)).
bought(spaghetti).
shops(mary).
end(model(6981)).

begin(model(6982)).
bought(fish).
shops(mary).
end(model(6982)).

begin(model(6983)).
bought(fish).
shops(mary).
end(model(6983)).

begin(model(6984)).
bought(fish).
shops(mary).
end(model(6984)).

begin(model(6985)).
bought(spaghetti).
shops(mary).
end(model(6985)).

begin(model(6986)).
bought(spaghetti).
shops(mary).
end(model(6986)).

begin(model(6987)).
bought(fish).
shops(mary).
end(model(6987)).

begin(model(6988)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6988)).

begin(model(6989)).
bought(spaghetti).
shops(mary).
end(model(6989)).

begin(model(6990)).
bought(fish).
shops(mary).
end(model(6990)).

begin(model(6991)).
end(model(6991)).

begin(model(6992)).
bought(fish).
shops(mary).
end(model(6992)).

begin(model(6993)).
bought(spaghetti).
shops(mary).
end(model(6993)).

begin(model(6994)).
bought(spaghetti).
shops(mary).
end(model(6994)).

begin(model(6995)).
bought(spaghetti).
shops(mary).
end(model(6995)).

begin(model(6996)).
bought(spaghetti).
shops(mary).
end(model(6996)).

begin(model(6997)).
bought(fish).
shops(mary).
end(model(6997)).

begin(model(6998)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(6998)).

begin(model(6999)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(6999)).

begin(model(7000)).
bought(spaghetti).
shops(mary).
end(model(7000)).

begin(model(7001)).
bought(spaghetti).
shops(mary).
end(model(7001)).

begin(model(7002)).
bought(fish).
shops(mary).
end(model(7002)).

begin(model(7003)).
bought(fish).
shops(mary).
end(model(7003)).

begin(model(7004)).
bought(spaghetti).
shops(mary).
end(model(7004)).

begin(model(7005)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7005)).

begin(model(7006)).
end(model(7006)).

begin(model(7007)).
bought(fish).
shops(mary).
end(model(7007)).

begin(model(7008)).
bought(fish).
shops(mary).
end(model(7008)).

begin(model(7009)).
bought(fish).
shops(mary).
end(model(7009)).

begin(model(7010)).
end(model(7010)).

begin(model(7011)).
bought(spaghetti).
shops(mary).
end(model(7011)).

begin(model(7012)).
bought(fish).
shops(mary).
end(model(7012)).

begin(model(7013)).
bought(fish).
shops(mary).
end(model(7013)).

begin(model(7014)).
bought(spaghetti).
shops(mary).
end(model(7014)).

begin(model(7015)).
bought(fish).
shops(mary).
end(model(7015)).

begin(model(7016)).
bought(fish).
shops(mary).
end(model(7016)).

begin(model(7017)).
bought(fish).
shops(mary).
end(model(7017)).

begin(model(7018)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7018)).

begin(model(7019)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7019)).

begin(model(7020)).
bought(spaghetti).
shops(mary).
end(model(7020)).

begin(model(7021)).
bought(spaghetti).
shops(mary).
end(model(7021)).

begin(model(7022)).
bought(fish).
shops(mary).
end(model(7022)).

begin(model(7023)).
bought(fish).
shops(mary).
end(model(7023)).

begin(model(7024)).
bought(fish).
shops(mary).
end(model(7024)).

begin(model(7025)).
bought(fish).
shops(mary).
end(model(7025)).

begin(model(7026)).
bought(fish).
shops(mary).
end(model(7026)).

begin(model(7027)).
bought(spaghetti).
shops(mary).
end(model(7027)).

begin(model(7028)).
bought(spaghetti).
shops(mary).
end(model(7028)).

begin(model(7029)).
bought(fish).
shops(mary).
end(model(7029)).

begin(model(7030)).
bought(fish).
shops(mary).
end(model(7030)).

begin(model(7031)).
bought(spaghetti).
shops(mary).
end(model(7031)).

begin(model(7032)).
bought(fish).
shops(mary).
end(model(7032)).

begin(model(7033)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7033)).

begin(model(7034)).
bought(fish).
shops(mary).
end(model(7034)).

begin(model(7035)).
bought(spaghetti).
shops(mary).
end(model(7035)).

begin(model(7036)).
end(model(7036)).

begin(model(7037)).
bought(fish).
shops(mary).
end(model(7037)).

begin(model(7038)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7038)).

begin(model(7039)).
bought(fish).
shops(mary).
end(model(7039)).

begin(model(7040)).
bought(spaghetti).
shops(mary).
end(model(7040)).

begin(model(7041)).
end(model(7041)).

begin(model(7042)).
bought(spaghetti).
shops(mary).
end(model(7042)).

begin(model(7043)).
bought(spaghetti).
shops(mary).
end(model(7043)).

begin(model(7044)).
bought(fish).
shops(mary).
end(model(7044)).

begin(model(7045)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7045)).

begin(model(7046)).
bought(spaghetti).
shops(mary).
end(model(7046)).

begin(model(7047)).
bought(spaghetti).
shops(mary).
end(model(7047)).

begin(model(7048)).
bought(fish).
shops(mary).
end(model(7048)).

begin(model(7049)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7049)).

begin(model(7050)).
bought(fish).
shops(mary).
end(model(7050)).

begin(model(7051)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7051)).

begin(model(7052)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7052)).

begin(model(7053)).
end(model(7053)).

begin(model(7054)).
bought(fish).
shops(mary).
end(model(7054)).

begin(model(7055)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7055)).

begin(model(7056)).
bought(fish).
shops(mary).
end(model(7056)).

begin(model(7057)).
bought(spaghetti).
shops(mary).
end(model(7057)).

begin(model(7058)).
bought(fish).
shops(mary).
end(model(7058)).

begin(model(7059)).
bought(fish).
shops(mary).
end(model(7059)).

begin(model(7060)).
bought(fish).
shops(mary).
end(model(7060)).

begin(model(7061)).
bought(fish).
shops(mary).
end(model(7061)).

begin(model(7062)).
bought(fish).
shops(mary).
end(model(7062)).

begin(model(7063)).
bought(fish).
shops(mary).
end(model(7063)).

begin(model(7064)).
bought(fish).
shops(mary).
end(model(7064)).

begin(model(7065)).
bought(fish).
shops(mary).
end(model(7065)).

begin(model(7066)).
end(model(7066)).

begin(model(7067)).
bought(fish).
shops(mary).
end(model(7067)).

begin(model(7068)).
bought(fish).
shops(mary).
end(model(7068)).

begin(model(7069)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7069)).

begin(model(7070)).
bought(fish).
shops(mary).
end(model(7070)).

begin(model(7071)).
bought(spaghetti).
shops(mary).
end(model(7071)).

begin(model(7072)).
end(model(7072)).

begin(model(7073)).
bought(spaghetti).
shops(john).
end(model(7073)).

begin(model(7074)).
bought(spaghetti).
shops(mary).
end(model(7074)).

begin(model(7075)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7075)).

begin(model(7076)).
bought(fish).
shops(mary).
end(model(7076)).

begin(model(7077)).
bought(fish).
shops(mary).
end(model(7077)).

begin(model(7078)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7078)).

begin(model(7079)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7079)).

begin(model(7080)).
bought(spaghetti).
shops(mary).
end(model(7080)).

begin(model(7081)).
bought(fish).
shops(mary).
end(model(7081)).

begin(model(7082)).
bought(spaghetti).
shops(mary).
end(model(7082)).

begin(model(7083)).
bought(spaghetti).
shops(mary).
end(model(7083)).

begin(model(7084)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7084)).

begin(model(7085)).
end(model(7085)).

begin(model(7086)).
bought(fish).
shops(mary).
end(model(7086)).

begin(model(7087)).
bought(fish).
shops(mary).
end(model(7087)).

begin(model(7088)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7088)).

begin(model(7089)).
bought(fish).
shops(mary).
end(model(7089)).

begin(model(7090)).
bought(fish).
shops(mary).
end(model(7090)).

begin(model(7091)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7091)).

begin(model(7092)).
bought(spaghetti).
shops(mary).
end(model(7092)).

begin(model(7093)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7093)).

begin(model(7094)).
bought(fish).
shops(mary).
end(model(7094)).

begin(model(7095)).
bought(fish).
shops(mary).
end(model(7095)).

begin(model(7096)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7096)).

begin(model(7097)).
bought(fish).
shops(mary).
end(model(7097)).

begin(model(7098)).
bought(fish).
shops(mary).
end(model(7098)).

begin(model(7099)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7099)).

begin(model(7100)).
bought(spaghetti).
shops(mary).
end(model(7100)).

begin(model(7101)).
bought(spaghetti).
shops(mary).
end(model(7101)).

begin(model(7102)).
end(model(7102)).

begin(model(7103)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7103)).

begin(model(7104)).
bought(spaghetti).
shops(mary).
end(model(7104)).

begin(model(7105)).
bought(spaghetti).
shops(mary).
end(model(7105)).

begin(model(7106)).
bought(fish).
shops(mary).
end(model(7106)).

begin(model(7107)).
bought(fish).
shops(mary).
end(model(7107)).

begin(model(7108)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7108)).

begin(model(7109)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7109)).

begin(model(7110)).
bought(fish).
shops(mary).
end(model(7110)).

begin(model(7111)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7111)).

begin(model(7112)).
bought(spaghetti).
shops(mary).
end(model(7112)).

begin(model(7113)).
bought(fish).
shops(mary).
end(model(7113)).

begin(model(7114)).
bought(spaghetti).
shops(mary).
end(model(7114)).

begin(model(7115)).
bought(spaghetti).
shops(mary).
end(model(7115)).

begin(model(7116)).
bought(spaghetti).
shops(mary).
end(model(7116)).

begin(model(7117)).
bought(fish).
shops(mary).
end(model(7117)).

begin(model(7118)).
bought(fish).
shops(mary).
end(model(7118)).

begin(model(7119)).
bought(spaghetti).
shops(mary).
end(model(7119)).

begin(model(7120)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7120)).

begin(model(7121)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7121)).

begin(model(7122)).
bought(spaghetti).
shops(mary).
end(model(7122)).

begin(model(7123)).
end(model(7123)).

begin(model(7124)).
end(model(7124)).

begin(model(7125)).
bought(fish).
shops(mary).
end(model(7125)).

begin(model(7126)).
bought(fish).
shops(mary).
end(model(7126)).

begin(model(7127)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7127)).

begin(model(7128)).
bought(fish).
shops(mary).
end(model(7128)).

begin(model(7129)).
bought(fish).
shops(mary).
end(model(7129)).

begin(model(7130)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7130)).

begin(model(7131)).
bought(fish).
shops(mary).
end(model(7131)).

begin(model(7132)).
end(model(7132)).

begin(model(7133)).
bought(fish).
shops(mary).
end(model(7133)).

begin(model(7134)).
bought(fish).
shops(mary).
end(model(7134)).

begin(model(7135)).
bought(spaghetti).
shops(mary).
end(model(7135)).

begin(model(7136)).
bought(spaghetti).
shops(mary).
end(model(7136)).

begin(model(7137)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7137)).

begin(model(7138)).
bought(fish).
shops(mary).
end(model(7138)).

begin(model(7139)).
bought(spaghetti).
shops(mary).
end(model(7139)).

begin(model(7140)).
bought(fish).
shops(mary).
end(model(7140)).

begin(model(7141)).
bought(spaghetti).
shops(mary).
end(model(7141)).

begin(model(7142)).
bought(fish).
shops(mary).
end(model(7142)).

begin(model(7143)).
bought(fish).
shops(mary).
end(model(7143)).

begin(model(7144)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7144)).

begin(model(7145)).
bought(fish).
shops(mary).
end(model(7145)).

begin(model(7146)).
bought(fish).
shops(mary).
end(model(7146)).

begin(model(7147)).
bought(spaghetti).
shops(mary).
end(model(7147)).

begin(model(7148)).
bought(fish).
shops(mary).
end(model(7148)).

begin(model(7149)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7149)).

begin(model(7150)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7150)).

begin(model(7151)).
bought(fish).
shops(mary).
end(model(7151)).

begin(model(7152)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7152)).

begin(model(7153)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7153)).

begin(model(7154)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7154)).

begin(model(7155)).
bought(spaghetti).
shops(mary).
end(model(7155)).

begin(model(7156)).
bought(fish).
shops(mary).
end(model(7156)).

begin(model(7157)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7157)).

begin(model(7158)).
bought(fish).
shops(mary).
end(model(7158)).

begin(model(7159)).
bought(spaghetti).
shops(mary).
end(model(7159)).

begin(model(7160)).
bought(fish).
shops(mary).
end(model(7160)).

begin(model(7161)).
bought(spaghetti).
shops(mary).
end(model(7161)).

begin(model(7162)).
bought(spaghetti).
shops(mary).
end(model(7162)).

begin(model(7163)).
bought(spaghetti).
shops(mary).
end(model(7163)).

begin(model(7164)).
bought(fish).
shops(mary).
end(model(7164)).

begin(model(7165)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7165)).

begin(model(7166)).
end(model(7166)).

begin(model(7167)).
bought(fish).
shops(mary).
end(model(7167)).

begin(model(7168)).
bought(fish).
shops(mary).
end(model(7168)).

begin(model(7169)).
bought(fish).
shops(mary).
end(model(7169)).

begin(model(7170)).
bought(spaghetti).
shops(john).
end(model(7170)).

begin(model(7171)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7171)).

begin(model(7172)).
end(model(7172)).

begin(model(7173)).
bought(fish).
shops(mary).
end(model(7173)).

begin(model(7174)).
bought(spaghetti).
shops(mary).
end(model(7174)).

begin(model(7175)).
bought(fish).
shops(mary).
end(model(7175)).

begin(model(7176)).
bought(spaghetti).
shops(mary).
end(model(7176)).

begin(model(7177)).
bought(fish).
shops(mary).
end(model(7177)).

begin(model(7178)).
bought(spaghetti).
shops(mary).
end(model(7178)).

begin(model(7179)).
bought(spaghetti).
shops(mary).
end(model(7179)).

begin(model(7180)).
bought(fish).
shops(mary).
end(model(7180)).

begin(model(7181)).
bought(fish).
shops(mary).
end(model(7181)).

begin(model(7182)).
bought(steak).
shops(john).
end(model(7182)).

begin(model(7183)).
end(model(7183)).

begin(model(7184)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7184)).

begin(model(7185)).
bought(fish).
shops(mary).
end(model(7185)).

begin(model(7186)).
bought(fish).
shops(mary).
end(model(7186)).

begin(model(7187)).
bought(fish).
shops(mary).
end(model(7187)).

begin(model(7188)).
bought(spaghetti).
shops(mary).
end(model(7188)).

begin(model(7189)).
bought(fish).
shops(mary).
end(model(7189)).

begin(model(7190)).
bought(spaghetti).
shops(mary).
end(model(7190)).

begin(model(7191)).
bought(fish).
shops(mary).
end(model(7191)).

begin(model(7192)).
bought(fish).
shops(mary).
end(model(7192)).

begin(model(7193)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7193)).

begin(model(7194)).
bought(spaghetti).
shops(mary).
end(model(7194)).

begin(model(7195)).
bought(spaghetti).
shops(mary).
end(model(7195)).

begin(model(7196)).
bought(fish).
shops(mary).
end(model(7196)).

begin(model(7197)).
bought(spaghetti).
shops(mary).
end(model(7197)).

begin(model(7198)).
bought(fish).
shops(mary).
end(model(7198)).

begin(model(7199)).
bought(fish).
shops(mary).
end(model(7199)).

begin(model(7200)).
bought(fish).
shops(mary).
end(model(7200)).

begin(model(7201)).
bought(spaghetti).
shops(mary).
end(model(7201)).

begin(model(7202)).
bought(fish).
shops(mary).
end(model(7202)).

begin(model(7203)).
bought(fish).
shops(mary).
end(model(7203)).

begin(model(7204)).
bought(fish).
shops(mary).
end(model(7204)).

begin(model(7205)).
bought(fish).
shops(mary).
end(model(7205)).

begin(model(7206)).
bought(spaghetti).
shops(mary).
end(model(7206)).

begin(model(7207)).
bought(fish).
shops(mary).
end(model(7207)).

begin(model(7208)).
bought(fish).
shops(mary).
end(model(7208)).

begin(model(7209)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7209)).

begin(model(7210)).
bought(spaghetti).
shops(mary).
end(model(7210)).

begin(model(7211)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7211)).

begin(model(7212)).
bought(spaghetti).
shops(mary).
end(model(7212)).

begin(model(7213)).
bought(steak).
shops(john).
end(model(7213)).

begin(model(7214)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7214)).

begin(model(7215)).
bought(spaghetti).
shops(mary).
end(model(7215)).

begin(model(7216)).
bought(fish).
shops(mary).
end(model(7216)).

begin(model(7217)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7217)).

begin(model(7218)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7218)).

begin(model(7219)).
bought(fish).
shops(mary).
end(model(7219)).

begin(model(7220)).
bought(fish).
shops(mary).
end(model(7220)).

begin(model(7221)).
bought(spaghetti).
shops(mary).
end(model(7221)).

begin(model(7222)).
bought(fish).
shops(mary).
end(model(7222)).

begin(model(7223)).
bought(fish).
shops(mary).
end(model(7223)).

begin(model(7224)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7224)).

begin(model(7225)).
bought(spaghetti).
shops(mary).
end(model(7225)).

begin(model(7226)).
bought(spaghetti).
shops(mary).
end(model(7226)).

begin(model(7227)).
end(model(7227)).

begin(model(7228)).
bought(fish).
shops(mary).
end(model(7228)).

begin(model(7229)).
bought(fish).
shops(mary).
end(model(7229)).

begin(model(7230)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7230)).

begin(model(7231)).
bought(fish).
shops(mary).
end(model(7231)).

begin(model(7232)).
bought(spaghetti).
shops(mary).
end(model(7232)).

begin(model(7233)).
bought(fish).
shops(mary).
end(model(7233)).

begin(model(7234)).
end(model(7234)).

begin(model(7235)).
bought(fish).
shops(mary).
end(model(7235)).

begin(model(7236)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7236)).

begin(model(7237)).
bought(spaghetti).
shops(mary).
end(model(7237)).

begin(model(7238)).
bought(fish).
shops(mary).
end(model(7238)).

begin(model(7239)).
end(model(7239)).

begin(model(7240)).
bought(fish).
shops(mary).
end(model(7240)).

begin(model(7241)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7241)).

begin(model(7242)).
bought(fish).
shops(mary).
end(model(7242)).

begin(model(7243)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7243)).

begin(model(7244)).
bought(fish).
shops(mary).
end(model(7244)).

begin(model(7245)).
end(model(7245)).

begin(model(7246)).
bought(fish).
shops(mary).
end(model(7246)).

begin(model(7247)).
bought(spaghetti).
shops(mary).
end(model(7247)).

begin(model(7248)).
bought(fish).
shops(mary).
end(model(7248)).

begin(model(7249)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7249)).

begin(model(7250)).
end(model(7250)).

begin(model(7251)).
bought(spaghetti).
shops(mary).
end(model(7251)).

begin(model(7252)).
end(model(7252)).

begin(model(7253)).
bought(fish).
shops(mary).
end(model(7253)).

begin(model(7254)).
bought(fish).
shops(mary).
end(model(7254)).

begin(model(7255)).
bought(fish).
shops(mary).
end(model(7255)).

begin(model(7256)).
bought(fish).
shops(mary).
end(model(7256)).

begin(model(7257)).
bought(fish).
shops(mary).
end(model(7257)).

begin(model(7258)).
bought(spaghetti).
shops(mary).
end(model(7258)).

begin(model(7259)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7259)).

begin(model(7260)).
bought(spaghetti).
shops(john).
end(model(7260)).

begin(model(7261)).
end(model(7261)).

begin(model(7262)).
bought(fish).
shops(mary).
end(model(7262)).

begin(model(7263)).
bought(fish).
shops(mary).
end(model(7263)).

begin(model(7264)).
bought(spaghetti).
shops(mary).
end(model(7264)).

begin(model(7265)).
bought(fish).
shops(mary).
end(model(7265)).

begin(model(7266)).
bought(spaghetti).
shops(mary).
end(model(7266)).

begin(model(7267)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7267)).

begin(model(7268)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7268)).

begin(model(7269)).
bought(fish).
shops(mary).
end(model(7269)).

begin(model(7270)).
bought(fish).
shops(mary).
end(model(7270)).

begin(model(7271)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7271)).

begin(model(7272)).
bought(fish).
shops(mary).
end(model(7272)).

begin(model(7273)).
bought(fish).
shops(mary).
end(model(7273)).

begin(model(7274)).
bought(fish).
shops(mary).
end(model(7274)).

begin(model(7275)).
bought(spaghetti).
shops(mary).
end(model(7275)).

begin(model(7276)).
bought(fish).
shops(mary).
end(model(7276)).

begin(model(7277)).
bought(fish).
shops(mary).
end(model(7277)).

begin(model(7278)).
bought(fish).
shops(mary).
end(model(7278)).

begin(model(7279)).
bought(fish).
shops(mary).
end(model(7279)).

begin(model(7280)).
bought(fish).
shops(mary).
end(model(7280)).

begin(model(7281)).
bought(fish).
shops(mary).
end(model(7281)).

begin(model(7282)).
bought(fish).
shops(mary).
end(model(7282)).

begin(model(7283)).
bought(fish).
shops(mary).
end(model(7283)).

begin(model(7284)).
bought(spaghetti).
shops(mary).
end(model(7284)).

begin(model(7285)).
bought(fish).
shops(mary).
end(model(7285)).

begin(model(7286)).
bought(fish).
shops(mary).
end(model(7286)).

begin(model(7287)).
bought(fish).
shops(mary).
end(model(7287)).

begin(model(7288)).
bought(fish).
shops(mary).
end(model(7288)).

begin(model(7289)).
bought(fish).
shops(mary).
end(model(7289)).

begin(model(7290)).
bought(fish).
shops(mary).
end(model(7290)).

begin(model(7291)).
bought(fish).
shops(mary).
end(model(7291)).

begin(model(7292)).
bought(fish).
shops(mary).
end(model(7292)).

begin(model(7293)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7293)).

begin(model(7294)).
bought(fish).
shops(mary).
end(model(7294)).

begin(model(7295)).
bought(fish).
shops(mary).
end(model(7295)).

begin(model(7296)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7296)).

begin(model(7297)).
bought(spaghetti).
shops(mary).
end(model(7297)).

begin(model(7298)).
bought(fish).
shops(mary).
end(model(7298)).

begin(model(7299)).
bought(fish).
shops(mary).
end(model(7299)).

begin(model(7300)).
bought(spaghetti).
shops(mary).
end(model(7300)).

begin(model(7301)).
end(model(7301)).

begin(model(7302)).
bought(spaghetti).
shops(mary).
end(model(7302)).

begin(model(7303)).
bought(fish).
shops(mary).
end(model(7303)).

begin(model(7304)).
bought(fish).
shops(mary).
end(model(7304)).

begin(model(7305)).
bought(fish).
shops(mary).
end(model(7305)).

begin(model(7306)).
bought(spaghetti).
shops(mary).
end(model(7306)).

begin(model(7307)).
bought(fish).
shops(mary).
end(model(7307)).

begin(model(7308)).
end(model(7308)).

begin(model(7309)).
bought(fish).
shops(mary).
end(model(7309)).

begin(model(7310)).
bought(fish).
shops(mary).
end(model(7310)).

begin(model(7311)).
bought(spaghetti).
shops(mary).
end(model(7311)).

begin(model(7312)).
bought(fish).
shops(mary).
end(model(7312)).

begin(model(7313)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7313)).

begin(model(7314)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7314)).

begin(model(7315)).
bought(spaghetti).
shops(mary).
end(model(7315)).

begin(model(7316)).
bought(fish).
shops(mary).
end(model(7316)).

begin(model(7317)).
bought(spaghetti).
shops(john).
end(model(7317)).

begin(model(7318)).
bought(fish).
shops(mary).
end(model(7318)).

begin(model(7319)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7319)).

begin(model(7320)).
end(model(7320)).

begin(model(7321)).
bought(fish).
shops(mary).
end(model(7321)).

begin(model(7322)).
bought(spaghetti).
shops(mary).
end(model(7322)).

begin(model(7323)).
bought(fish).
shops(mary).
end(model(7323)).

begin(model(7324)).
bought(fish).
shops(mary).
end(model(7324)).

begin(model(7325)).
bought(fish).
shops(mary).
end(model(7325)).

begin(model(7326)).
bought(fish).
shops(mary).
end(model(7326)).

begin(model(7327)).
bought(fish).
shops(mary).
end(model(7327)).

begin(model(7328)).
bought(spaghetti).
shops(mary).
end(model(7328)).

begin(model(7329)).
bought(fish).
shops(mary).
end(model(7329)).

begin(model(7330)).
end(model(7330)).

begin(model(7331)).
bought(fish).
shops(mary).
end(model(7331)).

begin(model(7332)).
bought(fish).
shops(mary).
end(model(7332)).

begin(model(7333)).
bought(fish).
shops(mary).
end(model(7333)).

begin(model(7334)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7334)).

begin(model(7335)).
bought(fish).
shops(mary).
end(model(7335)).

begin(model(7336)).
bought(fish).
shops(mary).
end(model(7336)).

begin(model(7337)).
bought(spaghetti).
shops(mary).
end(model(7337)).

begin(model(7338)).
end(model(7338)).

begin(model(7339)).
end(model(7339)).

begin(model(7340)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7340)).

begin(model(7341)).
bought(spaghetti).
shops(mary).
end(model(7341)).

begin(model(7342)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7342)).

begin(model(7343)).
end(model(7343)).

begin(model(7344)).
bought(fish).
shops(mary).
end(model(7344)).

begin(model(7345)).
bought(fish).
shops(mary).
end(model(7345)).

begin(model(7346)).
bought(spaghetti).
shops(mary).
end(model(7346)).

begin(model(7347)).
bought(fish).
shops(mary).
end(model(7347)).

begin(model(7348)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7348)).

begin(model(7349)).
bought(fish).
shops(mary).
end(model(7349)).

begin(model(7350)).
bought(fish).
shops(mary).
end(model(7350)).

begin(model(7351)).
bought(fish).
shops(mary).
end(model(7351)).

begin(model(7352)).
bought(fish).
shops(mary).
end(model(7352)).

begin(model(7353)).
bought(fish).
shops(mary).
end(model(7353)).

begin(model(7354)).
bought(fish).
shops(mary).
end(model(7354)).

begin(model(7355)).
bought(fish).
shops(mary).
end(model(7355)).

begin(model(7356)).
bought(fish).
shops(mary).
end(model(7356)).

begin(model(7357)).
bought(fish).
shops(mary).
end(model(7357)).

begin(model(7358)).
bought(fish).
shops(mary).
end(model(7358)).

begin(model(7359)).
bought(fish).
shops(mary).
end(model(7359)).

begin(model(7360)).
bought(spaghetti).
shops(mary).
end(model(7360)).

begin(model(7361)).
bought(fish).
shops(mary).
end(model(7361)).

begin(model(7362)).
bought(spaghetti).
shops(mary).
end(model(7362)).

begin(model(7363)).
bought(spaghetti).
shops(mary).
end(model(7363)).

begin(model(7364)).
bought(fish).
shops(mary).
end(model(7364)).

begin(model(7365)).
bought(fish).
shops(mary).
end(model(7365)).

begin(model(7366)).
bought(fish).
shops(mary).
end(model(7366)).

begin(model(7367)).
bought(fish).
shops(mary).
end(model(7367)).

begin(model(7368)).
bought(fish).
shops(mary).
end(model(7368)).

begin(model(7369)).
bought(fish).
shops(mary).
end(model(7369)).

begin(model(7370)).
bought(spaghetti).
shops(mary).
end(model(7370)).

begin(model(7371)).
end(model(7371)).

begin(model(7372)).
bought(spaghetti).
shops(mary).
end(model(7372)).

begin(model(7373)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7373)).

begin(model(7374)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7374)).

begin(model(7375)).
bought(spaghetti).
shops(mary).
end(model(7375)).

begin(model(7376)).
bought(fish).
shops(mary).
end(model(7376)).

begin(model(7377)).
bought(spaghetti).
shops(mary).
end(model(7377)).

begin(model(7378)).
bought(fish).
shops(mary).
end(model(7378)).

begin(model(7379)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7379)).

begin(model(7380)).
bought(spaghetti).
shops(mary).
end(model(7380)).

begin(model(7381)).
bought(spaghetti).
shops(mary).
end(model(7381)).

begin(model(7382)).
bought(fish).
shops(mary).
end(model(7382)).

begin(model(7383)).
end(model(7383)).

begin(model(7384)).
bought(spaghetti).
shops(mary).
end(model(7384)).

begin(model(7385)).
bought(fish).
shops(mary).
end(model(7385)).

begin(model(7386)).
end(model(7386)).

begin(model(7387)).
bought(fish).
shops(mary).
end(model(7387)).

begin(model(7388)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7388)).

begin(model(7389)).
end(model(7389)).

begin(model(7390)).
bought(fish).
shops(mary).
end(model(7390)).

begin(model(7391)).
bought(fish).
shops(mary).
end(model(7391)).

begin(model(7392)).
bought(fish).
shops(mary).
end(model(7392)).

begin(model(7393)).
bought(fish).
shops(mary).
end(model(7393)).

begin(model(7394)).
bought(fish).
shops(mary).
end(model(7394)).

begin(model(7395)).
bought(fish).
shops(mary).
end(model(7395)).

begin(model(7396)).
bought(fish).
shops(mary).
end(model(7396)).

begin(model(7397)).
bought(spaghetti).
shops(mary).
end(model(7397)).

begin(model(7398)).
bought(fish).
shops(mary).
end(model(7398)).

begin(model(7399)).
end(model(7399)).

begin(model(7400)).
end(model(7400)).

begin(model(7401)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7401)).

begin(model(7402)).
bought(fish).
shops(mary).
end(model(7402)).

begin(model(7403)).
bought(fish).
shops(mary).
end(model(7403)).

begin(model(7404)).
bought(fish).
shops(mary).
end(model(7404)).

begin(model(7405)).
bought(fish).
shops(mary).
end(model(7405)).

begin(model(7406)).
bought(fish).
shops(mary).
end(model(7406)).

begin(model(7407)).
bought(fish).
shops(mary).
end(model(7407)).

begin(model(7408)).
bought(fish).
shops(mary).
end(model(7408)).

begin(model(7409)).
bought(fish).
shops(mary).
end(model(7409)).

begin(model(7410)).
end(model(7410)).

begin(model(7411)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7411)).

begin(model(7412)).
bought(spaghetti).
shops(mary).
end(model(7412)).

begin(model(7413)).
bought(spaghetti).
shops(mary).
end(model(7413)).

begin(model(7414)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7414)).

begin(model(7415)).
bought(spaghetti).
shops(mary).
end(model(7415)).

begin(model(7416)).
bought(fish).
shops(mary).
end(model(7416)).

begin(model(7417)).
bought(fish).
shops(mary).
end(model(7417)).

begin(model(7418)).
bought(fish).
shops(mary).
end(model(7418)).

begin(model(7419)).
end(model(7419)).

begin(model(7420)).
bought(fish).
shops(mary).
end(model(7420)).

begin(model(7421)).
bought(spaghetti).
shops(john).
end(model(7421)).

begin(model(7422)).
bought(spaghetti).
shops(mary).
end(model(7422)).

begin(model(7423)).
bought(fish).
shops(mary).
end(model(7423)).

begin(model(7424)).
end(model(7424)).

begin(model(7425)).
bought(fish).
shops(mary).
end(model(7425)).

begin(model(7426)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7426)).

begin(model(7427)).
bought(fish).
shops(mary).
end(model(7427)).

begin(model(7428)).
bought(spaghetti).
shops(mary).
end(model(7428)).

begin(model(7429)).
end(model(7429)).

begin(model(7430)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7430)).

begin(model(7431)).
bought(fish).
shops(mary).
end(model(7431)).

begin(model(7432)).
bought(fish).
shops(mary).
end(model(7432)).

begin(model(7433)).
bought(fish).
shops(mary).
end(model(7433)).

begin(model(7434)).
bought(spaghetti).
shops(mary).
end(model(7434)).

begin(model(7435)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7435)).

begin(model(7436)).
bought(fish).
shops(mary).
end(model(7436)).

begin(model(7437)).
bought(fish).
shops(mary).
end(model(7437)).

begin(model(7438)).
bought(spaghetti).
shops(mary).
end(model(7438)).

begin(model(7439)).
bought(fish).
shops(mary).
end(model(7439)).

begin(model(7440)).
bought(fish).
shops(mary).
end(model(7440)).

begin(model(7441)).
bought(fish).
shops(mary).
end(model(7441)).

begin(model(7442)).
bought(spaghetti).
shops(mary).
end(model(7442)).

begin(model(7443)).
bought(fish).
shops(mary).
end(model(7443)).

begin(model(7444)).
bought(fish).
shops(mary).
end(model(7444)).

begin(model(7445)).
end(model(7445)).

begin(model(7446)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7446)).

begin(model(7447)).
bought(fish).
shops(mary).
end(model(7447)).

begin(model(7448)).
bought(fish).
shops(mary).
end(model(7448)).

begin(model(7449)).
bought(fish).
shops(mary).
end(model(7449)).

begin(model(7450)).
bought(fish).
shops(mary).
end(model(7450)).

begin(model(7451)).
bought(fish).
shops(mary).
end(model(7451)).

begin(model(7452)).
bought(spaghetti).
shops(mary).
end(model(7452)).

begin(model(7453)).
bought(fish).
shops(mary).
end(model(7453)).

begin(model(7454)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7454)).

begin(model(7455)).
bought(fish).
shops(mary).
end(model(7455)).

begin(model(7456)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7456)).

begin(model(7457)).
bought(fish).
shops(mary).
end(model(7457)).

begin(model(7458)).
bought(fish).
shops(mary).
end(model(7458)).

begin(model(7459)).
bought(fish).
shops(mary).
end(model(7459)).

begin(model(7460)).
bought(fish).
shops(mary).
end(model(7460)).

begin(model(7461)).
bought(spaghetti).
shops(mary).
end(model(7461)).

begin(model(7462)).
bought(fish).
shops(mary).
end(model(7462)).

begin(model(7463)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7463)).

begin(model(7464)).
bought(fish).
shops(mary).
end(model(7464)).

begin(model(7465)).
bought(fish).
shops(mary).
end(model(7465)).

begin(model(7466)).
bought(fish).
shops(mary).
end(model(7466)).

begin(model(7467)).
bought(fish).
shops(mary).
end(model(7467)).

begin(model(7468)).
bought(fish).
shops(mary).
end(model(7468)).

begin(model(7469)).
bought(fish).
shops(mary).
end(model(7469)).

begin(model(7470)).
bought(fish).
shops(mary).
end(model(7470)).

begin(model(7471)).
bought(fish).
shops(mary).
end(model(7471)).

begin(model(7472)).
bought(fish).
shops(mary).
end(model(7472)).

begin(model(7473)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7473)).

begin(model(7474)).
bought(fish).
shops(mary).
end(model(7474)).

begin(model(7475)).
bought(fish).
shops(mary).
end(model(7475)).

begin(model(7476)).
bought(fish).
shops(mary).
end(model(7476)).

begin(model(7477)).
bought(fish).
shops(mary).
end(model(7477)).

begin(model(7478)).
bought(spaghetti).
shops(mary).
end(model(7478)).

begin(model(7479)).
bought(spaghetti).
shops(john).
end(model(7479)).

begin(model(7480)).
bought(fish).
shops(mary).
end(model(7480)).

begin(model(7481)).
bought(fish).
shops(mary).
end(model(7481)).

begin(model(7482)).
bought(fish).
shops(mary).
end(model(7482)).

begin(model(7483)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7483)).

begin(model(7484)).
end(model(7484)).

begin(model(7485)).
bought(spaghetti).
shops(mary).
end(model(7485)).

begin(model(7486)).
bought(fish).
shops(mary).
end(model(7486)).

begin(model(7487)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7487)).

begin(model(7488)).
bought(fish).
shops(mary).
end(model(7488)).

begin(model(7489)).
bought(spaghetti).
shops(mary).
end(model(7489)).

begin(model(7490)).
bought(fish).
shops(mary).
end(model(7490)).

begin(model(7491)).
bought(fish).
shops(mary).
end(model(7491)).

begin(model(7492)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7492)).

begin(model(7493)).
bought(fish).
shops(mary).
end(model(7493)).

begin(model(7494)).
bought(spaghetti).
shops(mary).
end(model(7494)).

begin(model(7495)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7495)).

begin(model(7496)).
bought(fish).
shops(mary).
end(model(7496)).

begin(model(7497)).
bought(fish).
shops(mary).
end(model(7497)).

begin(model(7498)).
bought(fish).
shops(mary).
end(model(7498)).

begin(model(7499)).
bought(spaghetti).
shops(mary).
end(model(7499)).

begin(model(7500)).
bought(fish).
shops(mary).
end(model(7500)).

begin(model(7501)).
bought(spaghetti).
shops(mary).
end(model(7501)).

begin(model(7502)).
bought(fish).
shops(mary).
end(model(7502)).

begin(model(7503)).
bought(spaghetti).
shops(mary).
end(model(7503)).

begin(model(7504)).
bought(fish).
shops(mary).
end(model(7504)).

begin(model(7505)).
bought(spaghetti).
shops(mary).
end(model(7505)).

begin(model(7506)).
bought(spaghetti).
shops(mary).
end(model(7506)).

begin(model(7507)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7507)).

begin(model(7508)).
end(model(7508)).

begin(model(7509)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7509)).

begin(model(7510)).
bought(fish).
shops(mary).
end(model(7510)).

begin(model(7511)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7511)).

begin(model(7512)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7512)).

begin(model(7513)).
end(model(7513)).

begin(model(7514)).
bought(spaghetti).
shops(mary).
end(model(7514)).

begin(model(7515)).
bought(fish).
shops(mary).
end(model(7515)).

begin(model(7516)).
bought(spaghetti).
shops(mary).
end(model(7516)).

begin(model(7517)).
bought(spaghetti).
shops(mary).
end(model(7517)).

begin(model(7518)).
bought(spaghetti).
shops(mary).
end(model(7518)).

begin(model(7519)).
bought(spaghetti).
shops(mary).
end(model(7519)).

begin(model(7520)).
bought(spaghetti).
shops(mary).
end(model(7520)).

begin(model(7521)).
end(model(7521)).

begin(model(7522)).
bought(spaghetti).
shops(mary).
end(model(7522)).

begin(model(7523)).
bought(spaghetti).
shops(mary).
end(model(7523)).

begin(model(7524)).
bought(spaghetti).
shops(mary).
end(model(7524)).

begin(model(7525)).
bought(spaghetti).
shops(mary).
end(model(7525)).

begin(model(7526)).
bought(spaghetti).
shops(mary).
end(model(7526)).

begin(model(7527)).
bought(fish).
shops(mary).
end(model(7527)).

begin(model(7528)).
bought(fish).
shops(mary).
end(model(7528)).

begin(model(7529)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7529)).

begin(model(7530)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7530)).

begin(model(7531)).
bought(fish).
shops(mary).
end(model(7531)).

begin(model(7532)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7532)).

begin(model(7533)).
bought(fish).
shops(mary).
end(model(7533)).

begin(model(7534)).
bought(spaghetti).
shops(mary).
end(model(7534)).

begin(model(7535)).
bought(fish).
shops(mary).
end(model(7535)).

begin(model(7536)).
end(model(7536)).

begin(model(7537)).
bought(spaghetti).
shops(mary).
end(model(7537)).

begin(model(7538)).
bought(fish).
shops(mary).
end(model(7538)).

begin(model(7539)).
bought(fish).
shops(mary).
end(model(7539)).

begin(model(7540)).
bought(fish).
shops(mary).
end(model(7540)).

begin(model(7541)).
bought(spaghetti).
shops(mary).
end(model(7541)).

begin(model(7542)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7542)).

begin(model(7543)).
bought(spaghetti).
shops(mary).
end(model(7543)).

begin(model(7544)).
bought(fish).
shops(mary).
end(model(7544)).

begin(model(7545)).
bought(fish).
shops(mary).
end(model(7545)).

begin(model(7546)).
bought(fish).
shops(mary).
end(model(7546)).

begin(model(7547)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7547)).

begin(model(7548)).
bought(fish).
shops(mary).
end(model(7548)).

begin(model(7549)).
bought(fish).
shops(mary).
end(model(7549)).

begin(model(7550)).
bought(spaghetti).
shops(mary).
end(model(7550)).

begin(model(7551)).
bought(spaghetti).
shops(mary).
end(model(7551)).

begin(model(7552)).
end(model(7552)).

begin(model(7553)).
bought(fish).
shops(mary).
end(model(7553)).

begin(model(7554)).
bought(fish).
shops(mary).
end(model(7554)).

begin(model(7555)).
bought(fish).
shops(mary).
end(model(7555)).

begin(model(7556)).
bought(spaghetti).
shops(mary).
end(model(7556)).

begin(model(7557)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7557)).

begin(model(7558)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7558)).

begin(model(7559)).
bought(fish).
shops(mary).
end(model(7559)).

begin(model(7560)).
bought(fish).
shops(mary).
end(model(7560)).

begin(model(7561)).
end(model(7561)).

begin(model(7562)).
bought(spaghetti).
shops(mary).
end(model(7562)).

begin(model(7563)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7563)).

begin(model(7564)).
bought(spaghetti).
shops(john).
end(model(7564)).

begin(model(7565)).
bought(fish).
shops(mary).
end(model(7565)).

begin(model(7566)).
bought(fish).
shops(mary).
end(model(7566)).

begin(model(7567)).
bought(spaghetti).
shops(mary).
end(model(7567)).

begin(model(7568)).
bought(spaghetti).
shops(mary).
end(model(7568)).

begin(model(7569)).
bought(fish).
shops(mary).
end(model(7569)).

begin(model(7570)).
bought(fish).
shops(mary).
end(model(7570)).

begin(model(7571)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7571)).

begin(model(7572)).
bought(spaghetti).
shops(mary).
end(model(7572)).

begin(model(7573)).
bought(fish).
shops(mary).
end(model(7573)).

begin(model(7574)).
bought(fish).
shops(mary).
end(model(7574)).

begin(model(7575)).
bought(fish).
shops(mary).
end(model(7575)).

begin(model(7576)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7576)).

begin(model(7577)).
bought(fish).
shops(mary).
end(model(7577)).

begin(model(7578)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7578)).

begin(model(7579)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7579)).

begin(model(7580)).
bought(fish).
shops(mary).
end(model(7580)).

begin(model(7581)).
bought(spaghetti).
shops(mary).
end(model(7581)).

begin(model(7582)).
bought(fish).
shops(mary).
end(model(7582)).

begin(model(7583)).
bought(fish).
shops(mary).
end(model(7583)).

begin(model(7584)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7584)).

begin(model(7585)).
bought(fish).
shops(mary).
end(model(7585)).

begin(model(7586)).
bought(spaghetti).
shops(mary).
end(model(7586)).

begin(model(7587)).
bought(spaghetti).
shops(mary).
end(model(7587)).

begin(model(7588)).
bought(fish).
shops(mary).
end(model(7588)).

begin(model(7589)).
end(model(7589)).

begin(model(7590)).
bought(spaghetti).
shops(mary).
end(model(7590)).

begin(model(7591)).
bought(fish).
shops(mary).
end(model(7591)).

begin(model(7592)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7592)).

begin(model(7593)).
bought(spaghetti).
shops(mary).
end(model(7593)).

begin(model(7594)).
bought(fish).
shops(mary).
end(model(7594)).

begin(model(7595)).
bought(spaghetti).
shops(mary).
end(model(7595)).

begin(model(7596)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7596)).

begin(model(7597)).
bought(fish).
shops(mary).
end(model(7597)).

begin(model(7598)).
bought(fish).
shops(mary).
end(model(7598)).

begin(model(7599)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7599)).

begin(model(7600)).
bought(fish).
shops(mary).
end(model(7600)).

begin(model(7601)).
bought(spaghetti).
shops(mary).
end(model(7601)).

begin(model(7602)).
bought(spaghetti).
shops(mary).
end(model(7602)).

begin(model(7603)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7603)).

begin(model(7604)).
bought(fish).
shops(mary).
end(model(7604)).

begin(model(7605)).
bought(fish).
shops(mary).
end(model(7605)).

begin(model(7606)).
bought(fish).
shops(mary).
end(model(7606)).

begin(model(7607)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7607)).

begin(model(7608)).
bought(fish).
shops(mary).
end(model(7608)).

begin(model(7609)).
bought(spaghetti).
shops(mary).
end(model(7609)).

begin(model(7610)).
bought(fish).
shops(mary).
end(model(7610)).

begin(model(7611)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7611)).

begin(model(7612)).
bought(fish).
shops(mary).
end(model(7612)).

begin(model(7613)).
bought(fish).
shops(mary).
end(model(7613)).

begin(model(7614)).
bought(spaghetti).
shops(mary).
end(model(7614)).

begin(model(7615)).
bought(fish).
shops(mary).
end(model(7615)).

begin(model(7616)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7616)).

begin(model(7617)).
bought(spaghetti).
shops(mary).
end(model(7617)).

begin(model(7618)).
bought(fish).
shops(mary).
end(model(7618)).

begin(model(7619)).
bought(fish).
shops(mary).
end(model(7619)).

begin(model(7620)).
bought(fish).
shops(mary).
end(model(7620)).

begin(model(7621)).
bought(spaghetti).
shops(mary).
end(model(7621)).

begin(model(7622)).
bought(fish).
shops(mary).
end(model(7622)).

begin(model(7623)).
bought(fish).
shops(mary).
end(model(7623)).

begin(model(7624)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7624)).

begin(model(7625)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7625)).

begin(model(7626)).
bought(fish).
shops(mary).
end(model(7626)).

begin(model(7627)).
end(model(7627)).

begin(model(7628)).
bought(spaghetti).
shops(mary).
end(model(7628)).

begin(model(7629)).
bought(spaghetti).
shops(john).
end(model(7629)).

begin(model(7630)).
bought(fish).
shops(mary).
end(model(7630)).

begin(model(7631)).
bought(spaghetti).
shops(john).
end(model(7631)).

begin(model(7632)).
bought(fish).
shops(mary).
end(model(7632)).

begin(model(7633)).
end(model(7633)).

begin(model(7634)).
bought(fish).
shops(mary).
end(model(7634)).

begin(model(7635)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7635)).

begin(model(7636)).
bought(spaghetti).
shops(mary).
end(model(7636)).

begin(model(7637)).
bought(fish).
shops(mary).
end(model(7637)).

begin(model(7638)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7638)).

begin(model(7639)).
bought(fish).
shops(mary).
end(model(7639)).

begin(model(7640)).
bought(fish).
shops(mary).
end(model(7640)).

begin(model(7641)).
bought(fish).
shops(mary).
end(model(7641)).

begin(model(7642)).
bought(fish).
shops(mary).
end(model(7642)).

begin(model(7643)).
end(model(7643)).

begin(model(7644)).
bought(fish).
shops(mary).
end(model(7644)).

begin(model(7645)).
bought(spaghetti).
shops(mary).
end(model(7645)).

begin(model(7646)).
bought(fish).
shops(mary).
end(model(7646)).

begin(model(7647)).
bought(fish).
shops(mary).
end(model(7647)).

begin(model(7648)).
bought(fish).
shops(mary).
end(model(7648)).

begin(model(7649)).
bought(spaghetti).
shops(mary).
end(model(7649)).

begin(model(7650)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7650)).

begin(model(7651)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7651)).

begin(model(7652)).
end(model(7652)).

begin(model(7653)).
bought(spaghetti).
shops(mary).
end(model(7653)).

begin(model(7654)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7654)).

begin(model(7655)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7655)).

begin(model(7656)).
bought(fish).
shops(mary).
end(model(7656)).

begin(model(7657)).
bought(fish).
shops(mary).
end(model(7657)).

begin(model(7658)).
bought(fish).
shops(mary).
end(model(7658)).

begin(model(7659)).
end(model(7659)).

begin(model(7660)).
bought(fish).
shops(mary).
end(model(7660)).

begin(model(7661)).
bought(spaghetti).
shops(mary).
end(model(7661)).

begin(model(7662)).
bought(spaghetti).
shops(mary).
end(model(7662)).

begin(model(7663)).
bought(spaghetti).
shops(mary).
end(model(7663)).

begin(model(7664)).
bought(spaghetti).
shops(mary).
end(model(7664)).

begin(model(7665)).
bought(fish).
shops(mary).
end(model(7665)).

begin(model(7666)).
bought(fish).
shops(mary).
end(model(7666)).

begin(model(7667)).
bought(spaghetti).
shops(mary).
end(model(7667)).

begin(model(7668)).
end(model(7668)).

begin(model(7669)).
bought(fish).
shops(mary).
end(model(7669)).

begin(model(7670)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7670)).

begin(model(7671)).
bought(fish).
shops(mary).
end(model(7671)).

begin(model(7672)).
bought(fish).
shops(mary).
end(model(7672)).

begin(model(7673)).
bought(spaghetti).
shops(mary).
end(model(7673)).

begin(model(7674)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7674)).

begin(model(7675)).
bought(spaghetti).
shops(mary).
end(model(7675)).

begin(model(7676)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7676)).

begin(model(7677)).
bought(fish).
shops(mary).
end(model(7677)).

begin(model(7678)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7678)).

begin(model(7679)).
bought(fish).
shops(mary).
end(model(7679)).

begin(model(7680)).
bought(spaghetti).
shops(john).
end(model(7680)).

begin(model(7681)).
bought(fish).
shops(mary).
end(model(7681)).

begin(model(7682)).
bought(fish).
shops(mary).
end(model(7682)).

begin(model(7683)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7683)).

begin(model(7684)).
bought(spaghetti).
shops(mary).
end(model(7684)).

begin(model(7685)).
bought(fish).
shops(mary).
end(model(7685)).

begin(model(7686)).
end(model(7686)).

begin(model(7687)).
bought(fish).
shops(mary).
end(model(7687)).

begin(model(7688)).
bought(fish).
shops(mary).
end(model(7688)).

begin(model(7689)).
bought(spaghetti).
shops(mary).
end(model(7689)).

begin(model(7690)).
bought(fish).
shops(mary).
end(model(7690)).

begin(model(7691)).
bought(fish).
shops(mary).
end(model(7691)).

begin(model(7692)).
bought(fish).
shops(mary).
end(model(7692)).

begin(model(7693)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7693)).

begin(model(7694)).
bought(spaghetti).
shops(mary).
end(model(7694)).

begin(model(7695)).
bought(spaghetti).
shops(mary).
end(model(7695)).

begin(model(7696)).
end(model(7696)).

begin(model(7697)).
bought(fish).
shops(mary).
end(model(7697)).

begin(model(7698)).
bought(fish).
shops(mary).
end(model(7698)).

begin(model(7699)).
bought(fish).
shops(mary).
end(model(7699)).

begin(model(7700)).
bought(fish).
shops(mary).
end(model(7700)).

begin(model(7701)).
bought(fish).
shops(mary).
end(model(7701)).

begin(model(7702)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7702)).

begin(model(7703)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7703)).

begin(model(7704)).
bought(fish).
shops(mary).
end(model(7704)).

begin(model(7705)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7705)).

begin(model(7706)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7706)).

begin(model(7707)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7707)).

begin(model(7708)).
bought(spaghetti).
shops(mary).
end(model(7708)).

begin(model(7709)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7709)).

begin(model(7710)).
bought(spaghetti).
shops(mary).
end(model(7710)).

begin(model(7711)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7711)).

begin(model(7712)).
bought(fish).
shops(mary).
end(model(7712)).

begin(model(7713)).
bought(fish).
shops(mary).
end(model(7713)).

begin(model(7714)).
end(model(7714)).

begin(model(7715)).
bought(fish).
shops(mary).
end(model(7715)).

begin(model(7716)).
end(model(7716)).

begin(model(7717)).
bought(fish).
shops(mary).
end(model(7717)).

begin(model(7718)).
bought(spaghetti).
shops(john).
end(model(7718)).

begin(model(7719)).
bought(fish).
shops(mary).
end(model(7719)).

begin(model(7720)).
bought(fish).
shops(mary).
end(model(7720)).

begin(model(7721)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7721)).

begin(model(7722)).
bought(fish).
shops(mary).
end(model(7722)).

begin(model(7723)).
end(model(7723)).

begin(model(7724)).
bought(spaghetti).
shops(mary).
end(model(7724)).

begin(model(7725)).
bought(fish).
shops(mary).
end(model(7725)).

begin(model(7726)).
bought(fish).
shops(mary).
end(model(7726)).

begin(model(7727)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7727)).

begin(model(7728)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7728)).

begin(model(7729)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7729)).

begin(model(7730)).
bought(spaghetti).
shops(mary).
end(model(7730)).

begin(model(7731)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7731)).

begin(model(7732)).
bought(fish).
shops(mary).
end(model(7732)).

begin(model(7733)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7733)).

begin(model(7734)).
bought(fish).
shops(mary).
end(model(7734)).

begin(model(7735)).
bought(fish).
shops(mary).
end(model(7735)).

begin(model(7736)).
bought(fish).
shops(mary).
end(model(7736)).

begin(model(7737)).
bought(fish).
shops(mary).
end(model(7737)).

begin(model(7738)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7738)).

begin(model(7739)).
bought(spaghetti).
shops(mary).
end(model(7739)).

begin(model(7740)).
bought(fish).
shops(mary).
end(model(7740)).

begin(model(7741)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7741)).

begin(model(7742)).
bought(fish).
shops(mary).
end(model(7742)).

begin(model(7743)).
bought(fish).
shops(mary).
end(model(7743)).

begin(model(7744)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7744)).

begin(model(7745)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7745)).

begin(model(7746)).
bought(fish).
shops(mary).
end(model(7746)).

begin(model(7747)).
bought(fish).
shops(mary).
end(model(7747)).

begin(model(7748)).
bought(fish).
shops(mary).
end(model(7748)).

begin(model(7749)).
bought(fish).
shops(mary).
end(model(7749)).

begin(model(7750)).
bought(fish).
shops(mary).
end(model(7750)).

begin(model(7751)).
bought(fish).
shops(mary).
end(model(7751)).

begin(model(7752)).
end(model(7752)).

begin(model(7753)).
bought(spaghetti).
shops(mary).
end(model(7753)).

begin(model(7754)).
bought(fish).
shops(mary).
end(model(7754)).

begin(model(7755)).
bought(fish).
shops(mary).
end(model(7755)).

begin(model(7756)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7756)).

begin(model(7757)).
bought(spaghetti).
shops(mary).
end(model(7757)).

begin(model(7758)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7758)).

begin(model(7759)).
bought(fish).
shops(mary).
end(model(7759)).

begin(model(7760)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7760)).

begin(model(7761)).
end(model(7761)).

begin(model(7762)).
bought(fish).
shops(mary).
end(model(7762)).

begin(model(7763)).
bought(fish).
shops(mary).
end(model(7763)).

begin(model(7764)).
bought(spaghetti).
shops(mary).
end(model(7764)).

begin(model(7765)).
bought(spaghetti).
shops(mary).
end(model(7765)).

begin(model(7766)).
bought(fish).
shops(mary).
end(model(7766)).

begin(model(7767)).
bought(fish).
shops(mary).
end(model(7767)).

begin(model(7768)).
bought(fish).
shops(mary).
end(model(7768)).

begin(model(7769)).
end(model(7769)).

begin(model(7770)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7770)).

begin(model(7771)).
bought(fish).
shops(mary).
end(model(7771)).

begin(model(7772)).
bought(fish).
shops(mary).
end(model(7772)).

begin(model(7773)).
bought(fish).
shops(mary).
end(model(7773)).

begin(model(7774)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7774)).

begin(model(7775)).
bought(spaghetti).
shops(mary).
end(model(7775)).

begin(model(7776)).
bought(fish).
shops(mary).
end(model(7776)).

begin(model(7777)).
end(model(7777)).

begin(model(7778)).
bought(fish).
shops(mary).
end(model(7778)).

begin(model(7779)).
bought(fish).
shops(mary).
end(model(7779)).

begin(model(7780)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7780)).

begin(model(7781)).
bought(spaghetti).
shops(john).
end(model(7781)).

begin(model(7782)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7782)).

begin(model(7783)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7783)).

begin(model(7784)).
bought(fish).
shops(mary).
end(model(7784)).

begin(model(7785)).
bought(fish).
shops(mary).
end(model(7785)).

begin(model(7786)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7786)).

begin(model(7787)).
bought(fish).
shops(mary).
end(model(7787)).

begin(model(7788)).
bought(fish).
shops(mary).
end(model(7788)).

begin(model(7789)).
bought(fish).
shops(mary).
end(model(7789)).

begin(model(7790)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7790)).

begin(model(7791)).
bought(fish).
shops(mary).
end(model(7791)).

begin(model(7792)).
bought(fish).
shops(mary).
end(model(7792)).

begin(model(7793)).
bought(fish).
shops(mary).
end(model(7793)).

begin(model(7794)).
bought(fish).
shops(mary).
end(model(7794)).

begin(model(7795)).
bought(fish).
shops(mary).
end(model(7795)).

begin(model(7796)).
bought(fish).
shops(mary).
end(model(7796)).

begin(model(7797)).
bought(spaghetti).
shops(mary).
end(model(7797)).

begin(model(7798)).
bought(fish).
shops(mary).
end(model(7798)).

begin(model(7799)).
bought(fish).
shops(mary).
end(model(7799)).

begin(model(7800)).
bought(fish).
shops(mary).
end(model(7800)).

begin(model(7801)).
bought(spaghetti).
shops(mary).
end(model(7801)).

begin(model(7802)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7802)).

begin(model(7803)).
bought(spaghetti).
shops(mary).
end(model(7803)).

begin(model(7804)).
bought(fish).
shops(mary).
end(model(7804)).

begin(model(7805)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7805)).

begin(model(7806)).
bought(fish).
shops(mary).
end(model(7806)).

begin(model(7807)).
bought(spaghetti).
shops(mary).
end(model(7807)).

begin(model(7808)).
bought(fish).
shops(mary).
end(model(7808)).

begin(model(7809)).
bought(fish).
shops(mary).
end(model(7809)).

begin(model(7810)).
bought(fish).
shops(mary).
end(model(7810)).

begin(model(7811)).
bought(fish).
shops(mary).
end(model(7811)).

begin(model(7812)).
bought(fish).
shops(mary).
end(model(7812)).

begin(model(7813)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7813)).

begin(model(7814)).
bought(fish).
shops(mary).
end(model(7814)).

begin(model(7815)).
bought(fish).
shops(mary).
end(model(7815)).

begin(model(7816)).
bought(spaghetti).
shops(mary).
end(model(7816)).

begin(model(7817)).
bought(fish).
shops(mary).
end(model(7817)).

begin(model(7818)).
bought(fish).
shops(mary).
end(model(7818)).

begin(model(7819)).
bought(fish).
shops(mary).
end(model(7819)).

begin(model(7820)).
bought(fish).
shops(mary).
end(model(7820)).

begin(model(7821)).
bought(spaghetti).
shops(mary).
end(model(7821)).

begin(model(7822)).
end(model(7822)).

begin(model(7823)).
bought(fish).
shops(mary).
end(model(7823)).

begin(model(7824)).
bought(fish).
shops(mary).
end(model(7824)).

begin(model(7825)).
bought(spaghetti).
shops(mary).
end(model(7825)).

begin(model(7826)).
end(model(7826)).

begin(model(7827)).
bought(spaghetti).
shops(mary).
end(model(7827)).

begin(model(7828)).
bought(fish).
shops(mary).
end(model(7828)).

begin(model(7829)).
bought(fish).
shops(mary).
end(model(7829)).

begin(model(7830)).
bought(fish).
shops(mary).
end(model(7830)).

begin(model(7831)).
bought(fish).
shops(mary).
end(model(7831)).

begin(model(7832)).
bought(fish).
shops(mary).
end(model(7832)).

begin(model(7833)).
bought(fish).
shops(mary).
end(model(7833)).

begin(model(7834)).
bought(fish).
shops(mary).
end(model(7834)).

begin(model(7835)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7835)).

begin(model(7836)).
bought(fish).
shops(mary).
end(model(7836)).

begin(model(7837)).
bought(fish).
shops(mary).
end(model(7837)).

begin(model(7838)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7838)).

begin(model(7839)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7839)).

begin(model(7840)).
end(model(7840)).

begin(model(7841)).
bought(fish).
shops(mary).
end(model(7841)).

begin(model(7842)).
bought(fish).
shops(mary).
end(model(7842)).

begin(model(7843)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7843)).

begin(model(7844)).
bought(fish).
shops(mary).
end(model(7844)).

begin(model(7845)).
bought(fish).
shops(mary).
end(model(7845)).

begin(model(7846)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7846)).

begin(model(7847)).
bought(fish).
shops(mary).
end(model(7847)).

begin(model(7848)).
bought(spaghetti).
shops(mary).
end(model(7848)).

begin(model(7849)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7849)).

begin(model(7850)).
bought(spaghetti).
shops(mary).
end(model(7850)).

begin(model(7851)).
end(model(7851)).

begin(model(7852)).
bought(fish).
shops(mary).
end(model(7852)).

begin(model(7853)).
bought(fish).
shops(mary).
end(model(7853)).

begin(model(7854)).
bought(spaghetti).
shops(mary).
end(model(7854)).

begin(model(7855)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7855)).

begin(model(7856)).
bought(fish).
shops(mary).
end(model(7856)).

begin(model(7857)).
bought(fish).
shops(mary).
end(model(7857)).

begin(model(7858)).
bought(spaghetti).
shops(mary).
end(model(7858)).

begin(model(7859)).
bought(spaghetti).
shops(mary).
end(model(7859)).

begin(model(7860)).
bought(fish).
shops(mary).
end(model(7860)).

begin(model(7861)).
bought(fish).
shops(mary).
end(model(7861)).

begin(model(7862)).
bought(spaghetti).
shops(mary).
end(model(7862)).

begin(model(7863)).
bought(fish).
shops(mary).
end(model(7863)).

begin(model(7864)).
bought(fish).
shops(mary).
end(model(7864)).

begin(model(7865)).
bought(fish).
shops(mary).
end(model(7865)).

begin(model(7866)).
bought(fish).
shops(mary).
end(model(7866)).

begin(model(7867)).
bought(spaghetti).
shops(mary).
end(model(7867)).

begin(model(7868)).
bought(fish).
shops(mary).
end(model(7868)).

begin(model(7869)).
bought(fish).
shops(mary).
end(model(7869)).

begin(model(7870)).
bought(spaghetti).
shops(mary).
end(model(7870)).

begin(model(7871)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7871)).

begin(model(7872)).
bought(fish).
shops(mary).
end(model(7872)).

begin(model(7873)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7873)).

begin(model(7874)).
bought(spaghetti).
shops(mary).
end(model(7874)).

begin(model(7875)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7875)).

begin(model(7876)).
bought(spaghetti).
shops(john).
end(model(7876)).

begin(model(7877)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7877)).

begin(model(7878)).
bought(spaghetti).
shops(mary).
end(model(7878)).

begin(model(7879)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7879)).

begin(model(7880)).
bought(fish).
shops(mary).
end(model(7880)).

begin(model(7881)).
bought(fish).
shops(mary).
end(model(7881)).

begin(model(7882)).
bought(fish).
shops(mary).
end(model(7882)).

begin(model(7883)).
bought(spaghetti).
shops(mary).
end(model(7883)).

begin(model(7884)).
bought(spaghetti).
shops(mary).
end(model(7884)).

begin(model(7885)).
bought(fish).
shops(mary).
end(model(7885)).

begin(model(7886)).
bought(fish).
shops(mary).
end(model(7886)).

begin(model(7887)).
bought(spaghetti).
shops(mary).
end(model(7887)).

begin(model(7888)).
bought(fish).
shops(mary).
end(model(7888)).

begin(model(7889)).
bought(fish).
shops(mary).
end(model(7889)).

begin(model(7890)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7890)).

begin(model(7891)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7891)).

begin(model(7892)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7892)).

begin(model(7893)).
bought(fish).
shops(mary).
end(model(7893)).

begin(model(7894)).
bought(fish).
shops(mary).
end(model(7894)).

begin(model(7895)).
bought(fish).
shops(mary).
end(model(7895)).

begin(model(7896)).
bought(spaghetti).
shops(john).
end(model(7896)).

begin(model(7897)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7897)).

begin(model(7898)).
bought(spaghetti).
shops(mary).
end(model(7898)).

begin(model(7899)).
end(model(7899)).

begin(model(7900)).
bought(fish).
shops(mary).
end(model(7900)).

begin(model(7901)).
bought(fish).
shops(mary).
end(model(7901)).

begin(model(7902)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7902)).

begin(model(7903)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7903)).

begin(model(7904)).
bought(spaghetti).
shops(mary).
end(model(7904)).

begin(model(7905)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7905)).

begin(model(7906)).
bought(fish).
shops(mary).
end(model(7906)).

begin(model(7907)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7907)).

begin(model(7908)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7908)).

begin(model(7909)).
bought(fish).
shops(mary).
end(model(7909)).

begin(model(7910)).
bought(fish).
shops(mary).
end(model(7910)).

begin(model(7911)).
bought(spaghetti).
shops(mary).
end(model(7911)).

begin(model(7912)).
bought(fish).
shops(mary).
end(model(7912)).

begin(model(7913)).
bought(fish).
shops(mary).
end(model(7913)).

begin(model(7914)).
bought(fish).
shops(mary).
end(model(7914)).

begin(model(7915)).
bought(spaghetti).
shops(mary).
end(model(7915)).

begin(model(7916)).
bought(spaghetti).
shops(mary).
end(model(7916)).

begin(model(7917)).
bought(fish).
shops(mary).
end(model(7917)).

begin(model(7918)).
bought(spaghetti).
shops(mary).
end(model(7918)).

begin(model(7919)).
bought(spaghetti).
shops(mary).
end(model(7919)).

begin(model(7920)).
bought(spaghetti).
shops(mary).
end(model(7920)).

begin(model(7921)).
bought(fish).
shops(mary).
end(model(7921)).

begin(model(7922)).
bought(spaghetti).
shops(mary).
end(model(7922)).

begin(model(7923)).
bought(spaghetti).
shops(mary).
end(model(7923)).

begin(model(7924)).
bought(fish).
shops(mary).
end(model(7924)).

begin(model(7925)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7925)).

begin(model(7926)).
bought(fish).
shops(mary).
end(model(7926)).

begin(model(7927)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7927)).

begin(model(7928)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7928)).

begin(model(7929)).
bought(spaghetti).
shops(mary).
end(model(7929)).

begin(model(7930)).
bought(fish).
shops(mary).
end(model(7930)).

begin(model(7931)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7931)).

begin(model(7932)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7932)).

begin(model(7933)).
end(model(7933)).

begin(model(7934)).
bought(spaghetti).
shops(mary).
end(model(7934)).

begin(model(7935)).
bought(fish).
shops(mary).
end(model(7935)).

begin(model(7936)).
bought(fish).
shops(mary).
end(model(7936)).

begin(model(7937)).
bought(spaghetti).
shops(mary).
end(model(7937)).

begin(model(7938)).
bought(fish).
shops(mary).
end(model(7938)).

begin(model(7939)).
end(model(7939)).

begin(model(7940)).
bought(fish).
shops(mary).
end(model(7940)).

begin(model(7941)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7941)).

begin(model(7942)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7942)).

begin(model(7943)).
bought(fish).
shops(mary).
end(model(7943)).

begin(model(7944)).
bought(fish).
shops(mary).
end(model(7944)).

begin(model(7945)).
bought(fish).
shops(mary).
end(model(7945)).

begin(model(7946)).
bought(fish).
shops(mary).
end(model(7946)).

begin(model(7947)).
end(model(7947)).

begin(model(7948)).
bought(fish).
shops(mary).
end(model(7948)).

begin(model(7949)).
bought(fish).
shops(mary).
end(model(7949)).

begin(model(7950)).
bought(fish).
shops(mary).
end(model(7950)).

begin(model(7951)).
bought(spaghetti).
shops(mary).
end(model(7951)).

begin(model(7952)).
bought(fish).
shops(mary).
end(model(7952)).

begin(model(7953)).
bought(spaghetti).
shops(mary).
end(model(7953)).

begin(model(7954)).
bought(fish).
shops(mary).
end(model(7954)).

begin(model(7955)).
bought(spaghetti).
shops(mary).
end(model(7955)).

begin(model(7956)).
bought(spaghetti).
shops(mary).
end(model(7956)).

begin(model(7957)).
bought(fish).
shops(mary).
end(model(7957)).

begin(model(7958)).
bought(fish).
shops(mary).
end(model(7958)).

begin(model(7959)).
bought(fish).
shops(mary).
end(model(7959)).

begin(model(7960)).
bought(spaghetti).
shops(mary).
end(model(7960)).

begin(model(7961)).
bought(fish).
shops(mary).
end(model(7961)).

begin(model(7962)).
bought(fish).
shops(mary).
end(model(7962)).

begin(model(7963)).
bought(spaghetti).
shops(mary).
end(model(7963)).

begin(model(7964)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7964)).

begin(model(7965)).
bought(fish).
shops(mary).
end(model(7965)).

begin(model(7966)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7966)).

begin(model(7967)).
bought(fish).
shops(mary).
end(model(7967)).

begin(model(7968)).
bought(fish).
shops(mary).
end(model(7968)).

begin(model(7969)).
bought(spaghetti).
shops(mary).
end(model(7969)).

begin(model(7970)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(7970)).

begin(model(7971)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7971)).

begin(model(7972)).
bought(fish).
shops(mary).
end(model(7972)).

begin(model(7973)).
bought(steak).
shops(john).
end(model(7973)).

begin(model(7974)).
bought(fish).
shops(mary).
end(model(7974)).

begin(model(7975)).
bought(spaghetti).
shops(mary).
end(model(7975)).

begin(model(7976)).
bought(fish).
shops(mary).
end(model(7976)).

begin(model(7977)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7977)).

begin(model(7978)).
bought(fish).
shops(mary).
end(model(7978)).

begin(model(7979)).
bought(spaghetti).
shops(mary).
end(model(7979)).

begin(model(7980)).
bought(spaghetti).
shops(mary).
end(model(7980)).

begin(model(7981)).
bought(fish).
shops(mary).
end(model(7981)).

begin(model(7982)).
bought(fish).
shops(mary).
end(model(7982)).

begin(model(7983)).
bought(spaghetti).
shops(mary).
end(model(7983)).

begin(model(7984)).
bought(fish).
shops(mary).
end(model(7984)).

begin(model(7985)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7985)).

begin(model(7986)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7986)).

begin(model(7987)).
bought(fish).
shops(mary).
end(model(7987)).

begin(model(7988)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(7988)).

begin(model(7989)).
bought(fish).
shops(mary).
end(model(7989)).

begin(model(7990)).
bought(fish).
shops(mary).
end(model(7990)).

begin(model(7991)).
bought(spaghetti).
shops(mary).
end(model(7991)).

begin(model(7992)).
bought(fish).
shops(mary).
end(model(7992)).

begin(model(7993)).
bought(fish).
shops(mary).
end(model(7993)).

begin(model(7994)).
bought(spaghetti).
shops(mary).
end(model(7994)).

begin(model(7995)).
bought(fish).
shops(mary).
end(model(7995)).

begin(model(7996)).
bought(fish).
shops(mary).
end(model(7996)).

begin(model(7997)).
bought(spaghetti).
shops(mary).
end(model(7997)).

begin(model(7998)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(7998)).

begin(model(7999)).
bought(spaghetti).
shops(mary).
end(model(7999)).

begin(model(8000)).
bought(fish).
shops(mary).
end(model(8000)).

begin(model(8001)).
bought(fish).
shops(mary).
end(model(8001)).

begin(model(8002)).
end(model(8002)).

begin(model(8003)).
bought(spaghetti).
shops(mary).
end(model(8003)).

begin(model(8004)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8004)).

begin(model(8005)).
bought(fish).
shops(mary).
end(model(8005)).

begin(model(8006)).
bought(fish).
shops(mary).
end(model(8006)).

begin(model(8007)).
bought(fish).
shops(mary).
end(model(8007)).

begin(model(8008)).
bought(fish).
shops(mary).
end(model(8008)).

begin(model(8009)).
end(model(8009)).

begin(model(8010)).
bought(fish).
shops(mary).
end(model(8010)).

begin(model(8011)).
bought(fish).
shops(mary).
end(model(8011)).

begin(model(8012)).
bought(spaghetti).
shops(mary).
end(model(8012)).

begin(model(8013)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8013)).

begin(model(8014)).
bought(fish).
shops(mary).
end(model(8014)).

begin(model(8015)).
bought(fish).
shops(mary).
end(model(8015)).

begin(model(8016)).
bought(fish).
shops(mary).
end(model(8016)).

begin(model(8017)).
bought(fish).
shops(mary).
end(model(8017)).

begin(model(8018)).
bought(spaghetti).
shops(mary).
end(model(8018)).

begin(model(8019)).
bought(fish).
shops(mary).
end(model(8019)).

begin(model(8020)).
bought(fish).
shops(mary).
end(model(8020)).

begin(model(8021)).
bought(steak).
shops(john).
end(model(8021)).

begin(model(8022)).
bought(fish).
shops(mary).
end(model(8022)).

begin(model(8023)).
bought(fish).
shops(mary).
end(model(8023)).

begin(model(8024)).
bought(fish).
shops(mary).
end(model(8024)).

begin(model(8025)).
end(model(8025)).

begin(model(8026)).
bought(fish).
shops(mary).
end(model(8026)).

begin(model(8027)).
bought(spaghetti).
shops(mary).
end(model(8027)).

begin(model(8028)).
end(model(8028)).

begin(model(8029)).
end(model(8029)).

begin(model(8030)).
bought(spaghetti).
shops(mary).
end(model(8030)).

begin(model(8031)).
bought(spaghetti).
shops(john).
end(model(8031)).

begin(model(8032)).
bought(spaghetti).
shops(mary).
end(model(8032)).

begin(model(8033)).
bought(spaghetti).
shops(mary).
end(model(8033)).

begin(model(8034)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8034)).

begin(model(8035)).
bought(fish).
shops(mary).
end(model(8035)).

begin(model(8036)).
bought(fish).
shops(mary).
end(model(8036)).

begin(model(8037)).
bought(spaghetti).
shops(mary).
end(model(8037)).

begin(model(8038)).
end(model(8038)).

begin(model(8039)).
end(model(8039)).

begin(model(8040)).
bought(fish).
shops(mary).
end(model(8040)).

begin(model(8041)).
bought(spaghetti).
shops(mary).
end(model(8041)).

begin(model(8042)).
end(model(8042)).

begin(model(8043)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8043)).

begin(model(8044)).
bought(fish).
shops(mary).
end(model(8044)).

begin(model(8045)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8045)).

begin(model(8046)).
end(model(8046)).

begin(model(8047)).
bought(fish).
shops(mary).
end(model(8047)).

begin(model(8048)).
bought(spaghetti).
shops(mary).
end(model(8048)).

begin(model(8049)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8049)).

begin(model(8050)).
end(model(8050)).

begin(model(8051)).
bought(fish).
shops(mary).
end(model(8051)).

begin(model(8052)).
end(model(8052)).

begin(model(8053)).
bought(spaghetti).
shops(mary).
end(model(8053)).

begin(model(8054)).
bought(spaghetti).
shops(mary).
end(model(8054)).

begin(model(8055)).
bought(fish).
shops(mary).
end(model(8055)).

begin(model(8056)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8056)).

begin(model(8057)).
bought(fish).
shops(mary).
end(model(8057)).

begin(model(8058)).
bought(spaghetti).
shops(mary).
end(model(8058)).

begin(model(8059)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8059)).

begin(model(8060)).
bought(fish).
shops(mary).
end(model(8060)).

begin(model(8061)).
end(model(8061)).

begin(model(8062)).
bought(fish).
shops(mary).
end(model(8062)).

begin(model(8063)).
bought(spaghetti).
shops(mary).
end(model(8063)).

begin(model(8064)).
bought(fish).
shops(mary).
end(model(8064)).

begin(model(8065)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8065)).

begin(model(8066)).
bought(spaghetti).
shops(mary).
end(model(8066)).

begin(model(8067)).
bought(fish).
shops(mary).
end(model(8067)).

begin(model(8068)).
bought(fish).
shops(mary).
end(model(8068)).

begin(model(8069)).
bought(fish).
shops(mary).
end(model(8069)).

begin(model(8070)).
end(model(8070)).

begin(model(8071)).
bought(fish).
shops(mary).
end(model(8071)).

begin(model(8072)).
bought(fish).
shops(mary).
end(model(8072)).

begin(model(8073)).
bought(spaghetti).
shops(mary).
end(model(8073)).

begin(model(8074)).
bought(fish).
shops(mary).
end(model(8074)).

begin(model(8075)).
bought(fish).
shops(mary).
end(model(8075)).

begin(model(8076)).
bought(fish).
shops(mary).
end(model(8076)).

begin(model(8077)).
bought(fish).
shops(mary).
end(model(8077)).

begin(model(8078)).
bought(spaghetti).
shops(mary).
end(model(8078)).

begin(model(8079)).
bought(spaghetti).
shops(mary).
end(model(8079)).

begin(model(8080)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8080)).

begin(model(8081)).
bought(fish).
shops(mary).
end(model(8081)).

begin(model(8082)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8082)).

begin(model(8083)).
bought(fish).
shops(mary).
end(model(8083)).

begin(model(8084)).
bought(fish).
shops(mary).
end(model(8084)).

begin(model(8085)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8085)).

begin(model(8086)).
bought(fish).
shops(mary).
end(model(8086)).

begin(model(8087)).
bought(fish).
shops(mary).
end(model(8087)).

begin(model(8088)).
bought(fish).
shops(mary).
end(model(8088)).

begin(model(8089)).
bought(spaghetti).
shops(mary).
end(model(8089)).

begin(model(8090)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8090)).

begin(model(8091)).
bought(steak).
shops(john).
end(model(8091)).

begin(model(8092)).
bought(fish).
shops(mary).
end(model(8092)).

begin(model(8093)).
bought(fish).
shops(mary).
end(model(8093)).

begin(model(8094)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8094)).

begin(model(8095)).
bought(fish).
shops(mary).
end(model(8095)).

begin(model(8096)).
end(model(8096)).

begin(model(8097)).
bought(fish).
shops(mary).
end(model(8097)).

begin(model(8098)).
bought(fish).
shops(mary).
end(model(8098)).

begin(model(8099)).
bought(spaghetti).
shops(mary).
end(model(8099)).

begin(model(8100)).
bought(fish).
shops(mary).
end(model(8100)).

begin(model(8101)).
bought(fish).
shops(mary).
end(model(8101)).

begin(model(8102)).
bought(fish).
shops(mary).
end(model(8102)).

begin(model(8103)).
bought(fish).
shops(mary).
end(model(8103)).

begin(model(8104)).
bought(fish).
shops(mary).
end(model(8104)).

begin(model(8105)).
bought(fish).
shops(mary).
end(model(8105)).

begin(model(8106)).
end(model(8106)).

begin(model(8107)).
bought(fish).
shops(mary).
end(model(8107)).

begin(model(8108)).
bought(fish).
shops(mary).
end(model(8108)).

begin(model(8109)).
bought(spaghetti).
shops(mary).
end(model(8109)).

begin(model(8110)).
bought(fish).
shops(mary).
end(model(8110)).

begin(model(8111)).
bought(fish).
shops(mary).
end(model(8111)).

begin(model(8112)).
bought(fish).
shops(mary).
end(model(8112)).

begin(model(8113)).
bought(fish).
shops(mary).
end(model(8113)).

begin(model(8114)).
bought(spaghetti).
shops(mary).
end(model(8114)).

begin(model(8115)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8115)).

begin(model(8116)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8116)).

begin(model(8117)).
bought(spaghetti).
shops(mary).
end(model(8117)).

begin(model(8118)).
bought(fish).
shops(mary).
end(model(8118)).

begin(model(8119)).
bought(fish).
shops(mary).
end(model(8119)).

begin(model(8120)).
bought(fish).
shops(mary).
end(model(8120)).

begin(model(8121)).
bought(fish).
shops(mary).
end(model(8121)).

begin(model(8122)).
bought(spaghetti).
shops(mary).
end(model(8122)).

begin(model(8123)).
bought(fish).
shops(mary).
end(model(8123)).

begin(model(8124)).
bought(fish).
shops(mary).
end(model(8124)).

begin(model(8125)).
bought(spaghetti).
shops(mary).
end(model(8125)).

begin(model(8126)).
bought(fish).
shops(mary).
end(model(8126)).

begin(model(8127)).
bought(fish).
shops(mary).
end(model(8127)).

begin(model(8128)).
bought(fish).
shops(mary).
end(model(8128)).

begin(model(8129)).
bought(fish).
shops(mary).
end(model(8129)).

begin(model(8130)).
bought(fish).
shops(mary).
end(model(8130)).

begin(model(8131)).
end(model(8131)).

begin(model(8132)).
bought(fish).
shops(mary).
end(model(8132)).

begin(model(8133)).
bought(fish).
shops(mary).
end(model(8133)).

begin(model(8134)).
bought(fish).
shops(mary).
end(model(8134)).

begin(model(8135)).
end(model(8135)).

begin(model(8136)).
bought(fish).
shops(mary).
end(model(8136)).

begin(model(8137)).
bought(fish).
shops(mary).
end(model(8137)).

begin(model(8138)).
bought(spaghetti).
shops(mary).
end(model(8138)).

begin(model(8139)).
bought(spaghetti).
shops(mary).
end(model(8139)).

begin(model(8140)).
bought(fish).
shops(mary).
end(model(8140)).

begin(model(8141)).
bought(fish).
shops(mary).
end(model(8141)).

begin(model(8142)).
bought(fish).
shops(mary).
end(model(8142)).

begin(model(8143)).
bought(fish).
shops(mary).
end(model(8143)).

begin(model(8144)).
bought(fish).
shops(mary).
end(model(8144)).

begin(model(8145)).
bought(fish).
shops(mary).
end(model(8145)).

begin(model(8146)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8146)).

begin(model(8147)).
end(model(8147)).

begin(model(8148)).
bought(fish).
shops(mary).
end(model(8148)).

begin(model(8149)).
bought(spaghetti).
shops(mary).
end(model(8149)).

begin(model(8150)).
bought(spaghetti).
shops(mary).
end(model(8150)).

begin(model(8151)).
bought(fish).
shops(mary).
end(model(8151)).

begin(model(8152)).
bought(spaghetti).
shops(mary).
end(model(8152)).

begin(model(8153)).
bought(spaghetti).
shops(mary).
end(model(8153)).

begin(model(8154)).
bought(fish).
shops(mary).
end(model(8154)).

begin(model(8155)).
bought(fish).
shops(mary).
end(model(8155)).

begin(model(8156)).
bought(fish).
shops(mary).
end(model(8156)).

begin(model(8157)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8157)).

begin(model(8158)).
bought(steak).
shops(john).
end(model(8158)).

begin(model(8159)).
bought(fish).
shops(mary).
end(model(8159)).

begin(model(8160)).
end(model(8160)).

begin(model(8161)).
bought(fish).
shops(mary).
end(model(8161)).

begin(model(8162)).
bought(fish).
shops(mary).
end(model(8162)).

begin(model(8163)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8163)).

begin(model(8164)).
bought(spaghetti).
shops(john).
end(model(8164)).

begin(model(8165)).
bought(fish).
shops(mary).
end(model(8165)).

begin(model(8166)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8166)).

begin(model(8167)).
bought(fish).
shops(mary).
end(model(8167)).

begin(model(8168)).
bought(spaghetti).
shops(mary).
end(model(8168)).

begin(model(8169)).
bought(spaghetti).
shops(mary).
end(model(8169)).

begin(model(8170)).
bought(fish).
shops(mary).
end(model(8170)).

begin(model(8171)).
bought(fish).
shops(mary).
end(model(8171)).

begin(model(8172)).
bought(spaghetti).
shops(mary).
end(model(8172)).

begin(model(8173)).
bought(fish).
shops(mary).
end(model(8173)).

begin(model(8174)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8174)).

begin(model(8175)).
bought(fish).
shops(mary).
end(model(8175)).

begin(model(8176)).
bought(spaghetti).
shops(mary).
end(model(8176)).

begin(model(8177)).
bought(fish).
shops(mary).
end(model(8177)).

begin(model(8178)).
bought(fish).
shops(mary).
end(model(8178)).

begin(model(8179)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8179)).

begin(model(8180)).
end(model(8180)).

begin(model(8181)).
bought(fish).
shops(mary).
end(model(8181)).

begin(model(8182)).
bought(spaghetti).
shops(mary).
end(model(8182)).

begin(model(8183)).
bought(fish).
shops(mary).
end(model(8183)).

begin(model(8184)).
bought(fish).
shops(mary).
end(model(8184)).

begin(model(8185)).
bought(spaghetti).
shops(mary).
end(model(8185)).

begin(model(8186)).
bought(fish).
shops(mary).
end(model(8186)).

begin(model(8187)).
bought(fish).
shops(mary).
end(model(8187)).

begin(model(8188)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8188)).

begin(model(8189)).
bought(fish).
shops(mary).
end(model(8189)).

begin(model(8190)).
bought(fish).
shops(mary).
end(model(8190)).

begin(model(8191)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8191)).

begin(model(8192)).
bought(fish).
shops(mary).
end(model(8192)).

begin(model(8193)).
bought(fish).
shops(mary).
end(model(8193)).

begin(model(8194)).
bought(fish).
shops(mary).
end(model(8194)).

begin(model(8195)).
bought(fish).
shops(mary).
end(model(8195)).

begin(model(8196)).
bought(fish).
shops(mary).
end(model(8196)).

begin(model(8197)).
bought(fish).
shops(mary).
end(model(8197)).

begin(model(8198)).
bought(spaghetti).
shops(mary).
end(model(8198)).

begin(model(8199)).
bought(fish).
shops(mary).
end(model(8199)).

begin(model(8200)).
bought(fish).
shops(mary).
end(model(8200)).

begin(model(8201)).
bought(fish).
shops(mary).
end(model(8201)).

begin(model(8202)).
bought(spaghetti).
shops(mary).
end(model(8202)).

begin(model(8203)).
end(model(8203)).

begin(model(8204)).
bought(fish).
shops(mary).
end(model(8204)).

begin(model(8205)).
bought(spaghetti).
shops(mary).
end(model(8205)).

begin(model(8206)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8206)).

begin(model(8207)).
bought(spaghetti).
shops(mary).
end(model(8207)).

begin(model(8208)).
bought(fish).
shops(mary).
end(model(8208)).

begin(model(8209)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8209)).

begin(model(8210)).
bought(fish).
shops(mary).
end(model(8210)).

begin(model(8211)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8211)).

begin(model(8212)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8212)).

begin(model(8213)).
bought(spaghetti).
shops(mary).
end(model(8213)).

begin(model(8214)).
bought(fish).
shops(mary).
end(model(8214)).

begin(model(8215)).
bought(fish).
shops(mary).
end(model(8215)).

begin(model(8216)).
bought(spaghetti).
shops(mary).
end(model(8216)).

begin(model(8217)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8217)).

begin(model(8218)).
bought(fish).
shops(mary).
end(model(8218)).

begin(model(8219)).
bought(fish).
shops(mary).
end(model(8219)).

begin(model(8220)).
bought(fish).
shops(mary).
end(model(8220)).

begin(model(8221)).
bought(spaghetti).
shops(mary).
end(model(8221)).

begin(model(8222)).
end(model(8222)).

begin(model(8223)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8223)).

begin(model(8224)).
bought(spaghetti).
shops(mary).
end(model(8224)).

begin(model(8225)).
bought(fish).
shops(mary).
end(model(8225)).

begin(model(8226)).
end(model(8226)).

begin(model(8227)).
bought(fish).
shops(mary).
end(model(8227)).

begin(model(8228)).
bought(fish).
shops(mary).
end(model(8228)).

begin(model(8229)).
bought(fish).
shops(mary).
end(model(8229)).

begin(model(8230)).
bought(fish).
shops(mary).
end(model(8230)).

begin(model(8231)).
end(model(8231)).

begin(model(8232)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8232)).

begin(model(8233)).
bought(fish).
shops(mary).
end(model(8233)).

begin(model(8234)).
bought(fish).
shops(mary).
end(model(8234)).

begin(model(8235)).
bought(fish).
shops(mary).
end(model(8235)).

begin(model(8236)).
bought(spaghetti).
shops(mary).
end(model(8236)).

begin(model(8237)).
bought(fish).
shops(mary).
end(model(8237)).

begin(model(8238)).
bought(fish).
shops(mary).
end(model(8238)).

begin(model(8239)).
bought(spaghetti).
shops(mary).
end(model(8239)).

begin(model(8240)).
bought(fish).
shops(mary).
end(model(8240)).

begin(model(8241)).
bought(spaghetti).
shops(mary).
end(model(8241)).

begin(model(8242)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8242)).

begin(model(8243)).
bought(fish).
shops(mary).
end(model(8243)).

begin(model(8244)).
bought(fish).
shops(mary).
end(model(8244)).

begin(model(8245)).
bought(spaghetti).
shops(mary).
end(model(8245)).

begin(model(8246)).
bought(spaghetti).
shops(mary).
end(model(8246)).

begin(model(8247)).
bought(spaghetti).
shops(mary).
end(model(8247)).

begin(model(8248)).
bought(fish).
shops(mary).
end(model(8248)).

begin(model(8249)).
bought(fish).
shops(mary).
end(model(8249)).

begin(model(8250)).
bought(fish).
shops(mary).
end(model(8250)).

begin(model(8251)).
bought(fish).
shops(mary).
end(model(8251)).

begin(model(8252)).
bought(fish).
shops(mary).
end(model(8252)).

begin(model(8253)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8253)).

begin(model(8254)).
bought(fish).
shops(mary).
end(model(8254)).

begin(model(8255)).
bought(spaghetti).
shops(mary).
end(model(8255)).

begin(model(8256)).
bought(fish).
shops(mary).
end(model(8256)).

begin(model(8257)).
bought(fish).
shops(mary).
end(model(8257)).

begin(model(8258)).
bought(spaghetti).
shops(mary).
end(model(8258)).

begin(model(8259)).
end(model(8259)).

begin(model(8260)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8260)).

begin(model(8261)).
bought(fish).
shops(mary).
end(model(8261)).

begin(model(8262)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8262)).

begin(model(8263)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8263)).

begin(model(8264)).
bought(spaghetti).
shops(john).
end(model(8264)).

begin(model(8265)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8265)).

begin(model(8266)).
bought(fish).
shops(mary).
end(model(8266)).

begin(model(8267)).
bought(fish).
shops(mary).
end(model(8267)).

begin(model(8268)).
bought(steak).
shops(john).
end(model(8268)).

begin(model(8269)).
bought(fish).
shops(mary).
end(model(8269)).

begin(model(8270)).
bought(fish).
shops(mary).
end(model(8270)).

begin(model(8271)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8271)).

begin(model(8272)).
bought(spaghetti).
shops(john).
end(model(8272)).

begin(model(8273)).
bought(fish).
shops(mary).
end(model(8273)).

begin(model(8274)).
bought(spaghetti).
shops(mary).
end(model(8274)).

begin(model(8275)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8275)).

begin(model(8276)).
bought(fish).
shops(mary).
end(model(8276)).

begin(model(8277)).
bought(fish).
shops(mary).
end(model(8277)).

begin(model(8278)).
bought(spaghetti).
shops(mary).
end(model(8278)).

begin(model(8279)).
bought(fish).
shops(mary).
end(model(8279)).

begin(model(8280)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8280)).

begin(model(8281)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8281)).

begin(model(8282)).
bought(fish).
shops(mary).
end(model(8282)).

begin(model(8283)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8283)).

begin(model(8284)).
bought(spaghetti).
shops(mary).
end(model(8284)).

begin(model(8285)).
bought(fish).
shops(mary).
end(model(8285)).

begin(model(8286)).
bought(fish).
shops(mary).
end(model(8286)).

begin(model(8287)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8287)).

begin(model(8288)).
bought(fish).
shops(mary).
end(model(8288)).

begin(model(8289)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8289)).

begin(model(8290)).
bought(spaghetti).
shops(mary).
end(model(8290)).

begin(model(8291)).
bought(fish).
shops(mary).
end(model(8291)).

begin(model(8292)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8292)).

begin(model(8293)).
bought(fish).
shops(mary).
end(model(8293)).

begin(model(8294)).
bought(fish).
shops(mary).
end(model(8294)).

begin(model(8295)).
bought(fish).
shops(mary).
end(model(8295)).

begin(model(8296)).
bought(spaghetti).
shops(mary).
end(model(8296)).

begin(model(8297)).
bought(spaghetti).
shops(mary).
end(model(8297)).

begin(model(8298)).
bought(spaghetti).
shops(john).
end(model(8298)).

begin(model(8299)).
bought(spaghetti).
shops(mary).
end(model(8299)).

begin(model(8300)).
bought(fish).
shops(mary).
end(model(8300)).

begin(model(8301)).
bought(fish).
shops(mary).
end(model(8301)).

begin(model(8302)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8302)).

begin(model(8303)).
bought(fish).
shops(mary).
end(model(8303)).

begin(model(8304)).
end(model(8304)).

begin(model(8305)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8305)).

begin(model(8306)).
bought(fish).
shops(mary).
end(model(8306)).

begin(model(8307)).
bought(spaghetti).
shops(mary).
end(model(8307)).

begin(model(8308)).
bought(fish).
shops(mary).
end(model(8308)).

begin(model(8309)).
bought(spaghetti).
shops(mary).
end(model(8309)).

begin(model(8310)).
bought(fish).
shops(mary).
end(model(8310)).

begin(model(8311)).
bought(fish).
shops(mary).
end(model(8311)).

begin(model(8312)).
bought(spaghetti).
shops(mary).
end(model(8312)).

begin(model(8313)).
bought(fish).
shops(mary).
end(model(8313)).

begin(model(8314)).
bought(spaghetti).
shops(john).
end(model(8314)).

begin(model(8315)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8315)).

begin(model(8316)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8316)).

begin(model(8317)).
bought(steak).
shops(john).
end(model(8317)).

begin(model(8318)).
bought(fish).
shops(mary).
end(model(8318)).

begin(model(8319)).
bought(fish).
shops(mary).
end(model(8319)).

begin(model(8320)).
bought(fish).
shops(mary).
end(model(8320)).

begin(model(8321)).
bought(spaghetti).
shops(mary).
end(model(8321)).

begin(model(8322)).
bought(fish).
shops(mary).
end(model(8322)).

begin(model(8323)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8323)).

begin(model(8324)).
bought(fish).
shops(mary).
end(model(8324)).

begin(model(8325)).
bought(fish).
shops(mary).
end(model(8325)).

begin(model(8326)).
bought(fish).
shops(mary).
end(model(8326)).

begin(model(8327)).
bought(fish).
shops(mary).
end(model(8327)).

begin(model(8328)).
bought(spaghetti).
shops(mary).
end(model(8328)).

begin(model(8329)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8329)).

begin(model(8330)).
bought(fish).
shops(mary).
end(model(8330)).

begin(model(8331)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8331)).

begin(model(8332)).
bought(fish).
shops(mary).
end(model(8332)).

begin(model(8333)).
bought(fish).
shops(mary).
end(model(8333)).

begin(model(8334)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8334)).

begin(model(8335)).
bought(fish).
shops(mary).
end(model(8335)).

begin(model(8336)).
bought(fish).
shops(mary).
end(model(8336)).

begin(model(8337)).
bought(fish).
shops(mary).
end(model(8337)).

begin(model(8338)).
bought(spaghetti).
shops(mary).
end(model(8338)).

begin(model(8339)).
bought(fish).
shops(mary).
end(model(8339)).

begin(model(8340)).
end(model(8340)).

begin(model(8341)).
bought(fish).
shops(mary).
end(model(8341)).

begin(model(8342)).
bought(fish).
shops(mary).
end(model(8342)).

begin(model(8343)).
bought(fish).
shops(mary).
end(model(8343)).

begin(model(8344)).
bought(fish).
shops(mary).
end(model(8344)).

begin(model(8345)).
bought(spaghetti).
shops(mary).
end(model(8345)).

begin(model(8346)).
end(model(8346)).

begin(model(8347)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8347)).

begin(model(8348)).
bought(fish).
shops(mary).
end(model(8348)).

begin(model(8349)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8349)).

begin(model(8350)).
bought(spaghetti).
shops(mary).
end(model(8350)).

begin(model(8351)).
bought(fish).
shops(mary).
end(model(8351)).

begin(model(8352)).
end(model(8352)).

begin(model(8353)).
bought(fish).
shops(mary).
end(model(8353)).

begin(model(8354)).
bought(fish).
shops(mary).
end(model(8354)).

begin(model(8355)).
bought(fish).
shops(mary).
end(model(8355)).

begin(model(8356)).
bought(fish).
shops(mary).
end(model(8356)).

begin(model(8357)).
bought(fish).
shops(mary).
end(model(8357)).

begin(model(8358)).
bought(fish).
shops(mary).
end(model(8358)).

begin(model(8359)).
bought(fish).
shops(mary).
end(model(8359)).

begin(model(8360)).
bought(steak).
shops(john).
end(model(8360)).

begin(model(8361)).
bought(fish).
shops(mary).
end(model(8361)).

begin(model(8362)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8362)).

begin(model(8363)).
bought(spaghetti).
shops(mary).
end(model(8363)).

begin(model(8364)).
bought(spaghetti).
shops(mary).
end(model(8364)).

begin(model(8365)).
bought(fish).
shops(mary).
end(model(8365)).

begin(model(8366)).
bought(fish).
shops(mary).
end(model(8366)).

begin(model(8367)).
bought(fish).
shops(mary).
end(model(8367)).

begin(model(8368)).
bought(fish).
shops(mary).
end(model(8368)).

begin(model(8369)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8369)).

begin(model(8370)).
bought(fish).
shops(mary).
end(model(8370)).

begin(model(8371)).
bought(fish).
shops(mary).
end(model(8371)).

begin(model(8372)).
bought(spaghetti).
shops(mary).
end(model(8372)).

begin(model(8373)).
bought(fish).
shops(mary).
end(model(8373)).

begin(model(8374)).
bought(spaghetti).
shops(mary).
end(model(8374)).

begin(model(8375)).
end(model(8375)).

begin(model(8376)).
bought(fish).
shops(mary).
end(model(8376)).

begin(model(8377)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8377)).

begin(model(8378)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8378)).

begin(model(8379)).
bought(fish).
shops(mary).
end(model(8379)).

begin(model(8380)).
bought(fish).
shops(mary).
end(model(8380)).

begin(model(8381)).
bought(fish).
shops(mary).
end(model(8381)).

begin(model(8382)).
end(model(8382)).

begin(model(8383)).
bought(spaghetti).
shops(mary).
end(model(8383)).

begin(model(8384)).
bought(fish).
shops(mary).
end(model(8384)).

begin(model(8385)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8385)).

begin(model(8386)).
bought(fish).
shops(mary).
end(model(8386)).

begin(model(8387)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8387)).

begin(model(8388)).
bought(spaghetti).
shops(mary).
end(model(8388)).

begin(model(8389)).
bought(spaghetti).
shops(mary).
end(model(8389)).

begin(model(8390)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8390)).

begin(model(8391)).
bought(spaghetti).
shops(mary).
end(model(8391)).

begin(model(8392)).
bought(spaghetti).
shops(mary).
end(model(8392)).

begin(model(8393)).
bought(fish).
shops(mary).
end(model(8393)).

begin(model(8394)).
bought(fish).
shops(mary).
end(model(8394)).

begin(model(8395)).
bought(fish).
shops(mary).
end(model(8395)).

begin(model(8396)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8396)).

begin(model(8397)).
bought(fish).
shops(mary).
end(model(8397)).

begin(model(8398)).
bought(spaghetti).
shops(mary).
end(model(8398)).

begin(model(8399)).
bought(fish).
shops(mary).
end(model(8399)).

begin(model(8400)).
bought(fish).
shops(mary).
end(model(8400)).

begin(model(8401)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8401)).

begin(model(8402)).
bought(fish).
shops(mary).
end(model(8402)).

begin(model(8403)).
end(model(8403)).

begin(model(8404)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8404)).

begin(model(8405)).
bought(fish).
shops(mary).
end(model(8405)).

begin(model(8406)).
bought(fish).
shops(mary).
end(model(8406)).

begin(model(8407)).
bought(fish).
shops(mary).
end(model(8407)).

begin(model(8408)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8408)).

begin(model(8409)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8409)).

begin(model(8410)).
bought(spaghetti).
shops(mary).
end(model(8410)).

begin(model(8411)).
bought(spaghetti).
shops(mary).
end(model(8411)).

begin(model(8412)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8412)).

begin(model(8413)).
bought(fish).
shops(mary).
end(model(8413)).

begin(model(8414)).
bought(fish).
shops(mary).
end(model(8414)).

begin(model(8415)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8415)).

begin(model(8416)).
end(model(8416)).

begin(model(8417)).
bought(spaghetti).
shops(mary).
end(model(8417)).

begin(model(8418)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8418)).

begin(model(8419)).
bought(spaghetti).
shops(mary).
end(model(8419)).

begin(model(8420)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8420)).

begin(model(8421)).
bought(fish).
shops(mary).
end(model(8421)).

begin(model(8422)).
bought(fish).
shops(mary).
end(model(8422)).

begin(model(8423)).
bought(spaghetti).
shops(mary).
end(model(8423)).

begin(model(8424)).
bought(spaghetti).
shops(mary).
end(model(8424)).

begin(model(8425)).
end(model(8425)).

begin(model(8426)).
bought(fish).
shops(mary).
end(model(8426)).

begin(model(8427)).
bought(spaghetti).
shops(mary).
end(model(8427)).

begin(model(8428)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8428)).

begin(model(8429)).
bought(fish).
shops(mary).
end(model(8429)).

begin(model(8430)).
end(model(8430)).

begin(model(8431)).
bought(fish).
shops(mary).
end(model(8431)).

begin(model(8432)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8432)).

begin(model(8433)).
bought(fish).
shops(mary).
end(model(8433)).

begin(model(8434)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8434)).

begin(model(8435)).
bought(spaghetti).
shops(mary).
end(model(8435)).

begin(model(8436)).
bought(fish).
shops(mary).
end(model(8436)).

begin(model(8437)).
bought(fish).
shops(mary).
end(model(8437)).

begin(model(8438)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8438)).

begin(model(8439)).
bought(fish).
shops(mary).
end(model(8439)).

begin(model(8440)).
bought(spaghetti).
shops(mary).
end(model(8440)).

begin(model(8441)).
bought(fish).
shops(mary).
end(model(8441)).

begin(model(8442)).
bought(spaghetti).
shops(mary).
end(model(8442)).

begin(model(8443)).
bought(fish).
shops(mary).
end(model(8443)).

begin(model(8444)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8444)).

begin(model(8445)).
bought(fish).
shops(mary).
end(model(8445)).

begin(model(8446)).
bought(fish).
shops(mary).
end(model(8446)).

begin(model(8447)).
bought(fish).
shops(mary).
end(model(8447)).

begin(model(8448)).
bought(fish).
shops(mary).
end(model(8448)).

begin(model(8449)).
bought(fish).
shops(mary).
end(model(8449)).

begin(model(8450)).
bought(fish).
shops(mary).
end(model(8450)).

begin(model(8451)).
bought(fish).
shops(mary).
end(model(8451)).

begin(model(8452)).
bought(fish).
shops(mary).
end(model(8452)).

begin(model(8453)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8453)).

begin(model(8454)).
bought(fish).
shops(mary).
end(model(8454)).

begin(model(8455)).
bought(spaghetti).
shops(mary).
end(model(8455)).

begin(model(8456)).
bought(fish).
shops(mary).
end(model(8456)).

begin(model(8457)).
bought(spaghetti).
shops(mary).
end(model(8457)).

begin(model(8458)).
end(model(8458)).

begin(model(8459)).
bought(spaghetti).
shops(mary).
end(model(8459)).

begin(model(8460)).
bought(spaghetti).
shops(mary).
end(model(8460)).

begin(model(8461)).
bought(fish).
shops(mary).
end(model(8461)).

begin(model(8462)).
bought(spaghetti).
shops(mary).
end(model(8462)).

begin(model(8463)).
bought(fish).
shops(mary).
end(model(8463)).

begin(model(8464)).
bought(fish).
shops(mary).
end(model(8464)).

begin(model(8465)).
bought(fish).
shops(mary).
end(model(8465)).

begin(model(8466)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8466)).

begin(model(8467)).
bought(spaghetti).
shops(mary).
end(model(8467)).

begin(model(8468)).
bought(fish).
shops(mary).
end(model(8468)).

begin(model(8469)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8469)).

begin(model(8470)).
bought(spaghetti).
shops(mary).
end(model(8470)).

begin(model(8471)).
bought(fish).
shops(mary).
end(model(8471)).

begin(model(8472)).
bought(fish).
shops(mary).
end(model(8472)).

begin(model(8473)).
bought(fish).
shops(mary).
end(model(8473)).

begin(model(8474)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8474)).

begin(model(8475)).
bought(spaghetti).
shops(john).
end(model(8475)).

begin(model(8476)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8476)).

begin(model(8477)).
bought(fish).
shops(mary).
end(model(8477)).

begin(model(8478)).
bought(fish).
shops(mary).
end(model(8478)).

begin(model(8479)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8479)).

begin(model(8480)).
bought(fish).
shops(mary).
end(model(8480)).

begin(model(8481)).
bought(fish).
shops(mary).
end(model(8481)).

begin(model(8482)).
bought(spaghetti).
shops(john).
end(model(8482)).

begin(model(8483)).
bought(fish).
shops(mary).
end(model(8483)).

begin(model(8484)).
bought(fish).
shops(mary).
end(model(8484)).

begin(model(8485)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8485)).

begin(model(8486)).
bought(fish).
shops(mary).
end(model(8486)).

begin(model(8487)).
bought(fish).
shops(mary).
end(model(8487)).

begin(model(8488)).
bought(steak).
shops(john).
end(model(8488)).

begin(model(8489)).
bought(fish).
shops(mary).
end(model(8489)).

begin(model(8490)).
bought(fish).
shops(mary).
end(model(8490)).

begin(model(8491)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8491)).

begin(model(8492)).
bought(fish).
shops(mary).
end(model(8492)).

begin(model(8493)).
bought(spaghetti).
shops(mary).
end(model(8493)).

begin(model(8494)).
bought(spaghetti).
shops(mary).
end(model(8494)).

begin(model(8495)).
bought(fish).
shops(mary).
end(model(8495)).

begin(model(8496)).
bought(fish).
shops(mary).
end(model(8496)).

begin(model(8497)).
end(model(8497)).

begin(model(8498)).
bought(fish).
shops(mary).
end(model(8498)).

begin(model(8499)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8499)).

begin(model(8500)).
bought(fish).
shops(mary).
end(model(8500)).

begin(model(8501)).
bought(fish).
shops(mary).
end(model(8501)).

begin(model(8502)).
bought(fish).
shops(mary).
end(model(8502)).

begin(model(8503)).
bought(fish).
shops(mary).
end(model(8503)).

begin(model(8504)).
bought(spaghetti).
shops(mary).
end(model(8504)).

begin(model(8505)).
bought(spaghetti).
shops(mary).
end(model(8505)).

begin(model(8506)).
bought(fish).
shops(mary).
end(model(8506)).

begin(model(8507)).
bought(fish).
shops(mary).
end(model(8507)).

begin(model(8508)).
bought(fish).
shops(mary).
end(model(8508)).

begin(model(8509)).
bought(fish).
shops(mary).
end(model(8509)).

begin(model(8510)).
bought(steak).
shops(john).
end(model(8510)).

begin(model(8511)).
bought(fish).
shops(mary).
end(model(8511)).

begin(model(8512)).
bought(fish).
shops(mary).
end(model(8512)).

begin(model(8513)).
bought(fish).
shops(mary).
end(model(8513)).

begin(model(8514)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8514)).

begin(model(8515)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8515)).

begin(model(8516)).
end(model(8516)).

begin(model(8517)).
bought(spaghetti).
shops(mary).
end(model(8517)).

begin(model(8518)).
bought(fish).
shops(mary).
end(model(8518)).

begin(model(8519)).
bought(spaghetti).
shops(mary).
end(model(8519)).

begin(model(8520)).
end(model(8520)).

begin(model(8521)).
bought(spaghetti).
shops(mary).
end(model(8521)).

begin(model(8522)).
bought(fish).
shops(mary).
end(model(8522)).

begin(model(8523)).
end(model(8523)).

begin(model(8524)).
end(model(8524)).

begin(model(8525)).
bought(fish).
shops(mary).
end(model(8525)).

begin(model(8526)).
bought(fish).
shops(mary).
end(model(8526)).

begin(model(8527)).
bought(fish).
shops(mary).
end(model(8527)).

begin(model(8528)).
end(model(8528)).

begin(model(8529)).
bought(fish).
shops(mary).
end(model(8529)).

begin(model(8530)).
end(model(8530)).

begin(model(8531)).
bought(spaghetti).
shops(mary).
end(model(8531)).

begin(model(8532)).
bought(spaghetti).
shops(mary).
end(model(8532)).

begin(model(8533)).
bought(fish).
shops(mary).
end(model(8533)).

begin(model(8534)).
end(model(8534)).

begin(model(8535)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8535)).

begin(model(8536)).
bought(spaghetti).
shops(mary).
end(model(8536)).

begin(model(8537)).
bought(fish).
shops(mary).
end(model(8537)).

begin(model(8538)).
bought(fish).
shops(mary).
end(model(8538)).

begin(model(8539)).
end(model(8539)).

begin(model(8540)).
bought(spaghetti).
shops(mary).
end(model(8540)).

begin(model(8541)).
bought(spaghetti).
shops(mary).
end(model(8541)).

begin(model(8542)).
bought(fish).
shops(mary).
end(model(8542)).

begin(model(8543)).
bought(spaghetti).
shops(mary).
end(model(8543)).

begin(model(8544)).
bought(spaghetti).
shops(mary).
end(model(8544)).

begin(model(8545)).
bought(spaghetti).
shops(mary).
end(model(8545)).

begin(model(8546)).
bought(fish).
shops(mary).
end(model(8546)).

begin(model(8547)).
bought(fish).
shops(mary).
end(model(8547)).

begin(model(8548)).
bought(fish).
shops(mary).
end(model(8548)).

begin(model(8549)).
bought(spaghetti).
shops(mary).
end(model(8549)).

begin(model(8550)).
bought(spaghetti).
shops(mary).
end(model(8550)).

begin(model(8551)).
bought(fish).
shops(mary).
end(model(8551)).

begin(model(8552)).
bought(fish).
shops(mary).
end(model(8552)).

begin(model(8553)).
bought(fish).
shops(mary).
end(model(8553)).

begin(model(8554)).
bought(spaghetti).
shops(mary).
end(model(8554)).

begin(model(8555)).
bought(fish).
shops(mary).
end(model(8555)).

begin(model(8556)).
bought(fish).
shops(mary).
end(model(8556)).

begin(model(8557)).
bought(fish).
shops(mary).
end(model(8557)).

begin(model(8558)).
bought(fish).
shops(mary).
end(model(8558)).

begin(model(8559)).
bought(spaghetti).
shops(mary).
end(model(8559)).

begin(model(8560)).
bought(spaghetti).
shops(mary).
end(model(8560)).

begin(model(8561)).
end(model(8561)).

begin(model(8562)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8562)).

begin(model(8563)).
bought(fish).
shops(mary).
end(model(8563)).

begin(model(8564)).
bought(fish).
shops(mary).
end(model(8564)).

begin(model(8565)).
end(model(8565)).

begin(model(8566)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8566)).

begin(model(8567)).
bought(fish).
shops(mary).
end(model(8567)).

begin(model(8568)).
bought(spaghetti).
shops(mary).
end(model(8568)).

begin(model(8569)).
bought(spaghetti).
shops(mary).
end(model(8569)).

begin(model(8570)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8570)).

begin(model(8571)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8571)).

begin(model(8572)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8572)).

begin(model(8573)).
end(model(8573)).

begin(model(8574)).
bought(fish).
shops(mary).
end(model(8574)).

begin(model(8575)).
end(model(8575)).

begin(model(8576)).
bought(spaghetti).
shops(mary).
end(model(8576)).

begin(model(8577)).
bought(spaghetti).
shops(mary).
end(model(8577)).

begin(model(8578)).
bought(fish).
shops(mary).
end(model(8578)).

begin(model(8579)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8579)).

begin(model(8580)).
bought(fish).
shops(mary).
end(model(8580)).

begin(model(8581)).
bought(fish).
shops(mary).
end(model(8581)).

begin(model(8582)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8582)).

begin(model(8583)).
bought(fish).
shops(mary).
end(model(8583)).

begin(model(8584)).
bought(spaghetti).
shops(mary).
end(model(8584)).

begin(model(8585)).
bought(spaghetti).
shops(mary).
end(model(8585)).

begin(model(8586)).
bought(fish).
shops(mary).
end(model(8586)).

begin(model(8587)).
bought(spaghetti).
shops(mary).
end(model(8587)).

begin(model(8588)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8588)).

begin(model(8589)).
bought(fish).
shops(mary).
end(model(8589)).

begin(model(8590)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8590)).

begin(model(8591)).
bought(fish).
shops(mary).
end(model(8591)).

begin(model(8592)).
bought(fish).
shops(mary).
end(model(8592)).

begin(model(8593)).
bought(fish).
shops(mary).
end(model(8593)).

begin(model(8594)).
bought(fish).
shops(mary).
end(model(8594)).

begin(model(8595)).
bought(spaghetti).
shops(mary).
end(model(8595)).

begin(model(8596)).
bought(fish).
shops(mary).
end(model(8596)).

begin(model(8597)).
bought(fish).
shops(mary).
end(model(8597)).

begin(model(8598)).
bought(fish).
shops(mary).
end(model(8598)).

begin(model(8599)).
bought(fish).
shops(mary).
end(model(8599)).

begin(model(8600)).
bought(fish).
shops(mary).
end(model(8600)).

begin(model(8601)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8601)).

begin(model(8602)).
bought(fish).
shops(mary).
end(model(8602)).

begin(model(8603)).
bought(spaghetti).
shops(mary).
end(model(8603)).

begin(model(8604)).
bought(spaghetti).
shops(mary).
end(model(8604)).

begin(model(8605)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8605)).

begin(model(8606)).
bought(spaghetti).
shops(mary).
end(model(8606)).

begin(model(8607)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8607)).

begin(model(8608)).
bought(spaghetti).
shops(mary).
end(model(8608)).

begin(model(8609)).
bought(fish).
shops(mary).
end(model(8609)).

begin(model(8610)).
bought(fish).
shops(mary).
end(model(8610)).

begin(model(8611)).
bought(spaghetti).
shops(mary).
end(model(8611)).

begin(model(8612)).
bought(fish).
shops(mary).
end(model(8612)).

begin(model(8613)).
bought(spaghetti).
shops(mary).
end(model(8613)).

begin(model(8614)).
bought(fish).
shops(mary).
end(model(8614)).

begin(model(8615)).
bought(fish).
shops(mary).
end(model(8615)).

begin(model(8616)).
bought(fish).
shops(mary).
end(model(8616)).

begin(model(8617)).
bought(spaghetti).
shops(mary).
end(model(8617)).

begin(model(8618)).
bought(fish).
shops(mary).
end(model(8618)).

begin(model(8619)).
bought(fish).
shops(mary).
end(model(8619)).

begin(model(8620)).
bought(fish).
shops(mary).
end(model(8620)).

begin(model(8621)).
bought(fish).
shops(mary).
end(model(8621)).

begin(model(8622)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8622)).

begin(model(8623)).
bought(spaghetti).
shops(mary).
end(model(8623)).

begin(model(8624)).
bought(spaghetti).
shops(mary).
end(model(8624)).

begin(model(8625)).
bought(fish).
shops(mary).
end(model(8625)).

begin(model(8626)).
end(model(8626)).

begin(model(8627)).
bought(fish).
shops(mary).
end(model(8627)).

begin(model(8628)).
bought(spaghetti).
shops(mary).
end(model(8628)).

begin(model(8629)).
bought(fish).
shops(mary).
end(model(8629)).

begin(model(8630)).
bought(spaghetti).
shops(mary).
end(model(8630)).

begin(model(8631)).
bought(fish).
shops(mary).
end(model(8631)).

begin(model(8632)).
bought(fish).
shops(mary).
end(model(8632)).

begin(model(8633)).
bought(fish).
shops(mary).
end(model(8633)).

begin(model(8634)).
bought(spaghetti).
shops(mary).
end(model(8634)).

begin(model(8635)).
bought(fish).
shops(mary).
end(model(8635)).

begin(model(8636)).
bought(fish).
shops(mary).
end(model(8636)).

begin(model(8637)).
bought(spaghetti).
shops(mary).
end(model(8637)).

begin(model(8638)).
bought(fish).
shops(mary).
end(model(8638)).

begin(model(8639)).
bought(spaghetti).
shops(mary).
end(model(8639)).

begin(model(8640)).
bought(fish).
shops(mary).
end(model(8640)).

begin(model(8641)).
bought(spaghetti).
shops(mary).
end(model(8641)).

begin(model(8642)).
bought(fish).
shops(mary).
end(model(8642)).

begin(model(8643)).
bought(fish).
shops(mary).
end(model(8643)).

begin(model(8644)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8644)).

begin(model(8645)).
bought(spaghetti).
shops(mary).
end(model(8645)).

begin(model(8646)).
bought(fish).
shops(mary).
end(model(8646)).

begin(model(8647)).
end(model(8647)).

begin(model(8648)).
bought(fish).
shops(mary).
end(model(8648)).

begin(model(8649)).
end(model(8649)).

begin(model(8650)).
bought(fish).
shops(mary).
end(model(8650)).

begin(model(8651)).
bought(fish).
shops(mary).
end(model(8651)).

begin(model(8652)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8652)).

begin(model(8653)).
bought(fish).
shops(mary).
end(model(8653)).

begin(model(8654)).
bought(spaghetti).
shops(mary).
end(model(8654)).

begin(model(8655)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8655)).

begin(model(8656)).
end(model(8656)).

begin(model(8657)).
bought(steak).
shops(john).
end(model(8657)).

begin(model(8658)).
bought(fish).
shops(mary).
end(model(8658)).

begin(model(8659)).
bought(fish).
shops(mary).
end(model(8659)).

begin(model(8660)).
bought(fish).
shops(mary).
end(model(8660)).

begin(model(8661)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8661)).

begin(model(8662)).
bought(fish).
shops(mary).
end(model(8662)).

begin(model(8663)).
bought(spaghetti).
shops(mary).
end(model(8663)).

begin(model(8664)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8664)).

begin(model(8665)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8665)).

begin(model(8666)).
bought(fish).
shops(mary).
end(model(8666)).

begin(model(8667)).
bought(fish).
shops(mary).
end(model(8667)).

begin(model(8668)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8668)).

begin(model(8669)).
bought(fish).
shops(mary).
end(model(8669)).

begin(model(8670)).
bought(fish).
shops(mary).
end(model(8670)).

begin(model(8671)).
bought(spaghetti).
shops(mary).
end(model(8671)).

begin(model(8672)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8672)).

begin(model(8673)).
bought(spaghetti).
shops(mary).
end(model(8673)).

begin(model(8674)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8674)).

begin(model(8675)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8675)).

begin(model(8676)).
bought(fish).
shops(mary).
end(model(8676)).

begin(model(8677)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8677)).

begin(model(8678)).
bought(fish).
shops(mary).
end(model(8678)).

begin(model(8679)).
bought(spaghetti).
shops(mary).
end(model(8679)).

begin(model(8680)).
end(model(8680)).

begin(model(8681)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8681)).

begin(model(8682)).
end(model(8682)).

begin(model(8683)).
bought(fish).
shops(mary).
end(model(8683)).

begin(model(8684)).
bought(fish).
shops(mary).
end(model(8684)).

begin(model(8685)).
bought(spaghetti).
shops(mary).
end(model(8685)).

begin(model(8686)).
bought(fish).
shops(mary).
end(model(8686)).

begin(model(8687)).
bought(spaghetti).
shops(mary).
end(model(8687)).

begin(model(8688)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8688)).

begin(model(8689)).
end(model(8689)).

begin(model(8690)).
bought(spaghetti).
shops(mary).
end(model(8690)).

begin(model(8691)).
end(model(8691)).

begin(model(8692)).
bought(fish).
shops(mary).
end(model(8692)).

begin(model(8693)).
bought(steak).
shops(john).
end(model(8693)).

begin(model(8694)).
bought(fish).
shops(mary).
end(model(8694)).

begin(model(8695)).
end(model(8695)).

begin(model(8696)).
bought(fish).
shops(mary).
end(model(8696)).

begin(model(8697)).
bought(fish).
shops(mary).
end(model(8697)).

begin(model(8698)).
bought(spaghetti).
shops(mary).
end(model(8698)).

begin(model(8699)).
bought(spaghetti).
shops(mary).
end(model(8699)).

begin(model(8700)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8700)).

begin(model(8701)).
end(model(8701)).

begin(model(8702)).
bought(fish).
shops(mary).
end(model(8702)).

begin(model(8703)).
bought(spaghetti).
shops(mary).
end(model(8703)).

begin(model(8704)).
bought(fish).
shops(mary).
end(model(8704)).

begin(model(8705)).
bought(fish).
shops(mary).
end(model(8705)).

begin(model(8706)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8706)).

begin(model(8707)).
end(model(8707)).

begin(model(8708)).
bought(fish).
shops(mary).
end(model(8708)).

begin(model(8709)).
end(model(8709)).

begin(model(8710)).
bought(spaghetti).
shops(mary).
end(model(8710)).

begin(model(8711)).
end(model(8711)).

begin(model(8712)).
bought(fish).
shops(mary).
end(model(8712)).

begin(model(8713)).
bought(fish).
shops(mary).
end(model(8713)).

begin(model(8714)).
bought(spaghetti).
shops(mary).
end(model(8714)).

begin(model(8715)).
bought(fish).
shops(mary).
end(model(8715)).

begin(model(8716)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8716)).

begin(model(8717)).
bought(fish).
shops(mary).
end(model(8717)).

begin(model(8718)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8718)).

begin(model(8719)).
bought(spaghetti).
shops(mary).
end(model(8719)).

begin(model(8720)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8720)).

begin(model(8721)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8721)).

begin(model(8722)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8722)).

begin(model(8723)).
bought(fish).
shops(mary).
end(model(8723)).

begin(model(8724)).
bought(fish).
shops(mary).
end(model(8724)).

begin(model(8725)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8725)).

begin(model(8726)).
bought(spaghetti).
shops(mary).
end(model(8726)).

begin(model(8727)).
bought(fish).
shops(mary).
end(model(8727)).

begin(model(8728)).
bought(fish).
shops(mary).
end(model(8728)).

begin(model(8729)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8729)).

begin(model(8730)).
bought(spaghetti).
shops(mary).
end(model(8730)).

begin(model(8731)).
bought(fish).
shops(mary).
end(model(8731)).

begin(model(8732)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8732)).

begin(model(8733)).
bought(spaghetti).
shops(mary).
end(model(8733)).

begin(model(8734)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8734)).

begin(model(8735)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8735)).

begin(model(8736)).
bought(fish).
shops(mary).
end(model(8736)).

begin(model(8737)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8737)).

begin(model(8738)).
bought(fish).
shops(mary).
end(model(8738)).

begin(model(8739)).
bought(fish).
shops(mary).
end(model(8739)).

begin(model(8740)).
bought(spaghetti).
shops(mary).
end(model(8740)).

begin(model(8741)).
bought(fish).
shops(mary).
end(model(8741)).

begin(model(8742)).
end(model(8742)).

begin(model(8743)).
bought(spaghetti).
shops(mary).
end(model(8743)).

begin(model(8744)).
bought(fish).
shops(mary).
end(model(8744)).

begin(model(8745)).
bought(spaghetti).
shops(mary).
end(model(8745)).

begin(model(8746)).
bought(fish).
shops(mary).
end(model(8746)).

begin(model(8747)).
bought(fish).
shops(mary).
end(model(8747)).

begin(model(8748)).
bought(fish).
shops(mary).
end(model(8748)).

begin(model(8749)).
bought(fish).
shops(mary).
end(model(8749)).

begin(model(8750)).
bought(fish).
shops(mary).
end(model(8750)).

begin(model(8751)).
bought(fish).
shops(mary).
end(model(8751)).

begin(model(8752)).
bought(fish).
shops(mary).
end(model(8752)).

begin(model(8753)).
bought(fish).
shops(mary).
end(model(8753)).

begin(model(8754)).
bought(spaghetti).
shops(mary).
end(model(8754)).

begin(model(8755)).
bought(fish).
shops(mary).
end(model(8755)).

begin(model(8756)).
bought(fish).
shops(mary).
end(model(8756)).

begin(model(8757)).
bought(fish).
shops(mary).
end(model(8757)).

begin(model(8758)).
bought(fish).
shops(mary).
end(model(8758)).

begin(model(8759)).
bought(spaghetti).
shops(mary).
end(model(8759)).

begin(model(8760)).
end(model(8760)).

begin(model(8761)).
bought(fish).
shops(mary).
end(model(8761)).

begin(model(8762)).
bought(fish).
shops(mary).
end(model(8762)).

begin(model(8763)).
end(model(8763)).

begin(model(8764)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8764)).

begin(model(8765)).
bought(fish).
shops(mary).
end(model(8765)).

begin(model(8766)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8766)).

begin(model(8767)).
bought(spaghetti).
shops(mary).
end(model(8767)).

begin(model(8768)).
end(model(8768)).

begin(model(8769)).
bought(fish).
shops(mary).
end(model(8769)).

begin(model(8770)).
bought(fish).
shops(mary).
end(model(8770)).

begin(model(8771)).
bought(spaghetti).
shops(mary).
end(model(8771)).

begin(model(8772)).
bought(fish).
shops(mary).
end(model(8772)).

begin(model(8773)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8773)).

begin(model(8774)).
bought(fish).
shops(mary).
end(model(8774)).

begin(model(8775)).
bought(fish).
shops(mary).
end(model(8775)).

begin(model(8776)).
bought(spaghetti).
shops(mary).
end(model(8776)).

begin(model(8777)).
bought(spaghetti).
shops(mary).
end(model(8777)).

begin(model(8778)).
bought(spaghetti).
shops(mary).
end(model(8778)).

begin(model(8779)).
bought(spaghetti).
shops(mary).
end(model(8779)).

begin(model(8780)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8780)).

begin(model(8781)).
bought(fish).
shops(mary).
end(model(8781)).

begin(model(8782)).
bought(fish).
shops(mary).
end(model(8782)).

begin(model(8783)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8783)).

begin(model(8784)).
bought(fish).
shops(mary).
end(model(8784)).

begin(model(8785)).
bought(fish).
shops(mary).
end(model(8785)).

begin(model(8786)).
bought(fish).
shops(mary).
end(model(8786)).

begin(model(8787)).
bought(steak).
shops(john).
end(model(8787)).

begin(model(8788)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8788)).

begin(model(8789)).
bought(fish).
shops(mary).
end(model(8789)).

begin(model(8790)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8790)).

begin(model(8791)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8791)).

begin(model(8792)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8792)).

begin(model(8793)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8793)).

begin(model(8794)).
bought(fish).
shops(mary).
end(model(8794)).

begin(model(8795)).
bought(spaghetti).
shops(mary).
end(model(8795)).

begin(model(8796)).
bought(fish).
shops(mary).
end(model(8796)).

begin(model(8797)).
bought(fish).
shops(mary).
end(model(8797)).

begin(model(8798)).
bought(spaghetti).
shops(mary).
end(model(8798)).

begin(model(8799)).
bought(spaghetti).
shops(mary).
end(model(8799)).

begin(model(8800)).
end(model(8800)).

begin(model(8801)).
bought(fish).
shops(mary).
end(model(8801)).

begin(model(8802)).
bought(fish).
shops(mary).
end(model(8802)).

begin(model(8803)).
bought(spaghetti).
shops(mary).
end(model(8803)).

begin(model(8804)).
bought(fish).
shops(mary).
end(model(8804)).

begin(model(8805)).
bought(fish).
shops(mary).
end(model(8805)).

begin(model(8806)).
bought(fish).
shops(mary).
end(model(8806)).

begin(model(8807)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8807)).

begin(model(8808)).
bought(spaghetti).
shops(mary).
end(model(8808)).

begin(model(8809)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8809)).

begin(model(8810)).
bought(fish).
shops(mary).
end(model(8810)).

begin(model(8811)).
bought(spaghetti).
shops(mary).
end(model(8811)).

begin(model(8812)).
bought(fish).
shops(mary).
end(model(8812)).

begin(model(8813)).
bought(fish).
shops(mary).
end(model(8813)).

begin(model(8814)).
bought(fish).
shops(mary).
end(model(8814)).

begin(model(8815)).
bought(fish).
shops(mary).
end(model(8815)).

begin(model(8816)).
bought(fish).
shops(mary).
end(model(8816)).

begin(model(8817)).
bought(spaghetti).
shops(mary).
end(model(8817)).

begin(model(8818)).
bought(fish).
shops(mary).
end(model(8818)).

begin(model(8819)).
bought(spaghetti).
shops(mary).
end(model(8819)).

begin(model(8820)).
bought(fish).
shops(mary).
end(model(8820)).

begin(model(8821)).
bought(spaghetti).
shops(mary).
end(model(8821)).

begin(model(8822)).
bought(fish).
shops(mary).
end(model(8822)).

begin(model(8823)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8823)).

begin(model(8824)).
bought(fish).
shops(mary).
end(model(8824)).

begin(model(8825)).
bought(fish).
shops(mary).
end(model(8825)).

begin(model(8826)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8826)).

begin(model(8827)).
end(model(8827)).

begin(model(8828)).
bought(fish).
shops(mary).
end(model(8828)).

begin(model(8829)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8829)).

begin(model(8830)).
end(model(8830)).

begin(model(8831)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8831)).

begin(model(8832)).
end(model(8832)).

begin(model(8833)).
bought(fish).
shops(mary).
end(model(8833)).

begin(model(8834)).
bought(fish).
shops(mary).
end(model(8834)).

begin(model(8835)).
bought(fish).
shops(mary).
end(model(8835)).

begin(model(8836)).
end(model(8836)).

begin(model(8837)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8837)).

begin(model(8838)).
bought(fish).
shops(mary).
end(model(8838)).

begin(model(8839)).
bought(spaghetti).
shops(mary).
end(model(8839)).

begin(model(8840)).
bought(fish).
shops(mary).
end(model(8840)).

begin(model(8841)).
bought(spaghetti).
shops(mary).
end(model(8841)).

begin(model(8842)).
bought(fish).
shops(mary).
end(model(8842)).

begin(model(8843)).
bought(fish).
shops(mary).
end(model(8843)).

begin(model(8844)).
bought(fish).
shops(mary).
end(model(8844)).

begin(model(8845)).
bought(spaghetti).
shops(mary).
end(model(8845)).

begin(model(8846)).
bought(fish).
shops(mary).
end(model(8846)).

begin(model(8847)).
bought(fish).
shops(mary).
end(model(8847)).

begin(model(8848)).
end(model(8848)).

begin(model(8849)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8849)).

begin(model(8850)).
bought(spaghetti).
shops(mary).
end(model(8850)).

begin(model(8851)).
bought(fish).
shops(mary).
end(model(8851)).

begin(model(8852)).
bought(fish).
shops(mary).
end(model(8852)).

begin(model(8853)).
end(model(8853)).

begin(model(8854)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8854)).

begin(model(8855)).
bought(fish).
shops(mary).
end(model(8855)).

begin(model(8856)).
bought(spaghetti).
shops(mary).
end(model(8856)).

begin(model(8857)).
bought(fish).
shops(mary).
end(model(8857)).

begin(model(8858)).
bought(fish).
shops(mary).
end(model(8858)).

begin(model(8859)).
bought(spaghetti).
shops(mary).
end(model(8859)).

begin(model(8860)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8860)).

begin(model(8861)).
bought(fish).
shops(mary).
end(model(8861)).

begin(model(8862)).
bought(fish).
shops(mary).
end(model(8862)).

begin(model(8863)).
bought(fish).
shops(mary).
end(model(8863)).

begin(model(8864)).
bought(fish).
shops(mary).
end(model(8864)).

begin(model(8865)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8865)).

begin(model(8866)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8866)).

begin(model(8867)).
bought(fish).
shops(mary).
end(model(8867)).

begin(model(8868)).
bought(fish).
shops(mary).
end(model(8868)).

begin(model(8869)).
bought(fish).
shops(mary).
end(model(8869)).

begin(model(8870)).
bought(fish).
shops(mary).
end(model(8870)).

begin(model(8871)).
bought(fish).
shops(mary).
end(model(8871)).

begin(model(8872)).
bought(fish).
shops(mary).
end(model(8872)).

begin(model(8873)).
bought(spaghetti).
shops(mary).
end(model(8873)).

begin(model(8874)).
bought(fish).
shops(mary).
end(model(8874)).

begin(model(8875)).
bought(fish).
shops(mary).
end(model(8875)).

begin(model(8876)).
bought(fish).
shops(mary).
end(model(8876)).

begin(model(8877)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8877)).

begin(model(8878)).
bought(spaghetti).
shops(mary).
end(model(8878)).

begin(model(8879)).
bought(fish).
shops(mary).
end(model(8879)).

begin(model(8880)).
bought(fish).
shops(mary).
end(model(8880)).

begin(model(8881)).
bought(fish).
shops(mary).
end(model(8881)).

begin(model(8882)).
bought(fish).
shops(mary).
end(model(8882)).

begin(model(8883)).
bought(spaghetti).
shops(mary).
end(model(8883)).

begin(model(8884)).
bought(spaghetti).
shops(mary).
end(model(8884)).

begin(model(8885)).
bought(fish).
shops(mary).
end(model(8885)).

begin(model(8886)).
bought(fish).
shops(mary).
end(model(8886)).

begin(model(8887)).
end(model(8887)).

begin(model(8888)).
bought(spaghetti).
shops(mary).
end(model(8888)).

begin(model(8889)).
bought(fish).
shops(mary).
end(model(8889)).

begin(model(8890)).
bought(spaghetti).
shops(mary).
end(model(8890)).

begin(model(8891)).
bought(fish).
shops(mary).
end(model(8891)).

begin(model(8892)).
bought(fish).
shops(mary).
end(model(8892)).

begin(model(8893)).
bought(fish).
shops(mary).
end(model(8893)).

begin(model(8894)).
bought(spaghetti).
shops(mary).
end(model(8894)).

begin(model(8895)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8895)).

begin(model(8896)).
bought(fish).
shops(mary).
end(model(8896)).

begin(model(8897)).
bought(fish).
shops(mary).
end(model(8897)).

begin(model(8898)).
bought(fish).
shops(mary).
end(model(8898)).

begin(model(8899)).
bought(spaghetti).
shops(mary).
end(model(8899)).

begin(model(8900)).
bought(fish).
shops(mary).
end(model(8900)).

begin(model(8901)).
bought(spaghetti).
shops(mary).
end(model(8901)).

begin(model(8902)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8902)).

begin(model(8903)).
bought(fish).
shops(mary).
end(model(8903)).

begin(model(8904)).
bought(fish).
shops(mary).
end(model(8904)).

begin(model(8905)).
bought(spaghetti).
shops(mary).
end(model(8905)).

begin(model(8906)).
bought(spaghetti).
shops(mary).
end(model(8906)).

begin(model(8907)).
bought(fish).
shops(mary).
end(model(8907)).

begin(model(8908)).
bought(spaghetti).
shops(mary).
end(model(8908)).

begin(model(8909)).
bought(fish).
shops(mary).
end(model(8909)).

begin(model(8910)).
bought(fish).
shops(mary).
end(model(8910)).

begin(model(8911)).
bought(fish).
shops(mary).
end(model(8911)).

begin(model(8912)).
bought(fish).
shops(mary).
end(model(8912)).

begin(model(8913)).
bought(fish).
shops(mary).
end(model(8913)).

begin(model(8914)).
bought(fish).
shops(mary).
end(model(8914)).

begin(model(8915)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8915)).

begin(model(8916)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8916)).

begin(model(8917)).
bought(fish).
shops(mary).
end(model(8917)).

begin(model(8918)).
bought(fish).
shops(mary).
end(model(8918)).

begin(model(8919)).
bought(fish).
shops(mary).
end(model(8919)).

begin(model(8920)).
bought(fish).
shops(mary).
end(model(8920)).

begin(model(8921)).
bought(fish).
shops(mary).
end(model(8921)).

begin(model(8922)).
bought(fish).
shops(mary).
end(model(8922)).

begin(model(8923)).
bought(spaghetti).
shops(mary).
end(model(8923)).

begin(model(8924)).
bought(fish).
shops(mary).
end(model(8924)).

begin(model(8925)).
end(model(8925)).

begin(model(8926)).
bought(fish).
shops(mary).
end(model(8926)).

begin(model(8927)).
bought(fish).
shops(mary).
end(model(8927)).

begin(model(8928)).
bought(fish).
shops(mary).
end(model(8928)).

begin(model(8929)).
end(model(8929)).

begin(model(8930)).
bought(fish).
shops(mary).
end(model(8930)).

begin(model(8931)).
bought(fish).
shops(mary).
end(model(8931)).

begin(model(8932)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8932)).

begin(model(8933)).
bought(fish).
shops(mary).
end(model(8933)).

begin(model(8934)).
bought(fish).
shops(mary).
end(model(8934)).

begin(model(8935)).
bought(fish).
shops(mary).
end(model(8935)).

begin(model(8936)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8936)).

begin(model(8937)).
bought(fish).
shops(mary).
end(model(8937)).

begin(model(8938)).
bought(spaghetti).
shops(mary).
end(model(8938)).

begin(model(8939)).
bought(fish).
shops(mary).
end(model(8939)).

begin(model(8940)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8940)).

begin(model(8941)).
bought(fish).
shops(mary).
end(model(8941)).

begin(model(8942)).
end(model(8942)).

begin(model(8943)).
bought(fish).
shops(mary).
end(model(8943)).

begin(model(8944)).
bought(spaghetti).
shops(mary).
end(model(8944)).

begin(model(8945)).
bought(fish).
shops(mary).
end(model(8945)).

begin(model(8946)).
bought(spaghetti).
shops(mary).
end(model(8946)).

begin(model(8947)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8947)).

begin(model(8948)).
bought(spaghetti).
shops(mary).
end(model(8948)).

begin(model(8949)).
bought(fish).
shops(mary).
end(model(8949)).

begin(model(8950)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8950)).

begin(model(8951)).
bought(fish).
shops(mary).
end(model(8951)).

begin(model(8952)).
bought(fish).
shops(mary).
end(model(8952)).

begin(model(8953)).
bought(fish).
shops(mary).
end(model(8953)).

begin(model(8954)).
bought(fish).
shops(mary).
end(model(8954)).

begin(model(8955)).
bought(spaghetti).
shops(john).
end(model(8955)).

begin(model(8956)).
bought(spaghetti).
shops(mary).
end(model(8956)).

begin(model(8957)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8957)).

begin(model(8958)).
end(model(8958)).

begin(model(8959)).
bought(fish).
shops(mary).
end(model(8959)).

begin(model(8960)).
bought(fish).
shops(mary).
end(model(8960)).

begin(model(8961)).
bought(fish).
shops(mary).
end(model(8961)).

begin(model(8962)).
bought(spaghetti).
shops(mary).
end(model(8962)).

begin(model(8963)).
bought(fish).
shops(mary).
end(model(8963)).

begin(model(8964)).
bought(spaghetti).
shops(mary).
end(model(8964)).

begin(model(8965)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8965)).

begin(model(8966)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(8966)).

begin(model(8967)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8967)).

begin(model(8968)).
bought(fish).
shops(mary).
end(model(8968)).

begin(model(8969)).
bought(spaghetti).
shops(mary).
end(model(8969)).

begin(model(8970)).
bought(fish).
shops(mary).
end(model(8970)).

begin(model(8971)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(8971)).

begin(model(8972)).
bought(spaghetti).
shops(mary).
end(model(8972)).

begin(model(8973)).
bought(fish).
shops(mary).
end(model(8973)).

begin(model(8974)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8974)).

begin(model(8975)).
bought(fish).
shops(mary).
end(model(8975)).

begin(model(8976)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8976)).

begin(model(8977)).
bought(spaghetti).
shops(mary).
end(model(8977)).

begin(model(8978)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8978)).

begin(model(8979)).
bought(fish).
shops(mary).
end(model(8979)).

begin(model(8980)).
bought(fish).
shops(mary).
end(model(8980)).

begin(model(8981)).
end(model(8981)).

begin(model(8982)).
bought(fish).
shops(mary).
end(model(8982)).

begin(model(8983)).
end(model(8983)).

begin(model(8984)).
end(model(8984)).

begin(model(8985)).
bought(fish).
shops(mary).
end(model(8985)).

begin(model(8986)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8986)).

begin(model(8987)).
bought(fish).
shops(mary).
end(model(8987)).

begin(model(8988)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8988)).

begin(model(8989)).
bought(spaghetti).
shops(mary).
end(model(8989)).

begin(model(8990)).
end(model(8990)).

begin(model(8991)).
bought(spaghetti).
shops(mary).
end(model(8991)).

begin(model(8992)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(8992)).

begin(model(8993)).
bought(fish).
shops(mary).
end(model(8993)).

begin(model(8994)).
bought(fish).
shops(mary).
end(model(8994)).

begin(model(8995)).
bought(spaghetti).
shops(mary).
end(model(8995)).

begin(model(8996)).
bought(fish).
shops(mary).
end(model(8996)).

begin(model(8997)).
bought(fish).
shops(mary).
end(model(8997)).

begin(model(8998)).
bought(spaghetti).
shops(mary).
end(model(8998)).

begin(model(8999)).
bought(spaghetti).
shops(john).
end(model(8999)).

begin(model(9000)).
bought(spaghetti).
shops(mary).
end(model(9000)).

begin(model(9001)).
bought(steak).
shops(john).
end(model(9001)).

begin(model(9002)).
bought(spaghetti).
shops(mary).
end(model(9002)).

begin(model(9003)).
bought(fish).
shops(mary).
end(model(9003)).

begin(model(9004)).
bought(fish).
shops(mary).
end(model(9004)).

begin(model(9005)).
bought(fish).
shops(mary).
end(model(9005)).

begin(model(9006)).
bought(spaghetti).
shops(mary).
end(model(9006)).

begin(model(9007)).
bought(fish).
shops(mary).
end(model(9007)).

begin(model(9008)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9008)).

begin(model(9009)).
end(model(9009)).

begin(model(9010)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9010)).

begin(model(9011)).
bought(fish).
shops(mary).
end(model(9011)).

begin(model(9012)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9012)).

begin(model(9013)).
bought(fish).
shops(mary).
end(model(9013)).

begin(model(9014)).
end(model(9014)).

begin(model(9015)).
bought(fish).
shops(mary).
end(model(9015)).

begin(model(9016)).
bought(spaghetti).
shops(mary).
end(model(9016)).

begin(model(9017)).
bought(spaghetti).
shops(mary).
end(model(9017)).

begin(model(9018)).
bought(fish).
shops(mary).
end(model(9018)).

begin(model(9019)).
bought(fish).
shops(mary).
end(model(9019)).

begin(model(9020)).
bought(fish).
shops(mary).
end(model(9020)).

begin(model(9021)).
bought(fish).
shops(mary).
end(model(9021)).

begin(model(9022)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9022)).

begin(model(9023)).
bought(fish).
shops(mary).
end(model(9023)).

begin(model(9024)).
bought(fish).
shops(mary).
end(model(9024)).

begin(model(9025)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9025)).

begin(model(9026)).
end(model(9026)).

begin(model(9027)).
bought(fish).
shops(mary).
end(model(9027)).

begin(model(9028)).
bought(fish).
shops(mary).
end(model(9028)).

begin(model(9029)).
bought(fish).
shops(mary).
end(model(9029)).

begin(model(9030)).
bought(spaghetti).
shops(mary).
end(model(9030)).

begin(model(9031)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9031)).

begin(model(9032)).
bought(fish).
shops(mary).
end(model(9032)).

begin(model(9033)).
bought(fish).
shops(mary).
end(model(9033)).

begin(model(9034)).
bought(spaghetti).
shops(mary).
end(model(9034)).

begin(model(9035)).
bought(fish).
shops(mary).
end(model(9035)).

begin(model(9036)).
bought(fish).
shops(mary).
end(model(9036)).

begin(model(9037)).
bought(fish).
shops(mary).
end(model(9037)).

begin(model(9038)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9038)).

begin(model(9039)).
bought(spaghetti).
shops(mary).
end(model(9039)).

begin(model(9040)).
bought(fish).
shops(mary).
end(model(9040)).

begin(model(9041)).
bought(fish).
shops(mary).
end(model(9041)).

begin(model(9042)).
bought(fish).
shops(mary).
end(model(9042)).

begin(model(9043)).
bought(fish).
shops(mary).
end(model(9043)).

begin(model(9044)).
bought(fish).
shops(mary).
end(model(9044)).

begin(model(9045)).
bought(fish).
shops(mary).
end(model(9045)).

begin(model(9046)).
end(model(9046)).

begin(model(9047)).
bought(spaghetti).
shops(mary).
end(model(9047)).

begin(model(9048)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9048)).

begin(model(9049)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9049)).

begin(model(9050)).
bought(fish).
shops(mary).
end(model(9050)).

begin(model(9051)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9051)).

begin(model(9052)).
bought(spaghetti).
shops(mary).
end(model(9052)).

begin(model(9053)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9053)).

begin(model(9054)).
end(model(9054)).

begin(model(9055)).
bought(fish).
shops(mary).
end(model(9055)).

begin(model(9056)).
bought(fish).
shops(mary).
end(model(9056)).

begin(model(9057)).
bought(fish).
shops(mary).
end(model(9057)).

begin(model(9058)).
bought(fish).
shops(mary).
end(model(9058)).

begin(model(9059)).
end(model(9059)).

begin(model(9060)).
bought(fish).
shops(mary).
end(model(9060)).

begin(model(9061)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9061)).

begin(model(9062)).
bought(fish).
shops(mary).
end(model(9062)).

begin(model(9063)).
bought(spaghetti).
shops(mary).
end(model(9063)).

begin(model(9064)).
bought(fish).
shops(mary).
end(model(9064)).

begin(model(9065)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9065)).

begin(model(9066)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9066)).

begin(model(9067)).
end(model(9067)).

begin(model(9068)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9068)).

begin(model(9069)).
bought(fish).
shops(mary).
end(model(9069)).

begin(model(9070)).
bought(fish).
shops(mary).
end(model(9070)).

begin(model(9071)).
bought(fish).
shops(mary).
end(model(9071)).

begin(model(9072)).
bought(fish).
shops(mary).
end(model(9072)).

begin(model(9073)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9073)).

begin(model(9074)).
bought(fish).
shops(mary).
end(model(9074)).

begin(model(9075)).
bought(fish).
shops(mary).
end(model(9075)).

begin(model(9076)).
bought(fish).
shops(mary).
end(model(9076)).

begin(model(9077)).
bought(fish).
shops(mary).
end(model(9077)).

begin(model(9078)).
bought(fish).
shops(mary).
end(model(9078)).

begin(model(9079)).
bought(fish).
shops(mary).
end(model(9079)).

begin(model(9080)).
bought(fish).
shops(mary).
end(model(9080)).

begin(model(9081)).
end(model(9081)).

begin(model(9082)).
bought(fish).
shops(mary).
end(model(9082)).

begin(model(9083)).
bought(spaghetti).
shops(mary).
end(model(9083)).

begin(model(9084)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9084)).

begin(model(9085)).
end(model(9085)).

begin(model(9086)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9086)).

begin(model(9087)).
bought(fish).
shops(mary).
end(model(9087)).

begin(model(9088)).
bought(fish).
shops(mary).
end(model(9088)).

begin(model(9089)).
bought(fish).
shops(mary).
end(model(9089)).

begin(model(9090)).
bought(spaghetti).
shops(mary).
end(model(9090)).

begin(model(9091)).
bought(fish).
shops(mary).
end(model(9091)).

begin(model(9092)).
bought(fish).
shops(mary).
end(model(9092)).

begin(model(9093)).
end(model(9093)).

begin(model(9094)).
bought(fish).
shops(mary).
end(model(9094)).

begin(model(9095)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9095)).

begin(model(9096)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9096)).

begin(model(9097)).
bought(fish).
shops(mary).
end(model(9097)).

begin(model(9098)).
bought(fish).
shops(mary).
end(model(9098)).

begin(model(9099)).
bought(spaghetti).
shops(john).
end(model(9099)).

begin(model(9100)).
bought(fish).
shops(mary).
end(model(9100)).

begin(model(9101)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9101)).

begin(model(9102)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9102)).

begin(model(9103)).
bought(fish).
shops(mary).
end(model(9103)).

begin(model(9104)).
bought(fish).
shops(mary).
end(model(9104)).

begin(model(9105)).
bought(spaghetti).
shops(mary).
end(model(9105)).

begin(model(9106)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9106)).

begin(model(9107)).
bought(fish).
shops(mary).
end(model(9107)).

begin(model(9108)).
bought(spaghetti).
shops(mary).
end(model(9108)).

begin(model(9109)).
bought(spaghetti).
shops(john).
end(model(9109)).

begin(model(9110)).
bought(spaghetti).
shops(mary).
end(model(9110)).

begin(model(9111)).
bought(fish).
shops(mary).
end(model(9111)).

begin(model(9112)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9112)).

begin(model(9113)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9113)).

begin(model(9114)).
bought(fish).
shops(mary).
end(model(9114)).

begin(model(9115)).
bought(fish).
shops(mary).
end(model(9115)).

begin(model(9116)).
bought(fish).
shops(mary).
end(model(9116)).

begin(model(9117)).
bought(fish).
shops(mary).
end(model(9117)).

begin(model(9118)).
bought(spaghetti).
shops(mary).
end(model(9118)).

begin(model(9119)).
bought(fish).
shops(mary).
end(model(9119)).

begin(model(9120)).
bought(spaghetti).
shops(mary).
end(model(9120)).

begin(model(9121)).
bought(spaghetti).
shops(mary).
end(model(9121)).

begin(model(9122)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9122)).

begin(model(9123)).
bought(spaghetti).
shops(mary).
end(model(9123)).

begin(model(9124)).
bought(fish).
shops(mary).
end(model(9124)).

begin(model(9125)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9125)).

begin(model(9126)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9126)).

begin(model(9127)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9127)).

begin(model(9128)).
bought(spaghetti).
shops(mary).
end(model(9128)).

begin(model(9129)).
bought(fish).
shops(mary).
end(model(9129)).

begin(model(9130)).
bought(spaghetti).
shops(mary).
end(model(9130)).

begin(model(9131)).
bought(fish).
shops(mary).
end(model(9131)).

begin(model(9132)).
end(model(9132)).

begin(model(9133)).
bought(fish).
shops(mary).
end(model(9133)).

begin(model(9134)).
bought(spaghetti).
shops(mary).
end(model(9134)).

begin(model(9135)).
bought(spaghetti).
shops(mary).
end(model(9135)).

begin(model(9136)).
bought(spaghetti).
shops(mary).
end(model(9136)).

begin(model(9137)).
bought(fish).
shops(mary).
end(model(9137)).

begin(model(9138)).
bought(spaghetti).
shops(john).
end(model(9138)).

begin(model(9139)).
bought(fish).
shops(mary).
end(model(9139)).

begin(model(9140)).
bought(spaghetti).
shops(mary).
end(model(9140)).

begin(model(9141)).
bought(fish).
shops(mary).
end(model(9141)).

begin(model(9142)).
bought(fish).
shops(mary).
end(model(9142)).

begin(model(9143)).
bought(spaghetti).
shops(mary).
end(model(9143)).

begin(model(9144)).
bought(fish).
shops(mary).
end(model(9144)).

begin(model(9145)).
bought(fish).
shops(mary).
end(model(9145)).

begin(model(9146)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9146)).

begin(model(9147)).
bought(fish).
shops(mary).
end(model(9147)).

begin(model(9148)).
bought(spaghetti).
shops(mary).
end(model(9148)).

begin(model(9149)).
bought(fish).
shops(mary).
end(model(9149)).

begin(model(9150)).
bought(fish).
shops(mary).
end(model(9150)).

begin(model(9151)).
bought(fish).
shops(mary).
end(model(9151)).

begin(model(9152)).
end(model(9152)).

begin(model(9153)).
bought(fish).
shops(mary).
end(model(9153)).

begin(model(9154)).
bought(fish).
shops(mary).
end(model(9154)).

begin(model(9155)).
bought(spaghetti).
shops(mary).
end(model(9155)).

begin(model(9156)).
bought(fish).
shops(mary).
end(model(9156)).

begin(model(9157)).
bought(spaghetti).
shops(mary).
end(model(9157)).

begin(model(9158)).
bought(spaghetti).
shops(mary).
end(model(9158)).

begin(model(9159)).
bought(spaghetti).
shops(mary).
end(model(9159)).

begin(model(9160)).
bought(spaghetti).
shops(mary).
end(model(9160)).

begin(model(9161)).
bought(spaghetti).
shops(mary).
end(model(9161)).

begin(model(9162)).
bought(fish).
shops(mary).
end(model(9162)).

begin(model(9163)).
bought(spaghetti).
shops(mary).
end(model(9163)).

begin(model(9164)).
bought(fish).
shops(mary).
end(model(9164)).

begin(model(9165)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9165)).

begin(model(9166)).
bought(fish).
shops(mary).
end(model(9166)).

begin(model(9167)).
end(model(9167)).

begin(model(9168)).
bought(spaghetti).
shops(mary).
end(model(9168)).

begin(model(9169)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9169)).

begin(model(9170)).
bought(fish).
shops(mary).
end(model(9170)).

begin(model(9171)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9171)).

begin(model(9172)).
bought(fish).
shops(mary).
end(model(9172)).

begin(model(9173)).
bought(fish).
shops(mary).
end(model(9173)).

begin(model(9174)).
bought(spaghetti).
shops(mary).
end(model(9174)).

begin(model(9175)).
bought(spaghetti).
shops(mary).
end(model(9175)).

begin(model(9176)).
bought(fish).
shops(mary).
end(model(9176)).

begin(model(9177)).
bought(fish).
shops(mary).
end(model(9177)).

begin(model(9178)).
bought(fish).
shops(mary).
end(model(9178)).

begin(model(9179)).
bought(fish).
shops(mary).
end(model(9179)).

begin(model(9180)).
bought(fish).
shops(mary).
end(model(9180)).

begin(model(9181)).
bought(fish).
shops(mary).
end(model(9181)).

begin(model(9182)).
bought(fish).
shops(mary).
end(model(9182)).

begin(model(9183)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9183)).

begin(model(9184)).
bought(fish).
shops(mary).
end(model(9184)).

begin(model(9185)).
bought(fish).
shops(mary).
end(model(9185)).

begin(model(9186)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9186)).

begin(model(9187)).
bought(spaghetti).
shops(mary).
end(model(9187)).

begin(model(9188)).
bought(fish).
shops(mary).
end(model(9188)).

begin(model(9189)).
bought(steak).
shops(john).
end(model(9189)).

begin(model(9190)).
bought(spaghetti).
shops(mary).
end(model(9190)).

begin(model(9191)).
bought(spaghetti).
shops(mary).
end(model(9191)).

begin(model(9192)).
bought(fish).
shops(mary).
end(model(9192)).

begin(model(9193)).
bought(fish).
shops(mary).
end(model(9193)).

begin(model(9194)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9194)).

begin(model(9195)).
bought(spaghetti).
shops(mary).
end(model(9195)).

begin(model(9196)).
bought(fish).
shops(mary).
end(model(9196)).

begin(model(9197)).
bought(fish).
shops(mary).
end(model(9197)).

begin(model(9198)).
bought(fish).
shops(mary).
end(model(9198)).

begin(model(9199)).
bought(fish).
shops(mary).
end(model(9199)).

begin(model(9200)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9200)).

begin(model(9201)).
bought(spaghetti).
shops(mary).
end(model(9201)).

begin(model(9202)).
bought(fish).
shops(mary).
end(model(9202)).

begin(model(9203)).
bought(fish).
shops(mary).
end(model(9203)).

begin(model(9204)).
bought(fish).
shops(mary).
end(model(9204)).

begin(model(9205)).
bought(fish).
shops(mary).
end(model(9205)).

begin(model(9206)).
bought(fish).
shops(mary).
end(model(9206)).

begin(model(9207)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9207)).

begin(model(9208)).
bought(fish).
shops(mary).
end(model(9208)).

begin(model(9209)).
bought(spaghetti).
shops(mary).
end(model(9209)).

begin(model(9210)).
bought(fish).
shops(mary).
end(model(9210)).

begin(model(9211)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9211)).

begin(model(9212)).
bought(fish).
shops(mary).
end(model(9212)).

begin(model(9213)).
bought(fish).
shops(mary).
end(model(9213)).

begin(model(9214)).
bought(fish).
shops(mary).
end(model(9214)).

begin(model(9215)).
bought(spaghetti).
shops(mary).
end(model(9215)).

begin(model(9216)).
bought(fish).
shops(mary).
end(model(9216)).

begin(model(9217)).
bought(fish).
shops(mary).
end(model(9217)).

begin(model(9218)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9218)).

begin(model(9219)).
bought(fish).
shops(mary).
end(model(9219)).

begin(model(9220)).
bought(spaghetti).
shops(mary).
end(model(9220)).

begin(model(9221)).
bought(fish).
shops(mary).
end(model(9221)).

begin(model(9222)).
bought(fish).
shops(mary).
end(model(9222)).

begin(model(9223)).
bought(fish).
shops(mary).
end(model(9223)).

begin(model(9224)).
bought(fish).
shops(mary).
end(model(9224)).

begin(model(9225)).
bought(fish).
shops(mary).
end(model(9225)).

begin(model(9226)).
bought(fish).
shops(mary).
end(model(9226)).

begin(model(9227)).
bought(fish).
shops(mary).
end(model(9227)).

begin(model(9228)).
bought(fish).
shops(mary).
end(model(9228)).

begin(model(9229)).
end(model(9229)).

begin(model(9230)).
bought(fish).
shops(mary).
end(model(9230)).

begin(model(9231)).
bought(spaghetti).
shops(mary).
end(model(9231)).

begin(model(9232)).
bought(spaghetti).
shops(mary).
end(model(9232)).

begin(model(9233)).
end(model(9233)).

begin(model(9234)).
bought(fish).
shops(mary).
end(model(9234)).

begin(model(9235)).
bought(fish).
shops(mary).
end(model(9235)).

begin(model(9236)).
bought(fish).
shops(mary).
end(model(9236)).

begin(model(9237)).
bought(spaghetti).
shops(mary).
end(model(9237)).

begin(model(9238)).
bought(fish).
shops(mary).
end(model(9238)).

begin(model(9239)).
bought(fish).
shops(mary).
end(model(9239)).

begin(model(9240)).
bought(fish).
shops(mary).
end(model(9240)).

begin(model(9241)).
bought(fish).
shops(mary).
end(model(9241)).

begin(model(9242)).
bought(fish).
shops(mary).
end(model(9242)).

begin(model(9243)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9243)).

begin(model(9244)).
bought(spaghetti).
shops(mary).
end(model(9244)).

begin(model(9245)).
bought(fish).
shops(mary).
end(model(9245)).

begin(model(9246)).
bought(fish).
shops(mary).
end(model(9246)).

begin(model(9247)).
bought(fish).
shops(mary).
end(model(9247)).

begin(model(9248)).
bought(spaghetti).
shops(mary).
end(model(9248)).

begin(model(9249)).
bought(fish).
shops(mary).
end(model(9249)).

begin(model(9250)).
bought(fish).
shops(mary).
end(model(9250)).

begin(model(9251)).
bought(spaghetti).
shops(mary).
end(model(9251)).

begin(model(9252)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9252)).

begin(model(9253)).
bought(fish).
shops(mary).
end(model(9253)).

begin(model(9254)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9254)).

begin(model(9255)).
bought(spaghetti).
shops(mary).
end(model(9255)).

begin(model(9256)).
bought(fish).
shops(mary).
end(model(9256)).

begin(model(9257)).
bought(fish).
shops(mary).
end(model(9257)).

begin(model(9258)).
bought(fish).
shops(mary).
end(model(9258)).

begin(model(9259)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9259)).

begin(model(9260)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9260)).

begin(model(9261)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9261)).

begin(model(9262)).
end(model(9262)).

begin(model(9263)).
bought(fish).
shops(mary).
end(model(9263)).

begin(model(9264)).
bought(fish).
shops(mary).
end(model(9264)).

begin(model(9265)).
bought(spaghetti).
shops(mary).
end(model(9265)).

begin(model(9266)).
bought(spaghetti).
shops(mary).
end(model(9266)).

begin(model(9267)).
bought(fish).
shops(mary).
end(model(9267)).

begin(model(9268)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9268)).

begin(model(9269)).
bought(fish).
shops(mary).
end(model(9269)).

begin(model(9270)).
bought(fish).
shops(mary).
end(model(9270)).

begin(model(9271)).
bought(fish).
shops(mary).
end(model(9271)).

begin(model(9272)).
bought(fish).
shops(mary).
end(model(9272)).

begin(model(9273)).
bought(fish).
shops(mary).
end(model(9273)).

begin(model(9274)).
bought(fish).
shops(mary).
end(model(9274)).

begin(model(9275)).
bought(fish).
shops(mary).
end(model(9275)).

begin(model(9276)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9276)).

begin(model(9277)).
bought(fish).
shops(mary).
end(model(9277)).

begin(model(9278)).
bought(fish).
shops(mary).
end(model(9278)).

begin(model(9279)).
bought(fish).
shops(mary).
end(model(9279)).

begin(model(9280)).
bought(spaghetti).
shops(mary).
end(model(9280)).

begin(model(9281)).
bought(steak).
shops(john).
end(model(9281)).

begin(model(9282)).
bought(spaghetti).
shops(mary).
end(model(9282)).

begin(model(9283)).
bought(fish).
shops(mary).
end(model(9283)).

begin(model(9284)).
bought(spaghetti).
shops(mary).
end(model(9284)).

begin(model(9285)).
bought(fish).
shops(mary).
end(model(9285)).

begin(model(9286)).
bought(fish).
shops(mary).
end(model(9286)).

begin(model(9287)).
bought(spaghetti).
shops(mary).
end(model(9287)).

begin(model(9288)).
end(model(9288)).

begin(model(9289)).
bought(spaghetti).
shops(mary).
end(model(9289)).

begin(model(9290)).
end(model(9290)).

begin(model(9291)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9291)).

begin(model(9292)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9292)).

begin(model(9293)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9293)).

begin(model(9294)).
bought(fish).
shops(mary).
end(model(9294)).

begin(model(9295)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9295)).

begin(model(9296)).
end(model(9296)).

begin(model(9297)).
bought(spaghetti).
shops(mary).
end(model(9297)).

begin(model(9298)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9298)).

begin(model(9299)).
bought(fish).
shops(mary).
end(model(9299)).

begin(model(9300)).
bought(fish).
shops(mary).
end(model(9300)).

begin(model(9301)).
end(model(9301)).

begin(model(9302)).
bought(fish).
shops(mary).
end(model(9302)).

begin(model(9303)).
bought(spaghetti).
shops(mary).
end(model(9303)).

begin(model(9304)).
bought(spaghetti).
shops(mary).
end(model(9304)).

begin(model(9305)).
bought(fish).
shops(mary).
end(model(9305)).

begin(model(9306)).
bought(spaghetti).
shops(john).
end(model(9306)).

begin(model(9307)).
bought(fish).
shops(mary).
end(model(9307)).

begin(model(9308)).
bought(fish).
shops(mary).
end(model(9308)).

begin(model(9309)).
bought(fish).
shops(mary).
end(model(9309)).

begin(model(9310)).
bought(fish).
shops(mary).
end(model(9310)).

begin(model(9311)).
bought(spaghetti).
shops(mary).
end(model(9311)).

begin(model(9312)).
bought(fish).
shops(mary).
end(model(9312)).

begin(model(9313)).
bought(fish).
shops(mary).
end(model(9313)).

begin(model(9314)).
bought(fish).
shops(mary).
end(model(9314)).

begin(model(9315)).
bought(fish).
shops(mary).
end(model(9315)).

begin(model(9316)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9316)).

begin(model(9317)).
end(model(9317)).

begin(model(9318)).
bought(fish).
shops(mary).
end(model(9318)).

begin(model(9319)).
bought(spaghetti).
shops(mary).
end(model(9319)).

begin(model(9320)).
bought(fish).
shops(mary).
end(model(9320)).

begin(model(9321)).
bought(fish).
shops(mary).
end(model(9321)).

begin(model(9322)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9322)).

begin(model(9323)).
bought(fish).
shops(mary).
end(model(9323)).

begin(model(9324)).
bought(fish).
shops(mary).
end(model(9324)).

begin(model(9325)).
bought(fish).
shops(mary).
end(model(9325)).

begin(model(9326)).
bought(fish).
shops(mary).
end(model(9326)).

begin(model(9327)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9327)).

begin(model(9328)).
bought(spaghetti).
shops(mary).
end(model(9328)).

begin(model(9329)).
bought(fish).
shops(mary).
end(model(9329)).

begin(model(9330)).
bought(spaghetti).
shops(mary).
end(model(9330)).

begin(model(9331)).
bought(fish).
shops(mary).
end(model(9331)).

begin(model(9332)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9332)).

begin(model(9333)).
bought(fish).
shops(mary).
end(model(9333)).

begin(model(9334)).
bought(spaghetti).
shops(mary).
end(model(9334)).

begin(model(9335)).
bought(fish).
shops(mary).
end(model(9335)).

begin(model(9336)).
bought(fish).
shops(mary).
end(model(9336)).

begin(model(9337)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9337)).

begin(model(9338)).
end(model(9338)).

begin(model(9339)).
end(model(9339)).

begin(model(9340)).
bought(fish).
shops(mary).
end(model(9340)).

begin(model(9341)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9341)).

begin(model(9342)).
bought(fish).
shops(mary).
end(model(9342)).

begin(model(9343)).
bought(spaghetti).
shops(mary).
end(model(9343)).

begin(model(9344)).
bought(spaghetti).
shops(mary).
end(model(9344)).

begin(model(9345)).
end(model(9345)).

begin(model(9346)).
bought(fish).
shops(mary).
end(model(9346)).

begin(model(9347)).
end(model(9347)).

begin(model(9348)).
bought(fish).
shops(mary).
end(model(9348)).

begin(model(9349)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9349)).

begin(model(9350)).
bought(spaghetti).
shops(mary).
end(model(9350)).

begin(model(9351)).
bought(spaghetti).
shops(mary).
end(model(9351)).

begin(model(9352)).
bought(fish).
shops(mary).
end(model(9352)).

begin(model(9353)).
bought(fish).
shops(mary).
end(model(9353)).

begin(model(9354)).
bought(fish).
shops(mary).
end(model(9354)).

begin(model(9355)).
bought(spaghetti).
shops(mary).
end(model(9355)).

begin(model(9356)).
bought(fish).
shops(mary).
end(model(9356)).

begin(model(9357)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9357)).

begin(model(9358)).
bought(fish).
shops(mary).
end(model(9358)).

begin(model(9359)).
bought(spaghetti).
shops(mary).
end(model(9359)).

begin(model(9360)).
bought(spaghetti).
shops(mary).
end(model(9360)).

begin(model(9361)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9361)).

begin(model(9362)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9362)).

begin(model(9363)).
end(model(9363)).

begin(model(9364)).
bought(fish).
shops(mary).
end(model(9364)).

begin(model(9365)).
end(model(9365)).

begin(model(9366)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9366)).

begin(model(9367)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9367)).

begin(model(9368)).
bought(fish).
shops(mary).
end(model(9368)).

begin(model(9369)).
bought(fish).
shops(mary).
end(model(9369)).

begin(model(9370)).
bought(fish).
shops(mary).
end(model(9370)).

begin(model(9371)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9371)).

begin(model(9372)).
bought(fish).
shops(mary).
end(model(9372)).

begin(model(9373)).
bought(spaghetti).
shops(mary).
end(model(9373)).

begin(model(9374)).
end(model(9374)).

begin(model(9375)).
bought(fish).
shops(mary).
end(model(9375)).

begin(model(9376)).
bought(fish).
shops(mary).
end(model(9376)).

begin(model(9377)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9377)).

begin(model(9378)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9378)).

begin(model(9379)).
bought(fish).
shops(mary).
end(model(9379)).

begin(model(9380)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9380)).

begin(model(9381)).
bought(spaghetti).
shops(mary).
end(model(9381)).

begin(model(9382)).
bought(spaghetti).
shops(mary).
end(model(9382)).

begin(model(9383)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9383)).

begin(model(9384)).
bought(fish).
shops(mary).
end(model(9384)).

begin(model(9385)).
bought(fish).
shops(mary).
end(model(9385)).

begin(model(9386)).
end(model(9386)).

begin(model(9387)).
bought(fish).
shops(mary).
end(model(9387)).

begin(model(9388)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9388)).

begin(model(9389)).
bought(fish).
shops(mary).
end(model(9389)).

begin(model(9390)).
bought(fish).
shops(mary).
end(model(9390)).

begin(model(9391)).
bought(fish).
shops(mary).
end(model(9391)).

begin(model(9392)).
bought(spaghetti).
shops(mary).
end(model(9392)).

begin(model(9393)).
bought(fish).
shops(mary).
end(model(9393)).

begin(model(9394)).
bought(spaghetti).
shops(mary).
end(model(9394)).

begin(model(9395)).
bought(fish).
shops(mary).
end(model(9395)).

begin(model(9396)).
bought(fish).
shops(mary).
end(model(9396)).

begin(model(9397)).
bought(spaghetti).
shops(mary).
end(model(9397)).

begin(model(9398)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9398)).

begin(model(9399)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9399)).

begin(model(9400)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9400)).

begin(model(9401)).
bought(fish).
shops(mary).
end(model(9401)).

begin(model(9402)).
bought(fish).
shops(mary).
end(model(9402)).

begin(model(9403)).
bought(spaghetti).
shops(mary).
end(model(9403)).

begin(model(9404)).
bought(spaghetti).
shops(mary).
end(model(9404)).

begin(model(9405)).
bought(fish).
shops(mary).
end(model(9405)).

begin(model(9406)).
bought(spaghetti).
shops(mary).
end(model(9406)).

begin(model(9407)).
bought(fish).
shops(mary).
end(model(9407)).

begin(model(9408)).
bought(fish).
shops(mary).
end(model(9408)).

begin(model(9409)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9409)).

begin(model(9410)).
bought(fish).
shops(mary).
end(model(9410)).

begin(model(9411)).
bought(fish).
shops(mary).
end(model(9411)).

begin(model(9412)).
bought(fish).
shops(mary).
end(model(9412)).

begin(model(9413)).
bought(fish).
shops(mary).
end(model(9413)).

begin(model(9414)).
bought(spaghetti).
shops(mary).
end(model(9414)).

begin(model(9415)).
end(model(9415)).

begin(model(9416)).
bought(spaghetti).
shops(mary).
end(model(9416)).

begin(model(9417)).
bought(fish).
shops(mary).
end(model(9417)).

begin(model(9418)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9418)).

begin(model(9419)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9419)).

begin(model(9420)).
bought(spaghetti).
shops(mary).
end(model(9420)).

begin(model(9421)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9421)).

begin(model(9422)).
bought(spaghetti).
shops(mary).
end(model(9422)).

begin(model(9423)).
bought(spaghetti).
shops(mary).
end(model(9423)).

begin(model(9424)).
end(model(9424)).

begin(model(9425)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9425)).

begin(model(9426)).
end(model(9426)).

begin(model(9427)).
bought(spaghetti).
shops(mary).
end(model(9427)).

begin(model(9428)).
bought(fish).
shops(mary).
end(model(9428)).

begin(model(9429)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9429)).

begin(model(9430)).
bought(spaghetti).
shops(john).
end(model(9430)).

begin(model(9431)).
bought(spaghetti).
shops(mary).
end(model(9431)).

begin(model(9432)).
bought(fish).
shops(mary).
end(model(9432)).

begin(model(9433)).
bought(fish).
shops(mary).
end(model(9433)).

begin(model(9434)).
bought(fish).
shops(mary).
end(model(9434)).

begin(model(9435)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9435)).

begin(model(9436)).
bought(spaghetti).
shops(mary).
end(model(9436)).

begin(model(9437)).
bought(spaghetti).
shops(mary).
end(model(9437)).

begin(model(9438)).
bought(fish).
shops(mary).
end(model(9438)).

begin(model(9439)).
bought(fish).
shops(mary).
end(model(9439)).

begin(model(9440)).
end(model(9440)).

begin(model(9441)).
bought(fish).
shops(mary).
end(model(9441)).

begin(model(9442)).
bought(fish).
shops(mary).
end(model(9442)).

begin(model(9443)).
bought(fish).
shops(mary).
end(model(9443)).

begin(model(9444)).
bought(spaghetti).
shops(mary).
end(model(9444)).

begin(model(9445)).
bought(fish).
shops(mary).
end(model(9445)).

begin(model(9446)).
bought(fish).
shops(mary).
end(model(9446)).

begin(model(9447)).
bought(fish).
shops(mary).
end(model(9447)).

begin(model(9448)).
bought(fish).
shops(mary).
end(model(9448)).

begin(model(9449)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9449)).

begin(model(9450)).
bought(fish).
shops(mary).
end(model(9450)).

begin(model(9451)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9451)).

begin(model(9452)).
bought(fish).
shops(mary).
end(model(9452)).

begin(model(9453)).
bought(fish).
shops(mary).
end(model(9453)).

begin(model(9454)).
bought(fish).
shops(mary).
end(model(9454)).

begin(model(9455)).
bought(fish).
shops(mary).
end(model(9455)).

begin(model(9456)).
bought(fish).
shops(mary).
end(model(9456)).

begin(model(9457)).
bought(spaghetti).
shops(mary).
end(model(9457)).

begin(model(9458)).
bought(fish).
shops(mary).
end(model(9458)).

begin(model(9459)).
bought(spaghetti).
shops(mary).
end(model(9459)).

begin(model(9460)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9460)).

begin(model(9461)).
bought(fish).
shops(mary).
end(model(9461)).

begin(model(9462)).
bought(fish).
shops(mary).
end(model(9462)).

begin(model(9463)).
end(model(9463)).

begin(model(9464)).
bought(fish).
shops(mary).
end(model(9464)).

begin(model(9465)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9465)).

begin(model(9466)).
bought(fish).
shops(mary).
end(model(9466)).

begin(model(9467)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9467)).

begin(model(9468)).
bought(fish).
shops(mary).
end(model(9468)).

begin(model(9469)).
bought(spaghetti).
shops(mary).
end(model(9469)).

begin(model(9470)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9470)).

begin(model(9471)).
bought(fish).
shops(mary).
end(model(9471)).

begin(model(9472)).
bought(spaghetti).
shops(mary).
end(model(9472)).

begin(model(9473)).
bought(spaghetti).
shops(mary).
end(model(9473)).

begin(model(9474)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9474)).

begin(model(9475)).
end(model(9475)).

begin(model(9476)).
bought(fish).
shops(mary).
end(model(9476)).

begin(model(9477)).
bought(spaghetti).
shops(mary).
end(model(9477)).

begin(model(9478)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9478)).

begin(model(9479)).
bought(spaghetti).
shops(mary).
end(model(9479)).

begin(model(9480)).
bought(spaghetti).
shops(mary).
end(model(9480)).

begin(model(9481)).
bought(fish).
shops(mary).
end(model(9481)).

begin(model(9482)).
bought(fish).
shops(mary).
end(model(9482)).

begin(model(9483)).
bought(fish).
shops(mary).
end(model(9483)).

begin(model(9484)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9484)).

begin(model(9485)).
bought(fish).
shops(mary).
end(model(9485)).

begin(model(9486)).
bought(fish).
shops(mary).
end(model(9486)).

begin(model(9487)).
bought(fish).
shops(mary).
end(model(9487)).

begin(model(9488)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9488)).

begin(model(9489)).
bought(fish).
shops(mary).
end(model(9489)).

begin(model(9490)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9490)).

begin(model(9491)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9491)).

begin(model(9492)).
bought(fish).
shops(mary).
end(model(9492)).

begin(model(9493)).
bought(spaghetti).
shops(mary).
end(model(9493)).

begin(model(9494)).
bought(fish).
shops(mary).
end(model(9494)).

begin(model(9495)).
bought(fish).
shops(mary).
end(model(9495)).

begin(model(9496)).
bought(fish).
shops(mary).
end(model(9496)).

begin(model(9497)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9497)).

begin(model(9498)).
bought(fish).
shops(mary).
end(model(9498)).

begin(model(9499)).
bought(fish).
shops(mary).
end(model(9499)).

begin(model(9500)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9500)).

begin(model(9501)).
bought(spaghetti).
shops(mary).
end(model(9501)).

begin(model(9502)).
bought(spaghetti).
shops(mary).
end(model(9502)).

begin(model(9503)).
bought(spaghetti).
shops(mary).
end(model(9503)).

begin(model(9504)).
end(model(9504)).

begin(model(9505)).
bought(fish).
shops(mary).
end(model(9505)).

begin(model(9506)).
bought(fish).
shops(mary).
end(model(9506)).

begin(model(9507)).
bought(spaghetti).
shops(mary).
end(model(9507)).

begin(model(9508)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9508)).

begin(model(9509)).
bought(fish).
shops(mary).
end(model(9509)).

begin(model(9510)).
bought(fish).
shops(mary).
end(model(9510)).

begin(model(9511)).
bought(fish).
shops(mary).
end(model(9511)).

begin(model(9512)).
end(model(9512)).

begin(model(9513)).
end(model(9513)).

begin(model(9514)).
bought(fish).
shops(mary).
end(model(9514)).

begin(model(9515)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9515)).

begin(model(9516)).
end(model(9516)).

begin(model(9517)).
end(model(9517)).

begin(model(9518)).
bought(fish).
shops(mary).
end(model(9518)).

begin(model(9519)).
bought(fish).
shops(mary).
end(model(9519)).

begin(model(9520)).
bought(fish).
shops(mary).
end(model(9520)).

begin(model(9521)).
bought(fish).
shops(mary).
end(model(9521)).

begin(model(9522)).
bought(fish).
shops(mary).
end(model(9522)).

begin(model(9523)).
bought(fish).
shops(mary).
end(model(9523)).

begin(model(9524)).
bought(spaghetti).
shops(mary).
end(model(9524)).

begin(model(9525)).
bought(fish).
shops(mary).
end(model(9525)).

begin(model(9526)).
bought(spaghetti).
shops(mary).
end(model(9526)).

begin(model(9527)).
bought(spaghetti).
shops(mary).
end(model(9527)).

begin(model(9528)).
bought(fish).
shops(mary).
end(model(9528)).

begin(model(9529)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9529)).

begin(model(9530)).
bought(fish).
shops(mary).
end(model(9530)).

begin(model(9531)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9531)).

begin(model(9532)).
bought(fish).
shops(mary).
end(model(9532)).

begin(model(9533)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9533)).

begin(model(9534)).
bought(spaghetti).
shops(mary).
end(model(9534)).

begin(model(9535)).
bought(spaghetti).
shops(john).
end(model(9535)).

begin(model(9536)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9536)).

begin(model(9537)).
bought(fish).
shops(mary).
end(model(9537)).

begin(model(9538)).
bought(fish).
shops(mary).
end(model(9538)).

begin(model(9539)).
bought(fish).
shops(mary).
end(model(9539)).

begin(model(9540)).
bought(spaghetti).
shops(mary).
end(model(9540)).

begin(model(9541)).
bought(fish).
shops(mary).
end(model(9541)).

begin(model(9542)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9542)).

begin(model(9543)).
bought(spaghetti).
shops(mary).
end(model(9543)).

begin(model(9544)).
bought(fish).
shops(mary).
end(model(9544)).

begin(model(9545)).
end(model(9545)).

begin(model(9546)).
bought(fish).
shops(mary).
end(model(9546)).

begin(model(9547)).
bought(fish).
shops(mary).
end(model(9547)).

begin(model(9548)).
bought(steak).
shops(john).
end(model(9548)).

begin(model(9549)).
bought(fish).
shops(mary).
end(model(9549)).

begin(model(9550)).
bought(fish).
shops(mary).
end(model(9550)).

begin(model(9551)).
bought(fish).
shops(mary).
end(model(9551)).

begin(model(9552)).
bought(fish).
shops(mary).
end(model(9552)).

begin(model(9553)).
bought(fish).
shops(mary).
end(model(9553)).

begin(model(9554)).
end(model(9554)).

begin(model(9555)).
bought(spaghetti).
shops(mary).
end(model(9555)).

begin(model(9556)).
bought(fish).
shops(mary).
end(model(9556)).

begin(model(9557)).
bought(fish).
shops(mary).
end(model(9557)).

begin(model(9558)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9558)).

begin(model(9559)).
bought(fish).
shops(mary).
end(model(9559)).

begin(model(9560)).
bought(fish).
shops(mary).
end(model(9560)).

begin(model(9561)).
bought(fish).
shops(mary).
end(model(9561)).

begin(model(9562)).
end(model(9562)).

begin(model(9563)).
bought(spaghetti).
shops(mary).
end(model(9563)).

begin(model(9564)).
bought(fish).
shops(mary).
end(model(9564)).

begin(model(9565)).
end(model(9565)).

begin(model(9566)).
bought(fish).
shops(mary).
end(model(9566)).

begin(model(9567)).
end(model(9567)).

begin(model(9568)).
bought(fish).
shops(mary).
end(model(9568)).

begin(model(9569)).
bought(spaghetti).
shops(mary).
end(model(9569)).

begin(model(9570)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9570)).

begin(model(9571)).
bought(fish).
shops(mary).
end(model(9571)).

begin(model(9572)).
bought(fish).
shops(mary).
end(model(9572)).

begin(model(9573)).
bought(fish).
shops(mary).
end(model(9573)).

begin(model(9574)).
bought(spaghetti).
shops(mary).
end(model(9574)).

begin(model(9575)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9575)).

begin(model(9576)).
bought(spaghetti).
shops(mary).
end(model(9576)).

begin(model(9577)).
bought(fish).
shops(mary).
end(model(9577)).

begin(model(9578)).
bought(spaghetti).
shops(mary).
end(model(9578)).

begin(model(9579)).
bought(fish).
shops(mary).
end(model(9579)).

begin(model(9580)).
bought(fish).
shops(mary).
end(model(9580)).

begin(model(9581)).
bought(spaghetti).
shops(mary).
end(model(9581)).

begin(model(9582)).
bought(fish).
shops(mary).
end(model(9582)).

begin(model(9583)).
bought(fish).
shops(mary).
end(model(9583)).

begin(model(9584)).
bought(fish).
shops(mary).
end(model(9584)).

begin(model(9585)).
end(model(9585)).

begin(model(9586)).
bought(fish).
shops(mary).
end(model(9586)).

begin(model(9587)).
bought(fish).
shops(mary).
end(model(9587)).

begin(model(9588)).
bought(spaghetti).
shops(mary).
end(model(9588)).

begin(model(9589)).
bought(fish).
shops(mary).
end(model(9589)).

begin(model(9590)).
bought(fish).
shops(mary).
end(model(9590)).

begin(model(9591)).
bought(fish).
shops(mary).
end(model(9591)).

begin(model(9592)).
end(model(9592)).

begin(model(9593)).
bought(fish).
shops(mary).
end(model(9593)).

begin(model(9594)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9594)).

begin(model(9595)).
bought(fish).
shops(mary).
end(model(9595)).

begin(model(9596)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9596)).

begin(model(9597)).
bought(spaghetti).
shops(mary).
end(model(9597)).

begin(model(9598)).
bought(fish).
shops(mary).
end(model(9598)).

begin(model(9599)).
bought(fish).
shops(mary).
end(model(9599)).

begin(model(9600)).
bought(spaghetti).
shops(mary).
end(model(9600)).

begin(model(9601)).
bought(fish).
shops(mary).
end(model(9601)).

begin(model(9602)).
bought(fish).
shops(mary).
end(model(9602)).

begin(model(9603)).
end(model(9603)).

begin(model(9604)).
bought(fish).
shops(mary).
end(model(9604)).

begin(model(9605)).
bought(fish).
shops(mary).
end(model(9605)).

begin(model(9606)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9606)).

begin(model(9607)).
bought(fish).
shops(mary).
end(model(9607)).

begin(model(9608)).
bought(fish).
shops(mary).
end(model(9608)).

begin(model(9609)).
bought(fish).
shops(mary).
end(model(9609)).

begin(model(9610)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9610)).

begin(model(9611)).
bought(fish).
shops(mary).
end(model(9611)).

begin(model(9612)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9612)).

begin(model(9613)).
bought(fish).
shops(mary).
end(model(9613)).

begin(model(9614)).
bought(fish).
shops(mary).
end(model(9614)).

begin(model(9615)).
bought(spaghetti).
shops(mary).
end(model(9615)).

begin(model(9616)).
bought(fish).
shops(mary).
end(model(9616)).

begin(model(9617)).
bought(spaghetti).
shops(mary).
end(model(9617)).

begin(model(9618)).
end(model(9618)).

begin(model(9619)).
bought(fish).
shops(mary).
end(model(9619)).

begin(model(9620)).
bought(fish).
shops(mary).
end(model(9620)).

begin(model(9621)).
end(model(9621)).

begin(model(9622)).
end(model(9622)).

begin(model(9623)).
bought(fish).
shops(mary).
end(model(9623)).

begin(model(9624)).
bought(fish).
shops(mary).
end(model(9624)).

begin(model(9625)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9625)).

begin(model(9626)).
bought(spaghetti).
shops(mary).
end(model(9626)).

begin(model(9627)).
bought(fish).
shops(mary).
end(model(9627)).

begin(model(9628)).
bought(fish).
shops(mary).
end(model(9628)).

begin(model(9629)).
end(model(9629)).

begin(model(9630)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9630)).

begin(model(9631)).
bought(fish).
shops(mary).
end(model(9631)).

begin(model(9632)).
bought(fish).
shops(mary).
end(model(9632)).

begin(model(9633)).
end(model(9633)).

begin(model(9634)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9634)).

begin(model(9635)).
bought(spaghetti).
shops(mary).
end(model(9635)).

begin(model(9636)).
bought(fish).
shops(mary).
end(model(9636)).

begin(model(9637)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9637)).

begin(model(9638)).
bought(fish).
shops(mary).
end(model(9638)).

begin(model(9639)).
bought(fish).
shops(mary).
end(model(9639)).

begin(model(9640)).
bought(fish).
shops(mary).
end(model(9640)).

begin(model(9641)).
bought(fish).
shops(mary).
end(model(9641)).

begin(model(9642)).
bought(fish).
shops(mary).
end(model(9642)).

begin(model(9643)).
bought(spaghetti).
shops(mary).
end(model(9643)).

begin(model(9644)).
bought(fish).
shops(mary).
end(model(9644)).

begin(model(9645)).
bought(fish).
shops(mary).
end(model(9645)).

begin(model(9646)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9646)).

begin(model(9647)).
bought(spaghetti).
shops(mary).
end(model(9647)).

begin(model(9648)).
bought(spaghetti).
shops(mary).
end(model(9648)).

begin(model(9649)).
bought(fish).
shops(mary).
end(model(9649)).

begin(model(9650)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9650)).

begin(model(9651)).
bought(fish).
shops(mary).
end(model(9651)).

begin(model(9652)).
bought(fish).
shops(mary).
end(model(9652)).

begin(model(9653)).
bought(fish).
shops(mary).
end(model(9653)).

begin(model(9654)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9654)).

begin(model(9655)).
bought(fish).
shops(mary).
end(model(9655)).

begin(model(9656)).
bought(fish).
shops(mary).
end(model(9656)).

begin(model(9657)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9657)).

begin(model(9658)).
end(model(9658)).

begin(model(9659)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9659)).

begin(model(9660)).
bought(fish).
shops(mary).
end(model(9660)).

begin(model(9661)).
bought(fish).
shops(mary).
end(model(9661)).

begin(model(9662)).
bought(fish).
shops(mary).
end(model(9662)).

begin(model(9663)).
bought(fish).
shops(mary).
end(model(9663)).

begin(model(9664)).
bought(fish).
shops(mary).
end(model(9664)).

begin(model(9665)).
bought(fish).
shops(mary).
end(model(9665)).

begin(model(9666)).
bought(spaghetti).
shops(mary).
end(model(9666)).

begin(model(9667)).
bought(fish).
shops(mary).
end(model(9667)).

begin(model(9668)).
bought(fish).
shops(mary).
end(model(9668)).

begin(model(9669)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9669)).

begin(model(9670)).
bought(spaghetti).
shops(mary).
end(model(9670)).

begin(model(9671)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9671)).

begin(model(9672)).
bought(spaghetti).
shops(mary).
end(model(9672)).

begin(model(9673)).
bought(spaghetti).
shops(mary).
end(model(9673)).

begin(model(9674)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9674)).

begin(model(9675)).
bought(fish).
shops(mary).
end(model(9675)).

begin(model(9676)).
bought(fish).
shops(mary).
end(model(9676)).

begin(model(9677)).
bought(fish).
shops(mary).
end(model(9677)).

begin(model(9678)).
bought(fish).
shops(mary).
end(model(9678)).

begin(model(9679)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9679)).

begin(model(9680)).
bought(fish).
shops(mary).
end(model(9680)).

begin(model(9681)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9681)).

begin(model(9682)).
bought(fish).
shops(mary).
end(model(9682)).

begin(model(9683)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9683)).

begin(model(9684)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9684)).

begin(model(9685)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9685)).

begin(model(9686)).
bought(fish).
shops(mary).
end(model(9686)).

begin(model(9687)).
end(model(9687)).

begin(model(9688)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9688)).

begin(model(9689)).
bought(fish).
shops(mary).
end(model(9689)).

begin(model(9690)).
bought(fish).
shops(mary).
end(model(9690)).

begin(model(9691)).
end(model(9691)).

begin(model(9692)).
bought(fish).
shops(mary).
end(model(9692)).

begin(model(9693)).
bought(spaghetti).
shops(mary).
end(model(9693)).

begin(model(9694)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9694)).

begin(model(9695)).
bought(fish).
shops(mary).
end(model(9695)).

begin(model(9696)).
bought(spaghetti).
shops(mary).
end(model(9696)).

begin(model(9697)).
bought(fish).
shops(mary).
end(model(9697)).

begin(model(9698)).
bought(fish).
shops(mary).
end(model(9698)).

begin(model(9699)).
bought(fish).
shops(mary).
end(model(9699)).

begin(model(9700)).
bought(fish).
shops(mary).
end(model(9700)).

begin(model(9701)).
bought(fish).
shops(mary).
end(model(9701)).

begin(model(9702)).
bought(fish).
shops(mary).
end(model(9702)).

begin(model(9703)).
bought(spaghetti).
shops(mary).
end(model(9703)).

begin(model(9704)).
bought(spaghetti).
shops(mary).
end(model(9704)).

begin(model(9705)).
end(model(9705)).

begin(model(9706)).
bought(fish).
shops(mary).
end(model(9706)).

begin(model(9707)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9707)).

begin(model(9708)).
bought(fish).
shops(mary).
end(model(9708)).

begin(model(9709)).
bought(spaghetti).
shops(mary).
end(model(9709)).

begin(model(9710)).
bought(fish).
shops(mary).
end(model(9710)).

begin(model(9711)).
bought(fish).
shops(mary).
end(model(9711)).

begin(model(9712)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9712)).

begin(model(9713)).
bought(fish).
shops(mary).
end(model(9713)).

begin(model(9714)).
bought(fish).
shops(mary).
end(model(9714)).

begin(model(9715)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9715)).

begin(model(9716)).
bought(spaghetti).
shops(mary).
end(model(9716)).

begin(model(9717)).
bought(fish).
shops(mary).
end(model(9717)).

begin(model(9718)).
bought(spaghetti).
shops(mary).
end(model(9718)).

begin(model(9719)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9719)).

begin(model(9720)).
bought(fish).
shops(mary).
end(model(9720)).

begin(model(9721)).
end(model(9721)).

begin(model(9722)).
bought(fish).
shops(mary).
end(model(9722)).

begin(model(9723)).
bought(spaghetti).
shops(mary).
end(model(9723)).

begin(model(9724)).
bought(fish).
shops(mary).
end(model(9724)).

begin(model(9725)).
bought(fish).
shops(mary).
end(model(9725)).

begin(model(9726)).
bought(fish).
shops(mary).
end(model(9726)).

begin(model(9727)).
bought(fish).
shops(mary).
end(model(9727)).

begin(model(9728)).
bought(spaghetti).
shops(mary).
end(model(9728)).

begin(model(9729)).
bought(fish).
shops(mary).
end(model(9729)).

begin(model(9730)).
bought(spaghetti).
shops(mary).
end(model(9730)).

begin(model(9731)).
bought(spaghetti).
shops(john).
end(model(9731)).

begin(model(9732)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9732)).

begin(model(9733)).
bought(fish).
shops(mary).
end(model(9733)).

begin(model(9734)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9734)).

begin(model(9735)).
bought(spaghetti).
shops(mary).
end(model(9735)).

begin(model(9736)).
bought(fish).
shops(mary).
end(model(9736)).

begin(model(9737)).
bought(spaghetti).
shops(mary).
end(model(9737)).

begin(model(9738)).
end(model(9738)).

begin(model(9739)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9739)).

begin(model(9740)).
bought(fish).
shops(mary).
end(model(9740)).

begin(model(9741)).
bought(fish).
shops(mary).
end(model(9741)).

begin(model(9742)).
bought(fish).
shops(mary).
end(model(9742)).

begin(model(9743)).
bought(fish).
shops(mary).
end(model(9743)).

begin(model(9744)).
bought(fish).
shops(mary).
end(model(9744)).

begin(model(9745)).
bought(fish).
shops(mary).
end(model(9745)).

begin(model(9746)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9746)).

begin(model(9747)).
bought(fish).
shops(mary).
end(model(9747)).

begin(model(9748)).
bought(fish).
shops(mary).
end(model(9748)).

begin(model(9749)).
bought(spaghetti).
shops(mary).
end(model(9749)).

begin(model(9750)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9750)).

begin(model(9751)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9751)).

begin(model(9752)).
bought(fish).
shops(mary).
end(model(9752)).

begin(model(9753)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9753)).

begin(model(9754)).
bought(spaghetti).
shops(mary).
end(model(9754)).

begin(model(9755)).
bought(spaghetti).
shops(mary).
end(model(9755)).

begin(model(9756)).
bought(spaghetti).
shops(mary).
end(model(9756)).

begin(model(9757)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9757)).

begin(model(9758)).
bought(spaghetti).
shops(mary).
end(model(9758)).

begin(model(9759)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9759)).

begin(model(9760)).
bought(spaghetti).
shops(mary).
end(model(9760)).

begin(model(9761)).
bought(fish).
shops(mary).
end(model(9761)).

begin(model(9762)).
bought(spaghetti).
shops(mary).
end(model(9762)).

begin(model(9763)).
bought(fish).
shops(mary).
end(model(9763)).

begin(model(9764)).
bought(fish).
shops(mary).
end(model(9764)).

begin(model(9765)).
end(model(9765)).

begin(model(9766)).
bought(fish).
shops(mary).
end(model(9766)).

begin(model(9767)).
bought(fish).
shops(mary).
end(model(9767)).

begin(model(9768)).
bought(fish).
shops(mary).
end(model(9768)).

begin(model(9769)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9769)).

begin(model(9770)).
bought(fish).
shops(mary).
end(model(9770)).

begin(model(9771)).
bought(spaghetti).
shops(mary).
end(model(9771)).

begin(model(9772)).
bought(fish).
shops(mary).
end(model(9772)).

begin(model(9773)).
bought(fish).
shops(mary).
end(model(9773)).

begin(model(9774)).
bought(fish).
shops(mary).
end(model(9774)).

begin(model(9775)).
bought(fish).
shops(mary).
end(model(9775)).

begin(model(9776)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9776)).

begin(model(9777)).
bought(fish).
shops(mary).
end(model(9777)).

begin(model(9778)).
bought(spaghetti).
shops(mary).
end(model(9778)).

begin(model(9779)).
bought(fish).
shops(mary).
end(model(9779)).

begin(model(9780)).
bought(spaghetti).
shops(mary).
end(model(9780)).

begin(model(9781)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9781)).

begin(model(9782)).
bought(spaghetti).
shops(mary).
end(model(9782)).

begin(model(9783)).
bought(fish).
shops(mary).
end(model(9783)).

begin(model(9784)).
bought(fish).
shops(mary).
end(model(9784)).

begin(model(9785)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9785)).

begin(model(9786)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9786)).

begin(model(9787)).
bought(fish).
shops(mary).
end(model(9787)).

begin(model(9788)).
bought(fish).
shops(mary).
end(model(9788)).

begin(model(9789)).
bought(fish).
shops(mary).
end(model(9789)).

begin(model(9790)).
bought(spaghetti).
shops(mary).
end(model(9790)).

begin(model(9791)).
bought(fish).
shops(mary).
end(model(9791)).

begin(model(9792)).
bought(fish).
shops(mary).
end(model(9792)).

begin(model(9793)).
bought(fish).
shops(mary).
end(model(9793)).

begin(model(9794)).
bought(fish).
shops(mary).
end(model(9794)).

begin(model(9795)).
bought(fish).
shops(mary).
end(model(9795)).

begin(model(9796)).
bought(spaghetti).
shops(mary).
end(model(9796)).

begin(model(9797)).
bought(fish).
shops(mary).
end(model(9797)).

begin(model(9798)).
bought(fish).
shops(mary).
end(model(9798)).

begin(model(9799)).
bought(fish).
shops(mary).
end(model(9799)).

begin(model(9800)).
bought(spaghetti).
shops(mary).
end(model(9800)).

begin(model(9801)).
end(model(9801)).

begin(model(9802)).
bought(fish).
shops(mary).
end(model(9802)).

begin(model(9803)).
bought(fish).
shops(mary).
end(model(9803)).

begin(model(9804)).
bought(fish).
shops(mary).
end(model(9804)).

begin(model(9805)).
bought(fish).
shops(mary).
end(model(9805)).

begin(model(9806)).
bought(fish).
shops(mary).
end(model(9806)).

begin(model(9807)).
bought(fish).
shops(mary).
end(model(9807)).

begin(model(9808)).
bought(spaghetti).
shops(mary).
end(model(9808)).

begin(model(9809)).
end(model(9809)).

begin(model(9810)).
bought(fish).
shops(mary).
end(model(9810)).

begin(model(9811)).
bought(spaghetti).
shops(john).
end(model(9811)).

begin(model(9812)).
bought(fish).
shops(mary).
end(model(9812)).

begin(model(9813)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9813)).

begin(model(9814)).
bought(spaghetti).
shops(mary).
end(model(9814)).

begin(model(9815)).
bought(fish).
shops(mary).
end(model(9815)).

begin(model(9816)).
bought(fish).
shops(mary).
end(model(9816)).

begin(model(9817)).
bought(fish).
shops(mary).
end(model(9817)).

begin(model(9818)).
end(model(9818)).

begin(model(9819)).
bought(fish).
shops(mary).
end(model(9819)).

begin(model(9820)).
bought(fish).
shops(mary).
end(model(9820)).

begin(model(9821)).
bought(spaghetti).
shops(john).
end(model(9821)).

begin(model(9822)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9822)).

begin(model(9823)).
bought(fish).
shops(mary).
end(model(9823)).

begin(model(9824)).
bought(fish).
shops(mary).
end(model(9824)).

begin(model(9825)).
bought(spaghetti).
shops(mary).
end(model(9825)).

begin(model(9826)).
bought(fish).
shops(mary).
end(model(9826)).

begin(model(9827)).
bought(fish).
shops(mary).
end(model(9827)).

begin(model(9828)).
bought(fish).
shops(mary).
end(model(9828)).

begin(model(9829)).
bought(spaghetti).
shops(mary).
end(model(9829)).

begin(model(9830)).
bought(fish).
shops(mary).
end(model(9830)).

begin(model(9831)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9831)).

begin(model(9832)).
bought(fish).
shops(mary).
end(model(9832)).

begin(model(9833)).
bought(fish).
shops(mary).
end(model(9833)).

begin(model(9834)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9834)).

begin(model(9835)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9835)).

begin(model(9836)).
bought(fish).
shops(mary).
end(model(9836)).

begin(model(9837)).
bought(fish).
shops(mary).
end(model(9837)).

begin(model(9838)).
bought(fish).
shops(mary).
end(model(9838)).

begin(model(9839)).
bought(fish).
shops(mary).
end(model(9839)).

begin(model(9840)).
bought(fish).
shops(mary).
end(model(9840)).

begin(model(9841)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9841)).

begin(model(9842)).
bought(fish).
shops(mary).
end(model(9842)).

begin(model(9843)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9843)).

begin(model(9844)).
bought(spaghetti).
shops(mary).
end(model(9844)).

begin(model(9845)).
bought(fish).
shops(mary).
end(model(9845)).

begin(model(9846)).
bought(fish).
shops(mary).
end(model(9846)).

begin(model(9847)).
bought(spaghetti).
shops(mary).
end(model(9847)).

begin(model(9848)).
bought(fish).
shops(mary).
end(model(9848)).

begin(model(9849)).
bought(fish).
shops(mary).
end(model(9849)).

begin(model(9850)).
bought(spaghetti).
shops(mary).
end(model(9850)).

begin(model(9851)).
bought(spaghetti).
shops(mary).
end(model(9851)).

begin(model(9852)).
bought(fish).
shops(mary).
end(model(9852)).

begin(model(9853)).
bought(fish).
shops(mary).
end(model(9853)).

begin(model(9854)).
bought(spaghetti).
shops(mary).
end(model(9854)).

begin(model(9855)).
bought(spaghetti).
shops(mary).
end(model(9855)).

begin(model(9856)).
bought(fish).
shops(mary).
end(model(9856)).

begin(model(9857)).
bought(fish).
shops(mary).
end(model(9857)).

begin(model(9858)).
bought(steak).
shops(john).
end(model(9858)).

begin(model(9859)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9859)).

begin(model(9860)).
bought(fish).
shops(mary).
end(model(9860)).

begin(model(9861)).
bought(spaghetti).
shops(mary).
end(model(9861)).

begin(model(9862)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9862)).

begin(model(9863)).
bought(fish).
shops(mary).
end(model(9863)).

begin(model(9864)).
end(model(9864)).

begin(model(9865)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9865)).

begin(model(9866)).
bought(spaghetti).
shops(mary).
end(model(9866)).

begin(model(9867)).
bought(fish).
shops(mary).
end(model(9867)).

begin(model(9868)).
bought(fish).
shops(mary).
end(model(9868)).

begin(model(9869)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9869)).

begin(model(9870)).
bought(spaghetti).
shops(mary).
end(model(9870)).

begin(model(9871)).
bought(fish).
shops(mary).
end(model(9871)).

begin(model(9872)).
end(model(9872)).

begin(model(9873)).
end(model(9873)).

begin(model(9874)).
bought(fish).
shops(mary).
end(model(9874)).

begin(model(9875)).
bought(fish).
shops(mary).
end(model(9875)).

begin(model(9876)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9876)).

begin(model(9877)).
bought(fish).
shops(mary).
end(model(9877)).

begin(model(9878)).
bought(fish).
shops(mary).
end(model(9878)).

begin(model(9879)).
bought(fish).
shops(mary).
end(model(9879)).

begin(model(9880)).
bought(spaghetti).
shops(mary).
end(model(9880)).

begin(model(9881)).
bought(spaghetti).
shops(john).
end(model(9881)).

begin(model(9882)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9882)).

begin(model(9883)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9883)).

begin(model(9884)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9884)).

begin(model(9885)).
bought(spaghetti).
shops(mary).
end(model(9885)).

begin(model(9886)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9886)).

begin(model(9887)).
end(model(9887)).

begin(model(9888)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9888)).

begin(model(9889)).
bought(spaghetti).
shops(mary).
end(model(9889)).

begin(model(9890)).
bought(fish).
shops(mary).
end(model(9890)).

begin(model(9891)).
bought(fish).
shops(mary).
end(model(9891)).

begin(model(9892)).
bought(fish).
shops(mary).
end(model(9892)).

begin(model(9893)).
bought(fish).
shops(mary).
end(model(9893)).

begin(model(9894)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9894)).

begin(model(9895)).
bought(spaghetti).
shops(mary).
end(model(9895)).

begin(model(9896)).
bought(fish).
shops(mary).
end(model(9896)).

begin(model(9897)).
bought(fish).
shops(mary).
end(model(9897)).

begin(model(9898)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9898)).

begin(model(9899)).
bought(fish).
shops(mary).
end(model(9899)).

begin(model(9900)).
bought(fish).
shops(mary).
end(model(9900)).

begin(model(9901)).
bought(fish).
shops(mary).
end(model(9901)).

begin(model(9902)).
bought(fish).
shops(mary).
end(model(9902)).

begin(model(9903)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9903)).

begin(model(9904)).
bought(fish).
shops(mary).
end(model(9904)).

begin(model(9905)).
bought(spaghetti).
shops(mary).
end(model(9905)).

begin(model(9906)).
bought(spaghetti).
shops(mary).
end(model(9906)).

begin(model(9907)).
bought(fish).
shops(mary).
end(model(9907)).

begin(model(9908)).
bought(spaghetti).
shops(mary).
end(model(9908)).

begin(model(9909)).
bought(spaghetti).
shops(mary).
end(model(9909)).

begin(model(9910)).
bought(fish).
shops(mary).
end(model(9910)).

begin(model(9911)).
bought(fish).
shops(mary).
end(model(9911)).

begin(model(9912)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9912)).

begin(model(9913)).
bought(fish).
shops(mary).
end(model(9913)).

begin(model(9914)).
bought(fish).
shops(mary).
end(model(9914)).

begin(model(9915)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9915)).

begin(model(9916)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9916)).

begin(model(9917)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9917)).

begin(model(9918)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9918)).

begin(model(9919)).
bought(spaghetti).
shops(mary).
end(model(9919)).

begin(model(9920)).
bought(spaghetti).
shops(mary).
end(model(9920)).

begin(model(9921)).
bought(fish).
shops(mary).
end(model(9921)).

begin(model(9922)).
bought(spaghetti).
shops(mary).
end(model(9922)).

begin(model(9923)).
bought(spaghetti).
shops(mary).
end(model(9923)).

begin(model(9924)).
bought(fish).
shops(mary).
end(model(9924)).

begin(model(9925)).
bought(fish).
shops(mary).
end(model(9925)).

begin(model(9926)).
bought(fish).
shops(mary).
end(model(9926)).

begin(model(9927)).
bought(fish).
shops(mary).
end(model(9927)).

begin(model(9928)).
bought(fish).
shops(mary).
end(model(9928)).

begin(model(9929)).
end(model(9929)).

begin(model(9930)).
bought(fish).
shops(mary).
end(model(9930)).

begin(model(9931)).
bought(spaghetti).
shops(mary).
end(model(9931)).

begin(model(9932)).
bought(fish).
shops(mary).
end(model(9932)).

begin(model(9933)).
bought(fish).
shops(mary).
end(model(9933)).

begin(model(9934)).
bought(fish).
shops(mary).
end(model(9934)).

begin(model(9935)).
bought(fish).
shops(mary).
end(model(9935)).

begin(model(9936)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9936)).

begin(model(9937)).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9937)).

begin(model(9938)).
bought(fish).
shops(mary).
end(model(9938)).

begin(model(9939)).
bought(spaghetti).
shops(mary).
end(model(9939)).

begin(model(9940)).
bought(fish).
shops(mary).
end(model(9940)).

begin(model(9941)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9941)).

begin(model(9942)).
bought(fish).
shops(mary).
end(model(9942)).

begin(model(9943)).
bought(spaghetti).
shops(mary).
end(model(9943)).

begin(model(9944)).
bought(fish).
shops(mary).
end(model(9944)).

begin(model(9945)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9945)).

begin(model(9946)).
bought(fish).
shops(mary).
end(model(9946)).

begin(model(9947)).
end(model(9947)).

begin(model(9948)).
bought(fish).
shops(mary).
end(model(9948)).

begin(model(9949)).
bought(fish).
shops(mary).
end(model(9949)).

begin(model(9950)).
bought(spaghetti).
shops(mary).
end(model(9950)).

begin(model(9951)).
bought(fish).
shops(mary).
end(model(9951)).

begin(model(9952)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9952)).

begin(model(9953)).
bought(fish).
shops(mary).
end(model(9953)).

begin(model(9954)).
bought(fish).
shops(mary).
end(model(9954)).

begin(model(9955)).
bought(spaghetti).
shops(mary).
end(model(9955)).

begin(model(9956)).
bought(fish).
shops(mary).
end(model(9956)).

begin(model(9957)).
end(model(9957)).

begin(model(9958)).
bought(spaghetti).
shops(mary).
end(model(9958)).

begin(model(9959)).
bought(fish).
shops(mary).
end(model(9959)).

begin(model(9960)).
bought(spaghetti).
shops(mary).
end(model(9960)).

begin(model(9961)).
bought(fish).
shops(mary).
end(model(9961)).

begin(model(9962)).
bought(fish).
shops(mary).
end(model(9962)).

begin(model(9963)).
bought(fish).
shops(mary).
end(model(9963)).

begin(model(9964)).
bought(fish).
shops(mary).
end(model(9964)).

begin(model(9965)).
bought(fish).
shops(mary).
end(model(9965)).

begin(model(9966)).
bought(fish).
shops(mary).
end(model(9966)).

begin(model(9967)).
bought(fish).
shops(mary).
end(model(9967)).

begin(model(9968)).
bought(spaghetti).
shops(mary).
end(model(9968)).

begin(model(9969)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9969)).

begin(model(9970)).
bought(fish).
shops(mary).
end(model(9970)).

begin(model(9971)).
bought(spaghetti).
shops(mary).
end(model(9971)).

begin(model(9972)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9972)).

begin(model(9973)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9973)).

begin(model(9974)).
bought(spaghetti).
shops(mary).
end(model(9974)).

begin(model(9975)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(9975)).

begin(model(9976)).
bought(fish).
shops(mary).
end(model(9976)).

begin(model(9977)).
bought(spaghetti).
shops(mary).
end(model(9977)).

begin(model(9978)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9978)).

begin(model(9979)).
bought(spaghetti).
shops(mary).
end(model(9979)).

begin(model(9980)).
bought(spaghetti).
shops(mary).
end(model(9980)).

begin(model(9981)).
bought(fish).
shops(mary).
end(model(9981)).

begin(model(9982)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9982)).

begin(model(9983)).
bought(spaghetti).
shops(mary).
end(model(9983)).

begin(model(9984)).
bought(spaghetti).
shops(mary).
end(model(9984)).

begin(model(9985)).
end(model(9985)).

begin(model(9986)).
bought(fish).
shops(mary).
end(model(9986)).

begin(model(9987)).
bought(fish).
shops(mary).
end(model(9987)).

begin(model(9988)).
bought(spaghetti).
shops(john).
end(model(9988)).

begin(model(9989)).
bought(fish).
shops(mary).
end(model(9989)).

begin(model(9990)).
bought(spaghetti).
shops(mary).
end(model(9990)).

begin(model(9991)).
bought(fish).
shops(mary).
end(model(9991)).

begin(model(9992)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9992)).

begin(model(9993)).
bought(fish).
shops(mary).
end(model(9993)).

begin(model(9994)).
bought(fish).
shops(mary).
end(model(9994)).

begin(model(9995)).
bought(spaghetti).
shops(mary).
end(model(9995)).

begin(model(9996)).
bought(fish).
shops(mary).
end(model(9996)).

begin(model(9997)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(9997)).

begin(model(9998)).
bought(fish).
shops(mary).
end(model(9998)).

begin(model(9999)).
bought(fish).
shops(mary).
end(model(9999)).

begin(model(10000)).
bought(fish).
shops(mary).
end(model(10000)).

