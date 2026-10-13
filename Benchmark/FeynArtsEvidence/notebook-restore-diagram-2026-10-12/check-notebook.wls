(* Evaluate the candidate's input cells in a fresh bounded kernel and attach the
   rendered diagram sheet as the Paint cell's saved output. *)
$HistoryLength=0;
caseDirectory=DirectoryName[$InputFileName];
repo=ExpandFileName[FileNameJoin[{caseDirectory,"..","..",".."}]];
cpath=Last[StringSplit[StringTrim[Import["/proc/self/cgroup","Text"]],":"]];
cgroup="/sys/fs/cgroup"<>cpath;
If[StringTrim[Import[cgroup<>"/memory.max","Text"]]=!="1073741824" ||
 StringTrim[Import[cgroup<>"/memory.swap.max","Text"]]=!="0" ||
 StringTrim[Import[cgroup<>"/memory.oom.group","Text"]]=!="1",Print["CAP_MISSING"];Quit[10]];
Print["AGGREGATE_CAP=1073741824 SWAP_MAX=0 OOM_GROUP=1"];
nb=Get[FileNameJoin[{caseDirectory,"candidate.nb"}]];
If[Head[nb]=!=Notebook,Print["NOTEBOOK_PARSE_FAILED"];Quit[2]];
checks=0;failures=0;
assert[label_,condition_]:=(checks++;Print[If[TrueQ[condition],"PASS ","FAIL "],label];If[!TrueQ[condition],failures++]);
allCells=Cases[nb,_Cell,Infinity];
Print["TOTAL_CELLS=",Length[allCells]," INPUT_LIKE_CELLS=",Count[allCells,Cell[_BoxData,("Input"|"Code"),___]]];
newCells={};cellNumber=0;graphicsAttached=False;
Do[
 AppendTo[newCells,cell];
 If[MatchQ[cell,Cell[_BoxData,("Input"|"Code"),___]],
  cellNumber++;
  boxes=cell[[1,1]];
  If[Head[boxes]===List,boxes=RowBox[Riffle[boxes,";"]]];
  held=ToExpression[boxes,StandardForm,HoldComplete];
  If[Head[held]=!=HoldComplete,Print["PARSE_FAILURE=",cellNumber];Quit[3]];
  elapsed=AbsoluteTiming[value=CheckAbort[TimeConstrained[
   ReleaseHold[held/.HoldPattern[NotebookDirectory[]]->(FileNameJoin[{repo,"Examples"}]<>"/")],150,$TimedOut],$Aborted];][[1]];
  Print["CELL=",cellNumber," SECONDS=",elapsed," HEAD=",Head[value]];
  If[value===$TimedOut||value===$Aborted||FailureQ[value],Print["EVALUATION_FAILED=",InputForm[value]];Quit[4]];
  (* Only the rendered sheet is cached; the user's other saved outputs are kept as they are. *)
  If[Head[value]===Graphics,
   AppendTo[newCells,Cell[BoxData[ToBoxes[value,StandardForm]],"Output"]];
   graphicsAttached=True]];
 ,
 {cell,allCells}];
assert["every input and code cell evaluated",cellNumber>=8];
assert["the diagram sheet is rendered",Head[diagramSheet]===Graphics];
assert["the rendered sheet was cached in the notebook",graphicsAttached];
assert["four converted diagrams",Length[diagramTensors]===4];
counts=Count[#,_PropagatorDenominator,Infinity]&/@diagramTensors;
assert["one contact and three exchanges",Sort[counts]==={0,1,1,1}];
routes=Sort[Flatten[Cases[#,PropagatorDenominator[Momentum[q_,___],0]:>q,Infinity]&/@diagramTensors]];
assert["three expected off-shell routes",routes===Sort[{p3+p4,-p2+p4,-p2+p3}]];
contact=First[Select[diagramTensors,FreeQ[#,PropagatorDenominator]&]];
assert["contact equals the FeynGrav quartic source",ExpandScalarProduct[MomentumExpand[
 contact-FeynGrav`GravitonVertex[\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2,\[Mu]3,\[Nu]3,-p3,\[Mu]4,\[Nu]4,-p4]]]===0];
total=theAmplitude[p1,p2,p3,p4];
assert["the amplitude is a nonzero scalar",total=!=0&&FreeQ[total,_LorentzIndex]];
momenta={p1,p2,p3,p4};
eps=MapThread[Momentum[Polarization[#1,#2],D]&,{momenta,{I,I,-I,-I}}];
virtualities=Pair[Momentum[#,D],Momentum[#,D]]&/@momenta;
longitudinal=MapThread[Pair[Momentum[#1,D],#2]&,{momenta,eps}];
traces=Pair[#,#]&/@eps;
assert["off-shell input retained",AnyTrue[virtualities,!FreeQ[total,#]&]];
assert["longitudinal terms retained",And@@(!FreeQ[total,#]&/@longitudinal)];
assert["trace terms retained",And@@(!FreeQ[total,#]&/@traces)];
inputBoxes=Cases[nb,Cell[b_BoxData,("Input"|"Code"),___]:>b,Infinity];
assert["no Transversality option",FreeQ[inputBoxes,_String?(StringContainsQ[#,"Transversality"]&)]];
assert["no FeynArts context prefixes",FreeQ[inputBoxes,_String?(StringContainsQ[#,"FeynArts`"]&)]];
assert["no integration convenience wrapper",!MemberQ[$Packages,"FeynGrav`FeynArtsIntegration`"]];
Print["PERMUTATION_CHECK=",InputForm[CalcFormCalculate[theAmplitude[p1,p2,p3,p4]==theAmplitude[p2,p1,p3,p4]]]];
Print["CGROUP_MEMORY_PEAK=",StringTrim[Import[cgroup<>"/memory.peak","Text"]]];
Print["CGROUP_MEMORY_EVENTS=",StringReplace[StringTrim[Import[cgroup<>"/memory.events","Text"]],"\n"->"; "]];
Print["COMPLETED checks=",checks," failures=",failures];
If[failures===0,nb[[1]]=newCells;Put[nb,FileNameJoin[{caseDirectory,"validated.nb"}]]];
Quit[If[failures===0,0,5]];
