$HistoryLength=0;
caseDirectory=DirectoryName[$InputFileName];
repo=DirectoryName[$InputFileName,4];
cpath=Last[StringSplit[StringTrim[Import["/proc/self/cgroup","Text"]],":"]];
cgroup="/sys/fs/cgroup"<>cpath;
If[StringTrim[Import[cgroup<>"/memory.max","Text"]]=!="1073741824" ||
 StringTrim[Import[cgroup<>"/memory.swap.max","Text"]]=!="0" ||
 StringTrim[Import[cgroup<>"/memory.oom.group","Text"]]=!="1",Print["CAP_MISSING"];Quit[10]];
Print["AGGREGATE_CAP=1073741824 SWAP_MAX=0 OOM_GROUP=1"];
nb=Get[FileNameJoin[{caseDirectory,"candidate.nb"}]];
If[Head[nb]=!=Notebook,Quit[2]];
newCells={};cellNumber=0;
Do[
 AppendTo[newCells,cell];
 If[MatchQ[cell,Cell[_BoxData,"Input",___]],
  cellNumber++;
  held=ToExpression[cell[[1,1]],StandardForm,HoldComplete];
  If[Head[held]=!=HoldComplete,Print["PARSE_FAILURE=",cellNumber];Quit[3]];
  Print["BEGIN_INPUT_CELL=",cellNumber];
  elapsed=AbsoluteTiming[value=CheckAbort[TimeConstrained[
   ReleaseHold[held/.HoldPattern[NotebookDirectory[]]->(FileNameJoin[{repo,"Examples"}]<>"/")],100,$TimedOut],$Aborted];][[1]];
  Print["END_INPUT_CELL=",cellNumber," SECONDS=",elapsed," RESULT_BYTES=",ByteCount[value]];
  If[value===$TimedOut||value===$Aborted||FailureQ[value],Print["EVALUATION_FAILED=",InputForm[value]];Quit[4]];
  If[value=!=Null&&Head[value]=!=Short,AppendTo[newCells,Cell[BoxData[ToBoxes[value,StandardForm]],"Output"]]]],
 {cell,nb[[1]]}];
checks=0;failures=0;
assert[label_,condition_]:=(checks++;Print[If[TrueQ[condition],"PASS ","FAIL "],label];If[!TrueQ[condition],failures++]);
assert["one converted s-channel diagram",Length[sTopologies]===1&&Length[sConverted]===1];
assert["outgoing sum routing",sChannelRouting==={p3+p4}];
assert["off-shell pole without mass-shell substitutions",Together[FeynCalc`ExpandScalarProduct[sChannelPole]-FeynCalc`ExpandScalarProduct[1/FeynCalc`Pair[FeynCalc`Momentum[p3+p4,D],FeynCalc`Momentum[p3+p4,D]]]]===0];
Print["OFF_SHELL_POLE=",InputForm[sChannelPole]];
assert["FORM returned a scalar",FreeQ[AmplitudeSChannel,_FeynCalc`LorentzIndex|_CalcFormConverter`CalcFormCalculate]&&!FailureQ[AmplitudeSChannel]&&AmplitudeSChannel=!=0];
momenta={p1,p2,p3,p4};
eps=MapThread[FeynCalc`Momentum[FeynCalc`Polarization[#1,#2, FeynCalc`Transversality->False],D]&, {momenta,{I,I,-I,-I}}];
virtualities=FeynCalc`Pair[FeynCalc`Momentum[#,D],FeynCalc`Momentum[#,D]]&/@momenta;
longitudinal=MapThread[FeynCalc`Pair[FeynCalc`Momentum[#1,D],#2]&,{momenta,eps}];
traces=FeynCalc`Pair[#,#]&/@eps;
assert["external virtualities are unconstrained",And@@(MatchQ[#,_FeynCalc`Pair]&/@virtualities)];
assert["all four longitudinal contractions are unconstrained",And@@(MatchQ[#,_FeynCalc`Pair]&/@longitudinal)];
assert["all four traces are unconstrained",And@@(MatchQ[#,_FeynCalc`Pair]&/@traces)];
assert["amplitude retains external virtuality dependence",AnyTrue[virtualities,!FreeQ[AmplitudeSChannel,#]&]];
assert["amplitude retains longitudinal terms",And@@(!FreeQ[AmplitudeSChannel,#]&/@longitudinal)];
assert["amplitude retains trace terms",And@@(!FreeQ[AmplitudeSChannel,#]&/@traces)];
inputText=ToString[Cases[nb,Cell[b_,"Input",___]:>b,Infinity],InputForm];
assert["no specialised kinematic or reference code",And@@(StringFreeQ[inputText,#]&/@{"SetMandelstam","TrickMandelstam","helicityAssumptions","sChannelReference","polarisationProducts"})];
assert["no repeated FeynCalc prefixes",StringFreeQ[inputText,"FeynCalc`"]];
assert["no imposed assumptions",$Assumptions===True];
assert["no convenience integration wrapper",!MemberQ[$Packages,"FeynGrav`FeynArtsIntegration`"]];
Print["AMPLITUDE_BYTES=",ByteCount[AmplitudeSChannel]," EXPRESSION_HEAD=",Head[AmplitudeSChannel]];
Print["FORM_WORK_DIRECTORY=",sChannelWorkDirectory];
Print["CGROUP_MEMORY_PEAK=",StringTrim[Import[cgroup<>"/memory.peak","Text"]]];
Print["CGROUP_MEMORY_EVENTS=",StringReplace[StringTrim[Import[cgroup<>"/memory.events","Text"]],"\n"->"; "]];
Print["COMPLETED checks=",checks," failures=",failures];
If[failures===0,nb[[1]]=newCells;Put[nb,FileNameJoin[{caseDirectory,"validated.nb"}]]];
Quit[If[failures===0,0,5]];
