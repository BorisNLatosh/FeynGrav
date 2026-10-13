$HistoryLength=0;
caseDirectory=DirectoryName[$InputFileName];repo=DirectoryName[$InputFileName,4];
cpath=Last[StringSplit[StringTrim[Import["/proc/self/cgroup","Text"]],":"]];cgroup="/sys/fs/cgroup"<>cpath;
If[StringTrim[Import[cgroup<>"/memory.max","Text"]]=!="1073741824" ||
 StringTrim[Import[cgroup<>"/memory.swap.max","Text"]]=!="0" ||
 StringTrim[Import[cgroup<>"/memory.oom.group","Text"]]=!="1",Print["CAP_MISSING"];Quit[10]];
Print["AGGREGATE_CAP=1073741824 SWAP_MAX=0 OOM_GROUP=1"];
nb=Get[FileNameJoin[{caseDirectory,"candidate.nb"}]];
If[Head[nb]=!=Notebook,Quit[2]];
checks=0;failures=0;
assert[label_,condition_]:=(checks++;Print[If[TrueQ[condition],"PASS ","FAIL "],label];If[!TrueQ[condition],failures++]);
newCells={};cellNumber=0;
Do[
 AppendTo[newCells,cell];
 If[MatchQ[cell,Cell[_BoxData,"Input",___]],
  cellNumber++;
  held=ToExpression[cell[[1,1]],StandardForm,HoldComplete];
  If[Head[held]=!=HoldComplete,Print["PARSE_FAILURE=",cellNumber];Quit[3]];
  Print["BEGIN_INPUT_CELL=",cellNumber];
  elapsed=AbsoluteTiming[value=CheckAbort[TimeConstrained[
   ReleaseHold[held/.HoldPattern[NotebookDirectory[]]->(FileNameJoin[{repo,"Examples"}]<>"/")],150,$TimedOut],$Aborted];][[1]];
  Print["END_INPUT_CELL=",cellNumber," SECONDS=",elapsed," RESULT_BYTES=",ByteCount[value]];
  If[value===$TimedOut||value===$Aborted||FailureQ[value],Print["EVALUATION_FAILED=",InputForm[value]];Quit[4]];
  If[cellNumber===3,
   assert["diagram sheet is actual Graphics",Head[diagramSheet]===Graphics];
   assert["four native inserted diagrams painted",Length[Cases[diagramGraphics,FeynArts`DiagramGraphics[___][___],Infinity]]===4];
   assert["diagrams precede amplitude generation",!ValueQ[treeRawAmplitude]&&!ValueQ[diagramAmplitudes]&&!ValueQ[\[ScriptCapitalM]]];
   Print["SVG_EXPORT=",TimeConstrained[Export[FileNameJoin[{caseDirectory,"diagrams.svg"}],diagramSheet,"SVG"],20,$TimedOut]]];
  If[value=!=Null&&Head[value]=!=Short,AppendTo[newCells,Cell[BoxData[ToBoxes[value,StandardForm]],"Output"]]]],
 {cell,nb[[1]]}];
assert["four native topologies and converted tensors",Length[treeTopologies]===4&&Length[diagramTensors]===4];
counts=Count[#, _FeynCalc`PropagatorDenominator,Infinity]&/@diagramTensors;
assert["one contact and three single-propagator exchanges",Sort[counts]==={0,1,1,1}];
routes=Flatten[Cases[#,FeynCalc`PropagatorDenominator[FeynCalc`Momentum[q_,___],0]:>q,Infinity]&/@diagramTensors];
assert["three expected off-shell routes",Sort[routes]===Sort[{p3+p4,-p2+p4,-p2+p3}]];
assert["four successful scalar FORM results",ListQ[diagramAmplitudes]&&Length[diagramAmplitudes]===4&&
 And@@(FreeQ[#,_FeynCalc`LorentzIndex|_CalcFormConverter`CalcFormCalculate]&&!FailureQ[#]&&#=!=0&/@diagramAmplitudes)];
assert["ScriptCapitalM equals the complete sum",ValueQ[\[ScriptCapitalM]]&&\[ScriptCapitalM]===Total[diagramAmplitudes]];
assert["total is nonzero",\[ScriptCapitalM]=!=0];
momenta={p1,p2,p3,p4};
eps=MapThread[FeynCalc`Momentum[FeynCalc`Polarization[#1,#2,FeynCalc`Transversality->False],D]&, {momenta,{I,I,-I,-I}}];
virtualities=FeynCalc`Pair[FeynCalc`Momentum[#,D],FeynCalc`Momentum[#,D]]&/@momenta;
longitudinal=MapThread[FeynCalc`Pair[FeynCalc`Momentum[#1,D],#2]&,{momenta,eps}];
traces=FeynCalc`Pair[#,#]&/@eps;
assert["external virtualities unconstrained",And@@(MatchQ[#,_FeynCalc`Pair]&/@virtualities)];
assert["longitudinal contractions unconstrained",And@@(MatchQ[#,_FeynCalc`Pair]&/@longitudinal)];
assert["traces unconstrained",And@@(MatchQ[#,_FeynCalc`Pair]&/@traces)];
assert["total retains virtuality dependence",AnyTrue[virtualities,!FreeQ[\[ScriptCapitalM],#]&]];
assert["total retains all longitudinal contractions",And@@(!FreeQ[\[ScriptCapitalM],#]&/@longitudinal)];
assert["total retains all polarisation traces",And@@(!FreeQ[\[ScriptCapitalM],#]&/@traces)];
inputText=ToString[Cases[nb,Cell[b_,"Input",___]:>b,Infinity],InputForm];
assert["no specialised kinematics or reference code",And@@(StringFreeQ[inputText,#]&/@{"SetMandelstam","TrickMandelstam","helicityAssumptions","sChannelReference"})];
assert["no repeated FeynCalc prefixes",StringFreeQ[inputText,"FeynCalc`"]];
assert["no imposed assumptions",$Assumptions===True];
assert["no integration convenience wrapper",!MemberQ[$Packages,"FeynGrav`FeynArtsIntegration`"]];
Print["DIAGRAM_DENOMINATOR_COUNTS=",InputForm[counts]," ROUTES=",InputForm[routes]];
Print["PER_DIAGRAM_BYTES=",ByteCount/@diagramAmplitudes," TOTAL_BYTES=",ByteCount[\[ScriptCapitalM]]];
Print["FORM_WORK_DIRECTORY=",treeWorkDirectory];
Print["CGROUP_MEMORY_PEAK=",StringTrim[Import[cgroup<>"/memory.peak","Text"]]];
Print["CGROUP_MEMORY_EVENTS=",StringReplace[StringTrim[Import[cgroup<>"/memory.events","Text"]],"\n"->"; "]];
Print["COMPLETED checks=",checks," failures=",failures];
If[failures===0,nb[[1]]=newCells;Put[nb,FileNameJoin[{caseDirectory,"validated.nb"}]]];
Quit[If[failures===0,0,5]];
