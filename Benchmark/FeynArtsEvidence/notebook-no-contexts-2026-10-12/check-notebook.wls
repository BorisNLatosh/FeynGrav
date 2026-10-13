(* Evaluate the candidate notebook cells in a fresh bounded kernel.
   Reads only; the installed notebook is written separately so that the user's
   saved Output/Print/Message cells are preserved verbatim. *)
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
(* The user's notebook nests its cells inside CellGroupData, so collect them
   in document order rather than iterating nb[[1]] at the top level. *)
allCells=Cases[nb,_Cell,Infinity];
Print["TOTAL_CELLS=",Length[allCells]," INPUT_LIKE_CELLS=",Count[allCells,Cell[_BoxData,("Input"|"Code"),___]]];
cellNumber=0;evaluatedCells=0;
Do[
 If[MatchQ[cell,Cell[_BoxData,("Input"|"Code"),___]],
  cellNumber++;
  boxes=cell[[1,1]];
  If[Head[boxes]===List,boxes=RowBox[Riffle[boxes,";"]]];
  held=ToExpression[boxes,StandardForm,HoldComplete];
  If[Head[held]=!=HoldComplete,Print["PARSE_FAILURE=",cellNumber];Quit[3]];
  Print["BEGIN_CELL=",cellNumber," STYLE=",cell[[2]]];
  elapsed=AbsoluteTiming[value=CheckAbort[TimeConstrained[
   ReleaseHold[held/.HoldPattern[NotebookDirectory[]]->(FileNameJoin[{repo,"Examples"}]<>"/")],150,$TimedOut],$Aborted];][[1]];
  Print["END_CELL=",cellNumber," SECONDS=",elapsed];
  If[value===$TimedOut||value===$Aborted||FailureQ[value],Print["EVALUATION_FAILED=",InputForm[value]];Quit[4]];
  evaluatedCells++],
 {cell,allCells}];
assert["every input and code cell evaluated",evaluatedCells===cellNumber&&cellNumber>=5];
(* Unqualified names must resolve to the FeynArts context, not to a stray Global symbol. *)
contextTable=Context/@{InitializeModel,GenericModel,CreateTopologies,Adjacencies,InsertFields,
 T,Model,InsertionLevel,Classes,Paint,ColumnsXRows,Numbering,Simple,SheetHeader,AutoEdit,
 CreateFeynAmp,PreFactor,GaugeRules,MomentumConservation,Truncated};
Print["CONTEXTS=",InputForm[contextTable]];
assert["all stripped names resolve in the FeynArts context",And@@(#==="FeynArts`"&/@contextTable)];
assert["model loaded from the simplified cell",StringContainsQ[ToString[FeynArts`$Model],"FGPureGR5a149c98161e2b2e"]&&
 StringContainsQ[ToString[FeynArts`$GenericModel],"FGPureGR5a149c98161e2b2e"]];
assert["four diagrams generated",Length[treeTopologies]===4&&Length[diagramTensors]===4];
counts=Count[#, _PropagatorDenominator,Infinity]&/@diagramTensors;
assert["one contact and three exchanges",Sort[counts]==={0,1,1,1}];
routes=Sort[Flatten[Cases[#,PropagatorDenominator[Momentum[q_,___],0]:>q,Infinity]&/@diagramTensors]];
assert["three expected off-shell routes",routes===Sort[{p3+p4,-p2+p4,-p2+p3}]];
assert["four scalar FORM results",ListQ[diagramAmplitudes]&&Length[diagramAmplitudes]===4&&
 And@@(FreeQ[#,_LorentzIndex|_CalcFormCalculate]&&!FailureQ[#]&&#=!=0&/@diagramAmplitudes)];
assert["ScriptCapitalM equals the sum",ValueQ[\[ScriptCapitalM]]&&\[ScriptCapitalM]===Total[diagramAmplitudes]];
contact=First[Select[diagramTensors,FreeQ[#,PropagatorDenominator]&]];
Print["CONTACT_INDICES=",InputForm[DeleteDuplicates[Cases[contact,_LorentzIndex,Infinity]]]];
(* The notebook declares the external indices through LorentzIndexNames, so the
   source is rebuilt with those same names. *)
assert["contact equals the FeynGrav quartic source",ExpandScalarProduct[MomentumExpand[
 contact-FeynGrav`GravitonVertex[mu1,nu1,p1,mu2,nu2,p2,mu3,nu3,-p3,mu4,nu4,-p4]]]===0];
assert["diagram graphics produced before the amplitudes",Head[diagramSheet]===Graphics&&Length[diagramGraphics]===1];
momenta={p1,p2,p3,p4};
eps=MapThread[Momentum[Polarization[#1,#2,Transversality->False],D]&,{momenta,{I,I,-I,-I}}];
virtualities=Pair[Momentum[#,D],Momentum[#,D]]&/@momenta;
longitudinal=MapThread[Pair[Momentum[#1,D],#2]&,{momenta,eps}];
traces=Pair[#,#]&/@eps;
assert["off-shell input retained in the total",AnyTrue[virtualities,!FreeQ[\[ScriptCapitalM],#]&]];
assert["longitudinal terms retained in the total",And@@(!FreeQ[\[ScriptCapitalM],#]&/@longitudinal)];
assert["trace terms retained in the total",And@@(!FreeQ[\[ScriptCapitalM],#]&/@traces)];
inputBoxes=Cases[nb,Cell[b_BoxData,("Input"|"Code"),___]:>b,Infinity];
assert["no FeynArts context prefixes remain",FreeQ[inputBoxes,_String?(StringContainsQ[#,"FeynArts`"]&)]];
qualified=Cases[inputBoxes,s_String/;StringContainsQ[s,"FeynCalc`"|"FeynGrav`"],Infinity];
Print["REMAINING_QUALIFIED_NAMES=",InputForm[DeleteDuplicates[qualified]]];
assert["only the package-load line keeps a context",DeleteDuplicates[qualified]==={"FeynGrav`"}];
assert["no integration convenience wrapper",!MemberQ[$Packages,"FeynGrav`FeynArtsIntegration`"]];
Print["MODEL_DIRECTORY=",InputForm[DirectoryName[grModel]]," MODEL_NAME=",FileBaseName[grModel]];
Print["TOTAL_BYTES=",ByteCount[\[ScriptCapitalM]]];
Print["CGROUP_MEMORY_PEAK=",StringTrim[Import[cgroup<>"/memory.peak","Text"]]];
Print["CGROUP_MEMORY_EVENTS=",StringReplace[StringTrim[Import[cgroup<>"/memory.events","Text"]],"\n"->"; "]];
Print["COMPLETED checks=",checks," failures=",failures];
Quit[If[failures===0,0,5]];
