(* Evaluate the actual notebook cells, not an alternative hand-written amplitude. *)
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
If[Head[nb]=!=Notebook,Print["NOTEBOOK_PARSE_FAILED"];Quit[2]];
newCells={};cellNumber=0;
Do[
 AppendTo[newCells,cell];
 If[MatchQ[cell,Cell[_BoxData,"Input",___]],
  cellNumber++;
  held=ToExpression[cell[[1,1]],StandardForm,HoldComplete];
  If[Head[held]=!=HoldComplete,Print["CELL_PARSE_FAILURE=",cellNumber];Quit[3]];
  Print["BEGIN_INPUT_CELL=",cellNumber];
  elapsed=AbsoluteTiming[value=CheckAbort[TimeConstrained[
   ReleaseHold[held/.HoldPattern[NotebookDirectory[]]->(FileNameJoin[{repo,"Examples"}]<>"/")],90,$TimedOut],$Aborted];][[1]];
  Print["END_INPUT_CELL=",cellNumber," SECONDS=",elapsed," RESULT_BYTES=",ByteCount[value]];
  If[value===$TimedOut||value===$Aborted||FailureQ[value],
   Print["EVALUATION_FAILED=",InputForm[value]];Quit[4]];
  If[value=!=Null,AppendTo[newCells,Cell[BoxData[ToBoxes[value,StandardForm]],"Output"]]]],
 {cell,nb[[1]]}];
checks=0;failures=0;
assert[label_,value_]:=(checks++;Print[If[TrueQ[value],"PASS ","FAIL "],label];If[!TrueQ[value],failures++]);
assert["one s-channel topology",Length[sTopologies]===1];
assert["one converted s-channel diagram",Length[sConverted]===1];
assert["on-shell propagator is 1/s",TrueQ[Simplify[sChannelPole-1/s]===0]];
assert["FORM returned a scalar",FreeQ[sChannelFORM,_FeynCalc`Pair|_FeynCalc`LorentzIndex|_CalcFormConverter`CalcFormCalculate]&&!FailureQ[sChannelFORM]];
assert["exact symbolic agreement with original saved result",sChannelMatchesReference];
assert["nonzero all-equal helicities",Simplify[(AmplitudeSChannel/.{h1->1,h2->1,h3->1,h4->1})-
 9 I FeynGrav`\[Kappa]^2 t u/(4s),Assumptions->s+t+u==0]===0];
assert["opposite incoming helicities give zero s-channel",Simplify[AmplitudeSChannel/.{h1->1,h2->-1,h3->1,h4->1}]===0];
assert["no integration convenience wrapper loaded",!MemberQ[$Packages,"FeynGrav`FeynArtsIntegration`"]];
(* Independently extract and evaluate ONLY the original notebook's kinematic input cells.
   Neither its exchange tensor calculation nor saved amplitude is executed. *)
old=Get[FileNameJoin[{repo,"Examples","Graviton_Scattering_Tree_Level.nb"}]];
oldInputs=Cases[old,Cell[BoxData[b_],"Input",___]:>b,Infinity];
newValues=Join[Flatten[Table[FeynCalc`Pair[polarisationMomenta[[i]],polarisationMomenta[[j]]],{i,4},{j,4}]],
 Flatten[Table[FeynCalc`Pair[FeynCalc`Momentum[momenta[[i]],D],polarisationMomenta[[j]]],{i,4},{j,4}]]];
Block[{$Assumptions},Scan[ReleaseHold[ToExpression[#,StandardForm,HoldComplete]]&,oldInputs[[3;;24]]]];
oldValues=Join[Flatten[Table[FeynCalc`Pair[polarisationMomenta[[i]],polarisationMomenta[[j]]],{i,4},{j,4}]],
 Flatten[Table[FeynCalc`Pair[FeynCalc`Momentum[momenta[[i]],D],polarisationMomenta[[j]]],{i,4},{j,4}]]];
assert["all 32 polarisation products match original input cells",newValues===oldValues];
Print["AMPLITUDE_S=",InputForm[AmplitudeSChannel]];
Print["REFERENCE_RESIDUAL=",InputForm[sChannelReferenceResidual]];
Print["FORM_WORK_DIRECTORY=",sChannelWorkDirectory];
Print["CGROUP_MEMORY_PEAK=",StringTrim[Import[cgroup<>"/memory.peak","Text"]]];
Print["CGROUP_MEMORY_EVENTS=",StringReplace[StringTrim[Import[cgroup<>"/memory.events","Text"]],"\n"->"; "]];
Print["COMPLETED checks=",checks," failures=",failures];
If[failures===0,
 nb[[1]]=newCells;Put[nb,FileNameJoin[{caseDirectory,"validated.nb"}]]];
Quit[If[failures===0,0,5]];
