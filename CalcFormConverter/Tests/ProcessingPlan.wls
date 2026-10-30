(* Private planner parity: no whole-amplitude simplification is introduced. *)
$FeynCalcStartupMessages=False;
Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]],"CalcFormConverter.wl"}]];
checks=<||>;
assert[label_,value_] := (AssociateTo[checks,label->TrueQ[value]];If[!TrueQ[value],Print["FAIL\t",label]]);
Begin["CalcFormConverter`Private`"];
fixtures={
 {1,{},"NoDeclaredIntegration"},
 {(a^2-b^2)/(a-b),{},"NoDeclaredIntegration"},
 {FAD[p],{},"NoDeclaredIntegration"},
 {FAD[{l,m}],{l},"DeclaredIntegrand"},
 {1,{l},"DeclaredIntegrand"},
 {SPD[l,p] FAD[{l,m}],{l},"DeclaredIntegrand"},
 {PaVe[0,{}, {m^2},PaVeAutoOrder->False,PaVeAutoReduce->False],{},"RepresentedIntegrals"},
 {A0[m^2],{},"RepresentedIntegrals"},
 {A0[m^2] FAD[{l,m}],{l},"DeclaredIntegrand"},
 {GAD[mu].GAD[nu],{},"NoDeclaredIntegration"},
 {MTD[mu,nu],{},"NoDeclaredIntegration"},
 {1/A0[m^2],{},"RepresentedIntegrals"}};
variants={{True,False,False,False},{True,True,False,False},{False,False,False,False}};
Do[
 data=buildExportData[fixtures[[j,1]],Automatic,fixtures[[j,2]],Automatic,True];
 opts=variants[[k]];
 old=smPrepare[tdPrepare[data,opts[[1]],opts[[4]]],opts[[2]],opts[[3]],opts[[4]],Automatic];
 new=prepareProcessing[data,opts[[1]],opts[[4]],opts[[2]],opts[[3]],opts[[4]],Automatic];
 label=ToString[{j,k},InputForm];
 Global`assert[label<>" prepared parity",KeyDrop[new,"ProcessingPlan"]===old];
 Global`assert[label<>" rendered parity",renderExport[new,"/tmp/processing-plan.out",readProgramTemplate[]]===renderExport[old,"/tmp/processing-plan.out",readProgramTemplate[]]];
 Global`assert[label<>" classification",new["ProcessingPlan"]["InputCase"]===fixtures[[j,3]]];
 ,{j,Length[fixtures]},{k,Length[variants]}];
data=buildExportData[1,Automatic,{l}];
new=prepareProcessing[data,True,False,False,False,False,Automatic];
Global`assert["constant retains measure",new["ProcessingPlan"]["IntegrationScope"]==="WholeExpression" && new["ProcessingPlan"]["Selected"]["TensorDecomposition"]];
data=buildExportData[fixtures[[7,1]],Automatic,{}];
new=prepareProcessing[data,True,False,True,False,False,Automatic];
Global`assert["saved coefficients do not run tensor decomposition",!new["ProcessingPlan"]["Selected"]["TensorDecomposition"] && new["ProcessingPlan"]["Selected"]["ScalarMasterReduction"]];
Global`assert["private plan absent from mapping",FreeQ[new["Mapping"],"ProcessingPlan"]];
Global`assert["altered candidate rejected",KeyDrop[Join[new,<|"Body"->"(1)+(1)"|>],"ProcessingPlan"]=!=KeyDrop[new,"ProcessingPlan"]];
data=buildExportData[1,Automatic,{}];
events={};
old=Catch[smPrepare[tdPrepare[data,(AppendTo[events,"tensor"];bad),(AppendTo[events,"drop1"];False)],(AppendTo[events,"masters"];True),False,(AppendTo[events,"drop2"];False),Automatic],$failureTag];
oldEvents=events;events={};
new=Catch[prepareProcessing[data,(AppendTo[events,"tensor"];bad),(AppendTo[events,"drop1"];False),(AppendTo[events,"masters"];True),False,(AppendTo[events,"drop2"];False),Automatic],$failureTag];
Global`assert["failure and delayed option order",new===old && events===oldEvents && events==={"tensor","drop1"}];
events={};old=smPrepare[tdPrepare[data,(AppendTo[events,"tensor"];True),(AppendTo[events,"drop1"];False)],(AppendTo[events,"masters"];False),False,(AppendTo[events,"drop2"];False),Automatic];oldEvents=events;events={};
new=prepareProcessing[data,(AppendTo[events,"tensor"];True),(AppendTo[events,"drop1"];False),(AppendTo[events,"masters"];False),False,(AppendTo[events,"drop2"];False),Automatic];
Global`assert["successful delayed option order",KeyDrop[new,"ProcessingPlan"]===old && events===oldEvents];
End[];
(* Small actual executions cover ordinary algebra, an integrated tadpole and
   a saved coefficient. Both paths include the same existing reconstruction. *)
Do[
 ex={MTD[mu,nu] MTD[mu,nu],FAD[{l,m}],PaVe[0,{}, {m^2},PaVeAutoOrder->False,PaVeAutoReduce->False]}[[j]];
 loops=If[j===2,{l},{}];
 job=CalcFormExport[ex,FileNameJoin[{Directory[],"processing-plan-"<>ToString[j]<>".frm"}],LoopMomenta->loops];
 proc=RunProcess[{"form","-q",job["InputFile"]}];
 Export[FileNameJoin[{Directory[],"processing-plan-"<>ToString[j]<>".stdout"}],proc["StandardOutput"],"Text"];
 Export[FileNameJoin[{Directory[],"processing-plan-"<>ToString[j]<>".stderr"}],proc["StandardError"],"Text"];
 assert["FORM execution "<>ToString[j],proc["ExitCode"]===0 && proc["StandardError"]===""];
 manual=CalcFormImport[job["ResultFile"],job["MappingFile"]];
 automatic=CalcFormCalculate[ex,LoopMomenta->loops,FORMThreads->1];
 assert["standalone and calculated parity "<>ToString[j],!FailureQ[automatic] && manual===automatic];
 assert["expected result "<>ToString[j],If[j===1,manual===D,manual===If[j===2,I Pi^2,1] PaVe[0,{}, {m^2},PaVeAutoOrder->False,PaVeAutoReduce->False]]];
 ,{j,3}];
passed=Count[Values[checks],True];failed=Count[Values[checks],False];
Export[FileNameJoin[{Directory[],"baseline-result.json"}],<|"Completed"->True,"Passed"->passed,"Failed"->failed,"Checks"->checks|>,"RawJSON"];
Print["SUMMARY\tpassed=",passed,"\tfailed=",failed];
Export[FileNameJoin[{Directory[],"test-completed"}],"ProcessingPlan","Text"];
Quit[If[failed===0,0,1]];
