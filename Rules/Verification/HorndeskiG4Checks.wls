(* Source-only G4 regression. Use a fresh externally bounded kernel: at most
   4 GiB for the whole process tree, zero swap, 600 seconds plus cleanup grace.
   FEYNGRAV_RULE_AUDIT_ROOT optionally selects a candidate repository.
   FEYNGRAV_RULE_AUDIT_OUTPUT must be a fresh existing external evidence folder.
   FEYNGRAV_RULE_AUDIT_FORM optionally selects serial FORM; no main package,
   stored libraries, models, notebooks or generator are loaded. Two FORM
   simplification calls at most; their availability probes are not extra tests.
   The external launcher, not this script, enforces aggregate resource limits. *)
auditScriptFile=$InputFileName;
auditScriptDirectory=DirectoryName[auditScriptFile];
auditRoot=Environment["FEYNGRAV_RULE_AUDIT_ROOT"];
If[!StringQ[auditRoot]||auditRoot==="",auditRoot=DirectoryName[DirectoryName[auditScriptDirectory]]];
auditRun=Environment["FEYNGRAV_RULE_AUDIT_OUTPUT"];
auditForm=Environment["FEYNGRAV_RULE_AUDIT_FORM"];
If[!StringQ[auditForm]||auditForm==="",auditForm=Automatic];
If[!StringQ[auditRun]||!DirectoryQ[auditRun],Print["An existing external evidence directory is required."];Quit[2]];
If[StringStartsQ[ExpandFileName[auditRun]<>"/",ExpandFileName[auditRoot]<>"/"],Print["Evidence must be outside the candidate repository."];Quit[2]];
If[AnyTrue[{"diagnostics.log","checks.json","completion.json"},FileExistsQ[FileNameJoin[{auditRun,#}]]&],Print["Use fresh evidence paths; existing results are preserved."];Quit[2]];
$HistoryLength=0;$FeynCalcStartupMessages=False;
auditChecks={};auditFormCalls=0;auditClosed=False;
auditMsgPath=FileNameJoin[{auditRun,"diagnostics.log"}];
auditMsg=OpenWrite[auditMsgPath];$Messages={auditMsg};
auditWrite[]:=Export[FileNameJoin[{auditRun,"checks.json"}],auditChecks,"RawJSON"];
auditStop[reason_]:=(Print["STOP: ",reason];auditWrite[];
 If[!auditClosed,Close[auditMsg];auditClosed=True];
 Export[FileNameJoin[{auditRun,"completion.json"}],<|"Status"->"Failed","Passed"->0,"Failed"->1,"Reason"->ToString[reason],"RawAssertions"->Length[auditChecks],"FORMCalls"->auditFormCalls|>,"RawJSON"];Quit[2]);
SetAttributes[auditGuard,HoldAll];
auditGuard[expr_]:=Module[{result},result=CheckAbort[Check[expr,$Failed],$Aborted];
 If[!FreeQ[result,_Failure|$Failed|$Aborted],auditStop["Unexpected message, failure or abort"]];result];
auditAssert[label_,condition_]:=(AppendTo[auditChecks,<|"Name"->label,"Passed"->TrueQ[condition]|>];
 auditWrite[];Print[If[TrueQ[condition],"PASS ","FAIL "],label];If[!TrueQ[condition],auditStop[label]]);

(* Predeclare assertion names; incomplete or altered batches cannot pass by
   reporting only the assertions that happened to run. *)
auditCases={{1,2,0},{1,0,1},{1,0,2},{2,0,1},{2,0,2}};
auditCaseName[{n_,a_,b_}]:="n"<>ToString[n]<>"-a"<>ToString[a]<>"-b"<>ToString[b];
auditExpectedNames=Join[{"source-only-load","scale-fixture","public-batch-resolved"},
 Flatten[Table[With[{label=auditCaseName[c],n=c[[1]],m=c[[2]]+2c[[3]]},Join[
  (label<>"-"<>#& /@{"uncontracted-action","contracted-action","wrong-sign-control","half-factor-control"}),
  Flatten[Table[{label<>"-raw-pair-"<>ToString[j],label<>"-contracted-pair-"<>ToString[j]},{j,n}]],
  Table[label<>"-scalar-adjacent-"<>ToString[j],{j,m-1}],
  Table[label<>"-graviton-adjacent-"<>ToString[j],{j,n-1}],
  Table[label<>"-auxiliary-degree-"<>ToString[j],{j,n}],{label<>"-derivative-degree"}]],{c,auditCases}]],
 {"n3-batch-resolved","n3-b2-curvature","n3-b2-Hessian0","n3-b2-Hessian1","n3-b2-Hessian2","n3-b2-complete","n3-b2-wrong-sign-control","n3-b2-half-factor-control","n3-b2-T5-nonzero","n3-b2-omitted-T5-control"},
 Flatten[Table["n3-b2-seed-"<>ToString[j]<>"-pair-"<>ToString[q],{j,5},{q,3}]],
 Table["n3-b2-scalar-adjacent-"<>ToString[j],{j,3}],
 Table["n3-b2-graviton-adjacent-"<>ToString[j],{j,2}],
 Table["n3-b2-auxiliary-degree-"<>ToString[j],{j,3}],
 {"n3-b2-derivative-degree","two-FORM-calls"}];
If[Length[auditExpectedNames]=!=95||!DuplicateFreeQ[auditExpectedNames],auditStop["Invalid predeclared assertion ledger"]];
Export[FileNameJoin[{auditRun,"expected-assertions.json"}],auditExpectedNames,"RawJSON"];

auditGuard[
 auditSourceFiles={auditScriptFile,FileNameJoin[{auditRoot,"Rules","HorndeskiG4.wl"}],
  FileNameJoin[{auditScriptDirectory,"HorndeskiReference.wl"}],
  FileNameJoin[{auditScriptDirectory,"HorndeskiG4Reference.wl"}],
  FileNameJoin[{auditRoot,"CalcFormConverter","CalcFormConverter.wl"}]};
 Export[FileNameJoin[{auditRun,"source-hashes.json"}],
  (<|"Path"->ExpandFileName[#],"SHA256"->FileHash[#,"SHA256","HexString"]|>& /@auditSourceFiles),"RawJSON"];
 Get["FeynCalc`"];
 Block[{$ContextPath={"FeynCalc`","System`"}},Get[FileNameJoin[{auditRoot,"Rules","HorndeskiG4.wl"}]]];
 Block[{$ContextPath={"FeynCalc`","System`"}},Get[FileNameJoin[{auditRoot,"CalcFormConverter","CalcFormConverter.wl"}]]];
 $ContextPath={"FeynCalc`","System`"};
 Get[FileNameJoin[{auditScriptDirectory,"HorndeskiReference.wl"}]];
 Get[FileNameJoin[{auditScriptDirectory,"HorndeskiG4Reference.wl"}]];
];

auditNormal[expr_]:=Expand[ExpandScalarProduct[Contract[FCI[expr]]]];
auditHG[expr_]:=auditNormal[expr/.hgSP->SPD];
auditTriples[n_]:=Flatten[Table[{Symbol["auditMu"<>ToString[j]],Symbol["auditNu"<>ToString[j]],Symbol["auditK"<>ToString[j]]},{j,n}]];
auditEs[n_]:=Table[Symbol["auditE"<>ToString[j]],{j,n}];
auditKs[n_]:=Table[Symbol["auditK"<>ToString[j]],{j,n}];
auditPs[n_]:=Table[Symbol["auditP"<>ToString[j]],{j,n}];
auditProjection[n_]:=Times@@Table[FVD[auditEs[n][[j]],auditTriples[n][[3j-2]]]FVD[auditEs[n][[j]],auditTriples[n][[3j-1]]],{j,n}];
auditSerialMap[f_,items_List,opts___]:=RuleValidation`RuleRequire[Map[Function[item,RuleValidation`RuleBoundary[f[item]]],items]];
auditFORM[expr_]:=Module[{result},
 auditFormCalls++;If[auditFormCalls>2,auditStop["FORM call budget exceeded"]];
 Export[FileNameJoin[{auditRun,"form-input-"<>ToString[auditFormCalls]<>".wxf"}],expr,"WXF"];
 result=Block[{$ContextPath={"FeynCalc`","System`"}},Quiet[CalcFormConverter`FeynGravSimplify[
  FCI[expr],CalcFormConverter`FORMExecutable->auditForm,CalcFormConverter`FORMThreads->1,
  CalcFormConverter`TensorDecomposition->False,TimeConstraint->120,
  CalcFormConverter`KeepFiles->True,CalcFormConverter`WorkingDirectory->auditRun],CalcFormConverter`CalcFormCalculate::files]];
 Export[FileNameJoin[{auditRun,"form-output-"<>ToString[auditFormCalls]<>".wxf"}],result,"WXF"];
 If[!FreeQ[result,_Failure|$Failed|$Aborted],auditStop["FORM returned failure"]];auditNormal[result]];

auditGuard[Block[{RuleValidation`RuleParallelMap=auditSerialMap},
 DataType[auditScale,FCVariable]=True;
 auditAssert["source-only-load",Length[DownValues[FeynGrav`FeynGravImportHorndeskiG4]]===0];
 auditAssert["scale-fixture",auditNormal[SPD[auditScale auditE1,auditP1]]===auditScale FCI[SPD[auditE1,auditP1]]];
 auditPublicRaw=Table[HorndeskiG4`HorndeskiG4Uncontracted[auditTriples[c[[1]]],auditPs[c[[2]]+2c[[3]]],c[[3]]],{c,auditCases}];
 auditPublicContracted=Table[HorndeskiG4`HorndeskiG4[auditTriples[c[[1]]],auditPs[c[[2]]+2c[[3]]],c[[3]]],{c,auditCases}];
 auditPublicTags=Table[{Symbol["auditPublicRaw"<>ToString[j]],Symbol["auditPublicContracted"<>ToString[j]]},{j,Length[auditCases]}];
 auditPublicBatch=auditFORM[Total[Table[(auditPublicTags[[j,1]]auditPublicRaw[[j]]+auditPublicTags[[j,2]]auditPublicContracted[[j]])auditProjection[auditCases[[j,1]]],{j,Length[auditCases]}]]];
 auditAssert["public-batch-resolved",FreeQ[auditPublicBatch,_LorentzIndex|_Failure|$Failed|$Aborted]];
 Do[
  {auditN,auditA,auditB}=auditCases[[c]];auditM=auditA+2auditB;auditLabel=auditCaseName[auditCases[[c]]];
  hgSetup[auditEs[auditN],auditKs[auditN]];
  auditReference=auditHG[Total[Values[hg4ReferenceBlocks[auditA,auditB,auditPs[auditM]]]]];
  auditActual=auditNormal[Coefficient[Expand[auditPublicBatch],auditPublicTags[[c,1]]]];
  auditOther=auditNormal[Coefficient[Expand[auditPublicBatch],auditPublicTags[[c,2]]]];
  auditAssert[auditLabel<>"-uncontracted-action",Expand[auditActual-auditReference]===0];
  auditAssert[auditLabel<>"-contracted-action",Expand[auditOther-auditReference]===0];
  auditAssert[auditLabel<>"-wrong-sign-control",Expand[auditActual+auditReference]=!=0];
  auditAssert[auditLabel<>"-half-factor-control",Expand[auditActual-auditReference/2]=!=0];
  Do[
   auditPairRules={auditTriples[auditN][[3j-2]]->auditTriples[auditN][[3j-1]],auditTriples[auditN][[3j-1]]->auditTriples[auditN][[3j-2]]};
   auditAssert[auditLabel<>"-raw-pair-"<>ToString[j],Expand[FCI[auditPublicRaw[[c]]/.auditPairRules]-FCI[auditPublicRaw[[c]]]]===0];
   auditAssert[auditLabel<>"-contracted-pair-"<>ToString[j],Expand[FCI[auditPublicContracted[[c]]/.auditPairRules]-FCI[auditPublicContracted[[c]]]]===0],{j,auditN}];
  Do[auditAssert[auditLabel<>"-scalar-adjacent-"<>ToString[j],auditNormal[(auditActual/.{auditPs[auditM][[j]]->auditPs[auditM][[j+1]],auditPs[auditM][[j+1]]->auditPs[auditM][[j]]})-auditActual]===0],{j,auditM-1}];
  Do[auditOrder=Range[auditN];auditOrder[[{j,j+1}]]=auditOrder[[{j+1,j}]];
   auditAssert[auditLabel<>"-graviton-adjacent-"<>ToString[j],auditNormal[(auditActual/.Thread[Join[auditEs[auditN],auditKs[auditN]]->Join[auditEs[auditN][[auditOrder]],auditKs[auditN][[auditOrder]]]])-auditActual]===0],{j,auditN-1}];
  Do[auditAssert[auditLabel<>"-auxiliary-degree-"<>ToString[j],auditNormal[(auditActual/.auditEs[auditN][[j]]->auditScale auditEs[auditN][[j]])-auditScale^2 auditActual]===0],{j,auditN}];
  auditAssert[auditLabel<>"-derivative-degree",auditNormal[(auditActual/.Thread[Join[auditKs[auditN],auditPs[auditM]]->auditScale Join[auditKs[auditN],auditPs[auditM]]])-auditScale^(2auditB+2)auditActual]===0];
  Export[FileNameJoin[{auditRun,auditLabel<>".wxf"}],<|"Uncontracted"->auditActual,"Contracted"->auditOther,"Reference"->auditReference|>,"WXF"];
 ,{c,Length[auditCases]}];

 (* Only the five n=3,b=2 source seeds need FORM. Their complete labelled
    graviton/scalar permutations are linear substitutions after projection. *)
 auditN3Raw=Table[Symbol["HorndeskiG4`Private`T"<>ToString[j]<>"Uncontracted"][auditTriples[3],auditPs[4],2],{j,5}];
 auditN3Tags=Table[Symbol["auditN3Block"<>ToString[j]],{j,5}];
 auditN3Batch=auditFORM[Total[auditN3Tags auditN3Raw]auditProjection[3]];
 auditAssert["n3-batch-resolved",FreeQ[auditN3Batch,_LorentzIndex|_Failure|$Failed|$Aborted]];
 auditN3Seeds=auditNormal[Coefficient[Expand[auditN3Batch],#]]& /@auditN3Tags;
 auditN3Blocks=Table[Expand[Total[Flatten[Table[auditN3Seeds[[j]]/.Thread[
  Join[auditEs[3],auditKs[3],auditPs[4]]->Join[auditEs[3][[g]],auditKs[3][[g]],auditPs[4][[p]]]],
  {g,Permutations[Range[3]]},{p,Permutations[Range[4]]}]]]],{j,5}];
 hgSetup[auditEs[3],auditKs[3]];auditN3Reference=hg4ReferenceBlocks[0,2,auditPs[4]];
 auditActual=Total[auditN3Blocks];auditReference=auditHG[Total[Values[auditN3Reference]]];
 Export[FileNameJoin[{auditRun,"n3-b2.wxf"}],<|"SourceSeeds"->auditN3Seeds,"SourceBlocks"->auditN3Blocks,"Reference"->auditN3Reference|>,"WXF"];
 auditAssert["n3-b2-curvature",Expand[Total[auditN3Blocks[[1;;2]]]-auditHG[auditN3Reference["Curvature"]]]===0];
 Do[auditAssert["n3-b2-Hessian"<>ToString[j-1],Expand[auditN3Blocks[[j+2]]-auditHG[auditN3Reference[[j+1]]]]===0],{j,3}];
 auditAssert["n3-b2-complete",Expand[auditActual-auditReference]===0];
 auditAssert["n3-b2-wrong-sign-control",Expand[auditActual+auditReference]=!=0];
 auditAssert["n3-b2-half-factor-control",Expand[auditActual-auditReference/2]=!=0];
 auditAssert["n3-b2-T5-nonzero",Expand[auditN3Blocks[[5]]]=!=0];
 auditAssert["n3-b2-omitted-T5-control",Expand[Total[auditN3Blocks[[1;;4]]]-auditReference]=!=0];
 Do[auditAssert["n3-b2-seed-"<>ToString[j]<>"-pair-"<>ToString[q],Expand[
  FCI[auditN3Raw[[j]]/.{auditTriples[3][[3q-2]]->auditTriples[3][[3q-1]],auditTriples[3][[3q-1]]->auditTriples[3][[3q-2]]}]-FCI[auditN3Raw[[j]]]]===0],{j,5},{q,3}];
 Do[auditAssert["n3-b2-scalar-adjacent-"<>ToString[j],auditNormal[(auditActual/.{auditPs[4][[j]]->auditPs[4][[j+1]],auditPs[4][[j+1]]->auditPs[4][[j]]})-auditActual]===0],{j,3}];
 Do[auditOrder=Range[3];auditOrder[[{j,j+1}]]=auditOrder[[{j+1,j}]];
  auditAssert["n3-b2-graviton-adjacent-"<>ToString[j],auditNormal[(auditActual/.Thread[Join[auditEs[3],auditKs[3]]->Join[auditEs[3][[auditOrder]],auditKs[3][[auditOrder]]]])-auditActual]===0],{j,2}];
 Do[auditAssert["n3-b2-auxiliary-degree-"<>ToString[j],auditNormal[(auditActual/.auditEs[3][[j]]->auditScale auditEs[3][[j]])-auditScale^2 auditActual]===0],{j,3}];
 auditAssert["n3-b2-derivative-degree",auditNormal[(auditActual/.Thread[Join[auditKs[3],auditPs[4]]->auditScale Join[auditKs[3],auditPs[4]]])-auditScale^6 auditActual]===0];
 auditAssert["two-FORM-calls",auditFormCalls===2];
]];
Close[auditMsg];auditClosed=True;
If[FileByteCount[auditMsgPath]>0,auditStop["Unexpected diagnostics"]];
If[Lookup[auditChecks,"Name"]=!=auditExpectedNames||Length[auditChecks]=!=95,auditStop["Assertion ledger mismatch"]];
Export[FileNameJoin[{auditRun,"completion.json"}],<|"Status"->"Completed","Passed"->95,"Failed"->0,"RawAssertions"->95,"FORMCalls"->auditFormCalls|>,"RawJSON"];
Quit[0];
