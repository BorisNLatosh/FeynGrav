(* Focused scientific regression. Run only in a fresh, externally bounded kernel.
   FEYNGRAV_RULE_AUDIT_OUTPUT must name an existing evidence directory.
   FEYNGRAV_RULE_AUDIT_ROOT optionally selects a candidate repository root.
   No library generation, notebook execution or production writes occur. *)
auditScriptDirectory=DirectoryName[$InputFileName];
auditRoot=Environment["FEYNGRAV_RULE_AUDIT_ROOT"];
If[!StringQ[auditRoot]||auditRoot==="",auditRoot=DirectoryName[DirectoryName[auditScriptDirectory]]];
auditForm=Environment["FEYNGRAV_RULE_AUDIT_FORM"];
If[!StringQ[auditForm]||auditForm==="",auditForm=Automatic];
$HistoryLength=0;$FeynCalcStartupMessages=False;
auditRun=Environment["FEYNGRAV_RULE_AUDIT_OUTPUT"];
If[!StringQ[auditRun]||!DirectoryQ[auditRun],Print["An existing external evidence directory is required."];Quit[2]];
auditMsgPath=FileNameJoin[{auditRun,"diagnostics.log"}];
auditMsg=OpenWrite[auditMsgPath];$Messages={auditMsg};
auditLoad=CheckAbort[Check[
 Get["FeynCalc`"];
 Block[{$ContextPath={"FeynCalc`","System`"}},Get["FeynGrav`"]];
 Block[{$ContextPath={"FeynCalc`","System`"}},Get[FileNameJoin[{auditRoot,"Rules","HorndeskiG2.wl"}]];];
 Block[{$ContextPath={"FeynCalc`","System`"}},Get[FileNameJoin[{auditRoot,"Rules","HorndeskiG3.wl"}]];];
 Block[{$ContextPath={"FeynCalc`","System`"}},Get[FileNameJoin[{auditRoot,"Rules","HorndeskiG5.wl"}]];];
 True,$Failed],$Failed];
If[auditLoad=!=True,Quit[2]];
$ContextPath={"FeynCalc`","System`"};
Get[FileNameJoin[{auditScriptDirectory,"HorndeskiReference.wl"}]];
auditChecks={}; auditObservations={};
auditWrite[] := Export[FileNameJoin[{auditRun,"checks.json"}],auditChecks,"RawJSON"];
auditStop[reason_] := (
 Print["STOP: ",reason];auditWrite[];
 Export[FileNameJoin[{auditRun,"completion.json"}],
  <|"Status"->"Failed","Passed"->0,"Failed"->1,"Reason"->ToString[reason]|>,"RawJSON"];
 Quit[2]);
SetAttributes[auditGuard,HoldAll];
auditGuard[expr_] := Module[{res},res=CheckAbort[Check[expr,$Failed],$Failed];
 If[res===$Failed,auditStop["Unexpected message, failed evaluation or abort"]];res];
auditAssert[label_,value_] := (
 AppendTo[auditChecks,<|"Name"->label,"Passed"->TrueQ[value]|>];auditWrite[];
 Print[If[TrueQ[value],"PASS ","FAIL "],label];If[!TrueQ[value],auditStop[label]]);
auditNormal[expr_] := Expand[ExpandScalarProduct[Contract[FCI[expr]]]];
auditIndices[n_] := Flatten[Table[{Symbol["auditMu"<>ToString[i]],Symbol["auditNu"<>ToString[i]]},{i,n}]];
auditTriples[n_] := Flatten[Table[{Symbol["auditMu"<>ToString[i]],Symbol["auditNu"<>ToString[i]],Symbol["auditK"<>ToString[i]]},{i,n}]];
auditEs[n_] := Table[Symbol["auditE"<>ToString[i]],{i,n}];
auditKs[n_] := Table[Symbol["auditK"<>ToString[i]],{i,n}];
auditProject[expr_,n_] := Module[{out},out=Block[{$ContextPath={"FeynCalc`","System`"}},Quiet[CalcFormConverter`FeynGravSimplify[
 FCI[expr Times@@Table[FVD[auditEs[n][[i]],auditIndices[n][[2i-1]]] FVD[auditEs[n][[i]],auditIndices[n][[2i]]],{i,n}]],
 CalcFormConverter`FORMExecutable->auditForm,CalcFormConverter`FORMThreads->1,
 CalcFormConverter`TensorDecomposition->False,TimeConstraint->30,
 CalcFormConverter`KeepFiles->True,CalcFormConverter`WorkingDirectory->auditRun],CalcFormConverter`CalcFormCalculate::files]];
 If[FailureQ[out],Export[FileNameJoin[{auditRun,"form-failure.wxf"}],out,"WXF"];Print[InputForm[out]];auditStop["FORM projection failed"]];auditNormal[out]];




auditSerialMap[f_,items_List,opts___]:=RuleValidation`RuleRequire[Map[Function[item,RuleValidation`RuleBoundary[f[item]]],items]];
auditCapture[x_]:=Module[{auditLocal},x+auditLocal];
auditGuard[Block[{RuleValidation`RuleParallelMap=auditSerialMap},
 auditAssert["Module-external-symbol-not-captured",Count[auditCapture[auditLocal],auditLocal,Infinity]===1];
 Do[
  auditPs=If[family===2,{auditP1,auditP2},{auditP1,auditP2,auditP3}];
  auditArgs=If[family===2,auditIndices[1],auditTriples[1]];
  auditRef=If[family===5,(hgSetup[auditEs[1],auditKs[1]];auditNormal[Total[Values[hgReference[0,1,auditPs]]]/.hgSP->SPD]),auditNormal[auditReference[family,0,1,auditEs[1],auditKs[1],auditPs]]];
  auditBadNames=Switch[family,2,{Global`\[ScriptA]1,Global`\[ScriptB]1},3,{Global`\[ScriptA]1,HorndeskiG3`Private`\[ScriptM]},5,{Global`\[ScriptA]1,HorndeskiG5`Private`\[Alpha]}];
  Do[
   auditHead=Symbol["HorndeskiG"<>ToString[family]<>"`HorndeskiG"<>ToString[family]<>suffix];
   auditLabel=SymbolName[auditHead];Print["BEGIN ",auditLabel];
   auditGeneric=auditHead[auditArgs,auditPs,1];
   auditAssert[auditLabel<>"-independent-action",Expand[auditProject[auditGeneric,1]-auditRef]===0];
   If[suffix==="",
    auditAssert[auditLabel<>"-wrong-sign-control",Expand[auditRef+auditRef]=!=0];
    auditAssert[auditLabel<>"-half-factor-control",Expand[auditRef-auditRef/2]=!=0]];
   auditAssert[auditLabel<>"-malformed-input-rejected",FailureQ[auditHead[{{auditMu1}},auditPs,1]]];
   auditGenericFree=auditNormal[auditGeneric];
   Do[
    auditSpecific=auditHead[ReplacePart[auditArgs,1->bad],auditPs,1];
    auditExpected=auditGenericFree/.LorentzIndex[auditMu1,D]->LorentzIndex[bad,D];
    auditAssert[auditLabel<>"-collision-"<>ToString[bad,InputForm],auditNormal[auditSpecific-auditExpected]===0],{bad,auditBadNames}];
   auditSpecific=auditHead[ReplacePart[auditArgs,2->auditMu1],auditPs,1];
   auditExpected=auditGenericFree/.LorentzIndex[auditNu1,D]->LorentzIndex[auditMu1,D];
   auditAssert[auditLabel<>"-external-trace",auditNormal[auditSpecific-auditExpected]===0];
   auditSpecific=auditHead[auditArgs,auditPs/.auditP2->auditP1,1];
   auditExpected=auditGenericFree/.auditP2->auditP1;
   auditAssert[auditLabel<>"-equal-valued-legs",auditNormal[auditSpecific-auditExpected]===0];
  ,{suffix,{"","Uncontracted"}}];
 ,{family,{2,3,5}}];
 Do[
  auditPs=If[family===2,{auditP1,auditP2},{auditP1,auditP2,auditP3}];
  auditArgs=If[family===2,auditIndices[2],auditTriples[2]];
  auditHead=Symbol["HorndeskiG"<>ToString[family]<>"`HorndeskiG"<>ToString[family]<>"Uncontracted"];
  auditRef=If[family===5,(hgSetup[auditEs[2],auditKs[2]];auditNormal[Total[Values[hgReference[0,1,auditPs]]]/.hgSP->SPD]),auditNormal[auditReference[family,0,1,auditEs[2],auditKs[2],auditPs]]];
  auditAssert["two-graviton-action-G"<>ToString[family],Expand[auditProject[auditHead[auditArgs,auditPs,1],2]-auditRef]===0];
 ,{family,{2,3,5}}];
 auditAssert["G5-order-four-upper-b-rejected",FailureQ[HorndeskiG5`HorndeskiG5[auditTriples[4],Table[Symbol["auditP"<>ToString[j]],{j,7}],3]]];
 auditAssert["G5-uncontracted-order-four-upper-b-rejected",FailureQ[HorndeskiG5`HorndeskiG5Uncontracted[auditTriples[4],Table[Symbol["auditP"<>ToString[j]],{j,7}],3]]];
]];
Close[auditMsg];If[FileByteCount[auditMsgPath]>0,auditStop["Unexpected diagnostics"]];
If[Length[auditChecks]=!=48,auditStop["Assertion-count mismatch"]];
Export[FileNameJoin[{auditRun,"completion.json"}],<|"Status"->"Completed","Passed"->1,"Failed"->0,"RawAssertions"->Length[auditChecks]|>,"RawJSON"];
Quit[0];
