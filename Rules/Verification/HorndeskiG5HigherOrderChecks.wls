(* RULE-02 focused higher-order regression. Use one fresh externally bounded
   kernel; this script does not establish process memory/time limits.
   Set FEYNGRAV_RULE_AUDIT_OUTPUT to an existing fresh evidence directory.
   Optional ROOT and FORM variables match HorndeskiChecks.wls.
   Full n4 action comparisons are retained separately in the verification report.
   The n5 and n4,b2 checks below verify the entire fixed T8 sector, not full vertices. *)
$HistoryLength=0;$FeynCalcStartupMessages=False;
auditScriptDirectory=DirectoryName[$InputFileName];
auditRoot=Environment["FEYNGRAV_RULE_AUDIT_ROOT"];
If[!StringQ[auditRoot]||auditRoot==="",auditRoot=DirectoryName[DirectoryName[auditScriptDirectory]]];
auditForm=Environment["FEYNGRAV_RULE_AUDIT_FORM"];
If[!StringQ[auditForm]||auditForm==="",auditForm=Automatic];
auditRun=Environment["FEYNGRAV_RULE_AUDIT_OUTPUT"];
If[!StringQ[auditRun]||!DirectoryQ[auditRun],Print["An existing external evidence directory is required."];Quit[2]];
If[AnyTrue[{"checks.json","completion.json","diagnostics.log","expected-assertions.json"},FileExistsQ[FileNameJoin[{auditRun,#}]]&],Print["Scientific output files already exist."];Quit[2]];
auditMsgPath=FileNameJoin[{auditRun,"diagnostics.log"}];
auditMsg=OpenWrite[auditMsgPath];$Messages={auditMsg};auditChecks={};
auditExpected={};
auditWrite[]:=Export[FileNameJoin[{auditRun,"checks.json"}],auditChecks,"RawJSON"];
auditStop[reason_]:=(Print["STOP ",reason];auditWrite[];Export[FileNameJoin[{auditRun,"completion.json"}],<|"Status"->"Failed","Passed"->0,"Failed"->1,"Reason"->ToString[reason]|>,"RawJSON"];Quit[2]);
SetAttributes[auditGuard,HoldAll];
auditGuard[expr_]:=Module[{out},out=CheckAbort[Check[expr,$Failed],$Failed];If[out===$Failed||!FreeQ[out,_Failure|$Aborted|$Failed],auditStop["Unexpected message/failure/abort"]];out];
auditAssert[auditCheckLabel_,condition_]:=(AppendTo[auditChecks,<|"Name"->auditCheckLabel,"Passed"->TrueQ[condition]|>];auditWrite[];Print[If[TrueQ[condition],"PASS ","FAIL "],auditCheckLabel];If[!TrueQ[condition],auditStop[auditCheckLabel]]);
auditGuard[Get["FeynCalc`"]];
$ContextPath={"FeynCalc`","System`"};
Get[FileNameJoin[{auditScriptDirectory,"HorndeskiReference.wl"}]];
auditGuard[Block[{$ContextPath={"FeynCalc`","System`"}},Get["FeynGrav`"];]];
auditGuard[Block[{$ContextPath={"FeynCalc`","System`"}},Get[FileNameJoin[{auditRoot,"Rules","HorndeskiG5.wl"}]];]];
auditFORMCalls=0;auditFORMLimit=8;

auditNormal[expr_]:=Expand[ExpandScalarProduct[Contract[FCI[expr]]]];
auditHG[expr_]:=auditNormal[expr/.hgSP->SPD];
auditTriples[n_]:=Flatten[Table[{Symbol["auditMu"<>ToString[j]],Symbol["auditNu"<>ToString[j]],Symbol["auditK"<>ToString[j]]},{j,n}]];
auditEs[n_]:=Table[Symbol["auditE"<>ToString[j]],{j,n}];
auditKs[n_]:=Table[Symbol["auditK"<>ToString[j]],{j,n}];
auditPs[n_]:=Table[Symbol["auditP"<>ToString[j]],{j,n}];
auditSerialMap[f_,items_List,opts___]:=RuleValidation`RuleRequire[Map[Function[item,RuleValidation`RuleBoundary[f[item]]],items]];
auditProject[expr_,n_]:=auditNormal[expr Times@@Table[FVD[auditEs[n][[j]],auditTriples[n][[3j-2]]]FVD[auditEs[n][[j]],auditTriples[n][[3j-1]]],{j,n}]];
auditFinish[count_]:=(Close[auditMsg];If[FileByteCount[auditMsgPath]>0,auditStop["Unexpected diagnostics"]];If[Length[auditChecks]=!=count||Lookup[auditChecks,"Name"]=!=auditExpected,auditStop["Assertion ledger mismatch"]];Export[FileNameJoin[{auditRun,"completion.json"}],<|"Status"->"Completed","Passed"->1,"Failed"->0,"RawAssertions"->Length[auditChecks]|>,"RawJSON"];Quit[0]);

auditExpect[names_List]:=(auditExpected=names;If[!DuplicateFreeQ[names],auditStop["Duplicate assertions"]];Export[FileNameJoin[{auditRun,"expected-assertions.json"}],names,"RawJSON"]);
auditFORM[expr_]:=Module[{out},auditFORMCalls++;If[auditFORMCalls>auditFORMLimit,auditStop["FORM call allowance exceeded"]];out=Block[{$ContextPath={"FeynCalc`","System`"}},Quiet[CalcFormConverter`FeynGravSimplify[FCI[expr],CalcFormConverter`FORMExecutable->auditForm,CalcFormConverter`FORMThreads->1,CalcFormConverter`TensorDecomposition->False,TimeConstraint->60,CalcFormConverter`KeepFiles->True,CalcFormConverter`WorkingDirectory->auditRun],CalcFormConverter`CalcFormCalculate::files]];If[!FreeQ[out,_Failure|$Failed|$Aborted],auditStop["FORM calculation unsuccessful"]];auditNormal[out]];
auditNames=Join[Table["TII-dispatch-n"<>ToString[n],{n,0,6}],
 {"TII-upper-b-rejected","TII-upper-b-skips-kernels","public-upper-b-rejected","public-upper-b-skips-permutations",
 "public-b0-independent","public-n4-assembly","public-n5-assembly","dependency-failure","failed-call-not-cached",
 "retry-after-failure","abort-propagates","abort-not-cached","wrong-arity","malformed-triple",
 "n3-volume-regression","n4-volume-square","n4-inverse-identity","reference-has-no-kinematic-assignments"}];

auditCases={{4,2},{5,1}};
auditHigherNames=Flatten[Table[("n"<>ToString[c[[1]]]<>"-b"<>ToString[c[[2]]]<>"-"<>#& /@{"T8-resolved","independent-T8","negative-control","remaining-label-symmetry"}),{c,auditCases}]];
auditExpect[Join[auditNames,auditHigherNames]];
auditGuard[
 Block[{HorndeskiG5`Private`T5,HorndeskiG5`Private`T6,HorndeskiG5`Private`T7,HorndeskiG5`Private`T8},
  HorndeskiG5`Private`T5[args___]:=auditZ5;
  HorndeskiG5`Private`T6[args___]:=auditZ6;
  HorndeskiG5`Private`T7[args___]:=auditZ7;
  HorndeskiG5`Private`T8[args___]:=auditZ8;
  Do[auditAssert["TII-dispatch-n"<>ToString[n],
    HorndeskiG5`Private`TII[auditTriples[n],auditPs[3],1]===Total[Take[{auditZ5,auditZ6,auditZ7,auditZ8},Min[n+1,4]]]],{n,0,6}];
 ];
 auditHits=0;
 Block[{HorndeskiG5`Private`T5},HorndeskiG5`Private`T5[args___]:=(auditHits++;1);
  auditAssert["TII-upper-b-rejected",FailureQ[HorndeskiG5`Private`TII[auditTriples[4],auditPs[7],3]]];
  auditAssert["TII-upper-b-skips-kernels",auditHits===0];];
 Block[{HorndeskiG5`Private`labelledPermutations},HorndeskiG5`Private`labelledPermutations[x_]:=(auditHits++;{x});
  auditAssert["public-upper-b-rejected",And@@Flatten[Table[FailureQ[f[auditTriples[n],auditPs[2b+1],b]],
    {f,{HorndeskiG5`HorndeskiG5,HorndeskiG5`HorndeskiG5Uncontracted}},{n,1,4},{b,{6-Min[n,3]}}]]];
  auditAssert["public-upper-b-skips-permutations",auditHits===0];];
 Block[{HorndeskiG5`Private`TI,HorndeskiG5`Private`TII},
  HorndeskiG5`Private`TI[args___]:=1;HorndeskiG5`Private`TII[args___]:=Abort[];
  auditAssert["public-b0-independent",HorndeskiG5`HorndeskiG5Uncontracted[auditTriples[4],auditPs[1],0]===12 I Global`\[Kappa]^4];
 ];
 Block[{HorndeskiG5`Private`TI,HorndeskiG5`Private`TII},
  HorndeskiG5`Private`TI[args___]:=auditTi;HorndeskiG5`Private`TII[args___]:=auditTii;
  Do[auditAssert["public-n"<>ToString[n]<>"-assembly",Expand[
   HorndeskiG5`HorndeskiG5Uncontracted[auditTriples[n],auditPs[3],1]-
   (-I/2)Global`\[Kappa]^n Factorial[n]Factorial[3](auditTi-2auditTii)]===0],{n,4,5}];
 ];
 auditRetryTriples=auditTriples[4]/.auditK1->auditRetryK;
 auditBefore=DownValues[HorndeskiG5`Private`TII];
 Block[{HorndeskiG5`Private`T5},HorndeskiG5`Private`T5[args___]:=Failure["Controlled",<||>];
  auditAssert["dependency-failure",FailureQ[HorndeskiG5`Private`TII[auditRetryTriples,auditPs[3],1]]];];
 auditAssert["failed-call-not-cached",DownValues[HorndeskiG5`Private`TII]===auditBefore];
 Block[{HorndeskiG5`Private`T5,HorndeskiG5`Private`T6,HorndeskiG5`Private`T7,HorndeskiG5`Private`T8},
  HorndeskiG5`Private`T5[args___]:=1;HorndeskiG5`Private`T6[args___]:=2;
  HorndeskiG5`Private`T7[args___]:=3;HorndeskiG5`Private`T8[args___]:=4;
  auditAssert["retry-after-failure",HorndeskiG5`Private`TII[auditRetryTriples,auditPs[3],1]===10];];
 auditBefore=DownValues[HorndeskiG5`Private`TII];
 Block[{HorndeskiG5`Private`T5},HorndeskiG5`Private`T5[args___]:=Abort[];
  auditAssert["abort-propagates",CheckAbort[HorndeskiG5`Private`TII[auditTriples[4]/.auditK1->auditAbortK,auditPs[3],1],auditAborted]===auditAborted];];
 auditAssert["abort-not-cached",DownValues[HorndeskiG5`Private`TII]===auditBefore];
 auditAssert["wrong-arity",FailureQ[HorndeskiG5`HorndeskiG5[{},{}]]];
 auditAssert["malformed-triple",FailureQ[HorndeskiG5`HorndeskiG5[{auditMu},auditPs[3],1]]];
 hgSetup[auditEs[3],auditKs[3]];
 auditV3=Times@@(hgSP[#,#]& /@auditEs[3])/8-
 Total[Table[hgSP[auditEs[3][[i]],auditEs[3][[i]]]hgSP[auditEs[3][[Mod[i,3]+1]],auditEs[3][[Mod[i+1,3]+1]]]^2,{i,3}]]/4+
 hgSP[auditE1,auditE2]hgSP[auditE2,auditE3]hgSP[auditE3,auditE1];
 auditAssert["n3-volume-regression",Expand[Last[hgVolume]-auditV3]===0];
 hgSetup[auditEs[4],auditKs[4]];
 auditDet=Prepend[Table[auditLabels=Pick[Range[4],Table[BitGet[m,i-1],{i,4}],1];
   Det[Outer[hgSP,auditEs[4][[auditLabels]],auditEs[4][[auditLabels]]]],{m,1,15}],1];
 auditAssert["n4-volume-square",Expand[hgMul[hgVolume,hgVolume]-auditDet]===ConstantArray[0,16]];
 auditInvProduct=hgInv[auditP,auditQ]+Total[Table[hgShift[hgInv[auditP,auditEs[4][[j]]]hgSP[auditEs[4][[j]],auditQ],2^(j-1)],{j,4}]];
 auditAssert["n4-inverse-identity",Expand[auditInvProduct-hgSP[auditP,auditQ]hgUnit]===hgZero];
 auditAssert["reference-has-no-kinematic-assignments",OwnValues[D]==={}&&OwnValues[auditP1]==={}&&OwnValues[auditK1]==={}&&OwnValues[auditE1]==={}];
];
(* Independent fixed connection placements. Only the n-3 spectator metric
   labels vary; each of the last three differentiated metric legs is explicit.
   A_ab = (k_a e_b+e_a k_b)g^{-1}(e,p)/2-e_a e_b g^{-1}(k,p)/2.
   These are the connection pieces of the covariant Hessian, derived directly
   from the metric derivative. Division by (n-3)! matches the averaged source C. *)
auditConnection[ei_,ki_,pi_]:=hgCombine[Flatten[Table[
 {{m,hgInv[ei,pi][[m+1]]/2,ki,ei},
  {m,hgInv[ei,pi][[m+1]]/2,ei,ki},
  {m,-hgInv[ki,pi][[m+1]]/2,ei,ei}},{m,0,hgSize-1}],1]];
auditGuard[Block[{RuleValidation`RuleParallelMap=auditSerialMap},
 Do[
  {auditN,auditB}=auditCase;auditLabel="n"<>ToString[auditN]<>"-b"<>ToString[auditB];
  Print["BEGIN ",auditLabel," T8 ",AbsoluteTime[]];
  auditSeed=Block[{HorndeskiG5`Private`$scalarDummyIndices=Table[Unique["auditD"],{2auditB}]},
    HorndeskiG5`Private`T8[auditTriples[auditN],auditPs[2auditB+1],auditB]];
  auditProjected=FCI[auditSeed]/.Flatten[Table[{LorentzIndex[auditTriples[auditN][[3i-2]],D]->Momentum[auditEs[auditN][[i]],D],LorentzIndex[auditTriples[auditN][[3i-1]],D]->Momentum[auditEs[auditN][[i]],D]},{i,auditN}]];
  auditActual=auditFORM[auditProjected];
  auditAssert[auditLabel<>"-T8-resolved",FreeQ[auditActual,_LorentzIndex|_Failure|$Failed]];
  hgSetup[Take[auditEs[auditN],auditN-3],Take[auditKs[auditN],auditN-3]];
  auditHs=Table[auditConnection[auditEs[auditN][[auditN-3+i]],auditKs[auditN][[auditN-3+i]],auditPs[2auditB+1][[2auditB-2+i]]],{i,3}];
  auditTr=hgTrace /@auditHs;
  auditK=hgMul[hgMul[auditTr[[1]],auditTr[[2]]],auditTr[[3]]]-
   Total[Table[auditRest=Delete[Range[3],i];hgMul[auditTr[[i]],hgTrace2[auditHs[[auditRest[[1]]]],auditHs[[auditRest[[2]]]]]],{i,3}]]+
   2hgTrace3[auditHs[[1]],auditHs[[2]],auditHs[[3]]];
  auditK=hgMul[hgVolume,auditK];
  Do[auditK=hgMul[auditK,hgInv[auditPs[2auditB+1][[2i-1]],auditPs[2auditB+1][[2i]]]],{i,auditB-1}];
  auditRef=auditHG[Last[auditK]/(3Factorial[auditN-3])];
  auditAssert[auditLabel<>"-independent-T8",Expand[auditActual-auditRef]===0];
  auditAssert[auditLabel<>"-negative-control",Expand[auditActual-auditRef/2]=!=0];
  auditSym=If[auditN-3===1,True,Expand[(auditActual/.{auditE1->auditE2,auditE2->auditE1})-auditActual]===0];
  auditAssert[auditLabel<>"-remaining-label-symmetry",auditSym];
  Export[FileNameJoin[{auditRun,auditLabel<>"-T8.wxf"}],<|"Actual"->auditActual,"Reference"->auditRef|>,"WXF"];
 ,{auditCase,auditCases}];
]];
auditFinish[33];
