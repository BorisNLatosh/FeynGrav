(* Fresh-kernel regression for the shipped G4 libraries and public importer.
   Run under an external whole-process-tree limit. Set HORNAUDIT_RUN_DIR to
   a fresh evidence directory. Optional FG_HORNDESKI_REPOSITORY selects a
   checkout; FG_HORNDESKI_LIBRARY_ROOT selects a candidate containing Libs/.
   This script never writes to the selected checkout or candidate libraries.
   The only altered libraries are failure fixtures inside the evidence directory.
   All tensor comparisons use generic D and unrestricted auxiliary vectors. *)
$HistoryLength=0;$FeynCalcStartupMessages=False;
auditScriptDirectory=DirectoryName[$InputFileName];
auditRepo=Environment["FG_HORNDESKI_REPOSITORY"];
If[!StringQ[auditRepo],auditRepo=DirectoryName[DirectoryName[auditScriptDirectory]]];
auditLibraryRoot=Environment["FG_HORNDESKI_LIBRARY_ROOT"];
If[!StringQ[auditLibraryRoot],auditLibraryRoot=auditRepo];
auditRun=Environment["HORNAUDIT_RUN_DIR"];
If[!StringQ[auditRun]||!DirectoryQ[auditRun],Print["A fresh HORNAUDIT_RUN_DIR is required."];Quit[2]];
auditMsgPath=FileNameJoin[{auditRun,"diagnostics.log"}];
auditMsg=OpenWrite[auditMsgPath];$Messages={auditMsg};auditChecks={};
auditWrite[]:=Export[FileNameJoin[{auditRun,"checks.json"}],auditChecks,"RawJSON"];
auditStop[reason_]:=(Print["STOP ",reason];auditWrite[];
 Export[FileNameJoin[{auditRun,"completion.json"}],<|"Status"->"Failed","Passed"->0,"Failed"->1,"Reason"->reason|>,"RawJSON"];Quit[2]);
SetAttributes[auditGuard,HoldAll];
auditGuard[expr_]:=Module[{result},result=CheckAbort[Check[expr,$Failed],$Failed];
 If[result===$Failed||!FreeQ[result,_Failure|$Failed|$Aborted],auditStop["Unexpected diagnostic or unsuccessful evaluation"]];result];
auditAssert[auditCheckLabel_,condition_]:=(AppendTo[auditChecks,<|"Name"->auditCheckLabel,"Passed"->TrueQ[condition]|>];
 auditWrite[];Print[If[TrueQ[condition],"PASS ","FAIL "],auditCheckLabel];If[!TrueQ[condition],auditStop[auditCheckLabel]]);

auditCases=Sort[Flatten[Table[Append[ab,n],{ab,{{0,1},{0,2},{1,1},{2,0},{2,1},{3,0},{4,0}}},{n,1,2}],1]];
auditName[c_List]:="HorndeskiG4_"<>StringRiffle[ToString /@c,"_"];
auditExpectedNames=Join[
 {"source-package-not-loaded","baseline-import","baseline-provenance","candidate-import",
  "fourteen-library-identities","candidate-source-paths","John-import","projected-batch-resolved"},
 Flatten[Table[Join[(auditName[c]<>"-"<>#& /@{"independent-action","negative-control","zero-coupling","equal-momenta"}),
  Table[auditName[c]<>"-symmetric-pair-"<>ToString[j],{j,c[[3]]}]],{c,auditCases}]],
 Flatten[Table[("John-n"<>ToString[n]<>"-"<>#& /@{"G4-G5","Einstein-action","negative-control"}),{n,1,2}]],
 {"empty-family-rejected","empty-family-preserves-definitions","empty-family-preserves-record",
  "failed-library-rejected","failed-library-preserves-definitions","failed-library-preserves-record",
  "cancelled-import-propagated","cancelled-import-preserves-definitions","cancelled-import-preserves-record",
  "repaired-import-retry","retry-restores-correct-definitions","retry-record-hashes","retry-record-paths",
  "main-three-argument-distinct","import-order-argument-unevaluated","John-definitions-preserved","John-record-preserved","one-FORM-call"}];
If[Length[auditExpectedNames]=!=109||!DuplicateFreeQ[auditExpectedNames],auditStop["Invalid assertion declaration"]];
Export[FileNameJoin[{auditRun,"expected-assertions.json"}],auditExpectedNames,"RawJSON"];

auditGuard[Get["FeynCalc`"];
 Block[{$Context="G4LibraryLoader`",$ContextPath={"FeynCalc`","System`"}},Get[FileNameJoin[{auditRepo,"FeynGrav.wl"}]];];
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
auditFORMCalls=0;
auditFORM[expr_]:=Module[{out},auditFORMCalls++;If[auditFORMCalls>1,auditStop["FORM call allowance exceeded"]];
 Export[FileNameJoin[{auditRun,"public-projection-input.wxf"}],expr,"WXF"];
 out=Block[{$ContextPath={"FeynCalc`","System`"}},Quiet[CalcFormConverter`FeynGravSimplify[FCI[expr],
  CalcFormConverter`FORMExecutable->"/usr/local/bin/form",CalcFormConverter`FORMThreads->1,
  CalcFormConverter`TensorDecomposition->False,TimeConstraint->60,
  CalcFormConverter`KeepFiles->True,CalcFormConverter`WorkingDirectory->auditRun],CalcFormConverter`CalcFormCalculate::files]];
 Export[FileNameJoin[{auditRun,"public-projection-output.wxf"}],out,"WXF"];
 If[!FreeQ[out,_Failure|$Failed|$Aborted],auditStop["FORM projection unsuccessful"]];auditNormal[out]];
auditProvenanceQ[info_]:=AssociationQ[info]&&Length[info["Files"]]===14&&
 Sort[FileNameTake /@Lookup[info["Files"],"Path"]]===Sort[auditName /@auditCases]&&
 AllTrue[info["Files"],#["SHA256"]===FileHash[#["Path"],"SHA256","HexString"]&];

auditGuard[
 auditAssert["source-package-not-loaded",Length[DownValues[HorndeskiG4`HorndeskiG4]]===0];
 auditAssert["baseline-import",FeynGrav`FeynGravImportHorndeskiG4[]===Null];
 auditAssert["baseline-provenance",auditProvenanceQ[FeynGrav`FeynGravLibraryInformation[FeynGrav`FeynGravImportHorndeskiG4]]];
 auditAssert["candidate-import",Block[{FeynGrav`Private`packageDirectory=auditLibraryRoot<>"/"},FeynGrav`FeynGravImportHorndeskiG4[]]===Null];
 auditInfo=FeynGrav`FeynGravLibraryInformation[FeynGrav`FeynGravImportHorndeskiG4];
 auditAssert["fourteen-library-identities",auditProvenanceQ[auditInfo]];
 auditAssert["candidate-source-paths",Sort[Lookup[auditInfo["Files"],"Path"]]===Sort[ExpandFileName[FileNameJoin[{auditLibraryRoot,"Libs",auditName[#]}]]& /@auditCases]];
 auditAssert["John-import",FeynGrav`FeynGravImportHorndeskiG5[]===Null];
 auditJohnDefs=DownValues[FeynGrav`HorndeskiG5];
 auditJohnInfo=FeynGrav`FeynGravLibraryInformation[FeynGrav`FeynGravImportHorndeskiG5];
 auditCoupling=7+2 I;
 auditPublic=Table[FCI[FeynGrav`HorndeskiG4[auditTriples[c[[3]]],auditPs[c[[1]]+2c[[2]]],c[[2]],auditCoupling]],{c,auditCases}];
 auditTags=Table[Symbol["auditPublic"<>ToString[j]],{j,14}];
 auditJohnTags={auditJohn1,auditJohn2};
 auditJohnPublic=Table[FeynGrav`HorndeskiG5[auditTriples[n],auditPs[2],0,-auditBeta],{n,1,2}];
 auditProjected=auditFORM[(Total[Table[auditTags[[j]]auditPublic[[j]]auditProjection[auditCases[[j,3]]],{j,14}]]+
  Total[Table[auditJohnTags[[n]]auditJohnPublic[[n]]auditProjection[n],{n,1,2}]])/.FeynGrav`\[Kappa]->Global`\[Kappa]];
 auditAssert["projected-batch-resolved",FreeQ[auditProjected,_LorentzIndex|_Failure|$Failed|$Aborted]];
 auditG4John=<||>;
 Do[
  {auditA,auditB,auditN}=auditCases[[c]];auditLabel=auditName[auditCases[[c]]];auditMomenta=auditPs[auditA+2auditB];
  hgSetup[auditEs[auditN],auditKs[auditN]];
  auditReference=auditHG[Total[Values[hg4ReferenceBlocks[auditA,auditB,auditMomenta]]]];
  auditActual=auditNormal[Coefficient[auditProjected,auditTags[[c]]]];
  auditAssert[auditLabel<>"-independent-action",Expand[auditActual-auditCoupling auditReference]===0];
  auditAssert[auditLabel<>"-negative-control",Expand[auditActual-auditCoupling auditReference/2]=!=0];
  auditAssert[auditLabel<>"-zero-coupling",FeynGrav`HorndeskiG4[auditTriples[auditN],auditMomenta,auditB,0]===0];
  auditAssert[auditLabel<>"-equal-momenta",Expand[FCI[FeynGrav`HorndeskiG4[auditTriples[auditN],auditMomenta/.auditP2->auditP1,auditB,auditCoupling]]-(auditPublic[[c]]/.auditP2->auditP1)]===0];
  Do[auditSwap={auditTriples[auditN][[3j-2]]->auditTriples[auditN][[3j-1]],auditTriples[auditN][[3j-1]]->auditTriples[auditN][[3j-2]]};
   auditAssert[auditLabel<>"-symmetric-pair-"<>ToString[j],Expand[(auditPublic[[c]]/.auditSwap)-auditPublic[[c]]]===0],{j,auditN}];
  If[auditA===0&&auditB===1,AssociateTo[auditG4John,auditN->(-auditBeta/2) auditActual/auditCoupling]];
  Export[FileNameJoin[{auditRun,auditLabel<>"-public.wxf"}],<|"Public"->auditActual,"Independent"->auditCoupling auditReference|>,"WXF"];
 ,{c,14}];
 Do[
  hgSetup[auditEs[n],auditKs[n]];
  auditEinstein=hgTrace2[hgRicci,{{0,1,auditP1,auditP2}}]-hgMul[hgTrace[hgRicci],hgInv[auditP1,auditP2]]/2;
  auditJohnRef=auditHG[-2 I auditBeta Global`\[Kappa]^n Last[hgMul[hgVolume,auditEinstein]]];
  auditJohnFromG5=auditNormal[Coefficient[auditProjected,auditJohnTags[[n]]]];
  auditConservation={auditP2->-auditP1-Total[auditKs[n]]};
  auditAssert["John-n"<>ToString[n]<>"-G4-G5",auditNormal[(auditG4John[n]-auditJohnFromG5)/.auditConservation]===0];
  auditAssert["John-n"<>ToString[n]<>"-Einstein-action",auditNormal[(auditG4John[n]-auditJohnRef)/.auditConservation]===0];
  auditAssert["John-n"<>ToString[n]<>"-negative-control",auditNormal[(auditG4John[n]+auditJohnRef)/.auditConservation]=!=0];
 ,{n,1,2}];

 (* Test transactional recovery using isolated copies. No library read error
    should alter the last good definitions or their provenance. *)
 auditGoodDefs=DownValues[FeynGrav`HorndeskiG4];
 auditEmpty=CreateDirectory[FileNameJoin[{auditRun,"empty"}]];CreateDirectory[FileNameJoin[{auditEmpty,"Libs"}]];
 auditMissing=Block[{FeynGrav`Private`packageDirectory=auditEmpty<>"/"},FeynGrav`FeynGravImportHorndeskiG4[]];
 auditAssert["empty-family-rejected",FailureQ[auditMissing]];
 auditAssert["empty-family-preserves-definitions",DownValues[FeynGrav`HorndeskiG4]===auditGoodDefs];
 auditAssert["empty-family-preserves-record",FeynGrav`FeynGravLibraryInformation[FeynGrav`FeynGravImportHorndeskiG4]===auditInfo];
 auditBroken=CreateDirectory[FileNameJoin[{auditRun,"broken"}]];CreateDirectory[FileNameJoin[{auditBroken,"Libs"}]];
 Scan[CopyFile[#["Path"],FileNameJoin[{auditBroken,"Libs",FileNameTake[#["Path"]]}]]&,auditInfo["Files"]];
 auditBadPath=FileNameJoin[{auditBroken,"Libs","HorndeskiG4_4_0_2"}];
 auditBackup=FileNameJoin[{auditRun,"repair-original"}];CopyFile[auditBadPath,auditBackup];
 Export[auditBadPath,"$Failed\n","Text"];
 auditBadResult=Block[{FeynGrav`Private`packageDirectory=auditBroken<>"/"},FeynGrav`FeynGravImportHorndeskiG4[]];
 auditAssert["failed-library-rejected",FailureQ[auditBadResult]];
 auditAssert["failed-library-preserves-definitions",DownValues[FeynGrav`HorndeskiG4]===auditGoodDefs];
 auditAssert["failed-library-preserves-record",FeynGrav`FeynGravLibraryInformation[FeynGrav`FeynGravImportHorndeskiG4]===auditInfo];
 Export[auditBadPath,"Abort[]\n","Text"];
 auditCancelled=CheckAbort[Block[{FeynGrav`Private`packageDirectory=auditBroken<>"/"},FeynGrav`FeynGravImportHorndeskiG4[]],auditCancellationMarker];
 auditAssert["cancelled-import-propagated",auditCancelled===auditCancellationMarker];
 auditAssert["cancelled-import-preserves-definitions",DownValues[FeynGrav`HorndeskiG4]===auditGoodDefs];
 auditAssert["cancelled-import-preserves-record",FeynGrav`FeynGravLibraryInformation[FeynGrav`FeynGravImportHorndeskiG4]===auditInfo];
 CopyFile[auditBackup,auditBadPath,OverwriteTarget->True];
 auditAssert["repaired-import-retry",Block[{FeynGrav`Private`packageDirectory=auditBroken<>"/"},FeynGrav`FeynGravImportHorndeskiG4[]]===Null];
 auditAssert["retry-restores-correct-definitions",DownValues[FeynGrav`HorndeskiG4]===auditGoodDefs];
 auditRetryInfo=FeynGrav`FeynGravLibraryInformation[FeynGrav`FeynGravImportHorndeskiG4];
 auditAssert["retry-record-hashes",auditProvenanceQ[auditRetryInfo]];
 auditAssert["retry-record-paths",Sort[Lookup[auditRetryInfo["Files"],"Path"]]===Sort[ExpandFileName[FileNameJoin[{auditBroken,"Libs",auditName[#]}]]& /@auditCases]];
 auditAssert["main-three-argument-distinct",Head[FeynGrav`HorndeskiG4[auditTriples[1],auditPs[2],1]]===FeynGrav`HorndeskiG4];
 auditAssert["import-order-argument-unevaluated",Head[FeynGrav`FeynGravImportHorndeskiG4[1]]===FeynGrav`FeynGravImportHorndeskiG4];
 auditAssert["John-definitions-preserved",DownValues[FeynGrav`HorndeskiG5]===auditJohnDefs];
 auditAssert["John-record-preserved",FeynGrav`FeynGravLibraryInformation[FeynGrav`FeynGravImportHorndeskiG5]===auditJohnInfo];
 auditAssert["one-FORM-call",auditFORMCalls===1];
 Export[FileNameJoin[{auditRun,"import-provenance.wxf"}],<|"Candidate"->auditInfo,"Repaired"->auditRetryInfo,"John"->auditJohnInfo|>,"WXF"];
];
Close[auditMsg];
If[FileByteCount[auditMsgPath]>0,auditStop["Unexpected diagnostics"]];
If[Lookup[auditChecks,"Name"]=!=auditExpectedNames,auditStop["Assertion ledger mismatch"]];
Export[FileNameJoin[{auditRun,"completion.json"}],<|"Status"->"Completed","Passed"->1,"Failed"->0,"RawAssertions"->109,"FORMCalls"->auditFORMCalls|>,"RawJSON"];
Quit[0];
