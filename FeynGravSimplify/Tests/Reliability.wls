(* Fresh-kernel reliability suite: load-guard failure/recovery on isolated
   temporary copies, store preservation across Needs/re-Get/main load, and
   reporting status/cancellation for success, backend failure and controlled
   abort. Self-contained: no Get of scripts that Quit. Expected diagnostics are
   the Get::noopen for the controlled missing companion and the
   FeynGravSimplify::files retention notices from the failure/abort calls; the
   retained job directories are reported and intentionally kept for review. *)
$FeynCalcStartupMessages=False;Get["FeynCalc`"];
Get[FileNameJoin[{DirectoryName[$InputFileName],"Completion.wl"}]];
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
passed=0;failed=0;
check[caseLabel_,test_]:=If[TrueQ[test],passed++,failed++;Print["FAIL: ",caseLabel]];
tag[x_]:=If[FailureQ[x],x[[1]],None];
mismatch[label_,actual_,expected_]:=Print["MISMATCH ",label," actual=",
  ToString[actual,InputForm]," expected=",ToString[expected,InputForm]];
equal[a_,b_]:=!FailureQ[a]&&!FailureQ[b]&&TrueQ[Together[ExpandScalarProduct[Contract[FeynAmpDenominatorExplicit[FCI[a-b]]]]]===0];
sectionPassed=0;sectionFailed=0;
section[title_]:=(
  Print["SECTION ",title," passed=",passed-sectionPassed," failed=",failed-sectionFailed];
  sectionPassed=passed;sectionFailed=failed);

(* ---- Load-guard failure/recovery on isolated temporary copies ------------- *)
tempRoot=FileNameJoin[{$TemporaryDirectory,"fg-reliability-"<>CreateUUID[]}];
CreateDirectory[tempRoot];
CopyDirectory[FileNameJoin[{root,"CalcFormConverter"}],FileNameJoin[{tempRoot,"CalcFormConverter"}]];
CopyDirectory[FileNameJoin[{root,"FeynGravSimplify"}],FileNameJoin[{tempRoot,"FeynGravSimplify"}]];
tempConverter=FileNameJoin[{tempRoot,"CalcFormConverter","CalcFormConverter.wl"}];
tempCompanion=FileNameJoin[{tempRoot,"FeynGravSimplify","Contractor.wl"}];
savedCompanion=FileNameJoin[{tempRoot,"Contractor.wl.saved"}];
CopyFile[tempCompanion,savedCompanion];
DeleteFile[tempCompanion];
Get[tempConverter];
check["missing companion leaves guard open",
  !TrueQ[FeynGrav`Simplification`Private`$FacadeLoadedMark]];
check["missing companion has no callable Contractor preparation",
  Length[DownValues[FeynGrav`Simplification`Private`contractorPreparation]]===0];
check["missing companion has no callable Contractor discovery",
  Length[DownValues[FeynGrav`Simplification`Private`resolveExecutable]]===0];
CopyFile[savedCompanion,tempCompanion,OverwriteTarget -> True];
Get[tempConverter];
check["repair completes load",
  TrueQ[FeynGrav`Simplification`Private`$FacadeLoadedMark]&&
  Length[DownValues[FeynGrav`Simplification`Private`simplify]]>0&&
  Length[DownValues[FeynGrav`Simplification`Private`failure]]>0&&
  Length[DownValues[CalcFormConverter`FeynGravSimplify]]>0&&
  Length[Options[CalcFormConverter`FeynGravSimplify]]>0];
(* Loader scratch-state assertions live in Loading.wls; recovery here is tested
   through observable marker, definition and store behaviour. *)
(* New Stage A companions: missing codec/coordination also leave the guard open
   and are repaired from the repository copy. Temporary copies only. *)
DeleteFile[FileNameJoin[{tempRoot,"FeynGravSimplify","RestrictedCodec.wl"}]];
Clear[FeynGrav`Simplification`Private`$FacadeLoadedMark];
Get[tempConverter];
check["missing codec leaves guard open",
  !TrueQ[FeynGrav`Simplification`Private`$FacadeLoadedMark]&&
  Length[DownValues[FeynGrav`Simplification`Private`encode]]===0];
CopyFile[FileNameJoin[{root,"FeynGravSimplify","RestrictedCodec.wl"}],
  FileNameJoin[{tempRoot,"FeynGravSimplify","RestrictedCodec.wl"}],OverwriteTarget -> True];
Get[tempConverter];
check["codec repair completes",
  TrueQ[FeynGrav`Simplification`Private`$FacadeLoadedMark]&&
  Length[DownValues[FeynGrav`Simplification`Private`encode]]>0];
DeleteFile[FileNameJoin[{tempRoot,"FeynGravSimplify","RestrictedCoordination.wl"}]];
Clear[FeynGrav`Simplification`Private`$FacadeLoadedMark];
Get[tempConverter];
check["missing coordination leaves guard open",
  !TrueQ[FeynGrav`Simplification`Private`$FacadeLoadedMark]&&
  Length[DownValues[FeynGrav`Simplification`Private`simplify]]===0];
CopyFile[FileNameJoin[{root,"FeynGravSimplify","RestrictedCoordination.wl"}],
  FileNameJoin[{tempRoot,"FeynGravSimplify","RestrictedCoordination.wl"}],OverwriteTarget -> True];
Get[tempConverter];
check["coordination repair completes",
  TrueQ[FeynGrav`Simplification`Private`$FacadeLoadedMark]&&
  Length[DownValues[FeynGrav`Simplification`Private`simplify]]>0];
(* Empty companion with previous definitions present: the retry clears the
   partial interface before loading, so stale definitions cannot certify it. *)
Clear[FeynGrav`Simplification`Private`$FacadeLoadedMark];
Export[tempCompanion,"(* deliberately empty companion *)","Text"];
Get[tempConverter];
check["empty companion leaves guard open",
  !TrueQ[FeynGrav`Simplification`Private`$FacadeLoadedMark]];
check["empty companion clears stale Contractor preparation",
  Length[DownValues[FeynGrav`Simplification`Private`contractorPreparation]]===0];
check["empty companion clears stale Contractor discovery",
  Length[DownValues[FeynGrav`Simplification`Private`resolveExecutable]]===0];
CopyFile[savedCompanion,tempCompanion,OverwriteTarget -> True];
Get[tempConverter];
check["repair after empty companion",
  TrueQ[FeynGrav`Simplification`Private`$FacadeLoadedMark]&&
  Length[DownValues[FeynGrav`Simplification`Private`simplify]]>0];
Clear[FeynGrav`Simplification`Private`$FacadeLoadedMark];
Get[FileNameJoin[{root,"CalcFormConverter","CalcFormConverter.wl"}]];
check["repository reload succeeds",
  TrueQ[FeynGrav`Simplification`Private`$FacadeLoadedMark]&&
  Length[DownValues[FeynGrav`Simplification`Private`simplify]]>0];
DeleteDirectory[tempRoot,DeleteContents->True];
section["L load-guard recovery"];
If[failed>0,
  Print["STOP: loading prerequisite checks failed; dependent checks not run."];
  Quit[1]];

(* ---- Store preservation across Needs, re-Get and main load ---------------- *)
SetOptions[CalcFormConverter`FeynGravSimplify,CalcFormConverter`FORMThreads -> 4];
SetOptions[CalcFormConverter`FeynGravFORMCheck,System`TimeConstraint -> 7];
ownerCheckStore=Options[CalcFormConverter`CalcFormCheck];
Needs["CalcFormConverter`"];
Get[FileNameJoin[{root,"CalcFormConverter","CalcFormConverter.wl"}]];
check["needs/re-get preserve sentinels",
  (CalcFormConverter`FORMThreads /. Options[CalcFormConverter`FeynGravSimplify])===4&&
  (System`TimeConstraint /. Options[CalcFormConverter`FeynGravFORMCheck])===7];
Get[FileNameJoin[{root,"FeynGrav.wl"}]];
check["main package loaded",NameQ["FeynGrav`FeynGravInitialized"]&&TrueQ[FeynGrav`FeynGravInitialized]];
check["main load preserves sentinels",
  (CalcFormConverter`FORMThreads /. Options[CalcFormConverter`FeynGravSimplify])===4&&
  (System`TimeConstraint /. Options[CalcFormConverter`FeynGravFORMCheck])===7];
check["main load leaves owner stores unchanged",Options[CalcFormConverter`CalcFormCheck]===ownerCheckStore];
SetOptions[CalcFormConverter`FeynGravSimplify,CalcFormConverter`FORMThreads -> Automatic];
SetOptions[CalcFormConverter`FeynGravFORMCheck,System`TimeConstraint -> 10];
check["sentinels restored",
  (CalcFormConverter`FORMThreads /. Options[CalcFormConverter`FeynGravSimplify])===Automatic&&
  (System`TimeConstraint /. Options[CalcFormConverter`FeynGravFORMCheck])===10];
section["S store preservation"];

(* ---- Restricted algebra and policy parity (accepted baseline control) ---- *)
fixture=Environment["FG_CONTRACTOR_TEST_FIXTURE"];
runRestricted[x_,opts___]:=Quiet[FeynGravSimplify[x,System`Method -> "FORMBenchmark",
  CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 20,opts],FeynGravSimplify::files];
parityAssert[label_,test_,actual_,expected_]:=(check[label,test];
  If[!TrueQ[test],mismatch[label,actual,expected]]);
parityActual=runRestricted[MTD[mu,mu]];
parityAssert["parity tensor trace",parityActual===D,parityActual,D];
parityActual=runRestricted[MTD[mu,nu] FVD[p,mu]];
parityExpected=FCI[FVD[p,nu]];
parityAssert["parity free index",parityActual===parityExpected,parityActual,parityExpected];
parityActual=runRestricted[(2+3 I)/7];
parityAssert["parity exact number",parityActual===(2+3 I)/7,parityActual,(2+3 I)/7];
parityActual=runRestricted[(MTD[mu,nu]+FVD[p,mu] FVD[p,nu])^2];
parityExpected=D+2 SPD[p]+SPD[p]^2;
parityAssert["parity tensor power",equal[parityActual,parityExpected],parityActual,parityExpected];
parityActual=runRestricted[FVD[p,mu] FVD[q,mu] FAD[{p-q,m}]];
parityExpected=SPD[p,q] FAD[{p-q,m}];
parityAssert["parity propagator",equal[parityActual,parityExpected],parityActual,parityExpected];
parityActual=tag[runRestricted[GAD[mu]]];
parityAssert["parity invalid input",parityActual==="UnsupportedExpression",parityActual,"UnsupportedExpression"];
parityActual=tag[runRestricted[MTD[mu,mu],FeynCalc`LoopMomenta -> {q}]];
parityAssert["parity capability refusal",parityActual==="MethodCapability",parityActual,"MethodCapability"];
parityActual=tag[runRestricted[a,CalcFormConverter`FORMExecutable -> 42]];
parityAssert["parity invalid executable",parityActual==="InvalidOption",parityActual,"InvalidOption"];
selectedDelay=0;
parityActual=!FailureQ[FeynGravSimplify[a,System`Method -> "FORMBenchmark",
  System`TimeConstraint -> 10,CalcFormConverter`FORMExecutable :> (selectedDelay++; fixture),
  CalcFormConverter`FORMThreads :> (selectedDelay++; 1)]]&&selectedDelay===2;
parityAssert["parity selected delayed once",parityActual,parityActual,True];
unselectedDelay=0;
parityActual=!FailureQ[FeynGravSimplify[a,System`Method -> "FORMBenchmark",
  CalcFormConverter`FORMExecutable -> fixture,CalcFormConverter`FORMThreads -> 1,
  System`TimeConstraint -> 10,FeynGrav`ContractorExecutable :> (unselectedDelay++; Automatic),
  FeynGrav`ContractorThreads :> (unselectedDelay++; 4)]]&&unselectedDelay===0;
parityAssert["parity unselected delayed inert",parityActual,parityActual,True];
section["P parity"];

(* ---- Reporting: structural Print capture --------------------------------- *)
reportingReady=Length[DownValues[CalcFormConverter`FeynGravSimplify]]>0&&
  Length[Options[CalcFormConverter`FeynGravSimplify]]>0&&
  StringQ[fixture]&&FileExistsQ[fixture];
If[!TrueQ[reportingReady],
  Print["FAIL-FAST: reporting prerequisites missing; aborting."];
  Quit[1]];
SetAttributes[capturePrintLog,HoldAll];
capturedLog={};
capturePrintLog[call_]:=(capturedLog={};
  Block[{Print=Function[Null,AppendTo[capturedLog,{##}];Null]},call]);
terminalRecords[log_]:=Cases[log,{"FeynGravSimplify total (",m_,", ",s_,"): ",___}:>{m,s}];
progressTerminal[log_]:=Cases[log,{label_String,": ",s_String,"."}/;MemberQ[{"Complete","Failed","Aborted"},s]:>{label,s}];
processTiming[log_]:=Cases[log,{m_String," process: ",___," s elapsed (wall clock)."}:>{m}];
leakFree[log_]:=StringFreeQ[ToString[log,InputForm],"Private`"]&&StringFreeQ[ToString[log,InputForm],"status$"];
(* Success *)
successResult=capturePrintLog[FeynGravSimplify[a,System`Method -> "FORMBenchmark",
  CalcFormConverter`FORMExecutable -> fixture,CalcFormConverter`FORMThreads -> 1,
  System`TimeConstraint -> 10,CalcFormConverter`ShowTiming -> True,
  CalcFormConverter`ShowProgress -> True]];
successLog=capturedLog;
Print["CAPTURE success payload=",ToString[successLog,InputForm]];
check["success result",successResult===0];
check["success terminal timing",terminalRecords[successLog]==={{"FORMBenchmark","Complete"}}];
check["success terminal progress",progressTerminal[successLog]==={{"FORMBenchmark","Complete"}}];
check["success single process timing",Length[processTiming[successLog]]===1];
check["success no private leak",leakFree[successLog]];
If[terminalRecords[successLog]=!={{"FORMBenchmark","Complete"}},
  mismatch["success terminal",terminalRecords[successLog],{{"FORMBenchmark","Complete"}}]];
(* Known backend failure *)
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->"exit"];
failureResult=capturePrintLog[FeynGravSimplify[a,System`Method -> "FORMBenchmark",
  CalcFormConverter`FORMExecutable -> fixture,CalcFormConverter`FORMThreads -> 1,
  System`TimeConstraint -> 10,CalcFormConverter`ShowTiming -> True,
  CalcFormConverter`ShowProgress -> True]];
failureLog=capturedLog;
Print["CAPTURE failure payload=",ToString[failureLog,InputForm]];
check["failure result",FailureQ[failureResult]];
check["failure terminal timing",terminalRecords[failureLog]==={{"FORMBenchmark","Failed"}}];
check["failure terminal progress",progressTerminal[failureLog]==={{"FORMBenchmark","Failed"}}];
check["failure no private leak",leakFree[failureLog]];
If[terminalRecords[failureLog]=!={{"FORMBenchmark","Failed"}},
  mismatch["failure terminal",terminalRecords[failureLog],{{"FORMBenchmark","Failed"}}]];
failureJob=If[FailureQ[failureResult],Lookup[failureResult[[2]],"JobDirectory",None],None];
Print["RETAINED failure job=",ToString[failureJob,InputForm]];
(* Controlled abort with progress and timing enabled; real child stop check *)
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->"wait"];
pidFile=FileNameJoin[{$TemporaryDirectory,"fg-abort-pid-"<>CreateUUID[]}];
SetEnvironment["FG_CONTRACTOR_TEST_PID"->pidFile];
capturedLog={};
abortCaught=CheckAbort[
  capturePrintLog[Block[{Pause=Function[Null,Abort[],HoldAll]},
    FeynGravSimplify[a,System`Method -> "FORMBenchmark",
      CalcFormConverter`FORMExecutable -> fixture,CalcFormConverter`FORMThreads -> 1,
      System`TimeConstraint -> 10,CalcFormConverter`ShowTiming -> True,
      CalcFormConverter`ShowProgress -> True]]],$Aborted];
abortLog=capturedLog;
Print["CAPTURE abort payload=",ToString[abortLog,InputForm]];
abortPid=If[FileExistsQ[pidFile],Quiet[FromDigits[StringTrim[Import[pidFile,"Text"]]]],None];
If[FileExistsQ[pidFile],DeleteFile[pidFile]];
check["abort propagates",abortCaught===$Aborted];
check["abort terminal timing",terminalRecords[abortLog]==={{"FORMBenchmark","Aborted"}}];
check["abort terminal progress",progressTerminal[abortLog]==={{"FORMBenchmark","Aborted"}}];
check["abort no private leak",leakFree[abortLog]];
check["abort child stopped",IntegerQ[abortPid]&&!DirectoryQ["/proc/"<>ToString[abortPid]]];
If[terminalRecords[abortLog]=!={{"FORMBenchmark","Aborted"}},
  mismatch["abort terminal",terminalRecords[abortLog],{{"FORMBenchmark","Aborted"}}]];
SetEnvironment[{"FG_CONTRACTOR_TEST_MODE"->None,"FG_CONTRACTOR_TEST_PID"->None}];
section["R reporting"];
writeSuiteCompletion["Reliability", passed, failed];
Print["Reliability: ",passed," passed, ",failed," failed"];
Quit[If[failed===0,0,1]];
