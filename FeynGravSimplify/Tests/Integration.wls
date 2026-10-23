(* Fresh kernel. Set FG_CONTRACTOR_TEST_BINARY and FG_CONTRACTOR_TEST_FIXTURE.
   Fixture executable is built from ProcessFixture.go; it is test-only.
   Adapted for the API-01 facade: every Contractor call names
   System`Method -> "Contractor" explicitly, because duplicates resolve by the
   first rule and the Automatic default now selects the full pipeline. *)
$FeynCalcStartupMessages=False;
Get["FeynCalc`"];
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"FeynGrav.wl"}]];
passed=0;failed=0;
check[name_,test_]:=If[TrueQ[test],passed++,failed++;Print["FAIL: ",name]];
binary=Environment["FG_CONTRACTOR_TEST_BINARY"];
fixture=Environment["FG_CONTRACTOR_TEST_FIXTURE"];
If[!StringQ[binary]||!FileExistsQ[binary]||!StringQ[fixture]||!FileExistsQ[fixture],Print["Set test executable paths."];Quit[2]];
calc[x_,opts___Rule]:=Quiet[FeynGravSimplify[x,System`Method -> "Contractor",FeynGrav`ContractorExecutable->binary,FeynGrav`ContractorThreads->1,System`TimeConstraint->20,opts],FeynGravSimplify::files];
tag[x_]:=If[FailureQ[x],x[[1]],None];
removeFailure[x_]:=If[FailureQ[x]&&StringQ[Lookup[x[[2]],"JobDirectory",None]]&&DirectoryQ[x[[2]]["JobDirectory"]],DeleteDirectory[x[[2]]["JobDirectory"],DeleteContents->True]];
originalStreams=Streams[];
originalProcesses=Processes[];
Do[
 result=calc[item[[2]]];
 equal= !FailureQ[result] && TrueQ[Together[ExpandScalarProduct[Contract[FeynAmpDenominatorExplicit[FCI[result-item[[3]]]]]]]===0];
 check[item[[1]],equal];
,{item,{
 {"zero",0,0},{"constant",(2+3 I)/7,(2+3 I)/7},
 {"trace D",MTD[mu,mu],D},{"trace 4",MT[mu,nu]^2,4},
 {"vector chain",MTD[mu,nu] FVD[p,mu] FVD[q,nu],SPD[p,q]},
 {"free indices",(2+3 I)/7 MTD[mu,nu] MTD[nu,rho],(2+3 I)/7 MTD[mu,rho]},
 {"product of sums",(MTD[mu,nu]+FVD[p,mu] FVD[p,nu]) FVD[q,mu] FVD[q,nu],SPD[q]+SPD[p,q]^2},
 {"positive power",(MTD[mu,nu]+FVD[p,mu] FVD[p,nu])^2,D+2 SPD[p]+SPD[p]^2},
 {"large coefficient",100000 MTD[mu,nu] 100000 FVD[p,mu],10000000000 FVD[p,nu]},
 {"huge complex",(10^40+10^30 I) MTD[mu,nu] FVD[p,mu],(10^40+10^30 I) FVD[p,nu]},
 {"composite momentum",FVD[p-q,mu] FVD[p,mu],SPD[p-q,p]},
 {"propagator shorthand",FVD[p,mu] FVD[q,mu] FAD[{p-q,m}],SPD[p,q] FAD[{p-q,m}]},
 {"propagator internal",FCI[FVD[p,mu] FVD[q,mu] FAD[{p-q,m}]],FCI[SPD[p,q] FAD[{p-q,m}]]},
 {"Unicode and contexts",(Left`\[Mu]+Right`\[Mu]) MTD[mu,nu] FVD[p,mu],(Left`\[Mu]+Right`\[Mu]) FVD[p,nu]},
 {"opaque scalars",Log[a] B0[s,m^2,m^2] MTD[mu,mu],D Log[a] B0[s,m^2,m^2]},
 {"scalar potential",GravitonScalarPotentialVertex[{mu,nu},lambda] MTD[mu,nu],Contract[FCI[GravitonScalarPotentialVertex[{mu,nu},lambda] MTD[mu,nu]]]},
 {"scalar kinetic",GravitonScalarVertex[{mu,nu},p,q,m] MTD[mu,nu],Contract[FCI[GravitonScalarVertex[{mu,nu},p,q,m] MTD[mu,nu]]]}
}}];
check["automatic method selects full pipeline",FeynGravSimplify[MTD[mu,mu],System`TimeConstraint -> 20]===D];
(* The executable/threads value checks are dispatch-scoped: Automatic resolvess
   to the full pipeline, which never reads Contractor settings, so the checks
   run against the restricted backend that owns those values. *)
check["unsupported method",tag[FeynGravSimplify[a,System`Method -> "UnknownBackend"]]==="UnsupportedMethod"];
check["unknown option",tag[FeynGravSimplify[a,MadeUpOption->1]]==="InvalidOption"];
check["invalid threads",tag[FeynGravSimplify[a,System`Method -> "Contractor",FeynGrav`ContractorThreads->0]]==="InvalidOption"];
check["invalid timeout",tag[FeynGravSimplify[a,System`TimeConstraint->-1]]==="InvalidOption"];
check["invalid KeepFiles",tag[FeynGravSimplify[a,CalcFormConverter`KeepFiles->1]]==="InvalidOption"];
check["invalid arity",tag[FeynGravSimplify[]]==="InvalidArguments"];
check["bad executable option",tag[FeynGravSimplify[a,System`Method -> "Contractor",FeynGrav`ContractorExecutable->42]]==="InvalidOption"];
check["missing executable",tag[FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->"/does/not/exist/contractor"]]==="ContractorNotFound"];
check["unsupported before execution",FailureQ[calc[GAD[mu]]]];
check["capability refuses full-pipeline request",tag[FeynGravSimplify[MTD[mu,mu],System`Method->"Contractor",FeynCalc`LoopMomenta->{q}]]==="MethodCapability"];
check["option list",FeynGravSimplify[a,System`Method->"Contractor",{FeynGrav`ContractorExecutable->binary,FeynGrav`ContractorThreads->1}]===a];
(* A launch failure is separate from discovery failure. *)
result=Block[{StartProcess=Function[Null,$Failed,HoldAll]},
 Quiet[FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,FeynGrav`ContractorThreads->1,System`TimeConstraint->20],FeynGravSimplify::files]];
check["launch failure",tag[result]==="ContractorLaunchFailed"];removeFailure[result];
beforeJobs=FileNames["feyngrav-contractor-*",$TemporaryDirectory];
check["successful result",calc[a]===a];
check["successful job removed",Complement[FileNames["feyngrav-contractor-*",$TemporaryDirectory],beforeJobs]==={}];
result=calc[a,CalcFormConverter`KeepFiles->True];
kept=Complement[FileNames["feyngrav-contractor-*",$TemporaryDirectory],beforeJobs];
check["KeepFiles",result===a&&Length[kept]===1&&AllTrue[{"input.json","mapping.json","stdout.json","stderr.log","process.json"},FileExistsQ[FileNameJoin[{First[kept],#}]]&]];
If[Length[kept]===1,DeleteDirectory[First[kept],DeleteContents->True]];
(* Explicit worker option is forwarded; Automatic sends no worker argument. *)
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->"threads"];
check["workers forwarded",FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,FeynGrav`ContractorThreads->3,System`TimeConstraint->20]===3];
check["automatic workers unmodified",FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,FeynGrav`ContractorThreads->Automatic,System`TimeConstraint->20]===0];
check["default eight workers",FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,System`TimeConstraint->20]===8];
check["inert FORM settings on Contractor",FeynGravSimplify[a,System`Method->"Contractor",
    FeynGrav`ContractorExecutable->fixture,FeynGrav`ContractorThreads->2,
    CalcFormConverter`FORMExecutable->"/missing/form",CalcFormConverter`FORMThreads->3,
    System`TimeConstraint->20]===2];
check["invalid progress",tag[FeynGravSimplify[a,System`Method->"Contractor",CalcFormConverter`ShowProgress->1]]==="InvalidOption"];
check["invalid timing",tag[FeynGravSimplify[a,System`Method->"Contractor",CalcFormConverter`ShowTiming->1]]==="InvalidOption"];
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->"echo"];
SetAttributes[capture,HoldFirst];
capture[expr_]:=Module[{lines={},value},
 value=Block[{Print=Function[Null,AppendTo[lines,StringJoin[ToString /@ {##}]]]},expr];
 {value,lines}];
reported=capture[FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,System`TimeConstraint->20]];
check["quiet defaults",reported==={a,{}}];
reported=capture[FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,System`TimeConstraint->20,CalcFormConverter`ShowProgress->True]];
check["progress stages",reported[[1]]===a&&Length[reported[[2]]]===6&&StringContainsQ[Last[reported[[2]]],"Complete"]];
reported=capture[FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,System`TimeConstraint->20,CalcFormConverter`ShowTiming->True]];
check["timing separate from progress",reported[[1]]===a&&Length[reported[[2]]]===2&&StringStartsQ[First[reported[[2]]],"Contractor process:"]&&StringStartsQ[Last[reported[[2]]],"FeynGravSimplify total"]];
reported=capture[FeynGravSimplify[GAD[mu],System`Method->"Contractor",FeynGrav`ContractorExecutable->binary,FeynGrav`ContractorThreads->1,System`TimeConstraint->20,CalcFormConverter`ShowProgress->True,CalcFormConverter`ShowTiming->True]];
check["failed conversion reporting",FailureQ[reported[[1]]]&&Length[reported[[2]]]===3&&StringContainsQ[Last[reported[[2]]],"Failed"]];
result=FeynGravSimplify[PolarizationTensorD[p,mu,nu],System`Method->"Contractor",FeynGrav`ContractorExecutable->binary,FeynGrav`ContractorThreads->1,System`TimeConstraint->20];
check["notebook polarization failure",tag[result]==="UnsupportedMomentum"&&result[[2]]["Stage"]==="Encode"&&MatchQ[result[[2]]["MomentumSource"],HoldComplete[_Polarization]]];
removeFailure[result];
(* Exercise the real ten-second reporting interval with an idle child. *)
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->"progress"];
reported=capture[FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,System`TimeConstraint->15,CalcFormConverter`ShowProgress->True]];
ticks=Select[reported[[2]],StringStartsQ[#, "Contractor Execute: "]&];
check["elapsed progress without timing",reported[[1]]===0&&Length[ticks]===1&&StringContainsQ[First[ticks],"s elapsed (wall clock)."]&&StringContainsQ[Last[reported[[2]]],"Complete"]];
check["progress interval ten seconds",FeynGrav`Simplification`Private`$progressIntervalSeconds===10];
(* Accelerate only the private interval to check quiet operation and timeout cleanup. *)
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->"wait"];
reported=Block[{FeynGrav`Simplification`Private`$progressIntervalSeconds=0.05},
 capture[Quiet[FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,FeynGrav`ContractorThreads->1,System`TimeConstraint->0.2],FeynGravSimplify::files]]];
check["no elapsed output when disabled",tag[reported[[1]]]==="ContractorTimedOut"&&reported[[2]]==={}];removeFailure[reported[[1]]];
reported=Block[{FeynGrav`Simplification`Private`$progressIntervalSeconds=0.05},
 capture[Quiet[FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,FeynGrav`ContractorThreads->1,System`TimeConstraint->0.3,CalcFormConverter`ShowProgress->True],FeynGravSimplify::files]]];
check["elapsed progress before timeout",tag[reported[[1]]]==="ContractorTimedOut"&&AnyTrue[reported[[2]],StringStartsQ[#, "Contractor Execute: "]&]&&StringContainsQ[Last[reported[[2]]],"Failed"]];removeFailure[reported[[1]]];
(* Test malformed output and retained failure information. *)
Do[
 SetEnvironment["FG_CONTRACTOR_TEST_MODE"->item[[1]]];
 result=Quiet[FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,FeynGrav`ContractorThreads->1,System`TimeConstraint->5],FeynGravSimplify::files];
 check["failure "<>item[[1]],FailureQ[result] && MemberQ[item[[2]],tag[result]]];
 check["retained "<>item[[1]],FailureQ[result]&&DirectoryQ[result[[2]]["JobDirectory"]]&&FileExistsQ[FileNameJoin[{result[[2]]["JobDirectory"],"process.json"}]]];
 If[FailureQ[result],check["stage "<>item[[1]],KeyExistsQ[result[[2]],"Stage"]]];
 removeFailure[result];
,{item,{{"exit",{"ContractorProcessFailed"}},{"early",{"ContractorWriteFailed","ContractorProcessFailed"}},
 {"empty",{"InvalidOutput"}},{"truncated",{"InvalidOutput"}},{"unknown",{"InvalidOutput"}},{"unsafe",{"InvalidOutput"}}}}];
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->"stderr"];
check["stderr larger than pipe",FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,FeynGrav`ContractorThreads->1,System`TimeConstraint->10]===0];
(* Large round trip tests both pipe directions without altering expression. *)
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->"echo"];
large=Total[Table[Symbol["FixtureData`c"<>ToString[j]] FVD[Symbol["FixtureData`p"<>ToString[j]],mu],{j,1500}]];
check["large native transfer",FeynGravSimplify[large,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,FeynGrav`ContractorThreads->1,System`TimeConstraint->20]===FCI[large]];
(* Native timeout is checked after stdin consumption, not during a blocked write. *)
pidFile=FileNameJoin[{$TemporaryDirectory,"fg-contractor-test-pid-"<>CreateUUID[]}];
SetEnvironment[{"FG_CONTRACTOR_TEST_MODE"->"wait","FG_CONTRACTOR_TEST_PID"->pidFile}];
t=AbsoluteTiming[result=Quiet[FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,FeynGrav`ContractorThreads->1,System`TimeConstraint->0.5],FeynGravSimplify::files];][[1]];
check["timeout",tag[result]==="ContractorTimedOut" && t<5];
pid=FromDigits[StringTrim[Import[pidFile,"Text"]]];
check["timeout child stopped",!DirectoryQ["/proc/"<>ToString[pid]]];removeFailure[result];
check["abort propagated",CheckAbort[
 Block[{Pause=Function[Null,Abort[],HoldAll]},Quiet[FeynGravSimplify[a,System`Method->"Contractor",FeynGrav`ContractorExecutable->fixture,FeynGrav`ContractorThreads->1],FeynGravSimplify::files]];False,True]];
pid=FromDigits[StringTrim[Import[pidFile,"Text"]]];
check["abort child stopped",!DirectoryQ["/proc/"<>ToString[pid]]];DeleteFile[pidFile];
SetEnvironment[{"FG_CONTRACTOR_TEST_MODE"->None,"FG_CONTRACTOR_TEST_PID"->None}];
(* ProcessLink pipe streams may be registered by the owner's CalcFormCalculate
   handling on this branch when the restricted backend is compared with full
   pipeline runs; facade-owned launches close cleanly (probes p11-p15).
   All other streams must close. *)
extraStreams=Complement[Streams[],originalStreams];
regularStreams=Select[extraStreams,!StringContainsQ[ToString[#[[1]],InputForm],"ProcessLink"]&];
check["streams closed",regularStreams==={}];
(* Processes[] reaps finished full-pipeline (CalcFormCalculate) children too. The
   owner runtime on this branch never requests those children's StandardInput
   stream, so ProcessLink's unconditional cache Unset in removeRunningProcesses
   emits Unset::norep for that stream type. That is existing CalcForm* behaviour
   outside API-01 scope; facade-owned children reap silently (control probe p16).
   See Documentation/Verification/SimplificationExtraction-2026-10-20.md. *)
check["processes stopped",Select[Complement[Processes[],originalProcesses],ProcessStatus[#]==="Running"&]==={}];
Print["Integration: ",passed," passed, ",failed," failed"];
Quit[If[failed===0,0,1]];
