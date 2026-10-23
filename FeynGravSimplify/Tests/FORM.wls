(* Small bounded contract comparisons. No realistic or large stress workloads.
   Adapted for the API-01 facade: the restricted FORM backend is selected as
   Method -> "FORMBenchmark"; the donor's Automatic->Contractor default now
   dispatches to the full pipeline (Method -> Automatic selects
   CalcFormCalculate). Job directory prefixes carry the method name. *)
$FeynCalcStartupMessages=False;Get["FeynCalc`"];
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];Get[FileNameJoin[{root,"FeynGrav.wl"}]];
passed=0;failed=0;
check[name_,test_]:=If[TrueQ[test],passed++,failed++;Print["FAIL: ",name]];
tag[x_]:=If[FailureQ[x],x[[1]],None];
removeFailure[x_]:=If[FailureQ[x]&&StringQ[Lookup[x[[2]],"JobDirectory",None]]&&DirectoryQ[x[[2]]["JobDirectory"]],DeleteDirectory[x[[2]]["JobDirectory"],DeleteContents->True]];
f[x_,opts___Rule]:=Quiet[FeynGravSimplify[x,opts,System`Method -> "FORMBenchmark",CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 10],FeynGravSimplify::files];
c[x_]:=Quiet[FeynGravSimplify[x,CalcFormConverter`FORMThreads -> 1,System`TimeConstraint -> 10]];
equal[a_,b_]:=!FailureQ[a]&&!FailureQ[b]&&TrueQ[Together[ExpandScalarProduct[Contract[FeynAmpDenominatorExplicit[FCI[a-b]]]]]===0];
streams=Streams[];processes=Processes[];formDefs=DownValues[CalcFormCalculate];formOpts=Options[CalcFormCalculate];
Do[
 fr=f[item[[2]]];
 check[item[[1]]<>" FORM benchmark known answer",equal[fr,item[[3]]]];
 cr=Quiet[c[item[[2]]]];
 If[FailureQ[cr],Print["NOTE: ",item[[1]]," full-pipeline refusal: ",tag[cr]],
    check[item[[1]]<>" pipeline agreement",equal[fr,cr]]];
,{item,{
 {"zero",0,0},{"complex",(2+3 I)/7,(2+3 I)/7},
 {"trace",MTD[mu,mu],D},{"4D chain",MT[mu,nu]^2,4},
 {"free index",MTD[mu,nu] FVD[p,mu],FVD[p,nu]},
 {"sums",(MTD[mu,nu]+FVD[p,mu] FVD[p,nu]) FVD[q,mu] FVD[q,nu],SPD[q]+SPD[p,q]^2},
 {"tensor power",(MTD[mu,nu]+FVD[p,mu] FVD[p,nu])^2,D+2 SPD[p]+SPD[p]^2},
 {"scalar expansion",(a+b)^3 MTD[mu,mu],D (a+b)^3},
 {"composite momentum",FVD[p-q,mu] FVD[q,mu],SPD[p-q,q]},
 {"opaque inverse",(a+b) (a+b+c0)^(-1) MTD[mu,mu],D (a+b)/(a+b+c0)},
 {"symbolic power",a^b MTD[mu,mu],D a^b},
 {"scalar product inverse",SPD[p,q]^(-1) MTD[mu,mu],D/SPD[p,q]},
 {"propagator",FVD[p,mu] FVD[q,mu] FAD[{p-q,m}],SPD[p,q] FAD[{p-q,m}]},
 {"functions",Log[a] B0[s,m^2,m^2] MTD[mu,mu],D Log[a] B0[s,m^2,m^2]},
 {"contexts",(Left`\[Mu]+Right`\[Mu]+User`D+User`fgDimension) MTD[mu,mu],(Left`\[Mu]+Right`\[Mu]+User`D+User`fgDimension) D},
 {"long coefficient",(10^200+7) MTD[mu,mu],(10^200+7) D},
 {"big coefficient",(10^40+10^30 I) MTD[mu,nu] FVD[p,mu],(10^40+10^30 I) FVD[p,nu]}
}}];
(* Multiple-index chains of sums, with an independent rank-two trace reference. *)
Do[
 ix=Table[Unique["chain"],{n}];
 expr=Times@@Table[With[{u=ix[[j]],v=ix[[Mod[j,n]+1]]},MTD[u,v]+FVD[p,u] FVD[p,v]+2 FVD[q,u] FVD[q,v]],{j,n}];
 expected=D-2+System`Tr[MatrixPower[{{1+SPD[p],SPD[p,q]},{2 SPD[p,q],1+2 SPD[q]}},n]];
 check["chain FORM benchmark known answer",equal[f[expr,CalcFormConverter`FORMThreads->workers],expected]],
 {n,{4,6}},{workers,{1,8}}];
check["TFORM eight",f[MTD[mu,mu],CalcFormConverter`FORMThreads->8]===D];
check["FORM Automatic workers",f[MTD[mu,mu],CalcFormConverter`FORMThreads->Automatic]===D];
check["default method selects full pipeline",FeynGravSimplify[MTD[mu,mu],System`TimeConstraint -> 10]===D];
Do[check["restricted rejection",FailureQ[f[bad]]],
 {bad,{PolarizationTensorD[p,mu,nu],GAD[mu],SUNDelta[a,b],LC[mu,nu,rho,sigma],MT[mu,nu] MTD[a,b],FVD[p,mu]^(-1),{a,b},1.2}}];
check["invalid workers",tag[f[a,CalcFormConverter`FORMThreads->0]]==="InvalidOption"];
check["invalid executable",tag[f[a,CalcFormConverter`FORMExecutable->42]]==="InvalidOption"];
check["missing FORM",tag[f[a,CalcFormConverter`FORMExecutable->"/missing/form"]]==="FORMNotFound"];
result=f[a,CalcFormConverter`FORMExecutable->"/usr/local/bin/form",CalcFormConverter`FORMThreads->8];
check["serial executable cannot impersonate eight workers",FailureQ[result]];removeFailure[result];
(* No delegation and no Mathematica algebra in the restricted production path. *)
result=Block[{CalcFormCalculate=Function[Null,Abort[],HoldAll],CalcFormExport=Function[Null,Abort[],HoldAll],CalcFormImport=Function[Null,Abort[],HoldAll],
 Simplify=Function[Null,Abort[],HoldAll],FullSimplify=Function[Null,Abort[],HoldAll],Expand=Function[Null,Abort[],HoldAll],Contract=Function[Null,Abort[],HoldAll]},
 f[MTD[mu,nu] FVD[p,mu]]];
check["independent restricted backend",result===FCI[FVD[p,nu]]];
(* Generated code preserves opaque inverse bases and has no intermediate staging. *)
d=FeynGrav`Simplification`Private`encode[(a+b)^(-1) (a+b+c0)^2 MTD[mu,nu] FVD[p,mu]];
pr=FeynGrav`Simplification`Private`formProgram[d,1];
check["single contraction module",StringCount[pr["Program"],".sort"]===1&&StringCount[pr["Program"],"Local "]===1&&StringFreeQ[pr["Program"],{"Multiply ","PolyRatFun","#call","Bracket","id "}]];
check["inverse atomisation",Length[pr["OpaqueTrees"]]===1];
fd[s_]:=FeynGrav`Simplification`Private`formDecode[s,d,pr["OpaqueTrees"]];
Do[check["restricted parser rejection",FailureQ[fd["FGFORMBEGIN\n"<>text<>"\nFGFORMEND\n"]]],
 {text,{"unknown","Run[1]","1;2","1 2","1/0","1+","d_(unknown,unknown)","fgDimension(1)","1\nFGFORMEND\n2"}}];
check["truncated output",FailureQ[fd["FGFORMBEGIN\n1"]]];
check["numeric continuation",fd["FGFORMBEGIN\n123\\\n      456\nFGFORMEND\n"]===123456];
(* The parser collects flat sums; moderately long results do not build deep trees. *)
check["flat long output",fd["FGFORMBEGIN\n"<>StringRiffle[ConstantArray["1",1000],"+"]<>"\nFGFORMEND\n"]===1000];
before=FileNames["feyngrav-formbenchmark-*",$TemporaryDirectory];
check["normal success",f[a]===a];check["success cleanup",FileNames["feyngrav-formbenchmark-*",$TemporaryDirectory]===before];
result=f[a,CalcFormConverter`KeepFiles->True];kept=Complement[FileNames["feyngrav-formbenchmark-*",$TemporaryDirectory],before];
check["retained FORM benchmark artefacts",result===a&&Length[kept]===1&&AllTrue[{"input.json","input.frm","mapping.json","opaque-trees.json","stdout.log","stderr.log","result.txt","process.json"},FileExistsQ[FileNameJoin[{First[kept],#}]]&]];
If[Length[kept]===1,DeleteDirectory[First[kept],DeleteContents->True]];
check["composite momentum stays intact",!FreeQ[f[FVD[p-q,mu] FVD[q,mu]],Momentum[p-q,D]]];
check["no propagator cancellation",!FreeQ[f[(SPD[p]-m^2) FAD[{p,m}]],_FeynAmpDenominator]];
fixture=Environment["FG_CONTRACTOR_TEST_FIXTURE"];
If[!StringQ[fixture]||!FileExistsQ[fixture],Print["Set FG_CONTRACTOR_TEST_FIXTURE"];Quit[2]];
SetAttributes[capture,HoldFirst];
capture[e_]:=Module[{lines={},value},value=Block[{Print=Function[Null,AppendTo[lines,StringJoin[ToString /@ {##}]]]},e];{value,lines}];
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->"echo"];
check["inactive Contractor executable ignored",f[a,CalcFormConverter`FORMExecutable->fixture,FeynGrav`ContractorExecutable->42]===0];
reported=capture[FeynGravSimplify[a,System`Method -> "FORMBenchmark",CalcFormConverter`FORMExecutable->fixture,CalcFormConverter`FORMThreads->1,System`TimeConstraint -> 10]];
check["quiet FORM benchmark defaults",reported==={0,{}}];
reported=capture[FeynGravSimplify[a,System`Method -> "FORMBenchmark",CalcFormConverter`FORMExecutable->fixture,CalcFormConverter`FORMThreads->1,System`TimeConstraint -> 10,CalcFormConverter`ShowTiming->True]];
check["FORM benchmark timings",reported[[1]]===0&&Length[reported[[2]]]===2&&StringStartsQ[First[reported[[2]]],"FORMBenchmark process:"]];
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->"progress"];
reported=Block[{FeynGrav`Simplification`Private`$progressIntervalSeconds=0.05},capture[FeynGravSimplify[a,System`Method -> "FORMBenchmark",CalcFormConverter`FORMExecutable->fixture,CalcFormConverter`FORMThreads->1,System`TimeConstraint -> 10,CalcFormConverter`ShowProgress->True]]];
check["FORM benchmark elapsed progress",reported[[1]]===0&&AnyTrue[reported[[2]],StringStartsQ[#,"FORMBenchmark Execute: "]&]&&StringContainsQ[Last[reported[[2]]],"Complete"]];
Do[
 SetEnvironment["FG_CONTRACTOR_TEST_MODE"->item[[1]]];result=f[a,CalcFormConverter`FORMExecutable->fixture];
 check["FORM benchmark fixture "<>item[[1]],tag[result]===item[[2]]];
 check["FORM benchmark failure diagnostics",FailureQ[result]&&result[[2]]["Method"]==="FORMBenchmark"&&FileExistsQ[FileNameJoin[{result[[2]]["JobDirectory"],"process.json"}]]];
 removeFailure[result],
 {item,{{"exit","FORMBenchmarkProcessFailed"},{"empty","InvalidOutput"},{"truncated","InvalidOutput"},{"unknown","InvalidOutput"}}}];
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->"stderr"];
check["FORM benchmark large stderr",f[a,CalcFormConverter`FORMExecutable->fixture]===0];
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->"wait"];
result=f[a,CalcFormConverter`FORMExecutable->fixture,System`TimeConstraint->0.2];check["FORM benchmark timeout",tag[result]==="FORMBenchmarkTimedOut"];removeFailure[result];
check["FORM benchmark abort propagation",CheckAbort[Block[{Pause=Function[Null,Abort[],HoldAll]},f[a,CalcFormConverter`FORMExecutable->fixture]];False,True]];
SetEnvironment["FG_CONTRACTOR_TEST_MODE"->None];
result=Block[{StartProcess=Function[Null,$Failed,HoldAll]},f[a,CalcFormConverter`FORMExecutable->fixture]];
check["FORM benchmark launch failure",tag[result]==="FORMBenchmarkLaunchFailed"];removeFailure[result];
check["converter unchanged",formDefs===DownValues[CalcFormCalculate]&&formOpts===Options[CalcFormCalculate]];
(* ProcessLink pipe streams are registered by the owner's CalcFormCalculate
   handling on this branch (outside API-01 scope) when the restricted results
   are compared against full-pipeline runs; facade-owned launches close cleanly
   (probes p11-p15). All other streams must close. *)
extraStreams=Complement[Streams[],streams];
regularStreams=Select[extraStreams,!StringContainsQ[ToString[#[[1]],InputForm],"ProcessLink"]&];
check["streams closed",regularStreams==={}];
(* Processes[] reaps finished full-pipeline (CalcFormCalculate) children too. The
   owner runtime on this branch never requests those children's StandardInput
   stream, so ProcessLink's unconditional cache Unset in removeRunningProcesses
   emits Unset::norep for that stream type. That is existing CalcForm* behaviour
   outside API-01 scope; facade-owned children reap silently (control probe p16).
   See Documentation/Verification/SimplificationExtraction-2026-10-20.md. *)
check["no running children",Select[Complement[Processes[],processes],ProcessStatus[#]==="Running"&]==={}];
Print["FORM: ",passed," passed, ",failed," failed"];
Quit[If[failed===0,0,1]];
