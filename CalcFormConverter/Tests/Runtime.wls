Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
root=testDirectory;
beforeDirectory=Directory[];
info=CalcFormCheck[];
assert["installed FORM probe",AssociationQ[info] && TrueQ[info["Available"]] && StringQ[info["Version"]]];
assert["explicit executable",TrueQ[CalcFormCheck[FORMExecutable->info["Executable"]]["Available"]]];
spaceExecutable=FileNameJoin[{root,"space directory","form executable"}];
assert["executable path with spaces",TrueQ[CalcFormCheck[FORMExecutable->spaceExecutable]["Available"]]];
missing=CalcFormCheck[FORMExecutable->FileNameJoin[{root,"missing-form"}]];
assert["missing distinguished",missing["Status"]==="NotFound"];
nonexec=CalcFormCheck[FORMExecutable->FileNameJoin[{root,"nonexecutable"}]];
assert["launch failure distinguished",nonexec["Status"]==="LaunchFailed"];
assert["launch failure retains rendered diagnostic",AnyTrue[nonexec["Messages"],
 StringContainsQ[#, "nonexecutable"] &]];
(* Quiet at the call site must not erase diagnostics; redirection must not leak
   kernel messages into the caller's message stream. *)
launchCapturePath=FileNameJoin[{root,"launch-message-capture.log"}];
launchCapture=OpenWrite[launchCapturePath];
quietNonexec=Block[{$Messages={launchCapture}},
 Quiet[CalcFormCheck[FORMExecutable->FileNameJoin[{root,"nonexecutable"}]]]];
Close[launchCapture];
assert["quiet launch retains diagnostic",AnyTrue[quietNonexec["Messages"],
 StringContainsQ[#, "nonexecutable"] &]];
assert["launch diagnostics do not print",StringTrim[Import[launchCapturePath,"Text"]]===""];
assert["successful probe has no launch messages",info["Messages"]==={}];
If[$OperatingSystem === "Windows", Print["SKIP: PATH executable permission regression is Unix-specific."],
 pathProbe=CreateDirectory[FileNameJoin[{root,"path-permission-probe"}]];
 Export[FileNameJoin[{pathProbe,"form"}],"This is not executable.\n","Text"];
 pathSeparator=If[$OperatingSystem === "Windows",";",":"];
 originalPath=Environment["PATH"];
 resolvedFromPath=Block[{Environment},
   Environment["PATH"] := StringRiffle[{pathProbe,originalPath},pathSeparator];
   CalcFormConverter`Private`resolveExecutable["form"]];
 assert["PATH skips non-executable candidate",ExpandFileName[resolvedFromPath]===ExpandFileName[info["Executable"]]];
];
assertFailure["invalid probe timeout","InvalidOption",CalcFormCheck[TimeConstraint->0]];
assertFailure["invalid executable option","InvalidOption",CalcFormCheck[FORMExecutable->42]];
probeBefore=Sort[FileNames["calcform-probe-*",$TemporaryDirectory]];
assert["probe timeout",CalcFormCheck[FORMExecutable->FileNameJoin[{root,"probehang"}],TimeConstraint->.1]["Status"]==="TimedOut"];
assert["probe wrong arithmetic",CalcFormCheck[FORMExecutable->FileNameJoin[{root,"probebad"}]]["Status"]==="ProbeFailed"];
assert["probe cleanup",Sort[FileNames["calcform-probe-*",$TemporaryDirectory]]===probeBefore];

assert["Greek metric trace",CalcFormCalculate[MTD[\[Mu],\[Nu]] MTD[\[Mu],\[Nu]]]===D];

(* Binary equality uses one residual job and preserves failures as failures. *)
assert["equality metric trace",CalcFormCalculate[MTD[mu,nu]^2==D]===True];
assert["equality false trace",CalcFormCalculate[MTD[mu,nu]^2+1==D]===False];
assert["equality unresolved residual",CalcFormCalculate[MTD[mu,nu]^2==x]===(D-x==0)];
assert["evaluated booleans need no FORM",Block[{CalcFormConverter`CalcFormCheck},
 CalcFormConverter`CalcFormCheck[___]:=Throw["unexpected FORM probe"];
 Catch[{CalcFormCalculate[True],CalcFormCalculate[False]}]]==={True,False}];
assertFailure["boolean keeps option validation","InvalidOption",CalcFormCalculate[True,FORMThreads->0]];
assertFailure["chained equality rejected","UnsupportedEquality",CalcFormCalculate[x==y==z]];
equalityParent=CreateDirectory[FileNameJoin[{root,"equality-job"}]];
equalityKept=Quiet[CalcFormCalculate[MTD[mu,nu]^2==D,WorkingDirectory->equalityParent,KeepFiles->True],CalcFormCalculate::files];
equalityJobs=FileNames["calcform-job-*",equalityParent];
assert["equality retains one residual job",equalityKept===True && Length[equalityJobs]===1 &&
 CalcFormImport[FileNameJoin[{First[equalityJobs],"job.out"}],FileNameJoin[{First[equalityJobs],"job.map.json"}]]===0];
assertFailure["equality export failure propagates","ExportFailed",CalcFormCalculate[unknownTensor[x]==y,WorkingDirectory->equalityParent]];

(* Timing and progress must leave the mathematical result unchanged. *)
events={};
observed=Block[{CalcFormConverter`Private`reportCalculation},
 CalcFormConverter`Private`reportCalculation[event_Association]:=AppendTo[events,event];
 CalcFormCalculate[MTD[\[Mu],\[Nu]]^2,ShowTiming->True,ShowProgress->True]];
assert["observation preserves result",observed===D];
assert["reported stages",Select[Lookup[events,"Stage"],MemberQ[{"Check","Export","Execute","Import","Complete"},#]&]==={"Check","Export","Execute","Import","Complete"}];
timingEvents=Select[events,KeyExistsQ[#,"ElapsedSeconds"]&];
assert["execution timing reported",Length[timingEvents]===1 && NumberQ[First[timingEvents]["ElapsedSeconds"]] && First[timingEvents]["ElapsedSeconds"]>=0];
events={};
Block[{CalcFormConverter`Private`reportCalculation},
 CalcFormConverter`Private`reportCalculation[event_Association]:=AppendTo[events,event];
 CalcFormCalculate[1]];
assert["default output remains quiet",events==={}];
assertFailure["invalid timing option","InvalidOption",CalcFormCalculate[1,ShowTiming->42]];
assertFailure["invalid progress option","InvalidOption",CalcFormCalculate[1,ShowProgress->42]];
assertFailure["invalid worker count","InvalidOption",CalcFormCalculate[1,FORMThreads->0]];
assertFailure["fractional worker count","InvalidOption",CalcFormCheck[FORMThreads->1/2]];
assert["ordinary FORM cannot silently ignore workers",CalcFormCheck[FORMExecutable->info["Executable"],FORMThreads->2]["Status"]==="ThreadingUnavailable"];
If[StringQ[CalcFormConverter`Private`resolveExecutable["tform"]],
 threaded=CalcFormCheck[FORMThreads->2];
 assert["TFORM availability and version",TrueQ[threaded["Available"]] && StringQ[threaded["Version"]] && threaded["FORMThreads"]===2];
 assert["TFORM vs serial result",CalcFormCalculate[MTD[\[Mu],\[Nu]] FVD[p-l,\[Mu]] FVD[p,\[Nu]],FORMThreads->2]===CalcFormCalculate[MTD[\[Mu],\[Nu]] FVD[p-l,\[Mu]] FVD[p,\[Nu]]]],
 Print["SKIP: TFORM is not installed."]];
progressDir=CreateDirectory[FileNameJoin[{root,"progress-timeout"}]];
events={};
progressRun=Block[{CalcFormConverter`Private`reportCalculation,CalcFormConverter`Private`$runtimeProgressIntervalSeconds=.01},
 CalcFormConverter`Private`reportCalculation[event_Association]:=AppendTo[events,event];
 CalcFormConverter`Private`runtimeProcess[{FileNameJoin[{root,"hang"}]},progressDir,.1,True,True]];
assert["periodic execution progress",Length[events]>0 && AllTrue[events,#["Stage"]==="Execute" && NumberQ[#["ElapsedSeconds"]]&]];
assert["progress preserves timeout cleanup",progressRun["Status"]==="TimedOut" && progressRun["ProcessStatus"]==="Finished" && NumberQ[progressRun["ElapsedSeconds"]]];

parent=CreateDirectory[FileNameJoin[{root,"jobs with spaces"}]];
expr=MTD[mu,nu] FVD[p-l,mu] FVD[p,nu] FAD[{l,m}] + I/3;
auto=CalcFormCalculate[expr,LoopMomenta->{l},WorkingDirectory->parent];
assert["automated vs FeynCalc",!FailureQ[auto] && Simplify[Contract[ExpandScalarProduct[FeynAmpDenominatorExplicit[FCI[auto-expr]]]]]===0];
assert["successful cleanup",FileNames["*",parent]==={}];
kept=Quiet[CalcFormCalculate[expr,LoopMomenta->{l},WorkingDirectory->parent,KeepFiles->True],CalcFormCalculate::files];
firstJob=First[FileNames["calcform-job-*",parent]];
assert["retained successful job",And@@(FileExistsQ[FileNameJoin[{firstJob,#}]]& /@ {"job.frm","job.map.json","job.out","stdout.log","stderr.log"})];
assert["manual import equals automated",kept===CalcFormImport[FileNameJoin[{firstJob,"job.out"}],FileNameJoin[{firstJob,"job.map.json"}]]];
Quiet[CalcFormCalculate[1,WorkingDirectory->parent,KeepFiles->True],CalcFormCalculate::files];
assert["independent jobs",Length[FileNames["calcform-job-*",parent]]===2 && FileExistsQ[FileNameJoin[{firstJob,"job.out"}]]];
assertFailure["missing checker prevents calculation","FORMUnavailable",CalcFormCalculate[1,FORMExecutable->FileNameJoin[{root,"missing-form"}]]];
assertFailure["invalid working directory","InvalidDirectory",CalcFormCalculate[1,WorkingDirectory->FileNameJoin[{root,"missing-dir"}]]];
assertFailure["invalid keep option","InvalidOption",CalcFormCalculate[1,KeepFiles->42]];
exportFailure=CalcFormCalculate[unknownTensor[x],WorkingDirectory->parent];
assertFailure["export failure","ExportFailed",exportFailure];
assert["export failure retained",DirectoryQ[exportFailure[[2]]["JobDirectory"]] && exportFailure[[2]]["Stage"]==="Export"];
KeyValueMap[Function[{mode,tag},Module[{failure,directory},
 failure=CalcFormCalculate[2,FORMExecutable->FileNameJoin[{root,mode}],WorkingDirectory->parent,TimeConstraint->.15];
 assertFailure[mode,tag,failure];
 If[FailureQ[failure],directory=failure[[2]]["JobDirectory"];
 assert[mode<>" files retained",DirectoryQ[directory] && FileExistsQ[FileNameJoin[{directory,"stdout.log"}]]]]]],
 <|"exit"->"FORMFailed","missing"->"MissingResult","malformed"->"ImportFailed","mismatch"->"ImportFailed","hang"->"TimedOut"|>];

(* A large stdout/stderr producer verifies both streams are drained. *)
floodDir=CreateDirectory[FileNameJoin[{root,"flood-logs"}]];
flood=CalcFormConverter`Private`runtimeProcess[{FileNameJoin[{root,"flood"}]},floodDir,10];
assert["drain both streams",flood["Status"]==="Completed" && flood["ExitCode"]===0 &&
 FileByteCount[flood["StandardOutputFile"]]>=262144 && FileByteCount[flood["StandardErrorFile"]]>=262144];
assert["bounded log tails",StringLength[flood["StandardOutput"]]<=CalcFormConverter`Private`$runtimeLogTailCharacters && StringLength[flood["StandardError"]]<=CalcFormConverter`Private`$runtimeLogTailCharacters];

(* Inject a user abort at the polling boundary, after a real child starts. *)
abortDir=CreateDirectory[FileNameJoin[{root,"abort-logs"}]];
aborted=Block[{CalcFormConverter`Private`runtimePause},
 CalcFormConverter`Private`runtimePause[]:=(Pause[.1];Abort[]);
 CalcFormConverter`Private`runtimeProcess[{FileNameJoin[{root,"hang"}]},abortDir,Infinity]];
assert["abort handled",aborted["Status"]==="Aborted"];
assert["aborted process stopped",aborted["ProcessStatus"]==="Finished"];
abortProbe=Block[{CalcFormConverter`Private`runtimePause},
 CalcFormConverter`Private`runtimePause[]:=(Pause[.1];Abort[]);
 CalcFormCheck[FORMExecutable->FileNameJoin[{root,"probehang"}]]];
assert["probe abort handled",abortProbe["Status"]==="Aborted"];
assert["aborted probe cleanup",Sort[FileNames["calcform-probe-*",$TemporaryDirectory]]===probeBefore];
abortCalculate=Block[{CalcFormConverter`CalcFormCheck,CalcFormConverter`Private`runtimePause},
 CalcFormConverter`CalcFormCheck[___]:=<|"Available"->True,"Executable"->FileNameJoin[{root,"hang"}]|>;
 CalcFormConverter`Private`runtimePause[]:=(Pause[.1];Abort[]);
 CalcFormCalculate[2,WorkingDirectory->parent]];
assertFailure["calculation abort","Aborted",abortCalculate];
assert["aborted calculation retained",DirectoryQ[abortCalculate[[2]]["JobDirectory"]] && abortCalculate[[2]]["Process"]["ProcessStatus"]==="Finished"];
(* Interruption at acquisition and finalization, not only in the poll loop. *)
acquireDir=CreateDirectory[FileNameJoin[{root,"acquire-abort"}]];
acquiredStreams={};
acquireResult=Block[{CalcFormConverter`Private`openRuntimeLog},
 CalcFormConverter`Private`openRuntimeLog[path_]:=Module[{stream=OpenWrite[path]},
 AppendTo[acquiredStreams,stream];Abort[];stream];
 CalcFormConverter`Private`runtimeProcess[{FileNameJoin[{root,"hang"}]},acquireDir,Infinity]];
assert["acquisition abort handled",acquireResult["Status"]==="Aborted"];
assert["acquired streams all closed",AllTrue[acquiredStreams,!MemberQ[Streams[],#]&]];
assert["acquisition abort launches no process",acquireResult["ProcessStatus"]==="NotStarted"];
finalDir=CreateDirectory[FileNameJoin[{root,"final-abort"}]];
finalResult=Block[{CalcFormConverter`Private`inspectRuntimeProcess},
 CalcFormConverter`Private`inspectRuntimeProcess[_]:=Abort[];
 CalcFormConverter`Private`runtimeProcess[{FileNameJoin[{root,"missing"}]},finalDir,Infinity]];
assert["final inspection abort handled",finalResult["Status"]==="Aborted"];
assert["final inspection closes streams",FreeQ[Streams[],OutputStream[path_String,_]/;StringStartsQ[path,finalDir]]];
assert["final inspection leaves child stopped",finalResult["ProcessStatus"]==="Finished"];
probeAcquired=None;
probeAcquisition=Block[{CalcFormConverter`Private`createJobDirectory},
 CalcFormConverter`Private`createJobDirectory[_,prefix_]:=(probeAcquired=CreateDirectory[FileNameJoin[{root,prefix<>CreateUUID[]}]];Abort[];probeAcquired);
 CalcFormCheck[]];
assert["probe acquisition abort handled",probeAcquisition["Status"]==="Aborted"];
assert["probe directory cleaned after acquisition abort",StringQ[probeAcquired] && !DirectoryQ[probeAcquired]];
probeAcquired=None;
probeBeforeFinalize=Block[{CalcFormConverter`Private`runFORMProbe},
 CalcFormConverter`Private`runFORMProbe[_,directory_,_]:=(probeAcquired=directory;Abort[]);
 CalcFormCheck[]];
assert["probe body abort handled",probeBeforeFinalize["Status"]==="Aborted" && !DirectoryQ[probeAcquired]];
throwDir=CreateDirectory[FileNameJoin[{root,"throw-cleanup"}]];
nonlocal=Catch[Block[{CalcFormConverter`Private`runtimePause},
 CalcFormConverter`Private`runtimePause[]:=Throw["test-throw","test-tag"];
 CalcFormConverter`Private`runtimeProcess[{FileNameJoin[{root,"hang"}]},throwDir,Infinity]],"test-tag"];
assert["nonlocal exit preserved",nonlocal==="test-throw"];
assert["nonlocal exit closes streams",FreeQ[Streams[],OutputStream[path_String,_]/;StringStartsQ[path,throwDir]]];

(* Actual runtime cancellation stays deferred for protected installer processes. *)
protectedDir=CreateDirectory[FileNameJoin[{root,"protected-runtime"}]];
finishedProtected=False;
protectedResult=CheckAbort[Block[{CalcFormConverter`Private`runtimePause},
 CalcFormConverter`Private`runtimePause[]:=Abort[];
 CalcFormConverter`Private`runInstallation[{FileNameJoin[{root,"shortwait"}]},protectedDir]],"deferred"];
assert["installer runtime does not kill on abort",protectedResult==="deferred" && FileExistsQ[FileNameJoin[{protectedDir,"finished.txt"}]]];
assert["working directory unchanged",Directory[]===beforeDirectory];
finish["Runtime"];
