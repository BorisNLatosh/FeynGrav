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
assertFailure["invalid probe timeout","InvalidOption",CalcFormCheck[TimeConstraint->0]];
assertFailure["invalid executable option","InvalidOption",CalcFormCheck[FORMExecutable->42]];
probeBefore=Sort[FileNames["calcform-probe-*",$TemporaryDirectory]];
assert["probe timeout",CalcFormCheck[FORMExecutable->FileNameJoin[{root,"probehang"}],TimeConstraint->.1]["Status"]==="TimedOut"];
assert["probe wrong arithmetic",CalcFormCheck[FORMExecutable->FileNameJoin[{root,"probebad"}]]["Status"]==="ProbeFailed"];
assert["probe cleanup",Sort[FileNames["calcform-probe-*",$TemporaryDirectory]]===probeBefore];

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
assert["bounded log tails",StringLength[flood["StandardOutput"]]<=65536 && StringLength[flood["StandardError"]]<=65536];

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
