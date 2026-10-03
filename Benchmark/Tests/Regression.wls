(* Developer checks. User notebooks require neither this script nor a shell. *)
$FeynCalcStartupMessages=False;
root=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{root,"Support","Benchmark.wl"}]];
checks=0;failures=0;
assert[name_,test_] := (checks++;If[!TrueQ[test],failures++;Print["FAIL: ",name]]);
initial=BenchmarkConfiguration[];
assert["profile initially unset",initial["Profile"]===None];
count=0;
unset=Block[{CalcFormCheck},CalcFormCheck[___]:=(count++;Throw["unexpected"]);BenchmarkPrepare["Overview",initial]];
assert["unset starts no check",count===0 && MatchQ[unset,Failure["ProfileRequired",_]]];
assert["invalid counts rejected",FailureQ[BenchmarkPrepare["Overview",Join[initial,<|"Profile"->"Quick","Repetitions"->0|>]]]];
quick=FeynGravBenchmark`Private`settings[Join[initial,<|"Profile"->"Quick"|>]];
full=FeynGravBenchmark`Private`settings[Join[initial,<|"Profile"->"Full"|>]];
assert["profile defaults",Lookup[quick,{"Repetitions","PhysicalRepetitions","ExecutionTimeout"}]==={3,1,60} && Lookup[full,{"Repetitions","PhysicalRepetitions","ExecutionTimeout"}]==={5,3,600}];
assert["automatic workers capped",AllTrue[full["Workers"],#<=$ProcessorCount&]];
probeCount=0;
abortedPreparation=Block[{CalcFormCheck},CalcFormCheck[___]:=(probeCount++;<|"Status"->"Aborted","Available"->False|>);
 BenchmarkPrepare["Scaling",Join[initial,<|"Profile"->"Quick"|>]]];
assert["aborted probe stops preparation",probeCount===1 && MatchQ[abortedPreparation,Failure["Aborted",_]]];

assert["explicit workers preserved",FeynGravBenchmark`Private`settings[Join[initial,<|"Profile"->"Quick","Workers"->{16}|>]]["Workers"]==={16}];
space=CreateDirectory[FileNameJoin[{$TemporaryDirectory,"benchmark test "<>CreateUUID[]}]];
config=Join[initial,<|"Profile"->"Quick","Repetitions"->1,"PhysicalRepetitions"->1,"OutputDirectory"->space|>];
before=Directory[];
prepared=BenchmarkPrepare["Overview",config];
assert["setup does not change directory",Directory[]===before];
assert["source hashes correctly named",KeyExistsQ[prepared["Environment","SourceHashes"],"FeynGrav.wl"]];
prepared["Cases"]=Take[prepared["Cases"],1];
report=BenchmarkRun[prepared];
assert["small calculation completed",report["RunStatus"]==="Complete" && Length[report["Summary"]]===1];
assert["rerunning measurement preserves report",MatchQ[BenchmarkRun[prepared],Failure["RunAlreadyStarted",_]]];
assert["timing chart renders",Head[BenchmarkPlot[report]]===Graphics];
assert["one measured trial excludes warmup",First[report["Summary"]]["Trials"]===1 && Count[Lookup[report["Rows"],"Phase"],"Warmup"]===1];
assert["JSON round trip",AssociationQ[Import[FileNameJoin[{report["Directory"],"report.json"}],"RawJSON"]]];
assert["CSV exists",FileExistsQ[FileNameJoin[{report["Directory"],"summary.csv"}]]];
second=BenchmarkPrepare["Overview",config];second["Cases"]=Take[second["Cases"],1];
assert["rerun creates independent directory",prepared["Directory"]=!=second["Directory"]];
assert["cleanup rejects forged location",FailureQ[BenchmarkCleanup[Join[report,<|"Directory"->space|>]]]];
assert["cleanup rejects unknown run",FailureQ[BenchmarkCleanup[<|"ID"->"unknown"|>]]];
retained=CreateDirectory[FileNameJoin[{report["Directory"],"jobs","failed-fixture"}]];
Export[FileNameJoin[{retained,"stderr.log"}],"retained diagnostic","Text"];
owned=FeynGravBenchmark`Private`$ownedRuns[report["ID"],"SuccessfulDirectories"];
BenchmarkCleanup[report];
assert["successful jobs deleted",AllTrue[owned,!DirectoryQ[#]&]];
assert["report and failure log retained",FileExistsQ[FileNameJoin[{report["Directory"],"report.json"}]] && FileExistsQ[FileNameJoin[{retained,"stderr.log"}]]];
missing=BenchmarkPrepare["Overview",Join[config,<|"FORMExecutable"->FileNameJoin[{space,"missing-form"}]|>]];
missing["Cases"]=Take[missing["Cases"],1];missingResult=BenchmarkRun[missing];
assert["missing executable skipped",MemberQ[Lookup[missingResult["Rows"],"Status"],"Skipped"] && missingResult["Summary"]==={}];
(* Use a small known case through the staged path to exercise the adapter. *)
second["Suite"]="Representative";
staged=BenchmarkRun[second];
assert["staged/public results verified",AllTrue[Select[staged["Rows"],MemberQ[{"Execute","Import","Calculate"},#["Stage"]]&],#["Validation"]==="Verified"&]];
assert["stage sums correct",With[{measured=Select[staged["Rows"],#["Phase"]==="Measured"&]},
 Abs[Total[Lookup[Select[measured,MemberQ[{"Export","Execute","Import"},#["Stage"]]&],"Seconds"]]-First[Select[measured,#["Stage"]==="StageSum"&]]["Seconds"]]<10^-9]];
(* Timeout and abort diagnostics through the benchmark driver are injected;
   real child termination is tested by the converter's runtime suite. *)
Do[
 p=BenchmarkPrepare["Overview",config];p["Cases"]=Take[p["Cases"],1];p["Suite"]="Representative";
 r=Block[{FeynGravBenchmark`Private`executeFORM},
  FeynGravBenchmark`Private`executeFORM[_,_,_,directory_,_] := (Export[FileNameJoin[{directory,"stderr.log"}],"injected "<>mode,"Text"];
   <|"Status"->mode,"ExitCode"->1,"ElapsedSeconds"->0.01|>);
  BenchmarkRun[p]];
 assert[mode<>" retained",MemberQ[Lookup[r["Rows"],"Status"],mode] && FileNames["stderr.log",r["Directory"],Infinity]=!={}];
 If[mode==="Aborted",assert["abort stops driver",r["RunStatus"]==="Aborted"]],{mode,{"TimedOut","Aborted"}}];
(* Inconclusive validation must not enter a performance summary. *)
p=BenchmarkPrepare["Overview",config];p["Cases"]=Take[p["Cases"],1];p["Suite"]="Import";
inconclusive=Block[{CalcFormImport},CalcFormImport[___]:=77;BenchmarkRun[p]];
assert["inconclusive imports excluded",Select[inconclusive["Summary"],#["Stage"]==="Import"&]==={} &&
 AllTrue[Select[inconclusive["Rows"],#["Stage"]==="Import"&],#["Validation"]==="Inconclusive"&]];
(* Exercise Full repetition defaults on the tiny control, including the branch
   used by substantial physical workloads. *)
p=BenchmarkPrepare["Overview",Join[initial,<|"Profile"->"Full","OutputDirectory"->space|>]];
p["Cases"]=Take[p["Cases"],1];fullSmall=BenchmarkRun[p];
assert["Full small trials executed",First[fullSmall["Summary"]]["Trials"]===5];
p=BenchmarkPrepare["Overview",Join[initial,<|"Profile"->"Full","OutputDirectory"->space|>]];
p["Cases"]={Join[First[p["Cases"]],<|"Physical"->True|>]};fullPhysical=BenchmarkRun[p];
assert["Full physical trials executed",First[fullPhysical["Summary"]]["Trials"]===3];
(* The adapter deletes a stale output before launching a later trial. *)
d=CreateDirectory[FileNameJoin[{space,"stale"}]];output=FileNameJoin[{d,"job.out"}];Export[output,"old result","Text"];
Block[{CalcFormConverter`Private`runtimeProcess},
 CalcFormConverter`Private`runtimeProcess[___]:=<|"Status"->"Completed","ExitCode"->0|>;
 FeynGravBenchmark`Private`executeFORM[<|"InputFile"->FileNameJoin[{d,"job.frm"}],"ResultFile"->output|>,"unused",1,d,10]];
assert["stale result removed",!FileExistsQ[output]];
(* Real child cancellation through the adapter. The large polynomial prevents
   FORM from finishing before the deliberately tiny timeout/abort boundary. *)
source=FileNameJoin[{d,"job.frm"}];
Export[source,"Symbols x;\nLocal F=(1+x)^10000;\n.sort\n.end\n","Text"];
realJob=<|"InputFile"->source,"ResultFile"->output|>;
realTimeout=FeynGravBenchmark`Private`executeFORM[realJob,prepared["Checks","1","Result","Executable"],1,d,.001];
assert["real timeout stops FORM",realTimeout["Status"]==="TimedOut" && realTimeout["ProcessStatus"]==="Finished"];
realAbort=Block[{CalcFormConverter`Private`runtimePause},CalcFormConverter`Private`runtimePause[]:=Abort[];
 FeynGravBenchmark`Private`executeFORM[realJob,prepared["Checks","1","Result","Executable"],1,d,10]];
assert["real abort stops FORM",realAbort["Status"]==="Aborted" && realAbort["ProcessStatus"]==="Finished"];

assert["default repetition loops",Length[Range[0,quick["Repetitions"]]]===4 && Length[Range[0,full["Repetitions"]]]===6];
Do[
 nb=Get[f];inputs=Cases[nb,Cell[BoxData[b_],"Input",___]:>b,Infinity];
 assert[FileNameTake[f]<>" input parse",AllTrue[inputs,ToExpression[#,StandardForm,HoldComplete]=!=$Failed&]];
 assert[FileNameTake[f]<>" explicit profile",!FreeQ[inputs,"\"Profile\""] && !FreeQ[inputs,"None"]];
 assert[FileNameTake[f]<>" no initialization",FreeQ[nb,InitializationCell->True]],{f,FileNames["*.nb",root]}];
Print["Benchmark regression: ",checks," assertions; ",failures," failures."];
Print["Test artifacts: ",space];Quit[If[failures===0,0,1]];
