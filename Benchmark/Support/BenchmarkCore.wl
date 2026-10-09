(* Shared reporting and measurement implementation. Load through an entry point. *)
With[{root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]]},
 If[!TrueQ[FeynGravBenchmark`Private`$loadingGenerator] && !MemberQ[$Packages,"FeynGrav`"],
  Get[FileNameJoin[{root,"FeynGrav.wl"}]]]];
BeginPackage["FeynGravBenchmark`",If[TrueQ[FeynGravBenchmark`Private`$loadingGenerator],
 {"FeynCalc`","CalcFormConverter`"},{"FeynGrav`","FeynCalc`","CalcFormConverter`"}]];
BenchmarkConfiguration::usage="BenchmarkConfiguration[] returns editable settings. Choose Quick or Full before preparing a run.";
BenchmarkPrepare::usage="BenchmarkPrepare[suite,config] checks the environment and constructs inputs outside conversion timing.";
BenchmarkRun::usage="BenchmarkRun[prepared] executes sequential trials and saves reports.";
BenchmarkSummary::usage="BenchmarkSummary[report] displays validated measured timings.";
BenchmarkPlot::usage="BenchmarkPlot[report] plots validated timing medians.";
BenchmarkCleanup::usage="BenchmarkCleanup[report] deletes only successful working directories owned by this session, retaining reports and failed-job logs.";
Begin["`Private`"];
$benchmarkRoot=FileNameJoin[FileNameSplit[DirectoryName[DirectoryName[$InputFileName]]]];
$packageRoot=FileNameJoin[FileNameSplit[DirectoryName[$benchmarkRoot]]];
If[!AssociationQ[$ownedRuns],$ownedRuns=<||>];
If[!TrueQ[$importObserved],$importObserved=False];
$workflowMode=If[TrueQ[$loadingGenerator],"Generator","Main"];
If[$workflowMode==="Main",Get[FileNameJoin[{$benchmarkRoot,"Support","Workloads.wl"}]]];

(* ::Section:: *)
(* Configuration and provenance *)
BenchmarkConfiguration[] := <|"Profile"->None,"FORMExecutable"->Automatic,"TFORMExecutable"->Automatic,
 "Workers"->Automatic,"GeneratorThreads"->Automatic,"Repetitions"->Automatic,"PhysicalRepetitions"->Automatic,
 "ExecutionTimeout"->Automatic,"OutputDirectory"->Automatic|>;
settings[input_Association] := Module[{c=Join[BenchmarkConfiguration[],input],full},
 If[!MemberQ[{"Quick","Full"},c["Profile"]],Return[Failure["ProfileRequired",<|"MessageTemplate"->"Choose Quick or Full in the configuration cell; no benchmark was started."|>]]];
 full=c["Profile"]==="Full";
 AssociateTo[c,{"Workers"->Replace[c["Workers"],Automatic:>Select[If[full,{2,4,8},{2,4}],#<=$ProcessorCount&]],
  "Repetitions"->Replace[c["Repetitions"],Automatic->If[full,5,3]],
  "PhysicalRepetitions"->Replace[c["PhysicalRepetitions"],Automatic->If[full,3,1]],
  "ExecutionTimeout"->Replace[c["ExecutionTimeout"],Automatic->If[full,600,60]],
  "OutputDirectory"->Replace[c["OutputDirectory"],Automatic:>$TemporaryDirectory]}];
 If[!MatchQ[c["Workers"],{___Integer}] || !AllTrue[c["Workers"],#>1&] ||
  !AllTrue[Lookup[c,{"Repetitions","PhysicalRepetitions"}],IntegerQ[#]&&#>0&] ||
  !(c["ExecutionTimeout"]===Infinity || (NumberQ[c["ExecutionTimeout"]] && c["ExecutionTimeout"]>0)) ||
  !StringQ[c["OutputDirectory"]] || !DirectoryQ[c["OutputDirectory"]],
  Return[Failure["InvalidConfiguration",<|"MessageTemplate"->"Use positive repetition counts, integer worker counts greater than one, a positive timeout and an existing output directory."|>]]];
 AssociateTo[c,"Workers"->DeleteDuplicates[c["Workers"]]];c
];
json[x_Association] := Association@KeyValueMap[#1->json[#2]&,x];
json[x_List] := json /@ x;
json[x_?NumberQ] := If[Head[x]===Complex,ToString[x,InputForm],x];
json[x_String] := x;
json[True]=True;json[False]=False;json[Null]=Null;
json[x_] := ToString[x,InputForm];
fingerprint[x_] := Hash[x,"SHA256","HexString"];
metadata[c_] := Module[{e=FCI[c["Expression"]]},
 <|"Case"->c["ID"],"InputFingerprint"->fingerprint[e],"InputLeaves"->LeafCount[e],
 "DistinctSymbols"->Length[DeleteDuplicates[Cases[e,_Symbol,Infinity]]],
 "DistinctPairs"->Length[DeleteDuplicates[Cases[e,_Pair,Infinity]]],
 "DistinctPropagators"->Length[DeleteDuplicates[Cases[e,_PropagatorDenominator,Infinity]]],
 "Dimension"->"D","LoopMomenta"->ToString[c["LoopMomenta"],InputForm],
 "RequestedEntries"->Lookup[c,"RequestedEntries",Missing["NotApplicable"]]|>
];
environment[] := Module[{files},
 files=Join[FileNames["*.wl",FileNameJoin[{$packageRoot,"Rules"}]],
 {FileNameJoin[{$packageRoot,"Libs","FeynGravLibrariesGenerator.wl"}]},
 FileNames["*.prc",FileNameJoin[{$packageRoot,"CalcFormConverter"}],Infinity],{FileNameJoin[{$packageRoot,"FeynGrav.wl"}]},
 FileNames["*.wl",FileNameJoin[{$packageRoot,"CalcFormConverter"}]],
 FileNames["*",FileNameJoin[{$packageRoot,"CalcFormConverter","Templates"}]],
 FileNames["*.wl",FileNameJoin[{$benchmarkRoot,"Support"}]]];
 <|"WolframVersion"->$Version,"FeynCalcVersion"->$FeynCalcVersion,"SystemID"->$SystemID,
 "OperatingSystem"->$OperatingSystem,"ProcessorCount"->$ProcessorCount,
 "AvailableSystemMemoryBytes"->Quiet[Check[MemoryAvailable[],Missing["NotAvailable"]]],
 "KernelMemoryBytes"->MemoryInUse[],"KernelProcessID"->$ProcessID,
 "PackageRoot"->$packageRoot,"SourceHashes"->Association[Table[StringDrop[f,StringLength[$packageRoot]+1]->FileHash[f,"SHA256","HexString"],{f,Select[files,FileType[#]===File&]}]]|>
];

(* ::Section:: *)
(* Run ownership and preparation *)
BenchmarkPrepare[suite_String,input_Association] := Module[{c=settings[input],directory,id,env,checks=<||>,t,check,builders,cases={},built,seconds,probeFailure=None},
 If[suite==="Generation",Return[If[$workflowMode==="Generator",generationPrepare[input],
 Failure["FreshKernelRequired",<|"MessageTemplate"->"Restart the kernel and load GeneratorBenchmark.wl for library generation."|>]]]];
 If[$workflowMode=!= "Main",Return[Failure["FreshKernelRequired",<|"MessageTemplate"->"Restart the kernel for the main-package benchmarks."|>]]];
 If[FailureQ[c],Print[c];Return[c]];
 If[!MemberQ[{"Overview","Export","Import","Scaling","Representative"},suite],Return[Failure["UnknownSuite",<||>]]];
 id=CreateUUID["cfc-benchmark-"];
 directory=Quiet[Check[CreateDirectory[FileNameJoin[{ExpandFileName[c["OutputDirectory"]],id}]],$Failed]];
 If[directory===$Failed,Return[Failure["OutputDirectoryFailed",<|"MessageTemplate"->"Cannot create a benchmark run directory.","ParentDirectory"->c["OutputDirectory"]|>]]];
 CreateDirectory[FileNameJoin[{directory,"jobs"}]];Export[FileNameJoin[{directory,"owner.txt"}],id,"Text"];
 AssociateTo[$ownedRuns,id-><|"Directory"->directory,"SuccessfulDirectories"->{},"Started"->False|>];
 env=environment[];
 If[suite=!= "Export",
  Do[Print["Checking ",If[w===1,"FORM","TFORM ("<>ToString[w]<>" workers)"]];
   {t,check}=measure[CalcFormCheck[FORMExecutable->If[w===1,c["FORMExecutable"],c["TFORMExecutable"]],FORMThreads->w]];
   AssociateTo[checks,ToString[w]-><|"Seconds"->t,"Result"->check|>];
   If[check===$Aborted || (AssociationQ[check] && Lookup[check,"Status",None]==="Aborted"),
    Export[FileNameJoin[{directory,"report.json"}],json[<|"RunStatus"->"Aborted","Stage"->"Check","Environment"->env,"Checks"->checks|>],"RawJSON"];
    probeFailure=Failure["Aborted",<|"MessageTemplate"->"Benchmark preparation was aborted.","Directory"->directory,"Check"->check|>];Break[]],
   {w,If[suite==="Scaling",Prepend[c["Workers"],1],{1}]}]];
 If[FailureQ[probeFailure],Return[probeFailure]];
 Print["Preparing inputs; this is outside export, execution and import measurements."];
 If[MemberQ[{"Scaling","Representative"},suite] || (suite==="Import" && c["Profile"]==="Full"),loadPhysicalLibraries[]];
 builders=workloadBuilders[suite,c["Profile"]];
 Do[{seconds,built}=AbsoluteTiming[builder[]];
  AppendTo[cases,Join[built,<|"ConstructionSeconds"->seconds,"Metadata"->metadata[built]|>]];Print["Prepared ",built["ID"]],{builder,builders}];
 <|"ID"->id,"Suite"->suite,"Configuration"->c,"Directory"->directory,"Environment"->env,
 "Checks"->checks,"Cases"->cases|>
];
BenchmarkPrepare[___] := Failure["InvalidConfiguration",<|"MessageTemplate"->"Use a suite name and BenchmarkConfiguration association."|>];

(* This is the only dependency on private converter process functions. Source
   paths are absolute, output paths remain those declared by the exporter, and
   each execution gets its own log directory. Jobs are strictly sequential. *)
executeFORM[job_Association,executable_String,workers_Integer,directory_String,limit_] := (
 (* A previous successful result must never satisfy a later missing-result test. *)
 If[FileExistsQ[job["ResultFile"]],DeleteFile[job["ResultFile"]]];
 CalcFormConverter`Private`runtimeProcess[
  CalcFormConverter`Private`formCommand[executable,job["InputFile"],workers],directory,limit,True,True]);

(* ::Section:: *)
(* Timing, validation and reporting *)
SetAttributes[measure,HoldFirst];
measure[expr_] := Module[{t,result}, {t,result}=AbsoluteTiming[CheckAbort[expr,$Aborted]];{t,result}];
usable[check_] := AssociationQ[check] && TrueQ[Lookup[check,"Available",False]];
outcome[v_] := Which[v===$Aborted,"Aborted",v===$Failed,"Failure",
 MatchQ[v,Failure["TimedOut",_]],"TimedOut",MatchQ[v,Failure["Aborted",_]],"Aborted",FailureQ[v],"Failure",True,"Success"];
newJob[run_,name_] := CreateDirectory[FileNameJoin[{run["Directory"],"jobs",name<>"-"<>CreateUUID[]}]];
$generationColumns={"Case","Stage","Workers","Trials","MeanSeconds","MedianSeconds"};
$summaryColumns={"Case","Stage","Workers","Trials","MedianSeconds","MinimumSeconds","MaximumSeconds","SerialSpeedup"};
summaryRows[rows_] := Module[{groups,summary,serial},
 groups=GatherBy[Select[rows,Lookup[#,"Phase",None]==="Measured" && Lookup[#,"Status",None]==="Success" &&
 Lookup[#,"Validation",None]==="Verified" && NumberQ[Lookup[#,"Seconds",None]]&],Lookup[#,{"Case","Stage","Workers"}]&];
 summary=Map[Function[g,Join[KeyTake[First[g],{"Case","Stage","Workers"}],
  <|"Trials"->Length[g],"MeanSeconds"->Mean[Lookup[g,"Seconds"]],"MedianSeconds"->Median[Lookup[g,"Seconds"]],"MinimumSeconds"->Min[Lookup[g,"Seconds"]],"MaximumSeconds"->Max[Lookup[g,"Seconds"]]|>]],groups];
 Map[Function[r,serial=Select[summary,#["Case"]===r["Case"] && #["Stage"]===r["Stage"] && #["Workers"]===1&];
  Join[r,<|"SerialSpeedup"->If[r["Stage"]==="Execute" && serial=!={} && r["MedianSeconds"]>0,First[serial]["MedianSeconds"]/r["MedianSeconds"],Missing["NotApplicable"]]|>]],summary]
];
saveReport[run_,rows_,state_] := Module[{report,path,summary},
 summary=summaryRows[rows];
 report=Join[KeyDrop[run,{"Cases"}],<|"RunStatus"->state,"Cases"->(Join[#["Metadata"],KeyTake[#,{"ConstructionSeconds","Physical"}]]& /@ run["Cases"]),
 "Rows"->rows,"Summary"->If[run["Suite"]==="Generation",KeyTake[#,$generationColumns]& /@ summary,summary]|>];
 path=FileNameJoin[{run["Directory"],"report.json"}];
 Export[path,json[report],"RawJSON"];
 Export[FileNameJoin[{run["Directory"],"summary.csv"}],Prepend[(json[Lookup[#,If[run["Suite"]==="Generation",$generationColumns,$summaryColumns]]]& /@ summary),If[run["Suite"]==="Generation",$generationColumns,$summaryColumns]],"CSV"];
 report
];
BenchmarkRun[run_Association] := Module[
 {rows={},refs=<||>,report,state="Complete",cfg=run["Configuration"],suite=run["Suite"],append,validate,recordImport,
 exportJob,onePipeline,oneCalculate,markSuccessful,checks=run["Checks"],row,c,e,job,dir,execDir,executable,workers,
 seconds,value,result,process,rep,phase,reps,order,configs,preparedJob,aborted=False,base,stageTimes},
 If[suite==="Generation",Return[generationRun[run]]];
 If[!KeyExistsQ[$ownedRuns,run["ID"]],Return[Failure["UnknownRun",<||>]]];
 If[TrueQ[$ownedRuns[run["ID"],"Started"]],Return[Failure["RunAlreadyStarted",<|"MessageTemplate"->"Evaluate the preparation cell again to start a new run; previous reports are preserved."|>]]];
 $ownedRuns[run["ID"],"Started"]=True;
 append[data_Association] := (AppendTo[rows,data];report=saveReport[run,rows,"Running"];Null);
 base[c_,stage_,w_,ph_,r_] := Join[c["Metadata"],<|"Stage"->stage,"Workers"->w,"Phase"->ph,"Trial"->r|>];
 validate[c_,v_] := Module[{hash,known,test,id=c["ID"]},
  If[FailureQ[v] || v===$Aborted,Return["Failed"]];hash=fingerprint[v];known=c["Reference"];
  If[!MissingQ[known],test=TimeConstrained[Simplify[ExpandScalarProduct[FCI[v-known]]],30,$TimedOut];
    Return[If[test===0,"Verified","Inconclusive"]]];
  If[KeyExistsQ[refs,id],
    If[refs[id]===hash,
      rows=Map[If[Lookup[#,"Case",None]===id && Lookup[#,"Validation",None]==="Baseline",Join[#,<|"Validation"->"Verified"|>],#]&,rows];"Verified","Inconclusive"],
    AssociateTo[refs,id->hash];"Baseline"]
 ];
 markSuccessful[d_] := ($ownedRuns[run["ID"],"SuccessfulDirectories"]=Append[$ownedRuns[run["ID"],"SuccessfulDirectories"] ,d]);
 recordImport[c_,j_,w_,ph_,r_] := Module[{first=!TrueQ[$importObserved],timed,v,status},
  Print["  ",ph," ",r,": import (",w," worker configuration)"];
  $importObserved=True;timed=measure[CalcFormImport[j["ResultFile"],j["MappingFile"]]];v=Last[timed];status=validate[c,v];
  append[Join[base[c,"Import",w,ph,r],<|"Seconds"->First[timed],"Status"->outcome[v],
   "Validation"->status,"ValidationMethod"->If[MissingQ[c["Reference"]],"Repeated result fingerprint","Known reference"],
   "FirstBenchmarkImportInKernel"->first,"ResultBytes"->FileByteCount[j["ResultFile"]],
   "ResultLeaves"->If[FailureQ[v] || v===$Aborted,Null,LeafCount[v]],"ArtifactDirectory"->DirectoryName[j["ResultFile"]],
   "Diagnostic"->If[FailureQ[v],ToString[v,InputForm],""]|>]];
  If[v===$Aborted,Throw["Aborted","benchmark-stop"]];{v,First[timed],status}
 ];
 exportJob[c_,ph_,r_] := Module[{d=newJob[run,"export"],t,j,mapping,valid},
  Print["  ",ph," ",r,": export"];
  {t,j}=measure[CalcFormExport[c["Expression"],FileNameJoin[{d,"job.frm"}],LoopMomenta->c["LoopMomenta"]]];
  valid=If[AssociationQ[j],mapping=Import[j["MappingFile"],"RawJSON"];Lookup[mapping,"ExpressionDigest",None]===c["Metadata","InputFingerprint"],False];
  append[Join[base[c,"Export",0,ph,r],<|"Seconds"->t,"Status"->If[j===$Aborted,"Aborted",If[AssociationQ[j],"Success","Failure"]],
   "Validation"->If[valid,"Verified","Failed"],"ValidationMethod"->"Export mapping input digest",
   "FORMBytes"->If[AssociationQ[j],FileByteCount[j["InputFile"]],Null],"MappingBytes"->If[AssociationQ[j],FileByteCount[j["MappingFile"]],Null],
   "ArtifactDirectory"->d,"Diagnostic"->If[FailureQ[j],ToString[j,InputForm],""]|>]];
  If[j===$Aborted,Throw["Aborted","benchmark-stop"]];If[valid,{j,t,d},$Failed]
 ];
 onePipeline[c_,j_,w_,ph_,r_] := Module[{d=newJob[run,"execute"],proc,t,imported,valid,savedJob},
  Print["  ",ph," ",r,": execute with ",w," worker(s)"];
  {t,proc}=measure[executeFORM[j,checks[ToString[w],"Result","Executable"],w,d,cfg["ExecutionTimeout"]]];
  If[proc===$Aborted,Throw["Aborted","benchmark-stop"]];
  If[proc["Status"]==="Aborted",append[Join[base[c,"Execute",w,ph,r],<|"Status"->"Aborted","Validation"->"Failed","Seconds"->t,"Process"->proc|>]];Throw["Aborted","benchmark-stop"]];
  If[proc["Status"]=!="Completed" || proc["ExitCode"]=!=0 || !FileExistsQ[j["ResultFile"]],
   append[Join[base[c,"Execute",w,ph,r],<|"Seconds"->t,"Status"->If[proc["Status"]==="Completed","Failure",proc["Status"]],"Validation"->"Failed","Process"->proc,"ArtifactDirectory"->d|>]];Return[$Failed]];
  (* Snapshot outside timing: the next trial reuses the same generated program. *)
  CopyFile[j["ResultFile"],FileNameJoin[{d,"result.out"}]];
  savedJob=Join[j,<|"ResultFile"->FileNameJoin[{d,"result.out"}]|>];
  imported=recordImport[c,savedJob,w,ph,r];valid=Last[imported];
  append[Join[base[c,"Execute",w,ph,r],<|"Seconds"->t,"Status"->"Success","Validation"->valid,
   "ValidationMethod"->If[MissingQ[c["Reference"]],"Repeated result fingerprint","Known reference"],"Process"->proc,"ArtifactDirectory"->d|>]];
  If[valid==="Verified",markSuccessful[d]];
  {First[imported],t,imported[[2]],valid}
 ];
 oneCalculate[c_,w_,ph_,r_] := Module[{d=newJob[run,"calculate"],t,v,valid},
  Print["  ",ph," ",r,": complete CalcFormCalculate call"];
  {t,v}=measure[CalcFormCalculate[c["Expression"],LoopMomenta->c["LoopMomenta"],FORMThreads->w,
    FORMExecutable->checks[ToString[w],"Result","Executable"],WorkingDirectory->d,TimeConstraint->cfg["ExecutionTimeout"],ShowProgress->True]];
  If[outcome[v]==="Success",$importObserved=True];valid=validate[c,v];
  append[Join[base[c,"Calculate",w,ph,r],<|"Seconds"->t,"Status"->outcome[v],"Validation"->valid,
   "ValidationMethod"->If[MissingQ[c["Reference"]],"Agreement with staged result","Known reference"],"ArtifactDirectory"->d,"Diagnostic"->If[FailureQ[v],ToString[v,InputForm],""]|>]];
  If[v===$Aborted || MatchQ[v,Failure["Aborted",_]],Throw["Aborted","benchmark-stop"]];
  If[valid==="Verified",markSuccessful[d]]
 ];
 report=saveReport[run,rows,"Running"];
 state=CheckAbort[Catch[
 Do[
  Print["Benchmark: ",c["ID"]];reps=If[c["Physical"],cfg["PhysicalRepetitions"],cfg["Repetitions"]];
  append[Join[base[c,"Construction",0,"Preparation",0],<|"Seconds"->c["ConstructionSeconds"],"Status"->"Success","Validation"->"Recorded"|>]];
  If[suite==="Export",Do[preparedJob=exportJob[c,If[rep===0,"Warmup","Measured"],rep];If[ListQ[preparedJob],markSuccessful[preparedJob[[3]]]],{rep,0,reps}];Continue[]];
  Do[If[!usable[checks[ToString[w],"Result"]],
   append[Join[base[c,"Availability",w,"Preparation",0],<|"Status"->"Skipped","Validation"->"NotRun","Diagnostic"->checks[ToString[w]]|>]]],
   {w,If[suite==="Scaling",Prepend[cfg["Workers"],1],{1}]}];
  configs=Select[If[suite==="Scaling",Prepend[cfg["Workers"],1],{1}],usable[checks[ToString[#],"Result"]]&];
  If[configs==={},Continue[]];
  If[suite==="Overview",Do[oneCalculate[c,1,If[rep===0,"Warmup","Measured"],rep],{rep,0,reps}];Continue[]];
  If[suite==="Import",
   Print["Preparing FORM output before import trials."];preparedJob=exportJob[c,"Preparation",0];If[preparedJob===$Failed,Continue[]];
   job=First[preparedJob];dir=newJob[run,"fixture-execute"];
   process=executeFORM[job,checks["1","Result","Executable"],1,dir,cfg["ExecutionTimeout"]];
   append[Join[base[c,"Execute",1,"Preparation",0],<|"Seconds"->process["ElapsedSeconds"],"Status"->If[process["Status"]==="Completed",If[process["ExitCode"]===0 && FileExistsQ[job["ResultFile"]],"Success","Failure"],process["Status"]],"Validation"->"Recorded","Process"->process|>]];
   If[process["Status"]==="Aborted",Throw["Aborted","benchmark-stop"]];
   If[process["Status"]=!="Completed" || process["ExitCode"]=!=0 || !FileExistsQ[job["ResultFile"]],Continue[]];
   Do[result=recordImport[c,job,1,If[rep===0,"Warmup","Measured"],rep];result=Null,{rep,0,reps}];
   If[AllTrue[Select[rows,#["Case"]===c["ID"] && #["Stage"]==="Import"&],#["Validation"]==="Verified"&],markSuccessful[preparedJob[[3]]];markSuccessful[dir]];Continue[]];
  If[suite==="Scaling",
   preparedJob=exportJob[c,"Preparation",0];If[preparedJob===$Failed,Continue[]];job=First[preparedJob];
   Do[order=If[OddQ[rep],Reverse[configs],configs];Do[result=onePipeline[c,job,workers,If[rep===0,"Warmup","Measured"],rep];result=Null,{workers,order}],{rep,0,reps}];
   (* Keep the shared program/output if any execution failed or was inconclusive. *)
   If[AllTrue[Select[rows,#["Case"]===c["ID"] && #["Stage"]==="Execute"&],#["Status"]==="Success" && #["Validation"]==="Verified"&],markSuccessful[preparedJob[[3]]]];Continue[]];
  Do[phase=If[rep===0,"Warmup","Measured"];preparedJob=exportJob[c,phase,rep];If[preparedJob===$Failed,Continue[]];
   result=onePipeline[c,First[preparedJob],1,phase,rep];
   If[ListQ[result],append[Join[base[c,"StageSum",1,phase,rep],<|"Seconds"->preparedJob[[2]]+result[[2]]+result[[3]],"Status"->"Success","Validation"->Last[result],"ValidationMethod"->"Sum of isolated export, execute and import times"|>]]];
   oneCalculate[c,1,phase,rep];If[ListQ[result] && Last[result]==="Verified",markSuccessful[preparedJob[[3]]]];result=Null,
   {rep,0,reps}],
 {c,run["Cases"]}];"Complete","benchmark-stop"],"Aborted"];
 report=saveReport[run,rows,state];Print["Reports: ",FileNameJoin[{run["Directory"],"report.json"}]];report
];
BenchmarkRun[f_Failure] := f;
(* Dataset owns dynamic front-end state. Keep its display in StandardForm even
   when FeynCalc selects TraditionalForm for mathematical output. *)
BenchmarkSummary[r_Association] := If[Lookup[r,"Suite",None]==="Generation",
 StandardForm[Grid[Prepend[Lookup[#,$generationColumns,""]& /@ summaryRows[r["Rows"]],$generationColumns],Frame->All,Alignment->Left]],
 StandardForm[Dataset[r["Summary"]]]];
BenchmarkSummary[f_Failure] := f;
BenchmarkPlot[r_Association] /; Lookup[r,"Suite",None]==="Generation" := generationPlot[r];
BenchmarkPlot[r_Association] := Module[{s=r["Summary"]},If[s==={},"No validated measured trials.",
 BarChart[Lookup[s,"MedianSeconds"],BarOrigin->Left,ChartLabels->(#["Case"]<>" / "<>#["Stage"]<>" / "<>ToString[#["Workers"]]& /@ s),
  AxesLabel->{"Wall seconds (median)",None},ImageSize->{950,Max[240,28 Length[s]]},ImagePadding->{{310,25},{50,20}},LabelStyle->10]]];
BenchmarkPlot[f_Failure] := f;
BenchmarkCleanup[r_Association] := Module[{id=Lookup[r,"ID",None],owned,root,dirs},
 If[!KeyExistsQ[$ownedRuns,id],Return[Failure["UnknownRun",<|"MessageTemplate"->"Cleanup requires a run owned by this kernel session."|>]]];
 owned=$ownedRuns[id];root=owned["Directory"];
 If[Lookup[r,"Directory",None]=!=root || !FileExistsQ[FileNameJoin[{root,"owner.txt"}]] || Import[FileNameJoin[{root,"owner.txt"}],"Text"]=!=id,
 Return[Failure["OwnershipMismatch",<||>]]];
 dirs=DeleteDuplicates[owned["SuccessfulDirectories"]];
 Do[If[FileNameSplit[DirectoryName[d]]===FileNameSplit[FileNameJoin[{root,"jobs"}]] && DirectoryQ[d],DeleteDirectory[d,DeleteContents->True]],{d,dirs}];
 $ownedRuns[id,"SuccessfulDirectories"]={};
 Print["Successful working files removed. Reports and unsuccessful-job files remain in ",root];root
];
BenchmarkCleanup[_] := Failure["UnknownRun",<||>];
End[];
EndPackage[];
