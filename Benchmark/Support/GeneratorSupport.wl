(* ::Package:: *)
(* Load through GeneratorBenchmark.wl. *)
BeginPackage["FeynGravBenchmark`",{"FeynCalc`","CalcFormConverter`","FeynGravLibrariesGenerator`"}];
(* Compact presentation; full provenance and observations remain in JSON. *)
BenchmarkEnvironment::usage="BenchmarkEnvironment[prepared] displays the selected configuration and software versions.";
BenchmarkDiagnostics::usage="BenchmarkDiagnostics[report] displays only unsuccessful observations.";
Begin["`Private`"];

(* ::Section:: *)
(* Single adapter to generator internals; no interaction formulas are copied. *)
genSpecs[command_,args_] := FeynGravLibrariesGenerator`Private`requestSpecifications[command,args];
genName[spec_] := FeynGravLibrariesGenerator`Private`libraryFilename[spec];
genBuild[spec_] := Block[{Global`\[Kappa]},ReleaseHold[spec["Builder"]]];
genRead[path_] := Block[{FeynGrav`GaugeFixingEpsilonVector,FeynGrav`GaugeFixingEpsilonSUNYM,FeynGrav`\[Kappa]},
 Check[FeynGravLibrariesGenerator`Private`readLibrary[path],$Failed]];
genWrite[spec_,value_,path_] := Block[{FeynGrav`GaugeFixingEpsilonVector,FeynGrav`GaugeFixingEpsilonSUNYM,FeynGrav`\[Kappa]},
 Module[{mapped,written,read},
 mapped=FeynGravLibrariesGenerator`Private`libraryExpression[value,FeynGravLibrariesGenerator`Private`formalSymbols[spec["Builder"]]];
 If[genBad[mapped],Return[mapped]];
 written=Check[FeynGravLibrariesGenerator`Private`writeLibrary[path,mapped],$Failed];
 If[genBad[written],Return[written]];read=genRead[path];
 If[genBad[read],Return[read]];
 If[SameQ[read,mapped],Null,Failure["LibraryReadbackMismatch",<||>]]]];
genPublish[stage_,target_] := FeynGravLibrariesGenerator`Private`publishLibrary[stage,target];
genPublic[command_,args_,check_,cfg_,dir_] := With[{fn=Symbol["FeynGravLibrariesGenerator`"<>command]},
 Quiet[fn[Sequence@@args,OutputDirectory->dir,WorkingDirectory->dir,KeepFiles->True,
 FORMExecutable->check["Executable"],FORMThreads->check["FORMThreads"],TimeConstraint->cfg["ExecutionTimeout"]],CalcFormCalculate::files]];
genBad[x_] := !FreeQ[x,_Failure|$Failed|$Aborted];
genStatus[x_] := Which[!FreeQ[x,$Aborted|Failure["Aborted",_]],"Aborted",
 !FreeQ[x,Failure["TimedOut",_]],"TimedOut",genBad[x],"Failure",True,"Success"];

(* ::Section:: *)
(* Deterministic workloads *)
genCase[family_,args_List] := Module[{cmd="Generate"<>family<>"Specific",specs,id},
 specs=genSpecs[cmd,args];id=family<>"["<>StringRiffle[ToString/@args,","]<>"]";
 <|"ID"->id,"Kind"->"Generation","Command"->cmd,"Arguments"->args,"Specs"->specs,
 "Metadata"-><|"Case"->id,"Command"->cmd,"Arguments"->args,"Files"->(genName/@specs)|>|>];
genCases[profile_] := Join[
 genCase[#, {1}]& /@ {"GravitonScalars","GravitonFermions","GravitonVectors"},
 If[profile==="Full",Join[genCase[#,{2}]& /@ {"GravitonScalars","GravitonFermions","GravitonVectors"},
 {genCase["GravitonVertex",{1}],genCase["GravitonSUNYM",{1}],genCase["QuadraticGravityVertex",{1}],
 genCase["QuadraticGravityVertex",{2}],genCase["ScalarGaussBonnet",{2}],genCase["ScalarGaussBonnet",{3}],
 genCase["HorndeskiG2",{0,2,1}],genCase["HorndeskiG3",{0,1,1}],genCase["HorndeskiG4",{0,1,1}],genCase["HorndeskiG5",{2,0,1}]}],{}]];
genIndex[n_] := Symbol["FeynGravBenchmark`GeneratorData`i"<>ToString[n]];
genHelpers[profile_] := Flatten[Table[With[{id=family<>" pairs="<>ToString[n]},
 <|"ID"->id,"Kind"->"Helper","Family"->family,"Pairs"->n,
 "Metadata"-><|"Case"->id,"InternalPairs"->n,"Command"->family|>|>],
 {family,{"ITensor","CTensorGeneral","ETensor"}},{n,If[profile==="Full",Range[4],{1,2}]}],1];
genHelper[c_] := With[{a=genIndex/@Range[2 c["Pairs"]]},Switch[c["Family"],
 "ITensor",ITensor`ITensor[a],"CTensorGeneral",CTensorGeneral`CTensorGeneral[{},a],
 "ETensor",ETensor`ETensor[{genIndex[101],genIndex[102]},a]]];
(* ::Section:: *)
(* Preparation resolves workers without constructing any generation input. *)
genCheck[c_] := Module[{threads=c["GeneratorThreads"],path,check},
 If[threads===Automatic && c["FORMExecutable"]===Automatic && c["TFORMExecutable"]===Automatic,
 Return[CalcFormCheck[FORMThreads->Automatic]]];
 If[threads===Automatic,
 If[c["TFORMExecutable"]=!=Automatic,
 threads=If[IntegerQ[$ProcessorCount]&&$ProcessorCount>0,Min[8,$ProcessorCount],1];path=c["TFORMExecutable"],
 threads=1;path=c["FORMExecutable"]],
 path=If[threads===1,c["FORMExecutable"],c["TFORMExecutable"]]];
 CalcFormCheck[FORMExecutable->path,FORMThreads->threads]];
generationPrepare[input_Association] := Module[{cfg=settings[input],id,dir,check,seconds,run},
 If[FailureQ[cfg],Print[cfg];Return[cfg]];
 If[$workflowMode=!= "Generator" || MemberQ[$Packages,"FeynGrav`"],Return[Failure["FreshKernelRequired",<|"MessageTemplate"->"Use GeneratorBenchmark.wl in a fresh kernel."|>]]];
 If[!(cfg["GeneratorThreads"]===Automatic || (IntegerQ[cfg["GeneratorThreads"]]&&cfg["GeneratorThreads"]>0)),Return[Failure["InvalidConfiguration",<|"MessageTemplate"->"GeneratorThreads must be Automatic or a positive integer."|>]]];
 id=CreateUUID["generator-benchmark-"];dir=CreateDirectory[FileNameJoin[{ExpandFileName[cfg["OutputDirectory"]],id}]];
 If[!StringQ[dir],Return[Failure["OutputDirectoryFailed",<||>]]];
 CreateDirectory[FileNameJoin[{dir,"jobs"}]];Export[FileNameJoin[{dir,"owner.txt"}],id,"Text"];
 AssociateTo[$ownedRuns,id-><|"Directory"->dir,"SuccessfulDirectories"->{},"Started"->False|>];
 Print["Checking the selected FORM configuration; no rule expression is constructed in preparation."];
 {seconds,check}=measure[genCheck[cfg]];
 run=<|"ID"->id,"Suite"->"Generation","Configuration"->cfg,"Directory"->dir,
 "Environment"->environment[],"Checks"-><|"Generator"-><|"Seconds"->seconds,"Result"->check|>|>,
 "Cases"->Join[genCases[cfg["Profile"]],genHelpers[cfg["Profile"]]]|>;
 If[check===$Aborted || Lookup[check,"Status",None]==="Aborted",saveReport[run,{},"Aborted"];Return[Failure["Aborted",<|"Directory"->dir|>]]];
 saveReport[run,{},"Prepared"];run];

(* ::Section:: *)
(* Check completion and readability without comparing previous expressions. *)
genFiles[dir_,names_List] := Module[{actual,paths,values},
 actual=Sort[FileNameTake/@Select[FileNames["*",dir],FileType[#]===File&]];
 If[actual=!=Sort[names],Return[Failure["UnexpectedLibraryFiles",<|"Expected"->names,"Actual"->actual|>]]];
 paths=FileNameJoin[{dir,#}]& /@ names;values=genRead/@paths;
 If[genBad[values],Return[Failure["LibraryReadFailed",<||>]]];
 AssociationThread[names,MapThread[<|"Path"->#1,"Bytes"->FileByteCount[#1],"Fingerprint"->fingerprint[#2],"SHA256"->FileHash[#1,"SHA256","HexString"]|>&,{paths,values}]]];

(* ::Section:: *)
(* Public calls precede isolated stages. Every trial owns a fresh directory. *)
(* Redirect ordinary Print output only; Mathematica messages remain visible.
   The bar counts finished cases, not elapsed time or estimated remaining time. *)
generationRun[run_Association] := Module[{completed=0,label="Starting",stream,result,total=Length[run["Cases"]]},
 If[!KeyExistsQ[$ownedRuns,run["ID"]],Return[Failure["UnknownRun",<||>]]];
 If[TrueQ[$ownedRuns[run["ID"],"Started"]],Return[Failure["RunAlreadyStarted",<||>]]];
 stream=Quiet[Check[OpenWrite[FileNameJoin[{run["Directory"],"progress.log"}]],$Failed]];
 If[stream===$Failed,Return[Failure["ProgressLogFailed",<|"MessageTemplate"->"Could not open the benchmark progress log."|>]]];
 result=CheckAbort[
 Block[{$Output={stream},$generationUpdate=Function[{count,text},completed=count;label=text]},
 If[$FrontEnd===Null,generationRunBody[run],Monitor[generationRunBody[run],Column[{ProgressIndicator[completed,{0,Max[1,total]},ImageSize->500],
 Row[{completed," / ",total," cases completed"}],label}]]]],Close[stream];Abort[]];
 Close[stream];result];
$generationUpdate=Function[{count,text},Null];
generationRunBody[run_Association] := Module[{cfg=run["Configuration"],check=run["Checks","Generator","Result"],
 rows={},report,workers,append,record,validateFiles,mark,attempt,public,pipeline,helper,
 state,phases,phase,trial,c,spec,completedCases=0},
 If[!KeyExistsQ[$ownedRuns,run["ID"]],Return[Failure["UnknownRun",<||>]]];
 If[TrueQ[$ownedRuns[run["ID"],"Started"]],Return[Failure["RunAlreadyStarted",<||>]]];
 $ownedRuns[run["ID"],"Started"]=True;workers=If[usable[check],check["FORMThreads"],0];
 append[row_]:=(AppendTo[rows,row];report=saveReport[run,rows,"Running"]);
 record[c_,stage_,ph_,tr_,seconds_,status_,validation_,data_:<||>]:=append[Join[c["Metadata"],
 <|"Stage"->stage,"Workers"->If[c["Kind"]==="Helper",0,workers],"Phase"->ph,"Trial"->tr,
 "Seconds"->seconds,"Status"->status,"Validation"->validation|>,data]];
 mark[d_]:=($ownedRuns[run["ID"],"SuccessfulDirectories"]=Append[$ownedRuns[run["ID"],"SuccessfulDirectories"],d]);
 validateFiles[files_Association]:={"Verified"};
 SetAttributes[attempt,HoldRest];
 attempt[c_,stage_,ph_,tr_,d_,expr_] := Module[{t,v,status},
 $generationUpdate[completedCases,c["ID"]<>" / "<>ph<>" / "<>stage];
 Print[c["ID"]," / ",ph," ",tr," / ",stage];{t,v}=measure[expr];status=genStatus[v];
 record[c,stage,ph,tr,t,status,If[status==="Success","Pending","Failed"],
 <|"ArtifactDirectory"->d,"Diagnostic"->If[status==="Success","",ToString[v,InputForm]]|>];
 If[status==="Aborted",Throw["Aborted","generator-stop"]];
 If[status=!="Success",Throw[$Failed,"generator-stage"]];{v,t}];
 public[c_,ph_,tr_] := Module[{d=newJob[run,"generate"],t,v,files,valid,status},
 $generationUpdate[completedCases,c["ID"]<>" / "<>ph<>" / public generator"];
 Print[c["ID"]," / ",ph," ",tr," / public generator"];
 {t,v}=measure[genPublic[c["Command"],c["Arguments"],check,cfg,d]];status=genStatus[v];
 If[status==="Success" && v=!=Null,status="Failure"];
 files=If[status==="Success",genFiles[d,c["Metadata","Files"]],v];
 If[genBad[files],If[status==="Success",status="Failure"];valid={"Failed",False},valid=validateFiles[files]];
 record[c,"Generate",ph,tr,t,status,First[valid],<|"ArtifactDirectory"->d,"Files"->files,
 "ValidationMethod"->"Expected files present and readable",
 "Diagnostic"->If[status==="Success","",ToString[files,InputForm]]|>];
 If[status==="Aborted",Throw["Aborted","generator-stop"]];
 If[First[valid]==="Verified",mark[d]]];
 pipeline[c_,spec_,ph_,tr_] := Module[{name=genName[spec],cc,d,times={},pair,expr,job,proc,value,stage,target,valid,files,first},
 cc=Join[c,<|"Metadata"->Join[c["Metadata"],<|"Case"->name,"Library"->name|>]|>];d=newJob[run,"staged"];stage=FileNameJoin[{d,"library.staging"}];target=FileNameJoin[{d,name}];
 Catch[
 pair=attempt[cc,"Construction",ph,tr,d,genBuild[spec]];expr=First[pair];AppendTo[times,Last[pair]];
 pair=attempt[cc,"Export",ph,tr,d,CalcFormExport[expr,FileNameJoin[{d,"job.frm"}]]];job=First[pair];AppendTo[times,Last[pair]];expr=Null;
 pair=attempt[cc,"Execute",ph,tr,d,Module[{p=executeFORM[job,check["Executable"],workers,d,cfg["ExecutionTimeout"]]},
 If[AssociationQ[p]&&p["Status"]==="Completed"&&p["ExitCode"]===0&&FileExistsQ[job["ResultFile"]],p,
 Failure[If[AssociationQ[p]&&MemberQ[{"TimedOut","Aborted"},p["Status"]],p["Status"],"ExecutionFailed"],<|"Process"->p|>]]]];
 proc=First[pair];AppendTo[times,Last[pair]];
 (* Retain structured process diagnostics outside the execution timer. *)
 rows[[-1]]=Join[Last[rows],<|"Process"->proc|>];saveReport[run,rows,"Running"];
 first=!TrueQ[$importObserved];$importObserved=True;
 pair=attempt[cc,"Import",ph,tr,d,CalcFormImport[job["ResultFile"],job["MappingFile"]]];value=First[pair];AppendTo[times,Last[pair]];
 rows[[-1]]=Join[Last[rows],<|"FirstBenchmarkImportInKernel"->first,"ResultLeaves"->LeafCount[value],"ResultBytes"->FileByteCount[job["ResultFile"]]|>];
 pair=attempt[cc,"WriteVerify",ph,tr,d,genWrite[spec,value,stage]];AppendTo[times,Last[pair]];value=Null;
 pair=attempt[cc,"Publish",ph,tr,d,genPublish[stage,target]];AppendTo[times,Last[pair]];
 files=<|name-><|"Path"->target,"Bytes"->FileByteCount[target],"Fingerprint"->fingerprint[genRead[target]],"SHA256"->FileHash[target,"SHA256","HexString"]|>|>;
 valid={"Verified"};
 rows=Map[If[Lookup[#,"ArtifactDirectory",None]===d,Join[#,<|"Validation"->First[valid],"ValidationMethod"->"Successful stages and library read-back"|>],#]&,rows];
 record[cc,"StageSum",ph,tr,Total[times],"Success",First[valid],<|"ArtifactDirectory"->d,"Files"->files,"ValidationMethod"->"Successful stages and library read-back"|>];
 If[First[valid]==="Verified",mark[d]],"generator-stage"]];
 helper[c_] := Module[{reference,ph,tr,t,v,valid},

 Do[ph=If[tr===-1,"FirstObservation",If[tr===0,"Warmup","Measured"]];
 $generationUpdate[completedCases,c["ID"]<>" / "<>ph];
 Print[c["ID"]," / ",ph," ",tr];{t,v}=measure[genHelper[c]];
 valid=If[genBad[v],"Failed","Verified"];
 record[c,"Helper",ph,tr,t,genStatus[v],valid,<|"ValidationMethod"->"Successful helper evaluation"|>];
 If[genStatus[v]==="Aborted",Throw["Aborted","generator-stop"]],{tr,-1,cfg["Repetitions"]}]];
 report=saveReport[run,rows,"Running"];
 state=CheckAbort[Catch[
 Do[If[c["Kind"]==="Helper",helper[c];$generationUpdate[++completedCases,"Case finished"];Continue[]];
 If[!usable[check],record[c,"Availability","Preparation",0,Null,"Skipped","NotRun",<|"Diagnostic"->check|>];$generationUpdate[++completedCases,"Case skipped: FORM unavailable"];Continue[]];
 Do[phase=If[trial===-1,"FirstObservation",If[trial===0,"Warmup","Measured"]];public[c,phase,trial],{trial,-1,cfg["PhysicalRepetitions"]}];
 (* A successful public call already imported internally; never call the later
    staged import the first benchmark import in this session. *)
 If[AnyTrue[rows,Lookup[#,"Case",None]===c["ID"]&&Lookup[#,"Status",None]==="Success"&],$importObserved=True];
 Do[phase=If[trial===0,"Warmup","Measured"];Do[pipeline[c,spec,phase,trial],{spec,c["Specs"]}],{trial,0,cfg["PhysicalRepetitions"]}];$generationUpdate[++completedCases,"Case finished"],
 {c,run["Cases"]}];"Complete","generator-stop"],"Aborted"];
 report=saveReport[run,rows,state];Print["Reports: ",FileNameJoin[{run["Directory"],"report.json"}]];report];

BenchmarkEnvironment[r_Association] := Module[{c=r["Configuration"],e=r["Environment"],k=r["Checks","Generator","Result"]},
 StandardForm[Grid[{{"Profile",c["Profile"]},{"Wolfram",e["WolframVersion"]},{"FeynCalc",e["FeynCalcVersion"]},
 {"FORM",Lookup[k,"Version","Unavailable"]},{"Executable",Lookup[k,"Executable","Unavailable"]},
 {"Workers",Lookup[k,"FORMThreads","Unavailable"]},{"Generation repetitions",c["PhysicalRepetitions"]},
 {"Helper repetitions",c["Repetitions"]},{"FORM timeout (seconds)",c["ExecutionTimeout"]}},Alignment->Left,Frame->All]]];
BenchmarkEnvironment[_] := "No prepared benchmark.";
BenchmarkDiagnostics[r_Association] := Module[{rows=Select[r["Rows"],Lookup[#,"Status",None]=!="Success" || !MemberQ[{"Verified","Baseline"},Lookup[#,"Validation",None]]&],cols={"Case","Stage","Status","Diagnostic"}},
 If[rows==={},"No failed or inconclusive observations.",StandardForm[Grid[Prepend[Lookup[#,cols,""]& /@ rows,cols],Frame->All,Alignment->Left]]]];
BenchmarkDiagnostics[_] := "No benchmark report.";
generationPlot[r_] := Module[{s=summaryRows[r["Rows"]],groups},
 groups={{"Public command totals",Select[s,#["Stage"]==="Generate"&]},
 {"Supplementary tensors",Select[s,#["Stage"]==="Helper"&]},
 {"Individual stages",Select[s,!MemberQ[{"Generate","Helper","StageSum"},#["Stage"]]&]}};
 Column[Map[Function[g,Column[Prepend[Map[Function[part,
 BarChart[Lookup[#,{"MeanSeconds","MedianSeconds"}]& /@ part,BarOrigin->Left,
 ChartLayout->"Grouped",ChartLegends->{"Mean","Median"},
 ChartLabels->(Placed[Style[#["Case"]<>" / "<>#["Stage"],9],Before]& /@ part),
 AxesLabel->{"Wall time (seconds)",None},ImageSize->700,
 AspectRatio->Max[.3,.09 Length[part]],ImagePadding->All,PlotRangePadding->Scaled[.05]]],
 Partition[g[[2]],UpTo[8]]],Style[g[[1]],Bold]]]],Select[groups,Last[#]=!={}&]],Spacings->2]];

End[];EndPackage[];
