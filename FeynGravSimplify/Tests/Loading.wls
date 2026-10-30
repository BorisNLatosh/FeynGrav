(* No external process is needed. Adapted from the donor-branch Loading.wls for
   the API-01 facade: the preferred commands live in CalcFormConverter`, option
   stores are owned and documented in FeynGravSimplify/Facade.wl, and the
   installer edges name CalcFormConverter/CalcFormConverter.wl as owner. *)
$FeynCalcStartupMessages=False;Get["FeynCalc`"];
Get[FileNameJoin[{DirectoryName[$InputFileName],"Completion.wl"}]];
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
passed=0;failed=0;
check[caseLabel_,test_]:=If[TrueQ[test],passed++,failed++;Print["FAIL: ",caseLabel]];
heads={FeynCalc`Contract,FeynCalc`Pair,FeynCalc`FCI,FeynCalc`FeynCalcInternal,FeynCalc`DiracSimplify,FeynCalc`SUNSimplify};
defs=DownValues /@ heads; options=Options /@ heads;directory=Directory[];
processCalls=0;downloadCalls=0;
Block[{StartProcess=Function[Null,processCalls++;$Failed,HoldAll],RunProcess=Function[Null,processCalls++;$Failed,HoldAll],
  URLDownload=Function[Null,downloadCalls++;$Failed,HoldAll]},
 Get[FileNameJoin[{root,"FeynGrav.wl"}]];
 Get[FileNameJoin[{root,"FeynGrav.wl"}]];
];
check["load without processes or downloads",processCalls===0&&downloadCalls===0];
check["initialized",TrueQ[FeynGravInitialized]];
check["public context",Context[FeynGravSimplify]==="CalcFormConverter`"];
check["option contexts",Context[ContractorExecutable]==="FeynGrav`"&&Context[ContractorThreads]==="FeynGrav`"];
check["usage messages",AllTrue[{FeynGravSimplify,ContractorExecutable,ContractorThreads,FeynGravFORMExport,FeynGravFORMImport,FeynGravFORMCheck,FeynGravFORMInstall},StringQ[MessageName[#,"usage"]]&]];
check["current directory",Directory[]===directory];
check["FeynCalc definitions",defs===(DownValues /@ heads)];
check["FeynCalc options",options===(Options /@ heads)];
formDefinitions=DownValues[CalcFormCalculate];formOptions=Options[CalcFormCalculate];
Get[FileNameJoin[{root,"FeynGravSimplify","Contractor.wl"}]];
check["FORM definitions independent",formDefinitions===DownValues[CalcFormCalculate]];
check["FORM options independent",formOptions===Options[CalcFormCalculate]];
Get[FileNameJoin[{root,"FeynGravSimplify","FORM.wl"}]];
check["FORM benchmark keeps converter independent",formDefinitions===DownValues[CalcFormCalculate]&&formOptions===Options[CalcFormCalculate]];
discoveryTrace=Trace[Get[FileNameJoin[{root,"FeynGravSimplify","Contractor.wl"}]],
    HoldPattern[FeynGrav`Simplification`Private`resolveExecutable[___]]];
check["no discovery on module loading",discoveryTrace==={}];
(* Command discovery in the startup list. *)
check["command list",!FreeQ[DownValues[FeynGravCommands],"FeynGravSimplify"]];
(* Facade store contents: one owner, documented defaults, FORMThreads automatic. *)
check["simplify store",Options[CalcFormConverter`FeynGravSimplify]==={
    System`Method -> Automatic, FeynGrav`ContractorExecutable -> Automatic,
    FeynGrav`ContractorThreads -> 8, System`TimeConstraint -> Infinity,
    CalcFormConverter`FORMExecutable -> Automatic, CalcFormConverter`FORMThreads -> Automatic,
    CalcFormConverter`KeepFiles -> False, CalcFormConverter`ShowProgress -> False,
    CalcFormConverter`ShowTiming -> False, CalcFormConverter`WorkingDirectory -> Automatic,
    CalcFormConverter`TensorDecomposition -> True, CalcFormConverter`ReduceLoopCoefficients -> False,
    CalcFormConverter`ReduceToScalarMasters -> True, FeynCalc`DropScaleless -> False,
    CalcFormConverter`DiracAlgebra -> Automatic, CalcFormConverter`ColourAlgebra -> True,
    FeynCalc`Dimension -> Automatic, FeynCalc`LoopMomenta -> {}, FeynCalc`PaVeToABCD -> True}];
check["export wrapper store",Options[CalcFormConverter`FeynGravFORMExport]==={
    CalcFormConverter`TensorDecomposition -> True, CalcFormConverter`ReduceLoopCoefficients -> False,
    CalcFormConverter`ReduceToScalarMasters -> True, FeynCalc`DropScaleless -> False,
    FeynCalc`Dimension -> Automatic, FeynCalc`LoopMomenta -> {}, System`OverwriteTarget -> False,
    CalcFormConverter`DiracAlgebra -> Automatic, CalcFormConverter`ColourAlgebra -> True}];
check["check wrapper store",Options[CalcFormConverter`FeynGravFORMCheck]==={
    CalcFormConverter`FORMExecutable -> Automatic, CalcFormConverter`FORMThreads -> Automatic,
    System`TimeConstraint -> 10}];
check["install wrapper store",Options[CalcFormConverter`FeynGravFORMInstall]==={
    CalcFormConverter`FORMThreads -> Automatic}];
check["import wrapper has no options",Options[CalcFormConverter`FeynGravFORMImport]==={}];
(* User SetOptions must survive an explicit second load of the package. *)
SetOptions[CalcFormConverter`FeynGravSimplify, CalcFormConverter`ShowTiming -> True];
Get[FileNameJoin[{root,"FeynGrav.wl"}]];
check["re-load preserves user options",MemberQ[Options[CalcFormConverter`FeynGravSimplify],
    CalcFormConverter`ShowTiming -> True]];
SetOptions[CalcFormConverter`FeynGravSimplify, CalcFormConverter`ShowTiming -> False];
(* Wrapper stores are independent of the owner stores. *)
SetOptions[CalcFormConverter`FeynGravFORMExport, CalcFormConverter`TensorDecomposition -> False];
check["wrapper SetOptions isolated from owner",Options[CalcFormConverter`CalcFormExport][[1]]===(CalcFormConverter`TensorDecomposition -> True)];
SetOptions[CalcFormConverter`FeynGravFORMExport, CalcFormConverter`TensorDecomposition -> True];
(* Import wrapper shape: exactly two file arguments, no options. *)
check["import extra argument",FeynGravFORMImport["a","b","c"][[1]]==="InvalidArguments"];
check["import option rejected",FeynGravFORMImport["a","b",System`Method -> "FORM"][[1]]==="InvalidArguments"];
check["import stored shape",FeynGravFORMImport[][[1]]==="InvalidArguments"];
(* Restricted capability: a non-baseline full-pipeline request is refused before any job. *)
check["capability refusal tag",FeynGravSimplify[MTD[mu,mu],System`Method -> "FORMBenchmark",
    FeynCalc`LoopMomenta -> {q}][[1]]==="MethodCapability"];
check["capability refusal two settings",FeynGravSimplify[MTD[mu,mu],System`Method -> "Contractor",
    CalcFormConverter`TensorDecomposition -> False, FeynCalc`Dimension -> 4][[1]]==="MethodCapability"];
check["unknown method rejected",FeynGravSimplify[a,System`Method -> "UnknownBackend"][[1]]==="UnsupportedMethod"];
check["facade unknown option",FeynGravSimplify[a,MadeUpOption -> 1][[1]]==="InvalidOption"];
check["facade invalid arity",FeynGravSimplify[][[1]]==="InvalidArguments"];
(* A typographical option inside a directly polluted store is still rejected;
   SetOptions itself refuses unknown names, so the pollution is assigned. *)
checkWrapperSavedStore=Options[CalcFormConverter`FeynGravFORMCheck];
Options[CalcFormConverter`FeynGravFORMCheck]=Join[checkWrapperSavedStore,{CalcFormConverter`FORMRates -> 1}];
check["stored unsupported option rejected",FeynGravFORMCheck[][[1]]==="InvalidOption"];
Options[CalcFormConverter`FeynGravFORMCheck]=checkWrapperSavedStore;
(* Archive checks read source only and must require the modules when named. *)
Get[FileNameJoin[{root,"install.m"}]];
check["installer facade edges",AllTrue[
    {"FeynGravSimplify/RestrictedCodec.wl","FeynGravSimplify/Contractor.wl","FeynGravSimplify/FORM.wl",
     "FeynGravSimplify/RestrictedCoordination.wl","FeynGravSimplify/Facade.wl"},
    MemberQ[FeynGravInstaller`Private`runtimeDependencies,{"CalcFormConverter/CalcFormConverter.wl",#}]&]];
check["installer converter companion edges",AllTrue[
    {"CalcFormConverter/MappingEncoding.wl","CalcFormConverter/ExportAssembly.wl","CalcFormConverter/ImportReconstruction.wl"},
    MemberQ[FeynGravInstaller`Private`runtimeDependencies,{"CalcFormConverter/CalcFormConverter.wl",#}]&]];
(* The facade and converter companion modules are optional additions: published
   archives predate them and must not be required unconditionally. Dependency
   detection is revision-aware. *)
check["installer modules optional",AllTrue[
    {"FeynGravSimplify/RestrictedCodec.wl","FeynGravSimplify/Contractor.wl","FeynGravSimplify/FORM.wl",
     "FeynGravSimplify/RestrictedCoordination.wl","FeynGravSimplify/Facade.wl"},
    !MemberQ[FeynGravInstaller`Private`requiredFiles,#]&]];
check["installer converter companions optional",AllTrue[
    {"CalcFormConverter/MappingEncoding.wl","CalcFormConverter/ExportAssembly.wl","CalcFormConverter/ImportReconstruction.wl"},
    !MemberQ[FeynGravInstaller`Private`requiredFiles,#]&]];
dependencyEdges=FeynGravInstaller`Private`runtimeDependencies;
check["installer dependency sources and targets exist",AllTrue[dependencyEdges,
    FileExistsQ[FileNameJoin[{root,#[[1]]}]]&&FileExistsQ[FileNameJoin[{root,#[[2]]}]]&]];
check["installer required program template",MemberQ[FeynGravInstaller`Private`requiredFiles,
    "CalcFormConverter/Templates/Program.frm.in"]];
check["installer factorisation template edge",MemberQ[dependencyEdges,
    {"CalcFormConverter/PropagatorGroups.wl","CalcFormConverter/Templates/PropagatorFactors.frm.in"}]];
layout=CreateDirectory[];
WithCleanup[Null,
 Scan[Function[name,Module[{path=FileNameJoin[{layout,name}]},
   If[!DirectoryQ[DirectoryName[path]],CreateDirectory[DirectoryName[path],CreateIntermediateDirectories->True]];
   Export[path,"","Text"]]],FeynGravInstaller`Private`requiredFiles];
 (* A pre-facade archive (converter without the guarded Get edges) must validate:
   the dependency rows stay inactive and the modules are not required. *)
 Export[FileNameJoin[{layout,"CalcFormConverter","CalcFormConverter.wl"}],
   "(* archived converter source without facade dependency edges *)","Text"];
 check["installer accepts pre-facade layout",
   Catch[FeynGravInstaller`Private`validateLayout[layout],"FeynGravInstallerFailure"]===Null];
 (* With the guarded Get edges present, the modules become required, stepwise. *)
 Export[FileNameJoin[{layout,"CalcFormConverter","CalcFormConverter.wl"}],
   "If[!NameQ[\"FeynGrav`Simplification`Private`$FacadeLoadedMark\"],Get[FileNameJoin[{$moduleDirectory,\"MappingEncoding.wl\"}]];Get[FileNameJoin[{$moduleDirectory,\"ExportAssembly.wl\"}]];Get[FileNameJoin[{$moduleDirectory,\"ImportReconstruction.wl\"}]];Get[FileNameJoin[{$moduleDirectory,\"FeynGravSimplify\",\"RestrictedCodec.wl\"}]];Get[FileNameJoin[{$moduleDirectory,\"FeynGravSimplify\",\"Contractor.wl\"}]];Get[FileNameJoin[{$moduleDirectory,\"FeynGravSimplify\",\"FORM.wl\"}]];Get[FileNameJoin[{$moduleDirectory,\"FeynGravSimplify\",\"RestrictedCoordination.wl\"}]];Get[FileNameJoin[{$moduleDirectory,\"FeynGravSimplify\",\"Facade.wl\"}]];FeynGrav`Simplification`Private`$FacadeLoadedMark = True;]","Text"];
 result=Catch[FeynGravInstaller`Private`validateLayout[layout],"FeynGravInstallerFailure"];
 check["installer rejects missing codec module",FailureQ[result]&&MemberQ[result[[2]]["MissingFiles"],"FeynGravSimplify/RestrictedCodec.wl"]];
 Export[FileNameJoin[{layout,"FeynGravSimplify","RestrictedCodec.wl"}],"","Text"];
 result=Catch[FeynGravInstaller`Private`validateLayout[layout],"FeynGravInstallerFailure"];
 check["installer rejects missing Contractor module",FailureQ[result]&&MemberQ[result[[2]]["MissingFiles"],"FeynGravSimplify/Contractor.wl"]];
 Export[FileNameJoin[{layout,"FeynGravSimplify","Contractor.wl"}],"","Text"];
 result=Catch[FeynGravInstaller`Private`validateLayout[layout],"FeynGravInstallerFailure"];
 check["installer rejects missing FORM module",FailureQ[result]&&MemberQ[result[[2]]["MissingFiles"],"FeynGravSimplify/FORM.wl"]];
 Export[FileNameJoin[{layout,"FeynGravSimplify","FORM.wl"}],"","Text"];
 result=Catch[FeynGravInstaller`Private`validateLayout[layout],"FeynGravInstallerFailure"];
 check["installer rejects missing coordination module",FailureQ[result]&&MemberQ[result[[2]]["MissingFiles"],"FeynGravSimplify/RestrictedCoordination.wl"]];
 Export[FileNameJoin[{layout,"FeynGravSimplify","RestrictedCoordination.wl"}],"","Text"];
 result=Catch[FeynGravInstaller`Private`validateLayout[layout],"FeynGravInstallerFailure"];
 check["installer rejects missing Facade module",FailureQ[result]&&MemberQ[result[[2]]["MissingFiles"],"FeynGravSimplify/Facade.wl"]];
 Export[FileNameJoin[{layout,"FeynGravSimplify","Facade.wl"}],"","Text"];
 result=Catch[FeynGravInstaller`Private`validateLayout[layout],"FeynGravInstallerFailure"];
 check["installer rejects missing mapping companion",FailureQ[result]&&MemberQ[result[[2]]["MissingFiles"],"CalcFormConverter/MappingEncoding.wl"]];
 Export[FileNameJoin[{layout,"CalcFormConverter","MappingEncoding.wl"}],"","Text"];
 result=Catch[FeynGravInstaller`Private`validateLayout[layout],"FeynGravInstallerFailure"];
 check["installer rejects missing export companion",FailureQ[result]&&MemberQ[result[[2]]["MissingFiles"],"CalcFormConverter/ExportAssembly.wl"]];
 Export[FileNameJoin[{layout,"CalcFormConverter","ExportAssembly.wl"}],"","Text"];
 result=Catch[FeynGravInstaller`Private`validateLayout[layout],"FeynGravInstallerFailure"];
 check["installer rejects missing import companion",FailureQ[result]&&MemberQ[result[[2]]["MissingFiles"],"CalcFormConverter/ImportReconstruction.wl"]];
 Export[FileNameJoin[{layout,"CalcFormConverter","ImportReconstruction.wl"}],"","Text"];
 check["installer accepts complete layout",Catch[FeynGravInstaller`Private`validateLayout[layout],"FeynGravInstallerFailure"]===Null],
 DeleteDirectory[layout,DeleteContents->True]
];
(* Loader scratch-state assertions are isolated here; behavioural recovery and
   store preservation are covered by Reliability.wls. Names is evaluated before
   the literal symbols are interned, then the current symbols are inspected while
   held. ClearAll clears values and attributes but keeps the symbols. *)
guardNames=Names["CalcFormConverter`Private`facadeGuardLoaded"];
guardLoadNames=Names["CalcFormConverter`Private`facadeLoadResults"];
check["loader scratch symbols retain no values",guardNames=!={}&&guardLoadNames=!={}&&
  Length[OwnValues[CalcFormConverter`Private`facadeGuardLoaded]]===0&&
  Length[DownValues[CalcFormConverter`Private`facadeGuardLoaded]]===0&&
  Length[UpValues[CalcFormConverter`Private`facadeGuardLoaded]]===0&&
  Attributes[CalcFormConverter`Private`facadeGuardLoaded]==={}&&
  Length[OwnValues[CalcFormConverter`Private`facadeLoadResults]]===0&&
  Length[DownValues[CalcFormConverter`Private`facadeLoadResults]]===0&&
  Length[UpValues[CalcFormConverter`Private`facadeLoadResults]]===0&&
  Attributes[CalcFormConverter`Private`facadeLoadResults]==={}];
If[guardNames==={}||guardLoadNames==={},
  Print["MISMATCH loader scratch symbol absent ",
        ToString[{guardNames,guardLoadNames},InputForm]]];
writeSuiteCompletion["Loading", passed, failed];
Print["Loading: ",passed," passed, ",failed," failed"];
Quit[If[failed===0,0,1]];
