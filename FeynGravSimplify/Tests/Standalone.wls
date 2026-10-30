(* Separate fresh kernel: standalone converter loading only (no FeynGrav.wl).
   Adapted from the donor-branch loading strategy for the API-01 facade. *)
$FeynCalcStartupMessages=False;Get["FeynCalc`"];
Get[FileNameJoin[{DirectoryName[$InputFileName],"Completion.wl"}]];
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
passed=0;failed=0;
check[caseLabel_,test_]:=If[TrueQ[test],passed++,failed++;Print["FAIL: ",caseLabel]];
converter=FileNameJoin[{root,"CalcFormConverter","CalcFormConverter.wl"}];
directory=Directory[];
processCalls=0;
Block[{StartProcess=Function[Null,processCalls++;$Failed,HoldAll],RunProcess=Function[Null,processCalls++;$Failed,HoldAll]},
 Get[converter];
 Get[converter];
];
check["load without processes",processCalls===0];
check["current directory",Directory[]===directory];
check["main package not loaded",!NameQ["FeynGrav`FeynGravInitialized"]];
check["numeric short names not exposed",!MemberQ[$ContextPath,"FeynGrav`"]];
check["facade context",Context[CalcFormConverter`FeynGravSimplify]==="CalcFormConverter`"];
check["facade usage message",StringQ[MessageName[CalcFormConverter`FeynGravSimplify,"usage"]]];
check["contractor option symbols declared",Context[FeynGrav`ContractorExecutable]==="FeynGrav`"&&
    Context[FeynGrav`ContractorThreads]==="FeynGrav`"];
check["contractor usage strings",AllTrue[{FeynGrav`ContractorExecutable,FeynGrav`ContractorThreads},
    StringQ[MessageName[#,"usage"]]&]];
check["dependent behaviours present",StringQ[MessageName[CalcFormConverter`CalcFormCalculate,"usage"]]];
check["automatic store default",Options[CalcFormConverter`FeynGravSimplify][[6]]===(CalcFormConverter`FORMThreads -> Automatic)];
check["contractor threads default",Options[CalcFormConverter`FeynGravSimplify][[3]]===(FeynGrav`ContractorThreads -> 8)];
check["method default",Options[CalcFormConverter`FeynGravSimplify][[1]]===(System`Method -> Automatic)];
SetOptions[CalcFormConverter`FeynGravSimplify, CalcFormConverter`KeepFiles -> True];
Get[converter];
check["re-load preserves user options",MemberQ[Options[CalcFormConverter`FeynGravSimplify],
    CalcFormConverter`KeepFiles -> True]];
SetOptions[CalcFormConverter`FeynGravSimplify, CalcFormConverter`KeepFiles -> False];
(* Restricted capability refusal works without any main-package loading. *)
check["capability refusal",CalcFormConverter`FeynGravSimplify[MTD[mu,mu],System`Method -> "Contractor",
    FeynCalc`LoopMomenta -> {q}][[1]]==="MethodCapability"];
check["wrapper unknown option",CalcFormConverter`FeynGravFORMExport[MTD[mu,mu],"unused.file",
    System`Method -> "FORM"][[1]]==="InvalidOption"];
check["four option stores",Length[Options[CalcFormConverter`FeynGravSimplify]]===19&&
  Length[Options[CalcFormConverter`FeynGravFORMExport]]===9&&
  Length[Options[CalcFormConverter`FeynGravFORMCheck]]===3&&
  Length[Options[CalcFormConverter`FeynGravFORMInstall]]===1];
check["import has no options",Options[CalcFormConverter`FeynGravFORMImport]==={}];
check["names match defaults",AllTrue[
  {{CalcFormConverter`FeynGravSimplify,FeynGrav`Simplification`Private`$simplifyOptionDefaults,
     FeynGrav`Simplification`Private`$simplifyOptionNames},
   {CalcFormConverter`FeynGravFORMCheck,FeynGrav`Simplification`Private`$checkWrapperDefaults,
     FeynGrav`Simplification`Private`$checkWrapperOptionNames},
   {CalcFormConverter`FeynGravFORMInstall,FeynGrav`Simplification`Private`$installWrapperDefaults,
     FeynGrav`Simplification`Private`$installWrapperOptionNames}},
  Sort[#[[3]]]===Sort[First /@ #[[2]]]&]];
check["export names cover store",
  Sort[FeynGrav`Simplification`Private`$exportWrapperOptionNames]===
    Sort[First /@ FeynGrav`Simplification`Private`$exportWrapperDefaults]];
ownerCheckStore=Options[CalcFormConverter`CalcFormCheck];
SetOptions[CalcFormConverter`FeynGravFORMCheck,System`TimeConstraint -> 7];
check["standalone store isolation",(System`TimeConstraint /. Options[CalcFormConverter`FeynGravFORMCheck])===7&&
  Options[CalcFormConverter`CalcFormCheck]===ownerCheckStore];
SetOptions[CalcFormConverter`FeynGravFORMCheck,System`TimeConstraint -> 10];
writeSuiteCompletion["Standalone", passed, failed];
Print["Standalone: ",passed," passed, ",failed," failed"];
Quit[If[failed===0,0,1]];
