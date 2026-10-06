$FeynCalcStartupMessages=False;
$LoadAddOns={};
Get["FeynCalc`"];
beforeDirectory=Directory[];
processCalls=0;
SetAttributes[withoutProcesses,HoldAll];
withoutProcesses[expr_] := Block[{
 StartProcess=Function[Null,processCalls++;$Failed,HoldAll],
 RunProcess=Function[Null,processCalls++;$Failed,HoldAll]},expr];
beforeOptions=Options /@ {FeynCalc`SUNSimplify,FeynCalc`SUNTrace,FeynCalc`DiracSimplify,FeynCalc`DiracTrace};
beforeDefinitions=DownValues /@ {FeynCalc`Contract,FeynCalc`Pair,FeynCalc`FCI,FeynCalc`DiracTrace,FeynCalc`DiracSimplify,FeynCalc`SUNSimplify,FeynCalc`SUNTrace};
withoutProcesses[Get[FileNameJoin[{DirectoryName[DirectoryName[DirectoryName[$InputFileName]]],"FeynGrav.wl"}]]];
If[Directory[]=!=beforeDirectory || (DownValues /@ {FeynCalc`Contract,FeynCalc`Pair,FeynCalc`FCI,FeynCalc`DiracTrace,FeynCalc`DiracSimplify,FeynCalc`SUNSimplify,FeynCalc`SUNTrace})=!=beforeDefinitions ||
 !MemberQ[$ContextPath,"CalcFormConverter`"] || Context[CalcFormExport]=!="CalcFormConverter`" ||
 DownValues[CalcFormConverter`CalcFormExport]==={},Print["FAIL: automatic loading or namespace isolation"];Quit[1]];
beforeGravity=DownValues /@ {QuadraticGravityPropagator,QuadraticGravityVertex};
withoutProcesses[Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]],"CalcFormConverter.wl"}]]];
If[(DownValues /@ {QuadraticGravityPropagator,QuadraticGravityVertex})=!=beforeGravity ||
 Directory[]=!=beforeDirectory,Print["FAIL: independent reload changed existing definitions"];Quit[1]];
If[(Options /@ {FeynCalc`SUNSimplify,FeynCalc`SUNTrace,FeynCalc`DiracSimplify,FeynCalc`DiracTrace})=!=beforeOptions,Print["FAIL: loading changed FeynCalc options"];Quit[1]];
If[processCalls=!=0 || !AllTrue[{CalcFormCheck,CalcFormInstall,CalcFormCalculate},DownValues[#]=!={}&],
 Print["FAIL: runtime definitions or process launched during loading"];Quit[1]];
If[!AllTrue[{CalcFormConverter`ShowTiming,CalcFormConverter`ShowProgress,CalcFormConverter`FORMThreads,CalcFormConverter`DiracAlgebra,CalcFormConverter`ColourAlgebra},StringQ[MessageName[#,"usage"]]&],Print["FAIL: new option help"];Quit[1]];
Print["Automatic loading and namespace isolation passed."];
Export[FileNameJoin[{Environment["CFC_TEST_DIR"],"test-completed"}],"Loading","Text"];
Quit[];
