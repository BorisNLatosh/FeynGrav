(* Shared test support. Case identity never depends on insertion order. *)
$FeynCalcStartupMessages=False;
$testRoot=DirectoryName[$InputFileName];
Get[FileNameJoin[{DirectoryName[$testRoot],"CalcFormConverter.wl"}]];
testDirectory=Environment["CFC_TEST_DIR"];
failures=0; checks=0; cases=<||>;
assert[label_,value_] := (checks++; If[!TrueQ[value],failures++;Print["FAIL: ",label]]);
assertFailure[label_,tag_String,value_] := Module[{},
 assert[label,MatchQ[value,Failure[tag,_Association]]];
 If[!MatchQ[value,Failure[tag,_Association]],Print["Expected Failure[",tag,"]; received: ",InputForm[value]]]
];
add[label_,expr_,opts___] := Module[{job},
 job=CalcFormExport[expr,FileNameJoin[{testDirectory,label<>".frm"}],opts];
 assert[label<>" export",AssociationQ[job]];
 If[AssociationQ[job],AssociateTo[cases,label-><|"Expected"->FCI[expr],"Job"->job|>],Print[InputForm[job]]]
];
saveCases[] := Block[{$ContextPath={"System`"}},Put[cases,FileNameJoin[{testDirectory,"cases.wl"}]]];
finish[label_] := (Print[label,": ",checks," assertions; ",failures," failures."];Export[FileNameJoin[{testDirectory,"test-completed"}],label,"Text"];Quit[If[failures>0,1,0]]);
