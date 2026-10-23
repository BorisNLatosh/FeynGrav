Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
Clear[p,q,m,M,a,b,s];
pv[ids_,inv_,ms_] := PaVe[Sequence@@ids,inv,ms,PaVeAutoOrder->False,PaVeAutoReduce->False];
(* The reference changes only the generated output-selection threshold back to
   its previous value. Recurrences, kinematics and the mapping remain identical. *)
manual[expression_,label_,direct_:False,opts___] := Module[{job,program,reference,process,result,text},
 job=CalcFormExport[expression,FileNameJoin[{testDirectory,label<>".frm"}],opts];
 assert[label<>" export",AssociationQ[job]];If[!AssociationQ[job],Return[$Failed]];
 If[direct,program=Import[job["InputFile"],"Text"];
  reference=StringReplace[program,"#if termsin(cfcResult) > 0\nCollect cfcShared"->"#if termsin(cfcResult) > 4096\nCollect cfcShared"];
  assert[label<>" reference selector",reference=!=program];Export[job["InputFile"],reference,"Text"]];
 process=RunProcess[{"form",job["InputFile"]}];
 Export[FileNameJoin[{testDirectory,label<>".stdout"}],process["StandardOutput"],"Text"];
 Export[FileNameJoin[{testDirectory,label<>".stderr"}],process["StandardError"],"Text"];
 assert[label<>" execution",process["ExitCode"]===0&&process["StandardError"]===""];
 If[process["ExitCode"]=!=0,Return[$Failed]];
 text=Import[job["ResultFile"],"Text"];
 result=CalcFormImport[job["ResultFile"],job["MappingFile"]];
 assert[label<>" import",!FailureQ[result]];
 {result,text,job}
];
inputs={pv[{0,0},{s},{a,b}],
 (a+b)^3 pv[{1,1,1,1},{-2,-13/3,-38/5,-5,-3,-13/2},{2,3,5,7}],
 pv[{1,1,1},{-2,-3,-4},{2,3,5}]+pv[{0,0},{-2},{2,3}],
 LoopTensorCoefficient[{0,0},{0,p},{m^2,M^2},{2,1},D]};
Do[candidate=manual[inputs[[j]],"shared-"<>ToString[j]];
 reference=manual[inputs[[j]],"direct-"<>ToString[j],True];
 assert["shared framing "<>ToString[j],StringContainsQ[candidate[[2]],"CFC_GROUP_BODY"]];
 assert["legacy framing "<>ToString[j],StringContainsQ[reference[[2]],"CFC_SHARED 0\nCFC_BODY"]];
 assert["exact output equality "<>ToString[j],Together[candidate[[1]]-reference[[1]]]===0];
 assert["mapping unchanged "<>ToString[j],Import[candidate[[3,"MappingFile"]],"RawJSON"]===Import[reference[[3,"MappingFile"]],"RawJSON"]],{j,Length[inputs]}];
assert["scalar-master sharing independent of input threshold",StringContainsQ[
 Block[{CalcFormConverter`Private`$smEnabled=True,CalcFormConverter`Private`$lrSharedMinimumTerms=99999},
 CalcFormConverter`Private`lrSharedPreparation[<|"SharedBlocks"->True|>]],"#if termsin(cfcResult) > 0"]];
assert["power-only threshold preserved",StringContainsQ[
 Block[{CalcFormConverter`Private`$smEnabled=False,CalcFormConverter`Private`$lrSharedMinimumTerms=99999},
 CalcFormConverter`Private`lrSharedPreparation[<|"SharedBlocks"->True|>]],"#if termsin(cfcResult) > 99999"]];
assert["algebra eligibility preserved",Block[{CalcFormConverter`Private`$smEnabled=True},CalcFormConverter`Private`lrSharedPreparation[<|"SharedBlocks"->False|>]]===""];
assert["zero result",CalcFormCalculate[0]===0];
finish["Scalar master output sharing"];
