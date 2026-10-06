Get[FileNameJoin[{DirectoryName[$InputFileName], "Common.wl"}]];
serialOptions = {FORMThreads -> 1, TimeConstraint -> 30};
equalQ[a_, b_] := !FailureQ[a] && TrueQ[TimeConstrained[
    Simplify[Contract[DiracSimplify[Contract[ExpandScalarProduct[DotSimplify[FCI[a-b]]]], DiracOrder -> True]]], 30, $Failed] === 0];
compare[label_, expr_, threads_:1] := Module[{result},
    result = CalcFormCalculate[expr, FORMThreads -> threads, TimeConstraint -> 30];
    assert[label, equalQ[result, expr]];
    If[FailureQ[result], Print[ToString[result, InputForm]]];
];
Do[
    compare["square "<>ToString[t], GAD[mu].GAD[mu], t];
    compare["sandwich "<>ToString[t], GAD[mu].GAD[nu].GAD[mu], t];
    compare["long sandwich "<>ToString[t], GAD[mu].GSD[p].GAD[nu].GSD[q].GAD[mu], t];
    compare["routed slash "<>ToString[t], GSD[2 p-q].GSD[p+q], t];
    compare["anticommutator "<>ToString[t], GAD[nu].GAD[mu]+GAD[mu].GAD[nu], t];
    compare["scalar identity "<>ToString[t], (GSD[p]+m).(GSD[p]-m), t];
    compare["trace four "<>ToString[t], DiracTrace[GA[a].GA[b].GA[c].GA[d]], t];
    compare["trace dimensional "<>ToString[t], DiracTrace[GAD[mu].GSD[p].GAD[mu].GSD[q]], t];
    compare["odd trace "<>ToString[t], DiracTrace[GAD[a].GAD[b].GAD[c]], t];
    compare["trace sum "<>ToString[t], DiracTrace[(GSD[p]+m).(GSD[q]-m)+s], t];
    compare["trace with open chain "<>ToString[t], DiracTrace[GAD[mu].GAD[nu]] GAD[mu].GSD[p], t];
    compare["shared trace indices "<>ToString[t], DiracTrace[GAD[mu].GAD[nu]] DiracTrace[GAD[mu].GAD[nu].GSD[p].GSD[q]], t];
    compare["trace square "<>ToString[t], DiracTrace[GSD[p].GSD[q]]^2, t];
    compare["trace polynomial power "<>ToString[t], (DiracTrace[GSD[p].GSD[q]]+s)^3, t];
    compare["trace times colour "<>ToString[t], DiracTrace[GSD[p].GSD[q]] SUNT[a,b], t];
    compare["epsilon trace "<>ToString[t], LCD[a,b,c,d] DiracTrace[GAD[a].GAD[b].GAD[c].GAD[d]], t];
    compare["epsilon open "<>ToString[t], LCD[a,b,c,d] GAD[d].GAD[c], t];
    compare["scalar normalisation "<>ToString[t], DiracTrace[GSD[p].GSD[q],TraceOfOne->n/2], t];
    compare["two normalisations "<>ToString[t], DiracTrace[GSD[p].GSD[q],TraceOfOne->2] DiracTrace[GSD[r].GSD[s],TraceOfOne->n], t];
, {t,{1,2}}];
assert["unit trace scalar",CalcFormCalculate[DiracTrace[z],Sequence@@serialOptions]===4 z];
assert["zero trace",CalcFormCalculate[DiracTrace[0],Sequence@@serialOptions]===0];
assert["known square",CalcFormCalculate[GAD[mu].GAD[mu],Sequence@@serialOptions]===D];
assert["known trace",CalcFormCalculate[DiracTrace[GAD[mu].GAD[mu]],Sequence@@serialOptions]===4 D];
ordered = CalcFormCalculate[GAD[z].GAD[a].GSD[q].GSD[p],Sequence@@serialOptions];
assert["canonical idempotence", CalcFormCalculate[ordered,Sequence@@serialOptions]===ordered];
assert["canonical deterministic",CalcFormCalculate[GAD[z].GAD[a].GSD[q].GSD[p],Sequence@@serialOptions]===ordered];
compare["same names in contexts",GAD[DAA`mu].GAD[DAB`mu].GAD[DAA`mu]];
compare["metric introduces new label",MTD[a,z] GAD[z].GAD[b]];
compare["component introduces slash",FVD[p,z] GAD[z].GAD[b]];
compare["polarisation slot",GSD[Polarization[p,I]].GSD[q]];
(* A bounded deterministic collection exercises length and routing independently
   of the implementation's rewrite rule order. *)
SeedRandom[7301];
Do[compare["generated "<>ToString[k], Dot@@RandomChoice[{GSD[p],GSD[q],GSD[p-q]},Mod[k,6]+1]],{k,12}];
translation=CalcFormCalculate[GAD[b].GAD[a],DiracAlgebra->False,Sequence@@serialOptions];
assert["translation order",translation===FCI[GAD[b].GAD[a]]];
held=CalcFormCalculate[DiracTrace[GSD[q].GSD[p],TraceOfOne->n],DiracAlgebra->False,Sequence@@serialOptions];
assert["trace stays unevaluated",!FreeQ[held,DiracTrace]];
assert["trace normalisation preserved",equalQ[held,DiracTrace[GSD[q].GSD[p],TraceOfOne->n]]];
held=CalcFormCalculate[DiracTrace[GSD[p].GSD[q]]^2,DiracAlgebra->False,Sequence@@serialOptions];
assert["unprocessed squared traces",equalQ[held,DiracTrace[GSD[p].GSD[q]]^2]];
Do[assert["reject unsupported "<>ToString[k],FailureQ[CalcFormExport[bad[[k]],FileNameJoin[{testDirectory,"reject"<>ToString[k]<>".frm"}]]]],
 {bad,{{DiracTrace[GAD[a],TraceOfOne->foo[x]],DiracTrace[GAD[a],Contract->False],DiracTrace[GAD[a].GA[5]],DiracTrace[GAD[a] GA[b]],DiracTrace[SUNT[a].GAD[b]],DiracTrace[GAD[a]]^-1,Spinor[Momentum[p],m],GAD[a] GAD[b]}}}, {k,Length[bad]}];
assertFailure["invalid algebra option","InvalidOption",CalcFormExport[GAD[a],FileNameJoin[{testDirectory,"badoption.frm"}],DiracAlgebra->True]];
(* Local option defaults are captured, not retained in import caches. *)
oldOptions=Options[DiracTrace];
SetOptions[DiracTrace,TraceOfOne->2];
assert["effective trace default",CalcFormCalculate[DiracTrace[GAD[mu].GAD[mu]],Sequence@@serialOptions]===2 D];
Options[DiracTrace]=oldOptions;
assert["trace default restored",CalcFormCalculate[DiracTrace[GAD[mu].GAD[mu]],Sequence@@serialOptions]===4 D];
job=CalcFormExport[DiracTrace[GAD[a].GAD[b]]+GAD[c],FileNameJoin[{testDirectory,"metadata.frm"}]];
map=Import[job["MappingFile"],"RawJSON"];
assert["new format",map["Version"]===4];
writeResult[body_,m_:map]:=Module[{j=ExportString[m,"RawJSON","Compact"->True]},
 Export[job["MappingFile"],j,"Text"];
 Export[job["ResultFile"],"CFC"<>ToString[m["Version"]]<>" "<>Hash[j,"SHA256","HexString"]<>"\n"<>body,"Text"]];
Do[writeResult[body];assertFailure["invalid result "<>body,"InvalidResult",CalcFormImport[job["ResultFile"],job["MappingFile"]]],
 {body,{"g_(2,cfi1)","g_(42,cfi1)","cfcOpen(cfi1)","g_(1,5)","g_(1,cfi1)^2"}}];
Do[writeResult["0",broken];assertFailure["invalid metadata","InvalidMapping",CalcFormImport[job["ResultFile"],job["MappingFile"]]],
 {broken,{KeyDrop[map,"TraceLines"],KeyDrop[map,"DiracAlgebra"],Join[map,<|"DiracAlgebra"->"True"|>],Join[map,<|"GammaOrdering"->{}|>],Join[map,<|"TraceLines"->{<|"Line"->1,"TraceOfOne"->{"Integer","4"}|>}|>],Join[map,<|"TraceLines"->{<|"Line"->2,"TraceOfOne"->{"Unknown","x"}|>}|>]}}];
writeResult["gi_(1)"];assert["cache isolation after failure",CalcFormImport[job["ResultFile"],job["MappingFile"]]===1];
legacy=CalcFormExport[GAD[a],FileNameJoin[{testDirectory,"legacy3.frm"}],DiracAlgebra->False];
assert["translation version three",Import[legacy["MappingFile"],"RawJSON"]["Version"]===3];
heldJob=CalcFormExport[DiracTrace[GAD[a].GAD[b]],FileNameJoin[{testDirectory,"heldmetadata.frm"}],DiracAlgebra->False];
job=heldJob; map=Import[job["MappingFile"],"RawJSON"];
Do[writeResult[body,map];assertFailure["invalid held trace "<>body,"InvalidResult",CalcFormImport[job["ResultFile"],job["MappingFile"]]],
 {body,{"g_(2,cfi1)^2","gi_(2)*gi_(2)","1/g_(2,cfi1)","g_(3,cfi1)","gi_(2,cfi1)","g_(2,5)"}}];
writeResult["g_(2,cfi1,cfi2)",map];
writeResult["g_(2,unknown)",map];assertFailure["unknown trace identifier","UnknownIdentifier",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
writeResult["g_(2,cfi1,cfi2)",map];
assert["held trace parser recovers",!FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
(* Standalone exports carry their procedures and need no include search path. *)
manualJob=CalcFormExport[DiracTrace[GSD[p].GSD[q]] GAD[a],FileNameJoin[{testDirectory,"manual.frm"}]];
manualRun=RunProcess[{CalcFormCheck[FORMThreads->1]["Executable"],manualJob["InputFile"]}];
manualResult=CalcFormImport[manualJob["ResultFile"],manualJob["MappingFile"]];
assert["manual export run import",manualRun["ExitCode"]===0 && manualResult===CalcFormCalculate[DiracTrace[GSD[p].GSD[q]] GAD[a],Sequence@@serialOptions]];
assert["embedded procedures",!StringContainsQ[Import[manualJob["InputFile"],"Text"],"#include"]];
Do[Block[{$LeviCivitaSign=sign},
 compare["epsilon convention "<>ToString[sign],LCD[a,b,c,d] LCD[a,b,c,d] DiracTrace[GSD[p].GSD[q]]]],{sign,{-1,1,-I,I}}];
epsilonJob=CalcFormExport[LCD[a,b,c,d] LCD[a,b,c,d] GAD[e],FileNameJoin[{testDirectory,"eps4.frm"}]];
epsilonRun=RunProcess[{CalcFormCheck[FORMThreads->1]["Executable"],epsilonJob["InputFile"]}];
assertFailure["version four convention gate","EpsilonConventionMismatch",Block[{$LeviCivitaSign=-$LeviCivitaSign},CalcFormImport[epsilonJob["ResultFile"],epsilonJob["MappingFile"]]]];
finish["DiracAlgebra"];
