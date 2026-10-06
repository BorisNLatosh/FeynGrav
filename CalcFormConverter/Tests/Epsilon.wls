Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
compare[label_,expr_,threads_:1] := Module[{actual,expected},
 actual=CalcFormCalculate[expr,FORMThreads->threads];
 expected=Contract[EpsEvaluate[FCI[expr]]];
 assert[label, !FailureQ[actual] && Expand[Contract[EpsEvaluate[actual-expected]]]===0]
];
Do[Block[{$LeviCivitaSign=sg},
 compare["4D square "<>ToString[{sg,t}],LC[a,b,c,d]^2,t];
 compare["D square "<>ToString[{sg,t}],LCD[a,b,c,d]^2,t];
 compare["odd epsilon "<>ToString[{sg,t}],I LC[a,b][p-q,r],t]
],{sg,{-1,1,-I,I}},{t,{1,2}}];
Do[compare["shared "<>ToString[n],Eps@@(LorentzIndex/@{a,b,c,d}) (Eps@@(LorentzIndex/@Join[Take[{a,b,c,d},n],Take[{h,j,k,l},4-n]]))],{n,0,4}];
compare["multiple pairs",LC[a,b,c,d]^2 LC[h,j,k,l]^2];
compare["metric momentum",LC[a,b,c,d] MT[a,h] FV[p,b]];
compare["repeated slot",LC[a,a,c,d]];
compare["legacy",LeviCivita[a,b][p,q]];
job=CalcFormExport[LC[a,b,c,d],FileNameJoin[{testDirectory,"epsilon.frm"}]];
json=Import[job["MappingFile"],"Text"];map=ImportString[json,"RawJSON"];
writeResult[body_,m_:map]:=Module[{j=ExportString[m,"RawJSON","Compact"->True]},
 Export[job["MappingFile"],j,"Text"];
 Export[job["ResultFile"],"CFC"<>ToString[m["Version"]]<>" "<>Hash[j,"SHA256","HexString"]<>"\n"<>body,"Text"]];
writeResult["0"];
assertFailure["changed convention even after elimination","EpsilonConventionMismatch",Block[{$LeviCivitaSign=I},CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
assert["cache isolation",CalcFormImport[job["ResultFile"],job["MappingFile"]]===0];
writeResult["e_(cfi1,cfi2,cfi3)"];
assertFailure["arity","InvalidResult",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
writeResult["e_(1,cfi2,cfi3,cfi4)"];
assertFailure["types","InvalidResult",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
writeResult["0",Join[map,<|"EpsilonConvention"-><||>|>]];
assertFailure["metadata","InvalidMapping",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
assertFailure["mixed","MixedDimensions",CalcFormExport[LC[a,b,c,d] MTD[h,j],FileNameJoin[{testDirectory,"bad.frm"}]]];
assert["numerical indices rejected",FailureQ[CalcFormExport[Eps[LorentzIndex[0],LorentzIndex[b],LorentzIndex[c],LorentzIndex[d]],FileNameJoin[{testDirectory,"bad.frm"}]]]];
writeResult["e_(unknown,cfi2,cfi3,cfi4)"];
assert["unknown identifier",FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
writeResult["e_(cfi1,cfi2,cfi3,cfi4"];
assert["malformed call",FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
writeResult["e_(cfi1,cfi2,cfi3,cfi4)",Join[map,<|"Version"->1|>]];
assertFailure["legacy rejects epsilon grammar","InvalidResult",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
assertFailure["unsupported sign","UnsupportedEpsilonConvention",Block[{$LeviCivitaSign=2},CalcFormExport[LC[a,b,c,d],FileNameJoin[{testDirectory,"bad.frm"}]]]];
assert["Cartesian rejected",FailureQ[CalcFormExport[Eps[CartesianIndex[a],CartesianIndex[b],CartesianIndex[c]],FileNameJoin[{testDirectory,"bad.frm"}]]]];
compare["D routed slots",LCD[a,b][p-q,r+p]];
compare["permutation",LC[b,a,d,c]];
compare["all momentum",LC[][p,q,r,s]];
finish["Epsilon"];
