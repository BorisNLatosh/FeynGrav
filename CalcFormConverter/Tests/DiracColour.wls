Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
compare[label_, expr_, threads_:1] := Module[{actual, difference},
 actual=CalcFormCalculate[expr, FORMThreads->threads];
 difference=If[FailureQ[actual],actual,
     TimeConstrained[DiracSimplify[SUNSimplify[DotSimplify[FCI[actual-expr]]], DiracOrder -> True],30,$Failed]];
 assert[label, difference===0];
 If[difference=!=0,Print["DIFFERENCE ",label,": ",InputForm[difference]]]
];
Do[
 compare["gamma "<>ToString[t],GA[a],t];
 compare["D slash "<>ToString[t],GSD[p-q],t];
 compare["ordered "<>ToString[t],GAD[a].GAD[b].GAD[c],t];
 compare["commutator "<>ToString[t],GAD[a].GAD[b]-GAD[b].GAD[a],t];
 compare["metric "<>ToString[t],MTD[a,b] GAD[a],t];
 compare["momentum "<>ToString[t],FVD[p,a] GAD[a],t];
 compare["identity sum "<>ToString[t],(GAD[a]+m).(GAD[b]-m),t];
 compare["colour word "<>ToString[t],SUNT[a,b,c],t];
 compare["mixed spaces "<>ToString[t],SUNT[a].GAD[b].SUNT[c],t];
 compare["mixed sums "<>ToString[t],(SUNT[a]+SUNT[b]).(GAD[c]+m).SUNT[d],t];
 compare["epsilon gamma "<>ToString[t],LC[a,b,c,d] GA[a],t];
,{t,{1,2}}];
colourCases={SUNF[a,b,c],SUND[a,b,c],SUNDelta[a,b],SUNFDelta[i,j],SUNTF[{a,b},i,j],SUNTF[{a,b},i,j] SUNTF[{c},j,k],SUNN CA CF,SMP["g_s"] SUNT[a] GAD[b]};
Do[compare["colour "<>ToString[caseNumber],colourCases[[caseNumber]]],{caseNumber,Length[colourCases]}];
compare["contexts",GAD[AuditA`mu].GAD[AuditB`mu] SUNT[AuditA`mu,AuditB`mu]];
assert["word order exact",CalcFormCalculate[GAD[b].GAD[a],FORMThreads->1,DiracAlgebra->False]===FCI[GAD[b].GAD[a]]];
assert["colour order exact",CalcFormCalculate[SUNT[b,a],FORMThreads->1]===FCI[SUNT[b,a]]];
Do[assert["reject "<>ToString[i],FailureQ[CalcFormExport[bad[[i]],FileNameJoin[{testDirectory,"bad"<>ToString[i]<>".frm"}]]]],
 {bad,{{GA[5],GA[6],Spinor[Momentum[p],m],GAD[a] GAD[b],SUNT[a] SUNT[b],GAD[a]^2,GAD[a] GA[b],foo[GAD[a]],DOTunknown[a,b],DiracGamma[CartesianIndex[a]]}}}, {i,Length[bad]}];
job=CalcFormExport[GAD[a].GAD[b] SUNT[c],FileNameJoin[{testDirectory,"typed.frm"}],DiracAlgebra->False,ColourAlgebra->False];
map=Import[job["MappingFile"],"RawJSON"];
writeResult[body_,m_:map]:=Module[{j=ExportString[m,"RawJSON","Compact"->True]},
 Export[job["MappingFile"],j,"Text"];
 Export[job["ResultFile"],"CFC"<>ToString[m["Version"]]<>" "<>Hash[j,"SHA256","HexString"]<>"\n"<>body,"Text"]];
Do[writeResult[body];assert["malformed "<>body,FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {body,{"g_(2,cfi1)","g_(1,5)","g_(1)","gi_(1,cfi1)","g_(1,unknown)","g_(1,cfi1)^2","1/g_(1,cfi1)","g_(1,cfi1)*g_(1,cfi2)","cfcw1*cfcw1","g_(1,1)","g_(1,cfi1"}}];
writeResult["gi_(1)"];assert["identity",CalcFormImport[job["ResultFile"],job["MappingFile"]]===1];
writeResult["g_(1,cfi1)"];assert["cache remains usable",!FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
writeResult["0",KeyDrop[map,"DiracSpinLine"]];assertFailure["missing line","InvalidMapping",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
writeResult["0",Join[map,<|"DiracSpinLine"->2|>]];assertFailure["wrong line","InvalidMapping",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
writeResult["g_(1,cfi1)",Join[map,<|"Version"->1|>]];assert["legacy gate",FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
writeResult["0",KeyDrop[map,"EpsilonPresent"]];assertFailure["missing epsilon flag","InvalidMapping",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
writeResult["0",Join[map,<|"EpsilonPresent"->True|>]];assertFailure["missing epsilon convention","InvalidMapping",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
brokenMap=map;
brokenMap["Entries"]=Map[If[#["Kind"]==="ColourWord",Join[#,<|"Expression"->{"ColourWord",{"ColourIndexList",{"Integer","2"}}}|>],#]&,map["Entries"]];
writeResult["0",brokenMap];assertFailure["bad colour entry","InvalidMapping",CalcFormImport[job["ResultFile"],job["MappingFile"]]];
compare["commuting colour factored stages",(SPD[p,q]+SMP["g_s"] SUNF[a,b,c]) (SPD[p,r]+CA)];
compare["matrix factored coefficients",(GAD[a]+GAD[b]) (SPD[p,q]+SPD[p,r])];
DeclareNonCommutative[ncUnsupported];
assert["unknown noncommutative symbol",FailureQ[CalcFormExport[ncUnsupported.GAD[a],FileNameJoin[{testDirectory,"badNC.frm"}]]]];
UnDeclareNonCommutative[ncUnsupported];
finish["DiracColour"];
