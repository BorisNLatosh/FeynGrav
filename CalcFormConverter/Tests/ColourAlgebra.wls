Get[FileNameJoin[{DirectoryName[$InputFileName], "Common.wl"}]];
$PrePrint=.;
reference[x_] := TimeConstrained[CheckAbort[
 Simplify[Contract[DiracSimplify[SUNSimplify[DotSimplify[FCI[x]] /. SUNTrace[z_,___] :> SUNTrace[z,SUNTraceEvaluate->True],SUNNToCACF->False],DiracOrder->True]]],$Aborted],60,$TimedOut];
compare[label_,x_,threads_:1,opts___] := Module[{y,r},
 y=CalcFormCalculate[x,FORMThreads->threads,opts];
 r=If[FailureQ[y],y,reference[y-x]];
 assert[label,r===0];If[r=!=0,Print[label," : ",InputForm[{y,r}]]];y
];
cases={
 {"fundamental loop",SUNFDelta[i,i]}, {"adjoint loop",SUNDelta[a,a]},
 {"fundamental delta",SUNFDelta[i,j] SUNFDelta[j,k]}, {"adjoint delta",SUNDelta[a,b] SUNDelta[b,c]},
 {"open Casimir",SUNT[a,a]}, {"sandwich",SUNT[a,b,a]},
 {"completeness",SUNTF[{a},i,j] SUNTF[{a},k,l]},
 {"ff",SUNF[a,c,d] SUNF[b,c,d]}, {"dd",SUND[a,c,d] SUND[b,c,d]}, {"fd",SUNF[a,c,d] SUND[b,c,d]},
 {"empty trace",SUNTrace[1]}, {"one trace",SUNTrace[SUNT[a]]},
 {"two trace",SUNTrace[SUNT[a,b]]}, {"three trace",SUNTrace[SUNT[a,b,c]]},
 {"four trace",SUNTrace[SUNT[a,b,c,d]]},
 {"trace power",SUNTrace[SUNT[a,b]]^2},
 {"independent traces",SUNTrace[SUNT[a,b,c]] SUNTrace[SUNT[a,b,d]]},
 {"join",SUNTF[{a,b},i,j] SUNTF[{c},j,k]}, {"close",SUNTF[{a},i,j] SUNTF[{b},j,i]},
 {"open identity branches",(1+SUNT[a]).(1+SUNT[b])},
 {"colour coefficients",(CA+CF) SUNT[a,a] SMP["g_s"]},
 {"trace scalar branches",SUNTrace[2+3 SUNT[a,b]]},
 {"indices in separate spaces",SUNTF[{a},a,b] MTD[a,b]},
 {"context identities",SUNTF[{CtxOne`a,CtxTwo`a},CtxOne`a,CtxTwo`a]},
 {"upstream names",SUNTF[{j1,j2},i1,i2] SUNTF[{j3},i2,i3]}
};
Do[Do[compare[test[[1]]<>" "<>ToString[threads],test[[2]],threads],{test,cases}],{threads,{1,2}}];
assert["CF presentation",CalcFormCalculate[SUNT[a,a],FORMThreads->1]===CF];
assert["adjoint dimension presentation",CalcFormCalculate[SUNDelta[a,a],FORMThreads->1]===2 CA CF];
long=CalcFormCalculate[SUNTrace[SUNT[a,b,c,d]],FORMThreads->1];
assert["longer trace retained",!FreeQ[long,SUNTrace]];
compare["trace cyclic",SUNTrace[SUNT[a,b,c,d]]-SUNTrace[SUNT[c,d,a,b]]];
reversal=CalcFormCalculate[SUNTrace[SUNT[a,b,c]]-SUNTrace[SUNT[c,b,a]],FORMThreads->1];
assert["reversal not identified",reference[reversal-I SUNF[a,b,c]/2]===0 && reversal=!=0];
Do[compare["mixed switches "<>ToString[{colour,dirac,workers}],
 SUNTrace[SUNT[a,b]] (GAD[mu].GAD[nu]+m GAD[nu]) LCD[mu,rho,sigma,tau],workers,ColourAlgebra->colour,DiracAlgebra->dirac],
 {workers,{1,2}},{colour,{Automatic,False}},{dirac,{Automatic,False}}];
Do[compare["preserved "<>ToString[n],cases[[n,2]],1,ColourAlgebra->False],{n,Length[cases]}];

(* Connected implicit/explicit chains lose a single implicit matrix line. *)
promoted=CalcFormCalculate[SUNT[a] SUNTF[{a},i,j]+SUNFDelta[i,j],FORMThreads->1];
endpoints=DeleteDuplicates[Cases[promoted,SUNFIndex[s_Symbol]/;StringStartsQ[Context[s],"CalcFormConverter`ColourIndices`"]:>s,Infinity]];
assert["promotion has two endpoints",Length[endpoints]===2];
If[Length[endpoints]===2,
 left=SelectFirst[endpoints,SymbolName[#]==="left"&];right=SelectFirst[endpoints,SymbolName[#]==="right"&];
 assert["promotion preserves identity branch",reference[promoted-(SUNTF[{a},left,right] SUNTF[{a},i,j]+SUNFDelta[left,right] SUNFDelta[i,j])]===0];
 assert["free endpoints remain distinct",left=!=right]
];
assert["disconnected explicit chain retains implicit word",!FreeQ[CalcFormCalculate[SUNT[a] SUNTF[{b},i,j],FORMThreads->1],SUNT]];

(* Malformed results use a real mapping and matching digest. *)
job=CalcFormExport[SUNTF[{a,b},i,j] SUNDelta[c,d],FileNameJoin[{testDirectory,"colour-typed.frm"}]];
map=Import[job["MappingFile"],"RawJSON"];
writeResult[body_,m_]:=Module[{s=ExportString[m,"RawJSON","Compact"->True]},Export[job["MappingFile"],s,"Text"];Export[job["ResultFile"],"CFC"<>ToString[m["Version"]]<>" "<>Hash[s,"SHA256","HexString"]<>"\n"<>body,"Text"]];
Do[writeResult[body,map];assert["reject result "<>body,FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {body,{"cfcCT(cfca1,cfcq1)","cfcCT(cfcq1,cfcq1,cfcq2)","d_(cfca1,cfcq1)","cfcCTr(cfca1)","cfcCTp(cfcq1,cfcq2,cfcq1,cfcq2)","cfcCF(cfca1,cfca2)","cfca1+1","cfcCT(cfca1,cfcq1,cfcq2)^-1","cfcCT(cfca1,N1_?,cfcq1)","cfcCT(cfca1,unknown,cfcq1)","cfcCTr(cfca1,cfca2,cfca3,cfca4"}}];
Do[broken=map;broken["Colour"]=Join[map["Colour"],field];writeResult["0",broken];assert["reject metadata "<>ToString[field],FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {field,{<|"Mode"->"Other"|>,<|"Flavours"->2|>,<|"TraceNormalisation"->{1,1}|>,<|"Procedure"->"Unknown"|>,<|"Rank"->"cfca1"|>,<|"IndexDictionary"->{}|>,<|"GeneratedNamespace"->"Global`"|>,<|"ImplicitEndpoints"->{"cfca1","cfca2"}|>}}];
writeResult["d_(cfcq1,cfcq2)",map];assert["cache isolation after failures",!FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
Do[assert["invalid input "<>ToString[n],FailureQ[CalcFormExport[bad[[n]],FileNameJoin[{testDirectory,"invalidcolour"<>ToString[n]<>".frm"}]]]],
 {bad,{{SUNT[1],SUNTF[{1},i,j],SUNF[a,b,1],SUNTrace[GAD[mu]],SUNTrace[SUNTrace[SUNT[a]]],SUNTrace[SUNTF[{a},i,j]],SUNT[a] SUNT[b]}}},{n,Length[bad]}];
assert["invalid option",FailureQ[CalcFormExport[SUNT[a],FileNameJoin[{testDirectory,"invalidoption.frm"}],ColourAlgebra->"invalid"]]];
(* Version selection and generated-program independence. *)
Do[vjob=CalcFormExport[spec[[1]],FileNameJoin[{testDirectory,"version"<>ToString[spec[[2]]]<>".frm"}],Sequence@@spec[[3]]];
 assert["version "<>ToString[spec[[2]]],Import[vjob["MappingFile"],"RawJSON"]["Version"]===spec[[2]]],
 {spec,{{SPD[p,q],1,{}},{LCD[a,b,c,d],2,{}},{SUNT[a,b],3,{ColourAlgebra->False}},{GAD[a].GAD[b],4,{}},{SUNTrace[SUNT[a,b]],5,{ColourAlgebra->False}}}}];
assert["embedded procedure",StringFreeQ[Import[job["InputFile"],"Text"],"#include"]];
(* Additional endpoint and native dummy boundary checks. *)
writeResult["cfcCT(cfca1,cfcq1,N1_?)*cfcCT(cfca2,N1_?,cfcq2)",map];
dummyResult=CalcFormImport[job["ResultFile"],job["MappingFile"]];
assert["typed generated dummy pair",!FailureQ[dummyResult] && Length[Cases[dummyResult,SUNFIndex[z_Symbol]/;SymbolName[z]==="d1",Infinity]]===2];
Do[writeResult[body,map];assert["wrong generated index space "<>body,FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {body,{"g_(1,N1_?)","d_(cfca1,N1_?)","N1_?","cfcGenerated1"}}];
epjob=CalcFormExport[SUNT[a],FileNameJoin[{testDirectory,"free-endpoints.frm"}]];
epmap=Import[epjob["MappingFile"],"RawJSON"];
(* Switch the writer's file paths; metadata correspondence remains exact. *)
oldjob=job;job=epjob;
Do[writeResult[body,epmap];assert["free endpoint rejection "<>body,FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {body,{"1","d_(cfcq1,cfcq1)","cfcCT(cfca1,cfcq1,cfcq1)"}}];
broken=epmap;broken["Colour"]=Join[epmap["Colour"],<|"ImplicitEndpoints"->{}|>];
writeResult["0",broken];assert["cannot erase endpoint metadata",FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
job=oldjob;
(* Serial and threaded results have the same stable free endpoint identities. *)
p1=CalcFormCalculate[SUNT[a] SUNTF[{a},i,j],FORMThreads->1];
p2=CalcFormCalculate[SUNT[a] SUNTF[{a},i,j],FORMThreads->2];
assert["stable promoted endpoints across engines",p1===p2];
assert["colour option isolation",CalcFormCalculate[SUNT[a,a],FORMThreads->1,ColourAlgebra->False]===FCI[SUNT[a,a]] && CalcFormCalculate[SUNT[a,a],FORMThreads->1]===CF];
assert["True default", (ColourAlgebra /. Options[CalcFormCalculate]) === True && (ColourAlgebra /. Options[CalcFormExport]) === True];
assert["True alias", CalcFormCalculate[SUNT[a,a], FORMThreads->1, ColourAlgebra->True] === CalcFormCalculate[SUNT[a,a], FORMThreads->1, ColourAlgebra->Automatic]];
finish["ColourAlgebra"];
