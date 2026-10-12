Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
Clear[q,p,k,r,m,M,m0,m1,mu,nu];
reduce[x_, opts___] := coefficientCalculate[x,ReduceLoopCoefficients->True,FORMThreads->1,TimeConstraint->30,opts];
pave[ids_,inv_,ms_] := PaVe[Sequence@@ids,inv,ms,PaVeAutoOrder->False,PaVeAutoReduce->False];
lc[ids_,rs_,ms_,pw_] := LoopTensorCoefficient[ids,rs,ms,pw,D];
sp=FCI[SPD[p]];
assert["default unchanged",(ReduceLoopCoefficients/.Options[CalcFormCalculate])===False];
x=lc[{0},{0},{m^2},{2}];
assert["disabled saved coefficient",coefficientCalculate[x,FORMThreads->1]===x];
assert["saved tadpole derivative",Together[reduce[x]-(D-2)/(2m^2) pave[{0},{},{m^2}]]===0];
assert["integral normalisation",Together[reduce[FAD[{q,m,2}],LoopMomenta->{q}]-I Pi^2(D-2)/(2m^2)pave[{0},{},{m^2}]]===0];
assert["saved vacuum dropped",reduce[lc[{0},{},{},{}],DropScaleless->True]===0];
assert["saved vacuum retained",reduce[lc[{0,0},{},{},{}]]===lc[{0,0},{},{},{}]];
assert["unrelated constant not integrated",reduce[7+x]-reduce[x]===7];
assert["no implicit integration",reduce[SPD[q]]===FCI[SPD[q]]];
assert["scaleless retained",reduce[lc[{0},{0},{0},{2}]]===lc[{0},{0},{0},{2}]];
assert["scaleless dropped",reduce[lc[{0},{0},{0},{2}],DropScaleless->True]===0];
assert["explicit dimension retained",reduce[LoopTensorCoefficient[{0},{0},{m^2},{2},4]]===LoopTensorCoefficient[{0},{0},{m^2},{2},4]];
assert["coincident slots combined",Together[reduce[lc[{0},{0,0},{m^2,m^2},{1,1}]]-reduce[x]]===0];
assert["coefficient products",Together[reduce[(2+x)^2]-(2+reduce[x])^2]===0];
assert["coefficient round trip",Together[coefficientCalculate[reduce[x],FORMThreads->1]-reduce[x]]===0];
assert["rational routing",Together[reduce[FAD[{2q,m,2}],LoopMomenta->{q}]-I Pi^2/16 reduce[lc[{0},{0},{m^2/4},{2}]]]===0];
fourDimensional=ChangeDimension[FCI[FAD[{q,m,3}]],4];
assert["integer dimension avoids zero times UV pole",coefficientCalculate[fourDimensional,LoopMomenta->{q},ReduceLoopCoefficients->True,FORMThreads->1]===
 coefficientCalculate[fourDimensional,LoopMomenta->{q},FORMThreads->1]];
(* Independent analytic vacuum moments, including the metric factors. *)
vacuum[power_,s_,mass_] := (-1)^(power-s)Pi^(D/2-2)Gamma[power-s-D/2]mass^(D/2+s-power)/(2^s Gamma[power]);
Do[result=reduce[lc[ConstantArray[1,rank],{0,0},{m^2,m^2},{1,1}]];
 actual=result/.PaVe[a___]:>With[{z=DeleteCases[{a},_Rule]},vacuum[1,If[Drop[z,-2]==={0},0,Length[Drop[z,-2]]/2],First[z[[-1]]]]];
 assert["coincident labelled moments "<>ToString[rank],FullSimplify[FunctionExpand[
 actual-(-1)^rank/(rank+1)vacuum[2,0,m^2]],m>0]===0],{rank,1,3}];

Do[
 ids=If[s===0,{0},ConstantArray[0,2s]];
 result=reduce[lc[ids,{0},{m^2},{power}]];
 actual=result/.PaVe[a___]:>With[{z=DeleteCases[{a},_Rule]},vacuum[1,If[Drop[z,-2]==={0},0,Length[Drop[z,-2]]/2],First[z[[-1]]]]];
 assert["vacuum powers and rank "<>ToString[{power,s}],FullSimplify[FunctionExpand[actual-vacuum[power,s,m^2]],m>0]===0],
 {power,2,4},{s,0,3}];
(* Independent Euclidean-region Feynman-parameter quadrature, not the IBP
   recurrence. The Gamma prefactor gives dimensional analytic continuation of
   the unit-power reference coefficients. No global FeynCalc options change. *)
parameterValue[ids_,inv_,ms_,pw_,d_:3] := Module[{s,labels,n=Length[ms],total=Total[pw],pref,x},
 s=If[ids==={0},0,Count[ids,0]/2];labels=If[ids==={0},{},DeleteCases[ids,0]];
 pref=Pi^(d/2-2)(-1)^(total-s+Length[labels])Gamma[total-s-d/2]/(2^s Times@@(Gamma/@pw));
 If[n===1,Return[pref ms[[1]]^(d/2+s-total)]];
 pref NIntegrate[(1-x)^(pw[[1]]-1)x^(pw[[2]]-1+Length[labels])
   ((1-x)ms[[1]]+x ms[[2]]-x(1-x)inv[[1]])^(d/2+s-total),{x,0,1},AccuracyGoal->9]
];
numerical[x_] := N[x/.{Pair[Momentum[p,D],Momentum[p,D]]->-2,m0->2,m1->3,M->2,D->3}/.
 PaVe[a___]:>With[{z=DeleteCases[{a},_Rule]},parameterValue[Drop[z,-2],z[[-2]],z[[-1]],ConstantArray[1,Length[z[[-1]]]]]]];
Do[result=reduce[lc[ids,{0,p},{m0^2,m1^2},powers]];
 assert["bubble parameter integral "<>ToString[{ids,powers}],!FailureQ[result] && FreeQ[result,_LoopTensorCoefficient] &&
 Abs[numerical[result]-parameterValue[ids,{-2},{4,9},powers]]<10^-9],
 {ids,{{0},{0,0},{1},{1,1},{0,0,1}}},{powers,{{2,1},{1,2},{2,2},{3,1}}}];
user=lc[{0,0},{0,-p,-p},{0,0,M^2},{1,2,1}];
result=reduce[user,DropScaleless->True];
expected=((-1/M^4)+(D-1)/(M^2 sp))pave[{0,0},{sp},{0,0}]+pave[{0,0},{sp},{0,M^2}]/M^4;
assert["user coefficient explicit reduction",Together[result-expected]===0];
assert["user scaleless residue retained",!FreeQ[reduce[user],_LoopTensorCoefficient]];
assert["user TFORM agreement",Together[result-coefficientCalculate[user,ReduceLoopCoefficients->True,DropScaleless->True,FORMThreads->2]]===0];
(* Nonzero mass at threshold: Cayley determinant zero, so no singular division. *)
Internal`InheritedBlock[{Pair},Pair[Momentum[p,D],Momentum[p,D]]=4m^2;
 result=reduce[lc[{0},{0,p},{m^2,m^2},{2,1}]];
 assert["singular family preserved",result===lc[{0},{0,p},{m^2,m^2},{2,1}]]];
Internal`InheritedBlock[{Pair},Pair[Momentum[p,D],Momentum[p,D]]=0;
 assert["null family retained",!FreeQ[reduce[lc[{0},{0,p},{0,0},{2,1}]],_LoopTensorCoefficient]];
 assert["null family dropped",reduce[lc[{0},{0,p},{0,0},{2,1}],DropScaleless->True]===0]];
assertFailure["invalid option","InvalidOption",coefficientExport[x,FileNameJoin[{testDirectory,"bad.frm"}],ReduceLoopCoefficients->Automatic]];
assertFailure["invalid boolean option","InvalidOption",coefficientCalculate[True,ReduceLoopCoefficients->Automatic]];
assert["disabled decomposition rejected with declared loop",FailureQ[coefficientExport[FAD[{q,m}],FileNameJoin[{testDirectory,"bad-mode.frm"}],LoopMomenta->{q},TensorDecomposition->False,ReduceLoopCoefficients->True]]];
(* Malformed metadata/results and incremental reader boundaries. *)
job=coefficientExport[x,FileNameJoin[{testDirectory,"coefficient.frm"}],ReduceLoopCoefficients->True,OverwriteTarget->True];
run=RunProcess[{"form",job["InputFile"]}];
assert["manual execution",run["ExitCode"]===0];
expected=reduce[x];
Do[assert["chunk boundary "<>ToString[size],Block[{CalcFormConverter`Private`$pgReadChunkSize=size},CalcFormImport[job["ResultFile"],job["MappingFile"]]]===expected],{size,{1,3,17}}];
originalMap=Import[job["MappingFile"],"RawJSON"];
writeResult[body_,map_] := Module[{json=ExportString[map,"RawJSON","Compact"->True]},Export[job["MappingFile"],json,"Text"];Export[job["ResultFile"],"CFC"<>ToString[map["Version"]]<>" "<>Hash[json,"SHA256","HexString"]<>"\n"<>If[TrueQ[Lookup[Lookup[map,"LoopCoefficientReduction",<||>],"SharedCoefficientBlocks",False]],"CFC_SHARED 0\nCFC_BODY\n", ""]<>body,"Text"]];
Do[writeResult[bad,originalMap];assert["reject "<>bad,FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {bad,{"cfcIR(1,-2)","cfcLRKey(1)","cfcLRReduced(1)","cfcTC(1,-2,1)","cfcTC(2,-2", "unknown", "cfcLRReduced()", "cfcLRReduced(1,2)", "cfcLRReduced(cfcTC(1,-2)^2)", "cfcLRReducedPart(unknown)"}}];
Do[broken=originalMap;broken["TensorDecomposition"]=Join[broken["TensorDecomposition"],bad];writeResult["0",broken];
 assert["invalid interpretation "<>ToString[bad],FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {bad,{<|"Mode"->"Wrong"|>,<|"LoopMomentum"->{"Symbol","Global`q"}|>,<|"Families"->{}|>}}];
writeResult["0",KeyDrop[originalMap,"LoopCoefficientReduction"]];
assert["required reduction metadata",FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
assert["streams closed",FreeQ[Streams[],_InputStream?(StringContainsQ[First[#],"coefficient.out"]&)]];
broken=originalMap;broken["Version"]=6;writeResult["0",broken];
assert["saved mode cannot masquerade as integral mode",FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
assert["saved reduction version seven",originalMap["Version"]===7];
broken=originalMap;broken["LoopCoefficientReduction"]=KeyDrop[broken["LoopCoefficientReduction"],{"CoefficientBlocks","SharedCoefficientBlocks","DenominatorGroups","IncrementalSharedDefinitions"}];
writeResult["cfcTC(1,-2)",broken];
assert["legacy reduction without coefficient blocks",!FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
writeResult["cfcLRReduced(2)*cfcTC(1,-2)",broken];
assert["coefficient blocks require metadata",FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
broken=originalMap;broken["LoopCoefficientReduction"]=Join[broken["LoopCoefficientReduction"],<|"CoefficientBlocks"->"bad"|>];
writeResult["0",broken];assert["invalid coefficient block metadata",FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]];
writeResult["cfcLRReducedPart(2*cfcTC(1,-2))",originalMap];
assert["split coefficient blocks",CalcFormImport[job["ResultFile"],job["MappingFile"]]===2 pave[{0},{},{m^2}]];
writeResult["+(cfcLRReduced(2*cfcTC(1,-2))+cfcLRReducedPart(3*cfcTC(1,-2)))*(2+cfs1)",originalMap];
assert["split blocks remain additive",Together[CalcFormImport[job["ResultFile"],job["MappingFile"]]-5(2+D)pave[{0},{},{m^2}]]===0];

(* Frozen invariants reject changed kinematics before inverse coefficients
   are decoded, including a newly singular point. A later fresh job is isolated. *)
job2=coefficientExport[lc[{0},{0,p},{0,0},{2,1}],FileNameJoin[{testDirectory,"frozen.frm"}],ReduceLoopCoefficients->True,OverwriteTarget->True];
assert["frozen execution",RunProcess[{"form",job2["InputFile"]}]["ExitCode"]===0];
Internal`InheritedBlock[{Pair},Pair[Momentum[p,D],Momentum[p,D]]=0;
 assertFailure["changed kinematics","CoefficientConventionMismatch",CalcFormImport[job2["ResultFile"],job2["MappingFile"]]]];
assert["unchanged subsequent import",!FailureQ[CalcFormImport[job2["ResultFile"],job2["MappingFile"]]]];
Do[
 input=(FAD[{p,M}]+FAD[{p,m}]) lc[{0},{0},{m^2},{2}] LCD[mu,nu,rho,sigma] (GAD[alpha].GAD[beta]) SUNDelta[a,b];
 actual=coefficientCalculate[input,ReduceLoopCoefficients->True,DiracAlgebra->da,ColourAlgebra->ca,FORMThreads->1];
 reference=coefficientCalculate[input,ReduceLoopCoefficients->True,DiracAlgebra->da,ColourAlgebra->ca,FORMThreads->2];
 assert["combined algebra modes "<>ToString[{da,ca}],!FailureQ[actual]&&actual===reference],
 {da,{Automatic,False}},{ca,{True,False}}];
(* Compare the isolated lookup path with the same recurrence applied directly.
   These checks exercise repeated external prefactors, products and zero keys. *)
Clear[z,a,b];
isolatedCases = {
  0, 3 + x, (FAD[{p,M}] + FAD[{p,m}]) x, (1 + z + z^2)^4 x + (a + b)^3 x,
  (1 + SPD[p]) lc[{1,1},{0,p},{m^2,M^2},{2,1}],
  FVD[p,mu] x + MTD[mu,nu] lc[{0,0},{0},{m^2},{3}],
  x^2 + 3 x lc[{0,0},{0},{m^2},{3}]
};
Do[
  actual = reduce[test];
  reference = Internal`InheritedBlock[{CalcFormConverter`Private`lrIsolatedProcessing},
    Clear[CalcFormConverter`Private`lrIsolatedProcessing];
    CalcFormConverter`Private`lrIsolatedProcessing[] := "#$cfcLRNonlinear=1;\n#call cfcLRReduce\n";
    reduce[test]];
  assert["isolated recurrence equivalence "<>ToString[j],
    !FailureQ[actual] && !FailureQ[reference] && Together[actual-reference]===0],
  {j,Length[isolatedCases]}, {test,{isolatedCases[[j]]}}];
assert["integral spectator sums", Together[reduce[FAD[{q,m,2}](FAD[{p,M}]+FAD[{p,m}]),LoopMomenta->{q}] -
 I Pi^2(D-2)/(2m^2)pave[{0},{},{m^2}] FCI[FAD[{p,M}]+FAD[{p,m}]]]===0];
(* Final collection must combine different original coefficients and routings. *)
finalCollected=reduce[x+lc[{0},{0},{m^2},{1}]];
assert["one final PaVe occurrence",Length[Cases[finalCollected,_PaVe,{0,Infinity}]]===1];
assert["collected multiplier",Together[finalCollected-(1+(D-2)/(2m^2))pave[{0},{},{m^2}]]===0];
assert["native and mapped mass factors cancel",reduce[2m^2 x-(D-2)lc[{0},{0},{m^2},{1}]]===0];
shifted=reduce[FAD[{q,m}]+FAD[{q+p,m}],LoopMomenta->{q}];
assert["shifted equal tadpoles",shifted===2 I Pi^2 pave[{0},{},{m^2}]];
reflected=reduce[lc[{0,0,1},{0,p},{m^2,M^2},{1,1}]+lc[{0,0,1},{0,-p},{m^2,M^2},{1,1}]];
assert["equal invariants and relative labels",reflected===2pave[{0,0,1},{sp},{m^2,M^2}]];
assert["zero combined multiplier",reduce[x-x]===0];
(* Force the shared-block path on manageable inputs; production only uses it
   for large scalar-algebra jobs. Algebraic checks remain outside timings. *)
Do[reference=reduce[test];
 actual=Block[{CalcFormConverter`Private`$lrSharedMinimumTerms=0},reduce[test]];
 assert["shared coefficient equivalence "<>ToString[j],!FailureQ[actual] && Together[actual-reference]===0],
 {j,Length[isolatedCases]},{test,{isolatedCases[[j]]}}];
sharedInput=(a+b)^3 x+z lc[{0},{0},{m^2},{1}];
job=Block[{CalcFormConverter`Private`$lrSharedMinimumTerms=0},
 coefficientExport[sharedInput,FileNameJoin[{testDirectory,"shared.frm"}],ReduceLoopCoefficients->True,OverwriteTarget->True]];
assert["shared FORM success",RunProcess[{"form",job["InputFile"]}]["ExitCode"]===0];
expected=reduce[sharedInput];sharedText=Import[job["ResultFile"],"Text"];
Do[actual=Block[{CalcFormConverter`Private`$pgReadChunkSize=size},CalcFormImport[job["ResultFile"],job["MappingFile"]]];
 assert["shared reader chunk "<>ToString[size],!FailureQ[actual] && Together[actual-expected]===0],{size,{1,3,17}}];
assert["shared TFORM equality",Together[Block[{CalcFormConverter`Private`$lrSharedMinimumTerms=0},
 coefficientCalculate[sharedInput,ReduceLoopCoefficients->True,FORMThreads->2]]-expected]===0];
map=Import[job["MappingFile"],"RawJSON"];
header=First[StringSplit[sharedText,"\n"]]<>"\n";
Do[Export[job["ResultFile"],header<>bad,"Text"];
 assert["invalid shared block "<>ToString[j],FailureQ[CalcFormImport[job["ResultFile"],job["MappingFile"]]]],
 {j,Length[badShared={"CFC_SHARED -1\nCFC_BODY\n0", "CFC_SHARED 1\ncfcShare(1)=1",
 "CFC_SHARED 2\ncfcShare(2)=1;\ncfcShare(2)=2;\nCFC_BODY\n0", "CFC_SHARED 1\ncfcShare(1)=unknown;\nCFC_BODY\n0",
 "CFC_SHARED 1\ncfcShare(1)=cfcShare(1);\nCFC_BODY\n0", "CFC_SHARED 1\ncfcShare(1)=cfcTC(1,-2);\nCFC_BODY\n0",
 "CFC_SHARED 0\nCFC_BODY\ncfcShare(1)", "CFC_SHARED 1\ncfcShare(1)=1;\nCFC_BODY\ncfcShare(1,2)",
 "CFC_SHARED 1\ncfcShare(1)=1;\nCFC_BODY\ncfcShare(-1)", "CFC_SHARED 0\n0"}]}, {bad,{badShared[[j]]}}];
assert["shared streams closed",FreeQ[Streams[],_InputStream?(StringContainsQ[First[#],"shared.out"]&)]];
Export[job["ResultFile"],sharedText,"Text"];
retry=CalcFormImport[job["ResultFile"],job["MappingFile"]];
assert["shared failed imports do not poison retry",Together[retry-expected]===0];
finish["Loop coefficient reduction"];
