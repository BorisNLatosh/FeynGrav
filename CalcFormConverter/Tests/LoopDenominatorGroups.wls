(* Complete-denominator collection and incremental shared records. *)
Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
Clear[a,b,z,m,p,mu,nu];
saved=LoopTensorCoefficient[{0},{0},{m^2},{2},D];
input=(a+b)^3 saved+z LoopTensorCoefficient[{0},{0},{m^2},{1},D];
job=Block[{CalcFormConverter`Private`$lrSharedMinimumTerms=0},
 coefficientExport[input,FileNameJoin[{testDirectory,"denominator-groups.frm"}],ReduceLoopCoefficients->True]];
assert["export",AssociationQ[job]];
assert["FORM",RunProcess[{"form",job["InputFile"]}]["ExitCode"]===0];
source=Import[job["ResultFile"],"Text"];
target=FileNameJoin[{testDirectory,"framed-check.out"}];
read[t_]:=(Export[target,t,"Text"];CalcFormImport[target,job["MappingFile"]]);
reference=read[source];assert["reference",!FailureQ[reference]];
assert["independent tadpole relation",Together[reference-((a+b)^3(D-2)/(2m^2)+z)
 PaVe[0,{},{m^2},PaVeAutoOrder->False,PaVeAutoReduce->False]]===0];
Do[actual=Block[{CalcFormConverter`Private`$pgReadChunkSize=n},read[t]];
 assert["line endings and chunks "<>ToString[{n,j}],actual===reference],
 {n,{1,3,17,16384}},{j,3},{t,{{source,source<>"\n",StringReplace[source,"\n"->"\r\n"]}[[j]]}}];
Do[assert["malformed frame "<>ToString[j],FailureQ[read[t]]],
 {j,Length[bad={StringReplace[source,"CFC_END"->""],StringReplace[source,"CFC_END"->"CFC_EN"],source<>"\nx",
 StringReplace[source,"CFC_GROUP\n"->""],StringReplace[source,"CFC_ADD_SHARED 0"->"CFC_ADD_SHARED 1"]}]},
 {t,{bad[[j]]}}];
assert["retry after frame failures",read[source]===reference];
header=First[StringSplit[source,"\n"]]<>"\n";
readRecords[defs_,body_]:=read[header<>"CFC_SHARED "<>ToString[Length[defs]]<>"\n"<>
 StringRiffle[defs,";\n"]<>If[defs==={},"",";\n"]<>"CFC_GROUP_BODY\nCFC_ADD_SHARED 0\nCFC_GROUP\n(cfcTC(1,-2))*("<>body<>");\nCFC_END"];
firstResult=readRecords[{"cfcShare(1)=2","cfcShare(2)=cfcShare(1)+3"},"cfcShare(2)"];
secondResult=readRecords[{},"5"];
assert["earlier reference within a batch",firstResult===secondResult];
firstResult=readRecords[{"cfcShare(1)=2","cfcShare(2)=cfcShare(1)+3"},"cfcLRDen( cfcShare(2) )^2*cfcShare(1)^3"];
secondResult=readRecords[{},"8/25"];
assert["reference powers and denominator",firstResult===secondResult];
Do[assert["invalid definitions "<>ToString[j],FailureQ[readRecords[d,"1"]]],
 {j,Length[bad={{"cfcShare(1)=cfcShare(2)","cfcShare(2)=1"},{"cfcShare(1)=cfcShare(1)"},
 {"cfcShare(1)=unknown","cfcShare(2)=1/0"},{"cfcShare(1)=1","cfcShare(1)=2"},
 {"cfcShare(1)=0","cfcShare(2)=cfcLRDen(cfcShare(1))"}}]}, {d,{bad[[j]]}}];
assert["retry after invalid definitions",read[source]===reference];
assert["streams closed",FreeQ[Streams[],InputStream[target,_]]];
(* Two distinct original coefficients reduce to one denominator group. *)
collectionInput=a/(D-2) LoopTensorCoefficient[{0},{0},{m^2},{2},D]+
 b m^2/(D-2) LoopTensorCoefficient[{0},{0},{m^2},{3},D];
Do[collected=Block[{CalcFormConverter`Private`$lrSharedMinimumTerms=threshold},
 coefficientCalculate[collectionInput,ReduceLoopCoefficients->True,FORMThreads->workers]];
 assert["collected value "<>ToString[{threshold,workers}],!FailureQ[collected]&&
 Together[collected-(4a+(D-4)b)/(8m^2) PaVe[0,{},{m^2},PaVeAutoOrder->False,PaVeAutoReduce->False]]===0];
 assert["one mass denominator "<>ToString[{threshold,workers}],Count[collected,Power[m,-2],Infinity]===1],
 {threshold,{0,4096}},{workers,{1,2}}];
(* Capability flags are checked independently of the numerical result. *)
originalMap=Import[job["MappingFile"],"RawJSON"];
mapPath=FileNameJoin[{testDirectory,"malformed-groups.map.json"}];
Do[broken=originalMap;broken["LoopCoefficientReduction"]=Join[broken["LoopCoefficientReduction"],change];
 json=ExportString[broken,"RawJSON","Compact"->True];Export[mapPath,json,"Text"];
 Export[target,"CFC"<>ToString[broken["Version"]]<>" "<>Hash[json,"SHA256","HexString"]<>
 "\nCFC_SHARED 0\nCFC_BODY\n0","Text"];
 assertFailure["invalid denominator metadata "<>ToString[j],"InvalidMapping",CalcFormImport[target,mapPath]],
 {j,Length[changes={<|"DenominatorGroups"->"bad"|>,<|"IncrementalSharedDefinitions"->1|>,
 <|"DenominatorGroups"->False|>,<|"CoefficientBlocks"->False|>,<|"SharedCoefficientBlocks"->False|>}]},
 {change,{changes[[j]]}}];
assert["metadata failure does not poison retry",read[source]===reference];
(* A compact regression from the full coefficient: native OldFactArg in
   FORM 5.0.2 can change a scalar-product power in this mixed argument.
   Exercise the same shielded helper used by all three production passes. *)
commonFixture = "p.p^2+2/81*share(6)^2+share(1)*share(2)*share(3)*share(4)*share(5)^4*share(6)*share(7)*p.p";
commonPath = FileNameJoin[{testDirectory,"common-factor-regression.frm"}];
commonOutput = FileNameJoin[{testDirectory,"common-factor-regression.out"}];
commonSource = "Off Statistics;\nSymbols x,cfcDGPower;\nVectors p,q;\nIndices mu,nu,cfcDGIndex1,cfcDGIndex2;\nCFunctions f,g,share,cfcLRDen,cfcDGMetric;\n" <>
 "Local reference="<>commonFixture<>";\nLocal candidate=f(reference);\n.sort\nHide reference;\n.sort\n" <>
 CalcFormConverter`Private`ldgCommonFactors["f","g",{"mu","nu"}] <>
 "id f(x?)=x;\nid g(x?)=x;\n.sort\nOff OldFactArg;\nUnhide reference;\n.sort\n" <>
 "Drop reference,candidate;\nLocal residual=candidate-reference;\nLocal inverse=p.p^-3*p.q^-2;\n" <>
 CalcFormConverter`Private`ldgNegativePowerRules[{"(p.p)","(p.q)"}] <>
 ".sort\nDrop inverse;\nLocal routing=inverse-cfcLRDen(p.p)^3*cfcLRDen(p.q)^2;\n.sort\n" <>
 "#write <"<>commonOutput<>"> \"%E\",residual\n#write <"<>commonOutput<>"> \"%E\",routing\n.end\n";
Export[commonPath,commonSource,"Text"];
Do[If[FileExistsQ[commonOutput],DeleteFile[commonOutput]];
 commonRun=RunProcess[Join[engine,{commonPath}]];
 assert["native factor regression execution "<>First[engine],commonRun["ExitCode"]===0];
 assert["polynomial shielding and inverse products "<>First[engine],FileExistsQ[commonOutput]&&
  StringSplit[Import[commonOutput,"Text"]]==={"0","0"}],
 {engine,{{"form"},{"tform","-w2"}}}];
(* Public round trip includes inverse scalar products in recurrence weights. *)
Do[momentumCoefficient=((a+b)/SPD[p,p]+(a-b) SPD[p,p]^2) saved;
 momentumResult=Block[{CalcFormConverter`Private`$lrSharedMinimumTerms=threshold},
  coefficientCalculate[momentumCoefficient,ReduceLoopCoefficients->True,FORMThreads->1]];
 assert["inverse momentum multiplier "<>ToString[threshold],!FailureQ[momentumResult]&&
  Together[momentumResult-FCI[((a+b)/SPD[p,p]+(a-b) SPD[p,p]^2)(D-2)/(2m^2)
   PaVe[0,{},{m^2},PaVeAutoOrder->False,PaVeAutoReduce->False]]]===0],
 {threshold,{0,4096}}];

(* Metrics are a native FORM object whose indices are not visible to
   occurs(). Their temporary guard marker must never reach the result. *)
Do[tensorInput=(MTD[mu,nu](a+SPD[p,p]^2)+FVD[p,mu] FVD[p,nu](b+SPD[p,p])) saved;
 tensorReference=coefficientCalculate[tensorInput,ReduceLoopCoefficients->True,FORMThreads->1];
 tensorResult=Block[{CalcFormConverter`Private`$lrSharedMinimumTerms=0},
  coefficientCalculate[tensorInput,ReduceLoopCoefficients->True,FORMThreads->workers]];
 assert["free-index numerator preservation "<>ToString[workers],!FailureQ[tensorResult]&&
  Together[tensorResult-tensorReference]===0],{workers,{1,2}}];
(* Materialising the extracted bracket must preserve populated, cancelled,
   missing and tensor-bearing blocks in both engines. The production round
   trips above exercise the same boundary with actual reduced coefficients. *)
Do[materialiseRun=RunProcess[Join[engine,{FileNameJoin[{$testRoot,"Fixtures","MaterialiseBlocks.frm"}]}]];
 assert["materialised block execution "<>First[engine],materialiseRun["ExitCode"]===0];
 assert["materialised block residuals "<>First[engine],
  Length[StringCases[materialiseRun["StandardOutput"],RegularExpression["MATERIALISE_RESIDUAL[ \t]+0[ \r\n]"]]]===3],
 {engine,{{"form"},{"tform","-w1"},{"tform","-w2"}}}];
finish["Reduced denominator groups"];
