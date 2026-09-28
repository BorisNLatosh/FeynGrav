Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
(* No FORM process or FeynGrav library is required by this suite. *)
add["exact",-7/13+2 I/3];
add["trace_D",MTD[mu,nu] MTD[mu,nu]];
assertFailure["overwrite refused", "FileExists", CalcFormExport[1,FileNameJoin[{testDirectory,"exact.frm"}]]];
assertFailure["unsupported function", "UnsupportedExpression", CalcFormExport[f[x],FileNameJoin[{testDirectory,"bad.frm"}]]];
assert["no files for unsupported expression", !FileExistsQ[FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["Dirac rejected", "UnsupportedExpression", CalcFormExport[GAD[mu],FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["color rejected", "UnsupportedExpression", CalcFormExport[SUNF[a,b,c],FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["epsilon rejected", "UnsupportedExpression", CalcFormExport[LC[mu,nu,a,b],FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["mixed dimensions rejected", "MixedDimensions", CalcFormExport[MTD[mu,nu]+MT[a,b],FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["dimension mismatch rejected", "DimensionMismatch", CalcFormExport[MTD[mu,nu],FileNameJoin[{testDirectory,"bad.frm"}],Dimension->4]];
assertFailure["inexact number rejected", "UnsupportedExpression", CalcFormExport[0.25,FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["unsupported propagator rejected", "UnsupportedDenominator", CalcFormExport[SFAD[{l,m^2}],FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["nonlinear routing rejected", "UnsupportedMomentum", CalcFormExport[FVD[p l,mu],FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["symbolic routing coefficient rejected", "UnsupportedMomentum", CalcFormExport[FVD[x p,mu],FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["unknown scalar abbreviation rejected", "UnsupportedScalar", CalcFormExport[1/f[x],FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["invalid dimension rejected", "UnsupportedDimension", CalcFormExport[1,FileNameJoin[{testDirectory,"bad.frm"}],Dimension->I]];
assert["dimension infer D", Import[cases["trace_D"]["Job"]["MappingFile"],"RawJSON"]["Dimension"]==={"Symbol","System`D"}];

assertFailure["polarization propagator rejected", "UnsupportedMomentum", CalcFormExport[FAD[{Polarization[p,I],m}],FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["polarization invalid label rejected", "UnsupportedMomentum", CalcFormExport[FVD[Polarization[p,1],mu],FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["polarization unknown option rejected", "UnsupportedMomentum", CalcFormExport[FVD[Polarization[p,I,OtherOption->True],mu],FileNameJoin[{testDirectory,"bad.frm"}]]];
assertFailure["polarization mixed dimensions rejected", "MixedDimensions", CalcFormExport[FVD[Polarization[p,I],mu]+FV[Polarization[q,-I],nu],FileNameJoin[{testDirectory,"bad.frm"}]]];
Do[
 pol=Polarization[p,phase];
 assert["polarization exact encoding "<>ToString[phase,InputForm],CalcFormConverter`Private`decode[CalcFormConverter`Private`encode[pol]]===pol];
 Do[pol=Polarization[p,phase,Transversality->flag];
 assert["polarization option encoding "<>ToString[{phase,flag},InputForm],CalcFormConverter`Private`decode[CalcFormConverter`Private`encode[pol]]===pol],{flag,{True,False}}],{phase,{I,-I}}];

Do[assertFailure["nonlinear polarization label rejected "<>ToString[route,InputForm], "UnsupportedMomentum",
 CalcFormExport[FVD[Polarization[route,I],mu],FileNameJoin[{testDirectory,"bad.frm"}]]],
 {route,{p q, x p + q, f[p], Polarization[p,I], p^2, 0, 0.5 p}}];
Do[pol=Polarization[-p/2-q-r,phase,Transversality->flag];
 assert["routed polarization exact encoding "<>ToString[{phase,flag},InputForm],
 CalcFormConverter`Private`decode[CalcFormConverter`Private`encode[pol]]===pol],
 {phase,{I,-I}},{flag,{True,False}}];
routedData=CalcFormConverter`Private`buildExportData[FVD[Polarization[-p-q-r,I],mu],Automatic,{}];
assert["routed polarization remains one vector identity", Count[routedData["Mapping"]["Entries"], e_Association /; e["Kind"]==="Vector"]===1];

job1=CalcFormExport[x+y,FileNameJoin[{testDirectory,"digest1.frm"}]];
job2=CalcFormExport[x-y,FileNameJoin[{testDirectory,"digest2.frm"}]];
assert["expression fingerprint",Import[job1["MappingFile"],"Text"] =!= Import[job2["MappingFile"],"Text"]];

(* Fixed artifacts produced by the original version-one exporter and FORM 4.3.
   Never regenerate them in this test: they protect persisted-file compatibility. *)
fixtureRoot=FileNameJoin[{$testRoot,"Fixtures"}];
mappingFile=FileNameJoin[{fixtureRoot,"v1.map.json"}];
resultFile=FileNameJoin[{fixtureRoot,"v1.out"}];
expected=FCI[D+GreekContext`\[Kappa] FAD[{l,m}]^2+I/3+A0[m^2]];
assert["persisted version-one result",CalcFormImport[resultFile,mappingFile]===expected];
header=First[StringSplit[Import[resultFile,"Text"],"\n"]];
malformedFile=FileNameJoin[{testDirectory,"malformed.out"}];
KeyValueMap[Function[{body,tag},
 Export[malformedFile,header<>"\n"<>body,"Text"];
 assertFailure["malformed: "<>body,tag,CalcFormImport[malformedFile,mappingFile]]],
 <|"RunProcess[\"bad\"]"->"InvalidResult","x=1"->"InvalidResult",
 "unknownSymbol"->"UnknownIdentifier","cfA0(1,2)"->"InvalidResult",
 "d_(1,2)"->"InvalidResult","i_;Quit[]"->"InvalidResult","1+"->"InvalidResult",
 "1.25"->"InvalidResult","1 2"->"InvalidResult","1/0"->"InvalidResult","0^(-1)"->"InvalidResult"|>];
assertFailure["mismatched mapping","MappingMismatch",CalcFormImport[resultFile,job1["MappingFile"]]];
Export[malformedFile,"CFC1 "<>Hash[Import[job1["MappingFile"],"Text"],"SHA256","HexString"]<>"\n0","Text"];
assertFailure["changed expression mapping","MappingMismatch",CalcFormImport[malformedFile,job2["MappingFile"]]];
assertFailure["missing file","ReadFailed",CalcFormImport[FileNameJoin[{testDirectory,"missing.out"}],mappingFile]];
Export[malformedFile,header<>"\ncfA0(2)+cfB0(3,0,2)+cfC0(1,2,3,4,5,6)+cfD0(1,2,3,4,5,6,7,8,9,10)","Text"];
assert["masters from future reducer",CalcFormImport[malformedFile,mappingFile]===A0[2]+B0[3,0,2]+C0[1,2,3,4,5,6]+D0[1,2,3,4,5,6,7,8,9,10]];
badMapping=Import[mappingFile,"RawJSON"];
badMapping["Version"]=999;
badMappingFile=FileNameJoin[{testDirectory,"bad.map.json"}];
Export[badMappingFile,badMapping,"RawJSON"];
assertFailure["unsupported mapping version","InvalidMapping",CalcFormImport[malformedFile,badMappingFile]];
badMapping["Version"]=1;
badMapping["Entries"]={<|"Name"->"cfs1","Kind"->"Scalar","Expression"->{"RunProcess",{"Symbol","Global`x"}}|>};
json=ExportString[badMapping,"RawJSON"];
Export[badMappingFile,json,"Text"];
Export[malformedFile,"CFC1 "<>Hash[json,"SHA256","HexString"]<>"\ncfs1","Text"];
assertFailure["executable mapping head rejected","InvalidMapping",CalcFormImport[malformedFile,badMappingFile]];

(* Validate every encoded polarization, even if FORM's result does not use it. *)
Do[
 badMapping["Entries"]={<|"Name"->"cfv1","Kind"->"Vector","Expression"->node|>};
 json=ExportString[badMapping,"RawJSON"];Export[badMappingFile,json,"Text"];
 Export[malformedFile,"CFC1 "<>Hash[json,"SHA256","HexString"]<>"\n0","Text"];
 assertFailure["malformed polarization mapping "<>ToString[node,InputForm],"InvalidMapping",CalcFormImport[malformedFile,badMappingFile]],
 {node,{
  {"Polarization",{"Integer","2"},{"Complex",{"Integer","0"},{"Integer","1"}}},
  {"Polarization",{"Times",{"Symbol","Global`p"},{"Symbol","Global`q"}},{"Complex",{"Integer","0"},{"Integer","1"}}},
  {"Polarization",{"Symbol","Global`p"},{"Integer","1"}},
  {"Polarization",{"Symbol","Global`p"},{"Complex",{"Integer","0"},{"Integer","1"}},{"Transversality","Automatic"}},
  {"Polarization",{"Symbol","Global`p"},{"Complex",{"Integer","0"},{"Integer","1"}},{"Rule",{"Symbol","FeynCalc`Transversality"},{"Symbol","System`True"}}},
  {"Polarization",{"Symbol","Global`p"},{"Complex",{"Integer","0"},{"Integer","1"}},{"Transversality","True"},{"Transversality","False"}}
 }}];

(* Exercise the separated stages without writing an export job. *)
beforeFiles=FileNames["*",testDirectory,Infinity];
data=CalcFormConverter`Private`buildExportData[MTD[mu,nu] FVD[p,mu] FVD[p,nu]+A0[m^2],Automatic,{}];
rendered=CalcFormConverter`Private`renderExport[data,"/unused/result.out",CalcFormConverter`Private`readProgramTemplate[]];
assert["in-memory stages do not write files",FileNames["*",testDirectory,Infinity]===beforeFiles];
assert["rendered JSON preserves mapping",ImportString[rendered["MappingJSON"],"RawJSON"]===data["Mapping"]];
assert["template placeholders resolved",!StringContainsQ[rendered["Program"],RegularExpression["@[A-Z]+@"]]];
assert["version-one result marker",StringContainsQ[rendered["Program"],"CFC1 "<>Hash[rendered["MappingJSON"],"SHA256","HexString"]]];
assert["master declaration",StringContainsQ[rendered["Program"],"CFunctions cfA0,cfB0,cfC0,cfD0;"]];
(* Registry identity includes both kind and the complete Wolfram expression. *)
registryData=CalcFormConverter`Private`buildExportData[
 x+CFCRegistryTest`x+FVD[x,x]+1/(x+y)+z/(x+y),Automatic,{}];
registryEntries=registryData["Mapping"]["Entries"];
registryX=CalcFormConverter`Private`encode[x];
assert["registry separates scalar vector and index kinds",
 Sort[Lookup[Select[registryEntries,#["Expression"]===registryX&],"Kind"]]==={"Index","Scalar","Vector"}];
assert["registry reuses compound abbreviations",
 Count[Lookup[registryEntries,"Expression"],CalcFormConverter`Private`encode[1/(x+y)]]===1];
assert["registry preserves symbol contexts",
 Length[Select[registryEntries,#["Kind"]==="Scalar"&&
  MemberQ[{registryX,CalcFormConverter`Private`encode[CFCRegistryTest`x]},#["Expression"]]&]]===2];

(* Cached fragments must not reuse identifiers from a different export. *)
cacheExpression=(FVD[p,mu]+x FAD[{l,m}]+1/(x+y)) *
 (FVD[p,mu]+z FAD[{l,m}]+z/(x+y));
cacheFirst=CalcFormConverter`Private`buildExportData[cacheExpression,Automatic,{}];
cacheShifted=CalcFormConverter`Private`buildExportData[cacheExpression,Automatic,{q,p}];
cacheAgain=CalcFormConverter`Private`buildExportData[cacheExpression,Automatic,{}];
assert["export fragment caches are isolated between jobs",cacheFirst===cacheAgain];
cacheVectors=Select[cacheShifted["Mapping"]["Entries"],#["Kind"]==="Vector"&];
assert["cached vectors respect this job's registration order",
 Take[Lookup[cacheVectors,"Expression"],2] === (CalcFormConverter`Private`encode /@ {q,p}) &&
 AnyTrue[cacheShifted["Factors"],StringContainsQ[#,"cfv2("]&]];

(* Notebook-generated mappings may contain literal Unicode rather than the
   UTF-8 byte strings returned by RawJSON in command-line kernels. *)
unicodeMapping=CalcFormConverter`Private`buildExportData[MTD[\[Mu],\[Nu]]^2,Automatic,{}]["Mapping"];
unicodeJSON=FromCharacterCode[Normal[ExportByteArray[unicodeMapping,"RawJSON","Compact"->True]],"UTF-8"];
unicodeMapFile=FileNameJoin[{testDirectory,"unicode.map.json"}];
unicodeResultFile=FileNameJoin[{testDirectory,"unicode.out"}];
Export[unicodeMapFile,unicodeJSON,"Text",CharacterEncoding->"UTF-8"];
Export[unicodeResultFile,"CFC1 "<>Hash[unicodeJSON,"SHA256","HexString"]<>"\ncfs1\n","Text"];
assert["literal Unicode mapping imports contracted trace",CalcFormImport[unicodeResultFile,unicodeMapFile]===D];
Export[unicodeResultFile,"CFC1 "<>Hash[unicodeJSON,"SHA256","HexString"]<>"\nd_(cfi1,cfi2)\n","Text"];
assert["literal Unicode mapping restores Greek indices",CalcFormImport[unicodeResultFile,unicodeMapFile]===FCI[MTD[\[Mu],\[Nu]]]];
legacyDigest=Hash[FromCharacterCode[ToCharacterCode[unicodeJSON,"UTF-8"]],"SHA256","HexString"];
Export[unicodeResultFile,"CFC1 "<>legacyDigest<>"\ncfs1\n","Text"];
assert["legacy notebook Unicode checksum",CalcFormImport[unicodeResultFile,unicodeMapFile]===D];
finish["Core"];
