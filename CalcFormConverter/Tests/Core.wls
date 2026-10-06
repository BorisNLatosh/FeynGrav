Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
(* No FORM process or FeynGrav library is required by this suite. *)
add["exact",-7/13+2 I/3];
add["trace_D",MTD[mu,nu] MTD[mu,nu]];
assertFailure["overwrite refused", "FileExists", CalcFormExport[1,FileNameJoin[{testDirectory,"exact.frm"}]]];
assertFailure["unsupported function", "UnsupportedExpression", CalcFormExport[f[x],FileNameJoin[{testDirectory,"bad.frm"}]]];
assert["no files for unsupported expression", !FileExistsQ[FileNameJoin[{testDirectory,"bad.frm"}]]];
add["dirac_supported", GAD[mu]];
add["colour_supported", SUNF[a,b,c]];
add["epsilon_supported", LC[mu,nu,a,b]];
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
(* Denominator convenience metadata is non-authoritative. Mutating only those
   fields (and recomputing the mapping digest, as any editor must) changes
   nothing about reconstruction, which reads the identifier's "Expression"
   alone. This turns the FORMAT.md invariant into an executable contract: if any
   reconstruction path ever starts trusting the metadata, this test fails.
   CalcFormExport does not run FORM, so both result files are written here by
   hand, differing only in which mapping they are checked against. *)
metadataJob=CalcFormExport[FAD[{l,m}],FileNameJoin[{testDirectory,"metadata.frm"}]];
metadataMapping=Import[metadataJob["MappingFile"],"RawJSON"];
metadataEntry=FirstCase[metadataMapping["Entries"],e_Association/;e["Kind"]==="Denominator",Missing["None"]];
assert["denominator metadata present",AssociationQ[metadataEntry]];
assert["denominator convenience metadata",
 KeyTake[metadataEntry,{"Mass","Power","Prescription"}]===
  <|"Mass"->CalcFormConverter`Private`encode[m],"Power"->1,"Prescription"->"Feynman+i0"|>];
metadataIdentifier=metadataEntry["Name"];
metadataUnmutatedJSON=ExportString[metadataMapping,"RawJSON","Compact"->True];
metadataUnmutatedFile=FileNameJoin[{testDirectory,"metadata-unmutated.map.json"}];
Export[metadataUnmutatedFile,metadataUnmutatedJSON,"Text",CharacterEncoding->"UTF-8"];
metadataMapping["Entries"]=metadataMapping["Entries"]/.
  e_Association/;e["Kind"]==="Denominator":>
   Join[e,<|"Mass"->{"Integer","999999"},"Power"->42,"Prescription"->"Edited"|>];
metadataMutatedJSON=ExportString[metadataMapping,"RawJSON","Compact"->True];
metadataMutatedFile=FileNameJoin[{testDirectory,"metadata-mutated.map.json"}];
Export[metadataMutatedFile,metadataMutatedJSON,"Text",CharacterEncoding->"UTF-8"];
metadataResultFile=FileNameJoin[{testDirectory,"metadata-result.out"}];
Export[metadataResultFile,"CFC1 "<>Hash[metadataUnmutatedJSON,"SHA256","HexString"]<>"\n"<>metadataIdentifier<>"\n","Text"];
assert["unmutated mapping reconstructs the propagator",
 CalcFormImport[metadataResultFile,metadataUnmutatedFile]===FCI[FAD[{l,m}]]];
Export[metadataResultFile,"CFC1 "<>Hash[metadataMutatedJSON,"SHA256","HexString"]<>"\n"<>metadataIdentifier<>"\n","Text"];
assert["convenience metadata is non-authoritative",
 CalcFormImport[metadataResultFile,metadataMutatedFile]===FCI[FAD[{l,m}]]];
(* The renderer, not just the shipped template reader, owns the placeholder
   contract: a caller-supplied template missing a required slot must fail rather
   than silently render a program that drops a declaration or directive. *)
templateData=CalcFormConverter`Private`buildExportData[x+1,Automatic,{}];
completeTemplate="ok";
Scan[Function[p,completeTemplate=completeTemplate<>p<>"\n"],
 CalcFormConverter`Private`$requiredTemplatePlaceholders];
assert["complete synthetic template renders",
 AssociationQ[CalcFormConverter`Private`renderExport[templateData,
  FileNameJoin[{testDirectory,"template.out"}],completeTemplate]]];
resultWithAt = "result" <> FromCharacterCode[92] <> "@home@.out";
renderedResultWithAt = CalcFormConverter`Private`renderExport[templateData,
  resultWithAt, completeTemplate];
expectedResultWithAt = If[$OperatingSystem === "Windows",
  StringReplace[resultWithAt, "\\" -> "/"], resultWithAt];
assert["result paths use platform-appropriate separators and preserve at-signs",
  StringContainsQ[renderedResultWithAt["Program"], expectedResultWithAt]];
truncatedTemplate=StringReplace[completeTemplate,"@INDICES@"->""];
assertFailure["template missing a placeholder is rejected","MissingTemplate",
 Catch[CalcFormConverter`Private`renderExport[templateData,
  FileNameJoin[{testDirectory,"template.out"}],truncatedTemplate],
  CalcFormConverter`Private`$failureTag]];
unknownTemplate = completeTemplate <> "@UNKNOWN@";
assertFailure["template unknown placeholder is rejected","MissingTemplate",
 Catch[CalcFormConverter`Private`renderExport[templateData,
  FileNameJoin[{testDirectory,"template.out"}],unknownTemplate],
  CalcFormConverter`Private`$failureTag]];

(* Symbol validates names without interpreting source text. *)
symbolSideEffect = False;
Scan[Function[name,
 assertFailure["reject malformed symbol name " <> name, "InvalidMapping",
  Catch[CalcFormConverter`Private`decode[{"Symbol", name}],
   CalcFormConverter`Private`$failureTag]]],
 {"x", "`x", "Global`", "Global``x", "Global`1x", "Global`x+y",
  "Global`x;symbolSideEffect=True", "Global`x[1]", "Global`x y"}];
assert["symbol names never execute source", symbolSideEffect === False];
Scan[Function[symbol,
 assert["letter-like symbol round trip " <> SymbolName[symbol],
  CalcFormConverter`Private`decode[CalcFormConverter`Private`encode[symbol]] === symbol]],
 {\[EmptySet], \[DottedSquare], \[FormalA], \[Alpha], \[ScriptA], \[GothicA]}];

(* Invalid authoritative denominator data must fail even when unused. *)
validationJob = CalcFormExport[FAD[{l,m}],FileNameJoin[{testDirectory,"validation.frm"}]];
validationMapping = Import[validationJob["MappingFile"],"RawJSON"];
validationMomentum = {"Momentum",{"Symbol","Global`l"},{"Symbol","System`D"}};
validationNodes = {
 {"PropagatorDenominator",{"Integer","7"},{"Integer","0"}},
 {"PropagatorDenominator",{"Momentum",{"Symbol","Global`l"},{"Integer","4"}},{"Integer","0"}},
 {"PropagatorDenominator",validationMomentum,
  {"Pair",{"LorentzIndex",{"Symbol","Global`mu"},{"Symbol","System`D"}},
          {"LorentzIndex",{"Symbol","Global`nu"},{"Symbol","System`D"}}}},
 {"PropagatorDenominator",{"Momentum",{"Times",{"Symbol","Global`l"},{"Symbol","Global`p"}},
   {"Symbol","System`D"}},{"Integer","0"}},
 {"PropagatorDenominator",{"Momentum",{"Polarization",{"Symbol","Global`p"},
   {"Complex",{"Integer","0"},{"Integer","1"}}},{"Symbol","System`D"}},{"Integer","0"}}
};
Do[
 badValidationMapping = validationMapping;
 badValidationMapping["Entries"] = Map[If[#["Kind"] === "Denominator",
   Join[#,<|"Expression" -> {"FeynAmpDenominator",node}|>],#]&,validationMapping["Entries"]];
 validationJSON=ExportString[badValidationMapping,"RawJSON","Compact"->True];
 Export[validationJob["MappingFile"],validationJSON,"Text",CharacterEncoding->"UTF-8"];
 Do[
  Export[validationJob["ResultFile"],"CFC1 "<>Hash[validationJSON,"SHA256","HexString"]<>"\n"<>body,"Text"];
  assertFailure["invalid propagator: "<>ToString[node,InputForm]<>" / "<>body,"InvalidMapping",
   CalcFormImport[validationJob["ResultFile"],validationJob["MappingFile"]]],
  {body,{"cfd1","0"}}],
 {node,validationNodes}];
finish["Core"];

