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

(* Exercise the separated stages without writing an export job. *)
beforeFiles=FileNames["*",testDirectory,Infinity];
data=CalcFormConverter`Private`buildExportData[MTD[mu,nu] FVD[p,mu] FVD[p,nu]+A0[m^2],Automatic,{}];
rendered=CalcFormConverter`Private`renderExport[data,"/unused/result.out",CalcFormConverter`Private`readProgramTemplate[]];
assert["in-memory stages do not write files",FileNames["*",testDirectory,Infinity]===beforeFiles];
assert["rendered JSON preserves mapping",ImportString[rendered["MappingJSON"],"RawJSON"]===data["Mapping"]];
assert["template placeholders resolved",!StringContainsQ[rendered["Program"],RegularExpression["@[A-Z]+@"]]];
assert["version-one result marker",StringContainsQ[rendered["Program"],"CFC1 "<>Hash[rendered["MappingJSON"],"SHA256","HexString"]]];
assert["master declaration",StringContainsQ[rendered["Program"],"CFunctions cfA0,cfB0,cfC0,cfD0;"]];
finish["Core"];
