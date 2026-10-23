Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
Clear[p,q,r,s,t,u,m,a,b,mu];
master=PaVe[0,{}, {m^2},PaVeAutoOrder->False,PaVeAutoReduce->False];
bubble=PaVe[0,{s},{m^2,m^2},PaVeAutoOrder->False,PaVeAutoReduce->False];
pol=Pair[Momentum[p,D],Momentum[Polarization[q,I],D]];
conjugatePol=Pair[Momentum[p,D],Momentum[Polarization[q,-I],D]];
polPair=Pair[Momentum[Polarization[p,I],D],Momentum[Polarization[q,-I],D]];
identity=1/s-1/(s+t)-t/(s (s+t));
SetAttributes[oldPath,HoldAll];
oldPath[x_]:=Block[{CalcFormConverter`Private`$rcPolarisationMaximumStructures=0},x];
calc[x_,opts___]:=FeynGravSimplify[x,FORMThreads->1,TimeConstraint->30,opts];
rows={
 {"zero",pol identity master,0},
 {"complex",(2+3 I) pol (1/s+1/t) master,(2+3 I) pol (s+t)/(s t) master},
 {"different structures",(pol identity+polPair (1/s+1/t)) master,polPair (s+t)/(s t) master},
 {"unit structure",(pol identity+1/s+1/t)master,(s+t)/(s t)master},
 {"powers",pol^2 identity master,0},
 {"conjugation",(pol/s+conjugatePol/t)master,(pol/s+conjugatePol/t)master},
 {"dimension",pol (D-2) identity/(D-1) master,0},
 {"separate masters",pol identity master+polPair (1/s+1/t)bubble,polPair (s+t)/(s t)bubble},
 {"factorisation",pol ((s+t)^4/(s u)+(s+t)^4/(t u))master,pol (s+t)^5/(s t u)master},
 {"polarisation denominator",master/(pol+1),master/(pol+1)},
 {"negative polarisation power",master/pol,master/pol}
};
Do[result=calc[row[[2]]];Put[result,FileNameJoin[{testDirectory,StringReplace[row[[1]]," "->"-"]<>".wl"}]];
 assert[row[[1]]<>" returns",!FailureQ[result]];
 assert[row[[1]]<>" exact",Together[result-row[[3]]]===0];
 If[row[[3]]===0,assert[row[[1]]<>" literal zero",result===0]],{row,rows}];
assert["TFORM exact",Together[FeynGravSimplify[rows[[3,2]],FORMThreads->2,TimeConstraint->30]-rows[[3,3]]]===0];
assert["negative control",Together[calc[rows[[2,2]]]-rows[[2,3]]-pol master]=!=0];
assert["no assumptions",Block[{$Assumptions=s==t},calc[pol (s-t)master]]=!=0];
assert["bounded structure fallback",Together[Block[{CalcFormConverter`Private`$rcPolarisationMaximumStructures=1},calc[rows[[3,2]]]]-rows[[3,3]]]===0];
assert["bounded structural expansion fallback",Together[Block[{CalcFormConverter`Private`$rcPolarisationMaximumTerms=1},calc[rows[[3,2]]]]-rows[[3,3]]]===0];
job=FeynGravFORMExport[rows[[3,2]],FileNameJoin[{testDirectory,"manual.frm"}]];
proc=RunProcess[{"form",job["InputFile"]}];
assert["manual execution",proc["ExitCode"]===0&&proc["StandardError"]===""];
res=Block[{CalcFormConverter`Private`$pgReadChunkSize=7},FeynGravFORMImport[job["ResultFile"],job["MappingFile"]]];
assert["incremental import",Together[res-rows[[3,3]]]===0];
oldJob=oldPath[FeynGravFORMExport[rows[[3,2]],FileNameJoin[{testDirectory,"reference.frm"}]]];
assert["unchanged mapping",Import[job["MappingFile"],"RawJSON"]===Import[oldJob["MappingFile"],"RawJSON"]];
assert["no leaked internals",FreeQ[res,_Symbol?(StringStartsQ[SymbolName[#],"cfcPG"]&)]];
assert["no polarisation old path",Together[calc[(1/s+1/t)master]-oldPath[calc[(1/s+1/t)master]]]===0];
(* Keep boundaries cheap: use lowered private budgets as well as the actual
   degree/variable cutoffs. These are selector checks, not high-rank benchmarks. *)
Do[fixture=pol master (s^degree+t);
 assert["degree boundary "<>ToString[degree],Together[calc[fixture]-fixture]===0],{degree,{23,24,25}}];
vars=Array[Unique["kin"]&,13];
Do[fixture=pol master Total[Take[vars,n]];
 assert["variable boundary "<>ToString[n],Together[calc[fixture]-fixture]===0],{n,{11,12,13}}];
Do[fixture=pol master Total[Table[s^j t^(3-j),{j,0,n-1}]];
 assert["term selection boundary "<>ToString[n],Together[
   Block[{CalcFormConverter`Private`$rcMasterMaximumTerms=2},calc[fixture]]-fixture]===0],{n,{1,2,3}}];
Do[fixture=master (pol+polPair+conjugatePol);
 assert["structure selection boundary "<>ToString[n],Together[
   Block[{CalcFormConverter`Private`$rcPolarisationMaximumStructures=n},calc[fixture]]-fixture]===0],{n,{2,3,4}}];
Do[fixture=pol master (s+t)^3;
 assert["factor selection boundary "<>ToString[n],Together[
   Block[{CalcFormConverter`Private`$rcPolarisationFactorMaximumDegree=n},calc[fixture]]-fixture]===0],{n,{2,3,4}}];
fixture=pol MTD[mu,nu] master;
assert["free-index path unchanged",calc[fixture]===oldPath[calc[fixture]]];
fixture=pol Sin[s] master;
opaque=calc[fixture];opaqueReference=oldPath[calc[fixture]];
assert["opaque scalar path unchanged",If[FailureQ[opaqueReference],
 FailureQ[opaque]&&First[opaque]===First[opaqueReference],Together[opaque-opaqueReference]===0]];
fixture=pol master ((s+t)^5/(s u)+(s+t)^5/(t u))+
 polPair bubble ((s+t)^5/(s u)+(s+t)^5/(t u));
assert["reused factors across masters",Together[calc[fixture]-fixture]===0];
(* A rejected expression must have a real key: a missing Collect bracket used
   to collapse all rejections to index zero and poison later eligible inputs.
   Exercise the exported standalone procedure without expanding any shares. *)
cacheProgram=StringReplace[Import[job["InputFile"],"Text"],
 ".end" -> StringRiffle[{
  "Skip;", "Local cfcCacheProbe=cfcMRX1^25+cfcMRX2;", ".sort",
  "#$cfcPGRejectedHits=0;",
  "#call cfcPGInspectRational(cfcCacheProbe)",
  "#write \"CACHE_FIRST %$\",$cfcMROK",
  "#call cfcPGInspectRational(cfcCacheProbe)",
  "#write \"CACHE_REPEAT %$ %$\",$cfcMROK,$cfcPGRejectedHits",
  "Drop cfcCacheProbe;", ".sort", "Skip;", "Local cfcCacheProbe=1;", ".sort",
  "#call cfcPGInspectRational(cfcCacheProbe)",
  "#write \"CACHE_CHANGED %$\",$cfcMROK",
  "Drop cfcCacheProbe;", ".sort", "Skip;",
  "Local cfcCacheProbe=cfcShare(100000);", ".sort",
  "#call cfcPGInspectRational(cfcCacheProbe)",
  "Fill cfcMRMeta(100000)=1;",
  "#call cfcPGInspectRational(cfcCacheProbe)",
  "#write \"CACHE_RESOLVED %$\",$cfcMROK",
  "#call cfcPGInspectRational(cfcCacheProbe)",
  "#write \"CACHE_SUCCESS %$\",$cfcMROK",
  "Drop cfcCacheProbe;",
  ".sort",
  "Skip;",
  "Local cfcCacheProbe=cfcPolyN(100001)+cfcPolyN(100001)^2;",
  ".sort",
  "Fill cfcPGTerms(100001)=1;",
  "#call cfcPGRegisterResult(cfcCacheProbe)",
  "#write \"CACHE_REFERENCE_FIRST\"",
  "Drop cfcCacheProbe;",
  ".sort",
  "Skip;",
  "Local cfcCacheProbe=cfcMRX1+cfcMRX2;",
  ".sort",
  "#define cfcPGActive \"1\"",
  "#call cfcPGCoefficientKey(cfcCacheProbe)",
  "#define cfcProbeKey \"`$cfcPGRejectIndex'\"",
  "#call cfcMRFinal(cfcCacheProbe)",
  "#$cfcProbeReady=cfcPGAlgebraReady(`cfcProbeKey',0);",
  "#write \"CACHE_RATIONAL_READY %$\",$cfcProbeReady",
  "Drop cfcCacheProbe;",
  ".sort",
  "Skip;",
  "Local cfcCacheProbe=cfcMRX1+cfcMRX2;",
  ".sort",
  "#call cfcMRFinal(cfcCacheProbe)",
  "#define cfcPGFactorEnabled \"1\"",
  "Drop cfcCacheProbe;",
  ".sort",
  "Skip;",
  "Local cfcCacheProbe=cfcMRX1+cfcMRX2;",
  ".sort",
  "#call cfcMRFinal(cfcCacheProbe)",
  "#$cfcProbeReady=cfcPGAlgebraReady(`cfcProbeKey',1);",
  "#write \"CACHE_FACTORED_READY %$\",$cfcProbeReady",
  "Skip;",
  "Local cfcCacheDifference=cfcCacheProbe-cfcPGAlgebra(`cfcProbeKey',0);",
  "repeat;",
  "id cfcPolyN(cfcPGIndex?)=extrasymbol_(cfcPGIndex);",
  "endrepeat;",
  ".sort",
  "#$cfcProbeDifference=termsin_(cfcCacheDifference);",
  "#write \"CACHE_EXACT %$\",$cfcProbeDifference",
  "#undefine cfcPGFactorEnabled",
  "#undefine cfcPGActive",
  ".end"},"\n"]];
cachePath=FileNameJoin[{testDirectory,"cache-contract.frm"}];
Export[cachePath,cacheProgram,"Text"];
cacheProcess=RunProcess[{"form",cachePath}];
assert["rejection cache execution",cacheProcess["ExitCode"]===0&&cacheProcess["StandardError"]===""&&
 !StringContainsQ[cacheProcess["StandardOutput"],"Warning"]];
assert["rejection cached once",StringContainsQ[cacheProcess["StandardOutput"],"CACHE_FIRST 0"]&&
 StringContainsQ[cacheProcess["StandardOutput"],"CACHE_REPEAT 0 1"]];
assert["changed coefficient checked",StringContainsQ[cacheProcess["StandardOutput"],"CACHE_CHANGED 1"]];
assert["unresolved metadata not cached",StringContainsQ[cacheProcess["StandardOutput"],"CACHE_RESOLVED 1"]];
Export[FileNameJoin[{testDirectory,"cache-contract.log"}],cacheProcess["StandardOutput"],"Text"];
assert["successful inspection reused",StringContainsQ[cacheProcess["StandardOutput"],"CACHE_SUCCESS 1"]];
assert["known reference skipped before materialisation",StringContainsQ[cacheProcess["StandardOutput"],"CACHE_REFERENCE_FIRST"]];
assert["rational cache mode",StringContainsQ[cacheProcess["StandardOutput"],"CACHE_RATIONAL_READY 1"]];
assert["factorisation cache mode",StringContainsQ[cacheProcess["StandardOutput"],"CACHE_FACTORED_READY 1"]];
assert["cached algebra exact",StringContainsQ[cacheProcess["StandardOutput"],"CACHE_EXACT 0"]];
finish["Polarisation coefficients"];
