Get[FileNameJoin[{DirectoryName[$InputFileName], "Common.wl"}]];
equal[a_,b_] := !FailureQ[a] && Together[ExpandScalarProduct[FeynAmpDenominatorExplicit[FCI[a-b]]]]===0;
(* Force the new path independently of the older bounded multivariate path. *)
inputs={
 FAD[{p,m}] (D/(D-1)-1/(D-1)),
 FAD[{p,m}] (D^2-1)/(D-1),
 FAD[{p,m}] (D/(D-1)-1-1/(D-1)),
 FAD[{p,m}] (D+1)/D^3,
 FAD[{p,m}] ((D-2)/(D^2-4)),
 FAD[{p,m}] I (D^2-1)/(D-1),
 FAD[{p,m}] A0[m^2] (D^2-1)/(D-1),
 FAD[{p,m}] FVD[p,mu] (D^2-1)/(D-1),
 FAD[{p,m}] MTD[mu,nu] (D^2-1)/(D-1),
 FAD[{p,m}] Sqrt[D] (D^2-1)/(D-1),
 FAD[{p,m}] (D^2-1)/((D-1)(D+a)),
 FAD[{p,m}] SPD[p,q]^4 (D^2-1)/(D-1),
 (FAD[{p,m}]+FAD[{q,m}]^2)(D^2-1)/(D-1),
 Sum[FAD[{p,k m}] k (D^2-1)/(D-1),{k,17}]
};
Do[
 got=Block[{CalcFormConverter`Private`$rcMaximumTerms=0},CalcFormCalculate[inputs[[k]],FORMThreads->workers]];
 assert["dimension coefficient "<>ToString[{workers,k}],equal[got,inputs[[k]]]];
 If[FailureQ[got],Put[got,FileNameJoin[{testDirectory,"dimension-failure.wl"}]]],
 {workers,{1,2}},{k,Length[inputs]}];
Do[
 got=Block[{CalcFormConverter`Private`$pcMaximumBases=0},CalcFormCalculate[SPD[l]FAD[c l],FORMThreads->workers]];
 assert["direct scaled routing "<>ToString[{workers,c}],got===1/c^2],
 {workers,{1,2}},{c,{1,-1,2,-3/2}}];
got=Block[{CalcFormConverter`Private`$pcMaximumBases=0},CalcFormCalculate[SPD[l]^3 FAD[l]^2,FORMThreads->2]];
assert["direct repeated powers",got===FCI[SPD[l]]];
got=Block[{CalcFormConverter`Private`$pcMaximumBases=0},CalcFormCalculate[SPD[l]FAD[{l,m}],FORMThreads->1]];
assert["massive denominator excluded from direct path",got===FCI[SPD[l]FAD[{l,m}]]];
got=Block[{CalcFormConverter`Private`$pcMaximumBases=0},CalcFormCalculate[SPD[l]FAD[l-p],FORMThreads->1]];
assert["composite routing excluded from direct path",equal[got,SPD[l]FAD[l-p]]];
got=Block[{CalcFormConverter`Private`$pcMaximumBases=0},CalcFormCalculate[SPD[l]FAD[l]GAD[mu],FORMThreads->1]];
assert["direct Dirac coefficient",got===FCI[GAD[mu]]];
ex=Sum[Symbol["DimTest`a"<>ToString[k]] SPD[p,q]^k,{k,1,20}] (D^2-1)/(D-1)FAD[{l,m}];
got=CalcFormCalculate[ex,FORMThreads->2];
assert["automatic high variable count",equal[got,ex] && FreeQ[got,Power[D-1,-1]]];
job=Block[{CalcFormConverter`Private`$rcMaximumTerms=0},CalcFormExport[inputs[[2]],FileNameJoin[{testDirectory,"dimension.frm"}]]];
proc=RunProcess[{"form",job["InputFile"]}];
assert["manual generated programme",proc["ExitCode"]===0];
s=Import[job["ResultFile"],"Text"];
assert["ordinary arithmetic output",!StringContainsQ[s,"cfcDim"|"ratD"]];
assert["public import unchanged",equal[CalcFormImport[job["ResultFile"],job["MappingFile"]],inputs[[2]]]];
assert["mapping stays version one",Import[job["MappingFile"],"RawJSON"]["Version"]===1];
(* Explicit dimensions, not the spelling D, select the polynomial variable. *)
ex=ChangeDimension[FCI[FAD[{l,m}]],dd](dd^2-1)/(dd-1);
got=Block[{CalcFormConverter`Private`$rcMaximumTerms=0},CalcFormCalculate[ex,Dimension->dd,FORMThreads->1]];
assert["custom dimension",equal[got,(dd+1)ChangeDimension[FCI[FAD[{l,m}]],dd]]];
plain=CalcFormExport[MT[mu,nu],FileNameJoin[{testDirectory,"dimension-four.frm"}]];
assert["four-dimensional job unchanged",!StringContainsQ[Import[plain["InputFile"],"Text"],"PolyRatFun cfcDimRat"]];
finish["Dimension coefficients"];
