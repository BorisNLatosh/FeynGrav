Get[FileNameJoin[{DirectoryName[$InputFileName], "Common.wl"}]];
(* Force early conditioning on small, independently checkable expressions. *)
equal[a_,b_] := !FailureQ[a] && Together[Contract[ExpandScalarProduct[
 FeynAmpDenominatorExplicit[FCI[a-b]]]]]===0;
inputs={
 FAD[l](D^2-1)/(D-1),
 SPD[l] FAD[{l,m}](D/(D-1)-1/(D-1)),
 (SPD[l]-m^2)^3 FAD[{l,m}]^2/(D-2),
 FAD[l] (D^2-1) Sqrt[D]/(D-1),
 I FAD[l] FVD[p,mu](D^2-1)/(D-1),
 FAD[l] A0[m^2](D^2-1)/(D-1),
 FAD[l] (a+b)^2,
 FAD[l] (D+1)/D^3,
 FAD[l,l-p] SPD[l,p]^2(D^2-1)/(D-1),
 FAD[l] MTD[mu,nu]^2/(D-1),
 FAD[l]FVD[l,mu]FVD[l,mu]/(D-1),
 FAD[l](D/(D-1)-1-1/(D-1)),
 (FAD[l]+FAD[{p,m}]^2)(D^2-1)/(D-1),
 FAD[l]((D^2-1)/(D-1)+(a+b)^2)
};
Do[
 got=Block[{CalcFormConverter`Private`$dcfPreconditionMinimumTerms=0},
  CalcFormCalculate[inputs[[k]],FORMThreads->workers]];
 assert["early dimension algebra "<>ToString[{workers,k}],equal[got,inputs[[k]]]];
 assert["no private rational objects "<>ToString[{workers,k}],
  FreeQ[got,_Symbol?(StringStartsQ[SymbolName[#],"cfcDim"]&)]];
 If[FailureQ[got],Print[InputForm[got]]],
 {workers,{1,2}},{k,Length[inputs]}];
job=Block[{CalcFormConverter`Private`$dcfPreconditionMinimumTerms=0},
 CalcFormExport[inputs[[3]],FileNameJoin[{testDirectory,"precondition.frm"}]]];
s=Import[job["InputFile"],"Text"];
assert["preconditioning before general cancellation",
 First[StringPosition[s,"Compress dimension dependence"]][[1]] <
 First[StringPosition[s,"Exact numerator cancellation"]][[1]]];
assert["compressed coefficients excluded from multivariate selection",
 StringContainsQ[s,"match(cfcDimRat(?a)) || occurs(i_)"]];
assert["version unchanged",Import[job["MappingFile"],"RawJSON"]["Version"]===1];
(* Compare actual prepared tensor stages with an independent FeynCalc contraction. *)
staged=FAD[{l,m}] ((D-2) MTD[mu,nu]+FVD[p,mu]FVD[p,nu])/(D-2) *
 ((D-1) MTD[nu,rho]+FVD[q,nu]FVD[q,rho])/(D-1) *
 (FVD[l,rho]+FVD[r,rho]) FVD[r,mu];
Do[
 got=Block[{CalcFormConverter`Private`$stagePreparationMinimumLeaves=0,
   CalcFormConverter`Private`$dcfPreconditionMinimumTerms=0},
   CalcFormCalculate[staged,FORMThreads->workers]];
 assert["prepared tensor algebra "<>ToString[workers],equal[got,staged]],
 {workers,{1,2}}];
job=Block[{CalcFormConverter`Private`$stagePreparationMinimumLeaves=0},
 CalcFormExport[staged,FileNameJoin[{testDirectory,"prepared-dimension.frm"}]]];
s=Import[job["InputFile"],"Text"];
assert["prepared tensor stage path exercised",StringContainsQ[s,"Local cfcStage"]];
assert["dimension compacted between multiplications",
 Length[StringPosition[s,"PolyRatFun cfcDimRat;"]]>2];
finish["Dimension preconditioning"];
