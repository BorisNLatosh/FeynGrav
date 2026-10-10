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

assert["dimension functions temporarily abbreviated",
 StringContainsQ[s,"ToPolynomial,OnlyFunctions,cfcDimRat;"]];
assert["dimension aliases restored",StringContainsQ[s,"FromPolynomial;"]];
(* Repeated scaled massless denominators, free indices, complex coefficients
   and custom dimensions exercise cancellation inside prepared contractions. *)
stagedCases={
 staged FAD[l]^2 SPD[l]^2,
 (staged /. FAD[{l,m}]->FAD[-l/2]) SPD[l],
 staged FAD[p] (SPD[p]+I SPD[l]),
 (staged FAD[l] SPD[l]) /. r->-l+p,
 (staged FAD[l] SPD[l]) /. D->dim,
 staged FAD[l] SPD[l] FVD[q,sigma]
};
Do[
 got=Block[{CalcFormConverter`Private`$stagePreparationMinimumLeaves=0},
  CalcFormCalculate[stagedCases[[k]],FORMThreads->workers]];
 assert["early stage cancellation algebra "<>ToString[{workers,k}],
  equal[got,stagedCases[[k]]]],{workers,{1,2}},{k,Length[stagedCases]}];
job=Block[{CalcFormConverter`Private`$stagePreparationMinimumLeaves=0},
 CalcFormExport[stagedCases[[1]],FileNameJoin[{testDirectory,"early-massless.frm"}]]];
s=Import[job["InputFile"],"Text"];
assert["direct rules precede prepared contractions",
 First[StringPosition[s,"repeat;"]][[1]]<First[StringPosition[s,"Hide cfcStage"]][[1]]];
(* Explicit hooks preserve each boundary and pair abbreviation ownership. *)
stagePlan=<|"Rules"->"* dimension rules"|>;
plain=CalcFormConverter`Private`dcfStageSort[stagePlan];
hooked=CalcFormConverter`Private`dcfStageSort[stagePlan,"* direct identities\n"];
assert["stage hooks preserve old programme",hooked===StringReplace[plain,".sort\n"->"* direct identities\n.sort\n"]];
assert["stage hooks at all three boundaries",StringCount[hooked,"* direct identities"]===3];
assert["ordinary stage accepts hook",CalcFormConverter`Private`dcfStageSort[<||>,"* direct\n"]==="* direct\n.sort\n"];
assert["no dimension restoration without plan",CalcFormConverter`Private`dcfStageRestore[<||>]===""];
assert["dimension restoration owned by module",CalcFormConverter`Private`dcfStageRestore[stagePlan]==="FromPolynomial;\n.sort\n"];
finish["Dimension preconditioning"];
