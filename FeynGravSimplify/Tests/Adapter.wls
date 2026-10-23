(* Run in a fresh kernel. No external programs are used by this suite. *)
$FeynCalcStartupMessages = False;
Get["FeynCalc`"];
Get[FileNameJoin[{DirectoryName[$InputFileName], "Completion.wl"}]];
Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "RestrictedCodec.wl"}]];
Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "Contractor.wl"}]];
enc = FeynGrav`Simplification`Private`encode;
dec = FeynGrav`Simplification`Private`decode;
passed = 0; failed = 0;
check[name_, condition_] := If[TrueQ[condition], passed++, failed++; Print["FAIL: ", name]];
round[x_] := Module[{d = enc[x]}, If[FailureQ[d], d, dec[ExportString[d["Tree"], "RawJSON"], d]]];
check["zero", round[0] === 0];
check["constants", round[(2+3 I)/7] === (2+3 I)/7];
check["symbols", round[a b + c] === a b + c];
check["D distinct from user D", round[System`D + User`D] === System`D + User`D];
check["contexts and Unicode", round[Left`\[Mu] + Right`\[Mu]] === Left`\[Mu] + Right`\[Mu]];
check["shorthand", round[MTD[mu,nu] FVD[p,mu]] === FCI[MTD[mu,nu] FVD[p,mu]]];
check["composite momentum", round[FVD[p-q,mu]] === FCI[FVD[p-q,mu]]];
check["one-argument SP", round[SPD[p]] === FCI[SPD[p]]];
check["four dimensional", round[MT[mu,nu] FV[p,mu]] === FCI[MT[mu,nu] FV[p,mu]]];
check["denominator shorthand", round[FAD[{p-q,m}]] === FeynAmpDenominator[PropagatorDenominator[Momentum[p-q,D],m]]];
check["denominator internal", round[FCI[FAD[{p-q,m}]]] === FCI[FAD[{p-q,m}]]];
check["opaque scalar", round[Log[SPD[p,q]]] === Log[FCI[SPD[p,q]]]];
check["scalar inverse", round[(a+b)^(-2)] === (a+b)^(-2)];
check["large integers", round[10^30 + 10^40 I] === 10^30 + 10^40 I];
check["mixed dimension", FailureQ[enc[MT[mu,nu] MTD[a,b]]]];
check["evanescent", FailureQ[enc[Pair[LorentzIndex[mu,D-4],LorentzIndex[nu,D-4]]]]];
check["Dirac", FailureQ[enc[GAD[mu]]]];
check["colour", FailureQ[enc[SUNDelta[a,b]]]];
check["epsilon", FailureQ[enc[LC[mu,nu,a,b]]]];
check["hidden tensor", FailureQ[enc[f[MTD[mu,nu]]]]];
check["tensor in scalar function", FailureQ[enc[Log[FVD[p,mu]]]]];
check["negative tensor power", FailureQ[enc[FVD[p,mu]^(-1)]]];
check["symbolic tensor power", FailureQ[enc[FVD[p,mu]^n]]];
check["positive tensor power", !FailureQ[enc[(MTD[mu,nu] + FVD[p,mu] FVD[p,nu])^2]]];
check["container", FailureQ[enc[{a,b}]]];
check["equation", FailureQ[enc[a==b]]];
check["approximate number rejected", FailureQ[enc[1.2]]];
data=enc[a + MTD[mu,nu]];
Do[check["invalid response "<>s, FailureQ[dec[s,data]]], {s,{
 "", "[", "null", "true", "1.5", "\"unknown\"", "[\"Set\",\"a\",1]",
 "[\"Rational\",1,0]", "[\"Pair\",1,2]", "[\"Momentum\",\"fg1\"]",
 "[\"Complex\",\"fg1\",1]", "0 1"}}];
tensorData=enc[FVD[p,mu]];
check["invalid returned tensor inverse",FailureQ[dec[ExportString[{"Power",tensorData["Tree"],-1},"RawJSON"],tensorData]]];
check["invalid returned tensor exponent",FailureQ[dec[ExportString[{"Power",2,tensorData["Tree"]},"RawJSON"],tensorData]]];
(* A successful encoding must not call algebraic FeynCalc/Mathematica helpers. *)
calls=0;
Block[{Simplify,FullSimplify,Expand,FeynCalc`Contract,FeynCalc`DiracSimplify,
 FeynCalc`ExpandScalarProduct,FeynCalc`MomentumExpand,FeynCalc`FCI,FeynCalc`FeynCalcInternal},
 Scan[(DownValues[#]={})&,{Simplify,FullSimplify,Expand}];
 Simplify[___]:=(calls++;$Failed);FullSimplify[___]:=(calls++;$Failed);Expand[___]:=(calls++;$Failed);
 FeynCalc`Contract[___]:=(calls++;$Failed);FeynCalc`DiracSimplify[___]:=(calls++;$Failed);
 FeynCalc`ExpandScalarProduct[___]:=(calls++;$Failed);FeynCalc`MomentumExpand[___]:=(calls++;$Failed);
 FeynCalc`FCI[___]:=(calls++;$Failed);FeynCalc`FeynCalcInternal[___]:=(calls++;$Failed);
 probe=round[(MTD[mu,nu]+FVD[p,mu] FVD[p,nu]) FVD[q,mu] FVD[q,nu]];
];
check["no explicit simplification", calls===0 && !FailureQ[probe]];
writeSuiteCompletion["Adapter", passed, failed];
Print["Adapter: ",passed," passed, ",failed," failed"];
Quit[If[failed===0,0,1]];
