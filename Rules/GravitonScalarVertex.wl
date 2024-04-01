(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["GravitonScalarVertex`",{"FeynCalc`","CTensorGeneral`","indexArraySymmetrization`"}];


GravitonScalarVertex::usage = "GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The function returns an expression for the gravitational vertex of a scalar field kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta, and m is the scalar field mass.";


GravitonScalarPotentialVertex::usage = "GravitonScalarPotentialVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\)]. The function The function returns an expression for the gravitational vertex of a scalar field potential energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton Lorentz indices and \!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\) is the scalar field interaction couplign.";


Begin["Private`"];


(* GravitonScalarVertex = {indexArray,p1,p2,m} |-> I (Global`\[Kappa])^(Length[indexArray]/2) ( - (1/2) Contract[(FVD[p1,\[ScriptM]]FVD[p2,\[ScriptN]]+FVD[p1,\[ScriptN]]FVD[p2,\[ScriptM]])CITensor[{\[ScriptM],\[ScriptN]},indexArray]] - m^2 FeynCalcInternal[CTensor[indexArray]] ) //Expand; *)


(* GravitonScalarPotentialVertex = {indexArray,\[Lambda]}|-> I (Global`\[Kappa])^(Length[indexArray]/2) \[Lambda] FeynCalcInternal[CTensor[indexArray]] //Expand; *)


(* The code only uses CTensorGeneral *)
(* The code only uses memoization *)


Clear[GravitonScalarVertex];

GravitonScalarVertex[indexArray_,p1_,p2_,m_] := GravitonScalarVertex[indexArray,p1,p2,m] = I (Global`\[Kappa])^(Length[indexArray]/2) ( - (1/2) Contract[(FVD[p1,\[ScriptM]]FVD[p2,\[ScriptN]]+FVD[p1,\[ScriptN]]FVD[p2,\[ScriptM]])CTensorGeneral[{\[ScriptM],\[ScriptN]},indexArray]] - m^2 FeynCalcInternal[CTensorGeneral[{},indexArray]] ) //Expand;


Clear[GravitonScalarPotentialVertex];

GravitonScalarPotentialVertex[indexArray_,\[Lambda]_] := GravitonScalarPotentialVertex[indexArray,\[Lambda]] = I (Global`\[Kappa])^(Length[indexArray]/2) \[Lambda] FeynCalcInternal[CTensorGeneral[{},indexArray]] //Expand;


End[];


EndPackage[];
