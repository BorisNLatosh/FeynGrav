(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonScalarVertex`",{"FeynCalc`","ITensor`","CTensor`","CITensor`"}];

GravitonScalarVertex::usage = "GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The function returns an expression for the gravitational vertex of a scalar field kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta, and m is the scalar field mass.";

GravitonScalarPotentialVertex::usage = "GravitonScalarPotentialVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\)]. The function The function returns an expression for the gravitational vertex of a scalar field potential energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton Lorentz indices and \!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\) is the scalar field interaction couplign."

Begin["Private`"];

GravitonScalarVertex = {indexArray,p1,p2,m}|->Calc[ I (Global`\[Kappa])^(Length[indexArray]/2) ( - CITensor[{\[ScriptM],\[ScriptN]},indexArray] ITensor[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}] FVD[p1,\[ScriptA]]FVD[p2,\[ScriptB]] - m^2 CTensor[indexArray]) ];

GravitonScalarPotentialVertex = {indexArray,\[Lambda]}|->Calc[ I (Global`\[Kappa])^(Length[indexArray]/2) \[Lambda] CTensor[indexArray] ];

End[];

EndPackage[];
