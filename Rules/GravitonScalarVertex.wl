(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonScalarVertex`",{"FeynCalc`","ITensor`","CTensor`","CITensor`"}];

GravitonScalarVertex::usage = "GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The function returns an expression for the gravitational vertex of a scalar field kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta, and m is the scalar field mass.";

Begin["Private`"];

GravitonScalarVertex = {indexArray,p1,p2,m}|->Calc[ I (Global`\[Kappa])^(Length[indexArray]/2) ( - CITensor[{\[ScriptM],\[ScriptN]},indexArray] ITensor[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}] FVD[p1,\[ScriptA]]FVD[p2,\[ScriptB]] - m^2 CTensor[indexArray]) ];

End[];

EndPackage[];
