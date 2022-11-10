(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonFermionVertex`",{"FeynCalc`","ITensor`","CTensor`","CETensor`","ETensor`"}];

GravitonFermionVertex::usage = "GravitonFermionVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The function returns an expression for the graviton vertex of a Dirac fermion kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are fermion momenta, and m is the fermion mass.";

Begin["Private`"];

GravitonFermionVertex = {indexArray,p1,p2,m} |-> Calc[ (I Global`\[Kappa]^(Length[indexArray]/2)) ( 1/2 CETensor[{\[ScriptM],\[ScriptN]},indexArray] GAD[\[ScriptM]]FVD[p1+p2,\[ScriptN]] - CTensor[indexArray] m )  ] ;

End[];

EndPackage[];