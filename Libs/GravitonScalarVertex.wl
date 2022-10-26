(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonScalarVertex`",{"FeynCalc`","ITensor`","CTensor`","CITensor`"}];

GravitonScalarVertex::usage = "GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis]},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Vertex for gravitational interacation of a scalar field kinetic enery. It takes an array of graviton indices, in-going momenta of scalar fields, and the scalar field mass.";

Begin["Private`"];

GravitonScalarVertex = {indexArray,p1,p2,m}|->Calc[ I (Global`\[Kappa])^(Length[indexArray]/2) ( - CITensor[{\[ScriptM],\[ScriptN]},indexArray] ITensor[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}] FVD[p1,\[ScriptA]]FVD[p2,\[ScriptB]] - m^2 CTensor[indexArray]) ];

End[];

EndPackage[];
