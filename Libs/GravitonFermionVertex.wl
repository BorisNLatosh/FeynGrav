(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonFermionVertex`",{"FeynCalc`","ITensor`","CTensor`","CETensor`","ETensor`"}];

GravitonFermionVertex::usage = "GravitonFermionVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis]},p1,p2,m]. Vertex for gravitational interacation of a Dirac field kinetic enery. It takes an array of graviton indices, in-going momenta of Dirac fields, and the Dirac field mass.";

Begin["Private`"];

GravitonFermionVertex = {indexArray,p1,p2,m} |-> Calc[ (I Global`\[Kappa]^(Length[indexArray]/2)) ( 1/2 CETensor[{\[ScriptM],\[ScriptN]},indexArray] GAD[\[ScriptM]]FVD[p1+p2,\[ScriptN]] - CTensor[indexArray] m )  ] ;

End[];

EndPackage[];
