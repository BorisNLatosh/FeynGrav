(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["ETensor`",{"FeynCalc`","ITensor`"}];

ETensor::usage = "ETensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function is responsible for the perturbative expansion of \!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(m\)], \(\[Mu]\)]\). It takes two arrays. The first array contains two elements which are indices of the vierbein. The second array contains 2n elements which are indices of the perturbations.";

Begin["Private`"];

ETensor = {indexArrayExternal,indexArrayInternal}|->Expand[Binomial[-1/2,Length[indexArrayInternal]/2]ITensor[Join[indexArrayExternal,indexArrayInternal]]];

End[];

EndPackage[];

