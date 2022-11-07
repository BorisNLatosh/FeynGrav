(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["ETensorC`",{"FeynCalc`","ITensor`"}];

ETensorC::usage = "ETensorC[{m,\[Mu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. This function returns (\!\(\*SubscriptBox[\(\[GothicE]\), \(m\[Mu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\)."

Begin["Private`"];

ETensorC = {indexArrayExternal,indexArrayInternal}|->Expand[Binomial[1/2,Length[indexArrayInternal]/2]ITensor[Join[indexArrayExternal,indexArrayInternal]]];

End[];

EndPackage[];
