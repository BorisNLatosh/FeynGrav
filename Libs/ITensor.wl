(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["ITensor`",{"FeynCalc`","MTDWrapper`"}];

ITensor::usage = "ITensor[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function describes the perturbative expansion of \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\). It takes an array of 2n elements and returns the I tensor of n-th order.";

Begin["Private`"];

ITensorIndices = x|-> Partition[ Fold[ ( Join[ #1 , (#1/.{#2[[1]]->#2[[2]],#2[[2]]->#2[[1]]}) ] )& , x, Partition[x,2] ] , Length[x]];

ITensor = Piecewise[{{1,Length[#]==0}},Total[Power[2,-Length[#]/2](MTDWrapper[RotateLeft[#,1]]& /@ ITensorIndices[#])]]&;

End[];

EndPackage[];

