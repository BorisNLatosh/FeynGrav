(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["ITensor`",{"FeynCalc`","MTDWrapper`"}];

ITensor::usage = "ITensor[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\)]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(3\)]\)]\[Ellipsis]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\)] symmetrized with respect to each index pair.";

ITensorPlain::usage = "ITensorPlain[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. Returns MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\)]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(3\)]\)]\[Ellipsis]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\)].";

Begin["Private`"];

ITensorIndices = x|-> Partition[ Fold[ ( Join[ #1 , (#1/.{#2[[1]]->#2[[2]],#2[[2]]->#2[[1]]}) ] )& , x, Partition[x,2] ] , Length[x]];

ITensor = Piecewise[{{1,Length[#]==0}},Total[Power[2,-Length[#]/2](MTDWrapper[RotateLeft[#,1]]& /@ ITensorIndices[#])]]&;

ITensorPlain = MTDWrapper[RotateLeft[#,1]]&;

End[];

EndPackage[];

