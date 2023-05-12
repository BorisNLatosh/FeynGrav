(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["ITensor`",{"FeynCalc`","MTDWrapper`","indexArraySymmetrization`"}];

ITensor::usage = "ITensor[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\)]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(3\)]\)]\[Ellipsis]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\)] symmetrized with respect to each index pair.";

ITensorPlain::usage = "ITensorPlain[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. Returns MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\)]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(3\)]\)]\[Ellipsis]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\)].";

Begin["Private`"];

ITensorPlain = MTDWrapper[RotateLeft[#,1]]&;

ITensor = If[ Length[#]==0, 1 , 1/Power[2,Length[#]/2] 1/Factorial[Length[#]/2] Total[ITensorPlain/@indexArraySymmetrization[#]]//Expand  ]&;

End[];

EndPackage[];

