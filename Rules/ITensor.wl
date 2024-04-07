(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["ITensor`",{"FeynCalc`","MTDWrapper`","indexArraySymmetrization`"}];


ITensor::usage = "ITensor[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\)]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(3\)]\)]\[Ellipsis]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\)] symmetrized with respect to each index pair.";


ITensorPlain::usage = "ITensorPlain[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. Returns MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\)]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(3\)]\)]\[Ellipsis]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\)].";


Begin["Private`"];


Clear[ITensorPlain];

ITensorPlain[args_List] := ITensorPlain[args] = MTDWrapper[RotateLeft[args, 1]];


Clear[ITensor];

ITensor[args_List] := ITensor[args] = Expand[ 1/Power[2,Length[args]/2] 1/Factorial[Length[args]/2] Total[ITensorPlain/@indexArraySymmetrization[args]] ];


End[];


EndPackage[];
