(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["ITensorPlain`",{"FeynCalc`","MTDWrapper`"}];

ITensorPlain::usage = "ITensorPlain[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. Returns MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(2\)]\)]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(3\)]\)]\[Ellipsis]MTD[\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\)].";


Begin["Private`"];

ITensorPlain = MTDWrapper[RotateLeft[#,1]]&;

End[];


EndPackage[];
