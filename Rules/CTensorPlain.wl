(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CTensorPlain`",{"FeynCalc`","ITensorPlain`"}];

CTensorPlain::usage = "CTensorPlain[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)]. The tensor uses the recursive definition. The definition does not admit symmetries.";


Begin["Private`"];

CTensorPlain = If[Length[#]==0,1,1/Length[#] Sum[(-1)^(k-1) ITensorPlain[#[[;;2k]]]CTensorPlain[#[[2k+1;;]]],{k,1,Length[#]/2}]]&;

End[];


EndPackage[];
