(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CITensorPlain`",{"FeynCalc`","ITensorPlain`","CTensorPlain`"}];

CITensorPlain::usage = "CITensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not allow index symmetries.";

CITensorPlain2::usage = "";


Begin["Private`"];

CITensorPlain = Sum[ Power[-1,p] ITensorPlain[Join[#1,#2[[;;2p]]]] CTensorPlain[#2[[2p+1;;]]] ,{p,0,Length[#2]/2}]&;

End[];

EndPackage[];
