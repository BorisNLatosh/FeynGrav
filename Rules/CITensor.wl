(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["CITensor`",{"FeynCalc`","ITensor`","CTensor`","MTDWrapper`","indexArraySymmetrization`"}];


CITensor::usage = "CITensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The tensor is symmetric with respect to the permutation of the indices in each index pair and with respect to the permutations of the index pairs.";

CITensorPlain::usage = "CITensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not allow index symmetries.";


Begin["Private`"];


CITensorPlain = If[ Length[#1]==0 , 0 , Expand[ Sum[ Power[-1,p] ITensorPlain[Join[#1,#2[[;;2p]]]] CTensorPlain[#2[[2p+1;;]]] ,{p,0,Length[#2]/2}] ] ]&;

CITensor = {indexArrayExternal,indexArrayInternal} |-> Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[CITensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ];


End[];


EndPackage[];
