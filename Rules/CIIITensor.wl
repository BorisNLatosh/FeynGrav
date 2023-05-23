(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["CIIITensor`",{"FeynCalc`","MTDWrapper`","ITensor`","CTensor`","CIITensor`","indexArraySymmetrization`"}];


CIIITensor::usage = "CIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\)";

CIIITensorPlain::usage = "CIIITensorPlain[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not allow index symmetries.";


Begin["Private`"];


CIIITensorPlain = {indexArrayExternal,indexArrayInternal} |-> If[ Length[indexArrayExternal]==6 , Expand[Sum[ Power[-1,p1] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2p1]]] ] CIITensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2p1+1;;]]] ,{p1,0,Length[indexArrayInternal]/2}]] , 0];
CIIITensor = {indexArrayExternal,indexArrayInternal} |-> Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[CIIITensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ];


End[];


EndPackage[];
