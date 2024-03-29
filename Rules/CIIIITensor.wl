(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["CIIIITensor`",{"FeynCalc`","ITensor`","CTensor`","CIIITensor`","MTDWrapper`","indexArraySymmetrization`"}];


CIIIITensor::usage = "CIIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Lambda]\[Tau]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\).";


CIIIITensorPlain::usage = "CIIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Lambda]\[Tau]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not allow index symmetries.";


Begin["Private`"];


(* CIIIITensorPlain = {indexArrayExternal,indexArrayInternal} |-> If[ Length[indexArrayExternal]==8 , Expand[Sum[ Power[-1,p1] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2p1]]] ] CIIITensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2p1+1;;]]] ,{p1,0,Length[indexArrayInternal]/2}]] , 0]; *)


(* CIIIITensor = {indexArrayExternal,indexArrayInternal} |-> Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[CIIIITensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ]; *)


ClearAll[CIIIITensorPlain];

CIIIITensorPlain[indexArrayExternal_,indexArrayInternal_] := CIIIITensorPlain[indexArrayExternal,indexArrayInternal] = If[ Length[indexArrayExternal]!=8, 0, Expand[Sum[ Power[-1,p1] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2p1]]] ] CIIITensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2p1+1;;]]] ,{p1,0,Length[indexArrayInternal]/2}]] ];


ClearAll[CIIIITensor];

CIIIITensor[indexArrayExternal_,indexArrayInternal_] := CIIIITensor[indexArrayExternal,indexArrayInternal] = Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[CIIIITensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ];


End[];


EndPackage[];
