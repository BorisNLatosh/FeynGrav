(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CIIITensorPlain`",{"FeynCalc`","ITensorPlain`","CTensorPlain`"}];

CIIITensorPlain::usage = "CIIITensorPlain[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not allow index symmetries.";

Begin["Private`"];

(*The CIIITensorPlain does not account for symmetry. Internal indices are indices that are contracted with the h matrix. External indices are indices of the inverse metric. In this step, I generate an array containing all possible distributions of external and internal indices.*)
CIIITensorPlainStep1 = {indexArrayExternal,indexArrayInternal}|->{#[[1]],Join[indexArrayExternal[[;;2]],#[[2]]],Join[indexArrayExternal[[3;;4]],#[[3]]],Join[indexArrayExternal[[5;;]],#[[4]]]}&/@(FoldPairList[TakeDrop,indexArrayInternal,2#]&/@Select[Tuples[Range[0,Length[indexArrayInternal]/2],4],Total[#]==Length[indexArrayInternal]/2&]);

CIIITensorPlain = {indexArrayExternal,indexArrayInternal}|->(-1)^(Length[#[[2]]]/2+1)(-1)^(Length[#[[3]]]/2+1)(-1)^(Length[#[[4]]]/2+1)(CTensorPlain[#[[1]]]ITensorPlain[#[[2]]]ITensorPlain[#[[3]]]ITensorPlain[#[[4]]])&/@CIIITensorPlainStep1[indexArrayExternal,indexArrayInternal]//Total//Expand;


End[];

EndPackage[];
