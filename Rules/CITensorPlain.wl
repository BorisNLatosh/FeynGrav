(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CITensorPlain`",{"FeynCalc`","ITensorPlain`","CTensorPlain`"}];

CITensorPlain::usage = "CITensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\).";

CITensorPlainStep1::usage = "";


Begin["Private`"];
(*The CITensorPlain does not account for symmetry. Internal indices are indices that are contracted with the h matrix. External indices are indices of g^\[Mu]\[Nu]. In this step, I generate an array containing all possible distributions of external and internal indices.*)
CITensorPlainStep1 = {indexArrayExternal,indexArrayInternal}|->{Join[indexArrayExternal,#[[1]]],#[[2]]}&/@(FoldPairList[TakeDrop,indexArrayInternal,2#]&/@Select[Tuples[Range[0,Length[indexArrayInternal]/2],2],Total[#]==Length[indexArrayInternal]/2&]);

CITensorPlain = {indexArrayExternal,indexArrayInternal}|->(-1)^(Length[#[[1]]]/2+1)(ITensorPlain[#[[1]]]CTensorPlain[#[[2]]])&/@CITensorPlainStep1[indexArrayExternal,indexArrayInternal]//Total//Expand;

End[];

EndPackage[];
