(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CIITensor`",{"FeynCalc`","ITensor`","CTensor`","MTDWrapper`","indexArraySymmetrization`"}];

CIITensor::usage = "CIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The tensor is symmetric with respect to the permutation of the indices in each index pair and with respect to the permutations of the index pairs.";

CIITensorPlain::usage = "CIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not allow index symmetries.";

Begin["Private`"];

CIITensorPlain = {indexArrayExternal,indexArrayInternal} |-> (Power[-1,Length[#[[2]]]/2+Length[#[[3]]]/2 -2  ]CTensorPlain[#[[1]]] ITensorPlain[#[[2]]]ITensorPlain[#[[3]]] )&/@( MapThread[Join,{Join[{{}},Partition[indexArrayExternal,2]],#}]&/@( FoldPairList[TakeDrop,indexArrayInternal,2#]& /@ Select[Tuples[Range[0,Length[indexArrayInternal]/2],3],Total[#]==Length[indexArrayInternal]/2&] ) ) //Total//Expand;

CIITensor = {indexArrayExternal,indexArrayInternal} |-> If[ Length[indexArrayInternal]==0, MTDWrapper[indexArrayExternal] , 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[CIITensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]]//Expand ];

End[];

EndPackage[];
