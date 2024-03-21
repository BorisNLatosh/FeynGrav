(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["CTensorGeneral`",{"FeynCalc`","ITensor`","CTensor`","CITensor`","CIITensor`","CIIITensor`","CIIIITensor`","indexArraySymmetrization`"}];


CTensorPlainGeneral::usage = "CTensorPlainGeneral[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(p\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(p\)]\)},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\)\[Ellipsis] \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(p\)] \*SubscriptBox[\(\[Nu]\), \(p\)]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The number of the inverse metrics in the bracets is p = 0,\[Ellipsis],7. The definition does not allow for any symmetry.";


CTensorGeneral::usage = "CTensorGeneral[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(p\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(p\)]\)},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\)\[Ellipsis] \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(p\)] \*SubscriptBox[\(\[Nu]\), \(p\)]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The number of the inverse metrics in the bracets is p = 0,\[Ellipsis],7.";


Begin["Private`"];


CTensorPlainGeneral = {indexArrayExternal,indexArrayInternal} |-> Piecewise[{{ CTensorPlain[indexArrayInternal], Length[indexArrayExternal]==0}, {CITensorPlain[indexArrayExternal,indexArrayInternal], Length[indexArrayExternal]==2 1}, {CIITensorPlain[indexArrayExternal,indexArrayInternal], Length[indexArrayExternal]==2 2}, {CIIITensorPlain[indexArrayExternal,indexArrayInternal], Length[indexArrayExternal]==2 3}, {CIIIITensorPlain[indexArrayExternal,indexArrayInternal], Length[indexArrayExternal]==2 4}}];


CTensorGeneral = {indexArrayExternal,indexArrayInternal} |-> Piecewise[{{ CTensor[indexArrayInternal], Length[indexArrayExternal]==0}, {CITensor[indexArrayExternal,indexArrayInternal], Length[indexArrayExternal]==2 1}, {CIITensor[indexArrayExternal,indexArrayInternal], Length[indexArrayExternal]==2 2}, {CIIITensor[indexArrayExternal,indexArrayInternal], Length[indexArrayExternal]==2 3}, {CIIIITensor[indexArrayExternal,indexArrayInternal], Length[indexArrayExternal]==2 4} }];


End[];


EndPackage[];
