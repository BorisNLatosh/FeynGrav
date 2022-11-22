(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CIICTensorC`",{"FeynCalc`","ITensor`","CTensor`"}];

CIICTensorC::usage = "CIICTensorC[{\[Mu],\[Nu],\[Alpha],\[Beta]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\).";

Begin["Private`"];

CIICTensorCInternalIndices=Function[indexArray,FoldPairList[TakeDrop,indexArray,2#]&/@(n|->Select[Tuples[Range[0,n],3],((Total[#]==n)&&(#[[3]]<=1))&])[Length[indexArray]/2]];

CIICTensorCIndices=Function[{indexArrayExternal,indexArrayInternal},Function[MapThread[Join,{Join[{{}},Partition[indexArrayExternal,2]],#}]]/@CIICTensorCInternalIndices[indexArrayInternal]];

CIICTensorCCore = Function[{indexArrayExternal,indexArrayInternal},Total[  (indexArray|->(-1)^(Length[indexArray[[2]]]/2-1) CTensor[indexArray[[1]]](Times@@ITensor/@indexArray[[2;;]]) )/@CIICTensorCIndices[indexArrayExternal,indexArrayInternal]  ]];

CIICTensorC=Function[{indexArrayExternal,indexArrayInternal},Expand[Total[1/Factorial[Length[indexArrayInternal]/2] (Function[CIICTensorCCore[indexArrayExternal,#]]/@Flatten/@Permutations[Partition[indexArrayInternal,2]])]]];

End[];

EndPackage[];
