(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CIITensorC`",{"FeynCalc`","ITensor`","CTensor`"}];

CIITensorC::usage = "CIITensorC[{\[Mu],\[Nu],\[Alpha],\[Beta]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\).";

Begin["Private`"];

CIITensorCInternalIndices=Function[indexArray,FoldPairList[TakeDrop,indexArray,2#]&/@(n|->Select[Tuples[Range[0,n],3],((Total[#]==n)&&(#[[2]]<=1)&&(#[[3]]<=1))&])[Length[indexArray]/2]];

CIITensorCIndices=Function[{indexArrayExternal,indexArrayInternal},Function[MapThread[Join,{Join[{{}},Partition[indexArrayExternal,2]],#}]]/@CIITensorCInternalIndices[indexArrayInternal]];

CIITensorCCore = Function[{indexArrayExternal,indexArrayInternal},Total[  (indexArray|->CTensor[indexArray[[1]]](Times@@ITensor/@indexArray[[2;;]]) )/@CIITensorCIndices[indexArrayExternal,indexArrayInternal]  ]];

CIITensorC=Function[{indexArrayExternal,indexArrayInternal},Expand[Total[1/Factorial[Length[indexArrayInternal]/2] (Function[CIITensorCCore[indexArrayExternal,#]]/@Flatten/@Permutations[Partition[indexArrayInternal,2]])]]];

End[];

EndPackage[];
