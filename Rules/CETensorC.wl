(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CETensorC`",{"FeynCalc`","CTensor`","ETensorC`"}];

CETensorC::usage = "CETensorC[{m,\[Mu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. This function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SubscriptBox[\(\[GothicE]\), \(m\[Mu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\).";

Begin["Private`"];

CITensorInternalIndices=Function[indexArray , Function[FoldPairList[TakeDrop,indexArray,2#]]/@(Function[n,Select[Tuples[Range[0,n],2],Total[#]==n&]])[Length[indexArray]/2] ];

CITensorIndices=Function[{indexArrayExternal,indexArrayInternal},  Function[MapThread[Join,{Join[{{}},Partition[indexArrayExternal,2]],#}]]/@CITensorInternalIndices[indexArrayInternal]  ];

CETensorCCore = {indexArrayExternal,indexArrayInternal}|->Total[Function[CTensor[#[[1]]] ETensorC[#[[2]][[;;2]],#[[2]][[3;;]]]  ]/@CITensorIndices[indexArrayExternal,indexArrayInternal]];

CETensorC = {indexArrayExternal,indexArrayInternal}|->Expand[Total[1/Factorial[Length[indexArrayInternal]/2] (Function[CETensorCCore[indexArrayExternal,#]]/@Flatten/@Permutations[Partition[indexArrayInternal,2]])]];

End[];

EndPackage[];

