(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CETensor`",{"FeynCalc`","ETensor`","CTensor`"}];

CETensor::usage = "CETensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function is responsible for the perturbative expansion of \!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(m\)], \(\[Mu]\)]\). It takes two arrays. The first array contains two elements which are indices of the vierbein. The second array contains 2n elements which are indices of the perturbations.";

Begin["Private`"];
CITensorInternalIndices=Function[indexArray , Function[FoldPairList[TakeDrop,indexArray,2#]]/@(Function[n,Select[Tuples[Range[0,n],2],Total[#]==n&]])[Length[indexArray]/2] ];

CITensorIndices=Function[{indexArrayExternal,indexArrayInternal},  Function[MapThread[Join,{Join[{{}},Partition[indexArrayExternal,2]],#}]]/@CITensorInternalIndices[indexArrayInternal]  ];

CETensorCore = {indexArrayExternal,indexArrayInternal}|->Total[Function[CTensor[#[[1]]] ETensor[#[[2]][[;;2]],#[[2]][[3;;]]]  ]/@CITensorIndices[indexArrayExternal,indexArrayInternal]];

CETensor = {indexArrayExternal,indexArrayInternal}|->Expand[Total[1/Factorial[Length[indexArrayInternal]/2] (Function[CETensorCore[indexArrayExternal,#]]/@Flatten/@Permutations[Partition[indexArrayInternal,2]])]];

End[];

EndPackage[];
