(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CITensor`",{"FeynCalc`","ITensor`","CTensor`"}];

CITensor::usage = "CITensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function is responsible for the perturbative expansion of \!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\). It takes two arrays. The first array contains two elements which are indices of the metric. The second array contains 2n elements which are indices of the perturbations.";

Begin["Private`"];

CITensorInternalIndices=Function[indexArray , Function[FoldPairList[TakeDrop,indexArray,2#]]/@(Function[n,Select[Tuples[Range[0,n],2],Total[#]==n&]])[Length[indexArray]/2] ];

CITensorIndices=Function[{indexArrayExternal,indexArrayInternal},  Function[MapThread[Join,{Join[{{}},Partition[indexArrayExternal,2]],#}]]/@CITensorInternalIndices[indexArrayInternal]  ];

CITensorCore =Function[ {indexArrayExternal,indexArrayInternal},Total[  Function[(-1)^(Length[indexArrayInternal]/2) (-1)^(-Length[#[[1]]]/2) CTensor[#[[1]]](Times@@ITensor/@#[[2;;]]) ]/@CITensorIndices[indexArrayExternal,indexArrayInternal]  ]  ];

CITensor=Function[{indexArrayExternal,indexArrayInternal},Expand[Total[1/Factorial[Length[indexArrayInternal]/2] (Function[CITensorCore[indexArrayExternal,#]]/@Flatten/@Permutations[Partition[indexArrayInternal,2]])]]  ];

End[];

EndPackage[];
