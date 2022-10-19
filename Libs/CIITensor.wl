(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CIITensor`",{"FeynCalc`","ITensor`","CTensor`"}];

CIITensor::usage = "CIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function is responsible for the perturbative expansion of \!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\). It takes two arrays. The first array contains four elements which are indices of the metrics. The second array contains 2n elements which are indices of the perturbations.";

Begin["Private`"];

CIITensorInternalIndices=Function[indexArray , Function[FoldPairList[TakeDrop,indexArray,2#]]/@(Function[n,Select[Tuples[Range[0,n],3],Total[#]==n&]])[Length[indexArray]/2] ];

CIITensorIndices=Function[{indexArrayExternal,indexArrayInternal},  Function[MapThread[Join,{Join[{{}},Partition[indexArrayExternal,2]],#}]]/@CIITensorInternalIndices[indexArrayInternal]  ];

CIITensorCore =Function[ {indexArrayExternal,indexArrayInternal},Total[  Function[(-1)^(Length[indexArrayInternal]/2) (-1)^(-Length[#[[1]]]/2) CTensor[#[[1]]](Times@@ITensor/@#[[2;;]]) ]/@CIITensorIndices[indexArrayExternal,indexArrayInternal]  ]  ];

CIITensor=Function[{indexArrayExternal,indexArrayInternal},Expand[Total[1/Factorial[Length[indexArrayInternal]/2] (Function[CIITensorCore[indexArrayExternal,#]]/@Flatten/@Permutations[Partition[indexArrayInternal,2]])]]  ];

End[];

EndPackage[];
