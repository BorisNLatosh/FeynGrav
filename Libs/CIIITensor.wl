(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CIIITensor`",{"FeynCalc`","ITensor`","CTensor`"}];

CIIITensor::usage = "CIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function is responsible for the perturbative expansion of \!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\). It takes two arrays. The first array contains six elements which are indices of the metrics. The second array contains 2n elements which are indices of the perturbations.";

Begin["Private`"];

CIIITensorInternalIndices=Function[indexArray , Function[FoldPairList[TakeDrop,indexArray,2#]]/@(Function[n,Select[Tuples[Range[0,n],4],Total[#]==n&]])[Length[indexArray]/2] ];

CIIITensorIndices=Function[{indexArrayExternal,indexArrayInternal},  Function[MapThread[Join,{Join[{{}},Partition[indexArrayExternal,2]],#}]]/@CIIITensorInternalIndices[indexArrayInternal]  ];

CIIITensorCore =Function[ {indexArrayExternal,indexArrayInternal},Total[  Function[(-1)^(Length[indexArrayInternal]/2) (-1)^(-Length[#[[1]]]/2) CTensor[#[[1]]](Times@@ITensor/@#[[2;;]]) ]/@CIIITensorIndices[indexArrayExternal,indexArrayInternal]  ]  ];

CIIITensor=Function[{indexArrayExternal,indexArrayInternal},Expand[Total[1/Factorial[Length[indexArrayInternal]/2] (Function[CIIITensorCore[indexArrayExternal,#]]/@Flatten/@Permutations[Partition[indexArrayInternal,2]])]]  ];

End[];

EndPackage[];
