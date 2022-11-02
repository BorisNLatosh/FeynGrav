(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CITensorC`",{"FeynCalc`","ITensor`","CTensor`"}];

CITensorC::usage = "CITensorC[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\).";

Begin["Private`"];

CITensorCInternalIndices=Function[indexArray,  FoldPairList[TakeDrop,indexArray,2#]&/@(n|->Select[Tuples[Range[0,n],2],((Total[#]==n)&&(#[[2]]<=1))&])[Length[indexArray]/2]  ];

CITensorCIndices=Function[{indexArrayExternal,indexArrayInternal},Function[MapThread[Join,{Join[{{}},Partition[indexArrayExternal,2]],#}]]/@CITensorCInternalIndices[indexArrayInternal]  ];

CITensorCCore = Function[{indexArrayExternal,indexArrayInternal},Total[  (indexArray|->CTensor[indexArray[[1]]](Times@@ITensor/@indexArray[[2;;]]) )/@CITensorCIndices[indexArrayExternal,indexArrayInternal]  ]  ];

CITensorC=Function[{indexArrayExternal,indexArrayInternal},Expand[Total[1/Factorial[Length[indexArrayInternal]/2] (Function[CITensorCCore[indexArrayExternal,#]]/@Flatten/@Permutations[Partition[indexArrayInternal,2]])]]  ];

End[];

EndPackage[];
