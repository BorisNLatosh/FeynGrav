(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CIIITensorPlain`",{"FeynCalc`","ITensorPlain`","CTensorPlain`"}];

CIIITensorPlain::usage = "";

Begin["Private`"];

CIIITensorInternalIndices=Function[indexArray , Function[FoldPairList[TakeDrop,indexArray,2#]]/@(Function[n,Select[Tuples[Range[0,n],4],Total[#]==n&]])[Length[indexArray]/2] ];

CIIITensorIndices=Function[{indexArrayExternal,indexArrayInternal},  Function[MapThread[Join,{Join[{{}},Partition[indexArrayExternal,2]],#}]]/@CIIITensorInternalIndices[indexArrayInternal]  ];

CIIITensorPlain =Function[ {indexArrayExternal,indexArrayInternal},Total[  Function[(-1)^(Length[indexArrayInternal]/2) (-1)^(-Length[#[[1]]]/2) CTensorPlain[#[[1]]](Times@@ITensorPlain/@#[[2;;]]) ]/@CIIITensorIndices[indexArrayExternal,indexArrayInternal]  ]  ];

End[];

EndPackage[];
