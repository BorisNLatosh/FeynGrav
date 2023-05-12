(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CIIITensor`",{"FeynCalc`","MTDWrapper`","ITensor`","CTensor`","indexArraySymmetrization`"}];

CIIITensor::usage = "CIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\)";

CIIITensorPlain::usage = "CIIITensorPlain[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not allow index symmetries.";

Begin["Private`"];

(*CIIITensorInternalIndices=Function[indexArray , Function[FoldPairList[TakeDrop,indexArray,2#]]/@(Function[n,Select[Tuples[Range[0,n],4],Total[#]==n&]])[Length[indexArray]/2] ];

CIIITensorIndices=Function[{indexArrayExternal,indexArrayInternal},  Function[MapThread[Join,{Join[{{}},Partition[indexArrayExternal,2]],#}]]/@CIIITensorInternalIndices[indexArrayInternal]  ];

CIIITensorCore =Function[ {indexArrayExternal,indexArrayInternal},Total[  Function[(-1)^(Length[indexArrayInternal]/2) (-1)^(-Length[#[[1]]]/2) CTensor[#[[1]]](Times@@ITensor/@#[[2;;]]) ]/@CIIITensorIndices[indexArrayExternal,indexArrayInternal]  ]  ];

CIIITensor=Function[{indexArrayExternal,indexArrayInternal},Expand[Total[1/Factorial[Length[indexArrayInternal]/2] (Function[CIIITensorCore[indexArrayExternal,#]]/@Flatten/@Permutations[Partition[indexArrayInternal,2]])]]  ];*)

CIIITensorPlainStep1 = {indexArrayExternal,indexArrayInternal}|->{#[[1]],Join[indexArrayExternal[[;;2]],#[[2]]],Join[indexArrayExternal[[3;;4]],#[[3]]],Join[indexArrayExternal[[5;;]],#[[4]]]}&/@(FoldPairList[TakeDrop,indexArrayInternal,2#]&/@Select[Tuples[Range[0,Length[indexArrayInternal]/2],4],Total[#]==Length[indexArrayInternal]/2&]);

CIIITensorPlain = {indexArrayExternal,indexArrayInternal}|->(-1)^(Length[#[[2]]]/2+1)(-1)^(Length[#[[3]]]/2+1)(-1)^(Length[#[[4]]]/2+1)(CTensorPlain[#[[1]]]ITensorPlain[#[[2]]]ITensorPlain[#[[3]]]ITensorPlain[#[[4]]])&/@CIIITensorPlainStep1[indexArrayExternal,indexArrayInternal]//Total//Expand;

CIIITensor = {indexArrayExternal,indexArrayInternal} |-> If[ Length[indexArrayInternal]==0, MTDWrapper[indexArrayExternal] , 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[CIIITensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]]//Expand ];

End[];

EndPackage[];
