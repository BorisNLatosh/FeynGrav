(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CIIIITensor`",{"FeynCalc`","ITensor`","CTensor`","MTDWrapper`","indexArraySymmetrization`"}];

CIIIITensor::usage = "CIIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Lambda]\[Tau]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\).";

CIIIITensorPlain::usage = "CIIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Lambda]\[Tau]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not allow index symmetries.";

Begin["Private`"];

(*CIIIITensorInternalIndices=Function[indexArray , Function[FoldPairList[TakeDrop,indexArray,2#]]/@(Function[n,Select[Tuples[Range[0,n],5],Total[#]==n&]])[Length[indexArray]/2] ];

CIIIITensorIndices=Function[{indexArrayExternal,indexArrayInternal},  Function[MapThread[Join,{Join[{{}},Partition[indexArrayExternal,2]],#}]]/@CIIIITensorInternalIndices[indexArrayInternal]  ];

CIIIITensorCore =Function[ {indexArrayExternal,indexArrayInternal},Total[  Function[(-1)^(Length[indexArrayInternal]/2) (-1)^(-Length[#[[1]]]/2) CTensor[#[[1]]](Times@@ITensor/@#[[2;;]]) ]/@CIIIITensorIndices[indexArrayExternal,indexArrayInternal]  ]  ];

CIIIITensor=Function[{indexArrayExternal,indexArrayInternal},Expand[Total[1/Factorial[Length[indexArrayInternal]/2] (Function[CIIIITensorCore[indexArrayExternal,#]]/@Flatten/@Permutations[Partition[indexArrayInternal,2]])]]  ];*)

CIIIITensorPlainStep1 = {indexArrayExternal,indexArrayInternal}|->{#[[1]],Join[indexArrayExternal[[;;2]],#[[2]]],Join[indexArrayExternal[[3;;4]],#[[3]]],Join[indexArrayExternal[[5;;6]],#[[4]]],Join[indexArrayExternal[[7;;]],#[[5]]]}&/@(FoldPairList[TakeDrop,indexArrayInternal,2#]&/@Select[Tuples[Range[0,Length[indexArrayInternal]/2],5],Total[#]==Length[indexArrayInternal]/2&]);

CIIIITensorPlain = {indexArrayExternal,indexArrayInternal}|->(-1)^(Length[#[[2]]]/2+1)(-1)^(Length[#[[3]]]/2+1)(-1)^(Length[#[[4]]]/2+1)(-1)^(Length[#[[5]]]/2+1)(CTensorPlain[#[[1]]]ITensorPlain[#[[2]]]ITensorPlain[#[[3]]]ITensorPlain[#[[4]]]ITensorPlain[#[[5]]])&/@CIIIITensorPlainStep1[indexArrayExternal,indexArrayInternal]//Total//Expand;

CIIIITensor = {indexArrayExternal,indexArrayInternal} |-> If[ Length[indexArrayInternal]==0, MTDWrapper[indexArrayExternal] , 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[CIIIITensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]]//Expand ];

End[];

EndPackage[];
