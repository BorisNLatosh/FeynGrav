(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["CITensor`",{"FeynCalc`","ITensor`","CTensor`","MTDWrapper`","indexArraySymmetrization`"}];


CITensor::usage = "CITensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The tensor is symmetric with respect to the permutation of the indices in each index pair and with respect to the permutations of the index pairs.";


CITensorPlain::usage = "CITensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not allow index symmetries.";


Begin["Private`"];


(* The old code. The new code was optimised with the help of ChatGPT. *)
(* CITensorPlain = If[ Length[#1]==0 , 0 , Expand[ Sum[ Power[-1,p] ITensorPlain[Join[#1,#2[[;;2p]]]] CTensorPlain[#2[[2p+1;;]]] ,{p,0,Length[#2]/2}] ] ]&; *)


ClearAll[CITensorPlain];

CITensorPlain[args1_, args2_] := CITensorPlain[args1, args2] = Module[{},
  If[Length[args1] == 0, 0, Expand[Sum[Power[-1, p] ITensorPlain[Join[args1, args2[[;; 2 p]]]] CTensorPlain[args2[[2 p + 1 ;;]]], {p, 0, Length[args2]/2}]]
  ]
];


(* The old code. The new code was optimised with the help of ChatGPT. *)
(* CITensor = {indexArrayExternal,indexArrayInternal} |-> Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[CITensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ]; *)


ClearAll[CITensor];

CITensor[indexArrayExternal_, indexArrayInternal_] := CITensor[indexArrayExternal, indexArrayInternal] =
  Module[{},
    Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[CITensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ]
  ];


End[];


EndPackage[];
