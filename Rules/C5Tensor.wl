(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["C5Tensor`",{"FeynCalc`","ITensor`","CTensor`","CIIIITensor`","MTDWrapper`","indexArraySymmetrization`"}];


C5Tensor::usage = "C5Tensor[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(4\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(4\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(5\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(5\)]\)},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(2\)] \*SubscriptBox[\(\[Nu]\), \(2\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(3\)] \*SubscriptBox[\(\[Nu]\), \(3\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(4\)] \*SubscriptBox[\(\[Nu]\), \(4\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(5\)] \*SubscriptBox[\(\[Nu]\), \(5\)]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \[Ellipsis] \*SubscriptBox[\(\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\).";


C5TensorPlain::usage = "C5TensorPlain[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(4\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(4\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(5\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(5\)]\)},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns plain (\!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(2\)] \*SubscriptBox[\(\[Nu]\), \(2\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(3\)] \*SubscriptBox[\(\[Nu]\), \(3\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(4\)] \*SubscriptBox[\(\[Nu]\), \(4\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(5\)] \*SubscriptBox[\(\[Nu]\), \(5\)]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \[Ellipsis] \*SubscriptBox[\(\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not allow any symmetries.";


Begin["Private`"];


C5TensorPlain = {indexArrayExternal,indexArrayInternal} |-> If[ Length[indexArrayExternal]==2*5 , Expand[Sum[ Power[-1,p1] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2p1]]] ] CIIIITensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2p1+1;;]]] ,{p1,0,Length[indexArrayInternal]/2}]] , 0];


C5Tensor = {indexArrayExternal,indexArrayInternal} |-> Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[C5TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ];


End[];


EndPackage[];
