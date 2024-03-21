(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["C7Tensor`",{"FeynCalc`","ITensor`","CTensor`","C6Tensor`","MTDWrapper`","indexArraySymmetrization`"}];


C7Tensor::usage = "C6Tensor[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(4\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(4\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(5\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(5\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(6\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(6\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(7\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(7\)]\)},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(2\)] \*SubscriptBox[\(\[Nu]\), \(2\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(3\)] \*SubscriptBox[\(\[Nu]\), \(3\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(4\)] \*SubscriptBox[\(\[Nu]\), \(4\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(5\)] \*SubscriptBox[\(\[Nu]\), \(5\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(6\)] \*SubscriptBox[\(\[Nu]\), \(6\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(7\)] \*SubscriptBox[\(\[Nu]\), \(7\)]\)]\) \!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\).";


C7TensorPlain::usage = "C6Tensor[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(4\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(4\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(5\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(5\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(6\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(6\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(7\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(7\)]\)},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns ((\!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(2\)] \*SubscriptBox[\(\[Nu]\), \(2\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(3\)] \*SubscriptBox[\(\[Nu]\), \(3\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(4\)] \*SubscriptBox[\(\[Nu]\), \(4\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(5\)] \*SubscriptBox[\(\[Nu]\), \(5\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(6\)] \*SubscriptBox[\(\[Nu]\), \(6\)]\)]\) \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(7\)] \*SubscriptBox[\(\[Nu]\), \(7\)]\)]\) \!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not allow index symmetries.";


Begin["Private`"];


C7TensorPlain = {indexArrayExternal,indexArrayInternal} |-> If[ Length[indexArrayExternal]==2*6 , Expand[Sum[ Power[-1,p1] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2p1]]] ] C6TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2p1+1;;]]] ,{p1,0,Length[indexArrayInternal]/2}]] , 0];


C7Tensor = {indexArrayExternal,indexArrayInternal} |-> Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[C7TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ];


End[];


EndPackage[];
