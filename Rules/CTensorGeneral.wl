(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["CTensorGeneral`",{"FeynCalc`","MTDWrapper`","indexArraySymmetrization`"}];


CTensorPlainGeneral::usage = "CTensorPlainGeneral[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(p\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(p\)]\)},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\)\[Ellipsis] \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(p\)] \*SubscriptBox[\(\[Nu]\), \(p\)]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The number of the inverse metrics in the bracets is p = 0,\[Ellipsis],7. The definition does not allow for any symmetry.";


CTensorGeneral::usage = "CTensorGeneral[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(p\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(p\)]\)},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\)\[Ellipsis] \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(p\)] \*SubscriptBox[\(\[Nu]\), \(p\)]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The number of the inverse metrics in the bracets is p = 0,\[Ellipsis],7.";


Begin["Private`"];


(* I and C tensors functions. I place them here to reduce the external dependence. *)


(* I Tensor *)


Clear[ITensorPlain];

ITensorPlain[args_List] := ITensorPlain[args] = MTDWrapper[RotateLeft[args, 1]];


Clear[ITensor];

ITensor[args_List] := ITensor[args] = Expand[ 1/Power[2,Length[args]/2] 1/Factorial[Length[args]/2] Total[ITensorPlain/@indexArraySymmetrization[args]] ];


(* C Tensor *)


Clear[CTensorPlain];

CTensorPlain[args_List] := CTensorPlain[args] = If[Length[args] == 0, 1,   1/Length[args] Sum[(-1)^(k - 1) ITensorPlain[args[[;; 2 k]]] CTensorPlain[args[[2 k + 1 ;;]]], {k, 1, Length[args]/2}] ];


Clear[CTensor];

CTensor[args_List] := CTensor[args] = Expand[1/Power[2, Length[args]/2] 1/Factorial[Length[args]/2] Total[CTensorPlain /@ indexArraySymmetrization[args]]];


(* C1 Tensor *)


Clear[C1TensorPlain];

C1TensorPlain[args1_, args2_] := C1TensorPlain[args1, args2] =  If[ Length[args1] == 0, 0, Sum[Power[-1, p] ITensorPlain[Join[args1, args2[[;; 2 p]]]] CTensorPlain[args2[[2 p + 1 ;;]]], {p, 0, Length[args2]/2}] ];


Clear[C1Tensor];

C1Tensor[indexArrayExternal_, indexArrayInternal_] := C1Tensor[indexArrayExternal, indexArrayInternal] =  Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[C1TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ];


(* C2 Tensor *)


Clear[C2TensorPlain];

C2TensorPlain[indexArrayExternal_,indexArrayInternal_] := C2TensorPlain[indexArrayExternal,indexArrayInternal] = If[ Length[indexArrayExternal]!=4, 0, Sum[ Power[-1,p1] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2p1]]] ] C1TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2p1+1;;]]] ,{p1,0,Length[indexArrayInternal]/2}] ];


Clear[C2Tensor];

C2Tensor[indexArrayExternal_,indexArrayInternal_] := C2Tensor[indexArrayExternal,indexArrayInternal] = Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[C2TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ];


(* C3 Tensor *)


Clear[C3TensorPlain];

C3TensorPlain[indexArrayExternal_,indexArrayInternal_] := C3TensorPlain[indexArrayExternal,indexArrayInternal] = If[ Length[indexArrayExternal]!=6 , 0, Sum[ Power[-1,p1] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2p1]]] ] C2TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2p1+1;;]]] ,{p1,0,Length[indexArrayInternal]/2}] ];


Clear[C3Tensor];

C3Tensor[indexArrayExternal_,indexArrayInternal_] := C3Tensor[indexArrayExternal,indexArrayInternal] = Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[C3TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ];


(* C4 Tensor *)


Clear[C4TensorPlain];

C4TensorPlain[indexArrayExternal_,indexArrayInternal_] := C4TensorPlain[indexArrayExternal,indexArrayInternal] = If[ Length[indexArrayExternal]!=8, 0, Sum[ Power[-1,p1] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2p1]]] ] C3TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2p1+1;;]]] ,{p1,0,Length[indexArrayInternal]/2}] ];


Clear[C4Tensor];

C4Tensor[indexArrayExternal_,indexArrayInternal_] := C4Tensor[indexArrayExternal,indexArrayInternal] = Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[C4TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ];


(* C5 Tensor *)


Clear[C5TensorPlain];

C5TensorPlain[indexArrayExternal_,indexArrayInternal_] := C5TensorPlain[indexArrayExternal,indexArrayInternal] = If[ Length[indexArrayExternal]!=2*5, 0 , Sum[ Power[-1,p1] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2p1]]] ] C4TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2p1+1;;]]] ,{p1,0,Length[indexArrayInternal]/2}] ];


Clear[C5Tensor];

C5Tensor[indexArrayExternal_,indexArrayInternal_] := C5Tensor[indexArrayExternal,indexArrayInternal] = Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[C5TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ];


(* C6 Tensor *)


Clear[C6TensorPlain];

C6TensorPlain[indexArrayExternal_,indexArrayInternal_] := C6TensorPlain[indexArrayExternal,indexArrayInternal] = If[ Length[indexArrayExternal]!=2*6, 0, Sum[ Power[-1,p1] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2p1]]] ] C5TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2p1+1;;]]] ,{p1,0,Length[indexArrayInternal]/2}] ];


Clear[C6Tensor];

C6Tensor[indexArrayExternal_,indexArrayInternal_] := C6Tensor[indexArrayExternal,indexArrayInternal] = Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[C6TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ];


(* C7 Tensor *)


Clear[C7TensorPlain];

C7TensorPlain[indexArrayExternal_,indexArrayInternal_] := C7TensorPlain[indexArrayExternal,indexArrayInternal] = If[ Length[indexArrayExternal]!=2*6, 0, Sum[ Power[-1,p1] ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2p1]]] ] C6TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2p1+1;;]]] ,{p1,0,Length[indexArrayInternal]/2}] ];


Clear[C7Tensor];

C7Tensor[indexArrayExternal_,indexArrayInternal_] := C7Tensor[indexArrayExternal,indexArrayInternal] = Expand[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[C7TensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal]] ];


(* C Tensor General *)


Clear[CTensorPlainGeneral];

CTensorPlainGeneral[indexArrayExternal_,indexArrayInternal_] := CTensorPlainGeneral[indexArrayExternal,indexArrayInternal] = Switch[Length[indexArrayExternal]/2,
	0,CTensorPlain[indexArrayInternal],
	1,C1TensorPlain[indexArrayExternal,indexArrayInternal],
	2,C2TensorPlain[indexArrayExternal,indexArrayInternal],
	3,C3TensorPlain[indexArrayExternal,indexArrayInternal],
	4,C4TensorPlain[indexArrayExternal,indexArrayInternal],
	5,C5TensorPlain[indexArrayExternal,indexArrayInternal],
	6,C6TensorPlain[indexArrayExternal,indexArrayInternal],
	7,C7TensorPlain[indexArrayExternal,indexArrayInternal]];


Clear[CTensorGeneral];

CTensorGeneral[indexArrayExternal_,indexArrayInternal_] := CTensorGeneral[indexArrayExternal,indexArrayInternal] = Switch[Length[indexArrayExternal]/2,
	0,CTensor[indexArrayInternal],
	1,C1Tensor[indexArrayExternal,indexArrayInternal],
	2,C2Tensor[indexArrayExternal,indexArrayInternal],
	3,C3Tensor[indexArrayExternal,indexArrayInternal],
	4,C4Tensor[indexArrayExternal,indexArrayInternal],
	5,C5Tensor[indexArrayExternal,indexArrayInternal],
	6,C6Tensor[indexArrayExternal,indexArrayInternal],
	7,C7Tensor[indexArrayExternal,indexArrayInternal]];


End[];


EndPackage[];
