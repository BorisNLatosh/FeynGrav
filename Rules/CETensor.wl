(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["CETensor`",{"FeynCalc`","ETensor`","CTensorGeneral`","indexArraySymmetrization`"}];


CETensor::usage = "CETensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(m\)], \(\[Mu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\).";


CETensorPlain::usage = "CETensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(m\)], \(\[Mu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not admit any additional symemtry.";


Begin["Private`"];


(* CETensorPlain = {indexArrayExternal,indexArrayInternal} |-> Sum[ ETensorPlain[indexArrayExternal,indexArrayInternal[[;;2k]]] CTensorPlain[indexArrayInternal[[2k+1;;]]] ,{k,0,Length[indexArrayInternal]/2}] ; *)


(* CETensor = {indexArrayExternal,indexArrayInternal} |-> Expand[Total[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CETensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal] ]]; *)


Clear[CETensorPlain];

CETensorPlain[indexArrayExternal_,indexArrayInternal_] := CE1TensorPlain[indexArrayExternal,indexArrayInternal];
CETensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := CE2TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal];
CETensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := CE3TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal];


Clear[CE1TensorPlain];

CE1TensorPlain[indexArrayExternal_,indexArrayInternal_] := CE1TensorPlain[indexArrayExternal,indexArrayInternal] = Sum[ ETensorPlain[indexArrayExternal,indexArrayInternal[[;;2k]]] CTensorPlainGeneral[{},indexArrayInternal[[2k+1;;]]] ,{k,0,Length[indexArrayInternal]/2}] ;


Clear[CE2TensorPlain];

CE2TensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := CE2TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal] = Sum[ ETensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal[[;;2k]]] CTensorPlainGeneral[{},indexArrayInternal[[2k+1;;]]] ,{k,0,Length[indexArrayInternal]/2}] ;


Clear[CE3TensorPlain];

CE3TensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := CE3TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal] = Sum[ ETensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal[[;;2k]]] CTensorPlainGeneral[{},indexArrayInternal[[2k+1;;]]] ,{k,0,Length[indexArrayInternal]/2}] ;


Clear[CETensor];

CETensor[indexArrayExternal_,indexArrayInternal_] := CE1Tensor[indexArrayExternal,indexArrayInternal];
CETensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := CE2Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayInternal];
CETensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := CE3Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal];


Clear[CE1Tensor];

CE1Tensor[indexArrayExternal_,indexArrayInternal_] := CE1Tensor[indexArrayExternal,indexArrayInternal] = Expand[Total[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CETensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal] ]];


Clear[CE2Tensor];

CE2Tensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := CE2Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayInternal] = Expand[Total[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CETensorPlain[indexArrayExternal1,indexArrayExternal2,#]&/@indexArraySymmetrization[indexArrayInternal] ]];


Clear[CE3Tensor];

CE3Tensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := CE3Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal] = Expand[Total[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CETensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,#]&/@indexArraySymmetrization[indexArrayInternal] ]];


End[];


EndPackage[];
