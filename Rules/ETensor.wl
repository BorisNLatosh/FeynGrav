(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["ETensor`",{"FeynCalc`","ITensor`","indexArraySymmetrization`"}];


ETensorPlain::usage = "ETensorPlain[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(m\)], \(\[Mu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). This definition allows no additional symmetires.";


ETensor::usage = "ETensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(m\)], \(\[Mu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The tensor is symmetric.";


Begin["Private`"];


(* ETensorPlain = Expand[ Binomial[-1/2,Length[#2]/2] ITensorPlain[Join[#1,#2]] ]&; *)



(* ETensor = {indexArrayExternal,indexArrayInternal} |-> 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] ETensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal] //Total//Expand; *)


Clear[ETensorPlain];

ETensorPlain[indexArrayExternal_,indexArrayInternal_] := E1TensorPlain[indexArrayExternal,indexArrayInternal];
ETensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := E2TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal];
ETensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := E3TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal];


Clear[E1TensorPlain];
E1TensorPlain[indexArrayExternal_,indexArrayInternal_] := E1TensorPlain[indexArrayExternal,indexArrayInternal] = Binomial[-1/2,Length[indexArrayInternal]/2] ITensorPlain[Join[indexArrayExternal,indexArrayInternal]];


Clear[E2TensorPlain];
E2TensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := E2TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal] = Sum[ E1TensorPlain[indexArrayExternal1,indexArrayInternal[[;;2*k]]]E1TensorPlain[indexArrayExternal2,indexArrayInternal[[2*k+1;;]]] , {k,0,Length[indexArrayInternal]/2}];


Clear[E3TensorPlain];
E3TensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := E3TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal] = Sum[ E2TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal[[;;2*k]]]E1TensorPlain[indexArrayExternal3,indexArrayInternal[[2*k+1;;]]] , {k,0,Length[indexArrayInternal]/2}];


Clear[ETensor];

ETensor[indexArrayExternal_,indexArrayInternal_] := E1Tensor[indexArrayExternal,indexArrayInternal];
ETensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := E2Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayInternal];


Clear[E1Tensor];

E1Tensor[indexArrayExternal_,indexArrayInternal_] := E1Tensor[indexArrayExternal,indexArrayInternal] =  1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[ Map[ ETensorPlain[indexArrayExternal,#]& , indexArraySymmetrization[indexArrayInternal] ] ]//Expand;


Clear[E2Tensor];

E2Tensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := E2Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayInternal] =  1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[ Map[ ETensorPlain[indexArrayExternal1,indexArrayExternal2,#]& , indexArraySymmetrization[indexArrayInternal] ] ]//Expand;


Clear[E3Tensor];

E3Tensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := E3Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal] =  1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[ Map[ ETensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,#]& , indexArraySymmetrization[indexArrayInternal] ] ]//Expand;


End[];


EndPackage[];
