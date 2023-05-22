(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["CETensor`",{"FeynCalc`","ETensor`","CTensor`","indexArraySymmetrization`"}];


CETensor::usage = "CETensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(m\)], \(\[Mu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\).";
CETensorPlain::usage = "CETensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(m\)], \(\[Mu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The definition does not admit any additional symemtry.";


Begin["Private`"];


CETensorPlain = {indexArrayExternal,indexArrayInternal} |-> Sum[ ETensorPlain[indexArrayExternal,indexArrayInternal[[;;2k]]] CTensorPlain[indexArrayInternal[[2k+1;;]]] ,{k,0,Length[indexArrayInternal]/2}] ;

CETensor = {indexArrayExternal,indexArrayInternal} |-> Expand[Total[ 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CETensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal] ]];


End[];


EndPackage[];
