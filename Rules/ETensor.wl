(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["ETensor`",{"FeynCalc`","ITensor`","indexArraySymmetrization`"}];


ETensorPlain::usage = "ETensorPlain[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(m\)], \(\[Mu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). This definition allows no additional symmetires.";


ETensor::usage = "ETensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(m\)], \(\[Mu]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The tensor is symmetric.";


Begin["Private`"];


(* ETensorPlain = Expand[ Binomial[-1/2,Length[#2]/2] ITensorPlain[Join[#1,#2]] ]&; *)



(* ETensor = {indexArrayExternal,indexArrayInternal} |-> 1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] ETensorPlain[indexArrayExternal,#]&/@indexArraySymmetrization[indexArrayInternal] //Total//Expand; *)


Clear[ETensorPlain];

ETensorPlain[indexArrayExternal_,indexArrayInternal_] := ETensorPlain[indexArrayExternal,indexArrayInternal] = Binomial[-1/2,Length[indexArrayInternal]/2] ITensorPlain[Join[indexArrayExternal,indexArrayInternal]];


Clear[ETensor];

ETensor[indexArrayExternal_,indexArrayInternal_] := ETensor[indexArrayExternal,indexArrayInternal] =  1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] Total[ Map[ ETensorPlain[indexArrayExternal,#]& , indexArraySymmetrization[indexArrayInternal] ] ]//Expand;


End[];


EndPackage[];
