(* ::Package:: *)

BeginPackage["MTDWrapper`",{"FeynCalc`"}];


SetDirectory[DirectoryName[$InputFileName]];


MTDWrapper::usage =
"MTDWrapper[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\)}] \
gives the product of D-dimensional metric tensors \
MTD[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\[Nu]1]\[Ellipsis]MTD[\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\)], \
taking the indices in consecutive pairs. The input list must contain \
an even number of elements.";


Begin["Private`"];


MTDWrapper[indexArray_List] := 
	MTDWrapper[indexArray] =
		Apply[
			Times,
			MapApply[
				MTD,
				Partition[indexArray,2]
			]
		];


End[];


EndPackage[];
