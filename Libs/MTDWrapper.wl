(* ::Package:: *)

BeginPackage["MTDWrapper`",{"FeynCalc`"}];

SetDirectory[DirectoryName[$InputFileName]];

MTDWrapper::usage = "MTDWrapper[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\)}] = \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\)\!\(\*SuperscriptBox[\(\[Ellipsis]g\), \(\*SubscriptBox[\(\[Mu]\), \(n\)] \*SubscriptBox[\(\[Nu]\), \(n\)]\)]\). The function takes an array of 2n indices and returns a product of metric with these indices.";

MTDWrapper =Times@@MTD@@@Partition[#,2] &;

EndPackage[];
