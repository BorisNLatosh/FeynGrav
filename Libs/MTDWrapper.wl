(* ::Package:: *)

BeginPackage["MTDWrapper`",{"FeynCalc`"}];

SetDirectory[DirectoryName[$InputFileName]];

MTDWrapper::usage = "MTDWrapper[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\)}] = \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\)\!\(\*SuperscriptBox[\(\[Ellipsis]g\), \(\*SubscriptBox[\(\[Mu]\), \(n\)] \*SubscriptBox[\(\[Nu]\), \(n\)]\)]\). The function returns MTD[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\)]\[Ellipsis]MTD[\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\)].";

MTDWrapper =Times@@MTD@@@Partition[#,2] &;

EndPackage[];
