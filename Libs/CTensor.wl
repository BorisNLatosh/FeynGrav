(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["CTensor`",{"FeynCalc`","ITensor`"}];

CTensor::usage = "CTensor[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\).";

Begin["Private`"];

IndexDistribution = {n,m}|->Select[ Tuples[Range[n],m] , Total[#]==n & ];

SingleTermIndexDistribution = {x,m}|-> FoldPairList[TakeDrop,x,2 #]& /@ IndexDistribution[1/2 Length[x],m] ;

SingleTerm = {x,m}|->Total[  Times@@@(  ( (1/(Length[#]/2) ITensor[#])& /@ #)& /@ SingleTermIndexDistribution[x,m] )  ];
CTensorCore = x|-> Total[  (Power[-1,Length[x]/2+#[[2]]]/(Factorial[#[[2]]]Power[2,#[[2]]]) SingleTerm[Sequence@@#])& /@ ({x,#}& /@ Range[Length[x]/2]) ];

ITensorIndices = x|-> Partition[ Fold[ ( Join[ #1 , (#1/.{#2[[1]]->#2[[2]],#2[[2]]->#2[[1]]}) ] )& , x, Partition[x,2] ] , Length[x]];

CTensorIndices = x |-> Partition[ Flatten[ ITensorIndices /@ (Flatten /@ Permutations[Partition[x,2]]) ] , Length[x]];

CTensor = x |-> Piecewise[ {{1,Length[x]==0}}, Calc[ Total[ 1/(Power[2,Length[x]/2] Factorial[Length[x]/2]) (CTensorCore /@ CTensorIndices[x]) ]] ];

End[];

EndPackage[];
