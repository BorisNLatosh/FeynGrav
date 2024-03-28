(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["CTensor`",{"FeynCalc`","ITensor`","indexArraySymmetrization`"}];


CTensor::usage = "CTensor[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\) symmetric with respect to index permutations in each pair and parmutations of index pairs.";


CTensorPlain::usage = "CTensorPlain[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)]. The tensor uses the recursive definition. The definition does not admit symmetries.";


Begin["Private`"];


(* Old code. The new code is potimised with the ChatGPT. *)
(* CTensorPlain = If[ Length[#]==0 , 1 , 1/Length[#] Sum[(-1)^(k-1) ITensorPlain[#[[;;2k]]]CTensorPlain[#[[2k+1;;]]],{k,1,Length[#]/2}]//Expand ]&; *)


(* Old code. The new code is potimised with the ChatGPT. *)
(* CTensor = Expand[ 1/Power[2,Length[#]/2] 1/Factorial[Length[#]/2] Total[CTensorPlain/@indexArraySymmetrization[#]]  ]&; *)


ClearAll[CTensorPlain];

CTensorPlain[list_] := CTensorPlain[list] = Module[{len = Length[list]},
	If[len == 0, 1, 1/len Sum[(-1)^(k - 1) ITensorPlain[list[[;; 2 k]]] CTensorPlain[list[[2 k + 1 ;;]]], {k, 1, len/2}]//Expand ]
];


CTensor = Module[{lenHalf, factor, symmetrizedIndices},
	lenHalf = Length[#]/2;
	factor = 1/Power[2, lenHalf] 1/Factorial[lenHalf];
	symmetrizedIndices = indexArraySymmetrization[#];
	Expand[factor Total[CTensorPlain /@ symmetrizedIndices]]
]&;


End[];


EndPackage[];
