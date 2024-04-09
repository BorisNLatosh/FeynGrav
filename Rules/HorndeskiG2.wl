(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["HorndeskiG2`",{"FeynCalc`","ITensor`","CTensorGeneral`","indexArraySymmetrization`"}];


HorndeskiG2::usage = "HorndeskiG2[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b]. Expression for Horndeski interaction of \!\(\*SubscriptBox[\(G\), \(2\)]\) class. Involves a+2b\[GreaterEqual]3 scalars. Function arguments are {Subscript[\[Rho], i],Subscript[\[Sigma], i]} are graviton indices; Subscript[p, i] are scalar field momenta; b is the number of scalar field kinetic terms.";


HorndeskiG2Uncontracted::usage = "HorndeskiG2[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b]. Expression for Horndeski interaction of \!\(\*SubscriptBox[\(G\), \(2\)]\) class. Involves a+2b\[GreaterEqual]3 scalars. Function arguments are {Subscript[\[Rho], i],Subscript[\[Sigma], i]} are graviton indices; Subscript[p, i] are scalar field momenta; b is the number of scalar field kinetic terms.";


Begin["Private`"];


MomentaWrapper = scalarMomenta |-> Times @@ MapThread[ FVD, { scalarMomenta, DummyArray2[Length[scalarMomenta]/2] } ] ;


DummyArray2 = n |-> Flatten[ {ToExpression["\[ScriptA]"<>ToString[#]], ToExpression["\[ScriptB]"<>ToString[#]]}& /@ Range[n]];


Clear[HorndeskiG22];

HorndeskiG22[indexArray_,momentumIndexArray_,b_] := HorndeskiG22[indexArray,momentumIndexArray,b] = Total[Map[ I (Global`\[Kappa])^(Length[indexArray]/2) Power[-1,b] CTensorGeneral[DummyArray2[b],indexArray] MomentaWrapper[#[[;;2b]]] & ,Permutations[momentumIndexArray] ]] //Contract;


Clear[HorndeskiG2Uncontracted];

HorndeskiG2Uncontracted[indexArray_,momentumIndexArray_,b_] := HorndeskiG2Uncontracted[indexArray,momentumIndexArray,b] = Total[Map[ I (Global`\[Kappa])^(Length[indexArray]/2) Power[-1,b] CTensorGeneral[DummyArray2[b],indexArray] MomentaWrapper[#[[;;2b]]] & ,Permutations[momentumIndexArray] ]];


End[];


EndPackage[];
