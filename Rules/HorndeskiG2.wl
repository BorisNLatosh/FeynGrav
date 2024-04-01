(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["HorndeskiG2`",{"FeynCalc`","ITensor`","CTensorGeneral`","indexArraySymmetrization`"}];


HorndeskiG2::usage = "HorndeskiG2[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b]. Expression for Horndeski interaction of \!\(\*SubscriptBox[\(G\), \(2\)]\) class. Involves a+2b\[GreaterEqual]3 scalars. Function arguments are {Subscript[\[Rho], i],Subscript[\[Sigma], i]} are graviton indices; Subscript[p, i] are scalar field momenta; b is the number of scalar field kinetic terms.";


Begin["Private`"];


(*HorndeskiG21Core = {indexArray,momentumIndexArray} |-> I (Global`\[Kappa])^(Length[indexArray]/2) Power[-1,1] CITensor[{\[ScriptM]1,\[ScriptN]1},indexArray] FVD[momentumIndexArray[[1]],\[ScriptM]1]FVD[momentumIndexArray[[2]],\[ScriptN]1] //Contract ;
HorndeskiG21 = {indexArray,momentumIndexArray} |-> Plus@@(HorndeskiG21Core[indexArray,#]&/@Permutations[momentumIndexArray])//Calc; *)


(*HorndeskiG22Core = {indexArray,momentumIndexArray} |-> I (Global`\[Kappa])^(Length[indexArray]/2) Power[-1,2] CITensor[{\[ScriptM]1,\[ScriptN]1,\[ScriptM]2,\[ScriptN]2},indexArray] FVD[momentumIndexArray[[1]],\[ScriptM]1]FVD[momentumIndexArray[[2]],\[ScriptN]1] FVD[momentumIndexArray[[3]],\[ScriptM]2]FVD[momentumIndexArray[[4]],\[ScriptN]2] //Contract ;
HorndeskiG22 = {indexArray,momentumIndexArray} |-> Plus@@(HorndeskiG22Core[indexArray,#]&/@Permutations[momentumIndexArray])//Calc; *)


(* HorndeskiG2 = {indexArray,momentumIndexArray,b} |-> Piecewise[{{HorndeskiG21[indexArray,momentumIndexArray],b==1},{HorndeskiG22[indexArray,momentumIndexArray],b==2}}]; *)


Clear[HorndeskiG21Core];

HorndeskiG21Core[indexArray_,momentumIndexArray_] := HorndeskiG21Core[indexArray,momentumIndexArray] = I (Global`\[Kappa])^(Length[indexArray]/2) Power[-1,1] CTensorGeneral[{\[ScriptM]1,\[ScriptN]1},indexArray] FVD[momentumIndexArray[[1]],\[ScriptM]1]FVD[momentumIndexArray[[2]],\[ScriptN]1] //Contract ;


Clear[HorndeskiG21];

HorndeskiG21[indexArray_,momentumIndexArray_] := HorndeskiG21[indexArray,momentumIndexArray] = Plus@@(HorndeskiG21Core[indexArray,#]&/@Permutations[momentumIndexArray]) //Calc;


Clear[HorndeskiG22Core];

HorndeskiG22Core[indexArray_,momentumIndexArray_] := HorndeskiG22Core[indexArray,momentumIndexArray] =  I (Global`\[Kappa])^(Length[indexArray]/2) Power[-1,2] CTensorGeneral[{\[ScriptM]1,\[ScriptN]1,\[ScriptM]2,\[ScriptN]2},indexArray] FVD[momentumIndexArray[[1]],\[ScriptM]1]FVD[momentumIndexArray[[2]],\[ScriptN]1] FVD[momentumIndexArray[[3]],\[ScriptM]2]FVD[momentumIndexArray[[4]],\[ScriptN]2] //Contract ;


Clear[HorndeskiG22];

HorndeskiG22[indexArray_,momentumIndexArray_] := HorndeskiG22[indexArray,momentumIndexArray] = Plus@@(HorndeskiG22Core[indexArray,#]&/@Permutations[momentumIndexArray])//Calc;


Clear[HorndeskiG2];

HorndeskiG2[indexArray_,momentumIndexArray_,b_] := HorndeskiG2[indexArray,momentumIndexArray,b] = Switch[ b,
	1 , HorndeskiG21[indexArray,momentumIndexArray],
	2 , HorndeskiG22[indexArray,momentumIndexArray]];


End[];


EndPackage[];
