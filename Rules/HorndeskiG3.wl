(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["HorndeskiG3`",{"FeynCalc`","ITensor`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


HorndeskiG3::usage = "HorndeskiG3[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b + 1\)]\)},b]. Expression for Horndeski interaction of \!\(\*SubscriptBox[\(G\), \(3\)]\) class. Function arguments are {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\),\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton Lorentz indices and momenta; \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta; b is the number of scalar field kinetic term.";


Begin["Private`"];


takeIndices = indexArray |-> Flatten[ #[[;;2]]& /@ Partition[indexArray,3] ];


(*HorndeskiG30Core = {indexArrayMomentumK,momentumArray} |-> I Power[Global`\[Kappa],Length[indexArrayMomentumK]/3] (-1) ( CITensor[{\[ScriptM],\[ScriptN]},takeIndices[indexArrayMomentumK] ] FVD[momentumArray[[1]],\[ScriptM]]FVD[momentumArray[[1]],\[ScriptN]] - CIITensor[{\[ScriptM],\[ScriptN],\[ScriptR],\[ScriptS]}, takeIndices[indexArrayMomentumK[[;;Length[indexArrayMomentumK]-3]]] ] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL], indexArrayMomentumK[[Length[indexArrayMomentumK]-2]],indexArrayMomentumK[[Length[indexArrayMomentumK]-1]] ] FVD[indexArrayMomentumK[[Length[indexArrayMomentumK]]],\[ScriptL]] FVD[momentumArray[[1]],\[ScriptS]]  ) //Contract ;
HorndeskiG30Core1 = {indexArrayMomentumK,momentumArray} |-> Plus@@ ( HorndeskiG30Core[#,momentumArray]& /@ (Flatten/@Permutations[Partition[indexArrayMomentumK,3]]) ) //Contract ;
HorndeskiG30 = {indexArrayMomentumK,momentumArray} |-> Plus@@ ( HorndeskiG30Core1[indexArrayMomentumK,#]& /@ Permutations[momentumArray] ) //Contract ;
HorndeskiG31Core = {indexArrayMomentumK,momentumArray} |-> I Power[Global`\[Kappa],Length[indexArrayMomentumK]/3] Power[-1,1+1] ( CIITensor[{\[ScriptM],\[ScriptN],\[ScriptA]1,\[ScriptB]1},takeIndices[indexArrayMomentumK] ] FVD[momentumArray[[3]],\[ScriptM]]FVD[momentumArray[[3]],\[ScriptN]] - CIIITensor[{\[ScriptM],\[ScriptN],\[ScriptR],\[ScriptS],\[ScriptA]1,\[ScriptB]1}, takeIndices[indexArrayMomentumK[[;;Length[indexArrayMomentumK]-3]]] ] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL], indexArrayMomentumK[[Length[indexArrayMomentumK]-2]],indexArrayMomentumK[[Length[indexArrayMomentumK]-1]] ] FVD[Last[indexArrayMomentumK],\[ScriptL]] FVD[momentumArray[[3]],\[ScriptS]]  ) FVD[momentumArray[[1]],\[ScriptA]1] FVD[momentumArray[[2]],\[ScriptB]1] //Contract ;
HorndeskiG31Core1 = {indexArrayMomentumK,momentumArray} |-> Plus@@ ( HorndeskiG31Core[#,momentumArray]& /@ (Flatten/@Permutations[Partition[indexArrayMomentumK,3]]) ) //Contract ;
HorndeskiG31 = {indexArrayMomentumK,momentumArray} |-> Plus@@ ( HorndeskiG31Core1[indexArrayMomentumK,#]& /@ Permutations[momentumArray] ) //Contract ;
HorndeskiG32Core = {indexArrayMomentumK,momentumArray} |-> I Power[Global`\[Kappa],Length[indexArrayMomentumK]/3] Power[-1,2+1] ( CIIITensor[{\[ScriptM],\[ScriptN],\[ScriptA]1,\[ScriptB]1,\[ScriptA]2,\[ScriptB]2},takeIndices[indexArrayMomentumK] ] FVD[momentumArray[[5]],\[ScriptM]]FVD[momentumArray[[5]],\[ScriptN]] - CIIIITensor[{\[ScriptM],\[ScriptN],\[ScriptR],\[ScriptS],\[ScriptA]1,\[ScriptB]1,\[ScriptA]2,\[ScriptB]2}, takeIndices[indexArrayMomentumK[[;;Length[indexArrayMomentumK]-3]]] ] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL], indexArrayMomentumK[[Length[indexArrayMomentumK]-2]],indexArrayMomentumK[[Length[indexArrayMomentumK]-1]] ] FVD[Last[indexArrayMomentumK],\[ScriptL]] FVD[momentumArray[[5]],\[ScriptS]]  ) FVD[momentumArray[[1]],\[ScriptA]1] FVD[momentumArray[[2]],\[ScriptB]1]FVD[momentumArray[[3]],\[ScriptA]2] FVD[momentumArray[[4]],\[ScriptB]2] //Contract ;
HorndeskiG32Core1 = {indexArrayMomentumK,momentumArray} |-> Plus@@ ( HorndeskiG32Core[#,momentumArray]& /@ (Flatten/@Permutations[Partition[indexArrayMomentumK,3]]) ) //Contract ;
HorndeskiG32 = {indexArrayMomentumK,momentumArray} |-> Plus@@ ( HorndeskiG32Core1[indexArrayMomentumK,#]& /@ Permutations[momentumArray] ) //Contract ;
HorndeskiG3 = {indexArrayMomentumK,momentumArray,b} |-> Piecewise[{{HorndeskiG30[indexArrayMomentumK,momentumArray],b==0},{HorndeskiG31[indexArrayMomentumK,momentumArray],b==1},{HorndeskiG32[indexArrayMomentumK,momentumArray],b==2}}] *)


Clear[HorndeskiG30Core];

HorndeskiG30Core[indexArrayMomentumK_,momentumArray_] := HorndeskiG30Core[indexArrayMomentumK,momentumArray] = I Power[Global`\[Kappa],Length[indexArrayMomentumK]/3] (-1) ( CTensorGeneral[{\[ScriptM],\[ScriptN]},takeIndices[indexArrayMomentumK] ] FVD[momentumArray[[1]],\[ScriptM]]FVD[momentumArray[[1]],\[ScriptN]] - CTensorGeneral[{\[ScriptM],\[ScriptN],\[ScriptR],\[ScriptS]}, takeIndices[indexArrayMomentumK[[;;Length[indexArrayMomentumK]-3]]] ] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL], indexArrayMomentumK[[Length[indexArrayMomentumK]-2]],indexArrayMomentumK[[Length[indexArrayMomentumK]-1]] ] FVD[indexArrayMomentumK[[Length[indexArrayMomentumK]]],\[ScriptL]] FVD[momentumArray[[1]],\[ScriptS]]  ) //Contract ;


Clear[HorndeskiG30Core1];

HorndeskiG30Core1[indexArrayMomentumK_,momentumArray_] := HorndeskiG30Core1[indexArrayMomentumK,momentumArray] = Plus@@ ( HorndeskiG30Core[#,momentumArray]& /@ (Flatten/@Permutations[Partition[indexArrayMomentumK,3]]) ) //Contract ;


Clear[HorndeskiG30];

HorndeskiG30[indexArrayMomentumK_,momentumArray_] := HorndeskiG30[indexArrayMomentumK,momentumArray] = Plus@@ ( HorndeskiG30Core1[indexArrayMomentumK,#]& /@ Permutations[momentumArray] ) //Contract ;


Clear[HorndeskiG31Core];

HorndeskiG31Core[indexArrayMomentumK_,momentumArray_] := HorndeskiG31Core[indexArrayMomentumK,momentumArray] = I Power[Global`\[Kappa],Length[indexArrayMomentumK]/3] Power[-1,1+1] ( CTensorGeneral[{\[ScriptM],\[ScriptN],\[ScriptA]1,\[ScriptB]1},takeIndices[indexArrayMomentumK] ] FVD[momentumArray[[3]],\[ScriptM]]FVD[momentumArray[[3]],\[ScriptN]] - CTensorGeneral[{\[ScriptM],\[ScriptN],\[ScriptR],\[ScriptS],\[ScriptA]1,\[ScriptB]1}, takeIndices[indexArrayMomentumK[[;;Length[indexArrayMomentumK]-3]]] ] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL], indexArrayMomentumK[[Length[indexArrayMomentumK]-2]],indexArrayMomentumK[[Length[indexArrayMomentumK]-1]] ] FVD[Last[indexArrayMomentumK],\[ScriptL]] FVD[momentumArray[[3]],\[ScriptS]]  ) FVD[momentumArray[[1]],\[ScriptA]1] FVD[momentumArray[[2]],\[ScriptB]1] //Contract ;


Clear[HorndeskiG31Core1];

HorndeskiG31Core1[indexArrayMomentumK_,momentumArray_] := HorndeskiG31Core1[indexArrayMomentumK,momentumArray] = Plus@@ ( HorndeskiG31Core[#,momentumArray]& /@ (Flatten/@Permutations[Partition[indexArrayMomentumK,3]]) ) //Contract ;


Clear[HorndeskiG31];

HorndeskiG31[indexArrayMomentumK_,momentumArray_] := HorndeskiG31[indexArrayMomentumK,momentumArray] = Plus@@ ( HorndeskiG31Core1[indexArrayMomentumK,#]& /@ Permutations[momentumArray] ) //Contract ;


Clear[HorndeskiG32Core];

HorndeskiG32Core[indexArrayMomentumK_,momentumArray_] := HorndeskiG32Core[indexArrayMomentumK,momentumArray] = I Power[Global`\[Kappa],Length[indexArrayMomentumK]/3] Power[-1,2+1] ( CTensorGeneral[{\[ScriptM],\[ScriptN],\[ScriptA]1,\[ScriptB]1,\[ScriptA]2,\[ScriptB]2},takeIndices[indexArrayMomentumK] ] FVD[momentumArray[[5]],\[ScriptM]]FVD[momentumArray[[5]],\[ScriptN]] - CTensorGeneral[{\[ScriptM],\[ScriptN],\[ScriptR],\[ScriptS],\[ScriptA]1,\[ScriptB]1,\[ScriptA]2,\[ScriptB]2}, takeIndices[indexArrayMomentumK[[;;Length[indexArrayMomentumK]-3]]] ] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL], indexArrayMomentumK[[Length[indexArrayMomentumK]-2]],indexArrayMomentumK[[Length[indexArrayMomentumK]-1]] ] FVD[Last[indexArrayMomentumK],\[ScriptL]] FVD[momentumArray[[5]],\[ScriptS]]  ) FVD[momentumArray[[1]],\[ScriptA]1] FVD[momentumArray[[2]],\[ScriptB]1]FVD[momentumArray[[3]],\[ScriptA]2] FVD[momentumArray[[4]],\[ScriptB]2] //Contract ;


Clear[HorndeskiG32Core1];

HorndeskiG32Core1[indexArrayMomentumK_,momentumArray_] := HorndeskiG32Core1[indexArrayMomentumK,momentumArray] = Plus@@ ( HorndeskiG32Core[#,momentumArray]& /@ (Flatten/@Permutations[Partition[indexArrayMomentumK,3]]) ) //Contract ;


Clear[HorndeskiG32];

HorndeskiG32[indexArrayMomentumK_,momentumArray_] := HorndeskiG32[indexArrayMomentumK,momentumArray] = Plus@@ ( HorndeskiG32Core1[indexArrayMomentumK,#]& /@ Permutations[momentumArray] ) //Contract ;


Clear[HorndeskiG3];

HorndeskiG3[indexArrayMomentumK_,momentumArray_,b_] := HorndeskiG3[indexArrayMomentumK,momentumArray,b] = Switch[ b,
	0, HorndeskiG30[indexArrayMomentumK,momentumArray],
	1, HorndeskiG31[indexArrayMomentumK,momentumArray],
	2, HorndeskiG32[indexArrayMomentumK,momentumArray]];


End[];


EndPackage[];
