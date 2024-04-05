(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["GravitonVertex`",{"FeynCalc`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


GravitonVertex::usage = "GravitonVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\[Ellipsis]},\[CurlyEpsilon]].";


GravitonVertexUncontracted::usage = "GravitonVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\[Ellipsis]},\[CurlyEpsilon]].";


GravitonGhostVertex::usage = "GravitonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis]},\[Mu],p1,\[Nu],p2].";


GravitonGhostVertexUncontracted::usage = "GravitonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis]},\[Mu],p1,\[Nu],p2]. Nothing is contracted.";


Begin["Private`"];


TakeLorenzIndices = indexArray |-> Flatten[(#[[;;2]]&)/@Partition[indexArray,3]];


GravitonVertex[indexArray_,\[CurlyEpsilon]_] := GravitonVertex[indexArray,\[CurlyEpsilon]] = Contract[ Total[ ( I (Global`\[Kappa])^(Length[#]/3-2) CTensorGeneral[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[#[[7;;]]]]FVD[#[[3]],\[Lambda]1]FVD[#[[6]],\[Lambda]2] (2 GammaTensor[\[Alpha],\[Mu],\[Rho],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Sigma],\[Nu],\[Beta],\[Lambda]2,#[[4]],#[[5]]] - 2 GammaTensor[\[Alpha],\[Mu],\[Nu],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda]2,#[[4]],#[[5]]] - (1/2) \[CurlyEpsilon] GammaTensor[\[Mu],\[Alpha],\[Beta],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Nu],\[Rho],\[Sigma],\[Lambda]2,#[[4]],#[[5]]]) )&/@( Flatten/@Permutations[Partition[indexArray,3]] ) ] ];


GravitonVertexUncontracted[indexArray_,\[CurlyEpsilon]_] := GravitonVertexUncontracted[indexArray,\[CurlyEpsilon]] = Total[ ( I (Global`\[Kappa])^(Length[#]/3-2) CTensorGeneral[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[#[[7;;]]]]FVD[#[[3]],\[Lambda]1]FVD[#[[6]],\[Lambda]2] (2 GammaTensor[\[Alpha],\[Mu],\[Rho],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Sigma],\[Nu],\[Beta],\[Lambda]2,#[[4]],#[[5]]] - 2 GammaTensor[\[Alpha],\[Mu],\[Nu],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda]2,#[[4]],#[[5]]] - (1/2) \[CurlyEpsilon] GammaTensor[\[Mu],\[Alpha],\[Beta],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Nu],\[Rho],\[Sigma],\[Lambda]2,#[[4]],#[[5]]]) )&/@( Flatten/@Permutations[Partition[indexArray,3]] ) ] ;


Clear[GravitonGhostVertex1];

GravitonGhostVertex1[indexArray_,\[Mu]_,p1_,\[Nu]_,p2_] := GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2] =  CTensorGeneral[{\[Alpha],\[Beta],\[Mu],\[Nu]},TakeLorenzIndices[indexArray]]FVD[p1,\[Alpha]]FVD[p2,\[Beta]] ;


Clear[GravitonGhostVertex2];

GravitonGhostVertex2[indexArray_,\[Mu]_,p1_,\[Nu]_,p2_] := GravitonGhostVertex2[indexArray,\[Mu],p1,\[Nu],p2] = Map[ (CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[#[[4;;]]]](-1) FVD[#[[3]],\[Lambda]]( FVD[p1,\[Sigma]]GammaTensor[\[Beta],\[Rho],\[Alpha],\[Lambda],#[[1]],#[[2]]]- FVD[p2,\[Sigma]]GammaTensor[\[Alpha],\[Rho],\[Beta],\[Lambda],#[[1]],#[[2]]]+FVD[#[[3]],\[Rho]]GammaTensor[\[Sigma],\[Alpha],\[Beta],\[Lambda],#[[1]],#[[2]]]- FVD[#[[3]],\[Alpha]]GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda],#[[1]],#[[2]]] ) )& , Flatten/@Permutations[Partition[indexArray,3]] ]//Total;


Clear[GravitonGhostVertex3];

GravitonGhostVertex3[indexArray_,\[Mu]_,p1_,\[Nu]_,p2_] := GravitonGhostVertex3[indexArray,\[Mu],p1,\[Nu],p2] = Map[ (CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},TakeLorenzIndices[#[[7;;]]]] (-1)FVD[#[[3]],\[Lambda]1]FVD[#[[6]],\[Lambda]2]( GammaTensor[\[Rho],\[Alpha],\[Lambda],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Sigma],\[Beta],\[Tau],\[Lambda]2,#[[4]],#[[5]]] - GammaTensor[\[Rho],\[Alpha],\[Beta],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Sigma],\[Lambda],\[Tau],\[Lambda]2,#[[4]],#[[5]]] + GammaTensor[\[Alpha],\[Rho],\[Lambda],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Beta],\[Sigma],\[Tau],\[Lambda]2,#[[4]],#[[5]]]) )& , Flatten/@Permutations[Partition[indexArray,3]] ] //Total;


Clear[GravitonGhostVertex];

GravitonGhostVertex[indexArray_,\[Mu]_,p1_,\[Nu]_,p2_] := GravitonGhostVertex[indexArray,\[Mu],p1,\[Nu],p2] = Switch[Length[indexArray]/3,
	0, I (Global`\[Kappa])^(Length[indexArray]/3) GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2] //Contract ,
	1, I (Global`\[Kappa])^(Length[indexArray]/3) ( Contract[GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2]] + Contract[GravitonGhostVertex2[indexArray,\[Mu],p1,\[Nu],p2]] ) //Contract,
	_, I (Global`\[Kappa])^(Length[indexArray]/3) ( Contract[GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2]] + Contract[GravitonGhostVertex2[indexArray,\[Mu],p1,\[Nu],p2]] + Contract[GravitonGhostVertex3[indexArray,\[Mu],p1,\[Nu],p2]] ) //Contract
];


Clear[GravitonGhostVertexUncontracted];

GravitonGhostVertexUncontracted[gravitonParameters_,\[Mu]_,p1_,\[Nu]_,p2_] := GravitonGhostVertex[gravitonParameters,\[Mu],p1,\[Nu],p2] = Switch[Length[gravitonParameters]/3,
	1, I (Global`\[Kappa])^(Length[gravitonParameters]/3)(CTensorGeneral[{\[Alpha],\[Beta],\[Mu],\[Nu]},TakeLorenzIndices[gravitonParameters]]FVD[p1,\[Alpha]]FVD[p2,\[Beta]]) - I (Global`\[Kappa])^(Length[gravitonParameters]/3)Total[Map[ (CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[#[[4;;]]]] FVD[#[[3]],\[Lambda]]( FVD[p1,\[Sigma]]GammaTensor[\[Beta],\[Rho],\[Alpha],\[Lambda],#[[1]],#[[2]]]- FVD[p2,\[Sigma]]GammaTensor[\[Alpha],\[Rho],\[Beta],\[Lambda],#[[1]],#[[2]]]+FVD[#[[3]],\[Rho]]GammaTensor[\[Sigma],\[Alpha],\[Beta],\[Lambda],#[[1]],#[[2]]]- FVD[#[[3]],\[Alpha]]GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda],#[[1]],#[[2]]] ) )&, Flatten/@Permutations[Partition[gravitonParameters,3]] ]],
	_, I (Global`\[Kappa])^(Length[gravitonParameters]/3)(CTensorGeneral[{\[Alpha],\[Beta],\[Mu],\[Nu]},TakeLorenzIndices[gravitonParameters]]FVD[p1,\[Alpha]]FVD[p2,\[Beta]]) - I (Global`\[Kappa])^(Length[gravitonParameters]/3)Total[Map[ (CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[#[[4;;]]]] FVD[#[[3]],\[Lambda]]( FVD[p1,\[Sigma]]GammaTensor[\[Beta],\[Rho],\[Alpha],\[Lambda],#[[1]],#[[2]]]- FVD[p2,\[Sigma]]GammaTensor[\[Alpha],\[Rho],\[Beta],\[Lambda],#[[1]],#[[2]]]+FVD[#[[3]],\[Rho]]GammaTensor[\[Sigma],\[Alpha],\[Beta],\[Lambda],#[[1]],#[[2]]]- FVD[#[[3]],\[Alpha]]GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda],#[[1]],#[[2]]] ) )&, Flatten/@Permutations[Partition[gravitonParameters,3]] ]] - I (Global`\[Kappa])^(Length[gravitonParameters]/3) Total[Map[ (CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],l,t},TakeLorenzIndices[#[[7;;]]]] FVD[#[[3]],l1]FVD[#[[6]],l2]( GammaTensor[\[Rho],\[Alpha],l,l1,#[[1]],#[[2]]]GammaTensor[\[Sigma],\[Beta],t,l2,#[[4]],#[[5]]] - GammaTensor[\[Rho],\[Alpha],\[Beta],l1,#[[1]],#[[2]]]GammaTensor[\[Sigma],l,t,l2,#[[4]],#[[5]]] + GammaTensor[\[Alpha],\[Rho],l,l1,#[[1]],#[[2]]]GammaTensor[\[Beta],\[Sigma],t,l2,#[[4]],#[[5]]]) )& , Flatten/@Permutations[Partition[gravitonParameters,3]] ]] 
];


End[];


EndPackage[];
