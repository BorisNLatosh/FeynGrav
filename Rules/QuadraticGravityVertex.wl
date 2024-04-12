(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["QuadraticGravityVertex`",{"FeynCalc`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


GravitonVertex::usage = "GravitonVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\[Ellipsis]},\[CurlyEpsilon]].";


GravitonGhostVertex::usage = "GravitonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis]},\[Mu],p1,\[Nu],p2].";


VertexCurvatureSquare::usage = "VertexCurvatureSquare[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)}].";


Begin["Private`"];


TakeLorenzIndices = indexArray |-> Flatten[(#[[;;2]]&)/@Partition[indexArray,3]];


GravitonVertex[indexArray_,\[CurlyEpsilon]_] := GravitonVertex[indexArray,\[CurlyEpsilon]] = Contract[ Total[ ( I (Global`\[Kappa])^(Length[#]/3-2) CTensorGeneral[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[#[[7;;]]]]FVD[#[[3]],\[Lambda]1]FVD[#[[6]],\[Lambda]2] (2 GammaTensor[\[Alpha],\[Mu],\[Rho],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Sigma],\[Nu],\[Beta],\[Lambda]2,#[[4]],#[[5]]] - 2 GammaTensor[\[Alpha],\[Mu],\[Nu],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda]2,#[[4]],#[[5]]] - (1/2) \[CurlyEpsilon] GammaTensor[\[Mu],\[Alpha],\[Beta],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Nu],\[Rho],\[Sigma],\[Lambda]2,#[[4]],#[[5]]]) )&/@( Flatten/@Permutations[Partition[indexArray,3]] ) ] ];


VertexCurvatureSquare[gravitonParameters_List] := VertexCurvatureSquare[gravitonParameters] = Switch[ Length[gravitonParameters]/3 ,
	2, CTensorGeneral[ {\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, TakeLorenzIndices[gravitonParameters[[;;-6]]]] FVD[gravitonParameters[[-1-3]],\[ScriptM]](FVD[gravitonParameters[[-1-3]],\[Lambda]1]GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]-FVD[gravitonParameters[[-1-3]],\[Lambda]1]GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]])FVD[gravitonParameters[[-1]],\[ScriptR]](FVD[gravitonParameters[[-1]],\[Lambda]2]GammaTensor[\[ScriptS],\[ScriptL],\[ScriptT],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]-FVD[gravitonParameters[[-1]],\[Lambda]2]GammaTensor[\[ScriptL],\[ScriptT],\[ScriptS],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]),
	3, CTensorGeneral[ {\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, TakeLorenzIndices[gravitonParameters[[;;-6]]]] FVD[gravitonParameters[[-1-3]],\[ScriptM]](FVD[gravitonParameters[[-1-3]],\[Lambda]1]GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]-FVD[gravitonParameters[[-1-3]],\[Lambda]1]GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]])FVD[gravitonParameters[[-1]],\[ScriptR]](FVD[gravitonParameters[[-1]],\[Lambda]2]GammaTensor[\[ScriptS],\[ScriptL],\[ScriptT],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]-FVD[gravitonParameters[[-1]],\[Lambda]2]GammaTensor[\[ScriptL],\[ScriptT],\[ScriptS],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]) + 2 CTensorGeneral[ {\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT],\[ScriptI],\[ScriptJ]}, TakeLorenzIndices[gravitonParameters[[;;-9]]]] FVD[gravitonParameters[[-1-3*2]],\[ScriptM]](FVD[gravitonParameters[[-1-3*2]],\[Lambda]1]GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[Lambda]1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]-FVD[gravitonParameters[[-1-3*2]],\[Lambda]1]GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[Lambda]1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]])( FVD[gravitonParameters[[-1-3*1]],\[Lambda]2]GammaTensor[\[ScriptR],\[ScriptL],\[ScriptI],\[Lambda]2,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]FVD[gravitonParameters[[-1-3*0]],\[Lambda]3]GammaTensor[\[ScriptS],\[ScriptT],\[ScriptJ],\[Lambda]3,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]] - FVD[gravitonParameters[[-1-3*1]],\[Lambda]2]GammaTensor[\[ScriptR],\[ScriptL],\[ScriptT],\[Lambda]2,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]FVD[gravitonParameters[[-1-3*0]],\[Lambda]3]GammaTensor[\[ScriptS],\[ScriptI],\[ScriptJ],\[Lambda]3,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]),
	4, CTensorGeneral[ {\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, TakeLorenzIndices[gravitonParameters[[;;-6]]]] FVD[gravitonParameters[[-1-3]],\[ScriptM]](FVD[gravitonParameters[[-1-3]],\[Lambda]1]GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]-FVD[gravitonParameters[[-1-3]],\[Lambda]1]GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]])FVD[gravitonParameters[[-1]],\[ScriptR]](FVD[gravitonParameters[[-1]],\[Lambda]2]GammaTensor[\[ScriptS],\[ScriptL],\[ScriptT],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]-FVD[gravitonParameters[[-1]],\[Lambda]2]GammaTensor[\[ScriptL],\[ScriptT],\[ScriptS],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]) + 2 CTensorGeneral[ {\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT],\[ScriptI],\[ScriptJ]}, TakeLorenzIndices[gravitonParameters[[;;-9]]]] FVD[gravitonParameters[[-1-3*2]],\[ScriptM]](FVD[gravitonParameters[[-1-3*2]],\[Lambda]1]GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[Lambda]1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]-FVD[gravitonParameters[[-1-3*2]],\[Lambda]1]GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[Lambda]1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]])( FVD[gravitonParameters[[-1-3*1]],\[Lambda]2]GammaTensor[\[ScriptR],\[ScriptL],\[ScriptI],\[Lambda]2,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]FVD[gravitonParameters[[-1-3*0]],\[Lambda]3]GammaTensor[\[ScriptS],\[ScriptT],\[ScriptJ],\[Lambda]3,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]] - FVD[gravitonParameters[[-1-3*1]],\[Lambda]2]GammaTensor[\[ScriptR],\[ScriptL],\[ScriptT],\[Lambda]2,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]FVD[gravitonParameters[[-1-3*0]],\[Lambda]3]GammaTensor[\[ScriptS],\[ScriptI],\[ScriptJ],\[Lambda]3,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]]) + CTensorGeneral[ {\[ScriptA]1,\[ScriptB]1,\[ScriptA]2,\[ScriptB]2,\[ScriptA]3,\[ScriptB]3,\[ScriptA]4,\[ScriptB]4,\[ScriptA]5,\[ScriptB]5,\[ScriptA]6,\[ScriptB]6}, TakeLorenzIndices[gravitonParameters[[;;-3*4]]]] (FVD[gravitonParameters[[-1-3*3]],\[Lambda]1]GammaTensor[\[ScriptA]1,\[ScriptA]2,\[ScriptA]3,\[Lambda]1,gravitonParameters[[-3-3*3]],gravitonParameters[[-2-3*3]]]FVD[gravitonParameters[[-1-3*2]],\[Lambda]2]GammaTensor[\[ScriptB]1,\[ScriptB]2,\[ScriptB]3,\[Lambda]2,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]] - FVD[gravitonParameters[[-1-3*3]],\[Lambda]1]GammaTensor[\[ScriptA]1,\[ScriptA]2,\[ScriptB]2,\[Lambda]1,gravitonParameters[[-3-3*3]],gravitonParameters[[-2-3*3]]]FVD[gravitonParameters[[-1-3*2]],\[Lambda]2]GammaTensor[\[ScriptB]1,\[ScriptA]3,\[ScriptB]3,\[Lambda]2,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]] )(FVD[gravitonParameters[[-1-3*1]],\[Lambda]3]GammaTensor[\[ScriptA]4,\[ScriptA]5,\[ScriptA]6,\[Lambda]3,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]FVD[gravitonParameters[[-1-3*0]],\[Lambda]4]GammaTensor[\[ScriptB]4,\[ScriptB]5,\[ScriptB]6,\[Lambda]4,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]] - FVD[gravitonParameters[[-1-3*1]],\[Lambda]3]GammaTensor[\[ScriptA]4,\[ScriptA]5,\[ScriptB]5,\[Lambda]3,gravitonParameters[[-3-3*1]],gravitonParameters[[-2-3*1]]]FVD[gravitonParameters[[-1-3*0]],\[Lambda]4]GammaTensor[\[ScriptB]4,\[ScriptA]6,\[ScriptB]6,\[Lambda]4,gravitonParameters[[-3-3*0]],gravitonParameters[[-2-3*0]]] )
]


Clear[GravitonGhostVertex1];

GravitonGhostVertex1[indexArray_,\[Mu]_,p1_,\[Nu]_,p2_] := GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2] = CTensorGeneral[{\[Alpha],\[Beta],\[Mu],\[Nu]},TakeLorenzIndices[indexArray]]FVD[p1,\[Alpha]]FVD[p2,\[Beta]] ;


Clear[GravitonGhostVertex2];

GravitonGhostVertex2[indexArray_,\[Mu]_,p1_,\[Nu]_,p2_] := GravitonGhostVertex2[indexArray,\[Mu],p1,\[Nu],p2] = Map[ (CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[#[[4;;]]]](-1) FVD[#[[3]],\[Lambda]]( FVD[p1,\[Sigma]]GammaTensor[\[Beta],\[Rho],\[Alpha],\[Lambda],#[[1]],#[[2]]]- FVD[p2,\[Sigma]]GammaTensor[\[Alpha],\[Rho],\[Beta],\[Lambda],#[[1]],#[[2]]]+FVD[#[[3]],\[Rho]]GammaTensor[\[Sigma],\[Alpha],\[Beta],\[Lambda],#[[1]],#[[2]]]- FVD[#[[3]],\[Alpha]]GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda],#[[1]],#[[2]]] ) )& , Flatten/@Permutations[Partition[indexArray,3]] ]//Total;


Clear[GravitonGhostVertex3];

GravitonGhostVertex3[indexArray_,\[Mu]_,p1_,\[Nu]_,p2_] := GravitonGhostVertex3[indexArray,\[Mu],p1,\[Nu],p2] = Map[ (CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},TakeLorenzIndices[#[[7;;]]]] (-1)FVD[#[[3]],\[Lambda]1]FVD[#[[6]],\[Lambda]2]( GammaTensor[\[Rho],\[Alpha],\[Lambda],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Sigma],\[Beta],\[Tau],\[Lambda]2,#[[4]],#[[5]]] - GammaTensor[\[Rho],\[Alpha],\[Beta],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Sigma],\[Lambda],\[Tau],\[Lambda]2,#[[4]],#[[5]]] + GammaTensor[\[Alpha],\[Rho],\[Lambda],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Beta],\[Sigma],\[Tau],\[Lambda]2,#[[4]],#[[5]]]) )& , Flatten/@Permutations[Partition[indexArray,3]] ] //Total;


Clear[GravitonGhostVertex];

GravitonGhostVertex[indexArray_,\[Mu]_,p1_,\[Nu]_,p2_] := GravitonGhostVertex[indexArray,\[Mu],p1,\[Nu],p2] = Switch[Length[indexArray]/3,
	0, I (Global`\[Kappa])^(Length[indexArray]/3) GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2] //Contract ,
	1, I (Global`\[Kappa])^(Length[indexArray]/3) ( GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2] + GravitonGhostVertex2[indexArray,\[Mu],p1,\[Nu],p2] ) //Contract,
	_, I (Global`\[Kappa])^(Length[indexArray]/3) ( GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2] + GravitonGhostVertex2[indexArray,\[Mu],p1,\[Nu],p2] + GravitonGhostVertex3[indexArray,\[Mu],p1,\[Nu],p2] ) //Contract
];


End[];


EndPackage[];
