(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["GravitonVertex`",{"FeynCalc`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


GravitonVertex::usage = "GravitonVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(n\)]\)},\[CurlyEpsilon]]. The expression for a vertex describing a coupling of n gravitons within general relativity. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a graviton.";


GravitonVertexUncontracted::usage = "GravitonVertexUncontracted[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(n\)]\)},\[CurlyEpsilon]]. The expression for a vertex describing a coupling of n gravitons within general relativity. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a graviton. No contraction or simplification is carried out.";


GravitonGhostVertex::usage = "GravitonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\[Mu],p1,\[Nu],p2]. The expression for a vertex describing a coupling of n gravitons to its own ghost. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a graviton; \[Mu],\[Nu] are Lorentz index of ghosts; \!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\) are momenta of gohsts.";


GravitonGhostVertexUncontracted::usage = "GravitonGhostVertexUncontracted[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\[Mu],p1,\[Nu],p2]. The expression for a vertex describing a coupling of n gravitons to its own ghost. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a graviton; \[Mu],\[Nu] are Lorentz index of ghosts; \!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\) are momenta of gohsts.";


Begin["Private`"];


(* Supplementary functions. *)


TakeLorenzIndices = indexArray |-> Flatten[(#[[;;2]]&)/@Partition[indexArray,3]];


(* Graviton Vertex *)


GravitonVertex[indexArray_,\[CurlyEpsilon]_] := GravitonVertex[indexArray,\[CurlyEpsilon]] = Contract[ Total[ ( I (Global`\[Kappa])^(Length[#]/3-2) CTensorGeneral[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[#[[7;;]]]]FVD[#[[3]],\[Lambda]1]FVD[#[[6]],\[Lambda]2] (2 GammaTensor[\[Alpha],\[Mu],\[Rho],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Sigma],\[Nu],\[Beta],\[Lambda]2,#[[4]],#[[5]]] - 2 GammaTensor[\[Alpha],\[Mu],\[Nu],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda]2,#[[4]],#[[5]]] - (1/2) \[CurlyEpsilon] GammaTensor[\[Mu],\[Alpha],\[Beta],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Nu],\[Rho],\[Sigma],\[Lambda]2,#[[4]],#[[5]]]) )&/@( Flatten/@Permutations[Partition[indexArray,3]] ) ] ];


GravitonVertexUncontracted[indexArray_,\[CurlyEpsilon]_] := GravitonVertexUncontracted[indexArray,\[CurlyEpsilon]] = Total[ ( I (Global`\[Kappa])^(Length[#]/3-2) CTensorGeneral[{Global`\[Mu],Global`\[Nu],Global`\[Alpha],Global`\[Beta],Global`\[Rho],Global`\[Sigma]},TakeLorenzIndices[#[[7;;]]]]FVD[#[[3]],Global`\[Lambda]1]FVD[#[[6]],Global`\[Lambda]2] (2 GammaTensor[Global`\[Alpha],Global`\[Mu],Global`\[Rho],Global`\[Lambda]1,#[[1]],#[[2]]]GammaTensor[Global`\[Sigma],Global`\[Nu],Global`\[Beta],Global`\[Lambda]2,#[[4]],#[[5]]] - 2 GammaTensor[Global`\[Alpha],Global`\[Mu],Global`\[Nu],Global`\[Lambda]1,#[[1]],#[[2]]]GammaTensor[Global`\[Rho],Global`\[Beta],Global`\[Sigma],Global`\[Lambda]2,#[[4]],#[[5]]] - (1/2) \[CurlyEpsilon] GammaTensor[Global`\[Mu],Global`\[Alpha],Global`\[Beta],Global`\[Lambda]1,#[[1]],#[[2]]]GammaTensor[Global`\[Nu],Global`\[Rho],Global`\[Sigma],Global`\[Lambda]2,#[[4]],#[[5]]]) )&/@( Flatten/@Permutations[Partition[indexArray,3]] ) ] ;


(* Faddeev-Popov ghosts for the graviton field. *)


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
	1, I (Global`\[Kappa])^(Length[gravitonParameters]/3)(CTensorGeneral[{Global`\[Alpha],Global`\[Beta],\[Mu],\[Nu]},TakeLorenzIndices[gravitonParameters]]FVD[p1,Global`\[Alpha]]FVD[p2,Global`\[Beta]]) - I (Global`\[Kappa])^(Length[gravitonParameters]/3)Total[Map[ (CTensorGeneral[{\[Mu],Global`\[Alpha],\[Nu],Global`\[Beta],Global`\[Rho],Global`\[Sigma]},TakeLorenzIndices[#[[4;;]]]] FVD[#[[3]],Global`\[Lambda]]( FVD[p1,Global`\[Sigma]]GammaTensor[Global`\[Beta],Global`\[Rho],Global`\[Alpha],Global`\[Lambda],#[[1]],#[[2]]]- FVD[p2,Global`\[Sigma]]GammaTensor[Global`\[Alpha],Global`\[Rho],Global`\[Beta],Global`\[Lambda],#[[1]],#[[2]]]+FVD[#[[3]],Global`\[Rho]]GammaTensor[Global`\[Sigma],Global`\[Alpha],Global`\[Beta],Global`\[Lambda],#[[1]],#[[2]]]- FVD[#[[3]],Global`\[Alpha]]GammaTensor[Global`\[Rho],Global`\[Beta],Global`\[Sigma],Global`\[Lambda],#[[1]],#[[2]]] ) )&, Flatten/@Permutations[Partition[gravitonParameters,3]] ]],
	_, I (Global`\[Kappa])^(Length[gravitonParameters]/3)(CTensorGeneral[{Global`\[Alpha],Global`\[Beta],\[Mu],\[Nu]},TakeLorenzIndices[gravitonParameters]]FVD[p1,Global`\[Alpha]]FVD[p2,Global`\[Beta]]) - I (Global`\[Kappa])^(Length[gravitonParameters]/3)Total[Map[ (CTensorGeneral[{\[Mu],Global`\[Alpha],\[Nu],Global`\[Beta],Global`\[Rho],Global`\[Sigma]},TakeLorenzIndices[#[[4;;]]]] FVD[#[[3]],Global`\[Lambda]]( FVD[p1,Global`\[Sigma]]GammaTensor[Global`\[Beta],Global`\[Rho],Global`\[Alpha],Global`\[Lambda],#[[1]],#[[2]]]- FVD[p2,Global`\[Sigma]]GammaTensor[Global`\[Alpha],Global`\[Rho],Global`\[Beta],Global`\[Lambda],#[[1]],#[[2]]]+FVD[#[[3]],Global`\[Rho]]GammaTensor[Global`\[Sigma],Global`\[Alpha],Global`\[Beta],Global`\[Lambda],#[[1]],#[[2]]]- FVD[#[[3]],Global`\[Alpha]]GammaTensor[Global`\[Rho],Global`\[Beta],Global`\[Sigma],Global`\[Lambda],#[[1]],#[[2]]] ) )&, Flatten/@Permutations[Partition[gravitonParameters,3]] ]] - I (Global`\[Kappa])^(Length[gravitonParameters]/3) Total[Map[ (CTensorGeneral[{\[Mu],Global`\[Alpha],\[Nu],Global`\[Beta],Global`\[Rho],Global`\[Sigma],Global`l,Global`t},TakeLorenzIndices[#[[7;;]]]] FVD[#[[3]],Global`l1]FVD[#[[6]],Global`l2]( GammaTensor[Global`\[Rho],Global`\[Alpha],Global`l,Global`l1,#[[1]],#[[2]]]GammaTensor[Global`\[Sigma],Global`\[Beta],Global`t,Global`l2,#[[4]],#[[5]]] - GammaTensor[Global`\[Rho],Global`\[Alpha],Global`\[Beta],Global`l1,#[[1]],#[[2]]]GammaTensor[Global`\[Sigma],Global`l,Global`t,Global`l2,#[[4]],#[[5]]] + GammaTensor[Global`\[Alpha],Global`\[Rho],Global`l,Global`l1,#[[1]],#[[2]]]GammaTensor[Global`\[Beta],Global`\[Sigma],Global`t,Global`l2,#[[4]],#[[5]]]) )& , Flatten/@Permutations[Partition[gravitonParameters,3]] ]] 
];


End[];


EndPackage[];
