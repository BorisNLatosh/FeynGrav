(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["GravitonVectorVertex`",{"FeynCalc`","ITensor`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


GravitonMassiveVectorVertex::usage = "GravitonMassiveVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The expression for a vertex describing a coupling of n gravitons to the Proca field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) is the Lorentz index of a vector; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a vector; m is the Proca field mass.";


GravitonMassiveVectorVertexUncontracted::usage = "GravitonMassiveVectorVertexUncontracted[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The expression for a vertex describing a coupling of n gravitons to the Proca field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) is the Lorentz index of a vector; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a vector; m is the Proca field mass. No contraction or simplification is carried out.";


GravitonVectorVertex::usage = "GravitonVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\[CurlyEpsilon]]. The expression for a vertex describing a coupling of n gravitons to a massless vector field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(k\), \(i\)]\) is the momentum of a graviton; \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) is the Lorentz index of a vector; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a vector; \[CurlyEpsilon] is the gauge fixing parameter.";


GravitonVectorVertexUncontracted::usage = "GravitonVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\[CurlyEpsilon]]. The expression for a vertex describing a coupling of n gravitons to a massless vector field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(k\), \(i\)]\) is the momentum of a graviton; \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) is the Lorentz index of a vector; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a vector; \[CurlyEpsilon] is the gauge fixing parameter. No contraction or simplification is carried out.";


GravitonVectorGhostVertex::usage = "GravitonVectorGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. The expression for a vertex describing a coupling of n gravitons to a ghost for the massless vector field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a ghost.";


GravitonVectorGhostVertexUncontracted::usage = "GravitonVectorGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. The expression for a vertex describing a coupling of n gravitons to a ghost for the massless vector field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a ghost. No contraction or simplification is carried out.";


Begin["Private`"];


(* Supplementary functions. *)


TakeLorenzIndices = indexArray |-> Flatten[(#[[;;2]]&)/@Partition[indexArray,3]];


FReduced = {\[Mu],\[Nu],\[Sigma],\[Lambda]} |-> MTD[\[Mu],\[Sigma]]MTD[\[Nu],\[Lambda]] - MTD[\[Nu],\[Sigma]]MTD[\[Mu],\[Lambda]] ;


(* Proca field. *)


Clear[GravitonMassiveVectorVertexUncontracted];

GravitonMassiveVectorVertexUncontracted[indexArray_,\[Lambda]1_,p1_,\[Lambda]2_,p2_,m_] := GravitonMassiveVectorVertexUncontracted[indexArray,\[Lambda]1,p1,\[Lambda]2,p2,m] = I (Global`\[Kappa])^(Length[indexArray]/2) ( (1/2) CTensorGeneral[{Global`\[Mu],Global`\[Alpha],Global`\[Nu],Global`\[Beta]},indexArray] FVD[p1,Global`\[Sigma]1]FVD[p2,Global`\[Sigma]2]FReduced[Global`\[Mu],Global`\[Nu],Global`\[Sigma]1,\[Lambda]1]FReduced[Global`\[Alpha],Global`\[Beta],Global`\[Sigma]2,\[Lambda]2] + m^2 CTensorGeneral[{\[Lambda]1,\[Lambda]2},indexArray] ) ;


Clear[GravitonMassiveVectorVertex];

GravitonMassiveVectorVertex[indexArray_,\[Lambda]1_,p1_,\[Lambda]2_,p2_,m_] := GravitonMassiveVectorVertex[indexArray,\[Lambda]1,p1,\[Lambda]2,p2,m] = I (Global`\[Kappa])^(Length[indexArray]/2) ( (1/2) CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta]},indexArray] FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2]FReduced[\[Mu],\[Nu],\[Sigma]1,\[Lambda]1]FReduced[\[Alpha],\[Beta],\[Sigma]2,\[Lambda]2] + m^2 CTensorGeneral[{\[Lambda]1,\[Lambda]2},indexArray] ) //Contract;


(* Massless vector field. *)


Clear[GravitonVectorVertex1];

GravitonVectorVertex1[indexArray_,\[Lambda]1_,p1_,\[Lambda]2_,p2_] := GravitonVectorVertex1[indexArray,\[Lambda]1,p1,\[Lambda]2,p2] = CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta]},indexArray] 1/2 FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2]FReduced[\[Mu],\[Nu],\[Sigma]1,\[Lambda]1]FReduced[\[Alpha],\[Beta],\[Sigma]2,\[Lambda]2] ;


Clear[GravitonVectorVertex3];

GravitonVectorVertex3[indexArray_,\[Lambda]1_,p1_,\[Lambda]2_,p2_] := GravitonVectorVertex3[indexArray,\[Lambda]1,p1,\[Lambda]2,p2] =  (-1) CTensorGeneral[{\[Sigma]1,\[Lambda]1,\[Sigma]2,\[Lambda]2},TakeLorenzIndices[indexArray]] FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2]  ;


Clear[GravitonVectorVertex4];

GravitonVectorVertex4[indexArray_,\[Lambda]1_,p1_,\[Lambda]2_,p2_] := GravitonVectorVertex4[indexArray,\[Lambda]1,p1,\[Lambda]2,p2] = Map[ ( CTensorGeneral[{\[Mu],\[Nu],\[Mu]1,\[Lambda]1,\[Mu]2,\[Lambda]2},TakeLorenzIndices[#[[4;;]]]] (GammaTensor[\[Mu]1,\[Mu],\[Nu],\[Sigma],#[[1]],#[[2]]]FVD[#[[3]],\[Sigma]]FVD[p2,\[Mu]2] + GammaTensor[\[Mu]2,\[Mu],\[Nu],\[Sigma],#[[1]],#[[2]]]FVD[#[[3]],\[Sigma]]FVD[p1,\[Mu]1]) )& , Flatten/@Permutations[Partition[indexArray,3]] ]//Total;


Clear[GravitonVectorVertex5];

GravitonVectorVertex5[indexArray_,\[Lambda]1_,p1_,\[Lambda]2_,p2_] := GravitonVectorVertex5[indexArray,\[Lambda]1,p1,\[Lambda]2,p2] = Map[ ( (-1/2) CTensorGeneral[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Mu]1,\[Lambda]1,\[Mu]2,\[Lambda]2},TakeLorenzIndices[#[[7;;]]]] (FVD[#[[3]],\[Tau]1]FVD[#[[6]],\[Tau]2]GammaTensor[\[Mu]1,\[Mu],\[Nu],\[Tau]1,#[[1]],#[[2]]] GammaTensor[\[Mu]2,\[Alpha],\[Beta],\[Tau]2,#[[4]],#[[5]]] + FVD[#[[3]],\[Tau]2]FVD[#[[6]],\[Tau]1]GammaTensor[\[Mu]1,\[Mu],\[Nu],\[Tau]1,#[[4]],#[[5]]] GammaTensor[\[Mu]2,\[Alpha],\[Beta],\[Tau]2,#[[1]],#[[2]]] ) )& , Flatten/@Permutations[Partition[indexArray,3]] ]//Total;


Clear[GravitonVectorVertex];

GravitonVectorVertex[indexArray_,\[Lambda]1_,p1_,\[Lambda]2_,p2_,\[CurlyEpsilon]_] := GravitonVectorVertex[indexArray,\[Lambda]1,p1,\[Lambda]2,p2,\[CurlyEpsilon]] = Switch[Length[indexArray]/3,
	0, I (Global`\[Kappa])^(Length[indexArray]/3) ( GravitonVectorVertex1[TakeLorenzIndices[indexArray],\[Lambda]1,p1,\[Lambda]2,p2] + \[CurlyEpsilon] GravitonVectorVertex3[indexArray,\[Lambda]1,p1,\[Lambda]2,p2] ) //Contract,
	1, I (Global`\[Kappa])^(Length[indexArray]/3) ( GravitonVectorVertex1[TakeLorenzIndices[indexArray],\[Lambda]1,p1,\[Lambda]2,p2] + \[CurlyEpsilon] GravitonVectorVertex3[indexArray,\[Lambda]1,p1,\[Lambda]2,p2] + \[CurlyEpsilon] GravitonVectorVertex4[indexArray,\[Lambda]1,p1,\[Lambda]2,p2] ) //Contract,
	_, I (Global`\[Kappa])^(Length[indexArray]/3) ( GravitonVectorVertex1[TakeLorenzIndices[indexArray],\[Lambda]1,p1,\[Lambda]2,p2] + \[CurlyEpsilon] GravitonVectorVertex3[indexArray,\[Lambda]1,p1,\[Lambda]2,p2] + \[CurlyEpsilon] GravitonVectorVertex4[indexArray,\[Lambda]1,p1,\[Lambda]2,p2] + \[CurlyEpsilon] GravitonVectorVertex5[indexArray,\[Lambda]1,p1,\[Lambda]2,p2] ) //Contract];


Clear[GravitonVectorVertexUncontracted];

GravitonVectorVertexUncontracted[indexArray_,\[Lambda]1_,p1_,\[Lambda]2_,p2_,\[CurlyEpsilon]_] := GravitonVectorVertexUncontracted[indexArray,\[Lambda]1,p1,\[Lambda]2,p2,\[CurlyEpsilon]] = Switch[Length[indexArray]/3,
	1, I (Global`\[Kappa])^(Length[indexArray]/3) ( CTensorGeneral[{Global`\[Mu],Global`\[Alpha],Global`\[Nu],Global`\[Beta]},indexArray] 1/2 FVD[p1,Global`\[Sigma]1]FVD[p2,Global`\[Sigma]2]FReduced[Global`\[Mu],Global`\[Nu],Global`\[Sigma]1,\[Lambda]1]FReduced[Global`\[Alpha],Global`\[Beta],Global`\[Sigma]2,\[Lambda]2] - \[CurlyEpsilon] CTensorGeneral[{Global`\[Sigma]1,\[Lambda]1,Global`\[Sigma]2,\[Lambda]2},TakeLorenzIndices[indexArray]] FVD[p1,Global`\[Sigma]1]FVD[p2,Global`\[Sigma]2] + \[CurlyEpsilon] Total[ Map[ ( CTensorGeneral[{Global`\[Mu],Global`\[Nu],Global`\[Mu]1,\[Lambda]1,Global`\[Mu]2,\[Lambda]2},TakeLorenzIndices[#[[4;;]]]] (GammaTensor[Global`\[Mu]1,Global`\[Mu],Global`\[Nu],Global`\[Sigma],#[[1]],#[[2]]]FVD[#[[3]],Global`\[Sigma]]FVD[p2,Global`\[Mu]2] + GammaTensor[Global`\[Mu]2,Global`\[Mu],Global`\[Nu],Global`\[Sigma],#[[1]],#[[2]]]FVD[#[[3]],Global`\[Sigma]]FVD[p1,Global`\[Mu]1]) )& , Flatten/@Permutations[Partition[indexArray,3]] ] ] ) ,
	_, I (Global`\[Kappa])^(Length[indexArray]/3) ( CTensorGeneral[{Global`\[Mu],Global`\[Alpha],Global`\[Nu],Global`\[Beta]},indexArray] 1/2 FVD[p1,Global`\[Sigma]1]FVD[p2,Global`\[Sigma]2]FReduced[Global`\[Mu],Global`\[Nu],Global`\[Sigma]1,\[Lambda]1]FReduced[Global`\[Alpha],Global`\[Beta],Global`\[Sigma]2,\[Lambda]2] - \[CurlyEpsilon] CTensorGeneral[{Global`\[Sigma]1,\[Lambda]1,Global`\[Sigma]2,\[Lambda]2},TakeLorenzIndices[indexArray]] FVD[p1,Global`\[Sigma]1]FVD[p2,Global`\[Sigma]2] + \[CurlyEpsilon] Total[ Map[ ( CTensorGeneral[{Global`\[Mu],Global`\[Nu],Global`\[Mu]1,\[Lambda]1,Global`\[Mu]2,\[Lambda]2},TakeLorenzIndices[#[[4;;]]]] (GammaTensor[Global`\[Mu]1,Global`\[Mu],Global`\[Nu],Global`\[Sigma],#[[1]],#[[2]]]FVD[#[[3]],Global`\[Sigma]]FVD[p2,Global`\[Mu]2] + GammaTensor[Global`\[Mu]2,Global`\[Mu],Global`\[Nu],Global`\[Sigma],#[[1]],#[[2]]]FVD[#[[3]],Global`\[Sigma]]FVD[p1,Global`\[Mu]1]) )& , Flatten/@Permutations[Partition[indexArray,3]] ] ] + \[CurlyEpsilon] Total[ Map[ ( (-1/2) CTensorGeneral[{Global`\[Mu],Global`\[Nu],Global`\[Alpha],Global`\[Beta],Global`\[Mu]1,\[Lambda]1,Global`\[Mu]2,\[Lambda]2},TakeLorenzIndices[#[[7;;]]]] (FVD[#[[3]],Global`\[Tau]1]FVD[#[[6]],Global`\[Tau]2]GammaTensor[Global`\[Mu]1,Global`\[Mu],Global`\[Nu],Global`\[Tau]1,#[[1]],#[[2]]] GammaTensor[Global`\[Mu]2,Global`\[Alpha],Global`\[Beta],Global`\[Tau]2,#[[4]],#[[5]]] + FVD[#[[3]],Global`\[Tau]2]FVD[#[[6]],Global`\[Tau]1]GammaTensor[Global`\[Mu]1,Global`\[Mu],Global`\[Nu],Global`\[Tau]1,#[[4]],#[[5]]] GammaTensor[Global`\[Mu]2,Global`\[Alpha],Global`\[Beta],Global`\[Tau]2,#[[1]],#[[2]]] ) )& , Flatten/@Permutations[Partition[indexArray,3]] ] ] ) 
	];


(* Faddeev-Popov ghosts for the massless vector field. *)


Clear[GravitonVectorGhostVertex];

GravitonVectorGhostVertex[indexArray_,p1_,p2_] := GravitonVectorGhostVertex[indexArray,p1,p2] = - I (Global`\[Kappa])^(Length[indexArray]/2) FVD[p1,\[ScriptM]]FVD[p2,\[ScriptN]] CTensorGeneral[{\[ScriptM],\[ScriptN]},indexArray] //Contract;


Clear[GravitonVectorGhostVertexUncontracted];

GravitonVectorGhostVertexUncontracted[indexArray_,p1_,p2_] := GravitonVectorGhostVertexUncontracted[indexArray,p1,p2] = - I (Global`\[Kappa])^(Length[indexArray]/2) FVD[p1,Global`\[Mu]]FVD[p2,Global`\[Nu]] CTensorGeneral[{Global`\[Mu],Global`\[Nu]},indexArray] ;


End[];


EndPackage[];
