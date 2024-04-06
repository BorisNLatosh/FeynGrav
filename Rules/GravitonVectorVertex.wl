(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["GravitonVectorVertex`",{"FeynCalc`","ITensor`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


GravitonMassiveVectorVertex::usage = "GravitonMassiveVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The function returns an expression for the gravitational vertex of a massive vector field kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vectors Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are vectors momenta, and m is the vector field mass.";


GravitonMassiveVectorVertexUncontracted::usage = "GravitonMassiveVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Nothing is contracted. The function returns an expression for the gravitational vertex of a massive vector field kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vectors Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are vectors momenta, and m is the vector field mass.";


GravitonVectorVertex::usage = "GravitonVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\[CurlyEpsilon]]. The function returns an expression for the gravitational vertex of a massless vector field kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are gravitons momenta, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vectors Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are vectors momenta.";


GravitonVectorVertexUncontracted::usage = "GravitonVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\[CurlyEpsilon]]. The function returns an expression for the gravitational vertex of a massless vector field kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are gravitons momenta, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vectors Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are vectors momenta.";


GravitonVectorGhostVertex::usage = "GravitonVectorGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. The function returns an expression for the graviton vertex of the Faddeev-Popov ghost kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are ghosts momenta.";


Begin["Private`"];


TakeLorenzIndices = indexArray |-> Flatten[(#[[;;2]]&)/@Partition[indexArray,3]];


FReduced = {\[Mu],\[Nu],\[Sigma],\[Lambda]} |-> MTD[\[Mu],\[Sigma]]MTD[\[Nu],\[Lambda]] - MTD[\[Nu],\[Sigma]]MTD[\[Mu],\[Lambda]] ;


Clear[GravitonMassiveVectorVertexUncontracted];

GravitonMassiveVectorVertexUncontracted[indexArray_,\[Lambda]1_,p1_,\[Lambda]2_,p2_,m_] := GravitonMassiveVectorVertexUncontracted[indexArray,\[Lambda]1,p1,\[Lambda]2,p2,m] = I (Global`\[Kappa])^(Length[indexArray]/2) ( (1/2) CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta]},indexArray] FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2]FReduced[\[Mu],\[Nu],\[Sigma]1,\[Lambda]1]FReduced[\[Alpha],\[Beta],\[Sigma]2,\[Lambda]2] + m^2 CTensorGeneral[{\[Lambda]1,\[Lambda]2},indexArray] ) ;


Clear[GravitonMassiveVectorVertex];

GravitonMassiveVectorVertex[indexArray_,\[Lambda]1_,p1_,\[Lambda]2_,p2_,m_] := GravitonMassiveVectorVertex[indexArray,\[Lambda]1,p1,\[Lambda]2,p2,m] = I (Global`\[Kappa])^(Length[indexArray]/2) ( (1/2) CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta]},indexArray] FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2]FReduced[\[Mu],\[Nu],\[Sigma]1,\[Lambda]1]FReduced[\[Alpha],\[Beta],\[Sigma]2,\[Lambda]2] + m^2 CTensorGeneral[{\[Lambda]1,\[Lambda]2},indexArray] ) //Contract;


(* Massless Vectors *)


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
	1, I (Global`\[Kappa])^(Length[indexArray]/3) ( CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta]},indexArray] 1/2 FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2]FReduced[\[Mu],\[Nu],\[Sigma]1,\[Lambda]1]FReduced[\[Alpha],\[Beta],\[Sigma]2,\[Lambda]2] - \[CurlyEpsilon] CTensorGeneral[{\[Sigma]1,\[Lambda]1,\[Sigma]2,\[Lambda]2},TakeLorenzIndices[indexArray]] FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2] + \[CurlyEpsilon] Total[ Map[ ( CTensorGeneral[{\[Mu],\[Nu],\[Mu]1,\[Lambda]1,\[Mu]2,\[Lambda]2},TakeLorenzIndices[#[[4;;]]]] (GammaTensor[\[Mu]1,\[Mu],\[Nu],\[Sigma],#[[1]],#[[2]]]FVD[#[[3]],\[Sigma]]FVD[p2,\[Mu]2] + GammaTensor[\[Mu]2,\[Mu],\[Nu],\[Sigma],#[[1]],#[[2]]]FVD[#[[3]],\[Sigma]]FVD[p1,\[Mu]1]) )& , Flatten/@Permutations[Partition[indexArray,3]] ] ] ) ,
	_, I (Global`\[Kappa])^(Length[indexArray]/3) ( CTensorGeneral[{\[Mu],\[Alpha],\[Nu],\[Beta]},indexArray] 1/2 FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2]FReduced[\[Mu],\[Nu],\[Sigma]1,\[Lambda]1]FReduced[\[Alpha],\[Beta],\[Sigma]2,\[Lambda]2] - \[CurlyEpsilon] CTensorGeneral[{\[Sigma]1,\[Lambda]1,\[Sigma]2,\[Lambda]2},TakeLorenzIndices[indexArray]] FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2] + \[CurlyEpsilon] Total[ Map[ ( CTensorGeneral[{\[Mu],\[Nu],\[Mu]1,\[Lambda]1,\[Mu]2,\[Lambda]2},TakeLorenzIndices[#[[4;;]]]] (GammaTensor[\[Mu]1,\[Mu],\[Nu],\[Sigma],#[[1]],#[[2]]]FVD[#[[3]],\[Sigma]]FVD[p2,\[Mu]2] + GammaTensor[\[Mu]2,\[Mu],\[Nu],\[Sigma],#[[1]],#[[2]]]FVD[#[[3]],\[Sigma]]FVD[p1,\[Mu]1]) )& , Flatten/@Permutations[Partition[indexArray,3]] ] ] + \[CurlyEpsilon] Total[ Map[ ( (-1/2) CTensorGeneral[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Mu]1,\[Lambda]1,\[Mu]2,\[Lambda]2},TakeLorenzIndices[#[[7;;]]]] (FVD[#[[3]],\[Tau]1]FVD[#[[6]],\[Tau]2]GammaTensor[\[Mu]1,\[Mu],\[Nu],\[Tau]1,#[[1]],#[[2]]] GammaTensor[\[Mu]2,\[Alpha],\[Beta],\[Tau]2,#[[4]],#[[5]]] + FVD[#[[3]],\[Tau]2]FVD[#[[6]],\[Tau]1]GammaTensor[\[Mu]1,\[Mu],\[Nu],\[Tau]1,#[[4]],#[[5]]] GammaTensor[\[Mu]2,\[Alpha],\[Beta],\[Tau]2,#[[1]],#[[2]]] ) )& , Flatten/@Permutations[Partition[indexArray,3]] ] ] ) 
	];


(* Ghost *)


Clear[GravitonVectorGhostVertex];

GravitonVectorGhostVertex[indexArray_,p1_,p2_] := GravitonVectorGhostVertex[indexArray,p1,p2] = - I (Global`\[Kappa])^(Length[indexArray]/2) FVD[p1,\[ScriptM]]FVD[p2,\[ScriptN]] CTensorGeneral[{\[ScriptM],\[ScriptN]},indexArray] //Contract;


End[];


EndPackage[];
