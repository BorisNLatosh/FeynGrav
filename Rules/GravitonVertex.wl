(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["GravitonVertex`",{"FeynCalc`","ITensor`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


GravitonVertex::usage = "GravitonVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(n\)]\)},\[CurlyEpsilon]]. The expression for a vertex describing a coupling of n gravitons within general relativity. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a graviton.";


GravitonVertexUncontracted::usage = "GravitonVertexUncontracted[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(n\)]\)},\[CurlyEpsilon]]. The expression for a vertex describing a coupling of n gravitons within general relativity. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a graviton. No contraction or simplification is carried out.";


Begin["Private`"];


(* Supplementary functions. *)


TakeLorenzIndices = indexArray |-> Flatten[(#[[;;2]]&)/@Partition[indexArray,3]];


(* Graviton Vertex *)


GravitonVertex[indexArray_,\[CurlyEpsilon]_] := GravitonVertex[indexArray,\[CurlyEpsilon]] = Contract[ Total[ ( I (Global`\[Kappa])^(Length[#]/3-2) CTensorGeneral[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[#[[7;;]]]]FVD[#[[3]],\[Lambda]1]FVD[#[[6]],\[Lambda]2] (2 GammaTensor[\[Alpha],\[Mu],\[Rho],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Sigma],\[Nu],\[Beta],\[Lambda]2,#[[4]],#[[5]]] - 2 GammaTensor[\[Alpha],\[Mu],\[Nu],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda]2,#[[4]],#[[5]]] - (1/2) \[CurlyEpsilon] GammaTensor[\[Mu],\[Alpha],\[Beta],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Nu],\[Rho],\[Sigma],\[Lambda]2,#[[4]],#[[5]]]) )&/@( Flatten/@Permutations[Partition[indexArray,3]] ) ] ];


GravitonVertexUncontracted[indexArray_,\[CurlyEpsilon]_] := GravitonVertexUncontracted[indexArray,\[CurlyEpsilon]] = Total[ ( I (Global`\[Kappa])^(Length[#]/3-2) CTensorGeneral[{Global`\[Mu],Global`\[Nu],Global`\[Alpha],Global`\[Beta],Global`\[Rho],Global`\[Sigma]},TakeLorenzIndices[#[[7;;]]]]FVD[#[[3]],Global`\[Lambda]1]FVD[#[[6]],Global`\[Lambda]2] (2 GammaTensor[Global`\[Alpha],Global`\[Mu],Global`\[Rho],Global`\[Lambda]1,#[[1]],#[[2]]]GammaTensor[Global`\[Sigma],Global`\[Nu],Global`\[Beta],Global`\[Lambda]2,#[[4]],#[[5]]] - 2 GammaTensor[Global`\[Alpha],Global`\[Mu],Global`\[Nu],Global`\[Lambda]1,#[[1]],#[[2]]]GammaTensor[Global`\[Rho],Global`\[Beta],Global`\[Sigma],Global`\[Lambda]2,#[[4]],#[[5]]] - (1/2) \[CurlyEpsilon] GammaTensor[Global`\[Mu],Global`\[Alpha],Global`\[Beta],Global`\[Lambda]1,#[[1]],#[[2]]]GammaTensor[Global`\[Nu],Global`\[Rho],Global`\[Sigma],Global`\[Lambda]2,#[[4]],#[[5]]]) )&/@( Flatten/@Permutations[Partition[indexArray,3]] ) ] ;


End[];


EndPackage[];
