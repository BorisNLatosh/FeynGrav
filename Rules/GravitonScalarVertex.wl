(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["GravitonScalarVertex`",{"FeynCalc`","CTensorGeneral`","indexArraySymmetrization`"}];


GravitonScalarVertex::usage = "GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The expression for a vertex describing a coupling of n gravitons to the scalar field kinetic term. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\) are scalar field momenta; m is the scalar field mass.";


GravitonScalarPotentialVertex::usage = "GravitonScalarPotentialVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\[Lambda]]. The expression for a vertex describing a coupling of n gravitons to the scalar field power law potential \[Lambda] \!\(\*FractionBox[SuperscriptBox[\(\[Phi]\), \(n\)], \(n!\)]\). Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \[Lambda] is the coupling.";


GravitonScalarVertexUncontracted::usage = "GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The expression for a vertex describing a coupling of n gravitons to the scalar field kinetic term. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(1\)]\), \!\(\*SubscriptBox[\(p\), \(2\)]\) are scalar field momenta; m is the scalar field mass. No contraction or simplification is carried out.";


GravitonScalarPotentialVertexUncontracted::usage = "GravitonScalarPotentialVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\[Lambda]]. The expression for a vertex describing a coupling of n gravitons to the scalar field power law potential \[Lambda] \!\(\*FractionBox[SuperscriptBox[\(\[Phi]\), \(n\)], \(n!\)]\). Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \[Lambda] is the coupling. No contraction or simplification is carried out.";


Begin["Private`"];


(* Kinetic term. *)


Clear[GravitonScalarVertexUncontracted];

GravitonScalarVertexUncontracted[indexArray_,p1_,p2_,m_] := GravitonScalarVertexUncontracted[indexArray,p1,p2,m] = I (Global`\[Kappa])^(Length[indexArray]/2) ( - (1/2) (FVD[p1,Global`\[ScriptM]]FVD[p2,Global`\[ScriptN]]+FVD[p1,Global`\[ScriptN]]FVD[p2,Global`\[ScriptM]])CTensorGeneral[{Global`\[ScriptM],Global`\[ScriptN]},indexArray] - m^2 CTensorGeneral[{},indexArray] ) ;


Clear[GravitonScalarVertex];

GravitonScalarVertex[indexArray_,p1_,p2_,m_] := GravitonScalarVertex[indexArray,p1,p2,m] = I (Global`\[Kappa])^(Length[indexArray]/2) ( - (1/2) Contract[(FVD[p1,\[ScriptM]]FVD[p2,\[ScriptN]]+FVD[p1,\[ScriptN]]FVD[p2,\[ScriptM]])CTensorGeneral[{\[ScriptM],\[ScriptN]},indexArray]] - m^2 FeynCalcInternal[CTensorGeneral[{},indexArray]] ) //Expand;


(* Power law potential. *)


Clear[GravitonScalarPotentialVertexUncontracted];

GravitonScalarPotentialVertexUncontracted[indexArray_,\[Lambda]_] := GravitonScalarPotentialVertexUncontracted[indexArray,\[Lambda]] = I (Global`\[Kappa])^(Length[indexArray]/2) \[Lambda] CTensorGeneral[{},indexArray] ;


Clear[GravitonScalarPotentialVertex];

GravitonScalarPotentialVertex[indexArray_,\[Lambda]_] := GravitonScalarPotentialVertex[indexArray,\[Lambda]] = I (Global`\[Kappa])^(Length[indexArray]/2) \[Lambda] FeynCalcInternal[CTensorGeneral[{},indexArray]] //Expand;


End[];


EndPackage[];
