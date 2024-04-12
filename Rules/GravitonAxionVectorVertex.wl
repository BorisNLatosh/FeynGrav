(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["GravitonAxionVectorVertex`",{"FeynCalc`","CTensorGeneral`"}];


GravitonAxionVectorVertex::usage = "GravitonAxionVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(q\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(q\), \(2\)]\),\[Theta]]. The function returns the gravitational vertex for coupling of scalar axions to the U(1) field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons, \!\(\*SubscriptBox[\(q\), \(i\)]\) are momenta of vectors, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vector Lorentz indices, and \[Theta] is the coupling.";


GravitonAxionVectorVertexUncontracted::usage = "GravitonAxionVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(q\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(q\), \(2\)]\),\[Theta]]. The function returns the gravitational vertex for coupling of scalar axions to the U(1) field. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz indices of gravitons, \!\(\*SubscriptBox[\(q\), \(i\)]\) are momenta of vectors, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vector Lorentz indices, and \[Theta] is the coupling.";


Begin["Private`"];


Clear[GravitonAxionVectorVertex];

GravitonAxionVectorVertex[indexArray_,\[Lambda]1_,q1_,\[Lambda]2_,q2_,\[Theta]_] := GravitonAxionVectorVertex[indexArray,\[Lambda]1,q1,\[Lambda]2,q2,\[Theta]] = - I (Global`\[Kappa])^(Length[indexArray]/2) \[Theta] FeynCalcInternal[CTensorGeneral[{},indexArray]] LC[\[Tau]1,\[Lambda]1,\[Tau]2,\[Lambda]2] FV[q1,\[Tau]1]FV[q2,\[Tau]2] //Contract;


Clear[GravitonAxionVectorVertexUncontracted];

GravitonAxionVectorVertexUncontracted[indexArray_,\[Lambda]1_,q1_,\[Lambda]2_,q2_,\[Theta]_] := GravitonAxionVectorVertexUncontracted[indexArray,\[Lambda]1,q1,\[Lambda]2,q2,\[Theta]] = - I (Global`\[Kappa])^(Length[indexArray]/2) \[Theta] CTensorGeneral[{},indexArray] LC[\[Tau]1,\[Lambda]1,\[Tau]2,\[Lambda]2] FV[q1,\[Tau]1]FV[q2,\[Tau]2] ;


End[];


EndPackage[];
