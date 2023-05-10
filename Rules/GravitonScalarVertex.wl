(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonScalarVertex`",{"FeynCalc`","ITensorPlain`","CTensorPlain`","CITensorPlain`"}];

GravitonScalarVertex::usage = "GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The function returns an expression for the gravitational vertex of a scalar field kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta, and m is the scalar field mass.";

GravitonScalarPotentialVertex::usage = "GravitonScalarPotentialVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\)]. The function The function returns an expression for the gravitational vertex of a scalar field potential energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton Lorentz indices and \!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\) is the scalar field interaction couplign.";

Begin["Private`"];
(* Vertex for the scalar field kinetic energy. *)
GravitonScalarVertex = {indexArray,p1,p2,m}|->I (FeynGrav`\[Kappa])^(Length[indexArray]/2) 1/Power[2,Length[indexArray]/2] 1/Factorial[Length[indexArray]/2] Total[(- (1/2)(FVD[p1,\[ScriptM]]FVD[p2,\[ScriptN]]+FVD[p1,\[ScriptN]]FVD[p2,\[ScriptM]])CITensorPlain[{\[ScriptM],\[ScriptN]},#] - m^2 CTensorPlain[#])&/@indexArraySymmetrization[indexArray]]//Expand//Contract;
(* Vertex for the scalar field potential energy. *)
GravitonScalarPotentialVertex = {indexArray,\[Lambda]}|-> I (FeynGrav`\[Kappa])^(Length[indexArray]/2) \[Lambda] 1/Power[2,Length[indexArray]/2] 1/Factorial[Length[indexArray]/2] Total[CTensorPlain/@indexArraySymmetrization[indexArray]]//Expand//Contract;
(* This command prepares an array of indices that respect all symmetries. First, it prepares an array with is symmetric with respect to the graviton line permutations. Then it symmetrizes is with respect to each index pair. *)
indexArraySymmetrization = indexArray |-> Partition[Flatten[ Fold[Join[#1,#1/.{#2[[1]]->#2[[2]],#2[[2]]->#2[[1]]}]&,#,Partition[#,2]]&/@(Flatten/@Permutations[Partition[indexArray,2]]) ],Length[indexArray]];

End[];

EndPackage[];
