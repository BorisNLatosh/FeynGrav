(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonSUNYM`",{"FeynCalc`","CTensor`","CETensorC`","CITensorC`","CIITensorC`","GravitonFermionVertex`","GravitonVectorVertex`"}];

GravitonQuarkGluonVertex::usage = "GravitonQuarkGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\[Lambda],a}]. The function returns an expression for the gravitational vertex for quark-gluon vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\[Lambda],a} are the quark-gluon vertex parameters.";

GravitonThreeGluonVertex::usage = "GravitonThreeGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\)]. The function returns an expression for the gravitational vertex of three-gluon vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluons parameters.";

GravitonFourGluonVertex::usage = "GravitonFourGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(4\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(4\)]\),\!\(\*SubscriptBox[\(a\), \(4\)]\)]. The function returns an expression for the gravitational vertex of four-gluon vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluons parameters.";

GravitonGluonVertex::usage = "GravitonGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)]. The function returns an expression for the gravitational vertex for gluon kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(k\), \(i\)]\) are gravitons momenta, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluons parameters.}";

GravitonYMGhostVertex::usage = "GravitonYMGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)]. The function returns an expression for the gravitational vertex for ghosts. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are ghost parameters.";

GravitonGluonGhostVertex::usage = "GravitonGluonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\)},{\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)},{\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\)}]. The function returns an expression for the gravitational vertex for gluon-ghost vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluon and ghost parameters.";

Begin["Private`"];

GravitonQuarkGluonVertex={indexArray1,indexArray2}|->CETensorC[{indexArray2[[1]],\[ScriptM]},indexArray1] QuarkGluonVertex[\[ScriptM],indexArray2[[2]]]//Calc;

GravitonThreeGluonVertex={indexArray,p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3}|->GluonVertex[p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3,Explicit->True]/.Pair[LorentzIndex[\[ScriptX]_,D],LorentzIndex[\[ScriptY]_,D]]->CITensorC[{\[ScriptX],\[ScriptY]},indexArray]//Calc ;

GravitonFourGluonVertex={indexArray,p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3,p4,\[Lambda]4,a4}|->GluonVertex[p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3,p4,\[Lambda]4,a4,Explicit->True] /. Pair[LorentzIndex[\[ScriptX]_, D], LorentzIndex[\[ScriptY]_, D]] Pair[LorentzIndex[\[ScriptA]_, D], LorentzIndex[\[ScriptB]_, D]] -> CIITensorC[{\[ScriptX], \[ScriptY], \[ScriptA], \[ScriptB]}, indexArray]//Calc  ;

GravitonGluonVertex = {indexArray,p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,\[CurlyEpsilon]}|-> SUNDelta[SUNIndex[a1],SUNIndex[a2]] GravitonVectorVertex[indexArray,\[Lambda]1,p1,\[Lambda]2,p2,\[CurlyEpsilon]];

GravitonYMGhostVertex = {indexArray,p1,a,p2,b}|->SUNDelta[SUNIndex[a],SUNIndex[b]]GravitonVectorGhostVertex[indexArray,p1,p2];

GravitonGluonGhostVertex = {indexArray,array1,array2,array3} |-> Power[Global`\[Kappa],Length[indexArray]/2] GluonGhostVertex[array1,array2,array3,Explicit->True] CTensor[indexArray]//Calc;

End[];

EndPackage[];
