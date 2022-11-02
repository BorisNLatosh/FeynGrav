(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonSUNYM`",{"FeynCalc`","CTensor`","CETensorC`","CITensorC`","CIITensorC`","GravitonFermionVertex`","GravitonVectorVertex`"}];

GravitonQuarkGluonVertex::usage = "GravitonQuarkGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis]},{\[Mu],a}].";

GravitonThreeGluonVertex::usage = "GravitonFourGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis]},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\)}]. The function takes two arguments. The first one is an array of graviton indices. The second one is an array describing parameters of quarks.";

GravitonFourGluonVertex::usage = "GravitonFourGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis]},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(4\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(4\)]\),\!\(\*SubscriptBox[\(a\), \(4\)]\)}]. The function takes two arguments. The first one is an array of graviton indices. The second one is an array describing parameters of quarks."

GravitonGluonVertex::usage = "GravitonQuarkVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis]},\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)]. The function takes two arguments. The first argument is an array of gravitons Lorentz indices. The second argument is an array specifying quanrks parameters.";

GravitonYMGhostPropagator::usage = "GravitonYMGhostPropagator[p,a,b].";

GravitonYMGhostVertex::usage = "GravitonYMGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis]},\!\(\*SubscriptBox[\(p\), \(1\)]\),a,\!\(\*SubscriptBox[\(p\), \(2\)]\),b].";

GravitonGluonGhostVertex::usage = "GravitonGluonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis]},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),a},{\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),b},{\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),c}].";

Begin["Private`"];

GravitonQuarkGluonVertex={indexArray1,indexArray2}|->CETensorC[{indexArray2[[1]],\[ScriptM]},indexArray1] QuarkGluonVertex[\[ScriptM],indexArray2[[2]]]//Calc;

GravitonThreeGluonVertex={indexArray1,indexArray2}|->GluonVertex[Sequence@@indexArray2,Explicit->True]/.Pair[LorentzIndex[\[ScriptX]_,D],LorentzIndex[\[ScriptY]_,D]]->CITensorC[{\[ScriptX],\[ScriptY]},indexArray1]//Calc ;

GravitonFourGluonVertex={indexArray1,indexArray2}|->Explicit[GluonVertex[Sequence @@ indexArray2]] /. Pair[LorentzIndex[\[ScriptX]_, D], LorentzIndex[\[ScriptY]_, D]] Pair[LorentzIndex[\[ScriptA]_, D], LorentzIndex[\[ScriptB]_, D]] -> CIITensorC[{\[ScriptX], \[ScriptY], \[ScriptA], \[ScriptB]}, indexArray1]//Calc  ;

GravitonGluonVertex = {indexArray,\[ScriptM]1,\[ScriptP]1,\[ScriptL]1,\[ScriptM]2,\[ScriptP]2,\[ScriptL]2}|-> SUNDelta[SUNIndex[\[ScriptL]1],SUNIndex[\[ScriptL]2]] GravitonVectorVertex[indexArray,\[ScriptM]1,\[ScriptP]1,\[ScriptM]2,\[ScriptP]2];

GravitonYMGhostPropagator = {p,a,b}|->GravitonVectorGhostPropagator[p] SUNDelta[SUNIndex[a],SUNIndex[b]];

GravitonYMGhostVertex = {indexArray,p1,a,p2,b}|->SUNDelta[SUNIndex[a],SUNIndex[b]]GravitonVectorGhostVertex[indexArray,p1,p2];

GravitonGluonGhostVertex = {indexArray,array1,array2,array3} |-> Power[Global`\[Kappa],Length[indexArray]/2] GluonGhostVertex[array1,array2,array3,Explicit->True] CTensor[indexArray]//Calc;

End[];

EndPackage[];
