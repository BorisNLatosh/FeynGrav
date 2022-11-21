(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonVertex`",{"FeynCalc`","ITensor`","CTensor`","CIIITensor`","CIICTensorC`"}];

GravitonVertex::usage = "GravitonVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\[Ellipsis]}]. Returns the expression for the graviton vertex. Here {\!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\)} are graviton Lorentz indices and \!\(\*SubscriptBox[\(p\), \(i\)]\) are graviton momenta.";

GravitonGhostVertex1::usage = "GravitonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},p1,p2].";

Begin["Private`"];

arraySymmetrization=Function[{array,argumets},Join[array,(array/.{argumets[[1]]->argumets[[2]],argumets[[2]]->argumets[[1]]})]];

TTensorGravity={\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Rho]1,\[Sigma]1,p1,\[Rho]2,\[Sigma]2,p2}|->-FVD[p1,\[Mu]]FVD[p2,\[Nu]]MTD[\[Alpha],\[Rho]1]MTD[\[Beta],\[Sigma]1]MTD[\[Rho],\[Rho]2]MTD[\[Sigma],\[Sigma]2]+FVD[p1,\[Mu]]FVD[p2,\[Nu]]MTD[\[Alpha],\[Rho]1]MTD[\[Rho],\[Sigma]1]MTD[\[Beta],\[Rho]2]MTD[\[Sigma],\[Sigma]2]-2 FVD[p1,\[Mu]]FVD[p2,\[Alpha]]MTD[\[Beta],\[Rho]1]MTD[\[Rho],\[Sigma]1]MTD[\[Nu],\[Rho]2]MTD[\[Sigma],\[Sigma]2]+2FVD[p1,\[Mu]]FVD[p2,\[Beta]]MTD[\[Nu],\[Rho]1]MTD[\[Alpha],\[Sigma]1]MTD[\[Rho],\[Rho]2]MTD[\[Sigma],\[Sigma]2];

GravitonVertexCore = indexArray|->(TTensorGravity@@Join[{\[ScriptM]1,\[ScriptN]1,\[ScriptM]2,\[ScriptN]2,\[ScriptM]3,\[ScriptN]3},indexArray[[;;6]]])CIIITensor[{\[ScriptM]1,\[ScriptN]1,\[ScriptM]2,\[ScriptN]2,\[ScriptM]3,\[ScriptN]3},Flatten[(#[[;;2]]&)/@Partition[indexArray[[7;;]],3]]];

TTensorGravitonGaugeFixingTerm = {\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Rho]1,\[Sigma]1,p1,\[Rho]2,\[Sigma]2,p2}|->  (FVD[p1,\[Alpha]] ITensor[{\[Beta],\[Mu],\[Rho]1,\[Sigma]1}] + FVD[p1,\[Beta]]ITensor[{\[Alpha],\[Mu],\[Rho]1,\[Sigma]1}] - FVD[p1,\[Mu]]ITensor[{\[Alpha],\[Beta],\[Rho]1,\[Sigma]1}] ) (FVD[p2,\[Rho]]ITensor[{\[Sigma],\[Nu],\[Rho]2,\[Sigma]2}]+FVD[p2,\[Sigma]]ITensor[{\[Rho],\[Nu],\[Rho]2,\[Sigma]2}] - FVD[p2,\[Nu]]ITensor[{\[Rho],\[Sigma],\[Rho]2,\[Sigma]2}]);

GravitonGaugeFixingTerm = indexArray |-> (TTensorGravitonGaugeFixingTerm@@Join[{\[ScriptM]1,\[ScriptN]1,\[ScriptM]2,\[ScriptN]2,\[ScriptM]3,\[ScriptN]3},indexArray[[;;6]]])CIIITensor[{\[ScriptM]1,\[ScriptN]1,\[ScriptM]2,\[ScriptN]2,\[ScriptM]3,\[ScriptN]3},Flatten[(#[[;;2]]&)/@Partition[indexArray[[7;;]],3]]];

GravitonVertexIndexSymmetrizationMomenta=indexArray|->(Partition[Flatten[Permutations[Partition[indexArray,3]]],Length[indexArray]]);

GravitonVertexIndexSymmetrization = indexArray |->Partition[Flatten[GravitonVertexIndexSymmetrizationMomenta/@Partition[Fold[arraySymmetrization,indexArray,(#[[;;2]]&)/@Partition[indexArray,3]],Length[indexArray]]],Length[indexArray]];

GravitonVertexMain=Function[indexArray,Contract[I(-2/Global`\[Kappa]^2) (Global`\[Kappa])^(Length[indexArray]/3)/4 1/Power[2,Length[indexArray]/3] Total[GravitonVertexCore/@GravitonVertexIndexSymmetrization[indexArray]]]];

GravitonVertexGaugeFixingTerm=Function[indexArray,Contract[I (Global`\[Kappa])^(Length[indexArray]/3-2) 1/Power[2,Length[indexArray]/3] Total[GravitonGaugeFixingTerm/@GravitonVertexIndexSymmetrization[indexArray]]]];

GravitonVertex = indexArray |-> GravitonVertexMain[indexArray]+Global`\[CurlyEpsilon] 1/8 GravitonVertexGaugeFixingTerm[indexArray];

GravitonGhostVertex1 = {indexArray,\[Lambda]1,p1,\[Lambda]2,p2} |->  Calc[CIICTensorC[{\[ScriptA],\[ScriptB],\[Lambda]1,\[Lambda]2},indexArray]FVD[p1,\[ScriptA]]FVD[p2,\[ScriptB]]];

End[];

EndPackage[];
