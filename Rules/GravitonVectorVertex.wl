(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonVectorVertex`",{"FeynCalc`","ITensor`","CTensor`","CITensor`","CIITensor`","CIIITensor`","CIIIITensor`"}];

GravitonMassiveVectorVertex::usage = "GravitonMassiveVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. The function returns an expression for the gravitational vertex of a massive vector field kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vectors Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are vectors momenta, and m is the vector field mass.";

GravitonVectorVertex::usage = "GravitonVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. The function returns an expression for the gravitational vertex of a massless vector field kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(k\), \(i\)]\)} are gravitons momenta, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vectors Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are vectors momenta.";

GravitonVectorGhostVertex::usage = "GravitonVectorGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. The function returns an expression for the graviton vertex of the Faddeev-Popov ghost kinetic energy. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are ghosts momenta.";

Begin["Private`"];


FReduced = {\[Mu],\[Nu],\[Sigma],\[Lambda]} |-> MTD[\[Mu],\[Sigma]]MTD[\[Nu],\[Lambda]]-MTD[\[Nu],\[Sigma]]MTD[\[Mu],\[Lambda]];
GammaReduced = {\[Mu],\[Alpha],\[Beta],\[Lambda],\[Rho],\[Sigma]}|->MTD[\[Alpha],\[Lambda]]ITensor[{\[Beta],\[Mu],\[Rho],\[Sigma]}]+MTD[\[Beta],\[Lambda]]ITensor[{\[Alpha],\[Mu],\[Rho],\[Sigma]}]-MTD[\[Mu],\[Lambda]]ITensor[{\[Alpha],\[Beta],\[Rho],\[Sigma]}]//Calc;
TakeLorenzIndices = indexArray |-> Flatten[(#[[;;2]]&)/@Partition[indexArray,3]];


GravitonVectorVertex1 = {indexArray,\[Lambda]1,p1,\[Lambda]2,p2} |-> I (Global`\[Kappa])^(Length[indexArray]/2) CIITensor[{\[Mu],\[Alpha],\[Nu],\[Beta]},indexArray] 1/2 FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2]FReduced[\[Mu],\[Nu],\[Sigma]1,\[Lambda]1]FReduced[\[Alpha],\[Beta],\[Sigma]2,\[Lambda]2]//Calc;
GravitonVectorVertex2 = {indexArray,\[Lambda]1,p1,\[Lambda]2,p2} |-> I (Global`\[Kappa])^(Length[indexArray]/2) CITensor[{\[Lambda]1,\[Lambda]2},indexArray]//Calc;
GravitonMassiveVectorVertex = {indexArray,\[Lambda]1,p1,\[Lambda]2,p2,m} |-> GravitonVectorVertex1[indexArray,\[Lambda]1,p1,\[Lambda]2,p2] + m^2 GravitonVectorVertex2[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]//Calc;


GravitonVectorVertex3 = {indexArray,\[Lambda]1,p1,\[Lambda]2,p2} |->  I (Global`\[Kappa])^(Length[indexArray]/3) CIITensor[{\[Sigma]1,\[Lambda]1,\[Sigma]2,\[Lambda]2},TakeLorenzIndices[indexArray]] (-1)FVD[p1,\[Sigma]1]FVD[p2,\[Sigma]2] //Calc;
GravitonVectorVertex4a = {indexArray,\[Lambda]1,p1,\[Lambda]2,p2} |-> I (Global`\[Kappa])^(Length[indexArray]/3) CIIITensor[{\[Mu],\[Nu],\[Mu]1,\[Lambda]1,\[Mu]2,\[Lambda]2},TakeLorenzIndices[indexArray[[4;;]]]] (1/2)(GammaReduced[\[Mu]1,\[Mu],\[Nu],\[Sigma],indexArray[[1]],indexArray[[2]]]FVD[indexArray[[3]],\[Sigma]]FVD[p2,\[Mu]2] + GammaReduced[\[Mu]2,\[Mu],\[Nu],\[Sigma],indexArray[[1]],indexArray[[2]]]FVD[indexArray[[3]],\[Sigma]]FVD[p1,\[Mu]1]) //Calc;
GravitonVectorVertex4 = {indexArray,\[Lambda]1,p1,\[Lambda]2,p2} |->Total[Function[GravitonVectorVertex4a[#,\[Lambda]1,p1,\[Lambda]2,p2]]/@(Flatten/@Permutations[Partition[indexArray,3]])]//Calc;
GravitonVectorVertex5a = {indexArray,\[Lambda]1,p1,\[Lambda]2,p2} |-> I (Global`\[Kappa])^(Length[indexArray]/3) CIIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Mu]1,\[Lambda]1,\[Mu]2,\[Lambda]2},TakeLorenzIndices[indexArray[[7;;]]]](-1/8)FVD[indexArray[[3]],\[Tau]1]FVD[indexArray[[6]],\[Tau]2]  (GammaReduced[\[Mu]1,\[Mu],\[Nu],\[Tau]1,indexArray[[1]],indexArray[[2]]] GammaReduced[\[Mu]2,\[Alpha],\[Beta],\[Tau]2,indexArray[[4]],indexArray[[5]]] + GammaReduced[\[Mu]2,\[Mu],\[Nu],\[Tau]1,indexArray[[1]],indexArray[[2]]] GammaReduced[\[Mu]1,\[Alpha],\[Beta],\[Tau]2,indexArray[[4]],indexArray[[5]]] )// Calc;
GravitonVectorVertex5 = {indexArray,\[Lambda]1,p1,\[Lambda]2,p2} |->Total[Function[GravitonVectorVertex5a[#,\[Lambda]1,p1,\[Lambda]2,p2]]/@(Flatten/@Permutations[Partition[indexArray,3]])]//Calc;


GravitonVectorVertexI = {indexArray,\[Lambda]1,p1,\[Lambda]2,p2,\[CurlyEpsilon]} |-> GravitonVectorVertex1[TakeLorenzIndices[indexArray],\[Lambda]1,p1,\[Lambda]2,p2]+\[CurlyEpsilon] GravitonVectorVertex3[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]//Calc;
GravitonVectorVertexII = {indexArray,\[Lambda]1,p1,\[Lambda]2,p2,\[CurlyEpsilon]} |-> GravitonVectorVertex1[TakeLorenzIndices[indexArray],\[Lambda]1,p1,\[Lambda]2,p2]+\[CurlyEpsilon] GravitonVectorVertex3[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]+\[CurlyEpsilon] GravitonVectorVertex4[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]//Calc;
GravitonVectorVertexIII = {indexArray,\[Lambda]1,p1,\[Lambda]2,p2,\[CurlyEpsilon]} |-> GravitonVectorVertex1[TakeLorenzIndices[indexArray],\[Lambda]1,p1,\[Lambda]2,p2]+\[CurlyEpsilon] GravitonVectorVertex3[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]+\[CurlyEpsilon] GravitonVectorVertex4[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]+\[CurlyEpsilon] GravitonVectorVertex5[indexArray,\[Lambda]1,p1,\[Lambda]2,p2]//Calc;
GravitonVectorVertex ={indexArray,\[Lambda]1,p1,\[Lambda]2,p2,\[CurlyEpsilon]} |->Piecewise[{{GravitonVectorVertexI[indexArray,\[Lambda]1,p1,\[Lambda]2,p2,\[CurlyEpsilon]],Length[indexArray]/3==0},{GravitonVectorVertexII[indexArray,\[Lambda]1,p1,\[Lambda]2,p2,\[CurlyEpsilon]],Length[indexArray]/3==1},{GravitonVectorVertexIII[indexArray,\[Lambda]1,p1,\[Lambda]2,p2,\[CurlyEpsilon]],Length[indexArray]/3>=2}}];


GravitonVectorGhostVertex = {indexArray,p1,p2} |-> I (Global`\[Kappa])^(Length[indexArray]/2) Calc[- FVD[p1,\[ScriptM]]FVD[p2,\[ScriptN]] CITensor[{\[ScriptM],\[ScriptN]},indexArray]]//Calc;

End[];

EndPackage[];
