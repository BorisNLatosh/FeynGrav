(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonVertex`",{"FeynCalc`","ITensor`","CTensor`","CITensor`","CIITensor`","CIIITensor`","CIICTensorC`"}];

GravitonVertex::usage = "GravitonVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\[Ellipsis]},\[CurlyEpsilon]].";

GhostPropagator::usage = "GhostPropagator[\[Mu],\[Nu],p].";

GravitonGhostVertex::usage = "GravitonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis]},\[Mu],p1,\[Nu],p2].";


Begin["Private`"];


TTensor = {\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2}|->FVD[p1,\[Mu]]FVD[p2,\[Nu]]ITensor[{\[Alpha],\[Beta],\[Mu]1,\[Nu]1}]ITensor[{\[Rho],\[Sigma],\[Mu]2,\[Nu]2}]-FVD[p1,\[Mu]]FVD[p2,\[Nu]]ITensor[{\[Alpha],\[Rho],\[Mu]1,\[Nu]1}]ITensor[{\[Beta],\[Sigma],\[Mu]2,\[Nu]2}]+2FVD[p1,\[Mu]]FVD[p2,\[Alpha]]ITensor[{\[Beta],\[Rho],\[Mu]1,\[Nu]1}]ITensor[{\[Nu],\[Sigma],\[Mu]2,\[Nu]2}]-2FVD[p1,\[Mu]]FVD[p2,\[Beta]]ITensor[{\[Nu],\[Alpha],\[Mu]1,\[Nu]1}]ITensor[{\[Rho],\[Sigma],\[Mu]2,\[Nu]2}] // Calc;

TakeLorenzIndices = indexArray |-> Flatten[(#[[;;2]]&)/@Partition[indexArray,3]];

GravitonVertex1 = indexArray |-> I(1/2)(Global`\[Kappa])^(Length[indexArray]/3-2) (TTensor@@Join[{\[ScriptM]1,\[ScriptN]1,\[ScriptM]2,\[ScriptN]2,\[ScriptM]3,\[ScriptN]3},indexArray[[;;6]]]) CIIITensor[{\[ScriptM]1,\[ScriptN]1,\[ScriptM]2,\[ScriptN]2,\[ScriptM]3,\[ScriptN]3},TakeLorenzIndices[indexArray[[7;;]]]] //Calc;


ReducedGamma = {\[Mu],\[Alpha],\[Beta],\[Lambda],\[Rho],\[Sigma]} |->  MTD[\[Lambda],\[Alpha]]ITensor[{\[Beta],\[Mu],\[Rho],\[Sigma]}]+MTD[\[Lambda],\[Beta]]ITensor[{\[Alpha],\[Mu],\[Rho],\[Sigma]}]-MTD[\[Lambda],\[Mu]]ITensor[{\[Alpha],\[Beta],\[Rho],\[Sigma]}] //Calc;

GravitonVertex2 = indexArray |->I(-(1/8))(Global`\[Kappa])^(Length[indexArray]/3-2) FVD[indexArray[[3]],\[Lambda]1]FVD[indexArray[[6]],\[Lambda]2]ReducedGamma[\[Mu],\[Alpha],\[Beta],\[Lambda]1,indexArray[[1]],indexArray[[2]]]ReducedGamma[\[Nu],\[Rho],\[Sigma],\[Lambda]2,indexArray[[4]],indexArray[[5]]] CIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[indexArray[[7;;]]]]//Calc;


indexSymmetrization = indexArray |-> Flatten/@Permutations[Partition[indexArray,3]];

GravitonVertex = {indexArray,\[CurlyEpsilon]} |-> Total[(GravitonVertex1[#]+\[CurlyEpsilon] GravitonVertex2[#])&/@indexSymmetrization[indexArray]]//Calc;


GhostPropagator = {\[Mu],\[Nu],p} |-> I (-MTD[\[Mu],\[Nu]])FAD[p];


GravitonGhostVertex1 = {indexArray,\[Mu],p1,\[Nu],p2}|->I (Global`\[Kappa])^(Length[indexArray]/3) CIICTensorC[{\[Alpha],\[Beta],\[Mu],\[Nu]},TakeLorenzIndices[indexArray]]FVD[p1,\[Alpha]]FVD[p2,\[Beta]]//Calc;
GravitonGhostVertex2 = {indexArray,\[Mu],p1,\[Nu],p2}|->I (Global`\[Kappa])^(Length[indexArray]/3) 1/2 CITensor[{\[Rho],\[Sigma]},TakeLorenzIndices[indexArray[[4;;]]]]FVD[indexArray[[3]],\[Lambda]](FVD[p1,\[Sigma]]ReducedGamma[\[Mu],\[Rho],\[Nu],\[Lambda],indexArray[[1]],indexArray[[2]]]+FVD[p2,\[Sigma]]ReducedGamma[\[Nu],\[Rho],\[Mu],\[Lambda],indexArray[[1]],indexArray[[2]]])//Calc;
GravitonGhostVertex3 = {indexArray,\[Mu],p1,\[Nu],p2}|->I 1/4 (Global`\[Kappa])^(Length[indexArray]/3) CIITensor[{\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[indexArray[[7;;]]]]FVD[indexArray[[3]],\[Lambda]1]FVD[indexArray[[6]],\[Lambda]2]ReducedGamma[\[Rho],\[Alpha],\[Mu],\[Lambda]1,indexArray[[1]],indexArray[[2]]]ReducedGamma[\[Sigma],\[Beta],\[Nu],\[Lambda]2,indexArray[[4]],indexArray[[5]]]//Calc;


GravitonGhostVertex4 = {indexArray,\[Mu],p1,\[Nu],p2}|->I (Global`\[Kappa])^(Length[indexArray]/3) CITensor[{\[Alpha],\[Beta]},TakeLorenzIndices[indexArray[[4;;]]]]FVD[indexArray[[3]],\[Lambda]]ReducedGamma[\[Mu],\[Alpha],\[Nu],\[Lambda],indexArray[[1]],indexArray[[2]]] FVD[p2,\[Beta]]//Calc;
GravitonGhostVertex5 = {indexArray,\[Mu],p1,\[Nu],p2}|->I 1/2 (Global`\[Kappa])^(Length[indexArray]/3) CIITensor[{\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[indexArray[[7;;]]]]FVD[indexArray[[3]],\[Lambda]1]FVD[indexArray[[6]],\[Lambda]2]ReducedGamma[\[Mu],\[Alpha],\[Sigma],\[Lambda]1,indexArray[[1]],indexArray[[2]]]ReducedGamma[\[Rho],\[Beta],\[Nu],\[Lambda]2,indexArray[[4]],indexArray[[5]]]//Calc;


GravitonGhostVertex6 = {indexArray,\[Mu],p1,\[Nu],p2}|-> I (-(1/2))(Global`\[Kappa])^(Length[indexArray]/3) CITensor[{\[Alpha],\[Beta]},TakeLorenzIndices[indexArray[[4;;]]]]FVD[indexArray[[3]],\[Lambda]](FVD[indexArray[[3]],\[Alpha]]ReducedGamma[\[Beta],\[Mu],\[Nu],\[Lambda],indexArray[[1]],indexArray[[2]]] - FVD[indexArray[[3]],\[Mu]]ReducedGamma[\[Alpha],\[Beta],\[Nu],\[Lambda],indexArray[[1]],indexArray[[2]]])//Calc;
GravitonGhostVertex7a = {indexArray,\[Mu],p1,\[Nu],p2}|->I (-(1/2))(Global`\[Kappa])^(Length[indexArray]/3) CIITensor[{\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[indexArray[[7;;]]]](FVD[indexArray[[3]],\[Mu]]FVD[indexArray[[6]],\[Lambda]]ITensor[{\[Beta],\[Sigma],indexArray[[1]],indexArray[[2]]}]ReducedGamma[\[Rho],\[Alpha],\[Nu],\[Lambda],indexArray[[4]],indexArray[[5]]]-FVD[indexArray[[3]],\[Alpha]]FVD[indexArray[[6]],\[Lambda]]ITensor[{\[Beta],\[Sigma],indexArray[[1]],indexArray[[2]]}]ReducedGamma[\[Rho],\[Mu],\[Nu],\[Lambda],indexArray[[4]],indexArray[[5]]]) //Calc;
GravitonGhostVertex7b = {indexArray,\[Mu],p1,\[Nu],p2}|->I (-(1/2))(Global`\[Kappa])^(Length[indexArray]/3) CIITensor[{\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[indexArray[[7;;]]]] 1/2 FVD[indexArray[[3]],\[Lambda]1]FVD[indexArray[[6]],\[Lambda]2](ReducedGamma[\[Alpha],\[Beta],\[Rho],\[Lambda]1,indexArray[[1]],indexArray[[2]]]ReducedGamma[\[Sigma],\[Mu],\[Nu],\[Lambda]2,indexArray[[4]],indexArray[[5]]]-ReducedGamma[\[Beta],\[Mu],\[Rho],\[Lambda]1,indexArray[[1]],indexArray[[2]]]ReducedGamma[\[Sigma],\[Alpha],\[Nu],\[Lambda]2,indexArray[[4]],indexArray[[5]]]) //Calc;
GravitonGhostVertex7 = {indexArray,\[Mu],p1,\[Nu],p2}|->GravitonGhostVertex7a[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex7b[indexArray,\[Mu],p1,\[Nu],p2]//Calc;


GravitonGhostVertexI = {indexArray,\[Mu],p1,\[Nu],p2}|->GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2]//Calc;
GravitonGhostVertexII = {indexArray,\[Mu],p1,\[Nu],p2}|->GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex2[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex4[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex6[indexArray,\[Mu],p1,\[Nu],p2]//Calc;
GravitonGhostVertexIII = {indexArray,\[Mu],p1,\[Nu],p2}|->GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex2[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex3[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex4[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex5[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex6[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex7[indexArray,\[Mu],p1,\[Nu],p2]//Calc;
GravitonGhostVertexTotal = {indexArray,\[Mu],p1,\[Nu],p2}|->Piecewise[{{GravitonGhostVertexI[indexArray,\[Mu],p1,\[Nu],p2],Length[indexArray]==0},{GravitonGhostVertexII[indexArray,\[Mu],p1,\[Nu],p2],Length[indexArray]==3},{GravitonGhostVertexIII[indexArray,\[Mu],p1,\[Nu],p2],Length[indexArray]>3}}];
GravitonGhostVertex = {indexArray,\[Mu],p1,\[Nu],p2}|->Total[GravitonGhostVertexTotal[#,\[Mu],p1,\[Nu],p2]&/@indexSymmetrization[indexArray]]//Calc;


End[];

EndPackage[];
