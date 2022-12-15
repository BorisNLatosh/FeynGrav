(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["GravitonVertex`",{"FeynCalc`","ITensor`","CTensor`","CITensor`","CIITensor`","CIIITensor`","CIIIITensor`"}];

GravitonVertex::usage = "GravitonVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\[Ellipsis]},\[CurlyEpsilon]].";

GravitonGhostVertex::usage = "GravitonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis]},\[Mu],p1,\[Nu],p2].";


Begin["Private`"];


TakeLorenzIndices = indexArray |-> Flatten[(#[[;;2]]&)/@Partition[indexArray,3]];
ReducedGamma = {\[Mu],\[Alpha],\[Beta],\[Lambda],\[Rho],\[Sigma]} |->  MTD[\[Lambda],\[Alpha]]ITensor[{\[Beta],\[Mu],\[Rho],\[Sigma]}]+MTD[\[Lambda],\[Beta]]ITensor[{\[Alpha],\[Mu],\[Rho],\[Sigma]}]-MTD[\[Lambda],\[Mu]]ITensor[{\[Alpha],\[Beta],\[Rho],\[Sigma]}] //Calc;


TTensor = {\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2}|->FVD[p1,\[Mu]]FVD[p2,\[Nu]]ITensor[{\[Alpha],\[Beta],\[Mu]1,\[Nu]1}]ITensor[{\[Rho],\[Sigma],\[Mu]2,\[Nu]2}]-FVD[p1,\[Mu]]FVD[p2,\[Nu]]ITensor[{\[Alpha],\[Rho],\[Mu]1,\[Nu]1}]ITensor[{\[Beta],\[Sigma],\[Mu]2,\[Nu]2}]+2FVD[p1,\[Mu]]FVD[p2,\[Alpha]]ITensor[{\[Beta],\[Rho],\[Mu]1,\[Nu]1}]ITensor[{\[Nu],\[Sigma],\[Mu]2,\[Nu]2}]-2FVD[p1,\[Mu]]FVD[p2,\[Beta]]ITensor[{\[Nu],\[Alpha],\[Mu]1,\[Nu]1}]ITensor[{\[Rho],\[Sigma],\[Mu]2,\[Nu]2}] // Calc;
GravitonVertex1Alternative = indexArray |-> I(1/2)(Global`\[Kappa])^(Length[indexArray]/3-2) (TTensor@@Join[{\[ScriptM]1,\[ScriptN]1,\[ScriptM]2,\[ScriptN]2,\[ScriptM]3,\[ScriptN]3},indexArray[[;;6]]]) CIIITensor[{\[ScriptM]1,\[ScriptN]1,\[ScriptM]2,\[ScriptN]2,\[ScriptM]3,\[ScriptN]3},TakeLorenzIndices[indexArray[[7;;]]]] //Calc;


GravitonVertex1 = indexArray|->I(1/2)(Global`\[Kappa])^(Length[indexArray]/3-2) CIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[indexArray[[7;;]]]]FVD[indexArray[[3]],\[Lambda]1]FVD[indexArray[[6]],\[Lambda]2] (ReducedGamma[\[Beta],\[Mu],\[Rho],\[Lambda]1,indexArray[[1]],indexArray[[2]]]ReducedGamma[\[Sigma],\[Nu],\[Alpha],\[Lambda]2,indexArray[[4]],indexArray[[5]]]-ReducedGamma[\[Beta],\[Mu],\[Nu],\[Lambda]1,indexArray[[1]],indexArray[[2]]]ReducedGamma[\[Sigma],\[Alpha],\[Rho],\[Lambda]2,indexArray[[4]],indexArray[[5]]])//Calc;
GravitonVertex2 = indexArray |->I(-(1/8))(Global`\[Kappa])^(Length[indexArray]/3-2) FVD[indexArray[[3]],\[Lambda]1]FVD[indexArray[[6]],\[Lambda]2]ReducedGamma[\[Mu],\[Alpha],\[Beta],\[Lambda]1,indexArray[[1]],indexArray[[2]]]ReducedGamma[\[Nu],\[Rho],\[Sigma],\[Lambda]2,indexArray[[4]],indexArray[[5]]] CIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[indexArray[[7;;]]]]//Calc;
indexSymmetrization = indexArray |-> Flatten/@Permutations[Partition[indexArray,3]];

GravitonVertex = {indexArray,\[CurlyEpsilon]} |-> Total[(GravitonVertex1[#]+\[CurlyEpsilon] GravitonVertex2[#])&/@indexSymmetrization[indexArray]]//Calc;


GravitonGhostVertex1 = {indexArray,\[Mu],p1,\[Nu],p2}|->I (Global`\[Kappa])^(Length[indexArray]/3) CIITensor[{\[Alpha],\[Beta],\[Mu],\[Nu]},TakeLorenzIndices[indexArray]]FVD[p1,\[Alpha]]FVD[p2,\[Beta]]//Calc;
GravitonGhostVertex2a = {indexArray,\[Mu],p1,\[Nu],p2}|->I (Global`\[Kappa])^(Length[indexArray]/3) CIIITensor[{\[Mu],\[Rho],\[Nu],\[Sigma],\[Alpha],\[Beta]},TakeLorenzIndices[indexArray[[4;;]]]](-1/2) FVD[indexArray[[3]],\[Lambda]]( FVD[p1,\[Beta]]ReducedGamma[\[Sigma],\[Alpha],\[Rho],\[Lambda],indexArray[[1]],indexArray[[2]]]- FVD[p2,\[Beta]]ReducedGamma[\[Rho],\[Alpha],\[Sigma],\[Lambda],indexArray[[1]],indexArray[[2]]]+FVD[indexArray[[3]],\[Alpha]]ReducedGamma[\[Beta],\[Rho],\[Sigma],\[Lambda],indexArray[[1]],indexArray[[2]]]- FVD[indexArray[[3]],\[Rho]]ReducedGamma[\[Alpha],\[Beta],\[Sigma],\[Lambda],indexArray[[1]],indexArray[[2]]] )//Calc;
GravitonGhostVertex2 = {indexArray,\[Mu],p1,\[Nu],p2}|->Total[GravitonGhostVertex2a[#,\[Mu],p1,\[Nu],p2]&/@indexSymmetrization[indexArray]]//Calc;
GravitonGhostVertex3a = {indexArray,\[Mu],p1,\[Nu],p2}|->I (Global`\[Kappa])^(Length[indexArray]/3) CIIIITensor[{\[Mu],\[Rho],\[Nu],\[Sigma],\[Alpha],\[Beta],\[Lambda],\[Tau]},TakeLorenzIndices[indexArray[[7;;]]]] ( -(1/2)FVD[indexArray[[3]],\[Rho]]FVD[indexArray[[6]],\[Zeta]]ITensor[{\[Beta],\[Tau],indexArray[[1]],indexArray[[2]]}]ReducedGamma[\[Lambda],\[Alpha],\[Sigma],\[Zeta],indexArray[[4]],indexArray[[5]]] + (1/2) FVD[indexArray[[3]],\[Alpha]] FVD[indexArray[[6]],\[Zeta]]ITensor[{\[Beta],\[Tau],indexArray[[1]],indexArray[[2]]}]ReducedGamma[\[Lambda],\[Sigma],\[Rho],\[Zeta],indexArray[[4]],indexArray[[5]]] -(1/4)FVD[indexArray[[3]],\[Lambda]1]FVD[indexArray[[6]],\[Lambda]2]( ReducedGamma[\[Rho],\[Alpha],\[Lambda],\[Lambda]1,indexArray[[1]],indexArray[[2]]]ReducedGamma[\[Sigma],\[Beta],\[Tau],\[Lambda]2,indexArray[[4]],indexArray[[5]]]+ ReducedGamma[\[Alpha],\[Beta],\[Lambda],\[Lambda]1,indexArray[[1]],indexArray[[2]]]ReducedGamma[\[Tau],\[Rho],\[Sigma],\[Lambda]2,indexArray[[4]],indexArray[[5]]]- ReducedGamma[\[Beta],\[Rho],\[Lambda],\[Lambda]1,indexArray[[1]],indexArray[[2]]]ReducedGamma[\[Tau],\[Alpha],\[Sigma],\[Lambda]2,indexArray[[4]],indexArray[[5]]] ) ) //Calc;
GravitonGhostVertex3 = {indexArray,\[Mu],p1,\[Nu],p2}|->Total[GravitonGhostVertex3a[#,\[Mu],p1,\[Nu],p2]&/@indexSymmetrization[indexArray]]//Calc;


GravitonGhostVertexI = {indexArray,\[Mu],p1,\[Nu],p2}|->GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2]//Calc;
GravitonGhostVertexII = {indexArray,\[Mu],p1,\[Nu],p2}|->GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex2[indexArray,\[Mu],p1,\[Nu],p2]//Calc;
GravitonGhostVertexIII = {indexArray,\[Mu],p1,\[Nu],p2}|->GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex2[indexArray,\[Mu],p1,\[Nu],p2]+GravitonGhostVertex3[indexArray,\[Mu],p1,\[Nu],p2]//Calc;
GravitonGhostVertex = {indexArray,\[Mu],p1,\[Nu],p2}|->Piecewise[{{GravitonGhostVertexI[indexArray,\[Mu],p1,\[Nu],p2],Length[indexArray]==0},{GravitonGhostVertexII[indexArray,\[Mu],p1,\[Nu],p2],Length[indexArray]==3},{GravitonGhostVertexIII[indexArray,\[Mu],p1,\[Nu],p2],Length[indexArray]>3}}];


End[];

EndPackage[];
