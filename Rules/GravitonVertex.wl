(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["GravitonVertex`",{"FeynCalc`","ITensor`","CTensor`","CITensor`","CIITensor`","CIIITensor`","CIIIITensor`","GammaTensor`","indexArraySymmetrization`"}];


GravitonVertex::usage = "GravitonVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\[Ellipsis]},\[CurlyEpsilon]].";
GravitonGhostVertex::usage = "GravitonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis]},\[Mu],p1,\[Nu],p2].";


Begin["Private`"];


TakeLorenzIndices = indexArray |-> Flatten[(#[[;;2]]&)/@Partition[indexArray,3]];


TTensor = {\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Mu]1,\[Nu]1,p1,\[Mu]2,\[Nu]2,p2}|->FVD[p1,\[Mu]]FVD[p2,\[Nu]]ITensor[{\[Alpha],\[Beta],\[Mu]1,\[Nu]1}]ITensor[{\[Rho],\[Sigma],\[Mu]2,\[Nu]2}]-FVD[p1,\[Mu]]FVD[p2,\[Nu]]ITensor[{\[Alpha],\[Rho],\[Mu]1,\[Nu]1}]ITensor[{\[Beta],\[Sigma],\[Mu]2,\[Nu]2}]+2FVD[p1,\[Mu]]FVD[p2,\[Alpha]]ITensor[{\[Beta],\[Rho],\[Mu]1,\[Nu]1}]ITensor[{\[Nu],\[Sigma],\[Mu]2,\[Nu]2}]-2FVD[p1,\[Mu]]FVD[p2,\[Beta]]ITensor[{\[Nu],\[Alpha],\[Mu]1,\[Nu]1}]ITensor[{\[Rho],\[Sigma],\[Mu]2,\[Nu]2}] // Calc;


GravitonVertex1 = indexArray|->I 2(Global`\[Kappa])^(Length[indexArray]/3-2) CIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[indexArray[[7;;]]]]FVD[indexArray[[3]],\[Lambda]1]FVD[indexArray[[6]],\[Lambda]2] (GammaTensor[\[Alpha],\[Mu],\[Rho],\[Lambda]1,indexArray[[1]],indexArray[[2]]]GammaTensor[\[Sigma],\[Nu],\[Beta],\[Lambda]2,indexArray[[4]],indexArray[[5]]]-GammaTensor[\[Alpha],\[Mu],\[Nu],\[Lambda]1,indexArray[[1]],indexArray[[2]]]GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda]2,indexArray[[4]],indexArray[[5]]]) //Contract;
GravitonVertex2 = indexArray |->I (-(1/2)) (Global`\[Kappa])^(Length[indexArray]/3-2)  CIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[indexArray[[7;;]]]] FVD[indexArray[[3]],\[Lambda]1]FVD[indexArray[[6]],\[Lambda]2]GammaTensor[\[Mu],\[Alpha],\[Beta],\[Lambda]1,indexArray[[1]],indexArray[[2]]]GammaTensor[\[Nu],\[Rho],\[Sigma],\[Lambda]2,indexArray[[4]],indexArray[[5]]] //Contract;
indexSymmetrization = indexArray |-> Flatten/@Permutations[Partition[indexArray,3]];
(*GravitonVertex = {indexArray,\[CurlyEpsilon]} |-> Total[(GravitonVertex1[#] + \[CurlyEpsilon] GravitonVertex2[#])&/@indexSymmetrization[indexArray]]//Expand;*)


GravitonVertex = {indexArray,\[CurlyEpsilon]} |-> Contract[ Total[ ( I (Global`\[Kappa])^(Length[#]/3-2) CIIITensor[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[#[[7;;]]]]FVD[#[[3]],\[Lambda]1]FVD[#[[6]],\[Lambda]2] (2 GammaTensor[\[Alpha],\[Mu],\[Rho],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Sigma],\[Nu],\[Beta],\[Lambda]2,#[[4]],#[[5]]] - 2 GammaTensor[\[Alpha],\[Mu],\[Nu],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda]2,#[[4]],#[[5]]] - (1/2) \[CurlyEpsilon] GammaTensor[\[Mu],\[Alpha],\[Beta],\[Lambda]1,#[[1]],#[[2]]]GammaTensor[\[Nu],\[Rho],\[Sigma],\[Lambda]2,#[[4]],#[[5]]]) )&/@indexSymmetrization[indexArray] ] ];


GravitonGhostVertex1 = {indexArray,\[Mu],p1,\[Nu],p2}|->I (Global`\[Kappa])^(Length[indexArray]/3) CIITensor[{\[Alpha],\[Beta],\[Mu],\[Nu]},TakeLorenzIndices[indexArray]]FVD[p1,\[Alpha]]FVD[p2,\[Beta]] //Contract;
GravitonGhostVertex2a = {indexArray,\[Mu],p1,\[Nu],p2}|->I (Global`\[Kappa])^(Length[indexArray]/3) CIIITensor[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma]},TakeLorenzIndices[indexArray[[4;;]]]](-1) FVD[indexArray[[3]],\[Lambda]]( FVD[p1,\[Sigma]]GammaTensor[\[Beta],\[Rho],\[Alpha],\[Lambda],indexArray[[1]],indexArray[[2]]]- FVD[p2,\[Sigma]]GammaTensor[\[Alpha],\[Rho],\[Beta],\[Lambda],indexArray[[1]],indexArray[[2]]]+FVD[indexArray[[3]],\[Rho]]GammaTensor[\[Sigma],\[Alpha],\[Beta],\[Lambda],indexArray[[1]],indexArray[[2]]]- FVD[indexArray[[3]],\[Alpha]]GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda],indexArray[[1]],indexArray[[2]]] )//Contract;
GravitonGhostVertex2 = {indexArray,\[Mu],p1,\[Nu],p2}|->Total[GravitonGhostVertex2a[#,\[Mu],p1,\[Nu],p2]&/@indexSymmetrization[indexArray]];
GravitonGhostVertex3a = {indexArray,\[Mu],p1,\[Nu],p2}|->I (Global`\[Kappa])^(Length[indexArray]/3) CIIIITensor[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},TakeLorenzIndices[indexArray[[7;;]]]] (-1)FVD[indexArray[[3]],\[Lambda]1]FVD[indexArray[[6]],\[Lambda]2]( GammaTensor[\[Rho],\[Alpha],\[Lambda],\[Lambda]1,indexArray[[1]],indexArray[[2]]]GammaTensor[\[Sigma],\[Beta],\[Tau],\[Lambda]2,indexArray[[4]],indexArray[[5]]] - GammaTensor[\[Rho],\[Alpha],\[Beta],\[Lambda]1,indexArray[[1]],indexArray[[2]]]GammaTensor[\[Sigma],\[Lambda],\[Tau],\[Lambda]2,indexArray[[4]],indexArray[[5]]] + GammaTensor[\[Alpha],\[Rho],\[Lambda],\[Lambda]1,indexArray[[1]],indexArray[[2]]]GammaTensor[\[Beta],\[Sigma],\[Tau],\[Lambda]2,indexArray[[4]],indexArray[[5]]]) //Contract;
GravitonGhostVertex3 = {indexArray,\[Mu],p1,\[Nu],p2}|->Total[GravitonGhostVertex3a[#,\[Mu],p1,\[Nu],p2]&/@indexSymmetrization[indexArray]];


GravitonGhostVertexI = {indexArray,\[Mu],p1,\[Nu],p2}|->GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2]//Expand;
GravitonGhostVertexII = {indexArray,\[Mu],p1,\[Nu],p2}|->GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2] + GravitonGhostVertex2[indexArray,\[Mu],p1,\[Nu],p2]//Expand;
GravitonGhostVertexIII = {indexArray,\[Mu],p1,\[Nu],p2}|->GravitonGhostVertex1[indexArray,\[Mu],p1,\[Nu],p2] + GravitonGhostVertex2[indexArray,\[Mu],p1,\[Nu],p2] + GravitonGhostVertex3[indexArray,\[Mu],p1,\[Nu],p2]//Expand;
GravitonGhostVertex = {indexArray,\[Mu],p1,\[Nu],p2}|->Piecewise[{{GravitonGhostVertexI[indexArray,\[Mu],p1,\[Nu],p2],Length[indexArray]==0},{GravitonGhostVertexII[indexArray,\[Mu],p1,\[Nu],p2],Length[indexArray]==3},{GravitonGhostVertexIII[indexArray,\[Mu],p1,\[Nu],p2],Length[indexArray]>3}}];


End[];


EndPackage[];
