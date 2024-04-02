(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["HorndeskiG5`",{"FeynCalc`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


HorndeskiG5::usage = "";


T1::usage = "";
T2::usage = "";
T3::usage = "";
T4::usage = "";


Begin["Private`"];


MomentaWrapper = scalarMomenta |-> Times @@ MapThread[ FVD, { scalarMomenta, DummyArray2[Length[scalarMomenta]/2] } ] ;


DummyArray2 = n |-> Flatten[ {ToExpression["\[ScriptA]"<>ToString[#]], ToExpression["\[ScriptB]"<>ToString[#]]}& /@ Range[n]];


takeIndices = Flatten[ #[[;;2]]& /@ Partition[#,3] ] &;


Clear[T1];

T1[gravitonParameters_,scalarMomenta_,b_] := T1[gravitonParameters,scalarMomenta,b] = MomentaWrapper[scalarMomenta[[;;2b]]] FVD[scalarMomenta[[2b+1]], \[Alpha]] FVD[scalarMomenta[[2b+1]], \[Beta]] FVD[gravitonParameters[[-1]], \[Lambda]] (  CTensorGeneral[ Join[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3-1]]] ] + CTensorGeneral[ Join[{\[Mu],\[Beta],\[Nu],\[Alpha],\[Rho],\[Sigma]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3-1]]] ] - CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3-1]]] ]  ) (FVD[gravitonParameters[[-1]],\[Rho]] GammaTensor[\[Sigma],\[Mu],\[Nu],\[Lambda],gravitonParameters[[-3]],gravitonParameters[[-2]]] - FVD[gravitonParameters[[-1]],\[Mu]] GammaTensor[\[Rho],\[Sigma],\[Nu],\[Lambda],gravitonParameters[[-3]],gravitonParameters[[-2]]] ) ;


Clear[T2];

T2[gravitonParameters_,scalarMomenta_,b_] := T2[gravitonParameters,scalarMomenta,b] = (-1) MomentaWrapper[scalarMomenta[[;;2b]]] FVD[scalarMomenta[[2b+1]], \[Tau]] FVD[gravitonParameters[[-1-3]], \[Lambda]1] FVD[gravitonParameters[[-1]], \[Lambda]2] (  CTensorGeneral[ Join[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 2-1]]] ] + CTensorGeneral[ Join[{\[Mu],\[Beta],\[Nu],\[Alpha],\[Rho],\[Sigma],\[Lambda],\[Tau]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 2-1]]] ] - CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 2-1]]] ]  ) (FVD[gravitonParameters[[-1-3]],\[Rho]] GammaTensor[\[Sigma],\[Mu],\[Nu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] - FVD[gravitonParameters[[-1-3]],\[Mu]] GammaTensor[\[Rho],\[Sigma],\[Nu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] ) GammaTensor[\[Lambda],\[Alpha],\[Beta],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]];


Clear[T3];

T3[gravitonParameters_,scalarMomenta_,b_] := T3[gravitonParameters,scalarMomenta,b] = MomentaWrapper[scalarMomenta[[;;2b]]] FVD[gravitonParameters[[-1-3]], \[Lambda]1] FVD[gravitonParameters[[-1]], \[Lambda]2] FVD[scalarMomenta[[2b+1]], \[Alpha]] FVD[scalarMomenta[[2b+1]], \[Beta]] (  CTensorGeneral[ Join[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 2-1]]] ] + CTensorGeneral[ Join[{\[Mu],\[Beta],\[Nu],\[Alpha],\[Rho],\[Sigma],\[Lambda],\[Tau]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 2-1]]] ] - CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 2-1]]] ]  ) (  GammaTensor[\[Rho],\[Lambda],\[Mu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[Sigma],\[Tau],\[Nu],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[Rho],\[Lambda],\[Tau],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]GammaTensor[\[Sigma],\[Mu],\[Nu],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] );


Clear[T4];

T4[gravitonParameters_,scalarMomenta_,b_] := T4[gravitonParameters,scalarMomenta,b] = MomentaWrapper[scalarMomenta[[;;2b]]] FVD[gravitonParameters[[-1-3 2]] , \[Lambda]3] FVD[gravitonParameters[[-1-3]], \[Lambda]1] FVD[gravitonParameters[[-1]], \[Lambda]2] FVD[ scalarMomenta[[2b+1]], \[Epsilon]] (  CTensorGeneral[ Join[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau],\[Omega],\[Epsilon]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 3-1]]] ] + CTensorGeneral[ Join[{\[Mu],\[Beta],\[Nu],\[Alpha],\[Rho],\[Sigma],\[Lambda],\[Tau],\[Omega],\[Epsilon]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 3-1]]] ] - CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau],\[Omega],\[Epsilon]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 3-1]]] ]  ) GammaTensor[\[Omega],\[Alpha],\[Beta],\[Lambda]3,gravitonParameters[[-9]],gravitonParameters[[-8]]] (  GammaTensor[\[Rho],\[Lambda],\[Mu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[Sigma],\[Tau],\[Nu],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[Rho],\[Lambda],\[Tau],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]GammaTensor[\[Sigma],\[Mu],\[Nu],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] );


End[];


EndPackage[];
