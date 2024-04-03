(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["HorndeskiG5`",{"FeynCalc`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


HorndeskiG5::usage = "";


Begin["Private`"];


MomentaWrapper = scalarMomenta |-> Times @@ MapThread[ FVD, { scalarMomenta, DummyArray2[Length[scalarMomenta]/2] } ] ;


DummyArray2 = n |-> Flatten[ {ToExpression["\[ScriptA]"<>ToString[#]], ToExpression["\[ScriptB]"<>ToString[#]]}& /@ Range[n]];


takeIndices = Flatten[ #[[;;2]]& /@ Partition[#,3] ] &;


Clear[T1];

T1[gravitonParameters_,scalarMomenta_,b_] := T1[gravitonParameters,scalarMomenta,b] = MomentaWrapper[scalarMomenta[[;;2b]]] FVD[scalarMomenta[[2b+1]], \[Alpha]] FVD[scalarMomenta[[2b+1]], \[Beta]] FVD[gravitonParameters[[-1]], \[Lambda]] (  CTensorGeneral[ Join[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3-1]]] ] + CTensorGeneral[ Join[{\[Mu],\[Beta],\[Nu],\[Alpha],\[Rho],\[Sigma]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3-1]]] ] - CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3-1]]] ]  ) (FVD[gravitonParameters[[-1]],\[Rho]] GammaTensor[\[Sigma],\[Mu],\[Nu],\[Lambda],gravitonParameters[[-3]],gravitonParameters[[-2]]] - FVD[gravitonParameters[[-1]],\[Mu]] GammaTensor[\[Rho],\[Sigma],\[Nu],\[Lambda],gravitonParameters[[-3]],gravitonParameters[[-2]]] ) ;


Clear[T2];

T2[gravitonParameters_,scalarMomenta_,b_] := T2[gravitonParameters,scalarMomenta,b] = (-1) MomentaWrapper[scalarMomenta[[;;2b]]] FVD[scalarMomenta[[2b+1]], \[Tau]] FVD[gravitonParameters[[-1-3]], \[Lambda]1] FVD[gravitonParameters[[-1]], \[Lambda]2] (  CTensorGeneral[ Join[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 2-1]]] ] + CTensorGeneral[ Join[{\[Mu],\[Beta],\[Nu],\[Alpha],\[Rho],\[Sigma],\[Lambda],\[Tau]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 2-1]]] ] - CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 2-1]]] ]  ) (FVD[gravitonParameters[[-1-3]],\[Rho]] GammaTensor[\[Sigma],\[Mu],\[Nu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] - FVD[gravitonParameters[[-1-3]],\[Mu]] GammaTensor[\[Rho],\[Sigma],\[Nu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] ) GammaTensor[\[Lambda],\[Alpha],\[Beta],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] ;


Clear[T3];

T3[gravitonParameters_,scalarMomenta_,b_] := T3[gravitonParameters,scalarMomenta,b] = MomentaWrapper[scalarMomenta[[;;2b]]] FVD[gravitonParameters[[-1-3]], \[Lambda]1] FVD[gravitonParameters[[-1]], \[Lambda]2] FVD[scalarMomenta[[2b+1]], \[Alpha]] FVD[scalarMomenta[[2b+1]], \[Beta]] (  CTensorGeneral[ Join[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 2-1]]] ] + CTensorGeneral[ Join[{\[Mu],\[Beta],\[Nu],\[Alpha],\[Rho],\[Sigma],\[Lambda],\[Tau]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 2-1]]] ] - CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 2-1]]] ]  ) (  GammaTensor[\[Rho],\[Lambda],\[Mu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[Sigma],\[Tau],\[Nu],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[Rho],\[Lambda],\[Tau],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]GammaTensor[\[Sigma],\[Mu],\[Nu],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] ) ;


Clear[T4];

T4[gravitonParameters_,scalarMomenta_,b_] := T4[gravitonParameters,scalarMomenta,b] = (-1) MomentaWrapper[scalarMomenta[[;;2b]]] FVD[gravitonParameters[[-1-3 2]] , \[Lambda]3] FVD[gravitonParameters[[-1-3]], \[Lambda]1] FVD[gravitonParameters[[-1]], \[Lambda]2] FVD[ scalarMomenta[[2b+1]], \[Epsilon]] (  CTensorGeneral[ Join[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau],\[Omega],\[Epsilon]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 3-1]]] ] + CTensorGeneral[ Join[{\[Mu],\[Beta],\[Nu],\[Alpha],\[Rho],\[Sigma],\[Lambda],\[Tau],\[Omega],\[Epsilon]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 3-1]]] ] - CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau],\[Omega],\[Epsilon]},DummyArray2[b]] , takeIndices[gravitonParameters[[;;-3 3-1]]] ]  ) GammaTensor[\[Omega],\[Alpha],\[Beta],\[Lambda]3,gravitonParameters[[-9]],gravitonParameters[[-8]]] (  GammaTensor[\[Rho],\[Lambda],\[Mu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[Sigma],\[Tau],\[Nu],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[Rho],\[Lambda],\[Tau],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]GammaTensor[\[Sigma],\[Mu],\[Nu],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] ) ;


Clear[TI];

TI[gravitonParameters_,scalarMomenta_,b_] := TI[gravitonParameters,scalarMomenta,b] = Switch[Length[gravitonParameters]/3,
	1 , T1[gravitonParameters,scalarMomenta,b],
	2 , T1[gravitonParameters,scalarMomenta,b] + T2[gravitonParameters,scalarMomenta,b] + T3[gravitonParameters,scalarMomenta,b],
	_ ,  T1[gravitonParameters,scalarMomenta,b] + T2[gravitonParameters,scalarMomenta,b] + T3[gravitonParameters,scalarMomenta,b] + T4[gravitonParameters,scalarMomenta,b]
];


Clear[T5];

T5[gravitonParameters_,scalarMomenta_,b_] := T5[gravitonParameters,scalarMomenta,b] = (-1/3) MomentaWrapper[scalarMomenta[[;;2(b-1)]]] FVD[scalarMomenta[[2b-1]],\[Mu]]FVD[scalarMomenta[[2b-1]],\[Nu]]FVD[scalarMomenta[[2b]],\[Alpha]]FVD[scalarMomenta[[2b]],\[Beta]]FVD[scalarMomenta[[2b+1]],\[Rho]]FVD[scalarMomenta[[2b+1]],\[Sigma]] (CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]}, DummyArray2[b-1]], takeIndices[gravitonParameters]] - 3 CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Rho],\[Beta],\[Sigma]}, DummyArray2[b-1]], takeIndices[gravitonParameters]] + 2 CTensorGeneral[ Join[{\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Mu]}, DummyArray2[b-1]], takeIndices[gravitonParameters]]);


Clear[T6];

T6[gravitonParameters_,scalarMomenta_,b_] := T6[gravitonParameters,scalarMomenta,b] = MomentaWrapper[scalarMomenta[[;;2(b-1)]]] FVD[scalarMomenta[[2b-1]],\[Tau]]FVD[scalarMomenta[[2b]],\[Alpha]]FVD[gravitonParameters[[-1]],\[Lambda]]FVD[scalarMomenta[[2b]],\[Beta]]FVD[scalarMomenta[[2b+1]],\[Rho]]FVD[scalarMomenta[[2b+1]],\[Sigma]] GammaTensor[\[Omega],\[Mu],\[Nu],\[Lambda],gravitonParameters[[-3]],gravitonParameters[[-2]]] (CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Omega],\[Tau]}, DummyArray2[b-1]], takeIndices[gravitonParameters[[;;-1-3]]]] - 3 CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Rho],\[Beta],\[Sigma],\[Omega],\[Tau]}, DummyArray2[b-1]], takeIndices[gravitonParameters[[;;-1-3]]]] + 2 CTensorGeneral[ Join[{\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Mu],\[Omega],\[Tau]}, DummyArray2[b-1]], takeIndices[gravitonParameters[[;;-1-3]]]]);


Clear[T7];

T7[gravitonParameters_,scalarMomenta_,b_] := T7[gravitonParameters,scalarMomenta,b] = (-1) MomentaWrapper[scalarMomenta[[;;2(b-1)]]] FVD[scalarMomenta[[2b-1]],\[Tau]1]FVD[scalarMomenta[[2b]],\[Tau]2]FVD[scalarMomenta[[2b+1]],\[Rho]]FVD[scalarMomenta[[2b+1]],\[Sigma]]FVD[gravitonParameters[[-1-3]],\[Lambda]1]FVD[gravitonParameters[[-1]],\[Lambda]2] GammaTensor[\[Omega]1,\[Mu],\[Nu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[Omega]2,\[Alpha],\[Beta],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]](CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Omega]1,\[Tau]1,\[Omega]2,\[Tau]2}, DummyArray2[b-1]], takeIndices[gravitonParameters[[;;-1-3 2]]]] - 3 CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Rho],\[Beta],\[Sigma],\[Omega]1,\[Tau]1,\[Omega]2,\[Tau]2}, DummyArray2[b-1]], takeIndices[gravitonParameters[[;;-1-3 2]]]] + 2 CTensorGeneral[ Join[{\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Mu],\[Omega]1,\[Tau]1,\[Omega]2,\[Tau]2}, DummyArray2[b-1]], takeIndices[gravitonParameters[[;;-1-3 2]]]]);


Clear[T8];

T8[gravitonParameters_,scalarMomenta_,b_] := T8[gravitonParameters,scalarMomenta,b] = (1/3) MomentaWrapper[scalarMomenta[[;;2(b-1)]]] FVD[scalarMomenta[[2b-1]],\[Tau]1]FVD[scalarMomenta[[2b]],\[Tau]2]FVD[scalarMomenta[[2b+1]],\[Tau]3]FVD[gravitonParameters[[-1-3 2]],\[Lambda]1]FVD[gravitonParameters[[-1-3]],\[Lambda]2]FVD[gravitonParameters[[-1]],\[Lambda]2]  GammaTensor[\[Omega]1,\[Mu],\[Nu],\[Lambda]1,gravitonParameters[[-9]],gravitonParameters[[-8]]] GammaTensor[\[Omega]2,\[Alpha],\[Beta],\[Lambda]2,gravitonParameters[[-6]],gravitonParameters[[-5]]]GammaTensor[\[Omega]3,\[Rho],\[Sigma],\[Lambda]3,gravitonParameters[[-3]],gravitonParameters[[-2]]](CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Omega]1,\[Tau]1,\[Omega]2,\[Tau]2,\[Omega]3,\[Tau]3}, DummyArray2[b-1]], takeIndices[gravitonParameters[[;;-1-3 3]]]] - 3 CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Rho],\[Beta],\[Sigma],\[Omega]1,\[Tau]1,\[Omega]2,\[Tau]2,\[Omega]3,\[Tau]3}, DummyArray2[b-1]], takeIndices[gravitonParameters[[;;-1-3 3]]]] + 2 CTensorGeneral[ Join[{\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Mu],\[Omega]1,\[Tau]1,\[Omega]2,\[Tau]2,\[Omega]3,\[Tau]3}, DummyArray2[b-1]], takeIndices[gravitonParameters[[;;-1-3 3]]]]);


Clear[TII];

TII[gravitonParameters_,scalarMomenta_,b_] := TII[gravitonParameters,scalarMomenta,b] = Switch[ Length[gravitonParameters]/3,
	0, T5[gravitonParameters,scalarMomenta,b],
	1, T5[gravitonParameters,scalarMomenta,b] + T6[gravitonParameters,scalarMomenta,b],
	2, T5[gravitonParameters,scalarMomenta,b] + T6[gravitonParameters,scalarMomenta,b] + T7[gravitonParameters,scalarMomenta,b],
	3, T5[gravitonParameters,scalarMomenta,b] + T6[gravitonParameters,scalarMomenta,b] + T7[gravitonParameters,scalarMomenta,b] + T8[gravitonParameters,scalarMomenta,b]
];


Clear[HorndeskiG5];

HorndeskiG5[gravitonParameters_,scalarMomenta_,b_] := HorndeskiG5[gravitonParameters,scalarMomenta,b] = Switch[b,
	0, I/2 Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+2] Total[ TI@@@Tuples[{ Flatten/@ Permutations[ Partition[gravitonParameters,3]], Permutations[scalarMomenta] , {b}}] ] //Expand//Contract,
	_, I/2 Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+2] Total[ TI@@@Tuples[{ Flatten/@ Permutations[ Partition[gravitonParameters,3]], Permutations[scalarMomenta] , {b}}] ] + I/2 Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+2] b Total[ TII@@@Tuples[{ Flatten/@ Permutations[ Partition[gravitonParameters,3]], Permutations[scalarMomenta] , {b}}] ]//Expand//Contract
];


End[];


EndPackage[];
