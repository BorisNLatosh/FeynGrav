(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["HorndeskiG4`",{"FeynCalc`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


T1::usage = "";
T2::usage = "";
T3::usage = "";
T4::usage = "";
T5::usage = "";
HorndeskiG4Core::usage = "";
HorndeskiG4::usage = "";


Begin["Private`"];


takeIndices = Flatten[ #[[;;2]]& /@ Partition[#,3] ] &;


DummyArray2 = n |-> Flatten[ {ToExpression["\[ScriptA]"<>ToString[#]], ToExpression["\[ScriptB]"<>ToString[#]]}& /@ Range[n]];


MomentaWrapper = scalarMomenta |-> Times @@ MapThread[ FVD, { scalarMomenta, DummyArray2[Length[scalarMomenta]/2] } ] ;


T1 = {gravitonParameters,scalarMomenta,b} |-> I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 b]]] CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, DummyArray2[b] ], takeIndices[gravitonParameters[[;;-3-1]]] ] FVD[gravitonParameters[[-1]],\[ScriptM]] FVD[gravitonParameters[[-1]],\[ScriptL]] (GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]) //Contract ;


T2 = {gravitonParameters,scalarMomenta,b} |-> If[ Length[gravitonParameters]>=6 , I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 b]]] CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, DummyArray2[b] ], takeIndices[gravitonParameters[[;;-3 2-1]]] ] FVD[gravitonParameters[[-4]], \[ScriptL]1]FVD[gravitonParameters[[-1]], \[ScriptL]2] ( GammaTensor[\[ScriptM],\[ScriptA],\[ScriptR],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[ScriptN],\[ScriptS],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[ScriptM],\[ScriptA],\[ScriptB],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[ScriptN],\[ScriptR],\[ScriptS],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]])//Expand//Contract ,0 ] ;


T3 = {gravitonParameters,scalarMomenta,b} |-> If[ b!=0, I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] b MomentaWrapper[scalarMomenta[[;;2 (b-1)]]] ( CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, DummyArray2[b-1] ], takeIndices[gravitonParameters] ] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB]}, DummyArray2[b-1] ], takeIndices[gravitonParameters] ] ) FVD[scalarMomenta[[2b-1]],\[ScriptM]]FVD[scalarMomenta[[2b-1]],\[ScriptN]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]] //Expand//Contract ,0 ];


T4 = {gravitonParameters,scalarMomenta,b} |-> If[ (b!=0)&&(Length[gravitonParameters]>=6), -2 b I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 (b-1)]]] ( CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3-1]]] ] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3-1]]] ] ) FVD[gravitonParameters[[-1]],\[ScriptL]] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]] FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]] //Expand//Contract,0];


T5 = {gravitonParameters,scalarMomenta,b} |-> If[ (b!=0)&&(Length[gravitonParameters]>=9), b I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 (b-1)]]] ( CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3 2-1]]] ] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3 2-1]]] ] ) FVD[gravitonParameters[[-4]],\[ScriptL]1]FVD[gravitonParameters[[-1]],\[ScriptL]2] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]GammaTensor[\[ScriptL],\[ScriptA],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptT]] //Expand//Contract,0];


HorndeskiG4Core = {gravitonParameters,scalarMomenta,b} |-> T1[gravitonParameters,scalarMomenta,b]+T2[gravitonParameters,scalarMomenta,b];


HorndeskiG4Core1 = {gravitonParameters,scalarMomenta,b} |-> Plus @@ ( HorndeskiG4Core[#,scalarMomenta,b]& /@ ( Flatten /@ Permutations[Partition[gravitonParameters,3]] ) );


HorndeskiG4 = {gravitonParameters,scalarMomenta,b} |-> Plus @@ ( HorndeskiG4Core[gravitonParameters,#,b]& /@  Permutations[scalarMomenta] );


End[];


EndPackage[];
