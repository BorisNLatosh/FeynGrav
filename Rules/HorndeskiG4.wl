(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["HorndeskiG4`",{"FeynCalc`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


HorndeskiG4::usage = "HorndeskiG4[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b].";


HorndeskiG4Uncontracted::usage = "HorndeskiG4[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b].";


Begin["Private`"];


takeIndices = Flatten[ #[[;;2]]& /@ Partition[#,3] ] &;


DummyArray2 = n |-> Flatten[ {ToExpression["\[ScriptA]"<>ToString[#]], ToExpression["\[ScriptB]"<>ToString[#]]}& /@ Range[n]];


MomentaWrapper = scalarMomenta |-> Times @@ MapThread[ FVD, { scalarMomenta, DummyArray2[Length[scalarMomenta]/2] } ] ;


Clear[T1];

T1[gravitonParameters_,scalarMomenta_,b_] := T1[gravitonParameters,scalarMomenta,b] = I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 b]]] CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, DummyArray2[b] ], takeIndices[gravitonParameters[[;;-3-1]]] ] FVD[gravitonParameters[[-1]],\[ScriptM]] FVD[gravitonParameters[[-1]],\[ScriptL]] (GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]) //Contract ;


Clear[T2];

T2[gravitonParameters_,scalarMomenta_,b_] := T2[gravitonParameters,scalarMomenta,b] = If[ Length[gravitonParameters]>=6 , I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 b]]] CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, DummyArray2[b] ], takeIndices[gravitonParameters[[;;-3 2-1]]] ] FVD[gravitonParameters[[-4]], \[ScriptL]1]FVD[gravitonParameters[[-1]], \[ScriptL]2] ( GammaTensor[\[ScriptM],\[ScriptA],\[ScriptR],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[ScriptN],\[ScriptS],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[ScriptM],\[ScriptA],\[ScriptB],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[ScriptN],\[ScriptR],\[ScriptS],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]])//Expand//Contract ,0 ] ;


Clear[T3];

T3[gravitonParameters_,scalarMomenta_,b_] := T3[gravitonParameters,scalarMomenta,b] = If[ b!=0, I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] b MomentaWrapper[scalarMomenta[[;;2 (b-1)]]] ( CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, DummyArray2[b-1] ], takeIndices[gravitonParameters] ] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB]}, DummyArray2[b-1] ], takeIndices[gravitonParameters] ] ) FVD[scalarMomenta[[2b-1]],\[ScriptM]]FVD[scalarMomenta[[2b-1]],\[ScriptN]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]] //Expand//Contract ,0 ];


Clear[T4];

T4[gravitonParameters_,scalarMomenta_,b_] := T4[gravitonParameters,scalarMomenta,b] = If[ (b!=0)&&(Length[gravitonParameters]>=3), -2 b I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 (b-1)]]] ( CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3-1]]] ] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3-1]]] ] ) FVD[gravitonParameters[[-1]],\[ScriptL]] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]] FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]] //Expand//Contract,0];


Clear[T5];

T5[gravitonParameters_,scalarMomenta_,b_] := T5[gravitonParameters,scalarMomenta,b] = If[ (b!=0)&&(Length[gravitonParameters]>=6), b I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 (b-1)]]] ( CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3 2-1]]] ] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3 2-1]]] ] ) FVD[gravitonParameters[[-4]],\[ScriptL]1]FVD[gravitonParameters[[-1]],\[ScriptL]2] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]GammaTensor[\[ScriptL],\[ScriptA],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptT]] //Expand//Contract,0];


Clear[HorndeskiG4Core]

HorndeskiG4Core[gravitonParameters_,scalarMomenta_,b_] := HorndeskiG4Core[gravitonParameters,scalarMomenta,b] = Switch[ Length[gravitonParameters]/3,
	1, 
		Switch[b,
			0, T1[gravitonParameters,scalarMomenta,b] ,
			_, T1[gravitonParameters,scalarMomenta,b] + T3[gravitonParameters,scalarMomenta,b] + T4[gravitonParameters,scalarMomenta,b]
			],
	_, Switch[b,
			0, T1[gravitonParameters,scalarMomenta,b] + T2[gravitonParameters,scalarMomenta,b],
			1, T1[gravitonParameters,scalarMomenta,b] + T2[gravitonParameters,scalarMomenta,b] + T3[gravitonParameters,scalarMomenta,b] + T4[gravitonParameters,scalarMomenta,b] + T5[gravitonParameters,scalarMomenta,b]
		]
];


Clear[HorndeskiG4Core1];

HorndeskiG4Core1[gravitonParameters_,scalarMomenta_,b_] := HorndeskiG4Core1[gravitonParameters,scalarMomenta,b] = Plus @@ ( HorndeskiG4Core[#,scalarMomenta,b]& /@ ( Flatten /@ Permutations[Partition[gravitonParameters,3]] ) ) ;


Clear[HorndeskiG4];

HorndeskiG4[gravitonParameters_,scalarMomenta_,b_] := HorndeskiG4[gravitonParameters,scalarMomenta,b] = Switch[b,
	0, HorndeskiG4Core1[gravitonParameters,scalarMomenta,b] //Contract,
	_, Total[Map[ HorndeskiG4Core1[gravitonParameters,#,b]& ,  Permutations[scalarMomenta] ]] //Contract
];


Clear[HorndeskiG4CoreUncontracted]

HorndeskiG4CoreUncontracted[gravitonParameters_,scalarMomenta_,b_] := HorndeskiG4CoreUncontracted[gravitonParameters,scalarMomenta,b] = Switch[ Length[gravitonParameters]/3,
	1, 
		Switch[b,
			0, I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 b]]] CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, DummyArray2[b] ], takeIndices[gravitonParameters[[;;-3-1]]] ] FVD[gravitonParameters[[-1]],\[ScriptM]] FVD[gravitonParameters[[-1]],\[ScriptL]] (GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]) ,
			_, I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 b]]] CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, DummyArray2[b] ], takeIndices[gravitonParameters[[;;-3-1]]] ] FVD[gravitonParameters[[-1]],\[ScriptM]] FVD[gravitonParameters[[-1]],\[ScriptL]] (GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]) + I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] b MomentaWrapper[scalarMomenta[[;;2 (b-1)]]] ( CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, DummyArray2[b-1] ], takeIndices[gravitonParameters] ] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB]}, DummyArray2[b-1] ], takeIndices[gravitonParameters] ] ) FVD[scalarMomenta[[2b-1]],\[ScriptM]]FVD[scalarMomenta[[2b-1]],\[ScriptN]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]] -2 b I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 (b-1)]]] ( CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3-1]]] ] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3-1]]] ] ) FVD[gravitonParameters[[-1]],\[ScriptL]] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]] FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]]
			],
	_, Switch[b,
			0, I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 b]]] CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, DummyArray2[b] ], takeIndices[gravitonParameters[[;;-3-1]]] ] FVD[gravitonParameters[[-1]],\[ScriptM]] FVD[gravitonParameters[[-1]],\[ScriptL]] (GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]) + I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 b]]] CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, DummyArray2[b] ], takeIndices[gravitonParameters[[;;-3 2-1]]] ] FVD[gravitonParameters[[-4]], \[ScriptL]1]FVD[gravitonParameters[[-1]], \[ScriptL]2] ( GammaTensor[\[ScriptM],\[ScriptA],\[ScriptR],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[ScriptN],\[ScriptS],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[ScriptM],\[ScriptA],\[ScriptB],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[ScriptN],\[ScriptR],\[ScriptS],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]),
			_, I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 b]]] CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, DummyArray2[b] ], takeIndices[gravitonParameters[[;;-3-1]]] ] FVD[gravitonParameters[[-1]],\[ScriptM]] FVD[gravitonParameters[[-1]],\[ScriptL]] (GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]) + I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 b]]] CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, DummyArray2[b] ], takeIndices[gravitonParameters[[;;-3 2-1]]] ] FVD[gravitonParameters[[-4]], \[ScriptL]1]FVD[gravitonParameters[[-1]], \[ScriptL]2] ( GammaTensor[\[ScriptM],\[ScriptA],\[ScriptR],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[ScriptN],\[ScriptS],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] - GammaTensor[\[ScriptM],\[ScriptA],\[ScriptB],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]] GammaTensor[\[ScriptN],\[ScriptR],\[ScriptS],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]) + I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] b MomentaWrapper[scalarMomenta[[;;2 (b-1)]]] ( CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, DummyArray2[b-1] ], takeIndices[gravitonParameters] ] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB]}, DummyArray2[b-1] ], takeIndices[gravitonParameters] ] ) FVD[scalarMomenta[[2b-1]],\[ScriptM]]FVD[scalarMomenta[[2b-1]],\[ScriptN]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]] -2 b I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 (b-1)]]] ( CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3-1]]] ] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3-1]]] ] ) FVD[gravitonParameters[[-1]],\[ScriptL]] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]] FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]] + b I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] MomentaWrapper[scalarMomenta[[;;2 (b-1)]]] ( CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3 2-1]]] ] - CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, DummyArray2[b-1] ], takeIndices[gravitonParameters[[;;-3 2-1]]] ] ) FVD[gravitonParameters[[-4]],\[ScriptL]1]FVD[gravitonParameters[[-1]],\[ScriptL]2] GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]GammaTensor[\[ScriptL],\[ScriptA],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]] FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptT]]
		]
];


Clear[HorndeskiG4Core1Uncontracted];

HorndeskiG4Core1Uncontracted[gravitonParameters_,scalarMomenta_,b_] := HorndeskiG4Core1Uncontracted[gravitonParameters,scalarMomenta,b] = Plus @@ ( HorndeskiG4CoreUncontracted[#,scalarMomenta,b]& /@ ( Flatten /@ Permutations[Partition[gravitonParameters,3]] ) ) ;


Clear[HorndeskiG4Uncontracted];

HorndeskiG4Uncontracted[gravitonParameters_,scalarMomenta_,b_] := HorndeskiG4Uncontracted[gravitonParameters,scalarMomenta,b] = Switch[b,
	0, HorndeskiG4Core1Uncontracted[gravitonParameters,scalarMomenta,b] ,
	_, Total[Map[ HorndeskiG4Core1Uncontracted[gravitonParameters,#,b]& ,  Permutations[scalarMomenta] ]] 
];


End[];


EndPackage[];
