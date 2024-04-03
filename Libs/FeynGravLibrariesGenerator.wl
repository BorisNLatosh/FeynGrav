(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["FeynGravLibrariesGenerator`",{"FeynCalc`"}];


Needs["GravitonScalarVertex`","./../Rules/GravitonScalarVertex.wl"];
Needs["GravitonFermionVertex`","./../Rules/GravitonFermionVertex.wl"];
Needs["GravitonVectorVertex`","./../Rules/GravitonVectorVertex.wl"];
Needs["GravitonSUNYM`","./../Rules/GravitonSUNYM.wl"];
Needs["GravitonVertex`","./../Rules/GravitonVertex.wl"];
Needs["HorndeskiG2`","./../Rules/HorndeskiG2.wl"];
Needs["HorndeskiG3`","./../Rules/HorndeskiG3.wl"];
Needs["HorndeskiG4`","./../Rules/HorndeskiG4.wl"];
Needs["HorndeskiG4`","./../Rules/HorndeskiG5.wl"];
Needs["GravitonAxionVectorVertex`","./../Rules/GravitonAxionVectorVertex.wl"];
SetDirectory[DirectoryName[$InputFileName]];


CheckGravitonScalars::usage = "CheckGravitonScalars. This procedure checks what libraries for graviton-scalar interaction are present.";
CheckGravitonFermions::usage = "CheckGravitonFermions. This procedure checks what libraries for graviton-fermion interaction are present.";
CheckGravitonVectors::usage = "CheckGravitonFermions. This procedure checks what libraries for graviton-fermion interaction are present.";
CheckGravitonVertex::usage = "CheckGravitonVertex. This procedure checks what libraries for graviton vertices are present.";


CheckGravitonSUNYM::usage = "CheckGravitonSUNYM. This procedure checks what libraries for gravitational interaction for SU(N)YM model are present.";


CheckGravitonAxionVector::usage = "CheckGravitonAxionVector. This procedure checks what libraries for graviton-scalar axion-single vector interaction are present.";


CheckHorndeskiG2::usage = "CheckHorndeskiG2. This procedure checks what libraries for Horndeski G2 interactions are present.";
CheckHorndeskiG3::usage = "CheckHorndeskiG3. This procedure checks what libraries for Horndeski G3 interactions are present.";
CheckHorndeskiG4::usage = "CheckHorndeskiG4. This procedure checks what libraries for Horndeski G4 interactions are present.";
CheckHorndeskiG5::usage = "CheckHorndeskiG5. This procedure checks what libraries for Horndeski G5 interactions are present.";


GenerateGravitonScalars::usage = "GenerateGravitonScalars[n]. This procedure generates libraries for graviton-scalar interactions up to the order n. Pre-existing libraries will be removed!";
GenerateGravitonFermions::usage = "GenerateGravitonFermions[n]. This procedure generates libraries for graviton-fermion interactions up to the order n. Pre-existing libraries will be removed!";
GenerateGravitonVectors::usage = "GenerateGravitonVectors[n]. This procedure generates libraries for graviton-vector interactions up to the order n. Pre-existing libraries will be removed!";
GenerateGravitonVertex::usage = "GenerateGravitonVertex[n]. This procedure generates libraries for the gravity sector up to the order n. Pre-existing libraries will be removed!";


GenerateGravitonSUNYM::usage = "GenerateGravitonSUNYM[n]. This procedure generates libraries for gravitational interaction for SU(N)YM model up to the order n. Pre-existing libraries will be removed!";


GenerateGravitonAxionVector::usage = "GenerateGravitonAxionVector[n]. This procedure generates libraries for graviton-scalar axion-single vector interactions up to the order n. Pre-existing libraries will be removed!"


GenerateHorndeskiG2::usage = "GenerateHorndeskiG2[n]. This procedure generates libraries for Horndeski G2 interaction up to the order n. Pre-existing libraries will be removed!";
GenerateHorndeskiG3::usage = "GenerateHorndeskiG3[n]. This procedure generates libraries for Horndeski G3 interaction up to the order n. Pre-existing libraries will be removed!";
GenerateHorndeskiG4::usage = "GenerateHorndeskiG4[n]. This procedure generates libraries for Horndeski G4 interaction up to the order n. Pre-existing libraries will be removed!";
GenerateHorndeskiG5::usage = "GenerateHorndeskiG5[n]. This procedure generates libraries for Horndeski G5 interaction up to the order n. Pre-existing libraries will be removed!";


GenerateGravitonScalarsSpecific::usage = "GenerateGravitonScalarsSpecific[n]. This procedure generates libraries for graviton-scalar interactions specifically for the order n. Pre-existing libraries will be removed!";
GenerateGravitonFermionsSpecific::usage = "GenerateGravitonFermionsSpecific[n]. This procedure generates libraries for graviton-fermion interactions specifically for the order n. Pre-existing libraries will be removed!";
GenerateGravitonVectorsSpecific::usage = "GenerateGravitonVectorsSpecific[n]. This procedure generates libraries for graviton-vector interactions specifically for the order n. Pre-existing libraries will be removed!";
GenerateGravitonVertexSpecific::usage = "GenerateGravitonVertexSpecific[n]. This procedure generates libraries for the gravity sector specifically for the order n. Pre-existing libraries will be removed!";


GenerateGravitonSUNYMSpecific::usage = "GenerateGravitonSUNYMSpecific[n]. This procedure generates libraries for SU(N)YM model interactions specifically for the order n. Pre-existing libraries will be removed!";


GenerateHorndeskiG2Specific::usage = "GenerateHorndeskiG2Specific[a,b,n]. This procedure generates libraries for Horndeski G2 for given number of scalar field a, number of scalar field kinetic terms b, and the preturbation order n. Pre-existing libraries will be removed!";
GenerateHorndeskiG3Specific::usage = "GenerateHorndeskiG3Specific[a,b,n]. This procedure generates libraries for Horndeski G3 for given number of scalar field a, number of scalar field kinetic terms b, and the preturbation order n. Pre-existing libraries will be removed!";


Begin["Private`"];


DummyArray = n |->Flatten[ {ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]]}&/@Range[n] ];
DummyMomenta = n |-> ToExpression["p"<>ToString[#]]&/@Range[n];
DummyArrayMomenta = n |-> Flatten[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["p"<>ToString[#]]}&/@Range[n]];
DummyArrayMomentaK = n |-> Flatten[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["k"<>ToString[#]]}&/@Range[n]];


(* Procedures that check if libraries for simple models exist. *)


CheckGravitonScalars := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonScalarVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for gravitational interaction of a scalar field kinetic energy exist up to the order "<>ToString[i-1]];
	
	i = 1;
	
	While[FileExistsQ["GravitonScalarPotentialVertex_"<>ToString[i]] , i += 1];
	
	Print["Libraries for gravitational interaction of a scalar field potential energy exist up to the order "<>ToString[i-1]];
];


CheckGravitonFermions := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonFermionVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for gravitational interaction of a Dirac field field kinetic energy exist up to the order "<>ToString[i-1]];
	
];


CheckGravitonVectors := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonMassiveVectorVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for gravitational interaction of a massive vector field kinetic energy exist up to the order "<>ToString[i-1]];
	
	i = 1;
	
	While[FileExistsQ["GravitonVectorVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for gravitational interaction of a massless vector field kinetic energy exist up to the order "<>ToString[i-1]];
	
	i = 1;
	
	While[FileExistsQ["GravitonVectorGhostVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for gravitational interaction of a scalar Faddeev-Popov ghost kinetic energy exist up to the order "<>ToString[i-1]];
	
];


CheckGravitonVertex := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for graviton vertices exist up to the order "<>ToString[i-1]];
	
	i = 1;
	
	While[FileExistsQ["GravitonGhostVertex_"<>ToString[i]] , i += 1];
	
	Print["Libraries for graviton-ghost interaction exist up to the order "<>ToString[i-1]];
	
];


(* Procedures that check if libraries for SU(N) Yang-Mills model exist. *)


CheckGravitonSUNYM := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonQuarkGluonVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for gravitational interaction of quark-gluon interction energy exist up to the order "<>ToString[i-1]];
	
	i = 1;
	
	While[FileExistsQ["GravitonThreeGluonVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for gravitational interaction of three quarks interaction energy exist up to the order "<>ToString[i-1]];
	
	i = 1;
	
	While[FileExistsQ["GravitonFourGluonVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for gravitational interaction of four quarks interaction energy exist up to the order "<>ToString[i-1]];
	
	i = 1;
	
	While[FileExistsQ["GravitonGluonVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for gravitational interaction of a gluon kinetic energy exist up to the order "<>ToString[i-1]];
	
	i = 1;
	
	While[FileExistsQ["GravitonYMGhostVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for gravitational interaction of a scalar Faddeev-Popov SU(N)YM ghost kinetic energy exist up to the order "<>ToString[i-1]];
	
	i = 1;
	
	While[FileExistsQ["GravitonGluonGhostVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for gravitational interaction of gluon-ghost interaction energy exist up to the order "<>ToString[i-1]];
	
];


(* Procedures that check if libraries for simple Horndeski G2 interaction exist. *)


CheckHorndeskiG2 := Module[{a,i},

(* b = 1 *)
	For[ a = 1, a <= 4, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG2_"<>ToString[a]<>"_1_"<>ToString[i]], i += 1];
		Print["Libraries for Horndeski G2 interaction with a="<>ToString[a]<>", b=1 exist up to the order "<>ToString[i-1]];
	];
	
(* b = 2 *)
	For[ a = 0, a <= 2, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG2_"<>ToString[a]<>"_2_"<>ToString[i]], i += 1];
		Print["Libraries for Horndeski G2 interaction with a="<>ToString[a]<>", b=2 exist up to the order "<>ToString[i-1]];
	];
];


CheckHorndeskiG3 := Module[{a,i},

(* b = 0 *)
	For[ a = 2, a <= 5, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG3_"<>ToString[a]<>"_0_"<>ToString[i]], i += 1];
		Print["Libraries for Horndeski G3 interaction with a="<>ToString[a]<>", b=0 exist up to the order "<>ToString[i-1]];
	];
	
(* b = 1 *)
	For[ a = 0 , a <= 3, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG3_"<>ToString[a]<>"_1_"<>ToString[i]], i += 1];
		Print["Libraries for Horndeski G3 interaction with a="<>ToString[a]<>", b=1 exist up to the order "<>ToString[i-1]];
	];
	
(* b = 2 *)
	For[ a = 0 , a <= 1, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG3_"<>ToString[a]<>"_2_"<>ToString[i]], i += 1];
		Print["Libraries for Horndeski G3 interaction with a="<>ToString[a]<>", b=2 exist up to the order "<>ToString[i-1]];
	];
];


CheckHorndeskiG4 := Module[{a,i},

(* b = 0 *)
	i = 1;
	While[FileExistsQ["HorndeskiG4_1_0_"<>ToString[i]], i += 1];
	Print["Libraries for Horndeski G4 interactions with b=0 exist up to the order "<>ToString[i-1]];
	
(* b = 1 *)
	For[ a = 0 , a <= 2, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG4_"<>ToString[a]<>"_1_"<>ToString[i]], i += 1];
		Print["Libraries for Horndeski G4 interaction with a="<>ToString[a]<>", b=1 exist up to the order "<>ToString[i-1]];
	];
	
(* b = 2 *)
	i = 1;
	While[FileExistsQ["HorndeskiG4_0_2_"<>ToString[i]], i += 1];
	Print["Libraries for Horndeski G4 interaction with a=0, b=2 exist up to the order "<>ToString[i-1]];
];


CheckHorndeskiG5 := Module[{a,i},

(* b = 0 *)
	For[ a = 1 , a <= 4, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG5_"<>ToString[a]<>"_0_"<>ToString[i]], i += 1];
		Print["Libraries for Horndeski G5 interaction with a="<>ToString[a]<>", b=0 exist up to the order "<>ToString[i-1]];
	];
	
(* b = 1 *)
	For[ a = 1 , a <= 2, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG5_"<>ToString[a]<>"_1_"<>ToString[i]], i += 1];
		Print["Libraries for Horndeski G5 interaction with a="<>ToString[a]<>", b=1 exist up to the order "<>ToString[i-1]];
	];
	
];


(* Procedures that check if libraries for simple axion-like interaction exist. *)


CheckGravitonAxionVector := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonAxionVectorVertex_"<>ToString[i]], i += 1];
	
	Print["Libraries for gravitational interaction of a scalar axion coupled to a single vector field exist up to the order "<>ToString[i-1]];
];


(* Procedures that generates rules for simple models. *)


GenerateGravitonScalars[n_] := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonScalarVertex_"<>ToString[i]], 
		DeleteFile["GravitonScalarVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonScalarPotentialVertex_"<>ToString[i]], 
		DeleteFile["GravitonScalarPotentialVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	For[ i = 1, i <= n, i++,
		Put[ Evaluate[GravitonScalarVertex[DummyArray[i],Global`p1,Global`p2,Global`m]] , "GravitonScalarVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonScalarPotentialVertex[DummyArray[i],Global`\[Lambda]]] , "GravitonScalarPotentialVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


GenerateGravitonFermions[n_] := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonFermionVertex_"<>ToString[i]], 
		DeleteFile["GravitonFermionVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	For[ i = 1, i <= n, i++,
		Put[ Evaluate[GravitonFermionVertex[DummyArray[i],Global`p1,Global`p2,Global`m]] , "GravitonFermionVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


GenerateGravitonVectors[n_] := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonVectorVertex_"<>ToString[i]], 
		DeleteFile["GravitonVectorVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonMassiveVectorVertex_"<>ToString[i]], 
		DeleteFile["GravitonMassiveVectorVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonVectorGhostVertex_"<>ToString[i]], 
		DeleteFile["GravitonVectorGhostVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	For[ i = 1, i <= n, i++,
		Put[ Evaluate[GravitonVectorVertex[DummyArrayMomentaK[i],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`GaugeFixingEpsilonVector]] , "GravitonVectorVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonMassiveVectorVertex[DummyArray[i],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`m]] , "GravitonMassiveVectorVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonVectorGhostVertex[DummyArray[i],Global`p1,Global`p2]] , "GravitonVectorGhostVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


GenerateGravitonVertex[n_] := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonVertex_"<>ToString[i]], 
		DeleteFile["GravitonVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonGhostVertex_"<>ToString[i]], 
		DeleteFile["GravitonGhostVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	For[i=1,i<=n,i++,
		Put[ Evaluate[GravitonVertex[DummyArrayMomenta[2+i],Global`GaugeFixingEpsilon]] , "GravitonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonGhostVertex[DummyArrayMomenta[i],Global`\[Lambda]1,Global`k1,Global`\[Lambda]2,Global`k2]] , "GravitonGhostVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


(* Procedures that generates rules for SU(N) Yang-Mills model. *)


GenerateGravitonSUNYM[n_] := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonQuarkGluonVertex_"<>ToString[i]], 
		DeleteFile["GravitonQuarkGluonVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonThreeGluonVertex_"<>ToString[i]], 
		DeleteFile["GravitonThreeGluonVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonFourGluonVertex_"<>ToString[i]], 
		DeleteFile["GravitonFourGluonVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonGluonVertex_"<>ToString[i]], 
		DeleteFile["GravitonGluonVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonYMGhostVertex_"<>ToString[i]], 
		DeleteFile["GravitonYMGhostVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	While[FileExistsQ["GravitonGluonGhostVertex_"<>ToString[i]], 
		DeleteFile["GravitonGluonGhostVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	For[i=1,i<=n,i++,
		Put[ Evaluate[GravitonGluonVertex[DummyArrayMomentaK[i],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`GaugeFixingEpsilonSUNYM]] , "GravitonGluonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonThreeGluonVertex[DummyArray[i],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`p3,Global`\[Lambda]3,Global`a3]] , "GravitonThreeGluonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonFourGluonVertex[DummyArray[i],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`p3,Global`\[Lambda]3,Global`a3,Global`p4,Global`\[Lambda]4,Global`a4]] , "GravitonFourGluonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonQuarkGluonVertex[DummyArray[i],{Global`\[Lambda],Global`a}]] , "GravitonQuarkGluonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonYMGhostVertex[DummyArray[i],Global`p1,Global`a1,Global`p2,Global`a2]] , "GravitonYMGhostVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonGluonGhostVertex[DummyArray[i],{Global`\[Lambda]1,Global`a1,Global`p1},{Global`\[Lambda]2,Global`a2,Global`p2},{Global`\[Lambda]3,Global`a3,Global`p3}]] , "GravitonGluonGhostVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


(* Procedures that generates rules for Horndeski G2 interaction. *)


GenerateHorndeskiG2[n_] := Module[{a,i},

(* b = 1 *)
	For[ a = 1, a <= 4, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG2_"<>ToString[a]<>"_1_"<>ToString[i]], 
			DeleteFile["HorndeskiG2_"<>ToString[a]<>"_1_"<>ToString[i]];
			i += 1;
		];
	];
	
(* b = 2 *)
	For[ a = 0, a <= 2, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG2_"<>ToString[a]<>"_2_"<>ToString[i]], 
			DeleteFile["HorndeskiG2_"<>ToString[a]<>"_2_"<>ToString[i]];
			i += 1;
		];
	];
	
(* b = 1 *)
	For[ a = 1, a <= 4, a++,
		For[ i = 1, i <= n, i++,
			Put[ HorndeskiG2[DummyArray[i],DummyMomenta[a+2],1] , "HorndeskiG2_"<>ToString[a]<>"_1_"<>ToString[i] ];
			Print["Done for a="<>ToString[a]<>", b=1 for order "<>ToString[i]];
		];
	];
	
(* b = 2 *)
	For[ a = 0, a <= 2, a++,
		For[ i = 1, i <= n, i++,
			Put[ HorndeskiG2[DummyArray[i],DummyMomenta[a+4],2] , "HorndeskiG2_"<>ToString[a]<>"_2_"<>ToString[i] ];
			Print["Done for a="<>ToString[a]<>", b=2 for order "<>ToString[i]];
		];
	];
];


(* Procedures that generates rules for Horndeski G3 interaction. *)


GenerateHorndeskiG3[n_] := Module[{a,i},
(* b = 0 *)
	For[ a = 2, a <= 5, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG3_"<>ToString[a]<>"_0_"<>ToString[i]], 
				DeleteFile["HorndeskiG3_"<>ToString[a]<>"_0_"<>ToString[i]];
				i += 1;
		];
	];
	
(* b = 1 *)
	For[ a = 0, a <= 3, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG3_"<>ToString[a]<>"_1_"<>ToString[i]], 
				DeleteFile["HorndeskiG3_"<>ToString[a]<>"_1_"<>ToString[i]];
				i += 1;
		];
	];
	
(* b = 2 *)
	For[ a = 0, a <= 1, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG3_"<>ToString[a]<>"_2_"<>ToString[i]], 
				DeleteFile["HorndeskiG3_"<>ToString[a]<>"_2_"<>ToString[i]];
				i += 1;
		];
	];
	
(* b = 0 *)
	For[ a = 2, a <= 5, a++,
		For[ i = 1, i <= n, i++,
			Put[ HorndeskiG3[DummyArrayMomentaK[i],DummyMomenta[a+1],0] , "HorndeskiG3_"<>ToString[a]<>"_0_"<>ToString[i] ];
			Print["Done for a="<>ToString[a]<>", b=0 for order "<>ToString[i]];
		];
	];
	
(* b = 1 *)
	For[ a = 0, a <= 3, a++,
		For[ i = 1, i <= n, i++,
			Put[ HorndeskiG3[DummyArrayMomentaK[i],DummyMomenta[a+2+1],1] , "HorndeskiG3_"<>ToString[a]<>"_1_"<>ToString[i] ];
			Print["Done for a="<>ToString[a]<>", b=1 for order "<>ToString[i]];
		];
	];
	
(* b = 2 *)
	For[ a = 0, a <= 1, a++,
		For[ i = 1, i <= n, i++,
			Put[ HorndeskiG3[DummyArrayMomentaK[i],DummyMomenta[a+4+1],2] , "HorndeskiG3_"<>ToString[a]<>"_2_"<>ToString[i] ];
			Print["Done for a="<>ToString[a]<>", b=2 for order "<>ToString[i]];
		];
	];
];


GenerateHorndeskiG4[n_] := Module[{a,i},
(* b = 0 *)
	i = 1;
	While[FileExistsQ["HorndeskiG4_1_0_"<>ToString[i]], 
		DeleteFile["HorndeskiG4_1_0_"<>ToString[i]];
		i += 1;
	];
	
(* b = 1 *)
	For[ a = 0, a <= 2, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG4_"<>ToString[a]<>"_1_"<>ToString[i]], 
			DeleteFile["HorndeskiG4_"<>ToString[a]<>"_1_"<>ToString[i]];
			i += 1;
		];
	];
	
(* b = 2 *)
	i = 1;
	While[FileExistsQ["HorndeskiG4_0_2_"<>ToString[i]], 
		DeleteFile["HorndeskiG4_0_2_"<>ToString[i]];
		i += 1;
	];
	
(* b = 0 *)
	For[ i = 1, i <= n, i++,
		Put[ HorndeskiG4[DummyArrayMomentaK[i],DummyMomenta[1],0] , "HorndeskiG4_1_0_"<>ToString[i] ];
		Print["Done for a=1, b=0 for order "<>ToString[i]];
	];
	
(* b = 1 *)
	For[ a = 0, a <= 2, a++,
		For[ i = 1, i <= n, i++,
			Put[ HorndeskiG4[DummyArrayMomentaK[i],DummyMomenta[a+2],1] , "HorndeskiG4_"<>ToString[a]<>"_1_"<>ToString[i] ];
			Print["Done for a="<>ToString[a]<>", b=1 for order "<>ToString[i]];
		];
	];
	
(* b = 2 *)
	For[ i = 1, i <= n, i++,
		Put[ HorndeskiG4[DummyArrayMomentaK[i],DummyMomenta[0+4],2] , "HorndeskiG4_0_2_"<>ToString[i] ];
		Print["Done for a=0, b=2 for order "<>ToString[i]];
	];
];


GenerateHorndeskiG5[n_] := Module[{a,i},
(* b = 0 *)
	For[ a = 1, a <= 4, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG5_"<>ToString[a]<>"_0_"<>ToString[i]], 
			DeleteFile["HorndeskiG5_"<>ToString[a]<>"_0_"<>ToString[i]];
			i += 1;
		];
	];
	
(* b = 1 *)
	For[ a = 1, a <= 2, a++,
		i = 1;
		While[FileExistsQ["HorndeskiG5_"<>ToString[a]<>"_1_"<>ToString[i]], 
			DeleteFile["HorndeskiG5_"<>ToString[a]<>"_1_"<>ToString[i]];
			i += 1;
		];
	];

	
(* b = 0 *)
	For[ a = 1, a <= 4, a++,
		For[ i = 1, i <= n, i++,
			Put[ HorndeskiG5[DummyArrayMomentaK[i],DummyMomenta[a],1] , "HorndeskiG5_"<>ToString[a]<>"_0_"<>ToString[i] ];
			Print["Done for a="<>ToString[a]<>", b=0 for order "<>ToString[i]];
		];
	];
	
(* b = 1 *)
	For[ a = 1, a <= 2, a++,
		For[ i = 1, i <= n, i++,
			Put[ HorndeskiG4[DummyArrayMomentaK[i],DummyMomenta[2+a],1] , "HorndeskiG5_"<>ToString[a]<>"_1_"<>ToString[i] ];
			Print["Done for a="<>ToString[a]<>", b=1 for order "<>ToString[i]];
		];
	];

];


(* Procedures that generates rules for the simplest axion-like interaction. *)


GenerateGravitonAxionVector[n_] := Module[{i},

	i = 1;
	
	While[FileExistsQ["GravitonAxionVectorVertex_"<>ToString[i]], 
		DeleteFile["GravitonAxionVectorVertex_"<>ToString[i]];
		i += 1;
	];
	
	i = 1;
	
	For[i=1,i<=n,i++,
		Put[ Evaluate[GravitonAxionVectorVertex[DummyArray[i],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`\[CapitalTheta]]] , "GravitonAxionVectorVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


(* Procedures that generates specific rules for simple models. *)


GenerateGravitonScalarsSpecific[n_] := Module[{},
	If[FileExistsQ["GravitonScalarVertex_"<>ToString[n]], DeleteFile["GravitonScalarVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonScalarPotentialVertex_"<>ToString[n]], DeleteFile["GravitonScalarPotentialVertex_"<>ToString[n]]];
	Put[ Evaluate[GravitonScalarVertex[DummyArray[n],Global`p1,Global`p2,Global`m]] , "GravitonScalarVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonScalarPotentialVertex[DummyArray[n],Global`\[Lambda]]] , "GravitonScalarPotentialVertex_"<>ToString[n] ];
	Print["Done for order "<>ToString[n] ];
];


GenerateGravitonFermionsSpecific[n_] := Module[{},
	If[FileExistsQ["GravitonFermionVertex_"<>ToString[n]], DeleteFile["GravitonFermionVertex_"<>ToString[n]]];
	Put[ Evaluate[GravitonFermionVertex[DummyArray[n],Global`p1,Global`p2,Global`m]] , "GravitonFermionVertex_"<>ToString[n] ];
	Print["Done for order "<>ToString[n] ];
];


GenerateGravitonVectorsSpecific[n_] := Module[{},
	If[FileExistsQ["GravitonVectorVertex_"<>ToString[n]], DeleteFile["GravitonVectorVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonMassiveVectorVertex_"<>ToString[n]], DeleteFile["GravitonMassiveVectorVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonVectorGhostVertex_"<>ToString[n]], DeleteFile["GravitonVectorGhostVertex_"<>ToString[n]]];
	Put[ Evaluate[GravitonVectorVertex[DummyArrayMomentaK[n],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`GaugeFixingEpsilonVector]] , "GravitonVectorVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonMassiveVectorVertex[DummyArray[n],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`m]] , "GravitonMassiveVectorVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonVectorGhostVertex[DummyArray[n],Global`p1,Global`p2]] , "GravitonVectorGhostVertex_"<>ToString[n] ];
	Print["Done for order "<>ToString[n] ];
];


GenerateGravitonVertexSpecific[n_] := Module[{},
	If[FileExistsQ["GravitonVertex_"<>ToString[n]],DeleteFile["GravitonVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonGhostVertex_"<>ToString[n]],DeleteFile["GravitonGhostVertex_"<>ToString[n]]];
	Put[ Evaluate[GravitonVertex[DummyArrayMomenta[2+n],Global`GaugeFixingEpsilon]] , "GravitonVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonGhostVertex[DummyArrayMomenta[n],Global`\[Lambda]1,Global`k1,Global`\[Lambda]2,Global`k2]] , "GravitonGhostVertex_"<>ToString[n] ];
	Print["Done for order "<>ToString[n] ];
];


(* Procedures that generates specific rules for SU(N) Yang-Mills model. *)


GenerateGravitonSUNYMSpecific[n_] := Module[{},
	If[FileExistsQ["GravitonQuarkGluonVertex_"<>ToString[n]], DeleteFile["GravitonQuarkGluonVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonThreeGluonVertex_"<>ToString[n]], DeleteFile["GravitonThreeGluonVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonFourGluonVertex_"<>ToString[n]], DeleteFile["GravitonFourGluonVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonGluonVertex_"<>ToString[n]], DeleteFile["GravitonGluonVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonYMGhostVertex_"<>ToString[n]], DeleteFile["GravitonYMGhostVertex_"<>ToString[n]]];
	If[FileExistsQ["GravitonGluonGhostVertex_"<>ToString[n]], DeleteFile["GravitonGluonGhostVertex_"<>ToString[n]]];
	Put[ Evaluate[GravitonGluonVertex[DummyArrayMomentaK[n],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`GaugeFixingEpsilonSUNYM]] , "GravitonGluonVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonThreeGluonVertex[DummyArray[n],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`p3,Global`\[Lambda]3,Global`a3]] , "GravitonThreeGluonVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonFourGluonVertex[DummyArray[n],Global`p1,Global`\[Lambda]1,Global`a1,Global`p2,Global`\[Lambda]2,Global`a2,Global`p3,Global`\[Lambda]3,Global`a3,Global`p4,Global`\[Lambda]4,Global`a4]] , "GravitonFourGluonVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonQuarkGluonVertex[DummyArray[n],{Global`\[Lambda],Global`a}]] , "GravitonQuarkGluonVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonYMGhostVertex[DummyArray[n],Global`p1,Global`a1,Global`p2,Global`a2]] , "GravitonYMGhostVertex_"<>ToString[n] ];
	Put[ Evaluate[GravitonGluonGhostVertex[DummyArray[n],{Global`\[Lambda]1,Global`a1,Global`p1},{Global`\[Lambda]2,Global`a2,Global`p2},{Global`\[Lambda]3,Global`a3,Global`p3}]] , "GravitonGluonGhostVertex_"<>ToString[n] ];
	Print["Done" ];
];


(* Procedures that generates specific rules for Horndeski G2. *)


GenerateHorndeskiG2Specific[a_,b_,n_] := Module[{},
	If[FileExistsQ["HorndeskiG2_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n]],DeleteFile["HorndeskiG2_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n]]];
	Put[ HorndeskiG2[DummyArray[n],DummyMomenta[a+2b],b] , "HorndeskiG2_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n] ];
	Print["Done."]
];


(* Procedures that generates specific rules for Horndeski G3. *)


GenerateHorndeskiG3Specific[a_,b_,n_] := Module[{},
	If[ FileExistsQ["HorndeskiG3_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n]], DeleteFile["HorndeskiG3_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n]] ];
	Put[ HorndeskiG3[DummyArrayMomentaK[n],DummyMomenta[a+2b+1],b] , "HorndeskiG3_"<>ToString[a]<>"_"<>ToString[b]<>"_"<>ToString[n] ];
	Print["Done"];
];


End[];


EndPackage[];
