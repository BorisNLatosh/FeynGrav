(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["FeynGravLibrariesGenerator`",{"FeynCalc`"}];
Needs["DummyArray`","./../Rules/DummyArray.wl"];
Needs["GravitonScalarVertex`","./../Rules/GravitonScalarVertex.wl"];
Needs["GravitonFermionVertex`","./../Rules/GravitonFermionVertex.wl"];
Needs["GravitonVectorVertex`","./../Rules/GravitonVectorVertex.wl"];
Needs["GravitonSUNYM`","./../Rules/GravitonSUNYM.wl"];
Needs["GravitonVertex`","./../Rules/GravitonVertex.wl"];
SetDirectory[DirectoryName[$InputFileName]];

CheckGravitonScalars::usage = "CheckGravitonScalars. This procedure checks what libraries for graviton-scalar interaction are present.";
CheckGravitonFermions::usage = "CheckGravitonFermions. This procedure checks what libraries for graviton-fermion interaction are present.";
CheckGravitonVectors::usage = "CheckGravitonFermions. This procedure checks what libraries for graviton-fermion interaction are present.";
CheckGravitonSUNYM::usage = "CheckGravitonSUNYM. This procedure checks what libraries for gravitational interaction for SU(N)YM model are present.";
CheckGravitonVertex::usage = "CheckGravitonVertex. This procedure checks what libraries for graviton vertices are present.";

GenerateGravitonScalars::usage = "GenerateGravitonScalars[n]. This procedure generates libraries for graviton-scalar interactions up to the order n. Pre-existing libraries will be removed!";
GenerateGravitonFermions::usage = "GenerateGravitonFermions[n]. This procedure generates libraries for graviton-fermion interactions up to the order n. Pre-existing libraries will be removed!";
GenerateGravitonVectors::usage = "GenerateGravitonVectors[n]. This procedure generates libraries for graviton-vector interactions up to the order n. Pre-existing libraries will be removed!";
GenerateGravitonSUNYM::usage = "GenerateGravitonSUNYM[n]. This procedure generates libraries for gravitational interaction for SU(N)YM models up to the order n. Pre-existing libraries will be removed!";
GenerateGravitonVertex::usage = "GenerateGravitonVertex[n]. This procedure generates libraries for the gravity sector up to the order n. Pre-existing libraries will be removed!";


Begin["Private`"];


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


CheckGravitonVertex := Module[{i},
	i = 1;
	While[FileExistsQ["GravitonVertex_"<>ToString[i]], i += 1];
	Print["Libraries for graviton vertices exist up to the order "<>ToString[i-1]];
	i = 1;
	While[FileExistsQ["GravitonGhostVertex_"<>ToString[i]] , i += 1];
	Print["Libraries for graviton-ghost interaction exist up to the order "<>ToString[i-1]];
];


GenerateGravitonScalars[n_] := Module[{i,\[Rho],\[Sigma],p1,p2,m},
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
	For[i=1,i<=n,i++,
		Put[ Evaluate[GravitonScalarVertex[DummyArray[i],Global`p1,Global`p2,Global`m]] , "GravitonScalarVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonScalarPotentialVertex[DummyArray[i],Global`\[Lambda]]] , "GravitonScalarPotentialVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


GenerateGravitonFermions[n_] := Module[{i,\[Rho],\[Sigma],p1,p2,m},
	i = 1;
	While[FileExistsQ["GravitonFermionVertex_"<>ToString[i]], 
		DeleteFile["GravitonFermionVertex_"<>ToString[i]];
		i += 1;
	];
	i = 1;
	For[i=1,i<=n,i++,
		Put[ Evaluate[GravitonFermionVertex[DummyArray[i],Global`p1,Global`p2,Global`m]] , "GravitonFermionVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


GenerateGravitonVectors[n_] := Module[{i,\[Rho],\[Sigma],p1,p2,m},
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
	For[i=1,i<=n,i++,
		Put[ Evaluate[GravitonVectorVertex[DummyArray[i],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2]] , "GravitonVectorVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonMassiveVectorVertex[DummyArray[i],Global`\[Lambda]1,Global`p1,Global`\[Lambda]2,Global`p2,Global`m]] , "GravitonMassiveVectorVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonVectorGhostVertex[DummyArray[i],Global`p1,Global`p2]] , "GravitonVectorGhostVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


GenerateGravitonSUNYM[n_] := Module[{i,\[Rho],\[Sigma],p1,p2,m},
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
		Put[ Evaluate[GravitonQuarkGluonVertex[DummyArray[i],{Global`\[Lambda],Global`a}]] , "GravitonQuarkGluonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonThreeGluonVertex[DummyArray[i],{Global`\[Lambda]1,Global`a1,Global`p1,Global`\[Lambda]2,Global`a2,Global`p2,Global`\[Lambda]3,Global`a3,Global`p3}]] , "GravitonThreeGluonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonFourGluonVertex[DummyArray[i],{Global`\[Lambda]1,Global`a1,Global`p1,Global`\[Lambda]2,Global`a2,Global`p2,Global`\[Lambda]3,Global`a3,Global`p3,Global`\[Lambda]4,Global`a4,Global`p4}]] , "GravitonFourGluonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonGluonVertex[DummyArray[i],Global`\[Lambda]1,Global`a1,Global`p1,Global`\[Lambda]2,Global`a2,Global`p2]] , "GravitonGluonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonYMGhostVertex[DummyArray[i],Global`p1,Global`a1,Global`p2,Global`a2]] , "GravitonYMGhostVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonGluonGhostVertex[DummyArray[i],{Global`\[Lambda]1,Global`a1,Global`p1},{Global`\[Lambda]2,Global`a2,Global`p2},{Global`\[Lambda]3,Global`a3,Global`p3}]] , "GravitonGluonGhostVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


DummyArrayMomenta = n |-> ToExpression/@Flatten[Function[{"m"<>ToString[#],"n"<>ToString[#],"p"<>ToString[#]}]/@Range[n]];

GenerateGravitonVertex[n_] := Module[{i,k1,k2,\[Lambda]1,\[Lambda]2},
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
		Put[ Evaluate[GravitonVertex[DummyArrayMomenta[2+i],1]] , "GravitonVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonGhostVertex[DummyArrayMomenta[i],Global`\[Lambda]1,Global`k1,Global`\[Lambda]2,Global`k2]] , "GravitonGhostVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];


End[];

EndPackage[];
