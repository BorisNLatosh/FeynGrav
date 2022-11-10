(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["FeynGravLibrariesGenerator`",{"FeynCalc`"}];
Needs["DummyArray`","./../Rules/DummyArray.wl"];
Needs["GravitonScalarVertex`","./../Rules/GravitonScalarVertex.wl"];
Needs["GravitonFermionVertex`","./../Rules/GravitonFermionVertex.wl"];
Needs["GravitonVectorVertex`","./../Rules/GravitonVectorVertex.wl"];
SetDirectory[DirectoryName[$InputFileName]];

CheckGravitonScalars::usage = "CheckGravitonScalars. This procedure checks what libraries for graviton-scalar interaction are present.";
CheckGravitonFermions::usage = "CheckGravitonFermions. This procedure checks what libraries for graviton-fermion interaction are present.";
CheckGravitonVectors::usage = "CheckGravitonFermions. This procedure checks what libraries for graviton-fermion interaction are present.";

GenerateGravitonScalars::usage = "GenerateGravitonScalars[n]. This procedure generates libraries for graviton-scalar interactions up to the order n. Pre-existing libraries will be removed!";
GenerateGravitonFermions::usage = "GenerateGravitonFermions[n]. This procedure generates libraries for graviton-fermion interactions up to the order n. Pre-existing libraries will be removed!";
GenerateGravitonVectors::usage = "GenerateGravitonVectors[n]. This procedure generates libraries for graviton-vector interactions up to the order n. Pre-existing libraries will be removed!";


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


End[];

EndPackage[];
