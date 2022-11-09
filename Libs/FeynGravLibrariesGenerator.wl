(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["FeynGravLibrariesGenerator`",{"FeynCalc`"}];
Needs["DummyArray`","./../Rules/DummyArray.wl"];
Needs["GravitonScalarVertex`","./../Rules/GravitonScalarVertex.wl"];

SetDirectory[DirectoryName[$InputFileName]];

CheckGravitonScalars::usage = "CheckGravitonScalars. This procedure checks what libraries for graviton-scalar interaction are present.";

GenerateGravitonScalars::usage = "GenerateGravitonScalars[n]. This procedure generates libraries for graviton-scalar interactions up to the order n. Pre-existing libraries will be removed!";

Begin["Private`"];

CheckGravitonScalars := Module[{i},
	i = 1;
	While[FileExistsQ["GravitonScalarVertex_"<>ToString[i]], i += 1];
	Print["Libraries for gravitational interaction of a massless scalar field kinetic energy exist up to the order "<>ToString[i-1]];
	i = 1;
	While[FileExistsQ["GravitonMassiveScalarVertex_"<>ToString[i]], i += 1];
	Print["Libraries for gravitational interaction of a massive scalar field kinetic energy exist up to the order "<>ToString[i-1]];
	i = 1;
	While[FileExistsQ["GravitonScalarPotentialVertex_"<>ToString[i]] , i += 1];
	Print["Libraries for gravitational interaction of a scalar field potential energy exist up to the order "<>ToString[i-1]];
];

GenerateGravitonScalars[n_] := Module[{i,\[Rho],\[Sigma],p1,p2,m},
	i = 1;
	While[FileExistsQ["GravitonScalarVertex_"<>ToString[i]], 
		DeleteFile["GravitonScalarVertex_"<>ToString[i]];
		i += 1;
	];
	i = 1;
	While[FileExistsQ["GravitonMassiveScalarVertex_"<>ToString[i]], 
		DeleteFile["GravitonMassiveScalarVertex_"<>ToString[i]];
		i += 1;
	];
	i = 1;
	While[FileExistsQ["GravitonScalarPotentialVertex_"<>ToString[i]], 
		DeleteFile["GravitonScalarPotentialVertex_"<>ToString[i]];
		i += 1;
	];
	i = 1;
	For[i=1,i<=n,i++,
		Put[ Evaluate[GravitonScalarVertex[DummyArray[i],Global`p1,Global`p2,0]] , "GravitonScalarVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonScalarVertex[DummyArray[i],Global`p1,Global`p2,Global`m]] , "GravitonMassiveScalarVertex_"<>ToString[i] ];
		Put[ Evaluate[GravitonScalarPotentialVertex[DummyArray[i],\[Lambda]]] , "GravitonScalarPotentialVertex_"<>ToString[i] ];
		Print["Done for order "<>ToString[i] ];
	];
];

End[];

EndPackage[];
