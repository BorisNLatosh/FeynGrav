(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];
Needs["Nieuwenhuizen`","./Rules/Nieuwenhuizen.wl"];
SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["FeynGrav`",{"FeynCalc`"}];
Print[Style["FeynGrav version 2.0",Bold]];
Print["FeynGrav: FeynGravCommands print the list of all supported commands."];
Print["FeynGrav: Use '?CommandName' to see a brief description."];
Print["FeynGrav: Examples can be found in FeynGrav_Examples.nb and ArXiV:2201.06812."];


GravitonScalarVertex::usage = "GravitonScalarVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Expression for gravitational interaction of a scalar field kinetic energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta, m is the scalar field mass.";
GravitonScalarPotentialVertex::usage = "GravitonScalarPotentialVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\)]. Expression for gravitational interaction of a scalar field potential energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitational indices, \!\(\*SubscriptBox[\(\[Lambda]\), \(l\)]\) is the scalar field self-coupling constant.";
ScalarPropagator::usage = "ScalarPropagator[p,m]. Propagator of a scalar field with mass m.";


GravitonFermionVertex::usage = "GravitonFermionVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Expression for gravitational interaction of a Dirac field kinetic energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are fermion momenta, m is the fermion mass.";


GravitonVectorVertex::usage = "GravitonVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. Expression for gravitational interaction of a massless vector field kinetic energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton indices, \!\(\*SubscriptBox[\(k\), \(i\)]\) are graviton momenta, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vector fields indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are vector fields momenta.";
GravitonMassiveVectorVertex::usage = "GravitonMassiveVectorVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Expression for gravitational interaction of a  massive vector field kinetic energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton indices, \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vector fields indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are vector fields momenta, m is the vector field mass.";
GravitonVectorGhostVertex::usage = "GravitonVectorGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. Expression for gravitational interaction of the Faddeev-Popov ghost kinetic energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are ghosts momenta.";


GravitonGluonVertex::usage = "GravitonGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(l\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(l\)]\),\!\(\*SubscriptBox[\(a\), \(l\)]\)]. The function returns an expression for the gravitational vertex of 2,3, and 4 gluon vertices. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluons parameters."
GravitonQuarkGluonVertex::usage = "GravitonQuarkGluonVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\[Lambda],a]. The function returns an expression for the gravitational vertex for quark-gluon vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\[Lambda],a} are the quark-gluon vertex parameters.";
GravitonYMGhostVertex::usage = "GravitonYMGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\)]. The function returns an expression for the gravitational vertex for ghosts. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitons Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are ghost parameters.";
GravitonGluonGhostVertex::usage = "GravitonGluonGhostVertex[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\)]. The function returns an expression for the gravitational vertex for gluon-ghost vertex. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton Lorentz indices, {\!\(\*SubscriptBox[\(p\), \(i\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(a\), \(i\)]\)} are gluon and ghost parameters.";


GravitonVertex::usage = "GravitonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\[Ellipsis]]. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are Lorentz indices of gravitons. \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of gravitons."
GravitonGhostVertex::usage = "GravitonGhostVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are Lorentz indices of gravitons. \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of gravitons, {\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\),\!\(\*SubscriptBox[\(p\), \(i\)]\)} are ghosts indices and momenta."
GravitonPropagator::usage = "GravitonPropagator[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Graviton propagator in the harmonic gauge. \[Mu],\[Nu] are indices of the first vertex. \[Alpha],\[Beta] are indices of the second vertex. p is the graviton momentum. "
GravitonPropagatorTop::usage = "GravitonPropagatorTop[\[Mu],\[Nu],\[Alpha],\[Beta]]. Nominator of the graviton propagator in the harmonic gauge."
GravitonPropagatorAlternative::usage = "GravitonPropagatorAlternative[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Graviton propagator in the harmonic gauge realized without FAD.\[Mu],\[Nu] are indices of the first vertex. \[Alpha],\[Beta] are indices of the second vertex. p is the graviton momentum."


FeynGravCommands := Print[" 'GravitonVertex','GravitonScalarVertex','GravitonVectorVertex','GravitonFermionVertex', 'GravitonPropagator', 'GravitonPropagatorTop', 'GravitonPropagatorAlternative', 'GaugeProjector', 'NieuwenhuizenOperator1', 'NieuwenhuizenOperator2', 'NieuwenhuizenOperator0', 'NieuwenhuizenOperator0Bar', 'NieuwenhuizenOperator1BarBar' "];


SetDirectory[DirectoryName[$InputFileName]];


DummyArray = Flatten[ ( { ToExpression["m"<>ToString[#]], ToExpression["n"<>ToString[#]]} )& /@ Range[#] ]&;
DummyArrayK = Flatten[ ( { ToExpression["m"<>ToString[#]], ToExpression["n"<>ToString[#]], ToExpression["k"<>ToString[#]]} )& /@ Range[#] ]&;
dummyArrayP=n|->Flatten[Function[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["p"<>ToString[#]]}]/@Range[n]];


Module[{cursor},
	cursor = 1;
	Clear[GravitonScalarVertex];
	While[FileExistsQ["./Libs/GravitonScalarVertex_"<>ToString[cursor]],
		Evaluate[GravitonScalarVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArray[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{p1,p2,m}  ]  ] = Get["./Libs/GravitonScalarVertex_"<>ToString[cursor]];
		cursor++;
	];
	cursor = 1;
	Clear[GravitonScalarPotentialVertex];
	While[FileExistsQ["./Libs/GravitonScalarPotentialVertex_"<>ToString[cursor]],
		Evaluate[GravitonScalarPotentialVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArray[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{\[Lambda]}  ]] = Get["./Libs/GravitonScalarPotentialVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Scalar vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@DummyArray[cursor]);
	Remove[p1,p2,m,\[Lambda]];
]


Module[{cursor},
	cursor = 1;
	Clear[GravitonFermionVertex];
	While[FileExistsQ["./Libs/GravitonFermionVertex_"<>ToString[cursor]],
		Evaluate[GravitonFermionVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArray[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{p1,p2,m} ]] = Get["./Libs/GravitonFermionVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Fermion vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@DummyArray[cursor]);
	Remove[p1,p2,m];
]


Module[{cursor},
	cursor = 1;
	Clear[GravitonVectorVertex];
	While[FileExistsQ["./Libs/GravitonVectorVertex_"<>ToString[cursor]],
		Evaluate[GravitonVectorVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArrayK[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{\[Lambda]1,p1,\[Lambda]2,p2} ]] = Get["./Libs/GravitonVectorVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Vector vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@DummyArrayK[cursor]);
	Remove[\[Lambda]1,\[Lambda]2,p1,p2];
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonMassiveVectorVertex];
	While[FileExistsQ["./Libs/GravitonMassiveVectorVertex_"<>ToString[cursor]],
		Evaluate[GravitonMassiveVectorVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArray[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{\[Lambda]1,p1,\[Lambda]2,p2,m} ]] = Get["./Libs/GravitonMassiveVectorVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Massive Vector vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@DummyArray[cursor]);
	Remove[\[Lambda]1,\[Lambda]2,p1,p2,m];
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonVectorGhostVertex];
	While[FileExistsQ["./Libs/GravitonVectorGhostVertex_"<>ToString[cursor]],
		Evaluate[GravitonVectorGhostVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArray[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{p1,p2} ]] = Get["./Libs/GravitonVectorGhostVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Vector-Ghost vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@DummyArray[cursor]);
	Remove[p1,p2];
]


Module[{cursor},
	cursor = 1;
	Clear[GravitonGluonVertex];
	While[FileExistsQ["./Libs/GravitonGluonVertex_"<>ToString[cursor]],
		Evaluate[GravitonGluonVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArrayK[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{p1,\[Lambda]1,a1,p2,\[Lambda]2,a2}]] = Get["./Libs/GravitonGluonVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Gluon vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@DummyArrayK[cursor]);
	Remove[\[Lambda]1,a1,p1,\[Lambda]2,a2,p2];
]

Module[{cursor},
	cursor = 1;
	While[FileExistsQ["./Libs/GravitonThreeGluonVertex_"<>ToString[cursor]],
		Evaluate[GravitonGluonVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArrayK[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3}]] = Get["./Libs/GravitonThreeGluonVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Three-Gluon vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@DummyArrayK[cursor]);
	Remove[\[Lambda]1,a1,p1,\[Lambda]2,a2,p2,\[Lambda]3,a3,p3];
]

Module[{cursor},
	cursor = 1;
	While[FileExistsQ["./Libs/GravitonFourGluonVertex_"<>ToString[cursor]],
		Evaluate[GravitonGluonVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArrayK[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{p1,\[Lambda]1,a1,p2,\[Lambda]2,a2,p3,\[Lambda]3,a3,p4,\[Lambda]4,a4}]] = Get["./Libs/GravitonFourGluonVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Four-Gluon vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@DummyArrayK[cursor]);
	Remove[\[Lambda]1,a1,p1,\[Lambda]2,a2,p2,\[Lambda]3,a3,p3,\[Lambda]4,a4,p4];
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonQuarkGluonVertex];
	While[FileExistsQ["./Libs/GravitonQuarkGluonVertex_"<>ToString[cursor]],
		Evaluate[GravitonQuarkGluonVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArray[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{\[Lambda],a} ]] = Get["./Libs/GravitonQuarkGluonVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Quark-Gluon vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@DummyArray[cursor]);
	Remove[\[Lambda],a];
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonYMGhostVertex];
	While[FileExistsQ["./Libs/GravitonYMGhostVertex_"<>ToString[cursor]],
		Evaluate[GravitonYMGhostVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArray[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{p1,a1,p2,a2} ]] = Get["./Libs/GravitonYMGhostVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-(Yang-Mills) Ghost vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@DummyArray[cursor]);
	Remove[a1,p1,a2,p2];
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonGluonGhostVertex];
	While[FileExistsQ["./Libs/GravitonGluonGhostVertex_"<>ToString[cursor]],
		Evaluate[GravitonGluonGhostVertex[{Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@DummyArray[cursor]},Sequence@@Function[ToExpression[ToString[#]<>"_"]]/@{\[Lambda]1,a1,p1,\[Lambda]2,a2,p2,\[Lambda]3,a3,p3}  ]] = Get["./Libs/GravitonGluonGhostVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Gluon-Ghost vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@DummyArray[cursor]);
	Remove[\[Lambda]1,a1,p1,\[Lambda]2,a2,p2,\[Lambda]3,a3,p3];
]


Module[{cursor},
	cursor = 1;
	Clear[GravitonVertex];
	While[FileExistsQ["./Libs/GravitonVertex_"<>ToString[cursor]],
		Evaluate[GravitonVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@dummyArrayP[cursor+2])]] = Get["./Libs/GravitonVertex_"<>ToString[cursor]];
		cursor++;
	];
	cursor = 1;
	Clear[GravitonGhostVertex];
	While[FileExistsQ["./Libs/GravitonGhostVertex_"<>ToString[cursor]],
		Evaluate[GravitonGhostVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArrayP[cursor],{\[Lambda]1,k1,\[Lambda]2,k2}])]] = Get["./Libs/GravitonGhostVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArrayP[cursor]);
	Remove[\[Lambda]1,\[Lambda]2,k1,k2];
]


Remove[FeynGrav`dummyArrayP];
Remove[FeynGrav`n];
Remove[FeynGrav`\[Lambda]1,FeynGrav`\[Lambda]2,FeynGrav`p1,FeynGrav`p2];
Remove[FeynGrav`a,FeynGrav`\[Mu]];
Remove[FeynGrav`p1,FeynGrav`\[Mu]1,FeynGrav`a1,FeynGrav`p2,FeynGrav`\[Mu]2,FeynGrav`a2,FeynGrav`p3,FeynGrav`\[Mu]3,FeynGrav`a3,FeynGrav`p4,FeynGrav`\[Mu]4,FeynGrav`a4];
Remove[cursor];
ResetDirectory[];


Begin["Private`"];


ScalarPropagator[p_,m_] = I FAD[{p,m}];


GravitonPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I 1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]])FAD[k];
GravitonPropagatorTop[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_]=1/2(MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]);
GravitonPropagatorAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I ((1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]))/SPD[k,k]);


FeynGrav`GaugeFixingEpsilon =0;
Print["The Gauge fixing parameter \[CurlyEpsilon] is set to zero."];


End[];


EndPackage[];
