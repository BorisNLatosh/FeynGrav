(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];
Needs["Nieuwenhuizen`","./Rules/Nieuwenhuizen.wl"];
SetDirectory[DirectoryName[$InputFileName]];

BeginPackage["FeynGrav`",{"FeynCalc`"}];
Print[Style["FeynGrav version 1.4 experimental",Bold]];
Print["FeynGrav: FeynGravCommands print the list of all supported commands."];
Print["FeynGrav: Use '?CommandName' to see a brief description."];
Print["FeynGrav: Examples can be found in FeynGrav_Examples.nb and ArXiV:2201.06812."];


GravitonScalarVertex::usage = "GravitonScalarVertex[\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Expression for gravitational interaction of a scalar field kinetic energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta, m is the scalar field mass.";
GravitonScalarPotentialVertex::usage = "GravitonScalarPotentialVertex[\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(p\)]\)]. Expression for gravitational interaction of a scalar field potential energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are gravitational indices, \!\(\*SubscriptBox[\(\[Lambda]\), \(l\)]\) is the scalar field self-coupling constant.";


GravitonFermionVertex::usage = "GravitonFermionVertex[\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Expression for gravitational interaction of a Dirac field kinetic energy. {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are graviton indices, \!\(\*SubscriptBox[\(p\), \(i\)]\) are fermion momenta, m is the fermion mass.";


GravitonVertex::usage = "GravitonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\[Ellipsis]]. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are Lorentz indices of gravitons. \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of gravitons."
GravitonPropagator::usage = "GravitonPropagator[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Graviton propagator in the harmonic gauge. \[Mu],\[Nu] are indices of the first vertex. \[Alpha],\[Beta] are indices of the second vertex. p is the graviton momentum. "
GravitonPropagatorTop::usage = "GravitonPropagatorTop[\[Mu],\[Nu],\[Alpha],\[Beta]]. Nominator of the graviton propagator in the harmonic gauge."
GravitonPropagatorAlternative::usage = "GravitonPropagatorAlternative[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Graviton propagator in the harmonic gauge realized without FAD.\[Mu],\[Nu] are indices of the first vertex. \[Alpha],\[Beta] are indices of the second vertex. p is the graviton momentum."


GravitonVectorVertex::usage = "GravitonVectorVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for gravitational interacation of a massless vector field kinetic enery. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vectors Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are vector field momenta. All momenta are in-going."
GravitonMassiveVectorVertex::usage = "GravitonVectorVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Vertex for gravitational interacation of a vector field kinetic enery with a non-vanishing mass. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vectors Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are vector field momenta. m is the vector field mass. All momenta are in-going."


FeynGravCommands := Print[" 'GravitonVertex','GravitonScalarVertex','GravitonVectorVertex','GravitonFermionVertex', 'GravitonPropagator', 'GravitonPropagatorTop', 'GravitonPropagatorAlternative', 'GaugeProjector', 'NieuwenhuizenOperator1', 'NieuwenhuizenOperator2', 'NieuwenhuizenOperator0', 'NieuwenhuizenOperator0Bar', 'NieuwenhuizenOperator1BarBar' "];


SetDirectory[DirectoryName[$InputFileName]];

dummyArrayP=n|->Flatten[Function[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["p"<>ToString[#]]}]/@Range[n]];
dummyArray=n|->Flatten[Function[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]]}]/@Range[n]];

Module[{cursor},
	cursor = 1;
	Clear[GravitonVertex];
	While[FileExistsQ["./Libs/GravitonVertex_"<>ToString[cursor]],
		Evaluate[GravitonVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@dummyArrayP[cursor+2])]] = Get["./Libs/GravitonVertex_"<>ToString[cursor]];
		cursor++;
	];
	If[cursor-1<=0,Print["Graviton vertices are imported up to order 0"],Print["Graviton vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."]];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArrayP[cursor+2]);
];

Module[{cursor},
	cursor = 1;
	Clear[GravitonScalarVertex];
	While[FileExistsQ["./Libs/GravitonScalarVertex_"<>ToString[cursor]],
		Evaluate[GravitonScalarVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArray[cursor],{p1,p2,m}])]] = Get["./Libs/GravitonScalarVertex_"<>ToString[cursor]];
		cursor++;
	];
	cursor = 1;
	Clear[GravitonScalarPotentialVertex];
	While[FileExistsQ["./Libs/GravitonScalarPotentialVertex_"<>ToString[cursor]],
		Evaluate[GravitonScalarPotentialVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArray[cursor],{\[Lambda]}])]] = Get["./Libs/GravitonScalarPotentialVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Scalar vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArray[cursor]);
	Remove[p1,p2,m,\[Lambda]];
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonFermionVertex];
	While[FileExistsQ["./Libs/GravitonFermionVertex_"<>ToString[cursor]],
		Evaluate[GravitonFermionVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArray[cursor],{p1,p2,m}])]] = Get["./Libs/GravitonFermionVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Fermion vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArray[cursor]);
	Remove[p1,p2,m];
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonVectorVertex];
	While[FileExistsQ["./Libs/GravitonVectorVertex_"<>ToString[cursor]],
		Evaluate[GravitonVectorVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArray[cursor],{\[Lambda]1,\[Lambda]2,p1,p2}])]] = Get["./Libs/GravitonVectorVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Vector vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArray[cursor]);
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonMassiveVectorVertex];
	While[FileExistsQ["./Libs/GravitonMassiveVectorVertex_"<>ToString[cursor]],
		Evaluate[GravitonMassiveVectorVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArray[cursor],{\[Lambda]1,\[Lambda]2,p1,p2,m}])]] = Get["./Libs/GravitonMassiveVectorVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Massive Vector vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArray[cursor]);
]

Remove[FeynGrav`dummyArrayP,FeynGrav`dummyArray];
Remove[FeynGrav`n];
Remove[FeynGrav`\[Lambda]1,FeynGrav`\[Lambda]2,FeynGrav`p1,FeynGrav`p2];
Remove[FeynGrav`a,FeynGrav`\[Mu]];
Remove[FeynGrav`p1,FeynGrav`\[Mu]1,FeynGrav`a1,FeynGrav`p2,FeynGrav`\[Mu]2,FeynGrav`a2,FeynGrav`p3,FeynGrav`\[Mu]3,FeynGrav`a3,FeynGrav`p4,FeynGrav`\[Mu]4,FeynGrav`a4];
Remove[cursor];
ResetDirectory[];


Begin["Private`"];


GravitonPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I 1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]])FAD[k];
GravitonPropagatorTop[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_]=1/2(MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]);
GravitonPropagatorAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I ((1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]))/SPD[k,k]);


End[];


EndPackage[];
