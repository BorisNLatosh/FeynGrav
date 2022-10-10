(* ::Package:: *)

BeginPackage["FeynGrav`",{"FeynCalc`"}];


Print[Style["FeynGrav version 1.3",Bold]]
Print["FeynGrav: FeynGravCommands print the list of all supported commands."]
Print["FeynGrav: Use '?CommandName' to see a brief description."]
Print["FeynGrav: Examples can be found in FeynGrav_Examples.nb and ArXiV:2201.06812."]


GravitonVertex::usage = "GravitonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\[Ellipsis]]. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are Lorentz indices of gravitons. \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta of gravitons."
GravitonPropagator::usage = "GravitonPropagator[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Graviton propagator in the harmonic gauge. \[Mu],\[Nu] are indices of the first vertex. \[Alpha],\[Beta] are indices of the second vertex. p is the graviton momentum. "
GravitonPropagatorTop::usage = "GravitonPropagatorTop[\[Mu],\[Nu],\[Alpha],\[Beta]]. Nominator of the graviton propagator in the harmonic gauge."
GravitonPropagatorAlternative::usage = "GravitonPropagatorAlternative[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Graviton propagator in the harmonic gauge realized without FAD.\[Mu],\[Nu] are indices of the first vertex. \[Alpha],\[Beta] are indices of the second vertex. p is the graviton momentum."


GravitonScalarVertex::usage = "GravitonScalarVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for gravitational interacation of a massless scalar field kinetic enery. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta. All momenta are in-going."
GravitonMassiveScalarVertex::usage = "GravitonMassiveScalarVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Vertex for gravitational interacation of a scalar field kinetic enery with a non-vanishing mass. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta. m is the scalar field mass. All momenta are in-going."
GravitonVectorVertex::usage = "GravitonVectorVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for gravitational interacation of a massless vector field kinetic enery. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vectors Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are vector field momenta. All momenta are in-going."
GravitonMassiveVectorVertex::usage = "GravitonVectorVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Lambda]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Vertex for gravitational interacation of a vector field kinetic enery with a non-vanishing mass. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\) are vectors Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are vector field momenta. m is the vector field mass. All momenta are in-going."
GravitonFermionVertex::usage = "GravitonFermionVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\)]. Vertex for gravitational interacation of a massless Dirac fermion kinetic enery. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are fermion momenta. All momenta are in-going."
GravitonMassiveFermionVertex::usage = "GravitonMassiveFermionVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),m]. Vertex for gravitational interacation of a Dirac fermion kinetic enery with a non-vanishing mass. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\) are graviton Lorentz indices. \!\(\*SubscriptBox[\(p\), \(i\)]\) are fermion momenta. m is the fermion mass. All momenta are in-going."
(*GravitonQuarkGluonVertex::usage = "GravitonQuarkGluonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\[Sigma],a]. \!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\) are indices of graviton lines. \[Sigma] is the Lorentz index. a is the color index."
GravitonThreeGluonVertex::usage = "GravitonThreeGluonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\)]. \!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\) are indices of graviton lines. \!\(\*SubscriptBox[\(p\), \(i\)]\) are gluon momenta. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\) are gluon Lorentz indices. \!\(\*SubscriptBox[\(a\), \(i\)]\) are color indices."
GravitonFourGluonVertex::usage = "GravitonFourGluonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(a\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(a\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(a\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(4\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(4\)]\),\!\(\*SubscriptBox[\(a\), \(4\)]\)]. \!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\) are indices of graviton lines. \!\(\*SubscriptBox[\(p\), \(i\)]\) are gluon momenta. \!\(\*SubscriptBox[\(\[Mu]\), \(i\)]\) are gluon Lorentz indices. \!\(\*SubscriptBox[\(a\), \(i\)]\) are color indices."*)


GaugeProjector::usage = "GaugeProjector[\[Mu],\[Nu],p]. The standard gauge projectors \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\)-\!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator1::usage = "NieuwenhuizenOperator1[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(1\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Alpha]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Alpha]\)]\)). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."
NieuwenhuizenOperator2::usage = "NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(2\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\))-\!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0::usage = "NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(0\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0Bar::usage = "NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) =\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to the standard gauge projector \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator0BarBar::usage = "NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],p]. Nieuwenhuizen operator (\!\(\*OverscriptBox[OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."


FeynGravCommands := Print[" 'GravitonVertex','GravitonScalarVertex','GravitonMassiveScalarVertex','GravitonVectorVertex','GravitonFermionVertex','GravitonMassiveFermionVertex', 'GravitonPropagator', 'GravitonPropagatorTop', 'GravitonPropagatorAlternative', 'GaugeProjector', 'NieuwenhuizenOperator1', 'NieuwenhuizenOperator2', 'NieuwenhuizenOperator0', 'NieuwenhuizenOperator0Bar', 'NieuwenhuizenOperator1BarBar' "];


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
		Evaluate[GravitonScalarVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArray[cursor],{p1,p2}])]] = Get["./Libs/GravitonScalarVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Scalar vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArray[cursor]);
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonMassiveScalarVertex];
	While[FileExistsQ["./Libs/GravitonMassiveScalarVertex_"<>ToString[cursor]],
		Evaluate[GravitonMassiveScalarVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArray[cursor],{p1,p2,m}])]] = Get["./Libs/GravitonMassiveScalarVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Massive Scalar vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArray[cursor]);
	Remove[m];
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonFermionVertex];
	While[FileExistsQ["./Libs/GravitonFermionVertex_"<>ToString[cursor]],
		Evaluate[GravitonFermionVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArray[cursor],{p1,p2}])]] = Get["./Libs/GravitonFermionVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Fermion vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArray[cursor]);
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonMassiveFermionVertex];
	While[FileExistsQ["./Libs/GravitonMassiveFermionVertex_"<>ToString[cursor]],
		Evaluate[GravitonMassiveFermionVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArray[cursor],{p1,p2,m}])]] = Get["./Libs/GravitonMassiveFermionVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Massive Fermion vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArray[cursor]);
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

(*Module[{cursor},
	cursor = 1;
	Clear[GravitonQuarkGluonVertex];
	While[FileExistsQ["./Libs/GravitonQuarkGluonVertex_"<>ToString[cursor]],
		Evaluate[GravitonQuarkGluonVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArray[cursor],{\[Mu],a}])]] = Get["./Libs/GravitonQuarkGluonVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Quark-Gluon vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArray[cursor]);
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonThreeGluonVertex];
	While[FileExistsQ["./Libs/GravitonThreeGluonVertex_"<>ToString[cursor]],
		Evaluate[GravitonThreeGluonVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArray[cursor],{p1,\[Mu]1,a1,p2,\[Mu]2,a2,p3,\[Mu]3,a3}])]] = Get["./Libs/GravitonThreeGluonVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Three-Quark vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArray[cursor]);
]

Module[{cursor},
	cursor = 1;
	Clear[GravitonFourGluonVertex];
	While[FileExistsQ["./Libs/GravitonFourGluonVertex_"<>ToString[cursor]],
		Evaluate[GravitonFourGluonVertex[Sequence@@(Function[ToExpression[ToString[#]<>"_"]]/@Join[dummyArray[cursor],{p1,\[Mu]1,a1,p2,\[Mu]2,a2,p3,\[Mu]3,a3,p4,\[Mu]4,a4}])]] = Get["./Libs/GravitonFourGluonVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Four-Quark vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove/@(Function[ToExpression["FeynGrav`"<>ToString[#]]]/@dummyArray[cursor]);
]*)

Remove[FeynGrav`dummyArrayP,FeynGrav`dummyArray];
Remove[FeynGrav`n];
Remove[FeynGrav`\[Lambda]1,FeynGrav`\[Lambda]2,FeynGrav`p1,FeynGrav`p2];
Remove[FeynGrav`a,FeynGrav`\[Mu]];
Remove[FeynGrav`p1,FeynGrav`\[Mu]1,FeynGrav`a1,FeynGrav`p2,FeynGrav`\[Mu]2,FeynGrav`a2,FeynGrav`p3,FeynGrav`\[Mu]3,FeynGrav`a3,FeynGrav`p4,FeynGrav`\[Mu]4,FeynGrav`a4];
Remove[cursor];
ResetDirectory[];


Begin["Private`"];


(* Graviton propagators *)
GravitonPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I 1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]])FAD[k];
GravitonPropagatorTop[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_]=1/2(MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]);
GravitonPropagatorAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I ((1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]))/SPD[k,k]);


(* GaugeProjector *)
GaugeProjector = {\[Mu],\[Nu],k} |-> Pair[ LorentzIndex[\[Mu],D], LorentzIndex[\[Nu],D] ] - (Pair[Momentum[k,D], LorentzIndex[\[Mu],D]] Pair[Momentum[k,D],LorentzIndex[\[Nu],D]])/(Pair[ Momentum[k,D], Momentum[k,D]]);


(* Nieuwenhuizen Operators *)

NieuwenhuizenOperator1= {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->1/2 (GaugeProjector[\[Mu],\[Alpha],k](Pair[LorentzIndex[\[Nu],D],LorentzIndex[\[Beta],D]]-GaugeProjector[\[Nu],\[Beta],k])+GaugeProjector[\[Mu],\[Beta],k](Pair[LorentzIndex[\[Nu],D],LorentzIndex[\[Alpha],D]]-GaugeProjector[\[Nu],\[Alpha],k])+GaugeProjector[\[Nu],\[Alpha],k](Pair[LorentzIndex[\[Mu],D],LorentzIndex[\[Beta],D]]-GaugeProjector[\[Mu],\[Beta],k])+GaugeProjector[\[Nu],\[Beta],k](Pair[LorentzIndex[\[Mu],D],LorentzIndex[\[Alpha],D]]-GaugeProjector[\[Mu],\[Alpha],k]));
NieuwenhuizenOperator2= {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->1/2 (GaugeProjector[\[Mu],\[Alpha],k]GaugeProjector[\[Nu],\[Beta],k]+GaugeProjector[\[Mu],\[Beta],k]GaugeProjector[\[Nu],\[Alpha],k])-1/3 GaugeProjector[\[Mu],\[Nu],k]GaugeProjector[\[Alpha],\[Beta],k];
NieuwenhuizenOperator0= {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->1/3 GaugeProjector[\[Mu],\[Nu],k]GaugeProjector[\[Alpha],\[Beta],k];
NieuwenhuizenOperator0Bar= {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->(Pair[LorentzIndex[\[Mu],D],LorentzIndex[\[Nu],D]]-GaugeProjector[\[Mu],\[Nu],k])(Pair[LorentzIndex[\[Alpha],D],LorentzIndex[\[Beta],D]]-GaugeProjector[\[Alpha],\[Beta],k]);
NieuwenhuizenOperator0BarBar= {\[Mu],\[Nu],\[Alpha],\[Beta],k}|->GaugeProjector[\[Mu],\[Nu],k](Pair[LorentzIndex[\[Alpha],D],LorentzIndex[\[Beta],D]]-GaugeProjector[\[Alpha],\[Beta],k])+(Pair[LorentzIndex[\[Mu],D],LorentzIndex[\[Nu],D]]-GaugeProjector[\[Mu],\[Nu],k])GaugeProjector[\[Alpha],\[Beta],k];


End[];


EndPackage[];
