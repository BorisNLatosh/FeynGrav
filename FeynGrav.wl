(* ::Package:: *)

BeginPackage["FeynGrav`",{"FeynCalc`"}];


Print[Style["FeynGrav version 1.3",Bold]]
Print["FeynGrav: FeynGravCommands print the list of all supported commands."]
Print["FeynGrav: Use '?CommandName' to see a brief description."]
Print["FeynGrav: Examples can be found in FeynGrav_Examples.nb and ArXiV:2201.06812."]


GravitonVertex::usage = "Vertex for interaction of 3 or more gravitons. Its arguments are Lorentz indices and momenta of the corresponding gravitons. For instance GravitonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\)]."
GravitonPropagator::usage = "Propagator of a graviton in the harmonic gauge. Its denominator is given via FAD function."
GravitonPropagatorTop::usage = "The nominator of a graviton propagator in the harmonic gauge."
GravitonPropagatorAlternative::usage = "Propagator of a graviton in the harmonic gauge. Its denominator is given via SPD function."


GravitonScalarVertex::usage = "Vertex for interaction between a massless scalar field kinetic energy and gravitons. Takes 2n + 2 arguments. First 2n arguments are Lorentz indices of gravitons. The last two arguments are ingoint momenta of scalars."
GravitonMassiveScalarVertex::usage = "Vertex for interaction between a kinetic energy of a scalar field with a non-vanishing mass and gravitons. Takes 2n + 2 + 1 arguments. First 2n arguments are Lorentz indices of gravitons. Two other arguments are ingoint momenta of scalars. The last argument is the scalar field mass."
GravitonVectorVertex::usage = "Vertex for interaction between a massless vector field kinetic energy and gravitons. Takes 2n + 4 arguments. First 2n arguments are Lotentz indices of gravitons. The other two arguments are Lorentz indices of vectors. The last two arguments are vectors momenta."
GravitonFermionVertex::usage = "Vertex for interaction between a massless Dirac fermion kinetic energy and gravitons. Takes 2n + 2 arguments. First 2n arguments are Lorentz indices of gravitons. The last two arguments are ingoint momenta of fermions."
GravitonMassiveFermionVertex::usage = "Vertex for interaction between a kinetic energy of a Dirac fermion with a non-vanishing mass and gravitons. Takes 2n + 2 + 1 arguments. First 2n arguments are Lorentz indices of gravitons. The oher two arguments are ingoint momenta of fermions. The last argument is the fermion mass."


GaugeProjector::usage = "The standard gauge projectors \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\)-\!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator1::usage = "Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(1\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Alpha]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Alpha]\)]\)). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."
NieuwenhuizenOperator2::usage = "Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(2\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\))-\!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0::usage = "Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(0\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0Bar::usage = "Nieuwenhuizen operator (\!\(\*OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) =\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to the standard gauge projector \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator0BarBar::usage = "Nieuwenhuizen operator (\!\(\*OverscriptBox[OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."


FeynGravCommands := Print[" 'GravitonVertex','GravitonScalarVertex','GravitonMassiveScalarVertex','GravitonVectorVertex','GravitonFermionVertex','GravitonMassiveFermionVertex', 'GravitonPropagator', 'GravitonPropagatorTop', 'GravitonPropagatorAlternative', 'GaugeProjector', 'NieuwenhuizenOperator1', 'NieuwenhuizenOperator2', 'NieuwenhuizenOperator0', 'NieuwenhuizenOperator0Bar', 'NieuwenhuizenOperator1BarBar' "];


SetDirectory[DirectoryName[$InputFileName]];

dummyArrayP=n\[Function]Flatten[Function[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]],ToExpression["p"<>ToString[#]]}]/@Range[n]];
dummyArray=n\[Function]Flatten[Function[{ToExpression["m"<>ToString[#]],ToExpression["n"<>ToString[#]]}]/@Range[n]];

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

Remove[FeynGrav`dummyArrayP,FeynGrav`dummyArray];
Remove[FeynGrav`n];
Remove[FeynGrav`\[Lambda]1,FeynGrav`\[Lambda]2,FeynGrav`p1,FeynGrav`p2];
Remove[cursor];
ResetDirectory[];


Begin["Private`"];


(* Graviton propagators *)
GravitonPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I 1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]])FAD[k];
GravitonPropagatorTop[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_]=1/2(MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]);
GravitonPropagatorAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I ((1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]))/SPD[k,k]);


(* GaugeProjector *)
GaugeProjector = {\[Mu],\[Nu],k} \[Function] Pair[ LorentzIndex[\[Mu],D], LorentzIndex[\[Nu],D] ] - (Pair[Momentum[k,D], LorentzIndex[\[Mu],D]] Pair[Momentum[k,D],LorentzIndex[\[Nu],D]])/(Pair[ Momentum[k,D], Momentum[k,D]]);


(* Nieuwenhuizen Operators *)

NieuwenhuizenOperator1= {\[Mu],\[Nu],\[Alpha],\[Beta],k}\[Function]1/2 (GaugeProjector[\[Mu],\[Alpha],k](Pair[LorentzIndex[\[Nu],D],LorentzIndex[\[Beta],D]]-GaugeProjector[\[Nu],\[Beta],k])+GaugeProjector[\[Mu],\[Beta],k](Pair[LorentzIndex[\[Nu],D],LorentzIndex[\[Alpha],D]]-GaugeProjector[\[Nu],\[Alpha],k])+GaugeProjector[\[Nu],\[Alpha],k](Pair[LorentzIndex[\[Mu],D],LorentzIndex[\[Beta],D]]-GaugeProjector[\[Mu],\[Beta],k])+GaugeProjector[\[Nu],\[Beta],k](Pair[LorentzIndex[\[Mu],D],LorentzIndex[\[Alpha],D]]-GaugeProjector[\[Mu],\[Alpha],k]));
NieuwenhuizenOperator2= {\[Mu],\[Nu],\[Alpha],\[Beta],k}\[Function]1/2 (GaugeProjector[\[Mu],\[Alpha],k]GaugeProjector[\[Nu],\[Beta],k]+GaugeProjector[\[Mu],\[Beta],k]GaugeProjector[\[Nu],\[Alpha],k])-1/3 GaugeProjector[\[Mu],\[Nu],k]GaugeProjector[\[Alpha],\[Beta],k];
NieuwenhuizenOperator0= {\[Mu],\[Nu],\[Alpha],\[Beta],k}\[Function]1/3 GaugeProjector[\[Mu],\[Nu],k]GaugeProjector[\[Alpha],\[Beta],k];
NieuwenhuizenOperator0Bar= {\[Mu],\[Nu],\[Alpha],\[Beta],k}\[Function](Pair[LorentzIndex[\[Mu],D],LorentzIndex[\[Nu],D]]-GaugeProjector[\[Mu],\[Nu],k])(Pair[LorentzIndex[\[Alpha],D],LorentzIndex[\[Beta],D]]-GaugeProjector[\[Alpha],\[Beta],k]);
NieuwenhuizenOperator0BarBar= {\[Mu],\[Nu],\[Alpha],\[Beta],k}\[Function]GaugeProjector[\[Mu],\[Nu],k](Pair[LorentzIndex[\[Alpha],D],LorentzIndex[\[Beta],D]]-GaugeProjector[\[Alpha],\[Beta],k])+(Pair[LorentzIndex[\[Mu],D],LorentzIndex[\[Nu],D]]-GaugeProjector[\[Mu],\[Nu],k])GaugeProjector[\[Alpha],\[Beta],k];


End[];


EndPackage[];
