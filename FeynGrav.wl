(* ::Package:: *)

BeginPackage["FeynGrav`",{"FeynCalc`"}];


Print[Style["FeynGrav version 1.0",Bold]]
Print["FeynGrav: All expressions for itteraction veritce are taken from Libs directory for the sake of performance."]
Print["FeynGrav: 'FeynGrav_Libraries_Generator.wls' contains a script that generates expressions for the interaction vertices. "]
Print["FeynGrav: FeynGravCommands print the list of all supported commands."]
Print["FeynGrav: Use '?CommandName' to see a brief description."]
Print["FeynGrav: Examples can be found in 'FeynGrav_Examples.nb'."]


GravitonVertex::usage = "Vertex for interaction of 3 or more gravitons. Its arguments are Lorentz indices and momenta of the corresponding gravitons. For instance GravitonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\)]."
GravitonPropagator::usage = "Propagator of a graviton in the harmonic gauge. Its denominator is given via FAD function."
GravitonPropagatorTop::usage = "The nominator of a graviton propagator in the harmonic gauge."
GravitonPropagatorAlternative::usage = "Propagator of a graviton in the harmonic gauge. Its denominator is given via SPD function."


GravitonScalarVertex::usage = "Vertex for interaction between a massless scalar kinetic energy and gravitons. Takes 2n + 2 arguments. First 2n arguments are Lorentz indices of gravitons. The last two arguments are ingoint momenta of scalars."
GravitonVectorVertex::usage = "Vertex for interaction between a massless vector field kinetic energy and gravitons. Takes 2n + 4 arguments. First 2n arguments are Lotentz indices of gravitons. The next two arguments are Lorentz indices of vectors. The last two arguments are vectors momenta."
GravitonFermionVertex::usage = "Vertex for interaction between a massless Dirac fermion kinetic energy and gravitons. Takes 2n + 2 arguments. First 2n arguments are Lorentz indices of gravitons. The last two arguments are ingoint momenta of fermions."


GaugeProjector::usage = "The standard gauge projectors \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\)-\!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator1::usage = "Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(1\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Alpha]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Alpha]\)]\)). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."
NieuwenhuizenOperator2::usage = "Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(2\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\))-\!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0::usage = "Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(0\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0Bar::usage = "Nieuwenhuizen operator (\!\(\*OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) =\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to the standard gauge projector \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator0BarBar::usage = "Nieuwenhuizen operator (\!\(\*OverscriptBox[OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."


FeynGravCommands := Print[" 'GravitonVertex', 'GravitonPropagator', 'GravitonPropagatorTop', 'GravitonPropagatorAlternative', 'GravitonFermionVertex', 'GaugeProjector', 'NieuwenhuizenOperator1', 'NieuwenhuizenOperator2', 'NieuwenhuizenOperator0', 'NieuwenhuizenOperator0Bar', 'NieuwenhuizenOperator1BarBar' "];


SetDirectory[DirectoryName[$InputFileName]];
Module[{cursor,i},
	cursor = 1;
	Clear[GravitonVertex];
	While[FileExistsQ["./Libs/GravitonVertex_"<>ToString[cursor]],
		Evaluate[GravitonVertex@@Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"],ToExpression["p"<>ToString[i]<>"_"]},{i,1,cursor}]]] = Get["./Libs/GravitonVertex_"<>ToString[cursor]];
		cursor++;
	];
	If[cursor-1-2<0,Print["Graviton vertices are imported up to order 0"],Print["Graviton vertices are imported up to order "<>ToString[cursor-1-2]<>" in \[Kappa]."]];
	Remove@@Function[ToExpression["m"<>ToString[#]]]/@Range[cursor];
	Remove@@Function[ToExpression["n"<>ToString[#]]]/@Range[cursor];
	Remove@@Function[ToExpression["p"<>ToString[#]]]/@Range[cursor];
	];
Module[{cursor,p1,p2,i},
	cursor = 1;
	Clear[GravitonScalarVertex];
	While[FileExistsQ["./Libs/GravitonScalarVertex_"<>ToString[cursor]],
		Evaluate[GravitonScalarVertex@@Flatten[{Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,cursor}],{ToExpression["p1_"],ToExpression["p2_"]}}]] = Get["./Libs/GravitonScalarVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Scalar vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove@@Function[ToExpression["m"<>ToString[#]]]/@Range[cursor];
	Remove@@Function[ToExpression["n"<>ToString[#]]]/@Range[cursor];
	Remove[p1,p2];
	
	Remove[cursor,i];
]
Module[{cursor,p1,p2,i},
	cursor = 1;
	Clear[GravitonFermionVertex];
	While[FileExistsQ["./Libs/GravitonFermionVertex_"<>ToString[cursor]],
		Evaluate[GravitonFermionVertex@@Flatten[{Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,cursor}],{ToExpression["p1_"],ToExpression["p2_"]}}]] = Get["./Libs/GravitonFermionVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Fermion vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove@@Function[ToExpression["m"<>ToString[#]]]/@Range[cursor];
	Remove@@Function[ToExpression["n"<>ToString[#]]]/@Range[cursor];
	Remove[p1,p2];
	
	Remove[cursor,i];
]
Module[{cursor,p1,p2,\[Lambda]1,\[Lambda]2,i},
	cursor = 1;
	Clear[GravitonVectorVertex];
	While[FileExistsQ["./Libs/GravitonVectorVertex_"<>ToString[cursor]],
		Evaluate[GravitonVectorVertex@@Flatten[{Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,cursor}],{ToExpression["\[Lambda]1_"],ToExpression["\[Lambda]2_"],ToExpression["p1_"],ToExpression["p2_"]}}]] = Get["./Libs/GravitonVectorVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Vector vertices are imported up to order "<>ToString[cursor-1]<>" in \[Kappa]."];
	Remove@@Function[ToExpression["m"<>ToString[#]]]/@Range[cursor];
	Remove@@Function[ToExpression["n"<>ToString[#]]]/@Range[cursor];
	Remove[p1,p2,\[Lambda]1,\[Lambda]2];
	
	Remove[cursor,i];
]
ResetDirectory[];


Begin["Private`"];


(* Graviton propagators *)
GravitonPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I 1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]])FAD[k];
GravitonPropagatorTop[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_]=1/2(MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]);
GravitonPropagatorAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I ((1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]))/SPD[k,k]);


(* GaugeProjector *)
GaugeProjector[inputArray__]:=Module[{inputData},
		inputData = List[inputArray];
		If[Length[inputData]!=3,Return[0]];
		Return[MTD@@inputData[[1;;2]] - (Pair[Momentum[inputData[[3]],D],LorentzIndex[inputData[[1]],D]]Pair[Momentum[inputData[[3]],D],LorentzIndex[inputData[[2]],D]])/Pair[Momentum[inputData[[3]],D],Momentum[inputData[[3]],D]]];
];


(* Nieuwenhuizen Operators *)
NieuwenhuizenOperator1[inputArray__]:=Module[{inputData,indexArray,theMomentum},
	inputData = List[inputArray];
	If[Length[inputData]!=5,Return[0]];
	indexArray = inputData[[1;;4]];
	theMomentum = inputData[[5]];
	Return[ 1/2 ( GaugeProjector[indexArray[[1]],indexArray[[3]],theMomentum] (-GaugeProjector[indexArray[[2]],indexArray[[4]],theMomentum] + MTD[indexArray[[2]],indexArray[[4]]]) + GaugeProjector[indexArray[[1]],indexArray[[4]],theMomentum] (-GaugeProjector[indexArray[[2]],indexArray[[3]],theMomentum] + MTD[indexArray[[2]],indexArray[[3]]]) + GaugeProjector[indexArray[[2]],indexArray[[4]],theMomentum] (-GaugeProjector[indexArray[[1]],indexArray[[3]],theMomentum] + MTD[indexArray[[1]],indexArray[[3]]]) + GaugeProjector[indexArray[[2]],indexArray[[3]],theMomentum] (-GaugeProjector[indexArray[[1]],indexArray[[4]],theMomentum] + MTD[indexArray[[1]],indexArray[[4]]]) )];
];

NieuwenhuizenOperator2[inputArray__]:=Module[{inputData,indexArray,theMomentum},
	inputData = List[inputArray];
	If[Length[inputData]!=5,Return[0]];
	indexArray = inputData[[1;;4]];
	theMomentum = inputData[[5]];
	Return[ 1/2 (GaugeProjector[indexArray[[1]],indexArray[[3]],theMomentum] GaugeProjector[indexArray[[2]],indexArray[[4]],theMomentum] + GaugeProjector[indexArray[[1]],indexArray[[4]],theMomentum] GaugeProjector[indexArray[[2]],indexArray[[3]],theMomentum]) -1/3 (GaugeProjector[indexArray[[1]],indexArray[[2]],theMomentum]GaugeProjector[indexArray[[3]],indexArray[[4]],theMomentum]) ];
];

NieuwenhuizenOperator0[inputArray__]:=Module[{inputData,indexArray,theMomentum},
	inputData = List[inputArray];
	If[Length[inputData]!=5,Return[0]];
	indexArray = inputData[[1;;4]];
	theMomentum = inputData[[5]];
	Return[ 1/3 (GaugeProjector[indexArray[[1]],indexArray[[2]],theMomentum]GaugeProjector[indexArray[[3]],indexArray[[4]],theMomentum]) ];
];

NieuwenhuizenOperator0Bar[inputArray__]:=Module[{inputData,indexArray,theMomentum},
	inputData = List[inputArray];
	If[Length[inputData]!=5,Return[0]];
	indexArray = inputData[[1;;4]];
	theMomentum = inputData[[5]];
	Return[ (MTD[indexArray[[1]],indexArray[[2]]]-GaugeProjector[indexArray[[1]],indexArray[[2]],theMomentum])(MTD[indexArray[[3]],indexArray[[4]]]-GaugeProjector[indexArray[[3]],indexArray[[4]],theMomentum]) ];
];

NieuwenhuizenOperator0BarBar[inputArray__]:=Module[{inputData,indexArray,theMomentum},
	inputData = List[inputArray];
	If[Length[inputData]!=5,Return[0]];
	indexArray = inputData[[1;;4]];
	theMomentum = inputData[[5]];
	Return[ GaugeProjector[indexArray[[1]],indexArray[[2]],theMomentum](MTD[indexArray[[3]],indexArray[[4]]]-GaugeProjector[indexArray[[3]],indexArray[[4]],theMomentum])+(MTD[indexArray[[1]],indexArray[[2]]]-GaugeProjector[indexArray[[1]],indexArray[[2]],theMomentum])GaugeProjector[indexArray[[3]],indexArray[[4]],theMomentum] ];
];


End[];


EndPackage[];
