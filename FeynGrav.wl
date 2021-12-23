(* ::Package:: *)

BeginPackage["FeynGrav`",{"FeynCalc`"}];


Print["\!\(\*
StyleBox[\"FeynGrav\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"version 1.0\",\nFontWeight->\"Bold\"]\)"]
Print["FeynGrav: All expressions for itteraction veritce are evaluated on a call."]
Print["FeynGrav: Please, be patient. Evaluation can take some time."]
Print["FeynGrav: FeynGravCommands print the list of all supported commands."]
Print["FeynGrav: Use '?CommandName' to see a brief description."]


(*Vierbein::usage = "Expansion of a vierbein \!\(\*SubscriptBox[SuperscriptBox[\(\[GothicE]\), \(\[Mu]\)], \(\[Nu]\)]\). It takes an even number of arguments."
CITensor::usage = "CI-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) expansion. Take 2 or more arguments."
CIITensor::usage = "CII-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\) expansion. Take 4 or more arguments." 
CETensor::usage = "CE-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) \!\(\*SubscriptBox[SuperscriptBox[\(\[GothicE]\), \(\[Mu]\)], \(\[Nu]\)]\) expansion. Take 2 or more arguments."
CIIITensor::usage = "CIII-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\) expansion. Take 6 or more arguments."*)


GravitonVertex::usage = "Vertex for interaction of 3 or more gravitons. Its arguments are Lorentz indices and momenta of the corresponding gravitons. For instance GravitonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\)]."
GravitonPropagator::usage = "Propagator of a graviton in the harmonic gauge. Its denominator is given via FAD function."
GravitonPropagatorTop::usage = "The nominator of a graviton propagator in the harmonic gauge."
GravitonPropagatorAlternative::usage = "Propagator of a graviton in the harmonic gauge. Its denominator is given via SPD function."


GravitonScalarVertex::usage = "Vertex for interaction between a massless scalar kinetic energy and gravitons. Takes 2n + 2 arguments. First 2n arguments are Lorentz indices of gravitons. The last two arguments are ingoint momenta of scalars."
(*GravitonVectorVertex::usage = "Vertex for interaction between a massless vector field kinetic energy and gravitons. Takes 2n + 4 arguments. First 2n arguments are Lotentz indices of gravitons. The next two arguments are Lorentz indices of vectors. The last two arguments are vectors momenta."
GravitonFermionVertex::usage = "Vertex for interaction between a massless Dirac fermion kinetic energy and gravitons. Takes 2n + 2 arguments. First 2n arguments are Lorentz indices of gravitons. The last two arguments are ingoint momenta of fermions."*)


GaugeProjector::usage = "The standard gauge projectors \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\)-\!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator1::usage = "Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(1\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Alpha]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Alpha]\)]\)). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."
NieuwenhuizenOperator2::usage = "Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(2\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\))-\!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0::usage = "Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(0\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0Bar::usage = "Nieuwenhuizen operator (\!\(\*OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) =\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to the standard gauge projector \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator0BarBar::usage = "Nieuwenhuizen operator (\!\(\*OverscriptBox[OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."


FeynGravCommands := Print[" 'GravitonVertex', 'GravitonPropagator', 'GravitonPropagatorTop', 'GravitonPropagatorAlternative', 'GravitonFermionVertex', 'GaugeProjector', 'NieuwenhuizenOperator1', 'NieuwenhuizenOperator2', 'NieuwenhuizenOperator0', 'NieuwenhuizenOperator0Bar', 'NieuwenhuizenOperator1BarBar' "];


(* 
The package operates with quite a few functions which should be evaluated up to a certain perturbation order.
The package evaluate these functions firstly and then provide a user to perform calculations.
*)


Module[{cursor,i},
	cursor = 1;
	Clear[GravitonVertex];
	While[FileExistsQ["Libs/GravitonVertex_"<>ToString[cursor]],
		Evaluate[GravitonVertex@@Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"],ToExpression["p"<>ToString[i]<>"_"]},{i,1,cursor}]]] = Get["Libs/GravitonVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton vertices are imported up to order "<>ToString[cursor-1]<>"."];
	Remove@@Function[ToExpression["m"<>ToString[#]]]/@Range[cursor];
	Remove@@Function[ToExpression["n"<>ToString[#]]]/@Range[cursor];
	Remove@@Function[ToExpression["p"<>ToString[#]]]/@Range[cursor];
	];
Module[{cursor,p1,p2,i},
	cursor = 1;
	Clear[GravitonScalarVertex];
	While[FileExistsQ["Libs/GravitonScalarVertex_"<>ToString[cursor]],
		Evaluate[GravitonScalarVertex@@Flatten[{Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,cursor}],{ToExpression["p1_"],ToExpression["p2_"]}}]] = Get["Libs/GravitonScalarVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton vertices are imported up to order "<>ToString[cursor-1]<>"."];
	Remove@@Function[ToExpression["m"<>ToString[#]]]/@Range[cursor];
	Remove@@Function[ToExpression["n"<>ToString[#]]]/@Range[cursor];
	Remove[p1,p2];
	
	Remove[cursor,i];
]


Begin["Private`"];


(* Graviton propagators *)
GravitonPropagator[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I 1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]])FAD[k];
GravitonPropagatorTop[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_]=1/2(MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]);
GravitonPropagatorAlternative[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,k_]=I ((1/2 (MTD[\[Mu],\[Alpha]]MTD[\[Nu],\[Beta]]+MTD[\[Mu],\[Beta]]MTD[\[Nu],\[Alpha]]-MTD[\[Mu],\[Nu]]MTD[\[Alpha],\[Beta]]))/SPD[k,k]);


(* Vierbein *)
Vierbein[inputArray__]:=Module[{inputData},
	inputData = List[inputArray];
	If[Mod[Length[inputData],2]==1,Return[0]];
	Return[  Calc[ Binomial[-1/2,Length[inputData]/2-1] ITensor@@inputData]   ];
];


(* CETensor *)
CETensor[inputArray__]:=Module[{inputData,tensorValence,dummyArray,nonSymmetricExpression,symmetricExpression},
	inputData = List[inputArray];
	If[Mod[Length[inputData],2]==1,Return[0]];
	(* Calculations with a dummy array *)
	tensorValence = Length[inputData]/2;
	dummyArray = Flatten[  Table[ {ToExpression["\[ScriptM]"<>ToString[i]] , ToExpression["\[ScriptN]"<>ToString[i]]} ,{i,1,tensorValence}]  ];
	Evaluate[nonSymmetricExpression @@ Flatten[  Table[ {ToExpression["\[ScriptM]"<>ToString[i]<>"_"] , ToExpression["\[ScriptN]"<>ToString[i]<>"_"]} ,{i,1,tensorValence}]  ] ]=  Calc[  Sum[ ( METensorStructure @@ Sequence[Join[{tensorValence},{j0},{j1},dummyArray]]) ,{j0,0,tensorValence},{j1,0,tensorValence}]  ]  ;
	Evaluate[symmetricExpression @@ Flatten[  Table[ {ToExpression["\[ScriptM]"<>ToString[i]<>"_"] , ToExpression["\[ScriptN]"<>ToString[i]<>"_"]} ,{i,1,tensorValence}]  ] ] = 1/Factorial[tensorValence] Calc[  Plus@@nonSymmetricExpression@@@Table[ Flatten[ Permutations[Partition[dummyArray,2]][[i]] ] , {i,1,Factorial[tensorValence]}]  ];
	Return[ Calc[ symmetricExpression @@ inputData ]];	
];


(* METensorStructure *)
(* This is a completely internal command used in CETensor *)
METensorStructure[inputArray__]:=Module[{inputData,indexArray,indicesCTensor,indicesITensor},
	inputData = List[inputArray];
	(* Consistency checks *)
	If[ (inputData[[1]] == 0)||(inputData[[3]] == 0) , Return[0]];
	If[ inputData[[1]] != inputData[[2]] + inputData[[3]] , Return[0]];
	If[ Length[ inputData[[4;;]] ]/2 != inputData[[1]] , Return[0]];
	If[ (inputData[[1]]==1)&&(inputData[[2]]!=0), Return[0] ];
	(* Calculations *)
	indexArray = inputData[[4;;]];
	indicesITensor = indexArray[[1;;2]];
	If[ inputData[[2]] == 0 , indicesCTensor = {} , indicesCTensor = indexArray[[3;;3 + 2 (inputData[[2]]) -1]]  ];
	If[ inputData[[3]] != 1 , indicesITensor = Join[ indicesITensor , indexArray[[3 + 2 inputData[[2]];; ]] ] ];
	Return[  Calc[(CTensor @@ indicesCTensor) (Vierbein @@ indicesITensor)]  ];
];


(*
(* GravitonFermionVertex *)
GravitonFermionVertex[inputArray__] := Module[{inputData,TVertex,indexArray1,indexArray2},
	inputData = List[inputArray];
	If[ Mod[ Length[inputData],2] == 1 , Return[0]];
	If[ Length[inputData] < 3 , Return[0]];
	TVertex[\[Mu]_,\[Nu]_,p_,q_] = Calc[  1/2 FVD[p-q,\[Nu]]GAD[\[Mu]]  ];
	indexArray1 = Join[ {\[ScriptM],\[ScriptN]}, inputData[[ ;;Length[inputData]-2]] ];
	indexArray2 = Join[ {\[ScriptM],\[ScriptN]}, inputData[[Length[inputData]-1;;]] ];
	Return[  Calc[I Global`\[Kappa]^((Length[inputData]-2)/2) (CETensor @@ Sequence[indexArray1])(TVertex @@ Sequence[indexArray2])] ];
];*)


(* CIITensor *)
CIITensor[inputArray__]:=Module[{indexArray,tensorValence,temporaryExpression1,indexArrayForSymmetrisation},
	indexArray = List[inputArray];
	(* Consistency checks *)
	If[Length[indexArray]<4,Return[0]];
	If[Mod[Length[indexArray],2]==1,Return[0]];
	(* Last special case *)
	If[Length[indexArray]-4==0, Return[MCIITensorStructure@@Sequence[Join[{0,1,1},indexArray]]] ];
	(* Calculations *)
	tensorValence = Length[indexArray]/2-2;
	indexArrayForSymmetrisation = indexArray[[5;;]];
	Evaluate[temporaryExpression1@@Table[ ToExpression[ToString[indexArrayForSymmetrisation[[i]]]<>"_"],{i,1,Length[indexArrayForSymmetrisation]}] ] = Sum[KroneckerDelta[j0+j1+j2,tensorValence] (-1)^(tensorValence-j0) (MCIITensorStructure@@Sequence[Join[{j0,1+j1,1+j2},indexArray]]) ,{j0,0,tensorValence},{j1,0,tensorValence},{j2,0,tensorValence}];
	Return[Calc[1/Factorial[tensorValence] Sum[temporaryExpression1@@Flatten[Permutations[Partition[indexArrayForSymmetrisation,2]][[i]]],{i,1,tensorValence!}]]];
];


(* MCIITensorStructure is an internal function used in CIITensor  *)
MCIITensorStructure[inputArray__]:=Module[{inputData,indexArray,indexArrayExternal,indexArrayInternal,indexArray1,indexArray2,indexArray3},
	inputData = List[inputArray];
	If[ (inputData[[2]]==0)||(inputData[[3]]==0),Return[0]];
	If[2 Tr[ inputData[[1;;3]] ]!=Length[ inputData[[4;;]] ] , Return[0]];
	If[Length[inputData[[4;;]] ]==4, Return[Times@@ITensor@@@Partition[inputData[[4;;]],2]] ];
	indexArrayInternal = Partition[inputData[[4;;7]],2];
	indexArrayExternal = inputData[[8;;]];
	If[inputData[[1]]==0,indexArray1 = {}, indexArray1 = indexArrayExternal[[1;;2 inputData[[1]]]] ];
	If[inputData[[2]]==1,indexArray2 = indexArrayInternal[[1]] , indexArray2 = Join[ indexArrayInternal[[1]] , indexArrayExternal[[ 2 inputData[[1]] + 1 ;; 2 (inputData[[1]]+inputData[[2]]-1)]]] ];
	If[inputData[[3]]==1,indexArray3 = indexArrayInternal[[2]] , indexArray3 = Join[ indexArrayInternal[[2]] , indexArrayExternal[[ 2 (inputData[[1]]+inputData[[2]]-1)+1 ;;]] ]  ];
	Return[(CTensor@@indexArray1)(ITensor@@indexArray2)(ITensor@@indexArray3)];
];


(*
(* GravitonVectorVertex *)
GravitonVectorVertex[inputArray__] := Module[{inputData,TVertex},
	inputData = List[inputArray];
	If[Length[inputData]<6,Return[0]];
	If[Mod[Length[inputData],2]==1,Return[0]];
	TVertex[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,\[Rho]_,\[Sigma]_,p_,q_]=1/4 FVD[p,\[Lambda]]FVD[q,\[Tau]](ITensor[\[Mu],\[Nu],\[Rho],\[Sigma]]ITensor[\[Alpha],\[Beta],\[Lambda],\[Tau]]+ITensor[\[Mu],\[Nu],\[Lambda],\[Tau]]ITensor[\[Alpha],\[Beta],\[Rho],\[Sigma]]-ITensor[\[Mu],\[Nu],\[Rho],\[Tau]]ITensor[\[Alpha],\[Beta],\[Sigma],\[Lambda]]-ITensor[\[Mu],\[Nu],\[Sigma],\[Lambda]]ITensor[\[Alpha],\[Beta],\[Rho],\[Tau]])//Calc;
	Return[  Calc[ I (Global`\[Kappa])^(Length[inputData]/2-2) ( TVertex @@ Sequence[Join[ {\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]} , inputData[[Length[inputData]-3;;]]  ]  ] ) (CIITensor @@ Sequence[  Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]} , inputData[[;;Length[inputData]-4]]  ]  ])]  ];
]*)


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
