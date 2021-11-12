(* ::Package:: *)

BeginPackage["FeynGrav`",{"FeynCalc`"}];


Print["FeynGrav: All expressions for itteraction veritce are evaluated on a call."]
Print["FeynGrav: Please, be patient. Evaluation can take some time."]
Print["FeynGrav: FeynGravCommands print the list of all supported commands."]
Print["FeynGrav: Use '?CommandName' to see a brief description."]


(* ITensor::usage = "I-tensors which are responsible for \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) expansion. It takes wither null or an even number of arguments which are Lorentz indices."
CTensor::usage = "C-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) expansion. It takes either null or an even number of arguments which are Lorentz indices."
Vierbein::usage = "Expansion of a vierbein \!\(\*SubscriptBox[SuperscriptBox[\(\[GothicE]\), \(\[Mu]\)], \(\[Nu]\)]\). It takes an even number of arguments."
CITensor::usage = "CI-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) expansion. Take 2 or more arguments."
CIITensor::usage = "CII-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\) expansion. Take 4 or more arguments." 
CETensor::usage = "CE-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) \!\(\*SubscriptBox[SuperscriptBox[\(\[GothicE]\), \(\[Mu]\)], \(\[Nu]\)]\) expansion. Take 2 or more arguments."
CIIITensor::usage = "CIII-tensors which are responsible for \!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Rho]\[Sigma]\)]\) expansion. Take 6 or more arguments." *)
GravitonVertex::usage = "Vertex for interaction of 3 or more gravitons. Its arguments are Lorentz indices and momenta of the corresponding gravitons. For instance GravitonVertex[\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(2\)]\),\!\(\*SubscriptBox[\(p\), \(2\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(3\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(3\)]\),\!\(\*SubscriptBox[\(p\), \(3\)]\)]."
GravitonPropagator::usage = "Propagator of a graviton in the harmonic gauge. Its denominator is given via FAD function."
GravitonPropagatorTop::usage = "The nominator of a graviton propagator in the harmonic gauge."
GravitonPropagatorAlternative::usage = "Propagator of a graviton in the harmonic gauge. Its denominator is given via SPD function."
GravitonScalarVertex::usage = "Vertex for interaction between a massless scalar kinetic energy and gravitons. Takes 2n + 2 arguments. First 2n arguments are Lorentz indices of gravitons. The last two arguments are ingoint momenta of scalars."
GravitonVectorVertex::usage = "Vertex for interaction between a massless vector field kinetic energy and gravitons. Takes 2n + 4 arguments. First 2n arguments are Lotentz indices of gravitons. The next two arguments are Lorentz indices of vectors. The last two arguments are vectors momenta."
GravitonFermionVertex::usage = "Vertex for interaction between a massless Dirac fermion kinetic energy and gravitons. Takes 2n + 2 arguments. First 2n arguments are Lorentz indices of gravitons. The last two arguments are ingoint momenta of fermions."
GaugeProjectors::usage = "The standard gauge projectors \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\)-\!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator1::usage = "Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(1\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Alpha]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Alpha]\)]\)). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."
NieuwenhuizenOperator2::usage = "Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(2\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\))-\!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0::usage = "Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(0\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."
NieuwenhuizenOperator0Bar::usage = "Nieuwenhuizen operator (\!\(\*OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) =\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to the standard gauge projector \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."
NieuwenhuizenOperator0BarBar::usage = "Nieuwenhuizen operator (\!\(\*OverscriptBox[OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."


FeynGravCommands := Print[" 'GravitonVertex', 'GravitonPropagator', 'GravitonPropagatorTop', 'GravitonPropagatorAlternative', 'GravitonFermionVertex' "];


(* 
The package operates with quite a few functions which should be evaluated up to a certain perturbation order.
The package evaluate these functions firstly and then provide a user to perform calculations.
*)


Begin["Private`"];


(* ITensor *)
(* In the function has no arguments it returns 1. This definition if used for the sake of simplicity. *)
ITensor[]=1;
(* SetDelayed is used so the function would be evaluated only when it is needed. The function takes a single sequence as an argument. *)
ITensor[inputArray__]:=Module[{indexArray,generatingTerm},
	(* The input sequence is transformed into an array *)
	indexArray = List[inputArray];
	(* If the input sequence has an odd number of terms then the fucntion returns "0". If the number of arguments is even, then the function proceeds. *)
	If[Mod[Length[indexArray],2] != 0, Return[0]];
	(* I shift the array of the arguments by one symbol and generate a single term. *)
	generatingTerm=Times@@MTD@@@Partition[RotateLeft[indexArray,1],2];
	(* This single term is symemtrized with respect to index pairs. *)
	For[i=1,i<=Length[indexArray]/2,i++,
		generatingTerm = 1/2 (generatingTerm+(generatingTerm/.{indexArray[[2i]]->indexArray[[2i-1]],indexArray[[2i-1]]->indexArray[[2i]]}));
	];
	Return[Calc[generatingTerm]];
];


(* CTensor *)
(* If the function has no arguments it return 1. *)
CTensor[]=1;
(* SetDelayed is used so the function would be evaluated only when it is needed. The function takes a single sequence as an argument. *)
CTensor[inputArray__] := Module[{indexArray,tensorValence,temporaryExpression,numberOfTerms,summationIndices,symmetricExpression1,symmetricExpression2},
	(* The input sequence is transformed into an array *)
	indexArray = List[inputArray];
	(* If the input sequence has an odd number of terms then the fucntion returns "0". If the number of arguments is even, then the function proceeds. *)
	If[Mod[Length[indexArray],2] != 0, Return[0]];
	tensorValence = Length[indexArray]/2;
	temporaryExpression = 0;
	(* The expression is generated, but it is not yet symmetric *)
	For[numberOfTerms = 1, numberOfTerms <= tensorValence, numberOfTerms++,
		summationIndices = {#,1,5}&/@ Table[ToExpression["j"<>ToString[i]],{i,1,numberOfTerms}];
		temporaryExpression = temporaryExpression + (-1)^(tensorValence+numberOfTerms)/(Factorial[numberOfTerms] 2^numberOfTerms)Sum[MCTensorStructure@@Join[{numberOfTerms},Table[ToExpression["j"<>ToString[i]],{i,1,numberOfTerms}],indexArray] ,Evaluate[Sequence@@summationIndices]];
	];
	(* SYmmetrization *)
	Evaluate[symmetricExpression1@@Table[ToExpression[ToString[indexArray[[i]]] <> "_"] ,{i,1,Length[indexArray]}]] = Calc[temporaryExpression];
	Evaluate[symmetricExpression2@@Table[ToExpression[ToString[indexArray[[i]]] <> "_"] ,{i,1,Length[indexArray]}]] = 1/Factorial[tensorValence] Sum[symmetricExpression1@@Flatten[Permutations[Partition[indexArray,2]][[i]]],{i,1,tensorValence!}];
	Return[Calc[symmetricExpression2@@indexArray]];
];


(* MCTensorStructure *)
(* This is a completely internal command used in CTensor *)
MCTensorStructure[inputArray__] := Module[{inputData,numberOfMultipliers,partialOrders,indexArray,splitIndexArray,tmpPosition,i},
	inputData = List[inputArray];
	(* The number of multipliers *)
	numberOfMultipliers = inputData[[1]];
	(* The array of length of subarrays *)
	partialOrders = inputData[[2;;1+numberOfMultipliers]];
	(* The array of indices to be splitted *)
	indexArray = inputData[[numberOfMultipliers +2 ;;]];
	(* Make sure that the function received the correct number of arguments *)
	If[Mod[Length[indexArray],2]==1,Return[0]];
	If[Tr[partialOrders]!=Length[indexArray]/2,Return[0]];
	
	(* This part split indexArray in a set of subarray *)
	splitIndexArray = {};
	(* The initial cursor position is set on the first element of index Array. It runs along the array and split it *)
	tmpPosition = 1;
	For[i=1,i<=numberOfMultipliers,i++,
		AppendTo[ splitIndexArray , indexArray[[ tmpPosition ;; tmpPosition + 2 partialOrders[[i]] - 1 ]] ];
		tmpPosition = tmpPosition + 2 partialOrders[[i]]  ;
	];
	(* splitIndexArray now contain indexArray splitted in subarray *)
	(* Now I generay a multiplication of ITensors with the given indices *)
	Return[Calc[1/Times@@partialOrders Times@@ITensor@@@splitIndexArray]];
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


(* CIIITensor *)
CIIITensor[]=0;
CIIITensor[inputArray__]:=Module[{indexArray,tensorValence,temporaryExpression1,indexArrayForSymmetrisation},
	indexArray = List[inputArray];
	(* Consistency checks *)
	If[Length[indexArray]<6,Return[0]];
	If[Mod[Length[indexArray],2]==1,Return[0]];
	(* Last special case *)
	If[Length[indexArray]-6==0, Return[MCIIITensorStructure@@Sequence[Join[{0,1,1,1},indexArray]]] ];
	(* Calculations *)
	tensorValence = Length[indexArray]/2-3;
	indexArrayForSymmetrisation = indexArray[[7;;]];
	Evaluate[temporaryExpression1@@Table[ ToExpression[ToString[indexArrayForSymmetrisation[[i]]]<>"_"],{i,1,Length[indexArrayForSymmetrisation]}] ] = Sum[KroneckerDelta[j0+j1+j2+j3,tensorValence] (-1)^(tensorValence-j0) (MCIIITensorStructure@@Sequence[Join[{j0,1+j1,1+j2,1+j3},indexArray]]) ,{j0,0,tensorValence},{j1,0,tensorValence},{j2,0,tensorValence},{j3,0,tensorValence}];
	Return[Calc[1/Factorial[tensorValence] Sum[temporaryExpression1@@Flatten[Permutations[Partition[indexArrayForSymmetrisation,2]][[i]]],{i,1,tensorValence!}]]];
];


MCIIITensorStructure[inputArray__]:=Module[{inputData,indexArray,indexArrayExternal,indexArrayInternal,indexArray1,indexArray2,indexArray3,indexArray4},
	inputData = List[inputArray];
	If[2 Tr[ inputData[[1;;4]] ]!=Length[ inputData[[5;;]] ] , Return[0]];
	If[Length[inputData[[5;;]] ]==6, Return[Times@@ITensor@@@Partition[inputData[[5;;]],2]] ];
	indexArrayInternal = Partition[inputData[[5;;10]],2];
	indexArrayExternal = inputData[[11;;]];
	If[inputData[[1]]==0,indexArray1 = {}, indexArray1 = indexArrayExternal[[1;;2 inputData[[1]]]] ];
	If[inputData[[2]]==1,indexArray2 = indexArrayInternal[[1]] , indexArray2 = Join[ indexArrayInternal[[1]] , indexArrayExternal[[ 2 inputData[[1]] + 1 ;; 2 (inputData[[1]]+inputData[[2]]-1)]]] ];
	If[inputData[[3]]==1,indexArray3 = indexArrayInternal[[2]] , indexArray3 = Join[ indexArrayInternal[[2]] , indexArrayExternal[[ 2 (inputData[[1]]+inputData[[2]]-1)+1 ;; 2(inputData[[1]]+inputData[[2]]+inputData[[3]] - 2)]] ]  ];
	If[inputData[[4]]==1,indexArray4 = indexArrayInternal[[3]] , indexArray4 = Join[ indexArrayInternal[[3]] , indexArrayExternal[[ 2 (inputData[[1]]+inputData[[2]]+inputData[[3]]-2)+1 ;;  ]] ] ];
	Return[(CTensor@@indexArray1)(ITensor@@indexArray2)(ITensor@@indexArray3)(ITensor@@indexArray4)];
];


(* GravitonVertex *)
GravitonVertex[]=0;
GravitonVertex[inputArray__]:=Module[{inputData,tensorT,tensorValence,arrayMomenta,arrayIndices,nonsymmetricExpression,symmetricExpression,dummyArray},
	inputData = List[inputArray];
	If[Length[inputData]<9,Return[0]];
	If[Mod[Length[inputData],3]!=0,Return[0]];
	tensorValence = Length[ inputData ] /3;
	dummyArray = Flatten[Table[ { ToExpression["\[ScriptM]"<>ToString[i]],ToExpression["\[ScriptN]"<>ToString[i]],ToExpression["\[ScriptP]"<>ToString[i]] } , {i,1,tensorValence}]];
	tensorT[\[ScriptI]_,j_,a_,b_,r_,s_,\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,\[Rho]_,\[Sigma]_]=-MTD[\[Mu],\[ScriptI]]MTD[\[Nu],j]ITensor[\[Alpha],\[Beta],a,b]ITensor[\[Rho],\[Sigma],r,s]+MTD[\[Mu],\[ScriptI]]MTD[\[Nu],j]ITensor[\[Alpha],\[Rho],a,b]ITensor[\[Beta],\[Sigma],r,s]-2 MTD[\[Mu],\[ScriptI]]MTD[\[Alpha],j]ITensor[\[Beta],\[Rho],a,b]ITensor[\[Nu],\[Sigma],r,s]+2 MTD[\[Mu],\[ScriptI]]MTD[\[Beta],j]ITensor[\[Nu],\[Alpha],a,b]ITensor[\[Rho],\[Sigma],r,s]//Calc;
	arrayMomenta = Table[ Partition[dummyArray,3][[i]][[3]] ,{i,1,tensorValence}];
	arrayIndices = Table[ Partition[dummyArray,3][[i]][[1;;2]] ,{i,1,tensorValence}];
	Evaluate[nonsymmetricExpression@@Table[ToExpression[ToString[dummyArray[[i]]]<>"_"],{i,1,Length[inputData]}]]= Calc[(FVD[ arrayMomenta[[1]] ,\[Lambda]1]   FVD[ arrayMomenta[[2]] ,\[Lambda]2]) ( tensorT@@Sequence[Flatten[Join[{\[Lambda]1,\[Lambda]2} , arrayIndices[[;;2]] , {\[Rho]1,\[Sigma]1,\[Rho]2,\[Sigma]2,\[Rho]3,\[Sigma]3} ]]] )  (CIIITensor@@Sequence[Flatten[Join[{\[Rho]1,\[Sigma]1,\[Rho]2,\[Sigma]2,\[Rho]3,\[Sigma]3} , arrayIndices[[3;;]]]]]) ];
	Evaluate[symmetricExpression@@Table[ToExpression[ToString[dummyArray[[i]]]<>"_"],{i,1,Length[inputData]}]]= Calc[(-I 2/(Global`\[Kappa])^2)((Global`\[Kappa])^tensorValence/4)Plus@@nonsymmetricExpression@@@Table[ Flatten[ Permutations[Partition[dummyArray,3]][[i]] ] , {i,1,Factorial[tensorValence]}]] ;
	Return[Calc[symmetricExpression@@inputData]];
];


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


(* GravitonFermionVertex *)
GravitonFermionVertex[inputArray__] := Module[{inputData,TVertex,indexArray1,indexArray2},
	inputData = List[inputArray];
	If[ Mod[ Length[inputData],2] == 1 , Return[0]];
	If[ Length[inputData] < 3 , Return[0]];
	TVertex[\[Mu]_,\[Nu]_,p_,q_] = Calc[  1/2 FVD[p-q,\[Nu]]GAD[\[Mu]]  ];
	indexArray1 = Join[ {\[ScriptM],\[ScriptN]}, inputData[[ ;;Length[inputData]-2]] ];
	indexArray2 = Join[ {\[ScriptM],\[ScriptN]}, inputData[[Length[inputData]-1;;]] ];
	Return[  Calc[I Global`\[Kappa]^((Length[inputData]-2)/2) (CETensor @@ Sequence[indexArray1])(TVertex @@ Sequence[indexArray2])] ];
];


(* CITensor *)
CITensor[inputArray__]:=Module[{indexArray,tensorValence,temporaryExpression1,indexArrayForSymmetrisation},
	indexArray = List[inputArray];
	(* Consistency checks *)
	If[Length[indexArray]<2,Return[0]];
	If[Mod[Length[indexArray],2]==1,Return[0]];
	(* Last special case *)
	If[Length[indexArray]==2, Return[MCITensorStructure@@Sequence[Join[{0,1},indexArray]]] ];
	(* Calculations *)
	tensorValence = Length[indexArray]/2-1;
	indexArrayForSymmetrisation = indexArray[[3;;]];
	Evaluate[temporaryExpression1@@Table[ ToExpression[ToString[indexArrayForSymmetrisation[[i]]]<>"_"],{i,1,Length[indexArrayForSymmetrisation]}] ] = Sum[KroneckerDelta[j0+j1,tensorValence] (-1)^(tensorValence-j0) (MCITensorStructure@@Sequence[Join[{j0,1+j1},indexArray]]) ,{j0,0,tensorValence},{j1,0,tensorValence}];
	Return[Calc[1/Factorial[tensorValence] Sum[temporaryExpression1@@Flatten[Permutations[Partition[indexArrayForSymmetrisation,2]][[i]]],{i,1,tensorValence!}]]];
];


(* MCITensorStructure is an internal command used only in CITensor *)
MCITensorStructure[inputArray__]:=Module[{inputData,indexArray,indexArrayExternal,indexArrayInternal,indexArray1,indexArray2},
	inputData = List[inputArray];
	If[inputData[[2]]==0,Return[0]];
	If[2 Tr[ inputData[[1;;2]] ]!=Length[ inputData[[3;;]] ] , Return[0] ];
	If[Length[inputData[[3;;]] ]==2, Return[ITensor@@inputData[[3;;]] ]];
	indexArrayInternal = inputData[[3;;4]];
	indexArrayExternal = inputData[[5;;]];
	If[inputData[[1]]==0,indexArray1 = {}, indexArray1 = indexArrayExternal[[1;;2 inputData[[1]]]] ];
	If[inputData[[2]]==1,indexArray2 = indexArrayInternal , indexArray2 = Join[ indexArrayInternal , indexArrayExternal[[ 2 inputData[[1]] + 1 ;; 2 (inputData[[1]]+inputData[[2]]-1)]]] ];
	Return[(CTensor@@indexArray1)(ITensor@@indexArray2)];
];


(* GravitonScalarVertex *)
GravitonScalarVertex[inputArray__]:=Module[{inputData,TVertex},
	inputData = List[inputArray];
	If[ Length[inputData] < 4 , Return[0]];
	If[ Mod[Length[inputData],2] == 1,Return[0]];
	TVertex[m_,n_,p_,q_]=-(1/2)ITensor[m,n,a,b]FVD[p,a]FVD[q,b]//Calc;
	Return[ Calc[ I (Global`\[Kappa])^(Length[inputData]/2-1) ( TVertex @@Sequence[Join[ {\[ScriptM],\[ScriptN]} , inputData[[ Length[inputData]-1;;]] ]] ) (CITensor @@ Sequence[Join[ {\[ScriptM],\[ScriptN]} , inputData[[;;Length[inputData]-2]] ]])] ];
];


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


(* GravitonVectorVertex *)
GravitonVectorVertex[inputArray__] := Module[{inputData,TVertex},
	inputData = List[inputArray];
	If[Length[inputData]<6,Return[0]];
	If[Mod[Length[inputData],2]==1,Return[0]];
	TVertex[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,\[Rho]_,\[Sigma]_,p_,q_]=1/4 FVD[p,\[Lambda]]FVD[q,\[Tau]](ITensor[\[Mu],\[Nu],\[Rho],\[Sigma]]ITensor[\[Alpha],\[Beta],\[Lambda],\[Tau]]+ITensor[\[Mu],\[Nu],\[Lambda],\[Tau]]ITensor[\[Alpha],\[Beta],\[Rho],\[Sigma]]-ITensor[\[Mu],\[Nu],\[Rho],\[Tau]]ITensor[\[Alpha],\[Beta],\[Sigma],\[Lambda]]-ITensor[\[Mu],\[Nu],\[Sigma],\[Lambda]]ITensor[\[Alpha],\[Beta],\[Rho],\[Tau]])//Calc;
	Return[  Calc[ I (Global`\[Kappa])^(Length[inputData]/2-2) ( TVertex @@ Sequence[Join[ {\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]} , inputData[[Length[inputData]-3;;]]  ]  ] ) (CIITensor @@ Sequence[  Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]} , inputData[[;;Length[inputData]-4]]  ]  ])]  ];
]


(* GaugeProjectors *)
GaugeProjectors[inputArray__]:=Module[{inputData},
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
	Return[ 1/2 ( GaugeProjectors[indexArray[[1]],indexArray[[3]],theMomentum] (-GaugeProjectors[indexArray[[2]],indexArray[[4]],theMomentum] + MTD[indexArray[[2]],indexArray[[4]]]) + GaugeProjectors[indexArray[[1]],indexArray[[4]],theMomentum] (-GaugeProjectors[indexArray[[2]],indexArray[[3]],theMomentum] + MTD[indexArray[[2]],indexArray[[3]]]) + GaugeProjectors[indexArray[[2]],indexArray[[4]],theMomentum] (-GaugeProjectors[indexArray[[1]],indexArray[[3]],theMomentum] + MTD[indexArray[[1]],indexArray[[3]]]) + GaugeProjectors[indexArray[[2]],indexArray[[3]],theMomentum] (-GaugeProjectors[indexArray[[1]],indexArray[[4]],theMomentum] + MTD[indexArray[[1]],indexArray[[4]]]) )];
];

NieuwenhuizenOperator2[inputArray__]:=Module[{inputData,indexArray,theMomentum},
	inputData = List[inputArray];
	If[Length[inputData]!=5,Return[0]];
	indexArray = inputData[[1;;4]];
	theMomentum = inputData[[5]];
	Return[ 1/2 (GaugeProjectors[indexArray[[1]],indexArray[[3]],theMomentum] GaugeProjectors[indexArray[[2]],indexArray[[4]],theMomentum] + GaugeProjectors[indexArray[[1]],indexArray[[4]],theMomentum] GaugeProjectors[indexArray[[2]],indexArray[[3]],theMomentum]) -1/3 (GaugeProjectors[indexArray[[1]],indexArray[[2]],theMomentum]GaugeProjectors[indexArray[[3]],indexArray[[4]],theMomentum]) ];
];

NieuwenhuizenOperator0[inputArray__]:=Module[{inputData,indexArray,theMomentum},
	inputData = List[inputArray];
	If[Length[inputData]!=5,Return[0]];
	indexArray = inputData[[1;;4]];
	theMomentum = inputData[[5]];
	Return[ 1/3 (GaugeProjectors[indexArray[[1]],indexArray[[2]],theMomentum]GaugeProjectors[indexArray[[3]],indexArray[[4]],theMomentum]) ];
];

NieuwenhuizenOperator0Bar[inputArray__]:=Module[{inputData,indexArray,theMomentum},
	inputData = List[inputArray];
	If[Length[inputData]!=5,Return[0]];
	indexArray = inputData[[1;;4]];
	theMomentum = inputData[[5]];
	Return[ (MTD[indexArray[[1]],indexArray[[2]]]-GaugeProjectors[indexArray[[1]],indexArray[[2]],theMomentum])(MTD[indexArray[[3]],indexArray[[4]]]-GaugeProjectors[indexArray[[3]],indexArray[[4]],theMomentum]) ];
];

NieuwenhuizenOperator0BarBar[inputArray__]:=Module[{inputData,indexArray,theMomentum},
	inputData = List[inputArray];
	If[Length[inputData]!=5,Return[0]];
	indexArray = inputData[[1;;4]];
	theMomentum = inputData[[5]];
	Return[ GaugeProjectors[indexArray[[1]],indexArray[[2]],theMomentum](MTD[indexArray[[3]],indexArray[[4]]]-GaugeProjectors[indexArray[[3]],indexArray[[4]],theMomentum])+(MTD[indexArray[[1]],indexArray[[2]]]-GaugeProjectors[indexArray[[1]],indexArray[[2]],theMomentum])GaugeProjectors[indexArray[[3]],indexArray[[4]],theMomentum] ];
];


(* OLD CODE

(* ITensor *)
Clear[ITensor];
ITensor[]=1;
Module[{tensorValence,generatingTerm,i},
	For[tensorValence=1,tensorValence<=perturbationOrder+1,tensorValence++,
		(*A singel term is generated*)
		generatingTerm=Times@@MTD@@@Table[{ToExpression["m"<>ToString[Mod[i+1,tensorValence,1]]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}];
		(*It is symmetrized with respect to each index pair*)
		For[i=1,i<=tensorValence,i++,
			generatingTerm=Calc[1/2 (generatingTerm+(generatingTerm/.{ToExpression["m"<>ToString[i]]->ToExpression["n"<>ToString[i]],ToExpression["n"<>ToString[i]]->ToExpression["m"<>ToString[i]]}))];
		];
	Evaluate[ITensor@@Evaluate[Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]]]]=generatingTerm;
	];
]

(* CTensor *)
Clear[CTensor];
CTensor[]=1;
Module[{tensorValence,temporaryExpression,indexArrayArgumets,indexArrayM,indexArrayN,indexArray,indexArrayFull,numberOfTerms,summationIndices,summationIndicesArguments,iterationList,temporaryFactor,temporaryCTensor,tensorPermutationArray,i},
	For[tensorValence=1,tensorValence<=perturbationOrder,tensorValence++,
		indexArray=Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]];
		indexArrayFull=Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}];
		indexArrayArgumets=Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]];
		temporaryExpression=0;
		For[numberOfTerms=1,numberOfTerms<=tensorValence,numberOfTerms++,
			summationIndices=Table[ToExpression["j"<>ToString[i]],{i,1,numberOfTerms}];
			summationIndicesArguments=Table[ToExpression["j"<>ToString[i]<>"_"],{i,1,numberOfTerms}];
			iterationList={#,1,tensorValence}&/@summationIndices;
			Clear[temporaryFactor];
			Evaluate[temporaryFactor@@summationIndicesArguments]=KroneckerDelta[Tr[summationIndices],tensorValence](Times@@summationIndices)^-1;
			temporaryExpression=temporaryExpression+(-1)^(numberOfTerms+tensorValence)/(numberOfTerms! 2^numberOfTerms) Sum[temporaryFactor[Sequence@@summationIndices](Times@@If[Tr[summationIndices]==tensorValence,ITensor@@@TakeList[indexArray,2 summationIndices],0]),Evaluate[Sequence@@iterationList]]//Calc;
		];
		Evaluate[temporaryCTensor@@indexArrayArgumets]=temporaryExpression;
		Evaluate[CTensor@@indexArrayArgumets]=1/tensorValence! Sum[temporaryCTensor@@Flatten[Permutations[indexArrayFull][[i]]],{i,1,tensorValence!}]//Calc;
	];
]

(* CIIITensor *)
Clear[CIIITensor];
Module[{tensorValence,indexArray,indexArrayArguments,permutationArray,temporaryExpression},
	For[tensorValence=0,tensorValence<=perturbationOrder,tensorValence++,
		indexArray=Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}]];
		indexArrayArguments=Join[{ToExpression["\[ScriptM]_"],ToExpression["\[ScriptN]_"],ToExpression["\[ScriptA]_"],ToExpression["\[ScriptB]_"],ToExpression["\[ScriptR]_"],ToExpression["\[ScriptS]_"]},Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]<>"_"],ToExpression["n"<>ToString[\[ScriptI]]<>"_"]},{\[ScriptI],1,tensorValence}]]];
		permutationArray=Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}];
		permutationArray=Permutations[permutationArray];
		permutationArray=Table[Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]},Flatten[permutationArray[[\[ScriptI]]]]],{\[ScriptI],1,tensorValence!}];
		Evaluate[temporaryExpression@@indexArrayArguments]=Calc[Sum[If[\[ScriptN]0+\[ScriptN]1+\[ScriptN]2<=tensorValence,(-1)^(tensorValence-\[ScriptN]0) (CTensor@@indexArray[[;;2 \[ScriptN]0]])(ITensor@@Join[{\[ScriptM],\[ScriptN]},indexArray[[2 \[ScriptN]0+1;;2(\[ScriptN]0+\[ScriptN]1)]]])(ITensor@@Join[{\[ScriptA],\[ScriptB]},indexArray[[2(\[ScriptN]0+\[ScriptN]1)+1;;2(\[ScriptN]0+\[ScriptN]1+\[ScriptN]2)]]])(ITensor@@Join[{\[ScriptR],\[ScriptS]},indexArray[[2(\[ScriptN]0+\[ScriptN]1+\[ScriptN]2)+1;;]]]),0],{\[ScriptN]0,0,tensorValence},{\[ScriptN]1,0,tensorValence},{\[ScriptN]2,0,tensorValence}]];
		Evaluate[CIIITensor@@indexArrayArguments]=1/tensorValence! Plus@@temporaryExpression@@@permutationArray//Calc;
	];
]

(* GravitonVertex *)
Clear[GravitonVertex];
Context[\[Kappa]]="Global`"
Module[{tensorT,tensorValence,indexArray,indexArrayArgumets,indexArrayArgumetsVariables,temporaryExpression},
	tensorT[i_,j_,a_,b_,r_,s_,\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,\[Rho]_,\[Sigma]_]=-MTD[\[Mu],i]MTD[\[Nu],j]ITensor[\[Alpha],\[Beta],a,b]ITensor[\[Rho],\[Sigma],r,s]+MTD[\[Mu],i]MTD[\[Nu],j]ITensor[\[Alpha],\[Rho],a,b]ITensor[\[Beta],\[Sigma],r,s]-2MTD[\[Mu],i]MTD[\[Alpha],j]ITensor[\[Beta],\[Rho],a,b]ITensor[\[Nu],\[Sigma],r,s]+2MTD[\[Mu],i]MTD[\[Beta],j]ITensor[\[Nu],\[Alpha],a,b]ITensor[\[Rho],\[Sigma],r,s]//Calc;
	For[tensorValence=3,tensorValence<=perturbationOrder,tensorValence++,
		indexArray=Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}]];
		indexArrayArgumets=Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]],ToExpression["p"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}];
		indexArrayArgumetsVariables=Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]<>"_"],ToExpression["n"<>ToString[\[ScriptI]]<>"_"],ToExpression["p"<>ToString[\[ScriptI]]<>"_"]},{\[ScriptI],1,tensorValence}]];
		Evaluate[temporaryExpression@@indexArrayArgumetsVariables]=FVD[p1,\[Lambda]1]FVD[p2,\[Lambda]2](tensorT@@Join[{\[Lambda]1,\[Lambda]2},indexArray[[1;;4]],Flatten[Table[{ToExpression["\[Rho]"<>ToString[\[ScriptI]]],ToExpression["\[Sigma]"<>ToString[\[ScriptI]]]},{\[ScriptI],1,3}]]])(CIIITensor@@Join[Flatten[Table[{ToExpression["\[Rho]"<>ToString[\[ScriptI]]],ToExpression["\[Sigma]"<>ToString[\[ScriptI]]]},{\[ScriptI],1,3}]],indexArray[[5;;]]])//Calc;
		Evaluate[GravitonVertex@@indexArrayArgumetsVariables]=(-I 2/\[Kappa]^2)(\[Kappa]^tensorValence/4)Plus@@temporaryExpression@@@Table[Flatten[Permutations[indexArrayArgumets][[\[ScriptI]]]],{\[ScriptI],1,tensorValence!}]//Calc;
	];
]

(* Vierbein *)
Clear[Vierbein];
Module[{tensorValence},
	For[tensorValence=0,tensorValence<=perturbationOrder,tensorValence++,
		Evaluate[Vierbein@@Join[{ToExpression["\[ScriptM]_"],ToExpression["\[ScriptN]_"]},Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]]]]=Calc[Binomial[-1/2,tensorValence](ITensor@@Join[{\[ScriptM],\[ScriptN]},Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]]])];
	];
]

(* CETensor *)
(* WARNING! This function is actually does not work correctly. *)
Clear[CETensor];
Module[{tensorValence,indexArray,indexArrayArguments,permutationArray,temporaryExpression},
	For[tensorValence=0,tensorValence<=perturbationOrder,tensorValence++,
		indexArray=Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}]];
		indexArrayArguments=Join[{ToExpression["\[ScriptM]_"],ToExpression["\[ScriptN]_"]},Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]<>"_"],ToExpression["n"<>ToString[\[ScriptI]]<>"_"]},{\[ScriptI],1,tensorValence}]]];
		permutationArray=Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}];
		permutationArray=Permutations[permutationArray];
		permutationArray=Table[Join[{\[ScriptM],\[ScriptN]},Flatten[permutationArray[[\[ScriptI]]]]],{\[ScriptI],1,tensorValence!}];
		Evaluate[temporaryExpression@@indexArrayArguments]=Sum[(-1)^(tensorValence-\[ScriptN]0) (CTensor@@indexArray[[1;;2 \[ScriptN]0]])(Vierbein@@Join[{\[ScriptM],\[ScriptN]},indexArray[[2\[ScriptN]0+1;;2 tensorValence]]]),{\[ScriptN]0,0,tensorValence}]//Calc;
		Evaluate[CETensor@@indexArrayArguments]=1/tensorValence! Plus@@(temporaryExpression@@@permutationArray)//Calc;
	];
]

(* CITensor *)
Clear[CITensor];
Module[{tensorValence,indexArray,indexArrayArgumets,tensorPermutationArray,temporaryExpression},
	For[tensorValence=0,tensorValence<=perturbationOrder,tensorValence++,
		indexArray=Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]];
		indexArrayArgumets=Join[{ToExpression["\[ScriptM]_"],ToExpression["\[ScriptN]_"]},Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]]];
		tensorPermutationArray=Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}];
		tensorPermutationArray=Permutations[tensorPermutationArray];
		tensorPermutationArray=Table[Join[{\[ScriptM],\[ScriptN]},Flatten[tensorPermutationArray[[i]]]],{i,1,tensorValence!}];
		Clear[temporaryExpression];
		Evaluate[temporaryExpression@@indexArrayArgumets]=Sum[(-1)^(tensorValence-\[ScriptN]0) (CTensor@@indexArray[[1;;2 \[ScriptN]0]])(ITensor@@Join[{\[ScriptM],\[ScriptN]},indexArray[[2\[ScriptN]0+1;;2 tensorValence]]]),{\[ScriptN]0,0,tensorValence}]//Calc;
		Evaluate[CITensor@@indexArrayArgumets]=Evaluate[Calc[1/tensorValence! Plus@@(temporaryExpression@@@tensorPermutationArray)]];
	];
]

(* CIITensor *)
Clear[CIITensor];
Module[{tensorValence,indexArray,indexArrayArguments,tensorPermutationArray,temporaryExpression},
	For[tensorValence=0,tensorValence<=perturbationOrder,tensorValence++,
		indexArray=Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}]];
		indexArrayArguments=Join[{ToExpression["\[ScriptM]_"],ToExpression["\[ScriptN]_"],ToExpression["\[ScriptA]_"],ToExpression["\[ScriptB]_"]},Flatten[Table[{ToExpression["m"<>ToString[\[ScriptI]]<>"_"],ToExpression["n"<>ToString[\[ScriptI]]<>"_"]},{\[ScriptI],1,tensorValence}]]];
		tensorPermutationArray=Table[{ToExpression["m"<>ToString[\[ScriptI]]],ToExpression["n"<>ToString[\[ScriptI]]]},{\[ScriptI],1,tensorValence}];
		tensorPermutationArray=Permutations[tensorPermutationArray];
		tensorPermutationArray=Table[Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]},Flatten[tensorPermutationArray[[\[ScriptI]]]]],{\[ScriptI],1,tensorValence!}];
		Clear[temporaryExpression];
		Evaluate[temporaryExpression@@indexArrayArguments]=Sum[If[\[ScriptN]0+\[ScriptN]1<=tensorValence,(-1)^(tensorValence-\[ScriptN]0) (CTensor@@indexArray[[;;2 \[ScriptN]0]])(ITensor@@Join[{\[ScriptM],\[ScriptN]},indexArray[[2 \[ScriptN]0+1;;2(\[ScriptN]0+\[ScriptN]1)]]])(ITensor@@Join[{\[ScriptA],\[ScriptB]},indexArray[[2(\[ScriptN]0+\[ScriptN]1)+1;;]]]),0],{\[ScriptN]0,0,tensorValence},{\[ScriptN]1,0,tensorValence}]//Calc;
		Evaluate[CIITensor@@indexArrayArguments]=1/tensorValence! Plus@@temporaryExpression@@@tensorPermutationArray//Calc;
	];
]


(* GravitonScalarVertex *)
Clear[GravitonScalarVertex];
Module[{TVertex,tensorValence,indexArray},
	TVertex[m_,n_,p_,q_]=-(1/2)ITensor[m,n,a,b]FVD[p,a]FVD[q,b]//Calc;
	For[tensorValence=1,tensorValence<=perturbationOrder,tensorValence++,
		Evaluate[GravitonScalarVertex@@Join[Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]],{ToExpression["p_"],ToExpression["q_"]}]]=Calc[I \[Kappa]^tensorValence TVertex[m,n,p,q](CITensor@@Join[{m,n},Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]]])];
	];
]

(* GravitonVectorVertex *)
Clear[GravitonVectorVertex];
Module[{TVertex,tensorValence},
	TVertex[\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,\[Rho]_,\[Sigma]_,p_,q_]=1/4 FVD[p,\[Lambda]]FVD[q,\[Tau]](ITensor[\[Mu],\[Nu],\[Rho],\[Sigma]]ITensor[\[Alpha],\[Beta],\[Lambda],\[Tau]]+ITensor[\[Mu],\[Nu],\[Lambda],\[Tau]]ITensor[\[Alpha],\[Beta],\[Rho],\[Sigma]]-ITensor[\[Mu],\[Nu],\[Rho],\[Tau]]ITensor[\[Alpha],\[Beta],\[Sigma],\[Lambda]]-ITensor[\[Mu],\[Nu],\[Sigma],\[Lambda]]ITensor[\[Alpha],\[Beta],\[Rho],\[Tau]])//Calc;
	For[tensorValence=1,tensorValence<=perturbationOrder,tensorValence++,
		Evaluate[GravitonVectorVertex@@Join[Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]],{ToExpression["\[Rho]_"],ToExpression["\[Sigma]_"],ToExpression["p_"],ToExpression["q_"]}]]=Calc[I \[Kappa]^tensorValence TVertex[m,n,a,b,\[Rho],\[Sigma],p,q](CIITensor@@Join[{m,n,a,b},Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]]])];
	];
]

(* GravitonFermionVertex *)
Clear[GravitonFermionVertex];
Module[{TVertex,tensorValence},
	TVertex[\[Mu]_,\[Nu]_,p_,q_]=1/2 FVD[p-q,\[Nu]]GAD[\[Mu]]//Calc;
	For[tensorValence=1,tensorValence<=perturbationOrder,tensorValence++,
		Evaluate[GravitonFermionVertex@@Join[Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,tensorValence}]],{ToExpression["p_"],ToExpression["q_"]}]]=Calc[I \[Kappa]^tensorValence TVertex[m,n,p,q](CETensor@@Join[{m,n},Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]]])];
	];
]

*)


End[];


EndPackage[];
