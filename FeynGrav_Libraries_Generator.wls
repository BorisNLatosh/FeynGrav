#!/usr/bin/env wolframscript
(* ::Package:: *)

<<FeynCalc`


(* ITensor *)
(* The main body.*)
ITensorGenerator[inputArray__]:=Module[{indexArray,generatingTerm,generatingArrays,temporaryVariable},
	(* The input sequence is transformed into an array. *)
	indexArray = List[inputArray];
	(* Consistency check. *)
	If[Mod[Length[indexArray],2] != 0, Return[0]];
	(* Generating arrays of indices. *)
	generatingArrays = {indexArray};
	For[i=1,i<=Length[indexArray]/2,i++,
		temporaryVariable = Length[generatingArrays];
		For[j=1,j<=temporaryVariable,j++,
			AppendTo[generatingArrays,generatingArrays[[j]]/.{  Partition[indexArray,2][[i]][[1]]->Partition[indexArray,2][[i]][[2]],Partition[indexArray,2][[i]][[2]]->Partition[indexArray,2][[i]][[1]]  }]
		];
	];
	generatingArrays = Function[Partition[RotateLeft[#,1],2]]/@generatingArrays;
	Return[Plus@@(1/2^(Length[indexArray]/2) Function[Times@@Apply[MTD,#,1]]/@generatingArrays)];
];

(* ITensor Export *)
exportITensor[order_]:=Module[{cursor},
	Print["Export of I-tensors is initiated."];
	For[cursor=1,cursor<=order,cursor++,
		Evaluate[ITensorGenerator@@Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,cursor}]]]>>"Libs/ITensor_"<>ToString[cursor];
		Print["Done for order "<>ToString[cursor]];
	]
]

importITensor:=Module[{cursor},
	cursor = 1;
	Clear[ITensor];
	While[FileExistsQ["Libs/ITensor_"<>ToString[cursor]],
		Evaluate[ITensor@@Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,cursor}]]] = Get["Libs/ITensor_"<>ToString[cursor]];
		cursor++;
	];
	Print["I tensors are imported up to order "<>ToString[cursor-1]<>"."];
]


(* CTensor *)
(* Consistency condition. *)
CTensorGenerator[]=1;
CTensorGenerator[inputArray__] := Module[{indexArray,tensorValence,dummyArray,temporaryExpression,numberOfTerms,summationIndices,symmetrizationFunction},
	indexArray = List[inputArray];
	(* Consistency check. *)
	If[Mod[Length[indexArray],2] != 0, Return[0]];
	tensorValence = Length[indexArray]/2;	
	(* Creation of the generating term *)
	dummyArray = Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,tensorValence}]];
	temporaryExpression = 0;
	For[numberOfTerms = 1, numberOfTerms <= tensorValence, numberOfTerms++,
		summationIndices = {#,1,tensorValence}&/@ Table[ToExpression["j"<>ToString[i]],{i,1,numberOfTerms}];
		temporaryExpression += (-1)^(tensorValence+numberOfTerms)/(Factorial[numberOfTerms] 2^numberOfTerms) Sum[ CTensorStructure@@Join[{tensorValence,numberOfTerms},Table[ToExpression["j"<>ToString[i]],{i,1,numberOfTerms}],dummyArray] ,Evaluate[Sequence@@summationIndices]];
	];
	symmetrizationFunction = Function[Evaluate[dummyArray],Evaluate[Expand[temporaryExpression]]];
	Return[Expand[1/Factorial[tensorValence] Plus@@Apply[symmetrizationFunction,Flatten/@Permutations[Partition[indexArray,2]],1]]];
];
(* CTensorStructure *)
(* This is a completely internal command used in CTensor *)
CTensorStructure[inputArray__] := Module[{indexArray,perturbationOrder,numberOfMultipliers,partialOrders,partialArrays},
	(* Preparations *)
	indexArray = List[inputArray];
	perturbationOrder = indexArray[[1]];
	numberOfMultipliers = indexArray[[2]];
	partialOrders = indexArray[[3;;3+numberOfMultipliers-1]];
	indexArray = indexArray[[3+numberOfMultipliers;;]];
	(* Consistency check *)
	If[Total[partialOrders]!=perturbationOrder,Return[0]];
	If[Length[indexArray]/2!=perturbationOrder,Return[0]];
	(* Calculation *)
	partialArrays = {};
	For[i=1,i<=numberOfMultipliers,i++,
		AppendTo[partialArrays,indexArray[[;;2 partialOrders[[i]]]]];
		indexArray = indexArray[[2 partialOrders[[i]]+1;;]];
	];
	Return[Expand[1/(Times@@partialOrders) Times@@Apply[ITensor,partialArrays,1]]];
]

(* CTensor Export *)
exportCTensor[order_]:=Module[{cursor},
	Print["Export of I-tensors is initiated."];
	For[cursor=1,cursor<=order,cursor++,
		Evaluate[CTensorGenerator@@Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,cursor}]]]>>"Libs/CTensor_"<>ToString[cursor];
		Print["Done for order "<>ToString[cursor]];
	]
]

importCTensor:=Module[{cursor},
	cursor = 1;
	Clear[CTensor];
	CTensor[]=1;
	While[FileExistsQ["Libs/CTensor_"<>ToString[cursor]],
		Evaluate[CTensor@@Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,cursor}]]] = Get["Libs/CTensor_"<>ToString[cursor]];
		cursor++;
	];
	Print["C tensors are imported up to order "<>ToString[cursor-1]<>"."];
]


(* CIIITensor *)
CIIITensorGenerator[inputArray__]:=Module[{indexArray,tensorValence,temporaryExpression1,indexArrayForSymmetrisation},
	indexArray = List[inputArray];
	(* Consistency checks *)
	If[Length[indexArray]<6,Return[0]];
	If[Mod[Length[indexArray],2]==1,Return[0]];
	(* Last special case *)
	If[Length[indexArray]-6==0, Return[CIIITensorStructure@@Sequence[Join[{0,1,1,1},indexArray]]] ];
	(* Calculations *)
	tensorValence = Length[indexArray]/2-3;
	indexArrayForSymmetrisation = indexArray[[7;;]];
	Evaluate[temporaryExpression1@@Table[ ToExpression[ToString[indexArrayForSymmetrisation[[i]]]<>"_"],{i,1,Length[indexArrayForSymmetrisation]}] ] = Sum[KroneckerDelta[j0+j1+j2+j3,tensorValence] (-1)^(tensorValence-j0) (CIIITensorStructure@@Sequence[Join[{j0,1+j1,1+j2,1+j3},indexArray]]) ,{j0,0,tensorValence},{j1,0,tensorValence},{j2,0,tensorValence},{j3,0,tensorValence}];
	Return[Calc[1/Factorial[tensorValence] Sum[temporaryExpression1@@Flatten[Permutations[Partition[indexArrayForSymmetrisation,2]][[i]]],{i,1,tensorValence!}]]];
];

CIIITensorStructure[inputArray__]:=Module[{inputData,indexArray,indexArrayExternal,indexArrayInternal,indexArray1,indexArray2,indexArray3,indexArray4},
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

(* CIIITensor Export *)
exportCIIITensor[order_]:=Module[{cursor},
	Print["Export of CIII tensors is initiated."];
	For[cursor=1,cursor<=order,cursor++,
		Evaluate[CIIITensorGenerator@@Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,cursor}]]]>>"Libs/CIIITensor_"<>ToString[cursor];
		Print["Done for order "<>ToString[cursor]];
	]
]

importCIIITensor:=Module[{cursor},
	cursor = 1;
	Clear[CIIITensor];
	While[FileExistsQ["Libs/CIIITensor_"<>ToString[cursor]],
		Evaluate[CIIITensor@@Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,cursor}]]] = Get["Libs/CIIITensor_"<>ToString[cursor]];
		cursor++;
	];
	Print["CIII tensors are imported up to order "<>ToString[cursor-1]<>"."];
]


(* GravitonVertex *)
GravitonVertexGenerator[inputArray__]:=Module[{inputData,tensorT,tensorValence,arrayMomenta,arrayIndices,nonsymmetricExpression,symmetricExpression,dummyArray},
	inputData = List[inputArray];
	If[Length[inputData]<9,Return[0]];
	If[Mod[Length[inputData],3]!=0,Return[0]];
	tensorValence = Length[ inputData ] /3;
	dummyArray = Flatten[Table[ { ToExpression["\[ScriptM]"<>ToString[i]],ToExpression["\[ScriptN]"<>ToString[i]],ToExpression["\[ScriptP]"<>ToString[i]] } , {i,1,tensorValence}]];
	arrayMomenta = Table[ Partition[dummyArray,3][[i]][[3]] ,{i,1,tensorValence}];
	arrayIndices = Table[ Partition[dummyArray,3][[i]][[1;;2]] ,{i,1,tensorValence}];
	Evaluate[nonsymmetricExpression@@Table[ToExpression[ToString[dummyArray[[i]]]<>"_"],{i,1,Length[inputData]}]]= Calc[(FVD[ arrayMomenta[[1]] ,\[Lambda]1]   FVD[ arrayMomenta[[2]] ,\[Lambda]2]) ( TTensorGravity@@Sequence[Flatten[Join[{\[Lambda]1,\[Lambda]2} , arrayIndices[[;;2]] , {\[Rho]1,\[Sigma]1,\[Rho]2,\[Sigma]2,\[Rho]3,\[Sigma]3} ]]] )  (CIIITensor@@Sequence[Flatten[Join[{\[Rho]1,\[Sigma]1,\[Rho]2,\[Sigma]2,\[Rho]3,\[Sigma]3} , arrayIndices[[3;;]]]]]) ];
	Evaluate[symmetricExpression@@Table[ToExpression[ToString[dummyArray[[i]]]<>"_"],{i,1,Length[inputData]}]]= Calc[(-I 2/(Global`\[Kappa])^2)((Global`\[Kappa])^tensorValence/4)Plus@@nonsymmetricExpression@@@Table[ Flatten[ Permutations[Partition[dummyArray,3]][[i]] ] , {i,1,Factorial[tensorValence]}]] ;
	Return[Calc[symmetricExpression@@inputData]];
];

TTensorGravity[i_,j_,a_,b_,r_,s_,\[Mu]_,\[Nu]_,\[Alpha]_,\[Beta]_,\[Rho]_,\[Sigma]_]:=Module[{},
	Return[Expand[-MTD[\[Mu],i]MTD[\[Nu],j]ITensor[\[Alpha],\[Beta],a,b]ITensor[\[Rho],\[Sigma],r,s]+MTD[\[Mu],i]MTD[\[Nu],j]ITensor[\[Alpha],\[Rho],a,b]ITensor[\[Beta],\[Sigma],r,s]-2 MTD[\[Mu],i]MTD[\[Alpha],j]ITensor[\[Beta],\[Rho],a,b]ITensor[\[Nu],\[Sigma],r,s]+2 MTD[\[Mu],i]MTD[\[Beta],j]ITensor[\[Nu],\[Alpha],a,b]ITensor[\[Rho],\[Sigma],r,s]]];
]

(* Graviton vertices export *)
exportGravitonVertex[order_]:=Module[{cursor},
	Print["Export of graviton vertices is initiated."];
	For[cursor=1,cursor<=order,cursor++,
		Evaluate[GravitonVertexGenerator@@Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]],ToExpression["p"<>ToString[i]]},{i,1,cursor}]]]>>"Libs/GravitonVertex_"<>ToString[cursor];
		Print["Done for order "<>ToString[cursor]];
	]
]

(* Graviton vertices import *)
importGravitonVertex:=Module[{cursor},
	cursor = 1;
	Clear[GravitonVertex];
	While[FileExistsQ["Libs/GravitonVertex_"<>ToString[cursor]],
		Evaluate[GravitonVertex@@Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"],ToExpression["p"<>ToString[i]<>"_"]},{i,1,cursor}]]] = Get["Libs/GravitonVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton vertices are imported up to order "<>ToString[cursor-1]<>"."];
]


(* CITensor *)
CITensorGenerator[inputArray__]:=Module[{indexArray,tensorValence,temporaryExpression1,indexArrayForSymmetrisation},
	indexArray = List[inputArray];
	(* Consistency checks *)
	If[Length[indexArray]<2,Return[0]];
	If[Mod[Length[indexArray],2]==1,Return[0]];
	(* Last special case *)
	If[Length[indexArray]==2, Return[CITensorStructure@@Sequence[Join[{0,1},indexArray]]] ];
	(* Calculations *)
	tensorValence = Length[indexArray]/2-1;
	indexArrayForSymmetrisation = indexArray[[3;;]];
	Evaluate[temporaryExpression1@@Table[ ToExpression[ToString[indexArrayForSymmetrisation[[i]]]<>"_"],{i,1,Length[indexArrayForSymmetrisation]}] ] = Sum[KroneckerDelta[j0+j1,tensorValence] (-1)^(tensorValence-j0) (CITensorStructure@@Sequence[Join[{j0,1+j1},indexArray]]) ,{j0,0,tensorValence},{j1,0,tensorValence}];
	Return[Calc[1/Factorial[tensorValence] Sum[temporaryExpression1@@Flatten[Permutations[Partition[indexArrayForSymmetrisation,2]][[i]]],{i,1,tensorValence!}]]];
];

CITensorStructure[inputArray__]:=Module[{inputData,indexArray,indexArrayExternal,indexArrayInternal,indexArray1,indexArray2},
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

(* CITensor Export *)
exportCITensor[order_]:=Module[{cursor},
	Print["Export of CI tensors is initiated."];
	For[cursor=1,cursor<=order,cursor++,
		Evaluate[CITensorGenerator@@Flatten[Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,cursor}]]]>>"Libs/CITensor_"<>ToString[cursor];
		Print["Done for order "<>ToString[cursor]];
	]
]

importCITensor:=Module[{cursor},
	cursor = 1;
	Clear[CITensor];
	While[FileExistsQ["Libs/CITensor_"<>ToString[cursor]],
		Evaluate[CITensor@@Flatten[Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,cursor}]]] = Get["Libs/CITensor_"<>ToString[cursor]];
		cursor++;
	];
	Print["CI tensors are imported up to order "<>ToString[cursor-1]<>"."];
]


(* GravitonScalarVertex *)
GravitonScalarVertexGenerator[inputArray__]:=Module[{inputData,TVertex},
	inputData = List[inputArray];
	If[ Length[inputData] < 4 , Return[0]];
	If[ Mod[Length[inputData],2] == 1,Return[0]];
		Return[ Calc[ I (Global`\[Kappa])^(Length[inputData]/2-1) ( TTensorScalar @@Sequence[Join[ {\[ScriptM],\[ScriptN]} , inputData[[ Length[inputData]-1;;]] ]] ) (CITensor @@ Sequence[Join[ {\[ScriptM],\[ScriptN]} , inputData[[;;Length[inputData]-2]] ]])] ];
];

TTensorScalar[m_,n_,p_,q_]:=Module[{a,b},Return[Calc[-(1/2)ITensor[m,n,a,b]FVD[p,a]FVD[q,b]]]  ];

(* Graviton-Scalar vertices export *)
exportGravitonScalarVertex[order_]:=Module[{cursor},
	Print["Export of graviton-scalar vertices is initiated."];
	For[cursor=1,cursor<=order,cursor++,
		Evaluate[GravitonScalarVertexGenerator@@Flatten[{Table[{ToExpression["m"<>ToString[i]],ToExpression["n"<>ToString[i]]},{i,1,cursor}],{p1,p2}}]]>>"Libs/GravitonScalarVertex_"<>ToString[cursor];
		Print["Done for order "<>ToString[cursor]];
	]
]

(* Graviton-Scalar vertices import *)
importGravitonVertex:=Module[{cursor,p1,p2},
	cursor = 1;
	Clear[GravitonScalarVertex];
	While[FileExistsQ["Libs/GravitonScalarVertex_"<>ToString[cursor]],
		Evaluate[GravitonScalarVertex@@Flatten[{Table[{ToExpression["m"<>ToString[i]<>"_"],ToExpression["n"<>ToString[i]<>"_"]},{i,1,cursor}],{p1,p2}}]] = Get["Libs/GravitonScalarVertex_"<>ToString[cursor]];
		cursor++;
	];
	Print["Graviton-Scalar vertices are imported up to order "<>ToString[cursor-1]<>"."];
]


Module[{perturbationOrder = 2},
	exportITensor[perturbationOrder+1];
	importITensor;
	exportCTensor[perturbationOrder];
	importCTensor;
	exportCIIITensor[3+perturbationOrder];
	importCIIITensor;
	exportGravitonVertex[perturbationOrder+2];
	exportCITensor[perturbationOrder+1];
	importCITensor;
	exportGravitonScalarVertex[perturbationOrder];
]



