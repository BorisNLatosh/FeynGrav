(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];


BeginPackage["CETensor`",{"FeynCalc`","ETensor`","CTensorGeneral`","indexArraySymmetrization`"}];


CETensor::usage = 
"CETensor[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. \
The function returns (\!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(\[Mu]\)], \(m\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). \
The tensor is symmetric.";


CETensorPlain::usage = 
"CETensorPlain[{\[Mu],\[Nu]},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. \
The function returns (\!\(\*SqrtBox[\(-g\)]\) \!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(\[Mu]\)], \(m\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). \
The tensor is not symmetric.";


CEIETensor::usage = 
"CEIETensor[{\[Alpha],a},{\[Beta],b},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. \
The function returns (\!\(\*SqrtBox[\(-g\)]\) \!\(\*SubscriptBox[SuperscriptBox[\(\[GothicE]\), \(\[Alpha]\)], \(a\)]\) \!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(\[Beta]\)], \(b\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). \
The tensor is symmetric";


CEIETensorPlain::usage = 
"CEIETensorPlain[{\[Alpha],a},{\[Beta],b},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. \
The function returns (\!\(\*SqrtBox[\(-g\)]\) \!\(\*SubscriptBox[SuperscriptBox[\(\[GothicE]\), \(\[Alpha]\)], \(a\)]\) \!\(\*SuperscriptBox[SubscriptBox[\(\[GothicE]\), \(\[Beta]\)], \(b\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). \
The tensor is not symmetric";


Begin["Private`"];


(* CETensorPlain *)


Clear[CETensorPlain];

CETensorPlain[indexArrayExternal_,indexArrayInternal_] := CE1TensorPlain[indexArrayExternal,indexArrayInternal];
CETensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := CE2TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal];
CETensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := CE3TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal];


Clear[CE1TensorPlain];

CE1TensorPlain[indexArrayExternal_,indexArrayInternal_] := 
	CE1TensorPlain[indexArrayExternal,indexArrayInternal] = 
		Sum[
			ETensorPlain[indexArrayExternal,indexArrayInternal[[;;2k]]] CTensorPlainGeneral[{},indexArrayInternal[[2k+1;;]]] ,
			{k,0,Length[indexArrayInternal]/2}
		];


Clear[CE2TensorPlain];

CE2TensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := 
	CE2TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal] = 
		Sum[
			ETensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal[[;;2k]]] CTensorPlainGeneral[{},indexArrayInternal[[2k+1;;]]] ,
			{k,0,Length[indexArrayInternal]/2}
		];


Clear[CE3TensorPlain];

CE3TensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] :=
	CE3TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal] =
		Sum[
			ETensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal[[;;2k]]] CTensorPlainGeneral[{},indexArrayInternal[[2k+1;;]]] ,
			{k,0,Length[indexArrayInternal]/2}
		];


(* CETensor *)


Clear[CETensor];

CETensor[indexArrayExternal_,indexArrayInternal_] := CE1Tensor[indexArrayExternal,indexArrayInternal];
CETensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := CE2Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayInternal];
CETensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := CE3Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal];


Clear[CE1Tensor];

CE1Tensor[indexArrayExternal_,indexArrayInternal_] := 
	CE1Tensor[indexArrayExternal,indexArrayInternal] = 
		Total[ 
			Map[
				1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CETensorPlain[indexArrayExternal,#]&,
				indexArraySymmetrization[indexArrayInternal]
			]
		]//Expand;


Clear[CE2Tensor];

CE2Tensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := 
	CE2Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayInternal] = 
		Total[
			Map[
				1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CETensorPlain[indexArrayExternal1,indexArrayExternal2,#]&,
				indexArraySymmetrization[indexArrayInternal]
			] 
		]//Expand;


Clear[CE3Tensor];

CE3Tensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := 
	CE3Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal] = 
		Total[
			Map[
				1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CETensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,#]&,
				indexArraySymmetrization[indexArrayInternal]
			]
		]//Expand;


(* CEITensorPlain *)


Clear[CEITensorPlain];

CEITensorPlain[indexArrayExternal_,indexArrayInternal_] := CEI1TensorPlain[indexArrayExternal,indexArrayInternal];
CEITensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := CEI2TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal];
CEITensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := CEI3TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal];


Clear[CEI1TensorPlain];

CEI1TensorPlain[indexArrayExternal_,indexArrayInternal_] :=
	CEI1TensorPlain[indexArrayExternal,indexArrayInternal] =
		Sum[
			EITensorPlain[indexArrayExternal,indexArrayInternal[[;;2k]]] CTensorPlainGeneral[{},indexArrayInternal[[2k+1;;]]],
			{k,0,Length[indexArrayInternal]/2}
		];


Clear[CE2TensorPlain];

CE2TensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := 
	CE2TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal] = 
		Sum[
			EITensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal[[;;2k]]] CTensorPlainGeneral[{},indexArrayInternal[[2k+1;;]]] ,
			{k,0,Length[indexArrayInternal]/2}
		];


Clear[CE3TensorPlain];

CE3TensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := 
	CE3TensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal] = 
		Sum[
			EITensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal[[;;2k]]] CTensorPlainGeneral[{},indexArrayInternal[[2k+1;;]]] ,
			{k,0,Length[indexArrayInternal]/2}
		];


(* CEITensor *)


Clear[CEITensor];

CEITensor[indexArrayExternal_,indexArrayInternal_] := CEI1Tensor[indexArrayExternal,indexArrayInternal];
CEITensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := CEI2Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayInternal];
CEITensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := CEI3Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal];


Clear[CEI1Tensor];

CEI1Tensor[indexArrayExternal_,indexArrayInternal_] := 
	CEI1Tensor[indexArrayExternal,indexArrayInternal] = 
		Total[
			Map[
				1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CEITensorPlain[indexArrayExternal,#]&,
				indexArraySymmetrization[indexArrayInternal]
			]
		]//Expand;


Clear[CEI2Tensor];

CEI2Tensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := 
	CEI2Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayInternal] = 
		Total[
			Map[
				1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CEITensorPlain[indexArrayExternal1,indexArrayExternal2,#]&,
				indexArraySymmetrization[indexArrayInternal]
			]
		]//Expand;


Clear[CEI3Tensor];

CEI3Tensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := 
	CEI3Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal] = 
		Total[
			Map[
				1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CEITensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,#]&,
				indexArraySymmetrization[indexArrayInternal]
			]
		]//Expand;


(* CEITensor *)


Clear[CEITensor];

CEITensor[indexArrayExternal_,indexArrayInternal_] := CEI1Tensor[indexArrayExternal,indexArrayInternal];
CEITensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := CEI2Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayInternal];
CEITensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := CEI3Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal];


Clear[CEI1Tensor];

CEI1Tensor[indexArrayExternal_,indexArrayInternal_] := 
	CEI1Tensor[indexArrayExternal,indexArrayInternal] = 
		Total[
			Map[
				1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CEITensorPlain[indexArrayExternal,#]&,
				indexArraySymmetrization[indexArrayInternal]
			]
		]//Expand;


Clear[CEI2Tensor];

CEI2Tensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := 
	CEI2Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayInternal] = 
		Total[
			Map[
				1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CEITensorPlain[indexArrayExternal1,indexArrayExternal2,#]&,
				indexArraySymmetrization[indexArrayInternal]
			]
		]//Expand;


Clear[CEI3Tensor];

CEI3Tensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayExternal3_,indexArrayInternal_] := 
	CEI3Tensor[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,indexArrayInternal] = 
		Total[
			Map[
				1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CEITensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayExternal3,#]&,
				indexArraySymmetrization[indexArrayInternal]
			]
		]//Expand;


(* CEIETensorPlain and CEIETensor *)


Clear[CEIETensorPlain];

CEIETensorPlain[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := 
	CEIETensorPlain[indexArrayExternal1,indexArrayExternal2,indexArrayInternal] = 
		Sum[
			ETensorPlain[indexArrayExternal2,indexArrayInternal[[;;2k]]] CE1TensorPlain[indexArrayExternal1,indexArrayInternal[[2k+1;;]]] ,
			{k,0,Length[indexArrayInternal]/2}
		];


Clear[CEIETensor];

CEIETensor[indexArrayExternal1_,indexArrayExternal2_,indexArrayInternal_] := 
	CEIETensor[indexArrayExternal1,indexArrayExternal2,indexArrayInternal] = 
		Total[
			Map[
				1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2] CEIETensorPlain[indexArrayExternal1,indexArrayExternal2,#]&,
				indexArraySymmetrization[indexArrayInternal]
			]
		]//Expand;


End[];


EndPackage[];
