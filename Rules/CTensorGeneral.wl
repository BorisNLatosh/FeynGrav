(* ::Package:: *)

(* Resolve sibling dependencies before nested loads change $InputFileName.
   Keep the caller's working directory and context search path unchanged. *)
With[{rulesDirectory = DirectoryName[$InputFileName]},
    Block[{$ContextPath = $ContextPath},
        Scan[
            Needs[#[[1]], FileNameJoin[{rulesDirectory, #[[2]]}]] &,
            {
                {"ITensor`", "ITensor.wl"},
                {"indexArraySymmetrization`", "indexArraySymmetrization.wl"}
            }
        ]
    ]
];


(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["CTensorGeneral`",{"FeynCalc`","ITensor`","indexArraySymmetrization`"}];


CTensorPlainGeneral::usage = "CTensorPlainGeneral[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(p\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(p\)]\)},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\)\[Ellipsis] \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(p\)] \*SubscriptBox[\(\[Nu]\), \(p\)]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The number of the inverse metrics in the bracets is p = 0,\[Ellipsis],7. The definition does not allow for any symmetry.";


CTensorGeneral::usage = "CTensorGeneral[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(p\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(p\)]\)},{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}]. The function returns (\!\(\*SqrtBox[\(-g\)]\)\!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(1\)] \*SubscriptBox[\(\[Nu]\), \(1\)]\)]\)\[Ellipsis] \!\(\*SuperscriptBox[\(g\), \(\*SubscriptBox[\(\[Mu]\), \(p\)] \*SubscriptBox[\(\[Nu]\), \(p\)]\)]\)\!\(\*SuperscriptBox[\()\), \(\*SubscriptBox[\(\[Rho]\), \(1\)] \*SubscriptBox[\(\[Sigma]\), \(1\)] \*SubscriptBox[\(\[Ellipsis]\[Rho]\), \(n\)] \*SubscriptBox[\(\[Sigma]\), \(n\)]\)]\). The number of the inverse metrics in the bracets is p = 0,\[Ellipsis],7.";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
CTensorPlainGeneral::usage = CTensorPlainGeneral::usage <> " Supported signatures: CTensorPlainGeneral[indexArrayExternal, indexArrayInternal]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity; At most seven external index pairs are implemented. Condition: Length[indexArrayExternal] <= 14.." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
CTensorGeneral::usage = CTensorGeneral::usage <> " Supported signatures: CTensorGeneral[indexArrayExternal, indexArrayInternal]; argument 1: flat list, block size 2, length 0 to Infinity; argument 2: flat list, block size 2, length 0 to Infinity; At most seven external index pairs are implemented. Condition: Length[indexArrayExternal] <= 14.." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

Begin["`Private`"];


(* C Tensor *)


Clear[CTensorPlain];

CTensorPlain[{}] = 1;

CTensorPlain[indexArray_] :=
    RuleValidation`RuleCall[
        CTensorPlain[indexArray],
        {{1, "Array", 2, 0, Infinity}},
        (
(1/Length[indexArray])*
		Sum[
			(-1)^(i - 1) RuleValidation`RuleRequire[ITensorPlain[indexArray[[;; 2 i]]]] RuleValidation`RuleRequire[CTensorPlain[indexArray[[2 i + 1 ;;]]]] ,
			{i,Length[indexArray]/2}
		]
        ), True
    ];


Clear[CTensor];

CTensor[indexArray_] :=
    RuleValidation`RuleCall[
        CTensor[indexArray],
        {{1, "Array", 2, 0, Infinity}},
        (
1/Power[2, Length[indexArray]/2] 1/Factorial[Length[indexArray]/2] Total[RuleValidation`RuleRequire[ RuleValidation`RuleRequire[Map[ CTensorPlain , RuleValidation`RuleRequire[indexArraySymmetrization[indexArray]] ]] ]]
        ), True
    ];


(* C1 Tensor *)


Clear[C1TensorPlain];

C1TensorPlain[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C1TensorPlain[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
If[
			Length[indexArrayExternal] == 0,
			0,
			Sum[
				Power[-1, i] RuleValidation`RuleRequire[ITensorPlain[Join[indexArrayExternal, indexArrayInternal[[;; 2 i]]]]] RuleValidation`RuleRequire[CTensorPlain[indexArrayInternal[[2 i + 1 ;;]]]],
				{i,0,Length[indexArrayInternal]/2}
			]
		]
        ), True
    ];


Clear[C1Tensor];

C1Tensor[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C1Tensor[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				RuleValidation`RuleRequire[C1TensorPlain[indexArrayExternal,#]]& ,
				RuleValidation`RuleRequire[indexArraySymmetrization[indexArrayInternal]]
			]]
		]]
        ), True
    ];


(* C2 Tensor *)


Clear[C2TensorPlain];

C2TensorPlain[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C2TensorPlain[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
If[
			Length[indexArrayExternal]!=4,
			0,
			Sum[
				Power[-1,i] RuleValidation`RuleRequire[ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2 i]]] ]] RuleValidation`RuleRequire[C1TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2 i + 1;;]]]] ,
				{i,0,Length[indexArrayInternal]/2}
			]
		]
        ), True
    ];


Clear[C2Tensor];

C2Tensor[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C2Tensor[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				RuleValidation`RuleRequire[C2TensorPlain[indexArrayExternal,#]]&,
				RuleValidation`RuleRequire[indexArraySymmetrization[indexArrayInternal]]
			]]
		]]
        ), True
    ];


(* C3 Tensor *)


Clear[C3TensorPlain];

C3TensorPlain[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C3TensorPlain[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
If[
			Length[indexArrayExternal]!=6,
			0,
			Sum[
				Power[-1,i] RuleValidation`RuleRequire[ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2 i]]] ]] RuleValidation`RuleRequire[C2TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2 i+1;;]]]] ,
				{i,0,Length[indexArrayInternal]/2}
			]
		]
        ), True
    ];


Clear[C3Tensor];

C3Tensor[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C3Tensor[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				RuleValidation`RuleRequire[C3TensorPlain[indexArrayExternal,#]]&,
				RuleValidation`RuleRequire[indexArraySymmetrization[indexArrayInternal]]
			]]
		]]
        ), True
    ];


(* C4 Tensor *)


Clear[C4TensorPlain];

C4TensorPlain[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C4TensorPlain[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
If[
			Length[indexArrayExternal]!=8,
			0,
			Sum[
				Power[-1,i] RuleValidation`RuleRequire[ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2 i]]] ]] RuleValidation`RuleRequire[C3TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2 i+1;;]]]],
				{i,0,Length[indexArrayInternal]/2}
			]
		]
        ), True
    ];


Clear[C4Tensor];

C4Tensor[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C4Tensor[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				RuleValidation`RuleRequire[C4TensorPlain[indexArrayExternal,#]]&,
				RuleValidation`RuleRequire[indexArraySymmetrization[indexArrayInternal]]
			]]
		]]
        ), True
    ];


(* C5 Tensor *)


Clear[C5TensorPlain];

C5TensorPlain[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C5TensorPlain[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
If[
			Length[indexArrayExternal]!=2*5,
			0,
			Sum[
				Power[-1,i] RuleValidation`RuleRequire[ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2 i]]] ]] RuleValidation`RuleRequire[C4TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2 i+1;;]]]],
				{i,0,Length[indexArrayInternal]/2}
			]
		]
        ), True
    ];


Clear[C5Tensor];

C5Tensor[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C5Tensor[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				RuleValidation`RuleRequire[C5TensorPlain[indexArrayExternal,#]]&,
				RuleValidation`RuleRequire[indexArraySymmetrization[indexArrayInternal]]
			]]
		]]
        ), True
    ];


(* C6 Tensor *)


Clear[C6TensorPlain];

C6TensorPlain[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C6TensorPlain[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
If[
			Length[indexArrayExternal]!=2*6,
			0,
			Sum[
				Power[-1,i] RuleValidation`RuleRequire[ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2 i]]] ]] RuleValidation`RuleRequire[C5TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2 i+1;;]]]],
				{i,0,Length[indexArrayInternal]/2}
			]
		]
        ), True
    ];


Clear[C6Tensor];

C6Tensor[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C6Tensor[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				RuleValidation`RuleRequire[C6TensorPlain[indexArrayExternal,#]]&,
				RuleValidation`RuleRequire[indexArraySymmetrization[indexArrayInternal]]
			]]
		]]
        ), True
    ];


(* C7 Tensor *)


Clear[C7TensorPlain];

C7TensorPlain[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C7TensorPlain[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
If[
			Length[indexArrayExternal]!=2*7,
			0,
			Sum[
				Power[-1,i] RuleValidation`RuleRequire[ITensorPlain[ Join[indexArrayExternal[[;;2]],indexArrayInternal[[;;2 i]]] ]] RuleValidation`RuleRequire[C6TensorPlain[indexArrayExternal[[3;;]],indexArrayInternal[[2 i+1;;]]]] ,
				{i,0,Length[indexArrayInternal]/2}
			]
		]
        ), True
    ];


Clear[C7Tensor];

C7Tensor[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        C7Tensor[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}},
        (
1/Power[2,Length[indexArrayInternal]/2] 1/Factorial[Length[indexArrayInternal]/2]*
		Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				RuleValidation`RuleRequire[C7TensorPlain[indexArrayExternal,#]]&,
				RuleValidation`RuleRequire[indexArraySymmetrization[indexArrayInternal]]
			]]
		]]
        ), True
    ];


(* C Tensor General *)


Clear[CTensorPlainGeneral];

CTensorPlainGeneral[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        CTensorPlainGeneral[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}, {0, "Supported", Length[indexArrayExternal] <= 14, "At most seven external index pairs are implemented."}},
        (
Switch[Length[indexArrayExternal]/2,
			0,RuleValidation`RuleRequire[CTensorPlain[indexArrayInternal]],
			1,RuleValidation`RuleRequire[C1TensorPlain[indexArrayExternal,indexArrayInternal]],
			2,RuleValidation`RuleRequire[C2TensorPlain[indexArrayExternal,indexArrayInternal]],
			3,RuleValidation`RuleRequire[C3TensorPlain[indexArrayExternal,indexArrayInternal]],
			4,RuleValidation`RuleRequire[C4TensorPlain[indexArrayExternal,indexArrayInternal]],
			5,RuleValidation`RuleRequire[C5TensorPlain[indexArrayExternal,indexArrayInternal]],
			6,RuleValidation`RuleRequire[C6TensorPlain[indexArrayExternal,indexArrayInternal]],
			7,RuleValidation`RuleRequire[C7TensorPlain[indexArrayExternal,indexArrayInternal]]
		]
        ), True
    ];


Clear[CTensorGeneral];

CTensorGeneral[indexArrayExternal_, indexArrayInternal_] :=
    RuleValidation`RuleCall[
        CTensorGeneral[indexArrayExternal, indexArrayInternal],
        {{1, "Array", 2, 0, Infinity}, {2, "Array", 2, 0, Infinity}, {0, "Supported", Length[indexArrayExternal] <= 14, "At most seven external index pairs are implemented."}},
        (
Switch[Length[indexArrayExternal]/2,
			0,RuleValidation`RuleRequire[CTensor[indexArrayInternal]],
			1,RuleValidation`RuleRequire[C1Tensor[indexArrayExternal,indexArrayInternal]],
			2,RuleValidation`RuleRequire[C2Tensor[indexArrayExternal,indexArrayInternal]],
			3,RuleValidation`RuleRequire[C3Tensor[indexArrayExternal,indexArrayInternal]],
			4,RuleValidation`RuleRequire[C4Tensor[indexArrayExternal,indexArrayInternal]],
			5,RuleValidation`RuleRequire[C5Tensor[indexArrayExternal,indexArrayInternal]],
			6,RuleValidation`RuleRequire[C6Tensor[indexArrayExternal,indexArrayInternal]],
			7,RuleValidation`RuleRequire[C7Tensor[indexArrayExternal,indexArrayInternal]]
		]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
C1Tensor[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C1Tensor] <> SymbolName[C1Tensor], {arguments}, {2}];

C1TensorPlain[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C1TensorPlain] <> SymbolName[C1TensorPlain], {arguments}, {2}];

C2Tensor[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C2Tensor] <> SymbolName[C2Tensor], {arguments}, {2}];

C2TensorPlain[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C2TensorPlain] <> SymbolName[C2TensorPlain], {arguments}, {2}];

C3Tensor[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C3Tensor] <> SymbolName[C3Tensor], {arguments}, {2}];

C3TensorPlain[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C3TensorPlain] <> SymbolName[C3TensorPlain], {arguments}, {2}];

C4Tensor[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C4Tensor] <> SymbolName[C4Tensor], {arguments}, {2}];

C4TensorPlain[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C4TensorPlain] <> SymbolName[C4TensorPlain], {arguments}, {2}];

C5Tensor[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C5Tensor] <> SymbolName[C5Tensor], {arguments}, {2}];

C5TensorPlain[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C5TensorPlain] <> SymbolName[C5TensorPlain], {arguments}, {2}];

C6Tensor[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C6Tensor] <> SymbolName[C6Tensor], {arguments}, {2}];

C6TensorPlain[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C6TensorPlain] <> SymbolName[C6TensorPlain], {arguments}, {2}];

C7Tensor[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C7Tensor] <> SymbolName[C7Tensor], {arguments}, {2}];

C7TensorPlain[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[C7TensorPlain] <> SymbolName[C7TensorPlain], {arguments}, {2}];

CTensor[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[CTensor] <> SymbolName[CTensor], {arguments}, {1}];

CTensorGeneral[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[CTensorGeneral] <> SymbolName[CTensorGeneral], {arguments}, {2}];

CTensorPlain[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[CTensorPlain] <> SymbolName[CTensorPlain], {arguments}, {1}];

CTensorPlainGeneral[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[CTensorPlainGeneral] <> SymbolName[CTensorPlainGeneral], {arguments}, {2}];

End[];


EndPackage[];
