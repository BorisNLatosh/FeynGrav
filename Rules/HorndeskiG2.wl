(* ::Package:: *)

(* Resolve sibling dependencies before nested loads change $InputFileName.
   Keep the caller's working directory and context search path unchanged. *)
With[{rulesDirectory = DirectoryName[$InputFileName]},
    Block[{$ContextPath = $ContextPath},
        Scan[
            Needs[#[[1]], FileNameJoin[{rulesDirectory, #[[2]]}]] &,
            {
                {"ITensor`", "ITensor.wl"},
                {"CTensorGeneral`", "CTensorGeneral.wl"},
                {"indexArraySymmetrization`", "indexArraySymmetrization.wl"}
            }
        ]
    ]
];


(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["HorndeskiG2`",{"FeynCalc`","ITensor`","CTensorGeneral`","indexArraySymmetrization`"}];


HorndeskiG2::usage =
"HorndeskiG2[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b]. \
Expression for Horndeski interaction of \!\(\*SubscriptBox[\(G\), \(2\)]\) class. Involves a+2b\[GreaterEqual]3 scalars. Function arguments are {Subscript[\[Rho], i],Subscript[\[Sigma], i]} are graviton indices; Subscript[p, i] are scalar field momenta; b is the number of scalar field kinetic terms.";


HorndeskiG2Uncontracted::usage =
"HorndeskiG2[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b]. \
Expression for Horndeski interaction of \!\(\*SubscriptBox[\(G\), \(2\)]\) class. Involves a+2b\[GreaterEqual]3 scalars. Function arguments are {Subscript[\[Rho], i],Subscript[\[Sigma], i]} are graviton indices; Subscript[p, i] are scalar field momenta; b is the number of scalar field kinetic terms.";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
HorndeskiG2::usage = HorndeskiG2::usage <> " Supported signatures: HorndeskiG2[gravitonParameters, scalarMomenta, b]; argument 1: flat list, block size 2, length 0 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
HorndeskiG2Uncontracted::usage = HorndeskiG2Uncontracted::usage <> " Supported signatures: HorndeskiG2Uncontracted[gravitonParameters, scalarMomenta, b]; argument 1: flat list, block size 2, length 0 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

Begin["`Private`"];


Clear[MomentaWrapper];

MomentaWrapper[scalarMomenta_] :=
    RuleValidation`RuleCall[
        MomentaWrapper[scalarMomenta],
        {{1, "Array", 2, 0, Infinity}},
        (
Apply[
		Times,
		RuleValidation`RuleRequire[MapThread[
			FVD,
			{scalarMomenta, RuleValidation`RuleRequire[DummyArray2[ Quotient[Length[scalarMomenta], 2] ]] }
		]]
	]
        ), False
    ];


Clear[DummyArray2];

DummyArray2[n_] :=
    RuleValidation`RuleCall[
        DummyArray2[n],
        {{1, "Integer", 0}},
        (
Flatten[RuleValidation`RuleRequire[
		Table[
			{
				Symbol["\[ScriptA]" <> ToString[i]],
				Symbol["\[ScriptB]" <> ToString[i]]
			},
		{i, n}
	]
]]
        ), False
    ];


Clear[HorndeskiG2];

HorndeskiG2[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG2[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 2, 0, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        (
Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				I (Global`\[Kappa])^(Length[gravitonParameters]/2) Power[-1,b] RuleValidation`RuleRequire[CTensorGeneral[RuleValidation`RuleRequire[DummyArray2[b]],gravitonParameters]] RuleValidation`RuleRequire[MomentaWrapper[#[[;;2b]]]] & ,
				Permutations[scalarMomenta]
			]]
		]] //Contract
        ), True
    ];


Clear[HorndeskiG2Uncontracted];

HorndeskiG2Uncontracted[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG2Uncontracted[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 2, 0, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        (
Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				I (Global`\[Kappa])^(Length[gravitonParameters]/2) Power[-1,b] RuleValidation`RuleRequire[CTensorGeneral[RuleValidation`RuleRequire[DummyArray2[b]],gravitonParameters]] RuleValidation`RuleRequire[MomentaWrapper[#[[;;2b]]]] & ,
				Permutations[scalarMomenta]
			]]
		]]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
DummyArray2[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[DummyArray2] <> SymbolName[DummyArray2], {arguments}, {1}];

HorndeskiG2[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG2] <> SymbolName[HorndeskiG2], {arguments}, {3}];

HorndeskiG2Uncontracted[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG2Uncontracted] <> SymbolName[HorndeskiG2Uncontracted], {arguments}, {3}];

MomentaWrapper[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[MomentaWrapper] <> SymbolName[MomentaWrapper], {arguments}, {1}];

End[];


EndPackage[];
