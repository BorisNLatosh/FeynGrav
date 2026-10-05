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
                {"GammaTensor`", "GammaTensor.wl"},
                {"indexArraySymmetrization`", "indexArraySymmetrization.wl"}
            }
        ]
    ]
];


(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["HorndeskiG3`",{"FeynCalc`","ITensor`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


HorndeskiG3::usage =
"HorndeskiG3[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b + 1\)]\)},b]. \
Expression for Horndeski interaction of \!\(\*SubscriptBox[\(G\), \(3\)]\) class. Function arguments are {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\),\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton Lorentz indices and momenta; \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta; b is the number of scalar field kinetic term.";


HorndeskiG3Uncontracted::usage =
"HorndeskiG3[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b + 1\)]\)},b]. \
Expression for Horndeski interaction of \!\(\*SubscriptBox[\(G\), \(3\)]\) class. Function arguments are {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\),\!\(\*SubscriptBox[\(k\), \(i\)]\)} are graviton Lorentz indices and momenta; \!\(\*SubscriptBox[\(p\), \(i\)]\) are scalar field momenta; b is the number of scalar field kinetic term.";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
HorndeskiG3::usage = HorndeskiG3::usage <> " Supported signatures: HorndeskiG3[gravitonParameters, scalarMomenta, b]; argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
HorndeskiG3Uncontracted::usage = HorndeskiG3Uncontracted::usage <> " Supported signatures: HorndeskiG3Uncontracted[gravitonParameters, scalarMomenta, b]; argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

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


Clear[takeIndices];

takeIndices[indexArray_] :=
    RuleValidation`RuleCall[
        takeIndices[indexArray],
        {{1, "Array", 3, 0, Infinity}},
        (
Flatten[RuleValidation`RuleRequire[ #[[;;2]]& /@ Partition[indexArray,3] ]]
        ), False
    ];


Clear[HorndeskiG3Core];

HorndeskiG3Core[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG3Core[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				RuleValidation`RuleRequire[MomentaWrapper[#[[;;2b]]]]*
				(
					RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN]},RuleValidation`RuleRequire[DummyArray2[b]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters]]]] FVD[#[[2b+1]],\[ScriptM]] FVD[#[[2b+1]],\[ScriptN]]
					- RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptR],\[ScriptS]},RuleValidation`RuleRequire[DummyArray2[b]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]]]]RuleValidation`RuleRequire[GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]] FVD[#[[2b+1]],\[ScriptS]] FVD[gravitonParameters[[-1]],\[ScriptL]]
				) &,
				Permutations[scalarMomenta]
			]]
		]]
        ), True
    ];


Clear[HorndeskiG3];

HorndeskiG3[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG3[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
I Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+1]*
			Total[RuleValidation`RuleRequire[
				RuleValidation`RuleRequire[Map[
					RuleValidation`RuleRequire[HorndeskiG3UncontractedCore[#,scalarMomenta,b]]& ,
					Flatten/@Permutations[Partition[gravitonParameters,3]]
				]]
			]] //MomentumExpand//Contract
        ), True
    ];


Clear[HorndeskiG3UncontractedCore];

HorndeskiG3UncontractedCore[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG3UncontractedCore[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				RuleValidation`RuleRequire[MomentaWrapper[#[[;;2b]]]]*
				(
					RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN]},RuleValidation`RuleRequire[DummyArray2[b]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters]]]] FVD[#[[2b+1]],\[ScriptM]] FVD[#[[2b+1]],\[ScriptN]]
					- RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptR],\[ScriptS]},RuleValidation`RuleRequire[DummyArray2[b]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]]]]RuleValidation`RuleRequire[GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]] FVD[#[[2b+1]],\[ScriptS]] FVD[gravitonParameters[[-1]],\[ScriptL]]
				) &,
				Permutations[scalarMomenta]
			]]
		]]
        ), True
    ];


Clear[HorndeskiG3Uncontracted];

HorndeskiG3Uncontracted[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG3Uncontracted[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
I Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+1]*
			Total[RuleValidation`RuleRequire[
				RuleValidation`RuleRequire[Map[
					RuleValidation`RuleRequire[HorndeskiG3UncontractedCore[#,scalarMomenta,b]]& ,
					Flatten/@Permutations[Partition[gravitonParameters,3]]
				]]
			]]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
DummyArray2[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[DummyArray2] <> SymbolName[DummyArray2], {arguments}, {1}];

HorndeskiG3[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG3] <> SymbolName[HorndeskiG3], {arguments}, {3}];

HorndeskiG3Core[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG3Core] <> SymbolName[HorndeskiG3Core], {arguments}, {3}];

HorndeskiG3Uncontracted[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG3Uncontracted] <> SymbolName[HorndeskiG3Uncontracted], {arguments}, {3}];

HorndeskiG3UncontractedCore[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG3UncontractedCore] <> SymbolName[HorndeskiG3UncontractedCore], {arguments}, {3}];

MomentaWrapper[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[MomentaWrapper] <> SymbolName[MomentaWrapper], {arguments}, {1}];

takeIndices[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[takeIndices] <> SymbolName[takeIndices], {arguments}, {1}];

End[];


EndPackage[];
