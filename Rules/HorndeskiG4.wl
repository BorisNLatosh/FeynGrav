(* ::Package:: *)

(* Resolve sibling dependencies before nested loads change $InputFileName.
   Keep the caller's working directory and context search path unchanged. *)
With[{rulesDirectory = DirectoryName[$InputFileName]},
    Block[{$ContextPath = $ContextPath},
        Scan[
            Needs[#[[1]], FileNameJoin[{rulesDirectory, #[[2]]}]] &,
            {
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

BeginPackage["HorndeskiG4`",{"FeynCalc`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


HorndeskiG4::usage =
"HorndeskiG4[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b].";


HorndeskiG4Uncontracted::usage =
"HorndeskiG4[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)},{\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(p\), \(a + 2  b\)]\)},b].";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
HorndeskiG4::usage = HorndeskiG4::usage <> " Supported signatures: HorndeskiG4[gravitonParameters, scalarMomenta, b]; argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity; The contracted multi-graviton G4 branch for b >= 2 is not implemented. Condition: !(Length[gravitonParameters] >= 6 && b >= 2).." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
HorndeskiG4Uncontracted::usage = HorndeskiG4Uncontracted::usage <> " Supported signatures: HorndeskiG4Uncontracted[gravitonParameters, scalarMomenta, b]; argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b to Infinity." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

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


Clear[T1];

T1[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T1[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        (
I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 b]]]]*
		RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, RuleValidation`RuleRequire[DummyArray2[b]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]] FVD[gravitonParameters[[-1]],\[ScriptM]] FVD[gravitonParameters[[-1]],\[ScriptL]]*
		(
			RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]] - RuleValidation`RuleRequire[GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]]
		) //Contract
        ), True
    ];


Clear[T2];

T2[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T2[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 0, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        (
If[
			Length[gravitonParameters]>=6 ,
			I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 b]]]]*
			RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, RuleValidation`RuleRequire[DummyArray2[b]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]] FVD[gravitonParameters[[-4]], \[ScriptL]1]FVD[gravitonParameters[[-1]], \[ScriptL]2]*
			(
				RuleValidation`RuleRequire[GammaTensor[\[ScriptM],\[ScriptA],\[ScriptR],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]] RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptS],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]] - RuleValidation`RuleRequire[GammaTensor[\[ScriptM],\[ScriptA],\[ScriptB],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]] RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptR],\[ScriptS],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]
			)//Expand//Contract ,
			0
		]
        ), True
    ];


Clear[T3];

T3[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T3[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 0, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        (
If[ b!=0,
			I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] (-b) RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 (b-1)]]]]*
			(
				RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters]] ]] - RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters]] ]]
			) FVD[scalarMomenta[[2b-1]],\[ScriptM]]FVD[scalarMomenta[[2b-1]],\[ScriptN]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]] //Expand//Contract ,
			0
		]
        ), True
    ];


Clear[T4];

T4[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T4[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 0, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        (
If[
			(b!=0)&&(Length[gravitonParameters]>=3),
			-2 (-b) I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 (b-1)]]]]*
			(
				RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]] - RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]]
			) FVD[gravitonParameters[[-1]],\[ScriptL]] RuleValidation`RuleRequire[GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]] FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]] //Expand//Contract,
			0
		]
        ), True
    ];


Clear[T5];

T5[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T5[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 0, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        (
If[
			(b!=0)&&(Length[gravitonParameters]>=6),
			(-b) I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 (b-1)]]]]*
			(
				RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]] - RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]]
			) FVD[gravitonParameters[[-4]],\[ScriptL]1]FVD[gravitonParameters[[-1]],\[ScriptL]2] RuleValidation`RuleRequire[GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]]RuleValidation`RuleRequire[GammaTensor[\[ScriptL],\[ScriptA],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]*
			FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptT]] //Expand//Contract,
			0
		]
        ), True
    ];


Clear[HorndeskiG4Core]

HorndeskiG4Core[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG4Core[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}, {0, "Supported", !(Length[gravitonParameters] >= 6 && b >= 2), "The contracted multi-graviton G4 branch for b >= 2 is not implemented."}},
        (
Switch[ Length[gravitonParameters]/3,
			1,
				Switch[b,
					0, RuleValidation`RuleRequire[T1[gravitonParameters,scalarMomenta,b]] ,
					_, RuleValidation`RuleRequire[T1[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T3[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T4[gravitonParameters,scalarMomenta,b]]
				],
			_,
				Switch[b,
					0, RuleValidation`RuleRequire[T1[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T2[gravitonParameters,scalarMomenta,b]],
					1, RuleValidation`RuleRequire[T1[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T2[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T3[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T4[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T5[gravitonParameters,scalarMomenta,b]]
				]
			]
        ), True
    ];


Clear[HorndeskiG4Core1];

HorndeskiG4Core1[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG4Core1[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}, {0, "Supported", !(Length[gravitonParameters] >= 6 && b >= 2), "The contracted multi-graviton G4 branch for b >= 2 is not implemented."}},
        (
Plus @@ ( RuleValidation`RuleRequire[HorndeskiG4Core[#,scalarMomenta,b]]& /@ ( Flatten /@ Permutations[Partition[gravitonParameters,3]] ) )
        ), True
    ];


Clear[HorndeskiG4];

HorndeskiG4[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG4[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}, {0, "Supported", !(Length[gravitonParameters] >= 6 && b >= 2), "The contracted multi-graviton G4 branch for b >= 2 is not implemented."}},
        (
Switch[b,
			0, RuleValidation`RuleRequire[HorndeskiG4Core1[gravitonParameters,scalarMomenta,b]] //Contract,
			_, Total[RuleValidation`RuleRequire[RuleValidation`RuleRequire[Map[ RuleValidation`RuleRequire[HorndeskiG4Core1[gravitonParameters,#,b]]& ,  Permutations[scalarMomenta] ]]]] //Contract
		]
        ), True
    ];


Clear[HorndeskiG4CoreUncontracted]

HorndeskiG4CoreUncontracted[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG4CoreUncontracted[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        (
Switch[ Length[gravitonParameters]/3,
		1,
			Switch[b,
				0, I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 b]]]] RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, RuleValidation`RuleRequire[DummyArray2[b]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]] FVD[gravitonParameters[[-1]],\[ScriptM]] FVD[gravitonParameters[[-1]],\[ScriptL]] (RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]] - RuleValidation`RuleRequire[GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]]) ,
				_, I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 b]]]] RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, RuleValidation`RuleRequire[DummyArray2[b]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]] FVD[gravitonParameters[[-1]],\[ScriptM]] FVD[gravitonParameters[[-1]],\[ScriptL]] (RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]] - RuleValidation`RuleRequire[GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]]) + I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] b RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 (b-1)]]]] ( RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters]] ]] - RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters]] ]] ) FVD[scalarMomenta[[2b-1]],\[ScriptM]]FVD[scalarMomenta[[2b-1]],\[ScriptN]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]] -2 b I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 (b-1)]]]] ( RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]] - RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]] ) FVD[gravitonParameters[[-1]],\[ScriptL]] RuleValidation`RuleRequire[GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]] FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]]
			],
		_,
			Switch[b,
				0, I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 b]]]] RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, RuleValidation`RuleRequire[DummyArray2[b]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]] FVD[gravitonParameters[[-1]],\[ScriptM]] FVD[gravitonParameters[[-1]],\[ScriptL]] (RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]] - RuleValidation`RuleRequire[GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]]) + I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 b]]]] RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, RuleValidation`RuleRequire[DummyArray2[b]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]] FVD[gravitonParameters[[-4]], \[ScriptL]1]FVD[gravitonParameters[[-1]], \[ScriptL]2] ( RuleValidation`RuleRequire[GammaTensor[\[ScriptM],\[ScriptA],\[ScriptR],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]] RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptS],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]] - RuleValidation`RuleRequire[GammaTensor[\[ScriptM],\[ScriptA],\[ScriptB],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]] RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptR],\[ScriptS],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]),
				_, I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 b]]]] RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, RuleValidation`RuleRequire[DummyArray2[b]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]] FVD[gravitonParameters[[-1]],\[ScriptM]] FVD[gravitonParameters[[-1]],\[ScriptL]] (RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]] - RuleValidation`RuleRequire[GammaTensor[\[ScriptA],\[ScriptB],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]]) + I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 b]]]] RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, RuleValidation`RuleRequire[DummyArray2[b]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]] FVD[gravitonParameters[[-4]], \[ScriptL]1]FVD[gravitonParameters[[-1]], \[ScriptL]2] ( RuleValidation`RuleRequire[GammaTensor[\[ScriptM],\[ScriptA],\[ScriptR],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]] RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptS],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]] - RuleValidation`RuleRequire[GammaTensor[\[ScriptM],\[ScriptA],\[ScriptB],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]] RuleValidation`RuleRequire[GammaTensor[\[ScriptN],\[ScriptR],\[ScriptS],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]) + I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] b RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 (b-1)]]]] ( RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters]] ]] - RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters]] ]] ) FVD[scalarMomenta[[2b-1]],\[ScriptM]]FVD[scalarMomenta[[2b-1]],\[ScriptN]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]] -2 b I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 (b-1)]]]] ( RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]] - RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]] ) FVD[gravitonParameters[[-1]],\[ScriptL]] RuleValidation`RuleRequire[GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL],gravitonParameters[[-3]],gravitonParameters[[-2]]]] FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptA]]FVD[scalarMomenta[[2b]],\[ScriptB]] + b I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 (b-1)]]]] ( RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]] - RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]}, RuleValidation`RuleRequire[DummyArray2[b-1]] ], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]] ) FVD[gravitonParameters[[-4]],\[ScriptL]1]FVD[gravitonParameters[[-1]],\[ScriptL]2] RuleValidation`RuleRequire[GammaTensor[\[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]]RuleValidation`RuleRequire[GammaTensor[\[ScriptL],\[ScriptA],\[ScriptB],\[ScriptL]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]] FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptT]]
			]
		]
        ), True
    ];


Clear[HorndeskiG4Core1Uncontracted];

HorndeskiG4Core1Uncontracted[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG4Core1Uncontracted[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        (
Plus @@ ( RuleValidation`RuleRequire[HorndeskiG4CoreUncontracted[#,scalarMomenta,b]]& /@ ( Flatten /@ Permutations[Partition[gravitonParameters,3]] ) )
        ), True
    ];


Clear[HorndeskiG4Uncontracted];

HorndeskiG4Uncontracted[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG4Uncontracted[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        (
Switch[b,
			0, RuleValidation`RuleRequire[HorndeskiG4Core1Uncontracted[gravitonParameters,scalarMomenta,b]] ,
			_, Total[RuleValidation`RuleRequire[RuleValidation`RuleRequire[Map[ RuleValidation`RuleRequire[HorndeskiG4Core1Uncontracted[gravitonParameters,#,b]]& ,  Permutations[scalarMomenta] ]]]]
		]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
DummyArray2[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[DummyArray2] <> SymbolName[DummyArray2], {arguments}, {1}];

HorndeskiG4[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG4] <> SymbolName[HorndeskiG4], {arguments}, {3}];

HorndeskiG4Core[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG4Core] <> SymbolName[HorndeskiG4Core], {arguments}, {3}];

HorndeskiG4Core1[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG4Core1] <> SymbolName[HorndeskiG4Core1], {arguments}, {3}];

HorndeskiG4Core1Uncontracted[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG4Core1Uncontracted] <> SymbolName[HorndeskiG4Core1Uncontracted], {arguments}, {3}];

HorndeskiG4CoreUncontracted[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG4CoreUncontracted] <> SymbolName[HorndeskiG4CoreUncontracted], {arguments}, {3}];

HorndeskiG4Uncontracted[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG4Uncontracted] <> SymbolName[HorndeskiG4Uncontracted], {arguments}, {3}];

MomentaWrapper[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[MomentaWrapper] <> SymbolName[MomentaWrapper], {arguments}, {1}];

T1[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[T1] <> SymbolName[T1], {arguments}, {3}];

T2[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[T2] <> SymbolName[T2], {arguments}, {3}];

T3[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[T3] <> SymbolName[T3], {arguments}, {3}];

T4[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[T4] <> SymbolName[T4], {arguments}, {3}];

T5[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[T5] <> SymbolName[T5], {arguments}, {3}];

takeIndices[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[takeIndices] <> SymbolName[takeIndices], {arguments}, {1}];

End[];


EndPackage[];
