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

BeginPackage["HorndeskiG5`",{"FeynCalc`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


HorndeskiG5::usage = "";


HorndeskiG5Uncontracted::usage = "";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
HorndeskiG5::usage = HorndeskiG5::usage <> " Supported signatures: HorndeskiG5[gravitonParameters, scalarMomenta, b]; argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity; The G5 TII dispatcher is implemented only through three gravitons. Condition: Length[gravitonParameters] <= 9 || b == 0.." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
HorndeskiG5Uncontracted::usage = HorndeskiG5Uncontracted::usage <> " Supported signatures: HorndeskiG5Uncontracted[gravitonParameters, scalarMomenta, b]; argument 1: flat list, block size 3, length 3 to Infinity; argument 3: explicit integer >= 0; argument 2: flat list, block size 1, length 2 b + 1 to Infinity; The G5 TII dispatcher is implemented only through three gravitons. Condition: Length[gravitonParameters] <= 9 || b == 0.." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

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
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2b]]]] FVD[scalarMomenta[[2b+1]], \[Alpha]] FVD[scalarMomenta[[2b+1]], \[Beta]] FVD[gravitonParameters[[-1]], \[Lambda]]*
		(
			RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma]},RuleValidation`RuleRequire[DummyArray2[b]]] , RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]]
			+ RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Beta],\[Nu],\[Alpha],\[Rho],\[Sigma]},RuleValidation`RuleRequire[DummyArray2[b]]] , RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]]
			- RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},RuleValidation`RuleRequire[DummyArray2[b]]] , RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]]
		)*
		(
			FVD[gravitonParameters[[-1]],\[Rho]] RuleValidation`RuleRequire[GammaTensor[\[Sigma],\[Mu],\[Nu],\[Lambda],gravitonParameters[[-3]],gravitonParameters[[-2]]]]
			- FVD[gravitonParameters[[-1]],\[Mu]] RuleValidation`RuleRequire[GammaTensor[\[Rho],\[Sigma],\[Nu],\[Lambda],gravitonParameters[[-3]],gravitonParameters[[-2]]]]
		)
        ), True
    ];


Clear[T2];

T2[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T2[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 6, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
(-1) RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2b]]]] FVD[scalarMomenta[[2b+1]], \[Tau]] FVD[gravitonParameters[[-1-3]], \[Lambda]1] FVD[gravitonParameters[[-1]], \[Lambda]2]*
		(
			RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},RuleValidation`RuleRequire[DummyArray2[b]]] , RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]]
			+ RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Beta],\[Nu],\[Alpha],\[Rho],\[Sigma],\[Lambda],\[Tau]},RuleValidation`RuleRequire[DummyArray2[b]]] , RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]]
			- RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},RuleValidation`RuleRequire[DummyArray2[b]]] , RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]]
		)*
		(
			FVD[gravitonParameters[[-1-3]],\[Rho]] RuleValidation`RuleRequire[GammaTensor[\[Sigma],\[Mu],\[Nu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]]
			- FVD[gravitonParameters[[-1-3]],\[Mu]] RuleValidation`RuleRequire[GammaTensor[\[Rho],\[Sigma],\[Nu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]]
		) RuleValidation`RuleRequire[GammaTensor[\[Lambda],\[Alpha],\[Beta],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]
        ), True
    ];


Clear[T3];

T3[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T3[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 6, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2b]]]] FVD[gravitonParameters[[-1-3]], \[Lambda]1] FVD[gravitonParameters[[-1]], \[Lambda]2] FVD[scalarMomenta[[2b+1]], \[Alpha]] FVD[scalarMomenta[[2b+1]], \[Beta]]*
		(
			RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},RuleValidation`RuleRequire[DummyArray2[b]]] , RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]]
			+ RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Beta],\[Nu],\[Alpha],\[Rho],\[Sigma],\[Lambda],\[Tau]},RuleValidation`RuleRequire[DummyArray2[b]]] , RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]]
			- RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau]},RuleValidation`RuleRequire[DummyArray2[b]]] , RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]]
		)*
		(
			RuleValidation`RuleRequire[GammaTensor[\[Rho],\[Lambda],\[Mu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]] RuleValidation`RuleRequire[GammaTensor[\[Sigma],\[Tau],\[Nu],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]
			- RuleValidation`RuleRequire[GammaTensor[\[Rho],\[Lambda],\[Tau],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]]RuleValidation`RuleRequire[GammaTensor[\[Sigma],\[Mu],\[Nu],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]
		)
        ), True
    ];


Clear[T4];

T4[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T4[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 9, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
(-1) RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2b]]]] FVD[gravitonParameters[[-1-3 2]] , \[Lambda]3] FVD[gravitonParameters[[-1-3]], \[Lambda]1] FVD[gravitonParameters[[-1]], \[Lambda]2] FVD[ scalarMomenta[[2b+1]], \[Epsilon]]*
		(
			RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Alpha],\[Nu],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau],\[Omega],\[Epsilon]},RuleValidation`RuleRequire[DummyArray2[b]]] , RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 3-1]]]] ]]
			+ RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Beta],\[Nu],\[Alpha],\[Rho],\[Sigma],\[Lambda],\[Tau],\[Omega],\[Epsilon]},RuleValidation`RuleRequire[DummyArray2[b]]] , RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 3-1]]]] ]]
			- RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Lambda],\[Tau],\[Omega],\[Epsilon]},RuleValidation`RuleRequire[DummyArray2[b]]] , RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 3-1]]]] ]]
		) RuleValidation`RuleRequire[GammaTensor[\[Omega],\[Alpha],\[Beta],\[Lambda]3,gravitonParameters[[-9]],gravitonParameters[[-8]]]]*
		(
			RuleValidation`RuleRequire[GammaTensor[\[Rho],\[Lambda],\[Mu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]] RuleValidation`RuleRequire[GammaTensor[\[Sigma],\[Tau],\[Nu],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]
			- RuleValidation`RuleRequire[GammaTensor[\[Rho],\[Lambda],\[Tau],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]]RuleValidation`RuleRequire[GammaTensor[\[Sigma],\[Mu],\[Nu],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]
		)
        ), True
    ];


Clear[TI];

TI[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        TI[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
Switch[Length[gravitonParameters]/3,
			1 , RuleValidation`RuleRequire[T1[gravitonParameters,scalarMomenta,b]],
			2 , RuleValidation`RuleRequire[T1[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T2[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T3[gravitonParameters,scalarMomenta,b]],
			_ ,  RuleValidation`RuleRequire[T1[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T2[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T3[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T4[gravitonParameters,scalarMomenta,b]]
		]
        ), True
    ];


Clear[T5];

T5[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T5[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 0, Infinity}, {3, "Integer", 1}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
(-1/3) RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2(b-1)]]]] FVD[scalarMomenta[[2b-1]],\[Mu]]FVD[scalarMomenta[[2b-1]],\[Nu]]FVD[scalarMomenta[[2b]],\[Alpha]]FVD[scalarMomenta[[2b]],\[Beta]]FVD[scalarMomenta[[2b+1]],\[Rho]]FVD[scalarMomenta[[2b+1]],\[Sigma]]*
		(
			RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]}, RuleValidation`RuleRequire[DummyArray2[b-1]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters]]]]
			- 3 RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Rho],\[Beta],\[Sigma]}, RuleValidation`RuleRequire[DummyArray2[b-1]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters]]]]
			+ 2 RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Mu]}, RuleValidation`RuleRequire[DummyArray2[b-1]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters]]]]
		)
        ), True
    ];


Clear[T6];

T6[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T6[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 1}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2(b-1)]]]] FVD[scalarMomenta[[2b-1]],\[Tau]]FVD[scalarMomenta[[2b]],\[Alpha]]FVD[gravitonParameters[[-1]],\[Lambda]]FVD[scalarMomenta[[2b]],\[Beta]]*
		FVD[scalarMomenta[[2b+1]],\[Rho]]FVD[scalarMomenta[[2b+1]],\[Sigma]]*
		RuleValidation`RuleRequire[GammaTensor[\[Omega],\[Mu],\[Nu],\[Lambda],gravitonParameters[[-3]],gravitonParameters[[-2]]]]*
		(
			RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Omega],\[Tau]}, RuleValidation`RuleRequire[DummyArray2[b-1]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3]]]]]]
			- 3 RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Rho],\[Beta],\[Sigma],\[Omega],\[Tau]}, RuleValidation`RuleRequire[DummyArray2[b-1]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3]]]]]]
			+ 2 RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Mu],\[Omega],\[Tau]}, RuleValidation`RuleRequire[DummyArray2[b-1]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3]]]]]]
		)
        ), True
    ];


Clear[T7];

T7[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T7[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 6, Infinity}, {3, "Integer", 1}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
(-1) RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2(b-1)]]]] FVD[scalarMomenta[[2b-1]],\[Tau]1]FVD[scalarMomenta[[2b]],\[Tau]2]FVD[scalarMomenta[[2b+1]],\[Rho]]FVD[scalarMomenta[[2b+1]],\[Sigma]]*
		FVD[gravitonParameters[[-1-3]],\[Lambda]1]FVD[gravitonParameters[[-1]],\[Lambda]2]*
		RuleValidation`RuleRequire[GammaTensor[\[Omega]1,\[Mu],\[Nu],\[Lambda]1,gravitonParameters[[-6]],gravitonParameters[[-5]]]] RuleValidation`RuleRequire[GammaTensor[\[Omega]2,\[Alpha],\[Beta],\[Lambda]2,gravitonParameters[[-3]],gravitonParameters[[-2]]]]*
		(
			RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Omega]1,\[Tau]1,\[Omega]2,\[Tau]2}, RuleValidation`RuleRequire[DummyArray2[b-1]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3 2]]]]]]
			- 3 RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Rho],\[Beta],\[Sigma],\[Omega]1,\[Tau]1,\[Omega]2,\[Tau]2}, RuleValidation`RuleRequire[DummyArray2[b-1]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3 2]]]]]]
			+ 2 RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Mu],\[Omega]1,\[Tau]1,\[Omega]2,\[Tau]2}, RuleValidation`RuleRequire[DummyArray2[b-1]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3 2]]]]]]
		)
        ), True
    ];


Clear[T8];

T8[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T8[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 9, Infinity}, {3, "Integer", 1}, {2, "Array", 1, 2 b + 1, Infinity}},
        (
(1/3) RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2(b-1)]]]] FVD[scalarMomenta[[2b-1]],\[Tau]1]FVD[scalarMomenta[[2b]],\[Tau]2]FVD[scalarMomenta[[2b+1]],\[Tau]3]*
		FVD[gravitonParameters[[-1-3 2]],\[Lambda]1]FVD[gravitonParameters[[-1-3]],\[Lambda]2]FVD[gravitonParameters[[-1]],\[Lambda]2]*
		RuleValidation`RuleRequire[GammaTensor[\[Omega]1,\[Mu],\[Nu],\[Lambda]1,gravitonParameters[[-9]],gravitonParameters[[-8]]]] RuleValidation`RuleRequire[GammaTensor[\[Omega]2,\[Alpha],\[Beta],\[Lambda]2,gravitonParameters[[-6]],gravitonParameters[[-5]]]]*
		RuleValidation`RuleRequire[GammaTensor[\[Omega]3,\[Rho],\[Sigma],\[Lambda]3,gravitonParameters[[-3]],gravitonParameters[[-2]]]]*
		(
			RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Omega]1,\[Tau]1,\[Omega]2,\[Tau]2,\[Omega]3,\[Tau]3}, RuleValidation`RuleRequire[DummyArray2[b-1]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3 3]]]]]]
			- 3 RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Mu],\[Nu],\[Alpha],\[Rho],\[Beta],\[Sigma],\[Omega]1,\[Tau]1,\[Omega]2,\[Tau]2,\[Omega]3,\[Tau]3}, RuleValidation`RuleRequire[DummyArray2[b-1]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3 3]]]]]]
			+ 2 RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma],\[Mu],\[Omega]1,\[Tau]1,\[Omega]2,\[Tau]2,\[Omega]3,\[Tau]3}, RuleValidation`RuleRequire[DummyArray2[b-1]]], RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3 3]]]]]]
		)
        ), True
    ];


Clear[TII];

TII[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        TII[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 0, Infinity}, {3, "Integer", 1}, {2, "Array", 1, 2 b + 1, Infinity}, {0, "Supported", Length[gravitonParameters] <= 9, "The G5 TII dispatcher is implemented only through three gravitons."}},
        (
Switch[ Length[gravitonParameters]/3,
			0, RuleValidation`RuleRequire[T5[gravitonParameters,scalarMomenta,b]],
			1, RuleValidation`RuleRequire[T5[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T6[gravitonParameters,scalarMomenta,b]],
			2, RuleValidation`RuleRequire[T5[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T6[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T7[gravitonParameters,scalarMomenta,b]],
			3, RuleValidation`RuleRequire[T5[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T6[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T7[gravitonParameters,scalarMomenta,b]] + RuleValidation`RuleRequire[T8[gravitonParameters,scalarMomenta,b]]
		]
        ), True
    ];


Clear[HorndeskiG5];

HorndeskiG5[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG5[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b + 1, Infinity}, {0, "Supported", Length[gravitonParameters] <= 9 || b == 0, "The G5 TII dispatcher is implemented only through three gravitons."}},
        (
Switch[b,
			0, I/2 Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+2] Total[RuleValidation`RuleRequire[ TI@@@Tuples[{ Flatten/@ Permutations[ Partition[gravitonParameters,3]], Permutations[scalarMomenta] , {b}}] ]] //Expand//Contract,
			_, I/2 Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+2] Total[RuleValidation`RuleRequire[ TI@@@Tuples[{ Flatten/@ Permutations[ Partition[gravitonParameters,3]], Permutations[scalarMomenta] , {b}}] ]] + I/2 Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+2] (-b) Total[RuleValidation`RuleRequire[ TII@@@Tuples[{ Flatten/@ Permutations[ Partition[gravitonParameters,3]], Permutations[scalarMomenta] , {b}}] ]]//Expand//Contract
		]
        ), True
    ];


Clear[HorndeskiG5Uncontracted];

HorndeskiG5Uncontracted[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        HorndeskiG5Uncontracted[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b + 1, Infinity}, {0, "Supported", Length[gravitonParameters] <= 9 || b == 0, "The G5 TII dispatcher is implemented only through three gravitons."}},
        (
Switch[b,
			0, I/2 Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+2] Total[RuleValidation`RuleRequire[ TI@@@Tuples[{ Flatten/@ Permutations[ Partition[gravitonParameters,3]], Permutations[scalarMomenta] , {b}}] ]],
			_, I/2 Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+2] Total[RuleValidation`RuleRequire[ TI@@@Tuples[{ Flatten/@ Permutations[ Partition[gravitonParameters,3]], Permutations[scalarMomenta] , {b}}] ]] + I/2 Power[Global`\[Kappa],Length[gravitonParameters]/3] Power[-1,b+2] (-b) Total[RuleValidation`RuleRequire[ TII@@@Tuples[{ Flatten/@ Permutations[ Partition[gravitonParameters,3]], Permutations[scalarMomenta] , {b}}] ]]
		]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
DummyArray2[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[DummyArray2] <> SymbolName[DummyArray2], {arguments}, {1}];

HorndeskiG5[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG5] <> SymbolName[HorndeskiG5], {arguments}, {3}];

HorndeskiG5Uncontracted[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[HorndeskiG5Uncontracted] <> SymbolName[HorndeskiG5Uncontracted], {arguments}, {3}];

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

T6[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[T6] <> SymbolName[T6], {arguments}, {3}];

T7[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[T7] <> SymbolName[T7], {arguments}, {3}];

T8[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[T8] <> SymbolName[T8], {arguments}, {3}];

TI[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[TI] <> SymbolName[TI], {arguments}, {3}];

TII[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[TII] <> SymbolName[TII], {arguments}, {3}];

takeIndices[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[takeIndices] <> SymbolName[takeIndices], {arguments}, {1}];

End[];


EndPackage[];
