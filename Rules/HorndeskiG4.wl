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
    "HorndeskiG4[gravitonParameters, scalarMomenta, b] returns the contracted " <>
    "vertex of phi^a (X^b R - 2 b X^(b-1) H2), with unit coupling, " <>
    "X = (partial phi)^2 and H2 = (Box phi)^2 - H_mu_nu H^mu_nu. " <>
    "gravitonParameters is a flat list of n triples {mu, nu, k}, n >= 1; " <>
    "scalarMomenta is a flat list of a + 2 b incoming momenta, a >= 0. " <>
    "b must be an explicit integer: 0 <= b <= 5 for n = 1, " <>
    "and 0 <= b <= 4 for n >= 2. These are dependency bounds, not runtime guarantees. " <>
    "Successful contracted results are memoised. Invalid arguments or unsupported " <>
    "configurations return Failure; dependency failures propagate. " <>
    "See Rules/Contracts.md for the source-rule contract.";

HorndeskiG4Uncontracted::usage =
    "HorndeskiG4Uncontracted[gravitonParameters, scalarMomenta, b] returns the " <>
    "same vertex before final Lorentz contraction, with fresh internal indices " <>
    "on every call. It has the same input and dependency bounds as HorndeskiG4. " <>
    "Raw results are deliberately not memoised, so independent calls do not " <>
    "share dummy indices. Invalid arguments or unsupported configurations " <>
    "return Failure; dependency failures propagate.";

Begin["`Private`"];

(* Keep labelled external legs distinct even when their values coincide. *)
Clear[labelledPermutations];
labelledPermutations[legs_List] := legs[[#]] & /@ Permutations[Range[Length[legs]]];

(* MomentaWrapper and DummyArray2 use the same index list within one raw
   kernel. Each Tj below owns a fresh Block; no raw kernel is cached. *)
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
(* Retain the historical fixed-name fallback for direct private-helper calls.
   Public vertex construction always supplies a fresh per-kernel list. *)
Clear[DummyArray2];
DummyArray2[n_] :=
    RuleValidation`RuleCall[
        DummyArray2[n],
        {{1, "Integer", 0}},
        (
            If[ListQ[$scalarDummyIndices], Take[$scalarDummyIndices, 2 n],
                Flatten[RuleValidation`RuleRequire[
                        Table[
                            {
                                Symbol["\[ScriptA]" <> ToString[i]],
                                Symbol["\[ScriptB]" <> ToString[i]]
                            },
                            {i, n}
                        ]
                ]]
            ]
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

(* T1: Curvature: one differentiated connection. *)
Clear[T1Uncontracted];
T1Uncontracted[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T1Uncontracted[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 3, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        Module[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],
                \[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT],\[ScriptL]1,\[ScriptL]2},
            Block[{$scalarDummyIndices=Table[Unique["g4Scalar$"],{2 b}]},
                I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1]
                RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 b]]]]*
                RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]},
                            RuleValidation`RuleRequire[DummyArray2[b]] ],
                        RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]]
                FVD[gravitonParameters[[-1]],\[ScriptM]]
                FVD[gravitonParameters[[-1]],\[ScriptL]]*
                (
                    RuleValidation`RuleRequire[GammaTensor[
                            \[ScriptN],\[ScriptA],\[ScriptB],\[ScriptL],
                            gravitonParameters[[-3]],gravitonParameters[[-2]]
                    ]] -
                    RuleValidation`RuleRequire[GammaTensor[
                            \[ScriptA],\[ScriptB],\[ScriptN],\[ScriptL],
                            gravitonParameters[[-3]],gravitonParameters[[-2]]
                    ]]
                )
        ]], False
    ];

(* T2: Curvature: two connections; absent for fewer than two gravitons. *)
Clear[T2Uncontracted];
T2Uncontracted[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T2Uncontracted[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 0, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        Module[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],
                \[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT],\[ScriptL]1,\[ScriptL]2},
            Block[{$scalarDummyIndices=Table[Unique["g4Scalar$"],{2 b}]},
                If[
                    Length[gravitonParameters]>=6 ,
                    I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1]
                    RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 b]]]]*
                    RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]},
                                RuleValidation`RuleRequire[DummyArray2[b]] ],
                            RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]]
                    FVD[gravitonParameters[[-4]], \[ScriptL]1]
                    FVD[gravitonParameters[[-1]], \[ScriptL]2]*
                    (
                        RuleValidation`RuleRequire[GammaTensor[
                                \[ScriptM],\[ScriptA],\[ScriptR],\[ScriptL]1,
                                gravitonParameters[[-6]],gravitonParameters[[-5]]
                        ]]
                        RuleValidation`RuleRequire[GammaTensor[
                                \[ScriptN],\[ScriptS],\[ScriptB],\[ScriptL]2,
                                gravitonParameters[[-3]],gravitonParameters[[-2]]
                        ]] -
                        RuleValidation`RuleRequire[GammaTensor[
                                \[ScriptM],\[ScriptA],\[ScriptB],\[ScriptL]1,
                                gravitonParameters[[-6]],gravitonParameters[[-5]]
                        ]]
                        RuleValidation`RuleRequire[GammaTensor[
                                \[ScriptN],\[ScriptR],\[ScriptS],\[ScriptL]2,
                                gravitonParameters[[-3]],gravitonParameters[[-2]]
                        ]]
                    ) ,
                    0
                ]
        ]], False
    ];

(* T3: Hessian square: no connections, coefficient -2 b. *)
Clear[T3Uncontracted];
T3Uncontracted[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T3Uncontracted[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 0, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        Module[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],
                \[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT],\[ScriptL]1,\[ScriptL]2},
            Block[{$scalarDummyIndices=Table[Unique["g4Scalar$"],{2 b}]},
                If[ b!=0,
                    I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1] (-2 b)
                    RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 (b-1)]]]]*
                    (
                        RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB]},
                                    RuleValidation`RuleRequire[DummyArray2[b-1]] ],
                                RuleValidation`RuleRequire[takeIndices[gravitonParameters]] ]] -
                        RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB]},
                                    RuleValidation`RuleRequire[DummyArray2[b-1]] ],
                                RuleValidation`RuleRequire[takeIndices[gravitonParameters]] ]]
                    )
                    FVD[scalarMomenta[[2b-1]],\[ScriptM]]
                    FVD[scalarMomenta[[2b-1]],\[ScriptN]]
                    FVD[scalarMomenta[[2b]],\[ScriptA]]
                    FVD[scalarMomenta[[2b]],\[ScriptB]]  ,
                    0
                ]
        ]], False
    ];

(* T4: Hessian square: one connection, coefficient +4 b. *)
Clear[T4Uncontracted];
T4Uncontracted[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T4Uncontracted[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 0, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        Module[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],
                \[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT],\[ScriptL]1,\[ScriptL]2},
            Block[{$scalarDummyIndices=Table[Unique["g4Scalar$"],{2 b}]},
                If[
                    (b!=0)&&(Length[gravitonParameters]>=3),
                    4 b I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1]
                    RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 (b-1)]]]]*
                    (
                        RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS]},
                                    RuleValidation`RuleRequire[DummyArray2[b-1]] ],
                                RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]] -
                        RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS]},
                                    RuleValidation`RuleRequire[DummyArray2[b-1]] ],
                                RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3-1]]]] ]]
                    )
                    FVD[gravitonParameters[[-1]],\[ScriptL]]
                    RuleValidation`RuleRequire[GammaTensor[
                            \[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL],
                            gravitonParameters[[-3]],gravitonParameters[[-2]]
                    ]]
                    FVD[scalarMomenta[[2b-1]],\[ScriptS]]
                    FVD[scalarMomenta[[2b]],\[ScriptA]]
                    FVD[scalarMomenta[[2b]],\[ScriptB]] ,
                    0
                ]
        ]], False
    ];

(* T5: Hessian square: two connections, coefficient -2 b. *)
Clear[T5Uncontracted];
T5Uncontracted[gravitonParameters_, scalarMomenta_, b_] :=
    RuleValidation`RuleCall[
        T5Uncontracted[gravitonParameters, scalarMomenta, b],
        {{1, "Array", 3, 0, Infinity}, {3, "Integer", 0}, {2, "Array", 1, 2 b, Infinity}},
        Module[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],
                \[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT],\[ScriptL]1,\[ScriptL]2},
            Block[{$scalarDummyIndices=Table[Unique["g4Scalar$"],{2 b}]},
                If[
                    (b!=0)&&(Length[gravitonParameters]>=6),
                    -2 b I Power[Global`\[Kappa], Length[gravitonParameters]/3] Power[-1,b+1]
                    RuleValidation`RuleRequire[MomentaWrapper[scalarMomenta[[;;2 (b-1)]]]]*
                    (
                        RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptN],\[ScriptA],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]},
                                    RuleValidation`RuleRequire[DummyArray2[b-1]] ],
                                RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]] -
                        RuleValidation`RuleRequire[CTensorGeneral[ Join[{\[ScriptM],\[ScriptA],\[ScriptN],\[ScriptB],\[ScriptR],\[ScriptS],\[ScriptL],\[ScriptT]},
                                    RuleValidation`RuleRequire[DummyArray2[b-1]] ],
                                RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-3 2-1]]]] ]]
                    )
                    FVD[gravitonParameters[[-4]],\[ScriptL]1]
                    FVD[gravitonParameters[[-1]],\[ScriptL]2]
                    RuleValidation`RuleRequire[GammaTensor[
                            \[ScriptR],\[ScriptM],\[ScriptN],\[ScriptL]1,
                            gravitonParameters[[-6]],gravitonParameters[[-5]]
                    ]]
                    RuleValidation`RuleRequire[GammaTensor[
                            \[ScriptL],\[ScriptA],\[ScriptB],\[ScriptL]2,
                            gravitonParameters[[-3]],gravitonParameters[[-2]]
                    ]]*
                    FVD[scalarMomenta[[2b-1]],\[ScriptS]]FVD[scalarMomenta[[2b]],\[ScriptT]] ,
                    0
                ]
        ]], False
    ];

(* The differentiated monomial is phi^a (X^b R - 2 b X^(b-1) H2).
   T1/T2 are curvature; T3/T4/T5 have zero/one/two connections.
   Derivatives are momentum-stripped here: I (-1)^(b+1) occurs only once. *)
Clear[HorndeskiG4CoreUncontracted, HorndeskiG4Core1Uncontracted];
HorndeskiG4CoreUncontracted[g_, p_, b_] := RuleValidation`RuleCall[
    HorndeskiG4CoreUncontracted[g,p,b],
    {{1,"Array",3,3,Infinity},{3,"Integer",0},{2,"Array",1,2 b,Infinity}},
    Total[RuleValidation`RuleRequire[#]& /@ {
            T1Uncontracted[g,p,b], T2Uncontracted[g,p,b],
            T3Uncontracted[g,p,b], T4Uncontracted[g,p,b], T5Uncontracted[g,p,b]}], False];
HorndeskiG4Core1Uncontracted[g_,p_,b_] := RuleValidation`RuleCall[
    HorndeskiG4Core1Uncontracted[g,p,b],
    {{1,"Array",3,3,Infinity},{3,"Integer",0},{2,"Array",1,2 b,Infinity}},
    Total[RuleValidation`RuleRequire[HorndeskiG4CoreUncontracted[Flatten[#],p,b]]& /@
        labelledPermutations[Partition[g,3]]], False];

(* Seven inverse-metric pairs are available: T1/T4 need b+2 at n=1;
   T2/T5 need b+3 at n>=2. Reject excess b before generating permutations.
   At b=0 all scalars are undifferentiated, giving the explicit a! factor. *)
Clear[HorndeskiG4Uncontracted];
HorndeskiG4Uncontracted[g_,p_,b_] := RuleValidation`RuleCall[
    HorndeskiG4Uncontracted[g,p,b],
    {{1,"Array",3,3,Infinity},{3,"Integer",0},{2,"Array",1,2 b,Infinity},
        {0,"Supported",b <= If[Length[g]===3,5,4],
            "The G4 kernels require at most seven inverse-metric pairs: b <= 5 for one graviton, b <= 4 otherwise."}},
    If[b===0,
        Factorial[Length[p]] RuleValidation`RuleRequire[HorndeskiG4Core1Uncontracted[g,p,b]],
        Total[RuleValidation`RuleRequire[HorndeskiG4Core1Uncontracted[g,#,b]]& /@
            labelledPermutations[p]]], False];

(* Raw kernels are not memoised: every returned expression owns fresh dummies.
   Contract each canonical kernel before the linear labelled sums. Only these
   dummy-free results are cached; contraction commutes with external relabelling. *)
Clear[HorndeskiG4Core,HorndeskiG4Core1,T1,T2,T3,T4,T5];
T1[g_,p_,b_] := RuleValidation`RuleCall[T1[g,p,b],
    {{1,"Array",3,3,Infinity},{3,"Integer",0},{2,"Array",1,2 b,Infinity}},
    Contract[RuleValidation`RuleRequire[T1Uncontracted[g,p,b]]], True];
T2[g_,p_,b_] := RuleValidation`RuleCall[T2[g,p,b],
    {{1,"Array",3,3,Infinity},{3,"Integer",0},{2,"Array",1,2 b,Infinity}},
    Contract[RuleValidation`RuleRequire[T2Uncontracted[g,p,b]]], True];
T3[g_,p_,b_] := RuleValidation`RuleCall[T3[g,p,b],
    {{1,"Array",3,3,Infinity},{3,"Integer",0},{2,"Array",1,2 b,Infinity}},
    Contract[RuleValidation`RuleRequire[T3Uncontracted[g,p,b]]], True];
T4[g_,p_,b_] := RuleValidation`RuleCall[T4[g,p,b],
    {{1,"Array",3,3,Infinity},{3,"Integer",0},{2,"Array",1,2 b,Infinity}},
    Contract[RuleValidation`RuleRequire[T4Uncontracted[g,p,b]]], True];
T5[g_,p_,b_] := RuleValidation`RuleCall[T5[g,p,b],
    {{1,"Array",3,3,Infinity},{3,"Integer",0},{2,"Array",1,2 b,Infinity}},
    Contract[RuleValidation`RuleRequire[T5Uncontracted[g,p,b]]], True];
HorndeskiG4Core[g_,p_,b_] := RuleValidation`RuleCall[HorndeskiG4Core[g,p,b],
    {{1,"Array",3,3,Infinity},{3,"Integer",0},{2,"Array",1,2 b,Infinity}},
    Total[RuleValidation`RuleRequire[#]& /@ {T1[g,p,b],T2[g,p,b],T3[g,p,b],T4[g,p,b],T5[g,p,b]}], True];
HorndeskiG4Core1[g_,p_,b_] := RuleValidation`RuleCall[HorndeskiG4Core1[g,p,b],
    {{1,"Array",3,3,Infinity},{3,"Integer",0},{2,"Array",1,2 b,Infinity}},
    Total[RuleValidation`RuleRequire[HorndeskiG4Core[Flatten[#],p,b]]& /@
        labelledPermutations[Partition[g,3]]], True];
Clear[HorndeskiG4];
HorndeskiG4[g_,p_,b_] := RuleValidation`RuleCall[HorndeskiG4[g,p,b],
    {{1,"Array",3,3,Infinity},{3,"Integer",0},{2,"Array",1,2 b,Infinity},
        {0,"Supported",b <= If[Length[g]===3,5,4],
            "The G4 kernels require at most seven inverse-metric pairs: b <= 5 for one graviton, b <= 4 otherwise."}},
    If[b===0,Factorial[Length[p]] RuleValidation`RuleRequire[HorndeskiG4Core1[g,p,b]],
        Total[RuleValidation`RuleRequire[HorndeskiG4Core1[g,#,b]]& /@labelledPermutations[p]]], True];

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
