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

BeginPackage["ScalarGaussBonnet`",{"FeynCalc`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


ScalarGaussBonnet::usage =
"ScalarGaussBonnet[{\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\!\(\*SubscriptBox[\(k\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\),\!\(\*SubscriptBox[\(k\), \(n\)]\)}].";


ScalarGaussBonnet::usage = ScalarGaussBonnet::usage <> " Returns the scalar-Gauss-Bonnet interaction rule for at least two graviton triples. Around a flat background each curvature starts at first order in the graviton perturbation, so the curvature-squared Gauss-Bonnet combination starts at second order. The one-graviton contribution vanishes; it is not a missing interaction rule. The current implementation requires at least six list entries and returns Failure for a one-graviton call rather than returning the known zero.";

(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
ScalarGaussBonnet::usage = ScalarGaussBonnet::usage <> " Supported signatures: ScalarGaussBonnet[gravitonParameters]; argument 1: flat list, block size 3, length 6 to Infinity." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

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


(* The combinations of indices were calculated sepately. *)


Clear[TensorT];

TensorT[m_, n_, a_, b_, r_, s_, l_, t_, secondArray_, indexArray_] :=
    RuleValidation`RuleCall[
        TensorT[m, n, a, b, r, s, l, t, secondArray, indexArray],
        {{9, "Array", 2, 0, Infinity}, {10, "Array", 2, 0, Infinity}, {1, "Expression"}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}, {7, "Expression"}, {8, "Expression"}},
        (
RuleValidation`RuleRequire[CTensorGeneral[Join[{m,a,n,b,s,t,r,l},secondArray],indexArray]] - 4 RuleValidation`RuleRequire[CTensorGeneral[Join[{m,a,n,s,b,t,r,l},secondArray],indexArray]] + RuleValidation`RuleRequire[CTensorGeneral[Join[{m,r,n,s,a,l,b,t},secondArray],indexArray]]
        ), True
    ];


(* TensorT expands the metric factors and accepts only Lorentz-index pairs.
   Remove momenta from the remaining graviton triples before passing them on. *)
Clear[ScalarGaussBonnetCore];

ScalarGaussBonnetCore[gravitonParameters_] :=
    RuleValidation`RuleCall[
        ScalarGaussBonnetCore[gravitonParameters],
        {{1, "Array", 3, 6, Infinity}},
        (
Switch[Length[gravitonParameters]/3,
			2, RuleValidation`RuleRequire[TensorT[scm,scn,sca,scb,scr,scs,scl,sct,{},RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3*2]]]]]] FVD[gravitonParameters[[-1-3]],l1]FVD[gravitonParameters[[-1-3*0]],l2]FVD[gravitonParameters[[-1-3]],scm]FVD[gravitonParameters[[-1-3*0]],scr] RuleValidation`RuleRequire[GammaTensor[sca,scn,scb,l1,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]] RuleValidation`RuleRequire[GammaTensor[scl,scs,sct,l2,gravitonParameters[[-3]],gravitonParameters[[-2]]]] ,
			3, RuleValidation`RuleRequire[TensorT[scm,scn,sca,scb,scr,scs,scl,sct,{},RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3*2]]]]]] FVD[gravitonParameters[[-1-3]],l1]FVD[gravitonParameters[[-1-3*0]],l2]FVD[gravitonParameters[[-1-3]],scm]FVD[gravitonParameters[[-1-3*0]],scr] RuleValidation`RuleRequire[GammaTensor[sca,scn,scb,l1,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]] RuleValidation`RuleRequire[GammaTensor[scl,scs,sct,l2,gravitonParameters[[-3]],gravitonParameters[[-2]]]] + 2 RuleValidation`RuleRequire[TensorT[scm,scn,sca,scb,scr,scs,scl,sct,{i1,j1},RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3*3]]]]]] FVD[gravitonParameters[[-1-3*2]],l1]FVD[gravitonParameters[[-1-3]],l2]FVD[gravitonParameters[[-1]],l3]FVD[gravitonParameters[[-1-3*2]],scm] RuleValidation`RuleRequire[GammaTensor[sca,scn,scb,l1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]] RuleValidation`RuleRequire[GammaTensor[i1,scs,scl,l2,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]] RuleValidation`RuleRequire[GammaTensor[j1,scr,sct,l3,gravitonParameters[[-3]],gravitonParameters[[-2]]]] ,
			_, RuleValidation`RuleRequire[TensorT[scm,scn,sca,scb,scr,scs,scl,sct,{},RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3*2]]]]]] FVD[gravitonParameters[[-1-3]],l1]FVD[gravitonParameters[[-1-3*0]],l2]FVD[gravitonParameters[[-1-3]],scm]FVD[gravitonParameters[[-1-3*0]],scr] RuleValidation`RuleRequire[GammaTensor[sca,scn,scb,l1,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]] RuleValidation`RuleRequire[GammaTensor[scl,scs,sct,l2,gravitonParameters[[-3]],gravitonParameters[[-2]]]] + 2 RuleValidation`RuleRequire[TensorT[scm,scn,sca,scb,scr,scs,scl,sct,{i1,j1},RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3*3]]]]]] FVD[gravitonParameters[[-1-3*2]],l1]FVD[gravitonParameters[[-1-3]],l2]FVD[gravitonParameters[[-1]],l3]FVD[gravitonParameters[[-1-3*2]],scm] RuleValidation`RuleRequire[GammaTensor[sca,scn,scb,l1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]] RuleValidation`RuleRequire[GammaTensor[i1,scs,scl,l2,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]] RuleValidation`RuleRequire[GammaTensor[j1,scr,sct,l3,gravitonParameters[[-3]],gravitonParameters[[-2]]]] + RuleValidation`RuleRequire[TensorT[scm,scn,sca,scb,scr,scs,scl,sct,{i1,j1,i2,j2},RuleValidation`RuleRequire[takeIndices[gravitonParameters[[;;-1-3*4]]]]]] FVD[gravitonParameters[[-1-3*3]],l1] FVD[gravitonParameters[[-1-3*2]],l2] FVD[gravitonParameters[[-1-3]],l3] FVD[gravitonParameters[[-1]],l4] RuleValidation`RuleRequire[GammaTensor[i,scn,sca,l1,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*3]]]] RuleValidation`RuleRequire[GammaTensor[j1,scm,scb,l2,gravitonParameters[[-3-3*2]],gravitonParameters[[-2-3*2]]]] RuleValidation`RuleRequire[GammaTensor[i2,scs,scl,l3,gravitonParameters[[-3-3]],gravitonParameters[[-2-3]]]] RuleValidation`RuleRequire[GammaTensor[j2,scr,sct,gravitonParameters[[-3]],gravitonParameters[[-2]]]]
		]
        ), True
    ];


Clear[ScalarGaussBonnet];

ScalarGaussBonnet[gravitonParameters_] :=
    RuleValidation`RuleCall[
        ScalarGaussBonnet[gravitonParameters],
        {{1, "Array", 3, 6, Infinity}},
        (
I Power[Global`\[Kappa],Length[gravitonParameters]/3] 4*
		Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				ScalarGaussBonnetCore ,
				Flatten/@Permutations[Partition[gravitonParameters,3]]
			]]
		]]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
DummyArray2[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[DummyArray2] <> SymbolName[DummyArray2], {arguments}, {1}];

MomentaWrapper[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[MomentaWrapper] <> SymbolName[MomentaWrapper], {arguments}, {1}];

ScalarGaussBonnet[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[ScalarGaussBonnet] <> SymbolName[ScalarGaussBonnet], {arguments}, {1}];

ScalarGaussBonnetCore[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[ScalarGaussBonnetCore] <> SymbolName[ScalarGaussBonnetCore], {arguments}, {1}];

TensorT[arguments___] /; !MemberQ[{10}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[TensorT] <> SymbolName[TensorT], {arguments}, {10}];

takeIndices[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[takeIndices] <> SymbolName[takeIndices], {arguments}, {1}];

End[];


EndPackage[];
