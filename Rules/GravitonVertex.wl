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

BeginPackage["GravitonVertex`",{"FeynCalc`","ITensor`","CTensorGeneral`","GammaTensor`","indexArraySymmetrization`"}];


GravitonVertex::usage =
"GravitonVertex[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(n\)]\)}]. \
The expression for a vertex describing a coupling of n gravitons within general relativity. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a graviton.";


GravitonVertexUncontracted::usage =
"GravitonVertexUncontracted[{\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\!\(\*SubscriptBox[\(p\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Mu]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Nu]\), \(n\)]\),\!\(\*SubscriptBox[\(p\), \(n\)]\)}]. \
The expression for a vertex describing a coupling of n gravitons within general relativity. Here {\!\(\*SubscriptBox[\(\[Rho]\), \(i\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)} are Lorentz index of a graviton; \!\(\*SubscriptBox[\(p\), \(i\)]\) is the momentum of a graviton. No contraction or simplification is carried out.";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
GravitonVertex::usage = GravitonVertex::usage <> " Supported signatures: GravitonVertex[indexArray]; argument 1: flat list, block size 3, length 6 to Infinity." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GravitonVertexUncontracted::usage = GravitonVertexUncontracted::usage <> " Supported signatures: GravitonVertexUncontracted[indexArray]; argument 1: flat list, block size 3, length 6 to Infinity." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

Begin["`Private`"];


(* Supplementary functions. *)


Clear[TakeLorenzIndices];

TakeLorenzIndices[indexArray_] :=
    RuleValidation`RuleCall[
        TakeLorenzIndices[indexArray],
        {{1, "Array", 3, 0, Infinity}},
        (
Flatten[RuleValidation`RuleRequire[(#[[;;2]]&)/@Partition[indexArray,3]]]
        ), False
    ];


(* Graviton Vertex *)


Clear[GravitonVertex];

GravitonVertex[indexArray_] :=
    RuleValidation`RuleCall[
        GravitonVertex[indexArray],
        {{1, "Array", 3, 6, Infinity}},
        (
Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				( I (Global`\[Kappa])^(Length[#]/3-2) RuleValidation`RuleRequire[CTensorGeneral[{\[Mu],\[Nu],\[Alpha],\[Beta],\[Rho],\[Sigma]},RuleValidation`RuleRequire[TakeLorenzIndices[#[[7;;]]]]]]FVD[#[[3]],\[Lambda]1]FVD[#[[6]],\[Lambda]2] (2 RuleValidation`RuleRequire[GammaTensor[\[Alpha],\[Mu],\[Rho],\[Lambda]1,#[[1]],#[[2]]]]RuleValidation`RuleRequire[GammaTensor[\[Sigma],\[Nu],\[Beta],\[Lambda]2,#[[4]],#[[5]]]] - 2 RuleValidation`RuleRequire[GammaTensor[\[Alpha],\[Mu],\[Nu],\[Lambda]1,#[[1]],#[[2]]]]RuleValidation`RuleRequire[GammaTensor[\[Rho],\[Beta],\[Sigma],\[Lambda]2,#[[4]],#[[5]]]] ) )&,
				Flatten/@Permutations[Partition[indexArray,3]]
			]]
		]]//Expand
        ), True
    ];


Clear[GravitonVertexUncontracted];

GravitonVertexUncontracted[indexArray_] :=
    RuleValidation`RuleCall[
        GravitonVertexUncontracted[indexArray],
        {{1, "Array", 3, 6, Infinity}},
        (
Total[RuleValidation`RuleRequire[
			RuleValidation`RuleRequire[Map[
				( I (Global`\[Kappa])^(Length[#]/3-2) RuleValidation`RuleRequire[CTensorGeneral[{Global`\[Mu],Global`\[Nu],Global`\[Alpha],Global`\[Beta],Global`\[Rho],Global`\[Sigma]},RuleValidation`RuleRequire[TakeLorenzIndices[#[[7;;]]]]]]FVD[#[[3]],Global`\[Lambda]1]FVD[#[[6]],Global`\[Lambda]2] (2 RuleValidation`RuleRequire[GammaTensor[Global`\[Alpha],Global`\[Mu],Global`\[Rho],Global`\[Lambda]1,#[[1]],#[[2]]]]RuleValidation`RuleRequire[GammaTensor[Global`\[Sigma],Global`\[Nu],Global`\[Beta],Global`\[Lambda]2,#[[4]],#[[5]]]] - 2 RuleValidation`RuleRequire[GammaTensor[Global`\[Alpha],Global`\[Mu],Global`\[Nu],Global`\[Lambda]1,#[[1]],#[[2]]]]RuleValidation`RuleRequire[GammaTensor[Global`\[Rho],Global`\[Beta],Global`\[Sigma],Global`\[Lambda]2,#[[4]],#[[5]]]] ) )&,
				Flatten/@Permutations[Partition[indexArray,3]]
			]]
		]]
        ), True
    ];




(* Unsupported arities fail before any calculation. *)
GravitonVertex[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonVertex] <> SymbolName[GravitonVertex], {arguments}, {1}];

GravitonVertexUncontracted[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GravitonVertexUncontracted] <> SymbolName[GravitonVertexUncontracted], {arguments}, {1}];

TakeLorenzIndices[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[TakeLorenzIndices] <> SymbolName[TakeLorenzIndices], {arguments}, {1}];

End[];


EndPackage[];
