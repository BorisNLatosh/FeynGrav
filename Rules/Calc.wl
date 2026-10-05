(* ::Package:: *)

(*
	Calc` is a self-contained replacement for FeynCalc's Calc.

	FeynCalc 10.2 moved Calc into the FeynCalcLegacy addon, which is not loaded
	by default. The chain implemented below is Calc's pipeline

		AddOns/FeynCalcLegacy/LegacyFunctions/Calc.m

	from FeynCalc 10.2.1 ((C) Rolf Mertig, Frederik Orellana, Vladyslav
	Shtabovenko, GPL-3), with the legacy-only steps removed: Trick and
	PowerSimplify (together with its helper Power2). Both were measured to
	leave every expression that FeynGrav passes to Calc unchanged, so they are
	not reproduced here.

	The definitions below use the fully qualified name Calc`Calc on purpose. A
	bare Calc binds to whatever Calc already exists on $ContextPath, so with
	the FeynCalcLegacy addon loaded it would bind to FeynCalc`Calc and these
	definitions would silently overwrite FeynCalc's own function.

	Options[Calc] is kept for call compatibility. The options Assumptions and
	PowerExpand were consumed only by the removed PowerSimplify step and have
	no effect here.
*)


(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["Calc`",{"FeynCalc`"}];


Calc`Calc::usage =
"Calc[exp] performs several simplifications that involve Contract, DiracSimplify, SUNSimplify, DotSimplify, EpsEvaluate, ExpandScalarProduct and Expand2. The chain is applied repeatedly, and the fixed point is returned. \
The options Assumptions and PowerExpand are accepted for compatibility with FeynCalc's Calc; they had an effect only through the legacy PowerSimplify step and are inert here.";



(* Structural failures are returned as values; callers should use FailureQ. *)

Begin["`Private`"];


Clear[Calc`Calc];


Options[Calc`Calc] = { Assumptions -> True, PowerExpand -> True };


Calc`Calc[expr_, opts:OptionsPattern[]] :=
    RuleValidation`RuleCall[Calc`Calc[expr, opts], {},
	FixedPoint[
		Function[exp,
			Fold[RuleValidation`RuleRequire[#2[#1]] &, exp,
                {(SUNSimplify[#, Explicit -> False] &), Explicit, Contract,
                 DiracSimplify, Contract, EpsEvaluate, DiracSimplify,
                 DotSimplify, ExpandScalarProduct, Expand2}]
		],
		expr,
		5
	], False];




(* Unsupported arities fail before any calculation. *)


Calc`Calc[arguments___] /; Length[{arguments}] == 0 ||
    !AllTrue[Rest[{arguments}], MatchQ[#, _Rule | _RuleDelayed | {(_Rule | _RuleDelayed)...}] &] :=
    RuleValidation`RuleArityFailure["Calc`Calc", {arguments}, {1}];

End[];


EndPackage[];
