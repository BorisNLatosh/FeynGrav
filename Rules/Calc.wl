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

	Options[Calc] is kept for call compatibility. The options Assumptions and
	PowerExpand were consumed only by the removed PowerSimplify step and have
	no effect here.
*)


BeginPackage["Calc`",{"FeynCalc`"}];


Calc::usage = 
"Calc[exp] performs several simplifications that involve Contract, DiracSimplify, SUNSimplify, DotSimplify, EpsEvaluate, ExpandScalarProduct and Expand2. The chain is applied repeatedly, and the fixed point is returned. \
The options Assumptions and PowerExpand are accepted for compatibility with FeynCalc's Calc; they had an effect only through the legacy PowerSimplify step and are inert here.";


Begin["`Private`"];


Clear[Calc];


Options[Calc] = { Assumptions -> True, PowerExpand -> True };


Calc[expr_, OptionsPattern[]] :=
	FixedPoint[
		Function[exp,
			Expand2 @ ExpandScalarProduct @ DotSimplify @ DiracSimplify @ EpsEvaluate @
			Contract @ DiracSimplify @ Contract @ Explicit @
			(SUNSimplify[#,Explicit -> False]&) @ exp
		],
		expr,
		5
	];


End[];


EndPackage[];
