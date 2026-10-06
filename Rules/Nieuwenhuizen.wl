(* ::Package:: *)

(* Shared validation is loaded by absolute path without changing Directory[]. *)
With[{validationFile = FileNameJoin[{DirectoryName[$InputFileName], "RuleValidation.wl"}]},
    Block[{$ContextPath = $ContextPath}, Needs["RuleValidation`", validationFile]]
];

BeginPackage["Nieuwenhuizen`",{"FeynCalc`"}];


Needs["CalcFormConverter`", FileNameJoin[{DirectoryName[$InputFileName], "../CalcFormConverter", "CalcFormConverter.wl"}]];


GaugeProjector::usage =
"GaugeProjector[\[Mu],\[Nu],p]. \
	The standard gauge projector \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\)-\!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."


GaugeProjectorBar::usage =
"GaugeProjectorBar[\[Mu],\[Nu],p]. \
The standard gauge projector \!\(\*SubscriptBox[OverscriptBox[\(\[Theta]\), \(_\)], \(\[Mu]\[Nu]\)]\)(p) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."


NieuwenhuizenOperator1::usage =
"NieuwenhuizenOperator1[\[Mu],\[Nu],\[Alpha],\[Beta],p]. \
Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(1\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Nu]\[Alpha]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Alpha]\)]\)). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."


NieuwenhuizenOperator2::usage =
"NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],p]. \
Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(2\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(2\)]\)(\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Alpha]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Nu]\[Alpha]\)]\))-\!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."


NieuwenhuizenOperator0::usage =
"NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],p]. \
Nieuwenhuizen operator (\!\(\*SuperscriptBox[\(P\), \(0\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*FractionBox[\(1\), \(3\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors."


NieuwenhuizenOperator0Bar::usage =
"NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],p]. \
Nieuwenhuizen operator (\!\(\*OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) =\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\). Here \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to the standard gauge projector \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\)."


NieuwenhuizenOperator0BarBar::usage =
"NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],p]. \
Nieuwenhuizen operator (\!\(\*OverscriptBox[OverscriptBox[SuperscriptBox[\(P\), \(0\)], \(_\)], \(_\)]\)\!\(\*SubscriptBox[\()\), \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\) = \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Alpha]\[Beta]\)]\)+\!\(\*SubscriptBox[\(\[Theta]\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\). Here \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)=\!\(\*SubscriptBox[\(\[Eta]\), \(\[Mu]\[Nu]\)]\) - \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are the standard gauge projectors and \!\(\*SubscriptBox[\(\[Omega]\), \(\[Mu]\[Nu]\)]\) = \!\(\*SubscriptBox[\(p\), \(\[Mu]\)]\)\!\(\*SubscriptBox[\(p\), \(\[Nu]\)]\)/\!\(\*SuperscriptBox[\(p\), \(2\)]\) are projectors orthogonal to \!\(\*SubscriptBox[\(\[Theta]\), \(\[Mu]\[Nu]\)]\)."


NieuwenhuizenOperator::usage =
"NieuwenhuizenOperator[\!\(\*SubscriptBox[\(z\), \(1\)]\),\!\(\*SubscriptBox[\(z\), \(2\)]\),\!\(\*SubscriptBox[\(z\), \(0\)]\),\!\(\*SubscriptBox[\(z\), \(b\)]\),\!\(\*SubscriptBox[\(z\), \(bb\)]\),\[Mu],\[Nu],\[Alpha],\[Beta],p]. \
A linear combination of Nieuwenhuizen operators: \!\(\*SubscriptBox[\(z\), \(1\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(P\), \(1\)], \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\)(p)+\!\(\*SubscriptBox[\(z\), \(2\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(P\), \(2\)], \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\)(p)+\!\(\*SubscriptBox[\(z\), \(0\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(P\), \(0\)], \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\)(p)+\!\(\*SubscriptBox[\(z\), \(b\)]\)\!\(\*SubscriptBox[SuperscriptBox[OverscriptBox[\(P\), \(_\)], \(0\)], \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\)(p)+\!\(\*SubscriptBox[\(z\), \(bb\)]\)\!\(\*SubscriptBox[SuperscriptBox[OverscriptBox[\(P\), OverscriptBox[\(_\), \(_\)]], \(0\)], \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\)(p)."


NieuwenhuizenOperatorInverse::usage =
"NieuwenhuizenOperatorInverse[\!\(\*SubscriptBox[\(z\), \(1\)]\),\!\(\*SubscriptBox[\(z\), \(2\)]\),\!\(\*SubscriptBox[\(z\), \(0\)]\),\!\(\*SubscriptBox[\(z\), \(b\)]\),\!\(\*SubscriptBox[\(z\), \(bb\)]\),\[Mu],\[Nu],\[Alpha],\[Beta],p]. \
A linear combination of Nieuwenhuizen operators which is inverse for \!\(\*SubscriptBox[\(z\), \(1\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(P\), \(1\)], \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\)(p)+\!\(\*SubscriptBox[\(z\), \(2\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(P\), \(2\)], \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\)(p)+\!\(\*SubscriptBox[\(z\), \(0\)]\)\!\(\*SubscriptBox[SuperscriptBox[\(P\), \(0\)], \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\)(p)+\!\(\*SubscriptBox[\(z\), \(b\)]\)\!\(\*SubscriptBox[SuperscriptBox[OverscriptBox[\(P\), \(_\)], \(0\)], \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\)(p)+\!\(\*SubscriptBox[\(z\), \(bb\)]\)\!\(\*SubscriptBox[SuperscriptBox[OverscriptBox[\(P\), OverscriptBox[\(_\), \(_\)]], \(0\)], \(\[Mu]\[Nu]\[Alpha]\[Beta]\)]\)(p)."


NieuwenhuizenOperatorExpansion::usage =
"NieuwenhuizenOperatorExpansion[T,\[Mu],\[Nu],\[Alpha],\[Beta],p]. \
The function takes a tensor T with Lorentz indices {\[Mu],\[Nu],\[Alpha],\[Beta]} that may depend on the momentum p. The function expands the tensor in Nieuwenhuizen operators and returns a set of coordinates {\!\(\*SubscriptBox[\(z\), \(1\)]\),\!\(\*SubscriptBox[\(z\), \(2\)]\),\!\(\*SubscriptBox[\(z\), \(0\)]\),\!\(\*SubscriptBox[OverscriptBox[\(z\), \(_\)], \(0\)]\),\!\(\*SubscriptBox[OverscriptBox[\(z\), OverscriptBox[\(_\), \(_\)]], \(0\)]\)} which can be used with NieuwenhuizenOperator. Coefficients are returned only after verification of the complete reconstructed tensor. Unsupported structures, inconclusive verification and calculation or coefficient-solution failures return Failure objects.";


(* Keep helpers and memoised definitions local to this rule package. *)

(* Structural failures are returned as values; callers should use FailureQ. *)
GaugeProjector::usage = GaugeProjector::usage <> " Supported signatures: GaugeProjector[m, n, p]; argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
GaugeProjectorBar::usage = GaugeProjectorBar::usage <> " Supported signatures: GaugeProjectorBar[m, n, p]; argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
NieuwenhuizenOperator1::usage = NieuwenhuizenOperator1::usage <> " Supported signatures: NieuwenhuizenOperator1[\\[Mu], \\[Nu], \\[Alpha], \\[Beta], k]; argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
NieuwenhuizenOperator2::usage = NieuwenhuizenOperator2::usage <> " Supported signatures: NieuwenhuizenOperator2[\\[Mu], \\[Nu], \\[Alpha], \\[Beta], k]; argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
NieuwenhuizenOperator0::usage = NieuwenhuizenOperator0::usage <> " Supported signatures: NieuwenhuizenOperator0[\\[Mu], \\[Nu], \\[Alpha], \\[Beta], k]; argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
NieuwenhuizenOperator0Bar::usage = NieuwenhuizenOperator0Bar::usage <> " Supported signatures: NieuwenhuizenOperator0Bar[\\[Mu], \\[Nu], \\[Alpha], \\[Beta], k]; argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
NieuwenhuizenOperator0BarBar::usage = NieuwenhuizenOperator0BarBar::usage <> " Supported signatures: NieuwenhuizenOperator0BarBar[\\[Mu], \\[Nu], \\[Alpha], \\[Beta], k]; argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
NieuwenhuizenOperator::usage = NieuwenhuizenOperator::usage <> " Supported signatures: NieuwenhuizenOperator[z1, z2, z0, z0b, z0bb, \\[Mu], \\[Nu], \\[Alpha], \\[Beta], k]; argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association); argument 9: symbolic expression (not a list or association); argument 10: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
NieuwenhuizenOperatorInverse::usage = NieuwenhuizenOperatorInverse::usage <> " Supported signatures: NieuwenhuizenOperatorInverse[z1, z2, z0, z0b, z0bb, \\[Mu], \\[Nu], \\[Alpha], \\[Beta], k]; argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association); argument 7: symbolic expression (not a list or association); argument 8: symbolic expression (not a list or association); argument 9: symbolic expression (not a list or association); argument 10: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";
NieuwenhuizenOperatorExpansion::usage = NieuwenhuizenOperatorExpansion::usage <> " Supported signatures: NieuwenhuizenOperatorExpansion[T, m, n, a, b, p]; argument 1: symbolic expression (not a list or association); argument 2: symbolic expression (not a list or association); argument 3: symbolic expression (not a list or association); argument 4: symbolic expression (not a list or association); argument 5: symbolic expression (not a list or association); argument 6: symbolic expression (not a list or association)." <> " Invalid argument counts, malformed arrays and unsupported parameter ranges return Failure. Existing dependency failures are propagated; check FailureQ before using the result. See Rules/README.md for the argument contract.";

Begin["`Private`"];


Clear[GaugeProjector];

GaugeProjector[m_, n_, p_] :=
    RuleValidation`RuleCall[
        GaugeProjector[m, n, p],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}},
        (
Pair[LorentzIndex[m,D],LorentzIndex[n,D]]-FeynAmpDenominator[PropagatorDenominator[Momentum[p,D],0]] Pair[LorentzIndex[m,D],Momentum[p,D]] Pair[LorentzIndex[n,D],Momentum[p,D]]
        ), False
    ];
Clear[GaugeProjectorBar];

GaugeProjectorBar[m_, n_, p_] :=
    RuleValidation`RuleCall[
        GaugeProjectorBar[m, n, p],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}},
        (
FeynAmpDenominator[PropagatorDenominator[Momentum[p,D],0]] Pair[LorentzIndex[m,D],Momentum[p,D]] Pair[LorentzIndex[n,D],Momentum[p,D]]
        ), False
    ];


Clear[NieuwenhuizenOperator1];

NieuwenhuizenOperator1[\[Mu]_, \[Nu]_, \[Alpha]_, \[Beta]_, k_] :=
    RuleValidation`RuleCall[
        NieuwenhuizenOperator1[\[Mu], \[Nu], \[Alpha], \[Beta], k],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}},
        (
1/2 (RuleValidation`RuleRequire[GaugeProjector[\[Mu],\[Alpha],k]]RuleValidation`RuleRequire[GaugeProjectorBar[\[Nu],\[Beta],k]] + RuleValidation`RuleRequire[GaugeProjector[\[Mu],\[Beta],k]]RuleValidation`RuleRequire[GaugeProjectorBar[\[Nu],\[Alpha],k]] + RuleValidation`RuleRequire[GaugeProjector[\[Nu],\[Alpha],k]]RuleValidation`RuleRequire[GaugeProjectorBar[\[Mu],\[Beta],k]] + RuleValidation`RuleRequire[GaugeProjector[\[Nu],\[Beta],k]]RuleValidation`RuleRequire[GaugeProjectorBar[\[Mu],\[Alpha],k]]) //FeynAmpDenominatorCombine
        ), False
    ];
Clear[NieuwenhuizenOperator2];

NieuwenhuizenOperator2[\[Mu]_, \[Nu]_, \[Alpha]_, \[Beta]_, k_] :=
    RuleValidation`RuleCall[
        NieuwenhuizenOperator2[\[Mu], \[Nu], \[Alpha], \[Beta], k],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}},
        (
1/2 (RuleValidation`RuleRequire[GaugeProjector[\[Mu],\[Alpha],k]]RuleValidation`RuleRequire[GaugeProjector[\[Nu],\[Beta],k]] + RuleValidation`RuleRequire[GaugeProjector[\[Mu],\[Beta],k]]RuleValidation`RuleRequire[GaugeProjector[\[Nu],\[Alpha],k]]) - 1/3 RuleValidation`RuleRequire[GaugeProjector[\[Mu],\[Nu],k]]RuleValidation`RuleRequire[GaugeProjector[\[Alpha],\[Beta],k]] //FeynAmpDenominatorCombine
        ), False
    ];
Clear[NieuwenhuizenOperator0];

NieuwenhuizenOperator0[\[Mu]_, \[Nu]_, \[Alpha]_, \[Beta]_, k_] :=
    RuleValidation`RuleCall[
        NieuwenhuizenOperator0[\[Mu], \[Nu], \[Alpha], \[Beta], k],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}},
        (
1/3 RuleValidation`RuleRequire[GaugeProjector[\[Mu],\[Nu],k]]RuleValidation`RuleRequire[GaugeProjector[\[Alpha],\[Beta],k]] //FeynAmpDenominatorCombine
        ), False
    ];
Clear[NieuwenhuizenOperator0Bar];

NieuwenhuizenOperator0Bar[\[Mu]_, \[Nu]_, \[Alpha]_, \[Beta]_, k_] :=
    RuleValidation`RuleCall[
        NieuwenhuizenOperator0Bar[\[Mu], \[Nu], \[Alpha], \[Beta], k],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}},
        (
RuleValidation`RuleRequire[GaugeProjectorBar[\[Mu],\[Nu],k]]RuleValidation`RuleRequire[GaugeProjectorBar[\[Alpha],\[Beta],k]] //FeynAmpDenominatorCombine
        ), False
    ];
Clear[NieuwenhuizenOperator0BarBar];

NieuwenhuizenOperator0BarBar[\[Mu]_, \[Nu]_, \[Alpha]_, \[Beta]_, k_] :=
    RuleValidation`RuleCall[
        NieuwenhuizenOperator0BarBar[\[Mu], \[Nu], \[Alpha], \[Beta], k],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}},
        (
RuleValidation`RuleRequire[GaugeProjector[\[Mu],\[Nu],k]]RuleValidation`RuleRequire[GaugeProjectorBar[\[Alpha],\[Beta],k]] + RuleValidation`RuleRequire[GaugeProjectorBar[\[Mu],\[Nu],k]]RuleValidation`RuleRequire[GaugeProjector[\[Alpha],\[Beta],k]] //FeynAmpDenominatorCombine
        ), False
    ];


Clear[NieuwenhuizenOperator];

NieuwenhuizenOperator[z1_, z2_, z0_, z0b_, z0bb_, \[Mu]_, \[Nu]_, \[Alpha]_, \[Beta]_, k_] :=
    RuleValidation`RuleCall[
        NieuwenhuizenOperator[z1, z2, z0, z0b, z0bb, \[Mu], \[Nu], \[Alpha], \[Beta], k],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}, {7, "Expression"}, {8, "Expression"}, {9, "Expression"}, {10, "Expression"}},
        (
z1 RuleValidation`RuleRequire[NieuwenhuizenOperator1[\[Mu],\[Nu],\[Alpha],\[Beta],k]]+z2 RuleValidation`RuleRequire[NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],k]]+z0 RuleValidation`RuleRequire[NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],k]]+z0b RuleValidation`RuleRequire[NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],k]]+z0bb RuleValidation`RuleRequire[NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],k]]//FeynAmpDenominatorCombine
        ), False
    ];
Clear[NieuwenhuizenOperatorInverse];

NieuwenhuizenOperatorInverse[z1_, z2_, z0_, z0b_, z0bb_, \[Mu]_, \[Nu]_, \[Alpha]_, \[Beta]_, k_] :=
    RuleValidation`RuleCall[
        NieuwenhuizenOperatorInverse[z1, z2, z0, z0b, z0bb, \[Mu], \[Nu], \[Alpha], \[Beta], k],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}, {7, "Expression"}, {8, "Expression"}, {9, "Expression"}, {10, "Expression"}},
        (
1/z1 RuleValidation`RuleRequire[NieuwenhuizenOperator1[\[Mu],\[Nu],\[Alpha],\[Beta],k]]+1/z2 RuleValidation`RuleRequire[NieuwenhuizenOperator2[\[Mu],\[Nu],\[Alpha],\[Beta],k]]+1/z2 (((D-4)(z0 z0b -3 z0bb^2)-(D-7)z2 z0b)/((D-1)(z0 z0b -3 z0bb^2)-(D-4)z2 z0b)) RuleValidation`RuleRequire[NieuwenhuizenOperator0[\[Mu],\[Nu],\[Alpha],\[Beta],k]]+((D-1)z0-(D-4)z2)/((D-1)(z0 z0b -3 z0bb^2)-(D-4)z2 z0b) RuleValidation`RuleRequire[NieuwenhuizenOperator0Bar[\[Mu],\[Nu],\[Alpha],\[Beta],k]]-(3 z0bb)/((D-1)(z0 z0b -3 z0bb^2)-(D-4)z2 z0b) RuleValidation`RuleRequire[NieuwenhuizenOperator0BarBar[\[Mu],\[Nu],\[Alpha],\[Beta],k]]//FeynAmpDenominatorCombine
        ), False
    ];


(* Decide whether a tensor polynomial vanishes identically. A nonzero
   coefficient proves that it is not an identity; unresolved scalar
   equalities remain inconclusive. No numerical sampling is used. *)
Clear[tensorZeroStatus];

tensorZeroStatus[expr_] :=
    RuleValidation`RuleCall[
        tensorZeroStatus[expr],
        {},
        (
Module[{reduced, tensors, variables, polynomial, coefficients, decisions},
    reduced = Simplify[expr];
    If[SameQ[reduced, 0], Return[True]];
    tensors = DeleteDuplicates[Cases[reduced,
        pair_Pair /; !FreeQ[pair, _LorentzIndex], {0, Infinity}]];
    variables = Table[Unique["tensor$"], {Length[tensors]}];
    polynomial = Expand[reduced /. Thread[tensors -> variables]];
    If[!PolynomialQ[polynomial, variables], Return[Missing["Inconclusive"]]];
    coefficients = If[variables === {}, {polynomial}, Last /@ CoefficientRules[polynomial, variables]];
    decisions = Simplify[# == 0] & /@ coefficients;
    Which[
        AllTrue[decisions, TrueQ], True,
        MemberQ[decisions, False], False,
        True, Missing["Inconclusive"]
    ]
]
        ), False
    ];

(* Preserve FORM diagnostics rather than interpreting failures as tensors. *)
Clear[calculateTensor];

calculateTensor[expr_, stage_] :=
    RuleValidation`RuleCall[
        calculateTensor[expr, stage],
        {},
        (
Module[{result = CalcFormCalculate[expr]},
    Which[
        FailureQ[result], result,
        result === $Failed || result === $Aborted,
            Failure["TensorCalculationFailed", <|"MessageTemplate" ->
                "The tensor calculation did not complete.", "Stage" -> stage, "Result" -> result|>],
        True, result
    ]
]
        ), False
    ];

Clear[NieuwenhuizenSymmetryCheck];

NieuwenhuizenSymmetryCheck[T_, m_, n_, a_, b_, p_] :=
    RuleValidation`RuleCall[
        NieuwenhuizenSymmetryCheck[T, m, n, a, b, p],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}},
        (
Module[{residual, status, tag = Unique["symmetry$"]},
    Catch[
        Do[
            residual = RuleValidation`RuleRequire[calculateTensor[RuleValidation`RuleRequire[FeynAmpDenominatorExplicit[T - (T /. permutation)]], "Symmetry"]];
            If[FailureQ[residual], Throw[residual, tag]];
            status = RuleValidation`RuleRequire[tensorZeroStatus[residual]];
            If[status === False, Throw[False, tag]];
            If[status =!= True, Throw[Failure["SymmetryVerificationInconclusive", <|
                "MessageTemplate" -> "The required tensor symmetry could not be verified.",
                "Residual" -> residual|>], tag]],
            {permutation, {{m -> n, n -> m}, {a -> b, b -> a}, {m -> a, n -> b, a -> m, b -> n}}}
        ];
        True,
        tag
    ]
]
        ), False
    ];

Clear[NieuwenhuizenOperatorExpansion];

NieuwenhuizenOperatorExpansion[T_, m_, n_, a_, b_, p_] :=
    RuleValidation`RuleCall[
        NieuwenhuizenOperatorExpansion[T, m, n, a, b, p],
        {{1, "Expression"}, {2, "Expression"}, {3, "Expression"}, {4, "Expression"}, {5, "Expression"}, {6, "Expression"}},
        (
Module[
    {symmetry, z1, z2, z0, zb, zbb, variables, difference, equations,
     solutions, coefficients, residual, status},
    symmetry = RuleValidation`RuleRequire[NieuwenhuizenSymmetryCheck[T,m,n,a,b,p]];
    If[FailureQ[symmetry], Return[symmetry]];
    If[symmetry =!= True, Return[Failure["TensorSymmetryMismatch", <|
        "MessageTemplate" -> "The tensor does not have the symmetries required by the Nieuwenhuizen basis."|>]]];

    (* Retain the existing five coefficient equations, with local unknowns.
       These equations supply candidates; reconstruction below is decisive. *)
    variables = {z1,z2,z0,zb,zbb};
    difference = RuleValidation`RuleRequire[calculateTensor[RuleValidation`RuleRequire[FeynAmpDenominatorExplicit[
        T - RuleValidation`RuleRequire[NieuwenhuizenOperator[z1,z2,z0,zb,zbb,m,n,a,b,p]]]], "CoefficientExtraction"]];
    If[FailureQ[difference], Return[difference]];
    equations = (# == 0 &) /@ Coefficient[difference, RuleValidation`RuleRequire[FCI[{
        MTD[m,n] MTD[a,b], MTD[m,a] MTD[n,b], MTD[m,n] FVD[p,a] FVD[p,b],
        MTD[m,a] FVD[p,n] FVD[p,b], FVD[p,m] FVD[p,n] FVD[p,a] FVD[p,b]}]]];
    solutions = Quiet[Check[SolveValues[equations, variables], $Failed]];
    If[solutions === {}, Return[Failure["InconsistentCoefficientSystem", <|
        "MessageTemplate" -> "The coefficient equations have no solution.", "Equations" -> equations|>]]];
    If[!MatchQ[solutions, {{_,_,_,_,_}}], Return[Failure["CoefficientSolutionFailed", <|
        "MessageTemplate" -> "The coefficient equations did not yield one complete solution.",
        "Solutions" -> solutions|>]]];
    coefficients = First[solutions];
    If[!FreeQ[coefficients, Alternatives @@ variables] ||
        !FreeQ[coefficients, _ConditionalExpression | _C | _LorentzIndex | Indeterminate | _DirectedInfinity],
        Return[Failure["UnderdeterminedCoefficientSystem", <|
            "MessageTemplate" -> "The coefficient solution is incomplete or is not a list of scalar coefficients.",
            "CandidateCoefficients" -> coefficients|>]]];

    (* Check the full tensor, including terms absent from the five equations. *)
    residual = RuleValidation`RuleRequire[calculateTensor[difference /. Thread[variables -> coefficients], "Reconstruction"]];
    If[FailureQ[residual], Return[residual]];
    residual = Simplify[residual];
    status = RuleValidation`RuleRequire[tensorZeroStatus[residual]];
    Which[
        status === True, coefficients,
        status === False, Failure["UnsupportedTensorStructure", <|
            "MessageTemplate" -> "The candidate coefficients do not reconstruct the complete tensor in the supported Nieuwenhuizen basis.",
            "CandidateCoefficients" -> coefficients, "Residual" -> residual|>],
        True, Failure["ExpansionVerificationInconclusive", <|
            "MessageTemplate" -> "The reconstruction residual could not be proved to vanish.",
            "CandidateCoefficients" -> coefficients, "Residual" -> residual|>]
    ]
]
        ), False
    ];




(* Unsupported arities fail before any calculation. *)
GaugeProjector[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GaugeProjector] <> SymbolName[GaugeProjector], {arguments}, {3}];

GaugeProjectorBar[arguments___] /; !MemberQ[{3}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[GaugeProjectorBar] <> SymbolName[GaugeProjectorBar], {arguments}, {3}];

NieuwenhuizenOperator[arguments___] /; !MemberQ[{10}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[NieuwenhuizenOperator] <> SymbolName[NieuwenhuizenOperator], {arguments}, {10}];

NieuwenhuizenOperator0[arguments___] /; !MemberQ[{5}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[NieuwenhuizenOperator0] <> SymbolName[NieuwenhuizenOperator0], {arguments}, {5}];

NieuwenhuizenOperator0Bar[arguments___] /; !MemberQ[{5}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[NieuwenhuizenOperator0Bar] <> SymbolName[NieuwenhuizenOperator0Bar], {arguments}, {5}];

NieuwenhuizenOperator0BarBar[arguments___] /; !MemberQ[{5}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[NieuwenhuizenOperator0BarBar] <> SymbolName[NieuwenhuizenOperator0BarBar], {arguments}, {5}];

NieuwenhuizenOperator1[arguments___] /; !MemberQ[{5}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[NieuwenhuizenOperator1] <> SymbolName[NieuwenhuizenOperator1], {arguments}, {5}];

NieuwenhuizenOperator2[arguments___] /; !MemberQ[{5}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[NieuwenhuizenOperator2] <> SymbolName[NieuwenhuizenOperator2], {arguments}, {5}];

NieuwenhuizenOperatorExpansion[arguments___] /; !MemberQ[{6}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[NieuwenhuizenOperatorExpansion] <> SymbolName[NieuwenhuizenOperatorExpansion], {arguments}, {6}];

NieuwenhuizenOperatorInverse[arguments___] /; !MemberQ[{10}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[NieuwenhuizenOperatorInverse] <> SymbolName[NieuwenhuizenOperatorInverse], {arguments}, {10}];

NieuwenhuizenSymmetryCheck[arguments___] /; !MemberQ[{6}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[NieuwenhuizenSymmetryCheck] <> SymbolName[NieuwenhuizenSymmetryCheck], {arguments}, {6}];

calculateTensor[arguments___] /; !MemberQ[{2}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[calculateTensor] <> SymbolName[calculateTensor], {arguments}, {2}];

tensorZeroStatus[arguments___] /; !MemberQ[{1}, Length[{arguments}]] :=
    RuleValidation`RuleArityFailure[Context[tensorZeroStatus] <> SymbolName[tensorZeroStatus], {arguments}, {1}];

End[];


EndPackage[];
