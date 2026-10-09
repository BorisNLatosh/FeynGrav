(* ::Package:: *)

(* ::Title:: *)
(* FeynGrav conventions report *)

(* ::Text:: *)
(* Loaded by FeynGrav.wl inside its private context. Loading only defines
   helpers. Reporting reads current settings and existing import records;
   it does not import libraries, run algebra, write files or launch FORM.
   Fixed mathematics is stored as inert display boxes, never as executable
   formulas. This keeps it independent of assignments to D, kappa or momenta. *)

(* ::Section:: *)
(* Safe value display and dependency inspection *)

(* HoldComplete snapshots must never be released into the evaluator. Strip
   only their printed outer wrapper. InputForm preserves symbolic contents. *)
conventionHeldText[held_HoldComplete] := Module[{text},
    text = ToString[held, InputForm, PageWidth -> Infinity];
    StringTake[text, {StringLength["HoldComplete["] + 1, -2}]
];
conventionHeldText[_] := "Unavailable";
conventionShortName[name_String] := Last[StringSplit[name, "`"]];

SetAttributes[conventionSymbolValue, HoldAllComplete];
conventionSymbolValue[s_Symbol] := If[OwnValues[s] === {},
    "Unassigned (symbolic)", conventionHeldText[importSettingSnapshot[s]]];

conventionGetter[s_Symbol] := Module[{value},
    If[DownValues[s] === {}, Return["Unavailable"]];
    value = Quiet[Check[s[], $Failed]];
    If[value === $Failed || FailureQ[value] || Head[value] === s,
        "Unavailable", conventionHeldText[importSettingSnapshot[value]]]
];
conventionOption[head_Symbol, option_Symbol] := Module[{values},
    (* Delayed defaults are displayed as held expressions, never evaluated:
       evaluating them could change session state merely by printing the report. *)
    values = Join[
        Cases[Options[head], HoldPattern[Rule[option, value_]] :>
            conventionHeldText[HoldComplete[value]]],
        Cases[Options[head], HoldPattern[RuleDelayed[option, value_]] :>
            "Delayed default: " <> conventionHeldText[HoldComplete[value]]]
    ];
    If[Length[values] === 1, First[values], "Unavailable"]
];

(* ::Section:: *)
(* Fixed mathematical notation *)

(* Helpers construct boxes from strings only. The complete expression is held
   in the report data, including when the kernel has assigned physical symbols. *)
conventionGamma[up_String, down_String] := SubsuperscriptBox["\[CapitalGamma]", down, up];
conventionFourierBoxes[dim_String] := RowBox[{
    "f(x)", "=", "\[Integral]", FractionBox[RowBox[{SuperscriptBox["d", dim], "k"}],
        SuperscriptBox["(2\[Pi])", dim]], "f(k)",
    SuperscriptBox["e", RowBox[{"i", SuperscriptBox["k", "\[Mu]"], SubscriptBox["x", "\[Mu]"]}]]}];
conventionMath[label_String, plain_String, boxes_] := With[{b = boxes},
    conventionFormula[label, plain, HoldComplete[b]]];

(* ::Section:: *)
(* Compact import information *)

conventionImportRows[records_Association] := KeyValueMap[Function[{name, record},
    If[!AssociationQ[record], {conventionShortName[name], "Not recorded", "Not recorded"},
        {conventionShortName[name],
         StringRiffle[ToString /@ Sort[DeleteDuplicates[Flatten[Values[record["Orders"]]]]], ", "],
         If[record["ChangedSettings"] === {}, "Match recorded settings", "Settings differ"]}]
], records];

conventionDifferenceRows[records_Association] := Flatten[KeyValueMap[Function[{name, record},
    If[!AssociationQ[record], {}, Map[Function[key,
        {conventionShortName[name], conventionShortName[key],
         conventionHeldText[record["Settings"][key]],
         conventionHeldText[record["CurrentSettings"][key]]}], record["ChangedSettings"]]]
], records], 1];

(* ::Section:: *)
(* Report contents *)

conventionReport[] := Module[{records, differences, gaugeRows, sections},
    records = FeynGravLibraryInformation[];
    differences = conventionDifferenceRows[records];
    gaugeRows = {
        {"GaugeFixingEpsilon", "2", conventionSymbolValue[FeynGrav`GaugeFixingEpsilon]},
        {"GaugeFixingEpsilonCR", "-1/2", conventionSymbolValue[FeynGrav`GaugeFixingEpsilonCR]},
        {"GaugeFixingEpsilonVector", "-1", conventionSymbolValue[FeynGrav`GaugeFixingEpsilonVector]},
        {"GaugeFixingEpsilonSUNYM", "-1", conventionSymbolValue[FeynGrav`GaugeFixingEpsilonSUNYM]},
        {"GaugeFixingEpsilonHD", "Unassigned", conventionSymbolValue[FeynGrav`GaugeFixingEpsilonHD]},
        {"GaugeFixingEpsilonHD0", "Unassigned", conventionSymbolValue[FeynGrav`GaugeFixingEpsilonHD0]},
        {"GaugeFixingEpsilonHD1", "Unassigned", conventionSymbolValue[FeynGrav`GaugeFixingEpsilonHD1]}
    };
    sections = {
        conventionSection["1. Geometry and Fourier transforms", {
            conventionText["Fixed conventions: a flat Minkowski background with signature (+,-,-,-); the following curvature index order and Fourier phase define the convention."],
            conventionMath["Riemann tensor",
                "R_(mu nu)^alpha_beta = d_mu Gamma^alpha_(nu beta) - d_nu Gamma^alpha_(mu beta) + Gamma^alpha_(mu sigma) Gamma^sigma_(nu beta) - Gamma^alpha_(nu sigma) Gamma^sigma_(mu beta)",
                RowBox[{SubscriptBox["R", "\[Mu]\[Nu]"], SuperscriptBox["", "\[Alpha]"], SubscriptBox["", "\[Beta]"], "=",
                    SubscriptBox["\[PartialD]", "\[Mu]"], conventionGamma["\[Alpha]", "\[Nu]\[Beta]"], "-",
                    SubscriptBox["\[PartialD]", "\[Nu]"], conventionGamma["\[Alpha]", "\[Mu]\[Beta]"], "+",
                    conventionGamma["\[Alpha]", "\[Mu]\[Sigma]"], conventionGamma["\[Sigma]", "\[Nu]\[Beta]"], "-",
                    conventionGamma["\[Alpha]", "\[Nu]\[Sigma]"], conventionGamma["\[Sigma]", "\[Mu]\[Beta]"]}]],
            conventionMath["Four-dimensional Fourier transform",
                "f(x) = Integral[d^4 k/(2 pi)^4 f(k) exp(i k^mu x_mu)]", conventionFourierBoxes["4"]],
            conventionMath["D-dimensional extension",
                "f(x) = Integral[d^D k/(2 pi)^D f(k) exp(i k^mu x_mu)]", conventionFourierBoxes["D"]],
            conventionMath["Momentum-space derivative", "d_mu <-> i k_mu",
                RowBox[{SubscriptBox["\[PartialD]", "\[Mu]"], "\[LeftRightArrow]", "i", SubscriptBox["k", "\[Mu]"]}]],
            conventionMath["Conventional metric expansion and coupling",
                "g_mu_nu = eta_mu_nu + kappa h_mu_nu; kappa^2 = 32 pi G_N",
                RowBox[{SubscriptBox["g", "\[Mu]\[Nu]"], "=", SubscriptBox["\[Eta]", "\[Mu]\[Nu]"], "+",
                    "\[Kappa]", SubscriptBox["h", "\[Mu]\[Nu]"], ";   ", SuperscriptBox["\[Kappa]", "2"], "=", "32\[Pi]", SubscriptBox["G", "N"]}]],
            conventionText["The Newton-constant relation is the physical normalisation; the package does not substitute it automatically. Cheung-Remmen commands use a separate field parametrisation."],
            conventionTable[{"Current session setting", "Value"}, {
                {"Gravitational coupling (kappa)", conventionSymbolValue[FeynGrav`\[Kappa]]},
                {"FeynCalc Cartesian metric setting", conventionGetter[FeynCalc`FCGetMetricSignature]}}],
            conventionText["The Cartesian metric setting controls FeynCalc's Cartesian objects and conversions. Changing it does not transform the stored FeynGrav interaction rules into another signature."]
        }],
        conventionSection["2. Dimensions and momenta", {
            conventionText["Main-package Lorentz tensors generally use symbolic D. Four-dimensional shortcuts use four-dimensional indices; D-dimensional shortcuts retain D. Vertex momenta are incoming."],
            conventionTable[{"Current session setting", "Value"}, {{"D", conventionSymbolValue[D]}}],
            conventionText["The converter requires one consistent Lorentz space in each expression. It does not set D = 4 - 2 epsilon, impose momentum conservation, or supply on-shell conditions."]
        }],
        conventionSection["3. Gauge parameters and loaded libraries", Join[{
            conventionTable[{"Gauge parameter", "Initial default", "Current value"}, gaugeRows],
            conventionText["Conventional gravitational propagators read gauge values when called. Vector and Yang-Mills gauge values can be substituted when their libraries are imported. HD1 represents the retained longitudinal combination with the second coefficient set to zero."],
            conventionTable[{"Library importer", "Loaded file orders n", "Recorded vs current settings"}, conventionImportRows[records]],
            conventionText["For gravity and quadratic gravity, n labels coupling order and n + 2 graviton legs. For matter it counts attached gravitons. Horndeski rows list imported n values; detailed (a,b,n) tuples remain available through FeynGravLibraryInformation." ]},
            If[differences === {}, {conventionText["No differences were found in the recorded import settings."]}, {
                conventionTable[{"Importer", "Changed setting", "At import", "Now"}, differences],
                conventionText["Reimport the relevant library order when a gauge value was substituted during import. Other setting differences do not necessarily change every associated expression."]}], {
            conventionText["Not recorded means that provenance is unavailable. Records describe the last successful import, not manual vertex redefinitions or expressions calculated earlier. Historical generation settings are not recorded. Paths and hashes are available through FeynGravLibraryInformation[]."]}]],
        conventionSection["4. Polarisation objects", {
            conventionTable[{"Command syntax (momentum first)", "Lorentz space"}, {
                {"PolarizationVector[p, mu, phase]", "Four dimensions (FeynCalc)"},
                {"PolarizationVectorD[p, mu, phase]", "D dimensions"},
                {"PolarizationTensor[p, mu, nu, phase]", "Four dimensions"},
                {"PolarizationTensorD[p, mu, nu, phase]", "D dimensions"}}],
            conventionText["The optional phase defaults to I; -I denotes the conjugate polarisation. These are labels, not extra multiplicative factors of i. FeynGrav's shortcuts require symbolic momenta and indices."],
            conventionMath["Tensor construction", "epsilon_mu_nu(p) = epsilon_mu(p) epsilon_nu(p)",
                RowBox[{SubscriptBox["\[Epsilon]", "\[Mu]\[Nu]"], "(p)", "=", SubscriptBox["\[Epsilon]", "\[Mu]"], "(p)", SubscriptBox["\[Epsilon]", "\[Nu]"], "(p)"}]],
            conventionTable[{"Command", "Effective Transversality default"}, {
                {"PolarizationVector", conventionOption[FeynCalc`Polarization, FeynCalc`Transversality]},
                {"PolarizationVectorD", conventionOption[FeynCalc`Polarization, FeynCalc`Transversality]},
                {"PolarizationTensor", conventionOption[FeynCalc`Polarization, FeynCalc`Transversality]},
                {"PolarizationTensorD", conventionOption[FeynCalc`Polarization, FeynCalc`Transversality]}}],
            conventionText["The initial transversality default is False. All four commands inherit the effective omitted setting from Options[Polarization]. FeynGrav forwards an explicitly supplied option; when omitted, no Transversality option is inserted into Polarization. Declared defaults on the FeynGrav wrappers do not override that inherited setting. The tensor product alone enforces no helicity, tracelessness, normalisation or complete spin-2 polarisation sum."]
        }],
        conventionSection["5. Levi-Civita, Dirac and colour algebra", {
            conventionText["Epsilon tensors have exactly four slots in one Lorentz space. LCD denotes a rank-four tensor with D-dimensional slots, not a rank-D tensor."],
            conventionMath["Rank-four epsilon", "epsilon_(mu nu rho sigma)", SubscriptBox["\[Epsilon]", "\[Mu]\[Nu]\[Rho]\[Sigma]"]],
            conventionTable[{"Current session setting", "Value"}, {
                {"$LeviCivitaSign", conventionSymbolValue[FeynCalc`$LeviCivitaSign]},
                {"Default TraceOfOne", conventionOption[FeynCalc`DiracTrace, FeynCalc`TraceOfOne]},
                {"FeynCalc Dirac scheme", conventionGetter[FeynCalc`FCGetDiracGammaScheme]}}],
            conventionText["Supported epsilon-sign values are -1, 1, -I and I. Import requires the sign recorded at export, even if FORM eliminated every epsilon. Trace normalisation is captured from the effective TraceOfOne at export; a per-trace option may override its default."],
            conventionMath["Ordinary Clifford algebra", "gamma^mu gamma^nu + gamma^nu gamma^mu = 2 eta^(mu nu) 1",
                RowBox[{SuperscriptBox["\[Gamma]", "\[Mu]"], SuperscriptBox["\[Gamma]", "\[Nu]"], "+",
                    SuperscriptBox["\[Gamma]", "\[Nu]"], SuperscriptBox["\[Gamma]", "\[Mu]"], "=", "2", SuperscriptBox["\[Eta]", "\[Mu]\[Nu]"], "1"}]],
            conventionText["Matrix order is preserved with Dot. The converter supports ordinary open chains and explicit traces in one Lorentz space, but not gamma-five, chiral projectors, explicit spinor indices or external spinors. The displayed FeynCalc scheme does not imply converter support for all of that scheme's operations."],
            conventionMath["Fundamental SU(N) normalisation", "tr(T^a T^b) = delta^(ab)/2; [T^a,T^b] = i f^(abc) T^c",
                RowBox[{"tr(", SuperscriptBox["T", "a"], SuperscriptBox["T", "b"], ")", "=", FractionBox[SuperscriptBox["\[Delta]", "ab"], "2"], ";   ",
                    "[", SuperscriptBox["T", "a"], ",", SuperscriptBox["T", "b"], "]", "=", "i", SuperscriptBox["f", "abc"], SuperscriptBox["T", "c"]}]],
            conventionMath["Casimirs", "C_A = N; C_F = (N^2 - 1)/(2 N)",
                RowBox[{SubscriptBox["C", "A"], "=", "N", ";   ", SubscriptBox["C", "F"], "=", FractionBox[RowBox[{SuperscriptBox["N", "2"], "-", "1"}], "2N"]}]],
            conventionText["Colour processing uses one fundamental SU(N) group, with no physical flavour-count factor. Longer irreducible traces may remain. Multiple groups and higher representations are outside this interface."],
            conventionTable[{"Command defaults", "DiracAlgebra", "ColourAlgebra"}, {
                {"CalcFormExport", conventionOption[CalcFormConverter`CalcFormExport, CalcFormConverter`DiracAlgebra], conventionOption[CalcFormConverter`CalcFormExport, CalcFormConverter`ColourAlgebra]},
                {"CalcFormCalculate", conventionOption[CalcFormConverter`CalcFormCalculate, CalcFormConverter`DiracAlgebra], conventionOption[CalcFormConverter`CalcFormCalculate, CalcFormConverter`ColourAlgebra]}}],
            conventionText["DiracAlgebra -> Automatic enables ordinary chain simplification and explicit traces. ColourAlgebra -> True (or Automatic) enables SU(N) reduction. False requests translation only for that sector, with no added reduction procedures; native FORM identities may still apply. Per-call options may override these defaults."]
        }],
        conventionSection["6. Diagram and integral conventions", {
            conventionText["Supplied rules already include their prescribed factors of i and couplings. The converter adds no diagram symmetry factors, closed-loop signs, integration measures or renormalisation prescriptions. It performs algebraic processing, not loop integration."],
            conventionMath["Ordinary quadratic denominator", "FAD[{p,m}] represents 1/(p^2-m^2)",
                RowBox[{"FAD[{p,m}]", "\[LongRightArrow]", FractionBox["1", RowBox[{SuperscriptBox["p", "2"], "-", SuperscriptBox["m", "2"]}]]}]],
            conventionText["The legacy FAD representation does not explicitly record the sign of an infinitesimal i0 prescription. Do not interpret conversion as tracking an arbitrary prescription. Scalar A0, B0, C0 and D0 notation is translated; the converter does not choose a loop normalisation or evaluate these integrals."]
        }]
    };
    sections
];

(* ::Section:: *)
(* Static notebook and plain-text renderers *)

conventionPlain[conventionText[text_]] := text;
conventionPlain[conventionFormula[label_, plain_, _]] := label <> ": " <> plain;
conventionPlain[conventionTable[headers_, rows_]] := StringRiffle[
    StringRiffle[#, " | "] & /@ Prepend[rows, headers], "\n"];
conventionPlain[conventionSection[title_, items_]] := title <> "\n" <>
    StringRiffle[conventionPlain /@ items, "\n\n"];

conventionDisplay[conventionText[text_]] := Pane[text, ImageSize -> {600, Automatic}];
conventionDisplay[conventionFormula[label_, _, HoldComplete[boxes_]]] := Column[{
    Style[label, Italic], RawBoxes[FormBox[boxes, TraditionalForm]]}, Spacings -> .3];
conventionDisplay[conventionTable[headers_, rows_]] := With[{width = Min[300, Floor[600/Length[headers]]]},
    Grid[Prepend[Map[Pane[#, ImageSize -> {width, Automatic}] &, rows, {2}], Style[#, Bold] & /@ headers],
        Alignment -> Left, Frame -> All, Spacings -> {1, .6}]];

FeynGravConventions[] := Module[{sections = conventionReport[]},
    If[$FrontEnd === Null,
        Print["FeynGrav conventions\n\n", StringRiffle[conventionPlain /@ sections, "\n\n"]],
        Print[StandardForm[Style["FeynGrav conventions", Bold, 18]]];
        Scan[Function[section,
            Print[StandardForm[Style[section[[1]], Bold, 14]]];
            Scan[Function[item, Print[StandardForm[conventionDisplay[item]]]], section[[2]]]
        ], sections]
    ];
    Null
];
FeynGravConventions[args___] := Failure["InvalidConventionsArguments", <|
    "MessageTemplate" -> "Use FeynGravConventions[] without arguments.",
    "Function" -> "FeynGrav`FeynGravConventions",
    "ArgumentCount" -> Length[HoldComplete[args]]|>];
