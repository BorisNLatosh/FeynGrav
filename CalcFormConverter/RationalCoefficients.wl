(* ::Package:: *)

(* ::Title:: *)
(* Bounded rational simplification of propagator coefficients in FORM *)

(* ::Text:: *)
(* Loaded in the converter's private context. This module only describes
   algebra to be performed by FORM. It does not simplify expressions in the
   Wolfram kernel or change the mapping. Existing identifiers retain their
   meaning, including propagators, which never enter the rational algebra. *)

(* ::Section:: *)
(* Conservative eligibility and structural translation *)

(* These are work-selection limits, not time/memory guarantees. Runtime
   cancellation and TimeConstraint still apply to the complete FORM process.
   A rejected job/coefficient may use dimension-only rational arithmetic, then
   falls back to common-factor extraction and bracketing. *)
$rcMaximumTerms = 1000;
$rcMaximumVariables = 12;

(* Shared coefficient policy. The exporter has already validated dimensions,
   exactness and matrix spaces and expanded supported linear momentum routing.
   This policy reads encoded data only: it never decodes/evaluates user symbols.
   Runtime FORM inspection remains authoritative for complete coefficients and
   shared definitions. Mapping size is only the conservative propagator guard. *)
coefficientRoles[entries_List, pairPrefix_String] := Module[
    {scalars, vectors, pairs, pairNames, polarisation, polarisationNames},
    scalars = Association[Cases[entries, e_ /; e["Kind"] === "Scalar" :>
        (e["Expression"][[2]] -> e["Name"])]];
    vectors = Lookup[Select[entries, #["Kind"] === "Vector" &], "Name", {}];
    pairs = Flatten[Table[{vectors[[i]], vectors[[j]]},
        {i, Length[vectors]}, {j, i, Length[vectors]}], 1];
    pairNames = Table[pairPrefix <> intString[j], {j, Length[pairs]}];
    polarisation = Lookup[Select[entries, #["Kind"] === "Vector" &&
        MatchQ[#["Expression"], {"Polarization", ___}] &], "Name", {}];
    polarisationNames = Pick[pairNames,
        AnyTrue[#, MemberQ[polarisation, #] &] & /@ pairs];
    <|"Scalars" -> scalars, "Pairs" -> pairs, "PairNames" -> pairNames,
      "PolarisationNames" -> polarisationNames,
      "Variables" -> Join[Values[scalars], pairNames]|>
];

(* One source for stage allowances. The private legacy controls remain usable
   by focused tests. These profiles deliberately retain their different domains:
   propagator grouping cannot use the final master's shared-definition inspector.
   Limits select optional work; they neither cap memory nor catch runtime errors. *)
coefficientLimits["Propagator"] := <|"Terms" -> $rcMaximumTerms,
    "Variables" -> $rcMaximumVariables|>;
coefficientLimits["Master"] := <|"Terms" -> $rcMasterMaximumTerms,
    "Variables" -> $rcMaximumVariables, "Degree" -> $rcMasterMaximumDegree|>;
coefficientLimits["Polarisation"] := <|"Structures" -> $rcPolarisationMaximumStructures,
    "Terms" -> $rcPolarisationMaximumTerms,
    "RationalPasses" -> Max[0, $rcPolarisationMaximumRationalPasses]|>;
coefficientLimits["Factor"] := <|"Terms" -> $rcPolarisationFactorMaximumTerms,
    "Variables" -> $rcPolarisationFactorMaximumVariables,
    "Degree" -> $rcPolarisationFactorMaximumDegree|>;
coefficientLimits["Dimension"] := <|"PreconditionTerms" -> $dcfPreconditionMinimumTerms|>;
coefficientLimits["Polynomial"] := <|"Power" -> {0,32}, "InversePower" -> {-8,-1}|>;

(* Compile only the existing rational polynomial domain. A supplied scalar
   dictionary defines that domain: all commuting parameters for the general
   path, just the selected dimension for the univariate path. Unknown symbols,
   Pair components, radicals, approximate/complex leaves and opaque functions
   are ineligible here. Exact complex arithmetic remains with the final FORM
   inspector/imaginary-unit restoration; do not turn it into a real variable. *)
coefficientPolynomial[e_, scalars_Association, tag_] := Switch[First[e],
    "Symbol", Lookup[scalars, e[[2]], Throw[$Failed, tag]],
    "Integer", e[[2]],
    "Rational", "(" <> e[[2]] <> "/" <> e[[3]] <> ")",
    "Plus" | "Times", "(" <> StringRiffle[
        coefficientPolynomial[#, scalars, tag] & /@ Rest[e],
        If[First[e] === "Plus", "+", "*"]] <> ")",
    "Power", With[{n = coefficientInteger[e[[3]], tag]},
        If[!Between[n, coefficientLimits["Polynomial"]["Power"]], Throw[$Failed, tag]];
        "(" <> coefficientPolynomial[e[[2]], scalars, tag] <> ")^" <> intString[n]],
    _, Throw[$Failed, tag]
];
coefficientInteger[{"Integer", s_String}, tag_] := integerData[s];
coefficientInteger[_, tag_] := Throw[$Failed, tag];

(* No successful or failed translation is cached across jobs. Each abbreviation
   is visited once per domain. $Failed means predictable ineligibility only;
   FORM errors/aborts are never handled by this selector. *)
coefficientInverseRules[entries_List, scalars_Association, head_String] :=
    Map[Function[e, Module[{x = e["Expression"], tag = Unique["coefficient"], n},
        Catch[
            If[First[x] =!= "Power", Throw[$Failed, tag]];
            n = coefficientInteger[x[[3]], tag];
            If[!Between[n, coefficientLimits["Polynomial"]["InversePower"]], Throw[$Failed, tag]];
            {e["Name"], "id " <> e["Name"] <> "=" <> head <> "(1,(" <>
                coefficientPolynomial[x[[2]], scalars, tag] <> ")^" <> intString[-n] <> ");"},
            tag]
    ]], Select[entries, #["Kind"] === "Abbreviation" &]];

rcPlan[data_Association, requirePropagators_: True] := Module[
    {roles, pairs, names, variables, inverses, rules, limits = coefficientLimits["Propagator"]},
    If[data["Mapping"]["Version"] =!= 1 ||
        (TrueQ[requirePropagators] && !pgFactorisationQ[data]) || limits["Terms"] <= 0, Return[<||>]];
    roles = coefficientRoles[data["Mapping"]["Entries"], "cfcRX"];
    pairs = roles["Pairs"]; names = roles["PairNames"]; variables = roles["Variables"];
    If[Length[variables] > limits["Variables"], Return[<||>]];
    inverses = coefficientInverseRules[data["Mapping"]["Entries"], roles["Scalars"], "cfcRat"];
    (* All abbreviations must belong to this domain. The dimension-only pass
       instead leaves unrecognised definitions opaque, as before. *)
    If[MemberQ[inverses, $Failed], Return[<||>]];
    rules = Join[If[inverses === {}, {}, inverses[[All,2]]],
        MapThread["id " <> StringRiffle[#1, "."] <> "=" <> #2 <> ";" &, {pairs, names}],
        rcPowerRules[variables, "cfcRat"], {"Multiply cfcRat(1,1);"}];
    <|"Declarations" -> "\nSymbols " <> StringRiffle[
            Join[{"cfcRPower", "cfcRNum", "cfcRDen"}, names], ","] <>
            ";\nCFunction cfcRat;\n",
      "Rules" -> StringRiffle[rules, "\n"],
      "Restore" -> StringRiffle[MapThread[
          "id " <> #1 <> "=" <> StringRiffle[#2, "."] <> ";" &, {names, pairs}], "\n"]|>
];

(* ::Section:: *)
(* Embedded native FORM procedures *)

rcTemplate[name_String] := Module[{text},
    text = Quiet[Check[Import[FileNameJoin[{$moduleDirectory, "Templates", name}], "Text"], $Failed]];
    If[!StringQ[text], throwFailure["MissingTemplate", "Cannot read the rational coefficient template."]];
    text
];
rcReduction[plan_Association] := If[plan === <||>, "",
    StringReplace[rcTemplate["RationalCoefficients.frm.in"], {
        "@LIMIT@" -> intString[coefficientLimits["Propagator"]["Terms"]], "@RULES@" -> plan["Rules"]}]];
rcWrite[plan_Association, path_String, fallback_String] := If[plan === <||>, fallback,
    "#if `$cfcRDone`cfcSlot'' == 1\n" <>
    StringReplace[rcTemplate["RationalCoefficientOutput.frm.in"], {
        "@RESULT@" -> path, "@RESTORE@" -> plan["Restore"]}] <>
    "\n#else\n" <> fallback <> "\n#endif\n"];

(* The two callers share native rational arithmetic, but select work differently.
   Propagator coefficients keep their historical guards. Final master coefficients
   inspect the actual shared definitions in FORM before exposing their algebra. *)
rcPowerRules[variables_List, head_String] := Flatten[Map[Function[s, {
    "id " <> s <> "^cfcRPower?pos_=" <> head <> "(" <> s <> "^cfcRPower,1);",
    "id " <> s <> "^cfcRPower?neg_=" <> head <> "(1," <> s <> "^(-cfcRPower));"}], variables]];

$rcMasterMaximumTerms = 1000;
$rcMasterMaximumDegree = 24;
rcMasterPlan[entries_List] := Module[{roles, variables, pairs, names, powers, inspect, polNames, polPlan,
    limits = coefficientLimits["Master"]},
    If[limits["Terms"] <= 0, Return[<||>]];
    roles = coefficientRoles[entries, "cfcMRX"];
    pairs = roles["Pairs"]; names = roles["PairNames"];
    polNames = roles["PolarisationNames"]; variables = roles["Variables"];
    polPlan = rcPolarisationPlan[pairs, names, polNames, variables];
    powers = StringRiffle[rcPowerRules[Append[variables,"cfcMRImag"],"cfcMRat"],"\n"];
    inspect = StringJoin[Map[Function[s,
        "id " <> s <> "^cfcRPower?pos_=" <> s <> "*cfcMRDegree^cfcRPower;\n" <>
        "id " <> s <> "^cfcRPower?neg_=" <> s <> "*cfcMRDegree^(-cfcRPower);\n"], variables]];
    <|"Declarations" -> "\nSymbols cfcRPower,cfcMRIndex,cfcMRN,cfcMRD,cfcMRDegree,cfcMRImag" <>
            If[names === {},"",","<>StringRiffle[names,","]] <> ";\nCFunctions cfcMRat,cfcMRDenProbe;\nTable sparse cfcMRMeta(1);\n" <> Lookup[polPlan, "Declarations", ""],
      "Polarisation" -> polPlan,
      "Procedure" -> StringReplace[rcTemplate["MasterRationalCoefficients.frm.in"], {
        "@LIMIT@" -> intString[limits["Terms"]], "@DEGREE@" -> intString[limits["Degree"]],
        "@VARIABLE_LIMIT@" -> intString[limits["Variables"]], "@VARIABLES@" -> StringRiffle[variables,","],
        "@DOTS@" -> StringRiffle[MapThread["id "<>StringRiffle[#1,"."]<>"^cfcRPower?="<>#2<>"^cfcRPower;"&,{pairs,names}],"\n"],
        "@NATIVE_DEN_INSPECT@" -> If[polPlan === <||>, "", "id denom_(?a)=cfcMRDenProbe(?a);\nArgument cfcMRDenProbe;\nDropCoefficient;\nid i_=1;\n" <>
            StringRiffle[MapThread["id "<>StringRiffle[#1,"."]<>"^cfcRPower?="<>#2<>"^cfcRPower;"&,{pairs,names}],"\n"] <>
            "\n" <> inspect <> "EndArgument;\nid cfcMRDenProbe(cfcMRN?)=cfcMRN;\n"],
        "@NATIVE_DEN_RATIONAL@" -> If[polPlan === <||>, "", "id denom_(?a)=cfcMRat(1,?a);\n"],
        "@INSPECT@" -> inspect, "@POWERS@" -> powers,
        "@RESTORE@" -> StringRiffle[MapThread["id "<>#1<>"="<>StringRiffle[#2,"."]<>";"&,{names,pairs}],"\n"],
        "@FACTOR@" -> If[polPlan === <||>, "", "#call cfcPGFactor\n"]}]|>
];


(* Polarisation roles come from the validated dictionary, never identifier names.
   No grouping data is persisted in mappings: all tables live in one FORM job. *)
$rcPolarisationMaximumStructures = 1024;
$rcPolarisationMaximumTerms = 10000;
(* Total speculative rational passes across all polarisation groups in one job.
   Exhaustion restores the complete original master; it is not a runtime limit. *)
$rcPolarisationMaximumRationalPasses = 1024;
$rcPolarisationFactorMaximumTerms = 250;
$rcPolarisationFactorMaximumVariables = 8;
$rcPolarisationFactorMaximumDegree = 12;
rcPolarisationPlan[pairs_List, names_List, polNames_List, variables_List] := Module[
    {polPairs, plain, substitutions, limits = coefficientLimits["Polarisation"],
     factorLimits = coefficientLimits["Factor"]},
    If[polNames === {} || limits["Structures"] <= 0, Return[<||>]];
    polPairs = Pick[pairs, MemberQ[polNames, #] & /@ names];
    plain = Complement[variables, polNames];
    substitutions = {
        "@POL@" -> StringRiffle[polNames, ","],
        "@STRUCTURES@" -> intString[limits["Structures"]],
        "@WORK_LIMIT@" -> intString[limits["RationalPasses"]],
        "@TERMS@" -> intString[limits["Terms"]],
        "@FACTOR_TERMS@" -> intString[factorLimits["Terms"]],
        "@FACTOR_VARIABLES@" -> intString[factorLimits["Variables"]],
        "@FACTOR_DEGREE@" -> intString[factorLimits["Degree"]],
        "@POL_DOTS@" -> StringRiffle[MapThread[
            "id " <> StringRiffle[#1, "."] <> "^cfcRPower?=" <> #2 <> "^cfcRPower;" &,
            {polPairs, polNames}], "\n"],
        "@ALL_DOTS@" -> StringRiffle[MapThread[
            "id " <> StringRiffle[#1, "."] <> "^cfcRPower?=" <> #2 <> "^cfcRPower;" &,
            {pairs, names}], "\n"],
        "@POL_RESTORE@" -> StringRiffle[MapThread[
            "id " <> #1 <> "=" <> StringRiffle[#2, "."] <> ";" &,
            {polNames, polPairs}], "\n"],
        "@PLAIN@" -> StringRiffle[plain, ","],
        "@DENOMINATOR_KEYS@" -> StringJoin[("id " <> # <> "^cfcRPower?neg_=cfcPGInverse(" <> # <> ")^(-cfcRPower);\n" &) /@
            Join[Complement[variables, names], ("(" <> StringRiffle[#, "."] <> ")" &) /@
                Pick[pairs, !MemberQ[polNames, #] & /@ names]]],
        "@ATOM_COUNT@" -> StringRiffle[("theta_(abs_(count_(" <> # <> ",1))-1)+theta_(abs_(count_(" <> # <> ",1))-2)" &) /@
            variables, "+"] <> "+count_(cfcShare,1,cfcPolyN,1,cfcPolyD,1)"};
    <|"Declarations" -> "\nSymbols cfcPGHas,cfcPGFraction,cfcPGBad,cfcPGIndex;\n" <>
        "CFunctions cfcPGTag,cfcPGRef,cfcPGCollect,cfcPGCollectPart,cfcPGInverse,cfcPGDenProbe,cfcPGReject,cfcPGRejectPart;\n" <>
        "Table sparse cfcPGSize(1);\nTable sparse cfcPGDenOK(1);\nTable sparse cfcPGParts(1);\nTable sparse cfcPGReach(1);\nTable sparse cfcPGTerms(1);\nTable sparse cfcPGAtoms(1);\nTable sparse cfcPGRejectCost(1);\nTable sparse cfcPGRejectDegree(1);\nTable sparse cfcPGRejectVariables(1);\nTable sparse cfcPGResult(1);\nTable sparse cfcPGResultReady(1);\nTable sparse cfcPGInspectOK(1);\nTable sparse cfcPGInspectMeta(1);\nTable sparse cfcPGAlgebra(2);\nTable sparse cfcPGAlgebraReady(2);\nTable sparse cfcPGDenValue(1);\nTable sparse cfcPGDenReady(1);\n",
      "Procedure" -> StringReplace[rcTemplate["PolarisationCoefficients.frm.in"], substitutions]|>
];
