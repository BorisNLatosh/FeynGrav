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

rcPlan[data_Association] := Module[
    {entries, scalars, vectors, pairs, inverses, polynomial, integer, rules,
     names, variables, tag = Unique["rcPlan"]},
    If[!pgFactorisationQ[data] || $rcMaximumTerms <= 0, Return[<||>]];
    entries = data["Mapping"]["Entries"];
    scalars = Association[Cases[entries, e_ /; e["Kind"] === "Scalar" :>
        (e["Expression"][[2]] -> e["Name"])]];
    vectors = Lookup[Select[entries, #["Kind"] === "Vector" &], "Name", {}];
    pairs = Flatten[Table[{vectors[[i]], vectors[[j]]},
        {i, Length[vectors]}, {j, i, Length[vectors]}], 1];
    names = Table["cfcRX" <> IntegerString[i], {i, Length[pairs]}];
    variables = Join[Values[scalars], names];
    If[Length[variables] > $rcMaximumVariables, Return[<||>]];
    (* Read the already encoded, validated export vocabulary. Symbol contexts
       are compared as strings; nothing is re-evaluated through ToExpression. *)
    integer[{"Integer", s_String}] := If[StringStartsQ[s, "-"],
        -FromDigits[StringDrop[s, 1]], FromDigits[s]];
    integer[_] := Throw[$Failed, tag];
    polynomial[e_] := Switch[First[e],
        "Symbol", Lookup[scalars, e[[2]], Throw[$Failed, tag]],
        "Integer", e[[2]],
        "Rational", "(" <> e[[2]] <> "/" <> e[[3]] <> ")",
        "Plus" | "Times", "(" <> StringRiffle[polynomial /@ Rest[e],
            If[First[e] === "Plus", "+", "*"]] <> ")",
        "Power", With[{n = integer[e[[3]]]},
            If[!Between[n, {0, 32}], Throw[$Failed, tag]];
            "(" <> polynomial[e[[2]]] <> ")^" <> IntegerString[n]],
        _, Throw[$Failed, tag]];
    inverses = Catch[Map[Function[e, Module[{x = e["Expression"], n},
        If[First[x] =!= "Power", Throw[$Failed, tag]];
        n = integer[x[[3]]];
        If[!Between[n, {-8, -1}], Throw[$Failed, tag]];
        "id " <> e["Name"] <> "=cfcRat(1,(" <> polynomial[x[[2]]] <>
            ")^" <> IntegerString[-n] <> ");"
    ]], Select[entries, #["Kind"] === "Abbreviation" &]], tag];
    (* An opaque radical/function may be algebraically related to other atoms.
       Until that vocabulary is tested, retain the original path for the job. *)
    If[inverses === $Failed, Return[<||>]];
    rules = Join[inverses,
        MapThread["id " <> StringRiffle[#1, "."] <> "=" <> #2 <> ";" &, {pairs, names}],
        rcPowerRules[variables, "cfcRat"],
        {"Multiply cfcRat(1,1);"}];
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
        "@LIMIT@" -> IntegerString[$rcMaximumTerms], "@RULES@" -> plan["Rules"]}]];
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
rcMasterPlan[entries_List] := Module[{variables, vectors, pairs, names, powers, inspect, polarisation, polNames, polPlan},
    If[$rcMasterMaximumTerms <= 0, Return[<||>]];
    variables = Lookup[Select[entries, #["Kind"] === "Scalar" &], "Name", {}];
    vectors = Lookup[Select[entries, #["Kind"] === "Vector" &], "Name", {}];
    pairs = Flatten[Table[{vectors[[i]],vectors[[j]]},{i,Length[vectors]},{j,i,Length[vectors]}],1];
    names = Table["cfcMRX" <> intString[j],{j,Length[pairs]}];
    polarisation = Lookup[Select[entries, #["Kind"] === "Vector" &&
        MatchQ[#["Expression"], {"Polarization", ___}] &], "Name", {}];
    polNames = Pick[names, AnyTrue[#, MemberQ[polarisation, #] &] & /@ pairs];
    polPlan = rcPolarisationPlan[pairs, names, polNames, Join[variables, names]];
    variables = Join[variables,names];
    powers = StringRiffle[rcPowerRules[Append[variables,"cfcMRImag"],"cfcMRat"],"\n"];
    inspect = StringJoin[Map[Function[s,
        "id " <> s <> "^cfcRPower?pos_=" <> s <> "*cfcMRDegree^cfcRPower;\n" <>
        "id " <> s <> "^cfcRPower?neg_=" <> s <> "*cfcMRDegree^(-cfcRPower);\n"], variables]];
    <|"Declarations" -> "\nSymbols cfcRPower,cfcMRIndex,cfcMRN,cfcMRD,cfcMRDegree,cfcMRImag" <>
            If[names === {},"",","<>StringRiffle[names,","]] <> ";\nCFunctions cfcMRat,cfcMRDenProbe;\nTable sparse cfcMRMeta(1);\n" <> Lookup[polPlan, "Declarations", ""],
      "Polarisation" -> polPlan,
      "Procedure" -> StringReplace[rcTemplate["MasterRationalCoefficients.frm.in"], {
        "@LIMIT@" -> intString[$rcMasterMaximumTerms], "@DEGREE@" -> intString[$rcMasterMaximumDegree],
        "@VARIABLE_LIMIT@" -> intString[$rcMaximumVariables], "@VARIABLES@" -> StringRiffle[variables,","],
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
$rcPolarisationFactorMaximumTerms = 250;
$rcPolarisationFactorMaximumVariables = 8;
$rcPolarisationFactorMaximumDegree = 12;
rcPolarisationPlan[pairs_List, names_List, polNames_List, variables_List] := Module[
    {polPairs, plain, substitutions},
    If[polNames === {} || $rcPolarisationMaximumStructures <= 0, Return[<||>]];
    polPairs = Pick[pairs, MemberQ[polNames, #] & /@ names];
    plain = Complement[variables, polNames];
    substitutions = {
        "@POL@" -> StringRiffle[polNames, ","],
        "@STRUCTURES@" -> intString[$rcPolarisationMaximumStructures],
        "@TERMS@" -> intString[$rcPolarisationMaximumTerms],
        "@FACTOR_TERMS@" -> intString[$rcPolarisationFactorMaximumTerms],
        "@FACTOR_VARIABLES@" -> intString[$rcPolarisationFactorMaximumVariables],
        "@FACTOR_DEGREE@" -> intString[$rcPolarisationFactorMaximumDegree],
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
