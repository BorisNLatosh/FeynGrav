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
        Flatten[Map[Function[s, {
            "id " <> s <> "^cfcRPower?pos_=cfcRat(" <> s <> "^cfcRPower,1);",
            "id " <> s <> "^cfcRPower?neg_=cfcRat(1," <> s <> "^(-cfcRPower));"}], variables]],
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
