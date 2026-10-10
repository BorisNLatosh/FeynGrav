(* ::Package:: *)

(* ::Title:: *)
(* Rational simplification in the Lorentz dimension *)

(* ::Text:: *)
(* Large commuting coefficients are sums of momentum/mass monomials with
   rational functions of the dimension as coefficients. Only that univariate
   dependence enters PolyRatFun. Ordinary arithmetic is written to the result;
   the private rational function never becomes part of the saved vocabulary. *)

(* ::Section:: *)
(* Mapping-driven, univariate eligibility *)

(* Avoid extra modules for small jobs. This selects when to precondition the
   general cancellation pass; it is not a runtime or memory limit. *)
$dcfPreconditionMinimumTerms = 1000;

dcfPlan[data_Association] := Module[
    {dim = data["Mapping"]["Dimension"], entries, dimension, polynomial,
     inverses, names, integer, tag = Unique["dimensionPolynomial"]},
    If[!pgFactorisationQ[data] || !MatchQ[dim, {"Symbol", _String}], Return[<||>]];
    entries = data["Mapping"]["Entries"];
    names = Lookup[Select[entries, #["Kind"] === "Scalar" && #["Expression"] === dim &], "Name", {}];
    If[Length[names] =!= 1, Return[<||>]];
    dimension = First[names];
    integer[{"Integer", s_String}] := integerData[s];
    integer[_] := Throw[$Failed, tag];
    polynomial[e_] := Switch[First[e],
        "Symbol", If[e === dim, dimension, Throw[$Failed, tag]],
        "Integer", e[[2]],
        "Rational", "(" <> e[[2]] <> "/" <> e[[3]] <> ")",
        "Plus" | "Times", "(" <> StringRiffle[polynomial /@ Rest[e],
            If[First[e] === "Plus", "+", "*"]] <> ")",
        "Power", With[{n = integer[e[[3]]]},
            If[!Between[n, {0, 32}], Throw[$Failed, tag]];
            "(" <> polynomial[e[[2]]] <> ")^" <> intString[n]],
        _, Throw[$Failed, tag]];
    inverses = Map[Function[e, Catch[Module[{x = e["Expression"], n},
        If[First[x] =!= "Power", Throw[$Failed, tag]];
        n = integer[x[[3]]];
        If[!Between[n, {-8, -1}], Throw[$Failed, tag]];
        {e["Name"], "id " <> e["Name"] <> "=cfcDimRat(1,(" <>
            polynomial[x[[2]]] <> ")^" <> intString[-n] <> ");"}
    ], tag]], Select[entries, #["Kind"] === "Abbreviation" &]];
    inverses = DeleteCases[inverses, $Failed];
    (* Other abbreviations stay opaque. Treating them as independent scalar
       factors preserves identities, even if their hidden definitions involve D. *)
    <|"Declarations" -> "\nSymbols cfcDimPower,cfcDimNum,cfcDimDen;\nCFunction cfcDimRat;\n",
      "Check" -> "if (match(cfcDimRat(?a)) || occurs(" <> StringRiffle[Join[{dimension}, If[inverses === {}, {}, inverses[[All,1]]]], ","] <>
          "));\n$cfcDimNeeded`cfcSlot'=1;\nendif;",
      "Rules" -> StringRiffle[Join[If[inverses === {}, {}, inverses[[All,2]]], {
          "id " <> dimension <> "^cfcDimPower?pos_=cfcDimRat(" <> dimension <> "^cfcDimPower,1);",
          "id " <> dimension <> "^cfcDimPower?neg_=cfcDimRat(1," <> dimension <> "^(-cfcDimPower));",
          "Multiply cfcDimRat(1,1);"}], "\n"]|>
];

(* ::Section:: *)
(* FORM reduction and arithmetic-only output *)

dcfReduction[plan_Association] := If[plan === <||>, "",
    StringReplace[rcTemplate["DimensionCoefficients.frm.in"], "@RULES@" -> plan["Rules"]]];

dcfWrite[plan_Association, path_String, fallback_String] := If[plan === <||>, fallback,
    "#if `$cfcDimDone`cfcSlot'' == 1\n" <>
    StringReplace[rcTemplate["DimensionCoefficientOutput.frm.in"], "@RESULT@" -> path] <>
    "\n#else\n" <> fallback <> "\n#endif\n"];

(* ::Section:: *)
(* Preconditioning before denominator-basis expansion *)

(* Compress the dimension dependence before the general cancellation stage
   expands numerator powers. That stage still runs with its original rules and
   guards. Keep the rational functions opaque during cancellation; the existing
   dimension writer restores ordinary arithmetic after final grouping.
   Multivariate selection must not mistake the compressed term count for the
   complexity of the full coefficient, so these functions are marked unsafe
   for that separate factorisation path. Pure numerical ratios are unpacked. *)
dcfPrecondition[plan_Association] := If[plan === <||>, "",
    "\n* Compress dimension dependence before general numerator cancellation.\n.sort\n" <>
    "#$cfcDimInputTerms=termsin_(cfcResult);\n" <>
    "#if `$cfcDimInputTerms' > " <> intString[$dcfPreconditionMinimumTerms] <> "\n" <>
    "PolyRatFun cfcDimRat;\n" <> plan["Rules"] <>
    "\n.sort\nPolyRatFun;\n" <>
    "id cfcDimRat(cfcDimNum?number_,cfcDimDen?number_)=cfcDimNum/cfcDimDen;\n.sort\n" <>
    "#endif\n#$cfcDimInputTerms=0;\n"];

(* ::Section:: *)
(* Compact coefficients between prepared tensor stages *)

(* First sort the ordinary tensor product with rational functions inert. Only
   then enable rational arithmetic, so polynomial GCD work is done on collected
   terms rather than on every raw contraction. Disable it again before the next
   tensor multiplication. Used only for the existing prepared version-one
   stages; dimension-free and matrix/epsilon/colour jobs keep their old path. *)
dcfStageSort[plan_Association] := If[plan === <||>, ".sort\n",
    ".sort\nPolyRatFun cfcDimRat;\n" <> plan["Rules"] <>
    "\n.sort\nPolyRatFun;\n" <>
    "id cfcDimRat(cfcDimNum?number_,cfcDimDen?number_)=cfcDimNum/cfcDimDen;\n.sort\n"];
