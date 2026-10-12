(* ::Package:: *)

(* ::Title:: *)
(* Denominator groups inside reduced integral multipliers *)

(* ::Text:: *)
(* This companion is loaded in the converter's private context. The planner
   describes exact rational abbreviations without evaluating their algebra.
   FORM exposes their factors and collects complete denominator products. A
   private reciprocal marker keeps polynomial denominators out of numerators;
   reconstruction removes that marker and returns ordinary Wolfram arithmetic. *)

(* ::Section:: *)
(* Mapping-driven rational vocabulary *)

(* FORM matches a powered scalar product as p.q^n?, not (p.q)^n?.
   Keeping the latter parentheses on a left-hand side silently leaves inverse
   scalar products in numerator arguments. Extract them before common-factor
   processing, which must receive polynomial arguments. *)
ldgNegativePowerRules[atoms_List] := StringJoin[Map[Function[atom,
    "id " <> If[StringStartsQ[atom, "("], StringTake[atom, {2, -2}], atom] <>
    "^cfcDGPower?neg_=cfcLRDen(" <> atom <> ")^(-cfcDGPower);\n"], atoms]];

ldgPlan[inputEntries_List] := Module[
    {entries = inputEntries, register, inspect, names, vectorLookup, read, integer, tag = Unique["denominatorGroup"], rules, scalarNames,
     vectorNames, products, denominators, cleanup, table, replacements},
    (* Rational abbreviations can contain symbols never emitted separately.
       Register those leaves by role before writing native FORM arithmetic. *)
    register[kind_, e_, prefix_] := Module[{found, serial},
        found = SelectFirst[entries, #["Kind"] === kind && #["Expression"] === e &, None];
        If[found =!= None, Return[Null]];
        serial = 1 + Max[Prepend[FromDigits[StringDrop[#, StringLength[prefix]]] & /@
            Select[Lookup[entries, "Name"], StringMatchQ[#, prefix ~~ DigitCharacter..] &], 0]];
        AppendTo[entries, <|"Name" -> prefix <> intString[serial], "Kind" -> kind,
            "Expression" -> e|>]];
    inspect[e_] := Switch[First[e],
        "Symbol", register["Scalar", e, "cfs"],
        "Plus" | "Times", Scan[inspect, Rest[e]],
        "Power", inspect[e[[2]]],
        "Pair", If[MatchQ[Rest[e], {{"Momentum", {"Symbol", _String}, ___},
                {"Momentum", {"Symbol", _String}, ___}}],
            Scan[(register["Vector", #[[2]], "cfv"] &), Rest[e]]],
        _, Null];
    Scan[inspect[#["Expression"]] &, Select[inputEntries, #["Kind"] === "Abbreviation" &]];
    names = Association[(With[{e = #["Expression"]}, HoldComplete[e]] -> #["Name"] &) /@
        Select[entries, #["Kind"] === "Scalar" &]];
    vectorLookup = Association[(With[{e = #["Expression"]}, HoldComplete[e]] -> #["Name"] &) /@
        Select[entries, #["Kind"] === "Vector" &]];
    integer[{"Integer", n_String}] := integerData[n];
    integer[_] := Throw[$Failed, tag];
    read[e_, polynomial_: False] := Switch[First[e],
        "Integer", e[[2]],
        "Rational", "(" <> e[[2]] <> "/" <> e[[3]] <> ")",
        "Complex", "(" <> read[e[[2]], polynomial] <> "+i_*" <> read[e[[3]], polynomial] <> ")",
        "Symbol", Lookup[names, With[{v = e}, HoldComplete[v]], Throw[$Failed, tag]],
        "Plus" | "Times",
            If[First[e] === "Plus" && !polynomial, "cfcLRNum", ""] <>
            "(" <> StringRiffle[(read[#, polynomial || First[e] === "Plus"] &) /@ Rest[e],
                If[First[e] === "Plus", "+", "*"]] <> ")",
        "Power", With[{n = integer[e[[3]]]},
            If[polynomial && n < 0, Throw[$Failed, tag]];
            With[{base = read[e[[2]], polynomial || n < 0]},
            If[n < 0, "cfcLRDen(" <> base <> ")^" <> intString[-n],
                "(" <> base <> ")^" <> intString[n]]]],
        "Pair", If[!MatchQ[Rest[e], {{"Momentum", _, ___}, {"Momentum", _, ___}}],
            Throw[$Failed, tag]];
            "(" <> StringRiffle[(With[{v = #[[2]]},
                Lookup[vectorLookup, HoldComplete[v], Throw[$Failed, tag]]] &) /@ Rest[e], "."] <> ")",
        _, Throw[$Failed, tag]];
    rules = Map[Function[e, With[{body = Catch[read[e["Expression"]], tag]},
        If[body === $Failed, Nothing, {e["Name"], body}]]],
        Select[entries, #["Kind"] === "Abbreviation" &]];
    scalarNames = Lookup[Select[entries, #["Kind"] === "Scalar" &], "Name", {}];
    vectorNames = Lookup[Select[entries, #["Kind"] === "Vector" &], "Name", {}];
    products = ("(" <> StringRiffle[#, "."] <> ")" &) /@
        Flatten[Table[Table[{vectorNames[[i]], vectorNames[[j]]},
            {j, i, Length[vectorNames]}], {i, Length[vectorNames]}], 1];
    denominators = Lookup[Select[entries, #["Kind"] === "Denominator" &], "Name", {}];
    (* Normalise the small abbreviation dictionary once. Repeating multivariate
       FactArg on every amplitude term is prohibitively expensive. *)
    cleanup = "id cfcLRDen(cfcDGNum?number_)=1/cfcDGNum;\n" <>
        "id cfcLRNum(cfcDGNum?number_)=cfcDGNum;\n" <>
        "repeat;\nid cfcLRNum(cfcDGNum?)*cfcLRDen(cfcDGNum?)=1;\nendrepeat;\n" <>
        "id cfcLRNum(cfcDGNum?symbol_)=cfcDGNum;\n" <>
        StringJoin[("id cfcLRNum(" <> # <> ")=" <> # <> ";\n") & /@ products] <>
        "repeat;\n" <> StringJoin[("id " <> # <> "*cfcLRDen(" <> # <>
            ")=1;\n") & /@ Join[scalarNames, products]] <> "endrepeat;\n";
    table = If[rules === {}, "0", StringRiffle[MapIndexed[
        ("cfcDGEntry(" <> intString[First[#2]] <> ")*(" <> #1[[2]] <> ")") &, rules], "+\n"]];
    replacements = StringJoin[MapIndexed[
        ("id " <> #1[[1]] <> "=cfcDGTable[cfcDGEntry(" <> intString[First[#2]] <> ")];\n") &, rules]];
    <|"Entries" -> entries,
      "Indices" -> Lookup[Select[entries, #["Kind"] === "Index" &], "Name", {}],
      "Denominators" -> denominators, "Atoms" -> Join[scalarNames,products],
      "Products" -> products,
      "Declarations" -> "\nVectors cfcDGVector1,cfcDGVector2;\nTable sparse cfcKnown(1);\nIndices cfcDGIndex1,cfcDGIndex2;\nCFunctions cfcDGMetric,cfcPolyN,cfcPolyD,cfcAtom,cfcBlock,cfcNum,cfcNumPart,cfcWeight,cfcWeightPart,cfcOriginal,cfcRef,cfcFinal,cfcFinalPart;\nSymbols cfcDGPower,cfcDGNum,cfcDGDen;\n" <>
        "CFunctions cfcLRDen,cfcLRNum,cfcDimRat,cfcDGEntry,cfcDGDimN,cfcDGDimD;\n",
      "Preparation" -> ".sort\nHide cfcResult;\nLocal cfcDGTable=" <> table <> ";\n" <>
        "FactArg cfcLRDen;\nChainOut cfcLRDen;\nFactArg cfcLRNum;\nChainOut cfcLRNum;\n" <>
        cleanup <> "Bracket+ cfcDGEntry;\n.sort\nHide cfcDGTable;\nUnhide cfcResult;\nBracket+ cfcTC;\n.sort\n",
      "Rules" -> replacements <>
        "id cfcDimRat(cfcDGNum?,cfcDGDen?)=cfcDGDimN(cfcDGNum)*cfcDGDimD(cfcDGDen);\n" <>
        "FactArg cfcDGDimN;\nChainOut cfcDGDimN;\nFactArg cfcDGDimD;\nChainOut cfcDGDimD;\n" <>
        "id cfcDGDimN(cfcDGNum?)=cfcLRNum(cfcDGNum);\nid cfcDGDimD(cfcDGDen?)=cfcLRDen(cfcDGDen);\n" <>
        ldgNegativePowerRules[Join[scalarNames, products]] <> cleanup,
      "IntegralBracket" -> "Bracket cfcTC," <> StringRiffle[Prepend[denominators, "cfcLRDen"], ","] <> ";\n",
      "Bracket" -> "Bracket " <> StringRiffle[Prepend[denominators, "cfcLRDen"], ","] <> ";\n"|>
];

(* ::Section:: *)
(* Restricted reconstruction *)

ldgScalarCall[args_List, inverse_] := Module[{polynomial, value},
    polynomial[x_] := Which[
        IntegerQ[x] || Head[x] === Rational || Head[x] === Symbol, True,
        Head[x] === Complex, AllTrue[{Re[x], Im[x]}, MatchQ[#, _Integer | _Rational] &],
        MatchQ[x, Pair[_Momentum, _Momentum]], True,
        Head[x] === Plus || Head[x] === Times, AllTrue[List @@ x, polynomial],
        Head[x] === Power, IntegerQ[x[[2]]] && x[[2]] >= 0 && polynomial[x[[1]]],
        True, False];
    If[!TrueQ[Lookup[$tdImport, "DenominatorGroups", False]] || Length[args] =!= 1,
        throwFailure["InvalidResult", "Invalid scalar factor in a reduced integral multiplier."]];
    value = First[args];
    (* A shared factor is resolved only after the definition preamble has been
       validated. Tensor blocks cannot be used as polynomial denominators. *)
    If[Head[value] === lrSharedToken,
        If[!AssociationQ[$lrSharedValues], throwFailure["InvalidResult", "Missing scalar factor dictionary."]];
        value = Lookup[$lrSharedValues, First[value],
            throwFailure["InvalidResult", "Unknown shared scalar factor."]]];
    If[!polynomial[value] || (inverse && value === 0),
        throwFailure["InvalidResult", "Invalid rational denominator in a reduced integral multiplier."]];
    If[inverse, 1/value, value]
];

(* ::Section:: *)
(* Shared numerator output *)
(* The two input dictionaries remain separate until final integral assembly.
   Only denominator factors are exposed across that join. FORM owns all
   coefficient arithmetic and common-factor extraction. *)
(* OldFactArg can extract an incorrect dot-product power when its
   arguments mix scalar products with several indexed function factors.
   The polynomial round trip shields those objects while keeping this a
   cheap common-content extraction rather than full factorisation. *)
ldgCommonFactors[first_String, second_String, indices_List: {}] := Module[{guard, end, protect, restore},
    (* ToPolynomial rejects free Lorentz indices. Leave those numerator
       arguments intact: collection and sharing still apply, but optional
       common-content extraction is skipped for tensor-bearing terms. *)
    guard = If[indices === {}, "", "if (occurs(" <> StringRiffle[Prepend[indices, "cfcDGMetric"], ","] <> ")==0);\n"];
    end = If[indices === {}, "", "endif;\n"];
    (* FORM's occurs condition does not see indices stored in native d_.
       Expose that special object through a private function for the guard,
       then restore it before any definition is shared or written. *)
    protect = If[indices === {}, "", "Argument " <> first <> "," <> second <>
        ";\nid d_(cfcDGIndex1?,cfcDGIndex2?)=cfcDGMetric(cfcDGIndex1,cfcDGIndex2);\nEndArgument;\n"];
    restore = If[indices === {}, "", "Argument " <> first <> "," <> second <>
        ";\nid cfcDGMetric(cfcDGIndex1?,cfcDGIndex2?)=d_(cfcDGIndex1,cfcDGIndex2);\nEndArgument;\n"];
    StringJoin[
        protect, guard, "Argument ", first, ",", second, ";\nToPolynomial;\nEndArgument;\n", end, ".sort\n",
        "On OldFactArg;\n", guard, "FactArg ", first, ";\nFactArg ", second, ";\n",
        "ChainOut ", first, ";\nChainOut ", second, ";\nFromPolynomial;\n",
        "Argument ", first, ",", second, ";\nFromPolynomial;\nEndArgument;\n", end, restore]
];

ldgSharedOutput[plan_Association,path_String] := Module[{den=plan["Denominators"], substitutions, preparation},
    preparation=plan["MonomialRules"]<>".sort\n"<>
        If[plan["Dimension"]===<||>,"","PolyRatFun cfcDimRat;\n"<>plan["Dimension"]["Rules"]<>"\n.sort\nPolyRatFun;\n"];
    substitutions={"@RESULT@"->path,"@NORMALISE@"->den["Rules"],
        "@WEIGHT_PREPARATION@"->preparation,
        "@PROPAGATORS@"->StringRiffle[den["Denominators"],","],
        "@NEGATIVE_POWERS@"->ldgNegativePowerRules[den["Atoms"]]};
    substitutions=Join[substitutions,
        ("@COMMON_FACTOR_" <> First[#] <> "@" -> ldgCommonFactors[#[[1]], #[[2]], den["Indices"]]) & /@
            {{"cfcNum", "cfcNumPart"}, {"cfcWeight", "cfcWeightPart"}, {"cfcFinal", "cfcFinalPart"}}];
    substitutions=Join[substitutions,Map[Function[head,
        ("@UNWRAP_"<>head<>"@") -> StringJoin[("id "<>head<>"("<>#<>")="<>#<>";\n"&)/@den["Products"]]],
        {"cfcNum","cfcNumPart","cfcWeight","cfcWeightPart","cfcFinal","cfcFinalPart"}]];
    (* Empty spectator dictionaries must not leave a trailing declaration comma. *)
    StringReplace[StringReplace[rcTemplate["LoopDenominatorGroups.frm.in"],substitutions],",;"->";"]
];
