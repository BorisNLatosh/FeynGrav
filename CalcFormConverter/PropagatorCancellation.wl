(* ::Package:: *)

(* ::Title:: *)
(* Algebraic numerator cancellation against ordinary propagators *)

(* ::Text:: *)
(* Loaded in the private converter context. Only small exact rational routing
   matrices are processed in Wolfram. FORM performs numerator substitutions,
   expansion and cancellation. There are no integral identities, momentum
   shifts, kinematic assumptions or mass-difference divisions here. *)

(* ::Section:: *)
(* Bounded generation of independent denominator bases *)

(* A bound on generated programme branches, not on FORM execution time. Jobs
   exceeding it retain the existing algebra unchanged. Every basis consists
   only of denominators present in the term being processed. *)
$pcMaximumBases = 256;

pcPlan[data_Association] := Module[
    {specs = Lookup[data, "CancellationData", {}], pairs, aliases, rows, bases,
     candidates, matrix, rank, columns, inverse, rules, branches, declarations,
     ration, sum, names, polynomials, processing, routing, coefficients, loops,
     loopNames, entries, orderedPairs, groupCount},
    If[specs === {} || $pcMaximumBases < 1, Return[<||>]];
    entries = data["Mapping"]["Entries"];
    loops = data["Mapping"]["LoopMomenta"];
    loopNames = Lookup[Select[entries, #["Kind"] === "Vector" &&
        MemberQ[loops, #["Expression"]] &], "Name", {}];
    pairs = Union[Flatten[Map[Function[s, With[{v = s["Routing"][[All, 2]]},
        Sort /@ Tuples[v, 2]]], specs], 1]];
    (* Prefer loop squares, then loop/external products when loops are supplied.
       With no loop declarations all routing scalar products remain eligible;
       this is still an exact rational identity and does not infer integration. *)
    orderedPairs = SortBy[pairs, {(-Count[#, x_ /; MemberQ[loopNames, x]]) &,
        If[#[[1]] === #[[2]], 0, 1] &, Identity}];
    pairs = orderedPairs;
    aliases = Table["cfcPCX" <> IntegerString[i], {i, Length[pairs]}];
    names = Lookup[specs, "Name"];
    rows = Map[Function[s,
        routing = s["Routing"];
        coefficients = GroupBy[Flatten[Table[
            {Sort[{u[[2]], v[[2]]}], u[[1]] v[[1]]},
            {u, routing}, {v, routing}], 1], First -> Last, Total];
        Lookup[coefficients, pairs, 0]
    ], specs];
    (* Construct only independent subsets. Rank tests involve rational routing
       coefficients, never masses, D, external invariants or large numerators. *)
    bases = {{}};
    Do[
        candidates = Select[Append[#, i] & /@ bases,
            MatrixRank[rows[[#]]] === Length[#] &];
        bases = Join[bases, candidates];
        If[Length[bases] - 1 > $pcMaximumBases, Return[<||>]],
        {i, Length[specs]}];
    bases = SortBy[Rest[bases], {-Length[#] &, Identity}];
    If[bases === {}, Return[<||>]];
    ration[x_Integer] := intString[x];
    ration[x_Rational] := "(" <> intString[Numerator[x]] <> "/" <>
        IntegerString[Denominator[x]] <> ")";
    sum[x_List] := If[x === {}, "0", "(" <> StringRiffle[x, "+"] <> ")"];
    polynomials = MapThread[sum[Join[
        MapThread[If[#1 === 0, Nothing, ration[#1] <> "*" <> #2] &, {#1, aliases}],
        {"-(" <> #2["MassSquared"] <> ")"}]] &, {rows, specs}];
    branches = MapIndexed[Function[{basis, index},
        matrix = rows[[basis]];
        rank = Length[basis];
        (* Greedy independent columns define a deterministic invertible square
           matrix. Pivot expressions contain only NON-pivot scalar products, so
           the emitted id statements cannot rewrite one another cyclically. *)
        columns = {};
        Do[If[MatrixRank[matrix[[All, Append[columns, j]]]] > Length[columns],
            AppendTo[columns, j]], {j, Length[pairs]}];
        inverse = Inverse[matrix[[All, columns]]];
        rules = Table[
            "id " <> aliases[[columns[[i]]]] <> "=" <> sum[Join[
                Table[If[inverse[[i,j]] === 0, Nothing,
                    ration[inverse[[i,j]]] <> "*(" <> names[[basis[[j]]]] <>
                    "^-1+(" <> specs[[basis[[j]]]]["MassSquared"] <> "))"], {j, rank}],
                Table[With[{c = -(inverse.matrix)[[i,j]]},
                    If[MemberQ[columns,j] || c === 0, Nothing,
                        ration[c] <> "*" <> aliases[[j]]]], {j, Length[pairs]}]
            ]] <> ";", {i, rank}];
        If[First[index] === 1, "if (", "elseif ("] <>
            StringRiffle[("count(" <> names[[#]] <> ",1)>0") & /@ basis, " && "] <>
            ");\n" <> StringRiffle[rules, "\n"]
    ], bases];
    declarations = "\nSymbols cfcPCPower," <> StringRiffle[aliases, ","] <> ";\n";
    groupCount[label_String] := "Bracket+ " <> StringRiffle[names, ","] <>
        ";\n.sort\n#$cfcPCKeys=0;\nKeep Brackets;\n$cfcPCKeys=$cfcPCKeys+term_;\n" <>
        "ModuleOption noparallel;\n.sort\n#$" <> label <> "=termsin_($cfcPCKeys);\n";
    processing = "\n* Exact numerator cancellation; no shifts or scaleless removal.\n.sort\n" <>
        groupCount["cfcPCGroupsBefore"] <> "#$cfcPCBefore=termsin_(cfcResult);\nLocal cfcPCBackup=cfcResult;\n.sort\nHide cfcPCBackup;\n.sort\n" <>
        StringRiffle[MapThread["id " <> StringRiffle[#1, "."] <> "=" <> #2 <> ";" &,
            {pairs, aliases}], "\n"] <> "\n" <> StringRiffle[branches, "\n"] <>
        "\nendif;\n" <>
        (* Negative powers are temporary numerator polynomials, not propagators.
           Restore them before output, preserving unmatched polynomial powers. *)
        StringRiffle[MapThread["id " <> #1 <> "^cfcPCPower?neg_=" <> #2 <>
            "^(-cfcPCPower);" &, {names, polynomials}], "\n"] <> "\n" <>
        StringRiffle[MapThread["id " <> #1 <> "=" <> StringRiffle[#2, "."] <> ";" &,
            {aliases, pairs}], "\n"] <> "\n.sort\n" <>
        (* A valid cancellation can enlarge a polynomial. Preserve the complete
           original when the sorted candidate exceeds both sixteen terms and twice
           the original term count, or quadruples the denominator group count.
           These structural guards do not guarantee smaller factored output. The two paths
           rejoin before grouping, with only cfcResult active. *)
        groupCount["cfcPCGroupsAfter"] <> "#$cfcPCAfter=termsin_(cfcResult);\n" <>
        "#if ( ( `$cfcPCAfter' > 16 ) && ( `$cfcPCAfter' > {2*`$cfcPCBefore'} ) ) || ( `$cfcPCGroupsAfter' > {4*`$cfcPCGroupsBefore'} )\n" <>
        "Drop cfcResult;\n.sort\nUnhide cfcPCBackup;\nDrop cfcPCBackup;\nLocal cfcResult=cfcPCBackup;\n" <>
        ".sort\n#else\nUnhide cfcPCBackup;\nDrop cfcPCBackup;\n.sort\n#endif\n";
    <|"Declarations" -> declarations, "Processing" -> processing|>
];

(* ::Section:: *)
(* Direct massless monomial cancellation *)

(* A denominator with routing a p and zero mass is 1/(a^2 p^2). Removing a
   matching numerator p^2 creates no new terms. Run this BEFORE the general
   basis stage: its growth fallback must keep these inexpensive cancellations.
   Multiple routed momenta require polynomial identities and stay on that
   separate guarded path. No loop-momentum or on-shell assumption is involved. *)
pcDirectProcessing[data_Association] := Module[{rules},
    rules = Map[Function[s, Module[{routing, scale, name},
        routing = Normal[GroupBy[s["Routing"], Last -> First, Total]];
        routing = Select[routing, Last[#] =!= 0 &];
        If[Length[routing] =!= 1, Nothing,
            name = First[First[routing]]; scale = Last[First[routing]]^2;
            "id " <> name <> "." <> name <> "*" <> s["Name"] <>
                "=" <> intString[Denominator[scale]] <> "/" <>
                intString[Numerator[scale]] <> ";"]
    ]], Select[Lookup[data, "CancellationData", {}], #["MassSquared"] === "0" &]];
    If[rules === {}, "", "\n* Direct massless cancellations cannot increase the term count.\n.sort\nrepeat;\n" <>
        StringRiffle[rules, "\n"] <> "\nendrepeat;\n.sort\n"]
];
