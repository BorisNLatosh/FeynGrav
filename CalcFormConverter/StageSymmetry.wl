(* ::Package:: *)

(* ::Title:: *)
(* Verified pair symmetries between prepared tensor stages *)

(* ::Text:: *)
(* Private, process-free planning. A pair can be relabelled in a partial
   contraction only when its remaining partner is invariant under that swap.
   FORM checks the partner explicitly; graph connectivity alone is insufficient.
   No identities of particular vertices or propagators are assumed. *)

(* ::Section:: *)
(* Unambiguous two-index connections *)

$ssEnabled = True;
$ssMaximumStages = 8;

ssLinks[signatures_List, name_] := Module[{free, shared},
    If[Length[signatures] > $ssMaximumStages, Return[{}]];
    free = Keys[Select[#, # === 1 &]] & /@ signatures;
    Flatten[Table[
        shared = Intersection[free[[i]], free[[j]]];
        If[Length[shared] === 2,
            {<|"Left" -> i, "Right" -> j, "Indices" -> (name /@ shared)|>}, {}],
        {i, Length[free] - 1}, {j, i + 1, Length[free]}], 2]
];

(* ::Section:: *)
(* Native FORM checks and canonical representatives *)

(* The symmetric placeholders identify equivalent terms without averaging or
   expanding the remaining partner. Replacing them by two metrics restores the
   original free indices, including traces and momentum-contracted arguments.
   Only crossing pairs are summed, and only after their symmetry check passes.
   Other free indices are untouched. No dummy index or placeholder reaches the
   parser. Dimension-only abbreviations contain no indices and remain safe. *)
ssPlan[data_Association] := Module[{links, tensor, flag, check, checks, boundaries, active, code},
    If[!TrueQ[$ssEnabled] || data["Mapping"]["Version"] =!= 1, Return[<||>]];
    links = Lookup[data, "SymmetryLinks", {}];
    If[links === {} || data["Preparations"] === {}, Return[<||>]];
    tensor[n_] := "cfcStageSym" <> intString[n];
    flag[n_] := "cfcStageSymOK" <> intString[n];
    check[n_] := "cfcStageSymCheck" <> intString[n];
    checks = "\n* Verify partner symmetries before using canonical representatives.\n.sort\n" <>
        StringJoin[MapIndexed[Function[{link, pos}, With[
            {n = First[pos], stage = "cfcStage" <> intString[link["Right"]],
             a = link["Indices"][[1]], b = link["Indices"][[2]]},
            "Local " <> check[n] <> "=" <> stage <> "-" <> stage <>
                "*replace_(" <> a <> "," <> b <> "," <> b <> "," <> a <> ");\n"]], links]] <>
        ".sort\n" <> StringJoin[Table[
            "#$" <> flag[n] <> "=termsin_(" <> check[n] <> ");\nDrop " <> check[n] <> ";\n",
            {n, Length[links]}]] <> ".sort\n";
    boundaries = Table[
        active = Select[Range[Length[links]], links[[#]]["Left"] <= k < links[[#]]["Right"] &];
        If[active === {}, "",
            code = "#if " <> StringRiffle[("( `$" <> flag[#] <> "' == 0 )") & /@ active, " || "] <> "\n";
            code <> StringJoin[Map[Function[n, With[{pair = StringRiffle[links[[n]]["Indices"], ","]},
                "#if `$" <> flag[n] <> "' == 0\nMultiply " <> tensor[n] <> "(" <> pair <>
                    ");\nsum " <> pair <> ";\n#endif\n"]], active]] <> ".sort\n" <>
                StringJoin[Map[Function[n, "id " <> tensor[n] <>
                    "(cfcStageSymI?,cfcStageSymJ?)=d_(cfcStageSymI," <> links[[n]]["Indices"][[1]] <>
                    ")*d_(cfcStageSymJ," <> links[[n]]["Indices"][[2]] <> ");\n"], active]] <>
                ".sort\n#endif\n"],
        {k, Length[data["Multiplications"]]}];
    <|"Declarations" -> "\nIndices cfcStageSymI,cfcStageSymJ;\nTensor " <>
        StringRiffle[(tensor[#] <> "(symmetric)") & /@ Range[Length[links]], ","] <> ";\n",
      "Checks" -> checks, "Boundaries" -> boundaries|>
];
