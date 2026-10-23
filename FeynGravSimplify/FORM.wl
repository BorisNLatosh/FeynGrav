(* Restricted FORM benchmark backend. No CalcFormConverter runtime or algebra helpers.
   Uses failure/reject/decodeTree from RestrictedCodec.wl and the shared
   findOnPath/executableFileQ helpers defined in Contractor.wl. *)
Begin["FeynGrav`Simplification`Private`"];

formConfiguration[requested_, workers_] := Module[{n, path, name},
    n = If[workers === Automatic, If[IntegerQ[$ProcessorCount], Min[8, Max[1, $ProcessorCount]], 1], workers];
    name = If[n === 1, "form", "tform"];
    path = If[requested === Automatic, findOnPath[name],
        If[StringContainsQ[requested, {"/", "\\"}], ExpandFileName[requested], findOnPath[requested]]];
    If[!StringQ[path] || !executableFileQ[path], Return[failure["FORMNotFound",
        "Cannot locate the requested FORM/TFORM executable. Supply FORMExecutable explicitly.",
        <|"RequestedExecutable" -> requested, "FORMThreads" -> n|>]]];
    <|"Executable" -> ExpandFileName[path], "Workers" -> n|>
];

(* Reuse the Contractor tree and map. Non-positive/non-int32 powers are opaque,
   exactly as in Contractor's scalar walker: never cancel or expand their bases. *)
formProgram[data_Association, workers_Integer] := Module[
    {entries = data["Entries"], opaques = <||>, ids = <||>, atom, emit, body,
     declarations, names, code},
    atom[tree_] := Module[{key = With[{v = tree}, HoldComplete[v]], id},
        If[KeyExistsQ[ids, key], Return[ids[key]]];
        id = "fgOpaque" <> ToString[Length[opaques] + 1];
        AssociateTo[ids, key -> id]; AssociateTo[opaques, id -> tree]; id
    ];
    emit[x_Integer] := ToString[x, InputForm];
    emit[x_String] := If[x === "D", "fgDimension", x];
    emit[x_List] := Switch[First[x],
        "Plus", "(" <> StringRiffle[emit /@ Rest[x], "+"] <> ")",
        "Times", "(" <> StringRiffle[emit /@ Rest[x], "*"] <> ")",
        "Rational", "(" <> emit[x[[2]]] <> "/" <> emit[x[[3]]] <> ")",
        "Complex", "(" <> emit[x[[2]]] <> "+i_*" <> emit[x[[3]]] <> ")",
        "Power", If[IntegerQ[x[[3]]] && 1 <= x[[3]] <= 2147483647,
            "(" <> emit[x[[2]]] <> ")^" <> emit[x[[3]]], atom[x]],
        "Pair", Which[
            x[[2, 1]] === "LorentzIndex" && x[[3, 1]] === "LorentzIndex",
                "d_(" <> x[[2, 2]] <> "," <> x[[3, 2]] <> ")",
            x[[2, 1]] === "Momentum" && x[[3, 1]] === "Momentum",
                "(" <> x[[2, 2]] <> "." <> x[[3, 2]] <> ")",
            x[[2, 1]] === "Momentum", x[[2, 2]] <> "(" <> x[[3, 2]] <> ")",
            True, x[[3, 2]] <> "(" <> x[[2, 2]] <> ")"
        ]
    ];
    body = emit[data["Tree"]];
    names[role_] := Keys[Select[entries, #["Role"] === role &]];
    declarations = "Symbols " <> StringRiffle[Join[{"fgDimension"}, names["Scalar"], Keys[opaques]], ","] <> ";\n" <>
        "Dimension " <> If[data["Dimension"] === 4, "4", "fgDimension"] <> ";\n" <>
        If[names["Index"] === {}, "", "Indices " <> StringRiffle[names["Index"], ","] <> ";\n"] <>
        If[names["Momentum"] === {}, "", "Vectors " <> StringRiffle[names["Momentum"], ","] <> ";\n"];
    code = "Off Statistics;\n" <>
        If[workers > 1, "#if `NTHREADS_' != " <> ToString[workers + 1] <>
            "\n#message Requested worker count requires TFORM\n#terminate\n#endif\n", ""] <>
        declarations <>
        "* One factored expression: no staged multiplication or extra algebra.\n" <>
        "Local fgResult = " <> body <> ";\n.sort\n" <>
        "#write <result.txt> \"FGFORMBEGIN\"\n#write <result.txt> \"%E\",fgResult\n" <>
        "#write <result.txt> \"FGFORMEND\"\n.end\n";
    <|"Program" -> code, "OpaqueTrees" -> opaques|>
];

(* Parse FORM's arithmetic output as data. No ToExpression and no general FORM
   parser: only our generated symbols, native metrics/vectors and arithmetic. *)
formDecode[text_String, data_Association, opaques_Association] := Catch[Module[
    {lines, body, compact, tokens, pos = 1, peek, take, expect, invalid,
     sum, product, unary, power, primary, slot, identifier, known, tree},
    invalid[] := reject["InvalidOutput", "FORM returned an incomplete or unsupported result."];
    lines = StringSplit[StringTrim[text], "\n"];
    If[Length[lines] < 3 || StringTrim[First[lines]] =!= "FGFORMBEGIN" ||
        StringTrim[Last[lines]] =!= "FGFORMEND", invalid[]];
    body = StringRiffle[Rest[Most[lines]], "\n"];
    (* Only documented line continuations and whitespace are discarded. *)
    body = StringReplace[body, RegularExpression["(?<=[0-9])\\\\\\r?\\n[ \\t]*(?=[0-9])"] -> ""];
    compact = StringReplace[body, WhitespaceCharacter -> ""];
    tokens = StringCases[body, RegularExpression["[0-9]+|[A-Za-z][A-Za-z0-9_]*|[()+*^/,.-]"]];
    If[tokens === {} || StringJoin[tokens] =!= compact, invalid[]];
    peek[] := If[pos <= Length[tokens], tokens[[pos]], "END"];
    take[] := Module[{t = peek[]}, pos++; t];
    expect[t_] := If[take[] =!= t, invalid[]];
    known[id_, role_] := KeyExistsQ[data["Entries"], id] && data["Entries"][id]["Role"] === role;
    slot[id_, role_] := If[known[id, role],
        Join[{If[role === "Index", "LorentzIndex", "Momentum"], id}, If[data["Dimension"] === System`D, {"D"}, {}]], invalid[]];
    identifier[] := Module[{id = take[]},
        If[!StringMatchQ[id, RegularExpression["[A-Za-z][A-Za-z0-9_]*"]], invalid[]]; id];
    primary[] := Module[{t = take[], a, b}, Which[
        StringMatchQ[t, DigitCharacter ..], FromDigits[t],
        t === "(", a = sum[]; expect[")"]; a,
        t === "d_", expect["("]; a = slot[identifier[], "Index"]; expect[","];
            b = slot[identifier[], "Index"]; expect[")"]; {"Pair", a, b},
        t === "i_", {"Complex", 0, 1},
        t === "fgDimension", "D",
        KeyExistsQ[opaques, t], opaques[t],
        known[t, "Scalar"], t,
        known[t, "Momentum"], a = slot[t, "Momentum"];
            If[peek[] === ".", take[]; b = slot[identifier[], "Momentum"],
                expect["("]; b = slot[identifier[], "Index"]; expect[")"]]; {"Pair", a, b},
        True, invalid[]
    ]];
    power[] := Module[{a = primary[]}, If[peek[] === "^", take[]; {"Power", a, unary[]}, a]];
    unary[] := Switch[peek[], "+", take[]; unary[], "-", take[]; {"Times", -1, unary[]}, _, power[]];
    product[] := Module[{items, op, b},
        items = Reap[Sow[unary[]];
            While[MemberQ[{"*", "/"}, peek[]], op = take[]; b = unary[];
                If[op === "/", If[!IntegerQ[b] || b <= 0, invalid[]]; b = {"Rational", 1, b}];
                Sow[b]]][[2, 1]];
        If[Length[items] === 1, First[items], Prepend[items, "Times"]]];
    sum[] := Module[{items, op, b},
        items = Reap[Sow[product[]];
            While[MemberQ[{"+", "-"}, peek[]], op = take[]; b = product[];
                Sow[If[op === "-", {"Times", -1, b}, b]]]][[2, 1]];
        If[Length[items] === 1, First[items], Prepend[items, "Plus"]]];
    tree = sum[]; If[peek[] =!= "END", invalid[]];
    decodeTree[tree, data]
], $failureTag];

End[];
