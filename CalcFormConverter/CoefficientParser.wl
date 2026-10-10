(* ::Package:: *)

(* ::Title:: *)
(* Single-pass reconstruction of commuting coefficient trees *)

(* ::Text:: *)
(* Loaded in CalcFormConverter`Private`. This optional parser is used only
   within the validated version-one, no-UpValues import environment. It reads
   one coefficient, never the complete result file. A rejected attempt returns
   to the existing parser, which supplies the original ordered diagnostic.
   All factor values still come from importFactor and its typed dictionary;
   no input text is evaluated as Wolfram source. *)

(* ::Section:: *)
(* Integer syntax tree *)

(* ::Text:: *)
(* The built-in Wolfram virtual machine recognises arithmetic structure only.
   Positive token codes identify dictionary factors. Negative codes represent
   +, -, *, /, ( and ). Each node records {head, parent, factor, sign, inverse,
   depth}; heads 0, 1 and 2 mean factor, Times and Plus. Parent IDs precede
   their children. A negative root head means ineligible syntax.
   Unary signs and reciprocal flags belong to edges, so a nested sum remains
   one factor. The fixed stack bounds parentheses to 64 levels; deeper input
   falls back. Allocation accommodates two nodes per token plus the root pair.
   Compilation is lazy and explicitly targets WVM: no C compiler, external
   process, file or convention-dependent state is involved. *)

(* ::Input::Initialization:: *)
pgCoefficientTreeShape[] := pgCoefficientTreeShape[] = Compile[
    {{codes, _Integer, 1}},
    Module[{nodes = Table[0, {2 Length[codes] + 2}, {6}],
        stack = Table[0, {64}, {2}], count = 2, sum = 1, product = 2,
        depth = 0, want = True, negative = 1, inverse = 0,
        code = 0, parent = 0, valid = True},
        nodes[[1]] = {2, 0, 0, 1, 0, 0};
        nodes[[2]] = {1, 1, 0, 1, 0, 1};
        Do[
            code = codes[[k]];
            Which[
                code > 0,
                    If[!want, valid = False; Break[]];
                    count++;
                    nodes[[count]] = {0, product, code, negative, inverse,
                        nodes[[product, 6]] + 1};
                    want = False,
                code == -1 || code == -2,
                    If[want,
                        If[code == -2, negative = -negative],
                        count++;
                        nodes[[count]] = {1, sum, 0, If[code == -2, -1, 1],
                            0, nodes[[sum, 6]] + 1};
                        product = count; want = True; negative = 1; inverse = 0],
                code == -3 || code == -4,
                    If[want, valid = False; Break[]];
                    want = True; negative = 1; inverse = If[code == -4, 1, 0],
                code == -5,
                    If[!want || depth >= 64, valid = False; Break[]];
                    depth++;
                    stack[[depth]] = {sum, product}; parent = product;
                    count++;
                    nodes[[count]] = {2, parent, 0, negative, inverse,
                        nodes[[parent, 6]] + 1};
                    sum = count;
                    count++;
                    nodes[[count]] = {1, sum, 0, 1, 0, nodes[[sum, 6]] + 1};
                    product = count; want = True; negative = 1; inverse = 0,
                code == -6,
                    If[want || depth == 0, valid = False; Break[]];
                    sum = stack[[depth, 1]]; product = stack[[depth, 2]];
                    depth--; want = False,
                True,
                    valid = False; Break[]
            ],
            {k, Length[codes]}
        ];
        If[want || depth != 0, valid = False];
        If[!valid, nodes[[1, 1]] = -1];
        Take[nodes, count]
    ],
    CompilationTarget -> "WVM", RuntimeOptions -> "Speed"
];

(* ::Section:: *)
(* Validated factors and level-by-level reconstruction *)

(* ::Text:: *)
(* Lexical coverage and tree syntax are checked before factor decoding.
   Distinct factors use the same restricted decoder as polynomial leaves.
   At each depth, group child values by parent and apply Times or Plus in
   bulk. This replaces thousands of small recursive parser calls without
   expanding products of sums. Validate reciprocal operands before a zero
   multiplier or a later cancellation can hide division by zero. *)

(* ::Input::Initialization:: *)
pgParseCoefficientTree[text_String] := Module[
    {tokens, operators, distinct, codes, shape, n, heads, parents, payloads,
     signs, inverses, depths, decoded, values, atoms, internal, order, counts,
     selected, mask, children, batches, new, reciprocals},
    tokens = StringCases[text, RegularExpression[$flatFactorPattern <> "|[+*/()\\-]"]];
    If[tokens === {} ||
        StringReplace[StringJoin[tokens], WhitespaceCharacter -> ""] =!=
            StringReplace[text, WhitespaceCharacter -> ""], Return[$Failed]];
    operators = <|"+" -> -1, "-" -> -2, "*" -> -3, "/" -> -4, "(" -> -5, ")" -> -6|>;
    distinct = DeleteCases[DeleteDuplicates[tokens], "+" | "-" | "*" | "/" | "(" | ")"];
    codes = Lookup[Join[AssociationThread[distinct, Range[Length[distinct]]], operators], tokens];
    shape = pgCoefficientTreeShape[][codes];
    If[shape[[1, 1]] === -1, Return[$Failed]];
    decoded = importFactor /@ distinct;
    If[!FreeQ[decoded, _vectorToken | _indexToken | _caAToken | _caFToken],
        throwFailure["InvalidResult", "A vector or index occurs outside a tensor object."]];
    {heads, parents, payloads, signs, inverses, depths} = Transpose[shape];
    n = Length[shape];
    values = ConstantArray[0, n];
    atoms = Flatten[Position[heads, 0]];
    new = decoded[[payloads[[atoms]]]];
    reciprocals = Position[inverses[[atoms]], 1];
    If[reciprocals =!= {},
        If[MemberQ[Extract[new, reciprocals], 0],
            throwFailure["InvalidResult", "Division by zero."]];
        new = MapAt[1/# &, new, reciprocals]];
    values[[atoms]] = signs[[atoms]] new;
    internal = Flatten[Position[heads, 1 | 2]];
    (* Parent-sorted child IDs and counts define contiguous operand lists.
       Parent zero belongs only to the root; hence counts use a +1 offset. *)
    order = Ordering[parents];
    counts = BinCounts[parents, {0, n + 1, 1}];
    Do[
        selected = Pick[internal, depths[[internal]], level];
        Do[
            batches = Pick[selected, heads[[selected]], head];
            If[batches =!= {},
                mask = Normal[SparseArray[Thread[(batches + 1) -> 1], n + 1]];
                children = Pick[order, mask[[parents[[order]] + 1]], 1];
                new = Apply[If[head === 1, Times, Plus],
                    TakeList[values[[children]], counts[[batches + 1]]], {1}];
                reciprocals = Position[inverses[[batches]], 1];
                If[reciprocals =!= {},
                    If[MemberQ[Extract[new, reciprocals], 0],
                        throwFailure["InvalidResult", "Division by zero."]];
                    new = MapAt[1/# &, new, reciprocals]];
                values[[batches]] = signs[[batches]] new
            ],
            {head, {1, 2}}
        ],
        {level, Max[depths] - 1, 0, -1}
    ];
    First[values]
];
