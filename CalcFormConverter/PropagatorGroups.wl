(* ::Package:: *)

(* ::Title:: *)
(* Propagator grouping and incremental result input *)

(* ::Text:: *)
(* Loaded in CalcFormConverter`Private`. FORM writes ordinary products of
   denominator monomials and parenthesised coefficients. No new mathematical
   heads cross the file boundary. Only one top-level summand is buffered here;
   parsed coefficients, but not their source text or tokens, survive reading. *)

(* ::Section:: *)
(* FORM coefficient common-factor extraction *)

(* Version-one objects commute. Matrix, processed colour and epsilon jobs
   retain ordinary grouping until their factorised reconstruction is validated. *)
pgFactorisationQ[data_Association] := data["Mapping"]["Version"] === 1 &&
    AnyTrue[data["Mapping"]["Entries"], #["Kind"] === "Denominator" &];
pgOutput[data_Association, path_String] := Module[{template, names, indices, vectors},
    If[!pgFactorisationQ[data], Return["#write <" <> path <> "> \"%E\",cfcResult"]];
    template = Quiet[Check[Import[FileNameJoin[{$moduleDirectory, "Templates", "PropagatorFactors.frm.in"}], "Text"], $Failed]];
    If[!StringQ[template] || !ContainsAll[StringCases[template, RegularExpression["@[A-Z]+@"]], {"@RESULT@", "@DENOMINATORS@", "@INDEXCHECK@", "@COEFFICIENTBRACKET@"}],
        throwFailure["MissingTemplate", "Cannot read the propagator factorisation template."]];
    names = Lookup[Select[data["Mapping"]["Entries"], #["Kind"] === "Denominator" &], "Name"];
    (* occurs detects component indices but not indices in native metrics.
       A separate metric pattern keeps free tensors outside scalar content extraction. *)
    indices = Lookup[Select[data["Mapping"]["Entries"], #["Kind"] === "Index" &], "Name", {}];
    (* Use dictionary identities, not assumed names or physical momentum roles.
       With no vectors the previous scalar-only output path is unchanged. *)
    vectors = Lookup[Select[data["Mapping"]["Entries"], #["Kind"] === "Vector" &], "Name", {}];
    StringReplace[template, {"@COEFFICIENTBRACKET@" -> If[vectors === {}, "", "Bracket " <> StringRiffle[vectors, ","] <> ";"], "@RESULT@" -> path, "@DENOMINATORS@" -> StringRiffle[names, ","],
        "@INDEXCHECK@" -> If[indices === {}, "", "if ( occurs(" <> StringRiffle[indices, ","] <> ")" <>
            If[Length[indices] >= 2, " || match(d_(" <> indices[[1]] <> "?," <> indices[[2]] <> "?))", ""] <>
            " );\n$cfcIndexed`cfcSlot'=1;\nendif;"]}]
];

(* ::Section:: *)
(* Owned input streams and identity checks *)

$pgReadChunkSize = 1048576;
pgWithStream[path_String, action_] := Module[{stream},
    stream = Quiet[Check[OpenRead[path, BinaryFormat -> True], $Failed]];
    If[!MatchQ[stream, _InputStream], throwFailure["ReadFailed", "Cannot open the FORM result."]];
    Internal`WithLocalSettings[Null, action[stream], Quiet[Close[stream]]]
];
pgHeader[stream_, mapping_, digests_] := Module[{header, expected, newline},
    expected = (resultMarker[mapping["Version"]] <> " " <> # &) /@ digests;
    (* A malformed file must not make the header reader consume an entire
       result without a newline. The marker and SHA-256 have a fixed length. *)
    header = Quiet[Check[FromCharacterCode[BinaryReadList[stream, "UnsignedInteger8", StringLength[First[expected]]]], $Failed]];
    newline = Quiet[Check[BinaryRead[stream, "UnsignedInteger8"], $Failed]];
    If[newline === 13, newline = Quiet[Check[BinaryRead[stream, "UnsignedInteger8"], $Failed]]];
    If[!MemberQ[expected, header] || newline =!= 10,
        throwFailure["MappingMismatch", "The result does not correspond to this mapping file."]]
];

(* ::Section:: *)
(* Incremental top-level summand reader *)

(* Parentheses are scanned in bulk. Signs inside a coefficient are never
   visited individually. Outside parentheses, a sign starts a new summand
   unless it follows an exponent or another operator. Keeping the preceding
   significant character handles signed exponents across chunk boundaries.
   Fragments preserve whitespace: removing it could join invalid identifiers. *)
pgReadSummands[stream_, consume_] := Module[
    {chunk, depth = 0, fragments = {}, last = "", seen = False, emitted = 0,
     append, flush, outside, positions, start, at, mark, segment, tail},
    append[s_String] := If[s =!= "", AppendTo[fragments, s];
        tail = StringTrim[s]; If[tail =!= "", last = StringTake[tail, -1]; seen = True]];
    flush[] := If[seen, consume[StringJoin[fragments]]; emitted++;
        fragments = {}; last = ""; seen = False];
    outside[s_String] := Module[{begin = 1, stops, pos, ch},
        stops = StringPosition[s, "+" | "-"];
        Do[pos = stop[[1]];
            append[StringTake[s, {begin, pos - 1}]];
            ch = StringTake[s, {pos}];
            If[seen && !MemberQ[{"^", "*", "/", "+", "-", ","}, last], flush[]];
            append[ch]; begin = pos + 1,
            {stop, stops}];
        append[StringDrop[s, begin - 1]]
    ];
    While[True,
        chunk = Quiet[Check[FromCharacterCode[BinaryReadList[stream, "UnsignedInteger8", $pgReadChunkSize]], $Failed]];
        If[chunk === EndOfFile || chunk === "", Break[]];
        If[!StringQ[chunk], throwFailure["ReadFailed", "Cannot read the FORM result body."]];
        positions = StringPosition[chunk, "(" | ")"];
        start = 1;
        Do[at = position[[1]]; segment = StringTake[chunk, {start, at - 1}];
            If[depth === 0, outside[segment], append[segment]];
            mark = StringTake[chunk, {at}];
            If[mark === "(", depth++, depth--];
            If[depth < 0, throwFailure["InvalidResult", "Unmatched closing parenthesis in grouped result."]];
            append[mark]; start = at + 1,
            {position, positions}];
        segment = StringDrop[chunk, start - 1];
        If[depth === 0, outside[segment], append[segment]]
    ];
    If[depth =!= 0, throwFailure["InvalidResult", "Truncated parenthesis in grouped result."]];
    flush[];
    If[emitted === 0, throwFailure["InvalidResult", "The grouped result is empty."]]
];

(* ::Section:: *)
(* Typed grouping and reconstruction *)

(* Denominators remain inert until their prefactor has been separated from
   the coefficient. Only mapped identifiers can create pgDen tokens. *)
pgDenFactorQ[pgDen[_String]] := True;
pgDenFactorQ[Power[pgDen[_String], _Integer]] := True;
pgDenFactorQ[_] := False;

(* Parse a product of completely parenthesised factors separately. Each
   factor can use the existing flat parser. Eligibility only recognises syntax;
   the restricted parser still validates every factor, left to right. Any
   other shape falls back without changing its diagnostics. *)
pgParseCoefficient[text_String, parse_] := Module[
    {body = StringTrim[text], positions, depth = 0, start = 0, stop = 0,
     ranges = {}, eligible = True, ch},
    positions = StringPosition[body, "(" | ")"];
    Do[
        ch = StringTake[body, pos];
        If[ch === "(",
            If[depth === 0,
                If[StringTrim[StringTake[body, {stop + 1, pos[[1]] - 1}]] =!= If[ranges === {}, "", "*"], eligible = False];
                start = pos[[1]]];
            depth++,
            depth--;
            If[depth < 0, eligible = False];
            If[depth === 0, AppendTo[ranges, {start + 1, pos[[1]] - 1}]; stop = pos[[1]]]
        ], {pos, positions}];
    If[eligible && depth === 0 && ranges =!= {} && StringTrim[StringDrop[body, stop]] === "",
        Times @@ (parse[StringTake[body, #]] & /@ ranges),
        parse[body]]
];

(* Split FORM's native monomial*(coefficient) shape before parsing so the
   coefficient can still use the established flat parser. Parsing the enclosing
   parentheses as part of one product would unnecessarily disable that path.
   Other accepted expression shapes retain the general parser's diagnostics. *)
(* Only version-one commuting coefficients use this decomposition. Scan syntax
   before evaluation, then parse one additive subgroup at a time. Leaf pieces
   still pass through the restricted parser; no identifiers are evaluated here.
   Keep divisions, calls, powers of brackets and malformed shapes on the general
   path. This bounds token storage by a subgroup, not the enclosing coefficient.
   The source of one denominator group is still buffered by pgReadSummands. *)
$pgNestedParsing = False;
$pgNestedBlockCharacters = 32768;
(* Reparse rejected input through the original parser so error messages and
   first-failure ordering remain unchanged. Abort is deliberately not caught. *)
pgParseNestedChecked[text_String, parse_] := Module[{value},
    value = Catch[pgParseNested[text, parse], $failureTag];
    If[FailureQ[value], parse[text], value]
];
pgParseNested[text_String, parse_, level_: 0] := Module[
    {body = StringTrim[text], depth = 0, positions, previous = "", start = 1,
     ch, at, sums = {}, products = {}, closes = {}, valid = True,
     division = False, ranges, ends, pieces, outside, blocks, first, last},
    If[level >= 64 || !StringContainsQ[body, "("], Return[parse[body]]];
    (* Scan parenthesis positions in bulk; do not visit the arithmetic inside
       brackets until parsing that smaller subgroup. *)
    outside[lo_, hi_] := Module[{part, offset = 1, operator, location, before},
        part = StringTake[body, {lo, hi}];
        Do[
            location = item[[1]];
            before = StringTrim[StringTake[part, {offset, location - 1}]];
            If[before =!= "", previous = StringTake[before, -1]];
            operator = StringTake[part, {location}];
            Switch[operator,
                "+" | "-", If[previous =!= "" && !MemberQ[{"^", "*", "/", "+", "-", "(", ","}, previous], AppendTo[sums, lo + location - 1]],
                "*", AppendTo[products, lo + location - 1],
                "/", division = True];
            previous = operator; offset = location + 1,
            {item, StringPosition[part, "+" | "-" | "*" | "/"]}];
        before = StringTrim[StringDrop[part, offset - 1]];
        If[before =!= "", previous = StringTake[before, -1]]
    ];
    positions = StringPosition[body, "(" | ")"];
    Do[
        at = pos[[1]];
        If[depth === 0, outside[start, at - 1]];
        ch = StringTake[body, {at}];
        If[ch === "(", depth++, depth--;
            If[depth < 0, valid = False]; If[depth === 0, AppendTo[closes, at]]];
        previous = ch; start = at + 1,
        {pos, positions}];
    If[depth === 0, outside[start, StringLength[body]]];
    If[!valid || depth =!= 0, Return[parse[body]]];
    Which[
        sums =!= {},
            ends = Append[sums - 1, StringLength[body]];
            ranges = Transpose[{Prepend[sums, 1], ends}];
            (* Batch adjacent summands up to a modest text budget. This avoids
               thousands of parser initialisations for tiny polynomials while
               keeping large coefficient token lists out of memory. *)
            first = ranges[[1, 1]]; last = first - 1;
            blocks = Reap[Do[
                If[last >= first && range[[2]] - first + 1 > $pgNestedBlockCharacters,
                    Sow[{first, last}]; first = range[[1]]];
                last = range[[2]], {range, ranges}]; Sow[{first, last}]][[2, 1]];
            (* A batch may still contain brackets. Descend into its summands
               instead of sending the whole batch to the general parser. *)
            If[Length[blocks] === 1, blocks = ranges];
            Total[pgParseNested[StringTake[body, #], parse, level + 1] & /@ blocks],
        StringStartsQ[body, "("] && closes === {StringLength[body]},
            pgParseNested[StringTake[body, {2, -2}], parse, level + 1],
        !division && products =!= {},
            ranges = Transpose[{Prepend[products + 1, 1], Append[products - 1, StringLength[body]]}];
            pieces = StringTrim[StringTake[body, #]] & /@ ranges;
            (* Split only complete bracket factors and flat monomials. A call or
               powered bracket keeps the original parse and failure ordering. *)
            If[AllTrue[pieces, # =!= "" && (!StringContainsQ[#, "(" | ")"] ||
                (StringStartsQ[#, "("] && StringEndsQ[#, ")"])) &],
                Times @@ (pgParseNested[#, parse, level + 1] & /@ pieces), parse[body]],
        True, parse[body]
    ]
];

pgParse[text_String, values_, dim_] := Module[{trimmed = StringTrim[text], cut, prefix, body, depth = 0, balanced, factor, prepared, parse},
    parse[t_] := If[$diracImportLine === None, parseResult[t, values, dim],
        prepared = caPrepareResult[t, values]; parseGeneralResult[prepared[[1]], prepared[[2]], dim]];
    cut = StringPosition[trimmed, RegularExpression["\\*\\s*\\("], 1];
    If[cut =!= {} && StringEndsQ[trimmed, ")"],
        prefix = StringTake[trimmed, cut[[1, 1]] - 1];
        body = StringTake[trimmed, {cut[[1, 2]] + 1, -2}];
        balanced = True;
        Scan[Function[pos, depth += If[StringTake[body, pos] === "(", 1, -1];
            If[depth < 0, balanced = False]], StringPosition[body, "(" | ")"]];
        If[balanced && depth === 0 &&
            StringMatchQ[prefix, RegularExpression["[+-]?\\s*(?:1|cfd[0-9]+)(?:\\s*\\^\\s*[+-]?[0-9]+)?(?:\\s*\\*\\s*cfd[0-9]+(?:\\s*\\^\\s*[+-]?[0-9]+)?)*\\s*"]],
            (* Lexical shape is only eligibility. Every identifier is resolved
               through the validated dictionary, never inferred from its name. *)
            factor = parseGeneralResult[prefix, values, dim];
            Return[factor pgParseCoefficient[body, If[TrueQ[$pgNestedParsing] &&
                StringFreeQ[body, RegularExpression["[^A-Za-z0-9_+*/^(),.\\s-]"]],
                Function[t, pgParseNestedChecked[t, parse]], parse]]]
        ]
    ];
    parse[trimmed]
];

pgImport[path_, mapping_, digests_, values_, dim_] := Module[
    {names, tokens, groups = <||>, implicit = True, consume, parsed,
     factors, prefactor, coefficient, key, rows, result},
    names = Lookup[Select[mapping["Entries"], #["Kind"] === "Denominator" &], "Name", {}];
    If[names === {}, throwFailure["InvalidMapping", "Grouped output requires mapped denominators."]];
    tokens = Join[values, Association[(# -> pgDen[#]) & /@ names]];
    consume[text_] := (
        parsed = pgParse[text, tokens, dim];
        factors = If[Head[parsed] === Times, List @@ parsed, {parsed}];
        prefactor = Times @@ Select[factors, pgDenFactorQ];
        coefficient = Times @@ Select[factors, !pgDenFactorQ[#] &];
        If[!FreeQ[coefficient, _pgDen],
            throwFailure["InvalidResult", "A propagator occurs inside a grouped coefficient."]];
        (* Validation must visit every coefficient even after an earlier group
           has required explicit endpoints. Do not short-circuit this call. *)
        implicit = caImplicitEligible[coefficient] && implicit;
        key = ToString[prefactor, InputForm];
        If[KeyExistsQ[groups, key],
            rows = groups[key]; AppendTo[rows[[2]], coefficient]; AssociateTo[groups, key -> rows],
            AssociateTo[groups, key -> {prefactor, {coefficient}}]];
        parsed = factors = coefficient = Null
    );
    (* Reassociation is inappropriate for user symbols with arithmetic UpValues. *)
    Block[{$pgNestedParsing = mapping["Version"] === 1 &&
        AllTrue[DeleteDuplicates[Cases[{Values[values], dim}, _Symbol, Infinity, Heads -> True]], UpValues[#] === {} &]},
        If[TrueQ[$pgNestedParsing],
            withImportParser[tokens, dim,
                pgWithStream[path, Function[stream, pgHeader[stream, mapping, digests]; pgReadSummands[stream, consume]]]],
            pgWithStream[path, Function[stream, pgHeader[stream, mapping, digests]; pgReadSummands[stream, consume]]]]];
    result = Map[Function[row,
        (row[[1]] /. pgDen[name_String] :> values[name]) *
        dcReconstruct[caReconstruct[Total[row[[2]]], implicit]]], Values[groups]];
    Total[result]
];
