(* Result parsing and reconstruction: private companion to CalcFormConverter.

   Loaded inside the converter's private context by CalcFormConverter.wl. It
   contains the restricted and grouped result grammars, mapping validation and
   the public CalcFormImport entry point moved unchanged from the central
   converter in ARCH-01 Stage B. It executes no process. *)

(* ::Section:: *)
(*Import: FORM parsing and FeynCalc reconstruction*)


(* ::Subsection:: *)
(*Restricted result parser*)


(* ::Text:: *)
(*Section 5: FORM result parsing and FeynCalc reconstruction. A recursive-descent parser reads FORM's output text back into Wolfram expressions. It recognizes arithmetic, FORM's native tensor syntax (d_(i,j) metrics and p_(i) components), the reserved master-integral functions, epsilon tensors and version-three gamma words. Key design choice stated in the header: vector and index tokens keep DISTINCT types until they are converted into Pair objects, so a misplaced index is caught before arithmetic can hide it.*)

(* ::Input::Initialization:: *)
(* A grouped scalar import may reuse validated state across polynomial leaves.
   Block owns the caches: no symbol definitions or convention-dependent values
   survive an import, failure or abort. Limits bound retained cache entries,
   not the returned expression or the kernel's allocator. *)
$importParserActive = False;
$importParserCacheEntries = 4096;
$importParserCacheBytes = 4194304;
SetAttributes[withImportParser, HoldAll];
withImportParser[values_, dim_, body_] := Block[
    {$importParserActive = True, $importParserValues = values,
     $importParserDimension = dim, $importFactorCache = <||>,
     $importClassCache = <||>, $tdRatioDenominators = <||>, $importFactorBytes = 0, $importClassBytes = 0,
     $flatParserMinimumCharacters = Min[32, $flatParserMinimumCharacters],
     $flatParserMinimumReusePerDistinctFactor = Min[1, $flatParserMinimumReusePerDistinctFactor]},
    body
];
importFactor[t_String] := Module[{value, bytes},
    (* Exact integer literals and already validated scalar dictionary entries
       need no recursive parser. Do not cache the many distinct integers:
       they otherwise evict reusable powers and momentum monomials. *)
    If[StringMatchQ[t, RegularExpression["[0-9]+"]], Return[FromDigits[t]]];
    If[t === "i_", Return[I]];
    If[KeyExistsQ[$importParserValues, t],
        value = $importParserValues[t];
        If[FreeQ[value, _vectorToken | _indexToken | _caAToken | _caFToken], Return[value]]];
    If[StringStartsQ[t, "cfcShare(" | "cfcLRDen("],
        value = srFastFactor[t];
        If[value =!= $Failed, Return[value]]];
    If[KeyExistsQ[$importFactorCache, t], Return[$importFactorCache[t]]];
    value = If[$tdImport =!= <||> && StringStartsQ[t, "cfcDimRat("],
        tdFastDimensionRatio[t], $Failed];
    If[value === $Failed, value = parseGeneralResult[t, $importParserValues, $importParserDimension]];
    If[StringLength[t] <= 512,
        bytes = ByteCount[value] + ByteCount[t] + 256;
        If[bytes <= 16384 && bytes <= $importParserCacheBytes,
            If[Length[$importFactorCache] >= $importParserCacheEntries ||
                $importFactorBytes + bytes > $importParserCacheBytes,
                $importFactorCache = <||>; $importFactorBytes = 0];
            AssociateTo[$importFactorCache, t -> value]; $importFactorBytes += bytes]];
    value
];
importTokenClass[t_String, compoundPattern_] := Module[{value, bytes},
    If[TrueQ[$importParserActive] && KeyExistsQ[$importClassCache, t],
        Return[$importClassCache[t]]];
    value = Which[StringMatchQ[t, DigitCharacter ..], 0,
        StringMatchQ[t, RegularExpression["[A-Za-z][A-Za-z0-9_]*"]], 1,
        StringMatchQ[t, RegularExpression[compoundPattern]], 3, True, 2];
    If[TrueQ[$importParserActive] && StringLength[t] <= 512,
        bytes = ByteCount[t] + 256;
        If[bytes <= $importParserCacheBytes,
            If[Length[$importClassCache] >= $importParserCacheEntries ||
                $importClassBytes + bytes > $importParserCacheBytes,
                $importClassCache = <||>; $importClassBytes = 0];
            AssociateTo[$importClassCache, t -> value]; $importClassBytes += bytes]];
    value
];

(* FORM can wrap a long exact integer using backslash-newline plus indentation.
   Join only numeric tokens, never identifiers or arbitrary source lines. The
   bounded expression/record reader owns chunk boundaries before this step. *)
normaliseFORMNumbers[text_String] := If[!StringContainsQ[text,"\\"],text,
    StringReplace[text,RegularExpression["(?<![A-Za-z0-9_])([0-9]+)\\\\\\r?\\n[ \\t]*(?=[0-9])"] -> "$1"]];

parseGeneralResult[text_String, values_Association, dim_] := Module[
     {tokens, pos = 1, peek, take, expect, atom, power, unary, 
    product, sum,
       scalarValue, entryValue, call, dot, result, tokenPattern, 
    stripped, classes, source=normaliseFORMNumbers[text],
       tokenCount, compoundPattern, compoundValue},
     (*Lexing, part 1 -- what can be one token. Only COMPLETE component/metric calls are made composite: cfvN(cfiM) and d_(cfiM,cfiN). Everything else is lexed piecewise. The input text is tokenized as-is (not whitespace-stripped first), precisely so whitespace cannot glue separate identifier fragments into one token. Dots are deliberately left as ordinary operator tokens, which is what lets the dot-chain validation below run left-to-right.*)
     (* Local recursive functions refer to one another and to token storage.
        Explicit cleanup breaks those references after every small subgroup,
        including failures and aborts, rather than retaining parser closures. *)
     Internal`WithLocalSettings[Null,
     compoundPattern = 
    "(?:cfv[0-9]+\\s*\\(\\s*cfi[0-9]+\\s*\\)|d_\\s*\\(\\s*cfi[0-9]+\\\
s*,\\s*cfi[0-9]+\\s*\\))";
     (*The full token inventory: a composite call, an identifier, an integer, or one of the single-character operators/punctuation.*)
     tokenPattern = 
    RegularExpression[
     compoundPattern <> "|[A-Za-z][A-Za-z0-9_]*|[0-9]+|[+*/^(),.\\-]"];
     stripped = StringReplace[source, WhitespaceCharacter -> ""];
     tokens = StringCases[source, tokenPattern];
     (*Coverage check: the concatenation of ALL tokens, with whitespace removed, must equal the whitespace-stripped input. If not, some character was never tokenized -- i.e. the result contains syntax this parser does not know -- and the whole parse is rejected up front rather than misinterpreted. An empty token list is rejected for the same reason.*)
     If[StringReplace[StringJoin[tokens], WhitespaceCharacter -> ""] =!= 
      stripped || tokens === {}, 
    throwFailure["InvalidResult", "The result contains invalid syntax."]];
     (*Classify each DISTINCT lexeme once, into: 0 = integer, 1 = identifier, 2 = punctuation, 3 = native call Caching classification in an association avoids re-running the regular expressions at every occurrence, which matters for large results where a few identifiers repeat thousands of times.*)
     classes = Association[(# -> importTokenClass[#, compoundPattern] &) /@ DeleteDuplicates[tokens]];
     (*Decode composite calls lazily, in parse order, through the SAME call[]/entryValue[] machinery as ordinary calls, so identifier and argument validation is identical. Eager decoding would report a later unknown identifier before an earlier syntax error, which is a worse diagnostic. compoundValue is SetDelayed plus an inner Set, so only SUCCESSFUL results are cached and that cache lives only for this import.*)
     compoundValue[t_] := compoundValue[t] = With[
          {parts = 
        StringCases[t, RegularExpression["[A-Za-z][A-Za-z0-9_]*"]]},
          call[First[parts], entryValue /@ Rest[parts]]];
     (*Lookahead needs one token beyond the end; the sentinel is appended after tokenCount is recorded, and take[] uses tokenCount as its bound, so the sentinel is visible to peek[] but can never be consumed as input.*)
     tokenCount = Length[tokens];
     tokens = Append[tokens, "END"];
     peek[] := tokens[[pos]];
     take[] := (
         (*Consumption past the real end is an explicit error, not an index error.*)
     If[pos > tokenCount, 
      throwFailure["InvalidResult", "Unexpected end of FORM result."]];
         tokens[[pos++]]
       );
     expect[t_] := 
    If[take[] =!= t, 
     throwFailure["InvalidResult", "Unexpected token in FORM result."]];
     (*Type gate for every scalar-arithmetic position. A raw vectorToken/indexToken must never reach Times/Plus, because arithmetic (or a zero factor, or cancellation) could hide an invalid use. The check is FreeQ over the whole subtree, so it also catches tokens nested inside derived structures.*)
     scalarValue[x_] := If[! FreeQ[x, _vectorToken | _indexToken | _caAToken | _caFToken],
         
     throwFailure["InvalidResult", 
      "A vector or index occurs outside a tensor object."], x];
     (*Identifier resolution: the ONLY way an identifier becomes a value. It is looked up in the import's association; an unknown name fails with that name reported. Nothing from the text is ever evaluated as Wolfram source.*)
     entryValue[n_] := Lookup[values, n,
         
     throwFailure["UnknownIdentifier", 
      "Unknown identifier in FORM result.", <|"Identifier" -> n|>]];
     (*call[n, args]: resolve a function application. The Which is an exact whitelist of accepted (name, argument-type) shapes: d_ with two index tokens -> metric Pair a DECLARED VECTOR name with one index token -> momentum component, i.e. Pair[Momentum[v, dim], LorentzIndex[i, dim]] a master FORM name whose arguments pass -> the master masterArgumentsQ (arity + scalar-ness) head applied Anything else is an unsupported function or a type error.*)
     call[n_, args_] := Which[
     n === "cfcTC", tdCall[args],
     n === "cfcShare", lrSharedCall[args],
     n === "cfcLRNum", ldgScalarCall[args, False],
     n === "cfcLRDen", ldgScalarCall[args, True],
     MemberQ[{"cfcLRReduced","cfcLRReducedPart"},n], lrReducedCall[args],
     n === "cfcDimRat", tdDimensionRatio[args],
     MemberQ[{"cfcCT", "cfcCTr", "cfcCF", "cfcCD"}, n] || (n === "d_" && !FreeQ[args, _caAToken | _caFToken]), caCall[n, args],
     MemberQ[{"g_", "gi_"}, n], dcGammaCall[n, args, dim],
     n === "e_" && $epsilonImportFactor =!= None && Length[args] === 4 &&
       AllTrue[args, MatchQ[#, _indexToken | _vectorToken] &],
       (Eps @@ (args /. {indexToken[x_] :> LorentzIndex[x, dim],
           vectorToken[x_] :> Momentum[x, dim]}))/$epsilonImportFactor,
         
     n === "d_" && Length[args] == 2 && 
      MatchQ[args, {_indexToken, _indexToken}],
           
     Pair[LorentzIndex[args[[1, 1]], dim], 
      LorentzIndex[args[[2, 1]], dim]],
         
     KeyExistsQ[values, n] && MatchQ[values[n], _vectorToken] && 
      MatchQ[args, {_indexToken}],
           
     Pair[Momentum[values[n][[1]], dim], 
      LorentzIndex[args[[1, 1]], dim]],
         
     KeyExistsQ[$masterSpecByFORMName, n] && 
      masterArgumentsQ[$masterSpecByFORMName[n], scalarValue /@ args],
           $masterSpecByFORMName[n]["Head"] @@ (scalarValue /@ args),
         True, 
     throwFailure["InvalidResult", 
      "Unsupported function or argument types in FORM result.", <|
       "Function" -> n|>]
       ];
     (*dot[a, b]: the "." operator. Only two declared vectors may be dotted, producing a Pair of momenta. Memoized on the typed pair of values (dim is fixed for this import, so it need not be part of the key). A failure throws -- here and throughout, the throw happens before the inner Set could store anything, so only successes are cached.*)
     dot[a_, b_] := 
    dot[a, b] = If[MatchQ[{a, b}, {_vectorToken, _vectorToken}],
          Pair[Momentum[a[[1]], dim], Momentum[b[[1]], dim]],
          
      throwFailure["InvalidResult", 
       "Dot products require two declared vectors."]];
     (*atom[]: literals, parenthesized subexpressions, and calls. With binds the consumed token once, so the classification and the branch see the same value. All mutable parser state (pos) belongs to this invocation's Module, never to an outer scope.*)
     atom[] := With[{t = take[]},
         Which[
            (*Integer literal, built with FromDigits (never ToExpression*)
            classes[t] === 0, FromDigits[t],
            (*Complete native call (cfvN(cfiM) / d_(i,j)).*)
            classes[t] === 3, compoundValue[t],
            (*Parenthesized expression.*)
            t === "(", With[{v = sum[]}, expect[")"]; v],
            (*Identifier: a call if followed by "(", else a leaf.*)
            classes[t] === 1,
              If[peek[] === "(",
                 Module[{args = {}},
                    take[];
                    (*Comma-separated argument list; the empty argument list is handled by the If.*)
                    If[peek[] =!= ")",
                       AppendTo[args, sum[]];
                       While[peek[] === ",",
                          take[];
                          AppendTo[args, sum[]]
                        ]
                     ];
                    expect[")"];
                    call[t, args]
                  ],
                 (*FORM's i_ is the imaginary unit; everything else must be a known identifier from the mapping.*)
                 If[t === "i_", I, entryValue[t]]
               ],
            True, 
      throwFailure["InvalidResult", 
       "Expected a number, declared symbol or parenthesized \
expression."]
          ]
       ];
   
     (*Consume dots left to right before the optional integer exponent. Validate each dot immediately so a later unknown identifier cannot replace an earlier type error. Exponents may contain a sign and optional parentheses, as in ^(-2).*)
     power[] := Module[{v = atom[]},
         While[peek[] === ".",
            take[];
            v = dot[v, atom[]]
          ];
         If[peek[] === "^",
            Module[{n, sign = 1, parenthesized = False},
               take[];
               If[peek[] === "(", take[]; parenthesized = True];
               If[peek[] === "-", take[]; sign = -1,
                  If[peek[] === "+", take[]]
                ];
               n = take[];
               (*Only integer exponents are accepted in FORM output.*)
               If[! StringMatchQ[n, DigitCharacter ..],
                  throwFailure["InvalidResult", "FORM exponents must be integers."]
                ];
               If[parenthesized, expect[")"]];
               (*Guard 0^0 and 0^negative before they produce messages or Indeterminate/ComplexInfinity. The <= 0 test on the signed exponent is what covers both.*)
               If[v === 0 && sign FromDigits[n] <= 0,
                  throwFailure["InvalidResult", "Undefined power of zero."]
                ];
               v = If[$diracImportLine === None, scalarValue[v]^(sign FromDigits[n]),
                   dcResultPower[scalarValue[v], sign FromDigits[n]]]
             ]
          ];
         v
       ];
     (*unary[]: leading + and - signs. Negation is applied to the scalar-checked operand, so "-cfv1" is rejected as a vector in a scalar position rather than silently negated.*)
     unary[] := Switch[peek[],
         "+", take[]; unary[],
         "-", take[]; -scalarValue[unary[]],
         _, power[]
       ];
   
     (*product[] consumes unary factors and then * or / operators from left to right. Reap/Sow collects operands before constructing Times. Validate typed operands and check division by zero before arithmetic can hide an invalid token.*)
     product[] := With[{v = unary[]},
         If[! MemberQ[{"*", "/"}, peek[]],
            v,
            Module[{op, r, factors},
               factors = Reap[
                    Sow[scalarValue[v]];
                    While[MemberQ[{"*", "/"}, peek[]],
                       op = take[];
                       r = scalarValue[unary[]];
           If[op === "/" && r === 0, 
            throwFailure["InvalidResult", "Division by zero."]];
                       Sow[If[op === "*", r, If[$diracImportLine === None, 1/r, dcResultPower[r, -1]]]]
                     ]
                  ][[2, 1]];
               If[$diracImportLine === None, Times @@ factors, dcResultProduct[factors]]
             ]
          ]
       ];
     (*sum[]: product, then a left-to-right chain of + and -. Same Reap/Sow and same scalar gate; subtraction negates the whole right-hand product.*)
     sum[] := With[{v = product[]},
         If[! MemberQ[{"+", "-"}, peek[]],
            v,
            Module[{op, r, terms},
               terms = Reap[
                    Sow[scalarValue[v]];
                    While[MemberQ[{"+", "-"}, peek[]],
                       op = take[];
                       r = scalarValue[product[]];
                       Sow[If[op === "+", r, -r]]
                     ]
                  ][[2, 1]];
               Plus @@ terms
             ]
          ]
       ];
     (*Run the parser and require full consumption: leftover tokens mean the result had structure the grammar does not cover, which is an error rather than something to ignore.*)
     result = scalarValue[sum[]];
     If[pos <= tokenCount,
        throwFailure["InvalidResult", 
     "Unexpected trailing tokens in FORM result."]
      ];
     result,
     Clear[tokens, classes, stripped, compoundValue, dot, peek, take,
       expect, atom, power, unary, product, sum, scalarValue, entryValue, call]
     ]
   ];


(* ::Text:: *)
(*Use the flat path only for sufficiently long, repetitive result bodies. Its lexical subset allows bare vector components and metrics, but no tensor powers, nested calls or chained dots; a unary sign is allowed initially and in exponents. Other input uses the general parser. This character-count threshold is a heuristic; retune it with benchmarks and differential tests.*)

(* ::Input::Initialization:: *)
$flatParserMinimumCharacters = 131072;


(* ::Text:: *)
(*Minimum factor occurrences per distinct factor required by the flat path. This factor-count heuristic never changes accepted syntax or results; retune with import benchmarks and boundary/differential parser tests.*)

(* ::Input::Initialization:: *)
$flatParserMinimumReusePerDistinctFactor = 4;


$flatFactorPattern =
  "(?:cfv[0-9]++\\s*+\\(\\s*+cfi[0-9]++\\s*+\\)|d_\\s*+\\(\\s*+cfi[0-9]++\\s*+,\\s*+cfi[0-9]++\\s*+\\)|(?:cfv[0-9]++\\s*+\\.\\s*+cfv[0-9]++|cfs[0-9]++|cfa[0-9]++|cfd[0-9]++|i_|[0-9]++)(?:\\s*+\\^\\s*+[+-]?+\\s*+[0-9]++)?)";


(* ::Text:: *)
(*prepareFlatResult[text]: lexical eligibility test for the fast path. Deliberately LEXICAL ONLY -- it scans short tokens and then validates their alternation. A repeated whole-result regex would risk exhausting the matcher's internal repetition limit on large valid outputs, so no pattern here repeats over the whole text. Returns the validated tokens themselves (as flatTokenData), so the parser never has to scan the text a second time. $Failed means "use the general parser" -- it does NOT mean the result is invalid.*)

(* ::Input::Initialization:: *)
prepareFlatResult[text_String] := Module[
     {tokens, count, start, distinctFactors, 
    operators = {"+", "-", "*", "/"}},
     (*Lex with the flat factor pattern plus the four operators.*)
     tokens = 
    StringCases[text, RegularExpression[$flatFactorPattern <> "|[+*/-]"]];
     count = Length[tokens];
     If[count === 0, Return[$Failed]];
     (*Check lexical coverage exactly as in the general parser. Tokenize before removing whitespace so separate identifiers cannot be joined accidentally. The comparison uses WhitespaceCharacter on both strings, including Unicode whitespace.*)
     If[StringReplace[StringJoin[tokens], WhitespaceCharacter -> ""] =!=
          StringReplace[text, WhitespaceCharacter -> ""], 
    Return[$Failed]];
     (*A leading unary sign is allowed only at the very start.*)
     start = If[MemberQ[{"+", "-"}, First[tokens]], 2, 1];
     (*Factor/operator alternation requires an ODD number of tokens from start onward: F (op F)*.*)
     If[count < start || EvenQ[count - start + 1], Return[$Failed]];
     (*Factors occur at start, start + 2, ...; operators occur at start + 1, start + 3, .... Their absolute parity depends on the optional leading sign.*)
     distinctFactors = DeleteDuplicates[tokens[[start ;; ;; 2]]];
     If[Intersection[distinctFactors, operators] =!= {} ||
          (count > start && 
       Complement[DeleteDuplicates[tokens[[start + 1 ;; ;; 2]]], 
         operators] =!= {}),
        Return[$Failed]
      ];
     (*Require at least $flatParserMinimumReusePerDistinctFactor occurrences per distinct factor, four by default. The factor count is Quotient[count - start, 2] + 1. Equality is accepted; this boundary is not identical to the former token-count inequality.*)
     If[Quotient[count - start, 2] + 1 < 
        $flatParserMinimumReusePerDistinctFactor Length[distinctFactors], 
    Return[$Failed]];
     flatTokenData[tokens, start]
   ];


(* ::Text:: *)
(*Consume the validated token packet without tokenizing again. Reconstruct each distinct factor lazily through the general parser, retaining arithmetic and validation order. parseResult, not this definition alone, supplies fallback for ineligible input.*)

(* ::Input::Initialization:: *)
(* Grouped, commuting imports can resolve distinct factors in bulk. Lexical
   validation has already established factor/operator alternation. Each factor
   still uses the restricted decoder; input is never evaluated as Wolfram code.
   A failed speculative decode replays the ordered parser, preserving the first
   diagnostic (for example, division by zero before a later unknown name).
   The shared environment is enabled only when mapped symbols have no UpValues.
   Other imports retain per-occurrence evaluation in the ordered parser. *)
parseFlatResult[packet : flatTokenData[_List, _Integer], values_Association, dim_] :=
    Module[{result},
        If[!TrueQ[$importParserActive], Return[parseFlatResultOrdered[packet, values, dim]]];
        result = Catch[parseFlatResultBulk[packet], $failureTag];
        If[FailureQ[result], parseFlatResultOrdered[packet, values, dim], result]
    ];

parseFlatResultBulk[flatTokenData[tokens_List, start_Integer]] := Module[
    {factors, operators, distinct, dictionary, values, divisions, breaks,
     starts, ends, signs, terms},
    factors = tokens[[start ;; ;; 2]];
    operators = If[Length[tokens] > start, tokens[[start + 1 ;; ;; 2]], {}];
    distinct = DeleteDuplicates[factors];
    dictionary = AssociationThread[distinct, importFactor /@ distinct];
    If[!FreeQ[Values[dictionary], _vectorToken | _indexToken | _caAToken | _caFToken],
        throwFailure["InvalidResult", "A vector or index occurs outside a tensor object."]];
    values = Lookup[dictionary, factors];
    divisions = Flatten[Position[operators, "/"]] + 1;
    If[MemberQ[values[[divisions]], 0], throwFailure["InvalidResult", "Division by zero."]];
    If[divisions =!= {}, values = MapAt[1/# &, values, List /@ divisions]];
    (* Locate additive boundaries with built-in list operations, then construct
       each product once. This avoids an interpreted loop for every occurrence
       of a factor, while retaining nested coefficient factorisation. *)
    breaks = Flatten[Position[operators, "+" | "-"]];
    starts = Prepend[breaks + 1, 1]; ends = Append[breaks, Length[factors]];
    signs = Prepend[Replace[operators[[breaks]], {"+" -> 1, "-" -> -1}, {1}],
        If[start === 2 && First[tokens] === "-", -1, 1]];
    terms = Apply[Times, TakeList[values, ends - starts + 1], {1}];
    Total[signs terms]
];

parseFlatResultOrdered[flatTokenData[tokenList_List, start_Integer],
   values_Association, dim_] := Module[
     {tokens = tokenList, count = Length[tokenList], pos = start,
       factor, checked, product, firstSign, result, terms, op, r},
     Internal`WithLocalSettings[Null,
     firstSign = If[start === 2 && First[tokens] === "-", -1, 1];
     (*Memoize successful general-parser results by factor text within this call. A thrown failure exits before Set stores a value. No result text is evaluated as Wolfram source.*)
     factor[t_] := factor[t] = If[TrueQ[$importParserActive], importFactor[t],
         parseGeneralResult[t, values, dim]];
     (*Identical typed-token gate as the general parser.*)
     checked[x_] := If[! FreeQ[x, _vectorToken | _indexToken],
         
     throwFailure["InvalidResult", 
      "A vector or index occurs outside a tensor object."], x];
     (*Apply an initial unary minus to the first factor, matching the general parser. Later subtraction negates an entire product. Division validates its right operand before subsequent factors are read.*)
     product[sign_] := 
    With[{v = 
       If[sign === -1, -checked[factor[tokens[[pos++]]]], 
        factor[tokens[[pos++]]]]},
         If[pos > count || ! MemberQ[{"*", "/"}, tokens[[pos]]], v,
            Module[{operation, right, factors},
               factors = Reap[
                    Sow[checked[v]];
          While[pos <= count && MemberQ[{"*", "/"}, tokens[[pos]]],
                       operation = tokens[[pos++]];
                       right = checked[factor[tokens[[pos++]]]];
           If[operation === "/" && right === 0, 
            throwFailure["InvalidResult", "Division by zero."]];
                       Sow[If[operation === "*", right, 1/right]]
                     ]
                  ][[2, 1]];
               Times @@ factors
             ]
          ]
       ];
     (*Top level: one product, then +/- chains of products.*)
     result = product[firstSign];
     If[pos <= count,
        terms = Reap[
             Sow[checked[result]];
             While[pos <= count,
                op = tokens[[pos++]];
                r = checked[product[1]];
                Sow[If[op === "+", r, -r]]
              ]
           ][[2, 1]];
        result = Plus @@ terms
      ];
     checked[result],
     Clear[tokens, factor, checked, product]
     ]
   ];


(* ::Text:: *)
(*Try flat-path eligibility at or above $flatParserMinimumCharacters (131072 characters by default). Symbols with UpValues select the general parser to avoid skipping their per-occurrence arithmetic through factor caching. Eligibility messages and unexpected return values also select the general parser; Abort propagates. The thresholds are heuristics, not a universal performance guarantee.*)

(* ::Input::Initialization:: *)
parseResult[text_String, values_Association, dim_] := 
  Module[{prepared = $Failed},
     (* Short momentum monomials recur between coefficient leaves. Decode them
        once per import instead of constructing another general-parser state. *)
     If[TrueQ[$importParserActive] && StringLength[text] < $flatParserMinimumCharacters,
         Return[importFactor[text]]];
     If[StringLength[text] >= $flatParserMinimumCharacters &&
          
     (TrueQ[$importParserActive] || AllTrue[DeleteDuplicates[
       Cases[{Values[values], dim}, _Symbol, Infinity, Heads -> True]],
      UpValues[#] === {} &]),
        (*This is an optional optimization. Local messages or an unevaluated eligibility result must not escape as an apparently successful import. Check does not catch Abort, so user cancellation still propagates.*)
        prepared = Quiet[Check[prepareFlatResult[text], $Failed]]
      ];
     (*Match on the packet type itself: prepareFlatResult returning $Failed (or anything else) routes to the general parser.*)
     If[MatchQ[prepared, flatTokenData[_List, _Integer]],
        parseFlatResult[prepared, values, dim],
        parseGeneralResult[text, values, dim]
      ]
   ];


(* ::Subsection:: *)
(*Mapping validation and public importer*)


(* ::Text:: *)
(*Decode and validate every entry ONCE, before any parsing happens. "Including entries eliminated by FORM" is the point: FORM may have optimized an identifier away entirely, but the mapping still owns it, so it is decoded and kind-validated here and stays available for reconstruction. The resulting association doubles as the parser's symbol table, carrying not just values but vector/index TYPES -- that is how parseGeneralResult can tell a declared vector from a scalar. Ordering rule stated in the header: the denominator's "Expression" data is authoritative; convenience metadata (Momentum, Mass, Power, Dimension, Prescription) never overrides it during reconstruction.*)

(* ::Input::Initialization:: *)
decodeEntries[entries_List, dim_:Automatic] := Association[
     Map[
        Function[entry,
           (*Reconstruct the value from its encoded form using the general decoder. This is the step that can throw "InvalidMapping" for bad data or unsupported structure.*)
           Module[{value = decode[entry["Expression"]]},
              (*Now apply the kind's OWN validity predicate -- the pure functions stored in $kindSpecs. This is the payoff of keeping those functions in the kind table: the same vocabulary shared with export is enforced on import. TrueQ makes a non-boolean (an unevaluated predicate, say) a failure rather than a silent pass.*)
      If[! TrueQ[$kindSpecs[entry["Kind"]]["ValidExpression"][value]] ||
          (dim =!= Automatic && !consistentMappedDimensionQ[value, dim]),
                 
       throwFailure["InvalidMapping", 
        "Mapped expression does not match its declared kind."]
               ];
              (*Wrap Vector and Index values in distinct parser tokens. The default Switch branch stores scalars, abbreviations and denominators as plain expressions. Entry kinds have already been checked by the public importer.*)
              entry["Name"] -> Switch[entry["Kind"],
                  "Vector", vectorToken[value],
                  "Index", indexToken[value],
                  "ColourWord", colourWordToken[value[[1]]],
                  "ColourAdjointIndex", caAToken[value],
                  "ColourFundamentalIndex", caFToken[value],
                  _, value
                ]
            ]
         ],
        entries
      ]
   ];


(* ::Text:: *)
(*Public import entry point. The pipeline is a deliberate mirror of export: read and identify the file pair, validate correspondence, decode and validate all mapped values, and only then parse the result text. Note the ordering rationale: the digest check is a cheap identity check on the PAIR of files, while parsing is where grammar and type errors are found -- so a mismatched pair fails before any effort is spent on the result. The header comment is explicit that the digest detects a mismatched file pair; it does NOT replace expression or grammar validation.*)

(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormImport[resultFile_String, 
   mappingFile_String] := Catch[
     Module[{json, mapping, text, lines, digest, entries, dim, names, grouped,
     values, algebraMapping, capabilities},
        (*Read the mapping as UTF-8 text first. Legacy results retain their text reader; grouped results use an owned, bounded byte stream. Read failures do not catch user aborts.*)
        json = 
     Quiet[Check[
       Import[mappingFile, "Text", 
        CharacterEncoding -> "UTF-8"], $Failed]];
        If[!StringQ[json], throwFailure["ReadFailed", "Cannot read the result or mapping file."]];
        (*Try RawJSON string import first. If it fails, retry using an explicit UTF-8 byte buffer to handle Unicode import differences. Format and entry validation follow separately.*)
        mapping = Quiet[Check[ImportString[json, "RawJSON"], $Failed]];
        If[mapping === $Failed,
           mapping = 
      Quiet[Check[
        ImportByteArray[ByteArray[ToCharacterCode[json, "UTF-8"]], 
         "RawJSON"], $Failed]]];
        (*Identity gate: it must be an association, must declare this format name and a supported integer format version; and Lookup with a None default handles a missing key without a message.*)
        If[! AssociationQ[mapping] || 
      Lookup[mapping, "Format", None] =!= $formatName ||
             !MemberQ[{1, 2, 3, 4, 5, 6, 7, 8}, Lookup[mapping, "Version", None]],
           throwFailure["InvalidMapping", "Unsupported mapping format or version."]];
        smValidateMapping[mapping];
        capabilities = mappingCapabilities[mapping];
        algebraMapping = capabilities["AlgebraMapping"];
        grouped = capabilities["Grouped"];
        digest = Hash[#, "SHA256", "HexString"] & /@
            {json, FromCharacterCode[ToCharacterCode[json, "UTF-8"]]};
        If[grouped, pgWithStream[resultFile, Function[stream, pgHeader[stream, mapping, digest]]]];
        If[!grouped,
            text = Quiet[Check[Import[resultFile, "Text", CharacterEncoding -> "UTF-8"], $Failed]];
            If[!StringQ[text], throwFailure["ReadFailed", "Cannot read the result or mapping file."]];
            lines = StringSplit[StringReplace[text, "\r\n" -> "\n"], "\n"];
            If[Length[lines] < 2 || !MemberQ[(resultMarker[mapping["Version"]] <> " " <> # &) /@ digest, First[lines]],
                throwFailure["MappingMismatch", "The result does not correspond to this mapping file."]]
        ];
        (*Structural validation of the entries list BEFORE any value is decoded.*)
        entries = Lookup[mapping, "Entries", None];
        If[! ListQ[entries] || ! AllTrue[entries, AssociationQ], 
     throwFailure["InvalidMapping", "Invalid mapping entries."]];
        (*Identifier validation: every entry must have a name that obeys validEntryNameQ (kind prefix + digits), names must be unique, and every entry must carry an "Expression" key to decode. Note Lookup threads over the list of associations with a {} default, so a missing "Name" surfaces as Missing here rather than as a message.*)
        If[algebraMapping["Version"] < 3 && AnyTrue[entries,
            MemberQ[{"ColourTensor", "ColourWord", "NamedCoupling"}, Lookup[#, "Kind", None]] &],
            throwFailure["InvalidMapping", "Dirac/colour entries require version three."]];
        If[algebraMapping["Version"] < 5 && AnyTrue[entries, MemberQ[$caKinds, Lookup[#, "Kind", None]] &],
            throwFailure["InvalidMapping", "New colour entries require version five."]];
        names = Lookup[entries, "Name", {}];
        If[! DuplicateFreeQ[names] || ! AllTrue[entries,
               validEntryNameQ[#] &&
                 KeyExistsQ[#, "Expression"] &], 
     throwFailure["InvalidMapping", "Invalid or duplicate mapping identifiers."]];
        (*Dimension: decoded with the same strictness as the export side -- Symbol or Integer >= 2, with I explicitly excluded. It is validated BEFORE any entry is decoded, so a bad dimension is reported without first building expressions.*)
        dim = decode[Lookup[mapping, "Dimension", None]];
        If[! 
       MatchQ[dim, _Symbol | _Integer] || (IntegerQ[dim] && dim < 2) || 
      dim === I, throwFailure["InvalidMapping", "Invalid mapped dimension."]];
        (*Only now decode every mapped value: name -> value, with vectors and indices wrapped in their typed tokens.*)
        values = If[KeyExistsQ[mapping,"LoopCoefficientReduction"], None, decodeEntries[entries, dim]];
        (*Hand the result text (everything after the marker line) to the parser together with the symbol table and dimension.*)
        Block[{$smEnabled = (mapping["Version"]===8), $smEligibilityCache=<||>, $smDropScaleless=Lookup[Lookup[mapping,"TensorDecomposition",<||>],"DropScaleless",False], $tdImport = <||>, $tdCoefficientCache = <||>, $lrScalarCache = <||>, $lrScalarCacheBytes = 0, $importParserActive = False, $caImport = <||>, $epsilonImportFactor = None, $diracImportLine = None, $daImportLines = <||>, $daImportMode = False},
            tdImportSetup[mapping, dim];
            (* Check frozen reduction kinematics before decoding inverse scalar
               coefficients: changed exceptional invariants must not evaluate 1/0. *)
            If[values === None, values = decodeEntries[entries, dim]];
            If[algebraMapping["Version"] >= 3,
                If[!MemberQ[{True, False}, Lookup[mapping, "EpsilonPresent", None]] ||
                    Lookup[mapping, "EpsilonPresent", None] =!= KeyExistsQ[mapping, "EpsilonConvention"],
                    throwFailure["InvalidMapping", "Invalid version-three epsilon metadata."]];
                If[Lookup[mapping, "DiracSpinLine", None] =!= 1,
                    throwFailure["InvalidMapping", "Invalid reserved Dirac spin line."]];
                $diracImportLine = 1
            ];
            If[mapping["Version"] === 2 || (mapping["Version"] >= 3 && KeyExistsQ[mapping, "EpsilonConvention"]),
                With[{convention = Lookup[mapping, "EpsilonConvention", <||>]},
                    If[!AssociationQ[convention], throwFailure["InvalidMapping", "Invalid epsilon convention."]];
                    With[{sign = decode[Lookup[convention, "Sign", None]],
                          factor = decode[Lookup[convention, "ExportFactor", None]]},
                        If[!MemberQ[{-1, 1, -I, I}, sign] || factor =!= -I sign,
                            throwFailure["InvalidMapping", "Invalid epsilon convention or translation factor."]];
                        If[sign =!= $LeviCivitaSign,
                            throwFailure["EpsilonConventionMismatch", "The current $LeviCivitaSign differs from the exported convention."]];
                        $epsilonImportFactor = factor
                    ]
                ]
            ];
            If[algebraMapping["Version"] >= 4, daValidateMetadata[algebraMapping, entries]];
            If[algebraMapping["Version"] === 5, caValidateMetadata[algebraMapping, entries]];
            If[grouped,
                Return[smImportResult[pgImport[resultFile, mapping, digest, values, dim],mapping]]];
            If[$diracImportLine === None,
                parseResult[StringRiffle[Rest[lines], "\n"], values, dim],
                With[{prepared = caPrepareResult[StringRiffle[Rest[lines], "\n"], values]},
                    dcReconstruct[caReconstruct[parseGeneralResult[prepared[[1]], prepared[[2]], dim]]]]
            ]
        ]
      ], $failureTag];


(* ::Text:: *)
(*Same convention as the export entry point: a call that does not match the two-string signature returns a Failure object instead of throwing. So, as on the export side, a caller must inspect FailureQ on the result rather than relying on Catch to observe every error.*)

(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormImport[___] :=
  makeFailure["InvalidArguments",
    "Use CalcFormImport[resultFile, mappingFile]."];


