(* ::Section:: *)
(*SU(N) colour algebra: typed translation and FORM configuration*)

(* ::Text:: *)
(*Loaded in the converter's private context. One fundamental SU(N) group,
  tr(Ta Tb)=delta(a,b)/2. Tensor algebra belongs to the embedded FORM procedure;
  this companion validates boundaries and presents scalar Casimir factors.*)

(* ::Input::Initialization:: *)
CalcFormConverter`ColourAlgebra::usage = "ColourAlgebra is an option for CalcFormExport and CalcFormCalculate. True (the default) or Automatic reduces one fundamental SU(N) colour group in FORM, with tr(Ta Tb)=delta(a,b)/2, CA=N and CF=(N^2-1)/(2N). False preserves colour objects and unevaluated SUNTrace expressions. Longer irreducible traces may remain. Connected implicit/explicit chains may return generated free fundamental endpoints.";
$caMode = False;
$caActive = False;
$caImplicit = False;
$caNamespace = "";
$caEndpoints = {};
$caImport = <||>;
$caProcedureID = "CFC-SUn-1";
$caUpstreamSHA = "05548386b1e5a2872224a78bad6a424fb13f812ee6ab35d89d1c77a32f7034c3";
$caKinds = {"ColourAdjointIndex", "ColourFundamentalIndex", "ColourTrace"};
caVocabularyQ[x_] := !FreeQ[x, SUNT | SUNTF | SUNF | SUND | SUNDelta | SUNFDelta | SUNTrace | SUNN | CA | CF];
caTraceQ[caTrace[body_]] := FreeQ[body, _dcDiracWord | _daTrace | _caTrace] &&
    AllTrue[dcTerms[body], scalarQ[#[[1]]] && #[[2]] === {} &&
        AllTrue[#[[3]], colourIndexQ[#, SUNIndex] &] &];
caTraceQ[_] := False;
AssociateTo[$expressionSpecs, "ColourTraceBody" -> <|"Head" -> caTrace, "Arity" -> {1, 1}|>];
$expressionHeadByName = Map[#["Head"] &, $expressionSpecs];
$expressionNameByHead = Association[Reverse /@ Normal[$expressionHeadByName]];
AssociateTo[$kindSpecs, {
    "ColourAdjointIndex" -> <|"Prefix" -> "cfca", "Declaration" -> "ColourAdjoint", "ValidExpression" -> (colourIndexQ[#, SUNIndex] &)|>,
    "ColourFundamentalIndex" -> <|"Prefix" -> "cfcq", "Declaration" -> "ColourFundamental", "ValidExpression" -> (colourIndexQ[#, SUNFIndex] &)|>,
    "ColourTrace" -> <|"Prefix" -> "cfcr", "Declaration" -> "Symbols", "ValidExpression" -> caTraceQ|>
}];

(* ::Subsection:: *)
(*Trace normalisation and implicit identity branches*)

(* ::Input::Initialization:: *)
dcNormalize[x_SUNTrace] := Module[{args = List @@ x, opts, body},
    If[args === {} || !AllTrue[Rest[args], MatchQ[#, _Rule | _RuleDelayed] &],
        throwFailure["UnsupportedColourTrace", "Use SUNTrace[expression, options]."]];
    opts = Rest[args];
    If[!AllTrue[opts, MemberQ[First /@ Options[SUNTrace], First[#]] &&
        Last[#] === (First[#] /. Options[SUNTrace]) &],
        throwFailure["UnsupportedColourTrace", "Only default SUNTrace options are supported."]];
    If[!FreeQ[First[args], SUNTrace | DiracTrace | DiracGamma | SUNTF],
        throwFailure["UnsupportedColourTrace", "A colour trace must contain one implicit colour word and commuting scalar coefficients."]];
    body = caTrace[dcNormalize[First[args]]];
    If[!caTraceQ[body], throwFailure["UnsupportedColourTrace", "Unsupported colour trace body."]]; body
];

(* Scalar summands share the endpoints of an implicit matrix expression. Walk
   only its Plus/Times skeleton; never expand the complete tensor product. *)
caLift[x_Plus] := Plus @@ (If[dcSignature[#][[2]] === 0, # caIdentity, caLift[#]] & /@ List @@ x);
caLift[x_Times] := Times @@ (If[dcSignature[#][[2]] === 1, caLift[#], #] & /@ List @@ x);
caLift[x_] := x;
caPrepare[x_, digest_] := Module[{y = x},
    $caImplicit = dcSignature[y][[2]] === 1;
    $caNamespace = "CalcFormConverter`ColourIndices`h" <> digest <> "`";
    If[AnyTrue[Cases[y, _Symbol, Infinity, Heads -> True], StringStartsQ[Context[#], $caNamespace] &],
        throwFailure["UnsupportedColour", "Input collides with the generated colour-index namespace."]];
    If[$caImplicit, y = caLift[y]];
    y /. {CA -> SUNN, CF -> (SUNN^2 - 1)/(2 SUNN)}
];
caColourIndex[x_, register_] := Which[
    colourIndexQ[x, SUNIndex], register["ColourAdjointIndex", x],
    colourIndexQ[x, SUNFIndex], register["ColourFundamentalIndex", x],
    True, throwFailure["UnsupportedColour", "Colour indices must be symbolic and have a declared space."]
];
caAllocateEndpoints[register_] := If[$caEndpoints === {},
    $caEndpoints = caColourIndex[SUNFIndex[Symbol[$caNamespace <> #]], register] & /@ {"left", "right"}];
caNative[head_, args_List] := head <> "(" <> StringRiffle[args, ","] <> ")";
caEmitWord[a_List, register_] := (caAllocateEndpoints[register];
    If[a === {}, caNative["d_", $caEndpoints],
        caNative["cfcCT", Join[caColourIndex[#, register] & /@ a, $caEndpoints]]]);
caEmitTrace[caTrace[body_], emit_, register_] := "(" <> StringRiffle[
    ("(" <> emit[#[[1]]] <> "*" <>
        caNative["cfcCTr", caColourIndex[#, register] & /@ #[[3]]] <> ")") & /@ dcTerms[body], "+"] <> ")";
caEmitTensor[x_, register_] := Module[{args = List @@ x, names},
    If[Head[x] === SUNTF,
        Return[caNative[If[First[args] === {}, "d_", "cfcCT"],
            caColourIndex[#, register] & /@ Join[First[args], Rest[args]]]]];
    names = caColourIndex[#, register] & /@ args;
    Switch[Head[x],
        SUNDelta | SUNFDelta, caNative["d_", names],
        SUNF, caNative["cfcCF", names],
        SUND, "(2*(" <> caNative["cfcCTr", names] <> "+" <> caNative["cfcCTr", names[[{2, 1, 3}]]] <> "))",
        _, throwFailure["UnsupportedColour", "Unsupported colour tensor."]]
];
caConfigureEmitter[emit_Symbol, register_Symbol] := (
    emit[x_caTrace] := If[$caActive, caEmitTrace[x, emit, register], register["ColourTrace", x]];
    If[$caActive,
        register["Scalar", SUNN];
        emit[x_dcColourWord] := caEmitWord[x[[1]], register];
        emit[caIdentity] := caEmitWord[{}, register];
        emit[x : (_SUNF | _SUND | _SUNDelta | _SUNFDelta | _SUNTF)] := caEmitTensor[x, register]
    ]
);
caMetadata[entries_] := <|"Colour" -> <|
    "Mode" -> If[$caActive, "Automatic", "False"], "Group" -> "SU(N)",
    "Representation" -> "Fundamental", "TraceNormalisation" -> {1, 2}, "Flavours" -> 1,
    "Procedure" -> If[$caActive, $caProcedureID, "None"], "UpstreamSHA256" -> $caUpstreamSHA,
    "Rank" -> If[$caActive, First[Select[entries, #["Kind"] === "Scalar" && decode[#["Expression"]] === SUNN &]]["Name"], "None"],
    "IndexDictionary" -> Lookup[Select[entries, MemberQ[Take[$caKinds, 2], #["Kind"]] &], "Name", {}],
    "ImplicitEndpoints" -> $caEndpoints, "GeneratedNamespace" -> If[$caActive, $caNamespace, ""]|>|>;

(* ::Subsection:: *)
(*Self-contained FORM declarations and processing*)

(* ::Input::Initialization:: *)
caDeclarations[data_] := Module[{m = Lookup[data["Mapping"], "Colour", <||>], entries, rank, adj, fund},
    If[Lookup[m, "Mode", "False"] =!= "Automatic", Return[""]];
    entries = data["Mapping"]["Entries"]; rank = m["Rank"];
    adj = Lookup[Select[entries, #["Kind"] === "ColourAdjointIndex" &], "Name", {}];
    fund = Lookup[Select[entries, #["Kind"] === "ColourFundamentalIndex" &], "Name", {}];
    "\nSymbols cfcCNA,cfcCNorm,cfcCFlavours;\n" <>
    "Tensor cfcCT,cfcCTp,cfcCF(antisymmetric),cfcCD(symmetric);\nCFunction cfcCTr(cyclic);\n" <>
    "Dimension cfcCNA;\nIndices cfcCj1,cfcCj2,cfcCj3;\n" <>
    If[adj === {}, "", "Indices " <> StringRiffle[adj, ","] <> ";\n"] <>
    "Dimension " <> rank <> ";\nIndices cfcCi1,cfcCi2,cfcCi3,cfcCi4;\n" <>
    If[fund === {}, "", "Indices " <> StringRiffle[fund, ","] <> ";\n"] <>
    "Dimension " <> data["DimensionName"] <> ";\n"
];
caProcessing[data_] := Module[{m = Lookup[data["Mapping"], "Colour", <||>], proc},
    If[Lookup[m, "Mode", "False"] =!= "Automatic", Return[""]];
    proc = Import[FileNameJoin[{$moduleDirectory, "Templates", "ColourAlgebra.frm.in"}], "Text"];
    If[!StringQ[proc], throwFailure["InvalidTemplate", "Cannot read the SU(N) colour procedure."]];
    ".sort\nDimension " <> m["Rank"] <> ";\n" <> StringReplace[proc, "@COLOURRANK@" -> m["Rank"]] <> "\n.sort\nDimension " <> data["DimensionName"] <> ";\n"
];

(* ::Subsection:: *)
(*Version-five metadata, typed result calls and generated indices*)

(* ::Input::Initialization:: *)
caValidateMetadata[mapping_, entries_] := Module[{m = Lookup[mapping, "Colour", None], indices, endpoints, rank, ns, endpointEntries},
    If[!AssociationQ[m] || Sort[Keys[m]] =!= Sort[{"Mode", "Group", "Representation", "TraceNormalisation", "Flavours", "Procedure", "UpstreamSHA256", "Rank", "IndexDictionary", "ImplicitEndpoints", "GeneratedNamespace"}],
        throwFailure["InvalidMapping", "Invalid colour metadata fields."]];
    If[!MemberQ[{"Automatic", "False"}, m["Mode"]] || m["Group"] =!= "SU(N)" ||
        m["Representation"] =!= "Fundamental" || m["TraceNormalisation"] =!= {1, 2} || m["Flavours"] =!= 1 ||
        m["UpstreamSHA256"] =!= $caUpstreamSHA || m["Procedure"] =!= If[m["Mode"] === "Automatic", $caProcedureID, "None"],
        throwFailure["InvalidMapping", "Unsupported colour convention or procedure identity."]];
    indices = Lookup[Select[entries, MemberQ[Take[$caKinds, 2], #["Kind"]] &], "Name", {}];
    endpoints = m["ImplicitEndpoints"]; ns = m["GeneratedNamespace"];
    If[m["IndexDictionary"] =!= indices || !ListQ[endpoints] ||
        !MemberQ[{0, 2}, Length[endpoints]] || !DuplicateFreeQ[endpoints] || !AllTrue[endpoints, MemberQ[indices, #] &],
        throwFailure["InvalidMapping", "Invalid colour index dictionary or endpoints."]];
    If[m["Mode"] === "False",
        If[indices =!= {} || endpoints =!= {} || ns =!= "" || m["Rank"] =!= "None",
            throwFailure["InvalidMapping", "Translation-only colour metadata contains processing state."]],
        If[!StringQ[ns] || !StringMatchQ[ns, RegularExpression["CalcFormConverter`ColourIndices`h[0-9a-f]{64}`"]],
            throwFailure["InvalidMapping", "Invalid generated colour-index namespace."]];
        If[AnyTrue[entries, MemberQ[{"ColourTensor", "ColourWord", "ColourTrace"}, #["Kind"]] &],
            throwFailure["InvalidMapping", "Processed colour mappings cannot hide opaque colour objects."]];
        If[!DuplicateFreeQ[Lookup[Select[entries, MemberQ[Take[$caKinds, 2], #["Kind"]] &], "Expression"]],
            throwFailure["InvalidMapping", "Duplicate typed colour-index identities."]];
        If[AnyTrue[Select[entries, MemberQ[Take[$caKinds, 2], #["Kind"]] &],
            With[{v = decode[#["Expression"]]}, StringStartsQ[Context[Evaluate[v[[1]]]], ns]] && !MemberQ[endpoints, #["Name"]] &],
            throwFailure["InvalidMapping", "Undeclared generated free endpoint in the colour dictionary."]];
        rank = Select[entries, #["Name"] === m["Rank"] &];
        If[Length[rank] =!= 1 || rank[[1]]["Kind"] =!= "Scalar" || decode[rank[[1]]["Expression"]] =!= SUNN,
            throwFailure["InvalidMapping", "Invalid SU(N) rank entry."]];
        endpointEntries = Select[entries, MemberQ[endpoints, #["Name"]] &];
        If[endpoints =!= {} && (Lookup[endpointEntries, "Kind"] =!= {"ColourFundamentalIndex", "ColourFundamentalIndex"} ||
            (decode /@ Lookup[endpointEntries, "Expression"]) =!= (SUNFIndex[Symbol[ns <> #]] & /@ {"left", "right"})),
            throwFailure["InvalidMapping", "Generated endpoints must be the declared free fundamental indices."]]
    ];
    $caImport = m;
];

(* FORM's generated N<number>_? indices arise only in the fundamental sums of
   this procedure. Decode this exact lexical form, never arbitrary identifiers.
   The replacement does not evaluate source and lives only for this import. *)
caPrepareResult[text_, values_] := Module[{names, rules, more},
    If[Lookup[$caImport, "Mode", "False"] =!= "Automatic", Return[{text, values}]];
    names = DeleteDuplicates[StringCases[text, RegularExpression["(?<![A-Za-z0-9_])N[1-9][0-9]*_\\?(?![A-Za-z0-9_])"]]];
    rules = (# -> ("cfcGenerated" <> StringTake[#, {2, -3}])) & /@ names;
    more = Association[(Last[#] -> caFToken[SUNFIndex[Symbol[$caImport["GeneratedNamespace"] <> "d" <> StringDrop[Last[#], 12]]]]) & /@ rules];
    {StringReplace[text, rules], Join[values, more]}
];
caCall[name_, args_] := Module[{a, endpoints},
    If[Lookup[$caImport, "Mode", "False"] =!= "Automatic", throwFailure["InvalidResult", "Undeclared colour processing object."]];
    Switch[name,
        "d_", If[MatchQ[args, {_caAToken, _caAToken}], Return[caTensor["AdjointDelta", args[[All, 1]]]]];
            If[MatchQ[args, {_caFToken, _caFToken}], Return[caTensor["FundamentalDelta", args[[All, 1]]]]],
        "cfcCT", If[Length[args] >= 3 && AllTrue[Drop[args, -2], MatchQ[#, _caAToken] &] && MatchQ[Take[args, -2], {_caFToken, _caFToken}],
            Return[caTensor["Chain", args[[All, 1]]]]],
        "cfcCTr", If[Length[args] >= 4 && AllTrue[args, MatchQ[#, _caAToken] &], Return[caTensor["Trace", args[[All, 1]]]]],
        "cfcCF" | "cfcCD", If[Length[args] === 3 && AllTrue[args, MatchQ[#, _caAToken] &],
            Return[caTensor[If[name === "cfcCF", "F", "D"], args[[All, 1]]]]]
    ];
    throwFailure["InvalidResult", "Invalid colour object, index space or unreduced internal object."]
];
caTraceRestore[caTrace[body_]] := SUNTrace[dcReconstruct[body /. dcColourWord[a_List] :> colourWordToken[a]], SUNTraceEvaluate -> False];

(* Endpoint/dummy connectivity is checked term by term before restoring heads.
   Powers contribute their multiplicity. This walk does not expand tensors. *)
caIndexCounts[caTensor[_, a_List]] := Counts[a];
caIndexCounts[x_Times] := Merge[caIndexCounts /@ List @@ x, Total];
caIndexCounts[Power[x_, n_Integer]] /; n >= 0 := Map[n # &, caIndexCounts[x]];
caIndexCounts[_] := <||>;
caPresentScalar[c_] := Module[{r = Together[c], num, den, power = 0},
    If[r === 0, Return[0]];
    num = Numerator[r]; den = Denominator[r];
    (* Extract N^2-1 before factoring it into linear factors. Each successful
       division lowers the polynomial degree by two, so the loops terminate. *)
    If[PolynomialQ[num, SUNN] && PolynomialQ[den, SUNN],
        While[Exponent[num, SUNN] >= 2 && PolynomialRemainder[num, SUNN^2 - 1, SUNN] === 0,
            num = PolynomialQuotient[num, SUNN^2 - 1, SUNN]; power++];
        While[Exponent[den, SUNN] >= 2 && PolynomialRemainder[den, SUNN^2 - 1, SUNN] === 0,
            den = PolynomialQuotient[den, SUNN^2 - 1, SUNN]; power--]
    ];
    (Factor[num]/Factor[den] (2 CA CF)^power) /. SUNN -> CA
];
(* Validate each coefficient term before choosing one endpoint representation
   for the entire result, including results read a propagator group at a time. *)
caImplicitEligible[x_] := Module[{terms, endpoints, implicit, counts, tensors, validLine},
    If[$caImport === <||> || $caImport["Mode"] === "False", Return[True]];
    endpoints = If[$caImport["ImplicitEndpoints"] === {}, {}, SUNFIndex[Symbol[$caImport["GeneratedNamespace"] <> #]] & /@ {"left", "right"}];
    terms = If[Head[x] === Plus, List @@ x, {x}];
    validLine[t_] := (MatchQ[t, caTensor["Chain", a_List] /; Take[a, -2] === endpoints] || t === caTensor["FundamentalDelta", endpoints]);
    implicit = endpoints =!= {};
    Do[
        counts = caIndexCounts[term];
        If[AnyTrue[Values[counts], # > 2 &], throwFailure["InvalidResult", "A colour index occurs more than twice in a result term."]];
        If[AnyTrue[Keys[counts], StringStartsQ[Context[Evaluate[#[[1]]]], $caImport["GeneratedNamespace"]] &&
            StringStartsQ[SymbolName[Evaluate[#[[1]]]], "d"] && counts[#] =!= 2 &],
            throwFailure["InvalidResult", "A generated dummy index is not contracted exactly twice."]];
        If[term =!= 0 && endpoints =!= {} && !AllTrue[endpoints, Lookup[counts, #, 0] === 1 &],
            throwFailure["InvalidResult", "A generated free endpoint was lost or contracted."]];
        tensors = Cases[term, _caTensor, {0, Infinity}];
        If[endpoints =!= {} && !AnyTrue[tensors, validLine], implicit = False], {term, terms}];
    implicit
];
caReconstruct[x_, endpointChoice_: Automatic] := Module[{terms, endpoints, implicit, validLine, y, factors, rows, groups},
    If[$caImport === <||>, Return[x]];
    If[$caImport["Mode"] === "False", Return[x /. t_caTrace :> caTraceRestore[t]]];
    implicit = caImplicitEligible[x];
    If[endpointChoice === False, implicit = False];
    endpoints = If[$caImport["ImplicitEndpoints"] === {}, {}, SUNFIndex[Symbol[$caImport["GeneratedNamespace"] <> #]] & /@ {"left", "right"}];
    validLine[t_] := (MatchQ[t, caTensor["Chain", a_List] /; Take[a, -2] === endpoints] || t === caTensor["FundamentalDelta", endpoints]);
    y = If[implicit, x /. {t_caTensor /; validLine[t] :> If[t[[1]] === "Chain", colourWordToken[Drop[t[[2]], -2]], 1]}, x];
    y = y /. {caTensor["Chain", a_List] :> SUNTF[Drop[a, -2], a[[-2]], a[[-1]]],
        caTensor["FundamentalDelta", a_List] :> Apply[SUNFDelta, a],
        caTensor["AdjointDelta", a_List] :> Apply[SUNDelta, a],
        caTensor["F", a_List] :> Apply[SUNF, a], caTensor["D", a_List] :> Apply[SUND, a],
        caTensor["Trace", a_List] :> SUNTrace[Dot @@ (SUNT /@ a), SUNTraceEvaluate -> False]};
    (* Collect equal tensor factors; only the remaining commuting coefficient is
       factored and converted to Casimirs. No SUNSimplify or tensor algebra here. *)
    terms = If[Head[y] === Plus, List @@ y, {y}];
    rows = Map[Function[term, factors = If[Head[term] === Times, List @@ term, {term}];
        {Times @@ Select[factors, !FreeQ[#, Pair | Eps | DiracGamma | DiracTrace | diracWordToken | colourWordToken | daTraceToken | SUNT | SUNTF | SUNTrace | SUNF | SUND | SUNDelta | SUNFDelta] &],
         Times @@ Select[factors, FreeQ[#, Pair | Eps | DiracGamma | DiracTrace | diracWordToken | colourWordToken | daTraceToken | SUNT | SUNTF | SUNTrace | SUNF | SUND | SUNDelta | SUNFDelta] &]}], terms];
    groups = GatherBy[rows, First];
    Total[(#[[1, 1]] caPresentScalar[Total[#[[All, 2]]]]) & /@ groups]
];
