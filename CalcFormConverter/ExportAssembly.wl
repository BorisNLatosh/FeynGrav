(* Export construction and publication: private companion to CalcFormConverter.

   Loaded inside the converter's private context by CalcFormConverter.wl. It
   contains stage planning, in-memory conversion, FORM rendering, template
   validation, transactional publication and the public CalcFormExport entry
   point moved unchanged from the central converter in ARCH-01 Stage B. It
   launches no processes itself. *)

(* ::Section:: *)
(*Export: symbol mapping, serialization and FORM program generation*)


(* ::Subsection:: *)
(*Build conversion data in memory*)


(* ::Text:: *)
(*Plan multiplication only after serialization has fixed identifier and macro numbering. Each sum must have consistent index multiplicities across its terms, and no index may occur more than twice across all stages. Different stages may have different signatures. Ambiguous cases retain the original order.*)

(* ::Input::Initialization:: *)
stageIndexSignatures[stages_List] := Module[{counts, signatures},
	(*Leaf case: a Lorentz contraction. Counts all LorentzIndex objects at any depth, then KeySort gives a canonical key order so signatures from different stages are directly comparable with SameQ. The definition memoizes via counts[p] = ... so the same subexpression is only analysed once.*)
	counts[p_Eps] := counts[p] = KeySort[Counts[Cases[p, _LorentzIndex, Infinity]]];
	counts[p_Pair] := 
		counts[p] = KeySort[Counts[Cases[p, _LorentzIndex, Infinity]]];
		(*Sum: every term must have the SAME index signature, otherwise the sum is index-ambiguous and the whole planner is abandoned ($Failed). If they all agree, the signature of any one term is the signature of the sum.*)
		counts[x_Plus] := Module[{parts = counts /@ (List @@ x)},
		
		If[MemberQ[parts, $Failed] || ! SameQ @@ parts, $Failed, First[parts]]];
		(*Product: index counts add. Merge combines the per-factor associations by summing values for shared keys, so a mu that appears once in each of two factors ends up counted twice.*)
		counts[x_Times] := Module[{parts = counts /@ (List @@ x)},
			If[MemberQ[parts, $Failed], $Failed, KeySort[Merge[parts, Total]]]];
		(*Non-negative integer power: counts scale linearly with the exponent. Note the guard n >= 0 and the Integer requirement: a negative or symbolic power falls through to counts[_] below and is treated as carrying no indices.*)
		counts[Power[x_, n_Integer]] /; n >= 0 := 
			Module[{part = counts[x]},If[part === $Failed, $Failed, Map[n # &, part]]];
		(*Fallback for anything that is not a tensor structure: no indices. This makes the helper total, which the planner relies on when a stage is a pure coefficient.*)
		counts[_] := <||>;
		signatures = counts /@ stages;
		(*Reject the whole plan if any stage was ambiguous, or if any index occurs MORE THAN TWICE across the total set of stages -- that is exactly the situation where an index cannot be contracted unambiguously, so reordering factors could change the result.*)
		If[MemberQ[signatures, $Failed] ||AnyTrue[Values[Merge[signatures, Total]], # > 2 &], $Failed, signatures]
];


(* ::Text:: *)
(*Attach a non-sum tensor to its unique neighbouring sum before ordering the
  stages. In particular, both polarisation vectors of an external line should
  contract with that line before multiplying a large vertex. Arbitrary Times
  ordering can otherwise attach them to unrelated stages. Move a factor only
  when all its indices occur once and meet exactly one sum. Ambiguous index
  signatures and tensors connecting several sums retain the original grouping.
  This operates on positions after emission: mapping names remain unchanged.*)

(* ::Input::Initialization:: *)
attachStageFactors[factors_List, ranges_List] := Module[
    {groups = (Range @@ # &) /@ ranges, signatures, sums, owners, indices, targets,
     source, target},
    If[FreeQ[factors, _LorentzIndex], Return[groups]];
    signatures = stageIndexSignatures[factors];
    If[signatures === $Failed, Return[groups]];
    sums = Flatten[Position[factors, _Plus, {1}, Heads -> False]];
    owners = Association[Flatten[MapIndexed[Thread[#1 -> First[#2]] &, groups]]];
    Do[
        indices = Keys[signatures[[i]]];
        If[indices =!= {} && AllTrue[Values[signatures[[i]]], # === 1 &],
            targets = Select[sums, Function[j,
                AllTrue[indices, Lookup[signatures[[j]], #, 0] === 1 &]]];
            If[Length[targets] === 1,
                source = owners[i]; target = owners[First[targets]];
                If[source =!= target,
                    groups[[source]] = DeleteCases[groups[[source]], i];
                    groups[[target]] = Sort[Append[groups[[target]], i]]]
            ]
        ], {i, Complement[Range[Length[factors]], sums]}];
    groups
];


(* ::Text:: *)
(*The one-argument form computes stage signatures, using empty signatures for expressions without Lorentz indices, then delegates to the two-argument planner.*)

(* ::Input::Initialization:: *)
connectedStageOrder[stages_List] := connectedStageOrder[stages,
	If[FreeQ[stages, _LorentzIndex], 
	ConstantArray[<||>, Length[stages]], stageIndexSignatures[stages]]];


(* ::Text:: *)
(*The two-argument form consumes the supplied signatures, or $Failed, and returns stage positions in the chosen multiplication order.*)

(* ::Input::Initialization:: *)
connectedStageOrder[stages_List, signatures_] := Module[
	{free, sizes, greedy, baseline, candidates, score, starts},
	(* Ambiguous signatures never permit reordering. *)
	If[signatures === $Failed, Return[Range[Length[stages]]]];
	free = Keys[Select[#, # === 1 &]] & /@ signatures;
	If[AllTrue[free, # === {} &], Return[Range[Length[stages]]]];
	sizes = LeafCount /@ stages;
	(* Each starting point follows the existing connected greedy rule. This
	   explores at most eight paths, not all permutations of the factors. *)
	greedy[start_] := Module[{remaining, order = {start}, active = free[[start]], next},
		remaining = DeleteCases[Range[Length[stages]], start];
		While[remaining =!= {},
			next = First[SortBy[remaining,
				{-Length[Intersection[active, free[[#]]]],
				 If[free[[#]] === {}, 1, 0], sizes[[#]], #} &]];
			AppendTo[order, next];
			active = Complement[Union[active, free[[next]]], Intersection[active, free[[next]]]];
			remaining = DeleteCases[remaining, next]];
		order
	];
	starts = SortBy[Range[Length[stages]], {If[free[[#]] === {}, 1, 0], sizes[[#]], #} &];
	baseline = greedy[First[starts]];
	If[Length[stages] > 8, Return[baseline]];
	(* Open-index width is a cost proxy, not an algebraic condition. Prefer
	   a narrower maximum frontier, then less total width. Among ties,
	   narrower late intermediates are preferred because terms accumulate.
	   Preserve the baseline on a complete score tie. *)
	score[order_] := Module[{active = {}, widths},
		widths = Table[
			active = Complement[Union[active, free[[i]]], Intersection[active, free[[i]]]];
			Length[active], {i, order}];
		{Max[widths], Total[widths], Reverse[widths], If[order === baseline, 0, 1], order}
	];
	candidates = DeleteDuplicates[greedy /@ Select[starts, free[[#]] =!= {} &]];
	First[SortBy[candidates, score]]
];


(* ::Text:: *)
(*Minimum Wolfram leaf count that enables FORM stage preparation. This is a dispatch heuristic only and never changes accepted syntax or results; retune with representative FORM stage benchmarks and equivalence tests.*)

(* ::Input::Initialization:: *)
$stagePreparationMinimumLeaves = 1024;


(* ::Text:: *)
(*Normalize with FCI, validate the input, allocate mapping entries and serialize FORM expression pieces. Registry state and caches are local to this call. The helper performs no file I/O or FORM execution, but its result depends on package specifications and the receiving kernel's definitions.*)

(* ::Input::Initialization:: *)
buildExportData[expression_, requestedDimension_, loops_, algebra_:False, colour_:False] := Block[{$caMode = If[colour === True, Automatic, colour], $caActive = False, $caImplicit = False, $caNamespace = "", $caEndpoints = {}, $daMode = algebra, $daTraceLines = {}, $daNextLine = 1}, Module[
	{expr, originalDigest, hasDiracColour, hasDiracProcessing, hasColour, hasColourFormat, hasMatrixWords = False, dim, hasEpsilon, epsilonSign, epsilon, epsilonSlot, entries = {}, registry = <||>, counters = <||>,
	macros = {},
	register, scalar, vector, index, pair, denominator, emit, 
	abbreviation, makeMacro,body, dimensionName, payload, factorExpressions, factorTexts, 
	stageEnds, stageRanges, stageGroups,
	stageTexts, stageExpressions, stageSignatures, stageOrder,
	preparations = {}, multiplications = {}, symmetryLinks = {}, cancellationData},
	(*Validate LoopMomenta: must be a list, every element a symbol, and no duplicates. Anything else aborts before any work is done.*)
	If[! ListQ[loops] || ! AllTrue[loops, MatchQ[#, _Symbol] &] || ! DuplicateFreeQ[loops],
		throwFailure["InvalidLoopMomenta", 
		"LoopMomenta must be a list of distinct momentum symbols."]];
	(*FCI converts supported external shortcuts to FeynCalc internal notation before structural inspection. It is not a tensor contraction or integral-reduction step.*)
    If[!MemberQ[{Automatic, False}, algebra], throwFailure["InvalidOption", "DiracAlgebra must be Automatic or False."]];
	If[!MemberQ[{True, Automatic, False}, colour], throwFailure["InvalidOption", "ColourAlgebra must be True, Automatic or False."]];
	expr = FCI[expression];
    hasEpsilon = !FreeQ[expr, _Eps];
    epsilonSign = $LeviCivitaSign;
    If[hasEpsilon && !MemberQ[{-1, 1, -I, I}, epsilonSign],
        throwFailure["UnsupportedEpsilonConvention", "Unsupported $LeviCivitaSign value."]];
	(*Resolve/infer the dimension now, so it is fixed before names and entries are allocated.*)
	dim = chooseDimension[expr, requestedDimension];
    hasDiracColour = diracColourQ[expr];
    hasColour = caVocabularyQ[expr];
    $caActive = MemberQ[{True, Automatic}, colour] && hasColour;
    hasColourFormat = $caActive || !FreeQ[expr, SUNTrace];
    hasDiracProcessing = !FreeQ[expr, DiracTrace] || (algebra === Automatic && !FreeQ[expr, DiracGamma]);
    originalDigest = Hash[expr, "SHA256", "HexString"];
    If[hasDiracColour, expr = dcNormalize[expr];
        hasMatrixWords = !FreeQ[expr, _dcDiracWord | _dcColourWord | _daTrace]];
    If[$caActive, expr = caPrepare[expr, originalDigest]];
	(*register[kind, value, extra]: THE allocator and the only place identifiers are minted and mapping entries appended. Returns the existing name if this exact (kind, value) was seen before, so identical objects map to one identifier.*)
	register[kind_, value_, extra_ : <||>] := With[{key = HoldComplete[kind, value]},
	(*HoldComplete preserves the structural key without evaluating its contents again; kind distinguishes the same value in different roles. Only insertion allocates locals and serializes the mapped expression.*)
		If[KeyExistsQ[registry, key], registry[key],
			Module[{name, number, prefix},
			(*Numbering is per kind, so scalars, vectors, indices and denominators each get their own 1, 2, 3, ... sequence.*)
			number = Lookup[counters, kind, 0] + 1;
			AssociateTo[counters, kind -> number];
			prefix = $kindSpecs[kind]["Prefix"];
			name = prefix <> intString[number];
			AssociateTo[registry, key -> name];
			(*The mapping entry records kind, name and the encoded expression, merged with any kind-specific extras (denominators add Momentum/Mass/Power/Dimension/ Prescription). AppendTo preserves traversal order, which is what keeps numbering and the mapping file aligned.*)
				AppendTo[entries, Join[<|"Name" -> name, "Kind" -> kind, "Expression" -> encode[value]|>, extra]];
				name
			]
		]
	];
	(*makeMacro wraps an emitted sum in a FORM #define and returns its reference. Each sum, including a nested sum, allocates its own macro. Do not memoize this operation: numbering must follow serialization order.*)
	makeMacro[s_] := Module[{name = "CFCF" <> intString[Length[macros] + 1]},
		AppendTo[macros, "#define " <> name <> " \"(" <> s <> ")\""];
		"`" <> name <> "'"];
	(*scalar[s]: emit a scalar symbol. The imaginary unit is special- cased to FORM's predefined i_ rather than becoming an exported identifier.*)
	scalar[s_Symbol] := If[s === I, "i_", register["Scalar", s]];
	(*index: a LorentzIndex must have a symbol as its first argument; anything else is refused rather than silently named.*)
	index[LorentzIndex[i_Symbol, ___]] := register["Index", i];
	index[_] := throwFailure["UnsupportedIndex", "Lorentz indices must be symbols."];
	(*vector: decompose a momentum into {coefficient, identifier} pairs, i.e. the linear combination is returned as a list of term/coefficient records. Polarization labels stay atomic vector identities even when their momentum is routed.*)
	vector[Momentum[v_, ___]] := Module[{terms},
		terms = If[Head[v] === Plus, List @@ v, {v}];
		Map[
			Function[term,
				Module[{factors, momenta, coefficients},
				(*A bare vector identity: coefficient 1.*)
				If[vectorIdentityQ[term],
				{1, register["Vector", term]},
				(*Otherwise it must be a Times; anything else is an unsupported routing form.*)
				If[Head[term] =!= Times,
					throwFailure["UnsupportedMomentum", "Momentum routing must be a linear combination with exact rational coefficients."]];
				factors = List @@ term;
				(*Exactly one vector identity factor, and every remaining factor an exact Integer/Rational coefficient. Anything else (two vectors in a product, a symbolic coefficient) is rejected.*)
				momenta = Select[factors, vectorIdentityQ];
				coefficients = Select[factors, ! vectorIdentityQ[#] &];
				If[Length[momenta] =!= 1 || ! AllTrue[coefficients, MatchQ[#, _Integer | _Rational] &],
					throwFailure["UnsupportedMomentum", 
					"Momentum routing must be a linear combination with exact rational coefficients."]
				];
				{Times @@ coefficients, register["Vector", First[momenta]]}]
				]
			],
			terms
		]
	];
	(*vector of a Plus: handle the sum by mapping over its arguments.*)
	vector[x_Plus] := Flatten[vector /@ (List @@ x), 1];
	(*vector of a Times with an exact numeric coefficient in front: factor the coefficient out and scale each returned term.*)
	vector[Times[c : (_Integer | _Rational), m_Momentum]] := ({c #[[1]], #[[2]]} & /@ vector[m]);
	vector[_] := throwFailure["UnsupportedMomentum", "Expected a linear combination of dimension-tagged momenta."];
	(*pair: FORM rendering of Lorentz contractions, memoized only within this Module (the memo lives on the local symbol pair). Successful fragments are cached; failures throw before the assignment could happen, so nothing bad is ever cached. Metric contraction d_(i,j).*)
	pair[p : Pair[a_LorentzIndex, b_LorentzIndex]] := pair[p] = "d_(" <> index[a] <> "," <> index[b] <> ")";
	(*Index contracted with a momentum: sum over the routed terms, each contributing coefficient * p_(i).*)
	pair[p : Pair[a_LorentzIndex, b_Momentum]] := pair[p] = Module[{i = index[a]},
		"(" <> StringRiffle[("(" <> emit[#[[1]]] <> "*" <> #[[2]] <> "(" <> i <> "))") & /@ vector[b], "+"] <> ")"];
	(*Symmetry: Pair[Momentum, LorentzIndex] reuses the other order.*)
	pair[Pair[a_Momentum, b_LorentzIndex]] := pair[Pair[b, a]];
	(*Momentum-momentum: full double sum over routed terms, emitting coefficient * u.v for every pair of terms from the two vectors.*)
	pair[p : Pair[a_Momentum, b_Momentum]] := 
		pair[p] = Module[{va = vector[a], vb = vector[b]},
		"(" <> StringRiffle[Flatten[Table["(" <> emit[u[[1]] v[[1]]] <> "*" <> u[[2]] <> "." <> v[[2]] <> ")",{u, va}, {v, vb}]], "+"] <> ")"];
	pair[_] := throwFailure["UnsupportedPair", "Only Lorentz metrics, momentum components and scalar products are supported."];
    (* Expand only linear routing inside epsilon slots, preserving their order. *)
    epsilonSlot[a_LorentzIndex] := {{1, index[a]}};
    epsilonSlot[a_Momentum] := vector[a];
    epsilonSlot[_] := throwFailure["UnsupportedEpsilon", "Epsilon slots must be Lorentz indices or momenta."];
    (* FORM contracts the raw square positively; -I sign supplies FeynCalc's -sign^2. *)
    epsilon[x_Eps] := epsilon[x] = Module[{terms},
        If[Length[x] =!= 4, throwFailure["UnsupportedEpsilon", "Only rank-four Lorentz epsilon tensors are supported."]];
        terms = Tuples[epsilonSlot /@ (List @@ x)];
        If[terms === {}, Return["0"]];
        "(" <> StringRiffle[("(" <> emit[-I epsilonSign Times @@ #[[All, 1]]] <>
            "*e_(" <> StringRiffle[#[[All, 2]], ","] <> "))") & /@ terms, "+"] <> ")"
    ];
	(*denominator: one ordinary propagator denominator identifier.*)
	denominator[pd : PropagatorDenominator[mom_, mass_ : 0]] := 
		denominator[pd] = Module[{},
		(*Routing with polarization vectors inside a propagator is not supported, so reject before doing anything else.*)
		If[! FreeQ[mom, _Polarization], 
			throwFailure["UnsupportedMomentum", "Propagator routing cannot contain polarization vectors."]];
		(*Force the momentum through vector[] even though the result is discarded: this REGISTERS the vector and validates the routing form as a side effect.*)
		If[!propagatorRoutingQ[mom], throwFailure["UnsupportedMomentum",
          "Propagator routing must be a linear combination with exact rational coefficients."]];
		vector[mom];
		(*Mass must be an exact scalar expression.*)
		If[! scalarQ[mass], throwFailure["UnsupportedMass", "Propagator masses must be exact scalar expressions."]];
		(*Repeated propagators remain repeated factors or powers of the same identifier. The encoded FeynAmpDenominator is authoritative for reconstruction; Momentum, Mass, Power, Dimension and Prescription describe the original denominator; surviving powers are reconstructed after cancellation.*)
			register["Denominator", FeynAmpDenominator[pd], <|
				"Momentum" -> encode[mom], "Mass" -> encode[mass], 
				"Power" -> 1,
				"Dimension" -> encode[dim], 
				"Prescription" -> "Feynman+i0"|>]
		];
	denominator[x_] := throwFailure["UnsupportedDenominator", 
		"Only ordinary quadratic PropagatorDenominator objects are supported.", <|"Expression" -> HoldForm[x]|>];
		(*abbreviation: allocate a scalar identifier for a whole scalar subexpression, memoized so repeats reuse the same name.*)
		abbreviation[x_] := abbreviation[x] = If[scalarQ[x], register["Abbreviation", x],
			throwFailure["UnsupportedScalar", "Only supported scalar expressions may be abbreviated."]];
	(*emit: the expression -> FORM text dispatcher, ordered by specificity. Each clause is a Which test.*)
	emit[x_] := Which[
		(*Exact integers.*)
		IntegerQ[x], intString[x],
		(*Exact rationals, parenthesized as (num/den).*)
		Head[x] === Rational, "(" <> intString[Numerator[x]] <> "/" <> intString[Denominator[x]] <> ")",
		(*Exact complex: rendered with FORM's i_ and explicit signs.*)
		Head[x] === Complex && scalarQ[x], "(" <> emit[Re[x]] <> "+i_*" <> emit[Im[x]] <> ")",
		(*Scalar symbol (includes I -> i_ via scalar).*)
		Head[x] === Symbol && scalarQ[x], scalar[x],
		Head[x] === CalcFormConverter`LoopTensorCoefficient || Head[x] === PaVe, register["IntegralCoefficient", x],
		(*A sum becomes a #define macro, so the (possibly long) expression is named once and referenced thereafter. This is why emission order determines macro numbering.*)
		Head[x] === Plus, makeMacro[StringRiffle[emit /@ (List @@ x), "+"]],
		(*Products are parenthesized with explicit *.*)
		Head[x] === Times, "(" <> StringRiffle[emit /@ (List @@ x), "*"] <> ")",
		(*Lorentz contractions.*)
		Head[x] === Pair, pair[x],
        Head[x] === Eps, epsilon[x],
		(*A product of propagator denominators.*)
		Head[x] === FeynAmpDenominator, "(" <> StringRiffle[denominator /@ (List @@ x), "*"] <> ")",
		(*Non-negative integer powers, plus negative powers of a plain symbol, stay as explicit powers of the emitted base.*)
		Head[x] === Power && IntegerQ[x[[2]]] && (x[[2]] >= 0 || Head[x[[1]]] === Symbol),
			"(" <> emit[x[[1]]] <> ")^(" <> intString[x[[2]]] <> ")",
		(*Any other power must be a supported scalar to be abbreviated into a named identifier.*)
		Head[x] === Power, abbreviation[x],
		(*Master integrals (A0..D0) with valid scalar arguments emit as the FORM function recorded in the spec table.*)
		KeyExistsQ[$masterSpecByHead, Head[x]] && scalarQ[x],$masterSpecByHead[Head[x]]["FORMName"] <> "(" <> StringRiffle[emit /@ (List @@ x), ","] <> ")",
		(*Anything else is a hard failure carrying the offending expression unevaluated.*)
		True, throwFailure["UnsupportedExpression", "The expression contains an unsupported structure.", <|"Expression" -> HoldForm[x], "Head" -> Head[x]|>]
	];
    If[hasDiracColour, dcConfigureEmitter[emit, vector, index, register]];
    If[hasColourFormat, caConfigureEmitter[emit, register]];
	(*Dimension name and the requested loop vectors are registered BEFORE expression traversal, so their identifiers are fixed independently of anything emitted later. This is what makes identifier numbering stable across runs/plans.*)
	dimensionName = If[IntegerQ[dim], intString[dim], scalar[dim]];
	Scan[(register["Vector", #]) &, loops];
	(*Stage a top-level product with at least two immediate Plus factors. First serialize all factors in their original traversal order below; only then choose the multiplication order. Other factor types may occur in the same product.*)
	If[!hasColourFormat && !hasEpsilon && !hasMatrixWords && Head[expr] === Times && Count[List @@ expr, _Plus] >= 2,
		factorExpressions = List @@ expr;
		factorTexts = emit /@ factorExpressions;
		(*Find the positions of the top-level sums: each marks the end of a "stage" of the serialized product.*)
		stageEnds = Flatten[Position[factorExpressions, _Plus, {1}, Heads -> False]];
		(*The last stage always runs to the end of the factor list, covering any trailing non-sum factors.*)
		stageEnds[[-1]] = Length[factorTexts];
		stageRanges = MapThread[{#1 + 1, #2} &, {Prepend[Most[stageEnds], 0], stageEnds}];
		stageGroups = attachStageFactors[factorExpressions, stageRanges];
		(*Build each stage from its assigned factor positions. Emission above has
          already fixed the mapping and macro identifiers.*)
		stageTexts = ("(" <> StringRiffle[factorTexts[[#]], "*"] <> ")") & /@ stageGroups;
		stageExpressions = (Times @@ factorExpressions[[#]]) & /@ stageGroups;
		(*Index signatures drive the ordering; if there are no indices at all, use empty signatures (which makes the planner fall back to the original order).*)
		stageSignatures = If[FreeQ[stageExpressions, _LorentzIndex],
			ConstantArray[<||>, Length[stageExpressions]], 
			stageIndexSignatures[stageExpressions]];
		stageOrder = connectedStageOrder[stageExpressions, stageSignatures];
		stageTexts = stageTexts[[stageOrder]];
		(*Normalize large, unambiguous tensor stages once in FORM before reuse. The configurable leaf-count cutoff defaults to 1024. Hidden cfcStageN factors remain available until the program ends. Ambiguous signatures bypass preparation even when multiplication order is unchanged.*)
        If[ stageSignatures =!= $Failed && ! FreeQ[stageExpressions, _LorentzIndex] && Max[LeafCount /@ stageExpressions] >= $stagePreparationMinimumLeaves,preparations = stageTexts;
        symmetryLinks = ssLinks[stageSignatures[[stageOrder]], index];
        stageTexts = Table["cfcStage" <> intString[i], {i, Length[stageTexts]}]];
        (*The final program multiplies the stages left to right.*)
        body = First[stageTexts];
        multiplications = Rest[stageTexts],
        (*Not a product-of-sums: just emit the expression.*)
        body = emit[expr]];
        (* Record small routing matrices for FORM cancellation. Mass expressions
           use the same scalar serializer as the numerator. This registers any
           mass-only symbols without expanding the supplied expression. *)
        cancellationData = Map[Function[e, With[{pd = decode[e["Expression"]][[1]]},
            <|"Name" -> e["Name"], "Routing" -> vector[pd[[1]]],
              "MassSquared" -> emit[If[Length[pd] === 1, 0, pd[[2]]^2]]|>]],
            Select[entries, #["Kind"] === "Denominator" &]];
        (*Record the format, version, fingerprint of the FCI-normalized input, dimension, loop-momentum metadata, processing label and ordered entries. ExpressionDigest helps distinguish different exports but is not independently checked against a saved input. TensorAlgebraOnly means algebra and Lorentz contractions, without integral reduction.*)
        payload = <|"Format" -> $formatName, "Version" -> If[hasColourFormat, 5, If[hasDiracProcessing, 4, If[hasDiracColour, 3, If[hasEpsilon, 2, $formatVersion]]]], "ExpressionDigest" -> originalDigest,
        "Dimension" -> encode[dim], "LoopMomenta" -> (encode /@ loops), "Processing" -> If[$caActive, If[hasDiracProcessing && algebra === Automatic, "LorentzDiracAndColourAlgebra", "LorentzAndColourAlgebra"], If[hasDiracProcessing && algebra === Automatic, "LorentzAndDiracAlgebra", "TensorAlgebraOnly"]], "Entries" -> entries|>;
        If[AnyTrue[entries, #["Kind"] === "Denominator" &],
            AssociateTo[payload, "ResultLayout" -> "PropagatorGroups"]];
        If[hasEpsilon, AssociateTo[payload, "EpsilonConvention" -> <|
            "Sign" -> encode[epsilonSign], "ExportFactor" -> encode[-I epsilonSign]|>]];
        If[hasDiracColour, AssociateTo[payload, {"DiracSpinLine" -> 1, "EpsilonPresent" -> hasEpsilon}]];
        If[hasDiracProcessing || hasColourFormat, AssociateTo[payload, daMetadata[algebra, entries]]];
        If[hasColourFormat, AssociateTo[payload, caMetadata[entries]]];
        (*Single return value consumed by rendering and file writing: the dimension's FORM name, the body expression text, the remaining multiplication stages, the #define macros, the hoisted stage preparations, and the mapping payload.*)
        <|"DimensionName" -> dimensionName, "Body" -> body, "Multiplications" -> multiplications,"Factors" -> macros, "Preparations" -> preparations, "SymmetryLinks" -> symmetryLinks, "Mapping" -> payload, "CancellationData" -> cancellationData,
          "DenominatorSupports" -> If[Length[loops] === 1, tdDenominatorSupports[expr, denominator], Automatic]|>
   ]];


(* ::Subsection:: *)
(*Render the FORM program and mapping*)


(* ::Text:: *)
(*Generate program and mapping text from the supplied data, result path and template without file I/O. Rendering also uses the package specifications and platform path convention. The caller reads the template and writes the generated artifacts.*)

(* ::Input::Initialization:: *)
renderExport[data_Association, result_String, template_String] := 
  Module[
     (*Projections of the single big association produced by buildExportData. entries is the ordered registry table; declaration/json/digest/program are scalars built below.*)
    {entries = data["Mapping"]["Entries"], declaration, json, digest, 
    program, templateNames, unknownTemplateNames, rationalPlan, cancellationPlan, dimensionPlan, symmetryPlan, stagedDimensionQ, stageSort, algebraData},
     (*The template contract is enforced HERE, not only where the shipped template is read, because this function is the one that accepts an arbitrary template. Without this check a template missing a placeholder would silently render a program that omits a declaration or directive -- a FORM-level failure far from its cause. Required and unknown placeholders are rejected against the original template before any values are inserted.*)
     templateNames = templatePlaceholderNames[template];
     If[templateMissingPlaceholders[templateNames, $requiredTemplatePlaceholders] =!= {},
        throwFailure["MissingTemplate", 
     "The FORM program template is missing required placeholders.", <|
      "Placeholders" -> $requiredTemplatePlaceholders|>]];
     unknownTemplateNames = templateUnknownPlaceholders[templateNames,
       $requiredTemplatePlaceholders];
     If[unknownTemplateNames =!= {},
        throwFailure["MissingTemplate", 
     "The FORM program template contains unknown placeholders.", <|
      "Placeholders" -> unknownTemplateNames|>]];
     (*declaration[type]: produce the FORM declaration line for one declaration class, e.g. "Symbols cfs1,cfs2;" or "" if no entry of that class exists.*)
     declaration[type_] := Module[{names},
         (*Select mapping entries with the requested FORM declaration class, then collect their names in registry order. These entries have already been constructed by buildExportData.*)
         names = Lookup[
       Select[entries, $kindSpecs[#["Kind"]]["Declaration"] === type &], 
       "Name", {}];
         (*Empty string rather than an empty declaration line, so the template substitution doesn't leave stray "Symbols ;" text.*)
     If[names === {}, "", type <> " " <> StringRiffle[names, ","] <> ";"]
       ];
     rationalPlan = rcPlan[data];
     (* Tensor decomposition changes the saved result vocabulary, not the
        algebra before integration. Plan pre-decomposition optimisations from
        the recorded algebra capabilities, while keeping version-six output. *)
     algebraData = If[KeyExistsQ[data, "TensorProcessing"],
         Join[data, <|"Mapping" -> tdAlgebraMapping[data["Mapping"]]|>], data];
     dimensionPlan = dcfPlan[algebraData];
     cancellationPlan = pcPlan[algebraData];
     symmetryPlan = ssPlan[algebraData];
     stagedDimensionQ = dimensionPlan =!= <||> && data["Preparations"] =!= {};
     (* Each stage owns its sort boundaries. Supply the shared non-growing
        cancellation identities through an explicit hook; the dimension module
        also owns restoration of its temporary coefficient abbreviations. *)
     stageSort = dcfStageSort[If[stagedDimensionQ, dimensionPlan, <||>],
         If[data["Preparations"] === {}, "", pcDirectRules[data]]];
     (*Serialize the mapping and hash the exact resulting JSON text. Reformatting that text changes the digest. Compact output avoids extra whitespace but does not promise identical serialization across all kernel versions.*)
     json = 
    ExportString[data["Mapping"], "RawJSON", "Compact" -> True];
     digest = Hash[json, "SHA256", "HexString"];
     (*Fill the template. StringReplace with a list of -> rules is a single simultaneous pass, so placeholder text introduced by one replacement cannot be re-scanned and substituted by another -- important, since replacement values (expressions, paths) could in principle contain "@" sequences.*)
     program = StringReplace[template, {
          (*Use the version selected for this expression: epsilon jobs use version two.*)
          "@FORMATVERSION@" -> IntegerString[data["Mapping"]["Version"]],
          (*The "CFC<n>" artifact marker from the header.*)
      "@RESULTMARKER@" -> resultMarker[data["Mapping"]["Version"]],
          (*Declare the FORM functions for the masters (A0..D0). The names come from $masterSpecByFORMName, i.e. the "FORMName" fields of the spec table -- again, no name is hard-coded here.*)
          "@FUNCTIONS@" -> 
       "CFunctions " <> StringRiffle[Keys[$masterSpecByFORMName], ","] <> ";" <> daDeclarations[data] <> caDeclarations[data] <> Lookup[rationalPlan, "Declarations", ""] <> Lookup[cancellationPlan, "Declarations", ""] <> Lookup[dimensionPlan, "Declarations", ""] <> Lookup[symmetryPlan, "Declarations", ""] <> Lookup[data, "TensorDeclarations", ""],
          (*Declarations grouped by class, driven by $kindSpecs. Note "Symbols" covers Scalars, Abbreviations and Denominators -- they share a FORM class by design.*)
          "@SCALARS@" -> declaration["Symbols"], 
      "@DIMENSION@" -> data["DimensionName"],
          "@VECTORS@" -> declaration["Vectors"], 
      "@INDICES@" -> declaration["Indices"],
          (*The #define macros, one per line.*)
          "@FACTORS@" -> StringRiffle[data["Factors"], "\n"], 
      "@EXPRESSION@" -> data["Body"],
          (*Pre-prepared large tensor stages. When empty, the whole substitution is the empty string so no stray directives appear. Otherwise: one "Local cfcStageN = <stage>;" per preparation (MapIndexed supplies the 1-based counter), then normalisation (including eligible dimension compaction) and "Hide cfcStage1,cfcStage2,...;" so FORM can reuse the normalized stages later.*)
          "@PREPARATIONS@" -> If[data["Preparations"] === {}, "",
              
        StringJoin[
          MapIndexed[("Local cfcStage" <> intString[First[#2]] <> 
              " = " <> #1 <> ";\n") &,
                   data["Preparations"]]] <> stageSort <> "Hide " <>
               
         StringRiffle[
          Table["cfcStage" <> intString[i], {i, 
            Length[data["Preparations"]]}], ","] <> ";\n" <> Lookup[symmetryPlan, "Checks", ""]],
          (*Each multiplication follows a normalising stage boundary. Eligible prepared tensors compact their dimension dependence after ordinary tensor sorting, with rational arithmetic disabled for the next multiplication. The final boundary completes the last product before cancellation.*)
          "@MULTIPLICATIONS@" -> 
       StringJoin[MapIndexed[(stageSort <>
         If[symmetryPlan === <||>, "", symmetryPlan["Boundaries"][[First[#2]]]] <>
         "Multiply " <> #1 <> ";\n") &, data["Multiplications"]]] <> If[stagedDimensionQ, stageSort <> dcfStageRestore[dimensionPlan], ""] <> caProcessing[data] <> If[KeyExistsQ[data["Mapping"], "EpsilonConvention"], "contract 0;\n", ""] <> daProcessing[data] <> pcDirectProcessing[data] <> If[cancellationPlan === <||>, "", dcfPrecondition[dimensionPlan]] <> Lookup[cancellationPlan, "Processing", ""] <> Lookup[data, "TensorProcessing", ""] <>
         If[KeyExistsQ[data, "TensorProcessing"] && dimensionPlan =!= <||>,
             dcfStageSort[dimensionPlan] <> dcfStageRestore[dimensionPlan], ""] <> Lookup[data,"LoopReductionProcessing",""],
          "@GROUPING@" -> With[{denominators = Lookup[Select[entries, #["Kind"] === "Denominator" &], "Name", {}]},
              If[KeyExistsQ[data, "TensorProcessing"],
                  If[KeyExistsQ[data,"LoopReductionProcessing"], "#if `$cfcLRNonlinear' == 0\nBracket+ cfcTC;\n#else\n", ""] <>
                  "Bracket " <> StringRiffle[Prepend[denominators,"cfcTC"], ","] <> ";" <>
                  If[KeyExistsQ[data,"LoopReductionProcessing"],"\n#endif",""],
                  If[denominators === {}, "", If[pgFactorisationQ[data], "Bracket+ ", "Bracket "] <> StringRiffle[denominators, ","] <> ";"]]],
          (*The result path. Windows backslashes are normalized because FORM expects forward slashes there; Unix paths are preserved literally, including backslashes. Quoting is the template's job.*)
          "@RESULT@" -> If[$OperatingSystem === "Windows", 
             StringReplace[result, "\\" -> "/"], result], 
          "@OUTPUT@" -> pgOutput[data, If[$OperatingSystem === "Windows", StringReplace[result, "\\" -> "/"], result], rationalPlan, dimensionPlan],
          "@DIGEST@" -> digest}];
     (*Return both artifacts: the rendered FORM program and the exact JSON text whose hash was embedded in it. The caller can write them side by side and a verifier can re-hash MappingJSON to confirm it matches the @DIGEST@ in the program.*)
     <|"Program" -> program, "MappingJSON" -> json|>
   ];


(* ::Text:: *)
(*List the placeholders recognized by the renderer. renderExport requires this set in the original template and rejects unknown placeholder names before inserting replacement values.*)

(* ::Input::Initialization:: *)
$requiredTemplatePlaceholders = {
   "@FORMATVERSION@", "@RESULTMARKER@", "@FUNCTIONS@", "@SCALARS@",
   "@DIMENSION@", "@VECTORS@", "@INDICES@", "@FACTORS@",
   "@PREPARATIONS@", "@EXPRESSION@", "@MULTIPLICATIONS@", "@RESULT@",
   "@DIGEST@", "@GROUPING@", "@OUTPUT@"};


(* ::Text:: *)
(*templatePlaceholderNames[text]: every @NAME@ token appearing in the template, deduplicated. Used both to enforce the required set and, indirectly, to keep the renderer and the template in step.*)

(* ::Input::Initialization:: *)
templatePlaceholderNames[text_String] := 
  DeleteDuplicates[StringCases[text, RegularExpression["@[A-Za-z][A-Za-z0-9]*@"]]];

(* Shared required/unresolved placeholder audit. Stage-specific substitutions
   and their failure messages remain local to the renderer or algebra module. *)
templateMissingPlaceholders[tokenNames_List, required_List] :=
  Select[required, !MemberQ[tokenNames, #] &];
templateUnknownPlaceholders[tokenNames_List, allowed_List] :=
  Complement[tokenNames, allowed];

readProgramTemplate[] := Module[{template},
     template = 
    (*Suppress file-read messages and convert message-producing imports to $Failed. The following StringQ check turns unreadable template input into MissingTemplate.*)
    Quiet[Check[
      Import[FileNameJoin[{$moduleDirectory, "Templates", 
         "Program.frm.in"}], "Text"], $Failed]];
     (*Validate rather than trusting the I/O: anything that is not a string (including $Failed from a missing file, or a binary import result) becomes a structured "MissingTemplate" failure, which throws to the caller's Catch on $failureTag.*)
     If[! StringQ[template], 
    throwFailure["MissingTemplate", "Cannot read the FORM program template."]];
     (*The placeholder contract itself is enforced by renderExport, the function that actually substitutes and therefore owns the contract; keeping the check in one place avoids the two drifting.*)
     template
   ];


(* ::Subsection:: *)
(*Export paths and file writing*)


(* ::Text:: *)
(*Path resolution and validation. This is the "one place that turns a user-supplied filename into the three artifacts" function: the FORM program (.frm), its mapping sidecar (.map.json) and the FORM output (.out). It performs no I/O beyond queries -- no file is created or modified here.*)

(* ::Input::Initialization:: *)
exportPaths[file_String, overwrite_] := 
  Module[{input, mapping, result, paths},
     (*Validate the option before anything else, so a bad OverwriteTarget is reported as an option error rather than as a confusing file-existence failure later.*)
     If[! BooleanQ[overwrite], 
    throwFailure["InvalidOption", "OverwriteTarget must be True or False."]];
     (*Resolve the supplied filename to an absolute path. This does not provide filesystem locking or a canonical identity across symbolic links.*)
     input = ExpandFileName[file];
     (*Only .frm inputs are accepted; ToLowerCase makes the check case-insensitive so "MODEL.FRM" is fine.*)
     If[ToLowerCase[FileExtension[input]] =!= "frm", 
    throwFailure["InvalidPath", "The FORM input filename must end in .frm."]];
     (*The two companions are derived from the input by swapping the extension, so model.frm -> model.map.json and model.out, all in the same directory. FileBaseName drops only the final extension, hence the explicit ".map.json".*)
     mapping = 
    FileNameJoin[{DirectoryName[input], 
      FileBaseName[input] <> ".map.json"}];
     result = 
    FileNameJoin[{DirectoryName[input], FileBaseName[input] <> ".out"}];
     paths = {input, mapping, result};
     (*Fail early if the destination directory is missing, rather than discovering it as a write failure three steps later.*)
     If[! DirectoryQ[DirectoryName[input]], 
    throwFailure["InvalidPath", "The destination directory does not exist."]];
     (*The template embeds paths inside angle brackets. Reject delimiters, preprocessor quote characters and line breaks that could corrupt the generated program. Check all derived paths, not just the user-supplied filename.*)
     If[AnyTrue[paths, 
     StringContainsQ[#, {"\n", "\r", "<", ">", "`", "'", "\""}] &],
        throwFailure["InvalidPath", 
     "FORM output paths cannot contain quotes, angle brackets, \
backticks or line breaks."]];
     (*Refuse existing targets unless overwrite is enabled. This is an early check, not a concurrency guarantee. Rename operations also protect the program and mapping when overwrite is disabled; FORM creates the result later.*)
     If[! overwrite && AnyTrue[paths, FileExistsQ], 
    throwFailure["FileExists", 
     "An export target already exists. Use OverwriteTarget -> True to \
replace it."]];
     (*Return the resolved triple as an association, which is what the writer consumes.*)
     <|"InputFile" -> input, "MappingFile" -> mapping, 
    "ResultFile" -> result|>
   ];


(* ::Text:: *)
(*These wrappers isolate filesystem operations for error handling and failure-injection tests. Write/copy/rename return paths or $Failed; deletion returns a Boolean.*)

(* ::Input::Initialization:: *)
exportWriteText[path_, text_] := 
  Quiet[Check[
    Export[path, text, "Text", CharacterEncoding -> "UTF-8"], $Failed]];


(* ::Text:: *)
(*Copy, with overwrite controlled by the caller. Returns the target path on success, $Failed otherwise. Used for making backups.*)

(* ::Input::Initialization:: *)
exportCopyFile[source_, target_, overwrite_ : False] := 
  Quiet[Check[
    CopyFile[source, target, System`OverwriteTarget -> overwrite], $Failed]];


(* ::Text:: *)
(*Rename/move, the operation that actually installs a staged file into place. Returns the target path on success, $Failed otherwise.*)

(* ::Input::Initialization:: *)
exportRenameFile[source_, target_, overwrite_] := 
  Quiet[Check[
    RenameFile[source, target, System`OverwriteTarget -> overwrite], $Failed]];


(* ::Text:: *)
(*Return True if the path is already absent or DeleteFile completes without a message; return False on a reported deletion error. This helper does not independently recheck absence after deletion.*)

(* ::Input::Initialization:: *)
exportDeleteFile[path_] := ! FileExistsQ[path] || 
   TrueQ[Quiet[Check[DeleteFile[path]; True, False]]];


(* ::Text:: *)
(*The transactional writer. Contract stated in the header: both files are prepared BEFORE either existing target is replaced; the backups and REPLACED flags describe only this invocation's changes; this is explicitly NOT a lock or a crash-recovery protocol for concurrent writers.*)

(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormExport::cleanup = "Export cleanup could not remove these files: `1`.";

writeExport[paths_Association, rendered_Association, overwrite_] := 
  Module[
     (*The transactional pair is only the two files this invocation owns: the program and its mapping. ResultFile (.out) is written later by FORM itself, so it is deliberately not a transaction member.*)
     {targets = Lookup[paths, {"InputFile", "MappingFile"}],
       contents = Lookup[rendered, {"Program", "MappingJSON"}],
       staged, backups, existed, replaced = {False, False}, 
    committed = False,
       cleanup, outcome, result, aborted = False, 
    rollbackFailed = False,
       recovery = {}, cleanupFailed = {}, deleteTemporary, token},
     (*A per-invocation UUID namespaces every temporary and backup file, so concurrent or repeated runs cannot collide on the sidecar names.*)
     token = CreateUUID[];
     (*Staging files sit next to their targets (same directory), which keeps the final rename cheap and on the same filesystem.*)
     staged = (# <> "." <> token <> ".tmp") & /@ targets;
     backups = (# <> "." <> token <> ".bak") & /@ targets;
     existed = FileExistsQ /@ targets;
   
     (*Define cleanup without running it; WithCleanup invokes it on exit. Only targets replaced by this invocation are restored or removed. This is the crux of the safety property: a target that was never renamed into place is left exactly as it was, and its pre-emptive backup is simply deleted.*)
     deleteTemporary[path_] := If[FileExistsQ[path] && !TrueQ[exportDeleteFile[path]],
       AppendTo[cleanupFailed, path]];
     cleanup[] := Module[{restored = True},
         If[! committed,
            Do[
               If[replaced[[i]],
                  If[existed[[i]],
                     (*The target existed before: put the backup back. A non-string result means $Failed, i.e. restore did not happen.*)
         If[! StringQ[exportCopyFile[backups[[i]], targets[[i]], True]],
                        restored = False
                      ],
                     (*The target did not exist before: remove what we installed, so the filesystem returns to its original state.*)
         If[! exportDeleteFile[targets[[i]]], restored = False]
                   ]
                ],
               {i, Length[targets]}
             ]
          ];
         (*Attempt staging cleanup on every exit; record failed deletions.*)
         Scan[deleteTemporary, staged];
         (*Delete backups after a successful commit or completed restoration. If restoration fails, retain recovery copies and report their paths.*)
         If[restored,
            Scan[deleteTemporary, backups],
            rollbackFailed = True;
            recovery = Select[backups, FileExistsQ]
          ]
       ];
   
     (*Cleanup runs on success, a tagged failure, or abort. Record an abort only after cleanup, so rollback failure can report retained recovery files.*)
     result = CheckAbort[
         Catch[
            (*WithCleanup guarantees cleanup[] runs when the body exits by any route: normal return, Throw (our fail), or abort.*)
            WithCleanup[
               (*Phase 1: write both artifacts to their staging paths. The targets have not changed, although staging files exist. A failure here needs no restoration; cleanup attempts to remove staging files.*)
               Do[
                  
        If[! StringQ[exportWriteText[staged[[i]], contents[[i]]]],
                     
         throwFailure["WriteFailed", 
          "Cannot stage the FORM program and mapping.",
                        <|"Path" -> staged[[i]]|>]
                   ],
                  {i, Length[targets]}
                ];
               (*Phase 2: preserve existing targets as backups. Again, nothing has been replaced yet; this phase only creates safety copies, and it is where overwrite refusal is enforced as a second line of defence after exportPaths.*)
               Do[
                  If[existed[[i]],
                     
         If[! overwrite, 
          throwFailure["FileExists", "An export target already exists."]];
                     
         If[! StringQ[exportCopyFile[targets[[i]], backups[[i]]]],
                        
          throwFailure["WriteFailed", 
           "Cannot preserve the existing export before replacement.",
                           <|"Path" -> targets[[i]]|>]
                      ]
                   ],
                  {i, Length[targets]}
                ];
               (*Phase 3: install each staged file over its target.*)
               Do[
                  (*Replacement and ownership registration form one abort- protected step. AbortProtect guarantees that the rename and the setting of replaced[[i]] cannot be separated by an abort: if the user aborts at just the wrong moment, cleanup still knows this target was replaced and will restore it. Registration is what makes the rollback correct, not the rename itself.*)
                  AbortProtect[
                     
         outcome = 
          exportRenameFile[staged[[i]], targets[[i]], overwrite];
                     If[StringQ[outcome], replaced[[i]] = True]
                   ];
                  (*A failed rename is reported with an explicit note that rollback has already run (cleanup fires on the Throw), so the message matches the on-disk state.*)
                  If[! StringQ[outcome],
                     
         throwFailure["WriteFailed", 
          "Cannot replace the FORM program and mapping; original \
files were restored.",
                        <|"Path" -> targets[[i]]|>]
                   ],
                  {i, Length[targets]}
                ];
               (*Commit point: set BEFORE WithCleanup runs cleanup, so cleanup skips restoration and only deletes staging and backups.*)
               committed = True;
               (*Body value on success: the paths association. Note it is not the rendered text -- the caller already has that in `rendered`.*)
               paths,
               cleanup[]
             ],
            $failureTag
          ],
         (*Abort handling: mark that an abort happened, then let the abort continue to propagate after the post-processing below.*)
         aborted = True;
         $Aborted
       ];
     (*Rollback failure is reported FIRST, because it is the more important condition: the original files are not all back, and the recovery copies are listed so the user can fix it by hand.*)
     If[rollbackFailed,
        throwFailure["RollbackFailed", 
     "Export failed and the original files could not all be restored. \
Recovery copies were retained.",
           <|"RecoveryFiles" -> recovery, "Targets" -> targets,
             "RetainedFiles" -> DeleteDuplicates[Join[recovery, cleanupFailed]]|>]
      ];
     (*Report cleanup failures without changing a committed export into a failed export. Retain the original failure tag when export itself failed. After reporting cleanup, propagate any pending abort; rollback failure takes precedence above.*)
     If[cleanupFailed =!= {},
       Message[CalcFormConverter`CalcFormExport::cleanup, cleanupFailed];
       If[AssociationQ[result],
         result = Join[result, <|"CleanupFailure" -> makeFailure["CleanupFailed",
           "Export succeeded, but temporary files could not all be removed.",
           <|"RetainedFiles" -> cleanupFailed|>]|>],
         If[FailureQ[result], result = Failure[result[[1]],
           Join[result[[2]], <|"RetainedFiles" -> cleanupFailed|>]]]]];
     If[aborted, Abort[]];
     result
   ];


(* ::Subsection:: *)
(*Public export orchestration*)

(* ::Text:: *)
(* Report only completed exports. Automated calculations suppress this manual
   file hand-off because their temporary files may be deleted after import.
   Presentation never changes the returned association or shortens its paths. *)

(* ::Input::Initialization:: *)
$showExportSummary = True;
printExportSummary[paths_Association, frontEnd_: $FrontEnd] := Module[{rows},
    rows = Transpose[{{"FORM program", "Symbol mapping", "Expected result"},
        Lookup[paths, {"InputFile", "MappingFile", "ResultFile"}]}];
    If[frontEnd === Null,
        Print["FORM export completed"];
        Scan[Print[#[[1]], ": ", #[[2]]] &, rows],
        Print[StandardForm[Column[{
            Style["FORM export completed", Bold],
            Grid[Prepend[rows, {"File", "Location"}], Alignment -> Left,
                Frame -> All, Spacings -> {2, 1}]
        }, Spacings -> 1]]]
    ]
];



(* ::Text:: *)
(*The public exporter sequences path validation, in-memory conversion, template loading, rendering and transactional writing. Its tagged Catch converts conversion failures into returned Failure objects. Path and file operations are not pure functions.*)

(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormExport[expression_, file_String, 
   OptionsPattern[]] := Catch[
     (*Keep intermediate results local and read OverwriteTarget once. The one-argument OptionValue form resolves against this definition's OptionsPattern.*)
     Module[{paths, data, rendered, result,
     overwrite = OptionValue[System`OverwriteTarget]},
        (*Step 1: resolve and validate paths. Takes only the filename and the option value; does no writing. Fails fast on bad extension, missing directory, forbidden characters, or an existing target when overwriting is off.*)
        paths = exportPaths[file, overwrite];
        (*Step 2: build the export data. Note the ORDER: the whole expression is converted/validated BEFORE any file is touched, so an unsupported expression aborts with the filesystem still untouched. Also note that only here are Dimension and LoopMomenta read -- after the cheap path validation.*)
        data = 
     buildExportData[expression, OptionValue[FeynCalc`Dimension], 
      OptionValue[FeynCalc`LoopMomenta], OptionValue[CalcFormConverter`DiracAlgebra], OptionValue[CalcFormConverter`ColourAlgebra]];
        (*Step 3: render the FORM program. readProgramTemplate[] does the one piece of file I/O this stage needs (reading the template); the result path comes from paths, so the .out filename is fixed before the program text is generated.*)
        rendered = 
     renderExport[smPrepare[tdPrepare[data, OptionValue[CalcFormConverter`TensorDecomposition], OptionValue[FeynCalc`DropScaleless]], OptionValue[CalcFormConverter`ReduceToScalarMasters], OptionValue[CalcFormConverter`ReduceLoopCoefficients], OptionValue[FeynCalc`DropScaleless], OptionValue[FeynCalc`Dimension]], paths["ResultFile"], readProgramTemplate[]];
        (*Step 4: publish both files before announcing success. Preserve any
          returned failure, including transaction cleanup/recovery diagnostics. *)
        result = writeExport[paths, rendered, overwrite];
        If[AssociationQ[result] && TrueQ[$showExportSummary], printExportSummary[result]];
        result
      ], $failureTag];


(* ::Text:: *)
(*Fallback definition for any call that does not match -- wrong argument count, a non-string filename, or non-rule trailing arguments. This one does NOT throw and does NOT use Catch: it returns a Failure object directly. That is a deliberate convention (always hand back something FailureQ), but it means a bad-argument error and a validated runtime failure arrive by different mechanisms -- a bare call cannot be wrapped in a Catch that will observe both. Callers should test FailureQ on the result rather than rely on catching.*)

(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormExport[___] :=
  makeFailure["InvalidArguments",
    "Use CalcFormExport[expression, filename, options]."];


