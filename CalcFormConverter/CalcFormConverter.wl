(* ::Package:: *)

(* ::Title:: *)
(*CalcFormConverter*)

(* ::Text:: *)
(* Bosonic FeynCalc <-> FORM conversion. Runtime definitions load separately.
   Scalar abbreviations and propagators remain opaque to FORM in version 1.
   The mapping is JSON data, not executable Wolfram Language source. *)

(* ::Section:: *)
(*Public interface*)

(* ::Input::Initialization:: *)
BeginPackage["CalcFormConverter`", {"FeynCalc`"}];

(* 1. Public functions and options. *)
CalcFormConverter`CalcFormExport::usage =
  "CalcFormExport[expr, file, opts] exports an exact bosonic FeynCalc expression to a complete FORM program and a reversible JSON mapping. Returns an association with InputFile, MappingFile and ResultFile. Options: Dimension -> Automatic, LoopMomenta -> {}, OverwriteTarget -> False. No FORM process or integral reduction is performed.";
CalcFormConverter`CalcFormImport::usage =
  "CalcFormImport[resultFile, mappingFile] reads the dedicated result of an exported FORM program and reconstructs FeynCalc internal notation. It does not execute Wolfram Language source or run tensor/integral reduction.";

CalcFormConverter`CalcFormCheck::usage = "CalcFormCheck[opts] probes FORM on demand and returns availability, version and diagnostics. Options: FORMExecutable -> Automatic, FORMThreads -> 1, TimeConstraint -> 10.";
CalcFormConverter`CalcFormInstall::usage = "CalcFormInstall[opts] explicitly attempts Debian/Ubuntu installation of missing FORM or TFORM using system authorization, then verifies the requested configuration. FORMThreads -> 1 is the default; values above 1 require TFORM. It is never called automatically.";
CalcFormConverter`CalcFormCalculate::usage = "CalcFormCalculate[expr, opts] exports, executes FORM and imports its result. For a binary equality lhs == rhs, it calculates lhs - rhs and compares the imported residual with zero; the result may remain symbolic. True and False are returned directly. Options include Dimension, LoopMomenta, FORMExecutable, TimeConstraint, WorkingDirectory, KeepFiles, ShowTiming, ShowProgress and FORMThreads. Failed jobs are retained.";
CalcFormConverter`FORMExecutable::usage = "FORMExecutable selects the FORM executable by name or path; Automatic searches the Wolfram kernel's PATH for form, or tform when FORMThreads > 1.";
CalcFormConverter`WorkingDirectory::usage = "WorkingDirectory specifies an existing parent directory for unique FORM jobs; Automatic uses the system temporary directory.";
CalcFormConverter`ShowTiming::usage = "ShowTiming -> True prints FORM process elapsed wall-clock seconds, excluding the availability probe, export and import. Defaults to False; the returned expression is unchanged.";
CalcFormConverter`ShowProgress::usage = "ShowProgress -> True prints calculation stages and elapsed FORM execution time every ten seconds. It does not estimate a completion percentage. Defaults to False.";
CalcFormConverter`FORMThreads::usage = "FORMThreads specifies a positive integer worker count for CalcFormCheck, CalcFormInstall and CalcFormCalculate. The default 1 uses ordinary FORM; values above 1 select TFORM with -wN when FORMExecutable is Automatic. Explicit executables must pass a TFORM probe for multiple workers.";
CalcFormConverter`KeepFiles::usage = "KeepFiles -> True retains successful FORM calculation files and reports their location. Failed jobs are always retained.";

(* ::Subsection:: *)
(*Private initialization and export options*)

(* ::Input::Initialization:: *)
Begin["`Private`"];

$moduleDirectory = DirectoryName[$InputFileName];
$formatName = "CalcFormConverter";
$formatVersion = 1;
resultMarker[] := "CFC" <> IntegerString[$formatVersion];
intString[n_Integer] := If[n < 0, "-", ""] <> IntegerString[Abs[n]];
$failureTag = Unique["CalcFormFailure"];
fail[tag_, message_, data_: <||>] := Throw[
  Failure[tag, Join[<|"MessageTemplate" -> message|>, data]], $failureTag];

Options[CalcFormConverter`CalcFormExport] = {
  Dimension -> Automatic, LoopMomenta -> {}, OverwriteTarget -> False
};

(* ::Section:: *)
(*Expression inspection and reversible mapping data*)

(* ::Subsection:: *)
(*Supported heads and encoding*)

(* ::Input::Initialization:: *)
(* 2. Expression inspection, normalization and restricted data. Used for
   reversible abbreviations and the non-executable mapping file. *)
(* One specification drives mapping arities, master validation, FORM
   declarations, serialization and reconstruction. Arity is {minimum, maximum}. *)
$expressionSpecs = <|
  "Plus" -> <|"Head" -> Plus, "Arity" -> {0, Infinity}|>,
  "Times" -> <|"Head" -> Times, "Arity" -> {0, Infinity}|>,
  "Power" -> <|"Head" -> Power, "Arity" -> {2, 2}|>,
  "Pair" -> <|"Head" -> Pair, "Arity" -> {2, 2}|>,
  "Momentum" -> <|"Head" -> Momentum, "Arity" -> {1, 2}|>,
  "LorentzIndex" -> <|"Head" -> LorentzIndex, "Arity" -> {1, 2}|>,
  "FeynAmpDenominator" -> <|"Head" -> FeynAmpDenominator, "Arity" -> {1, Infinity}|>,
  "PropagatorDenominator" -> <|"Head" -> PropagatorDenominator, "Arity" -> {1, 2}|>,
  "A0" -> <|"Head" -> A0, "FORMName" -> "cfA0", "Arity" -> {1, 1}, "Arguments" -> "Scalar"|>,
  "B0" -> <|"Head" -> B0, "FORMName" -> "cfB0", "Arity" -> {3, 3}, "Arguments" -> "Scalar"|>,
  "C0" -> <|"Head" -> C0, "FORMName" -> "cfC0", "Arity" -> {6, 6}, "Arguments" -> "Scalar"|>,
  "D0" -> <|"Head" -> D0, "FORMName" -> "cfD0", "Arity" -> {10, 10}, "Arguments" -> "Scalar"|>
|>;
$heads = Map[#["Head"] &, $expressionSpecs];
$headNames = Association[Map[Reverse, Normal[$heads]]];
$masterSpecs = Select[$expressionSpecs, KeyExistsQ[#, "FORMName"] &];
$mastersByHead = Association[(#["Head"] -> #) & /@ Values[$masterSpecs]];
$mastersByFORM = Association[(#["FORMName"] -> #) & /@ Values[$masterSpecs]];
arityQ[spec_Association, n_Integer] := spec["Arity"][[1]] <= n <= spec["Arity"][[2]];
masterArgumentsQ[spec_Association, args_List] := arityQ[spec, Length[args]] &&
  Switch[spec["Arguments"], "Scalar", AllTrue[args, scalarQ], _, False];

(* Kind names, prefixes, declaration classes and value checks have one owner. *)
$kindSpecs = <|
  "Scalar" -> <|"Prefix" -> "cfs", "Declaration" -> "Symbols", "ValidExpression" -> (Head[#] === Symbol &)|>,
  "Vector" -> <|"Prefix" -> "cfv", "Declaration" -> "Vectors", "ValidExpression" -> (vectorIdentityQ[#] &)|>,
  "Index" -> <|"Prefix" -> "cfi", "Declaration" -> "Indices", "ValidExpression" -> (Head[#] === Symbol &)|>,
  "Abbreviation" -> <|"Prefix" -> "cfa", "Declaration" -> "Symbols", "ValidExpression" -> (scalarQ[#] &)|>,
  "Denominator" -> <|"Prefix" -> "cfd", "Declaration" -> "Symbols", "ValidExpression" -> (MatchQ[#, FeynAmpDenominator[_PropagatorDenominator]] &)|>
|>;
validEntryNameQ[entry_Association] := Module[{kind = Lookup[entry, "Kind", None], name = Lookup[entry, "Name", None]},
  KeyExistsQ[$kindSpecs, kind] && StringQ[name] &&
    StringMatchQ[name, RegularExpression[$kindSpecs[kind]["Prefix"] <> "[0-9]+"]]
];

symbolName[s_Symbol] := Context[s] <> SymbolName[s];
encode[n_Integer] := {"Integer", intString[n]};
encode[n_Rational] := {"Rational", intString[Numerator[n]], intString[Denominator[n]]};
encode[z_Complex] := {"Complex", encode[Re[z]], encode[Im[z]]};
(* Polarization labels identify independent vectors, including conjugation.
   Encode their sole supported option as data, never as a general Rule head. *)
(* A momentum label may be routed, but the polarization of that routing stays
   one vector identity: polarization is not a linear function of momentum. *)
physicalMomentumLabelQ[p_] := AllTrue[If[Head[p] === Plus, List @@ p, {p}],
  MatchQ[#, _Symbol | Times[(_Integer | _Rational), _Symbol]] &];
polarizationQ[Polarization[p_, phase_, opts___]] :=
  physicalMomentumLabelQ[p] && MemberQ[{I, -I}, phase] && MemberQ[{{}, {Transversality -> True}, {Transversality -> False}}, {opts}];
polarizationQ[_] := False;
vectorIdentityQ[x_] := Head[x] === Symbol || polarizationQ[x];
encode[x_Polarization] := If[polarizationQ[x],
  Join[{"Polarization", encode[x[[1]]], encode[x[[2]]]},
    If[Length[x] === 3, {{"Transversality", If[TrueQ[x[[3, 2]]], "True", "False"]}}, {}]],
  fail["UnsupportedMomentum", "Unsupported polarization vector identity."]];
encode[s_Symbol] := {"Symbol", symbolName[s]};
encode[x_] := Module[{name = Lookup[$headNames, Head[x], Missing["Unknown"]]},
  If[MissingQ[name], fail["UnsupportedHead", "Unsupported expression head.", <|"Expression" -> HoldForm[x], "Head" -> Head[x]|>]];
  Prepend[encode /@ (List @@ x), name]
];

(* ::Subsection:: *)
(*Restricted decoding and symbol validation*)

(* ::Input::Initialization:: *)
integerData[s_String] := If[StringMatchQ[s, RegularExpression["-?[0-9]+"]],
  If[StringStartsQ[s, "-"], -FromDigits[StringDrop[s, 1]], FromDigits[s]],
  fail["InvalidMapping", "Invalid integer in mapping."]];
integerData[_] := fail["InvalidMapping", "Expected an integer string."];

(* Only symbol leaves may create a symbol. Heads are selected from $heads;
   a string from a file is never evaluated as Wolfram Language code. Restored
   symbols and whitelisted heads still undergo normal kernel evaluation;
   this decoder does not isolate pre-existing symbol definitions. *)
validSymbolNameQ[s_String] := StringContainsQ[s, "`"] &&
  StringMatchQ[s, RegularExpression["[\\p{L}$][\\p{L}\\p{N}$]*(?:`[\\p{L}$][\\p{L}\\p{N}$]*)+"]];
validSymbolNameQ[_] := False;
decode[{"Integer", s_String}] := integerData[s];
decode[{"Rational", a_String, b_String}] := Module[{den = integerData[b]},
  If[den == 0, fail["InvalidMapping", "Zero rational denominator."]];
  integerData[a]/den
];
decode[{"Complex", a_, b_}] := Module[{re = decode[a], im = decode[b]},
  If[!MatchQ[{re, im}, {(_Integer | _Rational), (_Integer | _Rational)}],
    fail["InvalidMapping", "Complex coefficients must be exact numbers."]];
  re + I im
];
decode[{"Symbol", s_String}] := If[validSymbolNameQ[s], Symbol[s],
  fail["InvalidMapping", "Invalid fully qualified symbol name."]];
decode[{"Polarization", p_, phase_, opts___}] := Module[{momentum = decode[p], label = decode[phase], optionData = {opts}},
  If[!physicalMomentumLabelQ[momentum] || !MemberQ[{I, -I}, label] ||
     !MemberQ[{{}, {{"Transversality", "True"}}, {{"Transversality", "False"}}}, optionData],
    fail["InvalidMapping", "Invalid polarization vector identity."]];
  If[optionData === {}, Polarization[momentum, label],
    Polarization[momentum, label, Transversality -> (optionData[[1, 2]] === "True")]]
];
decode[{name_String, args___}] /; KeyExistsQ[$heads, name] := Module[{a = {args}, h = $heads[name]},
  If[!arityQ[$expressionSpecs[name], Length[a]],
    fail["InvalidMapping", "Invalid expression arity in mapping."]];
  h @@ (decode /@ a)
];
decode[_] := fail["InvalidMapping", "Unsupported mapping data."];

(* ::Subsection:: *)
(*Lorentz dimensions*)

(* ::Input::Initialization:: *)
(* Infer dimensions without expanding the expression or contracting indices. *)
space[Momentum[_, d_: 4]] := d;
space[LorentzIndex[_, d_: 4]] := d;
chooseDimension[x_, requested_] := Module[{spaces, dim},
  spaces = DeleteDuplicates[Cases[x, a : (_Momentum | _LorentzIndex) :> space[a], {0, Infinity}]];
  If[Length[spaces] > 1, fail["MixedDimensions", "Mixed Lorentz spaces are not supported.", <|"Dimensions" -> spaces|>]];
  dim = If[requested === Automatic, If[spaces === {}, D, First[spaces]], requested];
  If[!MatchQ[dim, _Symbol | _Integer] || (IntegerQ[dim] && dim < 2) || dim === I,
    fail["UnsupportedDimension", "Use a symbolic dimension or an integer dimension of at least 2."]];
  If[spaces =!= {} && First[spaces] =!= dim,
    fail["DimensionMismatch", "The requested dimension differs from the input; no implicit dimension conversion is performed."]];
  dim
];

(* ::Subsection:: *)
(*Supported momenta and scalar expressions*)

(* ::Input::Initialization:: *)
linearMomentumQ[Momentum[v_, ___]] := AllTrue[If[Head[v] === Plus, List @@ v, {v}],
  (vectorIdentityQ[#] || MatchQ[#, Times[(_Integer | _Rational), _?vectorIdentityQ]]) &];
linearMomentumQ[_] := False;

scalarQ[x_] := Which[
  MatchQ[x, _Integer | _Rational], True,
  Head[x] === Complex, MatchQ[{Re[x], Im[x]}, {(_Integer | _Rational), (_Integer | _Rational)}],
  Head[x] === Symbol, !MemberQ[{Indeterminate, Infinity, ComplexInfinity}, x],
  MatchQ[x, Pair[_Momentum, _Momentum]], AllTrue[List @@ x, linearMomentumQ],
  MemberQ[{Plus, Times}, Head[x]], AllTrue[List @@ x, scalarQ],
  Head[x] === Power, scalarQ[x[[1]]] && MatchQ[x[[2]], _Integer | _Rational],
  KeyExistsQ[$mastersByHead, Head[x]], masterArgumentsQ[$mastersByHead[Head[x]], List @@ x],
  True, False
];

(* ::Section:: *)
(*Export: symbol mapping, serialization and FORM program generation*)

(* ::Subsection:: *)
(*Build conversion data in memory*)

(* ::Input::Initialization:: *)
(* Plan only after serialization, so identifier and macro numbering keep the
   original traversal order. Consistent monomial index counts are required:
   ambiguous sums or indices occurring more than twice retain the old order. *)
stageIndexSignatures[stages_List] := Module[{counts, signatures},
  counts[p_Pair] := counts[p] = KeySort[Counts[Cases[p, _LorentzIndex, Infinity]]];
  counts[x_Plus] := Module[{parts = counts /@ (List @@ x)},
    If[MemberQ[parts, $Failed] || !SameQ @@ parts, $Failed, First[parts]]];
  counts[x_Times] := Module[{parts = counts /@ (List @@ x)},
    If[MemberQ[parts, $Failed], $Failed, KeySort[Merge[parts, Total]]]];
  counts[Power[x_, n_Integer]] /; n >= 0 := Module[{part = counts[x]},
    If[part === $Failed, $Failed, Map[n # &, part]]];
  counts[_] := <||>;
  signatures = counts /@ stages;
  If[MemberQ[signatures, $Failed] ||
     AnyTrue[Values[Merge[signatures, Total]], # > 2 &], $Failed, signatures]
];

connectedStageOrder[stages_List] := connectedStageOrder[stages,
  If[FreeQ[stages, _LorentzIndex], ConstantArray[<||>, Length[stages]], stageIndexSignatures[stages]]];
connectedStageOrder[stages_List, signatures_] := Module[
  {free, sizes, remaining, order = {}, active = {}, next},
  If[signatures === $Failed, Return[Range[Length[stages]]]];
  free = Keys[Select[#, # === 1 &]] & /@ signatures;
  If[AllTrue[free, # === {} &], Return[Range[Length[stages]]]];
  sizes = LeafCount /@ stages;
  remaining = Range[Length[stages]];
  While[remaining =!= {},
    (* Prefer shared open indices, then tensors, smaller factors and the
       original position. This is a deterministic heuristic, not a cost model. *)
    next = First[SortBy[remaining,
      {-Length[Intersection[active, free[[#]]]],
       If[free[[#]] === {}, 1, 0], sizes[[#]], #} &]];
    AppendTo[order, next];
    (* A shared open index contracts; an unshared one remains open. *)
    active = Complement[Union[active, free[[next]]], Intersection[active, free[[next]]]];
    remaining = DeleteCases[remaining, next]];
  order
];

(* Local mutable state belongs only to expression traversal. Rendering and
   file writing consume the resulting association and cannot change it. *)
buildExportData[expression_, requestedDimension_, loops_] := Module[
  {expr, dim, entries = {}, registry = <||>, counters = <||>, macros = {},
   register, scalar, vector, index, pair, denominator, emit, abbreviation, makeMacro,
   body, dimensionName, payload, factorExpressions, factorTexts, stageEnds, stageRanges,
   stageTexts, stageExpressions, stageSignatures, stageOrder,
   preparations = {}, multiplications = {}},
  If[!ListQ[loops] || !AllTrue[loops, MatchQ[#, _Symbol] &] || !DuplicateFreeQ[loops],
    fail["InvalidLoopMomenta", "LoopMomenta must be a list of distinct momentum symbols."]];
  expr = FCI[expression];
  dim = chooseDimension[expr, requestedDimension];

  register[kind_, value_, extra_: <||>] := With[{key = HoldComplete[kind, value]},
    (* HoldComplete preserves the structural key without evaluating its
       contents again; kind distinguishes the same value in different roles.
       Only insertion allocates locals and serializes the mapped expression. *)
    If[KeyExistsQ[registry, key], registry[key],
      Module[{name, number, prefix},
        number = Lookup[counters, kind, 0] + 1;
        AssociateTo[counters, kind -> number];
        prefix = $kindSpecs[kind]["Prefix"];
        name = prefix <> intString[number];
        AssociateTo[registry, key -> name];
        AppendTo[entries, Join[<|"Name" -> name, "Kind" -> kind, "Expression" -> encode[value]|>, extra]];
        name
      ]
    ]
  ];
  makeMacro[s_] := Module[{name = "CFCF" <> intString[Length[macros] + 1]},
    AppendTo[macros, "#define " <> name <> " \"(" <> s <> ")\""];
    "`" <> name <> "'"
  ];
  scalar[s_Symbol] := If[s === I, "i_", register["Scalar", s]];
  index[LorentzIndex[i_Symbol, ___]] := register["Index", i];
  index[_] := fail["UnsupportedIndex", "Lorentz indices must be symbols."];
  (* Return coefficient/name pairs for a linear combination. Polarization
     labels stay atomic vector identities even when their momentum is routed. *)
  vector[Momentum[v_, ___]] := Module[{terms},
    terms = If[Head[v] === Plus, List @@ v, {v}];
    Map[
      Function[term,
        Module[{factors, momenta, coefficients},
          If[vectorIdentityQ[term],
            {1, register["Vector", term]},
            If[Head[term] =!= Times,
              fail["UnsupportedMomentum", "Momentum routing must be a linear combination with exact rational coefficients."]
            ];
            factors = List @@ term;
            momenta = Select[factors, vectorIdentityQ];
            coefficients = Select[factors, !vectorIdentityQ[#] &];
            If[Length[momenta] =!= 1 || !AllTrue[coefficients, MatchQ[#, _Integer | _Rational] &],
              fail["UnsupportedMomentum", "Momentum routing must be a linear combination with exact rational coefficients."]
            ];
            {Times @@ coefficients, register["Vector", First[momenta]]}
          ]
        ]
      ],
      terms
    ]
  ];
  vector[x_Plus] := Flatten[vector /@ (List @@ x), 1];
  vector[Times[c : (_Integer | _Rational), m_Momentum]] := ({c #[[1]], #[[2]]} & /@ vector[m]);
  vector[_] := fail["UnsupportedMomentum", "Expected a linear combination of dimension-tagged momenta."];
  (* Memoize successful fragments only within this export's Module. First
     occurrences still validate and register in traversal order; failures throw
     before assignment. Do not memoize emit: sums allocate ordered macros. *)
  pair[p : Pair[a_LorentzIndex, b_LorentzIndex]] := pair[p] = "d_(" <> index[a] <> "," <> index[b] <> ")";
  pair[p : Pair[a_LorentzIndex, b_Momentum]] := pair[p] = Module[{i = index[a]},
    "(" <> StringRiffle[("(" <> emit[#[[1]]] <> "*" <> #[[2]] <> "(" <> i <> "))") & /@ vector[b], "+"] <> ")"];
  pair[Pair[a_Momentum, b_LorentzIndex]] := pair[Pair[b, a]];
  pair[p : Pair[a_Momentum, b_Momentum]] := pair[p] = Module[{va = vector[a], vb = vector[b]},
    "(" <> StringRiffle[Flatten[Table[
      "(" <> emit[u[[1]] v[[1]]] <> "*" <> u[[2]] <> "." <> v[[2]] <> ")",
      {u, va}, {v, vb}]], "+"] <> ")"];
  pair[_] := fail["UnsupportedPair", "Only Lorentz metrics, momentum components and scalar products are supported."];

  denominator[pd : PropagatorDenominator[mom_, mass_: 0]] := denominator[pd] = Module[{},
    If[!FreeQ[mom, _Polarization], fail["UnsupportedMomentum", "Propagator routing cannot contain polarization vectors."]];
    vector[mom];
    If[!scalarQ[mass], fail["UnsupportedMass", "Propagator masses must be exact scalar expressions."]];
    (* Positive integer powers remain powers of the same identifier. Each
       identifier denotes one ordinary Feynman denominator, including i0. *)
    register["Denominator", FeynAmpDenominator[pd], <|
      "Momentum" -> encode[mom], "Mass" -> encode[mass], "Power" -> 1,
      "Dimension" -> encode[dim], "Prescription" -> "Feynman+i0"|>]
  ];
  denominator[x_] := fail["UnsupportedDenominator", "Only ordinary quadratic PropagatorDenominator objects are supported.", <|"Expression" -> HoldForm[x]|>];
  abbreviation[x_] := abbreviation[x] = If[scalarQ[x], register["Abbreviation", x],
    fail["UnsupportedScalar", "Only supported scalar expressions may be abbreviated."]];

  emit[x_] := Which[
    IntegerQ[x], intString[x],
    Head[x] === Rational, "(" <> intString[Numerator[x]] <> "/" <> intString[Denominator[x]] <> ")",
    Head[x] === Complex && scalarQ[x], "(" <> emit[Re[x]] <> "+i_*" <> emit[Im[x]] <> ")",
    Head[x] === Symbol && scalarQ[x], scalar[x],
    Head[x] === Plus, makeMacro[StringRiffle[emit /@ (List @@ x), "+"]],
    Head[x] === Times, "(" <> StringRiffle[emit /@ (List @@ x), "*"] <> ")",
    Head[x] === Pair, pair[x],
    Head[x] === FeynAmpDenominator, "(" <> StringRiffle[denominator /@ (List @@ x), "*"] <> ")",
    Head[x] === Power && IntegerQ[x[[2]]] && (x[[2]] >= 0 || Head[x[[1]]] === Symbol),
      "(" <> emit[x[[1]]] <> ")^(" <> intString[x[[2]]] <> ")",
    Head[x] === Power, abbreviation[x],
    KeyExistsQ[$mastersByHead, Head[x]] && scalarQ[x],
      $mastersByHead[Head[x]]["FORMName"] <> "(" <> StringRiffle[emit /@ (List @@ x), ","] <> ")",
    True, fail["UnsupportedExpression", "The expression contains an unsupported structure.", <|"Expression" -> HoldForm[x], "Head" -> Head[x]|>]
  ];

  (* Dimension and requested loop vectors enter the registry before expression
     traversal, fixing their names independently of later staged emission. *)
  dimensionName = If[IntegerQ[dim], intString[dim], scalar[dim]];
  Scan[(register["Vector", #]) &, loops];
  (* Sort between top-level sums. Serialize first in original traversal order;
     only the later multiplication plan may change. The planner conservatively
     prioritizes connected tensor factors to avoid disjoint intermediate products. *)
  If[Head[expr] === Times && Count[List @@ expr, _Plus] >= 2,
    factorExpressions = List @@ expr;
    factorTexts = emit /@ factorExpressions;
    stageEnds = Flatten[Position[factorExpressions, _Plus, {1}, Heads -> False]];
    stageEnds[[-1]] = Length[factorTexts];
    stageRanges = MapThread[{#1 + 1, #2} &, {Prepend[Most[stageEnds], 0], stageEnds}];
    stageTexts = ("(" <> StringRiffle[Take[factorTexts, #], "*"] <> ")") & /@ stageRanges;
    stageExpressions = (Times @@ Take[factorExpressions, #]) & /@ stageRanges;
    stageSignatures = If[FreeQ[stageExpressions, _LorentzIndex],
      ConstantArray[<||>, Length[stageExpressions]], stageIndexSignatures[stageExpressions]];
    stageOrder = connectedStageOrder[stageExpressions, stageSignatures];
    stageTexts = stageTexts[[stageOrder]];
    (* Large, unambiguous tensor stages are normalized once in FORM before
       reuse. The 1024-leaf cutoff is a conservative heuristic for small jobs.
       Named hidden factors remain available until the program ends. Never
       prepare an ambiguous index expression: that could change its existing
       contraction behavior even when the stage order is unchanged. *)
    If[stageSignatures =!= $Failed && !FreeQ[stageExpressions, _LorentzIndex] &&
       Max[LeafCount /@ stageExpressions] >= 1024,
      preparations = stageTexts;
      stageTexts = Table["cfcStage" <> intString[i], {i, Length[stageTexts]}]];
    body = First[stageTexts];
    multiplications = Rest[stageTexts],
    body = emit[expr]];
  payload = <|"Format" -> $formatName, "Version" -> $formatVersion,
    "ExpressionDigest" -> Hash[expr, "SHA256", "HexString"],
    "Dimension" -> encode[dim], "LoopMomenta" -> (encode /@ loops),
    "Processing" -> "TensorAlgebraOnly", "Entries" -> entries|>;
  <|"DimensionName" -> dimensionName, "Body" -> body, "Multiplications" -> multiplications,
    "Factors" -> macros, "Preparations" -> preparations, "Mapping" -> payload|>
];

(* ::Subsection:: *)
(*Render the FORM program and mapping*)

(* ::Input::Initialization:: *)
(* Pure text generation: the caller supplies both the template and result path. *)
renderExport[data_Association, result_String, template_String] := Module[
  {entries = data["Mapping"]["Entries"], declaration, json, digest, program},
  declaration[type_] := Module[{names},
    names = Lookup[Select[entries, $kindSpecs[#["Kind"]]["Declaration"] === type &], "Name", {}];
    If[names === {}, "", type <> " " <> StringRiffle[names, ","] <> ";"]
  ];
  (* The digest binds the exact serialized mapping text. Reformatting JSON
     after this point would break correspondence with the generated result. *)
  json = ExportString[data["Mapping"], "RawJSON", "Compact" -> True];
  digest = Hash[json, "SHA256", "HexString"];
  program = StringReplace[template, {
    "@FORMATVERSION@" -> IntegerString[$formatVersion], "@RESULTMARKER@" -> resultMarker[],
    "@FUNCTIONS@" -> "CFunctions " <> StringRiffle[Keys[$mastersByFORM], ","] <> ";",
    "@SCALARS@" -> declaration["Symbols"], "@DIMENSION@" -> data["DimensionName"],
    "@VECTORS@" -> declaration["Vectors"], "@INDICES@" -> declaration["Indices"],
    "@FACTORS@" -> StringRiffle[data["Factors"], "\n"], "@EXPRESSION@" -> data["Body"],
    "@PREPARATIONS@" -> If[data["Preparations"] === {}, "",
      StringJoin[MapIndexed[("Local cfcStage" <> intString[First[#2]] <> " = " <> #1 <> ";\n") &,
        data["Preparations"]]] <> ".sort\nHide " <>
      StringRiffle[Table["cfcStage" <> intString[i], {i, Length[data["Preparations"]]}], ","] <> ";\n"],
    "@MULTIPLICATIONS@" -> StringJoin[(".sort\nMultiply " <> # <> ";\n") & /@ data["Multiplications"]],
    "@RESULT@" -> StringReplace[result, "\\" -> "/"], "@DIGEST@" -> digest}];
  <|"Program" -> program, "MappingJSON" -> json|>
];

readProgramTemplate[] := Module[{template},
  template = Quiet[Check[Import[FileNameJoin[{$moduleDirectory, "Templates", "Program.frm.in"}], "Text"], $Failed]];
  If[!StringQ[template], fail["MissingTemplate", "Cannot read the FORM program template."]];
  template
];

(* ::Subsection:: *)
(*Export paths and file writing*)

(* ::Input::Initialization:: *)
exportPaths[file_String, overwrite_] := Module[{input, mapping, result, paths},
  If[!BooleanQ[overwrite], fail["InvalidOption", "OverwriteTarget must be True or False."]];
  input = ExpandFileName[file];
  If[ToLowerCase[FileExtension[input]] =!= "frm", fail["InvalidPath", "The FORM input filename must end in .frm."]];
  mapping = FileNameJoin[{DirectoryName[input], FileBaseName[input] <> ".map.json"}];
  result = FileNameJoin[{DirectoryName[input], FileBaseName[input] <> ".out"}];
  paths = {input, mapping, result};
  If[!DirectoryQ[DirectoryName[input]], fail["InvalidPath", "The destination directory does not exist."]];
  If[AnyTrue[paths, StringContainsQ[#, {"\n", "\r", "<", ">", "`", "'", "\""}] &],
    fail["InvalidPath", "FORM output paths cannot contain quotes, angle brackets, backticks or line breaks."]];
  If[!overwrite && AnyTrue[paths, FileExistsQ], fail["FileExists", "An export target already exists. Use OverwriteTarget -> True to replace it."]];
  <|"InputFile" -> input, "MappingFile" -> mapping, "ResultFile" -> result|>
];

(* Small I/O boundaries allow failure injection without replacing filesystem primitives. *)
exportWriteText[path_, text_] := Quiet[Check[Export[path, text, "Text", CharacterEncoding -> "UTF-8"], $Failed]];
exportCopyFile[source_, target_, overwrite_: False] := Quiet[Check[CopyFile[source, target, OverwriteTarget -> overwrite], $Failed]];
exportRenameFile[source_, target_, overwrite_] := Quiet[Check[RenameFile[source, target, OverwriteTarget -> overwrite], $Failed]];
exportDeleteFile[path_] := !FileExistsQ[path] || TrueQ[Quiet[Check[DeleteFile[path]; True, False]]];

(* Prepare both files before replacing either target. The backups and replaced
   flags describe this invocation's recoverable changes; this is not a lock or
   a crash-recovery protocol for concurrent writers. *)
writeExport[paths_Association, rendered_Association, overwrite_] := Module[
  {targets = Lookup[paths, {"InputFile", "MappingFile"}],
   contents = Lookup[rendered, {"Program", "MappingJSON"}],
   staged, backups, existed, replaced = {False, False}, committed = False,
   cleanup, outcome, result, aborted = False, rollbackFailed = False,
   recovery = {}, token},
  token = CreateUUID[];
  staged = (# <> "." <> token <> ".tmp") & /@ targets;
  backups = (# <> "." <> token <> ".bak") & /@ targets;
  existed = FileExistsQ /@ targets;

  (* Define cleanup without running it; WithCleanup invokes it on exit. Only
     targets replaced by this invocation are restored or removed. *)
  cleanup[] := Module[{restored = True},
    If[!committed,
      Do[
        If[replaced[[i]],
          If[existed[[i]],
            If[!StringQ[exportCopyFile[backups[[i]], targets[[i]], True]],
              restored = False
            ],
            If[!exportDeleteFile[targets[[i]]], restored = False]
          ]
        ],
        {i, Length[targets]}
      ]
    ];
    Scan[exportDeleteFile, staged];
    (* A failed restoration must leave the recovery copies available. *)
    If[restored,
      Scan[exportDeleteFile, backups],
      rollbackFailed = True;
      recovery = Select[backups, FileExistsQ]
    ]
  ];

  (* Cleanup runs on success, a tagged failure, or abort. Record an abort only
     after cleanup, so rollback failure can report retained recovery files. *)
  result = CheckAbort[
    Catch[
      WithCleanup[
        Do[
          If[!StringQ[exportWriteText[staged[[i]], contents[[i]]]],
            fail["WriteFailed", "Cannot stage the FORM program and mapping.",
              <|"Path" -> staged[[i]]|>]
          ],
          {i, Length[targets]}
        ];
        Do[
          If[existed[[i]],
            If[!overwrite, fail["FileExists", "An export target already exists."]];
            If[!StringQ[exportCopyFile[targets[[i]], backups[[i]]]],
              fail["WriteFailed", "Cannot preserve the existing export before replacement.",
                <|"Path" -> targets[[i]]|>]
            ]
          ],
          {i, Length[targets]}
        ];
        Do[
          (* Replacement and ownership registration form one abort-protected step. *)
          AbortProtect[
            outcome = exportRenameFile[staged[[i]], targets[[i]], overwrite];
            If[StringQ[outcome], replaced[[i]] = True]
          ];
          If[!StringQ[outcome],
            fail["WriteFailed", "Cannot replace the FORM program and mapping; original files were restored.",
              <|"Path" -> targets[[i]]|>]
          ],
          {i, Length[targets]}
        ];
        committed = True;
        paths,
        cleanup[]
      ],
      $failureTag
    ],
    aborted = True;
    $Aborted
  ];
  If[rollbackFailed,
    fail["RollbackFailed", "Export failed and the original files could not all be restored. Recovery copies were retained.",
      <|"RecoveryFiles" -> recovery, "Targets" -> targets|>]
  ];
  If[aborted, Abort[]];
  result
];

(* ::Subsection:: *)
(*Public export orchestration*)

(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormExport[expression_, file_String, OptionsPattern[]] := Catch[
  Module[{paths, data, rendered, overwrite = OptionValue[OverwriteTarget]},
    paths = exportPaths[file, overwrite];
    data = buildExportData[expression, OptionValue[Dimension], OptionValue[LoopMomenta]];
    rendered = renderExport[data, paths["ResultFile"], readProgramTemplate[]];
    writeExport[paths, rendered, overwrite]
  ], $failureTag];
CalcFormConverter`CalcFormExport[___] := Failure["InvalidArguments", <|"MessageTemplate" -> "Use CalcFormExport[expression, filename, options]."|>];

(* ::Section:: *)
(*Import: FORM parsing and FeynCalc reconstruction*)

(* ::Subsection:: *)
(*Restricted result parser*)

(* ::Input::Initialization:: *)
(* 5. FORM result parsing and FeynCalc reconstruction. Recursive descent recognizes arithmetic,
   native tensor syntax and four reserved master-integral functions only.
   Vector/index tokens have distinct types until converted into Pair objects. *)
parseGeneralResult[text_String, values_Association, dim_] := Module[
  {tokens, pos = 1, peek, take, expect, atom, power, unary, product, sum,
   scalarValue, entryValue, call, dot, result, tokenPattern, stripped, classes,
   tokenCount, compoundPattern, compoundValue},
  (* Only complete component/metric calls form composite tokens. Tokenize the
     original text so whitespace cannot join identifier fragments. Dots remain
     ordinary operators to preserve left-to-right validation of invalid chains. *)
  compoundPattern = "(?:cfv[0-9]+\\s*\\(\\s*cfi[0-9]+\\s*\\)|d_\\s*\\(\\s*cfi[0-9]+\\s*,\\s*cfi[0-9]+\\s*\\))";
  tokenPattern = RegularExpression[compoundPattern <> "|[A-Za-z][A-Za-z0-9_]*|[0-9]+|[+*/^(),.\\-]"];
  stripped = StringReplace[text, WhitespaceCharacter -> ""];
  tokens = StringCases[text, tokenPattern];
  If[StringReplace[StringJoin[tokens], WhitespaceCharacter -> ""] =!= stripped || tokens === {}, fail["InvalidResult", "The result contains invalid syntax."]];
  (* Classify each distinct lexeme once: 0 = integer, 1 = identifier,
     2 = punctuation, 3 = complete native call. This avoids repeating regular
     expression matches at every occurrence in a large result. *)
  classes = Association[
    Map[# -> Which[
      StringMatchQ[#, DigitCharacter ..], 0,
      StringMatchQ[#, RegularExpression["[A-Za-z][A-Za-z0-9_]*"]], 1,
      StringMatchQ[#, RegularExpression[compoundPattern]], 3,
      True, 2
    ] &, DeleteDuplicates[tokens]]
  ];
  (* Decode lazily in parse order, using the same identifier and argument
     checks as ordinary calls. Eager decoding could report a later unknown
     identifier before an earlier syntax/type error. SetDelayed defines the
     reader; its inner Set caches only successful results within this import. *)
  compoundValue[t_] := compoundValue[t] = With[
    {parts = StringCases[t, RegularExpression["[A-Za-z][A-Za-z0-9_]*"]]},
    call[First[parts], entryValue /@ Rest[parts]]];
  (* The sentinel serves lookahead only. take[] and the trailing-token check
     use the original count, so it can never be consumed as input. *)
  tokenCount = Length[tokens];
  tokens = Append[tokens, "END"];
  peek[] := tokens[[pos]];
  take[] := (
    If[pos > tokenCount, fail["InvalidResult", "Unexpected end of FORM result."]];
    tokens[[pos++]]
  );
  expect[t_] := If[take[] =!= t, fail["InvalidResult", "Unexpected token in FORM result."]];
  (* Check types before arithmetic can cancel an invalid vector/index. *)
  scalarValue[x_] := If[!FreeQ[x, _vectorToken | _indexToken],
    fail["InvalidResult", "A vector or index occurs outside a tensor object."], x];
  entryValue[n_] := Lookup[values, n,
    fail["UnknownIdentifier", "Unknown identifier in FORM result.", <|"Identifier" -> n|>]];
  call[n_, args_] := Which[
    n === "d_" && Length[args] == 2 && MatchQ[args, {_indexToken, _indexToken}],
      Pair[LorentzIndex[args[[1, 1]], dim], LorentzIndex[args[[2, 1]], dim]],
    KeyExistsQ[values, n] && MatchQ[values[n], _vectorToken] && MatchQ[args, {_indexToken}],
      Pair[Momentum[values[n][[1]], dim], LorentzIndex[args[[1, 1]], dim]],
    KeyExistsQ[$mastersByFORM, n] && masterArgumentsQ[$mastersByFORM[n], scalarValue /@ args],
      $mastersByFORM[n]["Head"] @@ (scalarValue /@ args),
    True, fail["InvalidResult", "Unsupported function or argument types in FORM result.", <|"Function" -> n|>]
  ];
  (* The cache key is the typed pair of vector values; dim is fixed for this
     import. A failed dot throws before the inner assignment can store it. *)
  dot[a_, b_] := dot[a, b] = If[MatchQ[{a, b}, {_vectorToken, _vectorToken}],
    Pair[Momentum[a[[1]], dim], Momentum[b[[1]], dim]],
    fail["InvalidResult", "Dot products require two declared vectors."]];
  (* With binds the consumed token or first operand once. Mutable recursive
     state belongs to each invocation's Module, never to an outer parser scope. *)
  atom[] := With[{t = take[]},
    Which[
      classes[t] === 0, FromDigits[t],
      classes[t] === 3, compoundValue[t],
      t === "(", With[{v = sum[]}, expect[")"]; v],
      classes[t] === 1,
        If[peek[] === "(",
          Module[{args = {}},
            take[];
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
          If[t === "i_", I, entryValue[t]]
        ],
      True, fail["InvalidResult", "Expected a number, declared symbol or parenthesized expression."]
    ]
  ];

  (* Dot chains are consumed in order before the optional integer exponent.
     Keep validation interleaved with consumption: a later unknown identifier
     must not replace the failure already caused by an invalid earlier dot. *)
  power[] := Module[{v = atom[]},
    While[peek[] === ".",
      take[];
      v = dot[v, atom[]]
    ];
    If[peek[] === "^",
      Module[{n, sign = 1, parenthesized = False},
        take[];
        If[peek[] === "(",
          take[];
          parenthesized = True
        ];
        If[peek[] === "-",
          take[];
          sign = -1,
          If[peek[] === "+", take[]]
        ];
        n = take[];
        If[!StringMatchQ[n, DigitCharacter ..],
          fail["InvalidResult", "FORM exponents must be integers."]
        ];
        If[parenthesized, expect[")"]];
        If[v === 0 && sign FromDigits[n] <= 0,
          fail["InvalidResult", "Undefined power of zero."]
        ];
        v = scalarValue[v]^(sign FromDigits[n])
      ]
    ];
    v
  ];
  unary[] := Switch[peek[],
    "+", take[]; unary[],
    "-", take[]; -scalarValue[unary[]],
    _, power[]
  ];

  (* Reap/Sow collects arbitrarily long products and sums without repeatedly
     copying a growing list. Each recursive invocation owns its collector.
     Validate each operand before Times or Plus can hide a typed token through
     zero multiplication or cancellation. Division is consumed left to right. *)
  product[] := With[{v = unary[]},
    If[!MemberQ[{"*", "/"}, peek[]],
      v,
      Module[{op, r, factors},
        factors = Reap[
          Sow[scalarValue[v]];
          While[MemberQ[{"*", "/"}, peek[]],
            op = take[];
            r = scalarValue[unary[]];
            If[op === "/" && r === 0, fail["InvalidResult", "Division by zero."]];
            Sow[If[op === "*", r, 1/r]]
          ]
        ][[2, 1]];
        Times @@ factors
      ]
    ]
  ];
  sum[] := With[{v = product[]},
    If[!MemberQ[{"+", "-"}, peek[]],
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
  result = scalarValue[sum[]];
  If[pos <= tokenCount,
    fail["InvalidResult", "Unexpected trailing tokens in FORM result."]
  ];
  result
];

(* Large contracted FORM outputs often repeat a small set of complete factors.
   This lexical subset excludes parentheses, calls, unary signs after operators,
   and chained dots. Everything outside it retains the general parser above.
   Possessive quantifiers avoid extensive backtracking on long malformed input. *)
$flatFactorPattern = "(?:cfv[0-9]++\\s*+\\.\\s*+cfv[0-9]++|cfs[0-9]++|cfa[0-9]++|cfd[0-9]++|i_|[0-9]++)(?:\\s*+\\^\\s*+[+-]?+\\s*+[0-9]++)?";
flatResultQ[text_String] := StringMatchQ[text,
  RegularExpression["\\s*+[+-]?+\\s*+" <> $flatFactorPattern <> "(?:\\s*+[+*/-]\\s*+" <> $flatFactorPattern <> ")*+\\s*+"]];
(* The full-string guard establishes alternating factors and operators. It does
   not validate mapped values: cache misses still use the general parser, lazily
   in consumption order. Each invocation owns its factor cache and collectors. *)
parseFlatResult[text_String, values_Association, dim_] := Module[
  {tokens, count, pos = 1, factor, checked, product, firstSign = 1, result, terms, op, r},
  tokens = StringCases[text, RegularExpression[$flatFactorPattern <> "|[+*/-]"]];
  (* Regex whitespace and WhitespaceCharacter need not cover identical Unicode
     characters. Keep the general parser's lexical coverage check authoritative. *)
  If[StringReplace[StringJoin[tokens], WhitespaceCharacter -> ""] =!=
     StringReplace[text, WhitespaceCharacter -> ""],
    Return[parseGeneralResult[text, values, dim]]
  ];
  count = Length[tokens];
  If[MemberQ[{"+", "-"}, First[tokens]],
    firstSign = If[First[tokens] === "-", -1, 1];
    pos++
  ];
  (* Require more than roughly four occurrences per distinct factor before
     paying for memoized general-parser calls. Unique-heavy inputs fall back. *)
  If[Length[DeleteDuplicates[tokens[[pos ;; ;; 2]]]] > (count - pos + 1)/8,
    Return[parseGeneralResult[text, values, dim]]
  ];
  (* Never evaluate result text as Wolfram source. Throw exits before Set can
     store a failed reconstruction; successful cached values remain job-local. *)
  factor[t_] := factor[t] = parseGeneralResult[t, values, dim];
  checked[x_] := If[!FreeQ[x, _vectorToken | _indexToken],
    fail["InvalidResult", "A vector or index occurs outside a tensor object."], x];
  (* Preserve the general parser's evaluation boundaries: the initial unary
     minus belongs to the first factor, later subtraction negates a whole term,
     and division checks its right operand before consuming anything later. *)
  product[sign_] := With[{v = If[sign === -1, -checked[factor[tokens[[pos++]]]], factor[tokens[[pos++]]]]},
    If[pos > count || !MemberQ[{"*", "/"}, tokens[[pos]]], v,
      Module[{operation, right, factors},
        factors = Reap[
          Sow[checked[v]];
          While[pos <= count && MemberQ[{"*", "/"}, tokens[[pos]]],
            operation = tokens[[pos++]];
            right = checked[factor[tokens[[pos++]]]];
            If[operation === "/" && right === 0, fail["InvalidResult", "Division by zero."]];
            Sow[If[operation === "*", right, 1/right]]
          ]
        ][[2, 1]];
        Times @@ factors
      ]
    ]
  ];
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
  checked[result]
];
(* The size and repetition cutoffs are conservative heuristics, not an optimal
   crossover model. Symbols with UpValues use the general path so user-defined
   arithmetic is evaluated per occurrence rather than memoized as a factor. *)
parseResult[text_String, values_Association, dim_] :=
  If[StringLength[text] >= 131072 &&
     AllTrue[DeleteDuplicates[Cases[{Values[values], dim}, _Symbol, Infinity, Heads -> True]], UpValues[#] === {} &] &&
     flatResultQ[text],
    parseFlatResult[text, values, dim],
    parseGeneralResult[text, values, dim]
  ];

(* ::Subsection:: *)
(*Mapping validation and public importer*)

(* ::Input::Initialization:: *)
(* Decode and validate every entry once, including entries eliminated by FORM.
   The job-local dictionary also carries vector/index types for the parser.
   Denominator Expression data is authoritative; convenience metadata never
   overrides it during reconstruction. *)
decodeEntries[entries_List] := Association[
  Map[
    Function[entry,
      Module[{value = decode[entry["Expression"]]},
        If[!TrueQ[$kindSpecs[entry["Kind"]]["ValidExpression"][value]],
          fail["InvalidMapping", "Mapped expression does not match its declared kind."]
        ];
        entry["Name"] -> Switch[entry["Kind"],
          "Vector", vectorToken[value],
          "Index", indexToken[value],
          _, value
        ]
      ]
    ],
    entries
  ]
];

(* Validate correspondence and entry names before decoding, then validate
   every mapped value before parsing the result. The digest detects a mismatched
   file pair; it does not replace expression or grammar validation. *)
CalcFormConverter`CalcFormImport[resultFile_String, mappingFile_String] := Catch[
  Module[{json, mapping, text, lines, digest, entries, dim, names, values},
    json = Quiet[Check[Import[mappingFile, "Text", CharacterEncoding -> "UTF-8"], $Failed]];
    text = Quiet[Check[Import[resultFile, "Text", CharacterEncoding -> "UTF-8"], $Failed]];
    If[!StringQ[json] || !StringQ[text], fail["ReadFailed", "Cannot read the result or mapping file."]];
    (* Older kernels expose RawJSON strings as UTF-8 bytes, while notebook
       sessions may supply Unicode text. Keep version-one byte-string files
       readable and retry genuine Unicode through an explicit UTF-8 buffer. *)
    mapping = Quiet[Check[ImportString[json, "RawJSON"], $Failed]];
    If[mapping === $Failed,
      mapping = Quiet[Check[ImportByteArray[ByteArray[ToCharacterCode[json, "UTF-8"]], "RawJSON"], $Failed]]];
    If[!AssociationQ[mapping] || Lookup[mapping, "Format", None] =!= $formatName ||
       Lookup[mapping, "Version", None] =!= $formatVersion,
      fail["InvalidMapping", "Unsupported mapping format or version."]];
    lines = StringSplit[StringReplace[text, "\r\n" -> "\n"], "\n"];
    (* Early notebook exports could decode the UTF-8 byte string during
       writing, after its checksum was calculated. Accept that exact legacy
       representation too; both checksums still bind the complete mapping. *)
    digest = Hash[#, "SHA256", "HexString"] & /@
      {json, FromCharacterCode[ToCharacterCode[json, "UTF-8"]]};
    If[Length[lines] < 2 || !MemberQ[(resultMarker[] <> " " <> # &) /@ digest, First[lines]],
      fail["MappingMismatch", "The result does not correspond to this mapping file."]];
    entries = Lookup[mapping, "Entries", None];
    If[!ListQ[entries] || !AllTrue[entries, AssociationQ], fail["InvalidMapping", "Invalid mapping entries."]];
    names = Lookup[entries, "Name", {}];
    If[!DuplicateFreeQ[names] || !AllTrue[entries,
       validEntryNameQ[#] &&
       KeyExistsQ[#, "Expression"] &], fail["InvalidMapping", "Invalid or duplicate mapping identifiers."]];
    dim = decode[Lookup[mapping, "Dimension", None]];
    If[!MatchQ[dim, _Symbol | _Integer] || (IntegerQ[dim] && dim < 2) || dim === I, fail["InvalidMapping", "Invalid mapped dimension."]];
    values = decodeEntries[entries];
    parseResult[StringRiffle[Rest[lines], "\n"], values, dim]
  ], $failureTag];
CalcFormConverter`CalcFormImport[___] := Failure["InvalidArguments", <|"MessageTemplate" -> "Use CalcFormImport[resultFile, mappingFile]."|>];

(* ::Section:: *)
(*Load runtime definitions without executing processes*)

(* ::Input::Initialization:: *)
Get[FileNameJoin[{$moduleDirectory, "FORMRuntime.wl"}]];

(* ::Section:: *)
(*Close the package*)

(* ::Input::Initialization:: *)
End[];
EndPackage[];
