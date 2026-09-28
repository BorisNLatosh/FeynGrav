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
CalcFormConverter`CalcFormCalculate::usage = "CalcFormCalculate[expr, opts] exports, executes FORM and imports its result. Options include Dimension, LoopMomenta, FORMExecutable, TimeConstraint, WorkingDirectory, KeepFiles, ShowTiming, ShowProgress and FORMThreads. Failed jobs are retained.";
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
  "Vector" -> <|"Prefix" -> "cfv", "Declaration" -> "Vectors", "ValidExpression" -> (Head[#] === Symbol &)|>,
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
   a string from a file is never evaluated as Wolfram Language code. *)
validSymbolNameQ[s_String] := StringContainsQ[s, "`"] &&
  StringMatchQ[s, RegularExpression["[\\p{L}$][\\p{L}\\p{N}$]*(?:`[\\p{L}$][\\p{L}\\p{N}$]*)+"]];
validSymbolNameQ[_] := False;
decode[{"Integer", s_String}] := integerData[s];
decode[{"Rational", a_String, b_String}] := Module[{den = integerData[b]},
  If[den == 0, fail["InvalidMapping", "Zero rational denominator."]]; integerData[a]/den];
decode[{"Complex", a_, b_}] := Module[{re = decode[a], im = decode[b]},
  If[!MatchQ[{re, im}, {( _Integer | _Rational), (_Integer | _Rational)}],
    fail["InvalidMapping", "Complex coefficients must be exact numbers."]]; re + I im];
decode[{"Symbol", s_String}] := If[validSymbolNameQ[s], Symbol[s],
  fail["InvalidMapping", "Invalid fully qualified symbol name."]];
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
  MatchQ[#, _Symbol | Times[(_Integer | _Rational), _Symbol]] &];
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
(* Local mutable state belongs only to expression traversal. Rendering and
   file writing consume the resulting association and cannot change it. *)
buildExportData[expression_, requestedDimension_, loops_] := Module[
 {expr, dim, entries = {}, registry = <||>, counters = <||>, macros = {},
  register, scalar, vector, index, pair, denominator, emit, abbreviation, makeMacro,
  body, dimensionName, payload, factorExpressions, factorTexts, stageEnds, multiplications = {}},
  If[!ListQ[loops] || !AllTrue[loops, MatchQ[#, _Symbol] &] || !DuplicateFreeQ[loops],
    fail["InvalidLoopMomenta", "LoopMomenta must be a list of distinct momentum symbols."]];
  expr = FCI[expression];
  dim = chooseDimension[expr, requestedDimension];

  register[kind_, value_, extra_: <||>] := With[{key = HoldComplete[kind, value]},
    (* Structural keys avoid serializing repeated occurrences. Allocate fresh
       locals only when inserting a new entry into the registry. *)
    If[KeyExistsQ[registry, key], registry[key],
      Module[{name, number, prefix},
        number = Lookup[counters, kind, 0] + 1; AssociateTo[counters, kind -> number];
        prefix = $kindSpecs[kind]["Prefix"];
        name = prefix <> intString[number];
        AssociateTo[registry, key -> name];
        AppendTo[entries, Join[<|"Name" -> name, "Kind" -> kind, "Expression" -> encode[value]|>, extra]];
        name
      ]]
  ];
  makeMacro[s_] := Module[{name = "CFCF" <> intString[Length[macros] + 1]},
    AppendTo[macros, "#define " <> name <> " \"(" <> s <> ")\""];
    "`" <> name <> "'"
  ];
  scalar[s_Symbol] := If[s === I, "i_", register["Scalar", s]];
  index[LorentzIndex[i_Symbol, ___]] := register["Index", i];
  index[_] := fail["UnsupportedIndex", "Lorentz indices must be symbols."];
  vector[Momentum[v_, ___]] := Module[{terms},
    terms = If[Head[v] === Plus, List @@ v, {v}];
    Map[Function[term, Module[{factors, momenta, coefficients},
      If[Head[term] === Symbol, {1, register["Vector", term]},
      If[Head[term] =!= Times, fail["UnsupportedMomentum", "Momentum routing must be a linear combination with exact rational coefficients."]];
      factors = List @@ term; momenta = Select[factors, Head[#] === Symbol &];
      coefficients = Select[factors, Head[#] =!= Symbol &];
      If[Length[momenta] =!= 1 || !AllTrue[coefficients, MatchQ[#, _Integer | _Rational] &],
        fail["UnsupportedMomentum", "Momentum routing must be a linear combination with exact rational coefficients."]];
      {Times @@ coefficients, register["Vector", First[momenta]]}]
    ]], terms]
  ];
  vector[x_Plus] := Flatten[vector /@ (List @@ x), 1];
  vector[Times[c : (_Integer | _Rational), m_Momentum]] := ({c #[[1]], #[[2]]} & /@ vector[m]);
  vector[_] := fail["UnsupportedMomentum", "Expected a linear combination of dimension-tagged momenta."];
  pair[Pair[a_LorentzIndex, b_LorentzIndex]] := "d_(" <> index[a] <> "," <> index[b] <> ")";
  pair[Pair[a_LorentzIndex, b_Momentum]] := Module[{i = index[a]},
    "(" <> StringRiffle[("(" <> emit[#[[1]]] <> "*" <> #[[2]] <> "(" <> i <> "))") & /@ vector[b], "+"] <> ")"];
  pair[Pair[a_Momentum, b_LorentzIndex]] := pair[Pair[b, a]];
  pair[Pair[a_Momentum, b_Momentum]] := Module[{va = vector[a], vb = vector[b]},
    "(" <> StringRiffle[Flatten[Table[
      "(" <> emit[u[[1]] v[[1]]] <> "*" <> u[[2]] <> "." <> v[[2]] <> ")",
      {u, va}, {v, vb}]], "+"] <> ")"];
  pair[_] := fail["UnsupportedPair", "Only Lorentz metrics, momentum components and scalar products are supported."];

  denominator[pd : PropagatorDenominator[mom_, mass_: 0]] := Module[{},
    vector[mom]; If[!scalarQ[mass], fail["UnsupportedMass", "Propagator masses must be exact scalar expressions."]];
    (* Positive integer powers remain powers of the same identifier. Each
       identifier denotes one ordinary Feynman denominator, including i0. *)
    register["Denominator", FeynAmpDenominator[pd], <|
      "Momentum" -> encode[mom], "Mass" -> encode[mass], "Power" -> 1,
      "Dimension" -> encode[dim], "Prescription" -> "Feynman+i0"|>]
  ];
  denominator[x_] := fail["UnsupportedDenominator", "Only ordinary quadratic PropagatorDenominator objects are supported.", <|"Expression" -> HoldForm[x]|>];
  abbreviation[x_] := If[scalarQ[x], register["Abbreviation", x],
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

  (* 4. FORM program generation: declarations, factors and result output. *)
  dimensionName = If[IntegerQ[dim], intString[dim], scalar[dim]];
  Scan[(register["Vector", #]) &, loops];
  (* A new FORM expression is initially one input term, even when its source
     contains large sums. Sort between top-level sum factors so later products
     can use TFORM workers and combine intermediate terms before expanding more.
     Emit in the original traversal order to preserve the mapping and macros.
     Non-sum factors stay in order, in the same stage as the following sum;
     trailing factors belong to the final stage. No Wolfram expansion is used. *)
  If[Head[expr] === Times && Count[List @@ expr, _Plus] >= 2,
    factorExpressions = List @@ expr;
    factorTexts = emit /@ factorExpressions;
    stageEnds = Flatten[Position[factorExpressions, _Plus, {1}, Heads -> False]];
    stageEnds[[-1]] = Length[factorTexts];
    body = "(" <> StringRiffle[Take[factorTexts, First[stageEnds]], "*"] <> ")";
    multiplications = MapThread[
      ("(" <> StringRiffle[Take[factorTexts, {#1 + 1, #2}], "*"] <> ")") &,
      {Most[stageEnds], Rest[stageEnds]}],
    body = emit[expr]];
  payload = <|"Format" -> $formatName, "Version" -> $formatVersion,
    "ExpressionDigest" -> Hash[expr, "SHA256", "HexString"],
    "Dimension" -> encode[dim], "LoopMomenta" -> (encode /@ loops),
    "Processing" -> "TensorAlgebraOnly", "Entries" -> entries|>;
  <|"DimensionName" -> dimensionName, "Body" -> body, "Multiplications" -> multiplications,
    "Factors" -> macros, "Mapping" -> payload|>
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
  json = ExportString[data["Mapping"], "RawJSON", "Compact" -> True];
  digest = Hash[json, "SHA256", "HexString"];
  program = StringReplace[template, {
    "@FORMATVERSION@" -> IntegerString[$formatVersion], "@RESULTMARKER@" -> resultMarker[],
    "@FUNCTIONS@" -> "CFunctions " <> StringRiffle[Keys[$mastersByFORM], ","] <> ";",
    "@SCALARS@" -> declaration["Symbols"], "@DIMENSION@" -> data["DimensionName"],
    "@VECTORS@" -> declaration["Vectors"], "@INDICES@" -> declaration["Indices"],
    "@FACTORS@" -> StringRiffle[data["Factors"], "\n"], "@EXPRESSION@" -> data["Body"],
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

writeExport[paths_Association, rendered_Association, overwrite_] := Module[
 {targets = Lookup[paths, {"InputFile", "MappingFile"}], contents = Lookup[rendered, {"Program", "MappingJSON"}],
  staged, backups, existed, replaced = {False, False}, committed = False, cleanup, outcome, result, aborted = False, rollbackFailed = False, recovery = {}, token},
 token = CreateUUID[];
 staged = (# <> "." <> token <> ".tmp") & /@ targets;
 backups = (# <> "." <> token <> ".bak") & /@ targets;
 existed = FileExistsQ /@ targets;
 cleanup[] := Module[{restored = True},
   If[!committed,
     Do[If[replaced[[i]],
       If[existed[[i]],
         If[!StringQ[exportCopyFile[backups[[i]], targets[[i]], True]], restored = False],
         If[!exportDeleteFile[targets[[i]]], restored = False]]], {i, Length[targets]}]];
   Scan[exportDeleteFile, staged];
   (* A failed restoration must leave the recovery copies available. *)
   If[restored, Scan[exportDeleteFile, backups],
     rollbackFailed = True; recovery = Select[backups, FileExistsQ]]
 ];
 result = CheckAbort[Catch[WithCleanup[
   Do[
     If[!StringQ[exportWriteText[staged[[i]], contents[[i]]]],
       fail["WriteFailed", "Cannot stage the FORM program and mapping.", <|"Path" -> staged[[i]]|>]], {i, Length[targets]}];
   Do[If[existed[[i]],
     If[!overwrite, fail["FileExists", "An export target already exists."]];
     If[!StringQ[exportCopyFile[targets[[i]], backups[[i]]]],
       fail["WriteFailed", "Cannot preserve the existing export before replacement.", <|"Path" -> targets[[i]]|>]]], {i, Length[targets]}];
   Do[
     (* Replacement and ownership registration form one abort-protected step. *)
     AbortProtect[
       outcome = exportRenameFile[staged[[i]], targets[[i]], overwrite];
       If[StringQ[outcome], replaced[[i]] = True]];
     If[!StringQ[outcome], fail["WriteFailed", "Cannot replace the FORM program and mapping; original files were restored.",
       <|"Path" -> targets[[i]]|>]], {i, Length[targets]}];
   committed = True;
   paths,
   cleanup[]], $failureTag], aborted = True; $Aborted];
 If[rollbackFailed, fail["RollbackFailed", "Export failed and the original files could not all be restored. Recovery copies were retained.",
   <|"RecoveryFiles" -> recovery, "Targets" -> targets|>]];
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
parseResult[text_String, values_Association, dim_] := Module[
 {tokens, pos = 1, peek, take, expect, atom, power, unary, product, sum,
  scalarValue, entryValue, call, dot, result, tokenPattern, stripped, classes, tokenCount},
 tokenPattern = RegularExpression["[A-Za-z][A-Za-z0-9_]*|[0-9]+|[+*/^(),.\\-]"];
 stripped = StringReplace[text, WhitespaceCharacter -> ""];
 tokens = StringCases[text, tokenPattern];
 If[StringJoin[tokens] =!= stripped || tokens === {}, fail["InvalidResult", "The result contains invalid syntax."]];
 (* Classify each distinct lexeme once rather than matching a regular
    expression at every occurrence in a large result. *)
 classes = Association[Map[# -> Which[StringMatchQ[#, DigitCharacter ..], 0,
   StringMatchQ[#, RegularExpression["[A-Za-z][A-Za-z0-9_]*"]], 1, True, 2] &, DeleteDuplicates[tokens]]];
 (* The sentinel serves lookahead only. take[] and the trailing-token check
    use the original count, so it can never be consumed as input. *)
 tokenCount = Length[tokens]; tokens = Append[tokens, "END"];
 peek[] := tokens[[pos]];
 take[] := (
   If[pos > tokenCount, fail["InvalidResult", "Unexpected end of FORM result."]];
   tokens[[pos++]]);
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
 dot[a_, b_] := If[MatchQ[{a, b}, {_vectorToken, _vectorToken}],
   Pair[Momentum[a[[1]], dim], Momentum[b[[1]], dim]],
   fail["InvalidResult", "Dot products require two declared vectors."]];
 (* Allocate mutable locals only on branches that need them; immutable
    bindings remain local to each recursive invocation. *)
 atom[] := With[{t = take[]},
   Which[
     classes[t] === 0, FromDigits[t],
     t === "(", With[{v = sum[]}, expect[")"]; v],
     classes[t] === 1,
       If[peek[] === "(", Module[{args = {}}, take[];
         If[peek[] =!= ")", AppendTo[args, sum[]]; While[peek[] === ",", take[]; AppendTo[args, sum[]]]];
         expect[")"]; call[t, args]], If[t === "i_", I, entryValue[t]]],
     True, fail["InvalidResult", "Expected a number, declared symbol or parenthesized expression."]
   ]
 ];
 power[] := Module[{v = atom[]},
   While[peek[] === ".", take[]; v = dot[v, atom[]]];
   If[peek[] === "^", Module[{n, sign = 1, parenthesized = False}, take[];
     If[peek[] === "(", take[]; parenthesized = True];
     If[peek[] === "-", take[]; sign = -1, If[peek[] === "+", take[]]];
     n = take[]; If[!StringMatchQ[n, DigitCharacter ..], fail["InvalidResult", "FORM exponents must be integers."]];
     If[parenthesized, expect[")"]];
     If[v === 0 && sign FromDigits[n] <= 0, fail["InvalidResult", "Undefined power of zero."]];
     v = scalarValue[v]^(sign FromDigits[n])]]; v
 ];
 unary[] := Switch[peek[], "+", take[]; unary[], "-", take[]; -scalarValue[unary[]], _, power[]];
 (* Reap/Sow collects arbitrarily long products and sums without repeatedly
    copying a growing list. Nested parser calls own their collectors. *)
 product[] := With[{v = unary[]},
   If[!MemberQ[{"*", "/"}, peek[]], v,
     Module[{op, r, factors},
       factors = Reap[
         Sow[scalarValue[v]];
         While[MemberQ[{"*", "/"}, peek[]], op = take[]; r = scalarValue[unary[]];
           If[op === "/" && r === 0, fail["InvalidResult", "Division by zero."]];
           Sow[If[op === "*", r, 1/r]]]][[2, 1]];
       Times @@ factors]]];
 sum[] := With[{v = product[]},
   If[!MemberQ[{"+", "-"}, peek[]], v,
     Module[{op, r, terms},
       terms = Reap[
         Sow[scalarValue[v]];
         While[MemberQ[{"+", "-"}, peek[]], op = take[]; r = scalarValue[product[]];
           Sow[If[op === "+", r, -r]]]][[2, 1]];
       Plus @@ terms]]];
 result = scalarValue[sum[]];
 If[pos <= tokenCount, fail["InvalidResult", "Unexpected trailing tokens in FORM result."]];
 result
];

(* ::Subsection:: *)
(*Mapping validation and public importer*)

(* ::Input::Initialization:: *)
(* Decode and validate every entry once, including entries eliminated by FORM.
   The job-local dictionary also carries vector/index types for the parser. *)
decodeEntries[entries_List] := Association[Map[Function[entry, Module[{value = decode[entry["Expression"]]},
  If[!TrueQ[$kindSpecs[entry["Kind"]]["ValidExpression"][value]],
    fail["InvalidMapping", "Mapped expression does not match its declared kind."]];
  entry["Name"] -> Switch[entry["Kind"],
    "Vector", vectorToken[value], "Index", indexToken[value], _, value]
]], entries]];

(* Validate the result/mapping pair before reconstruction. *)
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
