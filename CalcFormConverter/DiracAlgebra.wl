(* ::Section:: *)
(*Dirac algebra: configuration, trace boundaries and FORM procedures*)

(* ::Text:: *)
(*Loaded in the converter's private context. All matrix algebra runs in FORM.
  Wolfram code validates traces, allocates lines and specialises a terminating
  Clifford rewrite system to the finite dictionary of indices and vectors.*)

(* ::Input::Initialization:: *)
CalcFormConverter`DiracAlgebra::usage = "DiracAlgebra is an option for CalcFormExport and CalcFormCalculate. Automatic simplifies and canonically orders ordinary open gamma chains and evaluates explicit DiracTrace expressions in FORM. False preserves translation-only behaviour and unevaluated traces. Gamma-five, spinors, and mixed Lorentz spaces are not supported. Colour processing is controlled separately by ColourAlgebra.";
$daMode = False;
$daTraceLines = {};
$daNextLine = 1;
$daImportLines = <||>;
$daImportMode = False;

(* ::Subsection:: *)
(*Validate explicit traces before allocating files or spin lines*)

(* ::Input::Initialization:: *)
dcNormalize[x_DiracTrace] := Module[{args = List @@ x, options, defaults, norm, body},
    If[args === {} || !AllTrue[Rest[args], MatchQ[#, _Rule | _RuleDelayed] &],
        throwFailure["UnsupportedDiracTrace", "Use DiracTrace[expression, options]."]];
    options = Rest[args]; defaults = Options[DiracTrace];
    If[!AllTrue[options, MemberQ[First /@ defaults, First[#]] &&
        (First[#] === TraceOfOne || Last[#] === (First[#] /. defaults)) &],
        throwFailure["UnsupportedDiracTrace", "Only TraceOfOne may differ from the default DiracTrace options."]];
    norm = TraceOfOne /. Join[options, defaults];
    If[!scalarQ[norm], throwFailure["UnsupportedDiracTrace", "TraceOfOne must be a supported scalar."]];
    If[!FreeQ[First[args], DiracTrace | SUNT | SUNTrace],
        throwFailure["UnsupportedDiracTrace", "Nested traces and implicit colour words inside a Dirac trace are not supported."]];
    body = dcNormalize[First[args]];
    daTrace[body, norm]
];

(* ::Subsection:: *)
(*Emit open chains and distinct trace occurrences*)

(* ::Input::Initialization:: *)
daEmitOpen[word_dcDiracWord, vector_, index_, emit_] := Module[{text},
    text = dcEmitWord[word, vector, index, emit];
    If[$daMode === Automatic,
        StringReplace[text, {"gi_(1)" -> "cfcOpen()", "g_(1," -> "cfcOpen("}], text]
];

daEmitTrace[daTrace[body_, norm_], vector_, index_, emit_] := Module[
    {line = ++$daNextLine, lineText, terms, text},
    lineText = intString[line];
    AppendTo[$daTraceLines, <|"Line" -> line, "TraceOfOne" -> encode[norm]|>];
    (* dcTerms supplies linear terms and an explicit identity for scalar terms.
       Each syntactic occurrence receives its own line, including powers. *)
    terms = dcTerms[body];
    text = "(" <> StringRiffle[("(" <> emit[#[[1]]] <> "*" <>
        StringReplace[dcEmitWord[dcDiracWord[#[[2]]], vector, index, emit],
            {"gi_(1)" -> "gi_(" <> lineText <> ")", "g_(1," -> "g_(" <> lineText <> ","}] <> ")") & /@ terms, "+"] <> ")";
    If[$daMode === Automatic, "(" <> emit[norm] <> "*" <> text <> ")", text]
];

daEmitTracePower[x_Power, emit_] := Module[{n = x[[2]]},
    If[!IntegerQ[n] || n < 0,
        throwFailure["UnsupportedDiracTrace", "Trace-containing powers must have a non-negative integer exponent."]];
    "(" <> StringRiffle[Table[emit[x[[1]]], {n}], "*"] <> ")"
];

(* The ordering depends on encoded identities, never on accidental allocation
   order or FORM internals. Include every vector/index: tensor contraction can
   introduce into a gamma word a label absent from the original word. *)
daOrderedEntries[entries_List] := SortBy[
    Select[entries, MemberQ[{"Index", "Vector"}, #["Kind"]] &],
    {#["Kind"], ExportString[#["Expression"], "RawJSON", "Compact" -> True]} &];
daMetadata[mode_, entries_] := <|
    "DiracAlgebra" -> If[mode === Automatic, "Automatic", "False"],
    "TraceLines" -> $daTraceLines,
    "GammaOrdering" -> (#["Name"] & /@ daOrderedEntries[entries])|>;

daDeclarations[data_] := If[Lookup[data["Mapping"], "DiracAlgebra", "False"] === "Automatic",
    "\nTensor cfcOpen;\nunittrace 1;", ""];

(* ::Subsection:: *)
(*Generate a terminating Clifford rewrite procedure*)

(* ::Input::Initialization:: *)
daPairText[a_, b_] := Which[
    a["Kind"] === "Index" && b["Kind"] === "Index", "d_(" <> a["Name"] <> "," <> b["Name"] <> ")",
    a["Kind"] === "Vector" && b["Kind"] === "Vector", a["Name"] <> "." <> b["Name"],
    a["Kind"] === "Vector", a["Name"] <> "(" <> b["Name"] <> ")",
    True, b["Name"] <> "(" <> a["Name"] <> ")"
];

daProcessing[data_] := Module[{mapping = data["Mapping"], objects, rules, name, square, procedure},
    If[Lookup[mapping, "DiracAlgebra", "False"] =!= "Automatic", Return[""]];
    objects = daOrderedEntries[mapping["Entries"]];
    rules = Table[
        name = item["Name"];
        square = If[item["Kind"] === "Index", data["DimensionName"], daPairText[item, item]];
        "id cfcOpen(?a," <> name <> "," <> name <> ",?b) = " <> square <> "*cfcOpen(?a,?b);\n",
        {item, objects}];
    Do[AppendTo[rules,
        "id cfcOpen(?a," <> objects[[j]]["Name"] <> "," <> objects[[i]]["Name"] <>
        ",?b) = -cfcOpen(?a," <> objects[[i]]["Name"] <> "," <> objects[[j]]["Name"] <>
        ",?b)+2*" <> daPairText[objects[[j]], objects[[i]]] <> "*cfcOpen(?a,?b);\n"],
        {j, Length[objects]}, {i, j - 1}];
    (* Shortening terms can generate Lorentz contractions and new inversions.
       repeat normalises them before testing the same finite rules again. *)
    procedure = Import[FileNameJoin[{$moduleDirectory, "Templates", "DiracAlgebra.frm.in"}], "Text"];
    If[!StringQ[procedure], throwFailure["InvalidTemplate", "Cannot read the Dirac algebra procedure."]];
    StringReplace[procedure, {
        "@TRACES@" -> StringJoin[("tracen " <> intString[#["Line"]] <> ";\n") & /@ mapping["TraceLines"]],
        "@RULES@" -> StringJoin[rules],
        "@EPSILON@" -> If[KeyExistsQ[mapping, "EpsilonConvention"], "contract 0;\n", ""]}]
];

(* ::Subsection:: *)
(*Validate metadata and reconstruct unevaluated traces*)

(* ::Input::Initialization:: *)
daValidateMetadata[mapping_, entries_] := Module[{mode, lines, numbers, norm},
    mode = Lookup[mapping, "DiracAlgebra", None];
    lines = Lookup[mapping, "TraceLines", None];
    If[!MemberQ[{"Automatic", "False"}, mode] || !ListQ[lines] ||
        !AllTrue[lines, AssociationQ[#] && Sort[Keys[#]] === {"Line", "TraceOfOne"} &],
        throwFailure["InvalidMapping", "Invalid Dirac processing metadata."]];
    numbers = Lookup[lines, "Line", {}];
    If[numbers =!= Range[2, Length[lines] + 1] ||
        Lookup[mapping, "GammaOrdering", None] =!= (#["Name"] & /@ daOrderedEntries[entries]),
        throwFailure["InvalidMapping", "Invalid trace lines or gamma ordering."]];
    $daImportMode = mode;
    Do[norm = decode[item["TraceOfOne"]];
        If[!scalarQ[norm], throwFailure["InvalidMapping", "Invalid trace normalisation."]];
        AssociateTo[$daImportLines, item["Line"] -> norm], {item, lines}];
];

daImportTrace[name_, args_, dim_] := Module[{line, slots},
    line = First[args];
    If[!IntegerQ[line] || !KeyExistsQ[$daImportLines, line],
        throwFailure["InvalidResult", "Undeclared Dirac spin line."]];
    If[$daImportMode =!= "False", throwFailure["InvalidResult", "An explicitly requested trace was not evaluated by FORM."]];
    slots = Rest[args];
    If[(name === "gi_" && slots =!= {}) || (name === "g_" && slots === {}) ||
        !AllTrue[slots, MatchQ[#, _indexToken | _vectorToken] &],
        throwFailure["InvalidResult", "Invalid trace word arguments."]];
    daTraceToken[line, slots /. {indexToken[x_] :> DiracGamma[LorentzIndex[x, dim], dim],
        vectorToken[x_] :> DiracGamma[Momentum[x, dim], dim]}, $daImportLines[line]]
];

(* Keep trace-line identity until product validation has finished. Two native
   words on the same line denote one matrix chain, not two scalar traces. *)
daResultLines[daTraceToken[line_Integer, _List, _]] := <|line -> 1|>;
daResultLines[x_Plus] := Merge[daResultLines /@ List @@ x, Max];
daResultLines[x_Times] := Merge[daResultLines /@ List @@ x, Total];
daResultLines[_] := <||>;
