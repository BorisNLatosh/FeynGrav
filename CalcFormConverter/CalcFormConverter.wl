(* ::Package:: *)

(* ::Title:: *)
(*CalcFormConverter*)


(* ::Text:: *)
(*Bosonic FeynCalc \[LeftRightArrow] FORM conversion. Runtime definitions load separately. Scalar abbreviations and propagators remain opaque to FORM in version 1. The mapping is JSON data, not executable Wolfram Language source.*)


(* ::Section:: *)
(*Public interface*)


(* ::Input::Initialization:: *)
BeginPackage["CalcFormConverter`", {"FeynCalc`"}];


(* ::Text::Initialization:: *)
(*(*1. Public functions and options.*)*)


(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormExport::usage ="CalcFormExport[expr, file, opts] exports an exact bosonic FeynCalc expression to a complete FORM program and a reversible JSON mapping. Returns an association with InputFile, MappingFile and ResultFile. Options: Dimension -> Automatic, LoopMomenta -> {}, OverwriteTarget -> False. No FORM process or integral reduction is performed.";


(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormImport::usage = "CalcFormImport[resultFile, mappingFile] reads the dedicated result of an exported FORM program and reconstructs FeynCalc internal notation. It does not execute Wolfram Language source or run tensor/integral reduction.";


(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormCheck::usage = "CalcFormCheck[opts] probes FORM on demand and returns availability, version and diagnostics. Options: FORMExecutable -> Automatic, FORMThreads -> 1, TimeConstraint -> 10.";


(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormInstall::usage = "CalcFormInstall[opts] explicitly attempts Debian/Ubuntu installation of missing FORM or TFORM using system authorization, then verifies the requested configuration. FORMThreads -> 1 is the default; values above 1 require TFORM. It is never called automatically.";


(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormCalculate::usage = "CalcFormCalculate[expr, opts] exports, executes FORM and imports its result. For a binary equality lhs == rhs, it calculates lhs - rhs and compares the imported residual with zero; the result may remain symbolic. True and False are returned directly. Options include Dimension, LoopMomenta, FORMExecutable, TimeConstraint, WorkingDirectory, KeepFiles, ShowTiming, ShowProgress and FORMThreads. Failed jobs are retained.";


(* ::Input::Initialization:: *)
CalcFormConverter`FORMExecutable::usage = "FORMExecutable selects the FORM executable by name or path; Automatic searches the Wolfram kernel's PATH for form, or tform when FORMThreads > 1.";


(* ::Input::Initialization:: *)
CalcFormConverter`WorkingDirectory::usage = "WorkingDirectory specifies an existing parent directory for unique FORM jobs; Automatic uses the system temporary directory.";


(* ::Input::Initialization:: *)
CalcFormConverter`ShowTiming::usage = "ShowTiming -> True prints FORM process elapsed wall-clock seconds, excluding the availability probe, export and import. Defaults to False; the returned expression is unchanged.";


(* ::Input::Initialization:: *)
CalcFormConverter`ShowProgress::usage = "ShowProgress -> True prints calculation stages and elapsed FORM execution time every ten seconds. It does not estimate a completion percentage. Defaults to False.";


(* ::Input::Initialization:: *)
CalcFormConverter`FORMThreads::usage = "FORMThreads specifies a positive integer worker count for CalcFormCheck, CalcFormInstall and CalcFormCalculate. The default 1 uses ordinary FORM; values above 1 select TFORM with -wN when FORMExecutable is Automatic. Explicit executables must pass a TFORM probe for multiple workers.";


(* ::Input::Initialization:: *)
CalcFormConverter`KeepFiles::usage = "KeepFiles -> True retains successful FORM calculation files and reports their location. Failed jobs are always retained.";


(* ::Subsection:: *)
(*Private initialization and export options*)


(* ::Input::Initialization:: *)
Begin["`Private`"];


(* ------------------------------------------------------------------- *)
(* 1. Locate this package on disk.                                     *)
(* $InputFileName is set by the kernel, while the file is being read   *)
(* by Get/Needs/<<, to the full path of that file. DirectoryName takes *)
(* its containing directory, so sibling .wl, .m, or data files can be  *)
(* loaded later with FileNameJoin[{$moduleDirectory, "..."}].          *)
(* Outside file loading (e.g. if this cell is evaluated in a notebook) *)
(* $InputFileName has no value and this stays unevaluated -- it only   *)
(* works inside an actual package file.                                *)
(* ------------------------------------------------------------------- *)
$moduleDirectory = DirectoryName[$InputFileName];


(* Human-readable name of the converter. Plain string data; no          *)
(* evaluation semantics. It is the identity half of the pair used to    *)
(* stamp and later recognize generated artifacts.                       *)
$formatName = "CalcFormConverter";


(* Wire-format version of the converter, as a machine-readable integer. *)
(* Kept separate from the name so it can be bumped independently when   *)
(* the emitted syntax changes.                                          *)
$formatVersion = 1;


(* resultMarker[] returns the short signature written into every file   *)
(* this converter produces: "CFC" <> "1" -> "CFC1".                      *)
(* It is SetDelayed (:=) and takes no arguments, so it re-reads         *)
(* $formatVersion on every call -- bumping the version above silently   *)
(* changes every marker emitted afterwards, with no other edits.        *)
resultMarker[] := "CFC" <> IntegerString[$formatVersion];


(* intString[n] renders an integer as a decimal string with an explicit *)
(* leading sign, suitable for writing into the target language:         *)
(*   intString[7]  -> "7"                                                *)
(*   intString[-7] -> "-7"                                               *)
(*   intString[0]  -> "0"                                                *)
(* If[n < 0, "-", ""] emits the sign (or nothing), then Appends...      *)
(* IntegerString[Abs[n]] gives the bare digits, since Abs removes the   *)
(* sign that IntegerString would otherwise re-emit.                     *)
(* The n_Integer pattern rejects non-integers loudly rather than        *)
(* silently producing a malformed literal. IntegerString is exact for   *)
(* arbitrary-precision integers too.                                    *)
intString[n_Integer] := If[n < 0, "-", ""] <> IntegerString[Abs[n]];


(* Create a private, unforgeable token used to tag this module's        *)
(* non-local error exits. Unique["CalcFormFailure"] manufactures a      *)
(* brand-new symbol with a unique numeric suffix on each evaluation     *)
(* (e.g. CalcFormFailure$4711), so no other code can name or collide    *)
(* with it. It is assigned no value, so it evaluates to itself; it      *)
(* serves purely as an inert marker for Throw/Catch.                    *)
(* -------------------------------------------------------------------- *)
$failureTag = Unique["CalcFormFailure"];
(* makeFailure constructs the package's one structured failure shape.    *)
(* Association Join is right-biased, so caller data may deliberately     *)
(* override MessageTemplate. throwFailure is the tagged nonlocal-exit     *)
(* form used by deep conversion and parser helpers. Runtime code and      *)
(* invalid-argument fallbacks return makeFailure directly.                *)
makeFailure[tag_String, message_String, data_ : <||>] :=
	Failure[tag, Join[<|"MessageTemplate" -> message|>, data]];
throwFailure[tag_String, message_String, data_ : <||>] :=
	Throw[makeFailure[tag, message, data], $failureTag];


(* Give CalcFormConverter`CalcFormExport its public option defaults.      *)
(* Options[symbol] = {...} is a plain assignment to the symbol's own      *)
(* Options value: it stores the list of default rules that OptionsPattern[]*)
(* and OptionValue[] will consult when the function is called.            *)
(* The left-hand side is deliberately written fully qualified as          *)
(* CalcFormConverter`CalcFormExport, so the assignment lands on the       *)
(* package's symbol even if this header is evaluated from a context where *)
(* the short name would resolve elsewhere (or create a stray Global` one). *)
Options[CalcFormConverter`CalcFormExport] = {
    (* FeynCalc owns these two option symbols. Qualifying them preserves  *)
    (* their historical identity while making it independent of context. *)
    FeynCalc`Dimension -> Automatic,
    FeynCalc`LoopMomenta -> {},
    System`OverwriteTarget -> False
};


(* ::Section:: *)
(*Expression inspection and reversible mapping data*)


(* ::Subsection:: *)
(*Supported heads and encoding*)


(* ------------------------------------------------------------------ *)
(* Section 2: expression inspection, normalization, and the restricted *)
(* data set. These definitions feed (a) reversible scalar             *)
(* abbreviations and (b) the non-executable JSON mapping file.         *)
(* Nothing here performs computation on physics expressions -- this    *)
(* layer only describes shapes and names.                              *)
(* ------------------------------------------------------------------ *)


(* One specification table drives all downstream machinery: mapping     *)
(* arities, master validation, FORM declarations, serialization, and    *)
(* reconstruction from JSON. Each entry is keyed by a stable string    *)
(* name and holds:                                                    *)
(*   "Head"      - the Wolfram head it describes (an actual symbol,    *)
(*                 e.g. Plus, used for pattern matching).              *)
(*   "Arity"     - {minimum, maximum} number of arguments accepted;    *)
(*                 Infinity as maximum means "unbounded".              *)
(*   "FORMName"  - present only for "master" heads that have a FORM    *)
(*                 counterpart; the identifier written into FORM code. *)
(*   "Arguments" - present only for masters; "Scalar" declares that    *)
(*                 every argument must be scalar, i.e. treat the       *)
(*                 whole object as opaque to FORM rather than          *)
(*                 decomposing it into Lorentz structure.              *)
(* Keys are strings, not symbols, because this table is also the       *)
(* schema of the persisted (JSON) mapping file.                        *)
(* ------------------------------------------------------------------ *)
$expressionSpecs = <|
	(* Addition: any number of terms, including 0 (Plus[] -> 0).       *)
	"Plus" -> <|"Head" -> Plus, "Arity" -> {0, Infinity}|>,
	(* Multiplication: any number of factors, including 0 (-> 1).      *)
	"Times" -> <|"Head" -> Times, "Arity" -> {0, Infinity}|>,
	(* Power[base, exponent]: exactly two arguments.                   *)
	"Power" -> <|"Head" -> Power, "Arity" -> {2, 2}|>,
	(* FeynCalc Pair[LorentzIndex, Momentum]: exactly two arguments.    *)
	"Pair" -> <|"Head" -> Pair, "Arity" -> {2, 2}|>,
	(* FeynCalc Momentum[4-vector] or Momentum[4-vector, dim].         *)
	"Momentum" -> <|"Head" -> Momentum, "Arity" -> {1, 2}|>,
	(* FeynCalc LorentzIndex[name] or LorentzIndex[name, dim].         *)
	"LorentzIndex" -> <|"Head" -> LorentzIndex, "Arity" -> {1, 2}|>,
	(* FeynCalc FeynAmpDenominator[...]: one or more denominators.      *)
	"FeynAmpDenominator" -> <|"Head" -> FeynAmpDenominator, 
	"Arity" -> {1, Infinity}|>,
	(* FeynCalc PropagatorDenominator[momentum, mass]: one or two args. *)
	"PropagatorDenominator" -> <|"Head" -> PropagatorDenominator, 
	"Arity" -> {1, 2}|>,
	(* ------------------------------------------------------------------ *)
	(* The four entries below are the "masters": scalar Passarino-Veltman  *)
	(* form factors. Each carries a "FORMName" (the identifier emitted     *)
	(* into FORM) and "Arguments" -> "Scalar" (its arguments are treated   *)
	(* as opaque scalars in version 1, so no Lorentz algebra is needed     *)
	(* on the FORM side).                                                   *)
	(* ------------------------------------------------------------------ *)
	(* A0[m^2]: one scalar argument.                                     *)
	"A0" -> <|"Head" -> A0, "FORMName" -> "cfA0", "Arity" -> {1, 1}, 
	"Arguments" -> "Scalar"|>,
	(* B0[p^2, m1^2, m2^2]: three scalar arguments.                      *)
	"B0" -> <|"Head" -> B0, "FORMName" -> "cfB0", "Arity" -> {3, 3}, 
	"Arguments" -> "Scalar"|>,
	(* C0[...]: six scalar arguments (three invariant squares plus three *)
	(* internal masses).                                                 *)
	"C0" -> <|"Head" -> C0, "FORMName" -> "cfC0", "Arity" -> {6, 6}, 
	"Arguments" -> "Scalar"|>,
	(* D0[...]: ten scalar arguments (the four-point case).             *)
	"D0" -> <|"Head" -> D0, "FORMName" -> "cfD0", "Arity" -> {10, 10}, 
	"Arguments" -> "Scalar"|>
|>;


(* Project the table down to just the head symbols, in table order:     *)
(*   {Plus, Times, Power, Pair, ..., A0, B0, C0, D0}                    *)
(* #["Head"] & looks up the "Head" value of each spec; Map applies it   *)
(* to every entry of the association. Useful as a membership test or as *)
(* the domain for pattern alternation.                                 *)
$expressionHeadByName = Map[#["Head"] &, $expressionSpecs];


(* Build the inverse lookup head -> spec name, e.g. Plus -> "Plus" and  *)
(* A0 -> "A0". Normal turns the association of rules into a list of     *)
(* Rule expressions, Reverse swaps each side, and Association reassembles*)
(* the list into an association. Note the direction: $expressionHeadByName maps name   *)
(* to head, so reversing gives head to name.                            *)
$expressionNameByHead = Association[Map[Reverse, Normal[$expressionHeadByName]]];


(* Restrict the table to the masters, i.e. the specs that declare a     *)
(* "FORMName" (A0, B0, C0, D0). KeyExistsQ[#, "FORMName"] & is the pure *)
(* predicate applied by Select to each spec association, so this is a   *)
(* view over the same data, not a copy of the inner specs.              *)
$masterSpecs = Select[$expressionSpecs, KeyExistsQ[#, "FORMName"] &];


(* Index the masters by Wolfram head, for O(1) lookup during export:    *)
(*   <|A0 -> spec, B0 -> spec, C0 -> spec, D0 -> spec|>                 *)
(* Values drops the string keys; the pure function (#["Head"] -> #) &   *)
(* pairs each spec with its own head symbol.                            *)
$masterSpecByHead = Association[(#["Head"] -> #) & /@ Values[$masterSpecs]];


(* Index the same masters by their FORM identifier, for O(1) lookup     *)
(* during reconstruction/import:                                       *)
(*   <|"cfA0" -> spec, "cfB0" -> spec, "cfC0" -> spec, "cfD0" -> spec|> *)
(* Together with $masterSpecByHead this gives the two-way bridge between   *)
(* Wolfram heads and FORM names.                                        *)
$masterSpecByFORMName = Association[(#["FORMName"] -> #) & /@ Values[$masterSpecs]];


(* arityQ[spec, n]: does argument count n fall inside the spec's        *)
(* allowed range? Part 1 of "Arity" is the minimum, part 2 the maximum, *)
(* and the chained inequality min <= n <= max is a single comparison    *)
(* that is True only when both hold. Infinity works directly as a       *)
(* maximum, so {0, Infinity} accepts any n and {2, 2} accepts only 2.   *)
(* This is a shape check only: it says nothing about the argument types.*)
arityQ[spec_Association, n_Integer] := spec["Arity"][[1]] <= n <= spec["Arity"][[2]];


(* masterArgumentsQ[spec, args]: full admissibility test for a master    *)
(* call -- the argument list must have an allowed length (delegated to  *)
(* arityQ via Length[args]) AND satisfy the declared argument class.    *)
(* Switch dispatches on spec["Arguments"]:                             *)
(*   "Scalar" -> AllTrue[args, scalarQ] requires every argument to      *)
(*               satisfy scalarQ (defined elsewhere in the package --   *)
(*               in version 1 scalar quantities stay opaque, so this is *)
(*               the gate that keeps only "scalar-safe" calls in).      *)
(*   _        -> any other/absent class is rejected outright, so a      *)
(*               spec with no recognized "Arguments" value never       *)
(*               validates.                                            *)
(* The && short-circuits, so scalarQ is never consulted when the arity  *)
(* check already failed.                                                *)
masterArgumentsQ[spec_Association, args_List] := arityQ[spec, Length[args]] && Switch[spec["Arguments"], "Scalar", AllTrue[args, scalarQ], _, False];


(* ------------------------------------------------------------------ *)
(* Single source of truth for the "kinds" of named objects the          *)
(* converter can declare. One table owns all four facets of a kind:     *)
(*   1. the name prefix used in generated FORM identifiers,             *)
(*   2. which FORM declaration class the names belong to,               *)
(*   3. what a valid Wolfram-side value of that kind looks like,        *)
(*   4. (implicitly, via the prefix regex used below) the name shape.   *)
(* Every other part of the package -- mapping files, master validation, *)
(* FORM declaration emission, serialization, reconstruction -- reads    *)
(* from here rather than hard-coding kind knowledge.                    *)
(* ------------------------------------------------------------------ *)
$kindSpecs = <|
	(* Scalar: a plain unassigned symbol stands for a scalar quantity.  *)
	(* Prefix "cfs" -> names like cfs1, cfs2. FORM class: Symbols.      *)
	(* ValidExpression uses SameQ (===), so an expression that merely    *)
	(* evaluates to a symbol (e.g. 2 - 1) is rejected -- the value must *)
	(* literally have head Symbol.                                     *)
	"Scalar" -> <|"Prefix" -> "cfs", "Declaration" -> "Symbols", 
	"ValidExpression" -> (Head[#] === Symbol &)|>,
	(* Vector: a Lorentz vector object. Prefix "cfv" -> cfv1, cfv2.     *)
	(* FORM class: Vectors (FORM's vector declaration). Validity is     *)
	(* delegated to vectorIdentityQ, defined elsewhere in the package.  *)
	"Vector" -> <|"Prefix" -> "cfv", "Declaration" -> "Vectors", 
	"ValidExpression" -> (vectorIdentityQ[#] &)|>,
	(* Index: a Lorentz index. Prefix "cfi" -> cfi1, cfi2.              *)
	(* FORM class: Indices. Same literal-symbol requirement as Scalar.  *)
	"Index" -> <|"Prefix" -> "cfi", "Declaration" -> "Indices", 
	"ValidExpression" -> (Head[#] === Symbol &)|>,
	(* Abbreviation: a symbol introduced as a shorthand for a scalar    *)
	(* subexpression. Prefix "cfa" -> cfa1, cfa2. FORM class: Symbols.  *)
	(* Validity reuses scalarQ, the same predicate that gates master    *)
	(* arguments in $expressionSpecs, keeping the two notions in sync.  *)
	"Abbreviation" -> <|"Prefix" -> "cfa", "Declaration" -> "Symbols", 
	"ValidExpression" -> (scalarQ[#] &)|>,
	(* Denominator: a scalar propagator denominator. Prefix "cfd" ->    *)
	(* cfd1, cfd2. Declared as a FORM Symbol (it will be an opaque      *)
	(* abbreviation, not a FORM function).                              *)
	(* The MatchQ pattern requires exactly ONE PropagatorDenominator    *)
	(* argument: FeynAmpDenominator[_PropagatorDenominator]. Note this  *)
	(* is narrower than the "FeynAmpDenominator" spec above, whose      *)
	(* arity is {1, Infinity} -- a multi-propagator denominator will    *)
	(* not validate here.                                              *)
	"Denominator" -> <|"Prefix" -> "cfd", "Declaration" -> "Symbols", 
		"ValidExpression" -> (validDenominatorQ[#] &)|>
|>;

(* validEntryNameQ[entry]: consistency check for one mapping-file entry. *)
(* An entry is an association that is expected to carry (at least) the   *)
(* keys "Kind" and "Name"; e.g. <|"Kind" -> "Scalar", "Name" -> "cfs7"|>. *)
(*                                                                      *)
(* Module introduces local kind/name so nothing leaks into the package  *)
(* context. Lookup[entry, key, None] returns the value or None when the *)
(* key is absent -- crucially it does NOT emit a Missing/KeyAbsent       *)
(* message, which is why it is used instead of entry["Kind"].           *)
(*                                                                      *)
(* The three conditions are checked with && , which short-circuits, so  *)
(* the expensive/lookup-dependent parts only run when earlier ones pass: *)
(*   1. KeyExistsQ[$kindSpecs, kind] -- the entry's kind must be one of  *)
(*      the five known kinds.                                            *)
(*   2. StringQ[name] -- the name must be a string, not a symbol or      *)
(*      number.                                                          *)
(*   3. StringMatchQ[name, RegularExpression[prefix <> "[0-9]+"]] -- the *)
(*      name must be the kind's prefix immediately followed by one or    *)
(*      more decimal digits. Because StringMatchQ anchors the whole      *)
(*      string, "cfs1" passes while "xcfs1", "cfs" and "cfs1a" fail.     *)
(*      The prefix is pulled from the table (so each kind enforces its   *)
(*      own namespace), and the "<>" string join happens only after      *)
(*      condition 1 has succeeded -- $kindSpecs[kind] would be an error  *)
(*      if kind were unknown, and short-circuiting prevents reaching it. *)
(* Returns True/False; it does not build or repair the entry or check    *)
(* any keys other than "Kind" and "Name".                                *)
validEntryNameQ[entry_Association] := 
	Module[{kind = Lookup[entry, "Kind", None], 
		name = Lookup[entry, "Name", None]},
			KeyExistsQ[$kindSpecs, kind] && StringQ[name] &&
				StringMatchQ[name, 
					RegularExpression[$kindSpecs[kind]["Prefix"] <> "[0-9]+"]]
	];


(* ------------------------------------------------------------------ *)
(* Encoding layer: turn a Wolfram expression into a nested,            *)
(* JSON-serializable list whose first element is a string tag.         *)
(* This is the writer half of the round trip; decoding reconstructs    *)
(* from the same tags. Note the wholesale use of encode[...] := which  *)
(* makes re-running this cell safe (no stale definitions survive).     *)
(* ------------------------------------------------------------------ *)

(* symbolName[s]: fully qualified name of a symbol, context plus short  *)
(* name, e.g. Global`x -> "Global`x" and System`Plus -> "System`Plus".  *)
(* Qualifying is what makes Symbol encoding unambiguous across           *)
(* contexts. Note this is immediate (=), so it is evaluated per call.   *)
symbolName[s_Symbol] := Context[s] <> SymbolName[s];

(* encode: integers become {"Integer", digits} with the digits produced *)
(* by intString (the explicit-sign formatter defined in the header).    *)
(* intString returns a STRING, so the value here is already JSON-ready. *)
encode[n_Integer] := {"Integer", intString[n]};

(* encode: rationals become {"Rational", numerator, denominator}, each   *)
(* part encoded independently. Recursion means the parts arrive as       *)
(* {"Integer", "..."} pairs, so the result stays uniform:               *)
(*   encode[2/3] -> {"Rational", {"Integer","2"}, {"Integer","3"}}       *)
encode[n_Rational] := {"Rational", intString[Numerator[n]], 
   intString[Denominator[n]]};

(* encode: complex numbers become {"Complex", re, im}, where re and im   *)
(* are themselves encoded values (not raw Ints). The head is written     *)
(* first, then the parts, giving a fixed positional schema that the      *)
(* decoder can rely on.                                                  *)
encode[z_Complex] := {"Complex", encode[Re[z]], encode[Im[z]]};

(* ------------------------------------------------------------------ *)
(* Polarization labels identify independent vector identities, and the *)
(* conjugation is part of that identity (phase I vs -I are different   *)
(* vectors, not a sign to be normalized away).                        *)
(* The sole supported option is encoded as DATA (a nested list), never *)
(* as a general Rule head, so the mapping file stays non-executable.   *)
(* ------------------------------------------------------------------ *)

(* A momentum label may be routed, i.e. be a sum of terms, but the      *)
(* polarization built on it is still ONE vector identity: polarization  *)
(* is not a linear function of momentum, so Polarization[p1 + p2, ...]  *)
(* must not be silently expanded into a sum of polarizations. This      *)
(* predicate defines the accepted momentum-label shapes.                *)
(*   If[p] is a Plus, List @@ p splits it into its terms; otherwise the *)
(*   label is treated as a single term. Every term must match           *)
(*   _Symbol (k), or a numeric multiple of a symbol                     *)
(*   Times[(_Integer | _Rational), _Symbol] (2 k, 3/2 k).               *)
(*   So p1 + 2 p2 is a legal label while p1 . p2 or a bare number is    *)
(*   not -- the check is purely syntactic on the label, not physical.   *)
physicalMomentumLabelQ[p_] :=
	AllTrue[If[Head[p] === Plus, List @@ p, {p}],
		MatchQ[#, _Symbol | Times[(_Integer | _Rational), _Symbol]] &];

(* polarizationQ[x]: the full validity test for one Polarization object. *)
(* The pattern binds p (momentum label), phase (the conjugation), and   *)
(* opts___ (zero or more trailing option arguments).                    *)
(* Three conditions, all required:                                      *)
(*   1. the momentum label passes physicalMomentumLabelQ;               *)
(*   2. the phase is literally I or -I;                                 *)
(*   3. the option sequence, wrapped as {opts}, is one of the three     *)
(*      whitelisted forms -- {}, {Transversality -> True}, or           *)
(*      {Transversality -> False}.                                      *)
(* Condition 3 is an exact, closed whitelist: any other option, a       *)
(* duplicate, or a different spelling makes the object invalid.         *)
polarizationQ[Polarization[p_, phase_, opts___]] :=
	physicalMomentumLabelQ[p] && MemberQ[{I, -I}, phase] && 
		MemberQ[{{}, {Transversality -> True}, {Transversality -> False}}, {opts}];

(* Catch-all: anything that is not a syntactically well-formed          *)
(* Polarization[...] is simply not a polarization. Without this,        *)
(* polarizationQ would stay unevaluated on other inputs and be neither  *)
(* True nor False, which would break the tests that use it as a         *)
(* boolean. This makes the predicate total.                             *)
polarizationQ[_] := False;

(* vectorIdentityQ[x]: the definition of a valid "Vector" value, reused *)
(* as the ValidExpression predicate in $kindSpecs. A vector identity is *)
(* either a plain symbol standing for the vector, or a valid            *)
(* Polarization object. Note this is what makes polarization           *)
(* expressions acceptable anywhere a Vector kind is expected.          *)
vectorIdentityQ[x_] := Head[x] === Symbol || polarizationQ[x];

(* encode: the Polarization case. If the object passes                 *)
(* polarizationQ, emit a tagged, reversible record:                    *)
(*   {"Polarization", encode[momentum], encode[phase], ...option...}   *)
(* The option is attached only when present (Length[x] === 3 means a   *)
(* third argument exists). It is written as DATA, the nested pair      *)
(* {"Transversality", "True"|"False"}, with TrueQ normalizing the      *)
(* value to a string rather than embedding a Rule or a Wolfram boolean.*)
(* If the object is NOT valid, the branch does not throw directly: it  *)
(* builds the throwFailure[...] call, which is a Throw, so the abort happens   *)
(* when the surrounding evaluation reaches that expression.            *)
encode[x_Polarization] := If[polarizationQ[x],
	Join[{"Polarization", encode[x[[1]]], encode[x[[2]]]},
		If[ Length[x] === 3, {{"Transversality", 
			If[TrueQ[x[[3, 2]]], "True", "False"]}}, {}]],
		throwFailure["UnsupportedMomentum", 
	"Unsupported polarization vector identity."]];

(* encode: a bare symbol becomes {"Symbol", "Context`name"}, using the   *)
(* fully qualified name from symbolName. This is how scalar            *)
(* abbreviations, vector names and index names travel into the file.   *)
(* Ordering matters: this clause must come before the generic catch-all *)
(* so symbols are recognized rather than treated as unknown heads.     *)
encode[s_Symbol] := {"Symbol", symbolName[s]};

(* encode: the generic case, covering every remaining expression whose  *)
(* head is a known operator. Three steps:                              *)
(*   1. look the head up in $expressionNameByHead (name -> head, so reversed) to  *)
(*      get its stable string name; an unregistered head becomes       *)
(*      Missing["Unknown"] rather than emitting a Lookup error.        *)
(*   2. If it is Missing, abort with a structured Failure that carries *)
(*      HoldForm[x] (so the offending expression is recorded unevaluated*)
(*      and Head[x] as data), via the throwFailure[...] machinery.             *)
(*   3. Otherwise recurse into every argument with encode /@ ... and   *)
(*      put the operator name in front. So Plus[a, b] becomes           *)
(*      {"Plus", encode[a], encode[b]} -- the tag names the head and    *)
(*      the arguments keep their order for exact reconstruction.       *)
(* Because List @@ x discards the head, the head name in the output is  *)
(* the only record of it, which is why unknown heads must fail loudly   *)
(* instead of being flattened.                                          *)
encode[x_] := 
	Module[{name = Lookup[$expressionNameByHead, Head[x], Missing["Unknown"]]},
		If[MissingQ[name], 
			throwFailure["UnsupportedHead", 
				"Unsupported expression head.", <|"Expression" -> HoldForm[x], "Head" -> Head[x]|>]];
			Prepend[encode /@ (List @@ x), name]
   ];


(* ::Subsection:: *)
(*Restricted decoding and symbol validation*)


(* integerData[s]: the decoder counterpart of encode[n_Integer], i.e.    *)
(* the function that turns a stored integer string back into a real     *)
(* Wolfram integer. Used while reading the non-executable mapping file. *)
(*                                                                     *)
(* The first clause is specialized to String input, which is what the   *)
(* file format holds (encode writes intString output, and intString     *)
(* always returns a string). The pattern match on s_String means this   *)
(* clause fires only for strings; it is therefore a type guard as well  *)
(* as a validity check.                                                *)
(*                                                                     *)
(* The RegularExpression["-?[0-9]+"] accepts an optional leading minus  *)
(* followed by one or more decimal digits -- exactly the shape         *)
(* intString produces. StringMatchQ anchors the whole string, so       *)
(* "12abc" and "1 2" are rejected, not partially matched.              *)
(*                                                                     *)
(* If the shape is valid, the sign is handled explicitly:              *)
(*   StringStartsQ[s, "-"] tests for the minus.                        *)
(*   If present: FromDigits[StringDrop[s, 1]] parses the digits AFTER  *)
(*     the sign (they are unsigned after the drop, so FromDigits yields *)
(*     a non-negative integer), then the leading - negates it.         *)
(*   If absent: FromDigits[s] parses the string directly.              *)
(* So "-7" -> -7 and "7" -> 7. The two-argument FromDigits accepts a   *)
(* digit string (any base as second argument, default 10).             *)
(*                                                                     *)
(* This mirrors intString in the header exactly, which is what keeps   *)
(* the encode/decode pair round-trippable: the writer emits            *)
(* "-" <> digits and this reader strips the sign and re-applies it.    *)
integerData[s_String] := If[StringMatchQ[s, RegularExpression["-?[0-9]+"]],
	If[StringStartsQ[s, "-"], -FromDigits[StringDrop[s, 1]], 
		FromDigits[s]],
		throwFailure["InvalidMapping", "Invalid integer in mapping."]];

(* Catch-all clause for non-string input: if the file contains a JSON    *)
(* number (parsed as a Wolfram Integer or Real) or any other non-string  *)
(* value where an integer string was expected, this aborts with the same *)
(* "InvalidMapping" failure. Note it does NOT coerce or repair the       *)
(* value -- integerData[3] fails rather than returning 3, so the         *)
(* round-trip is strict about representation, not just about value.      *)
(* Both clauses call the throwFailure[...] helper from the header, so both abort *)
(* by throwing to the private $failureTag rather than returning a        *)
(* sentinel value.                                                       *)
integerData[_] := throwFailure["InvalidMapping", "Expected an integer string."];


(* ------------------------------------------------------------------ *)
(* Decoding layer: the inverse of encode. Rebuilds Wolfram expressions *)
(* from the tagged nested lists stored in the mapping file.            *)
(*                                                                     *)
(* Threat model, stated explicitly in the header comment:              *)
(*   * Only symbol leaves may create a symbol.                         *)
(*   * Operator heads are never taken from the file -- they are        *)
(*     chosen by lookup in $expressionHeadByName, so a string in the file can never   *)
(*     become a head or be evaluated as Wolfram Language code.         *)
(*   * That is NOT a sandbox. Restored symbols and whitelisted heads   *)
(*     still undergo normal kernel evaluation, and this decoder does   *)
(*     not isolate pre-existing symbol definitions. So decoding an     *)
(*     expression that references a symbol with existing DownValues/   *)
(*     UpValues can still trigger that code.                           *)
(* ------------------------------------------------------------------ *)

(* Require an explicit, nonempty context and name. Symbol itself validates
   Mathematica's complete symbol alphabet below; Unicode letter categories
   exclude valid letter-like characters such as EmptySet and DottedSquare.
   Symbol accepts a name, never source expressions. Do not use ToExpression. *)
qualifiedSymbolNameQ[s_String] := StringContainsQ[s, "`"] &&
  !StringStartsQ[s, "`"] && !StringEndsQ[s, "`"] &&
  !StringContainsQ[s, "``"];
qualifiedSymbolNameQ[_] := False;

(* decode: integer leaf, exactly the reverse of encode[n_Integer].      *)
(* Delegates validation and conversion to integerData, which rejects    *)
(* anything that is not a canonical integer string.                    *)
decode[{"Integer", s_String}] := integerData[s];

(* decode: rational leaf. The denominator is decoded FIRST and checked  *)
(* for zero before any division happens -- 1/0 must surface as a        *)
(* structured "InvalidMapping" failure rather than as a Power::infy     *)
(* message and ComplexInfinity. Only the numerator is decoded by        *)
(* integerData; the denominator value is reused for the division, so    *)
(* integerData[b] is evaluated exactly once.                            *)
decode[{"Rational", a_String, b_String}] := 
	Module[{den = integerData[b]},
		If[den == 0, throwFailure["InvalidMapping", "Zero rational denominator."]];
		integerData[a]/den
	];

(* decode: complex leaf. The parts are decoded first, then re-validated *)
(* with MatchQ to be sure they are exact numbers (Integer or Rational). *)
(* This guards against a hand-edited file smuggling a symbol or a real  *)
(* into a coefficient position -- the Complex representation is only    *)
(* ever meant to carry exact parts. Reconstruction is re + I im, which *)
(* evaluates to the canonical Complex number (and would silently        *)
(* combine/renormalize parts, e.g. re == 0 collapsing to a pure         *)
(* imaginary).                                                          *)
decode[{"Complex", a_, b_}] := Module[{re = decode[a], im = decode[b]},
	If[! 
		MatchQ[{re, im}, {(_Integer | _Rational), (_Integer | _Rational)}],
		throwFailure["InvalidMapping", 
		"Complex coefficients must be exact numbers."]];
		re + I im
	];

(* decode: symbol leaf. This is the only place a new symbol can come     *)
(* into existence. Symbol[s] treats s purely as a NAME, so the lookup   *)
(* or creation of the symbol does not evaluate any code contained in    *)
(* the string; nothing from the file is ever run as WL. The readability *)
(* test runs first and any failure aborts with "InvalidMapping" rather *)
(* than creating an odd or unqualified symbol.                          *)
decode[{"Symbol", s_String}] := If[qualifiedSymbolNameQ[s],
  Quiet[Check[Symbol[s],
    throwFailure["InvalidMapping", "Invalid fully qualified symbol name."]]],
  throwFailure["InvalidMapping", "Invalid fully qualified symbol name."]];

(* decode: Polarization leaf. The three encoded parts -- momentum       *)
(* label, phase (conjugation), and an optional option record -- are     *)
(* decoded independently, then the same three conditions enforced by    *)
(* polarizationQ on the encoding side are re-checked on the way in:     *)
(* the momentum label must be physical, the phase must be I or -I, and  *)
(* optionData must be one of exactly {}, {{"Transversality","True"}},   *)
(* or {{"Transversality","False"}}. Note this is the DATA form (nested  *)
(* string pairs), not Wolfram Rules -- the file never contains Rule      *)
(* heads, and the Rule is created here, on our side of the boundary.    *)
(* opts___ captures the rest of the list, so a 3-element encoded        *)
(* polarization yields optionData === {} and a 4-element one yields a   *)
(* one-element list.                                                    *)
(* ------------------------------------------------------------------ *)
decode[{"Polarization", p_, phase_, opts___}] := 
	Module[{momentum = decode[p], label = decode[phase], optionData = {opts}},
		If[! physicalMomentumLabelQ[momentum] || ! 
			MemberQ[{I, -I}, label] || ! MemberQ[{{}, {{"Transversality", "True"}}, {{"Transversality", "False"}}}, optionData],
			throwFailure["InvalidMapping", "Invalid polarization vector identity."]];
		(* Validate, then rebuild: with no option data the short          *)
		(* Polarization[momentum, label] form is restored; otherwise the   *)
		(* Rule is reconstructed and the string "True"/"False" is mapped   *)
		(* back to the boolean True/False. optionData[[1, 2]] is the       *)
		(* second part of the inner pair (the stored string).              *)
		If[optionData === {}, Polarization[momentum, label],
			Polarization[momentum, label, 
			Transversality -> (optionData[[1, 2]] === "True")]]
		];

(* decode: the generic operator case, e.g.                                  *)
(*   {"Plus", arg1, ...} or {"Times", arg1, ...}                            *)
(* The Condition (/;) makes this clause applicable ONLY when the tag is a    *)
(* registered operator name in $expressionHeadByName. That is the whitelist that keeps an   *)
(* arbitrary string from ever becoming a head: a name absent from $expressionHeadByName     *)
(* fails the condition and falls through to the final decode[_] clause.      *)
(* {args} collects the trailing arguments into a list; h = $expressionHeadByName[name] is   *)
(* the ACTUAL head symbol (e.g. "Plus" -> Plus), so the file only ever       *)
(* supplies the name, never the head.                                        *)
(* Arity is then re-validated against $expressionSpecs (the same table used  *)
(* on the encoding side), so a mapping file claiming Plus[a,b,c] with a      *)
(* narrowed arity, or Power[a,b,c], is rejected rather than silently built.  *)
(* Finally the arguments are decoded first and the reconstructed expression  *)
(* is formed as h @@ (decode /@ a) -- which, being ordinary kernel          *)
(* evaluation, evaluates: Plus[2,3] yields 5, and any DownValues/UpValues    *)
(* on the involved symbols or heads do run. That is the documented,         *)
(* intentional consequence of "not an isolation layer".                      *)
decode[{name_String, args___}] /; KeyExistsQ[$expressionHeadByName, name] := 
	Module[{a = {args}, h = $expressionHeadByName[name]},
		If[! arityQ[$expressionSpecs[name], Length[a]],
			throwFailure["InvalidMapping", "Invalid expression arity in mapping."]];
			h @@ (decode /@ a)
		];

(* Final catch-all: any tagged list that matched nothing above (unknown  *)
(* tag, wrong shape, or an operator name not in $expressionHeadByName) is rejected as   *)
(* unsupported mapping data. Every decode path therefore ends in either  *)
(* a reconstructed expression or a thrown Failure -- there is no partial *)
(* or silently malformed result.                                        *)
decode[_] := throwFailure["InvalidMapping", "Unsupported mapping data."];


(* ::Subsection:: *)
(*Lorentz dimensions*)


(* ------------------------------------------------------------------ *)
(* Dimension inference. Note the two "space" readers deliberately do  *)
(* NOT look inside expressions: they read only the optional second     *)
(* argument of a Momentum or LorentzIndex object. Nothing is expanded, *)
(* no indices are contracted, no structural algebra is attempted --    *)
(* the dimension is metadata attached to those heads.                  *)
(* ------------------------------------------------------------------ *)


(* space[Momentum[p]] -> 4, space[Momentum[p, d]] -> d.                 *)
(* The pattern d_ : 4 is an optional pattern with a default, so the     *)
(* one-argument form binds d to 4 and the two-argument form binds the   *)
(* actual dimension. The first argument is ignored (_), so this works   *)
(* for routed/combined momenta too (Momentum[p1 + p2, d] still yields  *)
(* d). Note the pattern does NOT check that d is numeric or symbolic -- *)
(* that validation happens later, in chooseDimension.                   *)
space[Momentum[_, d_ : 4]] := d;


(* Same convention for LorentzIndex[index] -> 4 and                   *)
(* LorentzIndex[index, d] -> d. Keeping both readers identical is what *)
(* lets a single scan collect dimensions from either head.            *)
space[LorentzIndex[_, d_ : 4]] := d;


(* chooseDimension[x, requested]: decide the single spacetime          *)
(* dimension to use for converting expression x.                       *)
(*   requested is normally the Dimension option value from $options    *)
(*   (Automatic means "infer it"); it may also be an explicit symbol   *)
(*   (d, D) or integer.                                                *)
chooseDimension[x_, requested_] := Module[{spaces, dim},
	(* Collect the dimension of every Momentum/LorentzIndex that       *)
	(* occurs ANYWHERE in x. Cases with level spec {0, Infinity}       *)
	(* searches all levels including the head position at level 0, and *)
	(* the pattern a : (_Momentum | _LorentzIndex) :> space[a] feeds   *)
	(* each match to the readers above. DeleteDuplicates collapses     *)
	(* repeated dimensions, so the order is first-occurrence order.    *)
	(* Crucially nothing is expanded: this is a structural scan, not   *)
	(* evaluation of the physics content.                              *)
	spaces = 
		DeleteDuplicates[
			Cases[x, a : (_Momentum | _LorentzIndex) :> space[a], {0, 
			Infinity}]];
	(* More than one distinct dimension in one expression is not       *)
	(* something the converter will guess about (e.g. 4-vectors mixed  *)
	(* with d-dimensional ones), so it aborts with a structured        *)
	(* failure carrying the offending dimensions as data.              *)
	If[Length[spaces] > 1, 
		throwFailure["MixedDimensions", 
		"Mixed Lorentz spaces are not supported.", <|
		"Dimensions" -> spaces|>]];
	(* Three cases:                                                    *)
	(*   requested === Automatic and nothing found -> default to the   *)
	(*     symbolic dimension D.                                       *)
	(*   requested === Automatic with exactly one found -> that one.   *)
	(*   requested explicit -> use it verbatim, and the following      *)
	(*     checks decide whether it is consistent with the input.      *)
	dim =  If[requested === Automatic, If[spaces === {}, D, First[spaces]], requested];
	(* Validate the dimension value itself. Accepted: a Symbol (d, D)  *)
	(* or an Integer >= 2. Rejected: anything else (strings, reals,    *)
	(* lists, ...), integers below 2, and the imaginary unit I (which  *)
	(* is a Symbol, hence needs the extra explicit exclusion -- it is  *)
	(* FeynCalc's common symbol for the dimension, but it is not a     *)
	(* sensible spacetime dimension here).                             *)
	(* The condition is written so that a non-symbol, non-integer      *)
	(* value short-circuits into the failure before IntegerQ is even   *)
	(* meaningful.                                                     *)
	If[!MatchQ[dim, _Symbol | _Integer] || (IntegerQ[dim] && dim < 2) || dim === I,
		throwFailure["UnsupportedDimension", 
		"Use a symbolic dimension or an integer dimension of at least 2."]];
	(* Final consistency check: if the input carried dimensions and the *)
	(* chosen one differs from them, refuse. There is no implicit       *)
	(* dimension conversion and no attempt to reconcile, e.g. a         *)
	(* Momentum[p, 4] with Dimension -> d is an error, not something   *)
	(* that gets reinterpreted. SameQ (=!=) is used so that e.g. 4 and  *)
	(* 4. are not considered equal.                                     *)
	If[spaces =!= {} && First[spaces] =!= dim,
		throwFailure["DimensionMismatch", "The requested dimension differs from the input; no implicit dimension conversion is performed."]];
	(* Return the validated dimension; this is the value the rest of   *)
	(* the converter threads through declarations, FORM code and the   *)
	(* reconstruction path.                                            *)
	dim
];


(* ::Subsection:: *)
(*Supported momenta and scalar expressions*)


(* linearMomentumQ[Momentum[v, ...]]: is the momentum argument a linear  *)
(* combination of vector identities with exact rational coefficients?    *)
(*                                                                     *)
(* The first argument v is what matters; the trailing pattern ___       *)
(* swallows any remaining arguments (the optional dimension) so that    *)
(* Momentum[k], Momentum[k, d] and routed momenta are all classified    *)
(* the same way. This is the label-shape test used by scalarQ below.    *)
(*                                                                     *)
(* If v is a Plus, List @@ v splits it into its terms; otherwise v is   *)
(* treated as a single term. Each term must be either                *)
(*   * a vector identity (a bare symbol, or a Polarization object), or  *)
(*   * an exact rational multiple of one: Times[n, vec] with n an       *)
(*     Integer or Rational and vec again passing vectorIdentityQ.       *)
(* Note ?vectorIdentityQ must hold, so a numeric multiple of something  *)
(* that is not a vector identity (say Times[2, Pair[...]]) is refused.  *)
(* The predicate is structural: it never expands the sum or contracts   *)
(* anything.                                                            *)
linearMomentumQ[Momentum[v_, ___]] := 
	AllTrue[If[Head[v] === Plus, List @@ v, {v}],
		(vectorIdentityQ[#] || MatchQ[#, Times[(_Integer | _Rational), _?vectorIdentityQ]]) &];

(* Catch-all so the predicate is total: anything that is not a Momentum *)
(* object is simply not a linear momentum, rather than staying           *)
(* unevaluated and breaking the boolean logic in scalarQ.                *)
linearMomentumQ[_] := False;

(* ------------------------------------------------------------------ *)
(* scalarQ[x]: the central "may this be treated as an opaque scalar?"  *)
(* classifier. It is the predicate that gates master arguments in     *)
(* $expressionSpecs, gates the "Abbreviation" kind, and defines what  *)
(* the converter is willing to hand to FORM as a scalar. It is a      *)
(* deliberate whitelist of exact, structurally simple forms --         *)
(* inexact, symbolic-in-an-unhandled-way, and unevaluated constructs   *)
(* are rejected. Which[] tests the rules in order and returns the      *)
(* first True/False branch, so the ordering below IS the priority.     *)
(* ------------------------------------------------------------------ *)
scalarQ[x_] := Which[
	(* Exact numbers first: integers and rationals are always scalars. *)
	(* This also covers exact integers that arrive as e.g. 2 from a     *)
	(* collapsed expression.                                            *)
	MatchQ[x, _Integer | _Rational], True,
	(* Complex: a scalar only if it is EXACT, i.e. both real and        *)
	(* imaginary parts are Integer or Rational. So 1 + I and 2/3 - I/2  *)
	(* pass while 1. + I and a + I fail -- machine reals and symbolic   *)
	(* parts are not accepted.                                          *)
	Head[x] === Complex, 
	MatchQ[{Re[x], Im[x]}, {(_Integer | _Rational), (_Integer | _Rational)}],
	(* Symbols: a bare symbol is a scalar UNLESS it is one of the       *)
	(* indeterminate/infinite quantities. Listing them explicitly is    *)
	(* what keeps Indeterminate, Infinity and ComplexInfinity from      *)
	(* being emitted as if they were ordinary scalar names. Note that   *)
	(* I and Pi/E do pass this test; they are symbols, not numbers, and *)
	(* will travel as names.                                           *)
	Head[x] === Symbol, ! MemberQ[{Indeterminate, Infinity, ComplexInfinity}, x],
	(* Lorentz contractions: Pair[Momentum[...], Momentum[...]] is a    *)
	(* scalar provided both momentum arguments are linear momenta in    *)
	(* the sense of linearMomentumQ. This is exactly the scalar product *)
	(* that version 1 keeps opaque rather than expanding. Note the      *)
	(* pattern requires the literal Momentum head on both sides, so a   *)
	(* Pair involving an unrecognized vector argument fails here.       *)
	MatchQ[x, Pair[_Momentum, _Momentum]], AllTrue[List @@ x, linearMomentumQ],
	(* Compound exact expressions: a Plus (sum of scalars) or Times     *)
	(* (product of scalars, including the coefficient times scalar      *)
	(* case) is scalar if ALL of its arguments are scalars. This is the *)
	(* recursion that walks sums and products to their leaves, and it   *)
	(* is also what lets e.g. Times[2, Pair[...]] be handled.           *)
	MemberQ[{Plus, Times}, Head[x]], AllTrue[List @@ x, scalarQ],
	(* Powers: base must be scalar and the exponent must be an exact    *)
	(* Integer or Rational. So a^2 and a^(1/2) pass, while a^b, a^2.5   *)
	(* and a^(1 + I) do not -- symbolic exponents are deliberately out  *)
	(* of scope for version 1.                                          *)
	Head[x] === Power, scalarQ[x[[1]]] && MatchQ[x[[2]], _Integer | _Rational],
	(* Master integrals: if the head is one of the registered masters   *)
	(* (A0, B0, C0, D0, via $masterSpecByHead from the spec table),        *)
	(* delegate to masterArgumentsQ, which checks the arity range AND   *)
	(* that every argument is itself scalar. This is where scalarQ and  *)
	(* the $expressionSpecs table become mutually recursive: masters    *)
	(* are scalars when their arguments are scalars.                    *)
	KeyExistsQ[$masterSpecByHead, Head[x]], masterArgumentsQ[$masterSpecByHead[Head[x]], List @@ x],
	(* Default: anything not covered -- inexact reals, strings, lists,  *)
	(* graphics, unknown heads, other FeynCalc objects -- is NOT a      *)
	(* scalar. Being a total predicate returning False (rather than     *)
	(* staying unevaluated) is what makes it usable inside AllTrue and  *)
	(* as a validation gate that must produce a clear failure.          *)
	True, False
];


(* Shared propagator vocabulary. Routing may also contain sums of already
   dimension-tagged Momentum objects, as produced by FeynCalc evaluation. *)
propagatorRoutingQ[m_Momentum] := linearMomentumQ[m] && FreeQ[m, _Polarization];
propagatorRoutingQ[x_Plus] := AllTrue[List @@ x, propagatorRoutingQ];
propagatorRoutingQ[Times[c : (_Integer | _Rational), m_Momentum]] := propagatorRoutingQ[m];
propagatorRoutingQ[_] := False;
validDenominatorQ[FeynAmpDenominator[PropagatorDenominator[mom_, mass_:0]]] :=
  propagatorRoutingQ[mom] && scalarQ[mass];
validDenominatorQ[_] := False;

(* Check authoritative expression dimensions, not descriptive metadata. *)
consistentMappedDimensionQ[x_, dim_] := AllTrue[
  Cases[x, a : (_Momentum | _LorentzIndex) :> space[a], {0, Infinity}],
  SameQ[#, dim] &];


(* ::Section:: *)
(*Export: symbol mapping, serialization and FORM program generation*)


(* ::Subsection:: *)
(*Build conversion data in memory*)


(* ------------------------------------------------------------------ *)
(* Staging / planning. The ordering machinery exists because FORM      *)
(* identifier and macro numbering must follow the ORIGINAL traversal   *)
(* order, so planning happens only AFTER serialization. What the       *)
(* planner may change is only the order in which already-emitted       *)
(* factor groups are multiplied together.                              *)
(* Consistency requirement: every stage must have the same index       *)
(* count structure. Ambiguous sums, or an index occurring more than    *)
(* twice overall, mean the planner is not applicable -- in that case   *)
(* the old (original) order is kept.                                   *)
(* ------------------------------------------------------------------ *)


(* stageIndexSignatures[stages]: for each stage expression, produce an  *)
(* association index -> number of occurrences, e.g.                    *)
(* <|LorentzIndex[mu] -> 2, LorentzIndex[nu] -> 1|>.                  *)
(* All helper definitions are local to this Module, so nothing leaks   *)
(* and the recursion is self-contained.                                *)
stageIndexSignatures[stages_List] := Module[{counts, signatures},
	(* Leaf case: a Lorentz contraction. Counts all LorentzIndex        *)
	(* objects at any depth, then KeySort gives a canonical key order  *)
	(* so signatures from different stages are directly comparable     *)
	(* with SameQ. The definition memoizes via counts[p] = ... so the  *)
	(* same subexpression is only analysed once.                      *)
	counts[p_Pair] := 
		counts[p] = KeySort[Counts[Cases[p, _LorentzIndex, Infinity]]];
		(* Sum: every term must have the SAME index signature, otherwise   *)
		(* the sum is index-ambiguous and the whole planner is abandoned   *)
		(* ($Failed). If they all agree, the signature of any one term is  *)
		(* the signature of the sum.                                       *)
		counts[x_Plus] := Module[{parts = counts /@ (List @@ x)},
		
		If[MemberQ[parts, $Failed] || ! SameQ @@ parts, $Failed, First[parts]]];
		(* Product: index counts add. Merge combines the per-factor        *)
		(* associations by summing values for shared keys, so a mu that    *)
		(* appears once in each of two factors ends up counted twice.      *)
		counts[x_Times] := Module[{parts = counts /@ (List @@ x)},
			If[MemberQ[parts, $Failed], $Failed, KeySort[Merge[parts, Total]]]];
		(* Non-negative integer power: counts scale linearly with the      *)
		(* exponent. Note the guard n >= 0 and the Integer requirement: a  *)
		(* negative or symbolic power falls through to counts[_] below and *)
		(* is treated as carrying no indices.                              *)
		counts[Power[x_, n_Integer]] /; n >= 0 := 
			Module[{part = counts[x]},If[part === $Failed, $Failed, Map[n # &, part]]];
		(* Fallback for anything that is not a tensor structure: no        *)
		(* indices. This makes the helper total, which the planner relies  *)
		(* on when a stage is a pure coefficient.                          *)
		counts[_] := <||>;
		signatures = counts /@ stages;
		(* Reject the whole plan if any stage was ambiguous, or if any     *)
		(* index occurs MORE THAN TWICE across the total set of stages --   *)
		(* that is exactly the situation where an index cannot be          *)
		(* contracted unambiguously, so reordering factors could change    *)
		(* the result.                                                     *)
		If[MemberQ[signatures, $Failed] ||AnyTrue[Values[Merge[signatures, Total]], # > 2 &], $Failed, signatures]
];

(* connectedStageOrder: decide the order in which stage factors are     *)
(* multiplied. Two-argument form: the caller supplies precomputed       *)
(* signatures, or $Failed.                                             *)
connectedStageOrder[stages_List] := connectedStageOrder[stages,
	If[FreeQ[stages, _LorentzIndex], 
	ConstantArray[<||>, Length[stages]], stageIndexSignatures[stages]]];

(* Single-argument form: if a LorentzIndex appears anywhere, compute    *)
(* signatures; if the expression has no indices at all there is nothing *)
(* to order by, so use one empty signature per stage (which will hit    *)
(* the "all free sets empty" early return below and preserve order).    *)
connectedStageOrder[stages_List, signatures_] := Module[
	{free, sizes, remaining, order = {}, active = {}, next},
	(* If signature computation failed, do not attempt to be clever:   *)
	(* keep the original order.                                        *)
	If[signatures === $Failed, Return[Range[Length[stages]]]];
	(* free[[i]] = indices occurring exactly ONCE in stage i. These    *)
	(* are the indices that are still open and could be contracted by  *)
	(* joining stage i with another stage. (Despite the name, this is  *)
	(* the set of singly-occurring indices, not "free indices" in the  *)
	(* output sense.)                                                  *)
	free = Keys[Select[#, # === 1 &]] & /@ signatures;
	(* Nothing is shared with anything: every stage's indices occur    *)
	(* once, so no ordering can create a contraction. Preserve the     *)
	(* original order.                                                 *)
	If[AllTrue[free, # === {} &], Return[Range[Length[stages]]]];
	(* Raw size of each stage, used as a tie-breaker below. Their      *)
	(* evaluation cost is not modelled; this is a heuristic.           *)
	sizes = LeafCount /@ stages;
	remaining = Range[Length[stages]];
	(* Greedy loop: repeatedly pick the best remaining stage.          *)
	While[remaining =!= {},
		(* Prefer shared open indices, then tensors,          *)
		(* smaller factors and the                           *)
		(* original position. This is a deterministic heuristic,          *)
		(* not a cost model.                                *)
		next = First[SortBy[remaining,
			{-Length[Intersection[active, free[[#]]]],
			If[free[[#]] === {}, 1, 0], sizes[[#]], #} &]];
		AppendTo[order, next];
		(* Update the active index set by SYMMETRIC DIFFERENCE:         *)
		(* an index shared with an already-processed stage cancels       *)
		(* (it just got contracted); an index that was not active        *)
		(* becomes active (it is now open). Union minus Intersection is  *)
		(* exactly Complement[Union[...], Intersection[...]].            *)
		active = Complement[Union[active, free[[next]]], Intersection[active, free[[next]]]]; remaining = DeleteCases[remaining, next]];
	order
];

(* Minimum Wolfram leaf count that enables FORM stage preparation. This
   is a dispatch heuristic only and never changes accepted syntax or results;
   retune with representative FORM stage benchmarks and equivalence tests. *)
$stagePreparationMinimumLeaves = 1024;

(* ------------------------------------------------------------------ *)
(* buildExportData: the main export driver. Takes the (FeynCalc)       *)
(* expression, the Dimension option value, and the list of loop        *)
(* momentum symbols, and returns one association containing everything *)
(* needed downstream: the FORM program pieces plus the mapping payload. *)
(*                                                                     *)
(* Design note from the header comment: all mutable state (entries,    *)
(* registry, counters, macros) lives inside this Module and belongs to *)
(* the traversal. Rendering and file writing only CONSUME the returned *)
(* association; they cannot mutate it. That is what makes export a     *)
(* pure function of its inputs.                                        *)
(* ------------------------------------------------------------------ *)
buildExportData[expression_, requestedDimension_, loops_] := Module[
	{expr, dim, entries = {}, registry = <||>, counters = <||>, 
	macros = {},
	register, scalar, vector, index, pair, denominator, emit, 
	abbreviation, makeMacro,body, dimensionName, payload, factorExpressions, factorTexts, 
	stageEnds, stageRanges,
	stageTexts, stageExpressions, stageSignatures, stageOrder,
	preparations = {}, multiplications = {}},
	(* Validate LoopMomenta: must be a list, every element a symbol,   *)
	(* and no duplicates. Anything else aborts before any work is      *)
	(* done.                                                           *)
	If[! ListQ[loops] || ! AllTrue[loops, MatchQ[#, _Symbol] &] || ! DuplicateFreeQ[loops],
		throwFailure["InvalidLoopMomenta", 
		"LoopMomenta must be a list of distinct momentum symbols."]];
	(* FCI puts the expression into FeynCalc's internal representation *)
	(* (canonical ordering/notation) before anything else inspects it. *)
	expr = FCI[expression];
	(* Resolve/infer the dimension now, so it is fixed before names   *)
	(* and entries are allocated.                                      *)
	dim = chooseDimension[expr, requestedDimension];
	(* register[kind, value, extra]: THE allocator and the only place  *)
	(* identifiers are minted and mapping entries appended.            *)
	(* Returns the existing name if this exact (kind, value) was seen  *)
	(* before, so identical objects map to one identifier.             *)
	register[kind_, value_, extra_ : <||>] := With[{key = HoldComplete[kind, value]},
	(* HoldComplete preserves the structural key without evaluating its contents again; *)
	(* kind distinguishes the same value in different roles. Only insertion allocates   *)
	(* locals and serializes the mapped expression. *)
		If[KeyExistsQ[registry, key], registry[key],
			Module[{name, number, prefix},
			(* Numbering is per kind, so scalars, vectors, indices  *)
			(* and denominators each get their own 1, 2, 3, ...      *)
			(* sequence.                                            *)
			number = Lookup[counters, kind, 0] + 1;
			AssociateTo[counters, kind -> number];
			prefix = $kindSpecs[kind]["Prefix"];
			name = prefix <> intString[number];
			AssociateTo[registry, key -> name];
			(* The mapping entry records kind, name and the encoded *)
			(* expression, merged with any kind-specific extras      *)
			(* (denominators add Momentum/Mass/Power/Dimension/      *)
			(* Prescription). AppendTo preserves traversal order,    *)
			(* which is what keeps numbering and the mapping file    *)
			(* aligned.                                              *)
				AppendTo[entries, Join[<|"Name" -> name, "Kind" -> kind, "Expression" -> encode[value]|>, extra]];
				name
			]
		]
	];
	(* makeMacro[s]: wrap a FORM subexpression in a #define and return  *)
	(* the reference to it. The counter is Length[macros]+1, so macros  *)
	(* are numbered in the order they are created during emission.      *)
	(* Note: deliberately NOT memoized -- each top-level sum allocates  *)
	(* its own macro, keeping ordered macros in traversal order.       *)
	makeMacro[s_] := Module[{name = "CFCF" <> intString[Length[macros] + 1]},
		AppendTo[macros, "#define " <> name <> " \"(" <> s <> ")\""];
		"`" <> name <> "'"];
	(* scalar[s]: emit a scalar symbol. The imaginary unit is special-  *)
	(* cased to FORM's predefined i_ rather than becoming an exported   *)
	(* identifier.                                                     *)
	scalar[s_Symbol] := If[s === I, "i_", register["Scalar", s]];
	(* index: a LorentzIndex must have a symbol as its first argument; *)
	(* anything else is refused rather than silently named.            *)
	index[LorentzIndex[i_Symbol, ___]] := register["Index", i];
	index[_] := throwFailure["UnsupportedIndex", "Lorentz indices must be symbols."];
	(* vector: decompose a momentum into {coefficient, identifier}     *)
	(* pairs, i.e. the linear combination is returned as a list of     *)
	(* term/coefficient records. Polarization labels stay atomic       *)
	(* vector identities even when their momentum is routed.           *)
	vector[Momentum[v_, ___]] := Module[{terms},
		terms = If[Head[v] === Plus, List @@ v, {v}];
		Map[
			Function[term,
				Module[{factors, momenta, coefficients},
				(* A bare vector identity: coefficient 1.            *)
				If[vectorIdentityQ[term],
				{1, register["Vector", term]},
				(* Otherwise it must be a Times; anything else is  *)
				(* an unsupported routing form.                    *)
				If[Head[term] =!= Times,
					throwFailure["UnsupportedMomentum", "Momentum routing must be a linear combination with exact rational coefficients."]];
				factors = List @@ term;
				(* Exactly one vector identity factor, and every   *)
				(* remaining factor an exact Integer/Rational      *)
				(* coefficient. Anything else (two vectors in a   *)
				(* product, a symbolic coefficient) is rejected.   *)
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
	(* vector of a Plus: handle the sum by mapping over its arguments. *)
	vector[x_Plus] := Flatten[vector /@ (List @@ x), 1];
	(* vector of a Times with an exact numeric coefficient in front:   *)
	(* factor the coefficient out and scale each returned term.        *)
	vector[Times[c : (_Integer | _Rational), m_Momentum]] := ({c #[[1]], #[[2]]} & /@ vector[m]);
	vector[_] := throwFailure["UnsupportedMomentum", "Expected a linear combination of dimension-tagged momenta."];
	(* pair: FORM rendering of Lorentz contractions, memoized only     *)
	(* within this Module (the memo lives on the local symbol pair).    *)
	(* Successful fragments are cached; failures throw before the      *)
	(* assignment could happen, so nothing bad is ever cached.         *)
	(* Metric contraction d_(i,j).                                     *)
	pair[p : Pair[a_LorentzIndex, b_LorentzIndex]] := pair[p] = "d_(" <> index[a] <> "," <> index[b] <> ")";
	(* Index contracted with a momentum: sum over the routed terms,    *)
	(* each contributing coefficient * p_(i).                          *)
	pair[p : Pair[a_LorentzIndex, b_Momentum]] := pair[p] = Module[{i = index[a]},
		"(" <> StringRiffle[("(" <> emit[#[[1]]] <> "*" <> #[[2]] <> "(" <> i <> "))") & /@ vector[b], "+"] <> ")"];
	(* Symmetry: Pair[Momentum, LorentzIndex] reuses the other order.  *)
	pair[Pair[a_Momentum, b_LorentzIndex]] := pair[Pair[b, a]];
	(* Momentum-momentum: full double sum over routed terms, emitting  *)
	(* coefficient * u.v for every pair of terms from the two vectors. *)
	pair[p : Pair[a_Momentum, b_Momentum]] := 
		pair[p] = Module[{va = vector[a], vb = vector[b]},
		"(" <> StringRiffle[Flatten[Table["(" <> emit[u[[1]] v[[1]]] <> "*" <> u[[2]] <> "." <> v[[2]] <> ")",{u, va}, {v, vb}]], "+"] <> ")"];
	pair[_] := throwFailure["UnsupportedPair", "Only Lorentz metrics, momentum components and scalar products are supported."];
	(* denominator: one ordinary propagator denominator identifier.    *)
	denominator[pd : PropagatorDenominator[mom_, mass_ : 0]] := 
		denominator[pd] = Module[{},
		(* Routing with polarization vectors inside a propagator is   *)
		(* not supported, so reject before doing anything else.       *)
		If[! FreeQ[mom, _Polarization], 
			throwFailure["UnsupportedMomentum", "Propagator routing cannot contain polarization vectors."]];
		(* Force the momentum through vector[] even though the result *)
		(* is discarded: this REGISTERS the vector and validates the   *)
		(* routing form as a side effect.                             *)
		If[!propagatorRoutingQ[mom], throwFailure["UnsupportedMomentum",
          "Propagator routing must be a linear combination with exact rational coefficients."]];
		vector[mom];
		(* Mass must be an exact scalar expression.                   *)
		If[! scalarQ[mass], throwFailure["UnsupportedMass", "Propagator masses must be exact scalar expressions."]];
		(* Positive integer powers remain powers of the same identifier. *)
		(* Each identifier denotes one ordinary Feynman denominator, including i0. *)
		(* Register the whole FeynAmpDenominator, with the extra      *)
		(* fields the mapping file needs to reconstruct it: momentum, *)
		(* mass, power 1, the dimension, and the i0 prescription.     *)
			register["Denominator", FeynAmpDenominator[pd], <|
				"Momentum" -> encode[mom], "Mass" -> encode[mass], 
				"Power" -> 1,
				"Dimension" -> encode[dim], 
				"Prescription" -> "Feynman+i0"|>]
		];
	denominator[x_] := throwFailure["UnsupportedDenominator", 
		"Only ordinary quadratic PropagatorDenominator objects are supported.", <|"Expression" -> HoldForm[x]|>];
		(* abbreviation: allocate a scalar identifier for a whole scalar   *)
		(* subexpression, memoized so repeats reuse the same name.         *)
		abbreviation[x_] := abbreviation[x] = If[scalarQ[x], register["Abbreviation", x],
			throwFailure["UnsupportedScalar", "Only supported scalar expressions may be abbreviated."]];
	(* emit: the expression -> FORM text dispatcher, ordered by         *)
	(* specificity. Each clause is a Which test.                       *)
	emit[x_] := Which[
		(* Exact integers.                                             *)
		IntegerQ[x], intString[x],
		(* Exact rationals, parenthesized as (num/den).                *)
		Head[x] === Rational, "(" <> intString[Numerator[x]] <> "/" <> intString[Denominator[x]] <> ")",
		(* Exact complex: rendered with FORM's i_ and explicit signs.  *)
		Head[x] === Complex && scalarQ[x], "(" <> emit[Re[x]] <> "+i_*" <> emit[Im[x]] <> ")",
		(* Scalar symbol (includes I -> i_ via scalar).                *)
		Head[x] === Symbol && scalarQ[x], scalar[x],
		(* A sum becomes a #define macro, so the (possibly long)       *)
		(* expression is named once and referenced thereafter. This is *)
		(* why emission order determines macro numbering.              *)
		Head[x] === Plus, makeMacro[StringRiffle[emit /@ (List @@ x), "+"]],
		(* Products are parenthesized with explicit *.                  *)
		Head[x] === Times, "(" <> StringRiffle[emit /@ (List @@ x), "*"] <> ")",
		(* Lorentz contractions.                                       *)
		Head[x] === Pair, pair[x],
		(* A product of propagator denominators.                       *)
		Head[x] === FeynAmpDenominator, "(" <> StringRiffle[denominator /@ (List @@ x), "*"] <> ")",
		(* Non-negative integer powers, plus negative powers of a      *)
		(* plain symbol, stay as explicit powers of the emitted base.  *)
		Head[x] === Power && IntegerQ[x[[2]]] && (x[[2]] >= 0 || Head[x[[1]]] === Symbol),
			"(" <> emit[x[[1]]] <> ")^(" <> intString[x[[2]]] <> ")",
		(* Any other power must be a supported scalar to be           *)
		(* abbreviated into a named identifier.                        *)
		Head[x] === Power, abbreviation[x],
		(* Master integrals (A0..D0) with valid scalar arguments emit  *)
		(* as the FORM function recorded in the spec table.            *)
		KeyExistsQ[$masterSpecByHead, Head[x]] && scalarQ[x],$masterSpecByHead[Head[x]]["FORMName"] <> "(" <> StringRiffle[emit /@ (List @@ x), ","] <> ")",
		(* Anything else is a hard failure carrying the offending      *)
		(* expression unevaluated.                                     *)
		True, throwFailure["UnsupportedExpression", "The expression contains an unsupported structure.", <|"Expression" -> HoldForm[x], "Head" -> Head[x]|>]
	];
	(* Dimension name and the requested loop vectors are registered    *)
	(* BEFORE expression traversal, so their identifiers are fixed     *)
	(* independently of anything emitted later. This is what makes     *)
	(* identifier numbering stable across runs/plans.                  *)
	dimensionName = If[IntegerQ[dim], intString[dim], scalar[dim]];
	Scan[(register["Vector", #]) &, loops];
	(* ------------------------------------------------------------------ *)
	(* The staged-multiplication plan. Applies only when the top level    *)
	(* is a product of two or more sums, which is exactly the case where  *)
	(* factor order is a real choice. Serialization (emit) has already    *)
	(* happened above in original traversal order; only the later         *)
	(* multiplication plan may change.                                   *)
	(* ------------------------------------------------------------------ *)
	If[Head[expr] === Times && Count[List @@ expr, _Plus] >= 2,
		factorExpressions = List @@ expr;
		factorTexts = emit /@ factorExpressions;
		(* Find the positions of the top-level sums: each marks the end  *)
		(* of a "stage" of the serialized product.                      *)
		stageEnds = Flatten[Position[factorExpressions, _Plus, {1}, Heads -> False]];
		(* The last stage always runs to the end of the factor list,     *)
		(* covering any trailing non-sum factors.                        *)
		stageEnds[[-1]] = Length[factorTexts];
		stageRanges = MapThread[{#1 + 1, #2} &, {Prepend[Most[stageEnds], 0], stageEnds}];
		(* Build the text of each stage as a parenthesized product, and *)
		(* the corresponding expression, so the planner can inspect      *)
		(* indices.                                                      *)
		stageTexts = ("(" <> StringRiffle[Take[factorTexts, #], "*"] <> ")") & /@ stageRanges;
		stageExpressions = (Times @@ Take[factorExpressions, #]) & /@ 
		stageRanges;
		(* Index signatures drive the ordering; if there are no indices *)
		(* at all, use empty signatures (which makes the planner fall   *)
		(* back to the original order).                                  *)
		stageSignatures = If[FreeQ[stageExpressions, _LorentzIndex],
			ConstantArray[<||>, Length[stageExpressions]], 
			stageIndexSignatures[stageExpressions]];
		stageOrder = connectedStageOrder[stageExpressions, stageSignatures];
		stageTexts = stageTexts[[stageOrder]];
		(* Large, unambiguous tensor stages are normalized once in FORM *)
		(* before reuse. The 1024-leaf cutoff is a conservative heuristic *)
		(* for small jobs. Named hidden factors remain available until *)
		(* the program ends. Never prepare an ambiguous index expression: *)
		(* that could change its existing contraction behavior even when *)
		(* the stage order is unchanged. *)
        (* Only when the plan is unambiguous, indices are involved, and *)
        (* some stage is large (>= 1024 leaves) do we hoist stages into *)
        (* named cfcStageN temporaries. stageSignatures =!= $Failed is  *)
        (* the guard that keeps ambiguous index structures from being   *)
        (* prepared at all.                                              *)
        If[ stageSignatures =!= $Failed && ! FreeQ[stageExpressions, _LorentzIndex] && Max[LeafCount /@ stageExpressions] >= $stagePreparationMinimumLeaves,preparations = stageTexts;
        stageTexts = Table["cfcStage" <> intString[i], {i, Length[stageTexts]}]];
        (* The final program multiplies the stages left to right.        *)
        body = First[stageTexts];
        multiplications = Rest[stageTexts],
        (* Not a product-of-sums: just emit the expression.             *)
        body = emit[expr]];
        (* The mapping payload: format identity, version, a SHA-256 digest *)
        (* of the canonicalized expression for integrity/round-trip        *)
        (* checking, the dimension, loop momenta, the processing level     *)
        (* ("TensorAlgebraOnly" -- no tensor reduction is attempted), and  *)
        (* the ordered entries table.                                     *)
        payload = <|"Format" -> $formatName, "Version" -> $formatVersion, "ExpressionDigest" -> Hash[expr, "SHA256", "HexString"],
        "Dimension" -> encode[dim], "LoopMomenta" -> (encode /@ loops), "Processing" -> "TensorAlgebraOnly", "Entries" -> entries|>;
        (* Single return value consumed by rendering and file writing:    *)
        (* the dimension's FORM name, the body expression text, the       *)
        (* remaining multiplication stages, the #define macros, the       *)
        (* hoisted stage preparations, and the mapping payload.           *)
        <|"DimensionName" -> dimensionName, "Body" -> body, "Multiplications" -> multiplications,"Factors" -> macros, "Preparations" -> preparations, "Mapping" -> payload|>
   ];


(* ::Subsection:: *)
(*Render the FORM program and mapping*)


(* ------------------------------------------------------------------ *)
(* Pure text generation. Deliberately takes BOTH strings it needs --   *)
(* the FORM template and the result-file path -- as arguments, so this *)
(* function does no file I/O and no path discovery of its own. File    *)
(* writing happens in the caller. This is the "rendering only consumes *)
(* the association" half of the contract stated when buildExportData   *)
(* was introduced: everything here is a pure function of data + inputs.*)
(* ------------------------------------------------------------------ *)
renderExport[data_Association, result_String, template_String] := 
  Module[
     (* Projections of the single big association produced by          *)
     (* buildExportData. entries is the ordered registry table;         *)
     (* declaration/json/digest/program are scalars built below.        *)
    {entries = data["Mapping"]["Entries"], declaration, json, digest, 
    program, templateNames, unknownTemplateNames},
     (* The template contract is enforced HERE, not only where the       *)
     (* shipped template is read, because this function is the one that  *)
     (* accepts an arbitrary template. Without this check a template     *)
     (* missing a placeholder would silently render a program that      *)
     (* omits a declaration or directive -- a FORM-level failure far     *)
     (* from its cause. Required and unknown placeholders are rejected    *)
     (* against the original template before any values are inserted.    *)
     templateNames = templatePlaceholderNames[template];
     If[! AllTrue[$requiredTemplatePlaceholders, 
          MemberQ[templateNames, #] &],
        throwFailure["MissingTemplate", 
     "The FORM program template is missing required placeholders.", <|
      "Placeholders" -> $requiredTemplatePlaceholders|>]];
     unknownTemplateNames = Complement[templateNames, 
       $requiredTemplatePlaceholders];
     If[unknownTemplateNames =!= {},
        throwFailure["MissingTemplate", 
     "The FORM program template contains unknown placeholders.", <|
      "Placeholders" -> unknownTemplateNames|>]];
     (* declaration[type]: produce the FORM declaration line for one    *)
     (* declaration class, e.g. "Symbols cfs1,cfs2;" or "" if no entry  *)
     (* of that class exists.                                           *)
     declaration[type_] := Module[{names},
         (* Select the entries whose kind's Declaration class (looked   *)
         (* up in $kindSpecs, so the table remains the single owner of  *)
         (* this information) equals the requested class, then take     *)
         (* their "Name" fields in entry order. Lookup with a {}        *)
         (* default keeps entries missing "Name" from erroring -- they  *)
         (* simply contribute nothing.                                  *)
         names = Lookup[
       Select[entries, $kindSpecs[#["Kind"]]["Declaration"] === type &], 
       "Name", {}];
         (* Empty string rather than an empty declaration line, so the  *)
         (* template substitution doesn't leave stray "Symbols ;" text. *)
     If[names === {}, "", type <> " " <> StringRiffle[names, ","] <> ";"]
       ];
     (* The mapping is serialized first, because the digest must bind  *)
     (* the EXACT text of the mapping that accompanies the result. The *)
     (* "RawJSON" format with "Compact" -> True avoids insignificant    *)
     (* whitespace/newlines, which is what makes the hash stable across *)
     (* runs and machines.                                             *)
     (* The digest binds the exact serialized mapping text. 
   Reformatting JSON
        after this point would break correspondence with the generated\
 result. *)
     json = 
    ExportString[data["Mapping"], "RawJSON", "Compact" -> True];
     digest = Hash[json, "SHA256", "HexString"];
     (* Fill the template. StringReplace with a list of -> rules is a   *)
     (* single simultaneous pass, so placeholder text introduced by one *)
     (* replacement cannot be re-scanned and substituted by another --  *)
     (* important, since replacement values (expressions, paths) could  *)
     (* in principle contain "@" sequences.                            *)
     program = StringReplace[template, {
          (* Format version, taken from the header's $formatVersion.    *)
          "@FORMATVERSION@" -> IntegerString[$formatVersion], 
          (* The "CFC<n>" artifact marker from the header.              *)
      "@RESULTMARKER@" -> resultMarker[],
          (* Declare the FORM functions for the masters (A0..D0). The   *)
          (* names come from $masterSpecByFORMName, i.e. the "FORMName" fields *)
          (* of the spec table -- again, no name is hard-coded here.    *)
          "@FUNCTIONS@" -> 
       "CFunctions " <> StringRiffle[Keys[$masterSpecByFORMName], ","] <> ";",
          (* Declarations grouped by class, driven by $kindSpecs.       *)
          (* Note "Symbols" covers Scalars, Abbreviations and           *)
          (* Denominators -- they share a FORM class by design.         *)
          "@SCALARS@" -> declaration["Symbols"], 
      "@DIMENSION@" -> data["DimensionName"],
          "@VECTORS@" -> declaration["Vectors"], 
      "@INDICES@" -> declaration["Indices"],
          (* The #define macros, one per line.                          *)
          "@FACTORS@" -> StringRiffle[data["Factors"], "\n"], 
      "@EXPRESSION@" -> data["Body"],
          (* Pre-prepared large tensor stages. When empty, the whole    *)
          (* substitution is the empty string so no stray directives    *)
          (* appear. Otherwise: one "Local cfcStageN = <stage>;" per    *)
          (* preparation (MapIndexed supplies the 1-based counter),    *)
          (* then a single ".sort" and "Hide cfcStage1,cfcStage2,...;"  *)
          (* so FORM can reuse the normalized stages later.             *)
          "@PREPARATIONS@" -> If[data["Preparations"] === {}, "",
              
        StringJoin[
          MapIndexed[("Local cfcStage" <> intString[First[#2]] <> 
              " = " <> #1 <> ";\n") &,
                   data["Preparations"]]] <> ".sort\nHide " <>
               
         StringRiffle[
          Table["cfcStage" <> intString[i], {i, 
            Length[data["Preparations"]]}], ","] <> ";\n"],
          (* Remaining multiplication stages: each is ".sort" then       *)
          (* "Multiply <stage>;", i.e. the planned order becomes the    *)
          (* order of FORM operations.                                   *)
          "@MULTIPLICATIONS@" -> 
       StringJoin[(".sort\nMultiply " <> # <> ";\n") & /@ 
         data["Multiplications"]],
          (* The result path. Windows backslashes are normalized because *)
          (* FORM expects forward slashes there; Unix paths are preserved *)
          (* literally, including backslashes. Quoting is the template's  *)
          (* job.                                                        *)
          "@RESULT@" -> If[$OperatingSystem === "Windows", 
             StringReplace[result, "\\" -> "/"], result], 
          "@DIGEST@" -> digest}];
     (* Return both artifacts: the rendered FORM program and the exact *)
     (* JSON text whose hash was embedded in it. The caller can write  *)
     (* them side by side and a verifier can re-hash MappingJSON to    *)
     (* confirm it matches the @DIGEST@ in the program.               *)
     <|"Program" -> program, "MappingJSON" -> json|>
   ];

(* readProgramTemplate[]: load the FORM program template from the      *)
(* package's Templates directory, resolved relative to the module      *)
(* directory established at load time in the header. Called with no    *)
(* arguments, so it does the path discovery that renderExport          *)
(* deliberately refuses to do.                                         *)
(* The placeholder contract below is the single owner of the template  *)
(* interface: every placeholder the renderer substitutes must appear   *)
(* in Templates/Program.frm.in, and nothing else may.                  *)
$requiredTemplatePlaceholders = {
   "@FORMATVERSION@", "@RESULTMARKER@", "@FUNCTIONS@", "@SCALARS@",
   "@DIMENSION@", "@VECTORS@", "@INDICES@", "@FACTORS@",
   "@PREPARATIONS@", "@EXPRESSION@", "@MULTIPLICATIONS@", "@RESULT@",
   "@DIGEST@"};

(* templatePlaceholderNames[text]: every @NAME@ token appearing in the  *)
(* template, deduplicated. Used both to enforce the required set and,   *)
(* indirectly, to keep the renderer and the template in step.           *)
templatePlaceholderNames[text_String] := 
  DeleteDuplicates[StringCases[text, RegularExpression["@[A-Za-z][A-Za-z0-9]*@"]]];

readProgramTemplate[] := Module[{template},
     template = 
    (* Quiet suppresses Import's messages; Check catches them and      *)
    (* substitutes $Failed, so a missing or unreadable template yields *)
    (* $Failed instead of an uncaught message or a thrown error. Both  *)
    (* are needed: Quiet alone would leave the failed Import result    *)
    (* ($Failed) to be handled by the test below, while Check alone    *)
    (* would still print the message.                                  *)
    Quiet[Check[
      Import[FileNameJoin[{$moduleDirectory, "Templates", 
         "Program.frm.in"}], "Text"], $Failed]];
     (* Validate rather than trusting the I/O: anything that is not a   *)
     (* string (including $Failed from a missing file, or a binary      *)
     (* import result) becomes a structured "MissingTemplate" failure,  *)
     (* which throws to the caller's Catch on $failureTag.              *)
     If[! StringQ[template], 
    throwFailure["MissingTemplate", "Cannot read the FORM program template."]];
     (* The placeholder contract itself is enforced by renderExport, the  *)
     (* function that actually substitutes and therefore owns the        *)
     (* contract; keeping the check in one place avoids the two drifting. *)
     template
   ];


(* ::Subsection:: *)
(*Export paths and file writing*)


(* ------------------------------------------------------------------ *)
(* Path resolution and validation. This is the "one place that turns a *)
(* user-supplied filename into the three artifacts" function: the FORM *)
(* program (.frm), its mapping sidecar (.map.json) and the FORM output *)
(* (.out). It performs no I/O beyond queries -- no file is created or  *)
(* modified here.                                                      *)
(* ------------------------------------------------------------------ *)
exportPaths[file_String, overwrite_] := 
  Module[{input, mapping, result, paths},
     (* Validate the option before anything else, so a bad              *)
     (* OverwriteTarget is reported as an option error rather than as   *)
     (* a confusing file-existence failure later.                       *)
     If[! BooleanQ[overwrite], 
    throwFailure["InvalidOption", "OverwriteTarget must be True or False."]];
     (* ExpandFileName resolves relative paths and "~" against the      *)
     (* current directory/home, so everything downstream works with     *)
     (* one canonical absolute path.                                    *)
     input = ExpandFileName[file];
     (* Only .frm inputs are accepted; ToLowerCase makes the check      *)
     (* case-insensitive so "MODEL.FRM" is fine.                        *)
     If[ToLowerCase[FileExtension[input]] =!= "frm", 
    throwFailure["InvalidPath", "The FORM input filename must end in .frm."]];
     (* The two companions are derived from the input by swapping the   *)
     (* extension, so model.frm -> model.map.json and model.out, all in *)
     (* the same directory. FileBaseName drops only the final           *)
     (* extension, hence the explicit ".map.json".                      *)
     mapping = 
    FileNameJoin[{DirectoryName[input], 
      FileBaseName[input] <> ".map.json"}];
     result = 
    FileNameJoin[{DirectoryName[input], FileBaseName[input] <> ".out"}];
     paths = {input, mapping, result};
     (* Fail early if the destination directory is missing, rather than *)
     (* discovering it as a write failure three steps later.           *)
     If[! DirectoryQ[DirectoryName[input]], 
    throwFailure["InvalidPath", "The destination directory does not exist."]];
     (* Defensive sanitation of the paths. The generated FORM program   *)
     (* embeds these strings verbatim (quotes come from the template),  *)
     (* and FORM uses backticks/quotes for preprocessor constructs, so  *)
     (* a path containing them would let a filename corrupt or inject   *)
     (* into the generated program. Line breaks are equally             *)
     (* destructive. This is why the check is on the PATHS, not on the  *)
     (* raw user argument.                                              *)
     If[AnyTrue[paths, 
     StringContainsQ[#, {"\n", "\r", "<", ">", "`", "'", "\""}] &],
        throwFailure["InvalidPath", 
     "FORM output paths cannot contain quotes, angle brackets, \
backticks or line breaks."]];
     (* Refuse to clobber unless explicitly told to, and fail before    *)
     (* any staging work happens. Note this is a TOCTOU-friendly check: *)
     (* it is re-checked implicitly by the rename step in writeExport.  *)
     If[! overwrite && AnyTrue[paths, FileExistsQ], 
    throwFailure["FileExists", 
     "An export target already exists. Use OverwriteTarget -> True to \
replace it."]];
     (* Return the resolved triple as an association, which is what the *)
     (* writer consumes.                                                *)
     <|"InputFile" -> input, "MappingFile" -> mapping, 
    "ResultFile" -> result|>
   ];

(* Small I/O boundaries allow failure injection without replacing      *)
(* filesystem primitives. Each wrapper turns a hard I/O failure into   *)
(* $Failed instead of a thrown error or a printed message, so the      *)
(* transaction logic below can inspect outcomes with StringQ /         *)
(* FileExistsQ rather than catching messages. Interposing these        *)
(* functions is also the seam used to test rollback paths.             *)
(* ------------------------------------------------------------------ *)

(* Write text as UTF-8. Returns the path (a string) on success, or     *)
(* $Failed. Quiet + Check suppresses the message and captures the      *)
(* failure.                                                            *)
exportWriteText[path_, text_] := 
  Quiet[Check[
    Export[path, text, "Text", CharacterEncoding -> "UTF-8"], $Failed]];

(* Copy, with overwrite controlled by the caller. Returns the target   *)
(* path on success, $Failed otherwise. Used for making backups.        *)
exportCopyFile[source_, target_, overwrite_ : False] := 
  Quiet[Check[
    CopyFile[source, target, System`OverwriteTarget -> overwrite], $Failed]];

(* Rename/move, the operation that actually installs a staged file into *)
(* place. Returns the target path on success, $Failed otherwise.        *)
exportRenameFile[source_, target_, overwrite_] := 
  Quiet[Check[
    RenameFile[source, target, System`OverwriteTarget -> overwrite], $Failed]];

(* Delete, expressed as a predicate: True when the file is now absent   *)
(* (including the case where it never existed), False when it still     *)
(* exists. The idempotent form means cleanup code does not have to      *)
(* check existence first or worry about deleting twice.                 *)
exportDeleteFile[path_] := ! FileExistsQ[path] || 
   TrueQ[Quiet[Check[DeleteFile[path]; True, False]]];

(* ------------------------------------------------------------------ *)
(* The transactional writer. Contract stated in the header: both files *)
(* are prepared BEFORE either existing target is replaced; the backups *)
(* and REPLACED flags describe only this invocation's changes; this is *)
(* explicitly NOT a lock or a crash-recovery protocol for concurrent   *)
(* writers.                                                            *)
(* ------------------------------------------------------------------ *)
CalcFormConverter`CalcFormExport::cleanup = "Export cleanup could not remove these files: `1`.";

writeExport[paths_Association, rendered_Association, overwrite_] := 
  Module[
     (* The transactional pair is only the two files this invocation    *)
     (* owns: the program and its mapping. ResultFile (.out) is written *)
     (* later by FORM itself, so it is deliberately not a transaction   *)
     (* member.                                                         *)
     {targets = Lookup[paths, {"InputFile", "MappingFile"}],
       contents = Lookup[rendered, {"Program", "MappingJSON"}],
       staged, backups, existed, replaced = {False, False}, 
    committed = False,
       cleanup, outcome, result, aborted = False, 
    rollbackFailed = False,
       recovery = {}, cleanupFailed = {}, deleteTemporary, token},
     (* A per-invocation UUID namespaces every temporary and backup     *)
     (* file, so concurrent or repeated runs cannot collide on the      *)
     (* sidecar names.                                                  *)
     token = CreateUUID[];
     (* Staging files sit next to their targets (same directory), which *)
     (* keeps the final rename cheap and on the same filesystem.        *)
     staged = (# <> "." <> token <> ".tmp") & /@ targets;
     backups = (# <> "." <> token <> ".bak") & /@ targets;
     existed = FileExistsQ /@ targets;
   
     (* Define cleanup without running it; WithCleanup invokes it on    *)
     (* exit. Only targets replaced by this invocation are restored or  *)
     (* removed. This is the crux of the safety property: a target that *)
     (* was never renamed into place is left exactly as it was, and its *)
     (* pre-emptive backup is simply deleted.                           *)
     deleteTemporary[path_] := If[FileExistsQ[path] && !TrueQ[exportDeleteFile[path]],
       AppendTo[cleanupFailed, path]];
     cleanup[] := Module[{restored = True},
         If[! committed,
            Do[
               If[replaced[[i]],
                  If[existed[[i]],
                     (* The target existed before: put the backup back.  *)
                     (* A non-string result means $Failed, i.e. restore   *)
                     (* did not happen.                                  *)
         If[! StringQ[exportCopyFile[backups[[i]], targets[[i]], True]],
                        restored = False
                      ],
                     (* The target did not exist before: remove what we *)
                     (* installed, so the filesystem returns to its      *)
                     (* original state.                                  *)
         If[! exportDeleteFile[targets[[i]]], restored = False]
                   ]
                ],
               {i, Length[targets]}
             ]
          ];
         (* Attempt staging cleanup on every exit; record failed deletions. *)
         Scan[deleteTemporary, staged];
         (* 
     A failed restoration must leave the recovery copies available. *)
         (* Only delete backups if every replacement was undone;         *)
         (* otherwise keep them and record which ones survived so the   *)
         (* caller can be told where the recovery copies are.           *)
         If[restored,
            Scan[deleteTemporary, backups],
            rollbackFailed = True;
            recovery = Select[backups, FileExistsQ]
          ]
       ];
   
     (* Cleanup runs on success, a tagged failure, or abort. 
   Record an abort only
        after cleanup, 
   so rollback failure can report retained recovery files. *)
     result = CheckAbort[
         Catch[
            (* WithCleanup guarantees cleanup[] runs when the body exits *)
            (* by any route: normal return, Throw (our fail), or abort.  *)
            WithCleanup[
               (* Phase 1: write both artifacts to their staging paths.   *)
               (* Nothing user-visible has changed yet, so a failure here *)
               (* needs no restoration -- cleanup only deletes staging.   *)
               Do[
                  
        If[! StringQ[exportWriteText[staged[[i]], contents[[i]]]],
                     
         throwFailure["WriteFailed", 
          "Cannot stage the FORM program and mapping.",
                        <|"Path" -> staged[[i]]|>]
                   ],
                  {i, Length[targets]}
                ];
               (* Phase 2: preserve existing targets as backups. Again,  *)
               (* nothing has been replaced yet; this phase only creates  *)
               (* safety copies, and it is where overwrite refusal is     *)
               (* enforced as a second line of defence after exportPaths. *)
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
               (* Phase 3: install each staged file over its target.     *)
               Do[
                  (* 
        Replacement and ownership registration form one abort-
        protected step. *)
                  (* AbortProtect guarantees that the rename and the     *)
                  (* setting of replaced[[i]] cannot be separated by an  *)
                  (* abort: if the user aborts at just the wrong moment, *)
                  (* cleanup still knows this target was replaced and    *)
                  (* will restore it. Registration is what makes the     *)
                  (* rollback correct, not the rename itself.            *)
                  AbortProtect[
                     
         outcome = 
          exportRenameFile[staged[[i]], targets[[i]], overwrite];
                     If[StringQ[outcome], replaced[[i]] = True]
                   ];
                  (* A failed rename is reported with an explicit note   *)
                  (* that rollback has already run (cleanup fires on the *)
                  (* Throw), so the message matches the on-disk state.   *)
                  If[! StringQ[outcome],
                     
         throwFailure["WriteFailed", 
          "Cannot replace the FORM program and mapping; original \
files were restored.",
                        <|"Path" -> targets[[i]]|>]
                   ],
                  {i, Length[targets]}
                ];
               (* Commit point: set BEFORE WithCleanup runs cleanup, so  *)
               (* cleanup skips restoration and only deletes staging and *)
               (* backups.                                               *)
               committed = True;
               (* Body value on success: the paths association. Note it  *)
               (* is not the rendered text -- the caller already has that *)
               (* in `rendered`.                                         *)
               paths,
               cleanup[]
             ],
            $failureTag
          ],
         (* Abort handling: mark that an abort happened, then let the    *)
         (* abort continue to propagate after the post-processing below. *)
         aborted = True;
         $Aborted
       ];
     (* Rollback failure is reported FIRST, because it is the more      *)
     (* important condition: the original files are not all back, and   *)
     (* the recovery copies are listed so the user can fix it by hand.  *)
     If[rollbackFailed,
        throwFailure["RollbackFailed", 
     "Export failed and the original files could not all be restored. \
Recovery copies were retained.",
           <|"RecoveryFiles" -> recovery, "Targets" -> targets,
             "RetainedFiles" -> DeleteDuplicates[Join[recovery, cleanupFailed]]|>]
      ];
     (* Only if the transaction was clean do we re-raise the abort, so  *)
     (* an aborted export surfaces as an abort to the user rather than  *)
     (* as a silent success. Note the abort is re-raised AFTER the      *)
     (* rollback check above, exactly as the header comment describes.  *)
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


(* The public entry point. Everything below it (paths, data, rendering, *)
(* writing) is pure and independently testable; this function is the   *)
(* only place they are sequenced together, and the only place the      *)
(* package's failure convention is turned into a return value.         *)

(* Main definition. Pattern: an expression, a filename STRING, and     *)
(* options. The whole body is wrapped in Catch[..., $failureTag] so    *)
(* that any throwFailure[...] raised anywhere in the pipeline -- path          *)
(* validation, dimension/kind validation, encoding, rendering,         *)
(* transactional writing -- exits here and becomes the function's      *)
(* return value. Because the Catch is TAG-SPECIFIC, an unrelated       *)
(* Throw from user code or FeynCalc passes through untouched instead   *)
(* of being swallowed.                                                 *)
CalcFormConverter`CalcFormExport[expression_, file_String, 
   OptionsPattern[]] := Catch[
     (* Module is used here only to keep the four intermediates local   *)
     (* and to give `overwrite` a name so it is read from the options   *)
     (* exactly once. Note OptionValue is only meaningful inside a      *)
     (* function that declared OptionsPattern[] -- the inline form      *)
     (* OptionValue[System`OverwriteTarget] resolves against the surrounding   *)
     (* definition.                                                     *)
     Module[{paths, data, rendered, 
     overwrite = OptionValue[System`OverwriteTarget]},
        (* Step 1: resolve and validate paths. Takes only the filename   *)
        (* and the option value; does no writing. Fails fast on bad      *)
        (* extension, missing directory, forbidden characters, or an     *)
        (* existing target when overwriting is off.                      *)
        paths = exportPaths[file, overwrite];
        (* Step 2: build the export data. Note the ORDER: the whole     *)
        (* expression is converted/validated BEFORE any file is touched, *)
        (* so an unsupported expression aborts with the filesystem still *)
        (* untouched. Also note that only here are Dimension and         *)
        (* LoopMomenta read -- after the cheap path validation.          *)
        data = 
     buildExportData[expression, OptionValue[FeynCalc`Dimension], 
      OptionValue[FeynCalc`LoopMomenta]];
        (* Step 3: render the FORM program. readProgramTemplate[] does  *)
        (* the one piece of file I/O this stage needs (reading the       *)
        (* template); the result path comes from paths, so the .out      *)
        (* filename is fixed before the program text is generated.       *)
        rendered = 
     renderExport[data, paths["ResultFile"], readProgramTemplate[]];
        (* Step 4: commit. The writeExport transaction returns the paths *)
        (* association on success; that is the value of the Module, of   *)
        (* the Catch (no throw occurred) and hence of the call.          *)
        writeExport[paths, rendered, overwrite]
      ], $failureTag];

(* Fallback definition for any call that does not match -- wrong       *)
(* argument count, a non-string filename, or non-rule trailing         *)
(* arguments. This one does NOT throw and does NOT use Catch: it       *)
(* returns a Failure object directly. That is a deliberate convention  *)
(* (always hand back something FailureQ), but it means a bad-argument  *)
(* error and a validated runtime failure arrive by different           *)
(* mechanisms -- a bare call cannot be wrapped in a Catch that will    *)
(* observe both. Callers should test FailureQ on the result rather     *)
(* than rely on catching.                                              *)
CalcFormConverter`CalcFormExport[___] :=
  makeFailure["InvalidArguments",
    "Use CalcFormExport[expression, filename, options]."];


(* ::Section:: *)
(*Import: FORM parsing and FeynCalc reconstruction*)


(* ::Subsection:: *)
(*Restricted result parser*)


(* ------------------------------------------------------------------ *)
(* Section 5: FORM result parsing and FeynCalc reconstruction.         *)
(* A recursive-descent parser reads FORM's output text back into       *)
(* Wolfram expressions. It recognizes arithmetic, FORM's native tensor *)
(* syntax (d_(i,j) metrics and p_(i) components), and the four         *)
(* reserved master-integral functions -- nothing else.                 *)
(* Key design choice stated in the header: vector and index tokens     *)
(* keep DISTINCT types until they are converted into Pair objects, so  *)
(* a misplaced index is caught before arithmetic can hide it.          *)
(* ------------------------------------------------------------------ *)
parseGeneralResult[text_String, values_Association, dim_] := Module[
     {tokens, pos = 1, peek, take, expect, atom, power, unary, 
    product, sum,
       scalarValue, entryValue, call, dot, result, tokenPattern, 
    stripped, classes,
       tokenCount, compoundPattern, compoundValue},
     (* Lexing, part 1 -- what can be one token. Only COMPLETE          *)
     (* component/metric calls are made composite: cfvN(cfiM) and       *)
     (* d_(cfiM,cfiN). Everything else is lexed piecewise.              *)
     (* The input text is tokenized as-is (not whitespace-stripped      *)
     (* first), precisely so whitespace cannot glue separate identifier *)
     (* fragments into one token. Dots are deliberately left as         *)
     (* ordinary operator tokens, which is what lets the dot-chain     *)
     (* validation below run left-to-right.                             *)
     compoundPattern = 
    "(?:cfv[0-9]+\\s*\\(\\s*cfi[0-9]+\\s*\\)|d_\\s*\\(\\s*cfi[0-9]+\\\
s*,\\s*cfi[0-9]+\\s*\\))";
     (* The full token inventory: a composite call, an identifier, an   *)
     (* integer, or one of the single-character operators/punctuation.  *)
     tokenPattern = 
    RegularExpression[
     compoundPattern <> "|[A-Za-z][A-Za-z0-9_]*|[0-9]+|[+*/^(),.\\-]"];
     stripped = StringReplace[text, WhitespaceCharacter -> ""];
     tokens = StringCases[text, tokenPattern];
     (* Coverage check: the concatenation of ALL tokens, with          *)
     (* whitespace removed, must equal the whitespace-stripped input.   *)
     (* If not, some character was never tokenized -- i.e. the result   *)
     (* contains syntax this parser does not know -- and the whole      *)
     (* parse is rejected up front rather than misinterpreted. An empty *)
     (* token list is rejected for the same reason.                     *)
     If[StringReplace[StringJoin[tokens], WhitespaceCharacter -> ""] =!= 
      stripped || tokens === {}, 
    throwFailure["InvalidResult", "The result contains invalid syntax."]];
     (* Classify each DISTINCT lexeme once, into:                       *)
     (*   0 = integer, 1 = identifier, 2 = punctuation, 3 = native call *)
     (* Caching classification in an association avoids re-running the  *)
     (* regular expressions at every occurrence, which matters for      *)
     (* large results where a few identifiers repeat thousands of times.*)
     classes = Association[
         Map[# -> Which[
               StringMatchQ[#, DigitCharacter ..], 0,
               StringMatchQ[#, RegularExpression["[A-Za-z][A-Za-z0-9_]*"]], 
         1,
               StringMatchQ[#, RegularExpression[compoundPattern]], 3,
               True, 2
             ] &, DeleteDuplicates[tokens]]
       ];
     (* Decode composite calls lazily, in parse order, through the      *)
     (* SAME call[]/entryValue[] machinery as ordinary calls, so        *)
     (* identifier and argument validation is identical. Eager decoding *)
     (* would report a later unknown identifier before an earlier       *)
     (* syntax error, which is a worse diagnostic. compoundValue is     *)
     (* SetDelayed plus an inner Set, so only SUCCESSFUL results are    *)
     (* cached and that cache lives only for this import.               *)
     compoundValue[t_] := compoundValue[t] = With[
          {parts = 
        StringCases[t, RegularExpression["[A-Za-z][A-Za-z0-9_]*"]]},
          call[First[parts], entryValue /@ Rest[parts]]];
     (* Lookahead needs one token beyond the end; the sentinel is       *)
     (* appended after tokenCount is recorded, and take[] uses          *)
     (* tokenCount as its bound, so the sentinel is visible to peek[]   *)
     (* but can never be consumed as input.                             *)
     tokenCount = Length[tokens];
     tokens = Append[tokens, "END"];
     peek[] := tokens[[pos]];
     take[] := (
         (* Consumption past the real end is an explicit error, not an  *)
         (* index error.                                                *)
     If[pos > tokenCount, 
      throwFailure["InvalidResult", "Unexpected end of FORM result."]];
         tokens[[pos++]]
       );
     expect[t_] := 
    If[take[] =!= t, 
     throwFailure["InvalidResult", "Unexpected token in FORM result."]];
     (* Type gate for every scalar-arithmetic position. A raw           *)
     (* vectorToken/indexToken must never reach Times/Plus, because     *)
     (* arithmetic (or a zero factor, or cancellation) could hide an    *)
     (* invalid use. The check is FreeQ over the whole subtree, so it   *)
     (* also catches tokens nested inside derived structures.           *)
     scalarValue[x_] := If[! FreeQ[x, _vectorToken | _indexToken],
         
     throwFailure["InvalidResult", 
      "A vector or index occurs outside a tensor object."], x];
     (* Identifier resolution: the ONLY way an identifier becomes a     *)
     (* value. It is looked up in the import's association; an unknown  *)
     (* name fails with that name reported. Nothing from the text is    *)
     (* ever evaluated as Wolfram source.                               *)
     entryValue[n_] := Lookup[values, n,
         
     throwFailure["UnknownIdentifier", 
      "Unknown identifier in FORM result.", <|"Identifier" -> n|>]];
     (* call[n, args]: resolve a function application. The Which is an  *)
     (* exact whitelist of accepted (name, argument-type) shapes:       *)
     (*   d_ with two index tokens                     -> metric Pair   *)
     (*   a DECLARED VECTOR name with one index token   -> momentum     *)
     (*     component, i.e. Pair[Momentum[v, dim], LorentzIndex[i, dim]]*)
     (*   a master FORM name whose arguments pass        -> the master  *)
     (*     masterArgumentsQ (arity + scalar-ness)         head applied *)
     (* Anything else is an unsupported function or a type error.       *)
     call[n_, args_] := Which[
         
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
     (* dot[a, b]: the "." operator. Only two declared vectors may be    *)
     (* dotted, producing a Pair of momenta. Memoized on the typed pair *)
     (* of values (dim is fixed for this import, so it need not be part *)
     (* of the key). A failure throws -- here and throughout, the       *)
     (* throw happens before the inner Set could store anything, so     *)
     (* only successes are cached.                                      *)
     dot[a_, b_] := 
    dot[a, b] = If[MatchQ[{a, b}, {_vectorToken, _vectorToken}],
          Pair[Momentum[a[[1]], dim], Momentum[b[[1]], dim]],
          
      throwFailure["InvalidResult", 
       "Dot products require two declared vectors."]];
     (* atom[]: literals, parenthesized subexpressions, and calls.      *)
     (* With binds the consumed token once, so the classification and  *)
     (* the branch see the same value. All mutable parser state (pos)   *)
     (* belongs to this invocation's Module, never to an outer scope.   *)
     atom[] := With[{t = take[]},
         Which[
            (* Integer literal, built with FromDigits (never ToExpression*)
            classes[t] === 0, FromDigits[t],
            (* Complete native call (cfvN(cfiM) / d_(i,j)).             *)
            classes[t] === 3, compoundValue[t],
            (* Parenthesized expression.                                 *)
            t === "(", With[{v = sum[]}, expect[")"]; v],
            (* Identifier: a call if followed by "(", else a leaf.       *)
            classes[t] === 1,
              If[peek[] === "(",
                 Module[{args = {}},
                    take[];
                    (* Comma-separated argument list; the empty          *)
                    (* argument list is handled by the If.               *)
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
                 (* FORM's i_ is the imaginary unit; everything else  *)
                 (* must be a known identifier from the mapping.      *)
                 If[t === "i_", I, entryValue[t]]
               ],
            True, 
      throwFailure["InvalidResult", 
       "Expected a number, declared symbol or parenthesized \
expression."]
          ]
       ];
   
     (* Dot chains are consumed in order before the optional integer e\
xponent.
        Keep validation interleaved with consumption: 
   a later unknown identifier
        must not replace the failure already caused by an invalid earl\
ier dot. *)
     (* power[]: atom, then left-to-right dots, then at most one        *)
     (* exponent. Note the exponent is consumed and parsed only ONCE -- *)
     (* sign, optional parentheses and digits -- and the value is       *)
     (* applied at the end, so "(...)^(-2)" and "^2" both work.         *)
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
               (* Only integer exponents are accepted in FORM output.  *)
               If[! StringMatchQ[n, DigitCharacter ..],
                  throwFailure["InvalidResult", "FORM exponents must be integers."]
                ];
               If[parenthesized, expect[")"]];
               (* Guard 0^0 and 0^negative before they produce messages *)
               (* or Indeterminate/ComplexInfinity. The <= 0 test on the *)
               (* signed exponent is what covers both.                  *)
               If[v === 0 && sign FromDigits[n] <= 0,
                  throwFailure["InvalidResult", "Undefined power of zero."]
                ];
               v = scalarValue[v]^(sign FromDigits[n])
             ]
          ];
         v
       ];
     (* unary[]: leading + and - signs. Negation is applied to the      *)
     (* scalar-checked operand, so "-cfv1" is rejected as a vector in a *)
     (* scalar position rather than silently negated.                   *)
     unary[] := Switch[peek[],
         "+", take[]; unary[],
         "-", take[]; -scalarValue[unary[]],
         _, power[]
       ];
   
     (* Reap/
   Sow collects arbitrarily long products and sums without repeatedly
        copying a growing list. 
   Each recursive invocation owns its collector.
        Validate each operand before Times or Plus can hide a typed to\
ken through
        zero multiplication or cancellation. 
   Division is consumed left to right. *)
     (* product[]: unary, then a left-to-right chain of * and /.        *)
     (* Reap/Sow accumulates factors without repeatedly copying a       *)
     (* growing list. Every operand is scalar-checked BEFORE it reaches *)
     (* Times, so a typed token cannot be hidden by a zero factor.      *)
     (* Division checks its right operand for exact zero before the     *)
     (* quotient is formed.                                             *)
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
                       Sow[If[op === "*", r, 1/r]]
                     ]
                  ][[2, 1]];
               Times @@ factors
             ]
          ]
       ];
     (* sum[]: product, then a left-to-right chain of + and -. Same      *)
     (* Reap/Sow and same scalar gate; subtraction negates the whole    *)
     (* right-hand product.                                             *)
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
     (* Run the parser and require full consumption: leftover tokens     *)
     (* mean the result had structure the grammar does not cover, which  *)
     (* is an error rather than something to ignore.                     *)
     result = scalarValue[sum[]];
     If[pos <= tokenCount,
        throwFailure["InvalidResult", 
     "Unexpected trailing tokens in FORM result."]
      ];
     result
   ];

(* ------------------------------------------------------------------ *)
(* Fast path for large, highly repetitive FORM outputs.               *)
(*                                                                     *)
(* Large contracted outputs often repeat a small set of COMPLETE       *)
(* factors, e.g. "cfs1*cfv2.cfv3+cfs1*cfv2.cfv3+...". This lexical     *)
(* subset covers only that shape: no parentheses, no calls, no unary   *)
(* signs after operators, no chained dots. Anything outside the subset *)
(* keeps the general parser above.                                     *)
(* Possessive quantifiers (++, *+, ?+) are used so the regex engine    *)
(* does not backtrack extensively on long malformed input.             *)
(* Minimum result-body length in characters before flat-path planning. This is
   a dispatch heuristic only and never changes accepted syntax or results;
   retune with representative import benchmarks and differential parser tests. *)
$flatParserMinimumCharacters = 131072;
(* Minimum factor occurrences per distinct factor required by the flat path.
   This factor-count heuristic never changes accepted syntax or results; retune
   with import benchmarks and boundary/differential parser tests. *)
$flatParserMinimumReusePerDistinctFactor = 4;

(* ------------------------------------------------------------------ *)
$flatFactorPattern = 
  "(?:cfv[0-9]++\\s*+\\.\\\
s*+cfv[0-9]++|cfs[0-9]++|cfa[0-9]++|cfd[0-9]++|i_|[0-9]++)(?:\\s*+\\\
^\\s*+[+-]?+\\s*+[0-9]++)?";

(* prepareFlatResult[text]: lexical eligibility test for the fast path. *)
(* Deliberately LEXICAL ONLY -- it scans short tokens and then          *)
(* validates their alternation. A repeated whole-result regex would     *)
(* risk exhausting the matcher's internal repetition limit on large     *)
(* valid outputs, so no pattern here repeats over the whole text.       *)
(* Returns the validated tokens themselves (as flatTokenData), so the   *)
(* parser never has to scan the text a second time.                     *)
(* $Failed means "use the general parser" -- it does NOT mean the       *)
(* result is invalid.                                                   *)
prepareFlatResult[text_String] := Module[
     {tokens, count, start, distinctFactors, 
    operators = {"+", "-", "*", "/"}},
     (* Lex with the flat factor pattern plus the four operators.       *)
     tokens = 
    StringCases[text, RegularExpression[$flatFactorPattern <> "|[+*/-]"]];
     count = Length[tokens];
     If[count === 0, Return[$Failed]];
     (* Same complete-coverage check as the general lexer. Input is     *)
     (* tokenized BEFORE whitespace is removed, so separate identifiers *)
     (* cannot be glued together by deleting spaces.                    *)
     (* Keep the original text: 
   deleting whitespace before tokenizing could join
        separate identifiers. 
   Unicode coverage must agree with the general lexer. *)
     If[StringReplace[StringJoin[tokens], WhitespaceCharacter -> ""] =!=
          StringReplace[text, WhitespaceCharacter -> ""], 
    Return[$Failed]];
     (* A leading unary sign is allowed only at the very start.         *)
     start = If[MemberQ[{"+", "-"}, First[tokens]], 2, 1];
     (* Factor/operator alternation requires an ODD number of tokens    *)
     (* from start onward: F (op F)*.                                   *)
     If[count < start || EvenQ[count - start + 1], Return[$Failed]];
     (* Even positions are factors, odd positions must be operators.    *)
     distinctFactors = DeleteDuplicates[tokens[[start ;; ;; 2]]];
     If[Intersection[distinctFactors, operators] =!= {} ||
          (count > start && 
       Complement[DeleteDuplicates[tokens[[start + 1 ;; ;; 2]]], 
         operators] =!= {}),
        Return[$Failed]
      ];
     (* Repetition cutoff: the fast path only pays off when the result  *)
     (* REUSES a small factor vocabulary. Reject unique-heavy inputs    *)
     (* before allocating a token packet for them. distinctFactors was  *)
     (* computed for validation above and is reused here.               *)
     (* The predicate is written in factor units: from start onward,    *)
     (* factors occupy every other token, so their count is             *)
     (* Quotient[count - start, 2] + 1. Rejecting when that count is    *)
     (* less than $flatParserMinimumReusePerDistinctFactor occurrences  *)
     (* per distinct factor is EXACTLY the historical token-based       *)
     (* predicate Length[distinctFactors] > (count - start + 1)/8; the  *)
     (* rewrite only makes the intent readable and keeps the cutoff in  *)
     (* one named place.                                                *)
     If[Quotient[count - start, 2] + 1 < 
        $flatParserMinimumReusePerDistinctFactor Length[distinctFactors], 
    Return[$Failed]];
     flatTokenData[tokens, start]
   ];

(* parseFlatResult[flatTokenData[...]]: the fast-path parser. The packet *)
(* guarantees alternating factors/operators and complete lexical        *)
(* coverage, so this walks the token list directly and only performs    *)
(* the per-value checks lazily, in consumption order.                   *)
(* The pattern on the left-hand side doubles as a type guard: anything  *)
(* that is not a flatTokenData packet simply does not match, and the    *)
(* general parser is used instead.                                      *)
(* The packet establishes alternating factors/\
operators and complete lexical
   coverage. \
Mapped values are still checked lazily in consumption order. *)
parseFlatResult[flatTokenData[tokenList_List, start_Integer], 
   values_Association, dim_] := Module[
     {tokens = tokenList, count = Length[tokenList], pos = start,
       factor, checked, product, firstSign, result, terms, op, r},
     firstSign = If[start === 2 && First[tokens] === "-", -1, 1];
     (* Same contract as the general parser, but memoized per factor    *)
     (* TEXT so a repeated factor is re-parsed only once per import.    *)
     (* Never evaluate result text as Wolfram source. 
   Throw exits before Set can
        store a failed reconstruction; 
   successful cached values remain job-local. *)
     factor[t_] := factor[t] = parseGeneralResult[t, values, dim];
     (* Identical typed-token gate as the general parser.               *)
     checked[x_] := If[! FreeQ[x, _vectorToken | _indexToken],
         
     throwFailure["InvalidResult", 
      "A vector or index occurs outside a tensor object."], x];
     (* product[sign]: a factor, then *,/ chain. The initial unary      *)
     (* minus is applied to the FIRST FACTOR only (matching the general *)
     (* parser's evaluation boundaries); later subtraction negates a    *)
     (* whole term, and division validates its right operand before     *)
     (* consuming anything else.                                        *)
     (* Preserve the general parser's evaluation boundaries: 
   the initial unary
        minus belongs to the first factor, 
   later subtraction negates a whole term,
        and division checks its right operand before consuming anythin\
g later. *)
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
     (* Top level: one product, then +/- chains of products.            *)
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

(* parseResult: dispatch between the fast path and the general parser.  *)
(* The fast path is attempted only when the text is large (>= 128 KiB)  *)
(* AND no symbol reachable from the imported values (or the dimension)  *)
(* has UpValues. That second condition matters because the fast path    *)
(* MEMOIZES per factor text: with user-defined arithmetic, reusing a    *)
(* cached factor could skip evaluations the user's UpValues would have  *)
(* performed. With no UpValues anywhere in play, memoization is exact.  *)
(* Everything under Quiet[Check[...]] is optional optimization, so a    *)
(* message or an unevaluated eligibility result must not escape as an   *)
(* apparently successful import; Check does not catch Abort, so user    *)
(* cancellation still propagates.                                       *)
(* The size and repetition cutoffs are conservative heuristics, \
not an optimal
   crossover model. \
Symbols with UpValues use the general path so user-defined
   arithmetic is evaluated per occurrence rather than memoized as a fa\
ctor. *)
parseResult[text_String, values_Association, dim_] := 
  Module[{prepared = $Failed},
     If[StringLength[text] >= $flatParserMinimumCharacters &&
          
     AllTrue[DeleteDuplicates[
       Cases[{Values[values], dim}, _Symbol, Infinity, Heads -> True]], 
      UpValues[#] === {} &],
        (* This is an optional optimization. 
    Local messages or an unevaluated
           
     eligibility result must not escape as an apparently successful imp\
ort.
           Check does not catch Abort, 
    so user cancellation still propagates. *)
        prepared = Quiet[Check[prepareFlatResult[text], $Failed]]
      ];
     (* Match on the packet type itself: prepareFlatResult returning    *)
     (* $Failed (or anything else) routes to the general parser.        *)
     If[MatchQ[prepared, flatTokenData[_List, _Integer]],
        parseFlatResult[prepared, values, dim],
        parseGeneralResult[text, values, dim]
      ]
   ];


(* ::Subsection:: *)
(*Mapping validation and public importer*)


(* ------------------------------------------------------------------ *)
(* Decode and validate every entry ONCE, before any parsing happens.   *)
(* "Including entries eliminated by FORM" is the point: FORM may have  *)
(* optimized an identifier away entirely, but the mapping still owns    *)
(* it, so it is decoded and kind-validated here and stays available for *)
(* reconstruction.                                                     *)
(* The resulting association doubles as the parser's symbol table,     *)
(* carrying not just values but vector/index TYPES -- that is how      *)
(* parseGeneralResult can tell a declared vector from a scalar.        *)
(* Ordering rule stated in the header: the denominator's "Expression"  *)
(* data is authoritative; convenience metadata (Momentum, Mass, Power, *)
(* Dimension, Prescription) never overrides it during reconstruction.  *)
(* ------------------------------------------------------------------ *)
decodeEntries[entries_List, dim_:Automatic] := Association[
     Map[
        Function[entry,
           (* Reconstruct the value from its encoded form using the    *)
           (* general decoder. This is the step that can throw          *)
           (* "InvalidMapping" for bad data or unsupported structure.    *)
           Module[{value = decode[entry["Expression"]]},
              (* Now apply the kind's OWN validity predicate -- the pure *)
              (* functions stored in $kindSpecs. This is the payoff of   *)
              (* keeping those functions in the kind table: the same     *)
              (* vocabulary shared with export is enforced on import.   *)
              (* TrueQ makes a non-boolean                              *)
              (* (an unevaluated predicate, say) a failure rather than a *)
              (* silent pass.                                            *)
      If[! TrueQ[$kindSpecs[entry["Kind"]]["ValidExpression"][value]] ||
          (dim =!= Automatic && !consistentMappedDimensionQ[value, dim]),
                 
       throwFailure["InvalidMapping", 
        "Mapped expression does not match its declared kind."]
               ];
              (* Tag the value with its type for the parser. Only       *)
              (* Vector and Index are wrapped; everything else (scalars, *)
              (* abbreviations, denominators) is stored as the plain     *)
              (* expression, which is what the parser expects to find in *)
              (* a scalar position. The _ default is unreachable in      *)
              (* practice because the kind was already validated on the  *)
              (* export side, but it keeps the Switch total.             *)
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

(* ------------------------------------------------------------------ *)
(* Public import entry point. The pipeline is a deliberate mirror of   *)
(* export: read and identify the file pair, validate correspondence,   *)
(* decode and validate all mapped values, and only then parse the      *)
(* result text. Note the ordering rationale: the digest check is a     *)
(* cheap identity check on the PAIR of files, while parsing is where   *)
(* grammar and type errors are found -- so a mismatched pair fails     *)
(* before any effort is spent on the result.                           *)
(* The header comment is explicit that the digest detects a mismatched *)
(* file pair; it does NOT replace expression or grammar validation.    *)
(* ------------------------------------------------------------------ *)
CalcFormConverter`CalcFormImport[resultFile_String, 
   mappingFile_String] := Catch[
     Module[{json, mapping, text, lines, digest, entries, dim, names, 
     values},
        (* Read both files as UTF-8 text, with Quiet/Check converting   *)
        (* any I/O failure into $Failed so it can be reported as one   *)
        (* uniform failure rather than a raw message or abort.         *)
        json = 
     Quiet[Check[
       Import[mappingFile, "Text", 
        CharacterEncoding -> "UTF-8"], $Failed]];
        text = 
     Quiet[Check[
       Import[resultFile, "Text", 
        CharacterEncoding -> "UTF-8"], $Failed]];
        If[! StringQ[json] || ! StringQ[text], 
     throwFailure["ReadFailed", "Cannot read the result or mapping file."]];
        (* Parse the JSON into an association. Two attempts: the       *)
        (* straightforward string import, then explicit UTF-8 bytes,   *)
        (* because older kernels hand RawJSON strings around as UTF-8  *)
        (* bytes while notebook sessions may produce real Unicode      *)
        (* text. Retrying through a byte buffer keeps version-one      *)
        (* files readable across both behaviors.                       *)
        (* Older kernels expose RawJSON strings as UTF-8 bytes, 
    while notebook
           sessions may supply Unicode text. Keep version-one byte-
    string files
           
    readable and retry genuine Unicode through an explicit UTF-\
8 buffer. *)
        mapping = Quiet[Check[ImportString[json, "RawJSON"], $Failed]];
        If[mapping === $Failed,
           mapping = 
      Quiet[Check[
        ImportByteArray[ByteArray[ToCharacterCode[json, "UTF-8"]], 
         "RawJSON"], $Failed]]];
        (* Identity gate: it must be an association, must declare this *)
        (* format name and this exact format version. Note =!= is used *)
        (* so a version stored as a string or a real is not accepted   *)
        (* by accident; and Lookup with a None default handles a       *)
        (* missing key without a message.                              *)
        If[! AssociationQ[mapping] || 
      Lookup[mapping, "Format", None] =!= $formatName ||
             Lookup[mapping, "Version", None] =!= $formatVersion,
           throwFailure["InvalidMapping", "Unsupported mapping format or version."]];
        (* Split the result into lines for the marker check. CRLF is   *)
        (* normalized first, since the exporter may have written the   *)
        (* file on any platform.                                       *)
        lines = StringSplit[StringReplace[text, "\r\n" -> "\n"], "\n"];
        (* Digest check. The exporter hashed the exact compact JSON    *)
        (* text and wrote "<marker> <digest>" as the first line of the *)
        (* FORM result program. Two digests are accepted: one over the  *)
        (* string as read, and one over its UTF-8 round-trip, because  *)
        (* early notebook exports could decode the byte string during  *)
        (* writing AFTER the checksum was computed. Both bind the      *)
        (* complete mapping text; the marker prefix also pins the       *)
        (* format version, since resultMarker[] embeds $formatVersion.  *)
        (* 
    Early notebook exports could decode the UTF-8 byte string during
           writing, after its checksum was calculated. 
    Accept that exact legacy
           representation too; 
    both checksums still bind the complete mapping. *)
        digest = Hash[#, "SHA256", "HexString"] & /@
            {json, FromCharacterCode[ToCharacterCode[json, "UTF-8"]]};
        If[
     Length[lines] < 2 || ! 
       MemberQ[(resultMarker[] <> " " <> # &) /@ digest, First[lines]],
           throwFailure["MappingMismatch", 
      "The result does not correspond to this mapping file."]];
        (* Structural validation of the entries list BEFORE any value   *)
        (* is decoded.                                        *)
        entries = Lookup[mapping, "Entries", None];
        If[! ListQ[entries] || ! AllTrue[entries, AssociationQ], 
     throwFailure["InvalidMapping", "Invalid mapping entries."]];
        (* Identifier validation: every entry must have a name that     *)
        (* obeys validEntryNameQ (kind prefix + digits), names must be  *)
        (* unique, and every entry must carry an "Expression" key to    *)
        (* decode. Note Lookup threads over the list of associations    *)
        (* with a {} default, so a missing "Name" surfaces as Missing   *)
        (* here rather than as a message.                                *)
        names = Lookup[entries, "Name", {}];
        If[! DuplicateFreeQ[names] || ! AllTrue[entries,
               validEntryNameQ[#] &&
                 KeyExistsQ[#, "Expression"] &], 
     throwFailure["InvalidMapping", "Invalid or duplicate mapping identifiers."]];
        (* Dimension: decoded with the same strictness as the export    *)
        (* side -- Symbol or Integer >= 2, with I explicitly excluded.  *)
        (* It is validated BEFORE any entry is decoded, so a bad        *)
        (* dimension is reported without first building expressions.    *)
        dim = decode[Lookup[mapping, "Dimension", None]];
        If[! 
       MatchQ[dim, _Symbol | _Integer] || (IntegerQ[dim] && dim < 2) || 
      dim === I, throwFailure["InvalidMapping", "Invalid mapped dimension."]];
        (* Only now decode every mapped value: name -> value, with      *)
        (* vectors and indices wrapped in their typed tokens.           *)
        values = decodeEntries[entries, dim];
        (* Hand the result text (everything after the marker line) to   *)
        (* the parser together with the symbol table and dimension.     *)
        parseResult[StringRiffle[Rest[lines], "\n"], values, dim]
      ], $failureTag];

(* Same convention as the export entry point: a call that does not     *)
(* match the two-string signature returns a Failure object instead of  *)
(* throwing. So, as on the export side, a caller must inspect FailureQ *)
(* on the result rather than relying on Catch to observe every error.  *)
CalcFormConverter`CalcFormImport[___] :=
  makeFailure["InvalidArguments",
    "Use CalcFormImport[resultFile, mappingFile]."];


(* ::Section:: *)
(*Load runtime definitions without executing processes*)


(* Load the companion runtime definitions from the same directory as    *)
(* this package. $moduleDirectory was computed in the header with        *)
(* DirectoryName[$InputFileName], so this resolves relative to the FILE *)
(* rather than to Directory[] (the current working directory) -- which   *)
(* is what makes the package relocatable and independent of how the      *)
(* caller's session happens to be positioned.                            *)
(* ------------------------------------------------------------------ *)
Get[FileNameJoin[{$moduleDirectory, "FORMRuntime.wl"}]];

(* Get << "file" reads and evaluates the file in the CURRENT context   *)
(* (there is no separate namespace introduced here), inserting its     *)
(* definitions as if they had been evaluated at this point. That makes *)
(* this line a load-ORDER dependency: anything FORMRuntime.wl defines  *)
(* must not be needed by cells evaluated before it, and everything     *)
(* downstream may assume it is now present. Note the path is joined    *)
(* with FileNameJoin rather than string concatenation, so the right    *)
(* separator is used on every platform.                                *)
(*                                                                     *)
(* Two properties worth noting, because they differ from the rest of   *)
(* the package's style:                                                *)
(*                                                                     *)
(* 1. There is no error handling. If FORMRuntime.wl is missing or      *)
(*    fails to evaluate, Get issues a message and returns $Failed;     *)
(*    it does not call throwFailure[...], so this does NOT become a structured *)
(*    "MissingRuntime" Failure caught by $failureTag. The package      *)
(*    then continues with whatever definitions did load, and any later *)
(*    use of an undefined runtime symbol stays unevaluated and         *)
(*    surfaces as a confusing failure far from the actual cause --     *)
(*    exactly the symptom we saw earlier with vectorIdentityQ, which   *)
(*    is referenced by $kindSpecs and scalarQ but not defined in the   *)
(*    cells you showed me. A Check around this Get with a throwFailure[...]    *)
(*    would close that gap.                                            *)
(*                                                                     *)
(* 2. Get evaluates the file every time this cell runs, so this line   *)
(*    is not idempotent in the way the rest of the file is. Re-running *)
(*    it re-executes FORMRuntime.wl: harmless if that file only sets   *)
(*    definitions, but if it mutates state (counters, caches,          *)
(*    accumulated lists), a second evaluation of this package would    *)
(*    not be a no-op. Any mutable state in the runtime file should be  *)
(*    reset by its own definitions rather than appended to.            *)
(* ------------------------------------------------------------------ *)


(* ::Section:: *)
(*Close the package*)


(* ::Input::Initialization:: *)
End[];
EndPackage[];
