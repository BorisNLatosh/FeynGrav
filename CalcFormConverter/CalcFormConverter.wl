(* ::Package:: *)

(* ::Title:: *)
(*CalcFormConverter*)


(* ::Text:: *)
(*FeynCalc \[LeftRightArrow] FORM conversion with optional ordinary Dirac algebra. Runtime definitions load separately. Ordinary propagators support algebraic numerator cancellation before grouping. Eligible version-one scalar coefficients expose reciprocal polynomials for bounded FORM rational simplification. The mapping is JSON data, not executable Wolfram Language source.*)


(* ::Section:: *)
(*Public interface*)


(* ::Input::Initialization:: *)
BeginPackage["CalcFormConverter`", {"FeynCalc`"}];


(* ::Text:: *)
(*Public functions and options.*)


(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormExport::usage ="CalcFormExport[expr, file, opts] exports an exact supported FeynCalc expression, including ordinary Dirac words, SU(N) colour structures and traces and rank-four single-space Lorentz Eps tensors, to a complete FORM program and a reversible JSON mapping. Prints a readable file summary and returns an association with InputFile, MappingFile and ResultFile. Options: Dimension -> Automatic, LoopMomenta -> {}, OverwriteTarget -> False, DiracAlgebra -> Automatic, ColourAlgebra -> True. The generated program simplifies ordinary open chains and evaluates explicit DiracTrace expressions; DiracAlgebra -> False selects translation only. ColourAlgebra -> True (the default) or Automatic reduces fundamental SU(N) colour expressions; False preserves them and unevaluated SUNTrace. Propagator products are grouped automatically. Eligible small scalar coefficients undergo rational cancellation and numerator/denominator factorisation in FORM. Remaining version-one coefficients can simplify rational dependence on the symbolic Lorentz dimension alone; other cases retain common-factor extraction and momentum grouping. A direct massless monomial pass followed by a bounded denominator-basis stage cancels polynomial numerators against ordinary propagators before grouping, retaining constant terms and momentum routing. No Mathematica Simplify is called. No FORM process or integral reduction is performed.";


(* ::Input::Initialization:: *)
CalcFormConverter`ColourAlgebra::usage = "ColourAlgebra selects SU(N) reduction (True or Automatic) or preservation (False).";

CalcFormConverter`DiracAlgebra::usage = "DiracAlgebra selects ordinary open-chain simplification and explicit trace evaluation in FORM (Automatic), or translation only (False).";

CalcFormConverter`CalcFormImport::usage = "CalcFormImport[resultFile, mappingFile] reads the dedicated result of an exported FORM program and reconstructs FeynCalc internal notation. It does not execute Wolfram Language source or run tensor/integral reduction. Epsilon results require the exported $LeviCivitaSign setting. Versions one through five are supported. New grouped results are read incrementally and retain propagator products and coefficient factors; the final expression and individual coefficients still require kernel memory. Version-four results distinguish processed open chains from preserved explicit traces; import performs scalar Casimir presentation for processed colour jobs but no Dirac or colour tensor reduction.";


(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormCheck::usage = "CalcFormCheck[opts] probes FORM on demand and returns availability, version and diagnostics. Options: FORMExecutable -> Automatic, FORMThreads -> Automatic, TimeConstraint -> 10.";


(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormInstall::usage = "CalcFormInstall[opts] explicitly attempts Debian/Ubuntu installation of missing FORM or TFORM using system authorization, then verifies the requested configuration. FORMThreads -> Automatic prefers up to eight TFORM workers and accepts serial FORM when TFORM is missing; explicit values above 1 require TFORM. It is never called automatically.";


(* ::Input::Initialization:: *)
CalcFormConverter`CalcFormCalculate::usage = "CalcFormCalculate[expr, opts] exports, executes FORM and imports its result. For a binary equality lhs == rhs, it calculates lhs - rhs and compares the imported residual with zero; the result may remain symbolic. True and False are returned directly. Options include Dimension, LoopMomenta, FORMExecutable, TimeConstraint, WorkingDirectory, KeepFiles, ShowTiming, ShowProgress, FORMThreads, DiracAlgebra and ColourAlgebra. Failed jobs are retained. Polynomial numerators are cancelled against ordinary propagators using a direct massless monomial pass followed by a bounded independent-basis stage; no momentum shifts or scaleless removal are applied. Surviving propagator products are grouped automatically and imported incrementally. Eligible small scalar coefficients are rationally simplified and factorised in FORM; remaining version-one coefficients can simplify rational dependence on the symbolic Lorentz dimension alone. Other cases retain the existing grouped path. No Mathematica Simplify is called. DiracAlgebra -> Automatic simplifies ordinary open chains and evaluates explicit DiracTrace expressions; False selects translation only. ColourAlgebra -> True (the default) or Automatic reduces fundamental SU(N) colour structures; False preserves colour objects and unevaluated SUNTrace.";


(* ::Input::Initialization:: *)
CalcFormConverter`FORMExecutable::usage = "FORMExecutable selects the FORM executable by name or path; Automatic searches the kernel PATH according to FORMThreads. An explicit executable with automatic threads runs with one worker.";


(* ::Input::Initialization:: *)
CalcFormConverter`WorkingDirectory::usage = "WorkingDirectory specifies an existing parent directory for unique FORM jobs; Automatic uses the system temporary directory.";


(* ::Input::Initialization:: *)
CalcFormConverter`ShowTiming::usage = "ShowTiming -> True prints FORM process elapsed wall-clock seconds, excluding the availability probe, export and import. Defaults to False; the returned expression is unchanged.";


(* ::Input::Initialization:: *)
CalcFormConverter`ShowProgress::usage = "ShowProgress -> True prints calculation stages and elapsed FORM execution time every ten seconds. It does not estimate a completion percentage. Defaults to False.";


(* ::Input::Initialization:: *)
CalcFormConverter`FORMThreads::usage = "FORMThreads accepts Automatic (default) or a positive integer for CalcFormCheck, CalcFormInstall and CalcFormCalculate. Automatic prefers TFORM with Min[8, $ProcessorCount] workers, falling back to serial FORM only when TFORM is missing. Invalid processor counts use one worker. Explicit executables use one worker with Automatic. Explicit 1 selects serial FORM; larger counts require TFORM without fallback or capping.";


(* ::Input::Initialization:: *)
CalcFormConverter`KeepFiles::usage = "KeepFiles -> True retains successful FORM calculation files and reports their location. Failed jobs are always retained.";


(* ::Subsection:: *)
(*Private initialization and export options*)


(* ::Input::Initialization:: *)
Begin["`Private`"];


(* ::Text:: *)
(*$InputFileName identifies the file currently being read by Get or Needs. Save its directory while loading this package so templates and FORMRuntime.wl can be located relative to it. Outside file loading, $InputFileName is normally an empty string; this initialization is intended for package loading, not independent notebook evaluation.*)


(* ::Input::Initialization:: *)
$moduleDirectory = DirectoryName[$InputFileName];


(* ::Text:: *)
(*Human-readable name of the converter. Plain string data; no evaluation semantics. It is the identity half of the pair used to stamp and later recognize generated artefacts.*)


(* ::Input::Initialization:: *)
$formatName = "CalcFormConverter";


(* ::Text:: *)
(*Version of the persisted mapping and result format. Changes to format semantics require a compatibility decision; changing this integer alone does not implement migration or support for older versions.*)


(* ::Input::Initialization:: *)
$formatVersion = 1;


(* ::Text:: *)
(*resultMarker[] builds the result-header marker, currently "CFC1". The template also includes it when writing the result. The JSON mapping stores Format and Version separately. SetDelayed (:=) reads $formatVersion when the helper is called.*)

(* ::Input::Initialization:: *)
resultMarker[version_ : $formatVersion] := "CFC" <> IntegerString[version];

(* Bound only during import; legacy jobs cannot introduce epsilon calls. *)
$epsilonImportFactor = None;


(* ::Text:: *)
(*intString[n] renders an exact integer as decimal digits with a minus sign only for negative values. IntegerString[Abs[n]] supplies the digits and <> joins the strings. The n_Integer pattern limits this definition to integer arguments; other calls remain unevaluated.*)
(**)
(*intString[7] -> "7"*)
(*intString[-7] -> "-7"*)
(*intString[0] -> "0"*)

(* ::Input::Initialization:: *)
intString[n_Integer] := If[n < 0, "-", ""] <> IntegerString[Abs[n]];


(* ::Text:: *)
(*Create a fresh symbol for the private Throw/Catch tag. Unique prevents accidental name collisions within the kernel; the tag is not secret or unforgeable. Conversion helpers throw failures with this tag so public entry points can return them without catching unrelated throws.*)

(* ::Input::Initialization:: *)
$failureTag = Unique["CalcFormFailure"];


(* ::Text:: *)
(*makeFailure constructs the package's one structured failure shape. Association Join is right-biased, so caller data may deliberately override MessageTemplate. throwFailure is the tagged nonlocal-exit form used by deep conversion and parser helpers. Runtime code and invalid-argument fallbacks return makeFailure directly.*)

(* ::Input::Initialization:: *)
makeFailure[tag_String, message_String, data_ : <||>] :=
	Failure[tag, Join[<|"MessageTemplate" -> message|>, data]];
throwFailure[tag_String, message_String, data_ : <||>] :=
	Throw[makeFailure[tag, message, data], $failureTag];


(* ::Text:: *)
(*Give CalcFormConverter`CalcFormExport its public option defaults. Options[symbol] = {...} is a plain assignment to the symbol's own Options value: it stores the list of default rules that OptionsPattern[] and OptionValue[] will consult when the function is called. The left-hand side is deliberately written fully qualified as CalcFormConverter`CalcFormExport, so the assignment lands on the package's symbol even if this header is evaluated from a context where the short name would resolve elsewhere (or create a stray Global` one).*)

(* ::Input::Initialization:: *)
Options[CalcFormConverter`CalcFormExport] = {
    (*FeynCalc owns these two option symbols. Qualifying them preserves their historical identity while making it independent of context.*)
    FeynCalc`Dimension -> Automatic,
    FeynCalc`LoopMomenta -> {},
    System`OverwriteTarget -> False,
    CalcFormConverter`DiracAlgebra -> Automatic, CalcFormConverter`ColourAlgebra -> True
};


(* ::Section:: *)
(*Expression inspection and reversible mapping data*)


(* ::Subsection:: *)
(*Supported heads and encoding*)


(* ::Text:: *)
(*The specification table assigns stable mapping tags to supported heads. Each entry records its Wolfram Head and allowed Arity. Scalar master entries also provide a FORMName and the Scalar argument rule. Decoding checks arity; master validation, serialization and declarations use the corresponding fields. Tensor serialization has additional explicit rules. These helpers do not call tensor contraction or integral reduction, although ordinary kernel evaluation still applies.*)

(* ::Input::Initialization:: *)
$expressionSpecs = <|
	(*Addition: any number of terms, including 0 (Plus[] -> 0).*)
	"Plus" -> <|"Head" -> Plus, "Arity" -> {0, Infinity}|>,
	(*Multiplication: any number of factors, including 0 (-> 1).*)
	"Times" -> <|"Head" -> Times, "Arity" -> {0, Infinity}|>,
	(*Power[base, exponent]: exactly two arguments.*)
	"Power" -> <|"Head" -> Power, "Arity" -> {2, 2}|>,
	(*FeynCalc Pair takes two arguments and represents a metric, momentum component or scalar product in the supported vocabulary.*)
	"Pair" -> <|"Head" -> Pair, "Arity" -> {2, 2}|>,
	(*FeynCalc Momentum[label] or Momentum[label, dim]; the omitted dimension denotes four dimensions.*)
	"Momentum" -> <|"Head" -> Momentum, "Arity" -> {1, 2}|>,
	(*FeynCalc LorentzIndex[name] or LorentzIndex[name, dim].*)
	"LorentzIndex" -> <|"Head" -> LorentzIndex, "Arity" -> {1, 2}|>,
	(*FeynCalc FeynAmpDenominator[...]: one or more denominators.*)
	"FeynAmpDenominator" -> <|"Head" -> FeynAmpDenominator, 
	"Arity" -> {1, Infinity}|>,
	(*FeynCalc PropagatorDenominator[momentum, mass]: one or two args.*)
	"PropagatorDenominator" -> <|"Head" -> PropagatorDenominator, 
	"Arity" -> {1, 2}|>,
	(*Scalar Passarino-Veltman master functions use commuting FORM functions cfA0 through cfD0. Their scalar arguments are serialized recursively; this module supplies no integral evaluation or reduction rules. A0 takes one squared-mass argument.*)
	"A0" -> <|"Head" -> A0, "FORMName" -> "cfA0", "Arity" -> {1, 1}, 
	"Arguments" -> "Scalar"|>,
	(*B0[p^2, m1^2, m2^2]: three scalar arguments.*)
	"B0" -> <|"Head" -> B0, "FORMName" -> "cfB0", "Arity" -> {3, 3}, 
	"Arguments" -> "Scalar"|>,
	(*C0 has six scalar arguments: three external invariants and three squared internal masses.*)
	"C0" -> <|"Head" -> C0, "FORMName" -> "cfC0", "Arity" -> {6, 6}, 
	"Arguments" -> "Scalar"|>,
	(*D0[...]: ten scalar arguments (the four-point case).*)
	"D0" -> <|"Head" -> D0, "FORMName" -> "cfD0", "Arity" -> {10, 10}, 
	"Arguments" -> "Scalar"|>
|>;


(* ::Text:: *)
(*Map preserves the association keys and replaces each specification with its Head: <|"Plus" -> Plus, "Times" -> Times, ...|>. The decoder uses this name-to-head lookup.*)

(* ::Input::Initialization:: *)
$expressionHeadByName = Map[#["Head"] &, $expressionSpecs];


(* ::Text:: *)
(*Build the inverse lookup head -> spec name, e.g. Plus -> "Plus" and A0 -> "A0". Normal turns the association of rules into a list of Rule expressions, Reverse swaps each side, and Association reassembles the list into an association. Note the direction: $expressionHeadByName maps name to head, so reversing gives head to name.*)

(* ::Input::Initialization:: *)
$expressionNameByHead = Association[Map[Reverse, Normal[$expressionHeadByName]]];


(* ::Text:: *)
(*Select the specifications that contain FORMName, retaining their string keys. This creates a derived association at package initialization; subsequent assignments to the source table do not automatically update it.*)

(* ::Input::Initialization:: *)
$masterSpecs = Select[$expressionSpecs, KeyExistsQ[#, "FORMName"] &];


(* ::Text:: *)
(*Index master specifications by Wolfram head for direct lookup during export: <|A0 -> spec, B0 -> spec, ...|>.*)

(* ::Input::Initialization:: *)
$masterSpecByHead = Association[(#["Head"] -> #) & /@ Values[$masterSpecs]];


(* ::Text:: *)
(*Index master specifications by their FORM names for declarations and import: <|"cfA0" -> spec, "cfB0" -> spec, ...|>.*)

(* ::Input::Initialization:: *)
$masterSpecByFORMName = Association[(#["FORMName"] -> #) & /@ Values[$masterSpecs]];


(* ::Text:: *)
(*arityQ[spec, n]: does argument count n fall inside the spec's allowed range? Part 1 of "Arity" is the minimum, part 2 the maximum, and the chained inequality min <= n <= max is a single comparison that is True only when both hold. Infinity works directly as a maximum, so {0, Infinity} accepts any n and {2, 2} accepts only 2. This is a shape check only: it says nothing about the argument types.*)

(* ::Input::Initialization:: *)
arityQ[spec_Association, n_Integer] := spec["Arity"][[1]] <= n <= spec["Arity"][[2]];


(* ::Text:: *)
(*Require an allowed argument count and the supported Scalar argument class. AllTrue applies scalarQ to each argument only after the arity check passes. Other argument classes are rejected.*)

(* ::Input::Initialization:: *)
masterArgumentsQ[spec_Association, args_List] := arityQ[spec, Length[args]] && Switch[spec["Arguments"], "Scalar", AllTrue[args, scalarQ], _, False];


(* ::Text:: *)
(*The kind table defines identifier prefixes, FORM declaration classes and validity predicates for mapped values. Serialization and typed reconstruction also contain explicit rules for individual kinds; this table does not replace those rules or the master specifications.*)

(* ::Input::Initialization:: *)
$kindSpecs = <|
	(*Scalar identifiers use the cfs prefix and the FORM Symbols declaration. The decoded value must have head Symbol after normal evaluation; this predicate does not inspect or prevent prior evaluation.*)
	"Scalar" -> <|"Prefix" -> "cfs", "Declaration" -> "Symbols", 
	"ValidExpression" -> (Head[#] === Symbol &)|>,
	(*Vector: a Lorentz vector object. Prefix "cfv" -> cfv1, cfv2. FORM class: Vectors (FORM's vector declaration). Validity is delegated to vectorIdentityQ, defined elsewhere in the package.*)
	"Vector" -> <|"Prefix" -> "cfv", "Declaration" -> "Vectors", 
	"ValidExpression" -> (vectorIdentityQ[#] &)|>,
	(*Index: a Lorentz index. Prefix "cfi" -> cfi1, cfi2. FORM class: Indices. The decoded value must have head Symbol, as for Scalar.*)
	"Index" -> <|"Prefix" -> "cfi", "Declaration" -> "Indices", 
	"ValidExpression" -> (Head[#] === Symbol &)|>,
	(*Abbreviation: a symbol introduced as a shorthand for a scalar subexpression. Prefix "cfa" -> cfa1, cfa2. FORM class: Symbols. Validity reuses scalarQ, the same predicate that gates master arguments in $expressionSpecs, keeping the two notions in sync.*)
	"Abbreviation" -> <|"Prefix" -> "cfa", "Declaration" -> "Symbols", 
	"ValidExpression" -> (scalarQ[#] &)|>,
	(*A cfd identifier represents one FeynAmpDenominator containing one ordinary PropagatorDenominator. validDenominatorQ checks its routing and scalar mass. Import additionally checks dimensions against the mapping. A combined FAD is exported as a product of individual identifiers.*)
	"Denominator" -> <|"Prefix" -> "cfd", "Declaration" -> "Symbols", 
		"ValidExpression" -> (validDenominatorQ[#] &)|>
|>;


(* ::Text:: *)
(*Check that Kind is registered and Name consists of that kind's prefix followed by decimal digits. Lookup supplies defaults for absent keys. Short-circuit evaluation prevents access to an unknown kind. Expression content is validated separately during decoding.*)

(* ::Input::Initialization:: *)
validEntryNameQ[entry_Association] := 
	Module[{kind = Lookup[entry, "Kind", None], 
		name = Lookup[entry, "Name", None]},
			KeyExistsQ[$kindSpecs, kind] && StringQ[name] &&
				StringMatchQ[name, 
					RegularExpression[$kindSpecs[kind]["Prefix"] <> "[0-9]+"]]
	];


(* ::Text:: *)
(*Encode expressions as tagged JSON-compatible lists. symbolName returns Context[s] <> SymbolName[s], preserving the distinction between equal short names in different contexts. Its := definition evaluates the body on each call. SetDelayed does not clear obsolete definitions when the package is reloaded.*)

(* ::Input::Initialization:: *)
symbolName[s_Symbol] := Context[s] <> SymbolName[s];


(* ::Text:: *)
(*Store an integer as {"Integer", "digits"}, using intString for signed decimal text.*)

(* ::Input::Initialization:: *)
encode[n_Integer] := {"Integer", intString[n]};


(* ::Text:: *)
(*Store numerator and denominator as decimal strings, not nested integer records. For example, encode[2/3] returns {"Rational", "2", "3"}.*)

(* ::Input::Initialization:: *)
encode[n_Rational] := {"Rational", intString[Numerator[n]], 
   intString[Denominator[n]]};


(* ::Text:: *)
(*encode: complex numbers become {"Complex", re, im}, where re and im are themselves encoded values (not raw Ints). The head is written first, then the parts, giving a fixed positional schema that the decoder can rely on.*)

(* ::Input::Initialization:: *)
encode[z_Complex] := {"Complex", encode[Re[z]], encode[Im[z]]};


(* ::Text:: *)
(*Polarization labels identify independent vector identities, and the conjugation is part of that identity (phase I vs -I are different vectors, not a sign to be normalized away). The sole supported option is encoded as DATA (a nested list), never as a general Rule head, so the mapping file stays non-executable. A momentum label may be routed, i.e. be a sum of terms, but the polarization built on it is still ONE vector identity: polarization is not a linear function of momentum, so Polarization[p1 + p2, ...] must not be silently expanded into a sum of polarizations. This predicate defines the accepted momentum-label shapes. If p has head Plus, List @@ p splits it into its terms; otherwise the label is treated as a single term. Every term must match _Symbol (k), or a numeric multiple of a symbol Times[(_Integer | _Rational), _Symbol] (2 k, 3/2 k). So p1 + 2 p2 is a legal label while p1 . p2 or a bare number is not -- the check is purely syntactic on the label, not physical.*)

(* ::Input::Initialization:: *)
physicalMomentumLabelQ[p_] :=
	AllTrue[If[Head[p] === Plus, List @@ p, {p}],
		MatchQ[#, _Symbol | Times[(_Integer | _Rational), _Symbol]] &];


(* ::Text:: *)
(*polarizationQ[x]: the full validity test for one Polarization object. The pattern binds p (momentum label), phase (the conjugation), and opts___ (zero or more trailing option arguments). Three conditions, all required: 1. the momentum label passes physicalMomentumLabelQ; 2. the phase is literally I or -I; 3. the option sequence, wrapped as {opts}, is one of the three whitelisted forms -- {}, {Transversality -> True}, or {Transversality -> False}. Condition 3 is an exact, closed whitelist: any other option, a duplicate, or a different spelling makes the object invalid.*)

(* ::Input::Initialization:: *)
polarizationQ[Polarization[p_, phase_, opts___]] :=
	physicalMomentumLabelQ[p] && MemberQ[{I, -I}, phase] && 
		MemberQ[{{}, {Transversality -> True}, {Transversality -> False}}, {opts}];


(* ::Text:: *)
(*Catch-all: anything that is not a syntactically well-formed Polarization[...] is simply not a polarization. Without this, polarizationQ would stay unevaluated on other inputs and be neither True nor False, which would break the tests that use it as a boolean. This makes the predicate total.*)

(* ::Input::Initialization:: *)
polarizationQ[_] := False;


(* ::Text:: *)
(*vectorIdentityQ[x]: the definition of a valid "Vector" value, reused as the ValidExpression predicate in $kindSpecs. A vector identity is either a plain symbol standing for the vector, or a valid Polarization object. Note this is what makes polarization expressions acceptable anywhere a Vector kind is expected.*)

(* ::Input::Initialization:: *)
vectorIdentityQ[x_] := Head[x] === Symbol || polarizationQ[x];


(* ::Text:: *)
(*Encode the complete polarization identity and its optional transversality flag. The flag is stored as {"Transversality", "True"} or {"Transversality", "False"}, not as a general Rule. Invalid polarization input calls throwFailure immediately.*)

(* ::Input::Initialization:: *)
encode[x_Polarization] := If[polarizationQ[x],
	Join[{"Polarization", encode[x[[1]]], encode[x[[2]]]},
		If[ Length[x] === 3, {{"Transversality", 
			If[TrueQ[x[[3, 2]]], "True", "False"]}}, {}]],
		throwFailure["UnsupportedMomentum", 
	"Unsupported polarization vector identity."]];


(* ::Text:: *)
(*Encode a symbol as {"Symbol", "Context`name"}. This specialized definition handles symbol leaves; the generic definition below handles registered compound heads.*)

(* ::Input::Initialization:: *)
encode[s_Symbol] := {"Symbol", symbolName[s]};


(* ::Text:: *)
(*Look up the expression head in $expressionNameByHead, which maps heads to string tags. Reject unknown heads; otherwise encode each argument recursively and prepend the tag. HoldForm in the failure preserves the received expression for display, not its original unevaluated input.*)

(* ::Input::Initialization:: *)
encode[x_] := 
	Module[{name = Lookup[$expressionNameByHead, Head[x], Missing["Unknown"]]},
		If[MissingQ[name], 
			throwFailure["UnsupportedHead", 
				"Unsupported expression head.", <|"Expression" -> HoldForm[x], "Head" -> Head[x]|>]];
			Prepend[encode /@ (List @@ x), name]
   ];


(* ::Subsection:: *)
(*Restricted decoding and symbol validation*)


(* ::Text:: *)
(*integerData[s]: the decoder counterpart of encode[n_Integer], i.e. the function that turns a stored integer string back into a real Wolfram integer. Used while reading the non-executable mapping file. The first clause is specialized to String input, which is what the file format holds (encode writes intString output, and intString always returns a string). The pattern match on s_String means this clause fires only for strings; it is therefore a type guard as well as a validity check. The RegularExpression["-?[0-9]+"] accepts an optional leading minus followed by one or more decimal digits -- including leading zeroes that intString does not emit. StringMatchQ anchors the whole string, so "12abc" and "1 2" are rejected, not partially matched. If the shape is valid, the sign is handled explicitly: StringStartsQ[s, "-"] tests for the minus. If present: FromDigits[StringDrop[s, 1]] parses the digits AFTER the sign (they are unsigned after the drop, so FromDigits yields a non-negative integer), then the leading - negates it. If absent: FromDigits[s] parses the string directly. So "-7" -> -7 and "7" -> 7. FromDigits parses these strings in base ten. intString emits a minus sign only for negative integers; integerData handles that sign and also accepts unsigned decimal strings.*)

(* ::Input::Initialization:: *)
integerData[s_String] := If[StringMatchQ[s, RegularExpression["-?[0-9]+"]],
	If[StringStartsQ[s, "-"], -FromDigits[StringDrop[s, 1]], 
		FromDigits[s]],
		throwFailure["InvalidMapping", "Invalid integer in mapping."]];


(* ::Text:: *)
(*Catch-all clause for non-string input: if the file contains a JSON number (parsed as a Wolfram Integer or Real) or any other non-string value where an integer string was expected, this aborts with the same "InvalidMapping" failure. Note it does NOT coerce or repair the value -- integerData[3] fails rather than returning 3, so the round-trip is strict about representation, not just about value. Both clauses call the throwFailure[...] helper from the header, so both abort by throwing to the private $failureTag rather than returning a sentinel value.*)

(* ::Input::Initialization:: *)
integerData[_] := throwFailure["InvalidMapping", "Expected an integer string."];


(* ::Text:: *)
(*Decoding layer: the inverse of encode. Rebuilds Wolfram expressions from the tagged nested lists stored in the mapping file. Decoding contract: * Only symbol leaves may create a symbol. * Operator heads are never taken from the file -- they are chosen by lookup in $expressionHeadByName, so a string in the file can never become a head or be evaluated as Wolfram Language code. * That is NOT a sandbox. Restored symbols and whitelisted heads still undergo normal kernel evaluation, and this decoder does not isolate pre-existing symbol definitions. So decoding an expression that references a symbol with existing DownValues/ UpValues can still trigger that code. Require an explicit, nonempty context and name. Symbol itself validates Mathematica's complete symbol alphabet below; Unicode letter categories exclude valid letter-like characters such as EmptySet and DottedSquare. Symbol accepts a name, never source expressions. Do not use ToExpression.*)

(* ::Input::Initialization:: *)
qualifiedSymbolNameQ[s_String] := StringContainsQ[s, "`"] &&
  !StringStartsQ[s, "`"] && !StringEndsQ[s, "`"] &&
  !StringContainsQ[s, "``"];
qualifiedSymbolNameQ[_] := False;


(* ::Text:: *)
(*Decode a decimal integer string through integerData. Its accepted syntax includes leading zeroes and a leading minus; the exporter writes the canonical spelling.*)

(* ::Input::Initialization:: *)
decode[{"Integer", s_String}] := integerData[s];


(* ::Text:: *)
(*decode: rational leaf. The denominator is decoded FIRST and checked for zero before any division happens -- 1/0 must surface as a structured "InvalidMapping" failure rather than as a Power::infy message and ComplexInfinity. Only the numerator is decoded by integerData; the denominator value is reused for the division, so integerData[b] is evaluated exactly once.*)

(* ::Input::Initialization:: *)
decode[{"Rational", a_String, b_String}] := 
	Module[{den = integerData[b]},
		If[den == 0, throwFailure["InvalidMapping", "Zero rational denominator."]];
		integerData[a]/den
	];


(* ::Text:: *)
(*decode: complex leaf. The parts are decoded first, then re-validated with MatchQ to be sure they are exact numbers (Integer or Rational). This guards against a hand-edited file smuggling a symbol or a real into a coefficient position -- the Complex representation is only ever meant to carry exact parts. Reconstruction is re + I im, which evaluates to the canonical Complex number (and would silently combine/renormalize parts, e.g. re == 0 collapsing to a pure imaginary).*)

(* ::Input::Initialization:: *)
decode[{"Complex", a_, b_}] := Module[{re = decode[a], im = decode[b]},
	If[! 
		MatchQ[{re, im}, {(_Integer | _Rational), (_Integer | _Rational)}],
		throwFailure["InvalidMapping", 
		"Complex coefficients must be exact numbers."]];
		re + I im
	];


(* ::Text:: *)
(*Check context qualification, then let Symbol validate and reconstruct the name. Symbol does not interpret the string as source code. Existing definitions on the restored symbol can still evaluate; invalid names or messages during reconstruction become InvalidMapping.*)

(* ::Input::Initialization:: *)
decode[{"Symbol", s_String}] := If[qualifiedSymbolNameQ[s],
  Quiet[Check[Symbol[s],
    throwFailure["InvalidMapping", "Invalid fully qualified symbol name."]]],
  throwFailure["InvalidMapping", "Invalid fully qualified symbol name."]];


(* ::Text:: *)
(*decode: Polarization leaf. The three encoded parts -- momentum label, phase (conjugation), and an optional option record -- are decoded independently, then the same three conditions enforced by polarizationQ on the encoding side are re-checked on the way in: the momentum label must be a supported rational linear combination, the phase must be I or -I, and optionData must be one of exactly {}, {{"Transversality","True"}}, or {{"Transversality","False"}}. Note this is the DATA form (nested string pairs), not Wolfram Rules -- the file never contains Rule heads, and the Rule is created here, on our side of the boundary. opts___ captures the rest of the list, so a 3-element encoded polarization yields optionData === {} and a 4-element one yields a one-element list.*)

(* ::Input::Initialization:: *)
decode[{"Polarization", p_, phase_, opts___}] := 
	Module[{momentum = decode[p], label = decode[phase], optionData = {opts}},
		If[! physicalMomentumLabelQ[momentum] || ! 
			MemberQ[{I, -I}, label] || ! MemberQ[{{}, {{"Transversality", "True"}}, {{"Transversality", "False"}}}, optionData],
			throwFailure["InvalidMapping", "Invalid polarization vector identity."]];
		(*Validate, then rebuild: with no option data the short Polarization[momentum, label] form is restored; otherwise the Rule is reconstructed and the string "True"/"False" is mapped back to the boolean True/False. optionData[[1, 2]] is the second part of the inner pair (the stored string).*)
		If[optionData === {}, Polarization[momentum, label],
			Polarization[momentum, label, 
			Transversality -> (optionData[[1, 2]] === "True")]]
		];


(* ::Text:: *)
(*Accept only registered operator tags, validate their argument count, and recursively decode their arguments. For example, Power with three arguments fails. Applying the selected head performs ordinary evaluation, including existing definitions; Plus[2,3] becomes 5. Kind-specific validation follows in decodeEntries.*)

(* ::Input::Initialization:: *)
decode[{name_String, args___}] /; KeyExistsQ[$expressionHeadByName, name] := 
	Module[{a = {args}, h = $expressionHeadByName[name]},
		If[! arityQ[$expressionSpecs[name], Length[a]],
			throwFailure["InvalidMapping", "Invalid expression arity in mapping."]];
			h @@ (decode /@ a)
		];


(* ::Text:: *)
(*Reject unrecognized tags or malformed encoded shapes. Supported data is reconstructed using ordinary Wolfram evaluation; this fallback handles inputs that match none of the decoding definitions.*)

(* ::Input::Initialization:: *)
decode[_] := throwFailure["InvalidMapping", "Unsupported mapping data."];


(* ::Subsection:: *)
(*Lorentz dimensions*)


(* ::Text:: *)
(*Dimension inference. Note the two "space" readers deliberately do NOT look inside expressions: they read only the optional second argument of a Momentum or LorentzIndex object. Nothing is expanded, no indices are contracted, no structural algebra is attempted -- the dimension is metadata attached to those heads. space[Momentum[p]] -> 4, space[Momentum[p, d]] -> d. The pattern d_ : 4 is an optional pattern with a default, so the one-argument form binds d to 4 and the two-argument form binds the actual dimension. The first argument is ignored (_), so this works for routed/combined momenta too (Momentum[p1 + p2, d] still yields d). Note the pattern does NOT check that d is numeric or symbolic -- that validation happens later, in chooseDimension.*)

(* ::Input::Initialization:: *)
space[Momentum[_, d_ : 4]] := d;


(* ::Text:: *)
(*Same convention for LorentzIndex[index] -> 4 and LorentzIndex[index, d] -> d. Keeping both readers identical is what lets a single scan collect dimensions from either head.*)

(* ::Input::Initialization:: *)
space[LorentzIndex[_, d_ : 4]] := d;


(* ::Text:: *)
(*Choose one Lorentz space for the export. requested is the Dimension option: Automatic infers it from the expression, defaulting to D when there are no dimension-tagged objects. An explicit value must agree with the input.*)

(* ::Input::Initialization:: *)
chooseDimension[x_, requested_] := Module[{spaces, dim},
	(*Collect dimensions from Momentum and LorentzIndex objects at every level, including the whole expression at level 0. Heads are not searched. DeleteDuplicates preserves first-occurrence order. This structural scan performs no explicit expansion or contraction.*)
	spaces = 
		DeleteDuplicates[
			Cases[x, a : (_Momentum | _LorentzIndex) :> space[a], {0, 
			Infinity}]];
	(*More than one distinct dimension in one expression is not something the converter will guess about (e.g. 4-vectors mixed with d-dimensional ones), so it aborts with a structured failure carrying the offending dimensions as data.*)
	If[Length[spaces] > 1, 
		throwFailure["MixedDimensions", 
		"Mixed Lorentz spaces are not supported.", <|
		"Dimensions" -> spaces|>]];
	(*Three cases: requested === Automatic and nothing found -> default to the symbolic dimension D. requested === Automatic with exactly one found -> that one. requested explicit -> use it verbatim, and the following checks decide whether it is consistent with the input.*)
	dim =  If[requested === Automatic, If[spaces === {}, D, First[spaces]], requested];
	(*Accept a symbol or an integer of at least two dimensions. Reject other expressions and the imaginary unit I. In Wolfram Language, I evaluates to an exact Complex number; D is the usual symbolic dimension here.*)
	If[!MatchQ[dim, _Symbol | _Integer] || (IntegerQ[dim] && dim < 2) || dim === I,
		throwFailure["UnsupportedDimension", 
		"Use a symbolic dimension or an integer dimension of at least 2."]];
	(*Reject a requested dimension that differs structurally from the input dimension. UnsameQ (=!=) performs this comparison. No implicit dimension conversion is made.*)
	If[spaces =!= {} && First[spaces] =!= dim,
		throwFailure["DimensionMismatch", "The requested dimension differs from the input; no implicit dimension conversion is performed."]];
	(*Return the validated dimension; this is the value the rest of the converter threads through declarations, FORM code and the reconstruction path.*)
	dim
];


(* ::Subsection:: *)
(*Supported momenta and scalar expressions*)


(* ::Text:: *)
(*linearMomentumQ[Momentum[v, ...]]: is the momentum argument a linear combination of vector identities with exact rational coefficients? The first argument v is what matters; the trailing pattern ___ swallows any remaining arguments (the optional dimension) so that Momentum[k], Momentum[k, d] and routed momenta are all classified the same way. This is the label-shape test used by scalarQ below. If v is a Plus, List @@ v splits it into its terms; otherwise v is treated as a single term. Each term must be either * a vector identity (a bare symbol, or a Polarization object), or * an exact rational multiple of one: Times[n, vec] with n an Integer or Rational and vec again passing vectorIdentityQ. Note ?vectorIdentityQ must hold, so a numeric multiple of something that is not a vector identity (say Times[2, Pair[...]]) is refused. The predicate is structural: it never expands the sum or contracts anything.*)

(* ::Input::Initialization:: *)
linearMomentumQ[Momentum[v_, ___]] := 
	AllTrue[If[Head[v] === Plus, List @@ v, {v}],
		(vectorIdentityQ[#] || MatchQ[#, Times[(_Integer | _Rational), _?vectorIdentityQ]]) &];


(* ::Text:: *)
(*Catch-all so the predicate is total: anything that is not a Momentum object is simply not a linear momentum, rather than staying unevaluated and breaking the boolean logic in scalarQ.*)

(* ::Input::Initialization:: *)
linearMomentumQ[_] := False;


(* ::Text:: *)
(*scalarQ recognizes the supported exact scalar vocabulary. It validates masses, scalar abbreviations and master arguments. Acceptance as a scalar does not make the expression opaque: emit separately decides whether to serialize it directly or assign an abbreviation.*)

(* ::Input::Initialization:: *)
scalarQ[x_] := Which[
	(*Exact numbers first: integers and rationals are always scalars. This also covers exact integers that arrive as e.g. 2 from a collapsed expression.*)
	MatchQ[x, _Integer | _Rational], True,
	(*Complex coefficients must have exact integer or rational real and imaginary parts. Machine-precision complex numbers fail. A symbolic sum such as a + I is handled recursively by the Plus branch.*)
	Head[x] === Complex, 
	MatchQ[{Re[x], Im[x]}, {(_Integer | _Rational), (_Integer | _Rational)}],
	(*Accept scalar symbols except indeterminate or infinite quantities. Pi and E remain mapped symbols. The imaginary unit I has head Complex and is handled by the exact-complex branch above.*)
	Head[x] === Symbol, ! MemberQ[{Indeterminate, Infinity, ComplexInfinity}, x],
    Head[x] === SMP, namedCouplingQ[x],
	(*Accept scalar products of supported linear momenta. Direct scalar products are emitted as native FORM vector dots; scalar products inside an opaque abbreviation remain in that abbreviation's mapping definition.*)
	MatchQ[x, Pair[_Momentum, _Momentum]], AllTrue[List @@ x, linearMomentumQ],
	(*Compound exact expressions: a Plus (sum of scalars) or Times (product of scalars, including the coefficient times scalar case) is scalar if ALL of its arguments are scalars. This is the recursion that walks sums and products to their leaves, and it is also what lets e.g. Times[2, Pair[...]] be handled.*)
	MemberQ[{Plus, Times}, Head[x]], AllTrue[List @@ x, scalarQ],
	(*Powers: base must be scalar and the exponent must be an exact Integer or Rational. So a^2 and a^(1/2) pass, while a^b, a^2.5 and a^(1 + I) do not -- symbolic exponents are deliberately out of scope for version 1.*)
	Head[x] === Power, scalarQ[x[[1]]] && MatchQ[x[[2]], _Integer | _Rational],
	(*Master integrals: if the head is one of the registered masters (A0, B0, C0, D0, via $masterSpecByHead from the spec table), delegate to masterArgumentsQ, which checks the arity range AND that every argument is itself scalar. This is where scalarQ and the $expressionSpecs table become mutually recursive: masters are scalars when their arguments are scalars.*)
	KeyExistsQ[$masterSpecByHead, Head[x]], masterArgumentsQ[$masterSpecByHead[Head[x]], List @@ x],
	(*Default: anything not covered -- inexact reals, strings, lists, graphics, unknown heads, other FeynCalc objects -- is NOT a scalar. Being a total predicate returning False (rather than staying unevaluated) is what makes it usable inside AllTrue and as a validation gate that must produce a clear failure.*)
	True, False
];


(* ::Text:: *)
(*Shared propagator vocabulary. Routing may also contain sums of already dimension-tagged Momentum objects, as produced by FeynCalc evaluation.*)

(* ::Input::Initialization:: *)
propagatorRoutingQ[m_Momentum] := linearMomentumQ[m] && FreeQ[m, _Polarization];
propagatorRoutingQ[x_Plus] := AllTrue[List @@ x, propagatorRoutingQ];
propagatorRoutingQ[Times[c : (_Integer | _Rational), m_Momentum]] := propagatorRoutingQ[m];
propagatorRoutingQ[_] := False;
validDenominatorQ[FeynAmpDenominator[PropagatorDenominator[mom_, mass_:0]]] :=
  propagatorRoutingQ[mom] && scalarQ[mass];
validDenominatorQ[_] := False;


(* ::Text:: *)
(*Check authoritative expression dimensions, not descriptive metadata.*)

(* ::Input::Initialization:: *)
consistentMappedDimensionQ[x_, dim_] := AllTrue[
  Cases[x, a : (_Momentum | _LorentzIndex) :> space[a], {0, Infinity}],
  SameQ[#, dim] &];


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
	stageEnds, stageRanges,
	stageTexts, stageExpressions, stageSignatures, stageOrder,
	preparations = {}, multiplications = {}, cancellationData},
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
		(*Build the text of each stage as a parenthesized product, and the corresponding expression, so the planner can inspect indices.*)
		stageTexts = ("(" <> StringRiffle[Take[factorTexts, #], "*"] <> ")") & /@ stageRanges;
		stageExpressions = (Times @@ Take[factorExpressions, #]) & /@ 
		stageRanges;
		(*Index signatures drive the ordering; if there are no indices at all, use empty signatures (which makes the planner fall back to the original order).*)
		stageSignatures = If[FreeQ[stageExpressions, _LorentzIndex],
			ConstantArray[<||>, Length[stageExpressions]], 
			stageIndexSignatures[stageExpressions]];
		stageOrder = connectedStageOrder[stageExpressions, stageSignatures];
		stageTexts = stageTexts[[stageOrder]];
		(*Normalize large, unambiguous tensor stages once in FORM before reuse. The configurable leaf-count cutoff defaults to 1024. Hidden cfcStageN factors remain available until the program ends. Ambiguous signatures bypass preparation even when multiplication order is unchanged.*)
        If[ stageSignatures =!= $Failed && ! FreeQ[stageExpressions, _LorentzIndex] && Max[LeafCount /@ stageExpressions] >= $stagePreparationMinimumLeaves,preparations = stageTexts;
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
        <|"DimensionName" -> dimensionName, "Body" -> body, "Multiplications" -> multiplications,"Factors" -> macros, "Preparations" -> preparations, "Mapping" -> payload, "CancellationData" -> cancellationData|>
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
    program, templateNames, unknownTemplateNames, rationalPlan, cancellationPlan, dimensionPlan},
     (*The template contract is enforced HERE, not only where the shipped template is read, because this function is the one that accepts an arbitrary template. Without this check a template missing a placeholder would silently render a program that omits a declaration or directive -- a FORM-level failure far from its cause. Required and unknown placeholders are rejected against the original template before any values are inserted.*)
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
     dimensionPlan = dcfPlan[data];
     cancellationPlan = pcPlan[data];
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
       "CFunctions " <> StringRiffle[Keys[$masterSpecByFORMName], ","] <> ";" <> daDeclarations[data] <> caDeclarations[data] <> Lookup[rationalPlan, "Declarations", ""] <> Lookup[cancellationPlan, "Declarations", ""] <> Lookup[dimensionPlan, "Declarations", ""],
          (*Declarations grouped by class, driven by $kindSpecs. Note "Symbols" covers Scalars, Abbreviations and Denominators -- they share a FORM class by design.*)
          "@SCALARS@" -> declaration["Symbols"], 
      "@DIMENSION@" -> data["DimensionName"],
          "@VECTORS@" -> declaration["Vectors"], 
      "@INDICES@" -> declaration["Indices"],
          (*The #define macros, one per line.*)
          "@FACTORS@" -> StringRiffle[data["Factors"], "\n"], 
      "@EXPRESSION@" -> data["Body"],
          (*Pre-prepared large tensor stages. When empty, the whole substitution is the empty string so no stray directives appear. Otherwise: one "Local cfcStageN = <stage>;" per preparation (MapIndexed supplies the 1-based counter), then a single ".sort" and "Hide cfcStage1,cfcStage2,...;" so FORM can reuse the normalized stages later.*)
          "@PREPARATIONS@" -> If[data["Preparations"] === {}, "",
              
        StringJoin[
          MapIndexed[("Local cfcStage" <> intString[First[#2]] <> 
              " = " <> #1 <> ";\n") &,
                   data["Preparations"]]] <> ".sort\nHide " <>
               
         StringRiffle[
          Table["cfcStage" <> intString[i], {i, 
            Length[data["Preparations"]]}], ","] <> ";\n"],
          (*Remaining multiplication stages: each is ".sort" then "Multiply <stage>;", i.e. the planned order becomes the order of FORM operations.*)
          "@MULTIPLICATIONS@" -> 
       StringJoin[(".sort\nMultiply " <> # <> ";\n") & /@ 
         data["Multiplications"]] <> caProcessing[data] <> If[KeyExistsQ[data["Mapping"], "EpsilonConvention"], "contract 0;\n", ""] <> daProcessing[data] <> pcDirectProcessing[data] <> Lookup[cancellationPlan, "Processing", ""],
          "@GROUPING@" -> With[{denominators = Lookup[Select[entries, #["Kind"] === "Denominator" &], "Name", {}]},
              If[denominators === {}, "", If[pgFactorisationQ[data], "Bracket+ ", "Bracket "] <> StringRiffle[denominators, ","] <> ";"]],
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
     renderExport[data, paths["ResultFile"], readProgramTemplate[]];
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
     $importClassCache = <||>, $importFactorBytes = 0, $importClassBytes = 0,
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
    If[KeyExistsQ[$importFactorCache, t], Return[$importFactorCache[t]]];
    value = parseGeneralResult[t, $importParserValues, $importParserDimension];
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

parseGeneralResult[text_String, values_Association, dim_] := Module[
     {tokens, pos = 1, peek, take, expect, atom, power, unary, 
    product, sum,
       scalarValue, entryValue, call, dot, result, tokenPattern, 
    stripped, classes,
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
     stripped = StringReplace[text, WhitespaceCharacter -> ""];
     tokens = StringCases[text, tokenPattern];
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
     values},
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
             !MemberQ[{1, 2, 3, 4, 5}, Lookup[mapping, "Version", None]],
           throwFailure["InvalidMapping", "Unsupported mapping format or version."]];
        grouped = Lookup[mapping, "ResultLayout", None] === "PropagatorGroups";
        If[KeyExistsQ[mapping, "ResultLayout"] && !grouped,
            throwFailure["InvalidMapping", "Unknown result layout."]];
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
        If[mapping["Version"] < 3 && AnyTrue[entries,
            MemberQ[{"ColourTensor", "ColourWord", "NamedCoupling"}, Lookup[#, "Kind", None]] &],
            throwFailure["InvalidMapping", "Dirac/colour entries require version three."]];
        If[mapping["Version"] < 5 && AnyTrue[entries, MemberQ[$caKinds, Lookup[#, "Kind", None]] &],
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
        values = decodeEntries[entries, dim];
        (*Hand the result text (everything after the marker line) to the parser together with the symbol table and dimension.*)
        Block[{$importParserActive = False, $caImport = <||>, $epsilonImportFactor = None, $diracImportLine = None, $daImportLines = <||>, $daImportMode = False},
            If[mapping["Version"] >= 3,
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
            If[mapping["Version"] >= 4, daValidateMetadata[mapping, entries]];
            If[mapping["Version"] === 5, caValidateMetadata[mapping, entries]];
            If[grouped,
                Return[pgImport[resultFile, mapping, digest, values, dim]]];
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


(* ::Section:: *)
(*Load companion definitions without executing processes*)


(* ::Text:: *)
(*Load the private Dirac/colour and runtime definitions from the same directory as this package. $moduleDirectory was computed in the header with DirectoryName[$InputFileName], so this resolves relative to the FILE rather than to Directory[] (the current working directory) -- which is what makes the package relocatable and independent of how the caller's session happens to be positioned.*)

(* ::Input::Initialization:: *)
Get[FileNameJoin[{$moduleDirectory, "DiracColour.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "DiracAlgebra.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "ColourAlgebra.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "CoefficientParser.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "PropagatorGroups.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "RationalCoefficients.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "DimensionCoefficients.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "PropagatorCancellation.wl"}]];
Get[FileNameJoin[{$moduleDirectory, "FORMRuntime.wl"}]];


(* ::Text:: *)
(*The companion file supplies definitions in the current private context without launching processes. Reloading evaluates those definitions again. There is no structured missing-runtime error at this boundary: an unreadable file produces Get's own diagnostic. Restart the kernel after package updates to avoid retaining obsolete definitions.*)


(* ::Section:: *)
(*Close the package*)


(* ::Input::Initialization:: *)
End[];
EndPackage[];
