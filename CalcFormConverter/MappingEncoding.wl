(* Mapping encoding and structural validation: private companion to CalcFormConverter.

   Loaded inside the converter's private context by CalcFormConverter.wl. It
   contains the supported-head tables, reversible symbol/mapping encoding and
   decoding, dimension selection and structural validation moved unchanged
   from the central converter in ARCH-01 Stage B. It launches no processes. *)

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
    MemberQ[{CalcFormConverter`LoopTensorCoefficient, PaVe}, Head[x]], tdCoefficientQ[x],
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


(* Central read-only interpretation of the saved mapping capability contract.
   The identity gate and smValidateMapping remain at the import boundary; this
   helper performs the same algebra-version validation and grouped-layout gate
   once, then exposes only validated facts. It preserves every historical
   version branch by delegating version 6/7/8 interpretation to tdAlgebraMapping
   and leaving version-specific entry checks at their existing call sites. *)
mappingCapabilities[mapping_Association] := Module[
    {version, algebraMapping, grouped, layout},
    version = Lookup[mapping, "Version", None];
    If[!MemberQ[Range[8], version],
        throwFailure["InvalidMapping", "Unsupported mapping format or version."]];
    algebraMapping = tdAlgebraMapping[mapping];
    layout = Lookup[mapping, "ResultLayout", None];
    grouped = MemberQ[{"PropagatorGroups", "IntegralCoefficientGroups"}, layout];
    If[KeyExistsQ[mapping, "ResultLayout"] && !grouped,
        throwFailure["InvalidMapping", "Unknown result layout."]];
    <|"Version" -> version, "AlgebraMapping" -> algebraMapping,
      "AlgebraVersion" -> algebraMapping["Version"],
      "Grouped" -> grouped, "ResultLayout" -> layout|>
];


