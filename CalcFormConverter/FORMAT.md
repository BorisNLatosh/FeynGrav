# Persisted formats, versions 1 through 8

This document describes the JSON mapping and dedicated FORM result consumed by `CalcFormImport`. It is a compatibility contract for saved calculations. Internal helper associations returned by `buildExportData` and `renderExport` are private implementation details and are not this file format.

## Version and correspondence

The mapping contains `"Format": "CalcFormConverter"` and an integer `"Version"` from 1 through 8. The result begins with `CFC<version> <mapping-digest>`, followed by the FORM expression. For example, a **version-one** result begins with:

```text
CFC1 <mapping-digest>
<FORM expression>
```

The marker is derived from the format version in the implementation. The importer accepts versions 1 through 8; unsupported versions produce `Failure["InvalidMapping", ...]`. Changing the meaning of existing fields, symbol encoding or result grammar requires an explicit compatibility decision and, when incompatible, a new format version. Optional descriptive fields may be added without changing existing meanings.

The mapping is read as UTF-8 text. Its primary digest is Wolfram Language `Hash[jsonText, "SHA256", "HexString"]`. For compatibility with early notebook exports, the importer also accepts the digest of `FromCharacterCode[ToCharacterCode[jsonText, "UTF-8"]]`: those exports hashed the UTF-8 byte string before writing Unicode text. Both checks bind the complete mapping. These are Wolfram string hashes, not a specification to hash arbitrary raw file bytes with an external utility. Even JSON reformatting changes the digest. Preserve the mapping file with its result. This check detects mismatched artifacts, not deliberate tampering.

For the user workflow and commands, see [README.md](README.md); implementation invariants and tests are in [DEVELOPER.md](DEVELOPER.md).


### Version selection

Choose the highest applicable row below; a newer feature can coexist with and retain older convention metadata.

| Version | Export condition | Additional contract |
| --- | --- | --- |
| 1 | None of the newer structures below | Original tensor/scalar mapping and compatible polarisation identities |
| 2 | Epsilon tensors without a higher-version feature | Captured epsilon convention and translation factor |
| 3 | Dirac/colour translation without processing or explicit traces requiring later formats | Typed words and reserved open spin line |
| 4 | Dirac processing or explicit Dirac traces, without version-five colour features | Processing mode and independent trace spin lines |
| 5 | Processed colour structures or supported colour traces | Colour conventions, processing mode, typed indices/endpoints and procedure identity |
| 6 | Active one-loop tensor decomposition | Integral conventions, ordered families and grouped coefficient output; AlgebraVersion records the underlying version 1–5 capabilities |
| 7 | Reduction of existing loop coefficients | Existing coefficient normalisation; external factors are not integrated; underlying algebra capabilities remain separate |
| 8 | Active scalar-master reduction | Source interpretation, reduction conventions and generic denominator restrictions; underlying algebra capabilities remain separate |

Ordinary epsilon-free scalar/tensor exports remain version one when tensor decomposition is inactive. Version selection follows structures present in the input, not just an enabled option. See the version-specific sections below for exact fields and validation. Existing versions are still importable; a documentation reorganisation does not change their meaning.

## Mapping fields

The table describes common fields, using version one as the baseline. Later sections add version-specific fields and processing modes.

| Field | Meaning | Import requirement |
| --- | --- | --- |
| `Format` | Fixed format name above | Required and checked |
| `Version` | Integer format version | Required and checked |
| `Dimension` | Encoded symbolic or integer Lorentz dimension | Required; a symbol other than `I`, or integer at least 2 |
| `Entries` | Array of mapping entries | Required and validated |
| `ExpressionDigest` | `Hash[FCI[input], "SHA256", "HexString"]` | Always exported; participates in mapping digest, not otherwise interpreted |
| `LoopMomenta` | Array of encoded momentum symbols supplied by the user | Always exported; informational to the current importer |
| `ResultLayout` | Optional `"PropagatorGroups"` for grouped results; absent in legacy files and denominator-free exports | Checked when present; selects incremental input and requires denominator entries |
| `Processing` | `"TensorAlgebraOnly"` for version one; later versions record the enabled algebra mode | Always exported; informational to the current importer |

`ExpressionDigest` differentiates exports of different expressions even if they use exactly the same symbol dictionary. It is not a serialised expression or an independent proof of the FORM calculation. Future procedures must define any additional use of `LoopMomenta` or `Processing` explicitly.

Each entry requires `Name`, `Kind` and `Expression`. Names are unique within a mapping. Export numbering starts at one within each kind and follows traversal order. The importer accepts a kind's prefix followed by digits. Numbering is local to each export; consumers must not assume, for example, that `cfi1` always means alpha.

| Kind | FORM prefix | Decoded expression |
| --- | --- | --- |
| `Scalar` | `cfs` | Mathematica scalar symbol |
| `Vector` | `cfv` | Momentum symbol or supported `Polarization` identity, without a `Momentum` wrapper |
| `Index` | `cfi` | Lorentz index symbol, without a `LorentzIndex` wrapper |
| `Abbreviation` | `cfa` | Supported scalar expression |
| `Denominator` | `cfd` | `FeynAmpDenominator[PropagatorDenominator[...]]` |

A symbol used in different roles has distinct mapping entries. Symbol names retain their complete Mathematica contexts and Unicode names. Native tensor objects are reconstructed using the mapping's `Dimension`.

### Denominator authority and multiplicity

A denominator entry's **`Expression` is authoritative for reconstruction**. The exporter also records:

- `Momentum`: encoded routing from the `PropagatorDenominator`.
- `Mass`: encoded mass, with zero for an omitted mass.
- `Power`: integer 1, the power represented by this single identifier.
- `Dimension`: encoded Lorentz dimension.
- `Prescription`: `"Feynman+i0"` for ordinary quadratic FeynCalc denominators.

These fields are convenience metadata for a future reducer. The current importer neither uses them to override `Expression` nor checks their consistency. The importer validates the authoritative propagator routing and scalar mass, and checks dimensions within restored expressions against the mapping dimension. These checks also cover unused entries. A future consumer must derive its data from `Expression` or validate the redundant metadata before using it. Repeated propagators appear as repeated factors or powers of the identifier in the expression; `Power` is not their total multiplicity in a term or diagram.

## Restricted Mathematica expression encoding

All encoded expressions are JSON arrays. Exact leaf forms are:

```text
["Integer", "-7"]
["Rational", "-7", "13"]
["Complex", <real exact number>, <imaginary exact number>]
["Symbol", "Global`α"]
```

Integer strings are signed decimal values. Rational denominators must be nonzero. Complex components are encoded integers or rationals. Symbol strings must be valid fully qualified names.

Other nodes are `[headName, argument1, ...]`, with recursively encoded arguments:

| Heads | Allowed argument count |
| --- | --- |
| `Plus`, `Times` | Zero or more |
| `Power`, `Pair` | Two |
| `Momentum`, `LorentzIndex`, `PropagatorDenominator` | One or two |
| `FeynAmpDenominator` | One or more |
| `A0`, `B0`, `C0`, `D0` | One, three, six, ten respectively |

This is data, not Wolfram Language source. Arbitrary heads, assignments and executable strings are not accepted. Normal Wolfram evaluation still applies to restored symbols, so symbolic work should use unassigned symbols.

## FORM result grammar

After the header, the result is a single expression with optional whitespace and line wrapping. Supported forms are exact integers, declared scalar identifiers, `i_`, arithmetic `+ - * /`, parentheses and integer powers. Vector dots use `cfv1.cfv2`; components use `cfv1(cfi1)`; metrics use `d_(cfi1,cfi2)`. Scalar master functions use `cfA0`, `cfB0`, `cfC0`, `cfD0` with their documented scalar argument counts. Bare vectors/indices in scalar arithmetic, unknown identifiers, trailing statements and undefined division/powers of zero are rejected.

FORM output performs algebra and contractions, so the result need not retain the input factorisation. New results with denominator entries are grouped by complete mapped propagator products as described below; legacy results retain their existing interpretation. Opaque scalar abbreviations and denominator identifiers retain their definitions through the mapping.

### Tokenisation and precedence

The result body is tokenised into ASCII identifiers (`[A-Za-z][A-Za-z0-9_]*`), decimal digit sequences, and `+ - * / ^ ( ) , .`. Whitespace may separate tokens; it cannot hide other characters. There is no implicit multiplication, decimal/scientific notation, assignment, semicolon terminator, string literal or comment syntax. Use the dedicated `.out` file, not the FORM console transcript.

From lowest to highest, parsing handles sums, products/division, unary signs, integer powers and vector dots/atoms. Division chains are left associative: `24/3/2` gives `4`. An exponent is one signed integer, optionally parenthesized, such as `x^-2` or `x^(-2)`; arbitrary exponent expressions and chained powers are rejected. Unary signs precede a power expression, so `-2^2` is `-4`, whereas `(-2)^2` is `4`.

Identifiers must be declared in the mapping, except `i_` and the reserved call names. Function argument counts and scalar/vector/index roles are checked before reconstruction. A dot requires two vector values, a component requires one index, and `d_` requires two indices. Bare typed values cannot be cancelled into apparent scalars: `0*cfv1`, `cfv1-cfv1`, and `cfi1^0` fail. Unknown identifiers are rejected even in terms that would vanish. Division by zero and zero to a nonpositive power fail. Nested reciprocal operands are checked from the inside out before evaluating an enclosing denominator, including inside a zero-multiplied term; the optimised path must retain the general parser’s failure and message behaviour.

All mapping expressions are decoded and checked, including entries unused by the result. A valid digest does not bypass grammar, entry-kind, dimension or expression validation. Conversely, the digest authenticates no sender and proves no mathematical calculation. Restored symbols and whitelisted heads undergo normal Wolfram evaluation in the receiving kernel; this data format does not isolate pre-existing kernel definitions.

## Compatibility fixture

`Tests/Fixtures/v1.map.json` and `v1.out` were generated by the original version-one exporter and FORM 4.3 before the maintainability refactor. The fixed expected expression is checked in `Tests/Core.wls`. The tests must not regenerate these files: doing so would allow exporter and importer to change incompatibly together without detecting the break.

### Polarisation vector extension

Vector entries also accept `["Polarisation", <encoded exact rational linear momentum label>,
<encoded exact I or -I>]`, optionally followed by the literal data array
`["Transversality", "True"]` or `["Transversality", "False"]`. The option data
is decoded only inside this constrained node; general `Rule` expressions are
not accepted. The full identity, conjugation label and explicit option survive
reconstruction inside `Momentum`, using the mapping's Lorentz dimension.
The momentum label consists only of symbols, rational multiples of symbols
and their sums; nested polarisations, nonlinear products and arbitrary heads
are rejected. Polarisation is also accepted inside supported scalar abbreviations.

This is an additive version-one vocabulary extension. Existing version-one
artifacts remain readable and the fixed compatibility fixture is unchanged.
Older converter revisions reject new polarisation entries; new exports using
this extension require an importer with polarisation support. Ordinary symbol
vector entries retain their original encoding.


## Version two: Lorentz epsilon tensors

Epsilon-containing exports without Dirac/colour structures use Version 2 and marker `CFC2`. Existing fields,
digest binding and identifier dictionaries retain their meanings. The required
`EpsilonConvention` association contains encoded `Sign` and `ExportFactor` values.
`Sign` must be one of -1, 1, -I or I; `ExportFactor` must equal `-I Sign`.
The output grammar additionally accepts `e_(a,b,c,d)`, with four mapped index
or vector identifiers. Version-one output cannot introduce this function.

Export multiplies each native epsilon by ExportFactor; import divides each
surviving epsilon by that factor. Real FORM tests show the raw fully contracted
rank-four square is D(D-1)(D-2)(D-3), so the factor supplies FeynCalc's
`-$LeviCivitaSign^2` contraction coefficient. This corrects the originally proposed
factor `-Sign`, which failed the real-process sign tests.

Import validates convention metadata and its agreement with the current FeynCalc
setting before parsing, including scalar-only results. Invalid metadata returns
InvalidMapping; a different current setting returns EpsilonConventionMismatch.


## Version three: Dirac and colour translation

Translation-only gamma/colour jobs without explicit traces use Version 3 and marker `CFC3`. The required `DiracSpinLine`
is the integer 1; this line is reserved even for colour-only jobs. The required
boolean `EpsilonPresent` must agree with the presence of `EpsilonConvention`. The epsilon
convention association is required when epsilon tensors occur and retains its
version-two meaning. Its presence selects epsilon pair contraction in FORM.

Additional entry kinds, all declared as FORM Symbols:

| Kind | Prefix | Validated value |
|---|---|---|
| ColourTensor | cfct | SUNF, SUND, SUNDelta, SUNFDelta or SUNTF with typed symbolic indices |
| ColourWord | cfcw | Complete ordered nonempty list of adjoint labels, encoded with ColourWord |
| NamedCoupling | cfcp | SMP with one string argument |

Encoded colour objects use SUNIndex and SUNFIndex tags. ColourIndexList encodes
ordered lists, including the generator list inside SUNTF. NamedCoupling holds
one string as data; it is never interpreted as Wolfram source. Lists and these
heads do not become generally accepted scalar expressions.

The general output parser accepts `g_(1,...)` with one or more mapped Lorentz
indices/vectors and `gi_(1)` for the identity. Special gamma codes and other
spin lines are rejected. Gamma results and ColourWord entries remain typed
through arithmetic validation before reconstruction as ordered Dot products.
Multiple implicit words in the same space in a result product, and matrix
powers/denominators, are invalid. No new forms enter the flat parser grammar.

Scalar colour constants remain ordinary mapped symbols. Colour structures in
FORM are opaque complete objects: this format performs no SU(N) algebra and
preserves the input's normalisation. Older versions cannot introduce the new
entry kinds or native gamma result calls.


## Version four: configured Dirac processing and explicit traces

Version 4 (`CFC4`) retains version-three entries, `DiracSpinLine -> 1`, and
`EpsilonPresent`/`EpsilonConvention`. It adds required fields:

- `DiracAlgebra`: the string `"Automatic"` or `"False"`.
- `TraceLines`: an ordered list of associations with integer `Line` values
  consecutively numbered from 2, and encoded scalar `TraceOfOne` values.
- `GammaOrdering`: every mapped Index/Vector name in the canonical ordering.
  The importer independently derives and checks this list.

`Processing` is `LorentzAndDiracAlgebra` for automatic version-four jobs and
`TensorAlgebraOnly` for translation-only jobs. Normalisation is per trace;
FORM uses unit trace 1 and the exporter supplies each recorded scalar factor.
The open line is never traced. Independent occurrences, including powers,
receive distinct lines even when their expressions are identical.

With automatic processing only line 1 may survive in native `g_`/`gi_` output.
An unevaluated declared trace line is an invalid result. In translation-only
mode a declared trace line reconstructs as `DiracTrace[..., TraceOfOne -> n,
DiracTraceEvaluate -> False]`. It remains scalar with respect to the open
matrix space. Trace-line identity is retained until validation: repeated use
of one line in a product and powers/denominators of an unprocessed native
trace word are rejected. Unknown lines, malformed gamma arguments and internal
`cfcOpen` objects are invalid. No new syntax is added to the flat parser.

Gamma argument ordering is by `{Kind, compact JSON encoding of Expression}`:
indices precede vectors, with complete symbol contexts retained. This is
independent of dictionary allocation order. No gamma-five identities or
finite-dimensional basis reduction are implied. Import never reruns the
FORM algebra and does not consult the current trace-normalisation default.


## Version five: SU(N) colour processing

Version 5 (`CFC5`) is selected for automatic processing of any supported colour
object/constant, or for `SUNTrace` with either colour setting. All version-four
Dirac fields and applicable epsilon metadata remain required. Unaffected jobs
retain their previous version selection.

The required `Colour` association contains:

| Field | Contract |
| --- | --- |
| `Mode` | `"Automatic"` or `"False"` |
| `Group`, `Representation` | `"SU(N)"`, `"Fundamental"` |
| `TraceNormalisation`, `Flavours` | `[1,2]`, `1` |
| `Procedure` | `"CFC-SUn-1"` when processed, otherwise `"None"` |
| `UpstreamSHA256` | Pinned SHA-256 of the unchanged SUn.prc reference |
| `Rank` | Scalar dictionary identifier whose decoded value is SUNN; `"None"` when preserved |
| `IndexDictionary` | All colour-index entry identifiers, in registry order |
| `ImplicitEndpoints` | Empty or two distinct declared fundamental identifiers, left then right |
| `GeneratedNamespace` | ``CalcFormConverter`ColourIndices`h<64 lowercase hex digits>` ``; empty when preserved |

New entry kinds are `ColourAdjointIndex` (`cfcaN`, encoded `SUNIndex`),
`ColourFundamentalIndex` (`cfcqN`, encoded `SUNFIndex`) and `ColourTrace` (`cfcrN`).
The latter is an opaque, validated trace body used only in preserving mode;
its expression tag `ColourTraceBody` wraps normalised sums/products and ordered
`ColourWord` entries. It cannot contain nested traces, Dirac words or arbitrary
function heads. Automatic mode rejects opaque colour entries. Duplicate colour
identities and inconsistent endpoint declarations are rejected.

Processed output calls are `cfcCT(adjoint...,fundamental,fundamental)`,
`cfcCTr(adjoint...)` with at least four arguments, `cfcCF` and `cfcCD` with three
adjoint arguments, and native `d_` with two indices in the same space.
Indices are typed until each complete call is validated. Colour tensor
inverses, leaked temporary objects, wrong spaces and undeclared names fail.

FORM-generated `N<positive integer>_?` tokens have a separate restricted path,
active only in automatic version-five imports. They decode to generated
fundamental dummy indices and must each occur twice in every term where used.
No arbitrary unknown identifier is accepted. Generated free endpoints must
occur once per nonzero term. Endpoint connectivity determines whether the
whole result can return to implicit words or must remain explicit.

`Processing` is `LorentzAndColourAlgebra` or `LorentzDiracAndColourAlgebra` for
processed colour jobs; preserving jobs retain the previous descriptions.
Only the general parser is extended. Caches and colour metadata are scoped to
one import; the flat parser and saved versions one through four are unchanged.

The public option `ColourAlgebra -> True` is normalised to the existing enabled
mode. Version-five `Colour.Mode` remains `"Automatic"` or `"False"`; this option
alias does not introduce another mapping version.

## Grouped propagator results

New exports containing `Denominator` entries add `"ResultLayout": "PropagatorGroups"`. This field is covered by the mapping digest. Version selection, identifier meanings and the expression grammar remain unchanged; the layout adds no wrapper functions. Files without this field use the legacy reader. Unknown layout values are rejected.

After all configured algebra, the generated program brackets every mapped denominator identifier and sorts before `%E` output. Each group is a denominator monomial multiplying a parenthesised coefficient. All powers belong to the monomial. Several additive terms without denominators may appear at the top level; their prefactor is one. Zero is a valid complete result.

The grouped reader validates the marker/digest, mapped entries and convention metadata before parsing the body. It reads fixed-size byte chunks (FORM identifiers and grammar are ASCII), preserving whitespace and nesting across boundaries. It accepts the existing restricted algebraic grammar, checks each parsed group's prefactor and rejects propagators nested inside a coefficient. Repeated prefactors are combined after parsing. It never evaluates file contents as Wolfram source.

Colour connectivity is checked on the additive coefficient terms. The choice between implicit words and explicit generated endpoints is made for the complete result before reconstruction; one group cannot silently use a different endpoint convention from another. Existing Casimir presentation is applied within coefficients. Final multiplication by reconstructed propagators does not distribute their coefficients.

This is a storage/reconstruction layout, not denominator reduction. It does not promise that every older importer implementation understands the optimised reconstruction path, although the mathematical expression syntax has not changed. The current importer continues to accept saved versions one through five without layout metadata.


### Factorised coefficients

Eligible version-one grouped exports use `Bracket+` for indexed access to complete denominator products. Batches contain up to four coefficients per worker. For coefficients with no free Lorentz indices and at most 20,000 expanded terms, `content_` extracts a common numerical/monomial factor. Scalar function factors are excluded from the extracted content. Direct monomial division preserves negative powers. This remains the fallback for coefficients not selected for the bounded rational procedure. Output is an ordinary product of parenthesised content and residual sum, in the original propagator-group order. Within the residual, native brackets collect terms with the same momentum monomial, so nested sums and products are expected. All registered vectors supply the bracket list; if the list is empty this additional bracket is omitted. Free components may also appear in that list, without contracting or identifying their indices. Momentum grouping itself does not create auxiliary mathematical heads. Coefficients excluded from content extraction may still receive momentum brackets. Epsilon, Dirac and colour jobs keep their existing output path. No mapping version or vocabulary changes are required; earlier grouped and ungrouped files remain readable.

The incremental importer recognises complete parenthesised factor products and parses their factors left to right through the existing restricted parsers. It combines them using `Times`, without expanding the coefficient. Other syntax uses the existing parser path, including its identifier and malformed-input checks. Unit-prefactor groups may be written as `+1*(...)`, and a leading zero permits an empty overall result.

The version-one grouped importer may reconstruct a coefficient through an
internal arithmetic tree. This changes neither the grammar nor the returned
mathematical heads: factors still pass through the restricted typed decoder,
and products of sums remain factored. Malformed or ineligible tree input is
replayed through the established parser for its original diagnostic. Version
selection, layout metadata and mapping digests are unchanged. Current reconstruction uses a held expression built from validated factor references and fixed arithmetic operators; result-file text is never passed to the Wolfram evaluator as source. This private implementation choice adds no syntax to the saved format.

### Bounded rational coefficient output

Selected version-one coefficients may instead be printed as a factored
numerator divided by a factored denominator, multiplied by their original
propagator monomial. This uses only existing arithmetic syntax and mapped
identifiers. `RationalCoefficients.wl` reconstructs supported inverse-polynomial
abbreviations from the export dictionary for FORM arithmetic, then restores all
temporary scalar-product symbols before output. Neither `cfcRat` nor internal
`factor_` objects enter a saved result. `Processing` retains its existing
version-one interpretation; this scalar algebra needs no additional mapping
entry, convention or format version. Old result files remain readable.

The importer performs no new simplification. Coefficient quotients use its
existing restricted grammar; surviving propagators remain outside the coefficient. A preceding numerator-cancellation stage may reduce their powers or remove them entirely. Domain caveats of rational-function cancellation
apply: no value at an original pole is assigned by this representation.

## Numerator cancellation without a format change

`PropagatorCancellation.wl` emits exact numerator/denominator identities before final propagator grouping. Existing denominator entries continue to describe their original momentum, mass and unit inverse power; surviving powers appear in the result, and cancelled entries may be unused. Scalar masses occurring only inside a propagator may now receive ordinary scalar dictionary entries for FORM processing. No additional mapping kind or result head is introduced. Temporary `cfcPCX` scalar-product aliases and negative denominator powers used during processing are restored before output. Versions one through five retain their import interpretation.

### Dimension-only rational coefficients

Remaining version-one coefficients may contain a sum of monomials multiplied by ordinary numerator/denominator quotients in the mapped Lorentz dimension. `DimensionCoefficients.wl` emits this syntax after univariate FORM rational arithmetic. Its private `cfcDimRat` objects never enter output. Unsupported scalar abbreviations retain their recorded meaning. No new mapping fields or parser grammar are required. Direct massless cancellation precedes the guarded general numerator stage, so the latter's fallback does not undo it.

Dimension-only rational functions can also occur internally between prepared tensor stages and before general numerator cancellation. They are removed by the existing dimension coefficient writer. Temporary dimension abbreviations and verified two-index symmetry placeholders are restored within FORM. Symmetry canonicalisation is valid inside the checked contraction, not as an identity for an arbitrary free intermediate tensor. These processing changes introduce no saved mathematical heads or mapping metadata and do not reinterpret older files.

## Version six: one-loop tensor coefficients

Active decomposition writes `Version: 6`, `AlgebraVersion: 1..5`, `Processing: OneLoopTensorDecomposition` and `ResultLayout: IntegralCoefficientGroups`. AlgebraVersion selects the existing epsilon, Dirac and colour validators; a version-six number alone does not imply their presence.

`TensorDecomposition` records encoded `LoopMomentum`, `Normalisation: IntegralEqualsIPiSquaredTimesCoefficient`, `Basis: DistinctSymmetricPlacements`, boolean `DropScaleless`, and `Families`. Each family-table entry contains an encoded `Chord` and `MassSquared`. Import recomputes this table from the authoritative denominator expressions and requires agreement. This table lists distinct normalised loop-dependent denominators before pinches; individual coefficients select surviving entries by positive powers. The first surviving entry is the anchor. Spectator denominators retain their ordinary mapping entries.

The reserved result function is `cfcTC(n1,...,nM,-2,label1,...)`: M nonnegative powers, followed by -2 and sorted tensor labels. Label zero denotes a metric slot and must occur an even number of times. Positive labels refer to active dictionary positions other than the anchor. The empty label sequence is rank zero. `cfcTC(-1,rank)` denotes a polynomial vacuum coefficient with nonnegative even rank.

In version-six integral mode, a nonzero additive group must contain exactly one coefficient token, no undecomposed loop vectors and no surviving loop-dependent denominator identifier. Coefficient powers/products, unknown labels and malformed calls fail. Zero output remains valid. Streams buffer one top-level group and use the existing restricted parser; the flat parser is unchanged. Reconstruction maps labels to local chord positions, computes invariant ordering, and supplies I Pi^2. It applies no coefficient-function reduction.

`IntegralCoefficient` entries (`cfic` prefix) permit subsequent scalar round trips. `LoopTensorCoefficient` has five data fields (encoded label, chord, squared-mass and power lists, then dimension). `TensorPaVe` contains encoded label, invariant and squared-mass lists, reconstructed with optional reduction/reordering disabled. No arbitrary Wolfram evaluation syntax is accepted.

Version-six coefficients may also contain `cfcDimRat(numerator,denominator)`
from pre-integration dimension compaction. Both arguments must be exact
polynomials in the recorded Lorentz dimension, with a nonzero denominator.
The restricted importer restores their quotient; other variables, tensor
arguments, wrong arity and use outside active decomposition are rejected.
This internal function is never returned to the user.


### Optional reduction metadata

`LoopCoefficientReduction` records `Method: PartialFractionsAndFixedDimensionIBP`,
`MaximumDistinctChords: 4`, and the informational list `SingularSubfamilies`.
`Invariants` records encoded squared chord differences in lexicographic unordered-pair order. Import requires them to agree with the current scalar-product definitions before decoding inverse recurrence coefficients. The mathematical output remains the validated `cfcTC` vocabulary. The temporary
FORM function `cfcIR` must never reach the result; it is not an accepted parser
head. Scalar recurrence coefficients are ordinary validated abbreviation entries.

## Version seven: reduction of existing coefficients

For reduction of existing coefficient expressions, `TensorDecomposition.Mode`
is `ExistingCoefficients`, `LoopMomentum` is JSON null, `LoopMomenta` is empty,
and `Processing` is `ExistingLoopCoefficientReduction`. The family dictionary
is recomputed from the original `IntegralCoefficient` mapping entries, with
zero chords ordered before other chords. This mode requires reduction metadata.
This mode requires version seven. Version-six files retain their existing integral interpretation (absent `Mode`, or `Mode: Integral`); neither older formats nor inactive exports are reinterpreted. Reject a mode/version mismatch rather than silently changing normalisation.

Existing-coefficient mode preserves external constants, products and powers of
integral coefficients and does **not** insert another `I Pi^2`. Consequently,
the exactly-one-integral-token condition applies only to integrand decomposition.
Both modes retain typed label/power validation, incremental reading and
scaleless checks. Import performs reconstruction only; the reduction runs in FORM.

### Optional compact reduction blocks (versions six and seven)

`LoopCoefficientReduction.CoefficientBlocks` is an optional Boolean field,
covered by the mapping digest. Absence means false and preserves earlier files.
When true, `cfcLRReduced(expression)` and `cfcLRReducedPart(expression)` contain
validated linear combinations of `cfcTC` coefficients with commuting scalar
factors. The second spelling identifies a piece of an oversized combination;
both have identical mathematical meaning. Unknown identifiers, incorrect
arity, nonlinear coefficient products and unrecorded block syntax are rejected.

Earlier grouped writers emitted reduced combinations multiplied by the original amplitude
brackets, which may contain loop-independent spectator propagators on this
path. Loop-dependent propagators remain invalid in a completed integral result.
Reconstruction removes the wrappers without expanding those products.
The public result heads, coefficient conventions and normalisation are unchanged.


### Optional shared coefficient definitions (versions six and seven)

`LoopCoefficientReduction.SharedCoefficientBlocks` is an optional Boolean,
covered by the mapping digest. Absence is false. True requires
`CoefficientBlocks: true` and algebra version one. It selects this body layout
following the ordinary result identity header:

```text
CFC_SHARED 2
cfcShare(1)=cfs1+1;
cfcShare(2)=cfs1^2;
CFC_BODY
0+(cfcTC(1,-2))*(cfcShare(1)+2*cfcShare(2))
```

The count is non-negative; exactly that many definitions follow, each with a distinct positive integer
identifier. Identifiers need not be consecutive: FORM can reuse an existing
internal expression entry. Only amplitude blocks are written, rather than FORM's entire temporary-symbol
registry. A block can become unused when reduction removes its integral family. Each right-hand side uses the ordinary restricted commuting grammar.
It cannot reference another shared block or an integral token. Unknown
identifiers and undecomposed loop dependence remain errors. Definitions end
with a semicolon; the body marker occupies its own line. CRLF is accepted.
`CFC_SHARED 0` is valid and has no definitions. The importer reads definitions
incrementally and never executes them as source code.

Each linear body group identifies a final integral coefficient and its complete
multiplier. The multiplier can refer to shared amplitude blocks using
`cfcShare(positiveInteger)`. These private references disappear on import;
the returned expression contains ordinary products and sums. Small jobs and
matrix jobs can use direct final-key assembly. Older files without the new
metadata retain their original grammar and interpretation.


### Incremental denominator groups (versions six and seven)

Two optional Boolean fields in `LoopCoefficientReduction` refine the layouts
above. Both default to false and are covered by the existing mapping digest.
`DenominatorGroups: true` requires `CoefficientBlocks: true` and permits the
typed scalar calls `cfcLRNum(polynomial)` and `cfcLRDen(polynomial)`. They decode
to the polynomial and its reciprocal respectively; zero denominators, tensor
arguments and non-polynomial arguments are rejected.

`IncrementalSharedDefinitions: true` additionally requires
`SharedCoefficientBlocks: true` and permits references to **earlier** validated
shared definitions. Self, forward and duplicate definitions remain invalid.
With that flag the following alternative to `CFC_BODY` is accepted:

```text
CFC_SHARED 1
cfcShare(2)=cfs1+1;
CFC_GROUP_BODY
CFC_ADD_SHARED 1
cfcShare(5)=cfcShare(2)^2+1;
CFC_GROUP
(cfcTC(1,-2))*(cfcLRDen(cfs2)*cfcShare(5));
CFC_END
```

Each group is preceded by its additional-definition count, which may be zero.
Definitions and groups terminate with semicolons. `CFC_END` is mandatory;
only whitespace may follow. Its final newline is optional. Shared identifiers
need not increase: FORM may reuse an older identifier that has not previously
been written. Availability is determined by definition order, not integer order.
Legacy `CFC_BODY` output remains valid, including when a small job selects no
shared definitions. Without the new metadata, earlier files retain the original
non-recursive dictionary grammar.


## Version eight: scalar master reduction

Active scalar-master jobs use version eight. `AlgebraVersion` still identifies
the independent tensor/epsilon/Dirac/colour vocabulary. `ScalarMasterReduction`
records the Davydychev–Tarasov method, source interpretation (six for integrands,
seven for saved coefficients), symbolic D, four-point scope, normalisation,
generic denominator restrictions and exceptional families. These fields are
covered by the mapping digest. Earlier versions keep their original semantics;
import never initiates a new reduction.

Scalar masters use existing rank-zero integral tokens. Recurrence-only objects
are not accepted by the result parser. The existing grouped/shared incremental
output grammar remains in use.

A saved PaVe family can use a typed `MasterChord` record containing point count,
slot and ordered invariant list. It is a private coordinate in the family
metadata, not a Lorentz vector. Distances are decoded from the FeynCalc invariant
ordering. Unrelated invariant families are never joined; their cross-distances
are recorded as `UnrelatedFamilies`, not assigned physical scalar products.
The dictionary is reconstructed from the saved coefficient entries and checked
against the mapping before result parsing. Private chord objects cannot be
returned as physical momenta.

The existing restricted numeric grammar also accepts FORM's decimal-integer
continuation: a backslash immediately after decimal digits, a newline (LF or
CRLF), optional horizontal whitespace and further digits. These fragments form
one integer token. Identifiers and arbitrary expressions cannot be joined by
this syntax. Continuations may cross reader chunks; only the completed group
or shared definition is normalised.
