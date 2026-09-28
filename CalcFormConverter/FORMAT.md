# Persisted format, version 1

This document describes the JSON mapping and dedicated FORM result consumed by `CalcFormImport`. It is a compatibility contract for saved calculations. Internal helper associations returned by `buildExportData` and `renderExport` are private implementation details and are not this file format.

## Version and correspondence

The mapping contains `"Format": "CalcFormConverter"` and integer `"Version": 1`. The result begins with:

```text
CFC1 <mapping-digest>
<FORM expression>
```

The marker is derived from the format version in the implementation. Version 1 accepts only version 1; unsupported versions produce `Failure["InvalidMapping", ...]`. Changing the meaning of existing fields, symbol encoding or result grammar requires an explicit compatibility decision and, when incompatible, a new format version. Optional descriptive fields may be added without changing existing meanings.

The mapping is read as UTF-8 text. Its primary digest is Wolfram Language `Hash[jsonText, "SHA256", "HexString"]`. For compatibility with early notebook exports, the importer also accepts the digest of `FromCharacterCode[ToCharacterCode[jsonText, "UTF-8"]]`: those exports hashed the UTF-8 byte string before writing Unicode text. Both checks bind the complete mapping. These are Wolfram string hashes, not a specification to hash arbitrary raw file bytes with an external utility. Even JSON reformatting changes the digest. Preserve the mapping file with its result. This check detects mismatched artifacts, not deliberate tampering.

For the user workflow and commands, see [README.md](README.md); implementation invariants and tests are in [DEVELOPER.md](DEVELOPER.md).

## Mapping fields

| Field | Meaning | Import requirement |
| --- | --- | --- |
| `Format` | Fixed format name above | Required and checked |
| `Version` | Integer format version | Required and checked |
| `Dimension` | Encoded symbolic or integer Lorentz dimension | Required; a symbol other than `I`, or integer at least 2 |
| `Entries` | Array of mapping entries | Required and validated |
| `ExpressionDigest` | `Hash[FCI[input], "SHA256", "HexString"]` | Always exported; participates in mapping digest, not otherwise interpreted |
| `LoopMomenta` | Array of encoded momentum symbols supplied by the user | Always exported; informational to the current importer |
| `Processing` | `"TensorAlgebraOnly"` in this version | Always exported; informational to the current importer |

`ExpressionDigest` differentiates exports of different expressions even if they use exactly the same symbol dictionary. It is not a serialized expression or an independent proof of the FORM calculation. Future procedures must define any additional use of `LoopMomenta` or `Processing` explicitly.

Each entry requires `Name`, `Kind` and `Expression`. Names are unique within a mapping. Export numbering starts at one within each kind and follows traversal order. The importer accepts a kind's prefix followed by digits. Numbering is local to each export; consumers must not assume, for example, that `cfi1` always means alpha.

| Kind | FORM prefix | Decoded expression |
| --- | --- | --- |
| `Scalar` | `cfs` | Mathematica scalar symbol |
| `Vector` | `cfv` | Momentum symbol, without a `Momentum` wrapper |
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

These fields are convenience metadata for a future reducer. The current importer neither uses them to override `Expression` nor checks their consistency. A future consumer must derive its data from `Expression` or validate the redundant metadata before using it. Repeated propagators appear as repeated factors or powers of the identifier in the expression; `Power` is not their total multiplicity in a term or diagram.

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

FORM output performs algebra and contractions, so the result need not retain the original factorization or denominator grouping. Opaque scalar abbreviations and denominator identifiers retain their definitions through the mapping.

### Tokenization and precedence

The result body is tokenized into ASCII identifiers (`[A-Za-z][A-Za-z0-9_]*`), decimal digit sequences, and `+ - * / ^ ( ) , .`. Whitespace may separate tokens; it cannot hide other characters. There is no implicit multiplication, decimal/scientific notation, assignment, semicolon terminator, string literal or comment syntax. Use the dedicated `.out` file, not the FORM console transcript.

From lowest to highest, parsing handles sums, products/division, unary signs, integer powers and vector dots/atoms. Division chains are left associative: `24/3/2` gives `4`. An exponent is one signed integer, optionally parenthesized, such as `x^-2` or `x^(-2)`; arbitrary exponent expressions and chained powers are rejected. Unary signs precede a power expression, so `-2^2` is `-4`, whereas `(-2)^2` is `4`.

Identifiers must be declared in the mapping, except `i_` and the reserved call names. Function argument counts and scalar/vector/index roles are checked before reconstruction. A dot requires two vector values, a component requires one index, and `d_` requires two indices. Bare typed values cannot be cancelled into apparent scalars: `0*cfv1`, `cfv1-cfv1`, and `cfi1^0` fail. Unknown identifiers are rejected even in terms that would vanish. Division by zero and zero to a nonpositive power fail.

All mapping expressions are decoded and checked, including entries unused by the result. A valid digest does not bypass grammar, entry-kind, dimension or expression validation. Conversely, the digest authenticates no sender and proves no mathematical calculation. Restored symbols and whitelisted heads undergo normal Wolfram evaluation in the receiving kernel; this data format does not isolate pre-existing kernel definitions.

## Compatibility fixture

`Tests/Fixtures/v1.map.json` and `v1.out` were generated by the original version-one exporter and FORM 4.3 before the maintainability refactor. The fixed expected expression is checked in `Tests/Core.wls`. The tests must not regenerate these files: doing so would allow exporter and importer to change incompatibly together without detecting the break.
