# Rational coefficient simplification in FORM — 10 October 2026

> Historical focused experiment. The guarded public integration is described
> in [RationalCoefficientIntegration.md](RationalCoefficientIntegration.md).

## Status and scope

The focused experiment succeeds on the first term displayed in
`One-Loop-Counterterms.nb`. This implementation remains a standalone experiment;
normal `CalcFormExport` and `CalcFormCalculate` processing is unchanged.

The term was recovered as `First[CalcFormImport[...]]` from the preserved full
result, not reconstructed from notebook display boxes. The latter contain
presentation-only scalar-product boxes which cannot safely be treated as
ordinary input. The preserved result has SHA-256
`5990abf716a1d9b7a889282ea11adf5553345cd4188364c8d8a15d8154e79eaa`.

The full result was imported once to extract this test term; the full result
was **not** subjected to the new simplification. The original notebook,
FORM result and mapping remain unchanged.

## Implementation

The [reproducible example](../RationalCoefficient/README.md) includes the
original exported term and a runnable candidate FORM program. A small test
adapter reuses the generated expression/processing prefix, extracts the one
complete propagator product and keeps it outside the scalar algebra.

The candidate exposes reciprocal polynomial abbreviations, represents scalar
products by temporary variables, combines the coefficient using `PolyRatFun`,
and factorises the numerator and denominator separately with `Factorize`.
It restores the scalar products when printing the factors. The existing
restricted importer accepts the output with the unchanged version-one mapping.

No Wolfram `Simplify`, `FullSimplify`, `Factor`, `Cancel` or `Together` was used.
All algebraic simplification and equality checking took place in FORM 5.0.2.
The relevant facilities are described in the
[official FORM polynomial documentation](https://form-dev.github.io/form-docs/stable/manual/#polynomials).

## Results

| Measurement | Existing coefficient | FORM rational simplification |
|---|---:|---:|
| Result-file bytes, including header | 23,086 | 768 |
| Imported `LeafCount` | 10,697 | 388 |
| Imported Wolfram `ByteCount` | 309,488 | 10,960 |

The file is about **30 times smaller**, a **96.7% reduction**. `ByteCount`
describes this Wolfram expression, not process RSS or a guarantee for the full
calculation. Factor signs/order need not reproduce Mathematica's presentation.

The result contains the compact polynomial factors visible in the notebook:
`(M0^2-M2^2)`, `(l.p)^2-l^2 p^2` squared, and `l^2-2 l.p+p^2`.
The first of these may be printed as two linear mass factors with an equivalent
overall sign. Propagator factors and their powers are unchanged on import.

### Execution wall times

One warmup followed by three sequential measured trials per configuration,
with alternating baseline/candidate order. These timings include process
startup, evaluation and writing; equality verification is excluded. They do
not include Wolfram export or import.

| Engine | Existing median (range), seconds | Candidate median (range), seconds |
|---|---:|---:|
| FORM | 0.00983 (0.00953–0.00994) | 0.09189 (0.09136–0.09769) |
| TFORM, 10 workers | 0.01585 (0.01518–0.01608) | 0.11154 (0.10389–0.11914) |

The candidate adds computation to obtain the smaller representation. This
small example does not benefit from ten workers, and its timings do not
predict the full calculation's cost.

Separate resource trials of the calculation-only program recorded peak native
process RSS of 12,656 KiB for FORM and 16,764 KiB for TFORM. They used GNU time's
maximum-RSS measurement; these are not Wolfram memory measurements.

The two import observations were 0.220511 s and 0.009839 s in the same fresh
kernel, in that order. They are first/subsequent import observations, **not**
an import speed comparison between FORM and TFORM.

## Correctness checks

Twenty-eight standalone checks passed with real FORM and TFORM:

- Cross-multiplication against the original coefficient returns exactly zero.
  The check rereads the printed factors, so it also checks their serialisation.
- A second comparison against an independent transcription of the saved
  compact notebook expression returns exactly zero.
- No temporary invariant symbols or internal `factor_` objects leak into output.
- Polynomial factors, rational cancellation, complete cancellation to zero,
  negative powers, rational numerical coefficients, unity and zero work.
- Multiple complete propagator products and residual scalar functions are
  rejected by the deliberately narrow experiment.
- Unsupported complex input, mapping version, source/mapping mismatch and
  missing source-stage boundary are rejected during preparation.

Both real results also import successfully through the existing converter.
Their imported propagator products are exactly `SameQ` to the original.
Raw observations and hashes are in [RationalCoefficient.json](RationalCoefficient.json).

## Limits and next integration boundary

This establishes the requested coefficient-level result. It does not establish
that simplifying all 1,022 terms will be fast, memory-bounded or uniformly
beneficial. No Full benchmark or full-result factorisation was run.

Before enabling the procedure in normal exports, test a representative spread
of coefficients, choose conservative eligibility/work limits, and preserve
fallback behaviour for free tensors, scalar functions, radicals, complex,
Dirac, epsilon and colour structures. The present test adapter intentionally
rejects those unsupported inputs rather than silently changing them.

Rational cancellation is understood at generic parameter values. It does not
assign values at singular points or change propagator prescriptions. In
particular, the coefficient's `(p-l)^2` factor is not cancelled against an
opaque propagator.
