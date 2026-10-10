# Automatic rational coefficient processing — 10 October 2026

## Delivered behaviour

`CalcFormCalculate` now uses native FORM rational cancellation and polynomial
factorisation for eligible grouped scalar coefficients. `CalcFormExport`
embeds the same procedure, so manually executed new exports have the same
behaviour. Existing `.frm` files need re-exporting. There is no new public
option, mapping version, importer grammar or Mathematica simplification stage.

The procedure keeps the complete propagator monomial outside the algebra.
It exposes supported reciprocal-polynomial abbreviations, temporarily names
scalar products, uses `PolyRatFun`, factorises numerator and denominator, and
restores scalar products while writing ordinary arithmetic syntax.

`RationalCoefficients.wl` owns structural eligibility and FORM text generation.
`Templates/RationalCoefficients.frm.in` performs the algebra;
`Templates/RationalCoefficientOutput.frm.in` writes its factors. The existing
propagator template retains the fallback path and batch scheduling. Loading
these definitions performs no calculations or executable probes.

## Selection and safeguards

The initial private limits are 1,000 expanded terms per coefficient and 12
potential polynomial variables per job. Variables include the mapped scalar
symbols and all scalar-product pairs of registered vectors. Reciprocal powers
must be integers from -8 through -1; polynomial powers in their bases are
limited to 0 through 32 and their symbols must already be mapped.

A job with unsupported scalar abbreviations (for example radicals) retains the
previous path. Free-index, complex and scalar-function coefficients retain it
individually, including in a batch whose other coefficients are simplified.
Matrix, epsilon and colour jobs retain their existing processing. Jobs without
mapped propagators are unchanged. These limits select work; they do not
promise a hard cost bound for arbitrary multivariate polynomial arithmetic.
Existing process timeout, cancellation and failure retention remain in force.

No numerator/propagator cancellation, on-shell condition, momentum-conservation
identity or convention change is introduced. Scalar rational cancellation is
understood at generic parameter values, not as a prescription at an original
pole.

## User's saved first term through the public command

The exact term recovered for the preliminary experiment was evaluated through
`CalcFormCalculate` with serial FORM and with ten-worker TFORM. Both calls
returned 388 leaves instead of the original 10,697. The difference from the
independently verified standalone result reduced to zero in FORM.

Factor signs/order differ from that standalone representation, so `SameQ` is
false; this is not treated as a failed algebraic comparison. Observed public
call times were 0.359741 s and 0.371306 s respectively. These are single
observations, including availability checking, export and import, not a
thread-scaling benchmark.

## Complete retained-result replay

The preserved 171,993,554-byte result was replayed through the new output stage
with TFORM 5.0.2 and ten workers, without repeating the interaction/tensor
calculation. The original source, mapping and output were not changed.

| Observation | Result |
|---|---:|
| Complete propagator groups | 1,022 |
| Selected for rational processing | 37 |
| Retained on the fallback path | 985 |
| Original result bytes | 171,993,554 |
| Replay result bytes | 171,430,748 |
| Replay process wall time | 26.3579 s |
| Replay peak process-tree RSS | 2,730,500,096 bytes |

The file-size reduction is only about **0.33%** for this full result. The
30-fold reduction of the selected first term does not generalise to the whole
file under the initial limits. Replay time includes reading and sorting the
preserved result; it is neither an end-to-end calculation time nor a measured
increment to the original calculation. The replay uses equivalent whitespace
in the fallback closing parentheses.

### Complete algebraic check

FORM checked all 37 changed coefficients separately by cross-multiplying their
printed numerator and denominator against the corresponding original bracket.
Only the dimension-dependent rational factors were collected for this check,
keeping other monomials separate. Every residual was exactly zero.

A separate native FORM residual checked the sum of all 985 fallback groups
against the original after excluding the 37 selected brackets. That residual
was also exactly zero. The complete equality process finished in 36.5073 s
with peak process-tree RSS 2,121,859,072 bytes.

The complete new result then imported successfully in a fresh kernel:

- Import wall time: 141.595367 s.
- Returned `Plus`: 1,022 terms.
- Returned expression `ByteCount`: 2,166,871,080 bytes.
- Retained kernel memory: 1,176,676,488 bytes.
- Peak process-tree RSS: 1,346,252,800 bytes.

These are workload-specific observations, not a claimed import speedup. Brief
independent small-command checks also ran during this import. Resource-bounded
large trials stopped at 300 seconds, 6 GiB process-tree RSS or less than 2 GiB
available system memory; the accepted trials hit none of these limits.

### Rejected wider trial

A trial allowing 20,000 terms per rational coefficient failed after about
15 seconds: FORM needed a 48,432-word numerator inside a rational function,
exceeding its 40,000-word single-term limit. The wider setting was not enabled.
The implementation does not raise FORM's memory settings to conceal this
limit. Broader coverage requires a separate strategy and further validation.

## Regression verification

All **1,536 assertions** passed in the core, FORM and runtime suites, including
fixed legacy formats, exporter/importer checks, runtime behaviour, epsilon,
Dirac, colour and propagator grouping. The new `RationalCoefficients.wls`
contributes 38 assertions covering the public command, compact identities,
complete cancellation, negative powers, mixed batches, fallback limits and
unchanged mapping versions. Automatic loading and namespace-isolation checks
also passed.

During development, a missing newline before a generated `#else` suppressed
fallback output. It was corrected before acceptance. Mixed and entirely
unsupported batches now have explicit regression coverage, in addition to the
complete 985-group equality check. Early incomplete/incorrect development
outputs were not used as accepted performance or correctness evidence.

No Full benchmark, stored-library regeneration, notebook edit or commit was
performed. Raw observations and source hashes are in
[RationalCoefficientIntegration.json](RationalCoefficientIntegration.json).
