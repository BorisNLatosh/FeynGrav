# API-03: final PaVe output conversion — 30 October 2026

**Accepted within the focused scope below; uncommitted.** The full
`FeynGravSimplify` pipeline defaults to `PaVeToABCD -> True`. Explicit False
returns the previous pipeline output. PERF-05 remains open.

## Implementation and source identity

The existing ``FeynCalc`PaVeToABCD`` symbol is the sole new option; no mapping version,
mathematical head or raw import/export change is introduced. Conversion runs once
at the facade's final successful-result boundary. Restricted methods accept both
Booleans as no-ops, retaining their input vocabulary. The facade readiness
inventory includes the helper; the parent loader already clears all private
facade definitions on retry. The [guide](../../CalcFormConverter/UserGuide.md#final-pave-conversion-feyngravsimplify-only)
records timing, diagnostics and cleanup semantics.

Local reference: FeynCalc `LoopIntegrals/PaVeToABCD.m`. Its supported replacement
rules supply named scalar and tensor functions without globally enabling PaVe
ordering/reduction. Named-function evaluation may change expression size.
Baseline repository commit: `1b53368e6b29ff46ff490ea902f3d2108c2b94fc`;
pre-existing uncommitted PERF-05 and RULE-01 work is separate.

Retained evidence: `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-30-api03/`.
`Facade.before.wl` preserves the API-03 baseline; `final-hashes.json` pins the
final facade, new test and registered driver. Facade SHA-256:
`ea5b152a27746bff4c71ec68b0c96bc55cd58b18f0db055881f28b98f1037112`.
Test SHA-256: `78ae34bac3a9ca4b47c8773208ce8c36626268aaccd81a52d293bb8f01bc83fa`.

## Checks actually executed

Two sequential fresh bounded batches passed: an initial 66/66, then a final
67/67 including an explicit aborted-total-status assertion. These are successive
versions, not 133 independent checks. The final `PaVeOutput.wls` covers:

- Explicit expected `A0`, `A00`, `B0`, `B1`, `B00`, `B11`, `C0`, `D0` output;
  default/explicit True and exact False compatibility using controlled backend results.
- Symbolic D, complex coefficients, exact expected-expression residual and an
  altered-coefficient negative control; unsupported PaVe/formal coefficients retained.
- Option context/default, invalid values, first-wins duplicates, delayed value
  evaluated once, SetOptions/reload persistence and explicit override.
- Controlled conversion warning/abort failures and total-report status, plus
  unchanged zero, Boolean, Failure and equality handling.
- Six small real serial pipeline fixtures compared with CalcFormCalculate:
  a massive integrated tadpole and saved scalar bubble/triangle/box, tensor B1
  and unsupported triangle coefficients. False uses exact SameQ parity;
  True agrees with FeynCalc conversion of the legacy result. These are not
  independent derivations of the converter's reduction formulae.
- Restricted FORM trace with both Boolean options; ordinary full-pipeline trace,
  equality and named scalar round trip. An undecomposable/scaleless constant
  preserves the formal result and identical partial-reduction warning.

Expected diagnostics are retained separately in `expected-conversion.log` and
`partial-*.log`. Final kernel stdout contains only `SUMMARY passed=67 failed=0`.
The expected defaults in older Facade/Loading/Standalone suites were updated;
those complete suites and Contractor execution were **not** rerun here.
No full three-box, broad QED, TFORM comparison or complete option-space benchmark
was performed. No claim that all named tensor outputs are accepted as inputs.

## Resources and measurement limits

Both batches used the retained task-owned runner: aggregate 1 GiB, zero swap,
120 seconds plus 10 seconds grace and a fresh 2 GiB host-headroom gate. Final
pre-kernel available memory was 10,259,952 KiB. Kernel exit was zero after
5,513 ms. No timeout, unexpected warning, cap/OOM event or owned survivor was
recorded; the scope became inactive and its cgroup was removed. Raw wrapper,
observer and resource records are in `final/run-SMOKE/`. No user kernel was touched.
The highest retained cgroup peak sample was 213,852,160 bytes; it is neither a
final peak guarantee nor kernel RSS.

A synthetic 64-contribution coefficient sample across eight named families was
converted in 0.041696 seconds in the final batch (0.026671 in the earlier batch).
ByteCount grew from 39,488 to 85,696 bytes. Its exact named-basis residual was zero.
This small sample is not the retained three-box workload and is not a speedup
claim. Conversion cost belongs in future default whole-call PERF-05/PERF-06
measurements; older False/raw-output timings are not new-default timings.

Rules, libraries, models, notebooks and unrelated edits were preserved. Nothing
was committed or published.
