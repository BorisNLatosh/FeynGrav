# PERF-05 coefficient collection — 30 October 2026

**Increment 3 accepted within the focused linear one-loop scope; PERF-05 remains
open. Nothing committed.** See the [implemented collection contract](../../CalcFormConverter/IntegralCollection.md)
for integration scope, key identity, domain guards and work ownership.

## Implementation and identity

`LoopCoefficientReduction.wl` plans pre-reduction and direct-post-reduction
coefficient collection. The existing Laurent-monomial reader now exposes its
definitions through a small reusable helper; its original rule-string interface
remains. `RationalCoefficients.wl` permits that caller to request the existing
scalar policy without requiring a propagator in the mapping. The new native
`Templates/CoefficientCollection.frm.in` owns disposable coefficient arithmetic,
reassembly and a shared pre/direct-post attempt counter. The installer records
the new template dependency. Existing shared-output final processing is retained.

Evidence directory:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-30-perf05-collection/`.
`baseline/` and `initial-hashes.json` preserve pre-edit state;
`repair-hashes.json` pins the tested final production candidate.
`repair/run-REGRESSION/scripts-as-run/` retains the executed test and launcher.
Earlier PERF-05/API-03 and unrelated RULE-01 edits are preserved.

## Final executed verification

The final fresh kernel passed **69/69 assertions**. Its evidence script contains
44 sequential FORM invocations, including one two-worker TFORM call. The portable
`Tests/IntegralCollection.wls` has the same executed body, with only its package
root made relative, and is registered in the runtime suite. The complete driver
and complete historical suites were not run.

Ten candidate/reference pairs cover cross-denominator zeros and nonzero
coefficients, symbolic D, declared-loop tadpoles, cancellation of repeated-power
coefficients into a common master, the small direct-output path, mixed scalar
spectators and shared-polynomial identities. Every complete result agrees exactly
with the disabled-pass reference and an explicit expected expression; mapping
files are unchanged. An altered-coefficient negative control is rejected.

For the first zero-coefficient fixture, retained FORM output records
`CFC_MASTER_REQUESTED 0` with collection versus `CFC_MASTER_REQUESTED 1` without
it. This verifies removal before runtime recurrence, not merely a final zero.
Distinct nonzero input keys also cancel after reduction, including an exact zero
on the direct repeated-power-only output path.

Eight further candidate/reference pairs cover complex, metric, ordered Dirac,
radical, product-of-integrals, unreduced-integrand, reduction-disabled and
polarisation cases. Additional checks cover work allowances 0/1/2, the small
term guard, serial/TFORM agreement, ordinary constants and API-03 True/False
boundaries. Protected mappings remain unchanged and imported results contain no
collection helper objects. Exact residual arithmetic is confined to test code.

## Retained failures and repair sequence

1. The initial focused run passed 36/37 assertions. All mathematical comparisons
   passed, but the new pass was absent from generated code: the reused selector
   rejected positive/Laurent monomial abbreviations introduced by reduction
   planning. Safe existing monomial normalisation was moved ahead of strict
   inverse-polynomial selection. The original candidate is retained.
2. The second scope stopped on the first FORM job with `Specification out of
   order`: new `Drop`/`Unhide` specifications followed `Local` definitions.
   No result from that job was accepted. The template was corrected to follow
   FORM's module order and reviewed statically.
3. One explicitly documented targeted repair scope then passed 69/69. No further
   scope was used. The original two-scope plan, the reasoned extension and every
   failed record remain in `scope.md`, `first-review.md` and
   `syntax-repair-review.md`. Counts are not added across successive candidates.

## Resources and limits

All three scopes used aggregate 1 GiB, zero swap, 120 seconds plus 10 seconds
grace, fresh host headroom at least 2 GiB and task-owned process control. The
final kernel exited zero after 5,440 ms; prelaunch available memory was
10,011,452 KiB and the highest retained cgroup peak sample was 209,346,560 bytes.
This is not a final-peak guarantee or kernel RSS. Final raw diagnostics show no
unexpected warning/error; no scope reported a timeout or memory-cap/OOM event.
All owned processes terminated, scopes became inactive and cgroups were removed.
User kernels were untouched.

This is not complete PERF-05 or performance acceptance. No full three-box
warmup/three-trial timing, all-17-master comparison, retained QED zero/sample
regression, 232-group reconstruction, full tadpole or 1,490-term equality check
was repeated. Large-case output size and speed are unmeasured for this increment.
No new exceptional-kinematics support, multiloop reduction or universal coefficient
simplification is claimed. Installer dependency coverage was checked statically;
no installation was performed.
