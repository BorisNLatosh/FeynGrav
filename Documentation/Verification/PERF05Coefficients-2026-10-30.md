# PERF-05 shared coefficient policy — 30 October 2026

**Increment 2 accepted within the focused scope; PERF-05 remains open.**
No commit. See the [implemented policy](../../CalcFormConverter/CoefficientPolicy.md)
for classification, domains, stage allowances and remaining orchestration work.

## Changes and identity

`RationalCoefficients.wl` now owns the common mapping-role planner,
restricted polynomial/inverse compiler and named allowance profiles.
`DimensionCoefficients.wl` uses that compiler with the selected dimension as
its scalar domain. Propagator, master and polarisation planning consume the
same policy helpers. `ExportAssembly.wl` labels the private policy accordingly.
Existing FORM templates, algorithms, cache keys/modes, metadata, scoring,
rollback, mapping/output formats and API-03 definitions are unchanged.

Pre-edit source snapshots and hashes are retained under
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-30-perf05-coefficients/`.
`initial-hashes.json` preserves prior dirty-file identities; `baseline/` supplies
the literal comparison definitions; `candidate-hashes.json` pins executed
production sources and unit tests. The repository base is
`1b53368e6b29ff46ff490ea902f3d2108c2b94fc`, with earlier uncommitted PERF-05,
API-03 and unrelated RULE-01 work preserved.

## Executed checks

Two sequential fresh kernels completed with **72/72** and **52/52** assertions.
These are focused suites, not a rerun of the complete converter test collection.

- **FOCUSED:** 29 shared-policy checks cover exact rendering, polynomial/inverse
  boundaries, unsupported Pair/radical/complex-domain objects, domain-dependent
  eligibility, current allowances and exhausted-work clamping, distinct scalar,
  vector and polarisation roles, and negative controls. Forty literal comparisons
  across ten inputs verify propagator, dimension, master plans and full rendered
  programs against pre-edit definitions. Three further checks verify private
  policy metadata and rejection of an altered plan. All generated comparison
  programs and plans are exactly equal, not merely numerically sampled.
- **MATHEMATICS:** 52 assertions include 15 explicit exact-expression fixtures,
  zero and negative controls, symbolic D, rational/complex coefficients,
  scalar-product bilinearity, retained radical/inverse-product forms, tensor
  and ordered matrix output, approximate-input rejection, loop scope via a
  massive tadpole and an integrated zero, API-03 True/False boundaries, and
  term/variable/master/work-limit fallback. Thirty FORM calls execute
  sequentially, including one small two-worker TFORM comparison. Exact residuals
  use `Together`/`ExpandScalarProduct` in tests only; production adds neither.

The reusable unit suite is `Tests/CoefficientPolicy.wls` (29 checks).
`Tests/CoefficientArithmetic.wls` retains the executed 52-assertion mathematical
body with the standard Common.wl harness. Both are registered in the runtime
suite. The bounded run used the preserved evidence wrappers `Focused.wls` and
`Mathematics.wls`; mathematical test-body equality is checked statically.
Registration does not claim the entire standard driver was executed.

## Resources and diagnostics

Each scope enforced aggregate 1 GiB, zero swap, 120 seconds plus 10 seconds
grace, fresh host headroom of at least 2 GiB and task-owned process controls.
FOCUSED kernel exit: zero after 3,037 ms; highest retained cgroup peak sample
204,091,392 bytes. MATHEMATICS kernel exit: zero after 5,621 ms; highest retained
sample 216,506,368 bytes. Samples are neither final-peak guarantees nor kernel RSS.
Raw stdout contains only the successful assertion summaries. No unexpected
warning, timeout, cap/OOM event or owned survivor was recorded; both scopes
became inactive and their cgroups were removed. User kernels were untouched.
Raw logs, script copies/hashes, resource observations and termination records
are in `run-FOCUSED/` and `run-MATHEMATICS/`.

## Limits

This increment consolidates selection at existing processing points. It does not
add new scalar identities or claim stronger simplification, nor does it complete
before/after-reduction collection, changed-block recurrence or a pipeline-wide
aggregate allowance. The two retained large QED group zeros, full selected
tadpole, 232-group reconstruction and 1,490-term equality gates were not rerun.
Neither the warmup/three-trial full three-box benchmark nor all-17-master
comparison was repeated. Existing PERF-04 evidence remains historical baseline;
no new speedup, output-size or whole-call memory claim is made. General
exceptional-kinematics handling and broader multiloop work remain separate.
