# PERF-05 increment 1 — private processing contracts and planner

Status: accepted within the focused scope below; PERF-05 remains open. No commit.
Base checkout: `FeynArts_Integration`, `1b53368e6b29ff46ff490ea902f3d2108c2b94fc`.
Pre-existing RULE-01 source and documentation changes were preserved.

## Delivered behaviour

The existing export companion now records input scope, requested options and
selected preparation stages in a private, job-local plan. It preserves the former
nested tensor/reduction preparation order and delayed-option evaluation order.
The renderer, reduction algorithms and coefficient guards remain unchanged.
The private plan is not written into the mapping or generated FORM text.

See [processing contracts](../../CalcFormConverter/ProcessingContracts.md).
No public option, mapping version, mathematical head, new coefficient arithmetic,
parallel worker implementation or API-03 conversion is introduced.

## Focused evidence

The final `ProcessingPlan.wls` passes **123/123 assertions**:

- 12 fixtures × 3 option combinations × 3 assertions: exact prepared-data parity
  (excluding the new plan), complete rendered-program/mapping parity and input-case
  classification against the previous nested orchestration.
- Six contract controls: whole-expression scope for a declared constant, saved
  coefficient stage selection, absence of the plan from the mapping, deliberately
  altered export-data rejection, and delayed-option order on success and failure.
- Three small actual FORM cases, each checking execution, standalone import versus
  `CalcFormCalculate`, and the explicit expected result: contracted metric trace,
  massive integrated tadpole, and an already represented scalar coefficient.

Both fresh bounded batches passed 123/123; the second checks the portable harness
registered in the runtime suite, with FORM selected from PATH, raw child logs and
its completion marker. Production source was unchanged between those batches.
These are repeated checks of the same cases, not 246 independent cases.

Each batch used one fresh Wolfram kernel and six sequential FORM executions under
one aggregate scope: **1 GiB RAM, no swap, 120 seconds plus 10 seconds grace**,
with fresh available host memory above 2 GiB. Kernel intervals were 3.813 s and
3.740 s. Highest retained cgroup-memory-peak samples were 205,242,368 bytes and
206,315,520 bytes respectively; these are sampled cgroup values, not kernel RSS
or guaranteed final peaks. No memory event, unexpected diagnostic or surviving
owned process was recorded. Final raw FORM stdout/stderr files were reviewed.

Evidence directory:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-30-perf05-planner/`.
The first run is `run-SMOKE/`; final run is `portable-check/run-SMOKE/`.
Retained files include the original export companion, final source hashes,
archived scripts, exact invocation, assertion ledger, raw logs, prelaunch policy,
live resource observations, process identities and termination records.

## Limits and next boundary

Parity uses the existing preparers and renderer as its reference; this is not an
independent derivation of loop reduction. The negative control tests detection of
changed export data. The three expected results supply small independent checks,
not general loop-physics validation. Full three-box timing, broader QED workloads,
all option combinations, additional dimensions and TFORM equivalence were not
rerun. No performance improvement or maximal simplification is claimed.

The common coefficient policy, additional pre-reduction collection and coordinated
budget/cache changes remain later PERF-05 increments. API-03 is a separate next
implementation task at the now-documented final-output boundary; PERF-06 follows
stable serial strategy measurements. LIMIT-01/02 and Contractor scopes remain
separate. Existing resource limits and PERF-04/QED acceptance requirements remain.

## Final tested source identities

- `CalcFormConverter/ExportAssembly.wl`: `43915c625a125fd51c6c2b561b23072d745c5087ec445332fd012bcd4a85d9fa`
- `CalcFormConverter/Tests/ProcessingPlan.wls`: `2bf8d5b3d958c465ae37962a9a29e9b01b3d7f579b99a399bf4b0fa86ab3fc61`
- `CalcFormConverter/Tests/run.py`: `09744362d4c448813c18afbc73ad84c206a4d290e0973b1bd3b2a6d87f883855`
