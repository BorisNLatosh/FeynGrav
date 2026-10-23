# Scalar-master export planning — 23 October 2026

**Subsequent result:** the later [output-sharing check](MasterOutputSharing-2026-10-23.md)
completed this generic three-box import and verified all 17 master coefficients
against the retained output. The measurements and incomplete-trial descriptions
below remain the historical record of the earlier export-only changes.

## Scope and status

Uncommitted work on `FORM_Simplification`, based on
`f30dd8e3d58e8b3ffdb801eed9836caee646fc5b`.

The two identified export costs are corrected: coefficient registration no longer
repeatedly decodes and scans accumulated entries, and reduction planning follows
co-occurring denominator families and their possible pinches. Mathematical
recurrences, public options, mapping versions and import algorithms are unchanged.

**Qualified result:** export of the reported generic three-box input now finishes
in approximately 1.6 seconds. Its complete scalar-master result is **not yet
validated**: FORM produced 236,271,618 bytes and the bounded trial reached its
120-second time limit during import. This report does not claim that the complete
box calculation now fits the resource budget, or that its full result has been
independently checked. The smaller triangle reproducer completed and passed its
exact combined-versus-separate comparison.

## Implementation

- `LoopCoefficientReduction.wl`: initialise an exact held-expression registry
  once per job from scalar/abbreviation entries, preserving the first match,
  scalar validation, literal integer/rational output and identifier allocation.
  Newly encoded entries join the registry immediately; nothing persists between
  exports. This is structural equality, not algebraic equivalence.
- `ExportAssembly.wl` and `TensorDecomposition.wl`: retain private denominator
  support sets from the normalised expression. Sums carry alternatives; products
  combine supports; positive powers include mixed alternatives. No numerator
  expansion is required. Above 256 alternatives, union the supports as a safe
  over-approximation. Loop-independent denominators are removed when mapping
  identifiers to normalised loop slots.
- `LoopCoefficientReduction.wl` and `ScalarMasterReduction.wl`: obtain saved
  coefficient supports separately; prepare the downward closure of actual
  families, including same-route partial fractions, raised powers and pinches.
  Canonicalisation also chooses representatives from that closure. Existing
  callers without support information retain dictionary-wide preparation.
- These support sets are export-only data, not new fields in saved mappings.
  New mappings can contain fewer unused coefficients and generic restrictions;
  their digests consequently need not match the baseline after family pruning.
- `Tests/ExportPlanning.wls` supplies focused regressions and is registered in
  the existing `runtime` suite. Its generic fallback is tested explicitly.

No rules, stored libraries, FeynArts models or notebooks changed.

## Exact checks

| Check | Result and boundary |
| --- | --- |
| Registry-only baseline comparison | 10/10 assertions: four fixtures have byte-identical FORM programmes and mapping JSON; duplicate first-match behaviour and completed combined export checked |
| Family planning | 29/29 assertions: support discovery, powers, rational/coincident routes, spectators, conservative fallback, 31-family box closure, saved families/products, pinches, rank-four box coefficients, unchanged fixed-power mode, cache isolation, independent bubble reference and serial/TFORM agreement |
| Existing scalar-master regression | 91/91 assertions, including independent FeynCalc comparisons through rank six, zero/singular invariants, scaleless behaviour, malformed mappings, stream cleanup, manual execution/import, small chunks and matrix/epsilon spectators |
| Existing repeated-power regression | 131/131 assertions, including saved coefficients, routing, exceptional retention, collection and shared readers |
| Retained scalar triangles with massless Mandelstam setup | Complete call 0.395 s; exact residual zero against the sum of separately reduced terms after scalar `PaVeOrder` normalisation |
| Full generic rank-four three-box result | **Inconclusive**: export and FORM completed; import and independent full-result comparison did not complete |
| Full legacy/core/runtime suites and notebook calculations | **Not tested in this task**; focused suites above are not a claim of exhaustive regression coverage |

The existing scalar-master fixture emits one import partial-reduction message
for `PaVe[0,0,{0,0,0},{a,a,a},...]`. The original run passed all assertions but
was rejected by the wrapper's empty-diagnostic rule. A diagnostic run retained
logs per call, identified this exactly singular triangle, and again passed
91/91. An unpruned reference returned the identical retained coefficient and
message. This is expected conservative behaviour, not a newly suppressed warning.
The focused polynomial test similarly retains its expected partial-reduction
message in a separate file and checks its identity.

Earlier unsuccessful checks are retained: the first registry harness did not
permit file overwrite and used an incorrectly qualified dimension in a direct
fixture; the first family candidate had a missing closing bracket; two later
reference comparisons mixed FeynCalc internal/external vector notation. These
were corrected. The notation diagnostic gave zero residual against independent
`TID`. Failed runs are not counted as passes.

## Export performance

Three sequential fresh kernels, each with one warmup and one measured export per
configuration/workload. All compared jobs use default scalar-master reduction.
“Registry only” uses the new registry with conservative unpruned family planning;
“both fixes” adds family tracking. No FORM execution occurs in these export
measurements. The triangle numbers are three cold observations, without a separate
triangle warmup.

| Workload/configuration | Mean (s) | Median (s) | Range (s) |
| --- | ---: | ---: | ---: |
| One box, registry only | 0.561 | 0.570 | 0.535–0.577 |
| One box, both fixes | 0.522 | 0.535 | 0.483–0.547 |
| Three boxes, registry only | 5.029 | 5.064 | 4.840–5.183 |
| Three boxes, both fixes | 1.602 | 1.576 | 1.474–1.756 |
| Combined scalar triangles, both fixes | 0.240 | 0.254 | 0.205–0.262 |

The registry parity run separately measured one baseline box at 6.724 s and its
registry-only counterpart at 0.578 s. These are single observations. The earlier
35-second interrupted combined-export profile, with 34.30 s in lookups, is
historical diagnostic evidence, not a completed baseline timing.

The box support closure contains 6 singles, 12 pairs, 10 triples and 3 boxes:
31 families instead of 56. Generated FORM text fell from 864,668 to 351,030 bytes
(59.4% smaller). This programme-size reduction does not imply smaller algebraic
results. Export timing scopes peaked at 394,100,736–394,293,248 aggregate cgroup
bytes, including warmups and both configurations, not isolated export RSS.

Host: Intel i5-1235U, FORM 5.0.2, FeynCalc 10.2.1 (local package declaration),
Wolfram kernel from the installed 15.0 executable path. AC online was observed
after the export trials;
battery status was “Not charging”. Background load and CPU frequency were not
controlled, so these are local measurements, not portable performance guarantees.

## Complete-call trial and resource limits

The separate instrumented trial retained stage timings and output files:

| Input | Export (s) | FORM process wrapper (s) | Import (s) | Complete call |
| --- | ---: | ---: | ---: | --- |
| Scalar triangles | 0.234 | 0.036 | 0.048 | 0.395 s, result compared exactly |
| Generic three boxes | 1.389 | 21.107 | Incomplete | Scope timed out at 120 s during import |

The process-wrapper times include its launch/polling overhead. Availability
probes are separate records. The triangle output was 362 bytes; the box output
was 236,271,618 bytes. Peak sampled aggregate cgroup memory in this combined
trial was 1,022,898,176 bytes. No sampled memory-limit/OOM events or swap use
occurred. Systemd recorded `Result=timeout`, an empty control group and no
surviving observed task processes. There was no retry or limit increase.

Every scientific kernel ran sequentially under whole-tree `MemoryMax=1G`,
`MemorySwapMax=0`, a 120-second limit plus 10-second grace, and a fresh host
headroom check of at least 2 GiB. User kernels were untouched. A harmless policy
payload verified the adapted launcher first. Successful focused scopes peaked
at roughly 270–325 MB; these are cgroup charges, not Wolfram process RSS.

No independent numerical integral checks or full benchmark suite were run.

## Retained evidence

Local evidence root:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-23-perf03/`.
It contains baseline source copies, input, scripts as run, source hashes,
assertion logs, raw diagnostics, output, resource samples and completion records.

Key directories are `run-registry-corrected`, `run-planning-final`,
`run-master-regression`, `run-master-diagnostics`, `run-power-regression`,
`run-benchmark-1` through `run-benchmark-3`, and `run-end-to-end`.
The last is explicitly incomplete, not an accepted complete execution.
`benchmark-summary.json`, `host.json`, `final-source-hashes.json` and
`run-end-to-end/manual-resource-review.json` summarise the retained records.

The final edit after the calculations moves a routing comment beside its
function and registers the new test in the Python driver. Neither changes the
Wolfram computation; the driver was syntax-checked, not run as an entire suite.
