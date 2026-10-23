# Polarisation coefficient grouping — 23 October 2026

## Status: implemented candidate, not accepted

Branch `FORM_Simplification`, base commit `f30dd8e3d58e8b3ffdb801eed9836caee646fc5b`,
with the earlier uncommitted export/output-sharing/rational changes preserved.
The new grouping hook is present in the working tree. **It is not ready for
acceptance or publication:** the cache repairs pass focused checks and eliminate
duplicate-write warnings, but the renewed three-box warmup still reached its
120-second scope limit. Calculations stopped at that point. See the latest
review immediately below; subsequent sections preserve the earlier evidence.

Evidence root:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-23-polarisation-groups/`.
`baseline/` contains the pre-task source; `candidate-source/` and
`candidate-hashes.json` identify this candidate, including embedded templates.
Run directories retain generated standalone programmes, raw diagnostics,
resource observations, driver identities and outcomes. Reports and counters are
claims to assess against those files, not independent mathematical evidence.

## Work reuse and renewed bounded checks — latest review

The next repair is implemented on the same uncommitted checkout. Known reference
IDs are filtered before materialisation; successful inspections and intermediate
algebra are cached with separate factorisation modes; complete denominator
subproblems are reused. Constants/monomials skip full factorisation inspection.
Second fallback rounds require a strictly smaller reachable representation.
All numerical limits and mathematical identities remain unchanged.

**Exact/software checks:** 62 focused, 57 rational, 91 scalar-master, 48 shared
output and 38 propagator assertions pass. Both complete QED zero groups remain
literal zero; the sample remains exactly equal with 6,192 → 694 leaves.
The 1,490-term comparison remains inconclusive.

**Performance:** the same 29-group diagnostic falls from 32.4 to 24.2 seconds;
rational calls fall from 2,086 to 1,262. The complete warmup still reaches
120.01 seconds (export 1.521 s, FORM 115.256 s), peak 369,774,592 bytes, no OOM/swap
or FORM warnings. The first master now finishes 138 groups, but keeps its original
representation; its complete output group matches the reference exactly under
recursive shared-reference renaming. The second master begins before timeout. Partial output is
4,122,442 bytes without the completion frame; import is not reached. No repeated
trial was run after the resource event. Final size/memory and seventeen-master
equality gates remain open; the active automatic hook remains **unaccepted**.

Full scoped changes, exact check qualifications, the corrected test-only failure,
raw evidence and the proposed next policy decision are recorded in
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-23-polarisation-work-reuse/review.md`.
The 1 GiB/no-swap/120 s + 10 s/2 GiB-headroom limits were unchanged. All 162
protected files are unchanged. No commit was made.

## Earlier cache repair and repeated bounded checks

The user authorised this repair after the preceding timeout. Evidence is in
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-23-polarisation-cache-repair/`.
`repair.patch` isolates this turn's source/test edits from the already dirty
checkout; `candidate-hashes.json` identifies the revised source and templates.

### Implemented repair

- Both scalar-registration branches now assign `cfcPGSize` once per entry.
- A FORM-job-local cache reuses numerical eligibility rejections for an identical
  sorted shared expression. Unknown metadata is not cached; changed expressions
  are inspected afresh. Explicit brackets make the complete coefficient the key;
  oversized keys bypass caching through the alternative `Collect` head.
- Completed normalised coefficient candidates are cached before reattaching
  common content. Each use still performs the original per-group and whole-master
  representation comparison. The cache holds shared expressions, not expansions.
- A second fallback round is skipped when the first leaves the sorted shared
  expression unchanged. A still-rejected coefficient is not submitted again
  merely to enable a factorisation hook that requires rational eligibility.
- Diagnostics include rejection-cache hits, completed-candidate hits and unchanged
  fallback rounds. Limits, mathematical formulae, mapping and import formats are
  unchanged. These caches are job-local and are not memory guarantees.

### Reviewed final-candidate results

| Check | Retained run | Result |
| --- | --- | --- |
| Focused polarisation suite | `run-focused-final` | **Exact/software checks:** 57/57, no diagnostics; includes repeated rejection, changed coefficient and subsequently resolved metadata |
| Complete 120- and 386-term groups | `run-qed-final` | **Exact check:** both literal zero; 0.332 and 0.993 seconds |
| 30-term sample | same | **Exact check:** rational difference zero; 6,192 to 694 leaves; 0.162 seconds |
| 1,490-term group | same | Fallback exercised, 48 cached rejections; 329,285 to 41,784 leaves. Full symbolic equality remains **inconclusive** |
| Three-box warmup | `run-boxes-warmup` | **Failed performance gate:** 120.07-second scope timeout; no import or complete output |

The three-box run emitted **zero cache-table warnings**, compared with 1,748 in
its predecessor. Export took 1.317 seconds and the captured FORM interval was
115.348 seconds. Peak aggregate memory was 332,869,632 bytes, with zero sampled
memory-limit/OOM events and zero swap. Its partial 4,088,389-byte file lacks the
completion frame; its size is not an accepted output-size result. No full master
summary was reached, so this run does not quantify the completed-candidate cache's
benefit. Repeated rejected-coefficient work was reduced on the retained small
case, but the large-workload timeout is unresolved.

All runs used 1 GiB aggregate memory, no swap, 120 seconds plus 10 seconds grace,
headroom of at least 2 GiB and fresh sequential task-owned kernels. The final
focused and QED runs peaked at 250,146,816 and 382,275,584 bytes respectively.
`resource-review.json` confirms empty control groups and no surviving owned
processes, including the timed-out run. No user kernel was touched. Calculations
stopped after the timeout: remaining regressions, three measured trials, full
selected-tadpole equality and separate selected import were **not run**.

### Retained development failure

The first rejection-cache candidate omitted the bracket required by FORM's
`Collect`; its unset key incorrectly shared rejection decisions across expressions.
Its 53 original focused assertions passed but the QED zeros failed (`run-qed`).
That candidate and evidence are retained in `failed-candidate/`. The key was
corrected and four direct cache assertions were added. The corrected intermediate
and final candidates passed 57 assertions and the two QED zeros. No failed result
is counted as acceptance. The large group's `Expand` comparison returning a
nonzero representation is recorded as inconclusive, not as proof of inequality.

The runner's initial guard also mistook the previous **failed but empty** scope
for active work. Its local copy now permits `inactive` or `failed` only when the
control-group path is empty. No scientific process started during that refusal;
no historical scope was reset or signalled.

### Decision

The duplicate-write repair and focused behaviour checks pass. The complete
polarisation stage remains **unaccepted** because the large workload still times
out. Further work must identify the dominant bounded denominator/factorisation
and registration costs before another workload attempt; caching alone has not
established an acceptable runtime. Full-tadpole and 232-group acceptance gates
also remain open. The 162 protected notebook/rule/library/model files match their
retained hashes. No commit was made.

## Original implementation and historical evidence

- Mapping-driven polarisation atom classification in `RationalCoefficients.wl`.
- A dedicated native FORM template for shared-definition decomposition,
  structural collection, per-group rational arithmetic, bounded factorisation
  and deterministic representation selection.
- The existing final assembly hook and shared writer now register transitive
  dependencies of new factors. The restricted incremental reader is unchanged.
- Installer dependency listing and focused test entry point include the new
  template and suite. No new public option, result head or mapping version.
- Symbolic D, complex coefficients, polarisation conjugation labels and existing
  protected tensor paths remain part of the contract. No physical kinematic
  relations or Wolfram `Simplify`, `Together` or `Factor` are used in production.

Private limits remain: 1,024 structures/master, 10,000 structural terms/layer;
rational estimates 1,000 terms, 12 variables, degree 24; full factorisation 250
terms, eight variables, degree 12. These are work selectors, not memory or
factorisation-time guarantees. The FORM manual's sections 7.56, 8.11 and 11 explain
`FactArg`, native `denom_` and polynomial arithmetic:
https://form-dev.github.io/form-docs/stable/manual/.

## Exact and bounded checks

| Check | Evidence | Result and qualification |
| --- | --- | --- |
| Complete 120-term QED group | `run-native-unwrapping-qed/` | **Exact check:** literal zero |
| Complete 386-term QED group | same | **Exact check:** literal zero |
| 30-term QED sample | same | **Exact check:** rational difference zero; 6,192 to 694 leaves |
| Polarisation suite | `run-native-den-focused/` | **Exact/software checks:** 53/53; before the final native-denominator matching correction |
| Existing final-rational suite | `run-rational-final/` | 57/57 on the earlier candidate |
| Scalar-master suite | `run-masters-final/` | 91/91 on the earlier candidate |
| Shared-output suite | `run-sharing-final/` | 48/48 on the earlier candidate |
| Propagator-rational suite | `run-propagators-final/` | 38/38 on the earlier candidate |
| Largest 1,490-term QED group | `run-native-unwrapping-qed/large.json` | Conservative fallback exercised; 329,285 to 41,784 leaves; full symbolic equality **inconclusive** |
| Largest-group sample evaluations | `run-large-evaluations/` | Three exact rational evaluations agree on the preceding candidate; this is pointwise evidence, not a symbolic identity proof |

The focused suite includes repeated/distinct/unit structures, powers,
polarisation–momentum and polarisation–polarisation contractions, opposite
conjugation labels, complex arithmetic, symbolic D, separate masters,
factorisation, polarisation denominators, a deliberately altered negative control,
manual export/import, seven-character import chunks and serial/TFORM agreement.
It checks actual degree/variable boundaries and lowered private term/structure/
factorisation-degree boundaries. It does **not** establish all requested default
boundary values or every Dirac/colour/epsilon combination. An opaque-function case
preserves the converter's existing rejection tag; it is not a successful new
function-support test.

The final native-denominator correction passed the retained small QED checks.
The earlier full regression counts must not be presented as a completed regression
run of that last correction. All comparisons kept generic invariants and D;
no on-shell conditions or assumptions were introduced.

## Complete selected tadpole: still an open acceptance gate

The saved diagnosis establishes 232 nonzero structural groups after expansion of
its original multiplier. The prototype's shared representation instead exposed
328 raw structural keys, with cancellations still hidden inside scalar blocks.
The implementation has **not** yet reproduced the required 232-group result in
its complete retained-workload path.

`run-selected-final/` completed a FORM-only diagnostic on an earlier candidate in
54.92 seconds. Its whole-master score retained the original shared multiplier
(`selected=0`), and the complete native difference had zero terms. Individual
candidate groups showed 135 smaller forms, but the assembled representation was
rejected to preserve sharing. This is an exact preservation check on that
reconstructed diagnostic input, **not** proof that the final candidate simplifies
the full tadpole, nor an independent certification of its diagnostic reader.

That earlier run reported 328 raw keys, zero eliminated groups, 328 guarded/partial
groups and 11,806 factorisation calls. Its FORM running-time counters were
9,881 ms grouping/scoring, 26,704 ms rational processing and 12,133 ms factorisation.
These counters are not external wall times and do not describe the final source.

The final source's complete tadpole equality and separate selected-coefficient
import were **not run** after the three-box timeout. No claim that the whole
multiplier vanishes is made. The two separately reproduced zero groups cannot
supply the missing full-multiplier check.

## Three-box performance

One warmup and three measured sequential fresh-kernel trials completed on the
**earlier candidate that rejected native dimension denominators**. All three
retained exactly 4,364,149 output bytes and 2,366 shared definitions. Mapping and
imported WXF hashes matched the reference exactly. Raw output hashes differed
only through private reference numbering; `reference-renaming-check.json`
checks the correspondence after removing insignificant whitespace.

| Measurement, seconds | Mean | Median | Minimum–maximum |
| --- | ---: | ---: | ---: |
| Export | 1.637 | 1.601 | 1.535–1.775 |
| FORM | 5.188 | 5.104 | 5.103–5.356 |
| Import | 5.462 | 5.462 | 5.416–5.509 |
| Total | 12.287 | 12.307 | 12.212–12.341 |

Peak aggregate cgroup memory ranged from 391,340,032 to 392,093,696 bytes;
median 391,929,856 bytes. This preserved the approximately 392 MB baseline.
The previous pipeline median was 8.915 seconds, so this candidate added time.
Its grouping work fell back before coefficient arithmetic; no factorisation
speedup is implied. This workload **does contain encoded polarisation vectors**.
The pass-through measurements are not final-candidate acceptance measurements.

After recognising FORM's native denominators, `run-native-complete-warmup/`
reached the 120-second scope timeout. Export took 1.312 seconds; the captured
FORM portion ran about 115.52 seconds before termination. Import was not reached.
Its 4,088,389-byte partial output lacks the final frame and is invalid; it is not
an output-size improvement. Peak aggregate memory was 324,718,592 bytes, with no
sampled OOM, memory-limit or swap event. The final three measured trials were not
started.

Power state and external background load were not independently controlled;
timings are local measurements. The timeout and duplicate-table warnings are
hard acceptance failures regardless of that qualification.

## Failures, stop and process ownership

The final log contains 1,748 `Table element was already defined` warnings.
Static inspection identifies two writes to `cfcPGSize` when a scalar block is
fractional: the initial `1` is overwritten by the fraction marker. Each branch
must write its cache entry once. The captured log also shows many repeatedly
rejected high-cost coefficient attempts. Their exact contribution to runtime has
not been profiled independently.

`supervisor-termination-review.json` records the final scope's `Result=timeout`,
empty control group and zero surviving task-owned processes. Existing user
kernels were untouched. The runner lacks a normal primary-exit/completion receipt
because the scope terminated the process tree; the failed outcome is retained.

Earlier development attempts include FORM declaration/order/matching errors,
diagnostic reconstruction mistakes and a harness expectation mismatch. The
`run-structural-focused` raw 35 assertions passed, but its wrapper incorrectly
expected 35 aggregate results rather than one and rejected the run. These files
remain historical failures and are not counted as clean acceptance. Later
clean focused runs replaced their corresponding test claims. Native denominator
matching required an argument-field wildcard; an invalid field unwrap caused a
retained FORM failure before the corrected small QED checks passed.

## Remaining work before acceptance

1. Profile the remaining denominator/factorisation and registration work. The
   duplicate-write and unchanged/repeated-coefficient repairs are now implemented
   and pass their focused checks, but have not resolved the three-box timeout.
2. Investigate further bounded work reduction, including trivial factorisation
   attempts, without changing algebra, raising limits or splitting arbitrary
   summand batches. Measure the benefit on the retained workload.
3. Reconcile 328 raw shared keys with the retained 232 expanded structures,
   reproduce the complete-group cancellations in that path and complete an
   independent FORM equality check of the entire selected tadpole.
4. Repeat final-source regressions and the requested complete three-box warmup
   plus three trials. Require no size increase and investigate memory above 10%
   of the baseline. Complete separate selected-coefficient import measurements.
5. Finish the omitted boundary/protected-path checks and update this report.

No further scientific runs were launched after the timeout. Resumption requires
renewed authorisation under the requested resource-event stop rule. No commit,
publication, notebook edit, interaction-rule change, library regeneration or model
change was made. `protected-check.json` verifies all 162 protected files unchanged.
