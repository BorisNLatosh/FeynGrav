# PERF-05 coordinated work limits — 30 October 2026

**Increment 5 accepted within the focused scope below: 495/495 assertions pass.**
No commit. PERF-05 remains open for remaining dependency review and the retained
whole-call three-box/QED gates; this is not a new performance acceptance.

The baseline is `fafb84e` on `FeynArts_Integration`, plus accepted uncommitted
increment 4. One job-wide owner now coordinates optional rational passes and full
polynomial factorisation. Local eligibility guards remain in force; denial
preserves complete coefficients or restores the whole polarised master. See
[WorkLimits.md](../../CalcFormConverter/WorkLimits.md) for limits and exclusions.

## Evidence and identity

Evidence directory:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-30-perf05-budgets/`.

- `initial-hashes.json`: starting tree, including unrelated uncommitted work.
- `repair-hashes.json`: nine production source pins, unchanged during the final check.
- `reviewed-regression/`: reviewed harness, runner, manifest and static review.
- `reviewed-regression/run-REGRESSION/`: raw logs, generated programs/output,
  per-suite check ledgers, budget records, script identities, resources and termination.
- `acceptance-review.json`: reviewed acceptance summary; earlier failure records
  and `final-review.json` retain their historical status and have not been overwritten.

Executed combined-script SHA-256:
`c4d7e1655c573b6db22912449682f3bd01256d9ec25d7d176b2cf40a41bc63ca`.
Only test-helper names, separate output names and per-suite ledger retention
changed after the production repair. No assertion expectation or production
algebra was changed to obtain the final pass.

## Accepted final check

| Suite | Passing assertions | Coverage |
| --- | ---: | --- |
| WorkLimits | 162 | Actual export/FORM/import; common counters, fallback, fresh jobs, unchanged mappings, no leaked objects and exact negative control |
| CoefficientReuse | 114 | Existing 61 checks plus 53 cache-capacity, atomic factor-pair and cached-zero boundary checks |
| IntegralCollection | 69 | Pre/post collection, exact candidate/reference comparisons, protected domains, API-03 output and disabled-stage cases |
| RationalCoefficients | 38 | Symbolic-D rational identities, factors, conservative guards, multiple batches and serial/TFORM fixtures |
| PolarisationWorkBudget | 112 | Local budget boundaries, complete-master restoration, exact results, dense shared references and altered-coefficient rejection |
| **Total** | **495** | **All true; completed ledger and process exit 0** |

One fresh Wolfram kernel ran the combined batch, with serially invoked FORM/TFORM
jobs (135 calls by script inventory). Kernel elapsed time was **12,766 ms**.
The highest retained aggregate cgroup memory peak was **235,302,912 bytes**
(235.3 MB); this is a sampled cgroup high-water mark, not kernel RSS or a separately
queried final peak. The enforced limits were 1 GiB RAM, zero swap, 120 seconds plus
10 seconds grace. Fresh available memory was 10,369,932 KiB. All 128 observer
records were reviewed: no memory event, swap use or timeout; scope inactive and
removed, with no surviving owned processes. No unexpected Wolfram/FORM diagnostic
or nonempty retained stderr was found. Protected content and source pins were
identical before and after execution. User kernels were outside this scope.

The checks preserve generic symbols and symbolic D. They verify exact residuals,
complete-original fallbacks, shared collection/polarisation consumption, atomic
factor reservations, cache hits after arithmetic exhaustion and denial without
caching a false rejection. Negative controls fail equality as intended. This
establishes the selected software/algebra contracts, not an independent derivation
of loop physics or a claim of maximal simplification.

## Retained failures and their resolution

| Earlier scope | Historical result | Kernel elapsed | Peak aggregate bytes |
| --- | --- | --- | --- |
| `run-FOCUSED` | 159/162; two assertions exposed one factor-reservation bug; one fixture omitted its reduction option | 4,554 ms | 208,289,792 |
| `final/run-REGRESSION` | Corrected focused prefix reached its end; overlapping helper dispatch stopped the combined harness | 4,863 ms | 208,781,312 |
| `harness-repair/run-REGRESSION` | Filename-prefix correction did not resolve helper dispatch; batch incomplete | 4,589 ms | 209,211,392 |

The production repair explicitly evaluates the sum in the factor-pair reservation
comparison. The same audit corrected the collection cache-capacity comparison.
FORM requires preprocessor calculator braces for that arithmetic; see the
[official manual, sections 3.2 and 3.33](https://form-dev.github.io/form-docs/stable/manual/).
The direct-zero fixture now explicitly enables repeated-power reduction when
scalar-master reduction is disabled. The final tests exercise both repairs.

The subsequent failure initially looked like an output-name collision. Its cause
was overlapping Wolfram `run` definitions retained by concatenated suites: the
nine-argument reuse call dispatched to the export helper. The corrected harness
uses `runWorkLimits` and `runReuse`; expectations remain unchanged. Per-suite
ledgers are retained before proceeding. The exporter correctly refused overwrite
in the failed runs. Those runs are not counted as passes.

After the original two scopes and one harness correction were consumed, work
stopped. The user then explicitly authorised **one additional bounded check**.
That single check produced the accepted result above. No automatic retry, raised
resource limit or additional calculation followed it.

## Static checks and remaining limits

Production pins match `repair-hashes.json`; Python test-driver syntax, local
links and `git diff --check` pass. AGENTS.md remains ignored. The unrelated
`HorndeskiG4ContractedBranch.md` had changed before the first historical scope;
it was left as found and was stable throughout the accepted check. No interaction
rule, library, model or notebook was edited by this increment.

The current portable scripts correspond to the tested fixtures; only their
repository-relative loading and standalone result aggregation differ. The
combined batch is not a run of the entire converter suite. No full three-box
warmup/three-trial timing, output-size or memory gate was repeated; no full selected
QED tadpole equality or 1,490-term equality is inferred. Historical PERF-04/QED
limitations remain. Review remaining changed-dependency paths and run the agreed
whole-workload contract, including API-03 conversion, before closing PERF-05.
