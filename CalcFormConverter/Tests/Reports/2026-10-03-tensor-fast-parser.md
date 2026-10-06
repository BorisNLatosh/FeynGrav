# Bare tensor factors in the fast importer — 3 October 2026

## Outcome

Keep the narrow extension. On the retained 10,033,002-byte partial-bubble output, median public import time fell from **30.236768 s to 12.994945 s**: **57.02% less time**, or **2.327× speedup**. The scalar control showed no material regression. Exact `SameQ` comparisons passed for all three fixtures; all 515 regression assertions passed. These are local observations, not portable speed guarantees.

## Change

The fast lexer now accepts complete, unpowered `cfvN(cfiM)` components and `d_(cfiM,cfiN)` metrics. Tensor powers and nested syntax still fall back. Reconstruction, typed validation, error order, local cache lifetime and the public API are unchanged. The general parser already cached tensor reconstruction; this change reduces general arithmetic-parser dispatch for tensor-bearing results.

## Timing

| Fixture | Baseline median (range), s | Candidate median (range), s | Time reduction |
|---|---:|---:|---:|
| 250,114-byte tensor subset | 0.662198 (0.638693–0.702438) | 0.298672 (0.293282–0.330687) | 54.90% |
| 10,033,002-byte partial bubble | 30.236768 (29.969670–30.631674) | 12.994945 (12.775178–13.810757) | 57.02% |
| 320,070-byte scalar control | 0.804276 (0.796718–0.814988) | 0.786896 (0.785741–0.801498) | 2.16% |

The scalar ranges overlap; its 2.16% median difference is not claimed as an improvement. Tensor ranges are clearly separated. Earlier notebook timings came from another kernel/session and are not used as the baseline here.

## Memory

| Fixture | Baseline / candidate measured-import peak RSS, MiB | Baseline / candidate kernel memory retained after five imports, MiB |
|---|---:|---:|
| small | 350.16 / 328.27 | 44.67 / 17.39 |
| partial | 2244.89 / 715.09 | 1779.42 / 42.97 |
| scalar | 305.39 / 304.91 | 3.86 / 3.86 |

RSS was sampled every 50 ms and may miss brief peaks. The table takes the largest RSS sample during the three measured import phases, including memory retained from earlier calls. Kernel retention is measured after clearing the result, relative to before the first import; it includes persistent library/parser state and untimed validation effects, not an isolated allocation attribution. Raw per-call counters are retained in JSON.

Validation is separate: partial-bubble validation-phase RSS reached about 4.62 GiB for the baseline and 2.90 GiB for the candidate. Hashing, leaf counting and WXF serialisation are outside import timers. Separate exact comparison has its own process record. These measurements do not establish the cause of the previous Full-suite OOM.

## Method and provenance

- Wolfram 15.0.1 for Linux x86 (64-bit), FeynCalc 10.2.1. No FORM calculations were rerun. Baseline and candidate source snapshots were immutable during measurements; source and fixture SHA-256 hashes are in the JSON report.
- One fresh kernel per implementation per fixture. Each performed one separately recorded first import, one warmup, then three measured imports. Execution order was baseline/candidate for the subset, candidate/baseline for the partial bubble, baseline/candidate for the scalar control. These are three within-kernel samples, not three independent kernel-pair replicates.
- Public `CalcFormImport` alone was timed with `AbsoluteTiming`; `MaxMemoryUsed` wrapped the timing expression. `$HistoryLength=0`. Hashing, `LeafCount`, output serialisation and comparisons were outside timers; results were cleared between calls. The same result/mapping bytes were used by both versions.
- Exact `SameQ` was checked in a separate kernel on trusted WXF results emitted by the two implementations. Every repeated import also had the same SHA-256 expression fingerprint. The 10 MB result had 9,930,773 leaves.
- The small fixture takes complete initial terms from the retained output, cutting at the first top-level additive boundary after 250,000 body characters. The scalar control repeats `2*cfs2^2/3*cfa1` 20,000 times, using the same mapping. Fixture creation preceded timing.
- Enforced limits: 300 seconds per phase, 6 GiB kernel-process RSS, at least 2 GiB system available memory. Monitoring covered preparation, import and validation; no limit was hit and all processes exited zero. No simultaneous benchmark kernels ran.
- Retained local source snapshots, harness, generated scripts, logs, results and process records: `/tmp/cfc-tensor-fast-20261003-140233`. This temporary directory is local evidence and may be cleaned by the operating system; the report JSON preserves measured observations and hashes.
- Exact development harness invocation: `python3 /tmp/cfc-tensor-fast-20261003-140233/bench.py prepare`, then `python3 /tmp/cfc-tensor-fast-20261003-140233/bench.py run`. Kernel commands and their artifact paths are in the JSON. The development harness requires the retained source/fixture paths and is not a new package API or prerequisite for the shipped benchmark notebooks.

## Regression checks

`python3 CalcFormConverter/Tests/run.py --suite core` and `--suite form` passed. A final isolated Parser run included the added custom tensor-head evaluation-count case: 121 parser assertions. Counts: Core 111, Parser 121, Transactions 20, Installer 39, FORM export 31, FORM stages 128, FORM import 65; total 515, zero failures, including 39 new parser checks. No Full-bubble integration suite was run.

New checks cover fast dispatch, general-parser fallback for powers/nesting, exact result/failure agreement, unknown and incorrectly typed identifiers, malformed input, signs/division, Unicode whitespace, dimensions, mapping/cache isolation, scalar-product definition changes and custom tensor reconstruction evaluation counts.

No commit was made. Existing unrelated working-tree changes were preserved.
