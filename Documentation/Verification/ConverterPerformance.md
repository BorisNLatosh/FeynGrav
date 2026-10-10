# Converter performance history

Archived on 6 October 2026 from the developer guide. These are measurements from different revisions and workloads, not a rerun of the current source. Preserve their baselines, raw observations and qualifications when citing them. The [current developer guide](../../CalcFormConverter/DEVELOPER.md) describes maintenance responsibilities.

For later complete-calculation observations and review corrections, see the [10 October record](../../CalcFormConverter/Tests/Reports/EarlyDimensionCompaction.md#follow-up-complete-calculation-optimisation). Its timings use another workload and revision; they do not replace the historical measurements below.

### Earlier recorded performance checks

On the development machine, a retained 636 KB result containing 6,244 terms was used for paired full-import comparisons. The original importer took 7.626 and 7.683 seconds; the optimised importer took 2.989 and 3.573 seconds in the corresponding runs. All imported expressions were exactly equal under `SameQ`. These are measurements on one result, not a general performance guarantee.

A separate bounded comparison used the retained program that produced that result:

| Program structure | Serial FORM | TFORM, four workers |
| --- | ---: | ---: |
| One defining module | 0.861 s | 0.921 s |
| Staged multiplication in original factor order | 0.338 s | 0.180 s |

All four result files were byte-for-byte identical. These single-run measurements illustrate the effect of the generated program's structure; the complete scalar bubble was not evaluated for this comparison. Export-registry microbenchmarks also showed approximately 19–23% improvement on synthetic repeated-symbol expressions, with identical generated data.

Recorded verification covered all four suites, package loading, exact equality on the retained 6,244-term result, full-bubble export metadata and independent staged-program checks under TFORM. Assertion counts change as cases are added; use the current test output for the current revision. No system packages were installed during testing.


## Performance measurement

Separate in-memory conversion, file I/O, external FORM execution, import and notebook display. Full calculation comparisons should use `AbsoluteTiming`; an internal `buildExportData` measurement does not include rendering, file writes, the availability probe or FORM. `ShowTiming` reports elapsed execution time, not total calculation time or aggregate worker CPU time.

For a small change, compare baseline and candidate in the same fresh kernel, warm the relevant paths, alternate measurement order, repeat bounded runs and compare outputs. Synthetic examples help identify repeated work but do not establish an end-to-end improvement. Preserve malformed-input checks alongside successful examples. Use exact `SameQ` when a refactor is intended to preserve reconstruction; use an appropriate algebraic comparison for legitimate FORM transformations.

### Subsequent targeted measurements

On Wolfram 15.0.1, a 636 KB retained result with 6,244 terms was used to compare the already optimised token reader against additional parser changes: branch-local allocation, cached original token count and sentinel lookahead. Three counterbalanced full-import pairs were:

| Pair | Token-reader baseline | Additional parser changes |
| --- | ---: | ---: |
| 1 | 3.484508 s | 2.256755 s |
| 2 | 3.281041 s | 2.165659 s |
| 3 | 3.078409 s | 2.036773 s |

The medians were 3.281041 and 2.165659 seconds, about 34% lower. Every result was `SameQ`; focused malformed/valid inputs and the existing parser regressions also passed. The comparison did not rerun FORM, and it does not predict the speedup of a complete calculation.

A separate registry comparison constructed export data for a representative expression with 178,961 leaves. Four alternating pairs had medians 1.385728 and 1.274246 seconds, about 8% lower, with identical export data. This measurement excludes rendering and writing files. It is distinct from the earlier repeated-symbol microbenchmarks above.

These are recorded development observations, not performance requirements or current-machine guarantees. Later regression counts and timing runs should be reported with their actual revision, environment, scope and baseline. Faster functional syntax is not an optimisation rule: fresh symbols, repeated evaluation, list copying and built-in bulk operations must be assessed in the actual path. Retain validation, evaluation order, compatibility and cleanup behaviour when optimising.

<a id="polarization-identities"></a>

### Polarisation identities

`vectorIdentityQ` recognises momentum symbols and constrained FeynCalc
polarisation identities. `physicalMomentumLabelQ` also permits exact rational
linear momentum labels after routing substitutions. Register the complete
polarisation as one vector; do not distribute it over that routing. Dedicated encoding/decoding preserves the `I` versus
`-I` label and an optional Boolean `Transversality` setting without enabling
general rule decoding. Vector reconstruction still goes through `Momentum`
and `Pair`, so current FeynCalc scalar-product definitions apply at import.
Propagator routing explicitly excludes polarisations. Keep tests for free
components, contractions, conjugation, transversality, dimensions and rejected
identities when extending this vocabulary.


### Connected tensor stage ordering

`connectedStageOrder` computes index multiplicities recursively without distributing products. `Plus` branches must have identical signatures; `Times` adds counts and nonnegative integer powers scale them. Native `Pair` signatures are cached locally. Any inconsistent sum or index occurring more than twice makes the planner retain the original stage order. This also protects existing behaviour for ambiguous repeated-index expressions. Internal dummy indices have multiplicity two and are excluded from the open-index set. Scalar-only stages retain their original order when the whole product has no open indices.

For eligible products, the next stage maximises the number of shared open indices, then prefers a tensor stage, smaller `LeafCount`, and original position. Shared indices leave the active set after contraction. This is an inexpensive deterministic heuristic, not an optimal contraction-tree search. Serialisation, validation and macro registration precede planning; version-one mappings, fingerprints, failure behaviour and factor definitions remain unchanged. Runtime and importer code are unchanged.

#### Measurement against b73c1a7

On 2026-09-29, FORM/TFORM 4.3 on an Intel Core i5-1235U (10 physical/12 logical CPUs) gave the following external-process wall times. Baseline and candidate used identical exported expressions and mappings, with only stage order changed. Runs were sequential and alternated order. The smaller job contains one complete quadratic-gravity vertex, one propagator and a projector; the larger contains a vertex, two propagators and a projector. They are substantial partial products of `ScalarBubbleExample`, not the complete bubble.

| Input / engine | Pairs | Baseline median | Candidate median | Speedup | Wall-time reduction |
| --- | ---: | ---: | ---: | ---: | ---: |
| 89,320 leaves / FORM | 7 | 0.600567 s | 0.112637 s | 5.33x | 81.24% |
| Same / TFORM 2 workers | 7 | 0.384959 s | 0.104414 s | 3.69x | 72.88% |
| Same / TFORM 4 workers | 7 | 0.293125 s | 0.109719 s | 2.67x | 62.57% |
| 90,489 leaves / TFORM 4 workers | 3 | 23.185782 s | 1.388039 s | 16.70x | 94.01% |

Small scalar, free-index tensor and polarisation controls also produced identical outputs under FORM and TFORM with four workers. Two alternating pairs after warmup took roughly 2.8–5.6 milliseconds; process startup dominates, so no speedup is claimed for those controls. Some measured differences were small slowdowns below 0.3 milliseconds.

The larger baseline ranged from 23.120–23.666 s and candidate from 1.344–1.414 s. Output files were byte-identical: 6,244 terms / 624,358 bytes for the smaller job, 79,449 terms / 10,038,006 bytes for the larger. The actual generator reproduced the measured programs, apart from blank lines and isolated result paths; mappings were byte-identical. The smaller baseline's intermediate outputs were 31, 86, 6,244 terms, versus 3, 882, 6,244. The larger baseline produced 31, 4,293, 11,123, 79,449 terms, versus 3, 882, 6,244, 79,449.

Export is separate. Four alternating public-export pairs had medians 0.575905 s baseline and 0.638233 s candidate on the larger partial (+0.062328 s), and 1.142099 s versus 1.239619 s on the full bubble (+0.097520 s). These include writing and overwrite handling; the first pair was cold. Import and complete-call timings were not benchmarked. Byte-identical outputs leave import work unchanged, but that is not an end-to-end timing measurement.

The full bubble baseline timed out after 40 s at four workers; the candidate also timed out in a separate 30 s pilot. **Those stage-order measurements did not establish a complete-bubble speedup.** The subsequent normalisation measurements below use that connected-stage version as their baseline. A serial larger-partial baseline also exceeded a 40 s pilot limit, so it is excluded from the timing table. The laptop has heterogeneous cores, dynamic frequency and background desktop activity (initial load averages 1.42/1.57/1.42); timings are observations on this machine, not guarantees. Worker counts were controlled; CPU affinity and governor were not changed. No other heavy benchmark ran concurrently. No memory or disk-buffer settings were tuned.

#### Reproducing an execution comparison

Export the same expression with baseline and candidate revisions and preserve both mappings. `Tests/BenchmarkFORM.py` redirects each generated result into a separate new directory, warms both programs once, then runs alternating pairs sequentially. It checks the complete output digest after every run, records bytes, elapsed seconds and logs, and rejects failures, timeouts or unequal outputs. It does not run export/import or alter the original artifacts.

```sh
python3 Tests/BenchmarkFORM.py baseline.frm candidate.frm \
  --workers 4 --pairs 5 --timeout 60 --output /tmp/cfc-comparison
```

Use `--workers 1` for serial FORM. Use fresh kernels for export, and keep factor/dummy-index identities fixed between revisions. Historical experiment inputs, scripts and logs from this measurement were retained under `/tmp/cfc-form-perf-20260929`; these temporary files are not repository fixtures. Regression cases in `Tests/FormStages.wls` compare independent monolithic and staged programs, including free indices, contraction loops, internal dummy indices, polarisation and conservative fallback cases.

#### Sources and interpretation

The official [FORM reference manual](https://form-dev.github.io/form-docs/stable/manual/) explains that `.sort` combines terms, too many or too few boundaries can both hurt performance, and a newly defined expression starts as one input term, limiting parallelism in that module. The specialised [FORM benchmark discussion #702](https://github.com/form-dev/form/discussions/702) recommends repeated representative jobs and discusses noisy hosts. [FORM performance discussion #859](https://github.com/form-dev/form/discussions/859) shows sensitivity to thread placement and cache topology. These are documented observations and established measurement considerations. The connected-index ordering rule is this converter's heuristic, motivated by reducing intermediate products and validated only on the cases above; those sources do not guarantee its speedup.

Verification passed 421 named assertions across the core, FORM, runtime and integration suites, plus automatic-loading/namespace and full-bubble metadata checks. Runtime process tests required running outside the execution sandbox. The benchmark driver was additionally checked with paths containing spaces, deliberately different output, and a forced timeout.


<a id="normalizing-large-form-stage-factors"></a>

### Normalising large FORM stage factors

A second bounded optimisation round used the **uncommitted connected-stage implementation above as its baseline**, not b73c1a7. Its converter snapshot SHA256 is `719e8a04a53c2aa44fd0f199e42ff4ace2b954d1a44edc68c0d26a5874879ecf`; the complete snapshot and experiment files are in `/tmp/cfc-form-perf-round2-20260929`. These are incremental measurements and must not be presented as another comparison with the original revision.

`stageIndexSignatures` now supplies the shared eligibility check for ordering and preparation. When signatures are valid, tensor indices are present, and at least one stage has 1,024 leaves, the exporter records the ordered stage factors in its private `Preparations` field. Rendering defines `cfcStage1`, etc., normalises them in one module, then explicitly hides them. The result modules multiply these expression references in the existing connected order. This combines duplicate terms and expands short momentum routing once per factor instead of repeating that work for every incoming term.

The 1,024-leaf threshold is a conservative small-job bypass, **not an empirically optimal crossover**. There is no normalisation for inconsistent signatures or an index appearing more than twice. This restriction matters even when stage order would remain unchanged: independently contracting malformed index expressions could alter their existing behaviour. No `Sum`, `Renumber`, `PushHide` or `PopHide` is emitted. Declared named indices retain their identities; internal contractions belong to their own factor and free indices can still contract with later factors. Scalar master functions, abbreviations and denominators retain their existing meanings. Mapping bytes and result format are unchanged.

Hidden factors persist until the generated program ends. The full-bubble diagnostic reported 149,036 bytes of combined auxiliary expression contents, excluding FORM buffers and other overhead. Peak process memory and peak scratch usage were not measured. As documented in the [FORM manual's Hide section](https://form-dev.github.io/form-docs/stable/manual/#hide), hidden storage can spill to disk according to `ScratchSize`. This trades additional temporary storage and one preparation module for less repeated algebra; it is not a universal performance guarantee. The [specialist issue #828](https://github.com/form-dev/form/issues/828) illustrates interactions between ordinary Hide and push/pop hiding. This generator uses only explicit named Hide statements. The [FORM benchmark discussion #702](https://github.com/form-dev/form/discussions/702) informed the repeated, sequential measurement method.

#### Incremental execution results

Same i5-1235U laptop and FORM/TFORM 4.3; programs use identical inputs and mappings, with isolated output files. CPU frequency/affinity were not controlled. Initial load averages were 2.42/4.64/3.38. This session's absolute timings differ considerably from the previous session, so comparisons below use only the new paired baselines.

The complete 178,961-leaf bubble produced **byte-identical 14,172,390-byte outputs with 145,098 terms** in both implementations. Two completed comparisons at four TFORM workers were deliberately bounded; they are too few to establish a precise expected speedup:

| Full-bubble comparison | Connected-stage baseline | Prepared factors | Incremental speedup | Wall-time reduction |
| --- | ---: | ---: | ---: | ---: |
| Diagnostic pilot, candidate first | 157.14 s | 23.09 s | 6.81x | 85.31% |
| Actual generated files, baseline first | 149.58 s | 32.04 s | 4.67x | 78.58% |

The diagnostic comparison enabled FORM statistics; the second disabled them. An initial baseline attempt hit a 60-second limit; completed baseline runs were capped at 180 seconds. No additional full-bubble trials were run. Candidate times varied substantially, so both observations are reported explicitly. The output SHA256 in all four completed runs was `6af9ed3402e12f0dfcb0647cd726bda43b226ee489a748607ad6d69a0e46e818`.

The diagnostic explains the mechanism: each large vertex normalised to 1,173 terms. The expensive module generated 387,711,120 terms in the baseline and 93,193,677 with prepared factors; both produced the same 342,482 intermediate terms and ultimately the same result. These counters explain the observed reduction in repeated work; they are not a prediction for other inputs.

The existing partial products were tested with **five alternating pairs per engine** using actual generated files and statistics disabled. Every run checked the complete result hash. Medians:

| Input / engine | Connected-stage baseline | Prepared factors | Incremental speedup | Wall-time reduction |
| --- | ---: | ---: | ---: | ---: |
| Vertex + propagator / FORM | 0.0647 s | 0.0461 s | 1.40x | 28.66% |
| Same / TFORM 2 workers | 0.0561 s | 0.0497 s | 1.13x | 11.40% |
| Same / TFORM 4 workers | 0.0561 s | 0.0481 s | 1.17x | 14.30% |
| Vertex + two propagators / FORM | 1.4868 s | 0.9099 s | 1.63x | 38.80% |
| Same / TFORM 2 workers | 0.9822 s | 0.6953 s | 1.41x | 29.20% |
| Same / TFORM 4 workers | 0.8357 s | 0.6313 s | 1.32x | 24.45% |

These percentages are incremental to connected ordering. They should not be added to the earlier percentages or multiplied into a cumulative claim without a matching controlled comparison. Import and complete-call time were not benchmarked.


Export was measured separately with four alternating public-export pairs after one excluded warmup. Each measurement includes rendering, file writes and overwrite handling. The baseline and candidate package versions were loaded outside the timed call, using the same saved input expressions.

| Export input | Baseline median | Candidate median | Observed change |
| --- | ---: | ---: | ---: |
| Vertex + propagator | 0.3376 s | 0.3593 s | +21.7 ms |
| Vertex + two propagators | 0.3634 s | 0.3419 s | -21.4 ms |
| Full bubble | 0.7551 s | 0.6577 s | -97.4 ms |

These observations do not establish a general export improvement; the retained change targets FORM execution. Mapping files were byte-identical in every export comparison. Detailed observations and scripts are in the round-two experiment directory.

Focused verification passed **405 fresh assertions**: 181 core/parser/transaction/installer, 220 FORM export/stage/import, and 4 integration assertions. Loading/namespace isolation and full-bubble metadata checks also passed. New cases exercise prepared internal dummy contractions, free indices, momentum routing, polarisation, scalar master functions and denominators, plus large/tiny threshold paths and invalid-signature bypass. The unchanged runtime's 65 passing assertions from the first round were reused, not rerun or counted as fresh checks. The benchmark driver and fixed saved-format fixtures were unchanged. No second optimisation hypothesis was pursued after normalisation met the target.


### Reusing repeated factors in large Wolfram imports

This bounded round uses **clean revision `a5a398f`** as its baseline, including connected stage ordering, prepared FORM factors, export fragment caches and native-call import tokens. It changes Mathematica reconstruction only. The export implementation, generated program/mapping bytes, FORM stage plan and version-one format remain unchanged. The target was at least **1.25x speedup**, equivalent to at least **20% less elapsed time**; this distinction avoids conflating speedup with time reduction.

The initial profile found warm public exports of 0.3966 s (vertex), 0.3511 s (two propagators) and 0.6839 s (bubble), while full-bubble import took 44.80 s. A separate instrumented import took 67.31 s: about 1.91 s in lexical setup and 64.66 s in reconstruction. These unpaired observations identify the dominant stage, not a speedup. The contracted result has no parentheses/native calls, about 965,000 powers and 356,000 vector dots, with extensive factor repetition.

`parseResult` now selects a conservative flat-result path only above 131,072 characters and with more than roughly four occurrences per distinct factor. The accepted grammar permits integers, scalar identifiers, `i_` and simple vector dots with optional signed integer powers, plus unpowered complete `cfvN(cfiM)` and `d_(cfiM,cfiN)` factors, joined by arithmetic operators. Tensor powers and nested calls retain the general-parser fallback. Tensor reconstruction was already memoized in the general parser; this extension avoids repeated recursive arithmetic parsing and dispatch. `prepareFlatResult` scans individual factors/operators with `StringCases`, checks complete original-text coverage, and checks that factors alternate with operators after an optional initial sign. It also applies the repetition cutoff, reusing the distinct factors collected during validation. Successful eligibility returns a private `flatTokenData` packet containing the tokens and first-factor position; `parseFlatResult` consumes that packet without tokenizing again. Other syntax uses `parseGeneralResult`.

The former whole-string regex could emit `RegularExpression::maxrec` and return `False` on a valid 19,588,520-character result, unnecessarily selecting the general parser. Individual factor matching avoids expression-wide regex repetition. Eligibility remains an optional optimisation: local messages and unexpected or unevaluated return values fall back to the general parser; `Abort` is not caught. Neither identifier lookup nor arithmetic occurs during eligibility. The lexical-coverage check preserves the general parser's `WhitespaceCharacter` behaviour for Unicode input.

The token list can be substantial (about 206 MiB for the 19.6 MB retained result during review). Share it with reconstruction and release rejected candidates before fallback; do not add a per-character Wolfram list or a second tokenisation. `Tests/Parser.wls` includes a genuinely long factor stream, malformed endings, unexpected eligibility results and cancellation, in addition to flat/general differential tests.

The flat parser lazily reconstructs each distinct factor through `parseGeneralResult`; it never evaluates imported source text. Products, division and sums retain their original evaluation and validation order, including initial unary minus and subtraction of a whole product. Unknown identifiers and undefined arithmetic still fail before later factors are consumed. Every mapping entry is decoded/validated before either result path. Factor caches are local to one invocation. Mapped symbols with `UpValues` bypass factor memoisation so their arithmetic is evaluated per occurrence. Thresholds are conservative heuristics, not an optimal crossover model; unique-heavy inputs pay the eligibility/tokenisation checks and then fall back.

#### Token-eligibility follow-up measurement

On Wolfram 15.0.1, a retained 19,588,520-character result was imported once with the revised automatic dispatcher and once with `parseResult` forced to use the general parser (the path selected by the former guard on this result). Both calls used the public `CalcFormImport`, including file/mapping validation. The revised call took **19.702 s**, emitted no messages, and returned exactly the same **198,423-term** expression as the general-parser call, which took **63.958 s** (`SameQ`). FORM was not rerun. This is one sequential comparison, revised first, with no alternating repetitions or peak-memory measurement; it is evidence for this particular result, not a general performance guarantee.

<a id="public-command-measurements-original-repeated-factor-optimization"></a>

#### Public-command measurements (original repeated-factor optimisation)

Wolfram 15.0.1 on the same laptop; no simultaneous heavy benchmarks, affinity/governor changes, FORM execution or installation. Baseline and candidate were alternated in one kernel after one excluded warmup each. Every public `CalcFormImport` includes file reading, JSON/digest checks, all mapping validation and reconstruction. The original 14,172,390-byte result/mapping were read without modification, and every full result was `SameQ` (145,098 terms).

| Pair | Current baseline | Repeated factors | Speedup |
| --- | ---: | ---: | ---: |
| 1 | 63.5085 s | 19.5397 s | 3.25x |
| 2, candidate first | 50.4844 s | 19.2207 s | 2.63x |
| 3 | 54.4150 s | 20.2090 s | 2.69x |

The ratio of timing medians is **2.78x** (54.4150/19.5397), a **64.09% wall-time reduction**. Median paired speedup is **2.69x**; every pair exceeds the target. Warmups were 62.4370 and 20.2613 s. A real 505,767-character prefix pilot also returned exact results in four pairs, but those pilot timings preceded the final Unicode/UpValues guards and are not the final speedup claim.

Three-pair public-import controls, each with exact equality:

| Control | Baseline median | Candidate median | Interpretation |
| --- | ---: | ---: | --- |
| 30,000 terms, repeated scalar factors | 1.8495 s | 0.8637 s | 2.14x on another eligible shape |
| 15,000 terms, mostly unique factors | 0.7780 s | 0.7513 s | Fallback; no established gain |
| 15,000 native components | 0.2709 s | 0.2819 s | General parser; small noisy regression |
| Small scalar/native/master inputs | 0.0043–0.0046 s | 0.0041–0.0044 s | General parser; too short for precise claims |

The paired control medians and ratios of separate medians can disagree on this noisy host. These controls bound obvious regressions; they do not establish universal improvement. No new export optimisation was pursued after import proved dominant.

#### Memory and cold-call caveats

One fresh process per variant, `$HistoryLength = 0`, one full import and no retained reference expression gave peak tracked kernel memory of **988,828,528 bytes baseline versus 869,531,672 bytes candidate**, about 12.1% lower. Both expression hashes matched. Supplemental first-call times were 45.10 versus 16.36 s; the paired campaign above remains the timing evidence. Startup memory was approximately 154 MB in both processes.

`MaxMemoryUsed[]` is a session high-water mark, not OS RSS. In the repeated timing kernel, memory rose across calls in both implementations even after clearing the returned expression, so those cumulative peaks are not per-import memory estimates. In the separate fresh processes, memory after clearing the result was approximately 745 MB versus 292 MB. This round did not isolate all retained kernel state or change memory management. `ByteCount` reported the same 445,618,456 bytes for each expression but can overcount shared subexpressions; it is not a resident-memory measurement. Cache storage grows with distinct factors and their reconstructed values.

The available Wolfram integration supplied the official [memoisation workflow](https://reference.wolfram.com/language/workflow/WriteAFunctionThatRemembersComputedValues) and [memory-management documentation](https://reference.wolfram.com/language/tutorial/GlobalAspectsOfWolframSystemSessions). A separate stateless evaluator check confirmed that `Throw` exits before a memoising assignment stores a failed result. Remote results informed evaluation semantics only; all representative timings used the local kernel and FeynCalc.

#### Rejected candidates and verification

The shortlist stopped after this verified improvement. Caching all successful scalar checks and allocating power-parser mutable state only on operator branches were rejected after four alternating public-import pilot triplets: median baseline 1.7159 s, scalar cache 1.6006 s, branch-local power 1.5591 s. Neither met the target reliably; neither was retained.

The final scratch candidate passed 44 differential valid/malformed/Unicode cases with identical full `Failure` objects, plus a stateful `UpValues` case that retained all 6,000 evaluations. Repository regressions cover large repeated/unique results, nested fallback, failure order, whitespace and per-occurrence arithmetic. Core verification passed **201 assertions** (77 core, 71 parser, 14 transaction, 39 installer). The FORM suite passed **220 assertions** (29 export, 128 stage, 63 import). Separately, the runtime equality change passed **73 assertions**; together the reported checks total **494**, with zero failures. Integration-library tests were not rerun for this importer-only change.

Reproduction artifacts are retained in `/tmp/cfc-wl-perf-20260929`: baseline snapshot, candidate definitions, `full-import-bench.wls`, `controls.wls`, `fresh-memory.wls`, JSON observations and logs. These are temporary development artifacts, not committed benchmark fixtures. The final importer differs from the timed candidate only in integration under the dispatch helper and explanatory comments. The separately added binary-equality runtime handling is not part of these algebraic import timings.

## Bare tensor factors in the flat parser (3 October 2026)

The narrow grammar extension and its 39 new parser assertions passed 515 core/FORM assertions. A fresh-kernel comparison measured 57.0% less import time on the retained 10.03 MB tensor result, with exact result equality and no material scalar-control regression. See the [report and method](../../CalcFormConverter/Tests/Reports/2026-10-03-tensor-fast-parser.md) and [raw measurements](../../CalcFormConverter/Tests/Reports/2026-10-03-tensor-fast-parser.json). Three measured imports ran within each fresh kernel; this is not a portable performance guarantee.



## Archived user-guide performance summary

The following text preserves the earlier user-guide measurements alongside the developer history above.

### Previous performance guidance

Measure the stage you want to improve. `ShowTiming` covers the external calculation process; `AbsoluteTiming` around the full call covers checking, export and import as well. A retained result can be re-imported without repeating FORM, which helps isolate reconstruction time:

```mathematica
(* Substitute the actual retained directory reported by the calculation. *)
retainedDirectory = "/absolute/path/to/retained/job";
AbsoluteTiming[
  result = CalcFormImport[
    FileNameJoin[{retainedDirectory, "job.out"}],
    FileNameJoin[{retainedDirectory, "job.map.json"}]];
]
```

Keep expressions factored when practical and let the generated FORM modules perform expansion. TFORM worker counts affect FORM execution, not Wolfram export/import. More workers need not help short jobs or unsuitable expression shapes. Reuse a result and its unchanged mapping for import comparisons, restart after code updates, and compare repeated runs on representative inputs.

The importer caches decoded identifiers and token classifications, collects sums/products before constructing their expressions, and avoids per-token temporary variables. Lookahead uses a sentinel with separate bounds checks. Recursive parser branches allocate mutable locals only when needed; export registration similarly allocates insertion locals only for new entries. These choices reduce repeated work while retaining type checks and job-local state. They do not imply that `Function`, `With`, or `Set` is universally faster than `Module` or `SetDelayed`.

Complete native vector-component and metric calls are recognised as single import tokens and decoded lazily through the normal identifier and argument checks. Repeated calls reuse their validated values within that import. Other syntax, including nested arguments and dot chains, keeps the ordinary parser; successful dot reconstruction is cached locally. This preserves error order and avoids repeatedly parsing the same short tensor calls. The measured median paired improvement was about 29.5% across ten full-import comparisons on one retained result; scalar and master-function inputs showed little benefit. Cache memory grows with distinct calls and vector pairs, and peak memory has not been measured.

Large flat results can additionally reuse complete factors within one import. This path accepts scalar identifiers, integers and vector dots with optional integer powers, plus bare vector-component and metric calls. Tensor powers and nested calls use the general parser. Each distinct factor is still reconstructed by the general parser in consumption order. Tensor reconstruction was already cached there; the additional flat path avoids repeated arithmetic parsing and dispatch. Small inputs, insufficient repetition, nested syntax and mapped symbols with `UpValues` use the general path. Eligibility tokenises complete factors once, checks coverage and factor/operator order, then passes the same tokens to reconstruction. This avoids the whole-expression regular-expression recursion limit on large outputs. Unexpected eligibility results or messages use the general parser; cancellation still propagates. The 131,072-character threshold and roughly fourfold repetition cutoff are heuristics. They do not change the saved format or generated FORM program.

The 3 October 2026 bare-tensor extension reduced median import time on a retained 10.03 MB partial-bubble result from **30.24 s to 12.99 s (57.0% less time)**. Exact comparisons passed, and the scalar control showed no regression. The [measurement report](../../CalcFormConverter/Tests/Reports/2026-10-03-tensor-fast-parser.md) records raw timings, process memory, validation costs and limitations; this is a separate comparison from the earlier results below.

Against revision `a5a398f`, three alternating full-import pairs on a retained 14.17 MB, 145,098-term result had medians **54.42 s versus 19.54 s**: **2.78x speedup, or 64.1% less wall time**, with exact equality on every run. This measures import, not FORM or a complete calculation. A separate fresh-kernel comparison measured approximately 989 MB versus 870 MB peak tracked kernel memory; these are not OS resident-memory figures. Repeated-session memory retention and mostly unique inputs still need consideration. Methods, controls and limitations are in the [developer guide](../../CalcFormConverter/DEVELOPER.md#reusing-repeated-factors-in-large-wolfram-imports).

Repeated tensor, denominator and scalar-abbreviation conversions are cached within each export call. The first occurrence still performs validation and symbol registration; later occurrences reuse its serialised fragment. No cache is shared between jobs, and general expression emission is not cached because sums allocate ordered macros. This benefits expressions with repeated structures; mostly unique inputs can incur extra lookup and memory costs. Cache storage grows with the number and size of distinct cached expressions.

Bounded development comparisons found further full-import reductions of about 34% on one retained result after the token-reader improvement, and about 8% for in-memory export-data construction on one large expression after the registry adjustment. These measure different stages and baselines; they must not be added together or treated as guaranteed end-to-end speedups. Details and historical measurements are in the [developer guide](../../CalcFormConverter/DEVELOPER.md#performance-measurement).
