# Fast nested import and shared parser state — 9 October 2026

Historical implementation-stage record: preserve the measurements and checks below as evidence for that revision. The current implementation and later review corrections are described in the [developer guide](../../DEVELOPER.md) and [10 October follow-up](EarlyDimensionCompaction.md#independent-review-corrections).

## Implementation

Nested commuting coefficients are decomposed to eligible polynomial leaves. Their fast-parser threshold is 32 characters inside one import-local environment, replacing the ineffective combination of 32,768-character nested batches and a 131,072-character fast-path threshold. Flat lexical validation, typed factors, lazy failure order and general-parser fallback remain in force. Unsupported nested shapes continue through the general parser.

The environment reuses the validated dictionary, dimension, successful compound-factor decoding and token classifications. Integer coefficients, `i_` and validated scalar entries are decoded directly without constructing a general parser. Integers do not occupy the factor cache. Caches are bounded and cleared automatically at import exit, including failure and abort. Per-leaf memoisation avoids repeated shared-cache lookups; temporary parser definitions and buffers are released.

No public commands, options, saved formats, FORM processing or interaction expressions change. Reassociation remains disabled for dictionary symbols with UpValues and for epsilon, Dirac and colour mappings.

## Investigation

An initial shared-cache candidate remained slower. Profiling the retained 64-group fixture found 101,266 general-parser calls, including many integer coefficients, and about 14.5 seconds inside those calls (inclusive instrumented times). Direct literal decoding and avoiding integer-cache churn addressed this cost. Profiling timings are diagnostic observations, not speedup measurements.

## Matched measurements

Each trial used a fresh Wolfram kernel and the same saved output/mapping pair. The 64-group fixture was measured three times per version with alternating version order. These are first imports in separate kernels, not warmed repeated imports. The other two comparisons are single observations. No benchmark jobs ran concurrently.

| Fixture | Trials per version | Baseline seconds | Candidate seconds | Time reduction |
|---|---:|---:|---:|---:|
| Retained 64 groups (8,277,174 bytes) | 3 | 45.978 | 25.175 | 45.2% |
| Eight largest actual groups (4,081,510 bytes) | 1 | 21.538 | 13.084 | 39.2% |
| Repetitive scalar control (232,666 bytes) | 1 | 1.386 | 0.799 | 42.3% |

For the 64-group fixture, the raw import times were:

- Baseline: 33.736088, 45.978240, 50.118290 seconds.
- Candidate: 24.481083, 25.175432, 28.060181 seconds.

A final confirmation run after adding nested-public-import isolation and preserving private threshold overrides took 23.807726 seconds, with 1,122,127,872 bytes peak RSS and 223,005,888 retained kernel bytes. Its result hash also agreed. This additional run is not included in the three-trial medians. These safeguards do not change the ordinary default parsing path.

The baseline varied substantially, but its range did not overlap the candidate range. The table uses medians for this fixture and individual times for the other two; these are workload-specific observations, not portable speedup guarantees.

For the 64-group fixture, retained kernel memory was approximately 291.4 MB before and 223.0 MB after (23.5% lower). Peak process-tree RSS ranged from 1.148–1.206 GB before and 1.076–1.088 GB after. For the largest-eight fixture, peak RSS fell from 733.2 MB to 668.8 MB; retained kernel memory fell from 230.4 MB to 194.1 MB. The scalar control showed no memory regression.

All baseline/candidate result hashes agreed within each fixture. The 64-group result still occupies 102,295,120 bytes as a Mathematica expression. No mathematical compression of the final expression is claimed.

Import timings exclude size and hash inspection. Peak RSS covers the entire kernel process, including package loading and post-import inspection; retained memory is Wolfram `MemoryInUse[]` after import and size inspection. Every trial had a 300-second limit, a 6 GiB process-tree RSS limit and a minimum of 2 GiB available system memory. No reported trial reached a limit.

The largest groups were copied from the user's retained 171,993,554-byte result without changing the original. The complete result was not imported, FORM was not rerun for these measurements, and the Full benchmark suite was not run. Completion of the complete notebook calculation remains unverified.

Raw temporary evidence is in `/tmp/cfc-parallel-factor/final-*-measurement.json` and the matching `final-*.json` resource records. The frozen baseline is `/tmp/cfc-shared-baseline`; trial scripts are `shared-final-bench.wls` and `shared-final-series.py` in the same temporary evidence directory.


## Verification

The final code passed the selected suites in fresh kernels:

- Core: 111 assertions; parser: 121; shared parser: 30; export transactions: 20; mocked installer: 62.
- FORM export: 31; FORM stages: 137; import and legacy fixtures: 67.
- Runtime: 82; epsilon: 48; Dirac/colour translation: 65; Dirac algebra: 104; colour algebra: 139; propagator grouping: 113.

The new checks cover small-leaf fast dispatch, exact agreement and failure ordering against the general parser, bounded caches, exclusion of integer literals from the cache, failure/abort cleanup, changed scalar-product definitions, different dictionaries, nested public-import isolation and the UpValues guard. Existing forced-fast-path tests pass with their private threshold overrides preserved.

All 1,130 assertions passed. Relative documentation links and `git diff --check` passed. The final source hashes agree with those recorded by the final confirmation measurement. No stored libraries, notebook results or original FORM output files were changed; no commit was made. The standalone FeynGrav integration suite and Full benchmarks were not run.
