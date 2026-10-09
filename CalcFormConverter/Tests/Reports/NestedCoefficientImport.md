# Nested coefficient import — 9 October 2026

This records the initial nested-parser implementation. The subsequent [fast-leaf and shared-cache extension](FastNestedImport.md) supersedes its parser dispatch; the measurements below remain historical observations.

## Scope

Grouped version-one coefficients now use syntax-aware subdivision of nested sums and bracketed products. Adjacent summands are batched towards 32,768 characters to avoid invoking a fresh parser for every tiny polynomial. Restricted parsing still validates every leaf. Powers of brackets, divisions at the product level, calls and unsupported shapes retain the existing path. Decomposition stops at 64 recursive levels. Symbols with arithmetic UpValues disable it. Rejected decompositions use the previous parser to preserve diagnostics; cancellation is not caught.

The general parser explicitly clears its local recursive definitions and token references on success, failure and abort. This is necessary when it is called repeatedly for small subgroups.

No public option, saved format, FORM procedure or interaction formula changes. Epsilon, Dirac and colour mappings keep their previous grouped reconstruction. The reader still buffers one complete denominator coefficient as text. An indivisible polynomial and the final expression must fit in memory; the subdivision target is not a hard memory bound.

## Measurement

Sequential fresh Wolfram kernels imported the same retained 8,277,174-byte, 64-group output and mapping. These are single observations, not repeated-run medians. Import timing excludes checksum and size inspection. Peak process-tree RSS covers the whole bounded kernel run, including loading and post-import inspection. Retained memory is Wolfram `MemoryInUse[]` after import and expression size inspection.

| Observation | Previous importer | Nested importer with batching and cleanup |
|---|---:|---:|
| Import seconds | 53.890301 | 56.006339 |
| Peak process-tree RSS, bytes | 1,564,831,744 | 1,213,157,376 |
| Retained kernel bytes | 641,973,928 | 291,408,104 |
| Returned expression bytes | 102,295,120 | 102,295,120 |

The returned expression SHA-256 hashes agree: `36398911113692716794460706418958620760734602569645192792205934814723259467366` (decimal Wolfram hash). Peak RSS fell by approximately 22.5% and retained kernel memory by 54.6%. Import time increased by 3.9% in this comparison; no speedup is claimed.

An initial unbatched version without explicit parser cleanup increased memory and time and was superseded. Overlapping preliminary trials were discarded before the sequential comparison.

Each measured trial was limited to 300 seconds, 6 GiB process-tree RSS and at least 2 GiB available system memory. Neither reported trial reached a limit. The complete notebook calculation and Full benchmark suite were not rerun. These results do not establish that the complete notebook answer fits in RAM.

Temporary raw measurements: `/tmp/cfc-parallel-factor/nested-baseline-sequential.json`, `nested-baseline-measurement.json`, `nested-candidate-blocks.json`, and `nested-candidate-measurement.json`.

## Verification

All selected suites passed in fresh kernels:

- Core: 111 assertions; parser: 121; export transactions: 20; mocked installer: 62.
- FORM export: 31; FORM stages: 137; import and legacy fixtures: 67.
- Runtime: 82; epsilon: 48; Dirac/colour translation: 65; Dirac algebra: 104; colour algebra: 139.
- Propagator grouping: 113, including direct old/new parser agreement, exact failure agreement, forced subdivision of small scalar-product/component fixtures, chunk boundaries and preservation of factored expressions.

No assertion failures occurred. Existing stream-cleanup, abort, cache-isolation and saved-format checks passed. `git diff --check` passed. The standalone FeynGrav integration suite and Full benchmark suite were not run for this importer-only change.
