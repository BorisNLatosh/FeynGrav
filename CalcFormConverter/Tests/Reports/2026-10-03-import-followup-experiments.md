# Import follow-up experiments — 3 October 2026

## Outcome

**No additional optimization was retained.** The requested additional 70% reduction from the current 12.994945 s reference baseline (target about 3.8985 s) was not achieved. This does not establish that the target is impossible. The existing tensor-factor optimization and its previously measured 57.02% reduction remain unchanged.

The search stopped after two consecutive candidates failed to improve the bounded small-fixture pilot, as agreed. Neither candidate was promoted to the 10 MB performance comparison. No production code, parser tests or existing documentation were changed in this round; only these experiment reports were added.

## Coarse profile

A temporary instrumented copy added timestamps and kernel-memory readings at six stage boundaries, with no per-token timers. The retained 250,114-byte fixture gave medians 0.291083 s without instrumentation and 0.301281 s with it, an observed 3.50% difference. This sequential comparison includes order/host variability and is not a precise isolation of instrumentation overhead.

The instrumented 10,033,002-byte fixture then ran one separately recorded first import in a fresh kernel, one warmup and one measured import. The measured public import was **12.253615 s**, partitioned approximately as follows:

| Stage | Seconds | Fraction of import |
|---|---:|---:|
| Read, validate and prepare before eligibility | 0.2982 | 2.4% |
| Flat lexical eligibility | 1.1395 | 9.3% |
| Reconstruction loop, including individual product construction | 9.9133 | 80.9% |
| Final Plus construction | 0.0883 | 0.7% |
| Final typed-token check | 0.5998 | 4.9% |
| Return and cleanup after final check | 0.2145 | 1.8% |

The reconstruction loop dominates this observation. The 12.253615 s profile cannot be treated as a speedup against the earlier 12.994945 s reference: those are different sessions and measurement conditions. Stage timings include the small instrumentation cost and do not sum exactly because boundary-call overhead is between some stages.

## Candidate decisions

| Trial | Baseline median (range), s | Candidate median (range), s | Decision |
|---|---:|---:|---|
| 1: native additive boundaries and Table collection | 0.291083 (0.289842–0.292064) | 0.411561 (0.274602–0.420473) | Initial variable/regressive result; one confirmation allowed |
| 1 confirmation, candidate then baseline | 0.328723 (0.327766–0.394947) | 0.413389 (0.407736–0.593012) | Reject |
| 2: local Association factor cache | 0.328723 (0.327766–0.394947), latest control above | 0.485359 (0.473779–0.486730) | Reject; stop search |

Candidate 1 replaced nested cursor loops and Reap/Sow collection with precomputed additive boundaries and Table-based collection. Lazy factor reconstruction, initial unary-minus behavior, subsequent whole-product subtraction, division checks and final checks were retained. Exact `SameQ` on the small fixture passed; the candidate passed all 121 Parser assertions. The single reverse-order confirmation remained slower, so no larger performance trial followed.

Candidate 2 kept the current token reader and arithmetic loops, replacing the flat factor cache's local memoized DownValues with a local Association using lazy `KeyExistsQ` lookup and insertion only after successful decoding. It was clearly slower than the latest small control. It was rejected before differential correctness validation or regression tests; its correctness is therefore not certified. There was no fresh baseline pair specifically for candidate 2, and no claim is made about its performance on larger inputs.

## Memory observations and measurement boundaries

Public `CalcFormImport` alone was timed. Output history was disabled. Unlike the earlier comparison harness, no fingerprints, leaf counts or serialization ran between imports; only the final result was serialized after its import timer stopped. Results were cleared between calls. The final released-memory sample includes that untimed serialization, so it must not be treated as a pure import-retention measurement.

In the large profile, released kernel memory after the first import and warmup was approximately 156.9 and 164.8 MiB, before the final serialization. Growth therefore remains worth investigating independently; these samples do not identify its allocation source or establish a parser leak. On the small confirmation, baseline and Association candidate released-memory trajectories were essentially the same (approximately 151.5, 154.2, 157.3 and 160.2 MiB before final serialization). The Association experiment did not demonstrate a retention benefit.

Kernel data counters and sampled OS RSS are different measurements. Raw per-phase RSS and per-call kernel counters are retained in JSON. These experiments do not attribute the earlier Full-suite OOM. No full-bubble or FORM calculations were run.

## Reproducibility and limits

- Current baseline snapshot and all experimental copies, scripts, logs and WXF outputs: `/tmp/cfc-import-next-20261003-143734`. The temporary directory may eventually be cleaned; the accompanying JSON preserves raw counters, exact execution order, source hashes, experimental source diffs and correctness scope.
- Fixture bytes/mapping are unchanged from `/tmp/cfc-tensor-fast-20261003-140233/fixtures`; fixture hashes are preserved in JSON.
- Each small timing process used one fresh kernel, one separately recorded first import, one warmup and three measured imports. The large profile used one measured import. These are within-kernel samples, not three independent paired kernel replications.
- Exact process order: small baseline, small profile, large profile, small candidate 1, separate candidate-1 equality check and Parser run, small candidate-1 confirmation, small baseline confirmation, small candidate 2. Kernels ran sequentially.
- Timing processes used the previous bounded monitor: 300 seconds per phase, 6 GiB RSS, at least 2 GiB available system memory, sampled every 50 ms. No resource stop occurred; all timing processes exited zero.
- Harness invocation: `python3 /tmp/cfc-import-next-20261003-143734/run_one.py VERSION CASE [MEASURED_COUNT]`. Versions/cases and every generated kernel command are recorded in JSON. Separate candidate-1 equality used `validate.py candidate1 small`; Parser stdout is preserved in the JSON as well as the local log. The existing monitor dependency is recorded by path and hash.

No commit was made. New optimization work requires another decision; the existing implementation remains the accepted baseline.
