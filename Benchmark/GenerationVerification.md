# Library-generation benchmark verification — 8 October 2026

## Delivered

Notebook 06 replaces `Libs/Performance_Tests.nb`. It uses generator-only loading, shared benchmark reporting and process execution, and a dedicated generator adapter. Existing notebooks and shipped libraries were not regenerated or edited. These checks preceded the subsequent presentation changes.

## Initial implementation checks (8 October)

- Quick profile: all three public generation workloads, their stages, and six helper cases completed. 123 observations: 120 verified and three first-observation baselines; no failed or inconclusive rows.
- Full-only cases: all 13 additional public commands (including multi-file vector and Yang–Mills generation) and 12 helper cases completed with `PhysicalRepetitions -> 1`. 417 observations: 404 verified and 13 first-observation baselines; no failed or inconclusive rows. Each public command still included its first observation and warmup; staged workflows included a warmup. Full's default repetition loops were separately tested on small scalar/helper cases.
- Focused generation regressions: 26 checks passed, including unset profiles, worker settings, public/staged agreement, stage sums, nested timeout diagnostics, aborts, missing executables, helpers without FORM, static summaries, cleanup ownership and workflow-switch rejection.
- Existing benchmark regressions: 51 checks passed, including real process timeout/abort termination through the shared adapter.
- Display regressions: 26 checks passed, including all six notebook input expressions and absence of automatic evaluation.
- Explicit FORM/TFORM paths containing spaces: a two-worker scalar public/staged run passed; automatic threads with an explicit serial path resolved to one worker.
- Mathematica front end opened notebook 06: ten editable input cells, no automatic evaluation. A PDF preview of the notebook was rendered and visually inspected.
- Checksums confirmed all five existing notebooks and all 90 shipped libraries were unchanged (95 protected files).

## Boundaries and evidence

These are functional checks, not a hardware-performance comparison; some independent verification processes overlapped. We did not run all Full cases with three measured repetitions, rerun the converter Full benchmark suite, or independently derive the interaction formulas. First-observation baselines are deliberately excluded from measured summaries.

Machine-readable counts and local report paths are in [GenerationVerification.json](GenerationVerification.json). The local `benchmark-generation-work` workspace retains scripts, logs and a copy of the user's edited legacy notebook before removal. This backup is separate from the older notebook in Git history.

## Presentation and logging update — 9 October 2026

The final focused generation suite passed 29 checks. Mean and median are calculated independently; a synthetic sample `{1, 2, 9}` gives 4 and 2 respectively. Charts use width 700. Notebook 06 now displays compact configuration and timing tables, diagnostic rows only for unsuccessful observations, and a progress bar counting completed cases. Detailed progress goes to `progress.log`; Mathematica messages remain visible. Headless runs do not attempt front-end monitoring.

At the user’s request, performance measurements no longer compare generated expressions or bytes with earlier results. The generator’s own serialisation read-back check remains. The initial comparison results above are historical evidence, not a description of the current benchmark. Saved measurements and manual chart adjustments were preserved; no Full suite was rerun for these presentation changes.
