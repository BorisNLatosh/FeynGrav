# Native parallel coefficient factorisation — 9 October 2026

Historical implementation: full polynomial factorisation was subsequently replaced by [common-factor extraction](CommonFactorExtraction.md). Recorded measurements below remain unchanged.

## Implementation

TFORM can factorise separate coefficient expressions concurrently with `InParallel`. This corrects the earlier overly broad statement that native factorisation always executes on the master. The old output loop exposed only one expression at a time. Whole-expression scheduling permits independent factorisations on worker threads; it does not parallelise the internals of a single polynomial factorisation.

The exported FORM program now prepares a bounded-size queue of up to four coefficients per worker. Each worker processes one coefficient at a time. Serial FORM uses batches of one. The worker count is determined from the running executable's `NTHREADS_` macro, which includes the master in FORM 5.0.2; the template falls back to one if the macro is absent. Consequently the same export works with `form`, `tform -w8` or another selected count, without a separate process pool, shell script or dependency.

Complete propagator products are enumerated once, and the full result remains in shared hidden storage. Only the current batch is active. Free-index flags and the 20,000-term eligibility guard apply separately to each coefficient. Ineligible coefficients retain their unfactorised output. Factorisation itself writes no shared dollar variables. Results are written after each batch in the original key order, independently of worker completion order. Temporary expressions and their flags are cleared before reuse.

The existing general restrictions, epsilon/Dirac/colour bypass, importer, mappings, timeout, cancellation and file-retention behaviour are unchanged. `FORMThreads -> Automatic` still follows the existing up-to-eight-workers selection; this change does not alter public defaults. An explicit count selects more workers when wanted. The machine used for these checks reports 12 logical CPUs.

The queue size bounds the number of coefficient expressions, not bytes of RAM. More simultaneous factorisations can require more memory. Workers may become idle near a batch boundary while a particularly expensive coefficient finishes. Output may also pause until that batch is complete. There is no promise of continuous full CPU utilisation or linear scaling.

## Native feasibility probe

Four copies of the same nontrivial retained coefficient were factorised in one TFORM job with four workers. Without whole-expression scheduling the job took 10.97 seconds. With `InParallel` it took 3.62 seconds; the FORM log recorded 13.13 worker CPU seconds over 3.58 internal wall seconds. This establishes actual concurrent factorisation, not merely faster preparation. It is a feasibility control, not a representative full-workload benchmark.

## Retained 64-group comparison

All tests used the same previously verified 64-group subset and FORM/TFORM 5.0.2. Trials ran sequentially with 300-second, 6 GiB process-tree RSS and 2 GiB available-system-memory stop conditions. No limit was reached. These are single observations, not medians or portable performance guarantees.

An initial eight-worker queue with one coefficient per worker took 70.96 seconds and peaked at 725,155,840 bytes RSS. Increasing the queue to four coefficients per worker reduced idle time on this sample and was selected for delivery. The final control uses the identical queue/program with `NotInParallel` in place of `InParallel`, retaining eight workers for other stages.

| Final configuration | Wall time | Peak process RSS |
| --- | ---: | ---: |
| Sequential factorisation control | 108.85 s | 407,068,672 bytes |
| Native parallel factorisation, eight workers | 48.55 s | 772,370,432 bytes |

The observed speedup is **2.24×**, a **55.4%** time reduction. Memory increased from about 0.38 to 0.72 GiB. This trade-off is part of the parallelisation; no claim of reduced CPU work or lower memory is made.

FORM subtraction returned exactly zero for both parallel layouts against the previously verified expressions, including the selected layout against the original unfactorised subset. All 64 complete propagator keys remained in the same output order. Factor ordering/text can differ between execution modes; byte equality is not used as an algebraic test. Import timing was not repeated because the importer was unchanged; current correctness tests exercise complete export–execute–import calls.

The complete 1,022-group calculation was not rerun. Earlier full-result memory limitations remain unresolved; these observations apply only to the retained subset. No Full benchmark was run.

## Verification evidence

The focused suite passed 76 assertions, including serial execution, 2/4/12-worker calculations, full and partial batches, independent eligibility within a mixed batch, exact algebra, factor retention, free metric/component and term-count fallbacks, malformed output, stream cleanup and existing tensor/epsilon/Dirac/colour cases.

The wider regression suites also passed: Core (111), Parser (121), Transactions (20), Installer (62), Export (31), FormStages (128), Import (65), Runtime (82), Epsilon (48), DiracColour (65), DiracAlgebra (104) and ColourAlgebra (139). These were targeted converter regressions; the Full benchmark suite was not run.

Local evidence is retained under `/tmp/cfc-parallel-factor`: native probes, controlled programs, timing/memory JSON files, equality residuals, key-order checks and suite logs. These files are not runtime dependencies. The [preceding sequential implementation report](CoefficientFactorisationImplementation.md) remains a historical measurement record.

The [official FORM manual, section 7.78](https://form-dev.github.io/form-docs/stable/manual/) describes whole-expression scheduling. The pinned [FORM 5.0.2 startup source](https://github.com/form-dev/form/blob/v5.0.2/sources/startup.c) defines `NTHREADS_`; its value was checked with the installed serial and threaded executables.

Existing user edits, notebooks, stored libraries and calculation files were preserved. No commit was made. Previously exported programs must be exported again to include the new template.
