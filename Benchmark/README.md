# FeynGrav benchmarks

Open a notebook in Mathematica, evaluate its setup, choose `"Quick"` or `"Full"` in the configuration cell, then evaluate the remaining cells. The initial profile is `None`: evaluating an unchanged notebook does not start a benchmark. Opening a notebook never evaluates its input cells.

The suite uses Mathematica, FeynGrav and its normal FeynCalc dependency. Execution requires FORM, and multiple workers require TFORM. No Python, shell scripts, Git, downloaded data or manual fixture preparation are required. Keep the notebooks inside the downloaded FeynGrav tree so their relative support paths resolve.

| Notebook | Measures |
| --- | --- |
| [01 — Quick check](01_Overview_and_Quick_Check.nb) | Known contractions, package setup, availability checks and complete public calls |
| [02 — Export](02_Export.nb) | Public export with repeated/distinct objects, routed momenta, and factored/expanded inputs |
| [03 — Import](03_Import.nb) | Public import of locally generated, unchanged result/mapping pairs |
| [04 — Thread scaling](04_FORM_Thread_Scaling.nb) | Serial FORM and TFORM execution with alternating worker configurations |
| [05 — Representative calculations](05_Representative_Calculations.nb) | Construction, export, execution, import and total time for package expressions |
| [06 — Library generation](06_Library_Generation.nb) | Public generator calls, isolated generation stages and supplementary tensors |

## Configuration

Each notebook exposes `config`, an association from `BenchmarkConfiguration[]`:

| Key | Default behaviour |
| --- | --- |
| `Profile` | `None`; explicitly choose `"Quick"` or `"Full"` |
| `FORMExecutable`, `TFORMExecutable` | `Automatic`: kernel PATH discovery; absolute paths are accepted |
| `GeneratorThreads` | Notebook 06 only: `Automatic` prefers up to eight TFORM workers with serial fallback; a positive integer selects an explicit count |
| `Workers` | Quick: `{2,4}`; Full: `{2,4,8}`; automatic lists are capped at `$ProcessorCount` |
| `Repetitions` | Quick: 3 measured small/synthetic trials; Full: 5 |
| `PhysicalRepetitions` | Quick: 1 measured substantial trial; Full: 3 |
| `ExecutionTimeout` | Quick: 60 seconds per FORM execution; Full: 600 |
| `OutputDirectory` | An existing parent directory; automatic uses `$TemporaryDirectory` |

One warmup per case/configuration is recorded separately. Full adds 5,000-entry synthetic families and the full scalar-projected bubble. Expression sizes are measured after Mathematica evaluation; repeated terms can combine before export. Trials run sequentially in the current kernel. FORM worker counts do not configure Wolfram subkernels; generator rule definitions may themselves use parallel Wolfram evaluation.

Use a fresh kernel and avoid other heavy calculations during measurement. Source hashes describe files at preparation time; restart the kernel after editing package code or switching package copies so the loaded definitions match those files. Missing executables are recorded; dependent trials are skipped. Export requires no FORM process. Nothing installs software. A time limit applies only to FORM execution, not input construction, export, import, validation or the complete notebook.

## What the timings mean

- **Construction:** one preparation observation per workload, including package vertex evaluation; library loading is separate preparation.
- **Export:** `CalcFormExport` including normalisation, serialisation and writes to a fresh target. No overwrite benchmark is mixed in.
- **Execute:** the existing runtime process helper, including launch, log handling and final cleanup. Import and result snapshot copying are outside this timer.
- **Import:** `CalcFormImport` including reads, mapping/digest validation and reconstruction. Fixture generation is separate preparation.
- **StageSum:** the sum of separately timed export, execution and import operations.
- **Calculate:** a separate `CalcFormCalculate` call, including its availability check and bookkeeping. It is not expected to equal StageSum exactly.

The first observed benchmark import is flagged. It is not necessarily the first import in the kernel if you previously used the converter. First observations and warmups are excluded from measured medians. Timings do not identify which internal cache or initialisation step causes first-call overhead.

Execution scaling compares validated measured medians to serial FORM. Timeouts have no assigned speedup. A single measured trial is labelled by its trial count and supplies no evidence of variability. Initialisation, filesystem caching, background activity and thermal conditions can change results; no machine-independent speedup is promised.

## Validation and reports

Small contractions have known reference answers checked outside the timer. Larger outputs are checked using complete expression fingerprints for repeated imports, serial/threaded agreement and staged/public-call agreement. These are consistency checks, not independent validation of the physical model. Different expression forms can be algebraically equivalent; an unmatched fingerprint or an unresolved reference simplification is labelled inconclusive and excluded from the validated timing summary.

Evaluate the preparation cell again before starting another run. Each prepared run can be executed once, so rerunning only the measurement cell cannot overwrite its previous report. Every run creates a unique directory with `report.json`, `summary.csv` and `jobs/`. JSON contains raw observations, configuration, environment, source hashes, input fingerprints, file sizes, available process diagnostics and validation status. CSV contains validated measured timing summaries. Wolfram kernel memory information is labelled separately; the suite does not measure FORM peak memory.

The notebooks show a `Dataset` and a timing chart. Inspect the raw rows as well as medians: warmups, failures, skips and timeouts remain in the report. A report is checkpointed after each recorded row so completed observations survive an interrupted later stage. RunStatus describes whether the driver finished or was aborted, not whether every trial succeeded.

Choose a persistent `OutputDirectory` before running if reports should survive system temporary-directory cleanup. `BenchmarkCleanup[report]` removes only successful working directories registered to that run in the current kernel. Reports and unsuccessful-job files remain. Cleanup is disabled by default and is never required to inspect results. To clean files after restarting the kernel, inspect and remove the reported run directory manually.

## Maintenance

`Support/Workloads.wl` defines converter workloads. `Support/Benchmark.wl` loads the main-package workflow, while `Support/GeneratorBenchmark.wl` loads the generator workflow. They share `Support/BenchmarkCore.wl` for profiles, timing, process execution, reporting and run ownership. `Support/GeneratorSupport.wl` contains generator workloads and the single adapter to generator internals. Support definitions live in `` FeynGravBenchmark` ``; they add no CalcFormConverter public commands. The single `executeFORM` adapter uses the converter's private `formCommand` and `runtimeProcess` helpers and must be checked when those helpers change.

Developers can run `Tests/Regression.wls` with a Wolfram kernel for focused regression checks. User benchmarks do not depend on this test file or a command-line launcher.

The developer utility `Tests/BuildNotebooks.wls` regenerates the notebook cells from their shared layout without evaluating benchmark inputs.

Benchmark tables explicitly use `StandardForm` so FeynCalc’s formula formatting does not affect their interactive display. If an already-open notebook retains dynamic-table errors after updating, reopen the saved notebook. `Tests/Display.wls` checks the table wrappers and notebook input syntax without running benchmarks.

Distributed notebooks contain no saved evaluation output and start with an unset profile. Choose `Quick` or `Full` before running. The benchmarks explicitly request serial FORM for their baseline, independently of the converter’s automatic TFORM default. Before regenerating notebooks, preserve any local results you want to keep; the generator replaces the notebook files.

## Library generation (notebook 06)

Start a **fresh kernel** before switching between notebook 06 and notebooks 01–05. The generator workflow loads FeynCalc, CalcFormConverter and rule packages, without loading FeynGrav's main package. A guard rejects switching workflows in an already-used kernel.

Quick runs the scalar, fermion and vector specific-generation commands at order one. Full adds their order-two counterparts, general relativity at order one, Yang–Mills at order one, quadratic gravity at orders one and two, Gauss–Bonnet at orders two and three, and Horndeski G2 `[0,2,1]`, G3 `[0,1,1]`, G4 `[0,1,1]`, G5 `[2,0,1]`. Axion, unresolved G5 and higher Gauss–Bonnet cases are deliberately excluded. This is representative coverage, not complete inventory regeneration.

The supplementary section measures `ITensor`, `CTensorGeneral` with no external pairs, and `ETensor` with one external pair. Quick uses one and two internal pairs; Full uses one through four. Helper results are checked for successful completion without algebraic comparisons.

`GeneratorThreads` chooses one execution configuration. When it and both executable paths are automatic, the converter selects TFORM or falls back to FORM. With automatic threads and an explicit TFORM path, up to eight workers are requested; with only an explicit FORM path, one worker is requested. Explicit integer counts use the corresponding executable setting and never silently fall back. Notebook 04 remains the thread-scaling suite.

For each workload the public command runs before isolated construction: a first observation, one warmup, and `PhysicalRepetitions` measured trials. Each trial has a fresh library destination. First observations are **not cold timings**: other cases may have warmed shared memoised tensors. No definitions or caches are cleared. Helper trials similarly use a first observation, warmup and `Repetitions` measurements.

Isolated stages then run once as a warmup and for each measured repetition:

| Stage | Included work |
| --- | --- |
| Construction | Release the generator's held rule builder |
| Export | Public CalcFormExport, including file writes |
| Execute | Shared direct-process adapter and FORM output/log writing |
| Import | Public CalcFormImport |
| WriteVerify | Formal-symbol mapping, compact writing, read-back and exact comparison |
| Publish | Generator's publication helper on a new destination |
| StageSum | Sum of those six separately measured stages, per library |
| Generate | A separate public command including checks, normal progress printing and publication; multi-file commands are timed together |

Public calls retain converter job files inside the owned trial directory for diagnostics (`KeepFiles -> True`). Public/staged comparisons, fingerprints and file sizes are outside timers. Staged timings describe warmed rule construction and need not sum to the public-command time. The timeout applies to FORM execution only.

Generated files are checked for the expected file set and readability. No comparison against previous expressions or byte identity is performed. First observations and warmups are excluded from mean and median timings. Failed or incomplete stages retain diagnostics and do not enter timing summaries.

Notebook 06 uses static StandardForm grids. Its reports include command arguments, filenames, byte sizes, fingerprints and generator/rule/procedure hashes. Successful files may be removed with the shared ownership-checked cleanup; reports and unsuccessful trials remain.

The legacy `Libs/Performance_Tests.nb` is replaced by notebook 06. Its hand-made historical CPU timings are recoverable from Git and are not mixed with the new wall-clock measurements.

### Developer verification and notebook generation

`Tests/Generation.wls` checks the new workflow; `Tests/Regression.wls` and `Tests/Display.wls` cover shared infrastructure and display. `Tests/BuildNotebooks.wls` accepts a notebook filename through `FEYNGRAV_BENCHMARK_NOTEBOOK` (or a script argument when supported by the launcher). Set it to `06_Library_Generation.nb` to build only the new notebook and preserve saved results in notebooks 01–05. With no selection, the utility rebuilds all notebooks.

### Generator timing presentation

Notebook 06 displays a compact environment table and mean/median times for successful measured observations. Public totals, helpers and individual stages have separate charts. First observations and warmups remain in JSON; only unsuccessful observations appear in the diagnostic table. Generated files are checked for presence and readability, without comparisons against previous expressions or byte equality. The generator retains its normal serialisation read-back check. Retained-file notices are suppressed locally; other warnings remain visible.

Notebook 06 shows a progress bar counting completed cases, with the current case and stage. This is not an estimate of remaining time. Ordinary progress output is saved in `progress.log` in the run directory; warnings remain visible. Logging remains included in public-command timing. Reports and the progress log survive working-file cleanup.
