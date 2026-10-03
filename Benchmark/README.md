# CalcFormConverter benchmarks

Open a notebook in Mathematica, evaluate its setup, choose `"Quick"` or `"Full"` in the configuration cell, then evaluate the remaining cells. The initial profile is `None`: evaluating an unchanged notebook does not start a benchmark. Opening a notebook never evaluates its input cells.

The suite uses Mathematica, FeynGrav and its normal FeynCalc dependency. Execution requires FORM, and multiple workers require TFORM. No Python, shell scripts, Git, downloaded data or manual fixture preparation are required. Keep the notebooks inside the downloaded FeynGrav tree so their relative support paths resolve.

| Notebook | Measures |
| --- | --- |
| [01 — Quick check](01_Overview_and_Quick_Check.nb) | Known contractions, package setup, availability checks and complete public calls |
| [02 — Export](02_Export.nb) | Public export with repeated/distinct objects, routed momenta, and factored/expanded inputs |
| [03 — Import](03_Import.nb) | Public import of locally generated, unchanged result/mapping pairs |
| [04 — Thread scaling](04_FORM_Thread_Scaling.nb) | Serial FORM and TFORM execution with alternating worker configurations |
| [05 — Representative calculations](05_Representative_Calculations.nb) | Construction, export, execution, import and total time for package expressions |

## Configuration

Each notebook exposes `config`, an association from `BenchmarkConfiguration[]`:

| Key | Default behavior |
| --- | --- |
| `Profile` | `None`; explicitly choose `"Quick"` or `"Full"` |
| `FORMExecutable`, `TFORMExecutable` | `Automatic`: kernel PATH discovery; absolute paths are accepted |
| `Workers` | Quick: `{2,4}`; Full: `{2,4,8}`; automatic lists are capped at `$ProcessorCount` |
| `Repetitions` | Quick: 3 measured small/synthetic trials; Full: 5 |
| `PhysicalRepetitions` | Quick: 1 measured substantial trial; Full: 3 |
| `ExecutionTimeout` | Quick: 60 seconds per FORM execution; Full: 600 |
| `OutputDirectory` | An existing parent directory; automatic uses `$TemporaryDirectory` |

One warmup per case/configuration is recorded separately. Full adds 5,000-entry synthetic families and the full scalar-projected bubble. Expression sizes are measured after Mathematica evaluation; repeated terms can combine before export. Changing worker counts does not change the number of Mathematica kernels: trials always run sequentially in the current kernel.

Use a fresh kernel and avoid other heavy calculations during measurement. Source hashes describe files at preparation time; restart the kernel after editing package code or switching package copies so the loaded definitions match those files. Missing executables are recorded; dependent trials are skipped. Export requires no FORM process. Nothing installs software. A time limit applies only to FORM execution, not input construction, export, import, validation or the complete notebook.

## What the timings mean

- **Construction:** one preparation observation per workload, including package vertex evaluation; library loading is separate preparation.
- **Export:** `CalcFormExport` including normalization, serialization and writes to a fresh target. No overwrite benchmark is mixed in.
- **Execute:** the existing runtime process helper, including launch, log handling and final cleanup. Import and result snapshot copying are outside this timer.
- **Import:** `CalcFormImport` including reads, mapping/digest validation and reconstruction. Fixture generation is separate preparation.
- **StageSum:** the sum of separately timed export, execution and import operations.
- **Calculate:** a separate `CalcFormCalculate` call, including its availability check and bookkeeping. It is not expected to equal StageSum exactly.

The first observed benchmark import is flagged. It is not necessarily the first import in the kernel if you previously used the converter. First observations and warmups are excluded from measured medians. Timings do not identify which internal cache or initialization step causes first-call overhead.

Execution scaling compares validated measured medians to serial FORM. Timeouts have no assigned speedup. A single measured trial is labeled by its trial count and supplies no evidence of variability. Initialization, filesystem caching, background activity and thermal conditions can change results; no machine-independent speedup is promised.

## Validation and reports

Small contractions have known reference answers checked outside the timer. Larger outputs are checked using complete expression fingerprints for repeated imports, serial/threaded agreement and staged/public-call agreement. These are consistency checks, not independent validation of the physical model. Different expression forms can be algebraically equivalent; an unmatched fingerprint or an unresolved reference simplification is labeled inconclusive and excluded from the validated timing summary.

Evaluate the preparation cell again before starting another run. Each prepared run can be executed once, so rerunning only the measurement cell cannot overwrite its previous report. Every run creates a unique directory with `report.json`, `summary.csv` and `jobs/`. JSON contains raw observations, configuration, environment, source hashes, input fingerprints, file sizes, available process diagnostics and validation status. CSV contains validated measured timing summaries. Wolfram kernel memory information is labeled separately; the suite does not measure FORM peak memory.

The notebooks show a `Dataset` and a timing chart. Inspect the raw rows as well as medians: warmups, failures, skips and timeouts remain in the report. A report is checkpointed after each recorded row so completed observations survive an interrupted later stage. RunStatus describes whether the driver finished or was aborted, not whether every trial succeeded.

Choose a persistent `OutputDirectory` before running if reports should survive system temporary-directory cleanup. `BenchmarkCleanup[report]` removes only successful working directories registered to that run in the current kernel. Reports and unsuccessful-job files remain. Cleanup is disabled by default and is never required to inspect results. To clean files after restarting the kernel, inspect and remove the reported run directory manually.

## Maintenance

`Support/Workloads.wl` defines deterministic expressions and deferred builders. `Support/Benchmark.wl` handles profiles, timing, validation, process execution, reporting and run ownership. Support definitions live in `` FeynGravBenchmark` ``; they add no CalcFormConverter public commands. The single `executeFORM` adapter uses the converter's private `formCommand` and `runtimeProcess` helpers and must be checked when those helpers change.

Developers can run `Tests/Regression.wls` with a Wolfram kernel for focused regression checks. User benchmarks do not depend on this test file or a command-line launcher.

The developer utility `Tests/BuildNotebooks.wls` regenerates the notebook cells from their shared layout without evaluating benchmark inputs.

Benchmark tables explicitly use `StandardForm` so FeynCalc’s formula formatting does not affect their interactive display. If an already-open notebook retains dynamic-table errors after updating, reopen the saved notebook. `Tests/Display.wls` checks the table wrappers and notebook input syntax without running benchmarks.

Distributed notebooks contain no saved evaluation output and start with an unset profile. Choose `Quick` or `Full` before running. The benchmarks explicitly request serial FORM for their baseline, independently of the converter’s automatic TFORM default. Before regenerating notebooks, preserve any local results you want to keep; the generator replaces the notebook files.
