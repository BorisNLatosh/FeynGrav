# CalcFormConverter

A standalone Wolfram Language package for translating exact bosonic FeynCalc expressions to FORM and reconstructing FORM results. It loads automatically with FeynGrav. It does not load the library generator or FeynCalcLegacy.

## Manual workflow

Load FeynGrav normally, or load this module independently:

```mathematica
Get[FileNameJoin[{$UserBaseDirectory, "Applications", "FeynGrav",
  "CalcFormConverter", "CalcFormConverter.wl"}]];
```

Usage messages are available in Mathematica:

```mathematica
?CalcFormExport
?CalcFormImport
?CalcFormCheck
?CalcFormInstall
?CalcFormCalculate
?FORMThreads
```

After updating an already loaded package, restart the kernel and load FeynGrav again before comparing behavior or performance.

Export to an existing directory:

```mathematica
expression = MTD[mu, nu] FVD[l, mu] FVD[p - l, nu] FAD[{l, m}, {p - l, m}];
job = CalcFormExport[expression, "/tmp/bubble.frm", LoopMomenta -> {l}];
```

The returned association contains `InputFile`, `MappingFile`, and `ResultFile`. The exporter writes `bubble.frm` and `bubble.map.json`; FORM creates `bubble.out` when executed. Run FORM separately in a terminal:

```sh
form /tmp/bubble.frm
```

For manual execution with four TFORM workers, use `tform -w4 /tmp/bubble.frm` instead. Both executables consume the same generated program and produce the same result format.

Then in Mathematica:

```mathematica
result = CalcFormImport[job["ResultFile"], job["MappingFile"]];
```

Keep the mapping alongside the result. The result header must match the mapping's digest. A mapping also fingerprints the original expression, so a result from a different expression is rejected even when its symbol vocabulary is identical. This is a consistency check, not a cryptographic authentication mechanism.

Loading the package does not change the working directory, write files, or locate/launch FORM. `CalcFormExport` and `CalcFormImport` do not launch external processes. The runtime commands below launch them only when explicitly called.

## Automated workflow

```mathematica
status = CalcFormCheck[];
(* If FORM is missing, installation is an explicit separate request: *)
(* CalcFormInstall[] *)

result = CalcFormCalculate[expression, LoopMomenta -> {l}];
```

`CalcFormCheck[FORMExecutable -> Automatic, FORMThreads -> 1, TimeConstraint -> 10]` searches the kernel's `PATH` and runs a small arithmetic probe. An explicit executable path takes precedence. Its association reports `Available`, `Status`, `Executable`, `Version`, `RequestedEngine`, `FORMThreads` and installation guidance, plus process diagnostics when a probe ran. `NotFound` means no executable was found; `LaunchFailed`, `ProbeFailed`, `TimedOut` and `Aborted` distinguish other failures. An available executable with an unrecognized banner has `Version -> Missing["NotReported"]`.

```mathematica
CalcFormCheck[FORMExecutable -> "/usr/bin/form"]
result = CalcFormCalculate[expression,
  FORMExecutable -> "/usr/bin/form",
  LoopMomenta -> {l},
  TimeConstraint -> 600,
  WorkingDirectory -> "/tmp",
  KeepFiles -> True
];
```

Calculation options are `Dimension -> Automatic`, `LoopMomenta -> {}`, `FORMExecutable -> Automatic`, `TimeConstraint -> Infinity`, `WorkingDirectory -> Automatic`, `KeepFiles -> False`, `ShowTiming -> False`, `ShowProgress -> False`, and `FORMThreads -> 1`. The calculation timeout applies to FORM execution; the preceding availability probe has its own ten-second limit. `WorkingDirectory` names an existing parent directory; each call creates its own unique child. `Automatic` uses the system temporary directory. The Mathematica working directory is unchanged.

Import reconstructs the expression in the Wolfram kernel, so its time is separate from FORM execution. Assign large results with a trailing semicolon, as in these examples, to avoid the additional cost of formatting and displaying the full expression in a notebook.

The return value is a FeynCalc expression, or a `Failure` describing the stage and cause. Calculation failures after job creation include `JobDirectory`; process details include the exit code and log paths. Execution writes `stdout.log` and `stderr.log` while retaining only bounded diagnostic tails in memory. Timeout or user abort stops the calculation process and retains the job. One cleanup boundary covers log acquisition, execution and final inspection, including nonlocal exits. Probe directories have the same acquisition-to-cleanup protection. An abort during execution returns a failure rather than a partial expression.

Successful jobs are deleted unless `KeepFiles -> True`. Retained successful jobs report their path with `CalcFormCalculate::files`. Failed jobs are always retained. `job.frm`, `job.map.json` and `job.out` can then be used with the manual workflow. Never infer a successful calculation solely from a result file: check the returned value.

This workflow performs the template's existing algebra and Lorentz contractions. It does not perform loop integration or integral reduction.

### Timing, progress and parallel execution

```mathematica
result = CalcFormCalculate[expression,
  LoopMomenta -> {l},
  FORMThreads -> 4,
  ShowTiming -> True,
  ShowProgress -> True
];
```

`ShowTiming` prints elapsed wall-clock seconds for the calculation process, including its final output drain. It excludes the availability probe, export and import. This is elapsed time, not the sum of CPU time across workers. Process diagnostics on failures include `ElapsedSeconds` when execution started. The returned value remains the FeynCalc expression.

Use `AbsoluteTiming` to measure the complete call, including the availability check, export, execution and import:

```mathematica
{elapsedSeconds, ignored} = AbsoluteTiming[
  result = CalcFormCalculate[expression,
    LoopMomenta -> {l}, FORMThreads -> 4, ShowTiming -> True];
];
```

The trailing semicolon suppresses display of the large expression. Mathematica's `Timing` measures kernel CPU time and does not include the external FORM process's CPU time, so it should not be compared with `ShowTiming` as though both measured elapsed time.

`ShowProgress` prints the check, export, execution and import stages, followed by completion or failure. During FORM execution it also prints elapsed time every ten seconds. These updates show that the process is still running; they are not a percentage, an estimate of remaining work or counts of processed terms. Long export/import stages retain their stage label without periodic updates.

The default `FORMThreads -> 1` selects ordinary `form`. A positive integer above one selects `tform` from the kernel's PATH and passes `-wN`, requesting N worker threads. The availability probe uses the same worker count. An explicit `FORMExecutable` still takes precedence; with multiple workers, a successful arithmetic probe must also identify TFORM in its banner. Otherwise the check reports `ThreadingUnavailable`, avoiding an ordinary FORM executable silently ignoring the worker request. No fallback or installation occurs when TFORM is missing. `CalcFormCheck[FORMThreads -> 4]` checks this configuration separately.

TFORM must be installed separately if your FORM distribution does not include it. See the [official TFORM description](https://www.nikhef.nl/~form/maindir/publications/tform.pdf). Parallelism can reduce execution time, but scaling depends on expression structure, sorting, memory and disk activity. Increasing workers does not accelerate Mathematica export/import. Compare timings on a representative expression before selecting a worker count; no speedup for the complete scalar bubble has been measured.

### Explicit installation

`CalcFormInstall[FORMThreads -> n]` first checks the requested configuration; the default is `FORMThreads -> 1`. For `n > 1`, both this check and verification after installation use TFORM with the requested worker count. Working ordinary FORM does not prevent installation when TFORM is missing. It leaves a working installation alone and reports an existing but unusable executable without reinstalling it. Only a missing executable triggers an installation attempt.

Automatic installation currently supports Debian/Ubuntu Linux with `apt-get`. It runs `apt-get --no-remove -y install form`, using `pkexec --disable-internal-agent` for the system authentication dialog when the current process is not root. The package never collects passwords, changes repositories or launches installation from checking/calculating. The Debian/Ubuntu `form` package supplies both `form` and `tform`; the installation command is the same for either configuration. An active package-manager transaction is allowed to finish before a requested abort takes effect; installation has no calculation timeout.

Unsupported platforms, absent authentication agents, denied authorization and package-manager failures return guidance and diagnostic information. Install manually using your distribution's package manager or obtain FORM from the [official project](https://github.com/form-dev/form). On Debian/Ubuntu the manual command is `sudo apt-get install form`. If package lists are stale, update them separately before retrying; the helper does not do that automatically. Installation succeeds only after a fresh arithmetic probe passes.

For example, explicitly install and verify support for four TFORM workers:

```mathematica
CalcFormCheck[FORMThreads -> 4]
CalcFormInstall[FORMThreads -> 4]
```

A successful call returns the availability association, including version, probe output and installation guidance. `Available -> True` and `ExitCode -> 0` mean the requested configuration passed its probe. Guidance is included even when nothing needed installing. Checking or installing with four workers does not change future calculation defaults: pass `FORMThreads -> 4` to each calculation that should use them.

If the package manager reports success but the requested executable is still missing or fails its probe, installation returns `InstallationVerificationFailed` with the check result and retained logs. Installation success requires the requested executable to work.

### Troubleshooting

A kernel launched from a desktop may have a different `PATH` from your terminal; supply an absolute `FORMExecutable` path. If the file exists but the check reports `LaunchFailed`, inspect the returned messages and the operating system's execution restrictions. Reinstalling FORM does not fix a sandbox that prevents Mathematica from launching processes. If `ProbeFailed` or a calculation failure occurs, inspect standard output/error; calculation log files are retained in `JobDirectory`.

## Export options and failures

| Export option | Default | Meaning |
| --- | --- | --- |
| `Dimension` | `Automatic` | Infer one Lorentz space from the input; purely scalar inputs default to `D`. An explicit value must agree with existing tensor dimensions. |
| `LoopMomenta` | `{}` | Distinct momentum symbols recorded for later reduction. No loop-count restriction. |
| `OverwriteTarget` | `False` | Refuse an existing input, mapping, or result path unless replacement is explicitly requested. |

Use a `.frm` filename. Paths may contain spaces; quotes, angle brackets, backticks and line breaks are rejected. Export does not create the destination directory. With replacement enabled, export stages both files and keeps recovery copies while replacing the program and mapping. A failed operation or abort before commit restores the previous pair; if restoration itself fails, `Failure["RollbackFailed", ...]` reports retained `RecoveryFiles`. This handles recoverable errors and Wolfram interrupts, not a machine crash or concurrent writers to the same paths. After a successful export, FORM overwrites the result when run. Until then, any old result remains on disk and should not be treated as a new calculation.

Invalid arguments, unsupported expressions, incompatible dimensions, invalid mappings, unknown result identifiers and malformed output return `Failure` objects. Inspect them with `FailureQ[job]` or `FailureQ[result]` before proceeding.

## Supported vocabulary

- Exact integers, rationals, complex coefficients, scalar symbols, sums, products and scalar rational powers.
- Fully qualified symbols, including Greek/script names and identical names in distinct contexts.
- `MT`, `MTD`, `FV`, `FVD`, `SP`, `SPD`, and corresponding internal `Pair`, `LorentzIndex`, and `Momentum` expressions. `FCI` performs lightweight normalization.
- One consistent Lorentz space: four dimensions, a symbolic dimension such as `D`, or an integer dimension of at least two. Free indices are supported.
- Linear momentum routing with exact rational coefficients, including `-l` and `p-l`.
- Ordinary quadratic `FAD` / `FeynAmpDenominator[PropagatorDenominator[...], ...]`, including massless, massive, symbolic masses and repeated propagators.
- Scalar `A0`, `B0`, `C0`, `D0`, with respectively 1, 3, 6 and 10 positional arguments. Options on master functions and general `PaVe` objects are outside this first version.

Dirac/color objects, Levi-Civita tensors, noncommutative products, mixed Lorentz spaces, inexact numbers, nonlinear or symbolically weighted momentum routing, `SFAD`/`CFAD`, and unknown function heads are rejected. Unknown tensors are never automatically classified as scalars. Assigned symbols follow ordinary Wolfram Language evaluation; use unassigned symbols for symbolic inputs and imports.

## What FORM does

Metrics, components and scalar products become native FORM objects. FORM performs polynomial algebra and tensor contractions, and `.sort` combines terms at module boundaries. The source retains sums as reusable preprocessor definitions; expansion happens in FORM. The exporter never calls `Calc`, `Contract`, `TID`, or expands the complete expression. Only short linear momentum combinations are distributed during serialization.

For a top-level product containing at least two sum factors, generation proceeds in stages. The product is split at its sum factors, preserving the original order of every factor. The initial prefix becomes the starting expression; each subsequent part is multiplied in after a `.sort`. This can eliminate intermediate terms sooner and gives TFORM multiple input terms to distribute across workers in later modules. Eligibility requires at least two immediate `Plus` factors after `FCI` normalization; a sum hidden inside a power does not count. Other expression shapes retain a single defining module. No factor-size reordering heuristic is applied.

This matters because a newly defined FORM expression starts as a single input term: its defining module cannot benefit from parallel distribution. See the [FORM manual's discussion of parallelization](https://form-dev.github.io/form-docs/master/manual/). Merely increasing the worker count for a single large definition can therefore add overhead. Staging makes parallel work possible but does not guarantee that more workers will always be faster.

Generated calculation programs suppress source echo with `#-`; errors and the dedicated result file remain available. The complete program is retained in `job.frm` when job files are kept.

Propagators are commuting scalar identifiers. Each mapping entry stores its original FeynCalc definition, routing, mass, dimension, unit power and Feynman prescription; repeated occurrences/powers in the expression retain multiplicity. Radicals and inverse composite scalar expressions are also reversible scalar abbreviations. Consequently FORM cannot cancel a numerator against an opaque denominator or simplify relations involving opaque abbreviations. Import restores these objects exactly, although an equivalent product of denominators need not have the same grouping as the original combined `FAD`.

The converter inserts no symmetry factor, loop measure, on-shell condition or factor of `i`. Existing vertex factors of `i` and couplings are retained. The output is the algebraically contracted integrand, not an integrated self-energy.

The importer parses a restricted arithmetic grammar with native tensor syntax and `cfA0`, `cfB0`, `cfC0`, `cfD0`. It does not evaluate source text, assignments or arbitrary function calls from the result. Mapping expressions use a restricted JSON tree with whitelisted heads. Import does not simplify or reduce integrals.

## Organization and future reduction

- `CalcFormConverter.wl`: public interface and private implementation, organized into Mathematica initialization cells. Shared specifications define supported heads, master functions and symbol categories. Export builds conversion data in memory, renders the program and mapping, then writes the files through separate private functions.
- `Templates/Program.frm.in`: declarations, factor definitions, target expression, processing boundary, dedicated result output and termination.
- `Examples/ScalarBubble.wl`: the supplied scalar-projected quadratic-gravity bubble, before symmetry factor and integration measure. Load the cubic library with `importQuadraticGravity[1]` before evaluating the example.
- `FORMRuntime.wl`: executable discovery, arithmetic probe, streamed process logs, automated calculation and explicit installation. Loaded as definitions only in the same private context.
- `FORMAT.md`: version-one mapping schema, result grammar and compatibility policy.
- `Tests/`: separate core, FORM round-trip, runtime and FeynGrav integration suites, with a development-only Python driver. Fixed version-one artifacts test compatibility with saved calculations.

Future FORM procedures can be inserted at the marked processing boundary without changing the export/import commands. Denominator algebra, repeated-propagator reduction, exceptional kinematics, integral normalization, dimensional-regularization conventions and general one-loop tadpole/bubble/triangle/box reduction remain separate work. Master-integral names are already supported in both directions. Multiple loop momenta can already be recorded; no two-loop reduction is implemented.

## Verification

The cleanup implementation uses Wolfram Language's built-in [WithCleanup](https://reference.wolfram.com/language/ref/WithCleanup.html). Use a kernel that provides this function; verification was performed with Wolfram 15.0.1.

Run from this directory or use the full path:

```sh
python3 Tests/run.py
```

The default runs all four suites. Installer tests always use mocks and never modify system packages. To run them separately:

```sh
python3 Tests/run.py --suite core
python3 Tests/run.py --suite form
python3 Tests/run.py --suite runtime
python3 Tests/run.py --suite integration
```

| Suite | Dependencies | Coverage |
| --- | --- | --- |
| `core` | WolframKernel and FeynCalc | Mapping/parser checks, long and nested sums/products, typed-token rejection before cancellation, persisted version-one fixture, in-memory rendering, export rollback/recovery and mocked installation decisions |
| `form` | Core dependencies plus FORM | Bosonic export–FORM–import comparisons, staged and independent monolithic programs, free/contracted indices, mapping order and fallback cases |
| `runtime` | FORM dependencies; Linux/POSIX test environment | Real probes/calculations, optional installed TFORM checks, reporting options, controlled failing executables, logging, timeout, abort at acquisition/polling/finalization and cleanup |
| `integration` | FORM dependencies plus FeynGrav's cubic quadratic-gravity library | Vertex/propagator comparisons, loading and namespace isolation, complete bubble export |

Executables must be on `PATH`; `core` does not locate or launch FORM. The `runtime` suite requires Mathematica to be allowed to start external processes; a restricted execution sandbox may block it. The driver uses separate temporary directories and fresh kernels. Tests identify cases by descriptive keys, so adding a case does not change other tests' meaning. The fixed compatibility artifacts in `Tests/Fixtures` are read without regeneration.

The complete example bubble was constructed and exported on the development machine using FORM 4.3 as the available test backend. The latest recorded export after staged generation took **2.039 seconds**, producing **480,503 bytes** of FORM source and **6,685 bytes** of mapping from an expression with **178,961 leaves**. This measures conversion only. Full-bubble FORM contraction was not benchmarked, and no speedup claim is made. Smaller generated programs are executed in the test suite and compared with FeynCalc. The full-bubble test checks its mapping against the original propagators, masses, indices, abbreviations and loop momenta without executing the complete FORM job. Source size varies slightly with the output path embedded in the program.


### Recorded performance checks

On the development machine, a retained 636 KB result containing 6,244 terms was used for paired full-import comparisons. The original importer took 7.626 and 7.683 seconds; the optimized importer took 2.989 and 3.573 seconds in the corresponding runs. All imported expressions were exactly equal under `SameQ`. These are measurements on one result, not a general performance guarantee.

A separate bounded comparison used the retained program that produced that result:

| Program structure | Serial FORM | TFORM, four workers |
| --- | ---: | ---: |
| One defining module | 0.861 s | 0.921 s |
| Staged multiplication in original factor order | 0.338 s | 0.180 s |

All four result files were byte-for-byte identical. These single-run measurements illustrate the effect of the generated program's structure; the complete scalar bubble was not evaluated for this comparison. Export-registry microbenchmarks also showed approximately 19–23% improvement on synthetic repeated-symbol expressions, with identical generated data.

The completed regression run passed 285 assertions across the four suites, plus package-loading and full-bubble export checks. Independent staged-program checks also passed under TFORM. No system packages were installed during testing.

## Maintaining the converter

Keep the public export/import signatures independent of internal refactoring. The private export stages are:

1. `buildExportData[expression, dimension, loopMomenta]`: normalize and inspect the input, register symbols, and return the initial expression text, staged multiplication texts, factor macros and mapping data. It performs no file access.
2. `renderExport[data, resultPath, templateText]`: generate program and JSON text in memory. It performs no file access.
3. `writeExport[paths, rendered, overwrite]`: write the prepared files. Path checks and template reading are separate helpers used by the public command.

The export registry uses held expression keys containing both the symbol kind and value. This keeps identical names in different contexts and the same symbol in different roles distinct, without repeatedly encoding values as text. Each new mapping entry is encoded once. Staging must preserve the original traversal order so identifiers remain deterministic.

For a new scalar master function, add its head, FORM name, argument count and argument category to `$expressionSpecs`. Mapping decoding, export/import validation, serialization and FORM declarations use that specification. Add an explicit mathematical round-trip test and document the new vocabulary. Supporting a new tensor structure can still require translation and parser rules; the specification does not supply those algorithms.

Mapping prefixes, declaration classes and value checks live in `$kindSpecs`. Consult [the format contract](FORMAT.md) before changing persisted fields or their meaning. Preserve the existing version-one fixture when introducing another format version.

The import stages validate file correspondence and mapping names, decode all mapping entries into a job-local typed dictionary with `decodeEntries`, and reconstruct the result with `parseResult`. Every mapping expression is validated once, including entries absent from the result; repeated identifiers reuse their decoded value. The parser classifies each distinct lexical token once and uses documented `Reap`/`Sow` collectors for sums and products, constructing `Plus` and `Times` only after collecting and checking their operands. Keep vector/index validation before arithmetic evaluation so cancellation cannot hide an invalid token. The dictionary is local to each import; no decoded mapping state is shared between jobs.
