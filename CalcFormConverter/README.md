# CalcFormConverter

A standalone Wolfram Language package for translating exact bosonic FeynCalc expressions to FORM and reconstructing FORM results. It loads automatically with FeynGrav. It does not load the library generator or FeynCalcLegacy.

## Contents

- [Getting started](#getting-started)
- [Command and option reference](#command-and-option-reference)
- [Manual workflow](#manual-workflow)
- [Automated workflow](#automated-workflow)
- [Timing, progress and parallel execution](#timing-progress-and-parallel-execution)
- [Explicit installation](#explicit-installation)
- [Troubleshooting](#troubleshooting)
- [Export options and failures](#export-options-and-failures)
- [Supported vocabulary](#supported-vocabulary)
- [What FORM does](#what-form-does)
- [Performance guidance](#performance-guidance)
- [Developer guide](DEVELOPER.md) and [persisted format contract](FORMAT.md)

## Getting started

You need a Wolfram kernel with `WithCleanup` and an installed, loadable FeynCalc. Development verification uses Wolfram 15.0.1. FORM is needed to execute generated programs; it is not needed to load the converter, export a program or import an existing result. Python is used only by the developer test driver.

The converter loads with FeynGrav. To use it without loading the rest of FeynGrav, evaluate the `Get` command in the manual workflow below. FeynCalc is loaded as a package dependency. No vertex library is required for the neutral examples here. Restart the kernel after package updates to avoid mixing old and new definitions.

A small complete calculation, after loading:

```mathematica
Clear[p, mu, nu];
expression = MTD[mu, nu] FVD[p, mu] FVD[p, nu];
result = CalcFormCalculate[expression, TimeConstraint -> 60];
If[FailureQ[result], result, result === FCI[SPD[p, p]]]
(* True when FORM is available and the calculation succeeds. *)
```

`CalcFormCalculate` checks availability itself. If it reports `FORMUnavailable`, inspect `CalcFormCheck[]` before choosing an executable or explicitly installing FORM. It never installs software automatically.

For a binary equality `lhs == rhs`, `CalcFormCalculate` sends the residual `lhs - rhs` through one FORM job and compares the imported result with zero. The answer can remain a symbolic equality when it cannot be decided. Inputs already evaluated to `True` or `False` are returned directly. Chained equalities are unsupported (`UnsupportedEquality`); compare one pair per call. Manual export/import still accepts algebraic expressions; files retained from an equality calculation contain its residual.

## Command and option reference

Public commands and converter-specific options belong to the `` CalcFormConverter` `` context. After loading, their short names are normally available through `$ContextPath`; a fully qualified name such as ``CalcFormConverter`CalcFormImport`` also works. FeynCalc supplies tensor notation and its dimension/loop-momentum symbols; ordinary Wolfram options such as `TimeConstraint` retain their normal symbol identities.

| Call | Successful return | Work performed |
| --- | --- | --- |
| `CalcFormExport[expr, file, opts]` | Association with absolute `InputFile`, `MappingFile`, `ResultFile` paths | Normalize with `FCI`, validate, write program and mapping; the result path is reserved for FORM |
| `CalcFormImport[resultFile, mappingFile]` | Reconstructed FeynCalc internal expression | Read and validate the two files, reconstruct exact expressions; no options |
| `CalcFormCheck[opts]` | Availability association; inspect `Available` and `Status` | Locate executable and run an arithmetic probe when found |
| `CalcFormCalculate[expr, opts]` | Reconstructed expression, or Boolean/symbolic equality | Check, export, run, import, then clean up successful job files |
| `CalcFormInstall[FORMThreads -> n]` | Availability association after a successful check | Check existing installation; explicitly install only a missing requested engine on supported systems, then check again |

Conversion and calculation errors return `Failure`. A check that finds missing or unusable FORM normally returns an association with `Available -> False`; invalid options or inability to create a probe directory can instead return `Failure`. Check both cases:

```mathematica
status = CalcFormCheck[];
available = AssociationQ[status] && TrueQ[status["Available"]];
```

The check association always has `Available`, `Status`, `Executable`, `Version`, `RequestedEngine`, `FORMThreads`, and `InstallationGuidance`. When a probe ran it also reports `ExitCode`, `StandardOutput`, `StandardError`, and `Messages`. `RequestedEngine` describes the requested configuration, not an independent identification of an explicitly chosen binary. `Version` may be `Missing["NotReported"]`. Probe directories are cleaned up; use the returned diagnostic text.

| Option | Accepted values | Export default | Check default | Calculate default | Install default |
| --- | --- | --- | --- | --- | --- |
| `Dimension` | `Automatic`, a symbolic dimension other than `I`, or integer at least 2 | `Automatic` | — | `Automatic` | — |
| `LoopMomenta` | List of distinct unassigned symbols | `{}` | — | `{}` | — |
| `OverwriteTarget` | `True` or `False` | `False` | — | — | — |
| `FORMExecutable` | `Automatic`, executable name or path | — | `Automatic` | `Automatic` | — |
| `FORMThreads` | Positive integer worker count | — | `1` | `1` | `1` |
| `TimeConstraint` | Positive numeric seconds or `Infinity` | — | `10` | `Infinity` | — |
| `WorkingDirectory` | `Automatic` or existing parent-directory path | — | — | `Automatic` | — |
| `KeepFiles` | `True` or `False` | — | — | `False` | — |
| `ShowTiming` | `True` or `False` | — | — | `False` | — |
| `ShowProgress` | `True` or `False` | — | — | `False` | — |

A dash means that command does not accept the option. `CalcFormImport` has no option arguments. Installation accepts only `FORMThreads`; it does not accept an executable path or calculation timeout. Settings passed to one command do not change the defaults of later calls. Use `Options[CalcFormCalculate]` or `?CalcFormCalculate` to inspect the loaded interface.

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

An algebraic input returns a FeynCalc expression; a binary equality returns `True`, `False`, or a remaining symbolic equality. Errors return a `Failure` describing the stage and cause. Calculation failures after job creation include `JobDirectory`; process details include the exit code and log paths. Execution writes `stdout.log` and `stderr.log` while retaining only bounded diagnostic tails in memory. Timeout or user abort stops the calculation process and retains the job. One cleanup boundary covers log acquisition, execution and final inspection, including nonlocal exits. Probe directories have the same acquisition-to-cleanup protection. An abort during execution returns a failure rather than a partial expression.

Successful jobs are deleted unless `KeepFiles -> True`. Retained successful jobs report their path with `CalcFormCalculate::files`. Failed calculation jobs are retained once a job directory has been created. A failure during the initial availability check can occur before there are any job files. `job.frm`, `job.map.json` and `job.out` can then be used with the manual workflow. Never infer a successful calculation solely from a result file: check the returned value.

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

TFORM must be installed separately if your FORM distribution does not include it. See the [official TFORM description](https://www.nikhef.nl/~form/maindir/publications/tform.pdf). Parallelism can reduce execution time, but scaling depends on expression structure, sorting, memory and disk activity. Increasing workers does not accelerate Mathematica export/import. Compare timings on a representative expression before selecting a worker count. The full-bubble measurements in the developer guide compare generated programs at four workers; they do not measure scaling across worker counts.

### Explicit installation

`CalcFormInstall[FORMThreads -> n]` first checks the requested configuration; the default is `FORMThreads -> 1`. For `n > 1`, both this check and verification after installation use TFORM with the requested worker count. Working ordinary FORM does not prevent installation when TFORM is missing. It leaves a working installation alone and reports an existing but unusable executable without reinstalling it. Only a missing executable triggers an installation attempt.

Automatic installation currently supports Debian/Ubuntu Linux with `apt-get`. It runs `apt-get --no-remove -y install form`, using `pkexec --disable-internal-agent` for the system authentication dialog when the current process is not root. The package never collects passwords, changes repositories or launches installation from checking/calculating. The Debian/Ubuntu `form` package supplies both `form` and `tform`; the installation command is the same for either configuration. An active package-manager transaction is allowed to finish before a requested abort takes effect; installation has no calculation timeout.

Unsupported platforms, absent authentication agents, denied authorization and package-manager failures return guidance and diagnostic information. Install manually using your distribution's package manager or obtain FORM from the [official project](https://github.com/form-dev/form). On Debian/Ubuntu the manual command is `sudo apt-get install form`. If package lists are stale, update them separately before retrying; the helper does not do that automatically. Installation succeeds only after a fresh arithmetic probe passes.

For example, explicitly install and verify support for four TFORM workers:

```mathematica
CalcFormCheck[FORMThreads -> 4]
CalcFormInstall[FORMThreads -> 4]
```

A successful installation/check returns the availability association, including version, probe output and installation guidance. `Available -> True` and `ExitCode -> 0` mean the requested configuration passed its probe. Guidance is included even when nothing needed installing. Checking or installing with four workers does not change future calculation defaults: pass `FORMThreads -> 4` to each calculation that should use them.

If the package manager reports success but the requested executable is still missing or fails its probe, installation returns `InstallationVerificationFailed` with the check result and retained logs. Installation success requires the requested executable to work.

## Troubleshooting

A kernel launched from a desktop may have a different `PATH` from your terminal; supply an absolute `FORMExecutable` path. If the file exists but the check reports `LaunchFailed`, inspect the returned messages and the operating system's execution restrictions. Reinstalling FORM does not fix a sandbox that prevents Mathematica from launching processes. If `ProbeFailed` or a calculation failure occurs, inspect standard output/error; calculation log files are retained in `JobDirectory`.

For a calculation failure, the payload is available as `failure[[2]]`; keys such as `Stage`, `Cause`, `Check`, `Process`, and `JobDirectory` depend on where it failed. A nested `Cause` preserves export/import diagnostics. `KeepFiles -> True` still returns an expression, not a job association; the location is reported with `CalcFormCalculate::files`.

| Symptom or tag | What to check |
| --- | --- |
| `NotFound` / `FORMUnavailable` | Kernel `PATH`, explicit executable path, requested FORM versus TFORM configuration; inspect the nested `Check` |
| `ThreadingUnavailable` | Use a working TFORM executable for `FORMThreads > 1`; an ordinary FORM probe is insufficient |
| `InvalidOption` / `InvalidArguments` | Consult the command table; use actual Boolean values and a positive integer worker count |
| `InvalidPath` / `FileExists` | Existing destination directory, `.frm` extension, permitted path characters and intentional `OverwriteTarget -> True` |
| `MixedDimensions` / `DimensionMismatch` | Use one Lorentz space throughout; setting `Dimension` does not convert tensors |
| `Unsupported…` during export | Exact numbers, supported heads, rational linear momentum routing, ordinary quadratic denominators |
| `TimedOut` / `Aborted` | Retained calculation logs; choose a larger execution limit if appropriate. The availability probe remains a separate ten-second operation |
| `FORMFailed` / `MissingResult` | Exit code and logs; preserve the program/mapping/result together for diagnosis |
| `MappingMismatch` | Correct original mapping, unedited JSON text, and the dedicated result rather than console output |
| `InvalidMapping` / `UnknownIdentifier` / `InvalidResult` | Supported mapping version, declared names, result grammar, and unassigned imported symbols |
| `RollbackFailed` | Preserve the reported `RecoveryFiles` before attempting another export |
| `FORMUnusable` during installation | Resolve the existing executable's launch/probe failure; the installer deliberately does not replace it |
| `AuthorizationUnavailable` / `InstallationFailed` | System authentication agent and retained package-manager diagnostics; manual installation is separate |
| Long pause after FORM finishes | Import and notebook formatting are separate stages; suppress large output with a semicolon and time the complete call |

## Export options and failures

| Export option | Default | Meaning |
| --- | --- | --- |
| `Dimension` | `Automatic` | Infer one Lorentz space from the input; purely scalar inputs default to `D`. An explicit value must agree with existing tensor dimensions. |
| `LoopMomenta` | `{}` | Distinct momentum symbols recorded for later reduction. No loop-count restriction. |
| `OverwriteTarget` | `False` | Refuse an existing input, mapping, or result path unless replacement is explicitly requested. |

Use a `.frm` filename. Paths may contain spaces; quotes, angle brackets, backticks and line breaks are rejected. Export does not create the destination directory. With replacement enabled, export stages both files and keeps recovery copies while replacing the program and mapping. A failed operation or abort before commit restores the previous pair; if restoration itself fails, `Failure["RollbackFailed", ...]` reports retained `RecoveryFiles`. This handles recoverable errors and Wolfram interrupts, not a machine crash or concurrent writers to the same paths. Export replaces only the program and mapping; it does not delete an existing result. After a successful export, FORM overwrites the result when run. Until then, any old result remains on disk and should not be treated as a new calculation, even if exporting the same expression produces a matching mapping digest.

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

### Dimensions and symbol identity

`MT`, `FV`, and `SP` describe four-dimensional objects; `MTD`, `FVD`, and `SPD` describe the symbolic `D` space. The converter rejects mixtures rather than converting between them. Purely scalar input defaults to `D`. A symbolic dimension must be a single symbol: an explicit `D - 4` is an expression and is not accepted. For example, `CalcFormExport[x + 1, file, Dimension -> 4]` selects four dimensions when no tensor dimension contradicts it. `LoopMomenta` records symbols as metadata; it neither integrates them nor imposes kinematics.

The mapping retains full symbol contexts: ``Left`p`` and ``Right`p`` remain distinct even though their short names match. Greek/script symbols are encoded as data, while FORM receives generated ASCII identifiers. The same symbol used as a scalar, vector and index is registered separately for each role. Identifier numbering belongs to one export; never reuse a name such as `cfv1` across jobs without its mapping.

After a successful export, inspect the dictionary without changing its file:

```mathematica
mapping = Import[job["MappingFile"], "RawJSON"];
Dataset[KeyTake[#, {"Name", "Kind", "Expression"}] & /@ mapping["Entries"]]
```

`Name` is the generated FORM identifier, `Kind` is its scalar/vector/index/abbreviation/denominator role, and `Expression` is the restricted encoded definition. For example, a vector entry can map `cfv1` to ``{"Symbol", "Global`p"}``. The full [prefix and entry table](FORMAT.md#mapping-fields) explains each kind. Keep the original JSON file unchanged: rewriting or reformatting it changes the correspondence digest even when the decoded data seems identical.

Import reconstructs actual Wolfram symbols and allowed expression heads. Existing own-values, down-values or up-values can therefore affect evaluation. Use unassigned symbols and an appropriate kernel/context for saved calculations. The restricted parser prevents arbitrary source-text execution; it is not a sandbox for definitions already installed in the kernel. See [FORMAT.md](FORMAT.md) for encoding, metadata authority and compatibility details.

## What FORM does

Metrics, components and scalar products become native FORM objects. FORM performs polynomial algebra and tensor contractions, and `.sort` combines terms at module boundaries. The source retains sums as reusable preprocessor definitions; expansion happens in FORM. The exporter never calls `Calc`, `Contract`, `TID`, or expands the complete expression. Only short linear momentum combinations are distributed during serialization.

For a top-level product containing at least two immediate sum factors after `FCI`, generation proceeds in stages separated by `.sort`. Serialization retains the original traversal order and identifier assignment. A conservative planner then prefers stages sharing open tensor indices, breaking ties by tensor content, expression size and original position. This often contracts a small tensor with a connected vertex before expanding independent factors. It does not expand the Wolfram expression or call `Contract`.

Reordering requires every branch of each sum to have the same index multiplicities, with no index occurring more than twice across the product. Ambiguous signatures, repeated indices beyond this limit and scalar-only products retain their original stage order. Sums hidden inside powers do not count towards staging eligibility. Other expression shapes retain one defining module. The planner is a heuristic and does not guarantee an improvement for every tensor network. See [measured scope and limitations](DEVELOPER.md#connected-tensor-stage-ordering).

For eligible tensor products with a stage of at least 1,024 Wolfram leaves, FORM first normalizes the stage factors into named local expressions and hides them from subsequent operations. Multiplication then reuses their already combined terms, avoiding repeated expansion of routed momentum sums. Hidden factors remain available until the program ends. The cutoff is a conservative heuristic; it was not tuned to an optimal size. Ambiguous index signatures and small jobs keep the shorter program. Hidden storage can use memory or scratch disk, so this is a speed/memory tradeoff. See [incremental factor-normalization measurements](DEVELOPER.md#normalizing-large-form-stage-factors).

This matters because a newly defined FORM expression starts as a single input term: its defining module cannot benefit from parallel distribution. See the [FORM manual's discussion of parallelization](https://form-dev.github.io/form-docs/master/manual/). Merely increasing the worker count for a single large definition can therefore add overhead. Staging makes parallel work possible but does not guarantee that more workers will always be faster.

Generated calculation programs suppress source echo with `#-`; errors and the dedicated result file remain available. The complete program is retained in `job.frm` when job files are kept.

Propagators are commuting scalar identifiers. Each mapping entry stores its original FeynCalc definition, routing, mass, dimension, unit power and Feynman prescription; repeated occurrences/powers in the expression retain multiplicity. Radicals and inverse composite scalar expressions are also reversible scalar abbreviations. Consequently FORM cannot cancel a numerator against an opaque denominator or simplify relations involving opaque abbreviations. Import restores these objects exactly, although an equivalent product of denominators need not have the same grouping as the original combined `FAD`.

The converter inserts no symmetry factor, loop measure, on-shell condition or factor of `i`. Existing vertex factors of `i` and couplings are retained. The output is the algebraically contracted integrand, not an integrated self-energy.

The importer parses a restricted arithmetic grammar with native tensor syntax and `cfA0`, `cfB0`, `cfC0`, `cfD0`. It does not evaluate source text, assignments or arbitrary function calls from the result. Mapping expressions use a restricted JSON tree with whitelisted heads. Import does not simplify or reduce integrals.

## Organization and future reduction

- `CalcFormConverter.wl`: public interface and private implementation, organized into Mathematica initialization cells. Shared specifications define supported heads, master functions and symbol categories. Export builds conversion data in memory, renders the program and mapping, then writes the files through separate private functions.
- `Templates/Program.frm.in`: declarations, factor definitions, target expression, processing boundary, dedicated result output and termination.
- [`Examples/ScalarBubble.wl`](Examples/ScalarBubble.wl): the supplied scalar-projected quadratic-gravity bubble, before symmetry factor and integration measure. Its workflow comments show manual export/import, automated calculation, TFORM, progress and timing. Load FeynGrav and the cubic library with `importQuadraticGravity[1]` before evaluating `ScalarBubbleExample`. Loading the example file only defines the example function; it does not execute FORM.
- `FORMRuntime.wl`: executable discovery, arithmetic probe, streamed process logs, automated calculation and explicit installation. Loaded as definitions only in the same private context.
- `FORMAT.md`: version-one mapping schema, result grammar and compatibility policy.
- `Tests/`: separate core, FORM round-trip, runtime and FeynGrav integration suites, with a development-only Python driver. Fixed version-one artifacts test compatibility with saved calculations.

Future FORM procedures can be inserted at the marked processing boundary without changing the export/import commands. Denominator algebra, repeated-propagator reduction, exceptional kinematics, integral normalization, dimensional-regularization conventions and general one-loop tadpole/bubble/triangle/box reduction remain separate work. Master-integral names are already supported in both directions. Multiple loop momenta can already be recorded; no two-loop reduction is implemented.

## Performance guidance

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

Complete native vector-component and metric calls are recognized as single import tokens and decoded lazily through the normal identifier and argument checks. Repeated calls reuse their validated values within that import. Other syntax, including nested arguments and dot chains, keeps the ordinary parser; successful dot reconstruction is cached locally. This preserves error order and avoids repeatedly parsing the same short tensor calls. The measured median paired improvement was about 29.5% across ten full-import comparisons on one retained result; scalar and master-function inputs showed little benefit. Cache memory grows with distinct calls and vector pairs, and peak memory has not been measured.

Large flat results can additionally reuse complete factors within one import. This path accepts a conservative subset of scalar identifiers, integers, vector dots and integer powers; each distinct factor is still reconstructed by the general parser in consumption order. Small inputs, insufficient repetition, nested syntax and mapped symbols with `UpValues` use the general path. The 131,072-character threshold and roughly fourfold repetition cutoff are heuristics. They do not change the saved format or generated FORM program.

Against revision `a5a398f`, three alternating full-import pairs on a retained 14.17 MB, 145,098-term result had medians **54.42 s versus 19.54 s**: **2.78x speedup, or 64.1% less wall time**, with exact equality on every run. This measures import, not FORM or a complete calculation. A separate fresh-kernel comparison measured approximately 989 MB versus 870 MB peak tracked kernel memory; these are not OS resident-memory figures. Repeated-session memory retention and mostly unique inputs still need consideration. Methods, controls and limitations are in the [developer guide](DEVELOPER.md#reusing-repeated-factors-in-large-wolfram-imports).

Repeated tensor, denominator and scalar-abbreviation conversions are cached within each export call. The first occurrence still performs validation and symbol registration; later occurrences reuse its serialized fragment. No cache is shared between jobs, and general expression emission is not cached because sums allocate ordered macros. This benefits expressions with repeated structures; mostly unique inputs can incur extra lookup and memory costs. Cache storage grows with the number and size of distinct cached expressions.

Bounded development comparisons found further full-import reductions of about 34% on one retained result after the token-reader improvement, and about 8% for in-memory export-data construction on one large expression after the registry adjustment. These measure different stages and baselines; they must not be added together or treated as guaranteed end-to-end speedups. Details and historical measurements are in the [developer guide](DEVELOPER.md#performance-measurement).

## Development and tests

Run `python3 Tests/run.py --suite core` for conversion, parser, transaction and mocked installer tests without FORM. The default `python3 Tests/run.py` additionally runs FORM, runtime and FeynGrav integration suites with their dependencies. Tests do not install system packages.

See [DEVELOPER.md](DEVELOPER.md) for architecture, invariants, suite dependencies and measurement methodology. [FORMAT.md](FORMAT.md) is the persisted version-one contract; private helper associations are not public APIs.

### Polarization vectors

Lorentz components and scalar products support `Momentum[Polarization[p, I], dim]`
and the complex conjugate identity `Momentum[Polarization[p, -I], dim]`, with
`p` an unassigned symbol or an exact rational linear combination of momentum
symbols, such as `-p2-p3-p4`. The entire polarization of this momentum is one
vector identity; it is never distributed over the sum. An optional `Transversality -> True` or `False` is
preserved. The two labels remain distinct vectors; they are not factors of `I`.
This supports FeynGrav's `PolarizationTensor` after normalization with `FCI`.
FORM contracts these vectors, and import reconstructs their full identities,
allowing FeynCalc's current scalar-product and transversality definitions to
apply. No polarization sum, normalization or helicity condition is inferred.
Use one Lorentz dimension throughout. Color-labelled polarizations, other
options and polarization vectors in propagator routing remain unsupported.
