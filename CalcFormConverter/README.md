# CalcFormConverter

A standalone Wolfram Language package for translating supported exact FeynCalc expressions to FORM and reconstructing FORM results. It loads automatically with FeynGrav. It does not load the library generator or FeynCalcLegacy.

## Contents

- [Getting started](#getting-started)
- [Manual workflow](#manual-workflow)
- [Automated workflow](#automated-workflow)
- [Timing, progress and parallel execution](#timing-progress-and-parallel-execution)
- [Explicit installation](#explicit-installation)
- [Command and option reference](#command-and-option-reference)
- [Export options and failures](#export-options-and-failures)
- [Supported vocabulary](#supported-vocabulary)
- [Polarisation vectors](#polarisation-vectors)
- [Lorentz Levi-Civita tensors](#lorentz-levi-civita-tensors)
- [Dirac and colour translation](#dirac-and-colour-translation)
- [SU(N) colour algebra](#sun-colour-algebra)
- [What FORM does](#what-form-does)
- [Troubleshooting](#troubleshooting)
- [Performance guidance](#performance-guidance)
- [Organisation and future reduction](#organisation-and-future-reduction)
- [Development and tests](#development-and-tests)

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

After updating an already loaded package, restart the kernel and load FeynGrav again before comparing behaviour or performance.

Export to an existing directory:

```mathematica
expression = MTD[mu, nu] FVD[l, mu] FVD[p - l, nu] FAD[{l, m}, {p - l, m}];
job = CalcFormExport[expression, "/tmp/bubble.frm", LoopMomenta -> {l}];
```

A successful manual export prints a static summary with the complete paths labelled FORM program, Symbol mapping and Expected result. A kernel without a front end prints plain text. The returned association still contains `InputFile`, `MappingFile`, and `ResultFile`. Suppress its separate output while keeping the summary and recording elapsed time with:

```mathematica
AbsoluteTiming[
  job = CalcFormExport[expression, "/tmp/bubble.frm", LoopMomenta -> {l}];
]
```

This returns `{elapsedSeconds, Null}`; the paths remain accessible through `job`. `CalcFormCalculate` suppresses this manual export summary and retains its existing progress controls.

The exporter writes `bubble.frm` and `bubble.map.json`; FORM creates `bubble.out` when executed. Run FORM separately in a terminal:

```sh
form /tmp/bubble.frm
```

For manual execution with four TFORM workers, use `tform -w4 /tmp/bubble.frm` instead. Both executables consume the same generated program and produce the same result format.

Then in Mathematica:

```mathematica
result = CalcFormImport[job["ResultFile"], job["MappingFile"]];
```

Keep the mapping alongside the result. The result header must match the mapping's digest. A mapping also fingerprints the original expression, so a result from a different expression is rejected even when its symbol vocabulary is identical. This is a consistency check, not a cryptographic authentication mechanism.

### Automatic propagator grouping

Expressions containing ordinary quadratic propagators are automatically grouped by their **complete product of denominator factors**, including powers. There is no switch to disable this. For example, the result retains the structure

```mathematica
FAD[{p, m}] (a + b) + FAD[{p, m}]^2 FAD[q] (c + d)
```

rather than distributing each denominator product over its coefficient. Terms without propagators form the unit-prefactor group. Before grouping, the generated programme cancels reducible polynomial numerator scalar products against ordinary propagators. It does not impose kinematics, perform partial fractions or apply integral identities. Eligible commuting coefficients have common numerical and monomial factors extracted separately.

New grouped results are read incrementally. The importer retains one complete top-level summand's source text at a time, parses it with the existing restricted grammar, and discards its text and tokens before continuing. The final expression and the largest coefficient still need memory. Older saved results keep their existing interpretation, and jobs without propagators retain their existing output path. Re-export and rerun an old job to obtain the new grouped layout; updating the package does not rewrite an existing `.out` file. Do not call `Expand` on a large imported result unless the expanded representation is actually needed.

### Numerator–propagator cancellation

Both export and calculation automatically prepare a bounded basis of denominator polynomials. FORM expresses reducible scalar products through denominators present in each term, cancels their powers, restores unmatched numerator polynomials and then groups the surviving propagator products. For example:

```mathematica
CalcFormCalculate[SPD[l] FAD[{l, m}], LoopMomenta -> {l}]
(* 1 + m^2 FCI[FAD[{l, m}]] *)
```

The constant term is retained. There are no momentum shifts, scaleless-integral deletions, integrations or partial fractions. Scalar products outside the selected basis remain in the numerator. `LoopMomenta` gives loop-dependent scalar products priority in basis selection; without it, selection uses a deterministic ordering of all scalar products in the denominator routings. The algebra remains valid in either case, but the output basis may differ.

The current planner allows at most 256 nonempty independent denominator subsets. Before this search, a direct pass cancels matching squares against massless propagators with routing `a p`, where `a` is an exact nonzero rational. This pass handles repeated powers without creating terms. If the basis budget is exceeded, these direct cancellations remain in place. Routing matrices contain exact rational numbers only: no division by a mass difference, external invariant or Gram determinant is introduced. Cancellation does not penetrate opaque inverse-polynomial abbreviations or scalar functions. FORM also retains a hidden copy of the original expression and restores that directly cancelled expression if the candidate exceeds both 16 terms and twice the original expanded term count, or exceeds four times the original propagator-group count. This prevents large structural growth but does not guarantee smaller factored output. These checks apply to the complete job and occur after the candidate has been computed; they are not execution-time or memory limits.

Identities use ordinary quadratic propagators with their common Feynman prescription understood in the infinitesimal limit. They do not assert equality with a finite regulator omitted from a numerator. No new interpretation of older saved files is required; re-export existing programmes to include the stage.

The approach follows numerator cancellation described in [FeynCalc 9.0, section 3.2](https://arxiv.org/pdf/1601.01167) and the scalar-product basis approach of [Feng](https://arxiv.org/pdf/1204.2314). Small validation cases use `ApartFF[..., FDS -> False, DropScaleless -> False]` and direct rational identities, outside production calculations. See the [verification report](Tests/Reports/PropagatorCancellation.md).

### Bounded coefficient simplification

For version-one jobs, small scalar coefficients now undergo rational cancellation and numerator/denominator factorisation **inside FORM**, automatically in both `CalcFormExport` programs and `CalcFormCalculate`. The initial selection limit is 1,000 expanded terms per coefficient and 12 potential scalar variables per job, counting mapped scalar symbols and scalar products of registered vectors. Reciprocal-polynomial abbreviations must use already mapped symbols; their outer inverse powers are limited to eight and polynomial powers to 32. Other scalar abbreviations, including radicals, exclude this multivariate procedure. Free-index, complex and scalar-function coefficients also bypass it. These are conservative work-selection limits, not time or memory guarantees.

For remaining version-one coefficients, a separate pass simplifies rational dependence on the mapped symbolic Lorentz dimension alone. Momentum and mass monomials stay outside `PolyRatFun`; there is no full multivariate factorisation. Inverse polynomials depending only on that dimension are exposed within the same power limits. Other abbreviations remain opaque. This pass also accepts free-index, complex and scalar-function coefficients, and recognises a custom dimension symbol. It writes ordinary fractions and needs no importer or saved-format change. See the [dimension and massless-cancellation report](Tests/Reports/DimensionCoefficients.md) for validation and realistic-trial limits.

The remaining fallback uses `content_` to extract common numerical/monomial factors from coefficients with at most 20,000 terms and no free Lorentz indices, followed by momentum grouping. Epsilon, Dirac and colour jobs keep their existing single-level grouping. No new public option is needed. No Mathematica `Simplify` is called. Propagator factors stay opaque even when their scalar coefficient is simplified.

For example, `CalcFormCalculate[FAD[p] (D^2-1)/(D-1)]` returns the equivalent `(D+1) FCI[FAD[p]]` at generic `D`. Cancellation does not define a value at the original pole. See the [integration report](Tests/Reports/RationalCoefficientIntegration.md) for measured coverage and limits. Previously exported `.frm` files must be exported again to acquire this processing.

The stage prepares batches of up to four coefficients per worker, computes their common factors once, and normalises the residual expressions with TFORM whole-expression scheduling. Serial FORM uses batches of one. `FORMThreads` or a manual `tform -w8 job.frm` selects the workers. The batching rule is not a RAM cap. Existing execution timeouts and cancellation still apply to the complete FORM program.

Common factors are written outside the residual sums. Within each residual, a second native bracket groups repeated momentum monomials: for example, `p.q*(a+b) + q.q*(c+d)` keeps the two smaller sums instead of repeating the scalar products. The momentum dictionary supplies the grouping objects; no physical momentum names or on-shell relations are assumed. Scalar-only jobs retain the previous layout. This nested algebra is already supported by the restricted importer. This replaces the more expensive complete polynomial factorisation: output can be larger, but generating it can be much faster. The final imported expression must still fit in memory. See the [nested-grouping verification](Tests/Reports/NestedCoefficientGrouping.md) for current measurements and limitations, and the [common-factor report](Tests/Reports/CommonFactorExtraction.md) for the preceding stage. Re-export existing programs to use the new stage.

See [grouped-result format](FORMAT.md#grouped-propagator-results) and the [verification report](Tests/Reports/PropagatorGroups.md).

Loading the package does not change the working directory, write files, or locate/launch FORM. `CalcFormExport` and `CalcFormImport` do not launch external processes. The runtime commands below launch them only when explicitly called.

## Automated workflow

```mathematica
status = CalcFormCheck[];
(* If FORM is missing, installation is an explicit separate request: *)
(* CalcFormInstall[] *)

result = CalcFormCalculate[expression, LoopMomenta -> {l}];
```

`CalcFormCheck[FORMExecutable -> Automatic, FORMThreads -> Automatic, TimeConstraint -> 10]` searches the kernel's `PATH` and runs a small arithmetic probe. An explicit executable path takes precedence. Its association reports `Available`, `Status`, `Executable`, `Version`, `RequestedEngine`, `FORMThreads` and installation guidance, plus process diagnostics when a probe ran. `NotFound` means no executable was found; `LaunchFailed`, `ProbeFailed`, `TimedOut` and `Aborted` distinguish other failures. An available executable with an unrecognized banner has `Version -> Missing["NotReported"]`.

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

Calculation options are `DiracAlgebra -> Automatic`, `ColourAlgebra -> True`, `Dimension -> Automatic`, `LoopMomenta -> {}`, `FORMExecutable -> Automatic`, `TimeConstraint -> Infinity`, `WorkingDirectory -> Automatic`, `KeepFiles -> False`, `ShowTiming -> False`, `ShowProgress -> False`, and `FORMThreads -> Automatic`. The calculation timeout applies to FORM execution; the preceding availability probe has its own ten-second limit. `WorkingDirectory` names an existing parent directory; each call creates its own unique child. `Automatic` uses the system temporary directory. The Mathematica working directory is unchanged.

Import reconstructs the expression in the Wolfram kernel, so its time is separate from FORM execution. Assign large results with a trailing semicolon, as in these examples, to avoid the additional cost of formatting and displaying the full expression in a notebook.

An algebraic input returns a FeynCalc expression; a binary equality returns `True`, `False`, or a remaining symbolic equality. Errors return a `Failure` describing the stage and cause. Calculation failures after job creation include `JobDirectory`; process details include the exit code and log paths. Execution writes `stdout.log` and `stderr.log` while retaining only bounded diagnostic tails in memory. Timeout or user abort stops the calculation process and retains the job. One cleanup boundary covers log acquisition, execution and final inspection, including nonlocal exits. Probe directories have the same acquisition-to-cleanup protection. An abort during execution returns a failure rather than a partial expression.

Successful jobs are deleted unless `KeepFiles -> True`. Retained successful jobs report their path with `CalcFormCalculate::files`. Failed calculation jobs are retained once a job directory has been created. A failure during the initial availability check can occur before there are any job files. `job.frm`, `job.map.json` and `job.out` can then be used with the manual workflow. Never infer a successful calculation solely from a result file: check the returned value.

This workflow performs the template's existing algebra and Lorentz contractions. It does not perform loop integration or integral reduction.

## Timing, progress and parallel execution

```mathematica
result = CalcFormCalculate[expression,
  LoopMomenta -> {l},
  FORMThreads -> 4,
  ShowTiming -> True,
  ShowProgress -> True
];
```

`ShowTiming` prints elapsed wall-clock seconds for the calculation process, including its final output drain. It excludes the availability probe, export and import. This is elapsed time, not the sum of CPU time across workers. Process diagnostics on failures include `ElapsedSeconds` when execution started. The returned value remains the FeynCalc expression.

The first import in a fresh kernel can take longer than subsequent imports because of initialisation in the kernel and its dependencies. Measure the first call separately, then repeat the same saved result/mapping pair to measure subsequent calls:

```mathematica
firstImportSeconds = First[AbsoluteTiming[
  result = CalcFormImport[job["ResultFile"], job["MappingFile"]];
]];
repeatedImportSeconds = Table[
  First[AbsoluteTiming[
    result = CalcFormImport[job["ResultFile"], job["MappingFile"]];
  ]],
  {5}
];
```

Run this after FORM has produced the result file, check `FailureQ[result]`, and keep the files and symbol definitions unchanged between measurements. Report the first-call time and repeated-call times separately. Do not infer a universal slowdown ratio or attribute it to a particular cache from these timings alone. The converter's identifier and token caches are local to each import.

Use `AbsoluteTiming` to measure the complete call, including the availability check, export, execution and import:

```mathematica
{elapsedSeconds, ignored} = AbsoluteTiming[
  result = CalcFormCalculate[expression,
    LoopMomenta -> {l}, FORMThreads -> 4, ShowTiming -> True];
];
```

The trailing semicolon suppresses display of the large expression. Mathematica's `Timing` measures kernel CPU time and does not include the external FORM process's CPU time, so it should not be compared with `ShowTiming` as though both measured elapsed time.

`ShowProgress` prints the check, export, execution and import stages, followed by completion or failure. During FORM execution it also prints elapsed time every ten seconds. These updates show that the process is still running; they are not a percentage, an estimate of remaining work or counts of processed terms. Long export/import stages retain their stage label without periodic updates.

The default `FORMThreads -> Automatic` prefers TFORM with `Min[8, $ProcessorCount]` workers. A missing or invalid processor count uses one worker. A one-worker automatic configuration selects serial FORM. If TFORM is missing, automatic selection falls back to serial FORM; an existing TFORM that fails its probe is reported without fallback. Explicit `FORMThreads -> 1` selects serial FORM. Larger explicit integers require TFORM, are not capped, and do not fall back. An explicit `FORMExecutable` with automatic threads uses one worker; request a larger count explicitly to use that executable in parallel. Parallel probes must identify TFORM in the banner, otherwise they report `ThreadingUnavailable`.

The check result records the resolved integer in `FORMThreads`, the original option in `RequestedFORMThreads`, and a `SelectionReason` (`AutomaticTFORM`, `TFORMNotFound`, `SerialProcessorCount`, `ExplicitExecutable`, or `ExplicitThreads`). `ShowProgress -> True` reports the selected executable and worker count, explaining serial fallback. No checking or calculation installs software.

TFORM must be installed separately if your FORM distribution does not include it. See the [official TFORM description](https://www.nikhef.nl/~form/maindir/publications/tform.pdf). Parallelism can reduce execution time, but scaling depends on expression structure, sorting, memory and disk activity. Increasing workers does not accelerate Mathematica export/import. Compare timings on a representative expression before selecting a worker count. The full-bubble measurements in the developer guide compare generated programs at four workers; they do not measure scaling across worker counts.

## Explicit installation

`CalcFormInstall[FORMThreads -> n]` first checks the requested configuration; the default is `FORMThreads -> Automatic`, which accepts a working serial fallback without installation. For `n > 1`, both this check and verification after installation use TFORM with the requested worker count. With an explicit `n > 1`, working ordinary FORM does not prevent installation when TFORM is missing. It leaves a working installation alone and reports an existing but unusable executable without reinstalling it. Only a missing executable triggers an installation attempt.

Automatic installation currently supports Debian/Ubuntu Linux with `apt-get`. It runs `apt-get --no-remove -y install form`, using `pkexec --disable-internal-agent` for the system authentication dialog when the current process is not root. The package never collects passwords, changes repositories or launches installation from checking/calculating. The Debian/Ubuntu `form` package supplies both `form` and `tform`; the installation command is the same for either configuration. An active package-manager transaction is allowed to finish before a requested abort takes effect; installation has no calculation timeout.

Unsupported platforms, absent authentication agents, denied authorization and package-manager failures return guidance and diagnostic information. Install manually using your distribution's package manager or obtain FORM from the [official project](https://github.com/form-dev/form). On Debian/Ubuntu the manual command is `sudo apt-get install form`. If package lists are stale, update them separately before retrying; the helper does not do that automatically. Installation succeeds only after a fresh arithmetic probe passes.

For example, explicitly install and verify support for four TFORM workers:

```mathematica
CalcFormCheck[FORMThreads -> 4]
CalcFormInstall[FORMThreads -> 4]
```

A successful installation/check returns the availability association, including version, probe output and installation guidance. `Available -> True` and `ExitCode -> 0` mean the requested configuration passed its probe. Guidance is included even when nothing needed installing. Checking or installing with four workers does not change future calculation defaults: pass `FORMThreads -> 4` to each calculation that should use them.

If the package manager reports success but the requested executable is still missing or fails its probe, installation returns `InstallationVerificationFailed` with the check result and retained logs. Installation success requires the requested executable to work.

## Command and option reference

Public commands and converter-specific options belong to the `` CalcFormConverter` `` context. After loading, their short names are normally available through `$ContextPath`; a fully qualified name such as ``CalcFormConverter`CalcFormImport`` also works. FeynCalc supplies tensor notation and its dimension/loop-momentum symbols; ordinary Wolfram options such as `TimeConstraint` retain their normal symbol identities.

| Call | Successful return | Work performed |
| --- | --- | --- |
| `CalcFormExport[expr, file, opts]` | Association with absolute `InputFile`, `MappingFile`, `ResultFile` paths | Normalise with `FCI`, validate, write program and mapping; the result path is reserved for FORM |
| `CalcFormImport[resultFile, mappingFile]` | Reconstructed FeynCalc internal expression | Read and validate the two files, reconstruct exact expressions; no options |
| `CalcFormCheck[opts]` | Availability association; inspect `Available` and `Status` | Locate executable and run an arithmetic probe when found |
| `CalcFormCalculate[expr, opts]` | Reconstructed expression, or Boolean/symbolic equality | Check, export, run, import, then clean up successful job files |
| `CalcFormInstall[FORMThreads -> n]` | Availability association after a successful check | Check existing installation; explicitly install only a missing requested engine on supported systems, then check again |

### Comparing results

`CalcFormImport` and `CalcFormCalculate` return equivalent algebraic expressions, not a canonical simplified form. Eligible grouped scalar coefficients expose inverse polynomial factors such as `1/(D-1)` to FORM rational arithmetic; other scalar abbreviations retain their opaque interpretation. After import, an expression can therefore differ structurally from a FeynCalc reference even when their difference is zero. Neither command automatically calls `Simplify`. Processed colour jobs do have a scalar-only presentation step: it collects identical colour tensor structures and factors rank-dependent coefficients into the documented Casimir form. This is not a call to `SUNSimplify` or a general tensor reduction in Mathematica.

For a manageable scalar or already-contracted tensor example, compare the difference explicitly:

```mathematica
If[!FailureQ[result],
  difference = FCI[reference - result];
  comparison = TimeConstrained[Simplify[difference], 30, $TimedOut];
  verified = comparison === 0;
];
```

Use applicable assumptions when needed. A nonzero or timed-out simplification is inconclusive; it is not by itself evidence of an incorrect result. Substituting a particular dimension checks only that dimension. This comparison can be expensive on large expressions, so apply it deliberately rather than to every imported result.

Conversion and calculation errors return `Failure`. A check that finds missing or unusable FORM normally returns an association with `Available -> False`; invalid options or inability to create a probe directory can instead return `Failure`. Check both cases:

```mathematica
status = CalcFormCheck[];
available = AssociationQ[status] && TrueQ[status["Available"]];
```

The check association always has `Available`, `Status`, `Executable`, `Version`, `RequestedEngine`, `FORMThreads`, `RequestedFORMThreads`, `SelectionReason`, and `InstallationGuidance`. When a probe ran it also reports `ExitCode`, `StandardOutput`, `StandardError`, and `Messages`. `RequestedEngine` describes the resolved worker configuration (after automatic fallback), not an independent identification of an explicitly chosen binary. `RequestedFORMThreads` preserves the original option. `Version` may be `Missing["NotReported"]`. On launch failure, `Messages` includes available Wolfram message identifiers and bounded rendered diagnostic text; `StandardError` remains subprocess output. The text is captured without printing kernel launch messages in the notebook. It may still be empty if Wolfram supplies no diagnostic, and it does not necessarily expose an OS error number. Probe directories are cleaned up; use the returned diagnostic text.

| Option | Accepted values | Export default | Check default | Calculate default | Install default |
| --- | --- | --- | --- | --- | --- |
| `Dimension` | `Automatic`, a symbolic dimension other than `I`, or integer at least 2 | `Automatic` | — | `Automatic` | — |
| `LoopMomenta` | List of distinct unassigned symbols | `{}` | — | `{}` | — |
| `DiracAlgebra` | `Automatic` or `False` | `Automatic` | — | `Automatic` | — |
| `ColourAlgebra` | `True`, `Automatic` or `False` | `True` | — | `True` | — |
| `OverwriteTarget` | `True` or `False` | `False` | — | — | — |
| `FORMExecutable` | `Automatic`, executable name or path | — | `Automatic` | `Automatic` | — |
| `FORMThreads` | Automatic or positive integer worker count | — | `Automatic` | `Automatic` | `Automatic` |
| `TimeConstraint` | Positive numeric seconds or `Infinity` | — | `10` | `Infinity` | — |
| `WorkingDirectory` | `Automatic` or existing parent-directory path | — | — | `Automatic` | — |
| `KeepFiles` | `True` or `False` | — | — | `False` | — |
| `ShowTiming` | `True` or `False` | — | — | `False` | — |
| `ShowProgress` | `True` or `False` | — | — | `False` | — |

A dash means that command does not accept the option. `CalcFormImport` has no option arguments. Installation accepts only `FORMThreads`; it does not accept an executable path or calculation timeout. Settings passed to one command do not change the defaults of later calls. Use `Options[CalcFormCalculate]` or `?CalcFormCalculate` to inspect the loaded interface.

## Export options and failures

| Export option | Default | Meaning |
| --- | --- | --- |
| `Dimension` | `Automatic` | Infer one Lorentz space from the input; purely scalar inputs default to `D`. An explicit value must agree with existing tensor dimensions. |
| `LoopMomenta` | `{}` | Distinct momentum symbols recorded for later reduction. No loop-count restriction. |
| `DiracAlgebra` | `Automatic` | Simplify and order ordinary open chains and evaluate explicit Dirac traces; `False` selects translation only. |
| `ColourAlgebra` | `True` | Reduce fundamental SU(N) colour expressions in FORM; `False` preserves colour objects and traces. |
| `OverwriteTarget` | `False` | Refuse an existing input, mapping, or result path unless replacement is explicitly requested. |

Use a `.frm` filename. Paths may contain spaces and text such as `@home@`; quotes, angle brackets, backticks and line breaks are rejected. Literal backslashes in Unix paths are preserved; Windows separators are converted to forward slashes in the generated FORM program. Export does not create the destination directory. With replacement enabled, export stages both files and keeps recovery copies while replacing the program and mapping. A failed operation or abort before commit restores the previous pair; if restoration itself fails, `Failure["RollbackFailed", ...]` reports retained `RecoveryFiles`. This handles recoverable errors and Wolfram interrupts, not a machine crash or concurrent writers to the same paths. If temporary-file cleanup fails after a successful export, the returned association still contains the three paths and additionally contains `CleanupFailure`, a `Failure["CleanupFailed", ...]` whose `RetainedFiles` lists the affected paths. A warning also reports them. If export itself fails, its original failure tag is preserved with `RetainedFiles` added when cleanup fails. Export replaces only the program and mapping; it does not delete an existing result. After a successful export, FORM overwrites the result when run. Until then, any old result remains on disk and should not be treated as a new calculation, even if exporting the same expression produces a matching mapping digest.

Invalid arguments, unsupported expressions, incompatible dimensions, invalid mappings, unknown result identifiers and malformed output return `Failure` objects. Inspect them with `FailureQ[job]` or `FailureQ[result]` before proceeding.

## Supported vocabulary

- Exact integers, rationals, complex coefficients, scalar symbols, sums, products and scalar rational powers.
- Fully qualified symbols, including Greek/script names and identical names in distinct contexts.
- `MT`, `MTD`, `FV`, `FVD`, `SP`, `SPD`, and corresponding internal `Pair`, `LorentzIndex`, and `Momentum` expressions. `FCI` performs lightweight normalisation.
- One consistent Lorentz space: four dimensions, a symbolic dimension such as `D`, or an integer dimension of at least two. Free indices are supported.
- Linear momentum routing with exact rational coefficients, including `-l` and `p-l`.
- Ordinary quadratic `FAD` / `FeynAmpDenominator[PropagatorDenominator[...], ...]`, including massless, massive, symbolic masses and repeated propagators.
- Polarisation vector identities with phase `I` or `-I` and optional transversality.
- Rank-four Lorentz `LC`, `LCD`, legacy `LeviCivita` and internal `Eps`, with one consistent Lorentz space.
- Ordinary `GA`, `GAD`, `GS`, `GSD`, ordered Dirac words and supported `DiracTrace` expressions.
- Fundamental SU(N) `SUNT`, `SUNTF`, `SUNF`, `SUND`, `SUNDelta`, `SUNFDelta`, `SUNTrace`, `SUNN`, `CA`, `CF` and whitelisted `SMP` scalar couplings.
- Scalar `A0`, `B0`, `C0`, `D0`, with respectively 1, 3, 6 and 10 positional arguments. Options on master functions and general `PaVe` objects are outside the supported vocabulary.

Spinors, explicit Dirac indices, gamma-five/projectors, Cartesian or mixed-space Levi-Civita tensors, unsupported noncommutative products, mixed Lorentz spaces, inexact numbers, nonlinear or symbolically weighted momentum routing, `SFAD`/`CFAD`, and unknown function heads are rejected. Unknown tensors are never automatically classified as scalars. Assigned symbols follow ordinary Wolfram Language evaluation; use unassigned symbols for symbolic inputs and imports.

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

## Polarisation vectors

Lorentz components and scalar products support `Momentum[Polarization[p, I], dim]`
and the complex conjugate identity `Momentum[Polarization[p, -I], dim]`, with
`p` an unassigned symbol or an exact rational linear combination of momentum
symbols, such as `-p2-p3-p4`. The entire polarisation of this momentum is one
vector identity; it is never distributed over the sum. An optional `Transversality -> True` or `False` is
preserved. The two labels remain distinct vectors; they are not factors of `I`.
This supports FeynGrav's `PolarizationTensor` after normalisation with `FCI`.
FORM contracts these vectors, and import reconstructs their full identities,
allowing FeynCalc's current scalar-product and transversality definitions to
apply. No polarisation sum, normalisation or helicity condition is inferred.
Use one Lorentz dimension throughout. Colour-labelled polarisations, other
options and polarisation vectors in propagator routing remain unsupported.

## Lorentz Levi-Civita tensors

`LC`, `LCD`, legacy `LeviCivita` and internal `Eps` are supported after FCI
normalisation. Exactly four slots are required, each a symbolic Lorentz index
or supported momentum. Linear routing is distributed only inside those slots.
`LCD` is rank four with dimensional indices, not a rank-D tensor. Numerical
component indices, Cartesian tensors and mixed Lorentz spaces remain unsupported.
The axion rule now supplies consistent dimensional `Eps` slots and momentum components.

```mathematica
CalcFormCalculate[LC[mu, nu, rho, sigma]^2]
(* -24 under the default convention *)

job = CalcFormExport[LCD[mu, nu][p - q, r], "/absolute/path/epsilon.frm"];
(* Run form epsilon.frm separately in its directory. *)
result = CalcFormImport[job["ResultFile"], job["MappingFile"]];
```

FORM contracts epsilon pairs with `contract 0;`. Surviving tensors return as
`Eps`. The supported `$LeviCivitaSign` values are -1, 1, -I and I; the converter
never changes this variable. Export records the convention, and import requires
the same setting even if FORM eliminated every epsilon. A mismatch returns
`EpsilonConventionMismatch`. Epsilon jobs use version-two mappings unless they also contain Dirac/colour
structures, in which case they use version three, or version four when Dirac processing or explicit traces are required.
Processed colour or colour-trace jobs use version five. Other jobs use version one.

## Dirac and colour translation

The converter accepts ordinary `GA`, `GAD`, `GS`, `GSD` and internal `DiracGamma`,
including ordered `Dot` products and scalar identity terms. Four-dimensional
and D-dimensional objects must each belong to one consistent Lorentz space.
Matrix products use `Dot`; multiplying separate implicit chains with `Times`
returns `AmbiguousMatrixProduct`. One implicit open chain per term is supported, together with independent explicit Dirac traces. Explicit Dirac indices are not yet supported.

Colour support accepts `SUNT` words, commuting `SUNTF` matrix elements,
`SUNF`, `SUND`, `SUNDelta`, `SUNFDelta`, `SUNTrace`, `SUNN`, `CA`, `CF`, and `SMP[name_String]`.
Colour indices are symbolic. `ColourAlgebra -> Automatic` reduces the supported
SU(N) structures; `False` preserves them. Unknown heads are still rejected. Ordinary
FeynCalc evaluation (including FCI) may simplify input before conversion.

```mathematica
CalcFormCalculate[MTD[mu, nu] GAD[mu]]
(* DiracGamma[LorentzIndex[nu, D], D] *)

CalcFormCalculate[(GSD[p] + m) . GAD[mu] SUNT[a, b]]

job = CalcFormExport[SMP["g_s"] GAD[mu] SUNT[a],
    "/absolute/path/quark.frm"];
(* Run form quark.frm separately in its directory. *)
result = CalcFormImport[job["ResultFile"], job["MappingFile"]];
```

Each result is represented as commuting coefficient times Dirac word times
colour word. Colour ordering is preserved; Dirac ordering is preserved with `DiracAlgebra -> False`, and canonically rewritten using the Clifford relation by default. The two matrix spaces
commute with each other. With colour processing disabled, colour tensors are mapped as complete typed
objects. With processing enabled, indices and explicit matrix elements are
translated separately. Individual implicit generators are never treated as commuting scalars.

`DiracAlgebra -> Automatic` (the default on export and calculation) contracts
ordinary open chains, puts the remaining gamma arguments in a deterministic
order and evaluates explicitly supplied `DiracTrace` expressions. It never
traces an open chain implicitly. Four-dimensional and symbolic-dimensional
traces use FORM's dimension-general `tracen` procedure.

```mathematica
CalcFormCalculate[GAD[mu] . GAD[mu]]
(* D *)
CalcFormCalculate[DiracTrace[GAD[mu] . GAD[mu]]]
(* 4 D, with the normal FeynCalc TraceOfOne setting *)
CalcFormCalculate[DiracTrace[GSD[p] . GSD[q], TraceOfOne -> n]]
(* n Pair[Momentum[p, D], Momentum[q, D]] *)

(* Preserve open-word ordering and return explicit traces unevaluated. *)
CalcFormCalculate[GAD[nu] . GAD[mu], DiracAlgebra -> False]
CalcFormCalculate[DiracTrace[GAD[mu] . GAD[nu]], DiracAlgebra -> False]

(* The same processing is embedded in a standalone export. *)
job = CalcFormExport[DiracTrace[GSD[p] . GSD[q]] GAD[mu],
    "/absolute/path/trace.frm", DiracAlgebra -> Automatic];
(* Execute form trace.frm, or tform -w4 trace.frm, separately. *)
result = CalcFormImport[job["ResultFile"], job["MappingFile"]];
```

Trace normalisation is captured from the effective `TraceOfOne` option during
export. Changing that setting before import does not reinterpret the saved
result. Scalar terms inside a trace multiply its identity matrix; products and
non-negative integer powers of traces receive independent spin lines. Only
`TraceOfOne` may differ from the current `DiracTrace` option defaults. Nested
traces, implicit colour words inside a Dirac trace, and inverse/fractional
trace powers are rejected. Commuting colour factors remain supported.

Canonical ordering uses index identities first, then vector identities, sorted
by their encoded context-qualified representations. It is a Clifford-algebra
normal form, not a shortest-expression guarantee or a four-dimensional
basis involving gamma-five. Reordering can expand a compact input.
Gamma-five, chiral projectors, external spinors and arbitrary
noncommutative heads remain excluded.

Version-four mappings describe processed Dirac jobs and explicit traces.
Processed colour jobs and newly supported colour traces use version five.
Other translation-only gamma/colour jobs retain version three; older mappings remain
readable. The FORM procedures are embedded in each exported file, so execution
does not require access to the installed module's template directory.

The [library generator](../Libs/Generator.md) now uses `CalcFormCalculate`.
Its uncontracted quark–gluon rule retains dimensional gamma matrices, and
regression tests compare that rule directly with independent FeynCalc algebra.
Stored libraries are changed only when explicitly regenerated.

## SU(N) colour algebra

`ColourAlgebra -> True` is the default for export and calculation.
`Automatic` remains an equivalent enabling value for compatibility. It uses
an embedded adaptation of the official FORM `SUn.prc` procedure. No extra
installation, runtime download or access to FeynGrav's directory is needed by
an exported program. Colour runs before configured Dirac processing.

The conventions are one fundamental SU(N) group with symbolic `SUNN`,
`tr(Ta Tb) = delta(a,b)/2`, `[Ta,Tb] = I f(a,b,c) Tc`, `CA = SUNN` and
`CF = (SUNN^2-1)/(2 SUNN)`. The procedure's flavour multiplicity is one.
Higher representations, multiple independent groups and numerical colour
components are excluded. Symbolic labels retain their full Wolfram contexts.
Lorentz, adjoint and fundamental index spaces remain distinct even when the
same symbol labels all three.

```mathematica
CalcFormCalculate[SUNT[a, a]]
(* CF *)
CalcFormCalculate[SUNT[a, b, a]]
(* -SUNT[SUNIndex[b]]/(2 CA), equivalent to (CF-CA/2) SUNT[b] *)
CalcFormCalculate[SUNTrace[SUNT[a, b, c]]]
(* (SUND[a,b,c] + I SUNF[a,b,c])/4, in internal notation *)
CalcFormCalculate[SUNTrace[SUNT[a,b,c,d]], ColourAlgebra -> False]
(* An unevaluated SUNTrace. *)

(* Colour and Dirac settings are independent. *)
CalcFormCalculate[DiracTrace[GAD[mu] . GAD[nu]] SUNT[a,a]]
(* 4 CF MTD[mu,nu], in internal notation with default TraceOfOne. *)
CalcFormCalculate[GAD[nu] . GAD[mu] SUNT[a,a], DiracAlgebra -> False]

job = CalcFormExport[SUNF[a,c,d] SUNF[b,c,d],
    "/absolute/path/colour.frm", ColourAlgebra -> Automatic];
(* Separately execute form colour.frm or tform -w4 colour.frm. *)
CalcFormImport[job["ResultFile"], job["MappingFile"]]
(* CA SUNDelta[a,b], in internal notation. *)
```

Empty traces reduce to N, one-generator traces to zero, two-generator traces
to deltas and three-generator traces to `SUND` and `SUNF`. Longer irreducible
traces remain `SUNTrace`. Cyclic permutations are equivalent; reversal is not
silently identified with the original word. Trace bodies support commuting
scalar coefficients and one implicit colour word, with local sum distribution.
Nested traces, explicit `SUNTF` elements inside `SUNTrace`, and non-default
unevaluated trace options are rejected.

### Promotion to explicit fundamental endpoints

An implicit `SUNT` expression denotes a matrix. Its scalar identity summands
therefore share the same endpoints. For example:

```mathematica
CalcFormCalculate[SUNT[a] SUNTF[{a}, i, j] + SUNFDelta[i,j]]
```

Completeness connects the implicit line to the explicit one. The result uses
two generated **free** fundamental indices, `left` and `right`, in a namespace
``CalcFormConverter`ColourIndices`h<expression digest>` ``. Schematically it is

```text
(delta(left,j) delta(i,right) - delta(left,right) delta(i,j)/N)/2
+ delta(left,right) delta(i,j).
```

Those endpoints must not be summed or discarded. The scalar identity branch
has also acquired `delta(left,right)`. Promotion applies consistently to the
whole result when any term loses an unambiguous implicit line. If every term
retains that line, the importer reconstructs `SUNT` and scalar identities.
Generated dummy indices use `d<number>` in the same namespace and are checked
to occur exactly twice within each term. Separate implicit chains multiplied
with `Times` remain ambiguous and unsupported; explicit `SUNTF` products can
represent several lines.

Import collects identical tensor factors and applies exact rational operations
to their scalar coefficients, extracting `N^2-1 = 2 CA CF`. It does not call
`SUNSimplify` or perform colour tensor algebra. This presentation prefers
Casimirs but does not promise the shortest representation.

See [upstream attribution and adaptation notes](ThirdParty/FORMColour/README.md),
[format version five](FORMAT.md#version-five-sun-colour-processing) and the
[colour verification report](Tests/Reports/ColourAlgebra.md).

## What FORM does

Metrics, components and scalar products become native FORM objects. FORM performs polynomial algebra and tensor contractions, and `.sort` combines terms at module boundaries. The source retains sums as reusable preprocessor definitions; expansion happens in FORM. The exporter never calls `Calc`, `Contract`, `TID`, or expands the complete expression. Linear momentum routing and sums inside supported noncommutative products are distributed locally during serialisation; the complete tensor input is not globally expanded.

Epsilon-containing expressions, matrix words and version-five colour jobs (including preserved colour traces) bypass tensor staging/preparation. For an eligible top-level product containing at least two immediate sum factors after `FCI`, generation proceeds in stages separated by `.sort`. Serialisation retains the original traversal order and identifier assignment. A conservative planner builds connected orders by preferring shared open tensor indices, breaking ties by tensor content, expression size and original position. For products with up to eight stages it tries each tensor stage as a starting point, then prefers a smaller maximum open-index width, smaller total width and narrower late intermediates. Larger products keep the original single-start greedy planner. This often contracts a small tensor with a connected vertex before expanding independent factors. It does not expand the Wolfram expression or call `Contract`.

Reordering requires every branch of each sum to have the same index multiplicities, with no index occurring more than twice across the product. Ambiguous signatures, repeated indices beyond this limit and scalar-only products retain their original stage order. Sums hidden inside powers do not count towards staging eligibility. Other expression shapes retain one defining module. The planner is a heuristic and does not guarantee an improvement for every tensor network. See the [bounded order-search verification](Tests/Reports/ContractionOrderSearch.md) and [measured scope and limitations](../Documentation/Verification/ConverterPerformance.md#connected-tensor-stage-ordering).

For eligible tensor products with a stage of at least 1,024 Wolfram leaves, FORM first normalises the stage factors into named local expressions and hides them from subsequent operations. Multiplication then reuses their already combined terms, avoiding repeated expansion of routed momentum sums. Hidden factors remain available until the program ends. The cutoff is a conservative heuristic; it was not tuned to an optimal size. Ambiguous index signatures and small jobs keep the shorter program. Hidden storage can use memory or scratch disk, so this is a speed/memory tradeoff. See [incremental factor-normalisation measurements](../Documentation/Verification/ConverterPerformance.md#normalizing-large-form-stage-factors).

This matters because a newly defined FORM expression starts as a single input term: its defining module cannot benefit from parallel distribution. See the [FORM manual's discussion of parallelization](https://form-dev.github.io/form-docs/master/manual/). Merely increasing the worker count for a single large definition can therefore add overhead. Staging makes parallel work possible but does not guarantee that more workers will always be faster.

Generated calculation programs suppress source echo with `#-`; errors and the dedicated result file remain available. The complete program is retained in `job.frm` when job files are kept.

Propagators are commuting scalar identifiers. Each mapping entry stores its original FeynCalc definition, routing, mass, dimension, unit power and Feynman prescription; repeated occurrences/powers retain multiplicity except where removed by algebraic numerator cancellation. Radicals and inverse composite scalar expressions are also reversible scalar abbreviations. The numerator-cancellation stage exposes their quadratic polynomials transiently and restores surviving denominator identifiers. The bounded scalar-coefficient procedure can expose supported reciprocal-polynomial abbreviations; radicals and other unsupported abbreviations remain opaque. Import restores surviving mapped objects exactly, although an equivalent product of denominators need not have the same grouping as the original combined `FAD`.

The converter inserts no symmetry factor, loop measure, on-shell condition or factor of `i`. Existing vertex factors of `i` and couplings are retained. The output is the algebraically contracted integrand, not an integrated self-energy.

The importer parses a restricted arithmetic grammar with native Lorentz tensors, validated epsilon/gamma/colour objects and `cfA0`, `cfB0`, `cfC0`, `cfD0`. It does not evaluate source text, assignments or arbitrary function calls from the result. Mapping expressions use a restricted JSON tree with whitelisted heads. Import does not reduce integrals. Processed colour jobs receive the documented scalar-only coefficient presentation after reconstruction.

## Troubleshooting

A kernel launched from a desktop may have a different `PATH` from your terminal; supply an absolute `FORMExecutable` path. On Unix, discovery skips files with no execute permission bits. Explicit paths are still probed so launch failures retain their diagnostics. If the file exists but the check reports `LaunchFailed`, inspect the returned messages and the operating system's execution restrictions. Reinstalling FORM does not fix a sandbox that prevents Mathematica from launching processes. If `ProbeFailed` or a calculation failure occurs, inspect standard output/error; calculation log files are retained in `JobDirectory`.

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

## Performance guidance

Use [Quick or Full benchmarks](../Benchmark/README.md) to measure export, execution, import and complete calculations on your machine. `ShowTiming` measures FORM execution; `AbsoluteTiming` around the public command measures the complete call. Preparation and validation should be outside isolated-stage timers.

TFORM helps only when the generated work can be distributed. Worker overhead, memory, sorting and file I/O can dominate; compare serial FORM with several worker counts rather than assuming eight is fastest. Export caches and the repeated-factor importer help repeated expressions but need memory proportional to the distinct stored objects. Unsupported fast-parser shapes fall back to the general parser.

Earlier improvements used different revisions, workloads and measurement boundaries. See the [performance history](../Documentation/Verification/ConverterPerformance.md) and the [3 October tensor-parser comparison](Tests/Reports/2026-10-03-tensor-fast-parser.md) for measured results and limitations. They are not portable speedup promises and are not added together.

### Memory when importing nested groups

For eligible grouped version-one results, the importer reads each nested coefficient into a restricted arithmetic tree and reconstructs its sums and products in batches, retaining factorisation. The syntax scan uses Mathematica’s built-in virtual machine; no compiler installation or external process is required. Unsupported shapes use the existing smaller-piece and general parsers. Decoded-factor caches remain bounded and local to the import. Source text, tokens and the tree for one coefficient need memory alongside the growing final Mathematica expression. This is not a disk-backed expression format. See the [coefficient-parser verification](Tests/Reports/CoefficientTreeImport.md) for measurements and limits.

For large calculations, suppress display inside the timer:

```mathematica
AbsoluteTiming[result = CalcFormCalculate[expression];]
```

This displays the elapsed time and `Null`, rather than formatting the complete result.


## Organisation and future reduction

- `CalcFormConverter.wl`: public interface and private implementation, organised into Mathematica initialisation cells. Shared specifications define supported heads, master functions and symbol categories. Export builds conversion data in memory, renders the program and mapping, then writes the files through separate private functions.
- `Templates/Program.frm.in`: declarations, factor definitions, target expression, processing boundary, dedicated result output and termination.
- [`Examples/ScalarBubble.wl`](Examples/ScalarBubble.wl): the supplied scalar-projected quadratic-gravity bubble, before symmetry factor and integration measure. Its workflow comments show manual export/import, automated calculation, TFORM, progress and timing. Load FeynGrav and the cubic library with `importQuadraticGravity[1]` before evaluating `ScalarBubbleExample`. Loading the example file only defines the example function; it does not execute FORM.
- `DiracColour.wl`, `DiracAlgebra.wl`, `ColourAlgebra.wl`: typed translation and independently selectable Dirac/colour processing, with companion templates.
- `ThirdParty/FORMColour/`: unchanged upstream procedure, attribution and licence; the adapted procedure is embedded in exported files.
- `FORMRuntime.wl`: executable discovery, arithmetic probe, streamed process logs, automated calculation and explicit installation. Loaded as definitions only in the same private context.
- `FORMAT.md`: version-one through version-five mapping schemas, result grammar and compatibility policy.
- `Tests/`: separate core, FORM round-trip, runtime and FeynGrav integration suites, with a development-only Python driver. Fixed version-one artifacts test compatibility with saved calculations.

Future FORM procedures can be inserted at the marked processing boundary without changing the export/import commands. Denominator algebra, repeated-propagator reduction, exceptional kinematics, integral normalisation, dimensional-regularization conventions and general one-loop tadpole/bubble/triangle/box reduction remain separate work. Master-integral names are already supported in both directions. Multiple loop momenta can already be recorded; no two-loop reduction is implemented.

## Development and tests

Run `python3 Tests/run.py --suite core` for conversion, parser, transaction and mocked installer tests without FORM. The default `python3 Tests/run.py` additionally runs FORM, runtime and FeynGrav integration suites with their dependencies. Tests do not install system packages.

See [DEVELOPER.md](DEVELOPER.md) for architecture, invariants, suite dependencies and measurement methodology. [FORMAT.md](FORMAT.md) is the persisted format contract; private helper associations are not public APIs.
