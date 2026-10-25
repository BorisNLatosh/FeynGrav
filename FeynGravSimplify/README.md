# FeynGravSimplify: unified facade over the converter pipeline and Contractor-backed restricted methods

``CalcFormConverter`FeynGravSimplify[expr]`` is the unified front end of this
directory, planned for [FeynGrav 4.2.0](../Documentation/ReleaseNotes-4.2.0.md)
(currently unreleased). `Method -> Automatic` (the default) and
`Method -> "FORM"` resolve the call through the full `CalcFormConverter`
pipeline with `CalcFormCalculate` semantics: resolved converter options are
forwarded as ordinary rules and no `Method` option reaches the pipeline.
`Method -> "Contractor"` and `Method -> "FORMBenchmark"` select the restricted
external adapter documented on this page, whose FORM variant is described in
[FORM.md](FORM.md). `CalcFormCalculate` remains independent and unchanged;
there is no automatic backend fallback.

This README primarily documents the **restricted adapter** itself
(`Contractor` and `FORMBenchmark` methods): its input domain, options, process
I/O and runtime limitations are unchanged from the extracted implementation.
Facade-level option stores, validation and dispatch live in
`FeynGravSimplify/Facade.wl`.

All explicit contraction, expansion and coefficient collection take place in
the selected external backend. The adapter performs structural translation, validation and process
I/O only. Mathematica still evaluates the input normally before the call and
canonicalises reconstructed `Plus`, `Times`, numbers and FeynCalc objects when
they return; it performs no additional simplification pass.

## Module map and option stores

`FeynGravSimplify/` is loaded by `CalcFormConverter.wl` in this order:
`RestrictedCodec.wl`, `Contractor.wl`, `FORM.wl`, `RestrictedCoordination.wl`,
`Facade.wl`. The table states ownership and the private operations each module
consumes; most adapter internals are inherited from the donor branch.

| File | Owns | Consumes |
| --- | --- | --- |
| `RestrictedCodec.wl` | Restricted validation, expression-to-JSON encoding/decoding and the shared `failure`/`reject` helpers | No other Simplification module |
| `Contractor.wl` | Shared PATH/executable-file helpers, Contractor selection policy and `contractorPreparation` (command construction) | Shared `failure` from the codec |
| `FORM.wl` | Restricted FORM program generation, configuration and decoding helpers | Codec `failure`, `reject` and `decodeTree`; `findOnPath` and `executableFileQ` from `Contractor.wl` |
| `RestrictedCoordination.wl` | `simplify` job lifecycle, `formPreparation`, the native `runJob` process runner, retention messages and `reportProgress` | Codec, Contractor preparation and FORM helpers; `$progressIntervalSeconds`; `$showProgress`/`$backendLabel` set by the facade |
| `Facade.wl` | The five public facade commands with four independent option stores; `FeynGravFORMImport` has no options; `facadeReadyQ[]` owns the parent loader's readiness inventory | Codec `failure`; coordination `simplify`/`reportProgress`; sets `$showProgress` and `$backendLabel` around one restricted call |

The facade's option declarations are immutable ordered defaults plus derived name
whitelists. The export wrapper's whitelist is derived even though its historical
explicit list had a different order: membership is all the validator uses and no
`SupportedOptions` payload is emitted for that wrapper, so the order is not
observable. The public `Options[...]` stores remain independent and mutable
through `SetOptions`; validation always uses the immutable names so injected
unknown keys are rejected.

## Input domain

Supported input consists of exact integers, rationals and complex numbers,
symbolic scalar coefficients, `Plus`, `Times`, scalar powers, and Lorentz
`Pair` objects. `MT`, `MTD`, `FV`, `FVD`, `SP` and `SPD` shorthand is translated
structurally. Positive int32 integer powers of tensor expressions are supported;
their expansion is left to Contractor. Momentum sources can contain commuting
arithmetic in symbols and exact numbers and are preserved without expansion.
Polarization-vector momentum sources such as `Momentum[Polarization[p,I,...],D]`
are not supported yet. In particular, `PolarizationTensorD` produces these
objects and is rejected before Contractor starts; the failure includes the
held offending momentum source.
Index labels must be symbols. Use uniform four-dimensional or symbolic `D`
objects. Mixed dimensions and evanescent dimensions are rejected.

Ordinary `FAD[p]`, `FAD[{p,m}]` and multi-propagator equivalents are supported,
as are internal `FeynAmpDenominator[PropagatorDenominator[...], ...]` products.
They are carried as scalar atoms, with no propagator cancellation. Shorthand
routing such as `p-q` is retained inside `Momentum[p-q,D]`; it is not expanded
into separate Momentum objects. FAD options and three-element FAD specifications
are not supported in this first adapter.

The opaque scalar-function whitelist is `Sin`, `Cos`, `Tan`, `Exp`, `Log`,
`Abs`, `Conjugate`, `A0`, `A00`, `B0`, `B1`, `B00`, `B11`, `C0`, and `D0`.
Their arguments must themselves be supported scalars. Unknown function heads,
Dirac and colour objects, epsilon tensors, noncommutative chains, tensor-valued
function arguments, equations, containers and approximate numbers are rejected.
An unevaluated unsupported tensor is never silently treated as a scalar.

Contractor does not supply general rational simplification or expansion of
composite momenta. Its bounded coefficient arithmetic can leave overflowed
operations symbolic for ordinary Mathematica reconstruction. Returning a valid
expression does not assert that it is in a unique or maximally simplified form.

## Options

| Option | Default | Meaning |
| --- | --- | --- |
| `Method` | `Automatic` | Full-pipeline dispatch through `CalcFormConverter` (`Automatic` or `"FORM"`); `"Contractor"` and `"FORMBenchmark"` select the restricted adapter methods. No fallback. |
| `ContractorExecutable` | `Automatic` | Contractor method only. Prefer sibling `contractor/bin/contractor`, then search the kernel PATH; an explicit path/name never falls back. Inert on full-pipeline and FORM methods. |
| `ContractorThreads` | `8` | Contractor method only: number of parallel contraction workers, passed as `-threads n`. `Automatic` leaves Contractor's default unchanged. This is a worker count, not a total CPU cap. Inert otherwise. |
| `FORMExecutable` | `Automatic` | Full-pipeline and restricted-FORM methods: discover `form` for one worker or `tform` for multiple workers on PATH; explicit paths/names never fall back. |
| `FORMThreads` | `Automatic` | Full-pipeline default. Restricted `FORMBenchmark` worker count: `1` selects serial FORM; larger values require TFORM; `Automatic` selects up to eight according to the kernel processor count (one if unavailable). No silent serial fallback. |
| `TimeConstraint` | `Infinity` | Best-effort limit for native process execution/communication, not encoding or final reconstruction; see the pipe limitation below. The full-pipeline default follows `CalcFormCalculate`. |
| `ShowProgress` | `False` | Print adapter stages, elapsed execution time approximately every ten seconds while waiting, and completion/failure status; no percentage estimate. |
| `ShowTiming` | `False` | Print process wall-clock time and total wall-clock time (including conversion and cleanup). Early failures report total time only. Input evaluation before entering the function is excluded. |
| `KeepFiles` | `False` | Retain successful jobs when true; failures retain their diagnostics regardless. On the full pipeline this is the `CalcFormCalculate` option. |

The command's public option store contains the writable subset of these names
plus `Method`; it accepts restricted and full-pipeline options together,
validates keys on every call, and treats non-restricted-value requests through
the capability check described in the converter guide's
[simplified front end section](../CalcFormConverter/UserGuide.md#simplified-front-end-development).
Only the selected restricted backend's executable/worker pair is resolved and
validated; the facade does not resolve or forward the unselected pair. Delayed
values (`:>`) for that pair remain inactive. Immediate rules (`->`) retain
ordinary Wolfram evaluation before the call. Unknown option names still fail
on every method.
`ContractorExecutable` and `ContractorThreads` belong to the ``FeynGrav` `` context.
`Method`, `FORMExecutable`, `FORMThreads`, `KeepFiles`, `ShowProgress`,
`ShowTiming`, `TimeConstraint`, `Dimension`, `LoopMomenta`,
`DropScaleless`, `DiracAlgebra`, `ColourAlgebra`, `TensorDecomposition`,
`ReduceLoopCoefficients`, `ReduceToScalarMasters` and `WorkingDirectory` use
the public option symbols in the ``CalcFormConverter` ``, ``FeynCalc` `` and
``System` `` contexts; this does not call or depend on converter implementation helpers.

### Full versus restricted executable policy

| Path | Executable selection | Worker/thread choice | Fallback and inactive settings |
| --- | --- | --- | --- |
| Full pipeline (`Method -> Automatic` or `"FORM"`) | `CalcFormCalculate` uses `CalcFormCheck`/`selectFORMConfiguration`. `FORMExecutable -> Automatic` searches for `form` or `tform`; an explicit name/path is used as selected | `FORMThreads -> Automatic` prefers TFORM with up to eight workers when it is available; serial `form` is the fallback only when automatic TFORM discovery fails. Explicit worker counts above one require TFORM. An explicit executable with automatic threads runs one worker. | No fallback to another method. Contractor settings are not resolved or forwarded. |
| Restricted `"FORMBenchmark"` | `formConfiguration` searches for `form` for one worker or `tform` above one; an explicit name/path is used as selected | `FORMThreads -> Automatic` selects up to eight workers from the kernel processor count. One worker runs serial `form`; larger counts require `tform`; the generated program checks the requested count. | No serial fallback when automatic `tform` discovery fails. No fallback to the full pipeline or Contractor. Contractor settings are not resolved or forwarded. |
| Restricted `"Contractor"` | `ContractorExecutable -> Automatic` prefers the sibling `contractor/bin/contractor`, then searches PATH. An explicit name/path never falls back. | `ContractorThreads -> 8` is the public default; an explicit count is passed as `-threads n`. `Automatic` leaves Contractor's own default in place. | No fallback to the full pipeline or FORM. FORM settings are not resolved or forwarded. |

These policies remain separate. The restricted backends share the basic
`findOnPath` and `executableFileQ` helpers from `Contractor.wl`; their selection
and worker policies are distinct from each other and from the full pipeline.
The facade does not convert one policy into another. In
particular, an explicit executable with `FORMThreads -> Automatic` means one
worker on the full pipeline but keeps the automatic processor-count choice on
`"FORMBenchmark"`; the restricted generated program then checks that count, so a
serial executable cannot silently impersonate multiple workers.

Elapsed progress time starts after process launch and includes input transmission.
Progress updates require the native I/O loop to regain control; blocked writes
can defer them. `ShowTiming` is independent and need not be enabled.

Each job has a unique temporary directory. A retained job contains the request,
separate result and stderr files and process diagnostics. Failures identify the
stage and available executable, exit status and retained paths. User aborts
propagate after process cleanup. Successful calls return only the expression.
No executable is probed, built, installed or launched when FeynGrav loads.

Process lifecycle is deliberately split between two owners. `runJob` owns one
`ProcessObject` and its stdin/stdout/stderr and log streams; it registers the
process before enabling interruption, and its cleanup stops only that process,
drains readable output and closes every stream on completion, timeout or abort.
`simplify` separately owns the job directory: failures and `KeepFiles` jobs are
retained for diagnosis, while successful ordinary jobs are removed after
decoding. Cleanup failures are reported and the directory is retained. Only the
started process is signalled; unrelated kernels and processes are out of scope.

## Native process I/O limitation

The shared runtime uses Mathematica's `StartProcess`, `ProcessConnection`, `WriteString`
and `ReadString` directly; no Python helper or additional runtime is required.
It captures stdout and stderr separately and checks the process exit status.

Contractor requests are transmitted on stdin. The FORM method writes its program
to the job directory, closes stdin, and reads a separately framed result file.

A native `WriteString` blocked on a non-reading subprocess can defer Mathematica
interrupts, including `TimeConstraint` and user abort, until that write returns.
Consequently this is not a hard wall-clock cancellation guarantee. Once control
returns, the adapter terminates its owned process and closes its streams. This
known limitation is accepted for this implementation; no external watchdog is used.

## Example

```mathematica
<< FeynGrav`
FeynGravSimplify[MTD[mu,nu] FVD[p,mu] FVD[q,nu],
    Method -> "Contractor", ShowProgress -> True, ShowTiming -> True, TimeConstraint -> 30]
(* Pair[Momentum[p,D], Momentum[q,D]] *)
```

Use `KeepFiles -> True` when diagnosing a calculation. The command prints the
retained directory. These files contain the symbolic calculation supplied by
the caller and should be retained or shared deliberately.

## Compatibility and tests

The initial compatibility target is Contractor 0.1.0, source revision
`641febdaeb6b30835a5f7f846b9a26198ac20313` (`scalar-handling`). Executable discovery
does not infer a binary's revision from the surrounding checkout. For reproducible
calculations select the intended build with `ContractorExecutable`; retained
`process.json` files include its SHA-256 hash. Earlier builds may implement less
scalar algebra. See [verification](Verification.md) for the tested binaries.

Run the focused checks from fresh kernels. The verified record for this
branch used a task-owned bounded runner (1 GiB whole-tree memory cap, no
swap, 120 s + 10 s grace; its location is recorded in
[the extraction verification note](../Documentation/Verification/SimplificationExtraction-2026-10-20.md)).
Suite selection and resource enforcement are separate: the converter test driver
selects scripts, while the external runner supplies the limits. The documented
focused entry point is:

```sh
FG_CONTRACTOR_TEST_FIXTURE=/absolute/path/to/fixture-process \
  python3 CalcFormConverter/Tests/run.py --suite facade
```

It runs `Adapter`, `Loading`, `Standalone`, `Reliability` and `Facade` in fresh
kernels by default and requires `WolframKernel`, FeynCalc and FORM. It does not
require a real Contractor binary, does not install dependencies and is not
included in the default converter selection. The focused two-kernel bounded
check selects phases explicitly:

```sh
FG_CONTRACTOR_TEST_FIXTURE=/absolute/path/to/fixture-process \
  python3 CalcFormConverter/Tests/run.py --suite facade \
    --facade-phase Loading --facade-phase Facade
```

Each selected phase writes `suite-completed-<Phase>.json`; the driver requires
exit 0 plus a fresh matching record with positive passing and zero failed
assertions. Add `--evidence-dir <directory>` to retain per-phase logs and raw
completion records outside the temporary work directory. The selector and completion protocol passed the final Stage E Loading/Facade
check (151/151 assertions). The other three completion writers were not rerun
in that final batch. See the [Stage E verification report](../Documentation/Verification/Arch01StageE-2026-10-21.md). Direct developer invocation of one suite without the wrapper remains:

```sh
WolframKernel -noinit -noprompt -script FeynGravSimplify/Tests/<Suite>.wls
```

The focused contract suite is `FeynGravSimplify/Tests/Facade.wls`. It requires
`FG_CONTRACTOR_TEST_FIXTURE=/absolute/path/to/fixture-process` and aborts early
with a `FAIL-FAST` message if the facade definitions, stores or fixture are
missing. It covers supported successes, failure parity, option and refusal
contracts, error-data serialization against accepted literals and the single
terminal report. `Reliability.wls` covers fresh-kernel recovery, marker and
store preservation; `Loading.wls` covers installer layout and isolates the
implementation-specific loader scratch-state assertions. `Standalone.wls`
checks standalone loading, the four option stores and store isolation without
loading the main package. Loop fixtures in these suites produce expected
`CalcFormImport::partialmasters` diagnostics; they are partial-reduction
results, not full-reduction claims.

The integration and FORM suites additionally need a Contractor or FORM
executable and a Go-built **test double**, not an additional production
dependency:

```sh
go build -o /tmp/feyngrav-process-fixture FeynGravSimplify/Tests/ProcessFixture.go
FG_CONTRACTOR_TEST_BINARY=/absolute/path/to/contractor \
FG_CONTRACTOR_TEST_FIXTURE=/tmp/feyngrav-process-fixture \
WolframKernel -noinit -noprompt -script FeynGravSimplify/Tests/Integration.wls
```

The process-cleanup assertions in the integration suite use Linux `/proc`.
All runtime verification for this implementation was performed on Linux;
other operating systems have not been verified. No non-reading-process hard
cancellation test is claimed: the native pipe limitation above is intentionally
outside the accepted runtime guarantee.

The focused restricted-FORM suite is `FeynGravSimplify/Tests/FORM.wls`; it also
requires the test fixture environment variable shown above. See [FORM.md](FORM.md)
for the comparison contract and the new verification record.
