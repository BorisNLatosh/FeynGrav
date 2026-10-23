# Development note: unified public calculation interface (API-01)

**Status: unreleased development work. Not part of FeynGrav 4.1.0.** This note
records additions made on the `FORM_Simplification` development branch. It does
not change the published 4.1.0 interface, notes, tag or GitHub Release, and it
assigns no version number. Do not cite or announce these commands as released
functionality. The implementation was accepted on 20 October 2026 within the recorded
verification scope. The 23 October development checkpoint includes the subsequent
modularisation, reliability and example changes; it remains unreleased.

## What the branch adds

`CalcFormConverter` gains five short public commands, implemented in
`FeynGravSimplify/` and loaded once by the converter:

| Command | Purpose |
| --- | --- |
| `FeynGravSimplify[expr, opts]` | One-call calculation: full converter pipeline or restricted external adapter, selected once by `Method`, with no fallback. |
| `FeynGravFORMExport[expr, file, opts]` | Wrapper over `CalcFormExport` with its own option store. |
| `FeynGravFORMImport[resultFile, mappingFile]` | Wrapper over `CalcFormImport`; exactly two arguments. |
| `FeynGravFORMCheck[opts]` | Wrapper over `CalcFormCheck`. |
| `FeynGravFORMInstall[opts]` | Wrapper over `CalcFormInstall`. |

The restricted adapter is the unpublished donor simplification implementation
extracted from branch `Contraction_Integration_and_Benchmarking` at commit
`2f7f165` and adapted to the facade; see
[the extraction verification note](Verification/SimplificationExtraction-2026-10-20.md)
for file identities and the modification register. `CalcFormCalculate` and every
`CalcForm*` command are unchanged and remain the published interface.

## Method selection

The facade uses the standard ``System`Method`` option:

- `Automatic` (default) and `"FORM"` dispatch to the full converter pipeline
  (`CalcFormCalculate` semantics). Converter options are resolved once per call
  and forwarded as ordinary rules; no `Method` rule reaches `CalcFormCalculate`,
  and Contractor settings are neither read nor probed on this path.
- `"Contractor"` and `"FORMBenchmark"` dispatch to the restricted external
  adapter. `"Contractor"` selects the Contractor executable
  (`ContractorExecutable`, `ContractorThreads`, both in ``FeynGrav` ``);
  `"FORMBenchmark"` selects the restricted FORM comparison method
  (`FORMExecutable`, `FORMThreads`). There is no automatic backend fallback.

Restricted methods accept only the fixed capability baseline for the
full-pipeline settings `LoopMomenta`, `TensorDecomposition`,
`ReduceLoopCoefficients`, `ReduceToScalarMasters`, `DropScaleless`,
`DiracAlgebra`, `ColourAlgebra`, `Dimension` and `WorkingDirectory`. Any other
resolved value returns `MethodCapability` listing every deviation
(`Setting`, `RequestedValue`, `RestrictedBaseline`) without starting a backend;
the failure directs the caller to `Method -> "FORM"`. Only the selected
backend's executable/worker pair is resolved and value-validated; the unselected
pair is not resolved or forwarded by the facade. Its delayed values (`:>`) remain
inactive; immediate rules (`->`) retain ordinary Wolfram evaluation before the
call. On the full pipeline the Contractor pair is not resolved or forwarded;
`FORMExecutable`/`FORMThreads` are resolved and forwarded to the pipeline.

## Options and stores

`FeynGravSimplify` has one union option store containing the restricted and
full-pipeline names, with defaults `Method -> Automatic`,
`ContractorExecutable -> Automatic`, `ContractorThreads -> 8`,
`FORMExecutable -> Automatic`, `FORMThreads -> Automatic`,
`TimeConstraint -> Infinity`, `KeepFiles -> False`, `ShowProgress -> False`,
`ShowTiming -> False`, `WorkingDirectory -> Automatic`,
`TensorDecomposition -> True`, `ReduceLoopCoefficients -> False`,
`ReduceToScalarMasters -> True`, `DropScaleless -> False`,
`DiracAlgebra -> Automatic`, `ColourAlgebra -> True`,
`Dimension -> Automatic`, `LoopMomenta -> {}`. The four wrapper commands own
independent literal stores mirroring their `CalcForm*` owners.

Explicit call options take precedence over stored defaults. Because the Wolfram
evaluator resolves repeated option names first-wins, a value forced by a
wrapper cannot be overridden by an argument of the same name in the same call.
Delayed option values (`opts :> value`) are evaluated once per option read and
then forwarded as ordinary resolved rules. Supplied option names and stored
option names are validated against the documented sets before any process work;
unknown names, including names introduced by a direct `Options[...]`
assignment, return `InvalidOption`. `SetOptions` affects only the target
command, and module loading cannot reset configured stores: the converter loads
the `FeynGravSimplify/` modules behind a successful-load guard, so a later
``Needs["CalcFormConverter`"]`` does not re-run them. A standalone converter load
declares the facade symbols and the ``FeynGrav`ContractorExecutable`` and
``FeynGrav`ContractorThreads`` usage strings without starting processes, loading
libraries or probing executables.

## Migration from the unpublished donor interface

The donor interface existed only on the unpublished development branch; this is
therefore a documented development migration, not a released-interface break:

- ``FeynGrav`FeynGravSimplify`` becomes ``CalcFormConverter`FeynGravSimplify``.
- The donor default `Method -> Automatic` selected Contractor; on the facade,
  `Automatic` selects the full converter pipeline.
- The donor restricted `Method -> "FORM"` becomes `Method -> "FORMBenchmark"`;
  `Method -> "FORM"` now selects the full pipeline.
- ``FeynGrav`ContractorExecutable`` and ``FeynGrav`ContractorThreads`` keep their
  context and identities.

## Verification status and limits

The follow-up maintainability changes were accepted on 20 October 2026.
[Their verification record](Verification/SimplificationMaintainability-2026-10-20.md)
documents 78 passing composed-batch checks, the retained diagnostic limitations
and a subsequent static test-variable rename. This does not constitute a full
suite rerun or a release.


Focused suites under `FeynGravSimplify/Tests/` ran in fresh kernels under the
task-owned bounded runner (1 GiB whole-tree memory cap, no swap, 120 s plus
10 s termination grace, sequential launches). Final counts and the exact run
directories are recorded in
[the extraction verification note](Verification/SimplificationExtraction-2026-10-20.md).
The restricted Contractor protocol was exercised with a Go-built process
double; no real Contractor binary is installed on the verification host. The
restricted FORM method and the full pipeline ran real FORM/TFORM. Runs of the
FORM and Integration suites carry `Unset::norep` diagnostics from Wolfram
ProcessLink reaping finished full-pipeline children. A baseline control using
the unchanged legacy converter from `fa4fe35` reproduced the same diagnostics,
so this is recorded as an **existing limitation of the branch's `CalcForm*`
process hygiene**, retained at acceptance, and not as a facade or
adapter defect. Supported loop fixtures produce the converter's expected
`CalcFormImport::partialmasters` diagnostics because their coefficients remain
formal; such runs are partial-reduction results, not clean full-reduction
claims.

The final targeted batch passed 43 substantive checks. Its six automated
message-audit checks were ineffective; the retained log was reviewed manually.
Harness warnings and expected partial-reduction notices remain recorded. This
is not a warning-free run or a complete final-source rerun of every suite.

Not established by this work: non-Linux behaviour, front-end rendering,
performance or benchmark claims, loop-level certification, library or model
regeneration, and any change to published commands. A future release must fold
this note into its release notes, decide the version under the project policy
and obtain separate authorisation.

## Development checkpoint — 23 October 2026

The converter now separates mapping/encoding, export assembly and import
reconstruction. The facade and restricted adapters live in `FeynGravSimplify/`.
Focused changes cover companion readiness/retry, independent option stores,
abort/status reporting, installer readability and dependency inventory, and the
read-only conventions report. See the dated verification reports for the exact
snapshots, diagnostics and omitted coverage; this checkpoint does not rerun or
certify the full regression suite.

Examples 01–08 use FeynGravSimplify while preserving manual edits and saved
outputs. Example 07 includes the tadpole-dependent two-point Ward identities.
Example 08 uses the native minimally coupled scalar model with potential
couplings zero, corrects the identical-graviton bubble weight to one half and
explains the omitted scaleless higher-order contact. Its later notebook-box
repair has a held syntax check; the downstream loop analysis was not rerun.
A comprehensive syntax/interface audit of all 11 notebooks remains pending.

Scalar-master export has a reproduced performance limitation: coefficient
registration repeatedly decodes and searches existing entries, and route pooling
prepares unused denominator families. No fix is included here. Disabling master
reduction can produce coefficient functions promptly, but is not equivalent to
completing scalar-master reduction. Later master-reduction runtime remains to be
measured after correcting export. No interaction rules, stored libraries or
native model bundles are changed by this checkpoint.
