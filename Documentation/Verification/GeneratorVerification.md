# Library-generator migration verification

Date: 6 October 2026.

## Result and boundaries

The generator now uses CalcFormConverter for FORM translation, execution and
reconstruction. Stored libraries were not regenerated: SHA-256 comparison of
all 90 pre-existing files beside the generator confirmed no changes.
No Full benchmark suite was run and no commit was created.

The old gamma/colour/Greek-name text transformations and shell process layer
were removed. Library output is staged, read back and published with recovery.
`ColourAlgebra -> True` is the default; `Automatic` remains equivalent and
version-five mapping metadata is unchanged.

## Family calculations

All calculations below were generated in a temporary directory using real
serial FORM 4.3. Independent FeynCalc comparisons reduced the residuals to
symbolic zero, including colour comparison in a common trace/rank basis and
momentum expansion where required. Equality was not inferred from numerical
samples or an unresolved residual.

| Library family | Parameters | Generation | Independent comparison |
|---|---|---|---|
| GravitonScalarVertex | `[1]` | Passed | Zero residual |
| GravitonScalarPotentialVertex | `[1]` | Passed | Zero residual |
| GravitonFermionVertex | `[1]` | Passed | Zero residual |
| GravitonMassiveVectorVertex | `[1]` | Passed | Zero residual |
| GravitonVectorVertex | `[1]` | Passed | Zero residual |
| GravitonVectorGhostVertex | `[1]` | Passed | Zero residual |
| GravitonQuarkGluonVertex | `[1]` | Passed | Zero residual |
| GravitonGluonVertex | `[1]` | Passed | Zero residual |
| GravitonThreeGluonVertex | `[1]` | Passed | Zero residual |
| GravitonFourGluonVertex | `[1]` | Passed | Zero residual |
| GravitonYMGhostVertex | `[1]` | Passed | Zero residual |
| GravitonGluonGhostVertex | `[1]` | Passed | Zero residual |
| GravitonVertex | `[1]` | Passed | Zero residual |
| HorndeskiG2 | `[1, 1, 1]` | Passed | Zero residual |
| HorndeskiG3 | `[2, 0, 1]` | Passed | Zero residual |
| HorndeskiG4 | `[2, 0, 1]` | Passed | Zero residual |
| HorndeskiG5 | `[2, 0, 1]` | Passed | Zero residual |
| ScalarGaussBonnet | `[2]` | Passed | Zero residual |
| GravitonAxionVectorVertex | `[1]` | Passed | Zero residual |
| QuadraticGravityVertex | `[1]` | Passed | Zero residual |

Parameters are `{n}` except Horndeski `{a,b,n}`. Pure and quadratic gravity use
`n+2` external gravitons. Gauss–Bonnet uses two, the leading possible order of its flat-background
curvature-squared expansion.

Nineteen corresponding stored libraries also gave zero symbolic residuals
against the temporary results. The axion library was excluded from that legacy
comparison because its dimensional epsilon convention was deliberately
corrected; its new result was compared directly with the corrected rule.

An isolated package tree containing the temporary libraries passed **29 checks**:
main-package initialisation, additional-family imports, and all 20 public vertex
calls with renamed symbols and composite momentum arguments. The actual FeynGrav
importers were used; their code was not changed.

## Execution, conventions and failures

**22 checks** passed for serial FORM, automatic selection, two-worker TFORM,
an explicit executable path containing spaces, cross-engine agreement, timeout
retention, and the axion rule under all four `$LeviCivitaSign` values on FORM
and TFORM. Axion slot permutations and the unchanged physical prefactor were
included. A real timeout retained the old library and the converter job logs.

**27 generator failure/loading checks** passed. They cover invalid arguments/options, incoming
failures, `$Failed`, aborts and returned `$Aborted`, construction/calculation
failures, undeclared symbols, write/readback failures, publication rollback,
staging cleanup, ordered batch stopping, retry, legacy executable precedence,
unchanged directories/options, process-free loading and replacement of obsolete
more-specific definitions on reload. The [machine-readable observations](GeneratorVerification.json) retain the
counts and family results. Tests used temporary files and injected failures; no package
installation was attempted.

## Existing regression suites

| Suite | Passed assertions |
|---|---:|
| Core | 111 |
| Parser | 121 |
| Export transactions | 20 |
| Installer decisions, mocked | 62 |
| FORM export | 31 |
| FORM stages | 128 |
| FORM import, including fixed version-one files | 65 |
| Runtime | 82 |
| Epsilon | 48 |
| Dirac/colour translation | 65 |
| Dirac algebra | 104 |
| Colour algebra, including True/Automatic equivalence | 139 |
| Dirac/colour rule comparisons | 23 |
| Processed colour rule comparisons | 23 |
| **Total** | **1,022** |

Automatic loading and namespace-isolation checks also passed. The rule suites
cover zero, one and two gravitons and now exercise the dimensional quark rule
directly. Their successful export fixtures have distinct names so both suites
can run in the same test directory.

## Gauss–Bonnet batch starting order — corrected

Around flat space each curvature begins at first order in the graviton
perturbation. The curvature-squared Gauss–Bonnet interaction therefore has no
one-graviton contribution. The batch now generates orders `2` through `n`;
`n = 1` returns `Null` without rule construction, FORM execution or file writes.
The specific-generation command retains its requirement `n >= 2`.

No higher-order correctness or performance claim is made from these bounded
checks. Supported low-order comparisons all passed; there are no unresolved
comparison residuals in the delivered family table.

## Evidence and reproduction

Temporary verification scripts, JSON observations and logs were retained at
`/tmp/feyngrav-generator-migration` during development. The scripts are
`families.wls`, `baseline.wls`, `imports.wls`, `faults.wls` and `engines.wls`.
Family comparisons use `Contract`, `EpsEvaluate`, `SUNSimplify` in a common
symbolic-N basis, `DiracSimplify`, momentum expansion and `Simplify` outside
calculation timing, with explicit comparison time limits. Failed initial test
iterations were corrected and rerun; only final passing results are counted.

Existing suites can be rerun from the repository with
`python3 CalcFormConverter/Tests/run.py --suite core`, `--suite form` and
`--suite runtime`. The two rule suites and Loading.wls were run separately in
fresh kernels with a temporary `CFC_TEST_DIR`; the Full benchmarks and the
large integration bubble were not rerun.

## Follow-up: five generator robustness fixes

The follow-up fixes reject assigned source/destination placeholders before
construction, honour per-command `SetOptions` and nested option lists, avoid
creating duplicate legacy configuration symbols, propagate publication status
into batch completion reports, and restrict inventory to regular files with
canonical family-specific names.

- 24 focused checks passed, including real FORM generation after rejected
  assigned symbols and an injected backup-deletion failure after installation.
- All 20 representative family generations and independent comparisons passed
  again with the new symbol validation.
- The existing 27 generator failure/loading/reload checks passed again.
- Fresh loading with no legacy configuration and with preconfigured Global
  settings produced no shadowing warnings; the latter retained its executable
  setting and suppressed the startup message as requested.
- The 90 stored library files remain unchanged. The Gauss–Bonnet starting-order correction was made separately afterwards.

Focused test observations are in `/tmp/generator-five-tests.json`; the temporary
scripts and final logs are `/tmp/generator-five-tests.wls`,
`/tmp/generator-final-faults.wls`, `/tmp/generator-five-families.log` and
`/tmp/generator-legacy-loading.log`. Converter algebra was not changed in this
follow-up; its previously reported regression results were not re-counted as
new tests.

## Gauss–Bonnet batch correction checks

Seven focused checks passed: an empty order-one batch with no calculation or
process launch and no files; exact enumeration of orders two through four;
unchanged scalar-family enumeration; rejection of zero; the specific command's
existing one-graviton validation; real serial FORM generation at order two; and
exact agreement with the previously verified two-graviton expression. Outputs
were confined to a temporary directory. Evidence: `/tmp/test-gauss-batch.wls`,
`/tmp/test-gauss-batch.log` and `/tmp/test-gauss-batch.json`.

## Function help and startup text review

The descriptions now state each family's generated files, accepted parameters,
order ranges, options and failure behaviour. The introduction gives the current
converter workflow, destination and FORM/TFORM defaults.

Eight fresh-kernel checks passed, covering all 24 generation descriptions, all
12 inventory descriptions, gravity and Gauss–Bonnet order guidance, option
availability, startup text, process-free loading and preservation of `Directory[]`.
A separate fresh kernel confirmed startup suppression and the legacy executable
setting. No algebra or generation behaviour changed; mathematical regressions
were not repeated for this documentation-only update.
