# Scalar-master output sharing — 23 October 2026

## Scope and result

Uncommitted work on `FORM_Simplification`, HEAD
`f30dd8e3d58e8b3ffdb801eed9836caee646fc5b`, including the preceding
[export-planning fixes](ExportPlanning-2026-10-23.md).

The reported three generic off-shell rank-four boxes now complete export, FORM
processing and import within the existing resource limits. Output is 4,364,149
bytes, compared with the retained 236,271,618-byte direct output: **98.15% smaller**.
All 17 master coefficients agree exactly with that retained output, with symbolic
D and no imposed external kinematics. This establishes preservation of the
previous result; it is not an independent derivation of that complete amplitude.

No commit, interaction-rule edit, library regeneration, model rebuild or notebook
edit was performed. Existing uncommitted work, including new library files,
was preserved.

## Narrow implementation

`lrSharedPreparation` in `LoopCoefficientReduction.wl` now selects the existing
shared/factored assembly for every nonzero eligible scalar-master job. Previously
it required more than 4,096 terms before reduction, missing small dictionaries
whose reduced rational coefficients became large. Repeated-power-only jobs keep
that threshold. Nonlinear products of coefficients and algebra eligibility keep
their existing guards.

The existing `LoopDenominatorGroups.frm.in` already interns polynomial factors,
keeps amplitude blocks and recurrence weights separate until assembly, collects
each master and its complete denominator products, and extracts cheap common
factors. Selecting this path provides both polynomial sharing and factored
assembly. The template itself, recurrence identities, mapping versions, parser,
normalisation, dimensions and exceptional-case rules are unchanged. No public
option, mathematical head or global factorisation pass was introduced.

The new output has 2,366 shared definitions and 17 master groups. Some polynomials
remain inside larger definitions; this work does not claim maximal compression.
The final result uses ordinary Wolfram arithmetic and scalar PaVe objects.

## Correctness checks

| Check | Result | Boundary |
| --- | --- | --- |
| `Tests/MasterOutputSharing.wls` | **48/48 exact assertions** | Shared versus old direct selector for symbolic bubble, rank-four box, triangle/bubble combination and repeated-power coefficient; identical mappings, expected framing, zero result, unchanged eligibility and power-only threshold |
| Existing `ScalarMasterReduction.wls` | **91/91 assertions** | Independent FeynCalc comparisons on manageable cases through rank six, symbolic-D normalisation, scaleless/exceptional cases, saved coefficients, serial/TFORM, tiny chunks, malformed metadata and matrix/epsilon spectators |
| Complete three-box output | **17/17 exact zero residuals** | All final masters compared in FORM; scalar-product invariants and D remain symbolic |
| Deliberately modified output | **Nonzero residual −1** | Comparison detects the added constant |
| Small bubble performance control | **6/6 exact equalities** | Three measured calls per path following one warmup per path |
| Full legacy/core/runtime suites; notebook recalculations | **Not tested in this task** | No exhaustive suite or Full benchmark claim |
| New numerical parameter-integral comparisons | **Not tested** | Production mathematical algorithms did not change |

The scalar-master suite retains its known partial-reduction message for the
exactly singular saved triangle `PaVe[0,0,{0,0,0},{a,a,a},...]`. Its per-call log
is retained; the coefficient remains unchanged. No unexpected Wolfram diagnostic
or FORM failure occurred in the successful production/regression runs.

### Full-output comparison method

The new and old mapping JSON files are byte-identical. The audit extracts each
master group, expands only shared references and replaces identical polynomial
arguments on both sides with the same opaque atoms. It preserves the relation
between numerator atoms and their reciprocals, restores the unmatched explicit
factor D−2, and combines D-dependent fractions exactly using FORM's
`Denominators` and `PolyRatFun` facilities. All 17 residuals vanish before the
remaining polynomial atoms need expansion. Thus no numerical specialisation or
new kinematic identity is used. These facilities follow the
[FORM manual, sections 7.33 and 7.114](https://form-dev.github.io/form-docs/stable/manual/).

Four preliminary comparison-harness attempts were unsuccessful: the first
missed nested polynomial calls and used invalid `#terminate` syntax; later
attempts still lacked the explicit D−2 relation, reciprocal cancellation, or
rational combination of D-dependent denominators. A bounded residual inspection
identified these representation differences. All scripts and logs are preserved.
The final `comparison-v5/compare.frm` and `run-exact-comparison-v5` are the passing
comparison, including the mutation control. None of these harness corrections
changed the package implementation or mathematical output.

## Performance

Three sequential fresh-kernel trials of the complete manual export–execute–import
pipeline, without an in-kernel warmup. Timers exclude package/kernel startup and
subsequent WXF evidence serialisation. FORM is serial version 5.0.2; Wolfram 15.0
and FeynCalc 10.2.1 are the local dependencies. These are three cold-kernel trials,
not three cold filesystem-cache trials. Background load and power state were not
controlled. Output files and serialised results have identical SHA-256 hashes
across all three trials.

| Stage | Mean seconds | Median seconds | Range seconds |
| --- | ---: | ---: | ---: |
| Export | 1.453 | 1.303 | 1.271–1.785 |
| FORM processing and output | 1.003 | 0.954 | 0.952–1.102 |
| Import | 4.853 | 4.500 | 4.176–5.882 |
| Sum of those stages | 7.309 | 6.755 | 6.401–8.770 |

The earlier direct-output trial measured export 1.389 s and FORM 21.107 s, then
reached its 120 s whole-job limit during import. Its import duration and final
memory are incomplete observations; no precise import speedup ratio is claimed.
The old 236 MB output was reused for comparison and was not regenerated.

New peak sampled aggregate cgroup memory was **391,544,832–391,954,432 bytes**
(about 392 MB). This includes the task process tree and charged cache, and is not
kernel-only RSS. The earlier incomplete trial reached 1,022,898,176 bytes. The
new result's Wolfram `ByteCount` is 243,663,360 bytes; this structural metric is
not a physical-memory measurement and can count repeated shared subexpressions.

A small symbolic B00 control used one fresh kernel, one warmup per configuration
and three alternating measured complete calls per configuration:

| Output path | Mean seconds | Median seconds | Range seconds |
| --- | ---: | ---: | ---: |
| Previous direct selector | 0.1163 | 0.1199 | 0.1072–0.1218 |
| Shared/factored selector | 0.1143 | 0.1150 | 0.1098–0.1183 |

These small differences do not establish a speedup; no material small-control
overhead was observed in this sample.

## Resources, evidence and reproduction

Every kernel launch used the verified task-owned runner: aggregate 1 GiB memory,
zero swap, 120 s plus at most 10 s termination grace, fresh host available memory
at least 2 GiB, sequential fresh kernels. No sampled memory-limit/OOM event,
swap use or surviving task-owned calculator occurred. Existing user kernels were
untouched. Diagnostic failures were retained and no resource limit was raised.

Evidence directory:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-23-master-sharing/`.

- `baseline/`: pre-change source, helper and dirty diff.
- `run-focused`, `run-masters`: regression scripts, assertions and diagnostic logs.
- `run-full-job`, `run-full-job-2`, `run-full-job-3`: complete inputs, programmes,
  mappings, outputs, imported WXF results, stage timings and resource records.
- `comparison-v5/compare.frm`, with its retained `comparison-v4` include files:
  final exact comparison; `run-exact-comparison-v5` contains its raw output.
- `run-small-control`: paired small-job observations and equality checks.
- `measurements.json`, `source-sha256.json`, `protected-hashes.json`: summaries
  and identity checks. Each runner invocation also retains source/script hashes.

The successful run used `runner/run.py --name <fresh-name> --driver <driver.wls>
--expected 1`. The aggregate completion count is one suite/trial; inspect the
raw assertion counts above instead of interpreting it as one algebraic test.
For reproduction, review the saved runner and host paths before executing it.
`MasterOutputSharing` is registered in the existing runtime test entry point;
that entire entry point was not run here.

Production SHA-256 for `LoopCoefficientReduction.wl`:
`987f73238e3a15f2c29156e39989200df4149972425bc21b8824453f906a8246`.

The scoped PERF-03 reproducer is resolved. Arbitrary larger amplitudes, the
older realistic gravity workload and general matrix/colour performance are not
certified by these results. No reader redesign was needed for this reproducer.
