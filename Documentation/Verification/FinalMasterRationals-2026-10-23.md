# Final master-coefficient rational simplification — 23 October 2026

## Scope

Uncommitted work on `FORM_Simplification`, based on `f30dd8e` plus the earlier
export-planning and shared-output corrections. No interaction rules, stored
libraries, FeynArts models or notebooks were changed. No commit was made.

The final coefficient of an eligible scalar master now receives exact rational
arithmetic in FORM before denominator grouping and output. The motivating identity

```mathematica
(1/s - 1/(s+t) - t/(s (s+t))) PaVe[0, {}, {m^2},
  PaVeAutoOrder -> False, PaVeAutoReduce -> False]
```

returns literal `0`. Previously it remained nonzero in structure on repeated
calls, although `Together` proved that it was zero. The pass also handles
nonzero rational results, shared polynomial identities, complex coefficients and
scalar products. It uses no `$Assumptions` or new kinematic specialisations.
Rational identities are understood on the common domain of their denominators;
this is not a new prescription for evaluating singular kinematics.

## Implementation and limits

- `RationalCoefficients.wl` shares native scalar-power translation between the
  original propagator simplifier and the new master caller.
- `MasterRationalCoefficients.frm.in` computes conservative metadata for shared
  definitions, selects one final multiplier at a time, exposes its required
  definitions, and uses `PolyRatFun` for exact rational arithmetic. The
  [FORM reference manual](https://form-dev.github.io/form-docs/stable/manual/)
  documents this facility and extra-symbol reconstruction.
- Selection uses an estimated 1,000 expanded terms, maximum absolute degree 24
  per term (including denominator factors), and 12 actual scalar variables.
  Unknown dependencies or unsupported objects retain the old representation.
  These selection limits do not bound arbitrary rational-GCD cost or replace
  the external execution limits.
- Selected results return to the existing shared/factored writer. The pending
  definition scan now includes new denominator references inside `cfcLRDen`.
  Empty zero-group frames are retained for compatibility and reconstruct as zero.
- Matrix/epsilon, nonlinear coefficient-product and unshared output retain their
  existing path. The optional pass does not change recurrences, normalisation,
  scalar-master definitions, mappings or the incremental reader.
- `install.m` records the new template dependency; no installation was performed.

## Exact verification

| Check | Outcome | Scope |
| --- | --- | --- |
| New `MasterRationalCoefficients.wls` | 57/57 assertions | Literal-zero cancellations, nonzero/complex fractions, shared polynomials, symbolic D and masses, scalar products, saved/new integral input, serial/TFORM, size/degree fallback, unrelated mapping variables, manual/tiny-chunk import, unchanged mappings |
| Existing scalar-master suite | 91/91 assertions | Manageable independent FeynCalc comparisons through rank six, repeated powers, exceptional/scaleless cases, matrix/epsilon spectators and existing malformed-input checks |
| Existing output-sharing suite | 48/48 assertions | Exact shared/direct comparisons and preserved output-selection contracts |
| Existing propagator-rational suite | 38/38 assertions | Original arithmetic/factorisation, eligibility and serial/TFORM behaviour |
| Complete three-box workload | Exact unchanged output, mapping and result hashes in all three final trials | All 17 masters reproduce the prior shared-output result; that result has the separately retained exact FORM comparison with the original 236 MB output |
| Installer dependency and test-driver syntax | Static checks passed | The source references the new template and the installer checks its presence; no install/archive smoke run |

The scalar-master suite retains its expected partial-reduction message for the
exactly singular saved triangle in call 56. Its raw message and input are retained
separately. Successful final runs had no unexpected diagnostics or resource events.

The actual long QED `A[[1]]` was not rerun or certified: its notebook stores a
shortened display with 256 terms omitted and `Missing["NotStored"]`. The focused
fixtures establish the relevant cancellation mechanism. Coefficients exceeding
the selection budget can still remain unsimplified.

## Performance

Three sequential fresh-kernel trials, with no in-kernel warmup. A preliminary
trial preceded them; filesystem caches, power state and background load were not
controlled. Timers exclude package startup and subsequent WXF serialisation.
Dependencies: Wolfram 15.0, FeynCalc 10.2.1, serial FORM 5.0.2.

| Stage | Mean seconds | Median seconds | Range seconds |
| --- | ---: | ---: | ---: |
| Export | 1.437 | 1.443 | 1.421–1.447 |
| FORM and output | 2.288 | 2.262 | 2.204–2.398 |
| Import | 5.020 | 5.108 | 4.688–5.265 |
| Sum | 8.745 | 8.915 | 8.393–8.928 |

Output remains **4,364,149 bytes**, identical to the preceding shared-output
implementation and 98.15% smaller than the historical 236,271,618-byte output.
Peak sampled aggregate cgroup memory was **389,812,224–391,704,576 bytes** (about
392 MB), versus 391,544,832–391,954,432 bytes in the preceding measurements.
This includes the task process tree and charged cache; it is not kernel RSS.

The preceding complete-pipeline medians were export 1.303 s, FORM 0.954 s,
import 4.500 s, sum 6.755 s. The additional pass therefore has a measured cost;
this is not a speedup claim. Output-size and memory improvements are preserved,
and all new trials complete comfortably within the existing bounds. The older
236 MB import had not completed within its 120 s whole-job limit.

All three outputs have SHA-256
`ff8f1eb9d82c1d816e37a6551d5b5e66f79fe2f27f8231a00a2ebcb68b605b99`;
all imported WXF results have SHA-256
`b160849462f3fed404adf795cdb1dc42d72aa4f8d1fba7c7beb6f6358ac5008f`.
Both match the preceding implementation exactly.

## Retained development failures

The initial generated procedures had quoting and FORM statement-order errors.
The latter produced a TFORM nonzero/crash exit on the invalid programme. A
subsequent check detected a wrong coefficient caused by `Skip` being module-local:
inspection was modifying the original expression after a sort. Explicit selection
at each module boundary fixed this. Metadata originally retained a mutable dollar
reference; taking a preprocessor snapshot fixed false budget rejection.

The first wider suite exposed invalid zero-group framing, missing newly created
shared denominator definitions, and unmatched negative dot-product powers. These
were corrected; focused runs progressed from 31 failures, to one, to zero.
A diagnostic wrapper bracket typo and a regression-wrapper `Common.wl` path
warning were also corrected. Failed scripts, logs and observed wrong outputs
remain retained; they are not counted as successful checks.

## Resources and evidence

Every scientific launch used a fresh task-owned scope with aggregate 1 GiB RAM,
zero swap, 120 seconds plus 10 seconds termination grace, and fresh host headroom
of at least 2 GiB. Runs were sequential; existing user kernels were untouched.
No sampled memory-cap/OOM/swap event or surviving task-owned calculator occurred.

Evidence:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-23-final-master-rationals/`.

- `baseline/`: pre-task dirty diff, status and relevant source files.
- `run-focused-complex/`: final 57-assertion focused suite and retained cases.
- `run-masters-final/`: final scalar-master regression and per-call diagnostics.
- `run-sharing/`, `run-propagator-rationals-2/`: existing regression evidence.
- `run-boxes-1/` through `run-boxes-3/`: programmes, mappings, output, WXF,
  timing/resource/termination and source/script identity records.
- `measurements.json`, `source-sha256.json`, `static-checks.json`,
  `protected-hashes.json`: summaries and source identities.

No Full benchmarks, complete notebook evaluation, general numerical-integral
validation or exhaustive legacy suite was run. The earlier reports remain valid
historical measurements: [output sharing](MasterOutputSharing-2026-10-23.md),
[export planning](ExportPlanning-2026-10-23.md).
