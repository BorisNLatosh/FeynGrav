# Coordinated coefficient work allowances — PERF-05 increment 5

Status: **accepted within focused scope, 495/495 combined assertions**.
The shared work allowances preserve exact results in the verified fixtures.
See [the verification record](../Documentation/Verification/PERF05WorkLimits-2026-10-30.md).

## One owner per FORM job

`RationalCoefficients.wl` renders `Templates/CoefficientWorkLimits.frm.in` once,
before the algebra procedures. `ExportAssembly.wl` records the private
`WorkPolicy -> "JobRationalAndFactorAllowances"`. Counters are initialised once
per generated program. No batch, master, cache hit or rollback resets them.
The installer requires this template with its owning companion.

| Allowance | Initial private limit | Admission point |
| --- | --- | --- |
| Rational passes | 1,024 | Eligible propagator coefficient, collection miss, or final master rational call, including nested polarisation calls. |
| Full factor attempts | 2,048 | Two reserved atomically before propagator numerator/denominator factorisation; one for each eligible polarisation numerator or denominator. |

Existing term, degree, variable, structure, lookup, cache and local polarisation
limits remain conjunctive. The counters measure optional work admissions, not
uniform computational cost, elapsed time or memory. The defaults are conservative
initial choices, not calibrated optima. Earlier coefficients can spend the shared
allowance and leave later coefficients less simplified.

Completed collection hits bypass new arithmetic reservations. Master inspection
and algebra cache accounting at the existing rational hook remains unchanged;
completed polarisation-group hits bypass that hook. A denied pass is not cached
as a mathematical rejection or a completed partial factorisation. Work already
spent remains charged even when its enclosing candidate is rolled back.

## Exact fallback and limits of coverage

Reservation denial retains the complete original coefficient. During speculative
polarisation processing it triggers the established whole-master restoration;
it never publishes a partial candidate. No term is discarded and no kinematic
identity or assumption is introduced. Actual FORM failures, warnings, aborts and
resource events remain failures, without silent retries.

Cheap common-content extraction, fixed-phase dimension-only compaction, integral
recurrences, tensor algebra and output/import retain their own contracts. They
are not assigned artificial rational-pass costs. This is therefore not a universal
budget for every operation. The whole-process resource limits remain authoritative.

Private controls `$rcMaximumRationalPasses`, `$rcMaximumFactorisations` and
`$rcCoordinateWork` support small-boundary and matched-reference checks. Negative
limits clamp to zero. There is no new public option or mapping version. Temporary
counters do not become imported mathematical objects. `CFC_CW_SKIP` gives developer
context alongside the existing polarisation skip record.

## Verification entry points

`Tests/CoefficientWorkLimits.wls` contains 162 focused assertions against actual
export/FORM/import paths. `Tests/CoefficientReuse.wls` also includes cache
capacity, factor-pair and zero-cache boundary tests (53 added assertions). The
162 focused, 114 reuse, 69 collection, 38 rational and 112 polarisation-budget
checks all passed in one bounded combined batch. This does not certify the
complete converter suite or full-workload performance.

[Dependency review](DependencyPaths.md) and [final verification](../Documentation/Verification/PERF05Final-2026-10-30.md) now
complete the retained three-box/QED gates. No resource limits were raised;
large-case QED limitations remain explicit.
