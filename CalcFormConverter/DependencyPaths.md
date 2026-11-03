# Dependency ownership and stopping

The PERF-05 strategy uses the existing stage order with bounded optional
coefficient processing. Input-case planning does not create an integration
measure or bypass existing capability checks. No new kinematic assumptions apply.

| Boundary | Identity and dependency contract |
| --- | --- |
| `ExportAssembly.wl` | Validated input → tensor preparation → reduction preparation. ProcessingPlan is private metadata; requested stages remain authoritative. |
| `LoopCoefficientReduction.wl` / `CoefficientCollection.frm.in` | Immutable scalar rules per export. Complete sorted coefficients have exact keys; pre/post merges produce new keys. This collector does not dereference mutable shared blocks. |
| `MasterRationalCoefficients.frm.in` / `PolarisationCoefficients.frm.in` | Shared polynomials use immutable native extra-symbol identities. New definitions have new IDs; missing metadata is registered before scoring, known IDs skip materialisation. |
| Inspection and algebra caches | Unresolved metadata is not a completed rejection. Algebra keys distinguish rational/factor modes; save the outer key before nested inspection changes scratch state. Group/denominator readiness follows complete work only. |
| Work limits and selection | Job-local counters remain charged after rollback. Budget denial is not mathematical rejection. Original wins ties; another fallback requires strictly improved reachable score. |
| `TensorDecomposition.wl` import validation | Frozen invariant metadata rejects changed reduction kinematics with `CoefficientConventionMismatch`; new context requires a new export. |
| `LoopDenominatorGroups.frm.in` / `SharedCoefficientReader.wl` | Densify disposable labels; emit definitions before references; reconstruct incrementally without replaying algebra. |
| `FeynGravSimplify/Facade.wl` | API-03 conversion follows successful import and belongs in default complete-call timing. |

All caches belong to one generated FORM job. No cross-job reuse or arbitrary
in-place mutation of shared definitions is supported. Full-master rollback can
retain completed subproblem caches because their definitions are immutable; it
cannot refund work or mark incomplete candidates ready. There is no general
fixed-point expansion/factorisation loop.

The 62-check dependency/polarisation regression exercises changed keys,
unresolved-to-resolved metadata, successful/rejected inspections, both algebra
modes and reference-first registration. The earlier 495-check batch exercises
collection aliases/capacity and aggregate work/rollback boundaries. The 91-master
and 48-sharing regressions cover invariant validation and exact reconstruction.
See the final report (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance))
for exact retained-workload checks and limitations. A mutable dependency-graph
framework is neither required by these paths nor certified by this review.
