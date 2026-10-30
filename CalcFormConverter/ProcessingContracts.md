# Private processing contracts — PERF-05

Increment 1 established these processing decisions; increment 2 consolidates
coefficient selection. Neither adds mathematical identities or change tensor/scalar-master reduction, public options,
output formats, caches, work limits or restricted methods. Increment 2 now shares
the existing coefficient selection/compilation policy; see [CoefficientPolicy.md](CoefficientPolicy.md).

## Input scope

| Private input case | Meaning |
| --- | --- |
| `NoDeclaredIntegration` | No loop momenta and no represented integral functions. Propagators may still occur as ordinary algebraic objects; they do not create a measure. |
| `DeclaredIntegrand` | Declared loop momenta apply to the whole expression, including constants after cancellations and represented integral factors. Existing single-loop capability checks remain authoritative. |
| `RepresentedIntegrals` | No declared measure; supported integral coefficients or named scalar functions are present. This does not assert that tensor coefficients are scalar masters or that integrals have been evaluated. |

Presence flags distinguish propagators, `IntegralCoefficient` entries and named
`A0`–`D0` functions, including functions within scalar abbreviations. They do not
partition sums, extract loop-independent pieces or infer physical kinematics.
Unsupported vocabulary is still rejected by the existing exporter. Dimension
and loop identities are retained in encoded form without decoding user symbols.

## Ownership and execution order

`buildExportData` supplies validated input and a private named-function flag.
`prepareProcessing` preserves the former `smPrepare[tdPrepare[...], ...]` order.
It holds its arguments so tensor-option validation still precedes evaluation of
reduction options. The two existing DropScaleless evaluations remain separate;
this preserves standalone delayed-option behaviour as well as facade calls.

After successful preparation, `processingPlan` attaches a `ProcessingPlan`
association to the in-memory export data. It contains input scope, presence flags,
input algebra version, requested options and selected preparation stages. Existing
preparers determine eligibility, warnings and failures. Selection of scalar-master
reduction does not promise that every coefficient is reduced. The tensor stage is
not reported as selected merely because saved-coefficient reduction uses tensor
metadata internally.

The renderer still owns its existing algebra, cancellation, coefficient-processing
and output order. It does not serialise this private plan into the mapping or FORM
program. There is no new mathematical head, mapping version, persistent cache,
public option or additional iteration. `CoefficientPolicy -> "SharedScalarPolicy"`
records that boundary. Increment 4 records
`RevisitPolicy -> "JobLocalCollectionReuse"`: exact reuse at the existing
pre/direct-post collection boundaries, under one immutable job context. It does
not add a general iteration or change requested output stages.

## Coefficient and stopping contract for subsequent increments

Future coefficient extraction must preserve the declared measure, loop-dependent
numerators, free-index connections and ordered Dirac/colour/epsilon structures.
A scalar multiplier may be processed only with its complete relevant contribution
and shared-definition dependencies. Reuse and stopping must distinguish changed
context, ineligibility and spent work allowance; runtime failures remain failures.
Increment 2 consolidates selection; increment 3 adds bounded complete-coefficient
collection around runtime reduction. See [IntegralCollection.md](IntegralCollection.md).
Collection-boundary changed-input reuse is delivered in increment 4 and optional
rational/factor work coordination in increment 5. Dependency review and retained
whole-workload validation are accepted; see [final verification](../Documentation/Verification/PERF05Final-2026-10-30.md). No resource limit was increased.

The recorded delivery order is: establish these PERF-05 contracts; deliver API-03
separately at the final-output boundary; complete and measure PERF-05; then reprofile
and pursue PERF-06. API-03 was delivered separately on 30 October 2026; see
[its verification report](../Documentation/Verification/API03PaVeOutput-2026-10-30.md).
The shared selection/compilation policy is delivered in increment 2; broader
increment 3 delivers bounded linear collection and increment 4 adds bounded
job-local reuse there. All five increments and their retained acceptance checks
are complete. Current conservative exceptional
and scaleless retention, LIMIT-02 and restricted-backend boundaries are unchanged.

## Focused verification

`Tests/ProcessingPlan.wls` is registered in the runtime suite. Run it from a fresh
writable working directory with FORM on PATH inside the documented bounded runner.
It compares the old nested preparation and the new coordinator for 12 inputs and
three option combinations, including generated text and mapping equality. Additional
checks cover integration scope, saved-coefficient stage selection, delayed options,
a deliberately altered export-data negative control and three small FORM results.
The negative control tests parity detection, not an independent physics derivation.

The old preparers remain in use, so exact preparation/program parity is the main
behaviour-preservation evidence. Full three-box performance, broader QED checks,
new coefficient arithmetic and a complete option-product audit are not certified
by this first increment. See the dated verification report for source identities,
raw evidence and process-resource limits.

## Work coordination — increment 5

`WorkPolicy -> "JobRationalAndFactorAllowances"` records the shared optional-work
owner. See [WorkLimits.md](WorkLimits.md) for the charged stages, unchanged local
guards, exclusions and original-expression fallback. Accepted within focused
scope after 495/495 combined assertions; the [final verification](../Documentation/Verification/PERF05Final-2026-10-30.md)
adds dependency, retained QED and complete three-box acceptance.
