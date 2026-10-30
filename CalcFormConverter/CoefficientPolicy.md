# Shared coefficient-arithmetic policy — PERF-05 increment 2

The shared policy is implemented at the **existing coefficient-processing
points**. It consolidates mapping roles, rational-polynomial recognition and
stage allowances; it preserves the existing FORM algorithms and emitted algebra.
Increment 3 connects the policy to bounded collection before and after reduction;
see [IntegralCollection.md](IntegralCollection.md). Not every input receives every
arithmetic stage.

## Ownership and classification

`RationalCoefficients.wl` owns `coefficientRoles`, `coefficientPolynomial`,
`coefficientInteger`, `coefficientInverseRules` and `coefficientLimits`.
`rcPlan`, `dcfPlan`, `rcMasterPlan` and `rcPolarisationPlan` consume them.
No new companion, installer dependency, public option or persisted format is needed.

The input is the validated encoded mapping. No decoding, `ToExpression`, user
symbol evaluation or assumptions enter these selectors. Scalar symbols and
vector identities remain separate even when they originate from the same Wolfram
symbol. Pair atoms follow dictionary order; incoming/outgoing polarisation
identities remain distinct. Indices and matrix/colour objects are never entered
in the scalar-variable dictionary.

| Content | Current owner and treatment |
| --- | --- |
| Exact integer/rational numbers | FORM exact arithmetic; the shared polynomial compiler renders exact leaves without converting to floating point. |
| Exact complex values | Existing FORM imaginary-unit arithmetic. The older propagator rational path retains its complex exclusion; final masters use the existing imaginary-unit substitution/restoration. |
| Commuting scalar symbols | Validated dictionary identities; the dimension-only domain admits just the selected dimension, not every symbol with the spelling D. |
| Momentum scalar products | ExportAssembly already expands supported linear routing before native FORM sorting. Shared role planning supplies consistent dot-product identities; it does not add kinematic relations. |
| Polarisation products | Existing complete-structure grouping and scalar-coefficient inspection; polarisation atoms remain outside the per-group scalar-variable budget. |
| Metrics/components, Dirac/colour/epsilon objects | Existing structural algebra and runtime guards; no new scalarisation or matrix reordering. |
| Opaque radicals/functions or unsupported definitions | Conservative original path. The limited inverse-polynomial compiler rejects them; the dimension-only path can still treat unrelated definitions as opaque factors. |
| Approximate input | Existing unsupported-input rejection. No rationalisation or tolerance-based zero inference. |

`coefficientInverseRules` visits each abbreviation once for the requested domain.
A dimension-only rejection does not become a general-domain rejection: the
scalar dictionary is an explicit argument and no translation cache persists
between jobs. Predictable ineligibility returns `$Failed` internally; public
runtime failures are still failures and are never converted into that fallback.

## Arithmetic order and stage allowances

Exact sorting, collection and common-content extraction retain their existing
FORM owners. The common compiler replaces the duplicated polynomial readers in
propagator and dimension processing. Final master inspection still estimates
actual shared dependencies in FORM; it must not be replaced by mapping-wide
variable counting. The profiles are intentionally different:

| Profile | Selection and stopping |
| --- | --- |
| Polynomial recognition | Nonnegative polynomial powers 0–32; inverse abbreviation powers −8…−1. Unknown domain objects retain the old representation. |
| Job envelope (increment 5) | 1,024 rational passes and 2,048 full factor attempts; local limits also apply. |
| Propagator | At most 1,000 coefficient terms and 12 mapping scalar/dot variables; existing indexed/unsafe-object checks remain. |
| Collection reuse | At most 64 ready entries and 1,024 eligible visits in the pre/direct-post collector; saturation retains the complete current coefficient. See [IntegralCollection.md](IntegralCollection.md). |
| Dimension | Optional univariate processing; existing preconditioning threshold greater than 1,000 terms, with stage compaction at its existing locations. |
| Master | At most 1,000 estimated terms, 12 actual scalar variables and degree 24, including shared dependencies. |
| Polarisation | At most 1,024 structures, 10,000 structural terms and 1,024 speculative rational passes across the existing job. Negative private pass allowance clamps to zero. |
| Bounded polarisation factorisation | At most 250 terms, eight variables and degree 12, subject also to existing FORM argument constraints. |

`coefficientLimits` is the single named selection interface. Private legacy
controls remain usable by regression tests; no threshold was raised. Existing
propagator `Factorize` and final polarisation `FactArg` modes remain distinct;
this increment does not replace their representation or selection mechanisms.

Current caches, algebra modes, shared-definition metadata, deterministic scores
and complete-original rollback remain with their FORM procedures. No new cache,
retry loop or allowance replenishment is added. Unsupported work and spent
allowances preserve a complete valid result; actual errors, warnings, aborts or
resource events remain execution failures. This is consolidation of existing
stopping safeguards, not a budget covering every pipeline stage. Increment 5 adds a
shared envelope for optional multivariate rational/factor work; see
[WorkLimits.md](WorkLimits.md) for exclusions and focused verification.

## Scope and remaining work

The private processing plan now records `CoefficientPolicy -> "SharedScalarPolicy"`;
its revisit policy is `JobLocalCollectionReuse`. Plan metadata is not serialised.
The shared policy is used by current processors, while ordinary loop-free inputs
without eligible processing still use their previous path. No new integral
identity, integration scope, reduction rule, public mathematical head or API-03
conversion behaviour is introduced. Production Wolfram code does not call
`Simplify`, `Together`, `Factor`, `N` or `Rationalize`.

Increment 3 supplies bounded collection before and after reduction on the
eligible linear one-loop path. Collection-boundary reuse is delivered in increment 4. Increment 5 implements
common optional rational/factor allowances, accepted with 495/495 combined checks;
see [WorkLimits.md](WorkLimits.md). [Dependency ownership](DependencyPaths.md)
and [final verification](../Documentation/Verification/PERF05Final-2026-10-30.md) complete the retained QED/three-box gates.
The full selected QED tadpole and 1,490-term equality remain outside this acceptance.
