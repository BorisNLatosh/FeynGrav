# RULE-01 completion plan — contracted Horndeski G4

## Current outcome — 1 November 2026

RULE-01 is resolved for the corrected source generators. The authoritative
[acceptance report](HorndeskiG4-2026-11-01.md) and [derivation](HorndeskiG4Derivation.md)
supersede the open/rejected-candidate status below. The separate
[RULE-05 migration](HorndeskiLibraryMigration-2026-11-01.md) has now regenerated and verified all 14
stored G4 libraries. The following dated plans,
failures and superseded claims are preserved as history, not current instructions.


## Historical convention target update — 1 November 2026

The [Horndeski model page](../Models/Horndeski.md#action) now gives the consistently
translated action for (+---) and X without a half. Its G4 monomial is
`lambda phi^a (X^b R - 2 b X^(b-1) H2)`, where
`H2 = (Box phi)^2 - H_mu_nu H^mu_nu`. This supersedes the historical
`+b` Hessian coefficient as the acceptance target. Preserve the older formulae,
source hashes and comparisons below as dated records; they do not certify this
target. RULE-01 stays open. The [implementation audit plan](HorndeskiImplementationPlan.md)
separates source repair from downstream library migration (RULE-05).
No rule, library, model or notebook was changed by this update.

**Current status, 31 October 2026: OPEN.** The later independent review rejected
closure of the action-route replacement. Use the
[correction plan](HorndeskiG4CorrectionPlan.md) as the authoritative next steps.
It records unapplied scalar permutations, incomplete graviton labelling, the
curvature contraction error, duplicated reference code and missing checks.
No code or calculations were changed/run by this documentation update.

The earlier agent reported A 22/22, B 12/12, C/D 12/12, staged E, Boundary 7/7,
validation 10/10 and cache 4/4 and claimed resolution for `n <= 3, b <= 4`.
**That closure claim is withdrawn.** Preserve the recorded runs, but their
reference agreement does not independently validate the new action construction.
The following 30 October plan and starting hashes are historical context;
current source identity and the revised gates are in the correction plan.

## 1. Start from the reviewed candidate

Read the [verification report](HorndeskiG4ContractedBranch.md),
[rule contracts](../../Rules/Contracts.md),
[multiplicity report](RuleMultiplicityCorrection.md) and the live root
`AGENTS.md` if present. The repository documents suffice for understanding this
plan; the ignored local handoff is not a package dependency.

At review, the branch is `FeynArts_Integration`, HEAD
`1b53368e6b29ff46ff490ea902f3d2108c2b94fc`. The rule repair is **uncommitted**.
Do not reset, overwrite or attribute it to HEAD alone. Recheck status and hashes
before work and record any drift before using retained results.

| Input | Reviewed SHA-256 |
| --- | --- |
| `Rules/HorndeskiG4.wl` | `e23112b2d27d4c789c9d95fff8ab92e306ee29a6fab71a29cf4f8e16e54f882d` |
| `Libs/HorndeskiG4_0_2_2` | `21e2f6ae24f49cac71afcbd984a42d77e3df44ec3b1378946ccc2ad931442202` |

The candidate corrects contracted `T3/T4/T5` coefficients to `b/-2b/b` and routes
public direct calls with `b >= 2` through `HorndeskiG4Uncontracted` followed by
`Contract`. It preserves the private contracted-helper guards. The generator
already uses the uncontracted routine; neither it nor stored libraries changed.

Retained sign checks passed 20/20, candidate and production checks 13/13 each,
and the library comparison 7/7. The primary fixture is `n=2,a=0,b=2`. These are
exact symbolic-D consistency checks. Reuse them when their inputs still match.
They do not independently derive the complete published rule. The three-graviton
reference exceeded 600 seconds; `a>0` and `b>2` checks have not run. Failed attempts
must remain available and must never be counted as passes.

## 2. Establish an independent reference

Use [FeynGrav 3.0, section 3.3, equations (37)–(42)](https://arxiv.org/html/2406.14872#S3.SS3)
as the convention reference. Start from its covariant action, not its final
vertex or a transcription of the G4 source. For one monomial the target is

\[
S_{a,b}=\Upsilon_{a,b}\int d^D x\,\sqrt{-g}\,\phi^a
\left[X^b R+bX^{b-1}\left((\Box\phi)^2-
(\nabla_\mu\nabla_\nu\phi)(\nabla^\mu\nabla^\nu\phi)\right)\right],
\qquad X=g^{\mu\nu}\partial_\mu\phi\,\partial_\nu\phi.
\]

Here D continues the four-dimensional action, and X has **no extra 1/2**. For
`b=0`, omit the second term before evaluation; never manufacture `0 X^(-1)`.
Use `g=eta+kappa h`, the existing curvature and Fourier conventions, and incoming
momenta. No on-shell, helicity, transversality or D=4 substitutions.

1. Independently expand the inverse metric, determinant, Christoffel symbols,
   curvature and scalar Hessian to the needed graviton order. Use formal Lorentz
   contractions with symbolic D, not a fixed-dimensional component matrix.
2. Extract the coefficient of separately labelled external fields. Symmetric h
   differentiation carries the appropriate index-pair symmetrisation. Derive
   scalar/graviton multiplicities explicitly; do not insert an additional n!
   into an expression already summed over all graviton permutations.
3. Derive the five structures and their index/momentum assignments. In particular,
   the two Hessians give the relative `b,-2b,b` coefficients; this alone does not
   verify the curvature structures or overall Fourier normalisation.
4. Keep the reference builder independent of `HorndeskiG4`, its uncontracted
   counterpart, `T1`–`T5` and stored G4 libraries. Sharing FeynCalc notation and an
   algebra backend is acceptable. Any reused tensor primitive must have its own
   independently checked definition; do not hide the same vertex in a helper.
5. Record a symbolic derivation of the general permutation/counting structure,
   then obtain exact computational comparisons for the fixture ladder below.
   A rewrite of equation (42) in another syntax is a paper-transcription check,
   not an action derivation.

The direct source has no coupling argument: infer `n=Length[gravitons]/3` and
`a=Length[scalarMomenta]-2b`, and compare unit-coupling rules. Restore the same
single coupling on both sides for the library-backed comparison. The main-package
function has a different signature; retain the existing context normalisation.

Use one declared routing on both sides. If integration by parts introduces a
boundary-term difference, justify and apply total incoming momentum conservation
once to both expressions and label that comparison accordingly. Never set the
scalar and graviton momentum sums separately to zero. Record comparisons that
hold without momentum conservation separately from those that require it.

## 3. Make the comparison tractable before enlarging fixtures

Measure construction, contraction, export, FORM and import separately. Diagnose
the retained timeout; do not simply replay the 600-second calculation. Prefer
exact differences organised by the five action structures and complete labelled
permutation sums, preserving shared expressions and consistently renamed dummy
indices. A blockwise proof is acceptable only with an explicit exhaustive
reconstruction of the full vertex; selected components or samples are not enough.

### Low-compute A2 resolution route

Reuse the saved Gate A candidate/reference WXF rather than rebuilding the public
wrapper for every diagnostic. Validate the two-graviton curvature first with one
fixed labelled leg order and no scalar factors or permutation sum. Check only the
new second-order inverse-metric, derivative-Christoffel and Christoffel-squared
structures; the successful one-graviton case already constrains the lower-order
and single-Hessian terms. Production `T1`/`T2` helpers may localise a discrepancy
but cannot enter the independent reference.

A small exact two-dimensional component fixture with plane-wave metric
perturbations may supplement this audit. Compare the formal second-order Ricci
scalar against direct component differentiation of the covariant metric. This can
decide whether the reference implementation or the production curvature needs
repair, but fixed-dimensional/component evidence is diagnostic only, not symbolic-D
closure. No on-shell or transversality substitution is allowed.

After curvature agrees, inspect only products containing two Hessian/Christoffel
contributions, with fresh dummy indices. Restore scalar and graviton permutations
structurally and perform one exhaustive final comparison and one actual public
wrapper call. Use FORM on compact block residuals where useful, not repeated
whole-vertex contractions. If a total derivative explains a routing difference,
derive it on the small block before applying total momentum conservation once.

Use `FeynGravSimplify`/FORM for supported Lorentz-polynomial residuals after a
small cross-check against FeynCalc. The proposed algebra-only configuration is
`Method -> "FORM"`, `LoopMomenta -> {}`, `TensorDecomposition -> False`,
`ReduceToScalarMasters -> False`, `DropScaleless -> False`, `FORMThreads -> 1`,
with retained files and a finite timeout inside the external scope. Inspect the
live facade options first; this plan does not certify an untested call.

The public source wrapper calls `Contract` **before** an outer
`FeynGravSimplify` can receive its result. Therefore wrapping that call is not a
repair for expensive source construction or contraction. Simplifying the
uncontracted expression directly with FORM is a reference experiment, not proof
that the actual public wrapper completes. If a production change becomes
necessary, first document the measured bottleneck and a narrow proposal; compare
it with the reviewed candidate before replacing anything. Do not turn RULE-01
into a general converter/performance redesign.

For source and facade loading, prepare a small fresh-kernel compatibility check
with explicit contexts and a saved loading order. Keep library-backed loading
separate. Do not run the generator or import unrelated libraries just to access
FORM. A failed or unevaluated simplifier call cannot serve as an equality result.

## 4. Complete the fixture ladder

| Gate | Fixture `(n,a,b)` | Required new evidence |
| --- | --- | --- |
| A | `(1,0,1)` and `(2,0,1)` | Independent action reference validates signs, curvature terms and normalisation; small FORM/FeynCalc comparison establishes the residual method |
| B | `(2,0,2)` | Full primary vertex equals the independent action reference; reuse candidate/library regression evidence when hashes match |
| C | `(2,1,2)` | Five scalar legs: undifferentiated-field multiplicities and scalar permutations, compared with the action reference/general derivation |
| D | `(2,0,3)` | Six scalar legs: higher-b coefficients, derivative assignments and dispatcher behaviour, compared with the action reference/general derivation |
| E | `(3,0,2)` | Three gravitons: higher graviton order and permutation multiplicities, with full exact comparison; previous timeout is inconclusive |
| F | Small cases chosen below | Validation, propagation, caching, missing-term control and clean lifecycle |

Run A and B first, then C, D and finally E. Each passed fixture must establish:

- A resolved tensor expression, with the expected free-index rank and no private
  rule calls, unresolved contractions or failed backend objects.
- Exact zero residual after the explicitly stated routing, symbolic D and generic
  off-shell invariants. Retain the reference, candidate and residual.
- Kappa power n, a single common coupling, and momentum degree `2b+2` for nonzero
  vertices; zero results require a separate proof, not just failed evaluation.
- Symmetry within each graviton index pair and under identical external-leg
  exchanges. Adjacent leg transpositions generate the scalar/graviton permutation
  groups; use that fact to avoid factorially many redundant symmetry tests.
- Agreement of the executed public wrapper with the verified tensor. A separately
  assembled equivalent expression does not establish that wrapper's runtime.

Use exact algebra, not numerical agreement as the closure criterion. Generic
numerical samples may supplement the exact result. No test count alone proves
that the required mathematical comparisons actually took place.

For gate F, preserve existing `b=0/1` and one-graviton `b=2` evidence unless the
implementation or relevant dependencies change. Check malformed graviton/scalar
arrays, wrong arity, negative/noninteger b and too few scalar momenta. Check
inherited unsupported tensor-helper requests return Failure rather than zero.
Use a small isolated dependency-failure fixture to verify propagation and that
failure is not memoised; restore the dependency and retry in that same test.
Verify an identical successful call reuses its result, whereas a different input
is not served from that cache. Keep such instrumentation out of production.

Run a deliberately wrong overall sign and an omitted-T5 expression through the
**same new equality checker**. Select a nonzero generic primary T5 fixture and
require an explicit nonzero resolved residual, not merely failure to prove zero.
The old wrong-sign result does not automatically validate a new FORM checker.

`CTensorGeneral` currently supports up to seven external index pairs. Map that
inherited restriction to the G4 calls actually used, test a safe rejecting case,
and document the effective domain. This task does not extend that helper or
promise arbitrary b. Private contracted-helper restrictions remain distinct from
the public wrapper's route.

## 5. Bounded execution and evidence

All calculations are future work. The 29 October task-specific approvals allowed
4 GiB aggregate memory and up to 600 seconds, zero swap, at most 10 seconds
termination grace and fresh host available memory of at least 2 GiB. These are
ceilings for the assigned RULE-01 continuation, not global defaults or authority
for unrelated work. Reconcile the live assignment with root AGENTS before launch;
do not silently increase any ceiling or replenish failed-run allowances.

Proposed initial budget, to be fixed in the execution ledger before starting:
**six sequential mathematical scopes**, one each for A–F. Each has at most one
fresh parent Wolfram kernel and at most two sequential serial-FORM invocations
if needed; account for implicit Wolfram workers inside the same aggregate cap.
Use 120 seconds for A/F and up to 600 for B–E only within the confirmed task
exception. These are complete-scope limits, not a fresh timeout per child or
per assertion. Do not spread an unlimited computation over uncounted scopes.
If the concrete scripts cannot fit this allocation, revise the proposed batch
layout before execution rather than allowing scripts to self-release more runs.

Before scientific work, inspect the retained runner and repair the missing
post-timeout confirmation if necessary. One additional harmless owned-process
lifecycle fixture may be proposed for a changed runner, including its failure
path; count it separately. Do not claim the historical timeout obeyed the grace
bound merely because its processes are absent now.

Stop the sequence on a failed comparison, resource event, unexplained diagnostic,
source drift or uncertain termination. Preserve the failure; diagnose before a
new specifically justified/reviewed attempt. No automatic retry or fallback that
turns a failed equality check into a pass. The pre-existing `name::shdw` warning
is documented, not a blanket warning exemption; avoid main-package loading where
unnecessary and retain any known diagnostic explicitly.

The bounded runner must verify actual cgroup limits and ownership before launch,
monitor the whole descendant tree, and retain termination confirmation after the
grace period. Restrict process control to that task's cgroup with PID/start-time
checks. Never close, evaluate in or terminate the user's kernels.

Use a fresh evidence directory under
`/home/latosh/Documents/Codex/FeynGrav-evidence/`, not a historical run directory.
Retain the exact scripts and assertions, input/result hashes, source/dependency
versions, commands, raw diagnostics, per-stage and complete wall time, aggregate
memory/events, launch counts, exit status, and final owned-process checks. Include
pre/post checksums of protected rules, libraries, models and notebooks. WXF files
must be read with `Import[..., "WXF"]`, not `Get`.

## 6. Delivery and closure

Deliver the derivation, reproducible focused scripts in an appropriate existing
verification location (no new top-level Tests directory), the A–F outcome ledger,
and evidence links. Update the [report](HorndeskiG4ContractedBranch.md) without
erasing historical failures. Qualify the public contract and usage text to the
actual supported domain; record the direct-rule sign correction's release impact
without implying that unchanged stored libraries were corrected or regenerated.

Update the private task note, Known issues, Roadmap, Current status and local
AGENTS handoffs consistently. Source repairs, if required, must have focused
regressions before acceptance; avoid unrelated cosmetic refactors. Preserve all
notebook edits, interaction libraries, model bundles and unrelated dirty files.
No commit, push, branch change, model rebuild or library regeneration is included.

Mark RULE-01 resolved only when:

1. The independent action derivation and exact comparisons establish the primary
   repair and the representative generalisation fixtures, with explicit conventions.
2. The public route, symmetry, normalisation, failure/cache behaviour and negative
   controls pass with no unresolved required comparison.
3. The supported domain follows from the general derivation and inherited helper
   contracts; finite tests are not advertised as exhaustive arbitrary-order proof.
4. Resource/lifecycle records are complete and documentation accurately separates
   verified results, historical failures and remaining limits.

If gate E or another required gate still cannot complete, report the precise
bottleneck and keep RULE-01 open. A narrower closure needs an explicit maintainer
scope decision; it must not be achieved by silently dropping a timed-out fixture.
