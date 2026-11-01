# RULE-01 correction plan after independent review

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

31 October 2026. **OPEN — the action-route replacement is not accepted.**
This is the current correction handoff. It supersedes the closure claims in the
[historical completion plan](HorndeskiG4CompletionPlan.md) and the
[execution/verification report](HorndeskiG4ContractedBranch.md). Preserve those
records; do not count their agreement with a copied reference as independent
action validation.

The present assignment is documentation only. This plan does not launch another
agent, authorise calculations, edit production code, regenerate libraries, or
authorise a commit. On a subsequent implementation assignment, work in the order
below and review each gate before advancing. Do not repeat the old large tests
before fixing their reference and assertions.

## 1. Starting state and evidence classification

Reviewed checkout: `FeynArts_Integration`, HEAD
`a7c2a9aae891a854028d86fbf2fa988822fccf52`.
Uncommitted `Rules/HorndeskiG4.wl` SHA-256:
`7b22ee2eb7e76ac41dd9c57626f5af2f57db0ae824e3a8803cf2707c61c0479d`.
Unchanged `Libs/HorndeskiG4_0_2_2` SHA-256:
`21e2f6ae24f49cac71afcbd984a42d77e3df44ec3b1378946ccc2ad931442202`.
Verify the live branch, file hashes and dirty state before changing anything.
Do not reset the checkout or revert unrelated work. Archive the current candidate,
reference, scripts and reports before preparing corrected versions.

The earlier sign repair (`T3,T4,T5` coefficients `b,-2b,b`) and uncontracted-route
comparisons are historical consistency evidence on their recorded snapshots.
The later action-route replacement changes both public routines for `n <= 3,
b <= 4`; it also changes the source used by future library generation. Neither
that range nor the existing library is certified by the replacement's test counts.

The reviewed successful logs do contain reported passing assertions and clean
resource/termination records. Those observations remain valid as execution
evidence. They do not establish the mathematical claims below. In particular,
the reference and production geometric, derivative, selection, coefficient and
replacement helpers match after private-symbol renaming and whitespace removal.
They share the same defects.

Evidence root (read-only history):
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-30-rule01-independent-preparation/`.
The earlier `ExecutionReport.md`, failed OOM/timeouts, invalid-zero candidates,
and staged checkpoints must remain intact. Create a fresh correction evidence
directory with exact script/source/input hashes and a reconciled launch ledger.

## 2. Reproduce the defects cheaply before repairing them

| Finding | Source anchor | Required regression |
| --- | --- | --- |
| Scalar permutations are enumerated but their values are never used; one canonical coefficient is repeated | `actionBuild`, final nested maps | A polynomial depending on distinguished scalar slots must be genuinely permuted. Check exchanges across X/Hessian slots and differentiated/undifferentiated slots, not just reversal inside one symmetric pair. |
| `actionDistinctQ` selects one Q1 and `n-1` Q2 fields, rather than each external graviton once | `actionDistinctQ`, `actionSelectOrder` | Accept Q1 Q2 Q3 at order three; reject Q1 Q2^2. Inspect powers correctly without counting both a power and its base twice. |
| Inconsistent field labelling drops mixed terms: `trace1^2` uses only Q1, and the cubic trace uses Q1 Q2 Q3 which the selector rejects | `actionMetricRoot`, `actionInverseMetric` | Independently extract every labelled coefficient of the determinant/inverse metric through order three, including mixed trace terms. Do not fix only the final predicate. |
| An extra inverse metric is inserted between already mixed-index Christoffels | `actionScalarCurvature` | Check the connection-product contraction and its third-order expansion independently, with one fixed labelled order before permutations. |
| Reference and production share the same construction | `IndependentG4Reference.wl` and new `action*` helpers | Establish independent computational ancestry and sensitive primitive tests; renaming functions does not provide independence. |
| Scalar symmetry and full multilinearity were not tested; three-graviton omitted-T5 control selects order two | `GateC.wls`, `GateE.wls`, `ThirdE.wls` | Exercise all adjacent scalar/graviton exchanges and a genuinely order-three, nonzero T5 removal using the same resolved equality checker. |
| Earlier library comparison predates the replacement; current summaries claim resolution | Public source/library comparison and documentation | Recheck the corrected source against stored and published expressions; reconcile current status without erasing history. |

These are regression requirements, not permission to edit the historical scripts.
First demonstrate that a small fixture detects each relevant defect in a retained
copy. A test which passes both the defective and corrected implementation is not
a regression for that defect.

## 3. Re-establish the action and geometric conventions

Use the corrected covariant action in the [model page](../Models/Horndeski.md#action),
`lambda phi^a (X^b R - 2 b X^(b-1) H2)`, with
`X = g^(mu nu) partial_mu phi partial_nu phi` (no additional one-half),
`g = eta + kappa h`, symbolic D, unit coupling, and incoming momenta. Set the
`b=0` second bracket to zero before evaluating negative powers. Do not add
on-shell, helicity, transversality or dimension-four assumptions.

With the mixed-index connection used in the candidate, the curvature contraction
must have the form

\[
R=g^{\nu\beta}\left(\partial_\alpha\Gamma^\alpha_{\nu\beta}
-\partial_\beta\Gamma^\alpha_{\nu\alpha}
+\Gamma^\alpha_{\alpha\rho}\Gamma^\rho_{\nu\beta}
-\Gamma^\alpha_{\beta\rho}\Gamma^\rho_{\nu\alpha}\right).
\]

An overall opposite curvature convention may change the overall sign, but cannot
justify an additional inverse metric joining the two mixed-index connections.
Document the package's convention and Fourier map explicitly. Re-derive the
overall phase from derivatives and the action-to-vertex factor: derivative
factors `I p` and an additional `(-1)^(b+1)` must not count Fourier signs twice.
This phase audit is a required unresolved check, not a newly proved sign defect.

Check the inverse metric by multiplication through order three, and the
determinant against an independently derived trace-log expansion. Validate
Christoffel coefficients from metric compatibility/torsion freedom and curvature
from the connection formula, separately from the complete G4 vertex. Give each
independent factor fresh dummy indices, including factors produced by powers or
reuse; do not multiply two tensors with accidentally identical dummy names.

The Hessian block has coefficients `-2b,+4b,-2b` for zero, one and two
connections before Fourier factors. Establish these from the action; do not
rescale the complete vertex or preserve the printed relative coefficient.

References: the [translated Horndeski action](../Models/Horndeski.md#translation-from-the-literature),
[FeynGrav 3.0, section 3.3](https://arxiv.org/html/2406.14872#S3.SS3)
for the historical formula comparison, and
[Carroll, curvature equation (3.67)](https://preposterousuniverse.com/wp-content/uploads/grnotes-three.pdf)
for the connection-product definition. Neither reference certifies a transcription
or the candidate automatically.

## 4. Construct labelled fields consistently

Use a complete labelled expansion, for example

\[
h_{\mu\nu}(x)=\sum_{i=1}^{n} t_i H^{(i)}_{\mu\nu}e^{ik_i x},\qquad
\phi(x)=\sum_{j=1}^{a+2b} u_j e^{ip_j x},
\]

and extract the coefficient of `t1 ... tn u1 ... u_(a+2b)`. Apply the same field
sum in the inverse metric, determinant, every derivative and every Hessian.
Truncate at each multiplication using valid degree/label bounds. A sparse
multilinear label representation can discard repeated labels early; this is
coefficient extraction, not setting physical fields to zero. Retain an exhaustive
reconstruction/counting argument for every discarded sector.

An optimised production implementation may instead sum canonical assignments,
but it needs a derivation equivalent to the labelled extraction. In that route:

- Use explicit named permutation arguments. Apply a simultaneous scalar-label
  substitution to the canonical coefficient for each permutation; avoid sequential
  replacement collisions, especially when labels are exchanged.
- Include all undifferentiated scalar assignments from phi^a. The count `a` must
  affect the actual assignments rather than merely multiplying one fixed term.
- Use fresh auxiliary labels internally, so equal physical momenta do not merge
  external field identities or remove required multiplicities prematurely.
- Symmetrise each metric perturbation index pair once. Derive factorials from
  the selected convention; do not combine full labelled extraction and another
  n! or scalar permutation factor.
- Preserve exact symbolic D and tensor rank. Keep high-order and out-of-range
  routes explicit; do not silently reroute to a known suspect legacy formula.

The repair must fix the field expansion itself. Merely changing `n-1` to a Q3
test or inserting a scalar permutation into the existing incomplete expansion
does not meet this gate.

## 5. Restore genuine reference independence

Keep a separate reference builder which does not call, copy or rename production
G4 helpers. A suitable division is a full multivariate labelled-action extraction
for the reference and a separately derived canonical-assignment/permutation
construction for production. If both use coefficient extraction, implement and
check the primitives by genuinely different constructions; do not share the
geometric/selection routines under test.

Sharing FeynCalc notation and a trusted contraction/FORM backend is acceptable.
Sharing the vertex-construction algorithm and declaring two executions an
independent derivation is not. Record an explicit dependency/provenance table and
compare helper definitions to catch accidental copying before large checks.

Supplement the formal symbolic-D primitive checks with small exact component
metrics having enough independently varying entries to expose mixed trace and
connection-product terms. Fixed-dimensional components are supplementary
diagnostics, never a substitute for the full off-shell symbolic-D comparison.
When reference and production disagree, diagnose primitive blocks first. Do not
modify the reference merely to agree with production or vice versa.

## 6. Rebuild the equality and structural checks

The common checker must reject Failure, $Aborted, unevaluated rule/Contract calls,
private field markers, malformed tensors and unresolved backend output. Inspect
all terms' free indices and ensure every external symmetric graviton field enters
linearly. A nonzero result and the correct maximum kappa exponent do not prove
the free-index rank or homogeneity of every term.

For every nonzero fixture require exact kappa degree n and momentum degree
2b+2, using homogeneous rescaling that also counts powers. For a zero fixture
require a separately established identity. Test adjacent transpositions of all
scalar legs and all graviton triples, plus each graviton's index-pair symmetry.
Compare after consistent dummy renaming/contraction, not by arbitrary dummy names.
Tests crossing the derivative/undifferentiated boundary are mandatory for a>0.

For any integration-by-parts comparison derive the boundary term and impose one
common total incoming momentum relation on both expressions. Never set the scalar
and graviton momentum sums separately to zero. Label results that require total
momentum conservation separately from unrestricted off-shell identities.

Use both an overall-sign negative control and a genuine missing-action-block
control through the same equality checker. The omitted T5 block must be nonzero,
fully permuted and of the same graviton order, kappa degree, scalar multiplicity
and free-index rank as its parent. For Gate E select order three, not order two.
Reject a control that merely subtracts an unrelated nonzero expression. Require
a resolved exact nonzero residual, not just a failed attempt to prove zero.

## 7. Fixture ladder and integration checks

| Gate | Required result before proceeding |
| --- | --- |
| R0: micro-regressions | Defective copies fail the scalar-permutation, label, mixed-determinant and curvature tests; corrected candidates pass. No full vertex yet. |
| R1: primitives/reference | Independent inverse/determinant/connection/curvature/Hessian checks through order three, counting/Fourier derivation, reference-provenance review. |
| A: (1,0,1), (2,0,1) | Complete symbolic-D off-shell action equality, actual contracted/uncontracted public paths, structural/adjacent-symmetry checks and both negative controls where nonzero. Include affected b=0 and one-graviton b=2 controls. |
| B: (2,0,2) | Full primary equality, all scalar and graviton adjacent exchanges, resolved output, homogeneity, phase and exact source/library comparison with one common coupling/context normalisation. |
| C: (2,1,2) | Genuine undifferentiated-leg multiplicity and exchange tests, plus full action equality. Include a small a>=2 control to expose factorial errors. |
| D: (2,0,3) | Higher-b full equality and correctly constructed T5 control. |
| E: (3,0,2) | Full exact action equality, each graviton present once, all adjacent scalar/graviton exchanges, all index-pair checks and true order-three T5 omission. Staging requires pinned exhaustive checkpoints. |
| F: interface and domain | Malformed/arity/range checks on both public routines, dependency failure propagation and no failure memoisation, successful cache reuse versus genuinely changed input, renamed/composite/equal-momentum labels, and source/main-package context separation. |

Do not reuse old mathematical passes after changing a construction they depend on.
Untouched launcher/lifecycle checks can be reused only with matching identities.
The earlier tests called b=5 with `Permutations` replaced by identity: that is a
propagation fixture, not proof of prompt rejection by the actual public routine.
If an unsupported range is rejected, test the uninstrumented public call without
materialising factorially many permutations. Do not infer the action route's
domain from CTensorGeneral when that route does not call that helper.

The `n <= 3, b <= 4` label is not justified by A–E alone: these do not cover b=4,
all a, or every n/b combination. Before advertising that domain, supply a general
construction/counting argument plus tractable boundary checks, including a b=4
fixture. If that cannot be established within limits, keep the claim fixture-
specific and request an explicit maintainer scope decision; do not close by
silently shrinking the task or declaring untested combinations verified.

## 8. Reconcile the published rule, legacy source and stored libraries

Keep comparisons distinct: corrected action reference versus corrected direct
rule; direct versus uncontracted public rule; corrected direct versus the
unchanged stored library/main-package call; and action derivation versus the
published formula in matching conventions. Restore the same coupling exactly
once and normalise Global/FeynGrav kappa contexts deliberately.

The 29 October library pass used a different source. Repeat the primary
`HorndeskiG4_0_2_2` comparison and proportionate smaller available-library controls
after the corrected route is established. If a residual remains, localise it by
action structure, Fourier convention, routing and permutation multiplicity. Do
not infer a paper/library error solely from the rejected action builder. Record
all affected stored-library/generator consumers, including the changed
uncontracted function used for future generation.

Do not regenerate libraries, change models, rewrite saved notebooks or silently
compensate in the public importer to force agreement. A demonstrated mismatch
requiring downstream changes belongs to RULE-05 after a concrete impact report
and separately authorised delivery. Equality to an old erroneous library is
not a closure criterion: the corrected source must match the independent action,
and every old/new discrepancy must be explained. Unexplained required comparisons
keep RULE-01 open. Keep source acceptance and downstream delivery separate.

## 9. Bounded execution proposal for the future implementation

No calculation is released by this documentation update. The next agent should
first submit the corrected derivation, micro-test scripts, assertions, exact
hashes and launch ledger. Once implementation/testing is assigned, use sequential
fresh task-owned processes, aggregate 4 GiB RAM, zero swap, at most 600 seconds
plus 10 seconds termination grace, and fresh host headroom >=2 GiB. Use a shorter
120-second bound for micro/primitive/interface scopes where feasible. The cap
covers the parent and all implicit workers/FORM children, not each child separately.
Never touch the user's kernels. Do not increase any limit automatically.

Proposed initial allocation: R0 and R1 one scope each; A, B, C and D one each;
E up to three declared scopes (construction/exact checkpoint, tensor symmetries,
scalar symmetries/negative controls); F one scope; stored-library/public-package
comparison one scope; boundary/a>=2 controls one scope: **12 scopes maximum**.
Every scope has one fresh parent kernel and at most two explicitly listed
sequential serial-FORM children. This is a proposed ceiling, not a requirement
to run unnecessary scopes. Resolve scripts, counts and staging before release;
do not self-expand the allocation when a calculation fails.

Use FeynGravSimplify/FORM for supported polynomial residuals after a small exact
cross-check. An outer simplifier cannot eliminate the public wrapper's internal
construction/Contract cost. Measure construction, contraction, export, FORM,
import and total scope wall time separately; retain CPU/wall distinctions.

Stop on comparison failure, unexpected warning, source drift, resource event,
timeout or uncertain termination. Preserve evidence and review the cause before
a specifically justified new attempt. Checkpoints from timed-out scopes may be
diagnostic inputs only after checking complete writes, hashes and provenance;
they do not turn the failed scope into an accepted pass. Final acceptance needs
clean, bounded checks covering the full corrected vertex. Staging cannot drop
permutations, structures or required comparisons.

Archive source and reference bytes, scripts, expected assertions, dependency
versions, WXF inputs/results, raw messages, exact residuals, resource samples,
cgroup policy and termination records. Verify protected rules/libraries/models/
notebooks and unrelated work before/after. Read WXF with Import, never Get.

## 10. Closure checklist and documentation delivery

RULE-01 remains open until all applicable items are satisfied:

1. The review's construction defects have small failing-before/passing-after
   regressions and correct derivations, not merely altered expectations.
2. A genuinely independent, validated action reference establishes the full
   primary and representative generalisation fixtures with symbolic D.
3. Every required scalar/graviton symmetry, multilinearity, homogeneity,
   negative, public-interface and failure/cache check passes on the final source.
4. The published/source/library relationships and release impact are explicit;
   no required discrepancy or domain boundary is hidden.
5. Exact final-source/script identities and complete resource/lifecycle records
   support the claims. Tests of earlier source versions are labelled historical.
6. The supported domain is justified and approved; no arbitrary-order or loop
   certification is inferred from finite fixtures or symmetric wrong answers.
7. Repository report, completion/correction plans, rule contracts, documentation
   index, private task note, Known issues, Roadmap, Current status and both AGENTS
   handoffs agree. Historical failures and withdrawn closure claims remain dated.

Deliver reusable tests in an existing appropriate verification location, not a
new top-level Tests directory. Keep this task separate from coefficient/coupling
simplification and PERF-06. A correction-plan update does not authorise commits,
pushes, branch changes, installations or downstream regeneration.
