# Horndeski implementation audits and correction plan

1 November 2026. **RULE-01–RULE-05 completed within their individual scopes: [G2/G3/G5 audit](HorndeskiAudit-2026-11-01.md), [G4 source](HorndeskiG4-2026-11-01.md), [shipped libraries](HorndeskiLibraryMigration-2026-11-01.md), and [G5 higher orders](HorndeskiG5HigherOrders-2026-11-01.md).**
No source edit, kernel run, library regeneration, model rebuild or commit is
authorised by this document alone. Preserve the accepted uncommitted RULE-01
source, notebook edits and all historical evidence, including the rejected candidate.

The mathematical target is the [Horndeski model page](../Models/Horndeski.md#action),
with signature (+---), the common curvature convention, positive Fourier phase,
and $X=g^{\mu\nu}\partial_\mu\phi\partial_\nu\phi$ without a half. In particular,
the G4 Hessian coefficient is $-2G_{4,X}$ and the G5 cubic-Hessian coefficient
is $+G_{5,X}/3$. The model page derives the metric/function translation from
Kobayashi–Yamaguchi–Yokoyama. The old printed coefficients $+G_{4,X}$ and
$-G_{5,X}/6$ are not the acceptance reference in these conventions.

## Separate tasks and dependencies

| ID | Scope | Completion requirement |
| --- | --- | --- |
| RULE-03 — accepted within audited scope | Shared convention/reference checks and independent G2/G3 audit; correct demonstrated defects | Frozen reference conventions, independent unit-coupling derivation, supported-domain exact checks and justified source repairs or a documented no-change result |
| RULE-01 — source accepted | Repair the rejected G4 construction and establish its contracted/uncontracted rules against the corrected G4 action | All correction-plan gates, using the new action target; no acceptance based solely on agreement with the publication or existing library |
| RULE-04 — accepted within audited scope | Audit and correct G5 within its currently supported domain, including the cubic-Hessian coefficient and b = 0 controls | Independent full-tensor comparisons and structural/failure checks; preserve the independently checked John result |
| RULE-05 — accepted for shipped selections | Reconcile stored Horndeski libraries, ordinary imports and affected downstream consumers with accepted sources | Every changed library independently checked; affected consumers checked; unaffected files preserved; scientific migration notes and exact provenance |
| RULE-02 — accepted source extension | Extend G5 TII beyond three gravitons | All-order connection-class derivation; complete n4,a0,b1 action equality; selected higher T8 sectors and bounds; see the separate higher-order report |

The common convention/reference gate and RULE-01/RULE-03/RULE-04 source work
are accepted in their recorded scope. RULE-05 has completed the G4 migration
and reconciled all 60 shipped files. RULE-02 subsequently extends the G5 source
within its inverse-metric bounds; no further library or native-model changes.
Each task has its own evidence and closure; passing one does not close the others.

## 1. Establish independent references and cheap controls

Pin the branch, HEAD, dirty source, scripts and all 60 installed Horndeski
library hashes before implementation. Recheck the inventory rather than assuming
the documentation's count remains current. Use unit monomial couplings and
generic incoming momenta with symbolic D. Keep the metric/signature translation
separate from the Fourier map, differentiation multiplicities and dummy-index
normalisation.

The reference must derive the inverse metric, determinant, connection,
curvature and Hessians independently of production helpers. Use labelled
external fields and extract each label once; or provide a separately justified
equivalent derivation. Reusing renamed production code is not independence.
Validate primitive expansions before expensive full vertices.

Required inexpensive identities, modulo explicitly discarded total derivatives:

- Canonical scalar: $G_2=X/2-m^2\phi^2/2$ agrees with the minimal-scalar action.
- $G_3=f(\phi)$ is equivalent to $G_2=-f_{,\phi}X$.
- $\mathcal H_2\simeq R_{\mu\nu}\partial^\mu\phi\partial^\nu\phi$ in the stated curvature convention.
- $\Delta G_4=-\beta X/2$ and $G_5=-\beta\phi$ separately give the same John interaction, including its independently checked normalisation.
- Constant G3 and constant G5 are boundary terms; b = 0 differentiated-function terms are omitted before any negative power of X is formed.

At the vertex level, boundary identities use total incoming momentum
conservation. Never impose on-shell, helicity or D = 4 conditions to hide an
off-shell discrepancy. Numerical projections may guide diagnosis but cannot
replace full symbolic comparisons. Include a wrong sign, missing factor of two
and omitted nonzero action block as negative controls where applicable.

## 2. RULE-03: G2/G3 supported-domain audit

Audit both direct variants, generator selection and ordinary four-argument
imports. Check the coefficient convention, scalar differentiation/permutations,
G2's restored graviton n!, G3's connection contribution, incoming Fourier phases
and coupling insertion exactly once. Keep the existing public signatures.

Start with one graviton, then two. Include b = 0/1 where meaningful, a > 0 and
a >= 2 to distinguish differentiated and undifferentiated slots, and b > 1 to
exercise multiple kinetic factors. Cover each shipped (a,b) selection with
independent evidence or a proved reduction to tested structural cases; list
any excluded cases rather than certifying all orders. Check flat scalar-only
limits by a suitable independent construction even where the public interface
does not provide a zero-graviton library.

Inspect original formulae before assuming G2/G3 require changes: unlike the
G4/G5 relative-Hessian coefficients, a general G2/G3 implementation defect is
not established by the convention translation alone. Preserve valid rules and
performance behaviour. Record an audit with no formula change if they pass.

## 3. RULE-01: completed G4 source repair

The [acceptance report](HorndeskiG4-2026-11-01.md) records the delivered construction,
complete checked vertices, interface tests, analytical domain and optional b5
timeout. The requirements below are retained as the completed plan; they do not
authorise repeated calculations or the separate library migration.

Use the detailed [G4 correction plan](HorndeskiG4CorrectionPlan.md) for the
known permutation, field-labelling, curvature and reference defects. Replace
its historical action target by

$$
\lambda\sqrt{-g}\,\phi^a
\left[X^bR-2bX^{b-1}\mathcal H_2\right].
$$

For the two-Hessian block, expansion into zero, one and two connections has
relative coefficients $-2b,+4b,-2b$ before Fourier factors. These replace the
historical $b,-2b,b$ action coefficients. Do not multiply the whole vertex by
-2: the curvature contribution and phase bookkeeping require separate treatment.

Retain the correction plan's cheap defect regressions and A–F ladder, the
a > 0 scalar-slot tests, all adjacent scalar/graviton exchanges and the true
order-three omitted-T5 control. Add the independent G4-to-John comparison.
An unchanged stored-library mismatch can be an expected result of a justified
repair: classify it and send it to RULE-05 instead of forcing the source to
reproduce the old expression. No old reported closure is reinstated.

## 4. RULE-04: G5 supported-domain audit

Derive both the Einstein-tensor–Hessian and cubic-Hessian pieces from

$$
\lambda\sqrt{-g}\,\phi^a
\left[X^bG^{\mu\nu}H_{\mu\nu}+\frac b3X^{b-1}\mathcal H_3\right].
$$

Trace the normalisations through TI and TII and both public variants; do not
apply a text replacement to a prefactor without deriving the complete tensor.
Audit curvature contractions, each Hessian connection term, every external
permutation, scalar-slot assignments and successful-cache ownership.

Begin with b = 0, including a = 1 (John) and the stored a = 2,3 cases. For
b > 0, cover the stored (a,b) = (0,1),(1,1) cases at one and two gravitons.
Then establish the claimed direct three-graviton boundary on a separately
budgeted fixture and retain the rejection beyond it. The absence of the TII
guard on b = 0 is not proof of arbitrary-order correctness. Do not extend
TII in that supported-domain audit; the subsequently completed RULE-02
source extension has its own derivation and finite verification record.

## 5. RULE-05: libraries and consumers — completed

The [acceptance report](HorndeskiLibraryMigration-2026-11-01.md) maps the requirements below to
regeneration, independent/public comparisons, recovery and consumer evidence.
The numbered requirements are preserved as the completed contract.

After the relevant source is accepted:

1. Make a per-file impact table for the 16 G2, 20 G3, 14 G4 and 10 G5 libraries
   in the current inventory. Classify changed, unaffected or not yet verified.
2. Generate only affected libraries into a fresh candidate directory. Compare
   each candidate with the independent action result and its accepted direct
   source, then test the four-argument public import with a non-unit coupling.
3. Test import failure/retry, provenance and unchanged signatures as appropriate.
   Preserve unaffected library hashes and retain original affected expressions.
4. Trace actual consumers. Rebuild a native bundle only if an affected rule feeds
   it. In particular, preserve/recheck the John b = 0 identities; do not assume
   all scalar models use the general G4/G5 functions. Independently generated
   EsGB rules are a separate sector.
5. Check affected complete tree amplitudes, channel/weight inventories and Ward
   identities within an explicit scope. No blanket native-loop certification.
6. Update public examples only where needed under the authorised editing scope;
   preserve manual edits and historical saved results. Explain which old
   amplitudes need recomputation. Do not silently replace user research output.
7. Update source/library/model provenance, release notes and verification reports.
   Describe changes to mathematical results, conventions and any conversion of
   old couplings. A scientific correction is not merely a notation change.

No public importer should compensate for a wrong stored tensor by a hidden
global factor. Compare old versus new expressions, but require equality to the
independent corrected reference rather than preservation of a demonstrated error.

## Verification and resource contract

Require exact symbolic-D off-shell residuals, resolved dummy-index comparison,
all relevant scalar/graviton permutations, graviton multilinearity, scalar and
κ homogeneity, sensitive negative controls, and honest supported-domain bounds.
Check invalid inputs, unsupported orders, failure/abort propagation and cache
behaviour separately from physical identities.

Prepare small sequential batches with pinned sources/scripts and predeclared
assertions. The default remains whole-process-tree 1 GiB, zero swap, 120 seconds
plus 10 seconds grace and fresh 2 GiB host headroom. Historical RULE-01 exceptions
are not automatic new budgets. Stop and retain evidence on unexpected warnings,
resource events, ambiguous comparisons or production failures. Never touch user
kernels. Use FORM through the existing supported simplification interface when
appropriate, without treating algebra-backend agreement as independent physics.

The original registration changed documentation only. The subsequent
[audit delivery](HorndeskiAudit-2026-11-01.md) records the independent checks,
G2/G3/G5 source corrections, four regenerated G5 libraries and explicit limits.
That earlier delivery deferred G4. The later [RULE-01 delivery](HorndeskiG4-2026-11-01.md)
resolves the source repair and classifies every old G4 library. The subsequent
[RULE-05 migration](HorndeskiLibraryMigration-2026-11-01.md) regenerates and verifies those libraries
and reconciles the consumer scope. No model/notebook rewrite is included.
