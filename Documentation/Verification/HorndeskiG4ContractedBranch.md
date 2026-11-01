# Horndeski G4 contracted branch — verification history and current review

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

## Historical execution state — 1 November 2026: paused before corrected-reference checks

The current uncommitted G4 source is SHA-256
`c9e2708da14979a49e1adbfee04d4161b75e8f87b0fe54b8611df05b46ae21ed`;
`Libs/HorndeskiG4_0_2_2` remains unchanged at
`21e2f6ae24f49cac71afcbd984a42d77e3df44ec3b1378946ccc2ad931442202`.
The source still contains the historical `+b` Hessian action coefficient,
not the corrected `-2b` target above. It is not accepted. Other concurrent
Horndeski source/library deliveries are separate and do not validate G4.

Recent xPert evidence is under
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-31-rule01-xpert-pilot/`:

- X2A19 geometric factors passed 10/10, including scalar-perturbation removal
  and `NoScalar` handling. These are geometric inputs, not a corrected-action
  vertex proof.
- GBC passed 16/16 and established exact equality of the Gate B curvature
  block with production, with literal-zero residual. This does not validate
  the Hessian sector or the whole rule.
- A2's 11/11 full comparison and SA2H20's 18/18 Hessian comparison used the
  historical `+b` action. They are retained execution evidence, not acceptance
  evidence for the corrected action.
- X2B8 and X2B22 passed structural reference checks (9/9 and 13/13), but the
  actual public comparisons X2C21 and X2C22 both failed exact equality with a
  195,367-leaf residual. Reference-only structural passes are not Gate B passes.
- The combined block diagnostic timed out after 600 seconds following action
  assembly. Retained peak memory was about 1.50 GB with zero swap and no
  memory event; its task-owned scope was subsequently confirmed empty. The
  split Hessian check failed with the same residual (GBH 12/13). Box and cross
  component checks also failed (GBB 21/22 and GBX 21/22), all using the old
  coefficient and incomplete contracted-index handling.
- The adapter diagnosis includes collisions of contracted `a,b,c,d` as well as
  `v$...` symbols across multiplied factors. Partial alpha-renaming repairs
  are present in the scripts but have not been executed. A later static review
  also found weak dummy/free-index classification, incomplete per-factor
  independence checks, and telemetry/assertion defects. None of these scripts
  is an accepted corrected reference.

Preparation under
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-11-01-rule01-corrected-g4/`
currently contains a fresh bounded runner, a baseline manifest, a harmless
lifecycle payload, and a 12-assertion convention-gate derivation/script.
**Neither the lifecycle check nor any corrected-action scientific check has
run.** The baseline must be refreshed against concurrent non-G4 changes before
execution. No production rule, G4 library, model, or notebook was changed by
this investigation.

Next work is therefore not a continuation of the old public Gate B comparison:
refresh the baseline, validate the runner and convention gate, then have the
swarm prepare corrected `-2b` references with per-factor fresh-index ownership.
Re-establish A and B before deciding any production repair; C–F, library
migration under RULE-05, and final supported-domain documentation remain open.

## Current review — 31 October 2026: RULE-01 remains open

The reviewed action-route replacement was **not accepted**. Its reviewed source
snapshot had SHA-256
`7b22ee2eb7e76ac41dd9c57626f5af2f57db0ae824e3a8803cf2707c61c0479d`,
on `FeynArts_Integration` at HEAD `a7c2a9a` with uncommitted RULE-01 work.
The later review found these blockers in the actual source and test scripts:

1. `actionBuild` repeats one canonical coefficient over scalar permutations
   without applying the scalar permutation to that coefficient.
2. `actionDistinctQ` selects one Q1 and n-1 Q2 fields. At order three it excludes
   Q1 Q2 Q3. The determinant also uses only Q1 in trace1, so the mixed contribution
   of trace1 squared is lost at order two. Repair the field expansion, not only
   the predicate.
3. `actionScalarCurvature` joins two already mixed-index Christoffels through
   another inverse metric. The connection-product definition requires direct
   contraction of their shared upper/lower index.
4. Production and reference geometric/derivative/selection/coefficient helpers
   are identical after renaming private symbols and removing whitespace. Their
   agreement cannot independently validate those common algorithms.
5. The retained ladder does not check adjacent scalar-leg exchanges or complete
   graviton multilinearity. The order-three omitted-T5 test selects order two.
6. The earlier primary stored-library comparison predates this replacement;
   `n <= 3, b <= 4` is not an exhaustively verified or justified domain.

The inspected successful logs really record the reported passes and clean
termination, with no sampled swap or memory-limit events. This remains execution
and consistency evidence, not independent mathematical closure. The old sign
comparison and library agreement remain historical results on their own source
snapshots. They do not certify the current replacement or establish that the
published expressions are wrong.

Follow the [detailed correction plan](HorndeskiG4CorrectionPlan.md). Required
work starts with cheap failing-before/passing-after defect regressions and a
validated independent reference, then the corrected A–F ladder, public/library
comparisons and domain justification. No expensive calculation was rerun by this
review. This documentation update changes no code, library, model or notebook
and creates no commit.

## Historical delivery records

The dated records below preserve the earlier agent's findings, execution counts
and failures. Later closure/acceptance assertions about the action replacement
are superseded by the current review above; do not use them as current status.

## Scope and outcome — 29 October historical delivery

RULE-01 addresses the direct contracted `HorndeskiG4` source rule. The accepted
change covers the confirmed `T3`–`T5` sign defect and the primary
`n = 2, a = 0, b = 2` case, with supported `b = 0/1` controls and the unchanged
public `HorndeskiG4_0_2_2` library comparison. It is a primary-coverage delivery,
not closure of the general `a,b,n` problem.

Production changes are limited to:

- `Rules/HorndeskiG4.wl`: restore the paper-aligned signs of `T3`, `T4`, and
  `T5`; route public calls with `b >= 2` through the existing uncontracted rule
  followed by `Contract`; remove the obsolete public usage restriction.
- `Rules/Contracts.md`: document the public wrapper and the distinct private
  helper restriction.

The private `HorndeskiG4Core` and `HorndeskiG4Core1` helper guards are unchanged:
they still reject direct multi-graviton `b >= 2` evaluation. The public wrapper
avoids that slow helper path. No generator, stored library, native model,
notebook, or public importer was changed. No library regeneration was performed.

Production rule SHA-256:
`e23112b2d27d4c789c9d95fff8ab92e306ee29a6fab71a29cf4f8e16e54f882d`.
The unchanged primary library SHA-256 is
`21e2f6ae24f49cac71afcbd984a42d77e3df44ec3b1378946ccc2ad931442202`.
No commit has been created.

## Confirmed mathematical defects

The current contracted helpers had the following coefficients before correction:

| Term | Defective contracted coefficient | Paper/uncontracted coefficient |
| --- | --- | --- |
| `T3` | `(-b)` | `b` |
| `T4` | `-2 (-b)`, algebraically `2b` | `-2b` |
| `T5` | `(-b)` | `b` |

Thus the entire second bracket was negated relative to FeynGrav 3.0,
section 3.3, equation (42), and to the existing uncontracted implementation.
Git history shows that `ef844e0` changed these three signs from their earlier
paper-aligned values. The discrepancy was also present in the retained 4.1.0
snapshot.

The final sign-gate established exact nonzero residuals for one-graviton
`b = 1`, two-graviton `b = 1`, and one-graviton `b = 2`. In each case the residual
is exactly twice the current second bracket, and subtracting that bracket twice
gives exact agreement with contraction of the uncontracted rule. The `b = 0`
controls agreed without correction. These are exact symbolic-D comparisons with
generic scalar and graviton momenta, not numerical samples.

The public multi-graviton `b >= 2` branch was also missing. The existing
uncontracted rule already contains the general expression and is used by the
library generator. The new public path evaluates that expression and then calls
`Contract`, preserving the existing scalar and graviton permutation sums,
coupling powers, index routing, and multiplicity conventions.

## Accepted checks

- **Sign gate, 20/20 assertions:** exact nonzero residuals and sign relations
  for `n = 1, b = 1`, `n = 2, b = 1`, and `n = 1, b = 2`; paper-aligned
  reconstruction; `b = 0` controls; the original unsupported-`b = 2` failure;
  reference export; directory and external-process checks.
- **Wrapper candidate, 13/13 assertions:** exact agreement with the uncontracted
  rule for supported sign controls and the primary `n = 2, a = 0, b = 2` case;
  κ²; a negative control; and reference export.
- **Production, the same 13/13 assertions:** the production file is byte-identical
  to the tested wrapper candidate and passes every primary and supported-control
  comparison. The primary result has 89,893 leaves and zero exact residual.
- **Public library, 7/7 assertions:** unchanged library hash, successful public
  import, and exact agreement between the public library result and the new
  direct production result after normalising `Global`κ to `FeynGrav`κ.

A separate wrapper timing check passed 4/4 assertions. It measured 2.399813 s for
uncontracted construction and 23.8032 s for contraction of the primary case.
The initial helper-by-helper `b = 2` candidate exceeded 120 seconds and was not
selected.

| Accepted batch | Wall time | Highest retained aggregate cgroup memory peak |
| --- | ---: | ---: |
| Final sign gate | 38.205 s | 1,244,033,024 bytes |
| Wrapper timing | 29.116 s | 1,234,067,456 bytes |
| Wrapper candidate | 66.935 s | 1,271,537,664 bytes |
| Production | 114.768 s | 1,271,336,960 bytes |
| Public library | 6.624 s | 239,546,368 bytes |

These are retained peaks, not process RSS or a guaranteed final peak. Accepted
batches used whole-tree 4 GiB RAM and zero swap. Most used 120 seconds plus 10
seconds termination grace; generalisation attempts used the separately approved
600-second bound. Fresh host available memory exceeded 2 GiB before launches.
No accepted batch had a resource event, surviving owned calculator, or external
FORM process. Public package loading emitted the pre-existing `name::shdw`
warning described below; the accepted mathematical batches otherwise had no
unexpected payload diagnostics.

## Errors found and corrections made

All failed or diagnostic attempts below are retained in the evidence and are not
counted as accepted checks.

1. **Initial 1 GiB OOM failure.** The sign-gate rule launched multiple Wolfram
   worker kernels through its parallel-map helper and hit the whole-tree 1 GiB
   OOM limit. No result was produced. The maintainer authorised 4 GiB for the
   current RULE-01 batches; production parallel behaviour was not changed.
2. **Missing scalar permutations in the harness.** Six sign-relation checks
   failed because the reference second bracket omitted the scalar permutations
   used by the public rule. The harness was corrected to include them.
3. **Incorrect residual sign in the harness.** The exact relation is
   `residual = 2 * currentSecondBracket`, not `-2 * currentSecondBracket`.
   The assertion was corrected after deriving it from the public assembly.
4. **Missing graviton-leg permutations in the harness.** The two-graviton second
   bracket also required the `Core1` graviton permutations. The final harness
   uses the full tuple of scalar and graviton permutations and then passed.
5. **Incorrect helper contexts in a diagnostic.** A manual reference used
   `HorndeskiG4`Private`CTensorGeneral` and
   `HorndeskiG4`Private`GammaTensor` instead of the correct package contexts.
   These were corrected before any term-level conclusion was drawn.
6. **Incorrect WXF reading method.** `Get` was replaced by `Import[..., "WXF"]`
   for the saved expression comparison.
7. **Slow first candidate.** Direct helper-by-helper evaluation of the primary
   case exceeded 120 seconds. It was replaced by the public uncontracted-then-
   `Contract` wrapper, which completed the primary case.
8. **Public κ-context mismatch.** The first public-library comparison mixed
   `Global`κ with `FeynGrav`κ and produced a residual plus shadow warnings. The
   reference was normalised before loading FeynGrav, after which the residual
   was exactly zero.
9. **Pre-existing `name::shdw` warning.** Diagnostics showed no `Global`name
   before FeynGrav loading; the warning is introduced during FeynGrav loading
   itself. It is unrelated to RULE-01 and was not fixed as part of this task.
10. **Generalisation timeouts.** Combined `n = 3, b = 2` candidate/reference
    evaluation exceeded 120 seconds. The reference-only stage also exceeded 120
    seconds and then the separately approved 600-second bound, producing no
    result. The 600-second run's post-run integrity check additionally detected
    the `Rules/Contracts.md` edit made while that run was active, so the run is
    not accepted evidence. No further time increase or retry was made.

The `a > 0` and `b > 2` controls were prepared but not executed. No arbitrary-`n`,
arbitrary-`b`, complete-library, or independent full-action proof is claimed.
The public library comparison is regression evidence, not an independent
rederivation of the stored library.

## Remaining limits and review pointers

The 30 October review supports the narrow primary repair as a consistency
result. RULE-01 remains open for independent action validation and broader
`n`, `a`, and `b` coverage. Follow the [completion plan](HorndeskiG4CompletionPlan.md)
for the remaining derivation, fixture ladder, compatibility checks and bounded
execution. Planning and review do not count as new mathematical test results.

Preparation and progress notes:
`/home/latosh/Google Drive/Dr Boris Latosh Mind Vault/FeynGrav/Development/Horndeski G4 contracted branch.md`.

Evidence roots include:

- `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-29-rule01-sign-gate-4g-r4/`
- `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-29-rule01-wrapper-candidate-4g/`
- `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-29-rule01-production-4g/`
- `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-29-rule01-public-4g-r2/`
- `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-29-rule01-n3b2-reference-600s/`

Earlier failed-run and diagnostic directories are retained alongside them.

## Independent review and continuation — 30 October 2026

The review inspected the actual source diff, scripts, hashes, assertions and raw
resource/diagnostic records. The current rule matches the tested candidate. The
relative `b,-2b,b` signs follow from expanding the two scalar Hessians, consistent
with [the action in section 3.3](https://arxiv.org/html/2406.14872#S3.SS3).
However, the retained computational references use the existing uncontracted
implementation; the public library has the same ancestry. Their exact agreement
is regression/consistency evidence, not an independent derivation of every term
in the published vertex. No complete independent action comparison was added by
this review, and no new Wolfram or FORM calculation was run.

The failed 600-second reference run's observer record ends while its scope is
still deactivating and lists owned kernels at that instant. A read-only check on
30 October found none of those same PID/start-time identities still present.
This does not retrospectively establish termination within the recorded grace
period. Future runs must retain the final cleanup confirmation; the failed run
remains unaccepted. Successful-batch lifecycle evidence is unchanged.

The completion plan preserves the primary evidence and requires the three
remaining `(n,a,b)` fixtures `(3,0,2)`, `(2,0,3)` and `(2,1,2)`, independent
normalisation checks, explicit symmetry/negative/interface checks and an accurate
supported-domain statement. RULE-01 is not resolved by this documentation update.

## Independent action Gate A — 30 October 2026

The new reference independently expands the determinant, inverse metric,
Christoffel symbols, Ricci scalar and scalar Hessians from the covariant action;
it does not call `CTensorGeneral`, G4 routines/private helpers or stored G4
libraries. Diagnostics corrected its Ricci contraction, ordered scalar-pair
multiplicities, Hessian dummy-index ownership and overall `I (-1)^(b+1)`
convention. These were evidence-side corrections only.

The final bounded Gate A run passed **21/22 assertions**. `(1,0,1)` agrees
exactly with the public rule, all candidate symmetry and negative controls pass,
and the serial-FORM zero/nonzero pilot passes with separately measured export,
FORM and import stages. `(2,0,1)` does not agree: `A2Exact` fails with a resolved
nonzero residual of 2,894 leaves. Applying total incoming-momentum conservation
does not remove it. A separate pure-curvature diagnostic also remains nonzero,
so the mismatch is not isolated to the Hessian sector. This is an unresolved
comparison, not yet evidence justifying a production curvature edit.

Gate B, the public boundary scope and the later C–F fixtures were not run. The
final Gate A scope had one parent kernel and two serial-FORM children, no resource
events or surviving owned processes, zero swap, and a highest retained aggregate
peak of 1,287,839,744 bytes under the 4 GiB/120+10 s limits. Protected hashes and
repository state matched before/after that scope. Separate PERF-05 drift during
the wider investigation was pinned or recorded; no foreign edits were reverted.

Full corrections, failed diagnostics, timings and lifecycle details:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-30-rule01-independent-preparation/ExecutionReport.md`.
Earlier failed Gate A runs are retained alongside it; additional diagnostics are
in the sibling `2026-10-30-rule01-independent-diagnostic/` directory. RULE-01
remains open, and no production, library, model or notebook change resulted from
this independent-action batch.

## A2 resolution, action-derived rule, and initial gates — 31 October 2026

The preceding 21/22 record is historical. Subsequent structure-resolved checks
showed that the independent reference and the then-current production rule really
differed: FORM returned the same 2,894-leaf nonzero residual, and total momentum
conservation did not remove it. `T4` alone agreed; `T1`, `T2`, `T3`, and `T5`
did not. This motivated an action-derived public route rather than another sign
adjustment.

A disposable action-routed wrapper passed Gate A 22/22. The production rule now
contains an independent determinant/inverse-metric/Ricci/Hessian expansion and
uses it for `n <= 2, b <= 4`, in both contracted and uncontracted public calls.
Existing higher-graviton and out-of-range paths remain. The final construction
computes one canonical scalar assignment and applies scalar permutations after
assembly. Stored libraries, models and notebooks were not changed or regenerated.
Final production SHA-256:
`05a699ac647944218a6df50ba632768a846edea825095baefd4104385230d83d`.
The reviewed source is retained in the evidence directory.

Final accepted checks:

- **Gate A, 22/22:** exact independent action equality for `(1,0,1)` and
  `(2,0,1)`, symbolic D, generic off-shell momenta, symmetry and negative controls,
  and two serial-FORM controls. Reference/candidate construction took
  1.238117/2.925069 seconds for the two-graviton case.
- **Gate B, 12/12:** exact full `(2,0,2)` equality, actual public-wrapper execution,
  symmetry, wrong-sign and omitted-`T5` controls. Reference construction and
  contraction took 4.64346 and 3.189616 seconds; candidate construction took
  7.838442 seconds. The candidate has 14,436 leaves and zero residual.
- **Boundary, 7/7:** direct eight-pair helper rejection, then an instrumented
  public `b=5` failure with `Permutations` replaced by identity, avoiding `10!`
  assignments. A valid call after the failure succeeds. This documents the current
  `b <= 4` implementation boundary, not verification of every case within it.

The failed Gate B attempts are retained: repeated-structure construction hit the
4 GiB OOM limit; two later attempts timed out at 600 seconds; a `SeriesData`
implementation and a `Coefficient` implementation incorrectly returned zero and
were rejected. The accepted per-assignment-then-canonical construction passed
within 80.538 seconds for the complete Gate B scope, with no resource event or
owned survivor. Gate A and Boundary scopes also had clean termination and
protected-file integrity. Boundary's highest retained aggregate peak was
1,348,071,424 bytes.

Exact component fixtures independently validated the second-order formal and
exact-metric expansions after correcting harness index placement and series
handling. Fixed-dimensional component results are diagnostic only; the accepted
closure evidence is the symbolic-D action comparison above. The public wrapper's
internal `Contract` was not bypassed by an outer simplifier.

RULE-01 remains open for gates C, D and E: `(2,1,2)`, `(2,0,3)` and `(3,0,2)`.
No general arbitrary-`n`, arbitrary-`b`, stored-library or full-model validation
is claimed. Full failed-attempt and lifecycle details are appended to
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-30-rule01-independent-preparation/ExecutionReport.md`.

## Reported generalisation and withdrawn closure — 31 October 2026

The agent claimed resolution for `n <= 3, b <= 4`; **the review above withdraws
that claim**. The action
expansion now includes third-order inverse-metric and determinant terms and the
third graviton label. Production SHA-256:
`7b22ee2eb7e76ac41dd9c57626f5af2f57db0ae824e3a8803cf2707c61c0479d`.
Stored libraries, models and notebooks remain unchanged.

Accepted generalisation checks:

- **C `(2,1,2)`, 12/12:** exact action equality and scalar-multiplicity controls.
  Reference construction/contraction 5.95452/3.169536 seconds; candidate
  9.235604 seconds.
- **D `(2,0,3)`, 12/12:** exact higher-`b` equality and independent omitted-`T5`
  control. Reference construction/contraction 29.588179/5.256747 seconds;
  candidate 33.890105 seconds. An earlier attempt reached exact zero before
  timing out in the legacy private-`T5` control, which was replaced by the
  independent two-Christoffel reference block.
- **E `(3,0,2)`, complete in staged checks:** exact action equality with zero
  residual, then pair symmetry 6/6, first-leg symmetry 5/5, and third-leg plus
  negative controls 7/7. Reference construction/contraction 111.199024/61.236094
  seconds; candidate 171.829520 seconds. Earlier full scopes timed out after the
  exact comparison, so these results were retained in a checkpoint and completed
  in separate bounded scopes rather than discarded or recomputed unnecessarily.
- **Validation, 10/10:** wrong arity, malformed inputs, negative/noninteger `b`,
  too few scalars, dependency-failure propagation and no failure memoisation.
- **Cache, 4/4:** identical-call reuse and a different input not served from that
  cache, checked through `DownValues` without production instrumentation.

The independent reference and public wrapper agree exactly at symbolic D with
generic off-shell momenta. No outer simplifier bypassed the wrapper's internal
`Contract`. Final scopes had protected-file integrity and no surviving owned
calculators. Earlier OOM, timeout, invalid-zero and symmetry-harness failures are
retained and are not counted as passes.

The withdrawn closure did not certify arbitrary `n` or `b`, stored-library ancestry,
higher-order models or loop amplitudes. No commit was created. RULE-01 remains open; separate project
work includes: API-02, PERF-06, LIMIT-01/02, RULE-02,
PERF-02/VERIFY-02, CONTRACTOR-01/02/03 and FA-QG-01.
