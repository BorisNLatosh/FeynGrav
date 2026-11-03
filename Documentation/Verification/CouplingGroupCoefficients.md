# Canonical kinematics and coupling groups (PERF-07)

## PERF-07 accepted — complete-call repair, 3 November 2026

The user's unmodified full triangle call now completes in serial FORM (**32.208 s**)
and TFORM with eight workers (**25.420 s**), with all five scalar masters and zero
unresolved coefficients. The same 1 GiB/no-swap/120+10 s/fresh 2 GiB controls apply.
Retained scope peaks are 384,651,264 and 734,560,256 bytes, respectively; these
include the driver and result serialization, not isolated call RSS.

The complete-group path now uses FORM's native named-polynomial gcd_, div_ and
rem_ operations after sorted shared-definition expansion and common-denominator
construction. It avoids the oversized PolyRatFun argument without raising
MaxTermSize. Exact-zero handling, local/job work charges, module ordering and
reciprocal denominator packing are corrected. Legacy arithmetic remains in place.

All five master coefficients agree exactly with retained references over all ten
cubic coupling monomials. Complete master/coupling coverage and altered-coefficient
negative controls pass; serial and TFORM agree exactly. D and off-shell invariants
remain symbolic with only the supplied scalar-product relations. The tadpole
coefficient has 225 expanded terms. This is not an exceptional-limit identity.

Fresh focused/regression checks pass 263 assertions; the prior 10 dependency
assertions remain evidence for their unchanged template. Both QED zero groups and
the exact 6,192-to-694-leaf sample pass. Three-box measured calls after warmup:
27.021/27.330/27.239 s, all 17 masters exactly identical, 4,355,957 bytes and
2,367 shared definitions. Call peaks remain about 319 MB; later verification
scopes peak around 604 MB. No resource events or task-owned survivors.

PERF-07 is accepted within these retained cases and leaves the active list.
Larger QED full-tadpole/1,490-term equality limitations remain unchanged; no
universal simplification or resource guarantee is claimed. Historical failures
and the earlier restricted checkpoint are preserved below and superseded only
by this complete-call evidence. Notebook, rules, libraries, models and unrelated
documentation work are preserved. No commit or push.

Report: `Documentation/Verification/CouplingGroupCoefficients.md` and adjacent JSON.
Evidence: `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-11-03-perf07-rational-repair/`.

## Repair mechanism and exact comparison

The failed fourth complete group in the first nine-group master is proportional
to zeta^3/Lambda3^9. Its old rational addition tried to pack 38,116 numerator words,
2,642 denominator words and seven other words into a 40,000-word FORM term.
The isolated repaired group has 10,133 numerator and 273 denominator terms before
cancellation; a 28-term GCD reduces these to 3,446 and 84 terms. Both polynomial
remainders and internal reconstruction differences vanish. The subsequent full
calls and independent retained-reference comparisons provide the external check.

Reachable immutable definitions are expanded once per job, in dependency order.
One shared factor per term is exposed and sorted at each layer; literal dimension
denominators are included. Native content extraction retains negative powers when
forming the common denominator. Numerator and denominator remain named expressions
for GCD and exact division. Zero is handled before content extraction. The output
packs bracketed numerator chunks and one reciprocal of the **complete** denominator;
it never turns a denominator sum into a product of reciprocals.

The 1,000-named-input-term/degree-48/12-variable admission profile remains private.
Definition size, 48 expansion layers, 100,000 intermediate terms and a denominator
below 4,096 words provide additional predictable rejection points. Native operations
charge five units to the existing local and job work allowances. Rejected work keeps
the original coefficient; actual FORM errors remain reported failures, never silent
retries. These selectors do not guarantee memory use. No public option, mapping
version, production Mathematica Simplify/Together/Factor call or formula changed.

The public-call arguments and retained integrand are unchanged, and the generated
mapping matches input digest `e8eea1b219d6732e256e53ed93bfd41475fa0cb880d5aae3a49bf13cd95856a9`.
Engine/thread selection and KeepFiles/working-directory defaults select each
backend and retain evidence; they do not alter algebra. No selectedMaster filter
is present. Call timings exclude package load and post-call WXF serialization;
the reported memory scopes include those activities.

The five exact comparisons use A0, three B0s and C0 over all ten cubic coupling
monomials in lambda, eta and zeta: 50 zero rational residuals, 50 nonzero altered
controls and 50 serial/TFORM equalities. Reconstruction proves complete coupling
coverage, and a separate homogeneous-degree-one test proves complete coverage in
the five master functions for both results and the retained reference. D is
symbolic; s1,s2,s3 remain generic under the user's supplied scalar-product relations.
The 225-term tadpole is exactly equal to the retained 220-term assembled reference;
this term-count difference reflects representation. The original memory-heavy
12,807-term comparison was not repeated.

## Fresh regression and performance checks

| Suite | Fresh assertions |
| --- | ---: |
| Scalar coupling groups | 57 |
| Final rational coefficients | 59 |
| Scalar-master reduction | 91 |
| Shared output/import | 48 |
| Retained QED zeros/sample | 8 |
| Total | 263 |

The unchanged dependency-registration template retains its earlier 10 passing
assertions; they are not counted again. The deliberately partial scalar-master
fixture has its expected message recorded separately. Unexpected diagnostic
streams are empty. No cap/OOM/swap event or surviving task process was found.

The saved three-box workload uses the actual public facade and normal cleanup;
a tiny evidence-copy hook preserves the job before deletion. All 17 master
functions are exactly identical to the retained reference, with sensitive negative
controls. Output stays at 4,355,957 bytes and 2,367 shared definitions.

| Run | Full call (s) | Export (s) | FORM wall (s) | Import (s) | Call peak (bytes) | Verification scope peak (bytes) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| WARMUP | 29.001 | 1.656 | 19.279 | 4.971 | 319463424 | 603885568 |
| TRIAL1 | 27.021 | 1.616 | 17.874 | 4.649 | 319512576 | 603385856 |
| TRIAL2 | 27.330 | 1.726 | 18.180 | 4.566 | 319688704 | 603652096 |
| TRIAL3 | 27.239 | 1.613 | 18.026 | 4.687 | 319430656 | 603553792 |

The preceding measured calls were 28.812/31.078/31.829 s. Current samples retain
the under-60-second target, output size and memory improvements; this small
comparison does not establish a general speedup. Stage times omit setup, conversion
and cleanup and therefore do not sum to the full call. Later retained-reference
import and serialization account for the larger verification-scope peak.

## Retained failures and limitations

The original failed job and selected-master-only checkpoint remain retained.
Within the repair evidence, GROUP timed out during unsorted expansion; GROUP2
had a diagnostic-format error; GROUP3/4 exposed omitted literal denominators;
GROUP5 was stopped after a nested-reference expansion loop was diagnosed. GROUP6
passes the corrected isolated arithmetic. Tiny PROBE failures are retained.
FOCUSED ran an earlier uncorrected draft; FOCUSED2 exposed invalid denominator
packing. FOCUSED3 passes after explicit numerator bracketing and preserving the
whole denominator. None of those failed runs contributes to accepted counts.

The approval-service interruption, pending candidate and all raw source/script
hashes, FORM/Wolfram logs, process/resource observations and termination records
are preserved. The notebook SHA-256 throughout this repair is
`5524d9228be0a2d61bfe7d6d434efa4bf3d1dc9b4e2ccccfbfe76989a03f1a93`.
The earlier checkpoint's different notebook hash is historical, not overwritten
evidence. The current run did not edit or evaluate the notebook.

Acceptance concerns the retained generic-kinematics cases and affected software
paths. It does not prove singular limits, arbitrary coefficients, all amplitudes,
or larger-QED full-tadpole/1,490-term equality. MaxTermSize and resource limits were
not raised. No commit or push was made.

## Historical verification records

The following records describe earlier checkpoints. Their then-open assignments
and acceptance withdrawals are superseded by the completed repair above; their
raw evidence and limitations remain historical records.

## PERF-07 reopened — full-call failure, 3 November 2026

The user's unmodified triangle call fails in TFORM 5.0.2 (8 workers) after
259.197 s, with exit code 1: `PolyRatFun doesn't fit in a term`. The log gives
38,116 numerator words, 2,642 denominator words and 7 remaining words against
`MaxTermSize=40000`. This is a native term-size failure during arithmetic, not
an import failure, a demonstrated total-memory event or a typo in the call.
The repaired reference union is present in the failed export.

After normalising output paths, the failed export differs from the previously
verified program only by removing the `selectedMaster == 2` restriction around
`cfcPGFinal(piece)`. The full call starts a nine-group master and fails during
the fourth complete-group attempt, before any complete output group is written.
The earlier seven-group tadpole recovery, mathematical comparison of all five
reconstructed masters, 273 regression assertions and three-box measurements
remain valid **within that restricted scope**. They do not certify final
simplification of all masters in the ordinary call. Closure of PERF-07 is
withdrawn; further production acceptance requires that full path.

Next: isolate the failed complete group from this retained export, examine
intermediate rational growth and choose a conservative admission/fallback or
native arithmetic improvement. Preserve the complete original on predictable
rejection, never silently retry an actual FORM failure, and do not blindly raise
MaxTermSize or total resource limits. Then check the unmodified full call in
serial FORM and TFORM, exact all-master equality, tadpole compactness and the
existing QED/three-box gates. No new calculation or code/notebook edit was made
in this diagnosis. Default bounded-run requirements still apply.

Evidence: `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-11-03-perf07-full-call-failure/`;
original job `/tmp/calcform-job-49cac000-1054-4a0f-aaf6-9151fca44f49`.

## Earlier selected-master verification — 3 November 2026

The restricted verification below passed, but full-call acceptance is withdrawn. Changes remain
uncommitted on `FeynArts_Integration`, based on `02444ff`.

The dependency-registration defect is repaired: FORM sorts the reference union
before removing its coefficients, scoped only to the bookkeeping expression.
A root that is also a dependency no longer doubles a shared definition (D-1).
The other agent's complete-group retry and its separate 1,000 named-term,
degree-48, 12-variable profile are preserved; the legacy guards are unchanged.

All seven retained Horndeski groups and the complete 225-term tadpole coefficient
agree exactly with the saved references. All five triangle masters agree: the
other four were checked over all ten cubic coupling monomials, with exact
reconstruction coverage and negative controls. No on-shell or numerical
substitution was used in those comparisons; symbolic D is retained.

The six focused/regression suites pass 273 assertions (10 dependency, 57 coupling,
59 rational, 91 scalar-master, 48 sharing, 8 QED). Both retained QED zero groups
vanish; the sample is exact and shrinks from 6,192 to 694 leaves.
Three-box measured full calls after warmup: 28.812/31.078/31.829 s, all 17 masters
exactly identical, 4,355,957 output bytes, 2,367 shared definitions and
319–321 MB call peak. Separate verification/import/serialization scopes peak
at about 604 MB (708 MB warmup); this is not call memory or kernel RSS.

Every scientific scope kept 1 GiB, no swap, 120+10 s and fresh 2 GiB headroom.
No resource event or task-owned survivor. Notebook, interaction rules, libraries,
models and unrelated work are preserved. No commit or push.

## Repair and regression

The old `DropCoefficient` ran on incoming union terms before FORM sorted them.
Overlapping roots and children then combined into coefficients greater than one.
The output loop copied that multiplicity into the corresponding shared polynomial.
The defect predates the complete-group retry; it was present in `02444ff`.

`LoopDenominatorGroups.frm.in` now sorts the union first, selects only that
bookkeeping expression and removes the collected multiplicities. Definitions,
metadata, dependency order and the public format are preserved. The finite
closure still follows only reachable dependencies; no global polynomial expansion
or mathematical rescaling is introduced.

`Tests/SharedDependencyRegistration.wls` extracts the actual production closure.
Its fixture covers overlapping parents, a root also reached as a child, and two
dependency levels. The old-code control doubles three definitions and fails the
set identity; serial FORM and TFORM with the repair preserve all four definitions.
The suite is registered in `Tests/run.py` and passes 10 assertions. The retained
zero-arithmetic triangle control independently restores `cfcShare(3)=-1+cfs1`.

Results produced by the defective scalar-group path in `02444ff` should be
recomputed with the repaired converter. There is no valid universal rescaling
of previously saved amplitudes. The user's notebook was not opened for execution
or edited during this work.

## Exact mathematical evidence

The saved selected-master FORM export was changed only by the production closure
repair and isolated output paths. All seven complete groups pass native arithmetic;
no denominator fallback, work restoration or unreported retry occurs. Internal
counters report 109,648 ms rational work and 31 ms factor work; these counters are
not substituted for the separately retained wall-clock observations. Output is
569,006 bytes, and the FORM-only scope peak is 173,461,504 bytes.

Each of the 95/175/262/1825/2654/3686/4110-term source groups agrees exactly with
its retained compact `together.wxf` reference. The complete coefficient expands
to 225 terms and is exactly equal to the previously verified 220-term assembled
reference; the differing term count reflects equivalent representation. Every
altered-coefficient control is nonzero. The original 12,807-term comparison was
not repeated.

For B0(s1), B0(s2), B0(s3) and C0(s1,s3,s2), the ten degree-three monomials in
lambda, eta and zeta were compared separately, retaining the symbolic scales.
All 40 rational residuals are exactly zero, all 40 altered controls are nonzero,
and reconstruction of each complete coefficient proves no coupling terms were
omitted. Together with the tadpole identity this establishes all five retained
master coefficients. The largest of these comparison scopes peaks at
595,697,664 bytes. This exact decomposition replaces the earlier inconclusive
monolithic comparisons; it does not rely on numerical sampling.

## Regression and performance

| Suite | Assertions |
| --- | ---: |
| Shared dependency registration | 10 |
| Scalar coefficient groups | 57 |
| Final rational coefficients | 59 |
| Scalar-master reduction | 91 |
| Shared output/import | 48 |
| Retained QED zeros and sample | 8 |
| Total | 273 |

Expected partial-master messages in the scalar-master suite are retained and
checked separately; unexpected diagnostic streams are empty. These counts do not
include historical failed runs or count the same identity twice.

All four three-box calls use the saved workload, serial FORM, the actual public
`FeynGravSimplify` facade and normal cleanup, with a small evidence-copy hook.
All 17 masters are exactly identical to the retained result and altered controls
are sensitive. Output stays at 4,355,957 bytes and 2,367 shared definitions.

| Run | Full call (s) | Export (s) | FORM wall (s) | Import (s) | Call peak (bytes) | Verification scope peak (bytes) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| WARMUP | 28.412 | 1.665 | 18.883 | 4.896 | 320188416 | 707629056 |
| TRIAL1 | 28.812 | 1.810 | 18.710 | 5.242 | 319365120 | 603832320 |
| TRIAL2 | 31.078 | 1.891 | 20.663 | 5.230 | 319586304 | 603672576 |
| TRIAL3 | 31.829 | 1.867 | 21.195 | 5.463 | 320548864 | 604442624 |

The previous measured calls were 28.693/28.557/29.993 s. The present samples
retain the under-60-second goal but do not establish a speedup; the last two are
slower. Stage times do not sum to the whole call because it also includes setup,
final conversion and cleanup. Call memory remains below 110% of the approximately
319 MB baseline. The larger verification-scope peak occurs after the call while
importing the retained WXF for exact comparison; the earlier measured harness
deferred its result comparison. Scope and call memory must not be conflated.

## Limits, retained failures and evidence

Acceptance is bounded by the retained cases. The selected-master FORM payload
finished in 111.759 s (including its small driver/check overhead); this is not a
whole five-master triangle timing or a promise that every coefficient will fit
the limits. The saved export runs final grouping on the selected tadpole; the
four other reconstructed masters were separately verified exactly. The larger
QED full-tadpole/1,490-term equality limitations remain unchanged. No universal
simplification, arbitrary-kinematics, or new memory guarantee is claimed.

The first tiny FORM payload completed correctly, but its checker expected a
semicolon absent from FORM's printed definitions. The parser was corrected using
the saved log, without rerunning FORM. Its original failed wrapper is retained;
the installed portable suite independently passes afterward. The initial all-five
expanded comparison was inconclusive, not a proven mismatch. A first bubble
comparison checked only the seven tadpole monomials; its reconstruction check
correctly rejected incomplete coverage. The corrected ten-monomial checks above
supersede that harness. No production arithmetic repair or raised limit was
needed for these harness corrections.

Evidence root:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-11-03-perf07-registration-repair/`.
`reviewed-results.json`, `performance-results.json`, source/script manifests,
raw FORM/Wolfram logs, exact residuals, negative controls, fresh-memory gates,
cgroup resource samples and termination records are retained. Earlier evidence
roots and failed attempts remain unchanged. The prior public report is preserved
at `before/Documentation/Verification/CouplingGroupCoefficients.md` in that root.
The adjacent [verification manifest](CouplingGroupCoefficients-2026-11-03.json)
records the tested identities and final source hashes.
