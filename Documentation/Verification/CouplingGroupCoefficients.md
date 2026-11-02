# Canonical kinematics and coupling groups (PERF-07)

Checked 2 November 2026. Production mechanisms implemented; the retained
Horndeski coefficient's compact-result acceptance remains open. PERF-05's
previously accepted scope is unchanged. No notebook, interaction rule, library
or FeynArts model was changed.

## Change

The scalar final-coefficient path now uses exact scalar-product identities
supplied by the caller before denominator sharing and coefficient arithmetic.
The mapping records those identities and their scalar vocabulary. Import checks
that the same identities still hold, including extra external vectors that are
not integral-family chords. Unassigned products, loop products, polarisation
vectors and unsupported right-hand sides retain their established treatment.
No kinematics or assumptions are inferred.

Native FORM polynomial denominators participate in both inspection and rational
computation on the scalar-only path. Existing object, degree, variable and term
guards still decide eligibility.

Complete Laurent monomials in eligible spectator scalars can now be collected
using the existing structural grouping machinery. Roles come from the encoded
families, dimensions, masses, invariants and expression structure, never from
names such as lambda. Scalars in mixed denominators or unsupported powers are
excluded. Polarisation jobs retain their existing partition. The scalar path
registers the transitive shared-definition dependencies before inspection;
kinematic polynomials remain shared. Caches and work allowances remain local to
a FORM job. Exact zeros disappear; exhausted work restores the original master.
There is no new public option and no Wolfram Simplify/Together/Factor in production.

## Verification and limitations

The focused tests cover exact rational identities and negative controls,
Laurent powers, distinct structures, symbolic D, complex coefficients,
unsupported radicals and matrix objects, import consistency, changed or invalid
kinematic snapshots, zero/unassigned products, serial/TFORM agreement and small
incremental-import chunks. A thirteen-spectator fixture remains unreduced with
the old monolithic selector and becomes literal zero with grouping. The old
master-degree test explicitly disables grouping to continue testing that selector.

| Check | Result |
| --- | --- |
| New scalar coefficient suite | 48/48 assertions |
| Existing final rational suite | 59/59 |
| Scalar-master reduction | 91/91 |
| Shared-output/import | 48/48 |
| Retained QED fixtures | 8/8: both complete-group zeros, exact smaller 30-term sample and nonzero control |

Three-box public calls after one warmup:

| Trial | Full call (s) | Export (s) | FORM (s) | Import (s) | Call peak (bytes) | Complete test-scope peak (bytes) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | 28.693 | 1.655 | 19.143 | 4.748 | 319,238,144 | 444,706,816 |
| 2 | 28.557 | 1.666 | 18.726 | 5.202 | 319,074,304 | 444,219,392 |
| 3 | 29.993 | 1.879 | 19.802 | 5.152 | 319,516,672 | 444,620,800 |

All four saved three-box results are byte-identical to the retained result for
all 17 masters. The warmup additionally passed Wolfram SameQ and an altered-result
negative control. Each FORM output has **4,355,957 bytes and 2,367 definitions**,
unchanged from PERF-05 and below the 4,364,149-byte ceiling. The retained baseline
calls were 30.027/30.452/30.551 s; these observations are not a controlled speedup
claim. Call peaks remain approximately 319 MB, as in the matched baseline; test
scope peaks near 444 MB include serialisation and retained-evidence handling.
The warmup's extra post-call reference import raised its scope peak to 707 MB;
its call peak stayed 320 MB. The measured trials compare saved files afterwards
and do not include that second Wolfram expression.

The final malformed-metadata guard was tightened after the warmup/first trial;
all 48 focused assertions were rerun. That field is absent in the three-box
workload, and no arithmetic/template change occurred between measured trials.
Raw scripts, hashes, diagnostics and resource/termination records identify each
exact candidate. The initial rational-suite failure was an outdated degree-guard
expectation: the new grouping legitimately cancelled a high-degree spectator.
Its corrected fixture isolates the old monolithic selector; mathematical
expectations were retained.


The saved Horndeski triangle completes and produces a valid FORM result. A
selected-master diagnostic identifies all seven expected tadpole coupling groups.
Their arithmetic remains over-conservative: estimates exceed the per-group
limits and denominator fallback consumes the 1,024-pass job allowance. The
complete original master is restored. **This implementation has not recovered
the 225-term reference.** No global arithmetic guard was raised or disabled.

A complete symbolic five-master output comparison was attempted under the
ordinary limits. The first harness had a wrapped-identifier parsing error; its
corrected full-expansion check timed out. A direct-difference checker without
repeated denominator factorisation also timed out. Neither establishes equality
or a discrepancy. These histories remain retained. No further large comparison
was run and no timeout was converted into a passing check.

The earlier seven-group diagnostic established equality to the 225-term
reference under its separately authorised larger budget. It did not test this
production implementation and is not reused as production acceptance. Closing
PERF-07 still requires improved bounded arithmetic selection, recovery of the
retained compact coefficient, and an exact comparison of all five triangle
masters with a negative control. The whole large QED multiplier and the older
1,490-term QED equality also remain outside the present verification.

All new runs used fresh sequential task-owned scopes with aggregate 1 GiB RAM,
zero swap, a 120-second deadline plus 10-second grace, and fresh 2 GiB host
headroom. Timed-out scopes were verified empty before continuing independent
regressions. No user kernels were stopped.

Local retained evidence:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-11-02-perf07/`.
The portable focused test is
[`ScalarCoefficientGroups.wls`](../../CalcFormConverter/Tests/ScalarCoefficientGroups.wls).
