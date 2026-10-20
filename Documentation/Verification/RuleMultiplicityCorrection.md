# Graviton differentiation multiplicities

19 October 2026; checks were performed on the then-uncommitted working tree. The correction and subsequent regenerated libraries were committed in `e910bbe`. The retained snapshots identify the tested inputs.

The C and CE helpers return averaged Taylor coefficients. Differentiating n
labelled graviton fields supplies n!. The interaction rule must restore that
factor unless it already sums the graviton permutations.

## Changes

- Massive vectors and vector ghosts: restore n! in both routines.
- Massless vectors: restore n! only in the kinetic and connection-free gauge
  terms (helpers 1 and 3, and the corresponding inline uncontracted terms).
  The connection terms already sum permutations and are unchanged.
- SUNYM: restore n! in quark–gluon, three-gluon, four-gluon and gluon–ghost
  routines. Two-gluon and ghost wrappers inherit the vector corrections.
- Horndeski G2: restore the graviton n! separately from scalar permutations.
- Axion–vector: restore n! in the existing density-based implementation. This
  does not resolve or validate the separate epsilon-density/action convention.
- The preceding minimal-scalar correction follows the same principle.

The source-correction stage made no global change to C/CE helpers or permutation-based gravity/fermion/Horndeski routines. Subsequent library regeneration and scalar-model rebuilding are recorded in [ScalarWardCorrection.md](ScalarWardCorrection.md).

## Verification and limits

38 exact old/new comparisons passed at one and two gravitons, covering 19
expressions. Order one is unchanged; order two receives the required factor
for the tested direct terms. Massless full vertices were compared with the
gauge parameter zero; the two gauge-independent helpers were checked
separately. Connection permutation sums were preserved by source comparison.
The contracted four-gluon comparison identifies its generated dummy label
consistently between evaluations.

These are multiplicity regressions, not independent derivations of every
underlying action or complete Ward tests. Higher orders, the axion convention
and other pre-existing source defects are not certified by this check.
The three-gluon uncontracted variant was edited but not included in these
runtime comparisons; the generator uses the tested contracted variant.

The initial diagnostic had incorrect private helper contexts and compared
independently generated dummy names literally. Its failed assertions remain
in the retained evidence; the corrected diagnostic passed with no messages.
The kernel ran serially by replacing the parallel-map helper only inside
that disposable diagnostic kernel. Production parallel behaviour is unchanged.

Evidence: `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-19-scalar-normalisation-repair/other-multiplicity-check-r2/`.
Preserved old sources: `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-19-other-rule-multiplicities/`.
Whole-tree limit 1 GiB, zero swap, 120 seconds; highest retained cgroup
memory-peak sample 226,652,160 bytes, not process RSS or a guaranteed final peak.

## Regeneration

Use a fresh generator kernel. The shipped affected vector, SUNYM and axion
libraries currently extend through order two. Regenerate order two with
`GenerateGravitonVectorsSpecific[2]`, `GenerateGravitonSUNYMSpecific[2]`, and
`GenerateGravitonAxionVectorSpecific[2]`.
For G2, regenerate `GenerateHorndeskiG2Specific[a,b,2]` for
`{a,b} = {0,2}, {0,3}, {1,1}, {1,2}, {2,1}, {2,2}, {3,1}, {4,1}`.
Order-one coefficients do not change from these factorial corrections. The subsequent axion regeneration also migrated legacy epsilon slots at orders one and two; see the linked regeneration record. The listed affected shipped libraries have now been regenerated. Any additional
locally generated orders n >= 2 require corresponding regeneration too.
