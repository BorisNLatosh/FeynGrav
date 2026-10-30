# Coefficient collection around reduction — PERF-05 increment 3

The eligible linear one-loop path now processes complete coefficients **before
runtime repeated-power/scalar-master reduction**. Tensor decomposition and
numerator cancellation precede this step: it does not pull a loop-dependent
numerator out of an integral. Existing tensor-decomposition validation rejects
loop dependence hidden in scalar abbreviations.

`LoopCoefficientReduction.wl` owns the collection planner and calls the native
`Templates/CoefficientCollection.frm.in` procedure. The installer checks for this
new template whenever its owning companion is present.

## Before reduction

1. Sort and bracket by the complete existing `cfcTC` key. The key retains
   denominator slots and powers, dimension context and tensor labels; a family
   alone is never substituted for an integral identity.
2. Use the existing linearity guard. Products or powers of integral coefficients
   keep the previous general recurrence path; their factors are not treated as
   one integral or one common coefficient.
3. Visit each complete bracket coefficient once. Expose only known Laurent
   monomials with the existing reader, then apply the shared bounded rational
   policy on a disposable copy. This matters because recurrence planning adds
   monomial abbreviations such as `m^2` and `1/(2 m^2)` even to simple inputs.
4. Replace the original only if the copy is one complete rational function or
   exact zero. Retain numerator/denominator factors using the existing
   `cfcLRNum`/`cfcLRDen` representation and restore temporary dot-product atoms.
   An unsafe object or residual object preserves the complete original.
5. Reassemble all keys and rebuild the reduction dictionary. Exact-zero keys
   disappear before their runtime recurrence is requested. Export-time family
   matrices and reduction-rule planning still occur; this is not a claim that
   all planning cost for a cancelled integral has been removed.

No new routing, on-shell, permutation or exceptional-kinematics identity is
introduced. Existing verified final-key canonicalisation remains in place.

## After reduction

The existing lookup combines all contributions to an identical final integral.
Large eligible jobs retain their existing shared-output coefficient processing;
this increment does not run a second collector over those shared expressions.
The smaller direct-output linear path now invokes the same bounded collector on
its complete assembled multiplier before dimension processing and output.
This permits cancellation after distinct input coefficients reduce to one master.
API-03 conversion still runs only after successful import/reconstruction.

## Selection, fallback and work ownership

The new pass uses the shared propagator-domain policy: at most 12 mapped
scalar/dot variables, at most 1,000 terms per attempted coefficient, and the
existing restricted inverse-polynomial recognition. Known Laurent monomials are
normalised without expanding sums; remaining unsupported abbreviations disable
this optional collection plan. Complex, indexed, metric, named-integral and
already wrapped rational factors retain their conservative path at this stage.
The established final master machinery can handle cases beyond this narrow
pre-collection domain.

One counter covers **both new pre- and direct-post calls** in a FORM job, with
1,024 attempts from the existing rational-pass allowance profile. Rejected
attempts also spend it. A pre-collection dictionary larger than that limit is
left untouched; a spent allowance preserves the complete current coefficient.
The old polarisation counter remains separate: this is not yet a universal
pipeline-wide budget. No pass replenishes its own allowance, retries an error
or swallows a FORM failure. Resource limits remain authoritative.

A private `$lrCollectCoefficients` switch exists for matched regression
references; there is no new public option or saved mapping version. With
reduction disabled, ordinary tree expressions and represented integrals retain
the existing requested-stage behaviour. Matrix/colour/epsilon jobs keep their
existing domain protections. No new Wolfram algebra pass is added.

## Remaining PERF-05 work

Changed-block recurrence, broader coordination of work allowances and whole-call
measurements remain. Full three-box and retained QED gates have not been renewed
by these small checks. See the [verification report](../Documentation/Verification/PERF05Collection-2026-10-30.md).
