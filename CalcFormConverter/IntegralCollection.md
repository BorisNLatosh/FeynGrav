# Coefficient collection around reduction — PERF-05 increments 3–4

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

One arithmetic counter covers **both pre- and direct-post calls** in a FORM
job, with 1,024 attempts from the existing rational-pass allowance profile.
Rejected new attempts also spend it; exact cache hits do not. A pre-collection dictionary larger than that limit is
left untouched; a spent allowance preserves the complete current coefficient.
The local polarisation counter remains separate, but the increment-5 implementation
adds common rational/factor reservations above both paths. Denied arithmetic is
not cached as a rejection. This is not a universal operation budget; see
[WorkLimits.md](WorkLimits.md) and its focused verification. No pass replenishes its own allowance, retries an error
or swallows a FORM failure. Resource limits remain authoritative.

A private `$lrCollectCoefficients` switch exists for matched regression
references; there is no new public option or saved mapping version. With
reduction disabled, ordinary tree expressions and represented integrals retain
the existing requested-stage behaviour. Matrix/colour/epsilon jobs keep their
existing domain protections. No new Wolfram algebra pass is added.

## Changed-coefficient reuse — increment 4

`cfcCollectScalar` is now a bounded reuse coordinator around the unchanged
single-attempt algebra in `cfcCollectScalarOnce`. It uses FORM's native exact
argument interner to identify complete sorted coefficients. A split `Collect`
argument is ineligible: its alternate head must not supply a partial key.
This follows [FORM's documented argument and collection semantics](https://form-dev.github.io/form-docs/stable/manual/),
sections 7.8 and 7.20. No algebraic equivalence beyond FORM's existing sorting is
inferred from these keys.

The cache belongs to one generated FORM program with immutable mapping,
scalar-product rules, dimension, arithmetic domain and monomial definitions.
No shared-definition metadata is dereferenced by this collector. A different
complete coefficient gets a different key; a new job starts empty, even if its
identifiers happen to have the same spelling. Reuse across changed definitions,
new kinematics or mappings is deliberately unavailable.

Completed successful results and conservative rejections are remembered. A
successful result also gets a settled-output alias when it fits a complete key,
so revisiting its new representation does not repeat rational processing.
Readiness and value are separate; zero is a valid result, not a cache miss.
No table is filled from a caught FORM error: runtime failures remain failures.
No new iteration or expand/factor cycle is introduced. The existing pre/post
boundaries are the only revisiting points.

Additional initial storage/lookup limits are conservative selection choices:

- At most 1,024 eligible visits, including hits, use the existing allowance
  profile. This bounds key-building work separately from arithmetic attempts.
- At most 64 ready entries store results, rejections and output aliases. Two
  slots are reserved before new arithmetic; existing hits can still be used at
  capacity until the visit allowance is spent. New inputs retain their complete
  original when capacity is insufficient. This can skip useful simplification.
- Each visit builds at most two complete keys, subject to the existing 1,000-term
  selection and FORM's argument capacity. These limits are not memory guarantees.

Private `$lrReuseCoefficients` and `$rcCollectionMaximumCacheEntries` support
matched checks; neither is a public option. Counters `cfcCCVisits`, `cfcCCWorkUsed`,
`cfcCCHits`, `cfcCCEntries` and `cfcCCSkipped` are job-local developer state, not
new output records or exported objects. The initial capacity is not a measured
optimum. No cache is shared with or substituted for the mature polarisation
inspection/algebra caches, whose modes and rollback semantics stay unchanged.

## Remaining PERF-05 work

Reuse at these collection boundaries is implemented and focused-tested. Work-allowance coordination is accepted in increment 5 with 495/495 combined
assertions. Remaining changed-dependency paths and complete strategy acceptance
still need review and whole-call measurements. Full three-box
and retained QED gates have not been renewed by these small checks. See the
increment 3 report (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance))
and increment 4 report (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance)).

## Final dependency and workload review

[DependencyPaths.md](DependencyPaths.md) documents immutable context and readiness
ownership. The final verification (historical record; [current user guidance](../Documentation/ScientificScope.md#calculation-and-performance)) completes retained regression, QED
and whole-call checks. Isolated final-guard fixtures disable earlier collection
locally; separate default-path assertions verify its legitimate earlier zeros.
