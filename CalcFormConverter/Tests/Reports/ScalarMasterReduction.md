# Scalar master reduction — 11 October 2026

## Mathematical conventions

The reference identities are equations (15), (17) and (33) of
[Fleischer, Jegerlehner and Tarasov, hep-ph/9907327](https://arxiv.org/abs/hep-ph/9907327).
[Denner and Dittmaier, hep-ph/0509141](https://arxiv.org/abs/hep-ph/0509141)
provides independent context for ordinary and exceptional kinematics. Neither
paper's numerical small-determinant expansions is implemented.

Let n be the total propagator power, h the number of metric pairs, and k the
number of positive coefficient labels. In the existing parameter convention,

\[
 C^{\boldsymbol\nu}_{0^{2h},\boldsymbol j}
 =\frac{(-1)^{n-h+k}\pi^{D/2-2}\Gamma(n-h-D/2)}
 {2^h\prod_i\Gamma(\nu_i)}
 \int_{x_i\geq0}\!\delta(1-\sum_i x_i)
 \prod_i x_i^{\nu_i-1}\prod_{j\in\boldsymbol j}x_j\,
 \mathcal F^{D/2+h-n}\,\mathrm d\boldsymbol x.
\]

The positive labels refer to non-anchor chords. With J normalised by
1/(i pi^(d/2)) in its own dimension, an all-metric coefficient is
(-1/2)^h pi^(D/2-2) J^(D+2h). The common pi^(D/2-2) does not shift during the
recurrence. Thus each dimension-lowering identity acquires -1/2. The identity
C_{...j}^nu = -2 nu_j C_{...00}^{nu+e_j} fixes the raised-power conversion.
These factors are separate from I pi^2, which is included once for a newly
integrated expression and never added to an already supplied coefficient.

A massive tadpole obeys C_h^nu = m^2 C_(h-1)^nu/(D+2h-2nu), directly from the
Gamma-function ratio. This avoids introducing a spurious denominator-free
pinch. For a zero-momentum bubble the parameter polynomial is linear in the
masses; the zero-Gram identity reduces unequal masses without dividing by p^2.
Equal masses coalesce their powers after positive labels are converted.

Each recurrence step lowers dimension shift, total propagator power or the
number of active lines. Sorting between steps combines identical states.
Scaleless and unresolved singular inputs retain their coefficient meaning.
Generic nonzero determinants may occur in denominators; no numerical tolerance
or dimensional truncation is used.

## Completed correctness checks

- 25 initial exact reference comparisons through boxes and rank six at fixed
  nonsingular kinematics, plus symbolic A00, B1, B00 and B11 probes.
- The initial dedicated public suite passed 51 checks: default and disabled
  behaviour, exact coefficient comparisons, zero-momentum identities, scaleless
  policy, normalisation, independent saved families, both engines, manual
  workflow, mapping version and chunk boundaries.
- Eight independent parameter-quadrature comparisons for repeated-power
  triangle and box coefficients passed. References use neither the production
  recurrence nor FeynCalc tensor reduction.
- Existing core/parser/transaction/installer suites: 766 checks passed.
- Existing runtime and algebra suites: 1,247 checks passed, including the
  53 denominator-group checks. The additional 51 master checks also passed.
- Existing FORM export/staging/import suites: 257 checks passed.
- Existing package integration: 50 checks, loading/namespace isolation and
  complete-bubble export metadata passed. The stored libraries were not written.

FeynCalc's PaVeReduce left some zero-momentum reference functions unreduced.
Those initial direct comparisons were not counted as passes. Dedicated analytic
parameter identities provide the zero-momentum checks instead. Likewise,
comparisons dropping scaleless integrals must not be used to validate an output
that deliberately retains them.

## Final regressions and limitations

The final scalar-master suite passed 91 assertions, extending the initial checks
with malformed metadata, intermediate-object rejection, stream cleanup,
state-sharing equivalence, numeric continuation across chunks, mixed
Dirac/colour/epsilon spectators, changed scalar-product assignments, exceptional
saved coefficients and scalar-master permutations. Direct loop-dependent slashes,
scalar products and epsilon slots also agree exactly with independent B1 reductions. The existing core and runtime
suites were rerun after the reader change and passed again.

A saved invariant-only PaVe family is conservatively retained when a relevant
subfamily has an unresolved singularity or a preserved scaleless multi-line
integral. This is intentional partial reduction, not a numerical prescription.
Higher-point families and nonstandard dimensions remain formal. Exceptional
kinematics must be supplied before reduction; generic denominators cannot safely
be specialised to zero afterwards.

For the all-positive rank-six box performance fixture, the exact shared versus
unshared comparison passed. A separately bounded PaVeReduce comparison was
inconclusive. This fixture is not counted among independent exact reference
passes; the 25 manageable rank checks and eight parameter checks are separate.

## Performance measurements

The [raw observations](ScalarMasterPerformance.json) retain all warmups and
measured trials, mean/median/range, output bytes, import time, retained kernel
memory and monitored peak process memory. Each configuration had one warmup
and one measured observation in each of three fresh kernels. Configuration
order alternated between kernels. Jobs ran sequentially. Native FORM timing
markers in benchmark copies separate recurrence from subsequent assembly and
output; their resolution is 0.01 seconds. Zero at that resolution is not zero
computational cost. Very small subprocess timings are dominated by launch and
polling overhead.

| Workload | Coefficient-only FORM median | Master FORM median | Master import median | Master output bytes |
|---|---:|---:|---:|---:|
| B00 control | 0.0230 s | 0.0232 s | 0.0184 s | 399 |
| 100 repeated-coefficient multipliers | 0.0222 s | 0.0224 s | 0.0739 s | 49,436 |
| Rank-six triangle | 0.0228 s | 0.0228 s | 0.0244 s | 3,533 |
| Rank-six box | 0.0226 s | 0.0438 s | 0.0465 s | 12,855 |

These output bases differ: the table measures the **added cost** of master
reduction, not a speedup between equivalent outputs.

The same-result optimisation control contains 95 distinct rank-eight box
coefficients in one family. Sharing intermediate recurrence states reduced the
median FORM time from **1.579 s to 0.572 s**
(**63.7% less time**). Observed ranges were
1.565–1.604 s and
0.562–0.594 s respectively.
Native recurrence medians were 1.45 s and
0.44 s; subsequent assembly/output medians were
0.11 s and 0.12 s.
Exact algebraic repeatability passed outside timing. Equivalent outputs can
have different factor ordering and byte counts. This result is specific to
that workload; small controls show no useful timing gain.

On that control, median OS-reported FORM peak RSS was
23.7 MiB without sharing and
24.0 MiB with sharing. Sampled Wolfram kernel
RSS across these small trials was about 0.29 GiB. The monitor samples only the
launched kernel; it does not include FORM child processes. FORM peak RSS is
therefore recorded separately using the operating system's child-process
high-water mark. Retained Wolfram memory is another separate measurement and
must not be interpreted as process peak memory. Timing wrappers and native
markers are added only to benchmark copies, not exported production programs.

### Realistic two-point workload: incomplete

The retained full quadratic-gravity workload requested 2,466 coefficients.
Both bounded runs reached the 300-second limit: the initial unshared run at
300.36 s and the shared run at 300.62 s. Peak process memory was approximately
4.33 GiB and 4.28 GiB respectively. The guard also enforced a 6 GiB process RSS
ceiling and a 2 GiB available-system-memory floor.

Both outputs were incomplete, so neither was imported or counted as a successful
result. The shared trial reached assembly/output but did not finish it. No claim
is made that the full result fits in memory, imports successfully, or runs faster
end to end. A further unbounded run was deliberately not attempted. These trials
preceded the final scalar-permutation and diagnostic-metadata additions.

No Full benchmark suite or stored-library regeneration was run. Interaction
formulae, stored libraries and saved notebooks were outside the edit scope.

## Delivery checks

The final FORM export/staging/import and integration suites passed after the
last changes. The installed package then passed six smoke checks covering
serial/TFORM agreement, the disabled option, public defaults/help and a read-only
conventions report. Relative documentation links and whitespace checks passed.
Hashes confirmed that interaction rules, stored libraries and saved notebooks
were unchanged. No commit was made.

## Assembly follow-up — 11 October 2026

The historical large-trial limits above precede the conditional sort described in the [developer guide](../../DEVELOPER.md#parallel-materialisation-before-integral-block-expansion). A subsequent ten-worker TFORM trial completed in 252.49 s with 322,711,635 output bytes; its baseline stopped at 300 s. First-group block expansion took 15.32 s versus 102.92 s. These are individual battery-powered observations, not a complete baseline speedup estimate or a full large-result equality check.

Seven focused suites passed 965 assertions. A sequential small control with one warmup and three alternating measured trials per variant had four-worker TFORM medians of 0.354573 s and 0.262071 s, with byte-equal outputs; serial medians were 0.328739 s and 0.328083 s. The large fresh-kernel import did not return within 300 s and was terminated after a five-second grace period. Its sampled peak kernel RSS was 1.40 GiB; the completed FORM process peaked at 4.33 GiB. Final expression memory and end-to-end completion remain unverified. Sampled process monitoring is not an operating-system-enforced aggregate memory cap.
