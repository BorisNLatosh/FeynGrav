# Repeated-power coefficient reduction verification

> Historical stage report, including subsequent collection work. [Scalar-master reduction](ScalarMasterReduction.md) now supplies a later default-on stage. Enabled master reduction includes necessary repeated-power reduction even when `ReduceLoopCoefficients -> False`; set `ReduceToScalarMasters -> False` to reproduce this report's coefficient basis. Its version-six/seven discussion describes this stage; active master jobs use version eight. No measurements below are relabelled as current master-reduction timings.

Date: 10 October 2026. Tested with Wolfram Language 15.0.1, FeynCalc 10.2.1,
FORM 5.0.2 and TFORM 5.0.2 on Linux.

## Delivered behaviour

`ReduceLoopCoefficients -> True` is an opt-in FORM stage for new one-loop
calculations and for previously computed `LoopTensorCoefficient` expressions.
The default is `False`. It applies same-route partial fractions, converts
labelled parameter moments with their original Pochhammer factors and lowers
powers by fixed-dimension integration by parts. FORM sorts after each lowering
step to collect intermediate integrals before branching again.

Generic nonsingular subfamilies with up to four distinct routes return
unit-power PaVe coefficients. The ordinary coefficients can have more metric
labels than the original coefficient. This is a change of coefficient basis,
not analytic evaluation or complete reduction to scalar masters.

New integrals retain version six and their `I Pi^2` normalisation. Saved
coefficient expressions use version seven, with no additional measure or
normalisation. Versions one through six remain readable. Scalar-product
invariants used to plan reduction must match at import, checked before inverse
scalar abbreviations are decoded.

## Initial implementation checks (10 October)

| Check | Outcome |
| --- | --- |
| New focused reduction suite | 82 assertions passed |
| Existing core suite on the final compatibility gate | 766 assertions passed |
| Existing FORM export/import suite | 257 assertions passed |
| Runtime suite, including tensor decomposition and the new focused suite | 1,137 assertions passed; this includes the 82 above |
| Independent triangle/box parameter suite | 8 additional assertions passed |
| Main-package loading and independent converter reload | Passed; no subprocess calls, directory changes, altered gravity definitions or changed FeynCalc options |
| Repository whitespace check | Passed |

The focused suite covers exact massive vacuum moments through denominator
power four and tensor rank six, mass-derivative normalisation, 20 independent
bubble parameter comparisons, saved constants and products, coincident
propagators with labelled moments, rational momentum routing, singular
kinematics, explicit numerical dimensions, scaleless retention/removal and
the coefficient supplied by the user. It also checks serial/TFORM agreement,
all four enabled/disabled Dirac/colour combinations with epsilon tensors,
malformed mappings/results, stream boundaries and changed kinematics.

The separate triangle/box suite uses massive Euclidean configurations in
D=3, with squared masses selected from `{2,3,5,7}`, orthogonal unit spacelike
chords and powers `{2,1,1}` or `{2,1,1,1}`. Coefficient labels are `{0}`,
`{0,0}`, `{1}` and `{1,2}`. Independent Feynman-parameter integrals are evaluated
with 20- and 32-point Gauss-Legendre grids per variable. Both the residual and
the grid change must be below `10^-9`. The reference implements the parameter
integral directly, with its Gamma prefactor and dimensional analytic
continuation, rather than calling the production recurrence. Each reference
calculation has a 30-second bound. The test requires no extra libraries beyond
Wolfram and the converter's normal dependencies.

These are exact identities and numerical checks on the listed fixtures, not a
claim of verification at every kinematic point or arbitrary rank.

## Supplied coefficient

Writing `s = SPD[p]`, the example with labels `{0,0}`, chords `{0,-p,-p}`,
squared masses `{0,0,M^2}` and powers `{1,2,1}` reduces to

\[
\left(\frac{D-1}{M^2s}-\frac{1}{M^4}\right)B_{00}(s;0,0)
+\frac{B_{00}(s;0,M^2)}{M^4}
-\frac{\mathrm{LoopTensorCoefficient}(\{0,0\},\{0\},\{0\},\{2\},D)}{M^2s}.
\]

The last term is retained by default. `DropScaleless -> True` removes it.
The generic formula assumes nonzero `M` and `s`; singular input is handled
before attempting this inverse-coefficient reduction.

## Limits and development observations

- Singular Cayley subfamilies remain formal. No claim of complete exceptional-
  kinematics reduction is made.
- Reduction is restricted to symbolic D. Substituting an integer dimension
  before resolving a zero recurrence factor times a UV pole can give a false
  zero; an explicit-dimension regression prevents that operation.
- Coincident denominator slots must be merged after converting their labelled
  parameter moments. Tests cover the otherwise lost multiplicity factors.
- An early unsorted recurrence hit the 30-second limit in a box test. The
  final implementation sorts between steps and passes that case.
- An exploratory adaptive quadrature was stopped before its 300-second bound
  after completing the triangle observations. It is not counted as a completed
  box check. The retained fixed-grid suite completed all eight comparisons.
- The initial implementation did not benchmark the complete large quadratic-
  gravity answer. The subsequent measurements below supersede that limitation.
- No Full benchmark suite, interaction changes, stored-library regeneration,
  notebook edits or commit were performed.


## Earlier original-coefficient grouping (11 October 2026)

The recurrence previously carried every amplitude term through each lowering
step. The first optimisation reduced each distinct coefficient once while the amplitude
stayed hidden. Its indexed lookup held complete reduced linear combinations;
output wrote each combination multiplied by its original amplitude bracket
without expanding that product. Final-integral grouping below supersedes that
output layout. Nonlinear products of integral coefficients retain the
direct recurrence. Spectator propagators remain inside the amplitude brackets.

Import uses the existing restricted arithmetic-tree reader for eligible
commuting coefficient blocks. Typed coefficient reconstruction and repeated
scalar validation have bounded, import-local caches. No global simplification,
change to recurrence identities or additional physical assumptions is involved.

### Representative calculation

The input reproduces the complete polarised quadratic-gravity bubble used in
`One-Loop-Counterterms.nb`, with two external propagators, symbolic masses,
`LoopMomenta -> {l}`, `DropScaleless -> True`,
`ReduceLoopCoefficients -> True`, and ten TFORM workers. The source notebook and
its retained aborted job were preserved.

| Measurement | Wall time | Sampled peak process RSS |
| --- | ---: | ---: |
| Fresh FORM calculation from the original amplitude | 100.10 s (FORM reports 99.91 s) | 3.06 GiB |
| Reduction and output from retained decomposed input | 13.78 s | 2.39 GiB |
| Import of the latter complete reduced output | 191.33 s | 1.04 GiB |

The fresh FORM output is 104,023,075 bytes. The separately staged output is
104,238,817 bytes. Both are mathematically identical: a FORM subtraction,
keeping the same reduced coefficient blocks opaque and collecting rational
coefficients in D, returned exactly zero (97.66 s). Every one of the 2,466
original coefficient keys was also checked against the direct recurrence;
the difference was zero (13.28 s).

The import completed without a Failure. Retained Wolfram kernel memory was
983,079,888 bytes. `ByteCount` of the answer was 3,249,880,472 bytes; this counts
shared subexpressions repeatedly and must not be presented as physical RAM.
The import process, including startup, took 195.33 s. These are separate stage
measurements, not a measured public-command total or repeated-run median.

The user's earlier FORM run was aborted at 1,025.79 seconds. It supplies a
lower bound on the previous runtime, not a completed baseline for a precise
percentage speedup. Preliminary variants of the optimisation were stopped
when intermediate expansion or import exceeded the trial limits. Only the
completed final measurements above describe the delivered path.

Trials ran sequentially with limits of 300 seconds, 6 GiB process RSS and
2 GiB available system memory. RSS was sampled every 0.25 seconds. Measurements
are specific to this workload and machine; power and thermal conditions were
not controlled. Temporary evidence is retained under
`/tmp/reduction-slowdown` and may disappear on system cleanup.

### Final regression gate

Final checks passed: core 766 assertions; FORM export/import 257 assertions; runtime 1,164 assertions, including 101 focused reduction assertions and eight independent triangle/box parameter comparisons. Total: 2,187 assertions, no failures. Main-package loading and independent reload passed with no process calls or changes to existing definitions, options or working directory. The repository whitespace check passed. Focused additions cover direct-recurrence agreement, factored output, spectators, split blocks, nonlinear fallback, malformed wrappers and optional metadata.
No Full benchmark run, notebook changes, interaction changes, stored-library
regeneration or commit are included in this optimisation.

## Final-integral collection and shared multipliers (11 October 2026)

This supersedes the original-coefficient output layout above. The recurrence
still operates on distinct original keys. The writer now assembles contributions
by the final coefficient, including identical shifted families, then combines
like terms and simplifies rational dimension factors in FORM. Laurent-monomial
abbreviations are exposed for cancellation with native scalar factors. It does
not apply a general multivariate rational simplification to the full amplitude.

For large commuting-algebra jobs, already contracted amplitude blocks remain
shared during collection. The output records each amplitude block once and
then writes final integral groups referring to those blocks. The restricted
importer validates the definitions incrementally and reconstructs ordinary
factored Wolfram expressions. No private reference heads remain in the answer.
Epsilon/Dirac/colour jobs retain direct assembly, and nonlinear products of
integral coefficients retain their general recurrence path.

### Correctness

- The complete retained realistic amplitude was compared in FORM before and
  after shared-block conversion: the difference was exactly zero (10.52 s).
- The complete reduced lookup was compared with reassembly by final integral
  keys, retaining independent original-amplitude identifiers: the difference
  was exactly zero (2.76 s after preparing the two 10–13 MB expressions).
  Together with the first check, this verifies collection without expanding
  the Cartesian product of all amplitude terms and reduction coefficients.
- The fresh realistic result contains 1,273 PaVe occurrences and 1,273 distinct
  PaVe objects. No converter-private tokens were found in the imported result.
- The focused suite increased to 131 assertions, covering final collection,
  shifted families, scalar cancellations, forced shared-block round trips,
  serial/TFORM agreement, chunk boundaries, malformed definitions/references,
  stream cleanup and retry after failure. Existing independent parameter
  comparisons remain in the suite.
- Core, export/import and runtime suites passed: 766 + 257 + 1,194 = 2,217
  assertions, zero failures. The final read-ahead adjustment is additionally
  checked by the focused suite; it changes only byte-reading boundaries.

### Completed measurements

The workload and options are those in the earlier representative calculation,
including ten TFORM workers and `DropScaleless -> True`. Trials were sequential.

| Measurement | Wall time | Sampled peak process RSS |
| --- | ---: | ---: |
| Fresh standalone FORM calculation and new output | 118.21 s | 2.75 GiB |
| Complete import of that unmodified output | 84.56 s | 0.53 GiB |
| Import of the identical pair with 16 KiB definition read-ahead | 71.20 s | 0.52 GiB |
| Complete public `CalcFormCalculate` call, before the last reader adjustment | 215.16 s | 0.57 GiB for the Wolfram process |

The public-call RSS does not include its FORM child. Its FORM stage reported
127.84 s. The standalone FORM measurement separately covers that process.
The 118.21-second trial predates an output-only correction avoiding duplicate
expression text on stdout in FORM 5.0.2; the public call includes that correction.
No public-command total is extrapolated from the later import measurement.

The standalone output is **40,097,562 bytes**, versus 104,023,075 bytes for the
previous original-key output. The final read-ahead trial retained 420,781,144
bytes of Wolfram kernel memory. The smaller read window avoids decoding and
seeking backwards over almost a megabyte for every short definition; the
ordinary grouped-body reader retains its existing chunk size.

These are individual observations, not repeated-run medians or portable
speedup guarantees. Power and thermal conditions were not controlled. The
same 300-second, 6 GiB process-RSS and 2 GiB available-memory guards were used.

### Limits and rejected prototypes

Direct expansion of all final multipliers produced a 582 MiB output in a
completed prototype. Full multivariate rational variants exceeded FORM's
single-term limits or were stopped. An early shared writer also emitted the
entire internal extra-symbol registry; its import was stopped and is recorded
as incomplete. The retained implementation emits only amplitude blocks.
Some such blocks become unused when reduction removes an integral family.

Shared subexpressions reduce physical memory, but repeated references still
represent a large logical expression. Traversals such as `ByteCount`, full
formatting or a later global simplification may therefore be expensive.
Suppressing the complete notebook output remains appropriate. Shared blocks
are kept factored: this is not a claim that every cancellation across distinct
large blocks has been found or that multipliers are globally minimal.

Temporary scripts, raw logs, measurements and comparison outputs are retained
under `/tmp/reduction-collect`. The public-command job is
`/tmp/reduction-collect/calcform-job-00d9ebba-b7f8-4bb4-a901-c343d7902979`.
These temporary paths may disappear on system cleanup. No interaction rules,
stored libraries, user notebooks or benchmark notebooks were changed; no Full
benchmark or commit was made.
