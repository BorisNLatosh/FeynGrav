# Earlier simplification in the Lorentz dimension

10 October 2026. Parent commit: `9f875d6`. FORM/TFORM 5.0.2; Wolfram 15.0.1.

## Initial result (historical 200-second FORM target)

The complete public workflow met the 200-second FORM target in two fresh kernels, using the exact reconstructed input from `One-Loop-Counterterms.nb` and ten TFORM workers:

| Observation | FORM execution | Complete public call | Result file |
|---|---:|---:|---:|
| First | 103.58 s | 145.76 s | 9,611,925 bytes |
| Repeat | 111.87 s | 156.99 s | 9,611,925 bytes |

Both calls returned successfully. Their result files are byte-identical. The general denominator-basis cancellation pass remained enabled. These are complete calculations, not replays of an already contracted expression and not isolated runs with cancellation omitted.

The public-call timer includes availability checks, export, FORM and import, operating on the preconstructed input. Kernel/package loading and construction of the supplied expression are excluded. FORM time is the execution wall time reported by `ShowTiming`, not accumulated worker CPU time. Peak process-tree RSS for the two complete kernel/FORM runs was 4,305,592,320 and 4,369,391,616 bytes. RSS was sampled every 0.1 seconds and may miss shorter peaks. Each imported expression had `ByteCount` 174,278,808.

The laptop was on battery throughout both public runs: the monitor recorded mains offline and the battery discharging. The user's earlier 685.26-second FORM and 710.281-second overall observations were not repeated under matching power conditions. No controlled speedup ratio is claimed. The 200-second target was reached under the observed battery conditions; timings on other inputs and machines remain workload-dependent.

## Change

The previous sequence expanded dimension-dependent coefficients through tensor products and general numerator cancellation before simplifying them. The optimisation collects rational dependence on the symbolic Lorentz dimension earlier:

1. At eligible prepared tensor-stage boundaries, sort the ordinary tensor algebra first, then collect the dimension-only rational coefficient.
2. Disable rational arithmetic before the next tensor multiplication. This avoids polynomial GCD work on each raw contraction while retaining the compact coefficient as an internal commuting function.
3. After direct massless cancellation, precondition eligible expressions above the private 1,000-term threshold before running the unchanged general cancellation rules and growth guards.
4. After grouping, route already compressed coefficients through dimension-only output. Their reduced term count must not be mistaken for low multivariate polynomial complexity. Pure numerical ratios are restored as numbers.

The renderer uses `dcfStageSort` only when a version-one dimension plan and the existing prepared tensor stages are available. Other vocabularies retain their previous processing. There are no new options, physical assumptions, mathematical heads, saved-format versions or import algorithms. The existing output writer removes the internal rational functions and writes ordinary arithmetic.

The general growth guards still run after candidate expansion. Earlier dimension simplification addresses the measured cost; it is not a universal bound on arbitrary numerator rewriting.

## Algebraic verification

Input expression digest:
`d43dcf3b6594db5d58df9f3d2af85ecd6283d304cb271c8a35bb87769405295b`.

The newly saved notebook input and the retained input have the same digest and mapping. A test-only adapter translated the production output's explicitly parenthesised quotients into FORM rational-function notation. FORM gave an **exact zero residual** against the retained dimension-plus-massless reference. That reference had previously been verified against the original retained result with exact zero residuals; those logs were checked again. No numerical kinematics, on-shell conditions, transversality assumptions or Mathematica `Simplify` were used for this proof. The second production output is byte-identical to the first.

`Tests/DimensionPreconditioning.wls` passes 63 focused assertions. It forces early processing on small examples and compares serial FORM and TFORM against independent FeynCalc algebra, including actual prepared tensor stages, rational/negative powers of the dimension, free indices, massive numerators, scalar master functions, opaque radicals and zero coefficients. It checks stage ordering, output routing and the absence of leaked private objects.

The complete selected regression suites also passed: runtime (764 assertions), core (712 assertions) and FORM (235 assertions), for **1,711 assertions with no failures**. Automatic loading and namespace isolation checks passed separately. These checks do not constitute a Full benchmark run.

## Exploratory trials

- Dimension simplification immediately before general cancellation completed a retained-result replay in 96.90 seconds. Its complete-program trial took 225.55 seconds, but mains power was disconnected during that trial; it is not a matched timing comparison.
- Keeping rational arithmetic active throughout tensor multiplication was slower. The agent stopped this exploratory run after 205.64 seconds, before it completed tensor processing. Its incomplete result was discarded.
- Enabling rational arithmetic only after ordinary sorting at stage boundaries completed a full prototype in 121.85 seconds on battery. The two public-interface runs above verify the integrated implementation separately.

Trials were sequential. The experimental wrapper enforced 300 seconds, 6 GiB process-tree RSS and at least 2 GiB available system memory. The public calculations additionally used a 240-second FORM timeout. No CPU settings or system caches were changed, no Full benchmark suite was run, and no interaction libraries or notebook files were edited.

Re-export existing `.frm` files after reloading the package to obtain the new processing. Use `FORMThreads -> 10`, `ShowTiming -> True` and `KeepFiles -> True` to inspect comparable public runs; the measurement above also retained progress output and used a unique temporary working directory.

Machine-readable measurements, power observations, input/output fingerprints and source hashes: [EarlyDimensionCompaction.json](EarlyDimensionCompaction.json). FORM's rational-function and module behaviour is documented in the [official reference manual](https://form-dev.github.io/form-docs/stable/manual/).

## Follow-up: complete-calculation optimisation

A later request set a target below **20 seconds for the complete calculation**.
That target was not reached. The user subsequently accepted the retained
improvement and requested final verification, ending further optimisation.
The earlier verified fresh-kernel performance observation took
**26.768938 seconds including 0.182756 seconds of input construction**;
`CalcFormCalculate` itself took 26.586182 seconds and reported 16.24 seconds of
FORM execution. This is a single observation, not a repeated-run median.

This run used ten TFORM workers on mains power, with the same input digest above.
Peak process-tree RSS was 2,166,116,352 bytes, including the Wolfram kernel and
FORM subprocess. The wrapper's 33.15-second wall time also includes kernel and
package startup; it is not the public-command timing. Earlier battery-powered
observations are not controlled speedup baselines.

The retained changes combine native term iteration during output, bulk importer
syntax scans, held native reconstruction of validated arithmetic, early massless
cancellation, temporary abbreviations of dimension coefficients, and exact early
rejection of growing numerator-cancellation candidates. Prepared factors also
use verified two-index partner symmetries to combine intermediate terms before
multiplication. The symmetry optimisation is valid only inside that contraction;
it does not assert that the reduced intermediate tensor equals the original one
with arbitrary free indices. FORM checks the remaining partner explicitly and
leaves a pair unchanged when its check does not vanish.

The result file's SHA-256 is
`0a603c3c2573a77e52d47b71875f856a7eebc9e4b27ecaaf75a5c50927a79660`.
It is byte-identical to the retained, independently FORM-verified result. The
full imported expression has ByteCount 174,278,808. Core, FORM and runtime suites
passed **1,909 assertions**, including 64 new stage-symmetry checks with symmetric,
non-symmetric and antisymmetric partners. Three additional native-path checks
then confirmed that the symmetry guard passes and reduces a three-term
intermediate to two terms, for **1,912 distinct checks** in total. No Full
benchmark suite was run.

Native common-subexpression output, balanced contraction trees and several
additional coefficient-abbreviation variants were explored but not retained:
they were slower, increased output size, or gave no convincing measured benefit.
The source and output formats remain unchanged; no result cache is substituted
for a fresh calculation.

## Final verification after acceptance

The final fresh-kernel calculation used the unchanged retained sources and the
same notebook input. It completed successfully in **63.437993 seconds**, including
construction; FORM took 42.57 seconds. Its output is byte-identical to the verified
result above, and the reconstructed expression has the same SHA-256 fingerprint:
`3e833e63add335f4cbb74047e4180b2deeb82c4932da7c5def96f2aa57dc79bb`.

This correctness rerun is not a controlled timing comparison. The earlier
26.768938-second observation is not a guaranteed runtime. No additional
experimental parser or output-writer changes were retained. No commit was made.

Final focused regression checks passed **670 assertions with zero failures**: tree
parser (277), propagator grouping (153), cancellation (95), dimension preparation
(78) and stage symmetry (67). The symmetry checks include serial FORM and TFORM
and independent FeynCalc comparisons. `git diff --check` also passed.

## Independent review corrections

Fixed the reversed nested-denominator check order, which could emit
`Power::infy` before returning the intended division-by-zero failure. Added
direct hostile-source tests and observed dispatcher tests for the native parser.

Temporary serial FORM and TFORM probes now establish accepted sector assembly,
rejection after sector 1 of 2, and restoration of the original expression. An
initial fixture did not trigger rejection because its mixed scalar product was
not a rewritten pivot; the final fixture uses a routed square that does.

Replaced source-string insertion around sorts with `dcfStageSort[plan, beforeSort]`
and moved final abbreviation restoration into `dcfStageRestore[plan]`. Tests
verify the emitted stage sequence remains identical.

Core and FORM suites passed. After correcting the rejection fixture, all runtime
checks were also covered successfully, with its last four suites run separately
in fresh kernels: **1,964 distinct assertions, zero failures** in the final
coverage. `git diff --check` passed. No performance rerun, Full benchmark, stored
library regeneration or commit was performed for these review corrections.
