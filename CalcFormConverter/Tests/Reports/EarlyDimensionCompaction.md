# Earlier simplification in the Lorentz dimension

10 October 2026. Parent commit: `9f875d6`. FORM/TFORM 5.0.2; Wolfram 15.0.1.

## Result

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
