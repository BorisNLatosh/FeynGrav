# Dimension-only coefficients and direct massless cancellation

Date: 10 October 2026. FORM/TFORM 5.0.2; Wolfram 15.0.1.

## Implementation

Both passes are automatic in newly exported programmes and `CalcFormCalculate`:

1. Direct monomial cancellation removes a matching numerator square against a massless propagator whose routing is a rational multiple of one vector. It runs before general basis rewriting, does not increase term count, and survives that stage's growth fallback.
2. Remaining version-one propagator coefficients simplify their rational dependence on the mapped symbolic Lorentz dimension alone. Other monomials stay outside the rational function. Small coefficients still use the earlier bounded multivariate procedure first. Opaque abbreviations remain opaque unless they encode an eligible inverse polynomial in the dimension alone.

Output uses ordinary arithmetic. No public option, importer grammar or mapping version changes. No on-shell, transverse, partial-fraction or loop-integration rules are added. Neither production pass uses Mathematica `Simplify`.

## Focused verification

`Tests/DimensionCoefficients.wls` passes 47 assertions using serial FORM and two-worker TFORM: rational dimension identities, zero coefficients, dimension aliases, complex factors, scalar master functions, opaque factors, free tensors, multiple batches, repeated and scaled massless propagators, excluded massive/composite routing and Dirac coefficients. Some checks deliberately disable the older private multivariate or general-basis stage to exercise the new passes independently.

Core/parser/installer checks pass 712 assertions; FORM export/staging/import checks pass 235. Independent and main-package loading checks pass without starting processes or changing existing definitions/options. Together with runtime, 1,648 assertions pass.

The runtime suite passes 701 assertions, including epsilon, Dirac, colour, grouping, rational coefficients and propagator cancellation. The earlier radical test now checks that the radical does not enter the **multivariate** procedure: merely finding any `PolyRatFun` declaration no longer establishes an error. Algebraic radical round trips remain tested.

## Retained large result

The reconstructed notebook input has expression digest
`d43dcf3b6594db5d58df9f3d2af85ecd6283d304cb271c8a35bb87769405295b`.
Its retained contracted result is 171,993,554 bytes. Original files and the notebook were preserved.

Trials were sequential and stopped at 300 seconds, 6 GiB process-tree RSS or less than 2 GiB available system memory. These are experimental limits, not new package defaults.

**Full pipeline limitation:** replaying that result through all default post-contraction stages reached 300.32 seconds and was stopped before an output file was created. Peak process-tree RSS was 4,527,554,560 bytes. This does not establish a completed default `CalcFormCalculate` run on the full example.

An isolated replay then omitted the pre-existing general denominator-basis cancellation stage, retaining the generated direct massless pass and all subsequent coefficient/output stages unchanged. This isolates the additions from the unresolved cost of general basis rewriting.

| Isolated observation | Result |
|---|---:|
| Replay wall time, including reading the retained expression | 71.37 s |
| Peak replay process-tree RSS | 3,357,569,024 bytes |
| Output size | 11,346,673 bytes |
| Propagator groups | 1,394 |
| Public import wall time | 24.84 s |
| Peak import process-tree RSS, including kernel loading | 408,190,976 bytes |
| Imported expression `ByteCount` | 214,993,968 bytes |
| Retained kernel `MemoryInUse[]` | 263,802,040 bytes |

The output is approximately 93.4% smaller than the retained original. These are single observations, not repeated benchmark medians or a general speedup guarantee. Construction and tensor contraction are excluded from replay time. Kernel memory statistics and process-tree RSS are different measurements.

For equality checking, a test-only adapter rewrote the emitted explicit quotients into FORM rational-function syntax. FORM gave an exact zero residual against the previously verified dimension-plus-massless result, which had itself been checked against the original. The equality check completed in 11.06 seconds. It does not rely on numerical sampling or on a comparison of printed forms.

The public importer read the completed output with its unchanged mapping interpretation. No private rational-function objects leaked into it. The large default-pipeline timeout remains a separate limitation; the isolated result must not be presented as a successful full default run.

Machine-readable measurements and source hashes: [DimensionCoefficients.json](DimensionCoefficients.json).
