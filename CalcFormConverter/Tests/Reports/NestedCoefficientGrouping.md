# Nested coefficient grouping — 9 October 2026

## Implementation and limits

The existing complete-propagator grouping is retained. After extracting the common numerical/monomial factor from each eligible coefficient, FORM now brackets its residual by the registered vectors. This collects repeated scalar-product monomials inside each propagator coefficient. Native vector brackets also group free momentum components; free indices are preserved. No momenta, masses or dimensional symbols are selected by assumed physical names.

The vector list comes from validated mapping entries. Empty lists emit no additional bracket statement. The hidden full result retains its denominator brackets; the second bracket applies only to temporary primitive coefficients. Output uses ordinary nested sums and products with `%E`, without `Collect`, single-term wrappers, auxiliary variables or a new format version. The restricted importer already handles this grammar and remains unchanged.

Larger coefficients and those with free indices still bypass common-factor extraction, but may receive the additional grouping. Epsilon, matrix and colour jobs keep their existing output paths. No additional per-subgroup content scans or polynomial factor searches are introduced: the native second bracket already provided a useful benefit on the tested sample. The grouping choice is empirical, not a universal optimality guarantee. The final expression and largest coefficient still need to fit in memory.

## Candidate comparison

Three grouping choices were tested on the retained 64-group sample with TFORM 5.0.2 and eight workers. These were sequential exploratory observations, not medians. Source contraction had already been completed in the fixture.

| Grouping inside each residual | Wall time | Output bytes |
| --- | ---: | ---: |
| Registered vectors / momentum monomials | 1.51 s | 8,277,174 |
| Scalar symbols and abbreviations | 1.41 s | 14,005,113 |
| Dimension symbol alone | 1.51 s | 18,420,673 |
| Previous common-factor output | Previously measured | 19,189,632 |

Momentum grouping was selected. The other choices are experiments, not public options. The retained full-factorisation output was 12,236,847 bytes; the new representation is smaller on this sample without repeating the expensive factor search.

## Correctness and import

FORM subtraction of the selected output and the original unfactorised 64-group fixture returned exactly zero. A preliminary fresh-kernel import completed in 35.26 seconds; total process wall time including startup/loading was 38.40 seconds. Peak process RSS was 783,204,352 bytes. The returned expression occupied 102,295,120 bytes and retained kernel memory was 640,686,552 bytes. These memory measures are distinct.

The final focused suite passed 100 assertions, including nested-sum preservation, serial and threaded execution, free components, negative powers, chunk boundaries and malformed/unknown input. Wider regressions passed: Core 111, Parser 121, Transactions 20, Installer 62, Export 31, FormStages 128, Import 65, Runtime 82, Epsilon 48, DiracColour 65, DiracAlgebra 104 and ColourAlgebra 139 assertions. All reported zero failures. Local documentation links and `git diff --check` also passed.

### Final isolated comparison

The delivered template and baseline were run three times each in alternating order (baseline/candidate, candidate/baseline, baseline/candidate), after the regression suites completed. Both used eight workers.

| Output stage | Median wall time | Range | Maximum observed process RSS | Output bytes |
| --- | ---: | ---: | ---: | ---: |
| Common extraction only | 1.49 s | 1.46–1.57 s | 326,692,864 | 19,189,632 |
| Common extraction and nested grouping | 1.51 s | 1.47–1.51 s | 342,241,280 | 8,277,174 |

There is no measurable execution-time improvement beyond the observed variation. Output size falls by 56.9%, with only a small observed FORM memory increase. The delivered output is byte-identical to the candidate whose FORM subtraction was zero. Import measurements use separate fresh kernels and are single observations, not variability estimates.


| Fresh-kernel import | Import time | Peak process RSS | Returned expression bytes | Retained kernel bytes |
| --- | ---: | ---: | ---: | ---: |
| Common extraction only | 50.26 s | 3,560,460,288 | 357,942,152 | 3,415,430,400 |
| With nested grouping | 36.42 s | 782,331,904 | 102,295,120 | 641,944,696 |

Both imports finished successfully within the resource limits. Timings exclude kernel startup/loading; RSS includes the complete process. These single observations support a benefit for this sample only.

## Evidence and scope

Temporary programs, results and logs are under `/tmp/cfc-parallel-factor` (`nested-*`, `import64-nested*` and final comparison files). Regression logs use `/tmp/cfc-nested-*.log`. Runs are bounded by 300 seconds, 6 GiB process-tree RSS and 2 GiB available system memory. The full user calculation and Full benchmark suite were not run. User calculation files, saved notebooks and stored libraries were preserved; no commit was made.

The [official FORM manual, brackets](https://form-dev.github.io/form-docs/stable/manual/#brackets) describes native bracket output and vector handling. The [common-factor report](CommonFactorExtraction.md) preserves the preceding implementation and observations. Existing exported `.frm` programs must be regenerated to use this change.
