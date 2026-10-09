# Common-factor extraction — 9 October 2026

The measurements below describe the common-factor stage alone. It was subsequently extended with [nested coefficient grouping](NestedCoefficientGrouping.md); historical measurements are retained.

## Scope and implementation

The automatic grouped-output stage now extracts common numerical and monomial factors instead of performing complete polynomial factorisation. The public API, mapping versions, restricted importer and propagator grouping are unchanged. Existing exports must be regenerated to use this template.

FORM's native `content_` is evaluated once per eligible coefficient. Its numerical content uses the numerator GCD and denominator LCM; common symbol and scalar-product powers include Laurent powers. Scalar PaVe functions are stripped from the extracted content and left inside the residual sum. FORM does not cancel their formal reciprocals, so direct division by function-valued content is unsuitable. Direct division by the remaining monomial is exact, including negative powers. A development probe showed that polynomial `div_` on Laurent expressions loses terms; it is deliberately not used.

The existing 20,000-term and free-index eligibility guards remain conservative bounds. Zero coefficients bypass division. Epsilon, matrix and colour mappings retain ordinary grouping. Batches and stable output ordering remain; `ModuleOption inparallel` schedules the newly defined residual expressions. Content extraction itself is performed by the preprocessor. There is no claim that a single content scan is multithreaded. No coefficient is wrapped in a single FORM function argument.

The importer already accepts products of parenthesised factors. Full polynomial factorisation can produce smaller output; this change chooses cheaper generation rather than claiming equivalent compression. It does not guarantee that the full result will fit in Mathematica memory.

## Verification

The wider suites passed: Core 111, Parser 121, Transactions 20, Installer 62, Export 31, FormStages 128, Import 65, Runtime 82, Epsilon 48, DiracColour 65, DiracAlgebra 104 and ColourAlgebra 139 assertions.

The final focused suite passed 88 assertions with no failures. It covers common numerical factors, negative symbol and scalar-product powers, scalar functions, complex coefficients, multiple batches, free-index and size fallbacks, and exact serial/TFORM round trips. General grouping, malformed-output and legacy factor-reading checks remain in the suite. A retained 64-group result was compared to the original unfactorised expression by FORM subtraction, returning exactly zero.

## Measurements

The retained 64-group sample is a subset, not the complete user calculation. Source contraction has already been performed in this fixture, so these timings measure regeneration of grouped output, not the full original calculation. Tests used TFORM 5.0.2 with eight workers. After regressions finished, three common-factor runs and one full-factorisation control ran sequentially, with the control between the first and second common-factor observations.

| Output stage | Wall time | Peak process RSS | Output bytes |
| --- | ---: | ---: | ---: |
| Common-factor extraction | median 1.82 s; range 1.63–1.94 s (3 runs) | 308,613,120–332,214,272 | 19,189,632 |
| Previous parallel full factorisation | 53.13 s (1 run) | 742,207,488 | 12,236,847 |
| Grouping alone, retained reference | Not timed here | Not measured here | 26,012,080 |

Common-factor output is about 26.2% smaller than grouping alone, but 56.8% larger than full-factorisation output. This is a measured speed/compression trade-off on this sample, not a portable guarantee. No stop condition was reached.

A fresh-kernel import of the common-factor sample completed in 52.34 seconds (56.51 seconds including kernel startup/loading), with peak process-tree RSS 3,605,364,736 bytes (3.36 GiB). The returned expression occupied 357,942,152 bytes; retained kernel memory was 3,415,120,064 bytes. These are different memory measures. No stop condition was reached. The older full-factorisation import observation was 51.19 seconds with a 253,050,816-byte expression; it was not remeasured in this turn and is not a matched import-speed comparison. Common extraction does not preserve full factorisation's expression-size reduction.

## Sources and evidence

The [official FORM 5.0.2 manual, content_](https://form-dev.github.io/form-docs/stable/manual/#content_) describes common-factor extraction. The [earlier parallel-factorisation report](ParallelCoefficientFactorisation.md) retains the previous implementation and measurements as historical evidence.

Temporary probes, equality results and bounded measurements are under `/tmp/cfc-parallel-factor`; regression logs are `/tmp/cfc-content-{core,form,runtime}.log`. Trials use 300 seconds, 6 GiB process-tree RSS and 2 GiB available memory stop conditions. Existing user calculation files, notebooks and stored libraries were not changed. No Full benchmark or commit is included.
