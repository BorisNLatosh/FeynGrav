# Coefficient factorisation pilot — 9 October 2026

> Historical pilot: the subsequent [implementation report](CoefficientFactorisationImplementation.md) records the integrated output and factor-aware importer. The observations below used the earlier importer.

## Scope and method

This pilot tests native FORM 5.0.2 `Factorize` on three complete coefficients from the previously verified grouped result. Selection was deterministic: the lower quartile, median and largest group by non-whitespace text length among 1,022 groups. These are samples, not a complete workload trial.

No converter behaviour, public API or mathematical convention was changed. The original calculation files were read only; experimental files are in `/tmp/cfc-factor-probe`. No hidden rational identities, denominator cancellations, on-shell conditions or dimension substitutions were applied. In particular, existing scalar abbreviations remained independent symbols.

Each coefficient was placed in a separate FORM expression and factorised serially. FORM's `%E` output for a factorised expression contains `factor_` markers: it is not directly valid converter output. A temporary Python adapter checked their consecutive numbering and balanced factor boundaries, then wrote their product using ordinary parentheses and multiplication, retaining the original propagator prefactor and mapping header. This adapter is experimental tooling, not a proposed package dependency or production reader.

For each sample, a separate FORM job expanded the reconstructed factor product, subtracted the original coefficient and returned exactly zero. Both representations were then imported with the existing public `CalcFormImport` in separate fresh Wolfram kernels. Trials ran sequentially; baseline/candidate order alternated between samples. Each entry below is one first import, not a repeated-run median. No independent Mathematica expansion was included in the import timing.

The monitor used the existing limits: 300 seconds, 6 GiB process-tree RSS, or less than 2 GiB available system memory. All trials completed within those limits. RSS was sampled every 0.1 seconds, so very short process peaks may be missed. Kernel startup is excluded from import seconds and included in process RSS.

## Results

Coefficient text sizes below exclude whitespace, FORM's factor markers and the unchanged denominator prefactor.

| Sample | Original terms | Factors | Text bytes before | Text bytes after | Reduction | Factorisation wall time |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| q25 | 2,580 | 12 | 221,430 | 1,816 | 99.18% | 4.63 s |
| median | 4,633 | 9 | 396,388 | 204,604 | 48.38% | 0.31 s |
| largest | 15,616 | 6 | 1,302,877 | 789,350 | 39.41% | 1.21 s |

| Sample | Import before / after | Expression bytes before / after | Peak process RSS before / after | Retained kernel bytes before / after |
| --- | ---: | ---: | ---: | ---: |
| q25 | 0.782 / 0.377 s | 4,641,848 / 22,576 | 305,082,368 / 291,475,456 | 172,266,528 / 168,912,328 |
| median | 1.517 / 1.541 s | 8,259,184 / 4,095,800 | 356,548,608 / 306,196,480 | 218,597,744 / 179,443,984 |
| largest | 3.659 / 4.851 s | 27,516,768 / 16,957,296 | 447,152,128 / 340,320,256 | 272,154,608 / 209,654,904 |

All three FORM residuals were exactly zero. All six imports succeeded. `ByteCount` measures the reconstructed expression; retained Wolfram kernel memory and process RSS include other allocations and must not be interpreted as the same measurement.

## Interpretation and next step

The lower-quartile coefficient factorises particularly well. The median and largest coefficients retain large irreducible factors but still shrink appreciably. The sample does not justify extrapolating a full-output size or promising that the complete expression fits within 6 GiB.

Factorisation helps representation size and memory in all three samples. It does not uniformly improve import time: the largest sample's factored import is slower. Nested factor products require the general parser, while the original flat coefficients can use the optimised flat path. These single observations establish feasibility, not a stable timing speedup.

A production implementation should extract coefficients separately, preserve propagator grouping, write the factor product without exposing `factor_`, and validate reconstructed products. FORM's own factorised-expression representation and ordinary brackets are mutually exclusive, so appending a global `Factorize` to the existing grouped pipeline is not sufficient. A bounded fallback to the original coefficient would be needed for expensive or unsupported factorisations. Parsing individual factors through existing eligible fast paths merits a separate test.

The next useful integration experiment is per-coefficient factorisation with ordinary-product output and factor-aware import. No automatic factorisation was enabled by this pilot. No Full benchmark was run and no commit was made.

## Sources and evidence

The [official FORM 5.0.2 manual](https://form-dev.github.io/form-docs/stable/manual/) documents `Factorize` (section 7.58) and factor storage/access through `factor_` and `numfactors_` (chapter 11). These are the operations tested here; no assumptions were made about unrestricted tensor factorisation.

The temporary evidence directory contains `manifest.json`, `comparison.json`, selected coefficient source files, FORM programs and zero residuals, import scripts, per-trial memory/time JSON files and logs. These local artefacts are not installed dependencies. The unchanged package importer was used for every import.
