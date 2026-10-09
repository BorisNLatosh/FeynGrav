# Per-coefficient factorisation implementation — 9 October 2026

Historical implementation: full polynomial factorisation was subsequently replaced by [common-factor extraction](CommonFactorExtraction.md). Recorded measurements below remain unchanged.

> Historical sequential implementation: [native parallel factorisation](ParallelCoefficientFactorisation.md) supersedes its one-at-a-time FORM execution. The measurements below remain unchanged.

## Delivered behaviour

Grouped version-one exports now factorise eligible commuting coefficients separately in FORM. Complete denominator products remain outside their coefficients. Jobs without mapped denominators are unchanged. Epsilon, Dirac and colour jobs retain their existing grouping; this change does not reinterpret their algebra or saved mappings.

Each coefficient is read from indexed brackets of the hidden result, sorted, and passed to native `Factorize` only if it contains no free Lorentz indices and at most 20,000 expanded terms. Other coefficients are written unchanged. The temporary factorised expression is discarded before the next coefficient. Factors are emitted as ordinary products through `numfactors_` and indexed factor access; internal `factor_` markers never enter saved results. No additional executable, download, Python dependency or public option is introduced. The FORM procedure is embedded in each exported file.

The incremental importer now recognises complete parenthesised factor products, parses each factor through the established restricted parser and multiplies without expansion. Individual large flat factors remain eligible for the fast parser. Legacy grouped output and mappings one through five retain their interpretation.

The size guard is not a per-coefficient time limit. Polynomial factorisation can be expensive even for smaller inputs. The existing `TimeConstraint` bounds total FORM execution, including factorisation. Timeout or abort retains diagnostic files and does not silently retry or publish a successful result. No new physical identities, denominator cancellations or scalar-abbreviation relations are introduced.

## Verification

Core (111), parser (121), export transactions (20), installer mocks (62), FORM export (31), FORM stages (128), FORM import (65), runtime (82), epsilon (48), Dirac/colour translation (65), Dirac algebra (104) and colour algebra (139) assertions all passed. These runs also include the existing saved-format compatibility fixtures. Independent/main-package loading and namespace isolation passed; documentation links and whitespace checks passed.

The final focused suite passed **66 assertions**: serial FORM and TFORM; exact factor retention; zero/unit groups; powers and routing; complex factors; free metric/component fallback; scalar PaVe functions; oversized-coefficient fallback; tiny read chunks; malformed factor products and unknown identifiers; individual-factor fast-parser use; unchanged matrix/epsilon handling and global colour endpoint checks; and stream cleanup on failure/abort. The large sample was run with serial FORM 5.0.2. Focused TFORM tests used two workers.

During development, FORM failed on a free-index metric when converting it to polynomial notation. The implementation now bypasses such coefficients. Native `occurs` detects component indices but did not detect indices inside `d_` in the tested FORM version, so the guard includes an explicit metric pattern. The regression suite verifies this fallback.

## Bounded 64-group comparison

The same retained first 64 groups from the verified original calculation were used, not a newly selected favourable subset. The original 26,012,080-byte subset was factorised in a standalone temporary program with the production coefficient-output procedure. It predates the final free-index guard; its coefficients are scalar, so that guard does not change this sample. The final importer was used for both import measurements.

All trials ran sequentially. The monitor enforced 300 seconds, 6 GiB process-tree RSS and at least 2 GiB available system memory. Each import used a fresh kernel. These are single first-import observations, not repeated-run medians. Kernel startup is excluded from import time but included in monitored process time and memory. RSS was sampled every 0.1 seconds.

| Measurement | Grouped, unfactorised | Grouped, factorised |
| --- | ---: | ---: |
| Result-file bytes | 26,012,080 | 12,236,847 |
| Import seconds | 71.51 | 51.19 |
| Returned expression bytes | 456,613,224 | 253,050,816 |
| Peak process RSS bytes | 2,746,048,512 | 2,680,754,176 |
| Retained kernel bytes | 2,587,285,904 | 2,530,842,880 |

Factorisation and output took 177.49 seconds and peaked at 296,026,112 bytes RSS. A separate FORM subtraction of the complete 64-group original and factorised expressions returned exactly zero. The comparison fixture removed output line wrapping before inserting expressions into FORM source: an isolated `*` at the start of a source line otherwise denotes a FORM comment. This did not alter any algebraic token.

The output is 53.0% smaller and the returned expression 44.6% smaller. Import was 28.4% faster in this observation, but peak process RSS improved by only about 2.4%. Factorisation adds substantial FORM work, so this is not an end-to-end speedup claim. Wolfram's retained kernel allocations differ from the size of the returned expression and from process RSS.

## Limits and evidence

The complete 1,022-group result was not factorised or imported in this stage. The successful subset does not establish that the full result fits in memory, nor a portable speedup. Given the measured factorisation cost, no extrapolated full runtime is claimed. No Full benchmark, interaction changes or stored-library regeneration was performed. Existing edits were preserved and no commit was made.

The [earlier three-coefficient pilot](CoefficientFactorisation.md) remains a historical record. Local implementation evidence is in `/tmp/cfc-factor-impl` and `/tmp/cfc-factor-tests`, including the zero residual, trial metrics and suite logs. The [official FORM manual](https://form-dev.github.io/form-docs/stable/manual/) documents indexed brackets, `Factorize`, `numfactors_` and factor access. These temporary artefacts are not required to use the package.
