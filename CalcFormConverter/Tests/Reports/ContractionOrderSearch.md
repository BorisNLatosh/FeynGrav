# Bounded contraction-order search — 9 October 2026

## Scope

The private stage planner now considers the existing connected greedy path from each tensor-bearing starting factor for products with at most eight stages. It chooses smaller maximum open-index width, then smaller total width, then narrower late intermediates. A complete score tie retains the baseline. This is a bounded heuristic, not an optimal tensor-network solver. It adds no algebraic assumptions and performs no Wolfram-side expansion or contraction. Products with more than eight stages retain the original greedy order. Ambiguous signatures, scalar-only products and the existing epsilon/matrix/colour exclusions retain their established behaviour.

Serialisation still precedes planning, so identifier dictionaries, contexts, mapping formats and public commands/options are unchanged. Prepared factors and output processing are unchanged; only multiplication order is selected differently.

## Full-expression experiment

The six prepared factors contained 31, 1,173, 31, 163, 1,173 and 31 terms. The original order was `{1,2,3,4,5,6}`.

- Moving the last small factor earlier, `{1,2,3,4,6,5}`, produced 12,346,066 intermediate terms before the final large multiplication, versus 746,758 in the original path. It did not complete within 300 seconds and was stopped. Its partial output is not a successful calculation.
- The opposite-end connected order, `{6,5,1,4,2,3}`, had 638,639 terms before its expensive multiplication and fewer remaining open indices afterwards. The corrected complete run took 186.35 seconds externally (186.10 seconds reported by TFORM), with peak process RSS 5,136,146,432 bytes. Its complete output was byte-identical to the original result: SHA-256 `5990abf716a1d9b7a889282ea11adf5553345cd4188364c8d8a15d8154e79eaa`.

The initial temporary test harness rewrote an output path with an overbroad regular expression, damaging a later preprocessor guard. The opposite-end run completed the algebra but failed during output and is excluded from successful full-run timings. The corrected copy uses literal path replacement and preserves the original output template. The package and user files were not affected by this harness error.

The general planner selected the tested opposite-end order on the six saved tensor stages, without reference to physical momentum names or the expression's identity. The earlier instrumented baseline took 218.68 seconds. A fresh uninstrumented baseline was then run sequentially after the corrected candidate, with the same executable and ten workers:

| Configuration | External wall time | TFORM total CPU time | Peak process RSS |
| --- | ---: | ---: | ---: |
| Original order, fresh baseline | 213.50 s | 1,958.82 s | 5,309,362,176 bytes |
| Opposite-end order | 186.35 s | 1,659.92 s | 5,136,146,432 bytes |

The observed elapsed-time reduction is 12.7%; CPU time fell by 15.3%. These are single full-run observations, not medians. The fresh baseline output is also byte-identical to the candidate. Preparations, equations and output compression were unchanged. The candidate includes a few diagnostic timing/term-count prints; they were not subtracted from its time. Do not combine measurements from different revisions into a universal speedup.

## Regression checks

Final suites passed with no failures: Core 111, Parser 121, Transactions 20, Installer 62, Export 31, FormStages 137, Import 67, Runtime 82, Epsilon 48, DiracColour 65, DiracAlgebra 104, ColourAlgebra 139 and propagator grouping 100 assertions. Staging tests include a six-factor index graph, a more-than-eight-stage fallback and a monolithic FORM reference. Existing ambiguous-signature, scalar-only, free-index, repeated-index, mapping-traversal and preparation checks remain in place. An initial new unit fixture used equal factor sizes while expecting the unequal-size graph's path; it was corrected and the entire FORM suite rerun successfully. No package defect was hidden by changing the expected result.

Relative documentation links and `git diff --check` passed. The branch remains `FORM_Simplification`. Re-export existing FORM programs after reloading the package; old generated programs retain their original order.

## Evidence and limits

Temporary evidence is under `/tmp/cfc-order-audit`: bounded monitors, per-stage term counts/timings, completed and incomplete results, source copies, planner checks and checksums. Trials use ten workers with 300-second, 6-GiB process-RSS and 2-GiB available-memory stop conditions. No Full benchmark, library regeneration or commit is included. User calculation files remain unchanged.
