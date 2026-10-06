# FORM Dirac algebra verification — 5 October 2026

This report covers the processing extension after the translation-only stage
recorded in `DiracColour.md`. No performance measurements are claimed.

## Behaviour delivered

- `DiracAlgebra -> Automatic` on export and calculation: ordinary open-chain
  Clifford contractions and deterministic ordering, plus explicit Dirac traces.
- `DiracAlgebra -> False`: preserved word ordering and unevaluated traces.
- Native FORM `tracen` with per-occurrence `TraceOfOne` normalisation; one open
  line and independently allocated explicit trace lines.
- Embedded FORM procedures, version-four mappings and legacy format imports.
- General-parser validation of trace boundaries, malformed metadata/results,
  epsilon conventions and per-import state isolation.

Open-chain rules use a temporary noncommuting tensor, because it permits normal
FORM pattern matching while retaining native Lorentz contraction. The procedure
returns native `g_`/`gi_` objects before writing results. Ordering reduces adjacent
inversions; contraction terms shorten the word. No four-dimensional gamma-five
basis identities are used.

## Verification results

| Suite | Assertions | Result |
| --- | ---: | --- |
| Core | 111 | Passed |
| Parser | 121 | Passed |
| Export transactions | 20 | Passed |
| Installer mocks | 62 | Passed |
| FORM export | 31 | Passed |
| FORM stages | 128 | Passed |
| FORM import | 65 | Passed |
| Runtime | 82 | Passed |
| Epsilon | 48 | Passed |
| DiracColour | 66 | Passed |
| DiracAlgebra | 104 | Passed |
| DiracColourRules | 23 | Passed |

Total: 861 assertions passed. Loading checks additionally verified automatic
loading, independent reload, option help, unchanged working directory, unchanged
FeynCalc definitions (including DiracTrace and DiracSimplify), and no processes
launched during package loading.

The new tests use installed FORM 4.3 and TFORM with two workers. They cover known
Clifford identities, canonical-order idempotence, linear momentum routing,
scalar identities, trace sums/products/powers, multiple trace normalisations,
shared Lorentz indices, colour factors, epsilon tensors and all four supported
epsilon conventions. Twelve seeded generated slash words supplement the fixed
cases. The manual export–FORM–import result agrees with CalcFormCalculate.

Reference calculations run separately in FeynCalc with `DiracOrder -> True`.
FeynCalc's default leaves some anticommutator residuals unordered; initial checks
that omitted this option were corrected before judging algebraic agreement.
The translation-order regression explicitly opts out of the new processing.

The final runtime suites were rerun after trace-parser hardening. Representative
interaction checks cover zero, one and two gravitons; the original mixed-space
quark–gluon inputs remain rejected, with separate single-space copies used for
comparison. No interaction formula or stored library was modified.

## Boundaries

Gamma-five, chiral projectors, external spinors, explicit Dirac indices, nested
traces, implicit colour words inside traces and colour reduction remain outside
this implementation. Trace-containing inverse/fractional powers are rejected.
Canonical ordering need not minimise expression size. The generated finite
rewrite rules have quadratic source size in the Lorentz dictionary size.

No Full benchmark suite, library regeneration, generator migration or commit
was performed. Tests do not establish support for the excluded structures.
