# One-loop tensor decomposition verification

> Historical stage report. Later [repeated-power reduction](LoopCoefficientReduction.md) and [scalar-master reduction](ScalarMasterReduction.md) extend this pipeline. To reproduce coefficient-only behaviour with the current defaults, explicitly set `ReduceToScalarMasters -> False` and `ReduceLoopCoefficients -> False`. The measurements and checks below retain their original scope; they are not timings of the later default master-reduction pipeline.

Date: 10 October 2026.

Environment: Wolfram 15.0.1, FeynCalc 10.2.1, FORM/TFORM 5.0.2 on Linux.
Real process checks ran outside the tool sandbox, which blocks Wolfram's
subprocess launch. No package installation was performed.

## Implementation checked

The new default acts only with one explicitly declared loop momentum.
Decomposition takes place in the embedded `cfcTensorDecompose` FORM procedure;
Wolfram validates families and reconstructs coefficient functions. Repeated
powers, explicit dimensions and formal scaleless terms use
`LoopTensorCoefficient`. Ordinary coefficients retain PaVe notation with
optional reduction and reordering disabled. The normalisation is `I Pi^2`.

## Initial implementation checks

| Suite | Assertions | Result |
| --- | ---: | --- |
| Existing core/parser/transaction/installer suite | 766 | Passed |
| Existing FORM export/staging/import suite | 257 | Passed |
| Existing runtime, epsilon, Dirac, colour, grouping and cancellation suites | 941 | Passed |
| New tensor-decomposition suite | 90 | Passed |

The new suite compares bubble ranks 0–6 and tadpole/triangle/box ranks 0–3
against FeynCalc `TID` in a coefficient basis. Scalar-master and PaVe notation
are canonicalised for comparison outside the production path; explicit PaVe
presentation options are removed from the reference comparison. Epsilon slot
permutations are compared algebraically, including all four supported epsilon
signs. These comparisons pass; they are not based on textual identity.

Repeated-power vacuum tensors at ranks 0, 2 and 4 agree with mass
differentiation of the unit-power reference. Independent convergent radial
moments check the normalisation and Wick-rotation sign. Serial FORM and TFORM
agree for combined Dirac, colour and epsilon expressions with either algebra
sector enabled or disabled. Loop-dependent explicit Dirac traces also pass in both processing modes.

Additional checks cover rational/reversed routing, coalesced propagators,
pinches, spectator sums, formal polynomial integrals, explicit dimensions,
null massless kinematics, scaleless removal, unknown scales, same-named colour
and loop symbols, coefficient round trips, invalid options, malformed
metadata/results, unknown identifiers, chunk sizes down to one byte, CRLF,
changed scalar products, import-local state and stream closure on abort.

Independent and main-package loading checks pass without process launches,
directory changes or changes to FeynCalc definitions/options. The updated
headless conventions report returns Null and preserves the checked state.
Existing saved-format checks remain passing. Repository libraries, interaction
rules and saved notebooks were not modified.

## Limits

The initial verification did not run the Full benchmark suite, a realistic
complete-bubble decomposition or a GUI rendering session. The large-case
follow-up below is separate from those initial checks. Arbitrary finite rank is supported by the recurrence,
but the comparisons above do not establish every rank or topology.

The generalised-coefficient checks include vacuum mass derivatives and radial
moments; they do not constitute an independent numerical integration survey of
all repeated-power triangle and box coefficients. No scalar-master reduction,
coefficient evaluation, Laurent expansion or UV/IR separation is implemented.
Scaleless classification occurs during reconstruction and preserves undecided
cases. The final expression and each streamed group must still fit in memory.

## Large quadratic-gravity follow-up

The user reported a calculation exceeding 600 seconds. The retained programme
was inspected without altering the notebook or its files. A temporary copy of
its pre-decomposition stages did not finish within a 300-second limit.
Version six had disabled optimisations previously gated on version one.

Corrections:

- Select pre-integration dimension compaction, verified stage symmetries and
  cancellation sectors from the recorded algebra capability, retaining the
  version-six mapping and integral interpretation.
- Select metric slots with native `sum_`/`distrib_` and pair them with `dd_`,
  avoiding recursive generation of equivalent metric placements.
- Collect dimension-dependent rational coefficients again after decomposition.
- Use the existing bounded coefficient parser for eligible scalar integral
  groups. Validate complete dimension-ratio calls as typed scalar factors.
- Decode the integer-polynomial subset of these ratios without a general
  recursive parse. Cache only their validated denominators in a separate
  import-local cache (at most 512 entries, 16 KiB per entry). Other syntax
  retains the general parser and its diagnostics.

The supplied expression was reconstructed with deterministic plain-symbol
arguments in a fresh kernel. FORM trials used ten workers sequentially, with
limits of 300 seconds, 6 GiB process RSS and 2 GiB available system memory.
Small regression kernels also ran during parts of this investigation, so these
are diagnostic observations rather than controlled speedup measurements.

| Candidate | FORM wall time | Output | Sampled peak RSS |
| --- | ---: | ---: | ---: |
| Restored pre-integration optimisations | 156.37 s | 135 MiB | 2.60 GiB |
| Native metric-slot distribution | 85.00 s | 135 MiB | 2.66 GiB |
| Plus final dimension-coefficient collection | 95.07 s | 51.27 MiB | 2.63 GiB |

The first two complete outputs are byte-identical. An independent FORM
subtraction of the first and final complete outputs, in a common rational
dimension basis, gives exactly zero (26.07 seconds, 2.01 GiB sampled peak RSS).
The temporary comparison script initially had an incorrect FORM module order;
that verification-script error was corrected before the successful comparison.

Focused verification now passes 114 assertions, including independent
FeynCalc comparisons, repeated-slot multiplicities through rank eight,
serial/TFORM agreement, stage selection, typed polynomial decoding and malformed
token boundaries. The existing core suite passes 766 assertions and the existing
FORM export/staging/import suite passes 257. No Full benchmark or stored-library
generation was run.

The general-parser import was stopped after 249.20 seconds without a result.
Enabling grouped parsing alone also did not finish within the 300-second limit;
the owned Wolfram process ignored termination and was killed at 327.40 seconds.
Both are incomplete trials, not successful import timings. The final result
contains 392,172 dimension-ratio calls, 149,017 distinct ratios after whitespace
normalisation, and 169 distinct denominator polynomials.

Programme SHA-256 identities for the temporary large-case probes:

| Programme | SHA-256 |
| --- | --- |
| User's retained export | `27c0d6abfae31233a56e2ce8f86e7aa27c9568d255969a8fa9dcee61f4b2faec` |
| Restored pre-integration optimisation probe | `90709743d5bfc82c3702ced782fa7073de030b8e96c3b8fd71e89bc2926bd535` |
| Native placement probe | `d82dbc779b487387f1c16a03274033f85212a49fcd155584d10672e489c31e77` |
| Final coefficient collection probe | `e075620aa23ed32a7b203224739ffcdf061e35980e426962b57b0727358245f6` |

### Completed final import

The polynomial-aware importer completed on the unchanged 53,757,833-byte
(51.27 MiB) final output:

- `CalcFormImport` wall time: **255.69 seconds**.
- Fresh process including package loading and reporting: 261.23 seconds.
- Sampled peak process RSS: 600,500 KiB (586.43 MiB).
- Retained Wolfram kernel memory reported after import: 483,888,600 bytes.
- Returned expression: 30,425,536 leaves, no `Failure`, and no remaining
  `Momentum[l, ...]` dependence.

These are separate export/FORM/import observations, not a measured total of a
single public `CalcFormCalculate` call. The successful FORM and import stages
together took about 351 seconds, excluding input construction, export and
availability checks. The original user export remains unchanged. Restart the
Wolfram kernel and rerun notebook setup to load the corrected definitions.

The complete result remains large. This correction restores a functioning path
for the reproduced example; it does not establish a runtime bound for other
integrands or evaluate the resulting coefficient functions.
