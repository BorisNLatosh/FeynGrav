# Documentation verification — 6 October 2026

## Scope

The initial update changed Markdown documentation only. The follow-up below also corrects two Nieuwenhuizen usage strings. Neither update changes computational definitions, interaction libraries, saved formats, templates or example/benchmark notebooks. No full algebra regression suite or Full benchmark was run. The source is a development snapshot; older release compatibility is not inferred from these checks.

## Coverage and source review

- The reference covers every unique usage-bearing name in `FeynGrav.wl`, the public Nieuwenhuizen utilities and all commands in `FeynGravCommands[]`.
- Fresh-kernel public-name inspection additionally identified the introduction printer, initialisation state and gravitational coupling, which are documented. `validationFile` is a loading implementation symbol rather than an advertised callable interface; it is excluded from the public reference.
- Signatures were checked against definitions and library-binding constructors. The reference distinguishes flat pairs, flat triples and separate arguments, including G2's pair-only interface and the ghost leg placeholders.
- Options, gauge defaults, import ranges and generator family selection were checked in source. The fixed Nieuwenhuizen `1/3` convention is documented from the implementation rather than silently replaced by a different D-dimensional convention.
- Benchmark timing boundaries and explicit serial baselines were inspected against the support code. Benchmark notebooks were not evaluated or rewritten.

## Fresh-kernel checks

24 main-package/converter checks passed, followed by 3 generator checks in a separate kernel.

The checks cover loading without external processes, unchanged working directory, initialisation success, scalar-propagator equivalence after FCI normalisation, momentum-first polarisation signatures and dimensions, option/gauge defaults, invalid importer orders, one loaded scalar vertex, the projector coefficient, missing-executable diagnostics and failure-payload access.

Real serial FORM and two-worker TFORM each checked four small expressions: metric trace `D`, fully contracted four-dimensional epsilon `-24` under the current default convention, two-gamma trace `4 MTD[mu,nu]`, and two-generator colour trace `SUNDelta[a,b]/2`. These are representative documentation checks, not exhaustive convention or algebra coverage.

The documented scalar generator example returned `Null` and produced two readable files in a newly created temporary directory. It did not replace repository libraries. Installation was never invoked.

## Static and preservation checks

148 relative Markdown links and heading anchors, contents links, code-fence balance and coverage of 72 usage/listed API names were checked. The preservation inventory covered 175 existing non-Markdown files. Markdown tables and mathematical notation were reviewed in source; no native notebook or browser rendering claim is made. British spelling is used in authored prose while executable names and third-party material retain their original spelling.

The initial Markdown update passed a before/after SHA-256 check of all existing non-Markdown files, including stored libraries, Wolfram sources, templates and example/benchmark notebooks. The subsequent source-help correction changes only the two usage strings identified below. Historical reports retain their measurements and qualifications; their prose and navigation may have been corrected.

## Nieuwenhuizen source-help corrections — resolved

The follow-up corrects two usage strings in `Rules/Nieuwenhuizen.wl`:

- `GaugeProjectorBar` now displays its own name in the signature.
- `NieuwenhuizenOperator2` now displays the two distinct transverse products, `theta(mu,alpha) theta(nu,beta)` and `theta(mu,beta) theta(nu,alpha)`, matching its definition. The trace subtraction and fixed `1/3` convention are unchanged.

The [function reference](../Reference.md#projectors-and-operators) already gives the matching definitions. A source comparison confirms that all content outside these two usage blocks is unchanged. No computational definition was modified.

## Converter documentation refresh — 10 October 2026

This follow-up changes Markdown only. It cross-checks the retained converter
implementation, its regression tests and the latest verification record. It
does not repeat the earlier main-package signature audit or run calculations.

- Updated automatic grouping, guarded cancellation, coefficient processing,
  stage symmetry and held-parser descriptions, including the explicit stage
  sort/restoration interfaces and nested-zero diagnostic checks.
- Corrected the future-work boundary: numerator cancellation exists; integral
  reduction with repeated propagators remains outside this implementation.
- Added missing companion modules, navigation entries and links to the latest
  optimisation/review evidence. Marked superseded implementation reports as
  historical without changing their recorded measurements.
- Kept the 26.77-second observation and 63.44-second correctness rerun qualified
  as individual measurements. The 1,964 regression checks were completed for
  the preceding code correction; they were not rerun for this prose-only edit.
- Checked every local Markdown link and heading target, code-fence balance and
  `git diff --check`. All checks passed.
- SHA-256 comparisons confirmed that Wolfram source, test scripts, FORM
  templates, JSON records and saved notebooks were unchanged by this update.

External websites were not revalidated. No Full benchmarks, interaction-library
regeneration, public API changes or commit were included.
