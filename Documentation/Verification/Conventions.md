# Conventions report verification

Checked on 9 October 2026. This report concerns `FeynGravConventions[]`, implemented in `Conventions.wl` and loaded by the main package. No interaction formulas or stored libraries were changed.

## Interface and state inspection

A temporary fresh-kernel script passed 24 assertions covering:

- Six report sections, usage text, command discovery, invalid arguments and a `Null` success return.
- Plain-text output without raw associations, box expressions, held wrappers or absolute paths.
- Fixed mathematical display under assignments to `D`, the gravitational coupling, the vector gauge and the epsilon sign.
- Current dimension, epsilon sign, Dirac scheme, trace normalisation and colour-processing defaults.
- The effective FeynCalc vector transversality option, which is read from `Options[Polarization]` because `PolarizationVector` uses that option source.
- Unavailable option/getter information, missing import records, matching settings and changed settings.
- Failed imports preserving records, successful reimports updating the displayed orders/settings, and historical unassigned symbols remaining symbolic after later assignments.
- Unchanged working directory, import registry, representative vertex definitions and options after reporting. Instrumented file-writing, loading, hashing and process functions were not invoked by the report.

The scripts were temporary; no top-level test directory was introduced. The existing import bookkeeping performs the actual record keeping. The report does not create another registry.

## Mathematical checks

Independent FeynCalc contractions gave exact agreement in four manageable comparisons:

1. The linearised connection from `GammaTensor` and the conventional metric expansion.
2. The linear scalar curvature obtained with the supplied Riemann index order and positive Fourier phase, giving the coefficient
   `kappa (k^2 eta^(rho sigma) - k^rho k^sigma)`.
3. The one-graviton Horndeski G4 `phi^2 R` vertex and `I lambda` times that coefficient.
4. The one-graviton scalar vertex and
   `I kappa/2 (p_rho q_sigma + p_sigma q_rho - eta_(rho sigma) (p.q + m^2))`.

These checks support the stated convention at representative linear order. They are not a verification of every nonlinear curvature term or every stored interaction. No discrepancy was found in these comparisons. The gravitational coupling remains symbolic; the displayed Newton-constant relation is not an automatic substitution.

## Display

The installed Mathematica front end produced 44 static print cells and returned `Null`. The output contained no dynamic boxes. The report was printed to a temporary four-page PDF and visually reviewed: all six sections were present, with mathematical formulas, wrapped explanations and compact tables. Separate print cells avoid clipping an oversized single output cell. Plain-text output was checked separately in a kernel without a front end.

The frontend check used the normal FeynCalc-loaded formatting environment. Tables are explicitly protected by `StandardForm`, while mathematical boxes use `TraditionalForm`. Loading the package did not print the report automatically.

## Scope and preservation

The main-package changes relative to the pre-task file are the usage message, command-list entry and companion loader. Documentation was updated in the README, reference and index. Existing uncommitted generator/bookkeeping work was preserved. Stored libraries and example/benchmark notebooks were not edited or regenerated. No FORM process, full algebra regression or Full benchmark suite was needed for this read-only report. No commit was made.

## Review corrections

A subsequent independent review reproduced two reporting defects: the three FeynGrav polarisation wrappers displayed their declared options instead of the effective inherited `Polarization` option, and delayed option rules were shown as unavailable.

Both are corrected. The transversality table now uses `Options[Polarization]` for all four commands. Delayed defaults are labelled and displayed with their contents held; the report does not evaluate them to obtain a value.

A fresh-kernel run passed 32 assertions: the original 24 report checks (with the updated table label), four inherited-transversality rows, a vector contraction confirming the inherited setting, a delayed constant, and two checks that a delayed expression with a side effect is displayed without executing it. The reference now documents both behaviours. These changes affect reporting only; no calculation definitions or library files were changed.
