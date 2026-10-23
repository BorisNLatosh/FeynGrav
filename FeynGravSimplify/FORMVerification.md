> Source-path update (21 October 2026): the current `Simplification/` directory is now `FeynGravSimplify/`. Paths and hashes below describe their historical snapshots; external evidence archives were not moved.

# Restricted FORM verification — 12 October 2026

Branch: `Contraction_Integration_and_Benchmarking`, uncommitted changes over
`2bc3a3f`. This work does not merge the distinct `FORM_Simplification` branch.
Environment: Wolfram 15.0.1, FeynCalc 10.2.1, FORM/TFORM 5.0.2 on Linux;
Contractor source/build `641febda`. Source and executable hashes and retained logs
are listed in [the machine-readable record](Tests/Reports/2026-10-12-FORM.json).

- **95 restricted-FORM checks passed:** known algebraic answers and Contractor
  comparisons, serial/eight-worker execution, independent rank-two trace checks
  for products of four and six three-term tensors, free indices, composite
  momenta, opaque inverses/functions/propagators, huge coefficients including
  wrapped numeric output, identical unsupported-input failures, safe parsing,
  absence of delegation/Mathematica algebra, generated single-module structure,
  reporting, malformed output, process failure, large stderr, timeout and abort,
  retained diagnostics and absence of leftover running children or open streams.
- **73 Contractor integration checks passed** after sharing runtime handling.
- **42 representation checks passed.**
- **19 loading/installer checks passed:** no process on loading, preserved
  FeynCalc and CalcFormCalculate definitions/options, required module checks.

Total: 229 assertions across these runs. Assertions mix independent algebraic
comparisons and implementation regressions; they are not 229 independent proofs.
The tests use a Go process fixture only for runtime failures. No such helper is
required for production execution. The new parser handles FORM's indented decimal
continuations and rejects unsupported syntax without evaluating it.

No realistic amplitude, large benchmark, non-Linux execution or hard aggregate
memory cap was tested. The older Contractor-vs-CalcFormCalculate benchmark records
remain unchanged; no restricted-method speedup is claimed here. FORM's integer
arithmetic and Contractor's overflow preservation are not identical algorithms.

To reproduce the focused suite from the repository root, build the existing test
fixture as described in README.md and set `FG_CONTRACTOR_TEST_FIXTURE`, then run:

```sh
WolframKernel -noinit -noprompt -script Simplification/Tests/FORM.wls
```

The other suites retain their existing invocation requirements. Use bounded small
checks; a backend timeout is not an aggregate memory limit.
