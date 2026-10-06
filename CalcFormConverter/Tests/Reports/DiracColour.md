# Dirac and colour translation verification — 5 October 2026

> Historical implementation record. Forward-looking statements describe that stage. For current capabilities use the [converter guide](../../README.md). The original observations and limitations below are retained.

## Correctness

- DiracColour: 67 assertions passed, using serial FORM and TFORM with two workers.
- DiracColourRules: 23 assertions passed. Seven representative expression families
  were compared at zero, one and two gravitons: fermion, quark–gluon (an explicitly
  D-dimensional test copy), two gluons, three gluons, four gluons, Yang–Mills ghosts
  and gluon–ghost interactions. Two additional checks reject the original mixed
  quark–gluon expressions at positive graviton orders. The three-gluon test uses
  the contracted rule; the other listed translation inputs use uncontracted rules.
- Existing suites passed: Core 111, Parser 121, Transactions 20, Installer 62,
  FORM Export 31, FORM Stages 128, FORM Import 65, Runtime 82 and Epsilon 48.
- Automatic loading, independent reload, namespace isolation and absence of
  process launches during loading passed.
- Tests use FeynCalc algebra only as an independent comparison after translation.
  The converter itself adds no trace evaluation or colour reduction.

Final matrix-specific checks were repeated after the export dispatch adjustment.
No stored library or rule definition was changed. No Full benchmark suite or
complete library regeneration was run. The tests do not establish support for
arbitrary independent fermion lines, gamma-five schemes or colour reduction.

## Bounded epsilon-free timing check

The deterministic 1,500-coefficient workload from the epsilon timing check was
run in sequential fresh kernels, with one warmup and five measured imports and
exports each. Export targets were fresh; imports reused prepared file pairs.
The baseline was commit 4c20f0a (before both extensions), not an isolated snapshot
of the uncommitted epsilon work.

Initial measurements exposed extra export dispatch overhead. Extra emitter
rules are now installed only for matrix/colour jobs. The final sample was:

| Version | Export median [min, max], seconds | Import median [min, max], seconds |
|---|---|---|
| Candidate | .114559 [.111583, .124334] | .131344 [.123487, .133073] |
| Baseline | .114545 [.113008, .118810] | .121683 [.116843, .131050] |

Export medians are effectively equal in this sample. The candidate import
median is about 8% higher, with overlapping ranges. Earlier alternating runs
also varied; this bounded check cannot exclude a small import overhead and is
not a speedup claim. Large bosonic imports retain the existing flat parser;
version-three imports deliberately use the general parser for typed words.
