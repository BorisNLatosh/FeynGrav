# Levi-Civita verification — 5 October 2026

> Historical implementation record. Forward-looking statements describe that stage. For current capabilities use the [converter guide](../../README.md). The original observations and limitations below are retained.

Real serial FORM and TFORM (two workers) passed the convention checks for
`$LeviCivitaSign` = -1, 1, -I, I. The final Epsilon suite passed 48 assertions.
The existing suites passed: Core 111, Parser 121, Transactions 20, Installer 62,
FORM Export 31, FORM Stages 128, FORM Import 65, Runtime 82 assertions.
The fixed version-one fixture was preserved. The Full benchmarks were not run.

The originally planned translation `-Sign e_` failed the first real-process
square test. The implemented factor is `-I Sign`; its square gives the required
negative sign for real epsilon conventions. Odd-epsilon round trips and all four
conventions were checked separately.

## Epsilon-free performance check

A deterministic sum of 1,500 distinct scalar coefficients multiplying powers of
one scalar product was tested in sequential fresh kernels. One warmup preceded
five measured public exports and imports. Each export used a fresh target;
imports reused that kernel's prepared output/mapping pair. Baseline source was
commit 4c20f0a. Values below are wall seconds: median [minimum, maximum].

| Order | Version | Export | Import |
|---|---|---|---|
| Baseline then candidate | Baseline | .113343 [.112272, .114830] | .133321 [.118650, .146782] |
| Baseline then candidate | Candidate | .148689 [.127599, .206902] | .132513 [.121115, .167553] |
| Candidate then baseline | Candidate | .112738 [.110610, .115390] | .123276 [.122023, .126439] |
| Candidate then baseline | Baseline | .115515 [.111245, .118126] | .134065 [.123268, .137839] |

The initial export slowdown did not reproduce in reverse order. This small,
noisy check shows no consistent regression; it does not establish a portable
performance guarantee or cover large epsilon workloads.
