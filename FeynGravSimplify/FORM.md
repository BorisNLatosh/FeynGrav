# Restricted FORM method for benchmarking

```mathematica
FeynGravSimplify[expr, Method -> "Contractor", ContractorThreads -> 1]
FeynGravSimplify[expr, Method -> "FORMBenchmark", FORMThreads -> 1]
```

Use `ContractorThreads -> 8` and `FORMThreads -> 8` for eight-worker comparisons;
the latter uses TFORM. `ContractorThreads` defaults to `8`; `FORMThreads`
defaults to `Automatic`, which selects up to eight workers according to the
kernel processor count and serial `form` when only one worker is available.
`Method -> Automatic` on the unified facade dispatches to the full
`CalcFormConverter` pipeline, not to a restricted method; the restricted FORM
method is requested explicitly with `Method -> "FORMBenchmark"`.
`CalcFormCalculate` is unchanged.

## Shared algebraic scope

Both methods use the same input validator, ExpressionJSON tree and per-call
symbol map. The FORM method translates that tree to a single factored `Local`
expression with native metric/vector objects and a final sort. Its operations are
polynomial expansion, Lorentz contraction and coefficient collection. No staged
`Multiply` sequence, factor-order planner, rational-function facility, momentum
expansion, propagator cancellation, Dirac/colour/epsilon processing or integral
reduction is added. No private CalcFormConverter helper is called.

As in Contractor, positive int32 integer powers remain visible for expansion.
Other scalar powers are opaque, including their bases. Scalar functions and
propagator denominators retain the same opacity. Composite momentum expressions
remain distinct mapped vectors, so FORM does not distribute `Momentum[p-q,D]`.
Polarisation momentum sources and the other unsupported Contractor inputs remain
unsupported here. The adapter performs no Mathematica simplification pass.

This matches the intended algebraic operations, not an identical implementation
or traversal order. Native FORM may normalise tensor objects during expansion.
FORM has arbitrary-precision integer coefficients; Contractor uses bounded
coefficient arithmetic and preserves overflowed operations symbolically. Tests
compare algebraic answers, not printed forms or overflow-recovery expression
shapes. Overflow-heavy inputs therefore do not compare identical arithmetic work.

## Execution and output

`FORMExecutable -> Automatic` searches PATH for `form` with one worker and `tform`
with multiple workers. An explicit executable is used as supplied; there is no
fallback. `FORMThreads -> Automatic` selects `Min[8,$ProcessorCount]`, with one
worker if a valid kernel count is unavailable. A requested parallel configuration
is checked in the generated program against TFORM's actual worker count. The
reused option symbols do not change `CalcFormCalculate`'s own option defaults.

A fresh directly launched process receives `input.frm` as an argument. It writes
`result.txt`, delimited by required start/end markers. A dedicated arithmetic
parser accepts only generated identifiers, native metrics/vectors, exact numbers
and permitted operators; the common decoder validates the reconstructed tree.
No returned text is evaluated with `ToExpression`. Missing, malformed or unknown
output produces `Failure` even after a zero exit status.

`ShowProgress`, `ShowTiming`, `TimeConstraint` and `KeepFiles` also apply to FORM.
Progress messages name FORM; they report elapsed time approximately every ten
seconds while running. The timeout concerns execution, not encoding or decoding.
Cleanup and diagnostics share the existing native runtime. FORM uses an input
file and closes stdin, so it does not transmit a large request through the
Contractor stdin-write path. Neither timeout nor worker count is a memory cap.

Retained FORM jobs contain `input.json`, `input.frm`, `mapping.json`,
`opaque-trees.json`, `stdout.log`, `stderr.log`, `result.txt` when produced, and
`process.json`. The latter records the selected method, command, executable hash,
status and elapsed time. Failed jobs are retained; successful jobs are deleted
unless `KeepFiles -> True`. Loading defines both methods without probing or
launching either executable.

## Benchmark interpretation

Use identical factored inputs, explicit equal worker counts, warmups and
alternating repetitions. Keep correctness checks outside timing. Measure complete
calls with `AbsoluteTiming`, or distinguish them from the process time printed by
`ShowTiming`; frontend conversion and result parsing still differ between methods.

The existing `Benchmark/SmallContractions` and `Benchmark/LargerContractions`
records compare Contractor with **CalcFormCalculate**, including its staged
multiplications. They are historical evidence for that earlier comparison and
must not be relabelled as measurements of this restricted FORM method.

Verification: [12 October restricted-FORM checks](FORMVerification.md).
