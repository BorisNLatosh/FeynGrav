# CalcFormCalculate

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`CalcFormCalculate``. **Baseline:** 4.2.0.

Exports, executes FORM and imports the resulting expression in one call.

## Syntax and arguments

```mathematica
CalcFormCalculate[expr, opts]
```

`expr` is a supported exact FeynCalc expression. For a binary equality it calculates the residual `lhs - rhs` and compares it with zero; an unresolved result may remain symbolic. Boolean `True` and `False` return directly. A calculation failure is distinct from a mathematically false identity.

## Options

- [TensorDecomposition](../Options/TensorDecomposition.md)
- [ReduceLoopCoefficients](../Options/ReduceLoopCoefficients.md)
- [ReduceToScalarMasters](../Options/ReduceToScalarMasters.md)
- [DropScaleless](../Options/DropScaleless.md)
- [Dimension](../Options/Dimension.md)
- [LoopMomenta](../Options/LoopMomenta.md)
- [DiracAlgebra](../Options/DiracAlgebra.md)
- [ColourAlgebra](../Options/ColourAlgebra.md)
- [FORMExecutable](../Options/FORMExecutable.md)
- [FORMThreads](../Options/FORMThreads.md)
- [TimeConstraint](../Options/TimeConstraint.md)
- [WorkingDirectory](../Options/WorkingDirectory.md)
- [KeepFiles](../Options/KeepFiles.md)
- [ShowTiming](../Options/ShowTiming.md)
- [ShowProgress](../Options/ShowProgress.md)

Defaults are per command; inspect `Options[CalcFormCalculate]` for the current session. An option passed in one call does not modify later defaults.

## Algebra and one-loop interpretation

An empty `LoopMomenta` list retains integrand algebra. With exactly one declared loop momentum and `TensorDecomposition -> True`, the full pipeline interprets the expression as a one-loop integral, shifts routes and decomposes supported polynomial numerators. Default `ReduceToScalarMasters -> True` reduces supported coefficients through boxes; `ReduceLoopCoefficients -> False` does not disable the power lowering needed for that reduction.

Returned integral coefficients carry `I Pi^2` in the documented convention. `DropScaleless -> False` preserves formal scaleless terms. No scalar integral evaluation, dimensional expansion, omitted diagram factors or external wavefunctions are supplied automatically.

Ordinary Dirac and fundamental SU(N) algebra, rank-four single-space epsilon tensors and supported polarisation objects follow the full pipeline's documented input restrictions. Mixed dimensions, unsupported structures and invalid options can be rejected. Coefficient simplification is bounded; success does not promise maximal compactness or that every larger expression will fit the runtime limits.

## Files, timing and failures

Unique temporary jobs are used. Successful jobs are removed unless `KeepFiles -> True`; failed jobs retain diagnostics. `TimeConstraint` bounds external execution rather than the whole call. Check `FailureQ` before interpreting a returned object as a physical result. See the detailed guide for timing scopes and cancellation limitations.

## Example

In a fresh kernel with symbolic indices and momenta:

```mathematica
<< FeynGrav`
result = CalcFormCalculate[MTD[mu, nu] FVD[p, mu] FVD[q, nu]];
If[FailureQ[result], result, result === FCI[SPD[p, q]]]
```

Expected result: `True` when FORM is available and the contraction succeeds. The contraction gives `SPD[p, q]` in FeynCalc internal form. A `Failure` is displayed directly; a `False` comparison requires inspection of `result`.



## Related documentation

- [Complete converter guide](./Guide.md)
- [Supported vocabulary](./Guide.md#Supported%20vocabulary)
- [Troubleshooting](./Guide.md#Troubleshooting)
- [Common normalisation conventions](../../Conventions.md)
