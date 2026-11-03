# FeynGravSimplify

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`FeynGravSimplify``. **Baseline:** 4.2.0 development.

Unified calculation front end: full converter pipeline by default, or an explicitly selected restricted external method.

## Syntax and arguments

```mathematica
FeynGravSimplify[expr, opts]
```

`Method -> Automatic` and `"FORM"` use `CalcFormCalculate` semantics, followed by optional PaVe conversion. `Method -> "Contractor"` and `"FORMBenchmark"` select restricted adapters. There is no automatic backend fallback. Results are expressions or diagnostic `Failure` values.

## Options

- [Method](../Options/Method.md)
- [ContractorExecutable](../Options/ContractorExecutable.md)
- [ContractorThreads](../Options/ContractorThreads.md)
- [PaVeToABCD](../Options/PaVeToABCD.md)
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

Defaults are per command; inspect `Options[FeynGravSimplify]` for the current session. An option passed in one call does not modify later defaults.

## Algebra and one-loop interpretation

An empty `LoopMomenta` list retains integrand algebra. With exactly one declared loop momentum and `TensorDecomposition -> True`, the full pipeline interprets the expression as a one-loop integral, shifts routes and decomposes supported polynomial numerators. Default `ReduceToScalarMasters -> True` reduces supported coefficients through boxes; `ReduceLoopCoefficients -> False` does not disable the power lowering needed for that reduction.

Returned integral coefficients carry `I Pi^2` in the documented convention. `DropScaleless -> False` preserves formal scaleless terms. No scalar integral evaluation, dimensional expansion, omitted diagram factors or external wavefunctions are supplied automatically.

Ordinary Dirac and fundamental SU(N) algebra, rank-four single-space epsilon tensors and supported polarisation objects follow the full pipeline's documented input restrictions. Mixed dimensions, unsupported structures and invalid options can be rejected. Coefficient simplification is bounded; success does not promise maximal compactness or that every larger expression will fit the runtime limits.

## Method-specific behaviour

`Method` is the Wolfram **System** option symbol. `ContractorExecutable` and `ContractorThreads` belong to `FeynGrav`; the command itself belongs to `CalcFormConverter`.

The full pipeline defaults to `PaVeToABCD -> True`: supported final PaVe functions are converted to named functions such as A0/B0. `False` preserves the pipeline's original output. Both Boolean settings are no-ops for restricted methods. Conversion is not analytic evaluation of scalar integrals.

Restricted methods accept only their documented Lorentz/scalar vocabulary and fixed full-pipeline option baseline. They reject unsupported Dirac, colour, epsilon and polarisation structures and do not perform loop reduction. A non-baseline pipeline request returns `MethodCapability` before starting a backend. See the [restricted-method guide](./Methods.md) for the exact domain and process limits.

## Files, timing and failures

Unique temporary jobs are used. Successful jobs are removed unless `KeepFiles -> True`; failed jobs retain diagnostics. `TimeConstraint` bounds external execution rather than the whole call. Check `FailureQ` before interpreting a returned object as a physical result. See the detailed guide for timing scopes and cancellation limitations.

## Example

In a fresh kernel with symbolic indices and momenta:

```mathematica
<< FeynGrav`
result = FeynGravSimplify[MTD[mu, nu] FVD[p, mu] FVD[q, nu]];
If[FailureQ[result], result, result === FCI[SPD[p, q]]]
```

Expected result: `True` when FORM is available and the contraction succeeds. The contraction gives `SPD[p, q]` in FeynCalc internal form. A `Failure` is displayed directly; a `False` comparison requires inspection of `result`.



## Related documentation

- [Complete converter guide](./Guide.md)
- [Supported vocabulary](./Guide.md#Supported%20vocabulary)
- [Troubleshooting](./Guide.md#Troubleshooting)
- [Common normalisation conventions](../../Conventions.md)
