# CalcFormExport

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`CalcFormExport``. **Baseline:** 4.2.0 development.

Exports a supported exact FeynCalc expression to a complete FORM program and a reversible JSON mapping. It does not execute FORM.

## Syntax and arguments

```mathematica
CalcFormExport[expr, file, opts]
```

`expr` is the expression; `file` is the destination `.frm` filename in an existing directory. The result is an association containing `InputFile`, `MappingFile` and `ResultFile`, or `Failure`. Existing targets are refused unless `OverwriteTarget -> True` is supplied.

## Options

- [TensorDecomposition](../Options/TensorDecomposition.md)
- [ReduceLoopCoefficients](../Options/ReduceLoopCoefficients.md)
- [ReduceToScalarMasters](../Options/ReduceToScalarMasters.md)
- [DropScaleless](../Options/DropScaleless.md)
- [Dimension](../Options/Dimension.md)
- [LoopMomenta](../Options/LoopMomenta.md)
- [DiracAlgebra](../Options/DiracAlgebra.md)
- [ColourAlgebra](../Options/ColourAlgebra.md)
- [OverwriteTarget](../Options/OverwriteTarget.md)

Defaults are per command; inspect `Options[CalcFormExport]` for the current session. An option passed in one call does not modify later defaults.

## Algebra and one-loop interpretation

An empty `LoopMomenta` list retains integrand algebra. With exactly one declared loop momentum and `TensorDecomposition -> True`, the full pipeline interprets the expression as a one-loop integral, shifts routes and decomposes supported polynomial numerators. Default `ReduceToScalarMasters -> True` reduces supported coefficients through boxes; `ReduceLoopCoefficients -> False` does not disable the power lowering needed for that reduction.

Returned integral coefficients carry `I Pi^2` in the documented convention. `DropScaleless -> False` preserves formal scaleless terms. No scalar integral evaluation, dimensional expansion, omitted diagram factors or external wavefunctions are supplied automatically.

Ordinary Dirac and fundamental SU(N) algebra, rank-four single-space epsilon tensors and supported polarisation objects follow the full pipeline's documented input restrictions. Mixed dimensions, unsupported structures and invalid options can be rejected. Coefficient simplification is bounded; success does not promise maximal compactness or that every larger expression will fit the runtime limits.

## Example

In a fresh kernel with symbolic indices and momenta:

```mathematica
<< FeynGrav`
jobDirectory = CreateDirectory[];
job = CalcFormExport[MTD[mu, nu] FVD[p, mu] FVD[q, nu],
  FileNameJoin[{jobDirectory, "contraction.frm"}]];
job
```

Expected result: an association containing `InputFile`, `MappingFile` and `ResultFile`. The program and mapping files exist after a successful export; the result file is created only when FORM runs. Each rerun of this block uses a fresh directory. If `job` is a `Failure`, resolve it before execution or import.



## Related documentation

- [Complete converter guide](./Guide.md)
- [Supported vocabulary](./Guide.md#Supported%20vocabulary)
- [Troubleshooting](./Guide.md#Troubleshooting)
- [Common normalisation conventions](../../Conventions.md)
