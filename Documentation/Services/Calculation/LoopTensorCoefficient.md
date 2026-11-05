# LoopTensorCoefficient

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`LoopTensorCoefficient``. **Baseline:** 4.2.0.

An **inert output head** for a normalised one-loop tensor coefficient. It is not an evaluator.

## Structure

```mathematica
LoopTensorCoefficient[indices, chords, massesSquared, powers, dimension]
```

`chords` starts with zero and labels denominators `(q + chord)^2 - massSquared`. `massesSquared` and positive integer `powers` match the denominator family. Paired zero labels in `indices` represent metrics; `{0}` denotes a scalar coefficient. Empty families retain polynomial vacuum integrals. `dimension` is the coefficient's dimension.

## Normalisation and reduction

The corresponding unnormalised integral carries `I Pi^2`. Do not add this factor twice when combining returned converter expressions. There are no options on the inert head itself.

`CalcFormCalculate[expression, ReduceLoopCoefficients -> True]` can lower supported repeated powers. Default `ReduceToScalarMasters -> True` additionally reduces supported coefficients through boxes to scalar PaVe masters. Formal coefficients can remain for unsupported, singular or scaleless cases; they are not zero. No scalar integral evaluation or dimensional expansion is implied.

See the [one-loop guide](./Guide.md#One-loop%20tensor%20decomposition) and [calculation command](./CalcFormCalculate.md).
