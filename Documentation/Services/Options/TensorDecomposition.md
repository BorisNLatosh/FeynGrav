# TensorDecomposition

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`TensorDecomposition``. **Baseline:** 4.2.0.

This is an option symbol; it is supplied as a rule to an owning command, not called as a function.

## Meaning and accepted values

TensorDecomposition is an option for CalcFormExport and CalcFormCalculate, default True. With exactly one declared LoopMomenta entry, interpret the expression as a one-loop integral and decompose its polynomial numerator into coefficient functions in FORM. False retains integrand algebra.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [CalcFormExport](../Calculation/CalcFormExport.md) | `True` |
| [FeynGravFORMExport](../Calculation/FeynGravFORMExport.md) | `True` |
| [CalcFormCalculate](../Calculation/CalcFormCalculate.md) | `True` |
| [FeynGravSimplify](../Calculation/FeynGravSimplify.md) | `True` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
