# Dimension

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynCalc`Dimension``. **Baseline:** 4.2.0.

This page describes only FeynGrav’s use of this dependency-owned option.

## Meaning and accepted values

Selects one Lorentz dimension. Accepts `Automatic`, a symbolic dimension other than `I`, or an integer at least 2. Automatic infers it from the input and uses D for purely scalar input. It must agree with existing tensor dimensions; this option does not convert tensors between dimensions.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [CalcFormExport](../Calculation/CalcFormExport.md) | `Automatic` |
| [FeynGravFORMExport](../Calculation/FeynGravFORMExport.md) | `Automatic` |
| [CalcFormCalculate](../Calculation/CalcFormCalculate.md) | `Automatic` |
| [FeynGravSimplify](../Calculation/FeynGravSimplify.md) | `Automatic` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
