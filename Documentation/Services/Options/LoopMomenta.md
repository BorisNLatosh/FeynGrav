# LoopMomenta

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynCalc`LoopMomenta``. **Baseline:** 4.2.0 development.

This page describes only FeynGrav’s use of this dependency-owned option.

## Meaning and accepted values

A list of distinct unassigned integration-variable symbols. One entry activates default one-loop tensor decomposition. An empty list retains integrand algebra; multiple entries require `TensorDecomposition -> False`. Declaring loop momenta does not supply missing diagram factors or integrate scalar master functions.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [CalcFormExport](../Calculation/CalcFormExport.md) | `{}` |
| [FeynGravFORMExport](../Calculation/FeynGravFORMExport.md) | `{}` |
| [CalcFormCalculate](../Calculation/CalcFormCalculate.md) | `{}` |
| [FeynGravSimplify](../Calculation/FeynGravSimplify.md) | `{}` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
