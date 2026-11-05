# DiracAlgebra

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`DiracAlgebra``. **Baseline:** 4.2.0.

This is an option symbol; it is supplied as a rule to an owning command, not called as a function.

## Meaning and accepted values

DiracAlgebra selects ordinary open-chain simplification and explicit trace evaluation in FORM (Automatic), or translation only (False).

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [CalcFormExport](../Calculation/CalcFormExport.md) | `Automatic` |
| [FeynGravFORMExport](../Calculation/FeynGravFORMExport.md) | `Automatic` |
| [CalcFormCalculate](../Calculation/CalcFormCalculate.md) | `Automatic` |
| [FeynGravSimplify](../Calculation/FeynGravSimplify.md) | `Automatic` |
| `Generate* / Generate*Specific` | `Automatic` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
