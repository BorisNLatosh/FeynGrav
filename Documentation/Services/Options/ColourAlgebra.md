# ColourAlgebra

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`ColourAlgebra``. **Baseline:** 4.2.0 development.

This is an option symbol; it is supplied as a rule to an owning command, not called as a function.

## Meaning and accepted values

ColourAlgebra selects SU(N) reduction (True or Automatic) or preservation (False).

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [CalcFormExport](../Calculation/CalcFormExport.md) | `True` |
| [FeynGravFORMExport](../Calculation/FeynGravFORMExport.md) | `True` |
| [CalcFormCalculate](../Calculation/CalcFormCalculate.md) | `True` |
| [FeynGravSimplify](../Calculation/FeynGravSimplify.md) | `True` |
| `Generate* / Generate*Specific` | `True` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
