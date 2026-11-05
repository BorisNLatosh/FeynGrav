# TimeConstraint

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``System`TimeConstraint``. **Baseline:** 4.2.0.

This page describes only FeynGrav’s use of this dependency-owned option.

## Meaning and accepted values

Accepts positive numerical seconds or `Infinity`. Limits external execution (and restricted-adapter communication as documented), not input construction, export, import or total elapsed time. The check commands default to 10 seconds. A value is not a whole-process memory or CPU cap.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [CalcFormCalculate](../Calculation/CalcFormCalculate.md) | `Infinity` |
| [FeynGravSimplify](../Calculation/FeynGravSimplify.md) | `Infinity` |
| [CalcFormCheck](../Calculation/CalcFormCheck.md) | `10` |
| [FeynGravFORMCheck](../Calculation/FeynGravFORMCheck.md) | `10` |
| `Generate* / Generate*Specific` | `Infinity` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
