# ShowTiming

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`ShowTiming``. **Baseline:** 4.2.0 development.

This is an option symbol; it is supplied as a rule to an owning command, not called as a function.

## Meaning and accepted values

Boolean timing display. `CalcFormCalculate` reports process wall time, excluding probe, export and import. The facade also reports total call time; generator timing additionally distinguishes construction/public-call wall time. Input evaluation before entering a command is outside its timer. Display does not change the returned expression.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [CalcFormCalculate](../Calculation/CalcFormCalculate.md) | `False` |
| [FeynGravSimplify](../Calculation/FeynGravSimplify.md) | `False` |
| `Generate* / Generate*Specific` | `False` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
