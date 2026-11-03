# OverwriteTarget

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``System`OverwriteTarget``. **Baseline:** 4.2.0 development.

This page describes only FeynGrav’s use of this dependency-owned option.

## Meaning and accepted values

Boolean option for export only. `False` refuses existing input, mapping or result paths; `True` explicitly permits replacement. It is not a general installer or generation overwrite setting.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [CalcFormExport](../Calculation/CalcFormExport.md) | `False` |
| [FeynGravFORMExport](../Calculation/FeynGravFORMExport.md) | `False` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
