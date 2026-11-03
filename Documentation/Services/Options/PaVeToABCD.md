# PaVeToABCD

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynCalc`PaVeToABCD``. **Baseline:** 4.2.0 development.

This page describes only FeynGrav’s use of this dependency-owned option.

## Meaning and accepted values

Boolean option of `FeynGravSimplify` only. After a successful full-pipeline calculation, `True` converts supported PaVe functions to named A/B/C/D functions; `False` preserves the original pipeline output. Either Boolean is a no-op for restricted methods. Conversion is not analytic evaluation of scalar integrals. This symbol is owned by FeynCalc.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [FeynGravSimplify](../Calculation/FeynGravSimplify.md) | `True` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
